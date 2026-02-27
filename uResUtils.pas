unit uResUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  uResDefs, uDebug, uMimeDetect;

// Type detection mejorada
function ResUtils_IsImage(const Res: TResResult): Boolean;
function ResUtils_IsText(const Res: TResResult): Boolean;
function ResUtils_IsAudio(const Res: TResResult): Boolean;
function ResUtils_IsVideo(const Res: TResResult): Boolean;
function ResUtils_IsArchive(const Res: TResResult): Boolean;
function ResUtils_IsExecutable(const Res: TResResult): Boolean;
function ResUtils_IsFont(const Res: TResResult): Boolean;

// Categorización principal
function ResUtils_GetResourceCategory(const Res: TResResult): TResourceCategory; overload;
function ResUtils_GetResourceCategory(const Info: TResListInfo): TResourceCategory; overload;
function ResUtils_GetResourceCategory(const MimeType: TMimeType): TResourceCategory; overload;

// Resource kind checkers (mantener compatibilidad)
function ResUtils_CheckTextKind(const Name: string; const ResType: TResType;
  MimeType: TMimeType = mtUnknown; Data: TMemoryStream = nil): Integer;
function ResUtils_CheckImageKind(const Name: string; const ResType: TResType;
  MimeType: TMimeType = mtUnknown; Data: TMemoryStream = nil): Integer;

// Core reconstruction functions
function ResUtils_ReconstructBitmap(const Data: TMemoryStream): TMemoryStream;
function ResUtils_ReconstructIcon(const Data: TMemoryStream; const ResType: TResType): TMemoryStream;
function ResUtils_ReconstructText(const Data: TMemoryStream; const ResType: TResType): TMemoryStream;
function ResUtils_ReconstructCursor(const Data: TMemoryStream; const ResType: TResType): TMemoryStream;
function ResUtils_WrapPNGInCursor(const PNGData: TMemoryStream; Width, Height: Integer): TMemoryStream;

// Helper functions
function ResUtils_DetectTextEncoding(const Data: PByte; Size: Integer): TEncoding;
function ResUtils_WrapDIBInIcon(const DIBData: TMemoryStream; Width, Height: Integer): TMemoryStream;
function ResUtils_WrapPNGInIcon(const PNGData: TMemoryStream; Width, Height: Integer): TMemoryStream;
function ResUtils_WrapDIBInCursor(const DIBData: TMemoryStream; Width, Height: Integer): TMemoryStream;

implementation

uses
  Math;

//------------------------------------------------------------------------------
// Nuevas funciones de categorización
//------------------------------------------------------------------------------

function ResUtils_IsAudio(const Res: TResResult): Boolean;
begin
  Result := Res.Success and (TMimeDetector.GetFormatInfo(Res.MimeType).IsAudio);
end;

function ResUtils_IsVideo(const Res: TResResult): Boolean;
begin
  Result := Res.Success and (TMimeDetector.GetFormatInfo(Res.MimeType).IsVideo);
end;

function ResUtils_IsArchive(const Res: TResResult): Boolean;
begin
  Result := Res.Success and (TMimeDetector.GetFormatInfo(Res.MimeType).IsArchive);
end;

function ResUtils_IsExecutable(const Res: TResResult): Boolean;
begin
  Result := Res.Success and (TMimeDetector.GetFormatInfo(Res.MimeType).IsExecutable);
end;

function ResUtils_IsFont(const Res: TResResult): Boolean;
begin
  Result := Res.Success and (Res.MimeType = mtFont);
end;

//------------------------------------------------------------------------------
// Categorización basada en MimeType
//------------------------------------------------------------------------------

function ResUtils_GetResourceCategory(const MimeType: TMimeType): TResourceCategory;
var
  FormatInfo: TFileFormatInfo;
begin
  FormatInfo := TMimeDetector.GetFormatInfo(MimeType);

  if FormatInfo.IsText then
    Result := rcText
  else if FormatInfo.IsImage then
    Result := rcImage
  else if FormatInfo.IsAudio then
    Result := rcAudio
  else if FormatInfo.IsVideo then
    Result := rcVideo
  else if FormatInfo.IsArchive then
    Result := rcArchive
  else if FormatInfo.IsExecutable then
    Result := rcExecutable
  else if MimeType = mtFont then
    Result := rcFont
  else if MimeType = mtBinary then
    Result := rcBinary
  else
    Result := rcUnknown;
end;

//------------------------------------------------------------------------------
// Categorización basada en TResResult
//------------------------------------------------------------------------------

function ResUtils_GetResourceCategory(const Res: TResResult): TResourceCategory;
begin
  if not Res.Success then
    Exit(rcUnknown);

  Result := ResUtils_GetResourceCategory(Res.MimeType);
end;

//------------------------------------------------------------------------------
// Categorización basada en TResListInfo
//------------------------------------------------------------------------------

function ResUtils_GetResourceCategory(const Info: TResListInfo): TResourceCategory;
begin
  if Info.IsText then
    Result := rcText
  else if Info.IsImage then
    Result := rcImage
  else
    Result := ResUtils_GetResourceCategory(Info.MimeType);
end;

//------------------------------------------------------------------------------
// Funciones existentes (mantenidas para compatibilidad)
//------------------------------------------------------------------------------

function ResUtils_IsImage(const Res: TResResult): Boolean;
begin
  Result := Res.Success and (TMimeDetector.GetFormatInfo(Res.MimeType).IsImage);
end;

function ResUtils_IsText(const Res: TResResult): Boolean;
begin
  Result := Res.Success and (TMimeDetector.GetFormatInfo(Res.MimeType).IsText);
end;

function ResUtils_CheckTextKind(const Name: string; const ResType: TResType;
  MimeType: TMimeType = mtUnknown; Data: TMemoryStream = nil): Integer;
var
  I: Integer;
  Buffer: array[0..63] of Byte;
  BytesRead: Integer;
begin
  Result := 0;

  if MimeType <> mtUnknown then
  begin
    case MimeType of
      mtText, mtHtml, mtXml, mtJson, mtCss, mtJavaScript, mtRtf: Exit(1);
      mtBitmap, mtIcon, mtJpeg, mtPng, mtGif: Exit(-1);
    end;
  end;

  if ResType.IsInteger then
  begin
    case ResType.IntValue of
      6, 11, 16, 23, 24: Exit(1);
      2, 3, 12, 14: Exit(-1);
    end;
  end
  else
  begin
    if SameText(ResType.StrValue, 'HTML') or SameText(ResType.StrValue, 'XML') or
       SameText(ResType.StrValue, 'JSON') or SameText(ResType.StrValue, 'CSS') or
       SameText(ResType.StrValue, 'JS') or SameText(ResType.StrValue, 'TEXT') or
       SameText(ResType.StrValue, 'MANIFEST') or SameText(ResType.StrValue, 'VERSION') then
      Exit(1);
  end;

  if Data <> nil then
  begin
    Data.Position := 0;
    BytesRead := Data.Read(Buffer, SizeOf(Buffer));

    if BytesRead > 0 then
    begin
      Result := 1;
      for I := 0 to BytesRead - 1 do
      begin
        if Buffer[I] = 0 then
        begin
          Result := 0;
          Break;
        end;
        if (Buffer[I] < 32) and not (Buffer[I] in [9, 10, 13]) then
        begin
          Result := -1;
          Break;
        end;
      end;
    end;
  end;
end;

function ResUtils_CheckImageKind(const Name: string; const ResType: TResType;
  MimeType: TMimeType = mtUnknown; Data: TMemoryStream = nil): Integer;
var
  Header: array[0..7] of Byte;
begin
  Result := 0;

  if MimeType <> mtUnknown then
  begin
    case MimeType of
      mtBitmap, mtIcon, mtJpeg, mtPng, mtGif: Exit(1);
    end;
  end;

  if ResType.IsInteger then
  begin
    case ResType.IntValue of
      2, 3, 12, 14, 21, 22: Exit(1);
    end;
  end
  else
  begin
    if SameText(ResType.StrValue, 'BITMAP') or SameText(ResType.StrValue, 'BMP') or
       SameText(ResType.StrValue, 'ICON') or SameText(ResType.StrValue, 'ICO') or
       SameText(ResType.StrValue, 'PNG') or SameText(ResType.StrValue, 'JPG') or
       SameText(ResType.StrValue, 'JPEG') or SameText(ResType.StrValue, 'GIF') then
      Exit(1);
  end;

  if Data <> nil then
  begin
    Data.Position := 0;
    if Data.Size >= 2 then
    begin
      Data.Read(Header[0], 2);
      if (Header[0] = $42) and (Header[1] = $4D) then Exit(1);

      if Data.Size >= 8 then
      begin
        Data.Position := 0;
        Data.Read(Header, 8);
        if (Header[0] = $89) and (Header[1] = $50) and (Header[2] = $4E) and (Header[3] = $47) and
           (Header[4] = $0D) and (Header[5] = $0A) and (Header[6] = $1A) and (Header[7] = $0A) then Exit(1);
        if (Header[0] = $47) and (Header[1] = $49) and (Header[2] = $46) then Exit(1);
      end;

      if Data.Size >= 3 then
      begin
        Data.Position := 0;
        Data.Read(Header[0], 3);
        if (Header[0] = $FF) and (Header[1] = $D8) and (Header[2] = $FF) then Exit(1);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Funciones de reconstrucción (sin cambios)
//------------------------------------------------------------------------------

function ResUtils_ReconstructBitmap(const Data: TMemoryStream): TMemoryStream;
var
  BMPHeader: array[0..13] of Byte;
  InfoHeaderSize: DWord;
  FileSize: DWord;
  PixelOffset: DWord;
begin
  Result := nil;
  if (Data = nil) or (Data.Size < 40) then Exit;

  Data.Position := 0;
  Data.Read(InfoHeaderSize, 4);
  Data.Position := 0;

  FileSize := Data.Size + 14;
  PixelOffset := 14 + InfoHeaderSize;

  BMPHeader[0] := $42; BMPHeader[1] := $4D;
  BMPHeader[2] := Byte(FileSize); BMPHeader[3] := Byte(FileSize shr 8);
  BMPHeader[4] := Byte(FileSize shr 16); BMPHeader[5] := Byte(FileSize shr 24);
  BMPHeader[6] := 0; BMPHeader[7] := 0; BMPHeader[8] := 0; BMPHeader[9] := 0;
  BMPHeader[10] := Byte(PixelOffset); BMPHeader[11] := Byte(PixelOffset shr 8);
  BMPHeader[12] := Byte(PixelOffset shr 16); BMPHeader[13] := Byte(PixelOffset shr 24);

  Result := TMemoryStream.Create;
  try
    Result.Write(BMPHeader, 14);
    Data.Position := 0;
    Result.CopyFrom(Data, Data.Size);
    Result.Position := 0;
  except
    Result.Free;
    Result := nil;
  end;
end;

function ResUtils_WrapDIBInIcon(const DIBData: TMemoryStream; Width, Height: Integer): TMemoryStream;
type
  TIconDir = packed record
    idReserved: Word;
    idType: Word;
    idCount: Word;
  end;
  TIconDirEntry = packed record
    bWidth: Byte;
    bHeight: Byte;
    bColorCount: Byte;
    bReserved: Byte;
    wPlanes: Word;
    wBitCount: Word;
    dwBytesInRes: DWord;
    dwImageOffset: DWord;
  end;
var
  IconDir: TIconDir;
  IconEntry: TIconDirEntry;
  BitCount: Word;
begin
  Result := nil;
  if (DIBData = nil) or (DIBData.Size < 40) then Exit;

  BitCount := PWord(PByte(DIBData.Memory) + 14)^;

  Result := TMemoryStream.Create;
  try
    IconDir.idReserved := 0;
    IconDir.idType := 1;
    IconDir.idCount := 1;
    Result.Write(IconDir, SizeOf(IconDir));

    if Width >= 256 then
      IconEntry.bWidth := 0
    else
      IconEntry.bWidth := Width;

    if Height >= 256 then
      IconEntry.bHeight := 0
    else
      IconEntry.bHeight := Height;

    IconEntry.bColorCount := 0;
    IconEntry.bReserved := 0;
    IconEntry.wPlanes := 1;
    IconEntry.wBitCount := BitCount;
    IconEntry.dwBytesInRes := DIBData.Size;
    IconEntry.dwImageOffset := SizeOf(IconDir) + SizeOf(IconEntry);
    Result.Write(IconEntry, SizeOf(IconEntry));

    DIBData.Position := 0;
    Result.CopyFrom(DIBData, DIBData.Size);
    Result.Position := 0;
  except
    Result.Free;
    Result := nil;
  end;
end;

function ResUtils_WrapPNGInIcon(const PNGData: TMemoryStream; Width, Height: Integer): TMemoryStream;
type
  TIconDir = packed record
    idReserved: Word;
    idType: Word;
    idCount: Word;
  end;
  TIconDirEntry = packed record
    bWidth: Byte;
    bHeight: Byte;
    bColorCount: Byte;
    bReserved: Byte;
    wPlanes: Word;
    wBitCount: Word;
    dwBytesInRes: DWord;
    dwImageOffset: DWord;
  end;
var
  IconDir: TIconDir;
  IconEntry: TIconDirEntry;
begin
  Result := TMemoryStream.Create;
  try
    IconDir.idReserved := 0;
    IconDir.idType := 1;
    IconDir.idCount := 1;
    Result.Write(IconDir, SizeOf(IconDir));

    if Width >= 256 then
      IconEntry.bWidth := 0
    else
      IconEntry.bWidth := Width;

    if Height >= 256 then
      IconEntry.bHeight := 0
    else
      IconEntry.bHeight := Height;

    IconEntry.bColorCount := 0;
    IconEntry.bReserved := 0;
    IconEntry.wPlanes := 1;
    IconEntry.wBitCount := 32;
    IconEntry.dwBytesInRes := PNGData.Size;
    IconEntry.dwImageOffset := SizeOf(IconDir) + SizeOf(IconEntry);
    Result.Write(IconEntry, SizeOf(IconEntry));

    PNGData.Position := 0;
    Result.CopyFrom(PNGData, PNGData.Size);
    Result.Position := 0;
  except
    Result.Free;
    Result := nil;
  end;
end;

function ResUtils_ReconstructIcon(const Data: TMemoryStream; const ResType: TResType): TMemoryStream;
var
  Header: array[0..7] of Byte;
  IsPNG: Boolean;
  Width, Height: Integer;
  IsCursor: Boolean;
begin
  if (not IsCursor) and (Data.Size >= 6) then
  begin
    Data.Position := 0;
    Data.Read(Header, 6);
    Data.Position := 0;

    if (Header[0] = 0) and (Header[1] = 0) and
       (Header[2] = 1) and (Header[3] = 0) and
       (Header[4] > 0) and (Header[4] < 100) then
    begin
      DebugInfo('  Data is already a valid ICO file, returning as-is');
      Result := TMemoryStream.Create;
      Result.CopyFrom(Data, 0);
      Result.Position := 0;
      Exit;
    end;
  end;
  Result := nil;
  if (Data = nil) or (Data.Size < 8) then Exit;

  // Determinar si es cursor
  IsCursor := ResType.IsInteger and
              ((ResType.IntValue = 1) or (ResType.IntValue = 12));

  Data.Position := 0;
  Data.Read(Header, 8);
  Data.Position := 0;

  IsPNG := (Header[0] = $89) and (Header[1] = $50) and
           (Header[2] = $4E) and (Header[3] = $47) and
           (Header[4] = $0D) and (Header[5] = $0A) and
           (Header[6] = $1A) and (Header[7] = $0A);

  if IsPNG then
  begin
    Width := 0;
    Height := 0;
    if Data.Size > 24 then
    begin
      Data.Position := 16;
      Width := (Data.ReadByte shl 24) or
               (Data.ReadByte shl 16) or
               (Data.ReadByte shl 8) or
               Data.ReadByte;
      Height := (Data.ReadByte shl 24) or
                (Data.ReadByte shl 16) or
                (Data.ReadByte shl 8) or
                Data.ReadByte;
    end;
    Data.Position := 0;

    if IsCursor then
      Result := ResUtils_WrapPNGInCursor(Data, Width, Height)  // Necesitas crear esta función
    else
      Result := ResUtils_WrapPNGInIcon(Data, Width, Height);
    Exit;
  end;

  if (Header[0] = $28) and (Header[1] = $00) and
     (Header[2] = $00) and (Header[3] = $00) then
  begin
    if Data.Size >= 40 then
    begin
      Width := PInteger(PByte(Data.Memory) + 4)^;
      Height := PInteger(PByte(Data.Memory) + 8)^ div 2;

      if IsCursor then
        Result := ResUtils_WrapDIBInCursor(Data, Width, Height)  // Necesitas crear esta función
      else
        Result := ResUtils_WrapDIBInIcon(Data, Width, Height);
      Exit;
    end;
  end;

  // Si no se pudo reconstruir, devolver los datos originales
  Result := TMemoryStream.Create;
  Result.CopyFrom(Data, 0);
  Result.Position := 0;
end;

function ResUtils_DetectTextEncoding(const Data: PByte; Size: Integer): TEncoding;
begin
  Result := nil;
  if (Data = nil) or (Size < 2) then Exit;

  if (Data[0] = $FF) and (Data[1] = $FE) then
    Result := TEncoding.Unicode
  else if (Data[0] = $FE) and (Data[1] = $FF) then
    Result := TEncoding.BigEndianUnicode
  else if (Size >= 3) and (Data[0] = $EF) and (Data[1] = $BB) and (Data[2] = $BF) then
    Result := TEncoding.UTF8;
end;

function ResUtils_ReconstructText(const Data: TMemoryStream; const ResType: TResType): TMemoryStream;
var
  Buffer: PByte;
  Size: Integer;
  Encoding: TEncoding;
  UnicodeText: UnicodeString;
  AnsiText: AnsiString;
  StringLength: Word;
  Offset: Integer;
begin
  Result := nil;
  if (Data = nil) or (Data.Size < 2) then Exit;

  Data.Position := 0;
  Size := Data.Size;
  Buffer := Data.Memory;

  if ResType.IsInteger and (ResType.IntValue = 6) then
  begin
    Result := TMemoryStream.Create;
    try
      Result.WriteByte($EF); Result.WriteByte($BB); Result.WriteByte($BF);

      Offset := 0;
      while Offset + 1 < Size do
      begin
        StringLength := PWord(Buffer + Offset)^;
        Offset := Offset + 2;

        if (StringLength > 0) and (Offset + (StringLength * 2) <= Size) then
        begin
          SetLength(UnicodeText, StringLength);
          Move((Buffer + Offset)^, UnicodeText[1], StringLength * 2);
          AnsiText := UTF8Encode(UnicodeText);
          Result.Write(AnsiText[1], Length(AnsiText));
          Result.WriteByte(13); Result.WriteByte(10);
          Offset := Offset + (StringLength * 2);
        end
        else Break;
      end;
      Result.Position := 0;
    except
      Result.Free;
      Result := nil;
    end;
    Exit;
  end;

  if (not ResType.IsInteger and SameText(ResType.StrValue, 'TEXT')) or
     (ResType.IsInteger and (ResType.IntValue in [10, 16, 23, 24])) then
  begin
    if Size >= 2 then
    begin
      StringLength := PWord(Buffer)^;
      if (StringLength > 0) and (StringLength * 2 <= Size - 2) and (StringLength < 65535) then
      begin
        Data.Position := 2;
        SetLength(UnicodeText, StringLength);
        if Data.Read(UnicodeText[1], StringLength * 2) = StringLength * 2 then
        begin
          AnsiText := UTF8Encode(UnicodeText);
          Result := TMemoryStream.Create;
          Result.WriteByte($EF); Result.WriteByte($BB); Result.WriteByte($BF);
          Result.Write(AnsiText[1], Length(AnsiText));
          Result.Position := 0;
          Exit;
        end;
      end;
    end;
    Data.Position := 0;
  end;

  Encoding := ResUtils_DetectTextEncoding(Buffer, Size);
  if Encoding <> nil then
  begin
    try
      Data.Position := 0;
      if Encoding = TEncoding.Unicode then
      begin
        Data.Position := 2;
        SetLength(UnicodeText, (Size - 2) div 2);
        Data.Read(UnicodeText[1], Size - 2);
      end
      else if Encoding = TEncoding.BigEndianUnicode then
      begin
        Data.Position := 2;
        SetLength(UnicodeText, (Size - 2) div 2);
        Data.Read(UnicodeText[1], Size - 2);
      end
      else if Encoding = TEncoding.UTF8 then
      begin
        Result := TMemoryStream.Create;
        Data.Position := 3;
        Result.CopyFrom(Data, Size - 3);
        Result.Position := 0;
        Exit;
      end;

      AnsiText := UTF8Encode(UnicodeText);
      Result := TMemoryStream.Create;
      Result.WriteByte($EF); Result.WriteByte($BB); Result.WriteByte($BF);
      Result.Write(AnsiText[1], Length(AnsiText));
      Result.Position := 0;
    except
      Result.Free;
      Result := nil;
    end;
    Exit;
  end;

  Result := TMemoryStream.Create;
  Result.WriteByte($EF); Result.WriteByte($BB); Result.WriteByte($BF);
  Data.Position := 0;
  Result.CopyFrom(Data, Data.Size);
  Result.Position := 0;
end;

function ResUtils_ReconstructCursor(const Data: TMemoryStream; const ResType: TResType): TMemoryStream;
type
  TIconDir = packed record
    idReserved: Word;
    idType: Word;
    idCount: Word;
  end;

  TIconDirEntry = packed record
    bWidth: Byte;
    bHeight: Byte;
    bColorCount: Byte;
    bReserved: Byte;
    wPlanes: Word;     // Hotspot X
    wBitCount: Word;   // Hotspot Y
    dwBytesInRes: DWord;
    dwImageOffset: DWord;
  end;

  TBitmapInfoHeader = packed record
    biSize: DWord;
    biWidth: LongInt;
    biHeight: LongInt;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: DWord;
    biSizeImage: DWord;
    biXPelsPerMeter: LongInt;
    biYPelsPerMeter: LongInt;
    biClrUsed: DWord;
    biClrImportant: DWord;
  end;
var
  HotspotX, HotspotY: Word;
  BmpHeader: TBitmapInfoHeader;
  IconDir: TIconDir;
  IconEntry: TIconDirEntry;
  DibStream: TMemoryStream;
  RealHeight: Integer;
begin
  Result := nil;

  if (Data = nil) or (Data.Size < 4) then
    Exit;

  Data.Position := 0;

  // Leer hotspot (los primeros 4 bytes)
  HotspotX := Data.ReadWord;
  HotspotY := Data.ReadWord;

  // Leer BITMAPINFOHEADER (siguientes 40 bytes)
  Data.Read(BmpHeader, SizeOf(TBitmapInfoHeader));

  // Verificar que es un BITMAPINFOHEADER válido
  if BmpHeader.biSize <> SizeOf(TBitmapInfoHeader) then
    Exit;

  // Calcular altura real (la mitad porque incluye máscara AND)
  if BmpHeader.biHeight < 0 then
    RealHeight := (-BmpHeader.biHeight) div 2
  else
    RealHeight := BmpHeader.biHeight div 2;

  // Crear el archivo CUR
  Result := TMemoryStream.Create;
  try
    // Escribir ICONDIR
    IconDir.idReserved := 0;
    IconDir.idType := 2;      // CUR
    IconDir.idCount := 1;
    Result.Write(IconDir, SizeOf(IconDir));

    // Escribir ICONDIRENTRY
    if BmpHeader.biWidth >= 256 then
      IconEntry.bWidth := 0
    else
      IconEntry.bWidth := BmpHeader.biWidth;

    if RealHeight >= 256 then
      IconEntry.bHeight := 0
    else
      IconEntry.bHeight := RealHeight;

    IconEntry.bColorCount := 0;
    IconEntry.bReserved := 0;
    IconEntry.wPlanes := HotspotX;      // Hotspot X del cursor
    IconEntry.wBitCount := HotspotY;     // Hotspot Y del cursor
    IconEntry.dwBytesInRes := Data.Size - 4;  // Tamaño sin el hotspot
    IconEntry.dwImageOffset := SizeOf(IconDir) + SizeOf(IconEntry);
    Result.Write(IconEntry, SizeOf(IconEntry));

    // Escribir los datos del DIB (BITMAPINFOHEADER + datos de imagen)
    // Pero sin el hotspot inicial
    Data.Position := 4;  // Saltarse el hotspot
    Result.CopyFrom(Data, Data.Size - 4);

    Result.Position := 0;

  except
    Result.Free;
    Result := nil;
  end;
end;

function ResUtils_WrapDIBInCursor(const DIBData: TMemoryStream; Width, Height: Integer): TMemoryStream;
type
  TIconDir = packed record
    idReserved: Word;
    idType: Word;
    idCount: Word;
  end;
  TIconDirEntry = packed record
    bWidth: Byte;
    bHeight: Byte;
    bColorCount: Byte;
    bReserved: Byte;
    wPlanes: Word;     // Hotspot X
    wBitCount: Word;   // Hotspot Y
    dwBytesInRes: DWord;
    dwImageOffset: DWord;
  end;
var
  IconDir: TIconDir;
  IconEntry: TIconDirEntry;
  HotspotX, HotspotY: Word;
  ActualHeight: Integer;
  BitCount: Word;
  SafeWidth, SafeHeight: Integer;
begin
  Result := nil;
  if (DIBData = nil) or (DIBData.Size < 40) then Exit;

  try
    // Leer información correcta del DIB con validaciones
    DIBData.Position := 0;

    // Validar que podemos leer el ancho de forma segura
    if DIBData.Size < 40 then
    begin
      DebugError('DIB data too small');
      Exit;
    end;

    // El ancho está en bytes 4-7
    Width := PInteger(PByte(DIBData.Memory) + 4)^;

    // La altura está en bytes 8-11
    Height := PInteger(PByte(DIBData.Memory) + 8)^;

    // Obtener profundidad de color (bytes 14-15)
    BitCount := PWord(PByte(DIBData.Memory) + 14)^;

    // VALIDACIONES CRÍTICAS
    // =====================

    // Verificar que Width es razonable (entre 1 y 1024 píxeles)
    if (Width <= 0) or (Width > 1024) then
    begin
      DebugWarning(Format('Invalid cursor width: %d, using default 32', [Width]));
      Width := 32;
    end;

    // Verificar Height y calcular altura real
    if Height = 0 then
    begin
      DebugWarning('Height is zero, using default 32');
      ActualHeight := 32;
    end
    else if Height < 0 then
    begin
      // Top-down DIB, altura positiva después de dividir
      if (-Height) > 2048 then
        ActualHeight := 32
      else
        ActualHeight := (-Height) div 2;
    end
    else
    begin
      // Bottom-up DIB
      if Height > 2048 then
        ActualHeight := 32
      else
        ActualHeight := Height div 2;
    end;

    // Asegurar altura mínima
    if ActualHeight < 1 then
      ActualHeight := 1;

    // Calcular hotspot usando valores seguros
    SafeWidth := Width;
    SafeHeight := ActualHeight;

    // Asegurar que no son cero para la división
    if SafeWidth = 0 then SafeWidth := 32;
    if SafeHeight = 0 then SafeHeight := 32;

    HotspotX := SafeWidth div 2;
    HotspotY := SafeHeight div 2;

    // Asegurar que HotspotX y HotspotY están dentro del rango de Word (0-65535)
    if HotspotX > 65535 then HotspotX := 65535;
    if HotspotY > 65535 then HotspotY := 65535;

    DebugInfo(Format('  Cursor DIB: Raw Width=%d, Raw Height=%d, ActualHeight=%d, BitCount=%d, Hotspot(%d,%d)',
      [Width, Height, ActualHeight, BitCount, HotspotX, HotspotY]));

    // Crear el archivo CUR
    Result := TMemoryStream.Create;
    try
      // Escribir ICONDIR header con type=2 (CUR)
      IconDir.idReserved := 0;
      IconDir.idType := 2;      // 2 = Cursor
      IconDir.idCount := 1;
      Result.Write(IconDir, SizeOf(IconDir));

      // Para el directorio, usar valores seguros (0 significa 256)
      if SafeWidth >= 256 then
        IconEntry.bWidth := 0
      else
        IconEntry.bWidth := SafeWidth;

      if SafeHeight >= 256 then
        IconEntry.bHeight := 0
      else
        IconEntry.bHeight := SafeHeight;

      IconEntry.bColorCount := 0;
      IconEntry.bReserved := 0;
      IconEntry.wPlanes := HotspotX;
      IconEntry.wBitCount := HotspotY;
      IconEntry.dwBytesInRes := DIBData.Size;
      IconEntry.dwImageOffset := SizeOf(IconDir) + SizeOf(IconEntry);
      Result.Write(IconEntry, SizeOf(IconEntry));

      DIBData.Position := 0;
      Result.CopyFrom(DIBData, DIBData.Size);
      Result.Position := 0;

      DebugInfo(Format('  ✓ Cursor wrapped: %d bytes', [Result.Size]));
    except
      on E: Exception do
      begin
        DebugError('Error writing cursor: ' + E.Message);
        Result.Free;
        Result := nil;
      end;
    end;
  except
    on E: Exception do
    begin
      DebugError('Error processing DIB: ' + E.Message);
      Result := nil;
    end;
  end;
end;

function ResUtils_WrapPNGInCursor(const PNGData: TMemoryStream; Width, Height: Integer): TMemoryStream;
type
  TIconDir = packed record
    idReserved: Word;
    idType: Word;
    idCount: Word;
  end;
  TIconDirEntry = packed record
    bWidth: Byte;
    bHeight: Byte;
    bColorCount: Byte;
    bReserved: Byte;
    wPlanes: Word;     // Hotspot X
    wBitCount: Word;   // Hotspot Y
    dwBytesInRes: DWord;
    dwImageOffset: DWord;
  end;
var
  IconDir: TIconDir;
  IconEntry: TIconDirEntry;
  HotspotX, HotspotY: Word;
begin
  // Validar parámetros
  if (Width <= 0) or (Height <= 0) then
  begin
    Width := 32;
    Height := 32;
  end;

  HotspotX := Width div 2;
  HotspotY := Height div 2;

  // Asegurar rango
  if HotspotX > 65535 then HotspotX := 65535;
  if HotspotY > 65535 then HotspotY := 65535;

  Result := TMemoryStream.Create;
  try
    IconDir.idReserved := 0;
    IconDir.idType := 2;      // 2 = Cursor
    IconDir.idCount := 1;
    Result.Write(IconDir, SizeOf(IconDir));

    if Width >= 256 then
      IconEntry.bWidth := 0
    else
      IconEntry.bWidth := Width;

    if Height >= 256 then
      IconEntry.bHeight := 0
    else
      IconEntry.bHeight := Height;

    IconEntry.bColorCount := 0;
    IconEntry.bReserved := 0;
    IconEntry.wPlanes := HotspotX;
    IconEntry.wBitCount := HotspotY;
    IconEntry.dwBytesInRes := PNGData.Size;
    IconEntry.dwImageOffset := SizeOf(IconDir) + SizeOf(IconEntry);
    Result.Write(IconEntry, SizeOf(IconEntry));

    PNGData.Position := 0;
    Result.CopyFrom(PNGData, PNGData.Size);
    Result.Position := 0;
  except
    on E: Exception do
    begin
      DebugError('Error wrapping PNG cursor: ' + E.Message);
      Result.Free;
      Result := nil;
    end;
  end;
end;

end.
