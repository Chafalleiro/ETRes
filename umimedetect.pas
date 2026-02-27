unit uMimeDetect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, uDebug;

type
  // Original TMimeType - FULLY COMPATIBLE
  TMimeType = (
    mtUnknown,
    mtText,
    mtHtml,
    mtXml,
    mtJson,
    mtCss,
    mtJavaScript,
    mtRtf,
    mtEtChunk,           // <-- NUEVO: .etchnk (Chunk file)
    mtEtTpl,             // <-- NUEVO: .ettpl (Template file)
    mtBinary,
    mtBitmap,
    mtIcon,
    mtCursor,
    mtJpeg,
    mtPng,
    mtGif,
    mtAni,
    mtTiff,
    mtWave,
    mtAvi,
    mtMp3,
    mtFont,
    mtExecutable,
    mtArchive,
    mtPdf,
    mtWord,
    mtExcel,
    mtPowerPoint,
    mtRiff               // <-- NUEVO: Formato RIFF genérico
  );

type
  TFileFormatInfo = record
    Description: string;
    Extensions: string;
    IsText: Boolean;
    IsDocument: Boolean;
    IsImage: Boolean;
    IsAudio: Boolean;
    IsVideo: Boolean;
    IsArchive: Boolean;
    IsExecutable: Boolean;
    CanDisplay: Boolean;  // <-- AÑADIR ESTE CAMPO
  end;

  { TMimeDetector }
  TMimeDetector = class
  private
    class function DetectImageFormat(const Data: PByte; Size: Integer): TMimeType;
    class function DetectAudioFormat(const Data: PByte; Size: Integer): TMimeType;
    class function DetectVideoFormat(const Data: PByte; Size: Integer): TMimeType;
    class function DetectFontFormat(const Data: PByte; Size: Integer): TMimeType;
    class function DetectArchiveFormat(const Data: PByte; Size: Integer): TMimeType;
    class function DetectExecutableFormat(const Data: PByte; Size: Integer): TMimeType;
    class function DetectDocumentFormat(const Data: PByte; Size: Integer): TMimeType;
    class function DetectPackedDIB(const Data: PByte; Size: Integer): Boolean;
    class function DetectIconOrCursor(const Data: PByte; Size: Integer): TMimeType;

    class function IsTextBuffer(const Data: PByte; Size: Integer): Boolean;
    class function CheckRIFFSignature(const Data: PByte; Size: Integer): Boolean;
    class function CheckLineStart(const Data: PByte; Size: Integer; const Marker: string; MaxLines: Integer = 10): Boolean;
  public
    class function DetectFromStream(Stream: TStream): TMimeType;
    class function DetectFromMemory(Data: Pointer; Size: Integer): TMimeType;
    class function GetFormatInfo(MimeType: TMimeType): TFileFormatInfo;
    class function TypeToString(MimeType: TMimeType): String;
    class function BytesToHex(P: PByte; Count: Integer): String;
  end;

implementation

class function TMimeDetector.DetectFromStream(Stream: TStream): TMimeType;
var
  Header: array[0..4095] of Byte;  // Aumentamos para mejor detección de texto
  BytesRead: Integer;
  OriginalPos: Int64;
begin
  DebugInfo('Detecting From Stream: ');
  Result := mtUnknown;
  if Stream = nil then
    Exit;

  OriginalPos := Stream.Position;
  try
    Stream.Position := 0;
    BytesRead := Stream.Read(Header, SizeOf(Header));
    Stream.Position := OriginalPos;

    if BytesRead < 4 then
    begin
      // Too small, treat as text if printable
      Result := mtText;
      Exit;
    end;

    // ========================================================================
    // 1. FORMATOS DE IMAGEN (los más rápidos y específicos)
    // ========================================================================
    Result := DetectImageFormat(@Header, BytesRead);
    if Result <> mtUnknown then Exit;

    // ========================================================================
    // 2. FORMATOS DE AUDIO (incluyendo RIFF/WAVE)
    // ========================================================================
    Result := DetectAudioFormat(@Header, BytesRead);
    if Result <> mtUnknown then Exit;

    // ========================================================================
    // 3. FORMATOS DE VIDEO (incluyendo RIFF/AVI)
    // ========================================================================
    Result := DetectVideoFormat(@Header, BytesRead);
    if Result <> mtUnknown then Exit;

    // ========================================================================
    // 4. FORMATOS DE FUENTES
    // ========================================================================
    Result := DetectFontFormat(@Header, BytesRead);
    if Result <> mtUnknown then Exit;

    // ========================================================================
    // 5. FORMATOS DE ARCHIVO (ZIP, etc.)
    // ========================================================================
    Result := DetectArchiveFormat(@Header, BytesRead);
    if Result <> mtUnknown then Exit;

    // ========================================================================
    // 6. FORMATOS EJECUTABLES
    // ========================================================================
    Result := DetectExecutableFormat(@Header, BytesRead);
    if Result <> mtUnknown then Exit;

    // ========================================================================
    // 7. FORMATOS DE DOCUMENTO (incluye nuestros formatos personalizados)
    // ========================================================================
    Result := DetectDocumentFormat(@Header, BytesRead);
    if Result <> mtUnknown then Exit;

    // ========================================================================
    // 8. FINALMENTE, VERIFICAR SI ES TEXTO O BINARIO
    // ========================================================================
    if IsTextBuffer(@Header, BytesRead) then
      Result := mtText
    else
      Result := mtBinary;

  except
    Result := mtUnknown;
  end;
end;

class function TMimeDetector.DetectPackedDIB(const Data: PByte; Size: Integer): Boolean;
var
  InfoSize: DWord;
  Width: Integer;
  Height: Integer;
  Planes: Word;
  BitCount: Word;
  Compression: DWord;
begin
  DebugInfo('Detecting DIB type: ');
  Result := False;
  if (Data = nil) or (Size < 40) then  // BITMAPINFOHEADER is 40 bytes
    Exit;

  // Check if this looks like a BITMAPINFOHEADER
  // First DWORD is size of header (must be 40, 108, 124, etc)
  InfoSize := PDWord(Data)^;

  // Common header sizes for DIB
  if not (InfoSize in [40, 52, 56, 108, 124]) then
    Exit;

  if Size < InfoSize + 4 then
    Exit;

  // Width (must be positive or negative for top-down/bottom-up)
  Width := PInteger(Data + 4)^;
  if (Width = 0) or (Width > 10000) then  // Sanity check
    Exit;

  // Height (can be negative for top-down DIB)
  Height := PInteger(Data + 8)^;
  if (Height = 0) or (Abs(Height) > 10000) then  // Sanity check
    Exit;

  // Planes (must be 1)
  Planes := PWord(Data + 12)^;
  if Planes <> 1 then
    Exit;

  // BitCount (1, 4, 8, 16, 24, 32)
  BitCount := PWord(Data + 14)^;
  if not (BitCount in [1, 4, 8, 16, 24, 32]) then
    Exit;

  // Compression (0 = BI_RGB, 1 = BI_RLE8, 2 = BI_RLE4, 3 = BI_BITFIELDS, etc)
  Compression := PDWord(Data + 16)^;
  if Compression > 6 then  // BI_JPEG, BI_PNG also valid but rare
    Exit;

  Result := True;
end;

class function TMimeDetector.DetectIconOrCursor(const Data: PByte; Size: Integer): TMimeType;
var
  Reserved: Word;
  ResType: Word;
  ImageCount: Word;
  I: Integer;
  Offset: DWord;
begin
  Result := mtUnknown;

  if (Data = nil) or (Size < 6) then
    Exit;

  // ICON/CURSOR header format:
  // WORD  Reserved (0)
  // WORD  Type (1 = Icon, 2 = Cursor)
  // WORD  ImageCount
  Reserved := PWord(Data)^;
  if Reserved <> 0 then
    Exit;

  ResType := PWord(Data + 2)^;
  if not (ResType in [1, 2]) then
    Exit;

  ImageCount := PWord(Data + 4)^;
  if (ImageCount = 0) or (ImageCount > 255) then
    Exit;

  // Check if size is plausible
  if Size < 6 + (ImageCount * 16) then  // Each image entry is 16 bytes
    Exit;

  // Check first image entry for valid offset
  Offset := PDWord(Data + 6 + 12)^;  // Offset is last DWORD of first entry
  if (Offset = 0) or (Offset > DWord(Size)) then
    Exit;

  if ResType = 1 then
    Result := mtIcon
  else
    Result := mtCursor;  // We don't have separate mtCursor, so use mtIcon
end;

class function TMimeDetector.DetectImageFormat(const Data: PByte; Size: Integer): TMimeType;
var
  InfoHeaderSize: DWord;
  Width: Integer;
  Height: Integer;
  Planes: Word;
  BitCount: Word;
begin
  DebugInfo('Detecting Image type: ');
  Result := mtUnknown;
  if (Data = nil) or (Size < 4) then
    Exit;

  // ANI (también podría detectarse aquí, pero ya está en audio/video)
  if CheckRIFFSignature(Data, Size) then
  begin
    if (Size >= 12) and
       (Data[8] = Ord('A')) and (Data[9] = Ord('C')) and
       (Data[10] = Ord('O')) and (Data[11] = Ord('N')) then
       Exit(mtAni);
  end;

  // ICO/CUR - important: type 1 = ICO, type 2 = CUR
  if (Size >= 4) and (Data[0] = 0) and (Data[1] = 0) then
  begin
    if (Data[2] = 1) and (Data[3] = 0) then
      Exit(mtIcon)
    else if (Data[2] = 2) and (Data[3] = 0) then
      Exit(mtCursor);  // Necesitas añadir mtCursor a TMimeType
  end;

  // BMP with file header
  if (Data[0] = $42) and (Data[1] = $4D) then
    Exit(mtBitmap);

  // Packed DIB (no BITMAPFILEHEADER) - common in RT_BITMAP and RT_ICON resources
  if Size >= 40 then
  begin
    InfoHeaderSize := PDWord(Data)^;

    // Check for valid BITMAPINFOHEADER (size 40, 52, 56, 108, 124)
    if InfoHeaderSize in [40, 52, 56, 108, 124] then
    begin
      // Width must be positive and reasonable
      Width := PInteger(Data + 4)^;
      if (Width > 0) and (Width < 10000) then
      begin
        // Height can be positive or negative, but reasonable
        Height := PInteger(Data + 8)^;
        if (Abs(Height) > 0) and (Abs(Height) < 20000) then
        begin
          // Planes must be 1
          Planes := PWord(Data + 12)^;
          if Planes = 1 then
          begin
            // BitCount must be valid
            BitCount := PWord(Data + 14)^;
            if BitCount in [1, 4, 8, 16, 24, 32] then
            begin
              // This is a valid DIB - return mtBitmap
              Exit(mtBitmap);
            end;
          end;
        end;
      end;
    end;
  end;

  // ICO/CUR with resource header
  Result := DetectIconOrCursor(Data, Size);
  if Result <> mtUnknown then
    Exit;

  // PNG
  if (Size >= 8) and (Data[0] = $89) and (Data[1] = $50) and
     (Data[2] = $4E) and (Data[3] = $47) and
     (Data[4] = $0D) and (Data[5] = $0A) and
     (Data[6] = $1A) and (Data[7] = $0A) then
    Exit(mtPng);

  // JPEG
  if (Size >= 3) and (Data[0] = $FF) and (Data[1] = $D8) and (Data[2] = $FF) then
    Exit(mtJpeg);

  // GIF
  if (Size >= 3) and (Data[0] = $47) and (Data[1] = $49) and (Data[2] = $46) then
    Exit(mtGif);
end;

class function TMimeDetector.CheckRIFFSignature(const Data: PByte; Size: Integer): Boolean;
begin
  Result := (Size >= 4) and
            (Data[0] = Ord('R')) and
            (Data[1] = Ord('I')) and
            (Data[2] = Ord('F')) and
            (Data[3] = Ord('F'));
end;

class function TMimeDetector.DetectAudioFormat(const Data: PByte; Size: Integer): TMimeType;
begin
  Result := mtUnknown;
  if (Data = nil) or (Size < 12) then
    Exit;

  // WAVE (RIFF WAVE)
  if CheckRIFFSignature(Data, Size) then
  begin
    if (Size >= 12) and
       (Data[8] = Ord('W')) and (Data[9] = Ord('A')) and
       (Data[10] = Ord('V')) and (Data[11] = Ord('E')) then
      Exit(mtWave);
  end;

  // WAV
  if (Size >= 12) and (Data[0] = $52) and (Data[1] = $49) and
     (Data[2] = $46) and (Data[3] = $46) and
     (Data[8] = $57) and (Data[9] = $41) and
     (Data[10] = $56) and (Data[11] = $45) then
    Exit(mtWave);

  // MP3 ID3v2
  if (Size >= 3) and (Data[0] = $49) and (Data[1] = $44) and (Data[2] = $33) then
    Exit(mtMp3);

  // MP3 without ID3
  if (Size >= 2) and ((Data[0] = $FF) and ((Data[1] and $FE) = $FA)) or
     ((Data[0] = $FF) and ((Data[1] and $FE) = $F2)) then
    Exit(mtMp3);
end;

class function TMimeDetector.DetectVideoFormat(const Data: PByte; Size: Integer): TMimeType;
begin
  Result := mtUnknown;
  if (Data = nil) or (Size < 12) then
    Exit;

  // AVI
  if (Size >= 12) and (Data[0] = $52) and (Data[1] = $49) and
     (Data[2] = $46) and (Data[3] = $46) and
     (Data[8] = $41) and (Data[9] = $56) and
     (Data[10] = $49) and (Data[11] = $20) then
    Exit(mtAvi);
end;

class function TMimeDetector.DetectFontFormat(const Data: PByte; Size: Integer): TMimeType;
begin
  Result := mtUnknown;
  if (Data = nil) or (Size < 8) then
    Exit;

  // TrueType/OpenType
  if (Size >= 4) and (Data[0] = $00) and (Data[1] = $01) and
     (Data[2] = $00) and (Data[3] = $00) then
    Exit(mtFont);

  // WOFF (Web Open Font Format)
  if (Size >= 8) and (Data[0] = $77) and (Data[1] = $4F) and
     (Data[2] = $46) and (Data[3] = $46) and  // 'wOFF'
     (Data[4] = $00) and (Data[5] = $01) and  // version 1.0
     (Data[6] = $00) and (Data[7] = $00) then
    Exit(mtFont);

  // WOFF2 (Web Open Font Format 2)
  if (Size >= 8) and (Data[0] = $77) and (Data[1] = $4F) and
     (Data[2] = $46) and (Data[3] = $32) and  // 'wOF2'
     (Data[4] = $00) and (Data[5] = $01) and  // version 1.0
     (Data[6] = $00) and (Data[7] = $00) then
    Exit(mtFont);
end;

class function TMimeDetector.DetectArchiveFormat(const Data: PByte; Size: Integer): TMimeType;
begin
  Result := mtUnknown;
  if (Data = nil) or (Size < 4) then
    Exit;

  // ZIP
  if (Size >= 4) and (Data[0] = $50) and (Data[1] = $4B) and
     ((Data[2] = $03) and (Data[3] = $04) or
      (Data[2] = $05) and (Data[3] = $06) or
      (Data[2] = $07) and (Data[3] = $08)) then
    Exit(mtArchive);

  // RAR
  if (Size >= 7) and (Data[0] = $52) and (Data[1] = $61) and
     (Data[2] = $72) and (Data[3] = $21) and
     (Data[4] = $1A) and (Data[5] = $07) and
     ((Data[6] = $00) or (Data[6] = $01)) then
    Exit(mtArchive);

  // 7-Zip
  if (Size >= 6) and (Data[0] = $37) and (Data[1] = $7A) and
     (Data[2] = $BC) and (Data[3] = $AF) and
     (Data[4] = $27) and (Data[5] = $1C) then
    Exit(mtArchive);

  // GZIP
  if (Size >= 2) and (Data[0] = $1F) and (Data[1] = $8B) then
    Exit(mtArchive);
end;

class function TMimeDetector.DetectExecutableFormat(const Data: PByte; Size: Integer): TMimeType;
begin
  Result := mtUnknown;
  if (Data = nil) or (Size < 2) then
    Exit;

  // MZ executable
  if (Data[0] = $4D) and (Data[1] = $5A) then
    Exit(mtExecutable);

  // COM
  if (Data[0] = $E9) or (Data[0] = $EB) or (Data[0] = $CD) then
    Exit(mtExecutable);
end;

class function TMimeDetector.DetectDocumentFormat(const Data: PByte; Size: Integer): TMimeType;
var
  S: string;
  I: Integer;
begin
  Result := mtUnknown;

  // ========================================================================
  // RTF (Rich Text Format)
  // ========================================================================
  if (Size >= 5) and (Data[0] = Ord('{')) and (Data[1] = Ord('\')) and
     (Data[2] = Ord('r')) and (Data[3] = Ord('t')) and (Data[4] = Ord('f')) then
    Exit(mtRtf);

  // ========================================================================
  // PDF (Portable Document Format)
  // ========================================================================
  if (Size >= 4) and (Data[0] = Ord('%')) and (Data[1] = Ord('P')) and
     (Data[2] = Ord('D')) and (Data[3] = Ord('F')) then
    Exit(mtPdf);

  // ========================================================================
  // Solo continuamos si parece texto
  // ========================================================================
  if not IsTextBuffer(Data, Size) then
    Exit;

  // ========================================================================
  // FORMATOS PERSONALIZADOS (primeras líneas)
  // ========================================================================

  // ET Chunk
  if CheckLineStart(Data, Size, '<!-- CHUNK', 5) then
    Exit(mtEtChunk);

  // ET Template
  if CheckLineStart(Data, Size, '<!-- TPL', 5) then
    Exit(mtEtTpl);

  // ========================================================================
  // FORMATOS WEB ESTÁNDAR
  // ========================================================================

  // HTML/XML (buscar en primeras líneas)
  if CheckLineStart(Data, Size, '<!DOCTYPE HTML', 5) or
     CheckLineStart(Data, Size, '<!DOCTYPE html', 5) or
     CheckLineStart(Data, Size, '<HTML', 5) or
     CheckLineStart(Data, Size, '<html', 5) then
    Exit(mtHtml);

  if CheckLineStart(Data, Size, '<?xml', 5) then
    Exit(mtXml);

  // ========================================================================
  // JSON
  // ========================================================================

  // Crear string para búsqueda más fácil
  SetLength(S, Min(512, Size));
  for I := 0 to Length(S) - 1 do
    S[I + 1] := Chr(Data[I]);

  // Quitar espacios en blanco al inicio
  S := TrimLeft(S);

  // JSON debe comenzar con { o [
  if (Length(S) > 0) and ((S[1] = '{') or (S[1] = '[')) then
  begin
    // Verificar que tenga pares clave:valor
    if (Pos('":', S) > 0) then
      Exit(mtJson);
  end;

  // ========================================================================
  // CSS
  // ========================================================================

  // Buscar patrones comunes de CSS
  if (Pos('{', S) > 0) and (Pos('}', S) > 0) and
     (Pos(':', S) > 0) and (Pos(';', S) > 0) then
  begin
    // Verificar que no sea JSON
    if (S[1] <> '{') and (S[1] <> '[') then
      Exit(mtCss);
  end;

  // ========================================================================
  // JavaScript
  // ========================================================================

  // Buscar palabras clave comunes de JS
  if (Pos('function', S) > 0) or
     (Pos('var ', S) > 0) or
     (Pos('let ', S) > 0) or
     (Pos('const ', S) > 0) or
     (Pos('if (', S) > 0) or
     (Pos('for (', S) > 0) then
  begin
    // Asegurar que no sea HTML
    if (Pos('<', S) = 0) then
      Exit(mtJavaScript);
  end;
end;

class function TMimeDetector.CheckLineStart(const Data: PByte; Size: Integer; const Marker: string; MaxLines: Integer = 10): Boolean;
var
  I, J: Integer;
  LineStart: Integer;
  LinesChecked: Integer;
  MarkerLen: Integer;
  Match: Boolean;
begin
  Result := False;
  MarkerLen := Length(Marker);

  if (MarkerLen = 0) or (Size < MarkerLen) then
    Exit;

  LineStart := 0;
  LinesChecked := 0;
  I := 0;

  while (I < Size) and (LinesChecked < MaxLines) do
  begin
    // Buscar fin de línea (LF)
    if (Data[I] = 10) or (I = Size - 1) then
    begin
      // Verificar si la línea tiene suficiente longitud
      if (I - LineStart + 1 >= MarkerLen) then
      begin
        Match := True;
        for J := 0 to MarkerLen - 1 do
        begin
          if Chr(Data[LineStart + J]) <> Marker[J + 1] then
          begin
            Match := False;
            Break;
          end;
        end;

        if Match then
        begin
          Result := True;
          Exit;
        end;
      end;

      // Avanzar a la siguiente línea
      LineStart := I + 1;
      Inc(LinesChecked);
    end;

    Inc(I);
  end;
end;

class function TMimeDetector.IsTextBuffer(const Data: PByte; Size: Integer): Boolean;
var
  I: Integer;
  CheckSize: Integer;
begin
  Result := True;
  CheckSize := Min(1024, Size);

  for I := 0 to CheckSize - 1 do
  begin
    if Data[I] = 0 then
    begin
      Result := False;
      Exit;
    end;
    if (Data[I] < 32) and not (Data[I] in [9, 10, 13]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

class function TMimeDetector.DetectFromMemory(Data: Pointer; Size: Integer): TMimeType;
var
  Stream: TMemoryStream;
begin
  Result := mtUnknown;
  if (Data = nil) or (Size <= 0) then
    Exit;

  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(Data^, Size);
    Stream.Position := 0;
    Result := DetectFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

class function TMimeDetector.TypeToString(MimeType: TMimeType): string;
begin
  case MimeType of
    mtUnknown:     Result := 'Unknown';
    mtText:        Result := 'Text';
    mtHtml:        Result := 'HTML';
    mtXml:         Result := 'XML';
    mtJson:        Result := 'JSON';
    mtCss:         Result := 'CSS';
    mtJavaScript:  Result := 'JavaScript';
    mtRtf:         Result := 'RTF';
    mtEtChunk:     Result := 'ET Chunk';      // <-- NUEVO
    mtEtTpl:       Result := 'ET Template';    // <-- NUEVO
    mtBitmap:      Result := 'Bitmap';
    mtIcon:        Result := 'Icon';
    mtCursor:      Result := 'Icon';
    mtJpeg:        Result := 'JPEG';
    mtPng:         Result := 'PNG';
    mtGif:         Result := 'GIF';
    mtAni:         Result := 'Animated Cursor';
    mtRiff:        Result := 'RIFF Container'; // <-- NUEVO
    mtWave:        Result := 'Wave Audio';
    mtMp3:         Result := 'MP3 Audio';
    mtAvi:         Result := 'AVI Video';
    mtExecutable:  Result := 'Executable';
    mtArchive:     Result := 'Archive';
    mtFont:        Result := 'Font';
    mtBinary:      Result := 'Binary Data';
    else           Result := 'Unknown';
  end;
end;

class function TMimeDetector.BytesToHex(P: PByte; Count: Integer): String;
var
  I: Integer;
begin
  Result := '';
  if (Count <= 0) or (P = nil) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + IntToHex(P[I], 2);
  end;
end;

class function TMimeDetector.GetFormatInfo(MimeType: TMimeType): TFileFormatInfo;
begin
  // Inicializar valores por defecto
  Result.Description := 'Unknown';
  Result.Extensions := '';
  Result.IsText := False;
  Result.IsImage := False;
  Result.IsAudio := False;
  Result.IsVideo := False;
  Result.IsArchive := False;
  Result.IsExecutable := False;
  Result.CanDisplay := True;

  case MimeType of
    // Text formats
    mtText:
      begin
        Result.Description := 'Text';
        Result.Extensions := '.txt';
        Result.IsText := True;
      end;

    mtHtml:
      begin
        Result.Description := 'HTML';
        Result.Extensions := '.html,.htm';
        Result.IsText := True;
      end;

    mtXml:
      begin
        Result.Description := 'XML';
        Result.Extensions := '.xml';
        Result.IsText := True;
      end;

    mtJson:
      begin
        Result.Description := 'JSON';
        Result.Extensions := '.json';
        Result.IsText := True;
      end;

    mtCss:
      begin
        Result.Description := 'CSS';
        Result.Extensions := '.css';
        Result.IsText := True;
      end;

    mtJavaScript:
      begin
        Result.Description := 'JavaScript';
        Result.Extensions := '.js';
        Result.IsText := True;
      end;

    mtRtf:
      begin
        Result.Description := 'RTF';
        Result.Extensions := '.rtf';
        Result.IsText := True;
      end;

    // ====================================================================
    // NUEVOS FORMATOS PERSONALIZADOS
    // ====================================================================
    mtEtChunk:
      begin
        Result.Description := 'ET Chunk';
        Result.Extensions := '.etchnk';
        Result.IsText := True;
      end;

    mtEtTpl:
      begin
        Result.Description := 'ET Template';
        Result.Extensions := '.ettpl';
        Result.IsText := True;
      end;

    mtRiff:
      begin
        Result.Description := 'RIFF Container';
        Result.Extensions := '.riff,.avi,.wav,.ani';
        Result.IsText := False;
        Result.IsAudio := True;   // Puede contener audio
        Result.IsVideo := True;   // Puede contener video
      end;

    // Image formats
    mtBitmap:
      begin
        Result.Description := 'Bitmap';
        Result.Extensions := '.bmp';
        Result.IsImage := True;
      end;

    mtIcon:
      begin
        Result.Description := 'Icon';
        Result.Extensions := '.ico';
        Result.IsImage := True;
      end;

    mtCursor:
      begin
        Result.Description := 'Cursor';
        Result.Extensions := '.cur';
        Result.IsImage := True;
      end;

    mtJpeg:
      begin
        Result.Description := 'JPEG';
        Result.Extensions := '.jpg,.jpeg';
        Result.IsImage := True;
      end;

    mtPng:
      begin
        Result.Description := 'PNG';
        Result.Extensions := '.png';
        Result.IsImage := True;
      end;

    mtGif:
      begin
        Result.Description := 'GIF';
        Result.Extensions := '.gif';
        Result.IsImage := True;
      end;

    mtAni:
      begin
        Result.Description := 'Animated Cursor';
        Result.Extensions := '.ani';
        Result.IsImage := True;
        Result.CanDisplay := False;
      end;

    // Audio formats
    mtWave:
      begin
        Result.Description := 'Wave Audio';
        Result.Extensions := '.wav';
        Result.IsAudio := True;
        Result.CanDisplay := False;
      end;

    mtMp3:
      begin
        Result.Description := 'MP3 Audio';
        Result.Extensions := '.mp3';
        Result.IsAudio := True;
        Result.CanDisplay := False;
      end;

    // Video formats
    mtAvi:
      begin
        Result.Description := 'AVI Video';
        Result.Extensions := '.avi';
        Result.IsVideo := True;
        Result.CanDisplay := False;
      end;

    // Other formats
    mtExecutable:
      begin
        Result.Description := 'Executable';
        Result.Extensions := '.exe,.dll,.ocx';
        Result.IsExecutable := True;
        Result.CanDisplay := False;
      end;

    mtArchive:
      begin
        Result.Description := 'Archive';
        Result.Extensions := '.zip,.rar,.7z';
        Result.IsArchive := True;
        Result.CanDisplay := False;
      end;

    mtFont:
      begin
        Result.Description := 'Font';
        Result.Extensions := '.ttf,.fon';
        Result.CanDisplay := False;
      end;

    mtBinary:
      begin
        Result.Description := 'Binary Data';
        Result.Extensions := '.bin';
        Result.IsText := False;
      end;
  end;
end;

end.
