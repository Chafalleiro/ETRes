unit uResFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uResDefs, uDebug, Math;
(*
type
  TResHeader = packed record
    DataSize: DWord;
    HeaderSize: DWord;
    ResType: TResType;  // Variable length (int or string)
    ResName: TResType;  // Variable length (int or string)
    DataVersion: DWord;
    MemoryFlags: Word;
    LanguageID: Word;
    Version: DWord;
    Characteristics: DWord;
  end;
*)
// RES file format constants
const
  RES_MAGIC = $00000000;
  RES_HEADER_SIZE = 32;

// File parsing functions
function ResFile_Open(const FileName: String): TFileStream;
function ResFile_Close(var Stream: TFileStream): Boolean;
function ResFile_IsValid(Stream: TFileStream): Boolean;

// Resource parsing
function ResFile_Parse(Stream: TFileStream; out Resources: TResInfoArray): Boolean;
function ResFile_FindResource(Stream: TFileStream; const Resources: TResInfoArray;
  const Name: String; ResType: TResType; out ResInfo: TResInfo): Boolean;
function ResFile_LoadResource(Stream: TFileStream; const ResInfo: TResInfo): TMemoryStream;
function ResFile_LoadResourceByIndex(Stream: TFileStream; const Resources: TResInfoArray;
  Index: Integer): TMemoryStream;

// Helper functions
function ResFile_ReadWord(Stream: TFileStream): Word;
function ResFile_ReadDWord(Stream: TFileStream): DWord;
function ResFile_ReadString(Stream: TFileStream; Unicode: Boolean = False): String;
function ResFile_ReadResType(Stream: TFileStream): TResType;
function ResFile_AlignDWord(Value: Int64): Int64;
function ResFile_SkipNullPadding(Stream: TFileStream): Int64;
function ResFile_CalculateChecksum(Stream: TFileStream; Offset, Size: Int64): DWord;

// Debug functions
procedure ResFile_DumpResourceInfo(const Info: TResInfo);
procedure ResFile_DumpAllResources(const Resources: TResInfoArray);

implementation

// Open resource file
function ResFile_Open(const FileName: String): TFileStream;
begin
  if not FileExists(FileName) then
  begin
    DebugError('File not found: ' + FileName);
    Exit(nil);
  end;

  try
    Result := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
    on E: Exception do
    begin
      DebugError('Cannot open file ' + FileName + ': ' + E.Message);
      Result := nil;
    end;
  end;
end;

// Close resource file
function ResFile_Close(var Stream: TFileStream): Boolean;
begin
  Result := False;
  if Stream <> nil then
  begin
    FreeAndNil(Stream);
    Result := True;
  end;
end;

// Check if stream is a valid RES file
function ResFile_IsValid(Stream: TFileStream): Boolean;
begin
  Result := False;
  if Stream = nil then Exit;

  try
    Stream.Position := 0;
    // Basic check - file should have some data
    Result := (Stream.Size > 16);
  except
    Result := False;
  end;
end;

// Read a WORD (16-bit) from stream
function ResFile_ReadWord(Stream: TFileStream): Word;
begin
  Result := 0;
  if Stream = nil then Exit;

  if Stream.Read(Result, SizeOf(Word)) = SizeOf(Word) then
    Result := LEtoN(Result);
end;

// Read a DWORD (32-bit) from stream
function ResFile_ReadDWord(Stream: TFileStream): DWord;
begin
  Result := 0;
  if Stream = nil then Exit;

  if Stream.Read(Result, SizeOf(DWord)) = SizeOf(DWord) then
    Result := LEtoN(Result);
end;

// Read string from stream
function ResFile_ReadString(Stream: TFileStream; Unicode: Boolean = False): String;
var
  Len: Word;
  I: Integer;
  Ch: AnsiChar;
  WCh: WideChar;
  SavedPos: Int64;
begin
  Result := '';
  if Stream = nil then Exit;

  SavedPos := Stream.Position;

  try
    Len := ResFile_ReadWord(Stream);

    if Len = 0 then Exit;

    if Unicode then
    begin
      SetLength(Result, Len);
      for I := 1 to Len do
      begin
        if Stream.Read(WCh, SizeOf(WideChar)) <> SizeOf(WideChar) then Break;
        Result[I] := Char(WCh);
      end;
    end
    else
    begin
      SetLength(Result, Len);
      for I := 1 to Len do
      begin
        if Stream.Read(Ch, SizeOf(AnsiChar)) <> SizeOf(AnsiChar) then Break;
        Result[I] := Char(Ch);
      end;
    end;
  except
    Stream.Position := SavedPos;
    Result := '';
  end;
end;

// Read resource type/name
function ResFile_ReadResType(Stream: TFileStream): TResType;
var
  FirstWord: Word;
  SavedPos: Int64;
  IntVal: Word;
  StrVal: String;
  WCh: WideChar;
  TempWStr: WideString;
  FirstChar: WideChar;
  BytesRead: Integer;
begin
  // Initialize with default
  Result.IsInteger := True;
  Result.IntValue := 0;
  Result.StrValue := '';

  if Stream = nil then Exit;

  SavedPos := Stream.Position;
  BytesRead := 0;

  try
    DebugDetail('  ReadResType at offset: 0x' + IntToHex(SavedPos, 8));

    // Read first word
    if Stream.Read(FirstWord, SizeOf(Word)) <> SizeOf(Word) then
    begin
      Stream.Position := SavedPos;
      Exit;
    end;

    BytesRead := 2;
    FirstWord := LEtoN(FirstWord);
    DebugDetail('  First word: 0x' + IntToHex(FirstWord, 4));

    if FirstWord = $FFFF then
    begin
      DebugDetail('  Integer type detected');
      // Integer type/name
      if Stream.Read(IntVal, SizeOf(Word)) = SizeOf(Word) then
      begin
        IntVal := LEtoN(IntVal);
        DebugDetail('  Integer value: ' + IntToStr(IntVal));
        Result := ResType_CreateInt(IntVal);
        BytesRead := BytesRead + 2;
      end;
    end
    else
    begin
      DebugDetail('  String type detected');
      // String type/name - NULL-TERMINATED Unicode string!
      // FirstWord contains the first character
      FirstChar := WideChar(FirstWord);

      TempWStr := '';

      // Add the first character we already read
      if FirstChar <> #0 then
      begin
        TempWStr := TempWStr + FirstChar;
        DebugDetail('  First char: 0x' + IntToHex(Ord(FirstChar), 4) + ' = "' + FirstChar + '"');
      end
      else
      begin
        DebugDetail('  Empty string (null as first char)');
      end;

      // Read remaining characters until null terminator
      while True do
      begin
        if Stream.Read(WCh, SizeOf(WideChar)) <> SizeOf(WideChar) then
        begin
          DebugDetail('  End of stream reached');
          Break;
        end;

        BytesRead := BytesRead + 2;

        if WCh = #0 then
        begin
          DebugDetail('  Null terminator found at byte ' + IntToStr(BytesRead));
          Break;
        end;

        TempWStr := TempWStr + WCh;
        DebugDetail('  Char: 0x' + IntToHex(Ord(WCh), 4) + ' = "' + WCh + '"');
      end;

      StrVal := String(TempWStr);
      DebugDetail('  Result string: "' + StrVal + '" (length: ' +
                  IntToStr(Length(StrVal)) + ', total bytes: ' + IntToStr(BytesRead) + ')');

      Result := ResType_CreateStr(StrVal);

      DebugDetail('  Final position: 0x' + IntToHex(Stream.Position, 8));
    end;

  except
    on E: Exception do
    begin
      DebugError('  Exception in ReadResType: ' + E.Message);
      Stream.Position := SavedPos;
    end;
  end;
end;

// Align to DWORD boundary
function ResFile_AlignDWord(Value: Int64): Int64;
begin
  Result := Value;
  if (Value mod 4) <> 0 then
    Result := Value + (4 - (Value mod 4));
end;

// Skip null padding at beginning
function ResFile_SkipNullPadding(Stream: TFileStream): Int64;
var
  TestDWord: DWord;
begin
  Result := Stream.Position;

  if Stream = nil then Exit;

  while Stream.Position < Stream.Size do
  begin
    if Stream.Read(TestDWord, SizeOf(DWord)) <> SizeOf(DWord) then Break;

    if TestDWord <> 0 then
    begin
      Stream.Position := Stream.Position - 4;
      Result := Stream.Position;
      Break;
    end;
  end;
end;

// Calculate simple checksum
function ResFile_CalculateChecksum(Stream: TFileStream; Offset, Size: Int64): DWord;
var
  Buffer: array[0..4095] of Byte;
  BytesToRead, BytesRead: Integer;
  TotalRead: Int64;
  I: Integer;
begin
  Result := 0;
  if (Stream = nil) or (Size <= 0) then Exit;

  Stream.Position := Offset;
  TotalRead := 0;

  while TotalRead < Size do
  begin
    BytesToRead := Min(SizeOf(Buffer), Size - TotalRead);
    BytesRead := Stream.Read(Buffer[0], BytesToRead);

    if BytesRead <= 0 then Break;

    for I := 0 to BytesRead - 1 do
      Result := Result + Buffer[I];

    TotalRead := TotalRead + BytesRead;
  end;
end;

// Parse RES file and extract resource information - FIXED VERSION with proper Language ID handling
function ResFile_Parse(Stream: TFileStream; out Resources: TResInfoArray): Boolean;
var
  DataSize, HeaderSize: DWord;
  TypeID, NameID: TResType;
  LanguageID: Word;
  ResourceCount: Integer;
  StartPos, DataPos, SavedPos: Int64;
  CurrentResource, I: Integer;
  DataVersion: DWord;
  HeaderRemaining: Int64;
  MemoryFlags: Word;
  VersionInfo: DWord;      // Cambiado a DWord
  Characteristics: DWord;
  Temp: DWord;  // <-- AÑADIR ESTA LÍNEA
  TypeNameSize: Integer;    // <-- AÑADIR ESTA
  PaddingBytes: Integer;    // <-- AÑADIR ESTA
begin
  Result := False;
  SetLength(Resources, 0);
  ResourceCount := 0;

  if Stream = nil then
  begin
    DebugError('ResFile_Parse: Stream is nil');
    Exit;
  end;

  Stream.Position := 0;

  DebugInfo('=== Parsing RES file ===');
  DebugInfo('File size: ' + IntToStr(Stream.Size) + ' bytes');

  while (Stream.Position + 8) <= Stream.Size do
  begin
    StartPos := Stream.Position;
    CurrentResource := ResourceCount + 1;

    DebugDetail('Parsing resource #' + IntToStr(CurrentResource) +
                ' at offset: ' + IntToStr(StartPos) +
                ' (0x' + IntToHex(StartPos, 8) + ')');

    try
      SavedPos := Stream.Position;

      // Leer DataSize y HeaderSize
      DataSize := ResFile_ReadDWord(Stream);
      HeaderSize := ResFile_ReadDWord(Stream);

      DebugDetail('  DataSize: ' + IntToStr(DataSize) +
                  ', HeaderSize: ' + IntToStr(HeaderSize));

      if (HeaderSize < 8) or (HeaderSize > 1024) then
      begin
        DebugDetail('  Invalid HeaderSize, skipping 4 bytes...');
        Stream.Position := SavedPos + 4;
        Continue;
      end;

      // Leer tipo y nombre
      TypeID := ResFile_ReadResType(Stream);
      NameID := ResFile_ReadResType(Stream);

      DebugDetail('  Type: ' + ResType_ToString(TypeID) +
                  ', Name: ' + ResType_ToString(NameID));
      // Después de leer Type y Name
      TypeNameSize := (Stream.Position - (StartPos + 8));
      DebugDetail('  Type+Name total size: ' + IntToStr(TypeNameSize) + ' bytes');

      // Calcular padding para alinear a DWORD
      if (TypeNameSize mod 4) <> 0 then
      begin
        PaddingBytes := 4 - (TypeNameSize mod 4);
        DebugDetail('  Padding needed after name: ' + IntToStr(PaddingBytes) + ' bytes');
        Stream.Position := Stream.Position + PaddingBytes;
      end;

      // Ahora sí, leer los campos fijos
      DataPos := StartPos + HeaderSize;
      HeaderRemaining := DataPos - Stream.Position;
      DebugDetail('  Header remaining after alignment: ' + IntToStr(HeaderRemaining) + ' bytes');

      DebugDetail('  Header remaining: ' + IntToStr(HeaderRemaining) + ' bytes');

      // Inicializar campos temporales
      DataVersion := 0;
      MemoryFlags := 0;
      LanguageID := 0;
      VersionInfo := 0;
      Characteristics := 0;

      // ===== LEER TODOS LOS CAMPOS EN ORDEN =====

      // 1. DataVersion (DWORD)
      if HeaderRemaining >= 4 then
      begin
        DataVersion := ResFile_ReadDWord(Stream);
        DebugDetail('  DataVersion: 0x' + IntToHex(DataVersion, 8));
        HeaderRemaining := HeaderRemaining - 4;
      end;

      // 2. MemoryFlags (WORD) - 2 bytes
      if HeaderRemaining >= 2 then
      begin
        MemoryFlags := ResFile_ReadWord(Stream);
        DebugDetail('  MemoryFlags: 0x' + IntToHex(MemoryFlags, 4));
        HeaderRemaining := HeaderRemaining - 2;
      end;

      // 3. LanguageID (WORD) - 2 bytes
      if HeaderRemaining >= 2 then
      begin
        LanguageID := ResFile_ReadWord(Stream);
        DebugDetail('  LanguageID: ' + IntToStr(LanguageID) +
                    ' (0x' + IntToHex(LanguageID, 4) + ')');
        HeaderRemaining := HeaderRemaining - 2;
      end;

      // 4. Version (DWORD) - 4 bytes (¡cambiado de WORD a DWORD!)
      if HeaderRemaining >= 4 then
      begin
        VersionInfo := ResFile_ReadDWord(Stream);
        DebugDetail('  Version: 0x' + IntToHex(VersionInfo, 8));
        HeaderRemaining := HeaderRemaining - 4;
      end;

      // 5. Characteristics (DWORD)
      if HeaderRemaining >= 4 then
      begin
        Characteristics := ResFile_ReadDWord(Stream);
        DebugDetail('  Characteristics: 0x' + IntToHex(Characteristics, 8));
        HeaderRemaining := HeaderRemaining - 4;
      end;
      // Después de leer Characteristics, antes de saltar padding
//      DebugDetail(Format('    RAW bytes read: DataVer=0x%.8x, MemFlags=0x%.4x, Lang=0x%.4x, Ver=0x%.8x, Char=0x%.8x', [DataVersion, MemoryFlags, LanguageID, VersionInfo, Characteristics]));
      if HeaderRemaining > 0 then
      begin
        DebugDetail('  Header remaining bytes: ' + IntToStr(HeaderRemaining));
        // Opcional: leer y mostrar los primeros bytes
        if HeaderRemaining >= 4 then
        begin
          Temp := ResFile_ReadDWord(Stream);
          DebugDetail('    First 4 remaining bytes: 0x' + IntToHex(Temp, 8));
          Stream.Position := Stream.Position - 4;  // Devolver el puntero
        end;
      end;
      // Saltar cualquier padding restante
      if HeaderRemaining > 0 then
      begin
        DebugDetail('  Skipping ' + IntToStr(HeaderRemaining) + ' padding bytes');
        Stream.Position := Stream.Position + HeaderRemaining;
      end;

      // Asegurar que estamos en DataPos
      if Stream.Position <> DataPos then
        Stream.Position := DataPos;

      // Validar posición de datos
      if (DataPos < 0) or (DataPos >= Stream.Size) or
         (DataPos + DataSize > Stream.Size) then
      begin
        DebugDetail('  Invalid DataPos, skipping resource...');
        Stream.Position := SavedPos + 4;
        Continue;
      end;

      // ===== AÑADIR EL RECURSO AL ARRAY =====
      SetLength(Resources, ResourceCount + 1);

      // Asignar nombre
      if NameID.IsInteger then
        Resources[ResourceCount].Name := IntToStr(NameID.IntValue)
      else
        Resources[ResourceCount].Name := NameID.StrValue;

      // Asignar todos los campos
      Resources[ResourceCount].ResType := TypeID;
      Resources[ResourceCount].Size := DataSize;
      Resources[ResourceCount].LanguageID := LanguageID;
      Resources[ResourceCount].DataOffset := DataPos;
      Resources[ResourceCount].DataSize := DataSize;
      Resources[ResourceCount].DataVersion := DataVersion;
      Resources[ResourceCount].MemoryFlags := MemoryFlags;
      Resources[ResourceCount].VersionInfo := VersionInfo;   // DWord
      Resources[ResourceCount].Characteristics := Characteristics;

      Inc(ResourceCount);

      DebugInfo('  ✓ Resource ' + IntToStr(CurrentResource) + ': ' +
                Resources[ResourceCount-1].Name +
                ' Type: ' + ResType_ToString(TypeID) +
                ' Size: ' + IntToStr(DataSize) + ' bytes' +
                ' Lang: ' + IntToStr(LanguageID) +
                ' DataVer: 0x' + IntToHex(DataVersion, 8) +
                ' MemFlags: 0x' + IntToHex(MemoryFlags, 4) +
                ' Ver: 0x' + IntToHex(VersionInfo, 8) +
                ' Char: 0x' + IntToHex(Characteristics, 8) +
                ' Offset: 0x' + IntToHex(DataPos, 8));

      // Mover al siguiente recurso y alinear
      Stream.Position := DataPos + DataSize;
      if (Stream.Position mod 4) <> 0 then
      begin
        Stream.Position := Stream.Position + (4 - (Stream.Position mod 4));
        DebugDetail('  Aligned to DWORD: 0x' + IntToHex(Stream.Position, 8));
      end;

    except
      on E: Exception do
      begin
        DebugError('Exception parsing resource #' + IntToStr(CurrentResource) +
                  ' at offset 0x' + IntToHex(StartPos, 8) + ': ' + E.Message);
        Stream.Position := SavedPos + 4;
      end;
    end;
  end;

  DebugInfo('=== Parsing complete ===');
  DebugInfo('Found ' + IntToStr(ResourceCount) + ' resources');

  Result := ResourceCount > 0;
end;

// Find resource by name and type
function ResFile_FindResource(Stream: TFileStream; const Resources: TResInfoArray;
  const Name: String; ResType: TResType; out ResInfo: TResInfo): Boolean;
var
  I: Integer;
begin
  Result := False;

  // Initialize output
  ResInfo.Name := '';
  ResInfo.ResType.IsInteger := False;
  ResInfo.ResType.StrValue := '';
  ResInfo.Size := 0;
  ResInfo.LanguageID := 0;
  ResInfo.DataOffset := 0;
  ResInfo.DataSize := 0;

  for I := 0 to High(Resources) do
  begin
    if SameText(Resources[I].Name, Name) and
       ResType_AreEqual(Resources[I].ResType, ResType) then
    begin
      ResInfo := Resources[I];
      Result := True;
      Exit;
    end;
  end;
end;

// Load resource data into memory stream
function ResFile_LoadResource(Stream: TFileStream; const ResInfo: TResInfo): TMemoryStream;
begin
  Result := nil;

  if (Stream = nil) or (ResInfo.DataSize = 0) then Exit;

  try
    Result := TMemoryStream.Create;
    Stream.Position := ResInfo.DataOffset;
    Result.CopyFrom(Stream, ResInfo.DataSize);
    Result.Position := 0;
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      DebugError('Error loading resource: ' + E.Message);
    end;
  end;
end;

// Load resource by index
function ResFile_LoadResourceByIndex(Stream: TFileStream; const Resources: TResInfoArray;
  Index: Integer): TMemoryStream;
begin
  Result := nil;

  if (Stream = nil) or (Index < 0) or (Index > High(Resources)) then Exit;

  Result := ResFile_LoadResource(Stream, Resources[Index]);
end;

// Debug: Dump resource info
procedure ResFile_DumpResourceInfo(const Info: TResInfo);
begin
  DebugInfo('Resource Info:');
  DebugInfo('  Name: ' + Info.Name);
  DebugInfo('  Type: ' + ResType_ToString(Info.ResType));
  DebugInfo('  Size: ' + IntToStr(Info.Size) + ' bytes');
  DebugInfo('  LanguageID: ' + IntToStr(Info.LanguageID));
  DebugInfo('  DataOffset: ' + IntToStr(Info.DataOffset));
  DebugInfo('  DataSize: ' + IntToStr(Info.DataSize));
end;

// Debug: Dump all resources
procedure ResFile_DumpAllResources(const Resources: TResInfoArray);
var
  I: Integer;
begin
  DebugInfo('=== All Resources ===');
  DebugInfo('Count: ' + IntToStr(Length(Resources)));

  for I := 0 to High(Resources) do
  begin
    DebugInfo(Format('%3d: %-20s Type: %-15s Size: %8d Lang: %4d Offset: %8d',
      [I + 1, Resources[I].Name, ResType_ToString(Resources[I].ResType),
       Resources[I].Size, Resources[I].LanguageID, Resources[I].DataOffset]));
  end;
end;

end.
