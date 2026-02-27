(*

We pass to loadresourcebyname an array.
The first element is always the resource name and it's mandatory.
The second is the resource type, can be empty, integer or string.
The third is the multiimage options. "All", "Bigger", "Smaller", "First", "Last" or "Selection".
Selection must be followed by a list of indexes, we assume that the user knows what resources is searching for and if they dont exist we just inform of the issues found.
Then the caller functions gathers the multiresurgce resmanager returns and organizes it by checking the mimes the manager asociated with each element

Main functions to use:
ResMgr_LoadFile(const FileName: string): boolean; Load a file and parses into   TResInfoArray, ResMgr_FileName and TResLoadResult

ResMgr_LoadResource(const Params: array of const): TMultiResLoadResult; Parses an array of parameters and return the resulting resources in an array of resources with associated records

*)
unit uResManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  uResDefs, uResFile, uMimeDetect, uDebug, uResUtils;

var
  ResMgr_FileStream: TFileStream = nil;
  ResMgr_Resources: TResInfoArray;
  ResMgr_ResourceCount: integer = 0;
  ResMgr_FileName: string;  // Ya debería estar

// Mhaging resource names and info

function ResMgr_GetResourceCount: integer;
function ResMgr_GetResourceName(Index: integer): string;

function FilterMultiResult(const Source: TMultiResLoadResult; Mode: TMultiResourceMode; const Indices: array of integer): TMultiResLoadResult;
// Load resource with multi-mode support

// Helper to create load params
function ResMgr_CreateParams(const Name: string; ResType: TResType; Language: word = 0): TLoadParams;

// Set multi-mode
procedure ResMgr_SetMultiMode(var Params: TLoadParams; Mode: TMultiResMode; Index: integer = -1);

// Core functions - called from MainForm
procedure ResMgr_Init;
procedure ResMgr_Done;

function ResMgr_LoadFile(const FileName: string): boolean;

function ResMgr_LoadResource(const Params: array of const): TMultiResLoadResult;


// Resource listing - called from MainForm
function IsValidResource(const Info: TResInfo): boolean;
function ResMgr_GetAllResources: TResListArray;
function ResMgr_GetTextResources: TResListArray;
function ResMgr_GetImageResources: TResListArray;

// Error handling
function ResMgr_GetErrorString: string;

// Internal helper for icon groups (used by ResMgr_LoadResourceByInfo)
function ResMgr_ReconstructIconGroup(const GroupInfo: TResInfo; const GroupData: TMemoryStream): TMemoryStream;
function ResMgr_LoadGroupIcon(const GroupName: string): TMultiResLoadResult;
function ResMgr_LoadRawResource(const Params: array of const): TMemoryStream;

// wrappers
function ResUtils_IsImage(const Res: TResLoadResult): boolean;
function ResUtils_IsText(const Res: TResLoadResult): boolean;

procedure ExtractIconImages(ICOStream: TMemoryStream; var ImageStreams: TMemoryStreamArray; var ImageMimeTypes: TMimeTypeArray; var ImageDimensions: TStringArray);

procedure AddToResult(var Result: TMultiResLoadResult; const LoadResult: TResLoadResult; Index: integer);
function VarRecAsString(const V: TVarRec): string;

procedure ReduceToSingleItem(var Result: TMultiResLoadResult; const SelectedIndex: integer);

implementation

var
  LastError: string = '';

  //------------------------------------------------------------------------------
  // Initialization
  //------------------------------------------------------------------------------

procedure ResMgr_Init;
begin
  ResMgr_FileStream := nil;
  SetLength(ResMgr_Resources, 0);
  ResMgr_ResourceCount := 0;
  LastError := '';
end;

procedure ResMgr_Done;
var
  I: Integer;
begin
  // Liberar streams de recursos en memoria
  for I := 0 to ResMgr_ResourceCount - 1 do
    if ResMgr_Resources[I].Data <> nil then
      ResMgr_Resources[I].Data.Free;

  if ResMgr_FileStream <> nil then
  begin
    ResFile_Close(ResMgr_FileStream);
    ResMgr_FileStream := nil;
  end;
  SetLength(ResMgr_Resources, 0);
  ResMgr_ResourceCount := 0;
end;

// Helper para reducir un multi-resultado a un solo item

procedure ReduceToSingleItem(var Result: TMultiResLoadResult; const SelectedIndex: integer);
var
  SourceIndex: integer;
begin
  if (Result.Count = 0) or (SelectedIndex < 0) or (SelectedIndex >= Result.Count) then
  begin
    // Si no hay items válidos, vaciar todo
    SetLength(Result.Items, 0);
    SetLength(Result.ImageStreams, 0);
    SetLength(Result.ImageMimeTypes, 0);
    SetLength(Result.ImageDimensions, 0);
    SetLength(Result.ImageCategories, 0);
    Result.Count := 0;
    Exit;
  end;

  SourceIndex := SelectedIndex;

  // Mover el item seleccionado a la primera posición si es necesario
  if SourceIndex <> 0 then
  begin
    Result.Items[0] := Result.Items[SourceIndex];

    if Length(Result.ImageStreams) > 0 then
    begin
      Result.ImageStreams[0] := Result.ImageStreams[SourceIndex];
      Result.ImageMimeTypes[0] := Result.ImageMimeTypes[SourceIndex];
      Result.ImageDimensions[0] := Result.ImageDimensions[SourceIndex];
      Result.ImageCategories[0] := Result.ImageCategories[SourceIndex];
    end;
  end;

  // Redimensionar todos los arrays a 1
  SetLength(Result.Items, 1);

  if Length(Result.ImageStreams) > 0 then
  begin
    SetLength(Result.ImageStreams, 1);
    SetLength(Result.ImageMimeTypes, 1);
    SetLength(Result.ImageDimensions, 1);
    SetLength(Result.ImageCategories, 1);
  end;

  Result.Count := 1;
end;

function ResMgr_CreateParams(const Name: string; ResType: TResType; Language: word = 0): TLoadParams;
begin
  Result.Name := Name;
  Result.ResType := ResType;
  Result.Language := Language;
  Result.MultiMode := mrmCombined;  // Default to combined
  Result.SelectedIndex := -1;
  Result.MinSize := 0;
  Result.MaxSize := 0;
  Result.PreferredFormat := '';
end;

procedure ResMgr_SetMultiMode(var Params: TLoadParams; Mode: TMultiResMode; Index: integer = -1);
begin
  Params.MultiMode := Mode;
  Params.SelectedIndex := Index;
end;

//------------------------------------------------------------------------------
// File Loading
//------------------------------------------------------------------------------

function ResMgr_LoadFile(const FileName: string): boolean;
begin
  Result := False;
  LastError := '';

  // Unload previous
  if ResMgr_FileStream <> nil then
    ResFile_Close(ResMgr_FileStream);

  DebugInfo('Loading resource file: ' + FileName);

  try
    ResMgr_FileStream := ResFile_Open(FileName);
    if ResMgr_FileStream = nil then
    begin
      LastError := 'Cannot open file';
      Exit;
    end;

    if not ResFile_Parse(ResMgr_FileStream, ResMgr_Resources) then
    begin
      LastError := 'Failed to parse resource file';
      ResFile_Close(ResMgr_FileStream);
      Exit;
    end;

    ResMgr_ResourceCount := Length(ResMgr_Resources);
    Result := True;
    ResMgr_FileName := FileName;
    DebugInfo('Successfully loaded ' + IntToStr(ResMgr_ResourceCount) + ' resources');

  except
    on E: Exception do
    begin
      LastError := E.Message;
      DebugError('Error loading file: ' + E.Message);
      if ResMgr_FileStream <> nil then
        ResFile_Close(ResMgr_FileStream);
      Result := False;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Resource Loading
//------------------------------------------------------------------------------

// In ResMgr_LoadResourceByInfo, simplify to:

//------------------------------------------------------------------------------
// Resource Loading - Now just calls ResUtils
//------------------------------------------------------------------------------

function ResMgr_LoadResourceByInfo(const Info: TResInfo): TResLoadResult;
var
  Stream: TMemoryStream;
  ReconstructedStream: TMemoryStream;
  CategoryInfo: TResourceCategoryInfo;
var
  Header: array[0..5] of byte;
begin
  // Inicializar
  Result.Success := False;
  Result.Data := nil;
  Result.Size := 0;
  Result.MimeType := mtUnknown;
  Result.MimeTypeName := '';
  Result.ResourceName := Info.Name;  // <-- ASIGNAR EL NOMBRE
  Result.ResourceType := Info.ResType;
  Result.ResourceTypeName := ResType_ToDisplayString(Info.ResType);
  Result.ResourceCategory := rcUnknown;  // Inicializar
  Result.ResourceCategoryName := '';
  Result.CanDisplay := True;
  Result.DefaultView := 'hex';
  Result.LanguageID := Info.LanguageID;
  Result.ResourceIndex := -1;
  Result.DataOffset := Info.DataOffset;
  Result.ErrorMessage := '';

  if ResMgr_FileStream = nil then
  begin
    Result.ErrorMessage := 'No resource file loaded';
    LastError := Result.ErrorMessage;
    Exit;
  end;

  if Info.Data <> nil then
  begin
    DebugInfo('[ResMgr] Cargando desde memoria');
    Stream := TMemoryStream.Create;
    Info.Data.Position := 0;
    Stream.CopyFrom(Info.Data, Info.Data.Size);
    Stream.Position := 0;
  end
  else
  begin
    DebugInfo('[ResMgr] Cargando desde archivo');
    Stream := ResFile_LoadResource(ResMgr_FileStream, Info);
  end;

  if Stream = nil then
  begin
    Result.ErrorMessage := 'No se pudo cargar el recurso';
    Exit;
  end;

  // Aplicar reconstrucción según el tipo
  if Info.ResType.IsInteger then
  begin
    case Info.ResType.IntValue of
      2: // RT_BITMAP
      begin
        DebugInfo('  RT_BITMAP -> ResUtils_ReconstructBitmap');
        ReconstructedStream := ResUtils_ReconstructBitmap(Stream);
        if ReconstructedStream <> nil then
        begin
          Stream.Free;
          Stream := ReconstructedStream;
          Result.MimeType := mtBitmap;
        end;
      end;
      3:
      begin
        DebugInfo('  RT_ICON -> ResUtils_ReconstructIcon');
        ReconstructedStream := ResUtils_ReconstructIcon(Stream, Info.ResType);
        if ReconstructedStream <> nil then
        begin
          Stream.Free;
          Stream := ReconstructedStream;
          Result.MimeType := mtIcon;
        end;
      end;
      21, 22: // RT_ICON, RT_ANICURSOR, RT_ANIICON
      begin
        DebugInfo('  RT_ANI -> ResUtils_ReconstructIcon');
        ReconstructedStream := ResUtils_ReconstructIcon(Stream, Info.ResType);
        if ReconstructedStream <> nil then
        begin
          Stream.Free;
          Stream := ReconstructedStream;
          Result.MimeType := mtAni;
        end;
      end;

      14: // RT_GROUP_ICON
      begin
        DebugInfo('  RT_GROUP_ICON -> ResMgr_ReconstructIconGroup');
        ReconstructedStream := ResMgr_ReconstructIconGroup(Info, Stream);
        if ReconstructedStream <> nil then
        begin
          Stream.Free;
          Stream := ReconstructedStream;
          Result.MimeType := mtIcon;
        end;
      end;

      1: // RT_CURSOR
      begin
        DebugInfo('  CURSOR resource detected');
        ReconstructedStream := ResUtils_ReconstructCursor(Stream, Info.ResType);
        if ReconstructedStream <> nil then
        begin
          Stream.Free;
          Stream := ReconstructedStream;
          Result.MimeType := mtCursor;
          // DEBUG: Verificar los primeros bytes del grupo reconstruido
          if Stream.Size >= 6 then
          begin
            Stream.Position := 0;
            Stream.Read(Header, 6);
            Stream.Position := 0;
            DebugInfo(Format('  Reconstructed CURSOR starts: %.2x %.2x %.2x %.2x %.2x %.2x', [Header[0], Header[1], Header[2], Header[3], Header[4], Header[5]]));
          end;
        end;
      end;


      12: // RT_GROUP_CURSOR
      begin
        DebugInfo('  RT_GROUP_CURSOR -> ResMgr_ReconstructIconGroup');
        ReconstructedStream := ResMgr_ReconstructIconGroup(Info, Stream);
        if ReconstructedStream <> nil then
        begin
          Stream.Free;
          Stream := ReconstructedStream;
          Result.MimeType := mtCursor;
          if Stream.Size >= 6 then
          begin
            Stream.Position := 0;
            Stream.Read(Header, 6);
            Stream.Position := 0;
            DebugInfo(Format('  Reconstructed CURSOR starts: %.2x %.2x %.2x %.2x %.2x %.2x', [Header[0], Header[1], Header[2], Header[3], Header[4], Header[5]]));
          end;
        end;
      end;


      6, 11, 16, 23, 24: // Text resources
      begin
        DebugInfo('  Text resource -> ResUtils_ReconstructText');
        ReconstructedStream := ResUtils_ReconstructText(Stream, Info.ResType);
        if ReconstructedStream <> nil then
        begin
          Stream.Free;
          Stream := ReconstructedStream;
        end;
      end;

      else
        DebugInfo('  No reconstruction needed');
    end;
  end
  else
  begin
    // String resource types
    if SameText(Info.ResType.StrValue, 'BITMAP') or SameText(Info.ResType.StrValue, 'BMP') then
    begin
      DebugInfo('  String BITMAP -> ResUtils_ReconstructBitmap');
      ReconstructedStream := ResUtils_ReconstructBitmap(Stream);
      if ReconstructedStream <> nil then
      begin
        Stream.Free;
        Stream := ReconstructedStream;
        Result.MimeType := mtBitmap;
      end;
    end
    else if SameText(Info.ResType.StrValue, 'CURSOR') or SameText(Info.ResType.StrValue, 'CUR') then
    begin
      DebugInfo('  String ICON -> ResUtils_ReconstructIcon');
      ReconstructedStream := ResUtils_ReconstructCursor(Stream, Info.ResType);
      if ReconstructedStream <> nil then
      begin
        Stream.Free;
        Stream := ReconstructedStream;
        Result.MimeType := mtCursor;
      end;
    end

    else if SameText(Info.ResType.StrValue, 'ICON') or SameText(Info.ResType.StrValue, 'ICO') then
    begin
      DebugInfo('  String ICON -> ResUtils_ReconstructIcon');
      ReconstructedStream := ResUtils_ReconstructIcon(Stream, Info.ResType);
      if ReconstructedStream <> nil then
      begin
        Stream.Free;
        Stream := ReconstructedStream;
        Result.MimeType := mtIcon;
      end;
    end
    else if SameText(Info.ResType.StrValue, 'PNG') then
    begin
      DebugInfo('  String PNG -> no reconstruction needed');
      Result.MimeType := mtPng;
    end
    else if SameText(Info.ResType.StrValue, 'JPG') or SameText(Info.ResType.StrValue, 'JPEG') then
    begin
      DebugInfo('  String JPEG -> no reconstruction needed');
      Result.MimeType := mtJpeg;
    end
    else if SameText(Info.ResType.StrValue, 'GIF') then
    begin
      DebugInfo('  String GIF -> no reconstruction needed');
      Result.MimeType := mtGif;
    end
    else if SameText(Info.ResType.StrValue, 'TEXT') or SameText(Info.ResType.StrValue, 'HTML') or SameText(Info.ResType.StrValue, 'XML') or SameText(Info.ResType.StrValue, 'JSON') or
      SameText(Info.ResType.StrValue, 'CSS') or SameText(Info.ResType.StrValue, 'JS') then
    begin
      DebugInfo('  String TEXT -> ResUtils_ReconstructText');
      ReconstructedStream := ResUtils_ReconstructText(Stream, Info.ResType);
      if ReconstructedStream <> nil then
      begin
        Stream.Free;
        Stream := ReconstructedStream;
      end;
    end;
  end;

  // Detectar MIME type si no se asignó antes
  if Result.MimeType = mtUnknown then
  begin
    Result.MimeType := TMimeDetector.DetectFromStream(Stream);
    DebugInfo('  Detected MIME: ' + TMimeDetector.TypeToString(Result.MimeType));
  end;

  Result.MimeTypeName := TMimeDetector.TypeToString(Result.MimeType);

  // ========================================================================
  // ¡¡¡AQUÍ ESTABA EL ERROR!!! - FALTABA ASIGNAR LA CATEGORÍA
  // ========================================================================

  // Determinar categoría basada en el MIME type
  case Result.MimeType of
    mtText, mtHtml, mtXml, mtJson, mtCss, mtJavaScript, mtRtf:
      Result.ResourceCategory := rcText;

    mtBitmap, mtIcon, mtJpeg, mtPng, mtGif, mtAni, mtCursor:
      Result.ResourceCategory := rcImage;

    mtWave, mtMp3:
      Result.ResourceCategory := rcAudio;

    mtAvi:
      Result.ResourceCategory := rcVideo;

    mtExecutable:
      Result.ResourceCategory := rcExecutable;

    mtArchive:
      Result.ResourceCategory := rcArchive;

    mtFont:
      Result.ResourceCategory := rcFont;

    mtBinary:
      Result.ResourceCategory := rcBinary;

    else
      Result.ResourceCategory := rcUnknown;
  end;

  // Obtener información de categoría
  CategoryInfo := GetCategoryInfo(Result.ResourceCategory);
  Result.ResourceCategoryName := CategoryInfo.Name;
  Result.CanDisplay := CategoryInfo.CanDisplay;
  Result.DefaultView := CategoryInfo.DefaultView;

  Result.Data := Stream;
  Result.Size := Stream.Size;
  Result.Success := True;

  DebugInfo(Format('  Loaded: %s, Category: %s, View: %s, %d bytes', [Result.MimeTypeName, Result.ResourceCategoryName, Result.DefaultView, Result.Size]));
end;

function ResMgr_LoadResourceByIndex(Index: integer): TResLoadResult;
begin
  if (Index < 0) or (Index >= ResMgr_ResourceCount) then
  begin
    Result.Success := False;
    Result.ErrorMessage := 'Resource index out of range';
    LastError := Result.ErrorMessage;
    Exit;
  end;
  Result := ResMgr_LoadResourceByInfo(ResMgr_Resources[Index]);
  Result.ResourceIndex := Index;
end;

//------------------------------------------------------------------------------
// Cargar datos RAW del recurso (sin reconstrucción)
// Parámetros: array of const con el siguiente formato:
//   [modo, parámetros...]

// Modos:
//   0: Por índice - [0, Index]
//   1: Por nombre y tipo entero - [1, 'Nombre', TipoInteger]
//   2: Por nombre y tipo string - [2, 'Nombre', 'TipoString']
//   3: Por nombre y tipo (cualquiera) - [3, 'Nombre', ResType]
//------------------------------------------------------------------------------
function ResMgr_LoadRawResource(const Params: array of const): TMemoryStream;
var
  Mode: integer;
  I: integer;
  SearchIndex: integer;
  SearchName: string;
  SearchTypeInt: integer;
  SearchTypeStr: string;
  SearchType: TResType;
  Found: boolean;
begin
  Result := nil;
  LastError := '';

  if Length(Params) = 0 then
  begin
    LastError := 'No parameters specified';
    Exit;
  end;

  // El primer parámetro es el modo
  if Params[0].VType <> vtInteger then
  begin
    LastError := 'First parameter must be mode (integer)';
    Exit;
  end;

  Mode := Params[0].VInteger;

  // Verificar que hay suficientes parámetros para cada modo
  case Mode of
    0: // Por índice
    begin
      if Length(Params) < 2 then
      begin
        LastError := 'Index mode requires index parameter';
        Exit;
      end;
      if Params[1].VType <> vtInteger then
      begin
        LastError := 'Index must be integer';
        Exit;
      end;

      SearchIndex := Params[1].VInteger;

      if (SearchIndex < 0) or (SearchIndex >= ResMgr_ResourceCount) then
      begin
        LastError := Format('Index %d out of range', [SearchIndex]);
        Exit;
      end;
      if ResMgr_Resources[SearchIndex].Data <> nil then
      begin
        Result := TMemoryStream.Create;
        ResMgr_Resources[SearchIndex].Data.Position := 0;
        Result.CopyFrom(ResMgr_Resources[SearchIndex].Data, ResMgr_Resources[SearchIndex].Data.Size);
        Result.Position := 0;
        Exit;
      end;
      Result := ResFile_LoadResource(ResMgr_FileStream, ResMgr_Resources[SearchIndex]);
      if Result = nil then
        LastError := 'Failed to load resource data';
    end;

    1: // Por nombre y tipo entero
    begin
      if Length(Params) < 3 then
      begin
        LastError := 'Name+IntType mode requires name and type parameters';
        Exit;
      end;

      SearchName := VarRecAsString(Params[1]);
      if Params[2].VType <> vtInteger then
      begin
        LastError := 'Type must be integer';
        Exit;
      end;
      SearchTypeInt := Params[2].VInteger;

      Found := False;
      for I := 0 to ResMgr_ResourceCount - 1 do
      begin
        if (ResMgr_Resources[I].Name = SearchName) and ResMgr_Resources[I].ResType.IsInteger and (ResMgr_Resources[I].ResType.IntValue = SearchTypeInt) then
        begin
          Result := ResFile_LoadResource(ResMgr_FileStream, ResMgr_Resources[I]);
          Found := True;
          Break;
        end;
      end;

      if not Found then
        LastError := Format('Resource not found: "%s" (type %d)', [SearchName, SearchTypeInt]);
    end;

    2: // Por nombre y tipo string
    begin
      if Length(Params) < 3 then
      begin
        LastError := 'Name+StrType mode requires name and type parameters';
        Exit;
      end;

      SearchName := VarRecAsString(Params[1]);
      SearchTypeStr := UpperCase(VarRecAsString(Params[2]));

      Found := False;
      for I := 0 to ResMgr_ResourceCount - 1 do
      begin
        if (ResMgr_Resources[I].Name = SearchName) and not ResMgr_Resources[I].ResType.IsInteger and (UpperCase(ResMgr_Resources[I].ResType.StrValue) = SearchTypeStr) then
        begin
          Result := ResFile_LoadResource(ResMgr_FileStream, ResMgr_Resources[I]);
          Found := True;
          Break;
        end;
      end;

      if not Found then
        LastError := Format('Resource not found: "%s" (type %s)', [SearchName, SearchTypeStr]);
    end;

    3: // Por nombre y TResType
    begin
      if Length(Params) < 3 then
      begin
        LastError := 'Name+ResType mode requires name and type parameters';
        Exit;
      end;

      SearchName := VarRecAsString(Params[1]);

      // El tercer parámetro puede ser entero o string
      if Params[2].VType = vtInteger then
      begin
        SearchType.IsInteger := True;
        SearchType.IntValue := Params[2].VInteger;
      end
      else
      begin
        SearchType.IsInteger := False;
        SearchType.StrValue := UpperCase(VarRecAsString(Params[2]));
      end;

      Found := False;
      for I := 0 to ResMgr_ResourceCount - 1 do
      begin
        if (ResMgr_Resources[I].Name = SearchName) and ResType_AreEqual(ResMgr_Resources[I].ResType, SearchType) then
        begin
          Result := ResFile_LoadResource(ResMgr_FileStream, ResMgr_Resources[I]);
          Found := True;
          Break;
        end;
      end;

      if not Found then
      begin
        if SearchType.IsInteger then
          LastError := Format('Resource not found: "%s" (type %d)', [SearchName, SearchType.IntValue])
        else
          LastError := Format('Resource not found: "%s" (type %s)', [SearchName, SearchType.StrValue]);
      end;
    end;

    else
      LastError := Format('Invalid mode: %d', [Mode]);
  end;

  if (Result <> nil) and (Result.Size = 0) then
  begin
    Result.Free;
    Result := nil;
    LastError := 'Resource has zero size';
  end;
end;

// Main entry point - called from MainForm comboboxes and tree

//------------------------------------------------------------------------------
// Filter multi-result based on mode and indices
//------------------------------------------------------------------------------

function FilterMultiResult(const Source: TMultiResLoadResult; Mode: TMultiResourceMode; const Indices: array of integer): TMultiResLoadResult;
var
  I: integer;
  BestIndex: integer;
  LargestSize: int64;
  SmallestSize: int64;
begin
  // Initialize result
  Result.Success := Source.Success;
  Result.Count := 0;
  SetLength(Result.Items, 0);
  Result.Combined := Source.Combined;
  SetLength(Result.ImageStreams, 0);
  SetLength(Result.ImageMimeTypes, 0);
  SetLength(Result.ImageDimensions, 0);
  SetLength(Result.ImageCategories, 0);  // <-- AÑADIR
  Result.ErrorMessage := Source.ErrorMessage;

  if Source.Count = 0 then Exit;

  case Mode of
    mrmAll:
    begin
      // Return all images
      Result.Count := Source.Count;
      SetLength(Result.ImageStreams, Source.Count);
      SetLength(Result.ImageMimeTypes, Source.Count);
      SetLength(Result.ImageDimensions, Source.Count);
      SetLength(Result.ImageCategories, Source.Count);  // <-- AÑADIR
      SetLength(Result.Items, Source.Count);

      for I := 0 to Source.Count - 1 do
      begin
        Result.ImageStreams[I] := Source.ImageStreams[I];
        Result.ImageMimeTypes[I] := Source.ImageMimeTypes[I];
        Result.ImageDimensions[I] := Source.ImageDimensions[I];
        Result.ImageCategories[I] := Source.ImageCategories[I];  // <-- AÑADIR
        Result.Items[I] := Source.Items[I];
      end;
    end;

    mrmFirst:
    begin
      if Source.Count > 0 then
      begin
        SetLength(Result.ImageStreams, 1);
        SetLength(Result.ImageMimeTypes, 1);
        SetLength(Result.ImageDimensions, 1);
        SetLength(Result.ImageCategories, 1);  // <-- AÑADIR
        SetLength(Result.Items, 1);

        Result.ImageStreams[0] := Source.ImageStreams[0];
        Result.ImageMimeTypes[0] := Source.ImageMimeTypes[0];
        Result.ImageDimensions[0] := Source.ImageDimensions[0];
        Result.ImageCategories[0] := Source.ImageCategories[0];  // <-- AÑADIR
        Result.Items[0] := Source.Items[0];
        Result.Count := 1;
      end;
    end;

    mrmLast:
    begin
      if Source.Count > 0 then
      begin
        SetLength(Result.ImageStreams, 1);
        SetLength(Result.ImageMimeTypes, 1);
        SetLength(Result.ImageDimensions, 1);
        SetLength(Result.ImageCategories, 1);  // <-- AÑADIR
        SetLength(Result.Items, 1);

        Result.ImageStreams[0] := Source.ImageStreams[Source.Count - 1];
        Result.ImageMimeTypes[0] := Source.ImageMimeTypes[Source.Count - 1];
        Result.ImageDimensions[0] := Source.ImageDimensions[Source.Count - 1];
        Result.ImageCategories[0] := Source.ImageCategories[Source.Count - 1];  // <-- AÑADIR
        Result.Items[0] := Source.Items[Source.Count - 1];
        Result.Count := 1;
      end;
    end;

    mrmLargest:
    begin
      BestIndex := 0;
      LargestSize := Source.ImageStreams[0].Size;

      for I := 1 to Source.Count - 1 do
      begin
        if Source.ImageStreams[I].Size > LargestSize then
        begin
          LargestSize := Source.ImageStreams[I].Size;
          BestIndex := I;
        end;
      end;

      SetLength(Result.ImageStreams, 1);
      SetLength(Result.ImageMimeTypes, 1);
      SetLength(Result.ImageDimensions, 1);
      SetLength(Result.ImageCategories, 1);  // <-- AÑADIR
      SetLength(Result.Items, 1);

      Result.ImageStreams[0] := Source.ImageStreams[BestIndex];
      Result.ImageMimeTypes[0] := Source.ImageMimeTypes[BestIndex];
      Result.ImageDimensions[0] := Source.ImageDimensions[BestIndex];
      Result.ImageCategories[0] := Source.ImageCategories[BestIndex];  // <-- AÑADIR
      Result.Items[0] := Source.Items[BestIndex];
      Result.Count := 1;
    end;

    mrmSmallest:
    begin
      BestIndex := 0;
      SmallestSize := Source.ImageStreams[0].Size;

      for I := 1 to Source.Count - 1 do
      begin
        if Source.ImageStreams[I].Size < SmallestSize then
        begin
          SmallestSize := Source.ImageStreams[I].Size;
          BestIndex := I;
        end;
      end;

      SetLength(Result.ImageStreams, 1);
      SetLength(Result.ImageMimeTypes, 1);
      SetLength(Result.ImageDimensions, 1);
      SetLength(Result.ImageCategories, 1);  // <-- AÑADIR
      SetLength(Result.Items, 1);

      Result.ImageStreams[0] := Source.ImageStreams[BestIndex];
      Result.ImageMimeTypes[0] := Source.ImageMimeTypes[BestIndex];
      Result.ImageDimensions[0] := Source.ImageDimensions[BestIndex];
      Result.ImageCategories[0] := Source.ImageCategories[BestIndex];  // <-- AÑADIR
      Result.Items[0] := Source.Items[BestIndex];
      Result.Count := 1;
    end;

    mrmSelection:
    begin
      if Length(Indices) = 0 then Exit;

      SetLength(Result.ImageStreams, Length(Indices));
      SetLength(Result.ImageMimeTypes, Length(Indices));
      SetLength(Result.ImageDimensions, Length(Indices));
      SetLength(Result.ImageCategories, Length(Indices));  // <-- AÑADIR
      SetLength(Result.Items, Length(Indices));

      for I := 0 to High(Indices) do
      begin
        if (Indices[I] >= 0) and (Indices[I] < Source.Count) then
        begin
          Result.ImageStreams[I] := Source.ImageStreams[Indices[I]];
          Result.ImageMimeTypes[I] := Source.ImageMimeTypes[Indices[I]];
          Result.ImageDimensions[I] := Source.ImageDimensions[Indices[I]];
          Result.ImageCategories[0] := Source.ImageCategories[Indices[I]];  // <-- AÑADIR
          Result.Items[I] := Source.Items[Indices[I]];
        end;
      end;
      Result.Count := Length(Indices);
    end;

    else
      // For other modes (mrmBest, mrmText, etc.), just return all
      Result := FilterMultiResult(Source, mrmAll, []);
  end;
end;

//------------------------------------------------------------------------------
// Unified multi-resource loader - array parameters only
// Format: [Name, ResType, Mode, Format, Language, Indices...]
//------------------------------------------------------------------------------

function ResMgr_LoadResource(const Params: array of const): TMultiResLoadResult;
var
  I, J: integer;
  ResourceName: string;
  ResTypeStr: string;
  Mode: TMultiResourceMode;
  FormatPref: string;
  Lang: word;
  SelectedIndices: array of integer;
  ResourceIndices: array of integer;
  Info: TResInfo;
  LoadResult: TResLoadResult;
  BestIndex: integer;
  BestScore: integer;
  CurrentScore: integer;
  FormatUpper: string;
  MimeInfo: TFileFormatInfo;
  ParseState: integer; // 0=name, 1=type, 2=mode, 3=format, 4=lang, 5+=indices
  IsGroupResource: boolean;
  GroupInfo: TResInfo;
  GroupInfoIndex: integer;
  GroupResult: TResLoadResult;
  LargestSize: int64;
  SmallestSize: int64;
  CategoryInfo: TResourceCategoryInfo;
  ImageIndex: integer;
  // Nuevas variables para manejo de grupos
  RawGroupStream: TMemoryStream;
  ReconstructedGroupStream: TMemoryStream;
begin
  // -------------------------------------------------------------------------
  // 1. INICIALIZAR RESULTADO
  // -------------------------------------------------------------------------
  Result.Success := False;
  Result.Count := 0;
  SetLength(Result.Items, 0);
  Result.Combined := nil;
  SetLength(Result.ImageStreams, 0);
  SetLength(Result.ImageMimeTypes, 0);
  SetLength(Result.ImageDimensions, 0);
  SetLength(Result.ImageCategories, 0);
  Result.ErrorMessage := '';

  // -------------------------------------------------------------------------
  // 2. INICIALIZAR VARIABLES Y PARSEAR PARÁMETROS
  // -------------------------------------------------------------------------
  ResourceName := '';
  ResTypeStr := '';
  Mode := mrmAll;  // Default
  FormatPref := '';
  Lang := 0;
  SetLength(SelectedIndices, 0);
  ParseState := 0;

  for I := 0 to High(Params) do
  begin
    case Params[I].VType of
      vtString:
      begin
        case ParseState of
          0: ResourceName := string(Params[I].VString^);
          1: ResTypeStr := string(Params[I].VString^);
          3: FormatPref := string(Params[I].VString^);
        end;
        Inc(ParseState);
      end;

      vtAnsiString:
      begin
        case ParseState of
          0: ResourceName := string(Params[I].VAnsiString);
          1: ResTypeStr := string(Params[I].VAnsiString);
          3: FormatPref := string(Params[I].VAnsiString);
        end;
        Inc(ParseState);
      end;

      vtPChar:
      begin
        case ParseState of
          0: ResourceName := StrPas(Params[I].VPChar);
          1: ResTypeStr := StrPas(Params[I].VPChar);
          3: FormatPref := StrPas(Params[I].VPChar);
        end;
        Inc(ParseState);
      end;

      vtInteger:
      begin
        case ParseState of
          2: Mode := TMultiResourceMode(Params[I].VInteger);
          4: Lang := Params[I].VInteger;
          else
            // Add to selected indices
            SetLength(SelectedIndices, Length(SelectedIndices) + 1);
            SelectedIndices[High(SelectedIndices)] := Params[I].VInteger;
        end;
        if ParseState < 5 then
          Inc(ParseState);
      end;
    end;
  end;

  // Validar parámetros básicos
  if ResourceName = '' then
  begin
    Result.ErrorMessage := 'No resource name specified';
    LastError := Result.ErrorMessage;
    Exit;
  end;

  if ResMgr_FileStream = nil then
  begin
    Result.ErrorMessage := 'No resource file loaded';
    LastError := Result.ErrorMessage;
    Exit;
  end;

  FormatUpper := UpperCase(FormatPref);

  DebugInfo(Format('ResMgr_LoadResource: "%s", Type="%s", Mode=%d, Format="%s", Lang=%d', [ResourceName, ResTypeStr, Ord(Mode), FormatPref, Lang]));

  // -------------------------------------------------------------------------
  // 3. DETECTAR SI ES UN RECURSO DE GRUPO (GROUP_ICON o GROUP_CURSOR)
  // -------------------------------------------------------------------------
  IsGroupResource := False;
  GroupInfoIndex := -1;

  for I := 0 to ResMgr_ResourceCount - 1 do
  begin
    Info := ResMgr_Resources[I];
    if (Info.Name = ResourceName) and Info.ResType.IsInteger and (Info.ResType.IntValue in [12, 14]) then // 12=GROUP_CURSOR, 14=GROUP_ICON
    begin
      IsGroupResource := True;
      GroupInfo := Info;
      GroupInfoIndex := I;
      Break;
    end;
  end;

  // -------------------------------------------------------------------------
  // 4. TRATAMIENTO ESPECIAL PARA RECURSOS DE GRUPO
  // -------------------------------------------------------------------------
  if IsGroupResource then
  begin
    DebugInfo(Format('  Group resource detected (Type %d), special handling with Mode=%d', [Info.ResType.IntValue, Ord(Mode)]));

    // -----------------------------------------------------------------------
    // 4.1 Según el modo, usar la función especializada correspondiente
    // -----------------------------------------------------------------------

    if Mode = mrmRejoined then
    begin
      // CASO 1: Exportación monolítica - usar ReconstructIconGroup
      DebugInfo('  mrmRejoined: reconstructing group from RAW data');

      // Cargar datos RAW del grupo
      RawGroupStream := ResMgr_LoadRawResource([0, GroupInfoIndex]);
      if RawGroupStream = nil then
      begin
        Result.ErrorMessage := 'Failed to load raw group data';
        Exit;
      end;

      // Reconstruir el grupo completo
      ReconstructedGroupStream := ResMgr_ReconstructIconGroup(GroupInfo, RawGroupStream);
      RawGroupStream.Free;

      if ReconstructedGroupStream = nil then
      begin
        Result.ErrorMessage := 'Failed to reconstruct group';
        Exit;
      end;

      // Preparar resultado con un solo item
      SetLength(Result.Items, 1);
      Result.Items[0].Success := True;
      Result.Items[0].Data := ReconstructedGroupStream;
      Result.Items[0].Size := ReconstructedGroupStream.Size;
      Result.Items[0].ResourceName := GroupInfo.Name;
      Result.Items[0].ResourceType := GroupInfo.ResType;
      Result.Items[0].ResourceTypeName := ResType_ToDisplayString(GroupInfo.ResType);

      if GroupInfo.ResType.IntValue = 14 then
      begin
        Result.Items[0].MimeType := mtIcon;
        Result.Items[0].MimeTypeName := 'Icon';
        Result.Items[0].ResourceCategory := rcImage;
        Result.Items[0].ResourceCategoryName := 'Image';
        Result.Items[0].DefaultView := 'image';
      end
      else
      begin
        Result.Items[0].MimeType := mtCursor;
        Result.Items[0].MimeTypeName := 'Cursor';
        Result.Items[0].ResourceCategory := rcImage;
        Result.Items[0].ResourceCategoryName := 'Image';
        Result.Items[0].DefaultView := 'image';
      end;

      Result.Items[0].CanDisplay := True;
      Result.Items[0].LanguageID := GroupInfo.LanguageID;
      Result.Items[0].ResourceIndex := GroupInfoIndex;
      Result.Items[0].DataOffset := GroupInfo.DataOffset;

      Result.Combined := ReconstructedGroupStream;
      Result.Count := 1;
      Result.Success := True;

      DebugInfo(Format('  ✓ Returning rejoined group: %d bytes', [ReconstructedGroupStream.Size]));
      Exit;
    end
    else
    begin
      // CASO 2: Visualización - usar LoadGroupIcon para obtener imágenes individuales
      DebugInfo('  Using ResMgr_LoadGroupIcon for display');

      // Llamar a la función especializada para grupos
      Result := ResMgr_LoadGroupIcon(ResourceName);

      if not Result.Success then
      begin
        Result.ErrorMessage := 'Failed to load group icons';
        Exit;
      end;

      // -----------------------------------------------------------------------
      // 4.2 Aplicar filtros según el modo
      // -----------------------------------------------------------------------
      case Mode of
        mrmFirst:
        begin
          DebugInfo('  Applying mrmFirst filter: keeping first image only');
          if Result.Count > 0 then
            ReduceToSingleItem(Result, 0);
        end;

        mrmLast:
        begin
          DebugInfo('  Applying mrmLast filter: keeping last image only');
          if Result.Count > 0 then
            ReduceToSingleItem(Result, Result.Count - 1);
        end;

        mrmLargest:
        begin
          DebugInfo('  Applying mrmLargest filter: keeping largest image');
          if Result.Count = 0 then
          begin
            Result.Success := False;
            Result.ErrorMessage := 'No images to filter';
            Exit;
          end;

          BestIndex := 0;
          LargestSize := Result.Items[0].Size;

          for I := 1 to Result.Count - 1 do
          begin
            if Result.Items[I].Size > LargestSize then
            begin
              LargestSize := Result.Items[I].Size;
              BestIndex := I;
            end;
          end;

          ReduceToSingleItem(Result, BestIndex);
        end;

        mrmSmallest:
        begin
          DebugInfo('  Applying mrmSmallest filter: keeping smallest image');
          if Result.Count = 0 then
          begin
            Result.Success := False;
            Result.ErrorMessage := 'No images to filter';
            Exit;
          end;

          BestIndex := 0;
          SmallestSize := Result.Items[0].Size;

          for I := 1 to Result.Count - 1 do
          begin
            if Result.Items[I].Size < SmallestSize then
            begin
              SmallestSize := Result.Items[I].Size;
              BestIndex := I;
            end;
          end;

          ReduceToSingleItem(Result, BestIndex);
        end;

        mrmSelection:
        begin
          DebugInfo('  Applying mrmSelection filter: keeping selected indices');
          if Length(SelectedIndices) = 0 then
          begin
            Result.ErrorMessage := 'No indices specified for selection mode';
            Result.Success := False;
            Exit;
          end;

          // Crear arrays temporales con los índices seleccionados
          SetLength(Result.Items, Length(SelectedIndices));
          SetLength(Result.ImageStreams, Length(SelectedIndices));
          SetLength(Result.ImageMimeTypes, Length(SelectedIndices));
          SetLength(Result.ImageDimensions, Length(SelectedIndices));
          SetLength(Result.ImageCategories, Length(SelectedIndices));

          for I := 0 to High(SelectedIndices) do
          begin
            ImageIndex := SelectedIndices[I];
            if (ImageIndex >= 0) and (ImageIndex < Length(Result.ImageStreams)) then
            begin
              Result.Items[I] := Result.Items[ImageIndex];
              Result.ImageStreams[I] := Result.ImageStreams[ImageIndex];
              Result.ImageMimeTypes[I] := Result.ImageMimeTypes[ImageIndex];
              Result.ImageDimensions[I] := Result.ImageDimensions[ImageIndex];
              Result.ImageCategories[I] := Result.ImageCategories[ImageIndex];
            end;
          end;
          Result.Count := Length(SelectedIndices);
        end;

        mrmText, mrmImage, mrmAudio, mrmVideo:
        begin
          // Para grupos, estos modos devuelven las imágenes si corresponde
          if Mode = mrmImage then
            DebugInfo('  mrmImage: keeping all images')
          else
          begin
            // Si piden texto, audio, etc. de un grupo, devolver vacío
            DebugInfo(Format('  Mode %d not applicable to group, returning empty', [Ord(Mode)]));
            SetLength(Result.Items, 0);
            SetLength(Result.ImageStreams, 0);
            SetLength(Result.ImageMimeTypes, 0);
            SetLength(Result.ImageDimensions, 0);
            SetLength(Result.ImageCategories, 0);
            Result.Count := 0;
          end;
        end;

          // mrmAll, mrmBest, etc. - mantener todas las imágenes
        else
          DebugInfo('  Keeping all extracted images');
      end;

      // Actualizar contador
      Result.Count := Length(Result.Items);
      Result.Success := Result.Count > 0;

      if Result.Success then
        DebugInfo(Format('  ✓ Returning %d images from group', [Result.Count]))
      else
        Result.ErrorMessage := 'No images match the requested mode';

      Exit;
    end;
  end;

  // -------------------------------------------------------------------------
  // 5. RECURSOS NORMALES (NO GRUPO) - COLECTAR TODOS LOS QUE COINCIDEN
  // -------------------------------------------------------------------------
  SetLength(ResourceIndices, 0);
  for I := 0 to ResMgr_ResourceCount - 1 do
  begin
    Info := ResMgr_Resources[I];
    if (Info.Name <> '') and (Info.Name = ResourceName) then
    begin
      // Skip group resources here (already handled)
      if Info.ResType.IsInteger and (Info.ResType.IntValue in [12, 14]) then
        Continue;

      // Filter by resource type if specified
      if (ResTypeStr <> '') then
      begin
        if Info.ResType.IsInteger then
        begin
          if not SameText(ResType_ToDisplayString(Info.ResType), ResTypeStr) and (IntToStr(Info.ResType.IntValue) <> ResTypeStr) then
            Continue;
        end
        else
        begin
          if not SameText(Info.ResType.StrValue, ResTypeStr) then
            Continue;
        end;
      end;

      // Filter by language if specified
      if (Lang > 0) and (Info.LanguageID <> Lang) then
        Continue;

      SetLength(ResourceIndices, Length(ResourceIndices) + 1);
      ResourceIndices[High(ResourceIndices)] := I;
    end;
  end;

  if Length(ResourceIndices) = 0 then
  begin
    Result.ErrorMessage := 'No resource found with name: ' + ResourceName;
    LastError := Result.ErrorMessage;
    Exit;
  end;

  DebugInfo(Format('  Found %d matching resources', [Length(ResourceIndices)]));

  // -------------------------------------------------------------------------
  // 6. PROCESAR RECURSOS NORMALES SEGÚN EL MODO
  // -------------------------------------------------------------------------
  case Mode of
    mrmAll:
    begin
      SetLength(Result.Items, Length(ResourceIndices));
      for I := 0 to High(ResourceIndices) do
      begin
        Result.Items[I] := ResMgr_LoadResourceByIndex(ResourceIndices[I]);
        if not Result.Items[I].Success then
        begin
          Result.ErrorMessage := 'Failed to load item ' + IntToStr(I);
          Exit;
        end;
      end;
      Result.Count := Length(ResourceIndices);
      Result.Success := True;
    end;

    mrmFirst:
    begin
      if Length(ResourceIndices) > 0 then
      begin
        SetLength(Result.Items, 1);
        Result.Items[0] := ResMgr_LoadResourceByIndex(ResourceIndices[0]);
        if Result.Items[0].Success then
        begin
          Result.Count := 1;
          Result.Success := True;
        end
        else
          Result.ErrorMessage := Result.Items[0].ErrorMessage;
      end;
    end;

    mrmLast:
    begin
      if Length(ResourceIndices) > 0 then
      begin
        SetLength(Result.Items, 1);
        Result.Items[0] := ResMgr_LoadResourceByIndex(ResourceIndices[High(ResourceIndices)]);
        if Result.Items[0].Success then
        begin
          Result.Count := 1;
          Result.Success := True;
        end
        else
          Result.ErrorMessage := Result.Items[0].ErrorMessage;
      end;
    end;

    mrmLargest:
    begin
      BestIndex := -1;
      LargestSize := -1;
      for I := 0 to High(ResourceIndices) do
      begin
        if ResMgr_Resources[ResourceIndices[I]].Size > LargestSize then
        begin
          LargestSize := ResMgr_Resources[ResourceIndices[I]].Size;
          BestIndex := ResourceIndices[I];
        end;
      end;
      if BestIndex >= 0 then
      begin
        SetLength(Result.Items, 1);
        Result.Items[0] := ResMgr_LoadResourceByIndex(BestIndex);
        if Result.Items[0].Success then
        begin
          Result.Count := 1;
          Result.Success := True;
        end
        else
          Result.ErrorMessage := Result.Items[0].ErrorMessage;
      end;
    end;

    mrmSmallest:
    begin
      BestIndex := -1;
      SmallestSize := MaxInt;
      for I := 0 to High(ResourceIndices) do
      begin
        if ResMgr_Resources[ResourceIndices[I]].Size < SmallestSize then
        begin
          SmallestSize := ResMgr_Resources[ResourceIndices[I]].Size;
          BestIndex := ResourceIndices[I];
        end;
      end;
      if BestIndex >= 0 then
      begin
        SetLength(Result.Items, 1);
        Result.Items[0] := ResMgr_LoadResourceByIndex(BestIndex);
        if Result.Items[0].Success then
        begin
          Result.Count := 1;
          Result.Success := True;
        end
        else
          Result.ErrorMessage := Result.Items[0].ErrorMessage;
      end;
    end;

    mrmSelection:
    begin
      if Length(SelectedIndices) = 0 then
      begin
        Result.ErrorMessage := 'No indices specified for selection mode';
        Exit;
      end;

      SetLength(Result.Items, Length(SelectedIndices));
      for I := 0 to High(SelectedIndices) do
      begin
        if (SelectedIndices[I] >= 0) and (SelectedIndices[I] < Length(ResourceIndices)) then
        begin
          Result.Items[I] := ResMgr_LoadResourceByIndex(ResourceIndices[SelectedIndices[I]]);
          if not Result.Items[I].Success then
          begin
            Result.ErrorMessage := 'Failed to load selected index ' + IntToStr(SelectedIndices[I]);
            Exit;
          end;
        end
        else
        begin
          Result.ErrorMessage := 'Selected index out of range: ' + IntToStr(SelectedIndices[I]);
          Exit;
        end;
      end;
      Result.Count := Length(SelectedIndices);
      Result.Success := True;
    end;

    mrmBest:
    begin
      BestIndex := -1;
      BestScore := -1;

      for I := 0 to High(ResourceIndices) do
      begin
        LoadResult := ResMgr_LoadResourceByIndex(ResourceIndices[I]);
        if LoadResult.Success then
        begin
          CurrentScore := 0;

          if FormatPref <> '' then
          begin
            if SameText(LoadResult.MimeTypeName, FormatPref) then
              CurrentScore := CurrentScore + 1000
            else if (FormatUpper = 'TEXT') and (LoadResult.ResourceCategory = rcText) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'IMAGE') and (LoadResult.ResourceCategory = rcImage) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'AUDIO') and (LoadResult.ResourceCategory = rcAudio) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'VIDEO') and (LoadResult.ResourceCategory = rcVideo) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'HTML') and (LoadResult.MimeType = mtHtml) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'CSS') and (LoadResult.MimeType = mtCss) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'JS') and (LoadResult.MimeType = mtJavaScript) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'XML') and (LoadResult.MimeType = mtXml) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'JSON') and (LoadResult.MimeType = mtJson) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'PNG') and (LoadResult.MimeType = mtPng) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'JPG') or (FormatUpper = 'JPEG') and (LoadResult.MimeType = mtJpeg) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'GIF') and (LoadResult.MimeType = mtGif) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'BMP') and (LoadResult.MimeType = mtBitmap) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'ICO') and (LoadResult.MimeType = mtIcon) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'ANI') and (LoadResult.MimeType = mtAni) then
              CurrentScore := CurrentScore + 500
            else if (FormatUpper = 'CUR') and (LoadResult.MimeType = mtCursor) then
              CurrentScore := CurrentScore + 500;
          end;

          if Lang > 0 then
          begin
            if LoadResult.LanguageID = Lang then
              CurrentScore := CurrentScore + 300
            else if LoadResult.LanguageID = 0 then
              CurrentScore := CurrentScore + 100;
          end
          else if LoadResult.LanguageID = 0 then
            CurrentScore := CurrentScore + 200;

          if not LoadResult.ResourceType.IsInteger then
            CurrentScore := CurrentScore + 100;

          CurrentScore := CurrentScore + (LoadResult.Size div 2048);

          if CurrentScore > BestScore then
          begin
            BestScore := CurrentScore;
            BestIndex := ResourceIndices[I];
          end;

          LoadResult.Data.Free;
        end;
      end;

      if BestIndex >= 0 then
      begin
        SetLength(Result.Items, 1);
        Result.Items[0] := ResMgr_LoadResourceByIndex(BestIndex);
        if Result.Items[0].Success then
        begin
          Result.Count := 1;
          Result.Success := True;
        end
        else
          Result.ErrorMessage := Result.Items[0].ErrorMessage;
      end;
    end;

    mrmText, mrmImage, mrmAudio, mrmVideo, mrmArchive, mrmExecutable:
    begin
      SetLength(Result.Items, 0);
      for I := 0 to High(ResourceIndices) do
      begin
        LoadResult := ResMgr_LoadResourceByIndex(ResourceIndices[I]);
        if LoadResult.Success then
        begin
          case Mode of
            mrmText:
              if LoadResult.ResourceCategory = rcText then
                AddToResult(Result, LoadResult, ResourceIndices[I])
              else
                LoadResult.Data.Free;

            mrmImage:
              if LoadResult.ResourceCategory = rcImage then
                AddToResult(Result, LoadResult, ResourceIndices[I])
              else
                LoadResult.Data.Free;

            mrmAudio:
              if LoadResult.ResourceCategory = rcAudio then
                AddToResult(Result, LoadResult, ResourceIndices[I])
              else
                LoadResult.Data.Free;

            mrmVideo:
              if LoadResult.ResourceCategory = rcVideo then
                AddToResult(Result, LoadResult, ResourceIndices[I])
              else
                LoadResult.Data.Free;

            mrmArchive:
              if LoadResult.ResourceCategory = rcArchive then
                AddToResult(Result, LoadResult, ResourceIndices[I])
              else
                LoadResult.Data.Free;

            mrmExecutable:
              if LoadResult.ResourceCategory = rcExecutable then
                AddToResult(Result, LoadResult, ResourceIndices[I])
              else
                LoadResult.Data.Free;
          end;
        end;
      end;

      Result.Success := Result.Count > 0;
      if not Result.Success then
        Result.ErrorMessage := 'No resources match the specified filter';
    end;

    mrmByType, mrmByMime:
    begin
      SetLength(Result.Items, 0);
      for I := 0 to High(ResourceIndices) do
      begin
        LoadResult := ResMgr_LoadResourceByIndex(ResourceIndices[I]);
        if LoadResult.Success then
        begin
          if Mode = mrmByType then
          begin
            if SameText(LoadResult.ResourceTypeName, FormatPref) then
              AddToResult(Result, LoadResult, ResourceIndices[I])
            else
              LoadResult.Data.Free;
          end
          else // mrmByMime
          begin
            if SameText(LoadResult.MimeTypeName, FormatPref) then
              AddToResult(Result, LoadResult, ResourceIndices[I])
            else
              LoadResult.Data.Free;
          end;
        end;
      end;
      Result.Success := Result.Count > 0;
      if not Result.Success then
        Result.ErrorMessage := 'No resources match the specified type/MIME';
    end;

    else
    begin
      SetLength(Result.Items, Length(ResourceIndices));
      for I := 0 to High(ResourceIndices) do
      begin
        Result.Items[I] := ResMgr_LoadResourceByIndex(ResourceIndices[I]);
        if not Result.Items[I].Success then
        begin
          Result.ErrorMessage := 'Failed to load item ' + IntToStr(I);
          Exit;
        end;
      end;
      Result.Count := Length(ResourceIndices);
      Result.Success := True;
    end;
  end;

  // -------------------------------------------------------------------------
  // 7. ASEGURAR QUE TODOS LOS ITEMS TENGAN INFORMACIÓN COMPLETA DE CATEGORÍA
  // -------------------------------------------------------------------------
  for I := 0 to Result.Count - 1 do
  begin
    if Result.Items[I].Success then
    begin
      if Result.Items[I].ResourceCategory = rcUnknown then
      begin
        case Result.Items[I].MimeType of
          mtText, mtHtml, mtXml, mtJson, mtCss, mtJavaScript, mtRtf:
            Result.Items[I].ResourceCategory := rcText;
          mtBitmap, mtIcon, mtJpeg, mtPng, mtGif:
            Result.Items[I].ResourceCategory := rcImage;
          mtWave, mtMp3:
            Result.Items[I].ResourceCategory := rcAudio;
          mtAvi:
            Result.Items[I].ResourceCategory := rcVideo;
          mtExecutable:
            Result.Items[I].ResourceCategory := rcExecutable;
          mtArchive:
            Result.Items[I].ResourceCategory := rcArchive;
          mtFont:
            Result.Items[I].ResourceCategory := rcFont;
          mtBinary:
            Result.Items[I].ResourceCategory := rcBinary;
        end;
      end;

      if Result.Items[I].ResourceCategoryName = '' then
      begin
        CategoryInfo := GetCategoryInfo(Result.Items[I].ResourceCategory);
        Result.Items[I].ResourceCategoryName := CategoryInfo.Name;
        Result.Items[I].CanDisplay := CategoryInfo.CanDisplay;
        Result.Items[I].DefaultView := CategoryInfo.DefaultView;
      end;

      DebugInfo(Format('  Item %d: Category=%s (%d), MIME=%s', [I, Result.Items[I].ResourceCategoryName, Ord(Result.Items[I].ResourceCategory), Result.Items[I].MimeTypeName]));
    end;
  end;

  if Result.Success then
    DebugInfo(Format('  ✓ Loaded %d items with complete info', [Result.Count]))
  else
    DebugError('  ✗ Failed: ' + Result.ErrorMessage);
end;

// En la función AnalyzeResource, podemos añadir la categoría
procedure AnalyzeResource(Index: integer; var ListInfo: TResListInfo);
var
  LoadResult: TResLoadResult;
begin
  ListInfo.Index := Index;
  ListInfo.Name := ResMgr_Resources[Index].Name;
  ListInfo.ResType := ResMgr_Resources[Index].ResType;
  ListInfo.DisplayType := ResType_ToDisplayString(ResMgr_Resources[Index].ResType);
  ListInfo.Size := ResMgr_Resources[Index].Size;
  ListInfo.LanguageID := ResMgr_Resources[Index].LanguageID;
  ListInfo.DataOffset := ResMgr_Resources[Index].DataOffset;

  LoadResult := ResMgr_LoadResourceByIndex(Index);
  if LoadResult.Success then
  begin
    ListInfo.MimeType := LoadResult.MimeType;
    ListInfo.MimeString := TMimeDetector.TypeToString(LoadResult.MimeType);
    ListInfo.IsText := ResUtils_IsText(LoadResult);
    ListInfo.IsImage := ResUtils_IsImage(LoadResult);

    // Podríamos añadir un campo Category a TResListInfo si queremos
    // ListInfo.Category := ResUtils_GetResourceCategory(LoadResult);

    LoadResult.Data.Free;
  end
  else
  begin
    ListInfo.MimeType := mtUnknown;
    ListInfo.MimeString := 'Unknown';
    ListInfo.IsText := False;
    ListInfo.IsImage := False;
  end;
end;

// Helper: Check if resource is valid (not just padding/header)
function IsValidResource(const Info: TResInfo): boolean;
begin
  Result := False;

  // Skip resources with no name
  if Info.Name = '' then
    Exit;

  // Skip resources with zero size (padding entries)
  if Info.Size = 0 then
    Exit;

  // Skip resources at offset 0x20 or other typical header/padding offsets
  // if they have generic type "ZERO" or similar
  if (Info.DataOffset <= 32) and Info.ResType.IsInteger and (Info.ResType.IntValue = 0) then
    Exit;

  // Also skip if the resource type is 0 (ZERO) - usually padding
  if Info.ResType.IsInteger and (Info.ResType.IntValue = 0) then
    Exit;

  Result := True;
end;

function ResMgr_GetAllResources: TResListArray;
var
  I, Count: integer;
begin
  SetLength(Result, ResMgr_ResourceCount);
  Count := 0;

  for I := 0 to ResMgr_ResourceCount - 1 do
  begin
    if IsValidResource(ResMgr_Resources[I]) then
    begin
      AnalyzeResource(I, Result[Count]);
      Inc(Count);
    end;
  end;

  SetLength(Result, Count);
end;

function ResMgr_GetTextResources: TResListArray;
var
  I, Count: integer;
  Info: TResListInfo;
begin
  SetLength(Result, ResMgr_ResourceCount);
  Count := 0;

  for I := 0 to ResMgr_ResourceCount - 1 do
  begin
    if IsValidResource(ResMgr_Resources[I]) then
    begin
      AnalyzeResource(I, Info);
      if Info.IsText then
      begin
        Result[Count] := Info;
        Inc(Count);
      end;
    end;
  end;

  SetLength(Result, Count);
end;

function ResMgr_GetImageResources: TResListArray;
var
  I, Count: integer;
  Info: TResListInfo;
begin
  SetLength(Result, ResMgr_ResourceCount);
  Count := 0;

  for I := 0 to ResMgr_ResourceCount - 1 do
  begin
    if IsValidResource(ResMgr_Resources[I]) then
    begin
      AnalyzeResource(I, Info);
      if Info.IsImage then
      begin
        Result[Count] := Info;
        Inc(Count);
      end;
    end;
  end;

  SetLength(Result, Count);
end;

//------------------------------------------------------------------------------
// Icon Group Reconstruction - Updated to use ResUtils and save monolithic file.
//------------------------------------------------------------------------------

function ResMgr_ReconstructIconGroup(const GroupInfo: TResInfo; const GroupData: TMemoryStream): TMemoryStream;
type
  // Estructura exacta de un grupo de iconos en el recurso
  TGrpIconDir = packed record
    idReserved: word;      // 0
    idType: word;          // 1 para iconos, 2 para cursores
    idCount: word;         // Número de imágenes
  end;

  TGrpIconEntry = packed record
    bWidth: byte;          // Ancho (0=256)
    bHeight: byte;         // Alto (0=256)
    bColorCount: byte;     // Número de colores
    bReserved: byte;       // 0
    wPlanes: word;         // Planos de color (para cursor: hotspot X)
    wBitCount: word;       // Bits por píxel (para cursor: hotspot Y)
    dwBytesInRes: DWord;   // Tamaño de la imagen
    nId: word;             // ID del recurso
  end;

  // Estructura exacta de un archivo ICO/CUR
  TIconDir = packed record
    idReserved: word;      // 0
    idType: word;          // 1 para ICO, 2 para CUR
    idCount: word;         // Número de imágenes
  end;

  TIconDirEntry = packed record
    bWidth: byte;          // Ancho (0=256)
    bHeight: byte;         // Alto (0=256)
    bColorCount: byte;     // Número de colores
    bReserved: byte;       // 0
    wPlanes: word;         // Planos (para CUR: hotspot X)
    wBitCount: word;       // Bits (para CUR: hotspot Y)
    dwBytesInRes: DWord;   // Tamaño de la imagen
    dwImageOffset: DWord;  // Offset de la imagen en el archivo
  end;
var
  GrpHeader: TGrpIconDir;
  GrpEntries: array of TGrpIconEntry;
  IconHeader: TIconDir;
  IconEntries: array of TIconDirEntry;
  I, J: integer;
  IconResName: string;
  IconInfo: TResInfo;
  IconRawStream: TMemoryStream;  // <-- CAMBIADO: ahora usamos RAW stream
  Found: boolean;
  IsCursor: boolean;
  IconDataStreams: array of TMemoryStream;
  CurrentOffset: DWord;
  HotspotX, HotspotY: word;      // <-- AÑADIDO: para cursores
begin
  Result := nil;

  //--------------------------------------------------------------------------
  // Validar datos de entrada
  //--------------------------------------------------------------------------
  if (GroupData = nil) or (GroupData.Size < SizeOf(TGrpIconDir)) then
    Exit;

  //--------------------------------------------------------------------------
  // Leer el header del grupo
  //--------------------------------------------------------------------------
  GroupData.Position := 0;
  GroupData.Read(GrpHeader, SizeOf(TGrpIconDir));

  // Validar header
  if (GrpHeader.idReserved <> 0) then
    Exit;

  if (GrpHeader.idType <> 1) and (GrpHeader.idType <> 2) then
    Exit;

  if (GrpHeader.idCount = 0) or (GrpHeader.idCount > 100) then
    Exit;

  IsCursor := (GrpHeader.idType = 2);

  //--------------------------------------------------------------------------
  // Leer todas las entradas del grupo
  //--------------------------------------------------------------------------
  SetLength(GrpEntries, GrpHeader.idCount);
  SetLength(IconDataStreams, GrpHeader.idCount);

  for I := 0 to GrpHeader.idCount - 1 do
    IconDataStreams[I] := nil;

  try
    for I := 0 to GrpHeader.idCount - 1 do
    begin
      if GroupData.Position + SizeOf(TGrpIconEntry) > GroupData.Size then
        Break;

      GroupData.Read(GrpEntries[I], SizeOf(TGrpIconEntry));

      // Buscar el recurso individual
      IconResName := IntToStr(GrpEntries[I].nId);
      Found := False;

      for J := 0 to ResMgr_ResourceCount - 1 do
      begin
        if not ResMgr_Resources[J].ResType.IsInteger then
          Continue;

        if IsCursor then
        begin
          if (ResMgr_Resources[J].ResType.IntValue in [1, 21]) and (ResMgr_Resources[J].Name = IconResName) then
          begin
            IconInfo := ResMgr_Resources[J];
            Found := True;
            Break;
          end;
        end
        else
        begin
          if (ResMgr_Resources[J].ResType.IntValue in [3, 22]) and (ResMgr_Resources[J].Name = IconResName) then
          begin
            IconInfo := ResMgr_Resources[J];
            Found := True;
            Break;
          end;
        end;
      end;

      if not Found then
        Continue;

      // ======================================================================
      // CAMBIO IMPORTANTE: Cargar datos RAW en lugar de usar LoadResourceByInfo
      // ======================================================================
      if IsCursor then
      begin
        // Para cursores: RT_CURSOR = 1
        IconRawStream := ResMgr_LoadRawResource([1, IconResName, 1]);
        if IconRawStream <> nil then
        begin
          // Los cursores tienen hotspot al inicio (4 bytes)
          IconRawStream.Position := 0;
          HotspotX := IconRawStream.ReadWord;
          HotspotY := IconRawStream.ReadWord;

          // El resto son datos DIB
          IconDataStreams[I] := TMemoryStream.Create;
          IconDataStreams[I].CopyFrom(IconRawStream, IconRawStream.Size - 4);
          IconDataStreams[I].Position := 0;

          // Actualizar GrpEntries con los hotspots correctos
          GrpEntries[I].wPlanes := HotspotX;
          GrpEntries[I].wBitCount := HotspotY;

          IconRawStream.Free;
        end;
      end
      else
      begin
        // Para iconos: RT_ICON = 3
        IconRawStream := ResMgr_LoadRawResource([1, IconResName, 3]);
        if IconRawStream <> nil then
        begin
          // Los iconos son DIBs puros, sin cabecera adicional
          IconDataStreams[I] := TMemoryStream.Create;
          IconDataStreams[I].CopyFrom(IconRawStream, 0);
          IconDataStreams[I].Position := 0;
          IconRawStream.Free;
        end;
      end;
    end;

    //--------------------------------------------------------------------------
    // Crear el archivo ICO/CUR
    //--------------------------------------------------------------------------
    Result := TMemoryStream.Create;

    // Escribir el header del icono
    IconHeader.idReserved := 0;
    IconHeader.idType := GrpHeader.idType;
    IconHeader.idCount := GrpHeader.idCount;
    Result.Write(IconHeader, SizeOf(TIconDir));

    // Preparar las entradas del directorio
    SetLength(IconEntries, GrpHeader.idCount);
    CurrentOffset := SizeOf(TIconDir) + (GrpHeader.idCount * SizeOf(TIconDirEntry));

    for I := 0 to GrpHeader.idCount - 1 do
    begin
      // Copiar campos básicos
      IconEntries[I].bWidth := GrpEntries[I].bWidth;
      IconEntries[I].bHeight := GrpEntries[I].bHeight;
      IconEntries[I].bColorCount := GrpEntries[I].bColorCount;
      IconEntries[I].bReserved := 0;

      if IsCursor then
      begin
        // Para cursor: wPlanes/wBitCount son el hotspot (ya actualizado)
        IconEntries[I].wPlanes := GrpEntries[I].wPlanes;
        IconEntries[I].wBitCount := GrpEntries[I].wBitCount;
      end
      else
      begin
        // Para icono: usar los valores originales
        IconEntries[I].wPlanes := GrpEntries[I].wPlanes;
        IconEntries[I].wBitCount := GrpEntries[I].wBitCount;
      end;

      // Tamaño y offset
      if IconDataStreams[I] <> nil then
        IconEntries[I].dwBytesInRes := IconDataStreams[I].Size
      else
        IconEntries[I].dwBytesInRes := 0;

      IconEntries[I].dwImageOffset := CurrentOffset;

      // Actualizar offset para la siguiente imagen
      if IconDataStreams[I] <> nil then
        CurrentOffset := CurrentOffset + IconDataStreams[I].Size;
    end;

    // Escribir las entradas del directorio
    for I := 0 to GrpHeader.idCount - 1 do
      Result.Write(IconEntries[I], SizeOf(TIconDirEntry));

    // Escribir los datos de las imágenes
    for I := 0 to GrpHeader.idCount - 1 do
    begin
      if IconDataStreams[I] <> nil then
      begin
        Result.Position := IconEntries[I].dwImageOffset;
        IconDataStreams[I].Position := 0;
        Result.CopyFrom(IconDataStreams[I], IconDataStreams[I].Size);
      end;
    end;

    Result.Position := 0;

  finally
    // Liberar memoria
    for I := 0 to High(IconDataStreams) do
    begin
      if IconDataStreams[I] <> nil then
        IconDataStreams[I].Free;
    end;
  end;
end;

// wrappers
function ResUtils_IsImage(const Res: TResLoadResult): boolean;
begin
  Result := Res.Success and (TMimeDetector.GetFormatInfo(Res.MimeType).IsImage);
end;

function ResUtils_IsText(const Res: TResLoadResult): boolean;
begin
  Result := Res.Success and (TMimeDetector.GetFormatInfo(Res.MimeType).IsText);
end;

//------------------------------------------------------------------------------
// Extract individual images from an ICO file for display or save individual entries.
//------------------------------------------------------------------------------

procedure ExtractIconImages(ICOStream: TMemoryStream; var ImageStreams: TMemoryStreamArray; var ImageMimeTypes: TMimeTypeArray; var ImageDimensions: TStringArray);
var
  Header: array[0..5] of byte;
  IconCount: word;
  I: integer;
  J: integer;  // Variable para el contador de imágenes válidas
  WidthByte, HeightByte: byte;
  ImageSize: DWord;
  ImageOffset: DWord;
  ImageData: TMemoryStream;
  MimeType: TMimeType;
  ActualWidth, ActualHeight: integer;
  DirEntry: array[0..15] of byte;
  StreamSize: int64;
  EndOffset: int64;
  ValidCount: integer;
begin
  // Clear output arrays
  SetLength(ImageStreams, 0);
  SetLength(ImageMimeTypes, 0);
  SetLength(ImageDimensions, 0);

  if (ICOStream = nil) or (ICOStream.Size < 6) then
  begin
    DebugError('ExtractIconImages: Invalid ICO stream');
    Exit;
  end;

  ICOStream.Position := 0;
  ICOStream.Read(Header, 6);

  // Check ICO header
  if (Header[0] <> 0) or (Header[1] <> 0) then
  begin
    DebugError('ExtractIconImages: Invalid reserved field');
    Exit;
  end;

  if (Header[2] <> 1) and (Header[2] <> 2) then
  begin
    DebugError('ExtractIconImages: Invalid type field (should be 1=ICO, 2=CUR)');
    Exit;
  end;

  IconCount := Header[4] + (Header[5] shl 8);
  if (IconCount = 0) or (IconCount > 255) then
  begin
    DebugError('ExtractIconImages: Invalid icon count: ' + IntToStr(IconCount));
    Exit;
  end;

  StreamSize := ICOStream.Size;
  DebugInfo(Format('ExtractIconImages: ICO file with %d images, %d bytes total', [IconCount, StreamSize]));

  SetLength(ImageStreams, IconCount);
  SetLength(ImageMimeTypes, IconCount);
  SetLength(ImageDimensions, IconCount);

  // Inicializar todos los streams a nil
  for I := 0 to IconCount - 1 do
  begin
    ImageStreams[I] := nil;
    ImageMimeTypes[I] := mtUnknown;
  end;

  // Leer todos los directory entries
  for I := 0 to IconCount - 1 do
  begin
    // Verificar si hay suficiente espacio para el entry
    if ICOStream.Position + 16 > StreamSize then
    begin
      DebugWarning(Format('ExtractIconImages: Unexpected end of file reading entry %d', [I]));
      Break;
    end;

    // Leer el entry de 16 bytes
    ICOStream.Read(DirEntry, 16);

    WidthByte := DirEntry[0];
    HeightByte := DirEntry[1];

    // Convertir a dimensiones reales (0 significa 256)
    if WidthByte = 0 then
      ActualWidth := 256
    else
      ActualWidth := WidthByte;

    if HeightByte = 0 then
      ActualHeight := 256
    else
      ActualHeight := HeightByte;

    // Obtener tamaño y offset (DWORDs al final)
    ImageSize := PDWord(@DirEntry[8])^;
    ImageOffset := PDWord(@DirEntry[12])^;

    // Guardar dimensiones
    ImageDimensions[I] := Format('%dx%d', [ActualWidth, ActualHeight]);

    DebugInfo(Format('  Entry %d: ID in group, offset=%d, size=%d, %s', [I, ImageOffset, ImageSize, ImageDimensions[I]]));

    // Validar y extraer la imagen
    EndOffset := int64(ImageOffset) + int64(ImageSize);

    if (ImageSize > 0) and (ImageSize < 1024 * 1024) and  // Sanity check: menos de 1MB
      (int64(ImageOffset) >= 0) and (int64(ImageOffset) < StreamSize) and (EndOffset <= StreamSize) then
    begin
      // Guardar posición actual
      ICOStream.Position := ImageOffset;

      ImageData := TMemoryStream.Create;
      try
        ImageData.CopyFrom(ICOStream, ImageSize);
        ImageData.Position := 0;

        // Detectar MIME type
        MimeType := TMimeDetector.DetectFromStream(ImageData);

        // Rewind después de la detección
        ImageData.Position := 0;

        ImageStreams[I] := ImageData;
        ImageMimeTypes[I] := MimeType;

        DebugInfo(Format('  ✓ Extracted icon image %d: %s, %s, %d bytes at offset %d', [I, ImageDimensions[I], TMimeDetector.TypeToString(MimeType), ImageSize, ImageOffset]));
      except
        on E: Exception do
        begin
          DebugError(Format('  ✗ Error extracting image %d: %s', [I, E.Message]));
          ImageData.Free;
          ImageStreams[I] := nil;
          ImageMimeTypes[I] := mtUnknown;
        end;
      end;
    end
    else
    begin
      DebugWarning(Format('  ✗ Invalid image %d: offset=%d, size=%d, file size=%d', [I, ImageOffset, ImageSize, StreamSize]));
      ImageStreams[I] := nil;
      ImageMimeTypes[I] := mtUnknown;
    end;

    // IMPORTANTE: Volver a la posición de los directory entries
    // Después de extraer una imagen, el stream se queda en ImageOffset + ImageSize
    // Necesitamos volver a la posición del siguiente directory entry
    ICOStream.Position := 6 + (I + 1) * 16;
  end;

  // Contar imágenes válidas usando una variable separada
  ValidCount := 0;
  for I := 0 to High(ImageStreams) do
    if ImageStreams[I] <> nil then
      Inc(ValidCount);

  DebugInfo(Format('ExtractIconImages: Extracted %d valid images out of %d', [ValidCount, Length(ImageStreams)]));
end;

//------------------------------------------------------------------------------
// Load a group icon and return all individual icon images (reconstructed for display)
//------------------------------------------------------------------------------

function ResMgr_LoadGroupIcon(const GroupName: string): TMultiResLoadResult;
var
  I, J: integer;
  GroupInfo: TResInfo;
  GroupResult: TResLoadResult;
  FoundGroup: boolean;
  CategoryInfo: TResourceCategoryInfo;
  ReconstructedStream: TMemoryStream;
  TempRes: TResLoadResult;
  MimeType: TMimeType;
  HeaderCheck: array[0..7] of byte;
  IsPNG: boolean;
begin
  // Inicializar resultado
  Result.Success := False;
  Result.Count := 0;
  SetLength(Result.Items, 0);
  Result.Combined := nil;
  SetLength(Result.ImageStreams, 0);
  SetLength(Result.ImageMimeTypes, 0);
  SetLength(Result.ImageDimensions, 0);
  SetLength(Result.ImageCategories, 0);
  Result.ErrorMessage := '';

  DebugInfo('ResMgr_LoadGroupIcon: "' + GroupName + '"');

  FoundGroup := False;

  for I := 0 to ResMgr_ResourceCount - 1 do
  begin
    if (ResMgr_Resources[I].Name = GroupName) and ResMgr_Resources[I].ResType.IsInteger and (ResMgr_Resources[I].ResType.IntValue = 14) then
    begin
      GroupInfo := ResMgr_Resources[I];
      FoundGroup := True;

      // Cargar el grupo (ya reconstruido como ICO)
      GroupResult := ResMgr_LoadResourceByInfo(GroupInfo);
      if not GroupResult.Success then
      begin
        Result.ErrorMessage := 'Failed to load group data';
        Exit;
      end;

      try
        GroupResult.Data.Position := 0;

        // Extraer imágenes individuales (pueden ser DIB o PNG)
        ExtractIconImages(GroupResult.Data,
          Result.ImageStreams,
          Result.ImageMimeTypes,
          Result.ImageDimensions);

        // Configurar arrays
        SetLength(Result.Items, Length(Result.ImageStreams));
        SetLength(Result.ImageCategories, Length(Result.ImageStreams));

        // ====================================================================
        // RECONSTRUIR CADA IMAGEN SI ES NECESARIO
        // ====================================================================
        for J := 0 to High(Result.ImageStreams) do
        begin
          if Result.ImageStreams[J] <> nil then
          begin
            // Verificar MIME type actual
            MimeType := Result.ImageMimeTypes[J];

            // Detectar si es PNG (ya es visible directamente)
            if Result.ImageStreams[J].Size >= 8 then
            begin
              Result.ImageStreams[J].Position := 0;
              Result.ImageStreams[J].Read(HeaderCheck, 8);
              Result.ImageStreams[J].Position := 0;

              IsPNG := (HeaderCheck[0] = $89) and (HeaderCheck[1] = $50) and (HeaderCheck[2] = $4E) and (HeaderCheck[3] = $47) and (HeaderCheck[4] = $0D) and
                (HeaderCheck[5] = $0A) and (HeaderCheck[6] = $1A) and (HeaderCheck[7] = $0A);

              if IsPNG then
                MimeType := mtPng;
            end;

            // Solo reconstruir si es DIB (mtBitmap)
            if MimeType = mtBitmap then
            begin
              // Preparar estructura temporal para reconstrucción
              TempRes.Success := True;
              TempRes.Data := Result.ImageStreams[J];
              TempRes.ResourceType := ResType_CreateInt(3); // RT_ICON

              // Reconstruir el icono (DIB -> ICO con cabecera)
              ReconstructedStream := ResUtils_ReconstructIcon(TempRes.Data, TempRes.ResourceType);

              if ReconstructedStream <> nil then
              begin
                // Reemplazar el stream original con el reconstruido
                Result.ImageStreams[J].Free;
                Result.ImageStreams[J] := ReconstructedStream;
                MimeType := mtIcon;
              end;
            end;

            // Actualizar MIME type en el array
            Result.ImageMimeTypes[J] := MimeType;

            // Crear el Item con el stream (ya sea original o reconstruido)
            Result.Items[J].Success := True;
            Result.Items[J].Data := Result.ImageStreams[J];
            Result.Items[J].Size := Result.ImageStreams[J].Size;
            Result.Items[J].MimeType := MimeType;
            Result.Items[J].MimeTypeName := TMimeDetector.TypeToString(MimeType);
            Result.Items[J].ResourceName := GroupInfo.Name;
            Result.Items[J].ResourceType := ResType_CreateInt(3);
            Result.Items[J].ResourceTypeName := 'ICON';

            // Determinar categoría
            case MimeType of
              mtBitmap, mtIcon, mtJpeg, mtPng, mtGif:
                Result.Items[J].ResourceCategory := rcImage;
              else
                Result.Items[J].ResourceCategory := rcBinary;
            end;

            CategoryInfo := GetCategoryInfo(Result.Items[J].ResourceCategory);
            Result.Items[J].ResourceCategoryName := CategoryInfo.Name;
            Result.Items[J].CanDisplay := CategoryInfo.CanDisplay;
            Result.Items[J].DefaultView := CategoryInfo.DefaultView;

            Result.Items[J].LanguageID := GroupInfo.LanguageID;
            Result.Items[J].ResourceIndex := J;
            Result.Items[J].DataOffset := 0;
            Result.Items[J].ErrorMessage := '';

            Result.ImageCategories[J] := Result.Items[J].ResourceCategory;

            DebugInfo(Format('  Loaded icon %d: %s, %s, %d bytes', [J, Result.ImageDimensions[J], TMimeDetector.TypeToString(MimeType), Result.ImageStreams[J].Size]));
          end
          else
          begin
            Result.Items[J].Success := False;
            Result.Items[J].Data := nil;
            Result.ImageCategories[J] := rcUnknown;
          end;
        end;

        Result.Count := Length(Result.ImageStreams);
        Result.Success := Result.Count > 0;

        if Result.Success then
          DebugInfo(Format('  ✓ Loaded %d icons from group', [Result.Count]))
        else
          Result.ErrorMessage := 'Failed to extract any icons from group';

      finally
        // No liberamos GroupResult.Data
      end;

      Break;
    end;
  end;

  if not FoundGroup then
    Result.ErrorMessage := 'Group icon not found: ' + GroupName;
end;

//------------------------------------------------------------------------------
// Error Handling
//------------------------------------------------------------------------------

function ResMgr_GetErrorString: string;
begin
  Result := LastError;
end;

// Helper to add an item to result
procedure AddToResult(var Result: TMultiResLoadResult; const LoadResult: TResLoadResult; Index: integer);
begin
  SetLength(Result.Items, Result.Count + 1);
  Result.Items[Result.Count] := LoadResult;
  Result.Items[Result.Count].ResourceIndex := Index;
  Inc(Result.Count);
end;

function VarRecAsString(const V: TVarRec): string;
begin
  case V.VType of
    vtString: Result := V.VString^;
    vtAnsiString: Result := string(V.VAnsiString);
    vtPChar: Result := StrPas(V.VPChar);
    vtChar: Result := V.VChar;
    vtWideChar: Result := V.VWideChar;
    else
      Result := '';
  end;
end;

function ResMgr_GetResourceCount: Integer;
begin
  Result := ResMgr_ResourceCount;
end;

function ResMgr_GetResourceName(Index: Integer): string;
begin
  if (Index >= 0) and (Index < ResMgr_ResourceCount) then
    Result := ResMgr_Resources[Index].Name
  else
    Result := '';
end;

procedure ResMgr_UpdateResourceName(Index: Integer; const NewName: string);
begin
  if (Index >= 0) and (Index < ResMgr_ResourceCount) then
    ResMgr_Resources[Index].Name := NewName;
end;

initialization
  ResMgr_Init;

finalization
  ResMgr_Done;

end.
