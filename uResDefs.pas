unit uResDefs;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, uMimeDetect;

// Define type aliases FIRST, before they're used
// ============================================================================
// NUEVAS DEFINICIONES PARA CATEGORÍAS DE RECURSOS
// ============================================================================
type
  TLanguageInfo = record
    ID: Word;
    Name: string;
  end;

const
  LanguageCount = 9;
  LanguageList: array[0..LanguageCount-1] of TLanguageInfo = (
    (ID: 0;    Name: 'Neutral'),
    (ID: 9;    Name: 'English'),
    (ID: 1034; Name: 'Spanish (Spain)'),
    (ID: 1036; Name: 'French'),
    (ID: 1040; Name: 'Italian'),
    (ID: 1041; Name: 'Japanese'),
    (ID: 1042; Name: 'Korean'),
    (ID: 1049; Name: 'Russian'),
    (ID: 2052; Name: 'Chinese')
  );

type

  TResourceCategory = (
    rcUnknown,
    rcText,           // Documentos de texto
    rcImage,          // Imágenes
    rcAudio,          // Audio
    rcVideo,          // Video
    rcArchive,        // Archivos comprimidos
    rcExecutable,     // Ejecutables
    rcFont,           // Fuentes
    rcBinary          // Datos binarios no clasificados
  );

  TResourceCategoryInfo = record
    Category: TResourceCategory;
    Name: string;
    Description: string;
    CanDisplay: Boolean;     // Si podemos mostrarlo de alguna forma
    DefaultView: string;      // 'text', 'image', 'hex', 'audio', etc.
  end;

type
  TMemoryStreamArray = array of TMemoryStream;
  TMimeTypeArray = array of TMimeType;
  TStringArray = array of string;
  TResourceCategoryArray = array of TResourceCategory;

type
  TResType = record
    IsInteger: Boolean;
    IntValue: Integer;
    StrValue: String;
  end;

type
  TResInfo = record
    Name: String;
    ResType: TResType;
    Size: Int64;
    LanguageID: Integer;
    DataOffset: Int64;
    DataSize: Int64;
    DataVersion: DWord;
    MemoryFlags: Word;
    VersionInfo: DWord;
    Characteristics: DWord;
    Data: TMemoryStream;  // <-- NUEVO: para recursos en memoria
  end;

  TResInfoArray = array of TResInfo;

type

  // Multi-resource selection mode
  TMultiResourceMode = (
    mrmAll,        // Return all matching resources (default)
    mrmBest,       // Return best match based on format preference
    mrmFirst,      // Return only the first resource
    mrmLast,       // Return only the last resource
    mrmLargest,    // Return only the largest resource (by size)
    mrmSmallest,   // Return only the smallest resource
    mrmSelection,  // Return specific resources by index (requires indices)
    mrmByType,     // Return only resources of specific type
    mrmByMime,     // Return only resources with specific MIME type
    mrmText,       // Return only text resources
    mrmImage,      // Return only image resources
    mrmAudio,      // Return only audio resources
    mrmVideo,      // Return only video resources
    mrmArchive,    // Return only archive resources
    mrmExecutable,  // Return only executable resources
    mrmRejoined    // Return the rejoined group as a single stream
  );

  // Modern result type with everything we need
  TResResult = record
    Success: Boolean;
    Data: TMemoryStream;
    Size: Int64;
    ResType: TResType;
    MimeType: TMimeType;
    LanguageID: Integer;
    ErrorMessage: String;
  end;

  // Resource loading result - AHORA CON TODA LA INFORMACIÓN
  TResLoadResult = record
    Success: Boolean;
    Data: TMemoryStream;
    Size: Int64;
    MimeType: TMimeType;
    MimeTypeName: string;           // Nombre del MIME type (para mostrar)
    ResourceName: string;            // <-- AÑADIR ESTE CAMPO
    ResourceType: TResType;          // Tipo original del recurso
    ResourceTypeName: string;        // Nombre del tipo (para mostrar)
    ResourceCategory: TResourceCategory;      // Categoría (Text, Image, etc.)
    ResourceCategoryName: string;    // Nombre de la categoría
    CanDisplay: Boolean;             // Si podemos mostrarlo
    DefaultView: string;             // 'text', 'image', 'hex', etc.
    LanguageID: Word;
    ResourceIndex: Integer;
    DataOffset: Int64;
    ErrorMessage: string;
  end;

  // Resource list info - moved from uResManager
  TResListInfo = record
    Index: Integer;
    Name: string;
    ResType: TResType;
    DisplayType: string;
    Size: Int64;
    LanguageID: Integer;
    DataOffset: Int64;
    MimeType: TMimeType;
    MimeString: string;
    IsText: Boolean;
    IsImage: Boolean;
  end;
  TResListArray = array of TResListInfo;

  // Multi-resource load result - AHORA CON TODA LA INFORMACIÓN
  TMultiResLoadResult = record
    Success: Boolean;
    Count: Integer;
    Items: array of TResLoadResult;           // Cada item con toda su info
    Combined: TMemoryStream;                    // Stream combinado (para grupos)
    ImageStreams: TMemoryStreamArray;           // Streams individuales (para iconos)
    ImageMimeTypes: TMimeTypeArray;             // MIME types de cada imagen
    ImageDimensions: TStringArray;              // Dimensiones de cada imagen
    ImageCategories: TResourceCategoryArray;    // Categoría de cada imagen
    ErrorMessage: string;
  end;

  // Load mode for multi-resource formats
  TMultiResMode = (
    mrmCombined,      // Return combined/merged resource (default for icons)
    mrmSeparate,      // Return array of separate resources
    mrmFirstOnly,     // Return only the first resource
    mrmLargestOnly,   // Return only the largest resource (by size)
    mrmSmallestOnly,  // Return only the smallest resource
    mrmByIndex,       // Return specific index (requires parameter)
    mrmBySize         // Return resources matching size criteria
  );

  // Extended load parameters
  TLoadParams = record
    Name: string;
    ResType: TResType;
    Language: Word;
    MultiMode: TMultiResMode;
    SelectedIndex: Integer;     // For mrmByIndex
    MinSize: Int64;             // For filtering
    MaxSize: Int64;
    PreferredFormat: string;    // 'PNG', 'BMP', etc.
  end;

  // Load strategy for multiple resources
  TLoadStrategy = (
    lsBestGuess,      // Score and pick best match (default)
    lsFirst,          // Return first found
    lsLast,           // Return last found
    lsLargest,        // Return largest by size
    lsSmallest,       // Return smallest by size
    lsAll,            // Return all as separate items
    lsCombined,       // Return combined (for groups)
    lsByIndex,        // Return specific index
    lsByIndices       // Return multiple specific indices
  );


  // Extended load parameters
  TResourceLoadParams = record
    Name: string;                 // Resource name (mandatory)
    ResType: TResType;            // Resource type (can be empty)
    Language: Word;               // Language preference (0 = any)
    Mode: TMultiResourceMode;     // How to handle multiple resources
    Format: string;               // Preferred format for mrmBest/mrmByMime
    SelectedIndices: array of Integer;  // For mrmSelection
    MinSize: Int64;               // Minimum size filter
    MaxSize: Int64;               // Maximum size filter
  end;

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

// Funciones para categorías
function ResourceCategoryToString(Category: TResourceCategory): string;
function GetCategoryInfo(Category: TResourceCategory): TResourceCategoryInfo;

// ============================================================================
// Funciones existentes
// ============================================================================

// Helper functions
function ResType_CreateInt(Value: Integer): TResType;
function ResType_CreateStr(const Value: String): TResType;
function ResType_ToString(const ResType: TResType): String;
function ResType_AreEqual(const A, B: TResType): Boolean;
function ResType_ToDisplayString(const ResType: TResType): String;
function LanguageIDToName(ID: Word): string;

// Helper for load params
procedure SetMultiMode(var Params: TLoadParams; Mode: TMultiResMode; Index: Integer = -1);

// Helper for resource load params
function CreateLoadParams(const Name: string): TResourceLoadParams; overload;
function CreateLoadParams(const Name, Format: string): TResourceLoadParams; overload;
function CreateLoadParams(const Name, Format: string; Lang: Word): TResourceLoadParams; overload;

function CreateLoadParamsByName(const Name: string): TResourceLoadParams;
function CreateLoadParamsWithIntType(const Name: string; ResTypeInt: Integer): TResourceLoadParams;
function CreateLoadParamsWithMode(const Name: string; Mode: TMultiResourceMode): TResourceLoadParams;
function CreateLoadParamsWithFormat(const Name, Format: string): TResourceLoadParams;
function CreateLoadParamsWithLanguage(const Name: string; Lang: Word): TResourceLoadParams;
function CreateLoadParamsFull(const Name: string; const ResTypeStr: string;
  Lang: Word; Mode: TMultiResourceMode; const Format: string): TResourceLoadParams;

implementation

function LanguageIDToName(ID: Word): string;
var
  i: Integer;
begin
  for i := 0 to High(LanguageList) do
    if LanguageList[i].ID = ID then
      Exit(LanguageList[i].Name);
  Result := 'Unknown (' + IntToStr(ID) + ')';
end;


// ============================================================================
// Implementación de funciones de categoría
// ============================================================================

function ResourceCategoryToString(Category: TResourceCategory): string;
begin
  case Category of
    rcUnknown:    Result := 'Unknown';
    rcText:       Result := 'Text';
    rcImage:      Result := 'Image';
    rcAudio:      Result := 'Audio';
    rcVideo:      Result := 'Video';
    rcArchive:    Result := 'Archive';
    rcExecutable: Result := 'Executable';
    rcFont:       Result := 'Font';
    rcBinary:     Result := 'Binary';
    else          Result := 'Other';
  end;
end;

function GetCategoryInfo(Category: TResourceCategory): TResourceCategoryInfo;
begin
  Result.Category := Category;
  Result.Name := ResourceCategoryToString(Category);
  Result.CanDisplay := True;
  Result.DefaultView := 'hex';  // Default

  case Category of
    rcText:
      begin
        Result.Description := 'Text document';
        Result.CanDisplay := True;
        Result.DefaultView := 'text';
      end;

    rcImage:
      begin
        Result.Description := 'Image';
        Result.CanDisplay := True;
        Result.DefaultView := 'image';
      end;

    rcAudio:
      begin
        Result.Description := 'Audio';
        Result.CanDisplay := False;  // Todavía no
        Result.DefaultView := 'hex';
      end;

    rcVideo:
      begin
        Result.Description := 'Video';
        Result.CanDisplay := False;
        Result.DefaultView := 'hex';
      end;

    rcArchive:
      begin
        Result.Description := 'Archive/Compressed';
        Result.CanDisplay := False;
        Result.DefaultView := 'hex';
      end;

    rcExecutable:
      begin
        Result.Description := 'Executable';
        Result.CanDisplay := False;
        Result.DefaultView := 'hex';
      end;

    rcFont:
      begin
        Result.Description := 'Font';
        Result.CanDisplay := False;
        Result.DefaultView := 'hex';
      end;

    rcBinary:
      begin
        Result.Description := 'Binary data';
        Result.CanDisplay := True;
        Result.DefaultView := 'hex';
      end;

    else
      begin
        Result.Description := 'Unknown resource type';
        Result.CanDisplay := True;
        Result.DefaultView := 'hex';
      end;
  end;
end;

// ============================================================================
// Implementación de funciones existentes
// ============================================================================

function ResType_CreateInt(Value: integer): TResType;
begin
  Result.IsInteger := True;
  Result.IntValue := Value;
  Result.StrValue := '';
end;

function ResType_CreateStr(const Value: string): TResType;
begin
  Result.IsInteger := False;
  Result.IntValue := 0;
  Result.StrValue := UpperCase(Value);
end;

function ResType_ToString(const ResType: TResType): string;
begin
  if ResType.IsInteger then
    Result := IntToStr(ResType.IntValue)
  else
    Result := ResType.StrValue;
end;

function ResType_AreEqual(const A, B: TResType): boolean;
begin
  if A.IsInteger and B.IsInteger then
    Result := A.IntValue = B.IntValue
  else if (not A.IsInteger) and (not B.IsInteger) then
    Result := SameText(A.StrValue, B.StrValue)
  else
    Result := False;
end;

function ResType_ToDisplayString(const ResType: TResType): string;
begin
  if ResType.IsInteger then
  begin
    case ResType.IntValue of
      0: Result := 'ZERO';
      1: Result := 'CURSOR';
      2: Result := 'BITMAP';
      3: Result := 'ICON';
      4: Result := 'MENU';
      5: Result := 'DIALOG';
      6: Result := 'STRING';
      7: Result := 'FONTDIR';
      8: Result := 'FONT';
      9: Result := 'ACCELERATOR';
      10: Result := 'RCDATA';
      11: Result := 'MESSAGETABLE';
      12: Result := 'GROUP_CURSOR';
      14: Result := 'GROUP_ICON';
      16: Result := 'VERSION';
      17: Result := 'DLGINCLUDE';
      19: Result := 'PLUGPLAY';
      20: Result := 'VXD';
      21: Result := 'ANICURSOR';
      22: Result := 'ANIICON';
      23: Result := 'HTML';
      24: Result := 'MANIFEST';
      240: Result := 'DLGINIT';
      else
        Result := 'TYPE_' + IntToStr(ResType.IntValue);
    end;
  end
  else
  begin
    Result := ResType.StrValue;
  end;
end;

// Helper functions for load params
function CreateLoadParams(const Name: string): TResourceLoadParams;
begin
  Result := CreateLoadParamsByName(Name);
end;

function CreateLoadParams(const Name, Format: string): TResourceLoadParams;
begin
  Result := CreateLoadParamsByName(Name);
  Result.Format := Format;
  Result.Mode := mrmBest;
end;

function CreateLoadParams(const Name, Format: string; Lang: word): TResourceLoadParams;
begin
  Result := CreateLoadParamsByName(Name);
  Result.Format := Format;
  Result.Language := Lang;
  Result.Mode := mrmBest;
end;

procedure SetMultiMode(var Params: TLoadParams; Mode: TMultiResMode; Index: integer = -1);
begin
  Params.MultiMode := Mode;
  Params.SelectedIndex := Index;
end;

//------------------------------------------------------------------------------
// Helper functions for resource load parameters
//------------------------------------------------------------------------------

function CreateLoadParamsByName(const Name: string): TResourceLoadParams;
begin
  Result.Name := Name;
  Result.ResType.IsInteger := False;
  Result.ResType.StrValue := '';
  Result.Language := 0;
  Result.Mode := mrmAll;
  Result.Format := '';
  SetLength(Result.SelectedIndices, 0);
  Result.MinSize := 0;
  Result.MaxSize := 0;
end;

function CreateLoadParamsWithIntType(const Name: string; ResTypeInt: Integer): TResourceLoadParams;
begin
  Result := CreateLoadParamsByName(Name);
  Result.ResType := ResType_CreateInt(ResTypeInt);
end;

function CreateLoadParamsWithMode(const Name: string; Mode: TMultiResourceMode): TResourceLoadParams;
begin
  Result := CreateLoadParamsByName(Name);
  Result.Mode := Mode;
end;

function CreateLoadParamsWithFormat(const Name, Format: string): TResourceLoadParams;
begin
  Result := CreateLoadParamsByName(Name);
  Result.Format := Format;
  Result.Mode := mrmBest;  // Default to best match when format specified
end;

function CreateLoadParamsWithLanguage(const Name: string; Lang: Word): TResourceLoadParams;
begin
  Result := CreateLoadParamsByName(Name);
  Result.Language := Lang;
end;

function CreateLoadParamsFull(const Name: string; const ResTypeStr: string;
  Lang: Word; Mode: TMultiResourceMode; const Format: string): TResourceLoadParams;
begin
  Result := CreateLoadParamsByName(Name);
  Result.ResType := ResType_CreateStr(ResTypeStr);
  Result.Language := Lang;
  Result.Mode := Mode;
  Result.Format := Format;
end;

end.
