unit uResOperations;

{$mode objfpc}{$H+}

{===============================================================================
  Resource Operations - Unified API for Windows .RES file management
  ===============================================================================

  This unit provides a clean, consistent interface for all resource operations.
  It acts as a facade over the lower-level units (uResManager, uResFileIO, etc.)
  and should be the ONLY unit you need to use in your applications.

  ------------------------------------------------------------------------------
  DATA STRUCTURES & STORAGE
  ------------------------------------------------------------------------------

  TResInfo - Information about a resource (read-only, for queries)
  =====================
    Name: string          - Resource name (e.g., 'MAINICON', '1', 'README')
    ResType: TResType     - Resource type (can be integer or string)
    Size: Int64           - Size of resource data in bytes
    LanguageID: Integer   - Language identifier (0 = neutral)
    DataOffset: Int64     - Offset in the original file (do not modify)
    DataSize: Int64       - Same as Size (for compatibility)
    DataVersion: DWord    - Version info from header
    MemoryFlags: Word     - Memory flags from header
    VersionInfo: DWord    - Version field from header
    Characteristics: DWord - Characteristics from header

  TResType - Resource type representation
  =====================
    IsInteger: Boolean    - True if type is an integer ID
    IntValue: Integer     - Value when IsInteger = True
    StrValue: string      - Value when IsInteger = False

  Standard integer resource types (use constants RT_xxx):
    1  = CURSOR           2  = BITMAP          3  = ICON
    4  = MENU             5  = DIALOG          6  = STRING
    7  = FONTDIR          8  = FONT            9  = ACCELERATOR
    10 = RCDATA           11 = MESSAGETABLE    12 = GROUP_CURSOR
    14 = GROUP_ICON       16 = VERSION         17 = DLGINCLUDE
    19 = PLUGPLAY         20 = VXD             21 = ANICURSOR
    22 = ANIICON          23 = HTML            24 = MANIFEST

  ------------------------------------------------------------------------------
  STREAMS - What you get from LoadResource
  ------------------------------------------------------------------------------

  ResOp_LoadResource returns an array of TMemoryStream. The format of each stream
  depends on the resource type and the Mode parameter:

  Mode = mrmRejoined (for groups):
    - Icon groups (RT_GROUP_ICON): Complete .ico file (ICONDIR + images)
    - Cursor groups (RT_GROUP_CURSOR): Complete .cur file
    - Single resource: The raw resource data in its original format

  Mode = mrmImage, mrmAll, etc. (for individual images from groups):
    - Each stream is a complete, displayable image file:
      * ICO files: Valid .ico file with its own header
      * PNG files: Valid .png file
      * JPEG files: Valid .jpg file
      * GIF files: Valid .gif file
      * BMP files: Valid .bmp file (with BITMAPFILEHEADER)

  Mode = mrmText, mrmAll (for text resources):
    - Text resources (RT_HTML, RT_MANIFEST, RT_RCDATA with text):
      * UTF-8 encoded text (may include BOM)
      * Usually null-terminated
      * Can be loaded directly into TMemo or similar

  Mode = mrmAll (for other resources):
    - Raw binary data exactly as stored in the .res file
    - Format depends on the specific resource type

  IMPORTANT: You are responsible for freeing the streams after use!

  Example:
    Streams := nil;
    if ResOp_LoadResource('MAINICON', ResOp_TypeFromInt(RT_GROUP_ICON),
                          mrmImage, Streams) then
    try
      for I := 0 to High(Streams) do
        if Streams[I] <> nil then
          Image1.Picture.LoadFromStream(Streams[I]);
    finally
      for I := 0 to High(Streams) do
        Streams[I].Free;
    end;

  ------------------------------------------------------------------------------
  TEXT RESOURCES - Special handling
  ------------------------------------------------------------------------------

  Text resources (HTML, MANIFEST, XML, etc.) are stored as UTF-8 with an
  optional BOM (EF BB BF). When you load them:

  - The stream contains the raw bytes including BOM if present
  - To display in a TMemo, you can either:
    * Use TEncoding.UTF8.GetString(Stream.Memory, Stream.Size)
    * Or load directly if the memo supports UTF-8

  Example for text resources:
    if ResOp_LoadResource('README', ResOp_TypeFromInt(RT_HTML),
                          mrmAll, Streams) then
    begin
      SetString(Text, PAnsiChar(Streams[0].Memory), Streams[0].Size);
      Memo1.Text := Text;
    end;

  ------------------------------------------------------------------------------
  IMAGE RESOURCES - Display
  ------------------------------------------------------------------------------

  Images returned by LoadResource are always in a format that TImage can load
  directly. For icon groups loaded with mrmImage, you get multiple streams,
  one for each image size/depth in the group.

  Example for images:
    if ResOp_LoadResource('MAINICON', ResOp_TypeFromInt(RT_GROUP_ICON),
                          mrmImage, Streams) then
    begin
      // Streams[0] is the first image (usually 16x16)
      // Streams[1] is the second image (usually 32x32), etc.
      Image1.Picture.LoadFromStream(Streams[0]);
    end;

  ------------------------------------------------------------------------------
  QUICK START - Basic Usage:
  ------------------------------------------------------------------------------

  // 1. Initialize the system
  if not ResOp_Initialize then
    ShowMessage('Failed to initialize');

  // 2. Open a resource file
  if ResOp_OpenFile('myfile.res') then
  begin
    // 3. Get list of resources
    Resources := ResOp_GetAllResources;

    // 4. Load a specific resource
    Streams := nil;
    if ResOp_LoadResource('BITMAP1', ResOp_TypeFromInt(2), mrmAll, Streams) then
      // Use the stream(s)

    // 5. Save changes
    ResOp_SaveFile;  // Saves to the original file
    // or
    ResOp_SaveFile('newfile.res');  // Saves to a new file
  end;

  // 6. Shutdown when done
  ResOp_Shutdown;

  ------------------------------------------------------------------------------
  ERROR HANDLING:
  ------------------------------------------------------------------------------
  All functions return Boolean for success/failure. On failure, call
  ResOp_GetLastError to get the error message.

  Example:
    if not ResOp_OpenFile('test.res') then
      ShowMessage('Error: ' + ResOp_GetLastError);

  ------------------------------------------------------------------------------
  EXPORT PARAMETERS:
  ------------------------------------------------------------------------------
  The ExportResource functions accept a Param parameter that controls dialogs:

    EXPORT_DIALOG_ALL   = 0  - Show all dialogs (ask for filename, confirm overwrite)
    EXPORT_NO_OVERWRITE = 1  - Don't ask for overwrite confirmation
    EXPORT_NO_NAME      = 2  - Don't ask for filename (use default name in exports/)
    EXPORT_NO_MULTI     = 3  - Don't ask for multiple choices (auto-select all)
    EXPORT_SILENT       = 4  - No dialogs at all, auto-save with defaults

  ------------------------------------------------------------------------------
  MAIN FUNCTIONS SUMMARY:
  ------------------------------------------------------------------------------

  --- FILE MANAGEMENT ----------------------------------------------------------

  ResOp_Initialize: Boolean
    Initialize the resource system. Must be called before any other operations.
    Example: if not ResOp_Initialize then Exit;

  ResOp_Shutdown: Boolean
    Clean shutdown of the resource system. Frees all internal structures.
    Example: ResOp_Shutdown;

  ResOp_OpenFile(const FileName: string): Boolean
    Open a .res file and parse all resources.
    Example: if ResOp_OpenFile('resources.res') then ...

  ResOp_CloseFile: Boolean
    Close the currently open file without saving.
    Example: ResOp_CloseFile;

  ResOp_SaveFile(const FileName: string = ''): Boolean
    Save current resources to file. If FileName is empty, saves to original.
    Example: ResOp_SaveFile;                     // Save to original
             ResOp_SaveFile('backup.res');       // Save as new file

  ResOp_GetFileName: string
    Get the name of the currently open file.
    Example: ShowMessage('Editing: ' + ResOp_GetFileName);


  --- RESOURCE QUERIES ---------------------------------------------------------

  ResOp_GetResourceCount: Integer
    Return the number of resources in the current file.
    Example: Count := ResOp_GetResourceCount;

  ResOp_GetAllResources: TResListArray
    Return array of all resources with basic information.
    Example: Resources := ResOp_GetAllResources;

  ResOp_GetTextResources: TResListArray
    Return array of all text resources (HTML, MANIFEST, etc.).
    Example: TextRes := ResOp_GetTextResources;

  ResOp_GetImageResources: TResListArray
    Return array of all image resources (icons, bitmaps, etc.).
    Example: ImageRes := ResOp_GetImageResources;

  ResOp_FindResource(const Name: string; const ResType: TResType): Integer
    Find resource index by name and type. Returns -1 if not found.
    Example: Idx := ResOp_FindResource('MAINICON', ResOp_TypeFromInt(14));

  ResOp_GetResourceInfo(Index: Integer; out Info: TResInfo): Boolean
    Get detailed information about a resource by its index.
    Example: if ResOp_GetResourceInfo(5, Info) then ...


  --- RESOURCE OPERATIONS ------------------------------------------------------

  ResOp_LoadResource(const Params: array of const): TMultiResLoadResult
    Load resource data with full control. See detailed documentation above.
    Example: Result := ResOp_LoadResource(['MAINICON', '14', 10, '', 0]);

  ResOp_RenameResource(Index: Integer; const NewName: string;
    AutoSave: Boolean = False): Boolean
    Change a resource's name. If AutoSave=True, saves immediately.
    Example: ResOp_RenameResource(3, 'NEWNAME', True);

  ResOp_ChangeResourceType(Index: Integer; const NewType: TResType;
    AutoSave: Boolean = False): Boolean
    Change a resource's type. If AutoSave=True, saves immediately.
    Example: ResOp_ChangeResourceType(3, ResOp_TypeFromInt(23), True);

  ResOp_DeleteResource(Index: Integer; AutoSave: Boolean = False): Boolean
    Delete a resource. If AutoSave=True, saves immediately.
    Example: ResOp_DeleteResource(3, True);

  ResOp_AddResource(const Name: string; const ResType: TResType;
    Data: TMemoryStream; LanguageID: Word = 0; AutoSave: Boolean = False): Integer
    Add a new resource from a stream. Returns new index or -1 on error.
    Example: NewIdx := ResOp_AddResource('NEW', ResOp_TypeFromInt(10),
               MyStream, 0, True);

  ResOp_ImportResource(const FileName: string; const ResourceName: string;
    const ResType: TResType; LanguageID: Word = 0; AutoSave: Boolean = False): Boolean
    Import a resource from an external file.
    Example: ResOp_ImportResource('image.png', 'LOGO',
               ResOp_TypeFromInt(2), 0, True);

  ResOp_ExportResource(const ResourceName, ResTypeName: string;
    Param: Integer): Boolean
    Export a resource to an external file. Param controls dialogs:
      0 = Show all dialogs
      1 = Don't ask for overwrite
      2 = Don't ask for filename (use default)
      3 = Don't ask for multiple choices
      4 = No dialogs at all
    Example: ResOp_ExportResource('MAINICON', '14', 2);  // Quick export

  ResOp_ExportResToFile(const ResourceName, ResTypeName, DestFile: string): Boolean;
    Exports a resource to an external file without any user interaction.
    Returns True on success, False on failure. Use ResOp_GetLastError to retrieve the error message.
    Note: For resources that consist of multiple images (e.g., icon groups), this function
          will export only the first image. For full control over groups, use ResOp_ExportResource.

    Example:
      if ResOp_ExportResToFile('DOCUMENTATION_CSS', 'CSS', 'documentation.css') then
        WriteLn('Exported successfully')
      else
        WriteLn('Error: ', ResOp_GetLastError);

  --- UTILITIES ----------------------------------------------------------------

  ResOp_GetLastError: string
    Get the last error message from any failed operation.
    Example: ShowMessage('Error: ' + ResOp_GetLastError);

  ResOp_TypeFromInt(Value: Integer): TResType
    Create a TResType from an integer ID.
    Example: MyType := ResOp_TypeFromInt(14);  // GROUP_ICON

  ResOp_TypeFromString(const Value: string): TResType
    Create a TResType from a string name.
    Example: MyType := ResOp_TypeFromString('HTML');

  ResOp_TypeToString(const ResType: TResType): string
    Convert a TResType to its string representation (ID or name).
    Example: ShowMessage(ResOp_TypeToString(MyType));

  ResOp_ValidateResourceName(const Name: string; const ResType: TResType): Boolean
    Check if a resource name is valid (no duplicates, correct format).
    Example: if ResOp_ValidateResourceName('NEW', MyType) then ...

  ResOp_FindFreeOrdinalName(const BaseName: string; ResTypeInt: Integer): string
    Find an unused ordinal name for a given resource type.
    Example: NewName := ResOp_FindFreeOrdinalName('ICON', 3);


  --- DEBUGGING ----------------------------------------------------------------

  ResOp_SetDebugLevel(Level: TDebugLevel)
    Set debug output level (dlNone, dlError, dlWarning, dlInfo, dlDetail).
    Example: ResOp_SetDebugLevel(dlDetail);

  ResOp_GetDebugLevel: TDebugLevel
    Get current debug level.
    Example: if ResOp_GetDebugLevel = dlDetail then ...

  =============================================================================}

interface

uses
  uResDefs, uResManager, uResFileIO, uResFile, uMimeDetect, uDebug,
  Classes, SysUtils;

  //==============================================================================
  // CONSTANTS
  //==============================================================================

const
  // Parameter constants for export operations
  EXPORT_DIALOG_ALL = 0;  // Show all dialogs
  EXPORT_NO_OVERWRITE = 1;  // Don't ask for overwrite
  EXPORT_NO_NAME = 2;  // Don't ask for filename (use default)
  EXPORT_NO_MULTI = 3;  // Don't ask for multiple choices
  EXPORT_SILENT = 4;  // No dialogs at all

  // Resource type constants (for convenience)
  RT_CURSOR = 1;
  RT_BITMAP = 2;
  RT_ICON = 3;
  RT_MENU = 4;
  RT_DIALOG = 5;
  RT_STRING = 6;
  RT_FONTDIR = 7;
  RT_FONT = 8;
  RT_ACCELERATOR = 9;
  RT_RCDATA = 10;
  RT_MESSAGETABLE = 11;
  RT_GROUP_CURSOR = 12;
  RT_GROUP_ICON = 14;
  RT_VERSION = 16;
  RT_DLGINCLUDE = 17;
  RT_PLUGPLAY = 19;
  RT_VXD = 20;
  RT_ANICURSOR = 21;
  RT_ANIICON = 22;
  RT_HTML = 23;
  RT_MANIFEST = 24;

  //==============================================================================
  // TYPE ALIASES (for external use)
  //==============================================================================

type
  PResourceInfo = ^TResInfo;
  TResourceArray = array of TResInfo;
  TResourceCategory = uResDefs.TResourceCategory;
  TMultiResourceMode = uResDefs.TMultiResourceMode;

  //==============================================================================
  // INITIALIZATION & FILE MANAGEMENT
  //==============================================================================

// Initialize the resource system
function ResOp_Initialize: boolean;

// Shutdown the resource system and free resources
function ResOp_Shutdown: boolean;

// Open a resource file
function ResOp_OpenFile(const FileName: string): boolean;

// Close the current file (without saving)
function ResOp_CloseFile: boolean;

// Save the current file to disk (with optional auto-naming)
function ResOp_SaveFile(const FileName: string = ''): boolean;

// Get the name of the currently open file
function ResOp_GetFileName: string;

// Free resources
procedure ResOp_FreeResourceData(var Data: TMultiResLoadResult);

//==============================================================================
// RESOURCE QUERIES
//==============================================================================

// Get total resource count
function ResOp_GetResourceCount: integer;

// Get all resources as an array
function ResOp_GetAllResources: TResListArray;

// Get resources by category
function ResOp_GetTextResources: TResListArray;
function ResOp_GetImageResources: TResListArray;
function ResOp_GetAudioResources: TResListArray;
function ResOp_GetVideoResources: TResListArray;

// Find resource by name and type
function ResOp_FindResource(const Name: string; const ResType: TResType): integer;

// Get resource information by index
function ResOp_GetResourceInfo(Index: integer; out Info: TResInfo): boolean;

//==============================================================================
// RESOURCE OPERATIONS
//==============================================================================

// Load resource data into a stream (use mode to control grouping)
function ResOp_LoadResource(const Params: array of const): TMultiResLoadResult;

// Rename resource
function ResOp_RenameResource(Index: integer; const NewName: string; AutoSave: boolean = False): boolean;

// Update resource type
function ResOp_ChangeResourceType(Index: integer; const NewType: TResType; AutoSave: boolean = False): boolean;

// Delete a resource
function ResOp_DeleteResource(Index: integer; AutoSave: boolean = False): boolean;

// Add a new resource from a stream
function ResOp_AddResource(const Name: string; const ResType: TResType; Data: TMemoryStream; LanguageID: word = 0; AutoSave: boolean = False): integer;

// Import resource from external file
function ResOp_ImportResource(const FileName: string; const ResourceName: string; const ResType: TResType; LanguageID: word = 0; AutoSave: boolean = False): boolean;

//==============================================================================
// EXPORT RESOURCE - Overloaded versions for convenience
//==============================================================================

function ResOp_ExportResource(const ResourceName, ResTypeName: string; Param: integer): boolean;

function ResOp_ExportResToFile(const ResourceName, ResTypeName, DestFile: string): Boolean;

//==============================================================================
// UTILITY FUNCTIONS
//==============================================================================

// Get last error message
function ResOp_GetLastError: string;

// Convert between type formats
function ResOp_TypeFromInt(Value: integer): TResType;
function ResOp_TypeFromString(const Value: string): TResType;
function ResOp_TypeToString(const ResType: TResType): string;

// Validate resource name (checks duplicates and format)
function ResOp_ValidateResourceName(const Name: string; const ResType: TResType): boolean;

// Find a free ordinal name for a given type
function ResOp_FindFreeOrdinalName(const BaseName: string; ResTypeInt: integer): string;

//==============================================================================
// DEBUGGING
//==============================================================================

procedure ResOp_SetDebugLevel(Level: TDebugLevel);
function ResOp_GetDebugLevel: TDebugLevel;

implementation

uses
  Math;

var
  LastError: string = '';

  //==============================================================================
  // Helper functions
  //==============================================================================

procedure SetLastError(const Msg: string);
begin
  LastError := Msg;
  DebugError('ResOp: ' + Msg);
end;

//==============================================================================
// INITIALIZATION & FILE MANAGEMENT
//==============================================================================

function ResOp_Initialize: boolean;
begin
  try
    ResMgr_Init;
    Result := True;
  except
    on E: Exception do
    begin
      SetLastError('Initialize failed: ' + E.Message);
      Result := False;
    end;
  end;
end;

function ResOp_Shutdown: boolean;
begin
  try
    ResMgr_Done;
    Result := True;
  except
    on E: Exception do
    begin
      SetLastError('Shutdown failed: ' + E.Message);
      Result := False;
    end;
  end;
end;

function ResOp_OpenFile(const FileName: string): boolean;
begin
  Result := ResMgr_LoadFile(FileName);
  if not Result then
    SetLastError(ResMgr_GetErrorString);
end;

function ResOp_CloseFile: boolean;
begin
  DebugInfo('CLOSING FILE: ');
  Result := ResFile_Close(ResMgr_FileStream);
  ResMgr_FileName := '';
end;

function ResOp_SaveFile(const FileName: string = ''): boolean;
var
  SaveName: string;
begin
  if FileName <> '' then
    SaveName := FileName
  else if ResMgr_FileName <> '' then
    SaveName := ResMgr_FileName
  else
  begin
    SetLastError('No filename specified');
    Exit(False);
  end;
  Result := TResFileIO.SaveResourceFile(SaveName);
  if Result then
    ResMgr_FileName := SaveName
  else
    SetLastError('Save failed');
end;

function ResOp_GetFileName: string;
begin
  Result := ResMgr_FileName;
end;

procedure ResOp_FreeResourceData(var Data: TMultiResLoadResult);
var
  i, j: Integer;
  p: Pointer;
  AlreadyFreed: Boolean;
  StreamPtrs: array of Pointer;
begin
  SetLength(StreamPtrs, Length(Data.ImageStreams));
  for i := 0 to High(Data.ImageStreams) do
    StreamPtrs[i] := Pointer(Data.ImageStreams[i]);

  // Liberar ImageStreams (ahora ya tenemos las direcciones guardadas)
  for i := 0 to High(Data.ImageStreams) do
    if Data.ImageStreams[i] <> nil then
    begin
      Data.ImageStreams[i].Free;
      Data.ImageStreams[i] := nil;
    end;

  // Liberar Items, evitando los que están en StreamPtrs
  for i := 0 to High(Data.Items) do
    if Data.Items[i].Data <> nil then
    begin
      AlreadyFreed := False;
      for j := 0 to High(StreamPtrs) do
        if Pointer(Data.Items[i].Data) = StreamPtrs[j] then
        begin
          AlreadyFreed := True;
          Break;
        end;
      if not AlreadyFreed then
        Data.Items[i].Data.Free;
      Data.Items[i].Data := nil;
    end;

  // 3. Liberar Combined
  if Data.Combined <> nil then
  begin
    Data.Combined.Free;
    Data.Combined := nil;
  end;

  // 4. Reinicializar toda la estructura
  Data.Success := False;
  Data.Count := 0;
  Data.ErrorMessage := '';
  SetLength(Data.Items, 0);
  SetLength(Data.ImageStreams, 0);
  SetLength(Data.ImageMimeTypes, 0);
  SetLength(Data.ImageDimensions, 0);
  SetLength(Data.ImageCategories, 0);
end;

//==============================================================================
// RESOURCE QUERIES
//==============================================================================

function ResOp_GetResourceCount: integer;
begin
  Result := ResMgr_ResourceCount;
end;

function ResOp_GetAllResources: TResListArray;
begin
  Result := ResMgr_GetAllResources;
end;

function ResOp_GetTextResources: TResListArray;
begin
  Result := ResMgr_GetTextResources;
end;

function ResOp_GetImageResources: TResListArray;
begin
  Result := ResMgr_GetImageResources;
end;

function ResOp_GetAudioResources: TResListArray;
begin
  // TODO: Implement when ResMgr_GetAudioResources exists
  SetLength(Result, 0);
end;

function ResOp_GetVideoResources: TResListArray;
begin
  // TODO: Implement when ResMgr_GetVideoResources exists
  SetLength(Result, 0);
end;

function ResOp_FindResource(const Name: string; const ResType: TResType): integer;
var
  I: integer;
begin
  Result := -1;
  for I := 0 to ResMgr_ResourceCount - 1 do
  begin
    if (ResMgr_Resources[I].Name = Name) and ResType_AreEqual(ResMgr_Resources[I].ResType, ResType) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function ResOp_GetResourceInfo(Index: integer; out Info: TResInfo): boolean;
begin
  Result := False;
  if (Index >= 0) and (Index < ResMgr_ResourceCount) then
  begin
    Info := ResMgr_Resources[Index];
    Result := True;
  end
  else
    SetLastError('Index out of range');
end;

//==============================================================================
// RESOURCE OPERATIONS
//==============================================================================
{===============================================================================
  ResOp_LoadResource - Load resources with full metadata
  ==============================================================================

  This function is a direct wrapper for ResMgr_LoadResource. It accepts the same
  parameters and returns the same TMultiResLoadResult structure.

  ------------------------------------------------------------------------------
  PARAMETERS (array of const)
  ------------------------------------------------------------------------------

  Format: [Name, ResType, Mode, FormatPref, Language, Indices...]

  Name (string) - Required
    Resource name to search for. Can be a textual name or a numeric ID as string
    (e.g., '1', 'MAINICON').

  ResType (string) - Optional
    Resource type. Can be:
    - Number as string: '3' (ICON), '14' (GROUP_ICON), etc.
    - Textual name: 'ICON', 'HTML', 'MANIFEST', etc.
    - Empty '' for any type

  Mode (integer) - Optional, default = 0 (mrmAll)
    Selection mode when multiple resources match:

    0 = mrmAll       - Return all matching resources
    1 = mrmBest      - Best match according to preferred format
    2 = mrmFirst     - Return only the first resource
    3 = mrmLast      - Return only the last resource
    4 = mrmLargest   - Return the largest resource (by size)
    5 = mrmSmallest  - Return the smallest resource
    6 = mrmSelection - Return specific resources by index (requires indices)
    7 = mrmByType    - Filter by specific type
    8 = mrmByMime    - Filter by MIME type
    9 = mrmText      - Return only text resources
   10 = mrmImage     - Return only images
   11 = mrmAudio     - Return only audio
   12 = mrmVideo     - Return only video
   13 = mrmArchive   - Return only compressed archives
   14 = mrmExecutable - Return only executables
   15 = mrmRejoined  - Return complete group file (for icons/cursors)

  FormatPref (string) - Optional
    Preferred format for mrmBest/mrmByMime:
    'TEXT', 'IMAGE', 'HTML', 'PNG', 'JPEG', 'ICO', etc.

  Language (integer) - Optional, default = 0
    Language ID to filter (0 = any language)

  Indices... (integer) - Optional
    For mrmSelection, list of indices to select.
    Example: [..., 0, 2, 5] selects resources at positions 0, 2 and 5

  ------------------------------------------------------------------------------
  RETURN STRUCTURE: TMultiResLoadResult
  ------------------------------------------------------------------------------

  The function returns a record with the following information:

  Success: Boolean              - True if operation succeeded
  Count: Integer                - Number of loaded resources
  ErrorMessage: string          - Error message if Success = False

  Items: array of TResLoadResult - Detailed information for each resource
    Each TResLoadResult contains:

    [DATA]
      Data: TMemoryStream        - Stream with resource data (MUST FREE!)
      Size: Int64                - Size in bytes

    [IDENTIFICATION]
      ResourceName: string       - Resource name
      ResourceIndex: Integer     - Index in original file
      ResourceType: TResType     - Original type (integer or string)
      ResourceTypeName: string   - Type name ('ICON', 'HTML', etc.)

    [CLASSIFICATION]
      MimeType: TMimeType        - Detected MIME type
      MimeTypeName: string       - MIME type name ('PNG', 'JPEG', etc.)
      ResourceCategory: TResourceCategory - Category (Text, Image, Binary...)
      ResourceCategoryName: string - Category name

    [DISPLAY]
      CanDisplay: Boolean        - Whether it can be displayed directly
      DefaultView: string        - Suggested view: 'text', 'image', 'hex'

    [METADATA]
      LanguageID: Word           - Language ID
      DataOffset: Int64          - Offset in original file

  Combined: TMemoryStream - Combined stream (for groups with mrmRejoined)

  ImageStreams: TMemoryStreamArray - Individual streams (for icons/cursors)
  ImageMimeTypes: TMimeTypeArray    - MIME types for each image
  ImageDimensions: TStringArray     - Dimensions '16x16', '32x32', etc.
  ImageCategories: TResourceCategoryArray - Categories for each image

  ------------------------------------------------------------------------------
  USAGE EXAMPLES
  ------------------------------------------------------------------------------

  // 1. Load a specific resource by name and type
  Result := ResOp_LoadResource(['MAINICON', '14', 0, '', 0]);

  // 2. Load all images from a group
  Result := ResOp_LoadResource(['MAINICON', '14', 10, 'IMAGE', 0]);  // mrmImage = 10
  for I := 0 to High(Result.ImageStreams) do
    Image1.Picture.LoadFromStream(Result.ImageStreams[I]);

  // 3. Load the best text resource
  Result := ResOp_LoadResource(['README', '', 1, 'TEXT', 0]);  // mrmBest = 1

  // 4. Load specific resources by index
  Result := ResOp_LoadResource(['ICON', '3', 6, '', 0, 1, 3, 5]); // mrmSelection = 6

  // 5. Load a complete group as an ICO file
  Result := ResOp_LoadResource(['MAINICON', '14', 15, '', 0]); // mrmRejoined = 15
  Result.Combined.SaveToFile('icon.ico');

  ------------------------------------------------------------------------------
  IMPORTANT: MEMORY MANAGEMENT
  ------------------------------------------------------------------------------

  Returned streams (Items[I].Data, Combined, ImageStreams) are NOT freed
  automatically. You must free them when no longer needed:

  for I := 0 to Result.Count - 1 do
    if Result.Items[I].Data <> nil then
      Result.Items[I].Data.Free;

  if Result.Combined <> nil then
    Result.Combined.Free;

  for I := 0 to High(Result.ImageStreams) do
    if Result.ImageStreams[I] <> nil then
      Result.ImageStreams[I].Free;

  =============================================================================}

function ResOp_LoadResource(const Params: array of const): TMultiResLoadResult;
begin
  Result := ResMgr_LoadResource(Params);
  if not Result.Success then
    SetLastError(Result.ErrorMessage);
end;

function ResOp_RenameResource(Index: integer; const NewName: string; AutoSave: boolean = False): boolean;
begin
  Result := False;

  if (Index < 0) or (Index >= ResMgr_ResourceCount) then
  begin
    SetLastError('Index out of range');
    Exit;
  end;

  if not ResOp_ValidateResourceName(NewName, ResMgr_Resources[Index].ResType) then
  begin
    SetLastError('Invalid resource name');
    Exit;
  end;

  ResMgr_Resources[Index].Name := NewName;
  Result := True;

  if AutoSave then
    Result := ResOp_SaveFile;
end;

function ResOp_ChangeResourceType(Index: integer; const NewType: TResType; AutoSave: boolean = False): boolean;
begin
  Result := False;

  if (Index < 0) or (Index >= ResMgr_ResourceCount) then
  begin
    SetLastError('Index out of range');
    Exit;
  end;

  ResMgr_Resources[Index].ResType := NewType;
  Result := True;

  if AutoSave then
    Result := ResOp_SaveFile;
end;

function ResOp_DeleteResource(Index: Integer; AutoSave: Boolean = False): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Index < 0) or (Index >= ResMgr_ResourceCount) then
  begin
    SetLastError('Index out of range');
    Exit;
  end;

  // Liberar el stream en memoria si existe
  if ResMgr_Resources[Index].Data <> nil then
    ResMgr_Resources[Index].Data.Free;

  // Mover todos los recursos una posición hacia atrás
  for I := Index to ResMgr_ResourceCount - 2 do
    ResMgr_Resources[I] := ResMgr_Resources[I + 1];

  SetLength(ResMgr_Resources, ResMgr_ResourceCount - 1);
  ResMgr_ResourceCount := ResMgr_ResourceCount - 1;
  Result := True;

  if AutoSave then
    Result := ResOp_SaveFile;
end;

function ResOp_AddResource(const Name: string; const ResType: TResType; Data: TMemoryStream; LanguageID: word = 0; AutoSave: boolean = False): integer;
var
  NewIndex: integer;
    NewData: TMemoryStream;  // <-- Declarar aquí
begin
  DebugInfo(Format('[ResOp] AddResource: Name="%s", Type="%s", Lang=%d, DataSize=%d',
    [Name, ResType_ToString(ResType), LanguageID, Data.Size]));
  Result := -1;
  if Data = nil then
  begin
    SetLastError('Data stream is nil');
    Exit;
  end;

  if not ResOp_ValidateResourceName(Name, ResType) then
  begin
    SetLastError('Invalid resource name');
    Exit;
  end;

  NewIndex := ResMgr_ResourceCount;
  SetLength(ResMgr_Resources, NewIndex + 1);

  // Copiar los datos
  NewData := TMemoryStream.Create;
  Data.Position := 0;
  NewData.CopyFrom(Data, Data.Size);
  NewData.Position := 0;

  // Rellenar campos
  ResMgr_Resources[NewIndex].Name := Name;
  ResMgr_Resources[NewIndex].ResType := ResType;
  ResMgr_Resources[NewIndex].Size := Data.Size;
  ResMgr_Resources[NewIndex].LanguageID := LanguageID;
  ResMgr_Resources[NewIndex].DataOffset := 0;  // Indicador de recurso en memoria
  ResMgr_Resources[NewIndex].DataSize := Data.Size;
  ResMgr_Resources[NewIndex].DataVersion := 0;
  ResMgr_Resources[NewIndex].MemoryFlags := 0;
  ResMgr_Resources[NewIndex].VersionInfo := 0;
  ResMgr_Resources[NewIndex].Characteristics := 0;
  ResMgr_Resources[NewIndex].Data := NewData;

  ResMgr_ResourceCount := NewIndex + 1;
  Result := NewIndex;

  if AutoSave then
    ResOp_SaveFile;
end;

function ResOp_ImportResource(const FileName: string; const ResourceName: string; const ResType: TResType; LanguageID: word; AutoSave: boolean = False): boolean;
var
  DataStream: TMemoryStream;
  Idx: integer;
begin
  Result := False;
  DebugInfo(Format('[ResOp] ImportResource: File="%s", Name="%s", Type="%s", Lang=%d', [FileName, ResourceName, ResType_ToString(ResType), LanguageID]));
  if not FileExists(FileName) then
  begin
    SetLastError('File not found: ' + FileName);
    Exit;
  end;

  DataStream := TMemoryStream.Create;
  try
    DataStream.LoadFromFile(FileName);
    if DataStream.Size = 0 then
    begin
      SetLastError('File is empty: ' + FileName);
      Exit;
    end;
    DataStream.Position := 0;
    Idx := ResOp_AddResource(ResourceName, ResType, DataStream, LanguageID, AutoSave);
    Result := Idx >= 0;
    if Result then
      DebugInfo('[ResOp] ImportResource: Success')
    //    if not Result then
    else
    begin
      SetLastError('Failed to add resource');
      DebugError('[ResOp] ImportResource: Failed: ' + ResOp_GetLastError);
    end;
  except
    on E: Exception do
      SetLastError('Stream error: ' + E.Message);
  end;
  DataStream.Free;
end;

function ResOp_ExportResource(const ResourceName, ResTypeName: string; Param: integer): boolean;
begin
  Result := TResFileIO.ExportResource(ResourceName, ResTypeName, Param);
  if not Result then
    SetLastError('Export failed');
end;

function ResOp_ExportResToFile(const ResourceName, ResTypeName, DestFile: string): Boolean;
var
  PreparedStream: TMemoryStream;
  MimeType: TMimeType;
begin
  Result := False;
  PreparedStream := TResFileIO.PrepareResourceForSave(ResourceName, ResTypeName, 4); // Modo silencioso
  if PreparedStream = nil then
  begin
    SetLastError('Resource cannot be exported as a single file (maybe a group with multiple images)');
    Exit;
  end;
  try
    // Detectar si es texto y eliminar el posible null terminator sobrante
    PreparedStream.Position := 0;
    MimeType := TMimeDetector.DetectFromStream(PreparedStream);
    if MimeType in [mtText, mtHtml, mtXml, mtJson, mtCss, mtJavaScript, mtRtf] then
    begin
      if PreparedStream.Size > 0 then
      begin
        PreparedStream.Position := PreparedStream.Size - 1;
        if PreparedStream.ReadByte = 0 then
        begin
          // Recortar el stream eliminando el último byte
          PreparedStream.Size := PreparedStream.Size - 1;
        end;
      end;
    end;
    // Guardar el archivo (sin el null)
    PreparedStream.Position := 0;
    PreparedStream.SaveToFile(DestFile);
    Result := True;
  except
    on E: Exception do
      SetLastError('Error saving file: ' + E.Message);
  end;
  PreparedStream.Free;
end;

//==============================================================================
// UTILITY FUNCTIONS
//==============================================================================

function ResOp_GetLastError: string;
begin
  Result := LastError;
end;

function ResOp_TypeFromInt(Value: integer): TResType;
begin
  Result := ResType_CreateInt(Value);
end;

function ResOp_TypeFromString(const Value: string): TResType;
begin
  Result := ResType_CreateStr(Value);
end;

function ResOp_TypeToString(const ResType: TResType): string;
begin
  Result := ResType_ToString(ResType);
end;

function ResOp_ValidateResourceName(const Name: string; const ResType: TResType): boolean;
var
  I: integer;
  Num: integer;
begin
  Result := True;

  if Name = '' then
    Exit(False);

  if ResType.IsInteger then
  begin
    // Para grupos, iconos y cursores, el nombre debe ser numérico
    if ResType.IntValue in [1, 3, 12, 14] then
    begin
      Result := TryStrToInt(Name, Num);
      Exit;
    end;
  end;

  // Verificar duplicados
  for I := 0 to ResMgr_ResourceCount - 1 do
  begin
    if (ResMgr_Resources[I].Name = Name) and ResType_AreEqual(ResMgr_Resources[I].ResType, ResType) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function ResOp_FindFreeOrdinalName(const BaseName: string; ResTypeInt: integer): string;
var
  I, Num: integer;
  Found: boolean;
begin
  if TryStrToInt(BaseName, Num) then
  begin
    while True do
    begin
      Found := False;
      for I := 0 to ResMgr_ResourceCount - 1 do
      begin
        if ResMgr_Resources[I].ResType.IsInteger and (ResMgr_Resources[I].ResType.IntValue = ResTypeInt) and (ResMgr_Resources[I].Name = IntToStr(Num)) then
        begin
          Found := True;
          Inc(Num);
          Break;
        end;
      end;
      if not Found then
      begin
        Result := IntToStr(Num);
        Exit;
      end;
    end;
  end
  else
    Result := BaseName;
end;

//==============================================================================
// DEBUGGING
//==============================================================================

procedure ResOp_SetDebugLevel(Level: TDebugLevel);
begin
  DebugLevel := Level;
end;

function ResOp_GetDebugLevel: TDebugLevel;
begin
  Result := DebugLevel;
end;

end.
