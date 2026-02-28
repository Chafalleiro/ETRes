===============================================================================
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
  =====================
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

 1. Initialize the system

  if not ResOp_Initialize then
    ShowMessage('Failed to initialize');

 2. Open a resource file

  if ResOp_OpenFile('myfile.res') then
  begin
 
 3. Get list of resources

    Resources := ResOp_GetAllResources;
 4. Load a specific resource

    Streams := nil;

    if ResOp_LoadResource('BITMAP1', ResOp_TypeFromInt(2), mrmAll, Streams) then// Use the stream(s)

 5. Save changes

    ResOp_SaveFile;   //Saves to the original file
    // or

    ResOp_SaveFile('newfile.res');  // Saves to a new file
  end;

 6. Shutdown when done

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

   FILE MANAGEMENT
 ----------------------------------------------------------

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



  --- RESOURCE QUERIES 
  ---------------------------------------------------------

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



  --- RESOURCE OPERATIONS 
  ------------------------------------------------------

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
  Note: For resources that consist of multiple images (e.g., icon groups), this function will export only the first image. For full control over groups, use ResOp_ExportResource.

  Example:
  
    if ResOp_ExportResToFile('DOCUMENTATION_CSS', 'CSS', 'documentation.css') then
      WriteLn('Exported successfully')
    else
      WriteLn('Error: ', ResOp_GetLastError);     
      

  --- UTILITIES
   ----------------------------------------------------------------

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


  --- DEBUGGING
   ----------------------------------------------------------------

  ResOp_SetDebugLevel(Level: TDebugLevel)
  
    Set debug output level (dlNone, dlError, dlWarning, dlInfo, dlDetail).
    Example: ResOp_SetDebugLevel(dlDetail);

  ResOp_GetDebugLevel: TDebugLevel
  
    Get current debug level.
    Example: if ResOp_GetDebugLevel = dlDetail then ...

  =============================================================================
  ==============================================================================
 RESOURCE OPERATIONS
 ==============================================================================
===============================================================================
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

  1. Load a specific resource by name and type
  
	Result := ResOp_LoadResource(['MAINICON', '14', 0, '', 0]);

  2. Load all images from a group
  
 	Result := ResOp_LoadResource(['MAINICON', '14', 10, 'IMAGE', 0]);//mrmImage = 10
	for I := 0 to High(Result.ImageStreams) do
	Image1.Picture.LoadFromStream(Result.ImageStreams[I]);

  3. Load the best text resource
  
	Result := ResOp_LoadResource(['README', '', 1, 'TEXT', 0]);  //mrmBest = 1

  4. Load specific resources by index
  
	Result := ResOp_LoadResource(['ICON', '3', 6, '', 0, 1, 3, 5]);//mrmSelection = 6

  5. Load a complete group as an ICO file
  
	Result := ResOp_LoadResource(['MAINICON', '14', 15, '', 0]); //mrmRejoined = 15
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

  =============================================================================
