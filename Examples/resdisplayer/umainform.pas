unit uMainForm;

{$mode objfpc}{$H+}
{===============================================================================
  Test RES Operations
 ===============================================================================
 Refer to uResOperations die details.
 Comoboxes do a search for tresources with the same name. Most basic call to   MultiResult := ResOp_LoadResource([NAme, '', 0, '"TEXT/IMAGE"', 0]); TEXT/IMAGE here only tefers to a preference, if other reosurce kinds are found they will display.

 Filtered loading does a granular loading of a resource.
 First field is the name, Mandatory.
 Second is the resource type, test or int (int for windows resource defined types)
 Third is the list of items if mrmSelection is selected in the first combo.
 First combo is the loading type, refer to docs in (uResOperations)
 Second the mimetype.
 Third a language, select in combo or type it.

 Load Resource button will ask you for a resource file to load. Will call ResOp_OpenFile(OpenDialog1.FileName).
 Save Resource ResOp_SaveFile(FileName). ResOp_SaveFile only cares for its busineess, you will need to provide a filename and control if oy want to prevent overwtiting. Also a resource must be loaded before to fill the data.

 In the treeview you can:
 Doubleclick to load an item. It will be searchd by name and type, then displayed.
 Drag and drop a resource to another resource type container. Only individual items. Used: ResOp_ChangeResourceType(Data^.ResourceIndex, NewType, False)

 Secondary click opens a popup menu with options for
  Exporting, an item (it will be exported to the default dir with default name)     Example: ResOp_ExportResource('MAINICON', '14', 2);  // Quick export
  Exporting as, you will be asked to where to save and name.     Example:   ResOp_ExportResource("ANSTYLE", "CSS", 0);

  Change name, F2 or clicking adted 500ms will activate the edit interface. Only individual items. ResOp_RenameResource(Data^.ResourceIndex, NewText, False)
  Change Resource type. A dialog will prompt you for a resource type name. Only individual items. ResOp_ChangeResourceType(Data^.ResourceIndex, NewType, False)

  Add a new resource. ResOp_ImportResource(AddForm.SelectedFile, AddForm.ResourceName, AddForm.ResourceType, AddForm.LanguageID, False)
  Delete a reosurce. ResOp_DeleteResource(Data^.ResourceIndex, False)

  Changes must be saved or will be lose after closing.
 }
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Math,
  ComCtrls,  VirtualTrees, Menus,
  {$ifdef windows}
    ActiveX,
  {$else}
  FakeActiveX,
  {$endif}
  uResOperations, uResDefs, uDebug, uMimeDetect, uAddRes;

type

  { TMainForm }

  TVirtualNodeType = (ntGroup, ntResource);

  PVirtualNodeData = ^TVirtualNodeData;

  TVirtualNodeData = record
    NodeType: TVirtualNodeType;
    ResourceIndex: integer;
    DisplayName: string;
    ResTypeName: string;
    Size: int64;
    LanguageID: integer;
    DataOffset: int64;
    MimeString: string;
    IsText: boolean;
    IsImage: boolean;
    Confidence: integer; // -1=no, 0=maybe, 1=yes
  end;

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox1: TComboBox;
    eRName: TEdit;
    eRType: TEdit;
    eRindexList: TEdit;
    ImgControl: TPageControl;
    Label4: TLabel;
    Label5: TLabel;
    MainPagecontrol: TPageControl;
    ControlTxt: TPageControl;
    HexControl: TPageControl;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemAddResource: TMenuItem;
    MenuItemChangeType: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    MainTab: TTabSheet;
    ImgTab: TTabSheet;
    HexTab: TTabSheet;
    TxtTab: TTabSheet;
    UncertainComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TextComboBox: TComboBox;
    ImageComboBox: TComboBox;
    TestButton: TButton;
    Image1: TImage;
    Memo1: TMemo;
    VirtualStringTree1: TVirtualStringTree;

    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure FillComboBoxes;

    procedure ImageComboBoxChange(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItemAddResourceClick(Sender: TObject);
    procedure MenuItemChangeTypeClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure TextComboBoxChange(Sender: TObject);
    procedure UncertainComboBoxChange(Sender: TObject);

    procedure MenuItem1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);

    procedure TestButtonClick(Sender: TObject);
    procedure VirtualStringTree1DblClick(Sender: TObject);

    procedure VirtualStringTree1CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);

    procedure VirtualStringTree1DragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: boolean);
    procedure VirtualStringTree1DragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; const Pt: TPoint; var Effect: longword; Mode: TDropMode);
    procedure VirtualStringTree1DragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: longword; var Accept: boolean);

    procedure VirtualStringTree1Edited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VirtualStringTree1Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: boolean);
    procedure VirtualStringTree1NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: string);

    procedure VirtualStringTree1FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    // Métodos de display - TODOS con parámetro opcional CustomCaption
    procedure DisplayLoadedImage(const LoadResult: TResLoadResult; const CustomCaption: string = '');
    procedure DisplayLoadedText(const LoadResult: TResLoadResult; const CustomCaption: string = '');
    procedure DisplayLoadedHex(const LoadResult: TResLoadResult; const CustomCaption: string = '');

  private
    FResourcesChanged: boolean;  // Indica si algún recurso ha sido modificado
    procedure ListAllResources;
    procedure UpdateWindowTitle;
    procedure RefreshComboBoxes;

    procedure ClearAllTabs(PageControl: TPageControl);

    function AddImageTab(const aCaption: string): TImage;
    function AddTextTab(const aCaption: string): TMemo;
    function AddHexTab(const aCaption: string): TMemo;

    procedure ParseIndexList(const InputText: string; OutputList: TStringList);
    procedure DisplayTestResults(const MultiResult: TMultiResLoadResult);

    procedure ClearAllData;  // Nueva: limpia todos los datos antes de cargar nuevo archivo

    function FindAndSelectNode(StartNode: PVirtualNode; const aName, TypeName: string): Boolean;
    procedure RefreshResourceTree;
  public
    property ResourcesChanged: boolean read FResourcesChanged write FResourcesChanged;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.frm}

{ TMainForm }

//------------------------------------------------------------------------------
// Limpiar todos los datos antes de cargar un nuevo archivo
//------------------------------------------------------------------------------
procedure TMainForm.ClearAllData;
begin
  // Limpiar el árbol
  VirtualStringTree1.Clear;

  // Limpiar comboboxes
  TextComboBox.Clear;
  ImageComboBox.Clear;
  UncertainComboBox.Clear;

  // Limpiar pestañas
  ClearAllTabs(ImgControl);
  ClearAllTabs(ControlTxt);
  ClearAllTabs(HexControl);

  // Limpiar memo
  Memo1.Clear;

  // Resetear labels
  Label1.Caption := 'Text Resources (0)';
  Label2.Caption := 'Image Resources (0)';
  Label3.Caption := 'Uncertain Resources (0)';
  Label4.Caption := 'No file loaded';

  // Limpiar el sistema de recursos (cierra el archivo actual)
  ResOp_Shutdown;
  ResOp_Initialize;
end;

//------------------------------------------------------------------------------
// Inicialización del formulario
//------------------------------------------------------------------------------
procedure TMainForm.FormCreate(Sender: TObject);
begin
  DebugLevel := dlDetail;

  // Inicializar el sistema de recursos
  ResOp_Initialize;

  // Configurar el diálogo de apertura
  OpenDialog1.Title := 'Open Resource File';
  OpenDialog1.Filter := 'Resource files (*.res)|*.res|All files (*.*)|*.*';
  OpenDialog1.DefaultExt := '.res';
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));

  // Limpiar la interfaz
  ClearAllData;

  // Preguntar si queremos cargar un archivo al inicio
  if MessageDlg('Load Resource File',
     'Do you want to load a resource file?',
     mtConfirmation, [mbYes, mbNo], 0) = 6 then
  begin
    Button1Click(Sender);
  end;

  Memo1.Lines.Add('Ready. Use the "Open" button to load a .res file.');
end;

// uMainForm.pas - Updated FillComboBoxes with UncertainComboBox combobox

procedure TMainForm.FillComboBoxes;
var
  TextResources, ImageResources: TResListArray;
  I: integer;
begin
  TextResources := ResOp_GetTextResources;
  ImageResources := ResOp_GetImageResources;

  TextComboBox.Clear;
  TextComboBox.Items.Add('-- Select a text resource --');
  for I := 0 to High(TextResources) do
    TextComboBox.Items.AddObject(TextResources[I].Name,
      TObject(PtrInt(Ord(TextResources[I].IsText))));

  ImageComboBox.Clear;
  ImageComboBox.Items.Add('-- Select an image resource --');
  for I := 0 to High(ImageResources) do
    ImageComboBox.Items.AddObject(ImageResources[I].Name,
      TObject(PtrInt(Ord(ImageResources[I].IsImage))));

  Label1.Caption := 'Text Resources (' + IntToStr(Length(TextResources)) + ')';
  Label2.Caption := 'Image Resources (' + IntToStr(Length(ImageResources)) + ')';

  if TextComboBox.Items.Count > 1 then
    TextComboBox.ItemIndex := 1;
  if ImageComboBox.Items.Count > 1 then
    ImageComboBox.ItemIndex := 1;
end;

procedure TMainForm.ListAllResources;
var
  Resources: TResListArray;
  I: integer;
  Data: PVirtualNodeData;
  RootNode, TypeNode, ResNode: PVirtualNode;
  TypeGroups: TStringList;
  TypeName: string;
  ResInfo: TResListInfo;
begin
  Memo1.Lines.Add('=== Resources in File ===');

   Resources := ResOp_GetAllResources;

   Memo1.Lines.Add('Total resources: ' + IntToStr(Length(Resources)));
   Memo1.Lines.Add('');

   VirtualStringTree1.Clear;
   VirtualStringTree1.NodeDataSize := SizeOf(TVirtualNodeData);
   VirtualStringTree1.BeginUpdate;

   try
    // Configure tree columns if not already set
    if VirtualStringTree1.Header.Columns.Count = 0 then
    begin
      with VirtualStringTree1.Header.Columns.Add do
      begin
        Text := 'Name/ID';
        Width := 200;
        Options := [coAutoSpring, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible];
      end;
      with VirtualStringTree1.Header.Columns.Add do
      begin
        Text := 'Type';
        Width := 120;
        Options := [coAutoSpring, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible];
      end;
      with VirtualStringTree1.Header.Columns.Add do
      begin
        Text := 'Size';
        Width := 80;
        Options := [coAutoSpring, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible];
      end;
      with VirtualStringTree1.Header.Columns.Add do
      begin
        Text := 'Language';
        Width := 70;
        Options := [coAutoSpring, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible];
      end;
      with VirtualStringTree1.Header.Columns.Add do
      begin
        Text := 'Offset';
        Width := 80;
        Options := [coAutoSpring, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible];
      end;
      with VirtualStringTree1.Header.Columns.Add do
      begin
        Text := 'MIME Type';
        Width := 150;
        Options := [coAutoSpring, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible];
      end;
      with VirtualStringTree1.Header.Columns.Add do
      begin
        Text := 'Confidence';
        Width := 70;
        Options := [coAutoSpring, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible];
      end;

      VirtualStringTree1.Header.Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoVisible, hoShowSortGlyphs];
      VirtualStringTree1.Header.SortColumn := 1; // Sort by Type column initially
      VirtualStringTree1.Header.SortDirection := sdAscending;
    end;

    // Group resources by type for better organization
    TypeGroups := TStringList.Create;
    try
      TypeGroups.Sorted := True;
      TypeGroups.Duplicates := dupIgnore;

      // First, collect all unique display types
      for I := 0 to High(Resources) do
      begin
        TypeGroups.Add(Resources[I].DisplayType);
      end;

      // Sort alphabetically
      TypeGroups.Sort;

      // Create root nodes for each resource type
      for I := 0 to TypeGroups.Count - 1 do
      begin
        TypeName := TypeGroups[I];

        // Add type group node
        TypeNode := VirtualStringTree1.AddChild(nil);
        Data := VirtualStringTree1.GetNodeData(TypeNode);
        if Assigned(Data) then
        begin
          Data^.NodeType := ntGroup;
          Data^.DisplayName := TypeName + ' Resources';
          Data^.ResourceIndex := -1;
          Data^.Size := 0;
          Data^.LanguageID := 0;
          Data^.DataOffset := 0;
          Data^.MimeString := '';
          Data^.IsText := False;
          Data^.IsImage := False;
          Data^.Confidence := -1;
        end;
        VirtualStringTree1.Expanded[TypeNode] := True;

        // Add resources of this type under the group node
        for ResInfo in Resources do
        begin
          if ResInfo.DisplayType = TypeName then
          begin
            ResNode := VirtualStringTree1.AddChild(TypeNode);
            Data := VirtualStringTree1.GetNodeData(ResNode);
            if Assigned(Data) then
            begin
              Data^.NodeType := ntResource;
              Data^.ResourceIndex := ResInfo.Index;

              // ✅ FIXED: Use the actual resource name without interpretation
              if ResInfo.Name = '' then
                Data^.DisplayName := '(unnamed)'
              else
                Data^.DisplayName := ResInfo.Name;  // Just use the name as-is, no prefix
              Data^.ResTypeName := ResInfo.DisplayType;
              Data^.Size := ResInfo.Size;
              Data^.LanguageID := ResInfo.LanguageID;
              Data^.DataOffset := ResInfo.DataOffset;
              Data^.MimeString := ResInfo.MimeString;
              Data^.IsText := ResInfo.IsText;
              Data^.IsImage := ResInfo.IsImage;

              // Set confidence display
              if ResInfo.IsText then
                Data^.Confidence := 1
              else if ResInfo.IsImage then
                Data^.Confidence := 1
              else
                Data^.Confidence := 0;
            end;
          end;
        end;
      end;

    finally
      TypeGroups.Free;
    end;

    // Also display in Memo1 for reference
    Memo1.Lines.Add('Idx  Name/ID                         Type           Size       Lang  Offset  MIME');
    Memo1.Lines.Add('---  ------------------------------  ------------  ---------  ----  --------  ----------------');

    for I := 0 to High(Resources) do
    begin
      Memo1.Lines.Add(Format('%3d  %-28s  %-12s  %9d  %4d  $%8.8x  %-16s', [I + 1, Copy(Resources[I].Name + StringOfChar(' ', 28), 1, 28),  // ✅ Raw name, no modifications
        Resources[I].DisplayType, Resources[I].Size, Resources[I].LanguageID, Resources[I].DataOffset, Resources[I].MimeString]));
    end;

  finally
    VirtualStringTree1.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
// Destrucción del formulario
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
    VirtualStringTree1.Clear;

  // Limpiar comboboxes
  TextComboBox.Clear;
  ImageComboBox.Clear;
  UncertainComboBox.Clear;

  // Limpiar pestañas
  ClearAllTabs(ImgControl);
  ClearAllTabs(ControlTxt);
  ClearAllTabs(HexControl);

  // Limpiar memo
  Memo1.Clear;
  ResOp_Shutdown;
end;

//------------------------------------------------------------------------------
// Cargar archivo de recursos
//------------------------------------------------------------------------------

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    ClearAllData;

    Memo1.Lines.Add('=== Resource File Viewer ===');
    Memo1.Lines.Add('');

    if ResOp_OpenFile(OpenDialog1.FileName) then
    begin
      Memo1.Lines.Add('✓ Resource file loaded successfully');
      Memo1.Lines.Add('File: ' + ExtractFileName(OpenDialog1.FileName));
      Memo1.Lines.Add('');

      Label4.Caption := 'File: ' + ExtractFileName(OpenDialog1.FileName);

      ListAllResources;
      FillComboBoxes;
    end
    else
    begin
      Memo1.Lines.Add('✗ Error loading resource file:');
      Memo1.Lines.Add('  ' + ResOp_GetLastError);
      Label4.Caption := 'Load failed';
    end;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  FileName: string;
begin
  if ResOp_GetResourceCount = 0 then
  begin
    MessageDlg('No resources', 'There are no resources to save.', mtError, [mbOK], 0);
    Exit;
  end;

  if ResOp_GetFileName = '' then
  begin
    MessageDlg('No file', 'No file is currently open.', mtError, [mbOK], 0);
    Exit;
  end;

  if FResourcesChanged then
  begin
    if MessageDlg('Unsaved Changes',
       'There are unsaved changes. Save now?',
       mtConfirmation, [mbYes, mbNo], 0) <> 6 then
    begin
      Exit;
    end;
  end;

  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Save Resource File';
    SaveDialog.Filter := 'Resource files (*.res)|*.res|All files (*.*)|*.*';
    SaveDialog.DefaultExt := '.res';
    SaveDialog.FileName := ExtractFileName(ResOp_GetFileName);

    if SaveDialog.Execute then
    begin
      FileName := SaveDialog.FileName;
//      if SameText(FileName, ResOp_GetFileName) then
//        ResOp_CloseFile;  // Cierra el archivo actual si vamos a sobrescribirlo
      if FileExists(FileName) and
         not SameText(ResOp_GetFileName, FileName) then
      begin
        if MessageDlg('File exists',
           Format('File "%s" already exists. Overwrite?',
           [ExtractFileName(FileName)]),
           mtConfirmation, [mbYes, mbNo], 0) <> 6 then
        begin
          Exit;
        end;
      end;

      if ResOp_SaveFile(FileName) then
      begin
        Memo1.Lines.Add('');
        Memo1.Lines.Add('=== Save Result ===');
        Memo1.Lines.Add(Format('  ✓ File saved: %s', [ExtractFileName(FileName)]));

        FResourcesChanged := False;
        UpdateWindowTitle;
      end
      else
      begin
        Memo1.Lines.Add('');
        Memo1.Lines.Add('=== Save Result ===');
        Memo1.Lines.Add('  ✗ Failed to save file: ' + ResOp_GetLastError);
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
    RefreshResourceTree;
  Memo1.Lines.Add('  ✓ Tree refreshed');
end;

procedure TMainForm.TextComboBoxChange(Sender: TObject);
var
  MultiResult: TMultiResLoadResult;
  I: Integer;
begin
  if TextComboBox.ItemIndex <= 0 then Exit;

  ClearAllTabs(ImgControl);
  ClearAllTabs(ControlTxt);
  ClearAllTabs(HexControl);

  Memo1.Lines.Add('');
  Memo1.Lines.Add('=== Loading text resource ===');
  Memo1.Lines.Add(Format('  Name: "%s"', [TextComboBox.Text]));

  // Llamada directa - igual que antes pero via ResOp_
  MultiResult := ResOp_LoadResource([TextComboBox.Text, '', 0, 'TEXT', 0]);

  if MultiResult.Success then
    DisplayTestResults(MultiResult)
  else
    Memo1.Lines.Add('  ✗ ' + MultiResult.ErrorMessage);
end;

procedure TMainForm.ImageComboBoxChange(Sender: TObject);
var
  MultiResult: TMultiResLoadResult;
begin
  if ImageComboBox.ItemIndex <= 0 then Exit;

  // Clear tabs
  ClearAllTabs(ImgControl);
  ClearAllTabs(ControlTxt);
  ClearAllTabs(HexControl);

  Memo1.Lines.Add('');
  Memo1.Lines.Add('=== Loading image resource ===');
  Memo1.Lines.Add(Format('  Name: "%s"', [ImageComboBox.Text]));

  // Load with IMAGE format filter
  MultiResult := ResOp_LoadResource([ImageComboBox.Text, '', 0, 'IMAGE', 0]);

  if MultiResult.Success then
    DisplayTestResults(MultiResult)
  else
    Memo1.Lines.Add('  ✗ ' + MultiResult.ErrorMessage);
end;

procedure TMainForm.UncertainComboBoxChange(Sender: TObject);
var
  MultiResult: TMultiResLoadResult;
begin
  if UncertainComboBox.ItemIndex <= 0 then Exit;

  // Clear tabs
  ClearAllTabs(ImgControl);
  ClearAllTabs(ControlTxt);
  ClearAllTabs(HexControl);

  Memo1.Lines.Add('');
  Memo1.Lines.Add('=== Loading uncertain resource ===');
  Memo1.Lines.Add(Format('  Name: "%s"', [UncertainComboBox.Text]));

  // Load with no format filter
  MultiResult := ResOp_LoadResource([UncertainComboBox.Text, '', 0, '', 0]);

  if MultiResult.Success then
    DisplayTestResults(MultiResult)
  else
    Memo1.Lines.Add('  ✗ ' + MultiResult.ErrorMessage);
end;

//Search for resources
procedure TMainForm.TestButtonClick(Sender: TObject);
var
  ResourceName: string;
  FormatPref: string;
  Mode: TMultiResourceMode;
  Lang: word;
  IndexList: TStringList;
  I: integer;
  ParamArray: array of TVarRec;
  ParamCount: integer;
  MultiResult: TMultiResLoadResult;
  Idx: integer;
  ExpectedCategory: TResourceCategory;
  MismatchFound: boolean;
  CategoryInfo: TResourceCategoryInfo;
begin
  // Limpiar tabs
  ClearAllTabs(ImgControl);
  ClearAllTabs(ControlTxt);
  ClearAllTabs(HexControl);

  ResourceName := eRName.Text;
  if ResourceName = '' then
  begin
    Memo1.Lines.Add('Please enter a resource name');
    Exit;
  end;

  // Obtener valores
  Mode := TMultiResourceMode(ComboBox1.ItemIndex);

  if ComboBox2.ItemIndex > 0 then
    FormatPref := ComboBox2.Text
  else
    FormatPref := '';

  if ComboBox3.ItemIndex > 0 then
    Lang := StrToIntDef(Copy(ComboBox3.Text, 1, Pos(' ', ComboBox3.Text) - 1), 0)
  else
    Lang := 0;

  // Determinar categoría esperada
  ExpectedCategory := rcUnknown;
  if FormatPref <> '' then
  begin
    if (FormatPref = 'TEXT') or (FormatPref = 'HTML') or (FormatPref = 'CSS') or (FormatPref = 'JS') or (FormatPref = 'XML') or (FormatPref = 'JSON') then
      ExpectedCategory := rcText
    else if (FormatPref = 'IMAGE') or (FormatPref = 'PNG') or (FormatPref = 'JPG') or (FormatPref = 'JPEG') or (FormatPref = 'GIF') or (FormatPref = 'BMP') or (FormatPref = 'ICO') then
      ExpectedCategory := rcImage
    else if (FormatPref = 'AUDIO') or (FormatPref = 'WAVE') or (FormatPref = 'MP3') then
      ExpectedCategory := rcAudio
    else if (FormatPref = 'VIDEO') or (FormatPref = 'AVI') then
      ExpectedCategory := rcVideo;
  end;

  // Parsear índices
  IndexList := TStringList.Create;
  try
    ParseIndexList(eRIndexList.Text, IndexList);

    // Mostrar parámetros
    Memo1.Lines.Add('');
    Memo1.Lines.Add('=== Test Load Parameters ===');
    Memo1.Lines.Add(Format('Name: "%s"', [ResourceName]));
    Memo1.Lines.Add(Format('Mode: %s (%d)', [ComboBox1.Text, Ord(Mode)]));
    Memo1.Lines.Add(Format('Format: "%s"', [FormatPref]));
    if ExpectedCategory <> rcUnknown then
    begin
      CategoryInfo := GetCategoryInfo(ExpectedCategory);
      Memo1.Lines.Add(Format('Expected Category: %s', [CategoryInfo.Name]));
    end;
    Memo1.Lines.Add(Format('Language: %d', [Lang]));
    Memo1.Lines.Add(Format('Indices: %s', [IndexList.CommaText]));

    // Construir parámetros
    ParamCount := 5 + IndexList.Count;
    SetLength(ParamArray, ParamCount);

    ParamArray[0].VType := vtAnsiString;
    ParamArray[0].VAnsiString := Pointer(ResourceName);

    ParamArray[1].VType := vtAnsiString;
    ParamArray[1].VAnsiString := Pointer(eRType.Text); // ResourceType

    ParamArray[2].VType := vtInteger;
    ParamArray[2].VInteger := Ord(Mode);

    ParamArray[3].VType := vtAnsiString;
    ParamArray[3].VAnsiString := Pointer(FormatPref);

    ParamArray[4].VType := vtInteger;
    ParamArray[4].VInteger := Lang;

    for I := 0 to IndexList.Count - 1 do
    begin
      Idx := StrToIntDef(IndexList[I], -1);
      if Idx >= 0 then
      begin
        ParamArray[5 + I].VType := vtInteger;
        ParamArray[5 + I].VInteger := Idx;
      end;
    end;

    // Cargar
    MultiResult := ResOp_LoadResource(ParamArray);

    if MultiResult.Success then
    begin
      Memo1.Lines.Add(Format('✓ Success: Loaded %d item(s)', [MultiResult.Count]));

      // Validar tipos
      MismatchFound := False;

      // Validar imágenes expandidas
      if Length(MultiResult.ImageStreams) > 0 then
      begin
        for I := 0 to Length(MultiResult.ImageStreams) - 1 do
        begin
          if (MultiResult.ImageStreams[I] <> nil) and (I < Length(MultiResult.Items)) then
          begin
            if (ExpectedCategory <> rcUnknown) and (MultiResult.Items[I].ResourceCategory <> ExpectedCategory) then
            begin
              Memo1.Lines.Add(Format('  ⚠ WARNING: Image %d type mismatch', [I]));
              MismatchFound := True;
            end;
          end;
        end;
      end;

      // Validar items regulares
      for I := 0 to MultiResult.Count - 1 do
      begin
        if not MultiResult.Items[I].Success then Continue;

        if (ExpectedCategory <> rcUnknown) and (MultiResult.Items[I].ResourceCategory <> ExpectedCategory) then
        begin
          Memo1.Lines.Add(Format('  ⚠ WARNING: Item %d type mismatch', [I]));
          MismatchFound := True;
        end;
      end;

      if MismatchFound then
        Memo1.Lines.Add('  ⚠ Some items do not match the requested format!');

      // Mostrar usando la función unificada
      DisplayTestResults(MultiResult);
    end
    else
    begin
      Memo1.Lines.Add('✗ Failed: ' + MultiResult.ErrorMessage);
    end;

  finally
    IndexList.Free;
  end;
end;

procedure TMainForm.VirtualStringTree1DblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
  FormatFilter: string;
  MultiResult: TMultiResLoadResult;
begin
  Node := VirtualStringTree1.GetFirstSelected;
  if Node = nil then Exit;

  Data := VirtualStringTree1.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;

  // Clear tabs
  ClearAllTabs(ImgControl);
  ClearAllTabs(ControlTxt);
  ClearAllTabs(HexControl);

  Memo1.Lines.Add('');
  Memo1.Lines.Add('=== Loading resource by double-click ===');
  Memo1.Lines.Add(Format('  Name: "%s", Type: "%s"', [Data^.DisplayName, Data^.ResTypeName]));

  if Data^.IsImage then
    FormatFilter := 'IMAGE'
  else if Data^.IsText then
    FormatFilter := 'TEXT'
  else
    FormatFilter := '';

  MultiResult := ResOp_LoadResource([Data^.DisplayName, Data^.ResTypeName, 0, FormatFilter, 0]);

  if MultiResult.Success then
    DisplayTestResults(MultiResult)
  else
    Memo1.Lines.Add('  ✗ ' + MultiResult.ErrorMessage);
end;

procedure TMainForm.VirtualStringTree1CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  Data1, Data2: PVirtualNodeData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  if not Assigned(Data1) or not Assigned(Data2) then
    Exit;

  // Groups always come before resources
  if (Data1^.NodeType = ntGroup) and (Data2^.NodeType = ntResource) then
  begin
    Result := -1;
    Exit;
  end;
  if (Data1^.NodeType = ntResource) and (Data2^.NodeType = ntGroup) then
  begin
    Result := 1;
    Exit;
  end;

  // Sort within same node type
  case Column of
    0: Result := CompareText(Data1^.DisplayName, Data2^.DisplayName);
    1:
      if Data1^.NodeType = ntResource then
        Result := CompareText(Data1^.ResTypeName, Data2^.ResTypeName)
      else
        Result := CompareText(Data1^.DisplayName, Data2^.DisplayName);
    2:
      if Data1^.Size < Data2^.Size then
        Result := -1
      else if Data1^.Size > Data2^.Size then
        Result := 1
      else
        Result := 0;
    4:
      if Data1^.DataOffset < Data2^.DataOffset then
        Result := -1
      else if Data1^.DataOffset > Data2^.DataOffset then
        Result := 1
      else
        Result := 0;
    else
      Result := 0;
  end;
end;

procedure TMainForm.VirtualStringTree1DragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: boolean);
var
  Data: PVirtualNodeData;
begin
  Allowed := False;
  if Node = nil then Exit;
  Data := VirtualStringTree1.GetNodeData(Node);
  if not Assigned(Data) then Exit;
  // Solo recursos normales (no grupos) pueden ser arrastrados
  Allowed := (Data^.NodeType = ntResource) and
              not (Data^.ResTypeName = 'GROUP_ICON') and
              not (Data^.ResTypeName = 'CURSOR') and
              not (Data^.ResTypeName = 'ICON') and
              not (Data^.ResTypeName = 'GROUP_CURSOR');
end;

// Add these to your form's event handlers

procedure TMainForm.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PVirtualNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then
    Exit;

  case Column of
    0: CellText := Data^.DisplayName;
    1:
      if Data^.NodeType = ntGroup then
        CellText := ''
      else
        CellText := Data^.ResTypeName;
    2:
      if Data^.NodeType = ntGroup then
        CellText := Format('[%d items]', [Sender.ChildCount[Node]])
      else
        CellText := Format('%d', [Data^.Size]);
    3:
      if Data^.NodeType = ntGroup then
        CellText := ''
      else if Data^.LanguageID > 0 then
        CellText := IntToStr(Data^.LanguageID)
      else
        CellText := '';
    4:
      if Data^.NodeType = ntGroup then
        CellText := ''
      else
        CellText := '$' + IntToHex(Data^.DataOffset, 8);
    5:
      if Data^.NodeType = ntGroup then
        CellText := ''
      else
        CellText := Data^.MimeString;
    6:
      if Data^.NodeType = ntGroup then
        CellText := ''
      else
        case Data^.Confidence of
          -1: CellText := 'No';
          0: CellText := 'Maybe';
          1: CellText := 'Yes';
          else CellText := '';
        end;
  end;
end;

procedure TMainForm.VirtualStringTree1FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PVirtualNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    // Clean up any string fields if necessary
    Data^.DisplayName := '';
    Data^.ResTypeName := '';
    Data^.MimeString := '';
  end;
end;

procedure TMainForm.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer);
var
  Data: PVirtualNodeData;
begin
  writeln('IMGLOAD');
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then
    Exit;

  if Column = 0 then
  begin
    if Data^.NodeType = ntGroup then
      ImageIndex := 0  // Folder icon
    else if Data^.IsImage then
      ImageIndex := 1  // Image icon
    else if Data^.IsText then
      ImageIndex := 2  // Text icon
    else
      ImageIndex := 3; // Generic resource icon
  end;
end;

// uMainForm.pas - Add these helper methods
// Load and show resource

procedure TMainForm.DisplayLoadedImage(const LoadResult: TResLoadResult; const CustomCaption: string = '');
var
  Jpeg: TJPEGImage;
  Png: TPNGImage;
  Bitmap: TBitmap;
  aIcon: TIcon;
  ImageCtrl: TImage;
  TabCaption: string;
begin
  if not LoadResult.Success or (LoadResult.Data = nil) then
  begin
    Memo1.Lines.Add('  ✗ No image data to display');
    Exit;
  end;

  try
    LoadResult.Data.Position := 0;

    // Crear caption del tab
    if CustomCaption <> '' then
      TabCaption := CustomCaption
    else
      TabCaption := Format('%d (%s)', [LoadResult.ResourceIndex, LoadResult.MimeTypeName]);

    // Crear nuevo tab
    ImageCtrl := AddImageTab(TabCaption);

    if ImageCtrl = nil then
    begin
      Memo1.Lines.Add('  ✗ Failed to create image tab');
      Exit;
    end;

    case LoadResult.MimeType of
      mtBitmap:
      begin
        Bitmap := TBitmap.Create;
        try
          Bitmap.LoadFromStream(LoadResult.Data);
          ImageCtrl.Picture.Assign(Bitmap);
        finally
          Bitmap.Free;
        end;
      end;

      mtIcon:
      begin
        aIcon := TIcon.Create;
        try
          aIcon.LoadFromStream(LoadResult.Data);
          ImageCtrl.Picture.Assign(aIcon);
        finally
          aIcon.Free;
        end;
      end;

      mtJpeg:
      begin
        Jpeg := TJPEGImage.Create;
        try
          Jpeg.LoadFromStream(LoadResult.Data);
          ImageCtrl.Picture.Assign(Jpeg);
        finally
          Jpeg.Free;
        end;
      end;

      mtPng:
      begin
        Png := TPNGImage.Create;
        try
          Png.LoadFromStream(LoadResult.Data);
          ImageCtrl.Picture.Assign(Png);
        finally
          Png.Free;
        end;
      end;

      else
      begin
        Memo1.Lines.Add('  ⚠ Unsupported image format: ' + LoadResult.MimeTypeName);
        Exit;
      end;
    end;

    // Mostrar información
    if (ImageCtrl.Picture.Graphic <> nil) and not ImageCtrl.Picture.Graphic.Empty then
    begin
      Memo1.Lines.Add(Format('  ✓ Image loaded: %dx%d in tab "%s"', [ImageCtrl.Picture.Width, ImageCtrl.Picture.Height, TabCaption]));
    end;

  except
    on E: Exception do
      Memo1.Lines.Add('  ✗ Error displaying image: ' + E.Message);
  end;
end;

procedure TMainForm.DisplayLoadedText(const LoadResult: TResLoadResult; const CustomCaption: string = '');
var
  Buffer: TBytes;
  TextContent: string;
  Encoding: TEncoding;
  Memo: TMemo;
  TabCaption: string;
begin
  if not LoadResult.Success or (LoadResult.Data = nil) then
  begin
    Memo1.Lines.Add('  ✗ No text data to display');
    Exit;
  end;

  try
    LoadResult.Data.Position := 0;
    SetLength(Buffer, LoadResult.Data.Size);
    LoadResult.Data.Read(Buffer[0], LoadResult.Data.Size);

    // Crear caption del tab
    if CustomCaption <> '' then
      TabCaption := CustomCaption
    else
      TabCaption := Format('%d (%s)', [LoadResult.ResourceIndex, LoadResult.ResourceTypeName]);

    // Añadir nuevo tab
    Memo := AddTextTab(TabCaption);

    // Para RCDATA - mostrar raw
    if LoadResult.ResourceType.IsInteger and (LoadResult.ResourceType.IntValue = 10) then
    begin
      TextContent := TEncoding.UTF8.GetString(Buffer);
      Memo.Text := TextContent;
      Memo1.Lines.Add('  ✓ RCDATA loaded as text (raw)');
    end
    else
    begin
      // Detectar encoding
      if (LoadResult.Data.Size >= 2) and (Buffer[0] = $FF) and (Buffer[1] = $FE) then
      begin
        Encoding := TEncoding.Unicode;
        TextContent := Encoding.GetString(Buffer, 2, LoadResult.Data.Size - 2);
      end
      else if (LoadResult.Data.Size >= 2) and (Buffer[0] = $FE) and (Buffer[1] = $FF) then
      begin
        Encoding := TEncoding.BigEndianUnicode;
        TextContent := Encoding.GetString(Buffer, 2, LoadResult.Data.Size - 2);
      end
      else if (LoadResult.Data.Size >= 3) and (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
      begin
        TextContent := TEncoding.UTF8.GetString(Buffer, 3, LoadResult.Data.Size - 3);
      end
      else
      begin
        TextContent := TEncoding.UTF8.GetString(Buffer);
      end;

      Memo.Text := TextContent;
    end;

    Memo1.Lines.Add(Format('  ✓ Text loaded: %d lines in tab "%s"', [Memo.Lines.Count, TabCaption]));

  except
    on E: Exception do
      Memo1.Lines.Add('  ✗ Error displaying text: ' + E.Message);
  end;
end;

procedure TMainForm.DisplayLoadedHex(const LoadResult: TResLoadResult; const CustomCaption: string = '');
var
  I, J: integer;
  BytesPerRow: integer;
  HexLine, AsciiLine: string;
  Data: pbyte;
  Offset: integer;
  RowData: array[0..15] of byte;
  RowBytes: integer;
  Memo: TMemo;
  TabCaption: string;
begin
  if not LoadResult.Success or (LoadResult.Data = nil) then
  begin
    Memo1.Lines.Add('  ✗ No data for hex dump');
    Exit;
  end;

  // Crear caption del tab
  if CustomCaption <> '' then
    TabCaption := CustomCaption
  else
    TabCaption := Format('%d (Hex)', [LoadResult.ResourceIndex]);

  // Crear nuevo tab
  Memo := AddHexTab(TabCaption);

  if Memo = nil then
  begin
    Memo1.Lines.Add('  ✗ Failed to create hex tab');
    Exit;
  end;

  Memo.Clear;

  // ========================================================================
  // 1. MOSTRAR TODOS LOS PARÁMETROS DEL RECURSO
  // ========================================================================
  //  Memo.Lines.Add('=' * 80);
  Memo.Lines.Add('RESOURCE INFORMATION');
  //  Memo.Lines.Add('=' * 80);
  Memo.Lines.Add('');

  // Información básica
  Memo.Lines.Add(Format('Resource Name: ...... %s', [LoadResult.ResourceName]));
  Memo.Lines.Add(Format('Success: ........... %s', [BoolToStr(LoadResult.Success, True)]));
  Memo.Lines.Add(Format('Size: .............. %d bytes', [LoadResult.Size]));
  Memo.Lines.Add(Format('MIME Type: .......... %s (%d)', [LoadResult.MimeTypeName, Ord(LoadResult.MimeType)]));
  Memo.Lines.Add(Format('Resource Type: ...... %s', [LoadResult.ResourceTypeName]));
  Memo.Lines.Add(Format('Resource Index: ..... %d', [LoadResult.ResourceIndex]));
  Memo.Lines.Add(Format('Language ID: ........ %d', [LoadResult.LanguageID]));
  Memo.Lines.Add(Format('Data Offset: ........ $%x', [LoadResult.DataOffset]));
  Memo.Lines.Add('');

  // Información de categoría
  //  Memo.Lines.Add('-' * 80);
  Memo.Lines.Add('CATEGORY INFORMATION');
  //  Memo.Lines.Add('-' * 80);
  Memo.Lines.Add('');

  Memo.Lines.Add(Format('Resource Category: .. %s (%d)', [LoadResult.ResourceCategoryName, Ord(LoadResult.ResourceCategory)]));
  Memo.Lines.Add(Format('Can Display: ........ %s', [BoolToStr(LoadResult.CanDisplay, True)]));
  Memo.Lines.Add(Format('Default View: ....... %s', [LoadResult.DefaultView]));
  Memo.Lines.Add('');

  // Información del stream
  //  Memo.Lines.Add('-' * 80);
  Memo.Lines.Add('STREAM INFORMATION');
  //  Memo.Lines.Add('-' * 80);
  Memo.Lines.Add('');

  Memo.Lines.Add(Format('Stream Address: ..... %p', [Pointer(LoadResult.Data)]));
  Memo.Lines.Add(Format('Stream Size: ........ %d bytes', [LoadResult.Data.Size]));
  Memo.Lines.Add(Format('Stream Position: .... %d', [LoadResult.Data.Position]));
  Memo.Lines.Add('');

  // ========================================================================
  // 2. MOSTRAR EL DUMP HEXADECIMAL
  // ========================================================================

  LoadResult.Data.Position := 0;
  BytesPerRow := 16;

  Memo.Lines.Add(Format('Binary data - %d bytes', [LoadResult.Size]));
  Memo.Lines.Add(Format('Type: %s, MIME: %s', [LoadResult.ResourceTypeName, LoadResult.MimeTypeName]));
  Memo.Lines.Add('');
  Memo.Lines.Add('Offset    00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F    ASCII');
  Memo.Lines.Add('--------  ------------------------------------------------  ----------------');

  Data := LoadResult.Data.Memory;
  for I := 0 to (LoadResult.Size - 1) div BytesPerRow do
  begin
    Offset := I * BytesPerRow;
    RowBytes := Min(BytesPerRow, LoadResult.Size - Offset);

    for J := 0 to RowBytes - 1 do
      RowData[J] := Data[Offset + J];

    HexLine := Format('%.8x  ', [Offset]) + TMimeDetector.BytesToHex(@RowData, RowBytes);

    if RowBytes < BytesPerRow then
    begin
      for J := RowBytes to BytesPerRow - 1 do
        HexLine := HexLine + '   ';
    end;

    if RowBytes > 8 then
      Insert(' ', HexLine, 35);

    AsciiLine := '';
    for J := 0 to RowBytes - 1 do
    begin
      if (RowData[J] >= 32) and (RowData[J] < 127) then
        AsciiLine := AsciiLine + Chr(RowData[J])
      else
        AsciiLine := AsciiLine + '.';
    end;

    Memo.Lines.Add(HexLine + '  ' + AsciiLine);
  end;

  Memo1.Lines.Add(Format('  ✓ Hex dump created: %s, %d bytes', [TabCaption, LoadResult.Size]));
end;

//------------------------------------------------------------------------------
// Add a new image tab and return the TImage control
//------------------------------------------------------------------------------

function TMainForm.AddImageTab(const aCaption: string): TImage;
var
  NewTab: TTabSheet;
begin
  Result := nil;

  if ImgControl = nil then Exit;

  try
    NewTab := TTabSheet.Create(ImgControl);
    NewTab.Parent := ImgControl;
    NewTab.Caption := aCaption;
    NewTab.PageControl := ImgControl;

    Result := TImage.Create(NewTab);
    Result.Parent := NewTab;
    Result.Align := alClient;
    Result.Stretch := True;
    Result.Proportional := True;
    Result.Visible := True;

    // Switch to the new tab
    ImgControl.ActivePage := NewTab;
  except
    on E: Exception do
    begin
      Memo1.Lines.Add('  ⚠ Error creating image tab: ' + E.Message);
      Result := nil;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Clear all tabs from a page control
//------------------------------------------------------------------------------

procedure TMainForm.ClearAllTabs(PageControl: TPageControl);
var
  I: integer;
  TabSheet: TTabSheet;
  J: integer;
begin
  if PageControl = nil then Exit;

  // Delete all tabs from the end to the beginning
  for I := PageControl.PageCount - 1 downto 0 do
  begin
    TabSheet := PageControl.Pages[I];

    // If this is an image tab with a TImage that might own a stream,
    // we don't need to explicitly free anything - the TabSheet will
    // free its child controls when destroyed

    TabSheet.Free;
  end;
end;

//------------------------------------------------------------------------------
// Add a new text tab and return the TMemo control
//------------------------------------------------------------------------------

function TMainForm.AddTextTab(const aCaption: string): TMemo;
var
  NewTab: TTabSheet;
begin

  NewTab := TTabSheet.Create(ControlTxt);
  NewTab.Parent := ControlTxt;
  NewTab.Caption := aCaption;
  NewTab.PageControl := ControlTxt;

  Result := TMemo.Create(NewTab);
  Result.Parent := NewTab;
  Result.Align := alClient;
  Result.ScrollBars := ssAutoBoth;
  Result.Clear;
  Result.Visible := True;

  // Switch to the new tab
  ControlTxt.ActivePage := NewTab;
end;

//------------------------------------------------------------------------------
// Add a new hex tab and return the TMemo control
//------------------------------------------------------------------------------

function TMainForm.AddHexTab(const aCaption: string): TMemo;
var
  NewTab: TTabSheet;
begin
  NewTab := TTabSheet.Create(HexControl);
  NewTab.Parent := HexControl;
  NewTab.Caption := aCaption;
  NewTab.PageControl := HexControl;

  Result := TMemo.Create(NewTab);
  Result.Parent := NewTab;
  Result.Align := alClient;
  Result.ScrollBars := ssAutoBoth;
  Result.Font.Name := 'Courier New';
  Result.Font.Size := 9;
  Result.Clear;
  Result.Visible := True;

  // Switch to the new tab
  HexControl.ActivePage := NewTab;
end;

procedure TMainForm.ParseIndexList(const InputText: string; OutputList: TStringList);
var
  Parts: TStringList;
  I, J: integer;
  RangeStart, RangeEnd: integer;
  aText: string;
begin
  OutputList.Clear;
  if Trim(InputText) = '' then Exit;

  Parts := TStringList.Create;
  try
    // Separar por comas
    Parts.CommaText := InputText;

    for I := 0 to Parts.Count - 1 do
    begin
      aText := Trim(Parts[I]);
      if aText = '' then Continue;

      // Verificar si es un rango (ej: "0-5")
      if Pos('-', aText) > 0 then
      begin
        RangeStart := StrToIntDef(Copy(aText, 1, Pos('-', aText) - 1), -1);
        RangeEnd := StrToIntDef(Copy(aText, Pos('-', aText) + 1, Length(aText)), -1);

        if (RangeStart >= 0) and (RangeEnd >= RangeStart) then
        begin
          for J := RangeStart to RangeEnd do
            OutputList.Add(IntToStr(J));
        end;
      end
      else
      begin
        // Índice simple
        if StrToIntDef(aText, -1) >= 0 then
          OutputList.Add(aText);
      end;
    end;
  finally
    Parts.Free;
  end;
end;

procedure TMainForm.DisplayTestResults(const MultiResult: TMultiResLoadResult);
var
  I: integer;
  ImageCount, TextCount, HexCount: integer;
begin
  ImageCount := 0;
  TextCount := 0;
  HexCount := 0;

  // Mostrar imágenes expandidas (para grupos de iconos)
  if Length(MultiResult.ImageStreams) > 0 then
  begin
    Memo1.Lines.Add(Format('  Displaying %d expanded images:', [Length(MultiResult.ImageStreams)]));

    for I := 0 to Length(MultiResult.ImageStreams) - 1 do
    begin
      if (MultiResult.ImageStreams[I] <> nil) and (I < Length(MultiResult.Items)) then
      begin
        with MultiResult.Items[I] do
        begin
          DisplayLoadedImage(MultiResult.Items[I],
            Format('Image %d - %s (%s)', [I, MultiResult.ImageDimensions[I], ResourceCategoryName]));
          Inc(ImageCount);
          DisplayLoadedHex(MultiResult.Items[I]);
          Inc(HexCount);
        end;
      end;
    end;
    Exit; //Exit after showing the group (maybe a switch can solve the repetition of rendering
  end;

  // Mostrar items regulares - TODA LA INFO YA VIENE DEL MANAGER
  for I := 0 to MultiResult.Count - 1 do
  begin
    if not MultiResult.Items[I].Success then Continue;

    with MultiResult.Items[I] do
    begin
      Memo1.Lines.Add(Format('  Item %d: %s | Type: %s | MIME: %s | View: %s', [I, ResourceCategoryName, ResourceTypeName, MimeTypeName, DefaultView]));
      DisplayLoadedHex(MultiResult.Items[I]);
      Inc(HexCount);
      case ResourceCategory of
        rcText:
        begin
          DisplayLoadedText(MultiResult.Items[I]);
          Inc(TextCount);
        end;

        rcImage:
        begin
          DisplayLoadedImage(MultiResult.Items[I]);
          Inc(ImageCount);
        end;
      end;
    end;
  end;

  // Resumen
  Memo1.Lines.Add('');
  Memo1.Lines.Add('=== Display Summary ===');
  Memo1.Lines.Add(Format('Images: %d', [ImageCount]));
  Memo1.Lines.Add(Format('Text: %d', [TextCount]));
  Memo1.Lines.Add(Format('Hex: %d', [HexCount]));

  // Activar pestaña apropiada
  if ImageCount > 0 then
    MainPagecontrol.ActivePage := ImgTab
  else if TextCount > 0 then
    MainPagecontrol.ActivePage := TxtTab
  else
    MainPagecontrol.ActivePage := HexTab;
end;

//------------------------------------------------------------------------------
// EDICIÓN
//------------------------------------------------------------------------------

procedure TMainForm.VirtualStringTree1DragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: longword; Mode: TDropMode);
var
  SourceNode, TargetNode: PVirtualNode;
  SourceData, TargetData: PVirtualNodeData;
  NewTypeName: string;
  NewType: TResType;
begin
  SourceNode := Sender.FocusedNode;
  TargetNode := Sender.GetNodeAt(Pt.X, Pt.Y);

  if (SourceNode = nil) or (TargetNode = nil) then Exit;

  SourceData := Sender.GetNodeData(SourceNode);
  TargetData := Sender.GetNodeData(TargetNode);

  if not Assigned(SourceData) or not Assigned(TargetData) then Exit;

  // El target debe ser un grupo (tipo de recurso)
  if TargetData^.NodeType <> ntGroup then Exit;

  // Extraer el nombre del tipo del grupo (ej: "HTML Resources" -> "HTML")
  NewTypeName := Copy(TargetData^.DisplayName, 1, Pos(' ', TargetData^.DisplayName) - 1);

  // Convertir a TResType
  if TryStrToInt(NewTypeName, NewType.IntValue) then
    NewType.IsInteger := True
  else
  begin
    NewType.IsInteger := False;
    NewType.StrValue := UpperCase(NewTypeName);
  end;

  // Usar la API de operations para cambiar el tipo
  if ResOp_ChangeResourceType(SourceData^.ResourceIndex, NewType, False) then
  begin
    // Refrescar el árbol para que el recurso aparezca en su nuevo grupo
    RefreshResourceTree;
    RefreshComboBoxes;
    FResourcesChanged := True;
    UpdateWindowTitle;
    Memo1.Lines.Add(Format('  ✓ Resource "%s" moved to type "%s"',
      [SourceData^.DisplayName, NewTypeName]));
  end
  else
  begin
    MessageDlg('Drag Drop Failed',
      Format('Could not move resource: %s', [ResOp_GetLastError]),
      mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.VirtualStringTree1DragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: longword; var Accept: boolean);
var
  TargetNode: PVirtualNode;
  TargetData: PVirtualNodeData;
begin
  Accept := False;
  TargetNode := Sender.GetNodeAt(Pt.X, Pt.Y);
  if TargetNode = nil then Exit;
  TargetData := Sender.GetNodeData(TargetNode);
  if not Assigned(TargetData) then Exit;
  // Solo se puede soltar sobre nodos de grupo (tipos de recurso)
  Accept := (TargetData^.NodeType = ntGroup);
end;

procedure TMainForm.VirtualStringTree1Edited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  FResourcesChanged := True;
  UpdateWindowTitle;
end;

procedure TMainForm.VirtualStringTree1Editing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  Data: PVirtualNodeData;
  ResType: TResType;
begin
  Allowed := False;

  if Column <> 0 then Exit;

  Data := VirtualStringTree1.GetNodeData(Node);
  if not Assigned(Data) then Exit;

  if Data^.NodeType <> ntResource then Exit;

  // Convertir tipo para validación (aunque aquí solo permitimos/no permitimos)
  if TryStrToInt(Data^.ResTypeName, ResType.IntValue) then
    ResType.IsInteger := True
  else
  begin
    ResType.IsInteger := False;
    ResType.StrValue := Data^.ResTypeName;
  end;

  // No permitir editar nombres de iconos/cursores/grupos (deben ser numéricos)
  if ResType.IsInteger and (ResType.IntValue in [1, 3, 12, 14, 21, 22]) then
    Allowed := False
  else
    Allowed := True;
end;

procedure TMainForm.VirtualStringTree1NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: string);
var
  Data: PVirtualNodeData;
  OldName: string;
  ResType: TResType;
begin
  if Column <> 0 then Exit;

  Data := VirtualStringTree1.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;

  if Data^.DisplayName = NewText then Exit;  // Sin cambios

  OldName := Data^.DisplayName;

  // Validar el nuevo nombre
  if Trim(NewText) = '' then
  begin
    MessageDlg('Invalid Name', 'Resource name cannot be empty.', mtError, [mbOK], 0);
    VirtualStringTree1.Text[Node, Column] := OldName;
    Exit;
  end;

  // Convertir ResTypeName a TResType para validación
  if TryStrToInt(Data^.ResTypeName, ResType.IntValue) then
    ResType.IsInteger := True
  else
  begin
    ResType.IsInteger := False;
    ResType.StrValue := Data^.ResTypeName;
  end;

  // Validar usando la API de operations
  if not ResOp_ValidateResourceName(NewText, ResType) then
  begin
    MessageDlg('Invalid Name',
      Format('Name "%s" is already in use or invalid for this resource type.', [NewText]),
      mtError, [mbOK], 0);
    VirtualStringTree1.Text[Node, Column] := OldName;
    Exit;
  end;

  // === USAR LA API DE OPERATIONS ===
  if ResOp_RenameResource(Data^.ResourceIndex, NewText, False) then
  begin
    // Actualizar el árbol
    Data^.DisplayName := NewText;
    VirtualStringTree1.InvalidateNode(Node);
    RefreshComboBoxes;
    // Marcar cambios
    FResourcesChanged := True;
    UpdateWindowTitle;

    Memo1.Lines.Add(Format('  ✓ Resource renamed: "%s" -> "%s" (using ResOp)',
      [OldName, NewText]));
  end
  else
  begin
    MessageDlg('Rename Failed',
      Format('Could not rename resource: %s', [ResOp_GetLastError]),
      mtError, [mbOK], 0);
    VirtualStringTree1.Text[Node, Column] := OldName;
  end;
end;

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
  IsGroup: boolean;
begin
  // Habilitar/deshabilitar según lo que esté seleccionado
  Node := VirtualStringTree1.GetFirstSelected;
  if Node <> nil then
  begin
    Data := VirtualStringTree1.GetNodeData(Node);
    IsGroup := (Data <> nil) and (Data^.NodeType = ntResource) and (Data^.ResTypeName = 'GROUP_ICON') or (Data^.ResTypeName = 'GROUP_CURSOR') or (Data^.ResTypeName = 'CURSOR') or (Data^.ResTypeName = 'ICON');
    MenuItem1.Enabled := (Data <> nil) and (Data^.NodeType = ntResource);
    MenuItem2.Enabled := (Data <> nil) and (Data^.NodeType = ntResource);
    MenuItemChangeType.Enabled := (Data <> nil) and (Data^.NodeType = ntResource) and not IsGroup;
    MenuItemDelete.Enabled := (Data <> nil) and (Data^.NodeType = ntResource); // se puede elimin
  end
  else
  begin
    MenuItem1.Enabled := False;
    MenuItem2.Enabled := False;
    MenuItemChangeType.Enabled := False;
    MenuItemDelete.Enabled := False;
  end;
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
begin
  Node := VirtualStringTree1.GetFirstSelected;
  if Node = nil then Exit;

  Data := VirtualStringTree1.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;

  // Directo - ResTypeName ya es string
  if ResOp_ExportResource(Data^.DisplayName, Data^.ResTypeName, 2) then
    Memo1.Lines.Add(Format('  ✓ Resource "%s" exported successfully', [Data^.DisplayName]))
  else
    Memo1.Lines.Add(Format('  ✗ Failed to export "%s": %s',
      [Data^.DisplayName, ResOp_GetLastError]));
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
begin
  Node := VirtualStringTree1.GetFirstSelected;
  if Node = nil then Exit;

  Data := VirtualStringTree1.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;

  if ResOp_ExportResource(Data^.DisplayName, Data^.ResTypeName, 0) then
    Memo1.Lines.Add(Format('  ✓ Resource "%s" exported successfully', [Data^.DisplayName]))
  else
    Memo1.Lines.Add(Format('  ✗ Failed to export "%s"', [Data^.DisplayName]));
end;

// Declarar el método en la clase TMainForm (en la sección public o private)

// Implementación
procedure TMainForm.MenuItemAddResourceClick(Sender: TObject);
var
  AddForm: TfrmAddResource;
begin
  AddForm := TfrmAddResource.Create(Self);
  try
    if AddForm.ShowModal = mrOk then
    begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('=== Importing Resource ===');
      Memo1.Lines.Add(Format('  Name: "%s"', [AddForm.ResourceName]));
      Memo1.Lines.Add(Format('  Type: "%s"', [ResType_ToString(AddForm.ResourceType)]));
      Memo1.Lines.Add(Format('  Language: %d', [AddForm.LanguageID]));
      Memo1.Lines.Add(Format('  File: "%s"', [AddForm.SelectedFile]));

      if ResOp_ImportResource(AddForm.SelectedFile,
                              AddForm.ResourceName,
                              AddForm.ResourceType,
                              AddForm.LanguageID,
                              False) then
      begin
        RefreshResourceTree;
        RefreshComboBoxes;
        FResourcesChanged := True;
        UpdateWindowTitle;
        Memo1.Lines.Add(Format('  ✓ Resource "%s" imported successfully', [AddForm.ResourceName]));
      end
      else
        Memo1.Lines.Add(Format('  ✗ Failed to import resource: %s', [ResOp_GetLastError]));
    end;
  finally
    AddForm.Free;
  end;
end;

procedure TMainForm.MenuItemChangeTypeClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
  NewTypeStr: string;
  NewType: TResType;
begin
  Node := VirtualStringTree1.GetFirstSelected;
  if Node = nil then Exit;

  Data := VirtualStringTree1.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;

  // Diálogo para elegir nuevo tipo
  if InputQuery('Change Resource Type',
     Format('Enter new type for "%s":', [Data^.DisplayName]),
     NewTypeStr) then
  begin
    // Validar que no está vacío
    if Trim(NewTypeStr) = '' then
    begin
      MessageDlg('Invalid Type', 'Resource type cannot be empty.', mtError, [mbOK], 0);
      Exit;
    end;

    // Convertir a TResType
    if TryStrToInt(NewTypeStr, NewType.IntValue) then
      NewType.IsInteger := True
    else
    begin
      NewType.IsInteger := False;
      NewType.StrValue := UpperCase(NewTypeStr);
    end;

    // Validar que el tipo es válido (opcional, pero buena práctica)
    // Podríamos verificar que no hay otro recurso con el mismo nombre y nuevo tipo

    // === USAR LA API DE OPERATIONS ===
    if ResOp_ChangeResourceType(Data^.ResourceIndex, NewType, False) then
    begin
      // Actualizar el nombre del tipo en el nodo
      if NewType.IsInteger then
        Data^.ResTypeName := ResType_ToDisplayString(NewType)
      else
        Data^.ResTypeName := NewType.StrValue;

      // Refrescar el árbol para que se reordene si es necesario
      RefreshResourceTree;
      RefreshComboBoxes;
      // Marcar cambios
      FResourcesChanged := True;
      UpdateWindowTitle;

      Memo1.Lines.Add(Format('  ✓ Resource "%s" type changed to "%s" (using ResOp)',
        [Data^.DisplayName, Data^.ResTypeName]));
    end
    else
    begin
      MessageDlg('Change Failed',
        Format('Could not change resource type: %s', [ResOp_GetLastError]),
        mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.MenuItemDeleteClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
  ResName: string;
begin
  Node := VirtualStringTree1.GetFirstSelected;
  if Node = nil then Exit;

  Data := VirtualStringTree1.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;

  ResName := Data^.DisplayName;

  if MessageDlg('Confirm Delete',
     Format('Are you sure you want to delete resource "%s"?', [ResName]),
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if ResOp_DeleteResource(Data^.ResourceIndex, False) then
    begin
      RefreshResourceTree;
      RefreshComboBoxes;
      FResourcesChanged := True;
      UpdateWindowTitle;
      Memo1.Lines.Add(Format('  ✓ Resource "%s" deleted', [ResName]));
    end
    else
      Memo1.Lines.Add(Format('  ✗ Failed to delete "%s": %s', [ResName, ResOp_GetLastError]));
  end;
end;

procedure TMainForm.UpdateWindowTitle;
begin
  if FResourcesChanged then
    Caption := 'Resource Viewer - ' + ExtractFileName(ResOp_GetFileName) + ' *'
  else if ResOp_GetFileName <> '' then
    Caption := 'Resource Viewer - ' + ExtractFileName(ResOp_GetFileName)
  else
    Caption := 'Resource Viewer';
end;

procedure TMainForm.RefreshResourceTree;
var
  OldSelectedNode: PVirtualNode;
  OldSelectedData: PVirtualNodeData;
  OldName: string;
  OldType: string;
  I: Integer;
begin
  // Guardar la selección actual si existe
  OldSelectedNode := VirtualStringTree1.GetFirstSelected;
  if OldSelectedNode <> nil then
  begin
    OldSelectedData := VirtualStringTree1.GetNodeData(OldSelectedNode);
    if Assigned(OldSelectedData) then
    begin
      OldName := OldSelectedData^.DisplayName;
      OldType := OldSelectedData^.ResTypeName;
    end;
  end;

  // Limpiar y reconstruir el árbol
  ListAllResources;

  // Intentar restaurar la selección anterior
  if OldName <> '' then
  begin
    for I := 0 to VirtualStringTree1.RootNodeCount - 1 do
      FindAndSelectNode(VirtualStringTree1.GetFirstLevel(I), OldName, OldType);
  end;

  // Marcar que no hay cambios pendientes (opcional)
  // FResourcesChanged := False;
  // UpdateWindowTitle;
end;

procedure TMainForm.RefreshComboBoxes;
begin
  FillComboBoxes;  // Ya existe y usa ResOp_GetTextResources/ResOp_GetImageResources
end;

function TMainForm.FindAndSelectNode(StartNode: PVirtualNode; const aName, TypeName: string): Boolean;
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
begin
  Result := False;
  Node := StartNode;

  while Node <> nil do
  begin
    Data := VirtualStringTree1.GetNodeData(Node);
    if Assigned(Data) and (Data^.NodeType = ntResource) and
       (Data^.DisplayName = aName) and (Data^.ResTypeName = TypeName) then
    begin
      VirtualStringTree1.Selected[Node] := True;
      VirtualStringTree1.FocusedNode := Node;
      VirtualStringTree1.ScrollIntoView(Node, True);
      Exit(True);
    end;

    // Buscar en hijos
    if VirtualStringTree1.HasChildren[Node] then
    begin
      if FindAndSelectNode(VirtualStringTree1.GetFirstChild(Node), Name, TypeName) then
        Exit(True);
    end;

    Node := VirtualStringTree1.GetNextSibling(Node);
  end;
end;

end.
