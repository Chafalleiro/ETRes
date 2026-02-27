unit ETResControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics, Dialogs, Menus,
  VirtualTrees, uResOperations, uResDefs, uAddRes, uDebug;

type
  TResourceSelectedEvent = procedure(Sender: TObject; ResourceIndex: Integer) of object;

  PVirtualNodeData = ^TVirtualNodeData;
  TVirtualNodeData = record
    NodeType: (ntGroup, ntResource);
    ResourceIndex: Integer;
    DisplayName: string;
    ResTypeName: string;
    Size: Int64;
    LanguageID: Integer;
    DataOffset: Int64;
    MimeString: string;
    IsText: Boolean;
    IsImage: Boolean;
    Confidence: Integer;
  end;
  TETResExplorer = class(TCustomControl)
  private
    FTreeView: TVirtualStringTree;
    FPopupMenu: TPopupMenu;
    FFileName: string;
    FOnResourceSelected: TResourceSelectedEvent;
    FOnResourceLoaded: TNotifyEvent; // opcional

    procedure DoResourceSelected(ResourceIndex: Integer);
    procedure SetFileName(const AValue: string);

    procedure InitializeTree;
    procedure LoadResources;
    procedure DoExportClick(Sender: TObject);
    procedure DoExportAsClick(Sender: TObject);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  protected
    procedure Resize; override;
  public
//    procedure Register;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    function GetSelectedResource: Integer; // devuelve índice o -1
    property Tree: TVirtualStringTree read FTreeView;
    procedure LoadFromFile(const FileName: string);

    procedure TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    function GetSelectedResourceData(out Data: TMultiResLoadResult): Boolean;

    procedure FreeResourceData(var Data: TMultiResLoadResult);
  published
    property Align;
    property Anchors;
    property sFileName: string read FFileName write SetFileName;
    // otras propiedades estándar
    property OnResourceSelected: TResourceSelectedEvent read FOnResourceSelected write FOnResourceSelected;
    property OnResourceLoaded: TNotifyEvent read FOnResourceLoaded write FOnResourceLoaded;
  end;

  TETResEdit = class(TETResExplorer) // hereda del explorer y añade edición
  private
    procedure DoRename(Sender: TObject);
    procedure DoChangeType(Sender: TObject);
    procedure DoAdd(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoImport(Sender: TObject);
    procedure TreeEdit(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure TreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
  end;

procedure Register;   // <-- AÑADIR ESTA LÍNEA
implementation

{ TETResExplorer }

constructor TETResExplorer.Create(AOwner: TComponent);
var
  MI: TMenuItem;

begin
  inherited Create(AOwner);
  FTreeView := TVirtualStringTree.Create(Self);
  FTreeView.Parent := Self;
  FTreeView.Align := alClient;
  FTreeView.NodeDataSize := SizeOf(TVirtualNodeData);
  FTreeView.OnGetText := @TreeGetText;
  FTreeView.OnFreeNode := @TreeFreeNode;
  FTreeView.OnGetImageIndex := @TreeGetImageIndex;
  FTreeView.OnDblClick := @TreeDblClick;
  InitializeTree;
  // Popup menú
  FPopupMenu := TPopupMenu.Create(Self);
  // Añadir ítems Export y Export As
  MI := TMenuItem.Create(FPopupMenu);
  MI.Caption := 'Export Quick';
  MI.OnClick := @DoExportClick;
  FPopupMenu.Items.Add(MI);

  MI := TMenuItem.Create(FPopupMenu);
  MI.Caption := 'Export As...';
  MI.OnClick := @DoExportAsClick;
  FPopupMenu.Items.Add(MI);
  FTreeView.PopupMenu := FPopupMenu;

  FTreeView.OnFocusChanged := @TreeFocusChanged;
end;

procedure TETResExplorer.SetFileName(const aValue: string);
begin
  if FFileName = aValue then Exit;
  FFileName := aValue;
  if (csDesigning in ComponentState) then
  begin
    // En tiempo de diseño, podemos intentar cargar el archivo si existe
    // pero con cuidado de no generar errores molestos.
    if FileExists(aValue) then
    begin
      // No podemos usar ShowMessage en diseño, pero podemos cargar silenciosamente
      ResOp_OpenFile(aValue);
      Refresh;
    end;
  end
  else
  begin
    // En tiempo de ejecución, cargamos directamente
    if FileExists(aValue) then
    begin
      ResOp_OpenFile(aValue);
      Refresh;
    end;
  end;
end;

procedure TETResExplorer.InitializeTree;
begin
  FTreeView.Header.Options := [hoColumnResize, hoVisible];
  with FTreeView.Header.Columns.Add do
  begin
    Text := 'Name/ID';
    Width := 200;
  end;
  with FTreeView.Header.Columns.Add do
  begin
    Text := 'Type';
    Width := 120;
  end;
  with FTreeView.Header.Columns.Add do
  begin
    Text := 'Size';
    Width := 80;
  end;
  with FTreeView.Header.Columns.Add do
  begin
    Text := 'Language';
    Width := 70;
  end;
  with FTreeView.Header.Columns.Add do
  begin
    Text := 'MIME';
    Width := 150;
  end;
end;

procedure TETResExplorer.LoadResources;
var
  Resources: TResListArray;
  I: Integer;
  Data: PVirtualNodeData;
  TypeNode, ResNode: PVirtualNode;
  TypeGroups: TStringList;
  TypeName: string;
  ResInfo: TResListInfo;
begin
  FTreeView.Clear;
  FTreeView.BeginUpdate;
  try
    Resources := ResOp_GetAllResources;
    TypeGroups := TStringList.Create;
    try
      TypeGroups.Sorted := True;
      TypeGroups.Duplicates := dupIgnore;
      for I := 0 to High(Resources) do
        TypeGroups.Add(Resources[I].DisplayType);
      TypeGroups.Sort;
      for I := 0 to TypeGroups.Count - 1 do
      begin
        TypeName := TypeGroups[I];
        TypeNode := FTreeView.AddChild(nil);
        Data := FTreeView.GetNodeData(TypeNode);
        if Assigned(Data) then
        begin
          Data^.NodeType := ntGroup;
          Data^.DisplayName := TypeName + ' Resources';
          Data^.ResourceIndex := -1;
        end;
        FTreeView.Expanded[TypeNode] := True;
        for ResInfo in Resources do
        begin
          if ResInfo.DisplayType = TypeName then
          begin
            ResNode := FTreeView.AddChild(TypeNode);
            Data := FTreeView.GetNodeData(ResNode);
            if Assigned(Data) then
            begin
              Data^.NodeType := ntResource;
              Data^.ResourceIndex := ResInfo.Index;
              Data^.DisplayName := ResInfo.Name;
              Data^.ResTypeName := ResInfo.DisplayType;
              Data^.Size := ResInfo.Size;
              Data^.LanguageID := ResInfo.LanguageID;
              Data^.DataOffset := ResInfo.DataOffset;
              Data^.MimeString := ResInfo.MimeString;
              Data^.IsText := ResInfo.IsText;
              Data^.IsImage := ResInfo.IsImage;
              Data^.Confidence := Ord(ResInfo.IsText) or Ord(ResInfo.IsImage);
            end;
          end;
        end;
      end;
    finally
      TypeGroups.Free;
    end;
  finally
    FTreeView.EndUpdate;
  end;
end;

procedure TETResExplorer.Refresh;
begin
  LoadResources;
end;

procedure TETResExplorer.FreeResourceData(var Data: TMultiResLoadResult);
begin
  ResOp_FreeResourceData(Data);
end;

procedure TETResExplorer.TreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PVirtualNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then Exit;
  case Column of
    0: CellText := Data^.DisplayName;
    1: if Data^.NodeType = ntGroup then CellText := '' else CellText := Data^.ResTypeName;
    2: if Data^.NodeType = ntGroup then CellText := Format('[%d items]', [Sender.ChildCount[Node]])
       else CellText := IntToStr(Data^.Size);
    3: if (Data^.NodeType = ntResource) and (Data^.LanguageID > 0) then CellText := IntToStr(Data^.LanguageID);
    4: if Data^.NodeType = ntResource then CellText := Data^.MimeString;
  end;
end;

procedure TETResExplorer.TreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PVirtualNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    Data^.DisplayName := '';
    Data^.ResTypeName := '';
    Data^.MimeString := '';
  end;
end;

procedure TETResExplorer.TreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PVirtualNodeData;
begin
  if Column <> 0 then Exit;
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then Exit;
  if Data^.NodeType = ntGroup then
    ImageIndex := 0  // Folder icon
  else if Data^.IsImage then
    ImageIndex := 1  // Image icon
  else if Data^.IsText then
    ImageIndex := 2  // Text icon
  else
    ImageIndex := 3; // Generic
end;

procedure TETResExplorer.TreeDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
  FormatFilter: string;
  MultiResult: TMultiResLoadResult;
begin
  Node := FTreeView.GetFirstSelected;
  if Node = nil then Exit;
  Data := FTreeView.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;
  if Data^.IsImage then
    FormatFilter := 'IMAGE'
  else if Data^.IsText then
    FormatFilter := 'TEXT'
  else
    FormatFilter := '';
  MultiResult := ResOp_LoadResource([Data^.DisplayName, Data^.ResTypeName, 0, FormatFilter, 0]);
  // Aquí podrías lanzar un evento OnResourceLoaded o mostrar un visor por defecto.
  // Por ahora, solo mostramos en memo si existe, pero en un componente lo mejor es un evento.
  if not MultiResult.Success then
    ShowMessage('Error: ' + MultiResult.ErrorMessage);
end;

procedure TETResExplorer.DoExportClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
begin
  Node := FTreeView.GetFirstSelected;
  if Node = nil then Exit;
  Data := FTreeView.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;
  if ResOp_ExportResource(Data^.DisplayName, Data^.ResTypeName, 2) then
    ShowMessage('Exported successfully')
  else
    ShowMessage('Export failed: ' + ResOp_GetLastError);
end;

procedure TETResExplorer.DoExportAsClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
begin
  Node := FTreeView.GetFirstSelected;
  if Node = nil then Exit;
  Data := FTreeView.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;
  if ResOp_ExportResource(Data^.DisplayName, Data^.ResTypeName, 0) then
    ShowMessage('Exported successfully')
  else
    ShowMessage('Export failed: ' + ResOp_GetLastError);
end;

procedure TETResExplorer.LoadFromFile(const FileName: string);
begin
  if ResOp_OpenFile(FileName) then
    LoadResources
  else
    ShowMessage('Load failed: ' + ResOp_GetLastError);
end;

function TETResExplorer.GetSelectedResource: Integer;
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
begin
  Result := -1;
  Node := FTreeView.GetFirstSelected;
  if Node = nil then Exit;
  Data := FTreeView.GetNodeData(Node);
  if Assigned(Data) and (Data^.NodeType = ntResource) then
    Result := Data^.ResourceIndex;
end;

procedure TETResExplorer.TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PVirtualNodeData;
  ResIndex: Integer;
begin
  if Assigned(Node) then
  begin
    Data := Sender.GetNodeData(Node);
    if Assigned(Data) and (Data^.NodeType = ntResource) then
      ResIndex := Data^.ResourceIndex
    else
      ResIndex := -1;
  end
  else
    ResIndex := -1;

  DoResourceSelected(ResIndex);
end;

procedure TETResExplorer.DoResourceSelected(ResourceIndex: Integer);
begin
  if Assigned(FOnResourceSelected) then
    FOnResourceSelected(Self, ResourceIndex);
end;

function TETResExplorer.GetSelectedResourceData(out Data: TMultiResLoadResult): Boolean;
var
  Node: PVirtualNode;
  Info: PVirtualNodeData;
begin
  Result := False;
  // Inicializar TODOS los campos de la estructura
  Data.Success := False;
  Data.Count := 0;
  Data.ErrorMessage := '';
  SetLength(Data.Items, 0);
  Data.Combined := nil;
  SetLength(Data.ImageStreams, 0);
  SetLength(Data.ImageMimeTypes, 0);
  SetLength(Data.ImageDimensions, 0);
  SetLength(Data.ImageCategories, 0);


  Node := FTreeView.GetFirstSelected;
  if Node = nil then Exit;
  Info := FTreeView.GetNodeData(Node);
  if not Assigned(Info) or (Info^.NodeType <> ntResource) then Exit;

  // Llamada directa a ResOp_LoadResource
  Data := ResOp_LoadResource([Info^.DisplayName, Info^.ResTypeName, 0, '', 0]);
  if Data.Success then
  Result := Data.Success
  else
  Data.Success := False;
  Result := Data.Success;
end;

procedure TETResExplorer.Resize;
begin
  inherited;
  if Assigned(FTreeView) then
    FTreeView.SetBounds(0, 0, Width, Height);
end;

destructor TETResExplorer.Destroy;
begin
  // Limpieza adicional si es necesaria
  inherited;
end;

{ TETResEdit }

constructor TETResEdit.Create(AOwner: TComponent);
var
  MI: TMenuItem;
begin
  inherited Create(AOwner);
  // Añadir más ítems al menú: Rename, Change Type, Add, Delete, Import
  // Añadir más ítems al menú
  MI := TMenuItem.Create(FPopupMenu);
  MI.Caption := 'Rename';
  MI.OnClick := @DoRename;
  FPopupMenu.Items.Add(MI);

  MI := TMenuItem.Create(FPopupMenu);
  MI.Caption := 'Change Type...';
  MI.OnClick := @DoChangeType;
  FPopupMenu.Items.Add(MI);

  MI := TMenuItem.Create(FPopupMenu);
  MI.Caption := 'Add Resource...';
  MI.OnClick := @DoAdd;
  FPopupMenu.Items.Add(MI);

  MI := TMenuItem.Create(FPopupMenu);
  MI.Caption := 'Delete';
  MI.OnClick := @DoDelete;
  FPopupMenu.Items.Add(MI);

  MI := TMenuItem.Create(FPopupMenu);
  MI.Caption := 'Import...';
  MI.OnClick := @DoImport;
  FPopupMenu.Items.Add(MI);
  // Habilitar edición en el árbol
  FTreeView.TreeOptions.MiscOptions := FTreeView.TreeOptions.MiscOptions + [toEditable];
  FTreeView.OnEditing := @TreeEdit;
  FTreeView.OnNewText := @TreeNewText;
  FTreeView.OnFocusChanged := @TreeFocusChanged;
end;


procedure TETResEdit.DoRename(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := FTreeView.GetFirstSelected;
  if Node = nil then Exit;
  FTreeView.EditNode(Node, 0); // Inicia edición en columna 0
end;

procedure TETResEdit.TreeEdit(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  Data: PVirtualNodeData;
begin
  Allowed := False;
  if Column <> 0 then Exit;
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;
  // Permitir editar solo ciertos tipos (no iconos/grupos numéricos)
  Allowed := not (Data^.ResTypeName = 'ICON') and not (Data^.ResTypeName = 'CURSOR')
             and not (Data^.ResTypeName = 'GROUP_ICON') and not (Data^.ResTypeName = 'GROUP_CURSOR');
end;

procedure TETResEdit.TreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const NewText: string);
var
  Data: PVirtualNodeData;
  ResType: TResType;
begin
  if Column <> 0 then Exit;
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;
  if Data^.DisplayName = NewText then Exit;
  // Validar
  if Trim(NewText) = '' then
  begin
    ShowMessage('Name cannot be empty');
    Exit;
  end;
  // Convertir tipo para validación
  if TryStrToInt(Data^.ResTypeName, ResType.IntValue) then
    ResType.IsInteger := True
  else
  begin
    ResType.IsInteger := False;
    ResType.StrValue := Data^.ResTypeName;
  end;
  if not ResOp_ValidateResourceName(NewText, ResType) then
  begin
    ShowMessage('Name already in use or invalid');
    Exit;
  end;
  if ResOp_RenameResource(Data^.ResourceIndex, NewText, False) then
  begin
    Data^.DisplayName := NewText;
    Sender.InvalidateNode(Node);
  end
  else
    ShowMessage('Rename failed: ' + ResOp_GetLastError);
end;

procedure TETResEdit.DoChangeType(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
  NewTypeStr: string;
  NewType: TResType;
begin
  Node := FTreeView.GetFirstSelected;
  if Node = nil then Exit;
  Data := FTreeView.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;
  if InputQuery('Change Type', 'Enter new type (integer or string):', NewTypeStr) then
  begin
    if Trim(NewTypeStr) = '' then Exit;
    if TryStrToInt(NewTypeStr, NewType.IntValue) then
      NewType.IsInteger := True
    else
    begin
      NewType.IsInteger := False;
      NewType.StrValue := UpperCase(NewTypeStr);
    end;
    if ResOp_ChangeResourceType(Data^.ResourceIndex, NewType, False) then
    begin
      Data^.ResTypeName := ResType_ToDisplayString(NewType);
      FTreeView.InvalidateNode(Node);
      // Podríamos refrescar todo el árbol si cambia de grupo
      LoadResources;
    end
    else
      ShowMessage('Change failed: ' + ResOp_GetLastError);
  end;
end;

procedure TETResEdit.DoAdd(Sender: TObject);
begin
  // Aquí llamarías a un diálogo propio, por simplicidad, llamamos al de importar con un archivo vacío?
  // Mejor implementar un diálogo simple para nuevo recurso vacío o desde archivo.
  ShowMessage('Add not implemented yet');
end;

procedure TETResEdit.DoDelete(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PVirtualNodeData;
begin
  Node := FTreeView.GetFirstSelected;
  if Node = nil then Exit;
  Data := FTreeView.GetNodeData(Node);
  if not Assigned(Data) or (Data^.NodeType <> ntResource) then Exit;
  if MessageDlg('Confirm', 'Delete resource ' + Data^.DisplayName + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if ResOp_DeleteResource(Data^.ResourceIndex, False) then
      LoadResources
    else
      ShowMessage('Delete failed: ' + ResOp_GetLastError);
  end;
end;

procedure TETResEdit.DoImport(Sender: TObject);
var
  AddForm: TfrmAddResource;
begin
  AddForm := TfrmAddResource.Create(Self);
  try
    if AddForm.ShowModal = mrOk then
    begin
      if ResOp_ImportResource(AddForm.SelectedFile,
                              AddForm.ResourceName,
                              AddForm.ResourceType,
                              AddForm.LanguageID,
                              False) then
      begin
        LoadResources;
        ShowMessage('Resource imported');
      end
      else
        ShowMessage('Import failed: ' + ResOp_GetLastError);
    end;
  finally
    AddForm.Free;
  end;
end;

procedure TETResEdit.SaveToFile(const FileName: string);
begin
  if not ResOp_SaveFile(FileName) then
    ShowMessage('Save failed: ' + ResOp_GetLastError);
end;

procedure TETResEdit.LoadFromFile(const FileName: string);
begin
  if ResOp_OpenFile(FileName) then
    LoadResources
  else
    ShowMessage('Load failed: ' + ResOp_GetLastError);
end;

procedure Register;
begin
  RegisterComponents('ET Controls',[TETResExplorer,TETResEdit]);
  {$I etrescontrols_icon.ctrs}
end;
end.

