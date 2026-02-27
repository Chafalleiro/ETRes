unit uAddRes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uResOperations, uResDefs, uMimeDetect, uDebug;

type

  { TfrmAddResource }

  TfrmAddResource = class(TForm)
    btnLoad: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    cmbType: TComboBox;
    cmbLanguage: TComboBox;
    edtName: TEdit;
    lblLanguage: TLabel;
    lblType: TLabel;
    lblName: TLabel;
    OpenDialog1: TOpenDialog;
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSelectedFile: string;
    function GetLanguageID: Word;
    function GetResourceType: TResType;
    function GetResourceName: string;
    function GetSelectedFile: string;
  public
    FDetectedMime: TMimeType;  // <-- AÑADIR ESTA LÍNEA
    property SelectedFile: string read GetSelectedFile;
    property ResourceName: string read GetResourceName;
    property ResourceType: TResType read GetResourceType;
    property LanguageID: Word read GetLanguageID;
  end;

var
  frmAddResource: TfrmAddResource;

implementation

{$R *.frm}

procedure TfrmAddResource.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Inicializar combo de idiomas con valores numéricos
  cmbLanguage.Clear;
  for i := 0 to High(LanguageList) do
    cmbLanguage.Items.Add(LanguageList[i].Name);
  cmbLanguage.ItemIndex := 0; // Neutral por defecto

  // Hacer que el combo de tipo sea editable
  cmbType.Style := csDropDown;
end;

procedure TfrmAddResource.btnLoadClick(Sender: TObject);
var
  FileStream: TMemoryStream;
  Ext: string;
  SuggestedType: string;
  FileSize: Int64;
begin
  if OpenDialog1.Execute then
  begin
    FSelectedFile := OpenDialog1.FileName;

    // Detectar MIME type del archivo
    FileStream := TMemoryStream.Create;
    try
      FileStream.LoadFromFile(FSelectedFile);
      FileStream.Position := 0;
      FDetectedMime := TMimeDetector.DetectFromStream(FileStream);
    finally
      FileStream.Free;
    end;

    // Sugerir un tipo basado en la extensión o MIME
    Ext := LowerCase(ExtractFileExt(FSelectedFile));
    if Ext = '.txt' then SuggestedType := 'TEXT'
    else if Ext = '.html' then SuggestedType := 'HTML'
    else if Ext = '.css' then SuggestedType := 'CSS'
    else if Ext = '.js' then SuggestedType := 'JS'
    else if Ext = '.xml' then SuggestedType := 'XML'
    else if Ext = '.json' then SuggestedType := 'JSON'
    else if Ext = '.png' then SuggestedType := 'PNG'
    else if Ext = '.jpg' then SuggestedType := 'JPG'
    else if Ext = '.jpeg' then SuggestedType := 'JPG'
    else if Ext = '.gif' then SuggestedType := 'GIF'
    else if Ext = '.bmp' then SuggestedType := 'BMP'
    else if Ext = '.ico' then SuggestedType := 'ICO'
    else if Ext = '.cur' then SuggestedType := 'CURSOR'
    else if Ext = '.ani' then SuggestedType := 'ANI'
    else if Ext = '.wav' then SuggestedType := 'WAVE'
    else if Ext = '.mp3' then SuggestedType := 'MP3'
    else if Ext = '.avi' then SuggestedType := 'AVI'
    else if Ext = '.etchnk' then SuggestedType := 'CHUNK'
    else if Ext = '.ettpl' then SuggestedType := 'ETEMPLATE'
    else
      SuggestedType := UpperCase(Copy(Ext, 2, Length(Ext)-1));

    // Si el combo contiene el tipo sugerido, seleccionarlo; si no, añadirlo
    if cmbType.Items.IndexOf(SuggestedType) >= 0 then
      cmbType.Text := SuggestedType
    else
      cmbType.Text := SuggestedType;

    // Sugerir nombre a partir del nombre del archivo (sin extensión)
    edtName.Text := ChangeFileExt(ExtractFileName(FSelectedFile), '');

    // Depuración
    DebugInfo(Format('[btnLoadClick] File loaded: "%s", Size=%d, Detected MIME=%s',
      [FSelectedFile, FileSize, TMimeDetector.TypeToString(FDetectedMime)]));
  end;
end;

procedure TfrmAddResource.btnCancelClick(Sender: TObject);
begin

end;

function TfrmAddResource.GetLanguageID: Word;
var
  idx: Integer;
begin
  idx := cmbLanguage.ItemIndex;
  if (idx >= 0) and (idx <= High(LanguageList)) then
    Result := LanguageList[idx].ID
  else
    Result := 0;
end;

function TfrmAddResource.GetResourceType: TResType;
var
  s: string;
begin
  s := Trim(cmbType.Text);
  if TryStrToInt(s, Result.IntValue) then
    Result.IsInteger := True
  else
  begin
    Result.IsInteger := False;
    Result.StrValue := UpperCase(s);
  end;
end;

function TfrmAddResource.GetResourceName: string;
begin
  Result := edtName.Text;
end;

function TfrmAddResource.GetSelectedFile: string;
begin
  Result := FSelectedFile;
end;

procedure TfrmAddResource.btnOkClick(Sender: TObject);
begin
  // Validar que se haya seleccionado un archivo
  if FSelectedFile = '' then
  begin
    MessageDlg('Error', 'No file selected.', mtError, [mbOK], 0);
    ModalResult := mrNone;
    Exit;
  end;

  // Validar nombre
  if Trim(edtName.Text) = '' then
  begin
    MessageDlg('Error', 'Resource name cannot be empty.', mtError, [mbOK], 0);
    ModalResult := mrNone;
    Exit;
  end;

  // Validar tipo
  if Trim(cmbType.Text) = '' then
  begin
    MessageDlg('Error', 'Resource type cannot be empty.', mtError, [mbOK], 0);
    ModalResult := mrNone;
    Exit;
  end;
  DebugInfo(Format('[AddRes] OK clicked: Name="%s", Type="%s", Lang=%d, File="%s"',
    [edtName.Text, cmbType.Text, GetLanguageID, FSelectedFile]));
  // Si todo ok, el ModalResult se queda como mrOk (por defecto)
end;

end.
