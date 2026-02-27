unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, ETResControls, uResDefs, uMimeDetect, uDebug;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnLoad: TButton;
    btnSaveRes: TButton;
    ETResEdit1: TETResEdit;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveResClick(Sender: TObject);
    procedure ETResEdit1ResourceSelected(Sender: TObject; ResourceIndex: integer);
    procedure FormDestroy(Sender: TObject);
  private
    FLoading: boolean;

    procedure ClearTabs;

    function AddTextTab(const ACaption: string): TMemo;
    function AddHexTab(const ACaption: string): TMemo;
    function AddImageTab(const ACaption: string): TImage;

    procedure DisplayResource(var ResData: TMultiResLoadResult);

    procedure DisplayLoadedImage(const LoadResult: TResLoadResult; const CustomCaption: string = '');
    procedure DisplayLoadedText(const LoadResult: TResLoadResult; const CustomCaption: string = '');
    procedure DisplayLoadedHex(const LoadResult: TResLoadResult; const CustomCaption: string = '');
    procedure DisplayLoadedImageFromStream(Stream: TMemoryStream; const aCaption: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.frm}

{ TForm1 }

//DISPLAY **************************************************
procedure TForm1.DisplayLoadedText(const LoadResult: TResLoadResult; const CustomCaption: string);
var
  Memo: TMemo;
  TextContent: string;
begin
  if not LoadResult.Success or (LoadResult.Data = nil) then
    Exit;
  Memo := AddTextTab(CustomCaption);
  LoadResult.Data.Position := 0;
  SetString(TextContent, pansichar(LoadResult.Data.Memory), LoadResult.Data.Size);
  Memo.Text := TextContent;
end;

procedure TForm1.DisplayLoadedImage(const LoadResult: TResLoadResult; const CustomCaption: string);
var
  Img: TImage;
  Jpeg: TJPEGImage;
  Png: TPNGImage;
  Bmp: TBitmap;
  aIcon: TIcon;
begin
  if not LoadResult.Success or (LoadResult.Data = nil) then
    Exit;
  Img := AddImageTab(CustomCaption);
  LoadResult.Data.Position := 0;
  case LoadResult.MimeType of
    mtBitmap:
    begin
      Bmp := TBitmap.Create;
      try
        Bmp.LoadFromStream(LoadResult.Data);
        Img.Picture.Assign(Bmp);
      finally
        Bmp.Free;
      end;
    end;
    mtIcon:
    begin
      aIcon := TIcon.Create;
      try
        aIcon.LoadFromStream(LoadResult.Data);
        Img.Picture.Assign(aIcon);
      finally
        aIcon.Free;
      end;
    end;
    mtJpeg:
    begin
      Jpeg := TJPEGImage.Create;
      try
        Jpeg.LoadFromStream(LoadResult.Data);
        Img.Picture.Assign(Jpeg);
      finally
        Jpeg.Free;
      end;
    end;
    mtPng:
    begin
      Png := TPNGImage.Create;
      try
        Png.LoadFromStream(LoadResult.Data);
        Img.Picture.Assign(Png);
      finally
        Png.Free;
      end;
    end;
    else
      ShowMessage('Unsupported image format: ' + LoadResult.MimeTypeName);
  end;
end;

procedure TForm1.DisplayLoadedHex(const LoadResult: TResLoadResult; const CustomCaption: string);
var
  Memo: TMemo;
  i, j, n: integer;
  Data: pbyte;
  Line: string;
begin
  if not LoadResult.Success or (LoadResult.Data = nil) then
    Exit;
  Memo := AddHexTab(CustomCaption);
  LoadResult.Data.Position := 0;
  Data := LoadResult.Data.Memory;
  Memo.Lines.BeginUpdate;
  try
    for i := 0 to (LoadResult.Data.Size - 1) div 16 do
    begin
      Line := Format('%.8x: ', [i * 16]);
      for j := 0 to 15 do
      begin
        n := i * 16 + j;
        if n < LoadResult.Data.Size then
          Line := Line + IntToHex(Data[n], 2) + ' '
        else
          Line := Line + '   ';
        if j = 7 then Line := Line + ' ';
      end;
      Line := Line + ' ';
      for j := 0 to 15 do
      begin
        n := i * 16 + j;
        if n < LoadResult.Data.Size then
        begin
          if (Data[n] >= 32) and (Data[n] <= 126) then
            Line := Line + Chr(Data[n])
          else
            Line := Line + '.';
        end
        else
          Line := Line + ' ';
      end;
      Memo.Lines.Add(Line);
    end;
  finally
    Memo.Lines.EndUpdate;
  end;
end;

procedure TForm1.DisplayLoadedImageFromStream(Stream: TMemoryStream; const aCaption: string);
var
  Img: TImage;
  Jpeg: TJPEGImage;
  Png: TPNGImage;
  Bmp: TBitmap;
  aIcon: TIcon;
  Mime: TMimeType;
begin
  if Stream = nil then Exit;
  Stream.Position := 0;
  Mime := TMimeDetector.DetectFromStream(Stream);
  Stream.Position := 0;

  Img := AddImageTab(aCaption);
  case Mime of
    mtBitmap:
    begin
      Bmp := TBitmap.Create;
      try
        Bmp.LoadFromStream(Stream);
        Img.Picture.Assign(Bmp);
      finally
        Bmp.Free;
      end;
    end;
    mtIcon:
    begin
      aIcon := TIcon.Create;
      try
        aIcon.LoadFromStream(Stream);
        Img.Picture.Assign(aIcon);
      finally
        aIcon.Free;
      end;
    end;
    mtJpeg:
    begin
      Jpeg := TJPEGImage.Create;
      try
        Jpeg.LoadFromStream(Stream);
        Img.Picture.Assign(Jpeg);
      finally
        Jpeg.Free;
      end;
    end;
    mtPng:
    begin
      Png := TPNGImage.Create;
      try
        Png.LoadFromStream(Stream);
        Img.Picture.Assign(Png);
      finally
        Png.Free;
      end;
    end;
    else
      ShowMessage('Unsupported image format in group');
  end;
end;

//TREE **************************************************

procedure TForm1.ETResEdit1ResourceSelected(Sender: TObject; ResourceIndex: integer);
var
  ResData: TMultiResLoadResult;
begin
  if FLoading then Exit;
  FLoading := True;
  try
    ClearTabs;
    if ETResEdit1.GetSelectedResourceData(ResData) then
    begin
      try
        DisplayResource(ResData);
      finally
        ETResEdit1.FreeResourceData(ResData);
      end;
    end;
  finally
    FLoading := False;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ClearTabs;
  Memo1.Clear;
end;

procedure TForm1.DisplayResource(var ResData: TMultiResLoadResult);
var
  i: integer;
begin
  if Length(ResData.ImageStreams) > 0 then
  begin
    for i := 0 to High(ResData.ImageStreams) do
      DisplayLoadedImageFromStream(ResData.ImageStreams[i],
        Format('Image %d', [i]));
    Exit; // <-- IMPORTANTE
  end;

  for i := 0 to ResData.Count - 1 do
  begin
    with ResData.Items[i] do
      case ResourceCategory of
        rcText: DisplayLoadedText(ResData.Items[i]);
        rcImage: DisplayLoadedImage(ResData.Items[i]);
        else
          DisplayLoadedHex(ResData.Items[i]);
      end;
  end;
end;

//TABS **************************************************
procedure TForm1.ClearTabs;
var
  i: integer;
begin
  if PageControl1 = nil then Exit;
  for i := PageControl1.PageCount - 1 downto 0 do
    PageControl1.Pages[i].Free;

  // Forzar actualización
  PageControl1.Invalidate;
end;

function TForm1.AddTextTab(const ACaption: string): TMemo;
var
  Tab: TTabSheet;
begin
  Tab := TTabSheet.Create(PageControl1);
  Tab.PageControl := PageControl1;
  Tab.Caption := ACaption;
  Result := TMemo.Create(Tab);
  Result.Parent := Tab;
  Result.Align := alClient;
  Result.ScrollBars := ssAutoBoth;
  Result.Font.Name := 'Courier New';
  Result.Font.Size := 10;
  Result.WordWrap := False;
  PageControl1.ActivePage := Tab;
end;

function TForm1.AddHexTab(const ACaption: string): TMemo;
begin
  Result := AddTextTab(ACaption); // mismo aspecto, el contenido se llenará después
end;

function TForm1.AddImageTab(const ACaption: string): TImage;
var
  Tab: TTabSheet;
begin
  Tab := TTabSheet.Create(PageControl1);
  Tab.PageControl := PageControl1;
  Tab.Caption := ACaption;
  Result := TImage.Create(Tab);
  Result.Parent := Tab;
  Result.Align := alClient;
  Result.Stretch := True;
  Result.Proportional := True;
  Result.Center := True;
  PageControl1.ActivePage := Tab;
end;


//BUTTONS **************************************************
procedure TForm1.btnSaveResClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    ETResEdit1.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    ETResEdit1.LoadFromFile(OpenDialog1.FileName);
end;

end.
