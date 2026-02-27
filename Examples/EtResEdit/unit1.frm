object Form1: TForm1
  Left = 315
  Height = 381
  Top = 256
  Width = 708
  Caption = 'Form1'
  ClientHeight = 381
  ClientWidth = 708
  LCLVersion = '8.8'
  OnDestroy = FormDestroy
  object Panel1: TPanel
    AnchorSideRight.Control = Splitter1
    AnchorSideRight.Side = asrBottom
    Left = 0
    Height = 381
    Top = 0
    Width = 230
    Align = alLeft
    Anchors = [akTop, akLeft, akRight, akBottom]
    Caption = 'Panel1'
    ClientHeight = 381
    ClientWidth = 230
    TabOrder = 0
    object ETResEdit1: TETResEdit
      AnchorSideTop.Control = btnLoad
      AnchorSideTop.Side = asrBottom
      Left = 1
      Height = 349
      Top = 31
      Width = 228
      Align = alCustom
      Anchors = [akTop, akLeft, akRight, akBottom]
      sFileName = 'test.res'
      OnResourceSelected = ETResEdit1ResourceSelected
    end
    object btnLoad: TButton
      AnchorSideTop.Control = Panel1
      Left = 24
      Height = 25
      Top = 6
      Width = 75
      BorderSpacing.Top = 5
      Caption = 'Load Res'
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object btnSaveRes: TButton
      Left = 112
      Height = 25
      Top = 6
      Width = 75
      Caption = 'Save Res'
      TabOrder = 2
      OnClick = btnSaveResClick
    end
  end
  object Panel2: TPanel
    AnchorSideLeft.Control = Splitter1
    AnchorSideLeft.Side = asrBottom
    Left = 230
    Height = 381
    Top = 0
    Width = 478
    Align = alClient
    Caption = 'Panel2'
    ClientHeight = 381
    ClientWidth = 478
    TabOrder = 2
    object PageControl1: TPageControl
      AnchorSideBottom.Control = Splitter2
      AnchorSideBottom.Side = asrBottom
      Left = 1
      Height = 254
      Top = 1
      Width = 476
      Align = alCustom
      Anchors = [akTop, akLeft, akRight, akBottom]
      TabOrder = 0
    end
    object Panel3: TPanel
      AnchorSideTop.Control = Splitter2
      AnchorSideTop.Side = asrBottom
      AnchorSideBottom.Control = Panel2
      AnchorSideBottom.Side = asrBottom
      Left = 0
      Height = 125
      Top = 255
      Width = 474
      Align = alCustom
      Anchors = [akTop, akLeft, akBottom]
      Caption = 'Panel3'
      ClientHeight = 125
      ClientWidth = 474
      TabOrder = 1
      object Memo1: TMemo
        Left = 1
        Height = 123
        Top = 1
        Width = 472
        Align = alClient
        Anchors = [akTop, akLeft, akBottom]
        Lines.Strings = (
          'Memo1'
        )
        ScrollBars = ssAutoBoth
        TabOrder = 0
      end
    end
    object Splitter2: TSplitter
      Cursor = crVSplit
      Left = 0
      Height = 5
      Top = 250
      Width = 468
      Align = alNone
      ResizeAnchor = akBottom
    end
  end
  object Splitter1: TSplitter
    Left = 225
    Height = 299
    Top = 91
    Width = 5
    Align = alNone
  end
  object OpenDialog1: TOpenDialog
    Left = 48
    Top = 126
  end
  object SaveDialog1: TSaveDialog
    Left = 130
    Top = 126
  end
end
