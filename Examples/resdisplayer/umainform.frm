object MainForm: TMainForm
  Left = 315
  Height = 583
  Top = 256
  Width = 1104
  Caption = 'MainForm'
  ClientHeight = 583
  ClientWidth = 1104
  LCLVersion = '8.8'
  OnCreate = FormCreate
  object MainPagecontrol: TPageControl
    Left = 0
    Height = 583
    Top = 0
    Width = 1104
    ActivePage = MainTab
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object MainTab: TTabSheet
      Caption = 'MainTab'
      ClientHeight = 555
      ClientWidth = 1096
      object TextComboBox: TComboBox
        Left = 8
        Height = 23
        Top = 24
        Width = 192
        ItemHeight = 15
        TabOrder = 1
        Text = 'TextComboBox'
        OnChange = TextComboBoxChange
      end
      object TestButton: TButton
        AnchorSideTop.Control = ComboBox1
        AnchorSideTop.Side = asrBottom
        Left = 104
        Height = 25
        Top = 238
        Width = 75
        BorderSpacing.Left = 54
        BorderSpacing.Top = 10
        Caption = 'TestButton'
        TabOrder = 2
        OnClick = TestButtonClick
      end
      object ImageComboBox: TComboBox
        Left = 8
        Height = 23
        Top = 72
        Width = 192
        ItemHeight = 15
        TabOrder = 3
        Text = 'ImageComboBox'
        OnChange = ImageComboBoxChange
      end
      object Label1: TLabel
        Left = 8
        Height = 15
        Top = 8
        Width = 34
        Caption = 'Label1'
      end
      object Label2: TLabel
        Left = 8
        Height = 15
        Top = 56
        Width = 34
        Caption = 'Label2'
      end
      object Label3: TLabel
        Left = 8
        Height = 15
        Top = 104
        Width = 34
        Caption = 'Label3'
      end
      object UncertainComboBox: TComboBox
        Left = 8
        Height = 23
        Top = 120
        Width = 192
        ItemHeight = 15
        TabOrder = 4
        Text = 'UncertainComboBox'
        OnChange = UncertainComboBoxChange
      end
      object VirtualStringTree1: TVirtualStringTree
        AnchorSideLeft.Control = eRindexList
        AnchorSideLeft.Side = asrBottom
        AnchorSideTop.Control = Button3
        AnchorSideTop.Side = asrBottom
        AnchorSideBottom.Control = Memo1
        Left = 300
        Height = 310
        Top = 38
        Width = 778
        Anchors = [akTop, akLeft, akRight, akBottom]
        BorderSpacing.Left = 10
        BorderSpacing.Top = 5
        BorderSpacing.Bottom = 20
        Colors.FocusedSelectionColor = 16302604
        Constraints.MinHeight = 237
        DefaultText = 'Node'
        DragMode = dmAutomatic
        Header.AutoSizeIndex = 0
        Header.Columns = <>
        Header.MainColumn = -1
        Header.Options = [hoAutoResize, hoDblClickResize, hoShowSortGlyphs, hoVisible]
        PopupMenu = PopupMenu1
        TabOrder = 5
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.SelectionOptions = [toFullRowSelect]
        OnCompareNodes = VirtualStringTree1CompareNodes
        OnDblClick = VirtualStringTree1DblClick
        OnDragAllowed = VirtualStringTree1DragAllowed
        OnDragOver = VirtualStringTree1DragOver
        OnDragDrop = VirtualStringTree1DragDrop
        OnEdited = VirtualStringTree1Edited
        OnEditing = VirtualStringTree1Editing
        OnFreeNode = VirtualStringTree1FreeNode
        OnGetText = VirtualStringTree1GetText
        OnNewText = VirtualStringTree1NewText
      end
      object Button1: TButton
        AnchorSideTop.Control = TestButton
        AnchorSideTop.Side = asrBottom
        Left = 8
        Height = 25
        Top = 283
        Width = 75
        BorderSpacing.Top = 20
        Caption = 'Load Res'
        TabOrder = 6
        OnClick = Button1Click
      end
      object eRName: TEdit
        AnchorSideTop.Control = Label5
        AnchorSideTop.Side = asrBottom
        Left = 8
        Height = 23
        Top = 177
        Width = 92
        BorderSpacing.Top = 10
        TabOrder = 7
      end
      object eRType: TEdit
        AnchorSideLeft.Control = eRName
        AnchorSideLeft.Side = asrBottom
        AnchorSideTop.Control = Label5
        AnchorSideTop.Side = asrBottom
        Left = 110
        Height = 23
        Top = 177
        Width = 87
        BorderSpacing.Left = 10
        BorderSpacing.Top = 10
        TabOrder = 8
      end
      object Label4: TLabel
        AnchorSideLeft.Control = Button1
        AnchorSideTop.Control = Button1
        AnchorSideTop.Side = asrBottom
        Left = 8
        Height = 15
        Top = 318
        Width = 34
        BorderSpacing.Top = 10
        Caption = 'Label4'
      end
      object ComboBox1: TComboBox
        AnchorSideTop.Control = eRType
        AnchorSideTop.Side = asrBottom
        Left = 8
        Height = 23
        Top = 205
        Width = 92
        BorderSpacing.Top = 5
        ItemHeight = 15
        Items.Strings = (
          ' mrmAll'
          ' mrmBest'
          ' mrmFirst'
          ' mrmLast'
          ' mrmLargest'
          ' mrmSmallest'
          ' mrmSelection'
          ' mrmByType'
          ' mrmByMime'
          ' mrmText'
          ' mrmImage'
          ' mrmAudio'
          ' mrmVideo'
          ' mrmArchive'
          ' mrmExecutable'
          ' mrmRejoined'
        )
        TabOrder = 10
        Text = '  mrmAll'
      end
      object Splitter1: TSplitter
        AnchorSideBottom.Side = asrBottom
        Cursor = crVSplit
        Left = 0
        Height = 5
        Top = 363
        Width = 1096
        Align = alBottom
        MinSize = 5
        ResizeAnchor = akBottom
      end
      object eRindexList: TEdit
        AnchorSideLeft.Control = eRType
        AnchorSideLeft.Side = asrBottom
        AnchorSideTop.Control = Label5
        AnchorSideTop.Side = asrBottom
        Left = 207
        Height = 23
        Top = 177
        Width = 83
        BorderSpacing.Left = 10
        BorderSpacing.Top = 10
        TabOrder = 11
      end
      object Button2: TButton
        AnchorSideTop.Control = TestButton
        AnchorSideTop.Side = asrBottom
        Left = 200
        Height = 25
        Top = 283
        Width = 75
        BorderSpacing.Top = 20
        Caption = 'Save Res'
        TabOrder = 12
        OnClick = Button2Click
      end
      object ComboBox2: TComboBox
        AnchorSideTop.Control = eRType
        AnchorSideTop.Side = asrBottom
        Left = 110
        Height = 23
        Top = 205
        Width = 87
        BorderSpacing.Top = 5
        ItemHeight = 15
        Items.Strings = (
          'TEXT'
          'IMAGE'
          'AUDIO'
          'VIDEO'
          'HTML'
          'CSS'
          'JS'
          'XML'
          'JSON'
          'PNG'
          'JPG'
          'GIF'
          'BMP'
          'ICO'
          'WAVE'
          'MP3'
          'AVI'
          'CHUNK'
          'ETEMPLATE'
        )
        TabOrder = 13
      end
      object ComboBox3: TComboBox
        AnchorSideLeft.Control = ComboBox2
        AnchorSideLeft.Side = asrBottom
        AnchorSideTop.Control = eRindexList
        AnchorSideTop.Side = asrBottom
        Left = 207
        Height = 23
        Top = 205
        Width = 83
        BorderSpacing.Left = 10
        BorderSpacing.Top = 5
        ItemHeight = 15
        ItemIndex = 0
        Items.Strings = (
          'Neutral'
          'English'
          'Spanish'
          'French'
          'Italian'
          'Japanese'
          'Korean'
          'Russian'
          'Chinese'
        )
        TabOrder = 14
        Text = 'Neutral'
      end
      object Button3: TButton
        Left = 1000
        Height = 25
        Top = 8
        Width = 75
        Caption = 'Refresh tree'
        TabOrder = 15
        OnClick = Button3Click
      end
      object Label5: TLabel
        Left = 115
        Height = 15
        Top = 152
        Width = 85
        Caption = 'Filtered Loading'
      end
      object Memo1: TMemo
        AnchorSideTop.Control = Splitter1
        AnchorSideTop.Side = asrCenter
        Left = 0
        Height = 187
        Top = 368
        Width = 1096
        Align = alBottom
        Font.CharSet = ANSI_CHARSET
        Font.Height = -19
        Font.Name = 'AcPlus IBM VGA 9x16'
        Font.Pitch = fpFixed
        Font.Quality = fqDraft
        Lines.Strings = (
          'Memo1'
        )
        ParentFont = False
        ScrollBars = ssAutoBoth
        TabOrder = 0
      end
    end
    object ImgTab: TTabSheet
      Caption = 'ImgTab'
      ClientHeight = 555
      ClientWidth = 1096
      object ImgControl: TPageControl
        Left = 0
        Height = 555
        Top = 0
        Width = 1096
        Align = alClient
        TabOrder = 0
      end
    end
    object TxtTab: TTabSheet
      Caption = 'TxtTab'
      ClientHeight = 441
      ClientWidth = 927
      object ControlTxt: TPageControl
        Left = 0
        Height = 441
        Top = 0
        Width = 927
        Align = alClient
        TabOrder = 0
      end
    end
    object HexTab: TTabSheet
      Caption = 'HexTab'
      ClientHeight = 441
      ClientWidth = 927
      object HexControl: TPageControl
        Left = 0
        Height = 441
        Top = 0
        Width = 927
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 1048
    Top = 416
    object MenuItem1: TMenuItem
      Caption = 'Export resource'
      OnClick = MenuItem1Click
    end
    object MenuItem2: TMenuItem
      Caption = 'Export As'
      OnClick = MenuItem2Click
    end
    object Separator3: TMenuItem
      Caption = '-'
    end
    object MenuItemChangeType: TMenuItem
      Caption = 'Change Type'
      OnClick = MenuItemChangeTypeClick
    end
    object Separator2: TMenuItem
      Caption = '-'
    end
    object MenuItemAddResource: TMenuItem
      Caption = 'Add Resource'
      OnClick = MenuItemAddResourceClick
    end
    object Separator1: TMenuItem
      Caption = '-'
    end
    object MenuItemDelete: TMenuItem
      Caption = 'Delete Item'
      OnClick = MenuItemDeleteClick
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 1048
    Top = 488
  end
end
