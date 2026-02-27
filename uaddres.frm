object frmAddResource: TfrmAddResource
  Left = 86
  Height = 316
  Top = 85
  Width = 272
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'frmAddResource'
  ClientHeight = 316
  ClientWidth = 272
  LCLVersion = '8.8'
  OnCreate = FormCreate
  object lblName: TLabel
    Left = 128
    Height = 15
    Top = 72
    Width = 38
    Caption = 'Name: '
  end
  object lblType: TLabel
    Left = 128
    Height = 15
    Top = 128
    Width = 37
    Caption = 'lblType'
  end
  object lblLanguage: TLabel
    Left = 112
    Height = 15
    Top = 176
    Width = 65
    Caption = 'lblLanguage'
  end
  object btnLoad: TButton
    Left = 102
    Height = 25
    Top = 16
    Width = 75
    Caption = 'Load file'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object cmbType: TComboBox
    Left = 24
    Height = 23
    Top = 152
    Width = 224
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
    TabOrder = 1
    Text = 'cmbType'
  end
  object cmbLanguage: TComboBox
    Left = 24
    Height = 23
    Top = 208
    Width = 224
    ItemHeight = 15
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
    TabOrder = 2
    Text = 'cmbLanguage'
  end
  object edtName: TEdit
    Left = 24
    Height = 23
    Top = 96
    Width = 224
    TabOrder = 3
    Text = 'edtName'
  end
  object btnOk: TButton
    Left = 32
    Height = 25
    Top = 272
    Width = 75
    Caption = 'Accept'
    ModalResult = 1
    TabOrder = 4
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 168
    Height = 25
    Top = 272
    Width = 75
    Caption = 'btnCancel'
    ModalResult = 2
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object OpenDialog1: TOpenDialog
    Left = 216
    Top = 189
  end
end
