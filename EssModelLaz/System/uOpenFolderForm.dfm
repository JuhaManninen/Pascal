object OpenFolderForm: TOpenFolderForm
  Left = 174
  Top = 107
  Width = 398
  Height = 340
  Caption = 'Open folder'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    390
    313)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 6
    Top = 262
    Width = 39
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'File type'
  end
  object OkButton: TButton
    Left = 227
    Top = 281
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 309
    Top = 281
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object FileTypeCombo: TComboBox
    Left = 52
    Top = 258
    Width = 69
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    TabOrder = 2
    TabStop = False
  end
  object PathTreeView: TShellTreeView
    Left = 8
    Top = 8
    Width = 372
    Height = 240
    ObjectTypes = [otFolders]
    Root = 'rfDesktop'
    UseShellImages = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoRefresh = False
    HideSelection = False
    Indent = 19
    ParentColor = False
    RightClickSelect = True
    ShowRoot = False
    TabOrder = 3
  end
end
