object MainForm: TMainForm
  Left = 306
  Top = 122
  Width = 648
  Height = 526
  Caption = 'x'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 28
    Width = 640
    Height = 452
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 201
      Top = 1
      Width = 8
      Height = 430
      Cursor = crHSplit
      Beveled = True
    end
    object StatusPanel: TPanel
      Left = 1
      Top = 431
      Width = 638
      Height = 20
      Align = alBottom
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object DiagramPanel: TPanel
      Left = 209
      Top = 1
      Width = 430
      Height = 430
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
    object LeftPanel: TPanel
      Left = 1
      Top = 1
      Width = 200
      Height = 430
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object Splitter2: TSplitter
        Left = 0
        Top = 121
        Width = 200
        Height = 8
        Cursor = crVSplit
        Align = alTop
        Beveled = True
      end
      object ZoomPanel: TPanel
        Left = 0
        Top = 0
        Width = 200
        Height = 121
        Align = alTop
        BevelOuter = bvLowered
        TabOrder = 0
      end
      object TreePanel: TPanel
        Left = 0
        Top = 129
        Width = 200
        Height = 301
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 640
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object OpenButton: TSpeedButton
      Left = 1
      Top = 3
      Width = 23
      Height = 22
      Action = MainModule.FileOpenAction
      Flat = True
      Glyph.Data = {
        42030000424D42030000000000003600000028000000110000000F0000000100
        1800000000000C03000000000000000000000000000000000000CED3D6CED3D6
        CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3
        D6CED3D6CED3D6CED3D6CED3D600CED3D6000000000000000000000000000000
        000000000000000000000000000000000000CED3D6CED3D6CED3D6CED3D6CED3
        D600CED3D6000000000000008684008684008684008684008684008684008684
        008684008684000000CED3D6CED3D6CED3D6CED3D600CED3D600000000FFFF00
        0000008684008684008684008684008684008684008684008684008684000000
        CED3D6CED3D6CED3D600CED3D6000000FFFFFF00FFFF00000000868400868400
        8684008684008684008684008684008684008684000000CED3D6CED3D600CED3
        D600000000FFFFFFFFFF00FFFF00000000868400868400868400868400868400
        8684008684008684008684000000CED3D600CED3D6000000FFFFFF00FFFFFFFF
        FF00FFFF00000000000000000000000000000000000000000000000000000000
        0000CED3D600CED3D600000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FF
        FFFFFFFF00FFFF000000CED3D6CED3D6CED3D6CED3D6CED3D600CED3D6000000
        FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF000000CED3
        D6CED3D6CED3D6CED3D6CED3D600CED3D600000000FFFFFFFFFF00FFFF000000
        000000000000000000000000000000000000CED3D6CED3D6CED3D6CED3D6CED3
        D600CED3D6CED3D6000000000000000000CED3D6CED3D6CED3D6CED3D6CED3D6
        CED3D6CED3D6CED3D6000000000000000000CED3D600CED3D6CED3D6CED3D6CE
        D3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6
        000000000000CED3D600CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CE
        D3D6CED3D6000000CED3D6CED3D6CED3D6000000CED3D6000000CED3D600CED3
        D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D600000000
        0000000000CED3D6CED3D6CED3D6CED3D600CED3D6CED3D6CED3D6CED3D6CED3
        D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CED3D6CE
        D3D6CED3D600}
    end
  end
  object MainMenu1: TMainMenu
    Left = 46
    Top = 55
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Action = MainModule.FileOpenAction
      end
      object OpenFolderAction1: TMenuItem
        Action = MainModule.OpenFolderAction
      end
      object ReopenMenuItem: TMenuItem
        Caption = '&Reopen'
        object TMenuItem
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Printdiagram1: TMenuItem
        Action = MainModule.PrintDiagramAction
      end
      object Generatedocumentation1: TMenuItem
        Action = MainModule.DocGenAction
      end
      object Previewdocumentation1: TMenuItem
        Action = MainModule.DocGenPreviewAction
      end
      object ExportXmiAction1: TMenuItem
        Action = MainModule.ExportXmiAction
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Changesettings1: TMenuItem
        Action = MainModule.SettingsAction
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = MainModule.ExitAction
      end
    end
    object Diagram1: TMenuItem
      Caption = 'Diagram'
      object Copydiagramtoclipboard1: TMenuItem
        Action = MainModule.CopyDiagramClipboardAction
      end
      object Layoutdiagram1: TMenuItem
        Action = MainModule.LayoutDiagramAction
      end
      object Unhidediagramelements1: TMenuItem
        Action = MainModule.UnhideElementsAction
      end
      object Saveaspicture1: TMenuItem
        Action = MainModule.SaveDiagramAction
      end
    end
    object Help: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Action = MainModule.AboutAction
      end
    end
  end
end
