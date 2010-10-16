object Form1: TForm1
  Left = 372
  Top = 267
  ClientHeight = 439
  ClientWidth = 621
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 421
    Width = 621
    Height = 18
    Align = alBottom
    Alignment = taLeftJustify
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 0
    object ProgressBar: TProgressBar
      Left = 470
      Top = 1
      Width = 150
      Height = 16
      Align = alRight
      TabOrder = 0
      Visible = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 621
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Edit1: TEdit
      Left = 190
      Top = 4
      Width = 337
      Height = 24
      TabStop = False
      ReadOnly = True
      TabOrder = 3
    end
    object ReloadButton: TButton
      Left = 10
      Top = 4
      Width = 59
      Height = 24
      Caption = '&Reload'
      Enabled = False
      TabOrder = 0
      OnClick = ReloadButtonClick
    end
    object BackButton: TButton
      Left = 69
      Top = 4
      Width = 59
      Height = 24
      Caption = '&Back'
      Enabled = False
      TabOrder = 1
      OnClick = FwdBackClick
    end
    object FwdButton: TButton
      Left = 128
      Top = 4
      Width = 59
      Height = 24
      Caption = '&Forward'
      Enabled = False
      TabOrder = 2
      OnClick = FwdBackClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 33
    Width = 621
    Height = 388
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = 'Panel3'
    TabOrder = 2
    object Viewer: THTMLViewer
      Left = 1
      Top = 1
      Width = 619
      Height = 386
      OnHotSpotCovered = HotSpotChange
      OnHotSpotClick = HotSpotClick
      OnFormSubmit = SubmitEvent
      OnHistoryChange = HistoryChange
      OnProgress = ViewerProgress
      TabStop = True
      TabOrder = 0
      Align = alClient
      DefBackground = clWindow
      BorderStyle = htFocused
      HistoryMaxCount = 6
      DefFontName = 'Times New Roman'
      DefPreFontName = 'Courier New'
      DefFontColor = clWindowText
      DefOverLinkColor = clFuchsia
      ImageCacheCount = 6
      NoSelect = False
      CharSet = DEFAULT_CHARSET
      PrintMarginLeft = 2.000000000000000000
      PrintMarginRight = 2.000000000000000000
      PrintMarginTop = 2.000000000000000000
      PrintMarginBottom = 2.000000000000000000
      PrintScale = 1.000000000000000000
      OnMouseMove = ViewerMouseMove
      OnProcessing = ProcessingHandler
      OnPrintHTMLHeader = ViewerPrintHTMLHeader
      OnPrintHTMLFooter = ViewerPrintHTMLFooter
      OnInclude = ViewerInclude
      OnSoundRequest = SoundRequest
      OnMetaRefresh = MetaRefreshEvent
      OnObjectClick = ObjectClick
      OnRightClick = RightClick
      object MediaPlayer: TMediaPlayer
        Left = 336
        Top = 40
        Width = 253
        Height = 30
        Visible = False
        TabOrder = 3
        OnNotify = MediaPlayerNotify
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'htm'
    Filter = 'html files|*.htm;*.html|all files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Left = 329
    Top = 2
  end
  object MainMenu: TMainMenu
    Left = 291
    Top = 4
    object File1: TMenuItem
      Caption = '&File'
      object Open: TMenuItem
        Caption = '&Open'
        ShortCut = 114
        OnClick = OpenFileClick
      end
      object OpenTextFile: TMenuItem
        Caption = 'Open &Text File'
        OnClick = OpenTextFileClick
      end
      object OpenImageFile: TMenuItem
        Caption = 'Open &Image File'
        OnClick = OpenImageFileClick
      end
      object PrinterSetup1: TMenuItem
        Caption = 'Printer Setup...'
        OnClick = PrinterSetup1Click
      end
      object Printpreview: TMenuItem
        Caption = 'Print pre&view'
        Enabled = False
        OnClick = PrintpreviewClick
      end
      object Print1: TMenuItem
        Caption = '&Print...'
        Enabled = False
        OnClick = Print1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Edit2: TMenuItem
      Caption = '&Edit'
      OnClick = Edit2Click
      object Find1: TMenuItem
        Caption = '&Find'
        Enabled = False
        OnClick = Find1Click
      end
      object CopyItem: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
        OnClick = CopyItemClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object SelectAllItem: TMenuItem
        Caption = 'Select &All'
        Enabled = False
        OnClick = SelectAllItemClick
      end
    end
    object options1: TMenuItem
      Caption = '&Options'
      object ShowImages: TMenuItem
        Caption = '&Show images'
        OnClick = ShowImagesClick
      end
      object Fonts: TMenuItem
        Caption = 'Default &Font/Colors...'
        OnClick = FontColorsClick
      end
    end
    object HistoryMenuItem: TMenuItem
      Caption = '&History'
      Visible = False
    end
    object About1: TMenuItem
      Caption = '&About'
      OnClick = About1Click
    end
  end
  object PrintDialog: TPrintDialog
    FromPage = 1
    MinPage = 1
    MaxPage = 9999
    Options = [poPageNums]
    ToPage = 1
    Left = 362
    Top = 1
  end
  object FindDialog: TFindDialog
    Options = [frDown, frHideWholeWord, frDisableWholeWord]
    OnFind = FindDialogFind
    Left = 249
    Top = 65535
  end
  object PopupMenu: TPopupMenu
    Left = 400
    Top = 1
    object Viewimage: TMenuItem
      Caption = '&View image'
      OnClick = ViewimageClick
    end
    object CopyImageToClipboard: TMenuItem
      Caption = '&Copy image to clipboard'
      OnClick = CopyImageToClipboardClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object OpenInNewWindow: TMenuItem
      Caption = '&Open in new window'
      OnClick = OpenInNewWindowClick
    end
  end
  object MetaTimer: TTimer
    Enabled = False
    OnTimer = MetaTimerTimer
    Left = 242
    Top = 75
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 291
    Top = 71
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 448
  end
end
