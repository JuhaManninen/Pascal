object MainModule: TMainModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 219
  Top = 203
  Height = 540
  Width = 783
  object ActionList: TActionList
    Left = 81
    Top = 128
    object CopyDiagramClipboardAction: TAction
      Caption = 'Copy diagram to clipboard'
      Enabled = False
      ShortCut = 16451
      OnExecute = CopyDiagramClipboardActionExecute
    end
    object PrintDiagramAction: TAction
      Caption = 'Print diagram'
      Visible = False
    end
    object DocGenAction: TAction
      Caption = 'Generate documentation'
      Enabled = False
      OnExecute = DocGenActionExecute
    end
    object AboutAction: TAction
      Caption = 'About...'
      OnExecute = AboutActionExecute
    end
    object ExportXmiAction: TAction
      Caption = 'Export model to XMI-file'
      Enabled = False
      OnExecute = ExportXmiActionExecute
    end
    object LayoutDiagramAction: TAction
      Caption = 'Layout diagram'
      Enabled = False
      Hint = 'Layout'
      OnExecute = LayoutDiagramActionExecute
    end
    object FileOpenAction: TAction
      Caption = 'Open...'
      Hint = 'Open...'
      OnExecute = FileOpenActionExecute
    end
    object ExitAction: TAction
      Caption = 'Exit'
      OnExecute = ExitActionExecute
    end
    object SettingsAction: TAction
      Caption = 'Change settings...'
      OnExecute = SettingsActionExecute
    end
    object UnhideElementsAction: TAction
      Caption = 'Show hidden diagram elements'
      OnExecute = UnhideElementsActionExecute
      OnUpdate = UnhideElementsActionUpdate
    end
    object SaveDiagramAction: TAction
      Caption = 'Save as picture...'
      Enabled = False
      OnExecute = SaveDiagramActionExecute
    end
    object DocGenPreviewAction: TAction
      Caption = 'Preview documentation'
      Enabled = False
      OnExecute = DocGenPreviewActionExecute
    end
    object OpenFolderAction: TAction
      Caption = 'Open folder...'
      OnExecute = OpenFolderActionExecute
    end
  end
  object CloseTimer: TTimer
    Enabled = False
    Interval = 200
    OnTimer = CloseTimerTimer
    Left = 72
    Top = 40
  end
end
