object SubmitForm: TSubmitForm
  Left = 200
  Top = 99
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Submit Results'
  ClientHeight = 273
  ClientWidth = 427
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 19
    Width = 43
    Height = 16
    Caption = 'Action:'
  end
  object Label2: TLabel
    Left = 9
    Top = 43
    Width = 52
    Height = 16
    Caption = 'Method:'
  end
  object Label3: TLabel
    Left = 18
    Top = 71
    Width = 50
    Height = 16
    Caption = 'Results'
  end
  object ActionText: TEdit
    Left = 67
    Top = 14
    Width = 342
    Height = 24
    TabStop = False
    ReadOnly = True
    TabOrder = 0
  end
  object MethodText: TEdit
    Left = 68
    Top = 41
    Width = 342
    Height = 24
    TabStop = False
    ReadOnly = True
    TabOrder = 1
  end
  object ResultBox: TListBox
    Left = 17
    Top = 90
    Width = 392
    Height = 145
    TabStop = False
    ItemHeight = 16
    TabOrder = 2
  end
  object Button1: TButton
    Left = 172
    Top = 242
    Width = 73
    Height = 28
    Caption = '&Close'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
end
