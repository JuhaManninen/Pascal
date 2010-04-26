unit uOpenFolderForm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ShellCtrls;

type
  TOpenFolderForm = class(TForm)
    Label2: TLabel;
    OkButton: TButton;
    Button2: TButton;
    FileTypeCombo: TComboBox;
    PathTreeView: TShellTreeView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

end.