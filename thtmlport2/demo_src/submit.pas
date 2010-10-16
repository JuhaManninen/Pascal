unit Submit;

{$mode objfpc}{$H+}

interface

uses
  LclIntf, LMessages, LclType, LResources, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TSubmitForm = class(TForm)
    ActionText: TEdit;
    MethodText: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ResultBox: TListBox;
    Label3: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  SubmitForm: TSubmitForm;

implementation

{$IFNDEF LCL}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TSubmitForm.Button1Click(Sender: TObject);
begin
  Close;
end;


end.
