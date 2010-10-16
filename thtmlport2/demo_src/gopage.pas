unit Gopage;

{$mode objfpc}{$H+}

interface

uses 
  LclIntf, LResources, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls, Spin;

type
  TGoPageForm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Bevel1: TBevel;
    PageNum: TSpinEdit;
    procedure PageNumEnter(Sender: TObject);
    procedure PageNumKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private

  public

  end;

var
  GoPageForm: TGoPageForm;

implementation

{$IFNDEF LCL}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TGoPageForm.PageNumEnter(Sender: TObject);
begin
  PageNum.SelectAll;
end;

procedure TGoPageForm.PageNumKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
  Begin
    Key := 0;
    OKBtn.Click;
  end;
end;


end.
