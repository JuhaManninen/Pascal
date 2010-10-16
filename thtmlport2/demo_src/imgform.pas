unit ImgForm;

{$mode objfpc}{$H+}

interface

uses
  LclIntf, LMessages, LclType, LResources, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls;

type
  TImageForm = class(TForm)
    Image1: TImage;
    procedure FormShow(Sender: TObject);
  private

  public
    ImageFormBitmap: TBitmap;
  end;

var
  ImageForm: TImageForm;

implementation

{$IFNDEF LCL}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TImageForm.FormShow(Sender: TObject);
begin
  Image1.Picture.Bitmap := ImageFormBitmap;
  Width := Image1.Width + 30;  {makes for better fit}
  ClientHeight := Image1.Height;
  ClientWidth := Image1.Width;
end;


end.
