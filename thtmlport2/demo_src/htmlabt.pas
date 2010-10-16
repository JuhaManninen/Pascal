unit HTMLAbt;

{$mode objfpc}{$H+}

interface

uses
  LclIntf, LMessages, LclType, LResources, LCLVersion,
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Htmlview, ExtCtrls;

const
  Version = '9.45';

type
  TAboutBox = class(TForm)
    BitBtn1: TBitBtn;
    Panel1: TPanel;
    Viewer: THTMLViewer;
  private

  public
    constructor CreateIt(AOwner: TComponent; const ProgName, CompName: string);
  end;

var
  AboutBox: TAboutBox;

implementation

{$IFNDEF LCL}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

constructor TAboutBox.CreateIt(AOwner: TComponent; const ProgName, CompName: string);
var
  S: string[210];
begin
  inherited Create(AOwner);
  Viewer.DefFontName := 'MS Sans Serif';
  Viewer.DefFontSize := 9;
  Viewer.DefFontColor := clNavy;
  S :='<body bgcolor="ffffeb" text="000080">'+
      '<center>'+
      '<h1>'+ProgName+'</h1>'+
      '<font color="Maroon">A demo program for the '+CompName+' component</font>'+
      '<h3>Version ' + Version + ' compiled with Lazarus ' + lcl_version + '</h3>' +
      '</center>'+
      '</body>';
  Viewer.LoadFromBuffer(@S[1], Length(S), '');
end;


end.
