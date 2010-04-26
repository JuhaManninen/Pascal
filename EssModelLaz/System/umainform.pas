{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

unit uMainForm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus, uMainModule, ComCtrls, uDiagramFrame, Buttons;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Printdiagram1: TMenuItem;
    Generatedocumentation1: TMenuItem;
    About1: TMenuItem;
    ExportXmiAction1: TMenuItem;
    Diagram1: TMenuItem;
    Copydiagramtoclipboard1: TMenuItem;
    Layoutdiagram1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    N3: TMenuItem;
    Changesettings1: TMenuItem;
    StatusPanel: TPanel;
    Unhidediagramelements1: TMenuItem;
    Help: TMenuItem;
    ReopenMenuItem: TMenuItem;
    LeftPanel: TPanel;
    DiagramPanel: TPanel;
    Splitter1: TSplitter;
    ZoomPanel: TPanel;
    TreePanel: TPanel;
    Splitter2: TSplitter;
    Panel1: TPanel;
    Panel2: TPanel;
    OpenButton: TSpeedButton;
    Saveaspicture1: TMenuItem;
    Previewdocumentation1: TMenuItem;
    OpenFolderAction1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Created : boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses uConst, Math;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //Free här istället för destroy, annars visas inte savechanged-dialog för diagram
  MainModule.Free;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not Created then
  begin
    Created := True;
    Caption := uConst.ProgName;
    MainModule := TMainModule.Create(Self);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Height := Max( Round(Screen.Height * 0.75), 480 );
  Width := Max( Round(Screen.Width * 0.75) , 640 );
end;

end.
