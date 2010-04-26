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

unit uSettingsForm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  contnrs, StdCtrls;

type
  TSettingsForm = class(TForm)
    OkButton: TButton;
    ShellCheck: TCheckBox;
    DelphiIDECheck: TCheckBox;
    DiSaveCombo: TComboBox;
    Label1: TLabel;
    Button2: TButton;
    ShowAssocCheck: TCheckBox;
    VisibilityCombo: TComboBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure DelphiIDECheckClick(Sender: TObject);
    procedure ShellCheckClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    { Private declarations }
    ShellChanged,IDEChanged : boolean;
    procedure ReadSettings;
    procedure SaveSettings;
  public
    { Public declarations }
  end;

implementation

uses uRegisterExtension, uConst, uIntegrator, uConfig;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

const
  MenuStr = 'View as model diagram';

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  ReadSettings;
  IDEChanged := False;
  ShellChanged := False;
end;

procedure TSettingsForm.DelphiIDECheckClick(Sender: TObject);
begin
  IDEChanged := True;
end;


procedure TSettingsForm.ShellCheckClick(Sender: TObject);
begin
  ShellChanged := True;
end;

procedure TSettingsForm.OkButtonClick(Sender: TObject);
begin
  SaveSettings;
  Close;
end;

procedure TSettingsForm.ReadSettings;
begin
  ShellCheck.Checked := ShellIsExtensionRegistered('.pas', MenuStr);
  DelphiIDECheck.Checked := DelphiIsToolRegistered(MenuStr);
  DiSaveCombo.ItemIndex := integer(Config.DiSave);
  ShowAssocCheck.Checked := Config.DiShowAssoc;
  VisibilityCombo.ItemIndex := Config.DiVisibilityFilter;
end;

procedure TSettingsForm.SaveSettings;
var
  Ints : TClassList;
  I,J : integer;
  Exts : TStringList;
begin
  //Delphi IDE
  if IDEChanged then
  begin
    if DelphiIDECheck.Checked then
    begin
      if not DelphiIsToolRegistered(MenuStr) then
        DelphiRegisterTool(MenuStr,Application.ExeName,'','$EDNAME')
    end
    else
    begin
      if DelphiIsToolRegistered(MenuStr) then
        DelphiUnRegisterTool(MenuStr)
    end;
  end;

  //Shell
  if ShellChanged then
  begin
    Ints := Integrators.Get(TImportIntegrator);
    try
      for I := 0 to Ints.Count - 1 do
      begin
        Exts := TImportIntegratorClass(Ints[I]).GetFileExtensions;
        try
          for J := 0 to Exts.Count - 1 do
            if ShellCheck.Checked then
              ShellRegisterExtension( Exts.Names[J] , MenuStr ,'"' + Application.ExeName + '" "%L"')
            else
              ShellUnRegisterExtension( Exts.Names[J] , MenuStr);
        finally
          Exts.Free;
        end;
      end;
    finally
      Ints.Free;
    end;
  end;

  Config.DiSave := TDiSaveSetting(DiSaveCombo.ItemIndex);
  Config.DiShowAssoc := ShowAssocCheck.Checked;
  Config.DiVisibilityFilter := VisibilityCombo.ItemIndex;

  Config.StoreSettings;
end;

end.