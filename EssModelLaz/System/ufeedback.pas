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

unit uFeedback;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses classes, extctrls;

type
  TEldeanFeedback = class abstract
    procedure Message(const M : string); virtual; abstract;
  end;

  TGuiFeedback = class(TEldeanFeedback)
  private
    P : TPanel;
    T : TTimer;
  private
    procedure OnTimer(Sender : TObject);
  public
    constructor Create(P : TPanel);
    destructor Destroy; override;
    procedure Message(const M : string); override;
  end;

var
  NilFeedback : TEldeanFeedback;


implementation

uses SysUtils;

{ TGuiFeedback }

constructor TGuiFeedback.Create(P: TPanel);
begin
  Self.P := P;
  T := TTimer.Create(nil);
  T.OnTimer := OnTimer;
end;

destructor TGuiFeedback.Destroy;
begin
  FreeAndNil(T);
  inherited;
end;

procedure TGuiFeedback.Message(const M: string);
begin
  P.Caption := M;
  P.Visible := True;
  P.Height := 20;
  P.Refresh;
  T.Enabled := False;
  T.Interval := 5000;
  T.Enabled := True;
end;

procedure TGuiFeedback.OnTimer(Sender: TObject);
begin
//  P.Caption := '';
//  P.Refresh;
  if P.Height>=0 then
  begin
    P.Height := P.Height - 5;
    T.Interval := 25;
  end
  else
  begin
    P.Visible := False;
    T.Enabled := False;
  end;
end;


{ TNilFeedback }

type
  TNilFeedback = class(TEldeanFeedback)
    procedure Message(const M : string); override;
  end;

procedure TNilFeedback.Message(const M: string);
begin
//
end;


initialization
  NilFeedBack := TNilFeedback.Create;
finalization
  NilFeedBack.Free;
end.