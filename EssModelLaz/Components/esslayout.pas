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

unit essLayout;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses essConnectPanel;

type
  TEssLayout = class
  protected
    Panel : TEssConnectPanel;
  public
    constructor Create(Panel : TEssConnectPanel);
    procedure Execute; virtual; abstract;
    class function CreateLayout(Panel : TEssConnectPanel) : TEssLayout;
  end;

implementation
uses Controls,
  ExtCtrls,
  SugiyamaLayout,
  Classes;

type
  TSimpleLayout = class(TEssLayout)
    procedure Execute; override;
  end;

{ TEssLayout }

constructor TEssLayout.Create(Panel: TEssConnectPanel);
begin
  Self.Panel := Panel;
end;

class function TEssLayout.CreateLayout(Panel: TEssConnectPanel): TEssLayout;
begin
//  Result := TSimpleLayout.Create(Panel);
  Result := TSugiyamaLayout.Create(Panel);
end;

{ TSimpleLayout }

procedure TSimpleLayout.Execute;
var
  I,NextX,NextY : integer;
  C : TControl;
  L : TList;
begin
  NextX := 50;
  NextY := 50;
  L := Panel.GetManagedObjects;
  try
    for I := 0 to L.Count-1 do
    begin
      C := TControl(L[I]);
      if C is TCustomPanel then
      begin
        C.BoundsRect := Rect(NextX, NextY, NextX + C.Width, NextY + C.Height);
        Inc(NextX, C.Width + 20);
        if NextX > 2000 then
        begin
          NextX := 50;
          Inc(NextY, 200);
        end;
      end;
    end;
  finally
    L.Free;
  end;
end;


end.