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

unit uTreeViewFrame;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls;

type
  TTreeViewFrame = class(TFrame)
    tvModel: TTreeView;
    ilTreeImages: TImageList;
    procedure tvModelDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation
uses uTreeViewIntegrator;
{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TTreeViewFrame.tvModelDblClick(Sender: TObject);
var
  clickedNode: TTreeNode;
  locatedNode: TViewNode;
begin
  clickedNode := (Sender as TTreeView).Selected;
  if (clickedNode is TViewNode) and  not((clickedNode as TViewNode).IsImplementation) then
  begin
    // Jump to a matching node with IsImplementation = True
    locatedNode := (clickedNode as TViewNode).LocateNode( (clickedNode as TViewNode).Data,True );
    if Assigned(locatedNode) then
      locatedNode.Selected := True;
  end;
end;


end.