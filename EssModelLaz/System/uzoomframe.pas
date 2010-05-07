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

unit uZoomFrame;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  uViewIntegrator, ExtCtrls;

type
  TZoomFrame = class(TFrame)
    ZoomImage: TImage;
    procedure ZoomImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FrameMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
  private
    { Private declarations }
    Diagram : TDiagramIntegrator;
    procedure OnUpdateZoom(Sender : TObject);
    procedure SyncScroll(X,Y : integer);
    procedure RedrawZoom;
  public
    constructor Create(AOwner: TComponent; Diagram : TDiagramIntegrator); reintroduce;
    { Public declarations }
  end;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TZoomFrame }

constructor TZoomFrame.Create(AOwner: TComponent; Diagram: TDiagramIntegrator);
begin
  inherited Create(AOwner);
  Self.Diagram := Diagram;
  Parent := AOwner as TWinControl;
  Diagram.OnUpdateZoom := OnUpdateZoom;
end;

procedure TZoomFrame.OnUpdateZoom(Sender: TObject);
begin
  RedrawZoom;
end;

procedure TZoomFrame.SyncScroll(X, Y: integer);
begin
  Diagram.SetZoomedScroll(X,Y,ZoomImage.Width,ZoomImage.Height);
  RedrawZoom;;
end;

procedure TZoomFrame.ZoomImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SyncScroll(X,Y);
  MouseCapture := True;
end;

procedure TZoomFrame.FrameMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseCapture := False;
end;

procedure TZoomFrame.FrameMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if MouseCapture then
    SyncScroll(X,Y);
end;

procedure TZoomFrame.FrameResize(Sender: TObject);
begin
  //Force image.canvas to recreate itself with new size
  ZoomImage.Picture.Graphic := nil;
  RedrawZoom;
end;

procedure TZoomFrame.RedrawZoom;
begin
  Diagram.DrawZoom(ZoomImage.Canvas,ZoomImage.Width,ZoomImage.Height);
end;

end.