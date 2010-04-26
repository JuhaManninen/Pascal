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

unit uRtfdComponents;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses
  LCLIntf, LCLType, LMessages, Messages, ExtCtrls, Classes, StdCtrls, Controls,
  Contnrs, Forms,
  uListeners, uModel, uModelEntity, uViewIntegrator, uDiagramFrame;

type

  //Baseclass for a diagram-panel
  TRtfdBoxClass = class of TRtfdBox;
  TRtfdBox = class(TPanel) // , IModelEntityListener
  private
    FMinVisibility : TVisibility;
    procedure SetMinVisibility(const Value: TVisibility);
    procedure OnChildMouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: Classes.TOperation); override;
  public
    Frame: TDiagramFrame;
    Entity: TModelEntity;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity; Frame: TDiagramFrame; MinVisibility : TVisibility); reintroduce; virtual;
    procedure RefreshEntities; virtual; abstract;
    procedure Paint; override;
    // Listener methods.
    procedure BeforeChange(Sender: TModelEntity); virtual;
    procedure AfterChange(Sender: TModelEntity); virtual;
    procedure BeforeAddChild(Sender: TModelEntity; NewChild: TModelEntity); virtual;
    procedure AfterAddChild(Sender: TModelEntity; NewChild: TModelEntity); virtual;
    procedure BeforeRemove(Sender: TModelEntity); virtual;
    procedure AfterRemove(Sender: TModelEntity); virtual;
    procedure BeforeEntityChange(Sender: TModelEntity); virtual;
    procedure AfterEntityChange(Sender: TModelEntity); virtual;
  public
    property MinVisibility : TVisibility write SetMinVisibility;
  end;

  TRtfdClass = class(TRtfdBox) // , IAfterClassListener
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity; Frame: TDiagramFrame; MinVisibility : TVisibility); override;
    destructor Destroy; override;
    procedure RefreshEntities; override;
    procedure AfterAddChild(Sender: TModelEntity; NewChild: TModelEntity); override;
  end;

  TRtfdInterface = class(TRtfdBox) // , IAfterInterfaceListener
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity; Frame: TDiagramFrame; MinVisibility : TVisibility); override;
    destructor Destroy; override;
    procedure RefreshEntities; override;
    procedure AfterAddChild(Sender: TModelEntity; NewChild: TModelEntity); override;
  end;

  TRtfdUnitPackage = class(TRtfdBox)
  public
    P: TUnitPackage;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity; Frame: TDiagramFrame; MinVisibility : TVisibility); override;
    procedure RefreshEntities; override;
    procedure DblClick; override;
  end;

//  TRtfdCustomLabel = class(TCustomLabel, IModelEntityListener)
  TRtfdCustomLabel = class(TGraphicControl) // , IModelEntityListener
  private
    FCaption: TCaption;
    FAlignment: TAlignment;
    FTransparent: Boolean;
    Entity: TModelEntity;
    function GetAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetTransparent(const Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure AdjustBounds;
    procedure DoDrawText(var Rect: TRect; Flags: Integer);
  protected
    procedure Paint; override;
    procedure SetText(const Value: TCaption);
    function GetText: TCaption;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); reintroduce; virtual;
    function WidthNeeded : integer; virtual;
    // Listener methods.
    procedure BeforeChange(Sender: TModelEntity); virtual;
    procedure AfterChange(Sender: TModelEntity); virtual;
    procedure BeforeAddChild(Sender: TModelEntity; NewChild: TModelEntity); virtual;
    procedure AfterAddChild(Sender: TModelEntity; NewChild: TModelEntity); virtual;
    procedure BeforeRemove(Sender: TModelEntity); virtual;
    procedure AfterRemove(Sender: TModelEntity); virtual;
    procedure BeforeEntityChange(Sender: TModelEntity); virtual;
    procedure AfterEntityChange(Sender: TModelEntity); virtual;
  public
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

  TRtfdClassName = class(TRtfdCustomLabel) // , IAfterClassListener
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
    procedure AfterEntityChange(Sender: TModelEntity); override;
  end;

  TRtfdInterfaceName = class(TRtfdCustomLabel) // , IAfterInterfaceListener
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
    procedure AfterEntityChange(Sender: TModelEntity); override;
  end;

  //Left-justified label with visibility-icon
  TVisibilityLabel = class(TRtfdCustomLabel)
    procedure Paint; override;
    function WidthNeeded : integer; override;
  end;

  TRtfdOperation = class(TVisibilityLabel) // , IAfterOperationListener
  private
    O: TOperation;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
    procedure AfterEntityChange(Sender: TModelEntity); override;
//    procedure IAfterOperationListener.EntityChange = EntityChange;
  end;

  TRtfdAttribute = class(TVisibilityLabel) // , IAfterAttributeListener
  private
    A: TAttribute;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
    procedure AfterEntityChange(Sender: TModelEntity); override;
//    procedure IAfterAttributeListener.EntityChange = EntityChange;
  end;

  TRtfdSeparator = class(TGraphicControl)
  public
    constructor Create(Owner: TComponent); override;
    procedure Paint; override;
  end;

  TRtfdStereotype = class(TRtfdCustomLabel)
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity; Caption: string); reintroduce;
  end;

  TRtfdUnitPackageName = class(TRtfdCustomLabel) // , IAfterUnitPackageListener
  private
    P: TUnitPackage;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
    procedure AfterEntityChange(Sender: TModelEntity); override;
//    procedure IAfterUnitPackageListener.EntityChange = EntityChange;
  end;

  //Class to display mame of package at upper-left corner in a unitpackage diagram
  TRtfdUnitPackageDiagram = class(TRtfdCustomLabel) // , IAfterUnitPackageListener
  private
    P: TUnitPackage;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
    procedure AfterEntityChange(Sender: TModelEntity); override;
//    procedure IAfterUnitPackageListener.EntityChange = EntityChange;
  end;

implementation

uses Graphics, uError, SysUtils, essConnectPanel, uIterators,
uConfig, uRtfdDiagramFrame, Math;

const
  ClassShadowWidth = 3;
  cDefaultWidth = 185;
  cDefaultHeight = 41;

{ TRtfdBox }
constructor TRtfdBox.Create(Owner: TComponent; Entity: TModelEntity; Frame: TDiagramFrame; MinVisibility : TVisibility);
begin
  inherited Create(Owner);
  Color := clWhite;
  BorderWidth := ClassShadowWidth;
  Self.Frame := Frame;
  Self.Entity := Entity;
  Self.FMinVisibility := MinVisibility;
  ShowHint := True;
  Hint := Entity.Documentation.ShortDescription;
end;

procedure TRtfdBox.Paint;
const
  TopH = 39;
  TopColor : array[boolean] of TColor = ($EAF4F8, clWhite);
var
  R: TRect;
  Sw: integer;
begin
  Sw := ClassShadowWidth;
  R := GetClientRect;
  with Canvas do
  begin
    //Shadow
    Brush.Color := clSilver;
    Pen.Color := clSilver;
    RoundRect(R.Right - Sw - 8, R.Top + Sw, R.Right, R.Bottom, 8, 8);
    FillRect(Rect(Sw, R.Bottom - Sw, R.Right, R.Bottom));

    //Holes
    Brush.Color := (Parent as TessConnectPanel).Color;
    FillRect(Rect(R.Left, R.Bottom - Sw, R.Left + Sw, R.Bottom));
    FillRect(Rect(R.Right - Sw, R.Top, R.Right, R.Top + Sw));

    //Background
    Brush.Color := clWhite;
    Pen.Color := clBlack;

    Brush.Color := TopColor[ Config.IsLimitedColors ];
    RoundRect(R.Left, R.Top, R.Right - Sw, R.Top + TopH, 8, 8);
    Brush.Color := clWhite;
    Rectangle(R.Left, R.Top + TopH - 8, R.Right - Sw, R.Bottom - Sw);
    FillRect( Rect(R.Left+1,R.Top + TopH - 8, R.Right - Sw - 1, R.Top + TopH + 1 - 8) );
  end;
end;

procedure TRtfdBox.BeforeChange(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdBox.AfterChange(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdBox.BeforeAddChild(Sender, NewChild: TModelEntity);
begin
  //Stub
end;

procedure TRtfdBox.AfterAddChild(Sender, NewChild: TModelEntity);
begin
  //Stub
end;

procedure TRtfdBox.BeforeEntityChange(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdBox.AfterEntityChange(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdBox.BeforeRemove(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdBox.AfterRemove(Sender: TModelEntity);
begin
  //Stub
end;


procedure TRtfdBox.SetMinVisibility(const Value: TVisibility);
begin
  if Value<>FMinVisibility then
  begin
    FMinVisibility := Value;
    RefreshEntities;
  end;
end;


//The following declarations are needed for helping essconnectpanel to
//catch all mouse actions. All controls that are inserted (classname etc)
//in rtfdbox will get their mousedown-event redefined.
type
  TCrackControl = class(TControl);

procedure TRtfdBox.Notification(AComponent: TComponent; Operation: Classes.TOperation);
begin
  inherited;
  //Owner=Self must be tested because notifications are being sent for all components
  //in the form. TRtfdLabels are created with Owner=box.
  if (Operation = opInsert) and (Acomponent.Owner = Self) and (Acomponent is TControl) then
    TCrackControl(AComponent).OnMouseDown := OnChildMouseDown;
end;

procedure TRtfdBox.OnChildMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  pt.X := X;
  pt.Y := Y;
  pt := TControl(Sender).ClientToScreen(pt);
  pt := ScreenToClient(pt);
  MouseDown(Button,Shift,pt.X,pt.Y);
end;



{ TRtfdClass }

constructor TRtfdClass.Create(Owner: TComponent; Entity: TModelEntity; Frame: TDiagramFrame; MinVisibility : TVisibility);
begin
  inherited Create(Owner, Entity, Frame, MinVisibility);
  PopupMenu := Frame.ClassInterfacePopupMenu;
  Entity.AddListener(Self); // IAfterClassListener(
  RefreshEntities;
end;

destructor TRtfdClass.Destroy;
begin
  Entity.RemoveListener(Self); // IAfterClassListener(
  inherited;
end;

procedure TRtfdClass.AfterAddChild(Sender: TModelEntity; NewChild: TModelEntity);
begin
  RefreshEntities;
end;

procedure TRtfdClass.RefreshEntities;
var
  NeedH,NeedW,I : integer;
  C: TClass;
  Omi,Ami : TBaseModelIterator;
  WasVisible : boolean;
begin
  C := Entity as TClass;

  WasVisible := Visible;
  Hide;
  DestroyComponents;

  NeedW := 0;
  NeedH := (ClassShadowWidth * 2) + 4;
  Inc(NeedH, TRtfdClassName.Create(Self, Entity).Height);

  //Get names in visibility order
  if FMinVisibility > Low(TVisibility) then
  begin
    Omi := TModelIterator.Create(C.GetOperations,TOperation,FMinVisibility,ioVisibility);
    Ami := TModelIterator.Create(C.GetAttributes,TAttribute,FMinVisibility,ioVisibility);
  end
  else
  begin
    Omi := TModelIterator.Create(C.GetOperations,ioVisibility);
    Ami := TModelIterator.Create(C.GetAttributes,ioVisibility);
  end;

  //Separator
  if (Ami.Count>0) or (Omi.Count>0) then
    Inc(NeedH, TRtfdSeparator.Create(Self).Height);

  //Attributes
  while Ami.HasNext do
    Inc(NeedH, TRtfdAttribute.Create(Self,Ami.Next).Height);

  //Separator
  if (Ami.Count>0) and (Omi.Count>0) then
    Inc(NeedH, TRtfdSeparator.Create(Self).Height);

  //Operations
  while Omi.HasNext do
    Inc(NeedH, TRtfdOperation.Create(Self,Omi.Next).Height);

  for I := 0 to ControlCount-1 do
    if Controls[I] is TRtfdCustomLabel then
      NeedW := Max( TRtfdCustomLabel(Controls[I]).WidthNeeded,NeedW);

  Height :=  Max(NeedH,cDefaultHeight);
  Width  :=  Max(NeedW,cDefaultWidth);

  Visible := WasVisible;
end;

{ TRtfdUnitPackage }

constructor TRtfdUnitPackage.Create(Owner: TComponent; Entity: TModelEntity; Frame: TDiagramFrame; MinVisibility : TVisibility);
begin
  inherited Create(Owner, Entity, Frame, MinVisibility);
  PopupMenu := Frame.PackagePopupMenu;
  P := Entity as TUnitPackage;
  RefreshEntities;
end;

procedure TRtfdUnitPackage.DblClick;
begin
  PostMessage(Frame.Handle, WM_ChangePackage, 0, 0);
end;

procedure TRtfdUnitPackage.RefreshEntities;
begin
  DestroyComponents;
  TRtfdUnitPackageName.Create(Self, P);
  Height := 45;
end;

{ TRtfdCustomLabel }

constructor TRtfdCustomLabel.Create(Owner: TComponent;
  Entity: TModelEntity);
begin
  inherited Create(Owner);
  Parent := Owner as TWinControl;
  Self.Entity := Entity;
  Align := alTop;
  Height := Abs(Font.Height);
  FAlignment := taLeftJustify;
  FTransparent := True;
  //Top must be assigned so that all labels appears beneath each other when align=top
  Top := MaxInt;
end;

function TRtfdCustomLabel.WidthNeeded: integer;
begin
  Result := Width + 4 + (2 * ClassShadowWidth);
end;

//
procedure TRtfdCustomLabel.BeforeEntityChange(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdCustomLabel.AfterEntityChange(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdCustomLabel.BeforeRemove(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdCustomLabel.AfterRemove(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdCustomLabel.BeforeAddChild(Sender, NewChild: TModelEntity);
begin
  //Stub
end;

procedure TRtfdCustomLabel.AfterAddChild(Sender, NewChild: TModelEntity);
begin
  //Stub
end;

procedure TRtfdCustomLabel.BeforeChange(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdCustomLabel.AfterChange(Sender: TModelEntity);
begin
  //Stub
end;


{ TVisibilityLabel }

const
  IconW = 10;

procedure TVisibilityLabel.Paint;
var
  Rect : TRect;
  Pic : Graphics.TBitmap;
begin
{ifdef WIN32}
  Rect := ClientRect;

  case Entity.Visibility of
    viPrivate : Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisPrivateImage.Picture.Bitmap;
    viProtected : Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisProtectedImage.Picture.Bitmap;
    viPublic : Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisPublicImage.Picture.Bitmap;
  else
    Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisPublicImage.Picture.Bitmap;
  end;
  Canvas.Draw(Rect.Left,Rect.Top + 1, Pic );

  Canvas.Font := Font;
  Canvas.TextOut(Rect.Left + IconW + 4, Rect.Top, Caption);
{endif}
end;


function TVisibilityLabel.WidthNeeded: integer;
begin
  Result := Width + IconW;
end;

{ TRtfdClassName }

constructor TRtfdClassName.Create(Owner: TComponent; Entity: TModelEntity);
begin
  inherited Create(Owner, Entity);
  Font.Style := [fsBold];
  Alignment := taCenter;
  Entity.AddListener(Self); // IAfterClassListener(
  AfterEntityChange(nil);
end;

destructor TRtfdClassName.Destroy;
begin
  Entity.RemoveListener(Self); // IAfterClassListener(
  inherited;
end;

procedure TRtfdClassName.AfterEntityChange(Sender: TModelEntity);
var
  Mi : TBaseModelIterator;
begin
  Mi := (Entity as TClass).GetOperations;
  while Mi.HasNext do
    if (Mi.Next as TOperation).IsAbstract then
    begin
      Font.Style := Font.Style + [fsItalic];
      Break;
    end;
  if ((Owner as TRtfdBox).Frame as TDiagramFrame).Diagram.Package<>Entity.Owner then
    Caption := Entity.FullName
  else
    Caption := Entity.Name;
end;


{ TRtfdInterfaceName }

constructor TRtfdInterfaceName.Create(Owner: TComponent;
  Entity: TModelEntity);
begin
  inherited Create(Owner, Entity);
  Font.Style := [fsBold];
  Alignment := taCenter;
  Entity.AddListener(Self); // IAfterInterfaceListener(
  AfterEntityChange(nil);
end;

destructor TRtfdInterfaceName.Destroy;
begin
  Entity.RemoveListener(Self); // IAfterInterfaceListener(
  inherited;
end;

procedure TRtfdInterfaceName.AfterEntityChange(Sender: TModelEntity);
begin
  if ((Owner as TRtfdBox).Frame as TDiagramFrame).Diagram.Package<>Entity.Owner then
    Caption := Entity.FullName
  else
    Caption := Entity.Name;
end;


{ TRtfdSeparator }

constructor TRtfdSeparator.Create(Owner: TComponent);
begin
  //Cannot inherit from TCustomLabel in Kylix because it does not have a paint-method
  inherited Create(Owner);
  Parent := Owner as TWinControl;
  AutoSize := False;
  Height := 16;
  //Top must be assigned so that all labels appears beneath each other when align=top
  Top := MaxInt;
  Align := alTop;
end;

procedure TRtfdSeparator.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  //Canvas.FillRect(R);
  Canvas.Pen.Color := clBlack;
  Canvas.MoveTo(R.Left, R.Top + (Height div 2));
  Canvas.LineTo(R.Right, R.Top + (Height div 2));
end;

{ TRtfdPackageName }

constructor TRtfdUnitPackageName.Create(Owner: TComponent;
  Entity: TModelEntity);
begin
  inherited Create(Owner, Entity);
  Font.Style := [fsBold];
  Alignment := taCenter;
  P := Entity as TUnitPackage;
  P.AddListener(Self); // IAfterUnitPackageListener(
  AfterEntityChange(nil);
end;

destructor TRtfdUnitPackageName.Destroy;
begin
  P.RemoveListener(Self); // IAfterUnitPackageListener(
  inherited;
end;

procedure TRtfdUnitPackageName.AfterEntityChange(Sender: TModelEntity);
begin
  Caption := P.Name;
end;

{ TRtfdOperation }

constructor TRtfdOperation.Create(Owner: TComponent; Entity: TModelEntity);
begin
  inherited Create(Owner, Entity);
  O := Entity as TOperation;
  O.AddListener(Self); // IAfterOperationListener(
  AfterEntityChange(nil);
end;

destructor TRtfdOperation.Destroy;
begin
  O.RemoveListener(Self); // IAfterOperationListener(
  inherited;
end;

procedure TRtfdOperation.AfterEntityChange(Sender: TModelEntity);
const
  ColorMap: array[TOperationType] of TColor = (clGreen, clRed, clBlack, clGray);
  //   otConstructor,otDestructor,otProcedure,otFunction);
begin
  //Default uml-syntax
  //visibility name ( parameter-list ) : return-type-expression { property-string }
  { TODO : show parameters and returntype for operation }
  Caption := O.Name + '(...)';
  Font.Style := [];
  Font.Color := ColorMap[O.OperationType];
  if O.IsAbstract then
    Font.Style := [fsItalic];
end;

{ TRtfdAttribute }

constructor TRtfdAttribute.Create(Owner: TComponent; Entity: TModelEntity);
begin
  inherited Create(Owner, Entity);
  A := Entity as TAttribute;
  A.AddListener(Self); // IAfterAttributeListener(
  AfterEntityChange(nil);
end;

destructor TRtfdAttribute.Destroy;
begin
  A.RemoveListener(Self); // IAfterAttributeListener(
  inherited;
end;

procedure TRtfdAttribute.AfterEntityChange(Sender: TModelEntity);
begin
  //uml standard syntax is:
  //visibility name [ multiplicity ] : type-expression = initial-value { property-string }
  if Assigned(A.TypeClassifier) then
    Caption := A.Name + ' : ' + A.TypeClassifier.Name
  else
    Caption := A.Name;
end;

{ TRtfdUnitPackageDiagram }

constructor TRtfdUnitPackageDiagram.Create(Owner: TComponent;
  Entity: TModelEntity);
begin
  //This class is the caption in upper left corner for a unitdiagram
  inherited Create(Owner, Entity);
  Color := clBtnFace;
  Font.Name := 'Times New Roman';
  Font.Style := [fsBold];
  Font.Size := 12;
  Alignment := taLeftJustify;
  P := Entity as TUnitPackage;
  P.AddListener(Self); // IAfterUnitPackageListener(
  AfterEntityChange(nil);
end;

destructor TRtfdUnitPackageDiagram.Destroy;
begin
  P.RemoveListener(Self); // IAfterUnitPackageListener(
  inherited;
end;

procedure TRtfdUnitPackageDiagram.AfterEntityChange(Sender: TModelEntity);
begin
  Caption := '   ' + P.FullName;
end;


{ TRtfdInterface }

constructor TRtfdInterface.Create(Owner: TComponent; Entity: TModelEntity;
  Frame: TDiagramFrame; MinVisibility : TVisibility);
begin
  inherited Create(Owner, Entity, Frame, MinVisibility);
  Entity.AddListener(Self); // IAfterInterfaceListener(
  PopupMenu := Frame.ClassInterfacePopupMenu;
  RefreshEntities;
end;

destructor TRtfdInterface.Destroy;
begin
  Entity.RemoveListener(Self); // IAfterInterfaceListener(
  inherited;
end;

procedure TRtfdInterface.RefreshEntities;
var
  NeedW,NeedH,I : integer;
  OMi,AMi : TBaseModelIterator;
  WasVisible : boolean;
  Int : TInterface;
begin
  Int := Entity as TInterface;

  WasVisible := Visible;
  Hide;
  DestroyComponents;

  NeedW := 0;
  NeedH := (ClassShadowWidth * 2) + 4;

  Inc(NeedH, TRtfdStereotype.Create(Self, nil, 'interface').Height);
  Inc(NeedH, TRtfdInterfaceName.Create(Self, Entity).Height);

  //Get names in visibility order
  if FMinVisibility > Low(TVisibility) then
  begin
    Omi := TModelIterator.Create(Int.GetOperations,TOperation,FMinVisibility,ioVisibility);
    Ami := TModelIterator.Create(Int.GetAttributes,TAttribute,FMinVisibility,ioVisibility);
  end
  else
  begin
    Omi := TModelIterator.Create(Int.GetOperations,ioVisibility);
    Ami := TModelIterator.Create(Int.GetAttributes,ioVisibility);
  end;

  //Separator
  if (Ami.Count>0) or (Omi.Count>0) then
    Inc(NeedH, TRtfdSeparator.Create(Self).Height);

  //Attributes
  while Ami.HasNext do
    Inc(NeedH, TRtfdAttribute.Create(Self,Ami.Next).Height);

  //Separator
  if (Ami.Count>0) and (Omi.Count>0) then
    Inc(NeedH, TRtfdSeparator.Create(Self).Height);

  //Operations
  while Omi.HasNext do
    Inc(NeedH, TRtfdOperation.Create(Self,Omi.Next).Height);

  for I := 0 to ControlCount-1 do
    if Controls[I] is TRtfdCustomLabel then
      NeedW := Max( TRtfdCustomLabel(Controls[I]).WidthNeeded,NeedW);

  Height :=  Max(NeedH,cDefaultHeight);
  Width  :=  Max(NeedW,cDefaultWidth);

  Visible := WasVisible;
end;

procedure TRtfdInterface.AfterAddChild(Sender, NewChild: TModelEntity);
begin
  RefreshEntities;
end;

{ TRtfdStereotype }

constructor TRtfdStereotype.Create(Owner: TComponent; Entity: TModelEntity; Caption: string);
begin
  inherited Create(Owner, Entity);
  Alignment := taCenter;
  Self.Caption := '<<' + Caption + '>>';
end;

function TRtfdCustomLabel.GetAlignment: TAlignment;
begin
  Result := FAlignment;
end;

procedure TRtfdCustomLabel.SetAlignment(const Value: TAlignment);
begin
  if Value <> FAlignment then
    begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TRtfdCustomLabel.Paint;
var
  Al: Integer;
  oldFont: TFont;
  r: TRect;
begin
  inherited;
  { TODO : Fix }
  oldFont := Canvas.Font;
  Canvas.Font := Font;
  if FTransparent then
    Canvas.Brush.Style := bsClear
  else
    Canvas.Brush.Style := bsSolid;
  Al := DT_LEFT;
  case FAlignment of
    taLeftJustify: Al := DT_LEFT;
    taRightJustify: Al := DT_RIGHT;
    taCenter: Al := DT_CENTER;
  end;
  r := ClientRect;
  DrawText(Canvas.Handle,PChar(Caption),Length(Caption),r,Al);
  Canvas.Font := oldFont;
end;

procedure TRtfdCustomLabel.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;


function TRtfdCustomLabel.GetText: TCaption;
begin
  Result := FCaption;
end;

procedure TRtfdCustomLabel.SetText(const Value: TCaption);
begin
  inherited;
  if FCaption <> Value then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TRtfdCustomLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  Adjustbounds;
end;

procedure TRtfdCustomLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
  AAlignment: TAlignment;
begin
  if not (csReading in ComponentState) then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(Rect, (DT_EXPANDTABS or DT_CALCRECT));
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    AAlignment := FAlignment;
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    if AAlignment = taRightJustify then Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TRtfdCustomLabel.DoDrawText(var Rect: TRect; Flags: Longint);
var
  Text: string;
begin
  Text := Caption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  Flags := Flags or DT_NOPREFIX;
//!!!  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  end
  else
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
end;


end.