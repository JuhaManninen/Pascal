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
  LCLIntf, LCLType, LMessages, Messages, ExtCtrls, Classes, Controls,
  Contnrs, Forms,
  uModel, uModelEntity, uDiagramFrame;

type

  TRtfdBox = class;

  { TRtfdBox }

  //Baseclass for a diagram-panel
  TRtfdBoxClass = class of TRtfdBox;
  TRtfdBox = class(TPanel) // , IModelEntityListener
  private
    FMinVisibility : TVisibility;
    procedure OnChildMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetMinVisibility(const Value: TVisibility);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    Frame: TDiagramFrame;
    Entity: TModelEntity;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity;
                       Frame: TDiagramFrame; MinVisibility: TVisibility); reintroduce; virtual;
    destructor Destroy; override;
    procedure RefreshEntities; virtual; abstract;
  public
    property MinVisibility : TVisibility write SetMinVisibility;
  end;


  TRtfdClass = class;

  // Listener for TRtfdClass.
  TRtfdClassListener = class(TListenerBase)
  private
    FRtfdOwner: TRtfdClass;
  public
    constructor Create(ARtfdOwner: TRtfdClass);
    procedure AfterAddChild(Sender: TModelEntity; NewChild: TModelEntity); override;
  end;

  { TRtfdClass }
  TRtfdClass = class(TRtfdBox) // , IAfterClassListener
  private
    FListener: TRtfdClassListener;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity;
                       Frame: TDiagramFrame; MinVisibility: TVisibility); override;
    destructor Destroy; override;
    procedure RefreshEntities; override;
  public
    property Listener: TRtfdClassListener read FListener;
  end;


  TRtfdInterface = class;

  // Listener for TRtfdInterface.
  TRtfdInterfaceListener = class(TListenerBase)
  private
    FRtfdOwner: TRtfdInterface;
  public
    constructor Create(ARtfdOwner: TRtfdInterface);
    procedure AfterAddChild(Sender: TModelEntity; NewChild: TModelEntity); override;
  end;

  { TRtfdInterface }
  TRtfdInterface = class(TRtfdBox) // , IAfterInterfaceListener
  private
    FListener: TRtfdInterfaceListener;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity;
                       Frame: TDiagramFrame; MinVisibility : TVisibility); override;
    destructor Destroy; override;
    procedure RefreshEntities; override;
  public
    property Listener: TRtfdInterfaceListener read FListener;
  end;


  TRtfdUnitPackage = class(TRtfdBox)
  private
    procedure DblClick(Sender: TObject);
  public
    P: TUnitPackage;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity;
                       Frame: TDiagramFrame; MinVisibility: TVisibility); override;
    procedure RefreshEntities; override;
  end;

  TRtfdCustomLabel = class;

  { TRtfdCustomLabel }
//  TRtfdCustomLabel = class(TCustomLabel, IModelEntityListener)
  TRtfdCustomLabel = class(TGraphicControl) // , IModelEntityListener
  private
    Entity: TModelEntity;
    FCaption: TCaption;
    FAlignment: TAlignment;
    FTransparent: Boolean;
    function GetAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetTransparent(const Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure AdjustBounds;
    procedure DoDrawText(var Rect: TRect; Flags: Integer);
  protected
    procedure SetText(const Value: TCaption);
    function GetText: TCaption;
    procedure Paint; override;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); reintroduce; virtual;
    destructor Destroy; override;
    function WidthNeeded : integer; virtual;
  public
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;


  TRtfdClassName = class;

  // Listener for TRtfdClassName.
  TRtfdClassNameListener = class(TListenerBase)
  private
    FRtfdOwner: TRtfdClassName;
  public
    constructor Create(ARtfdOwner: TRtfdClassName);
    procedure AfterEntityChange(Sender: TModelEntity); override;
  end;

  { TRtfdClassName }
  TRtfdClassName = class(TRtfdCustomLabel) // , IAfterClassListener
  private
    FListener: TRtfdClassNameListener;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
  end;


  TRtfdInterfaceName = class;

  // Listener for TRtfdInterfaceName.
  TRtfdInterfaceNameListener = class(TListenerBase)
  private
    FRtfdOwner: TRtfdInterfaceName;
  public
    constructor Create(ARtfdOwner: TRtfdInterfaceName);
    procedure AfterEntityChange(Sender: TModelEntity); override;
  end;

  { TRtfdInterfaceName }
  TRtfdInterfaceName = class(TRtfdCustomLabel) // , IAfterInterfaceListener
  private
    FListener: TRtfdInterfaceNameListener;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
  end;


  //Left-justified label with visibility-icon
  TVisibilityLabel = class(TRtfdCustomLabel)
//    procedure Paint; override;
    function WidthNeeded : integer; override;
  end;


  TRtfdOperation = class;

  // Listener for TRtfdOperation.
  TRtfdOperationListener = class(TListenerBase)
  private
    FRtfdOwner: TRtfdOperation;
  public
    constructor Create(ARtfdOwner: TRtfdOperation);
    procedure AfterEntityChange(Sender: TModelEntity); override;
//    procedure IAfterOperationListener.EntityChange = EntityChange;
  end;

  { TRtfdOperation }
  TRtfdOperation = class(TVisibilityLabel) // , IAfterOperationListener
  private
    FListener: TRtfdOperationListener;
    O: TMdlOperation;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
  end;


  TRtfdAttribute = class;

  // Listener for TRtfdAttribute.
  TRtfdAttributeListener = class(TListenerBase)
  private
    FRtfdOwner: TRtfdAttribute;
  public
    constructor Create(ARtfdOwner: TRtfdAttribute);
    procedure AfterEntityChange(Sender: TModelEntity); override;
//    procedure IAfterAttributeListener.EntityChange = EntityChange;
  end;

  { TRtfdAttribute }
  TRtfdAttribute = class(TVisibilityLabel) // , IAfterAttributeListener
  private
    FListener: TRtfdAttributeListener;
    A: TAttribute;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
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


  TRtfdUnitPackageName = class;

  // Listener for TRtfdUnitPackageName.
  TRtfdUnitPackageNameListener = class(TListenerBase)
  private
    FRtfdOwner: TRtfdUnitPackageName;
  public
    constructor Create(ARtfdOwner: TRtfdUnitPackageName);
    procedure AfterEntityChange(Sender: TModelEntity); override;
//    procedure IAfterUnitPackageListener.EntityChange = EntityChange;
  end;

  { TRtfdUnitPackageName }
  TRtfdUnitPackageName = class(TRtfdCustomLabel) // , IAfterUnitPackageListener
  private
    FListener: TRtfdUnitPackageNameListener;
    P: TUnitPackage;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
  end;


  TRtfdUnitPackageDiagram = class;

  // Listener for TRtfdUnitPackageDiagram.
  TRtfdUnitPackageDiagramListener = class(TListenerBase)
  private
    FRtfdOwner: TRtfdUnitPackageDiagram;
  public
    constructor Create(ARtfdOwner: TRtfdUnitPackageDiagram);
    procedure AfterEntityChange(Sender: TModelEntity); override;
//    procedure IAfterUnitPackageListener.EntityChange = EntityChange;
  end;

  //Class to display mame of package at upper-left corner in a unitpackage diagram
  TRtfdUnitPackageDiagram = class(TRtfdCustomLabel) // , IAfterUnitPackageListener
  private
    FListener: TRtfdUnitPackageDiagramListener;
    P: TUnitPackage;
  public
    constructor Create(Owner: TComponent; Entity: TModelEntity); override;
    destructor Destroy; override;
  end;

  { TRtfdBoxList }

  TRtfdBoxList = class(TObjectList)
  private
    function GetItems(AIndex: integer): TRtfdBox;
    procedure SetItems(AIndex: integer; const AValue: TRtfdBox);
  public
    property Items[AIndex: integer]: TRtfdBox read GetItems write SetItems; default;
  end;


implementation

uses
  Graphics, uError, SysUtils, essConnectPanel, uIterators,
  uConfig, Math;

const
  ClassShadowWidth = 3;
  cDefaultWidth = 185;
  cDefaultHeight = 41;


//The following declarations are needed for helping essconnectpanel to
//catch all mouse actions. All controls that are inserted (classname etc)
//in rtfdbox will get their mousedown-event redefined.
type
  TCrackControl = class(TControl);

{ TRtfdBox }

constructor TRtfdBox.Create(Owner: TComponent; Entity: TModelEntity;
                            Frame: TDiagramFrame; MinVisibility : TVisibility);
begin
  inherited Create(Owner);
  Self.Frame := Frame;
  Self.Entity := Entity;
  Self.FMinVisibility := MinVisibility;
  Color := clWhite;
  BorderWidth := ClassShadowWidth;
  ShowHint := True;
  Hint := Entity.Documentation.ShortDescription;
end;

destructor TRtfdBox.Destroy;
begin
  inherited Destroy;
end;

procedure TRtfdBox.SetMinVisibility(const Value: TVisibility);
begin
  if Value<>FMinVisibility then
  begin
    FMinVisibility := Value;
    RefreshEntities;
  end;
end;

procedure TRtfdBox.Notification(AComponent: TComponent; Operation: TOperation);
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


{ TRtfdClassListener }

constructor TRtfdClassListener.Create(ARtfdOwner: TRtfdClass);
begin
  FRtfdOwner := ARtfdOwner;
end;

procedure TRtfdClassListener.AfterAddChild(Sender: TModelEntity; NewChild: TModelEntity);
begin
  FRtfdOwner.RefreshEntities;
end;


{ TRtfdClass }

constructor TRtfdClass.Create(Owner: TComponent; Entity: TModelEntity;
                              Frame: TDiagramFrame; MinVisibility : TVisibility);
begin
  inherited Create(Owner, Entity, Frame, MinVisibility);
  PopupMenu := Frame.ClassInterfacePopupMenu;
  FListener := TRtfdClassListener.Create(Self);
  Entity.AddListener(FListener); // IAfterClassListener(
  RefreshEntities;
end;

destructor TRtfdClass.Destroy;
begin
  Entity.RemoveListener(FListener); // IAfterClassListener(
  FListener.Free;
  inherited;
end;

procedure TRtfdClass.RefreshEntities;
var
  NeedH,NeedW,I : integer;
  C: TMdlClass;
  Omi,Ami : TBaseModelIterator;
  tempOmi,tempAmi : TBaseModelIterator;
  WasVisible : boolean;
begin
  C := Entity as TMdlClass;

  WasVisible := Visible;
  Hide;
  DestroyComponents;

  NeedW := 0;
  NeedH := (ClassShadowWidth * 2) + 4;
  Inc(NeedH, TRtfdClassName.Create(Self, Entity).Height);

  tempOmi := C.GetOperations;
  tempAmi := C.GetAttributes;
  //Get names in visibility order
  if FMinVisibility > Low(TVisibility) then
  begin
    Omi := TModelIterator.Create(tempOmi, TMdlOperation, FMinVisibility, ioVisibility);
    Ami := TModelIterator.Create(tempAmi, TAttribute, FMinVisibility, ioVisibility);
  end
  else begin
    Omi := TModelIterator.Create(tempOmi, ioVisibility);
    Ami := TModelIterator.Create(tempAmi, ioVisibility);
  end;
  try
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
  finally
    Ami.Free;
    Omi.Free;
    tempAmi.Free;
    tempOmi.Free;
  end;

  for I := 0 to ControlCount-1 do
    if Controls[I] is TRtfdCustomLabel then
      NeedW := Max(TRtfdCustomLabel(Controls[I]).WidthNeeded,NeedW);
  Width  := Max(NeedW,cDefaultWidth);

  Height := Max(NeedH, cDefaultHeight);
  Visible := WasVisible;
end;


{ TRtfdInterfaceListener }

constructor TRtfdInterfaceListener.Create(ARtfdOwner: TRtfdInterface);
begin
  FRtfdOwner := ARtfdOwner;
end;

procedure TRtfdInterfaceListener.AfterAddChild(Sender: TModelEntity; NewChild: TModelEntity);
begin
  FRtfdOwner.RefreshEntities;
end;

{ TRtfdInterface }

constructor TRtfdInterface.Create(Owner: TComponent; Entity: TModelEntity;
  Frame: TDiagramFrame; MinVisibility : TVisibility);
begin
  inherited Create(Owner, Entity, Frame, MinVisibility);
  PopupMenu := Frame.ClassInterfacePopupMenu;
  FListener := TRtfdInterfaceListener.Create(Self);
  Entity.AddListener(FListener); // IAfterInterfaceListener(
  RefreshEntities;
end;

destructor TRtfdInterface.Destroy;
begin
  Entity.RemoveListener(FListener); // IAfterInterfaceListener(
  FListener.Free;
  inherited;
end;

procedure TRtfdInterface.RefreshEntities;
var
  NeedW,NeedH,I : integer;
  OMi, AMi : TBaseModelIterator;
  tempOMi, tempAMi : TBaseModelIterator;
  WasVisible : boolean;
  Int : TMdlInterface;
begin
  Int := Entity as TMdlInterface;

  WasVisible := Visible;
  Hide;
  DestroyComponents;

  NeedW := 0;
  NeedH := (ClassShadowWidth * 2) + 4;

  Inc(NeedH, TRtfdStereotype.Create(Self, nil, 'interface').Height);
  Inc(NeedH, TRtfdInterfaceName.Create(Self, Entity).Height);

  tempOmi := Int.GetOperations;
  tempAmi := Int.GetAttributes;
  //Get names in visibility order
  if FMinVisibility > Low(TVisibility) then
  begin
    Omi := TModelIterator.Create(tempOMi, TMdlOperation, FMinVisibility, ioVisibility);
    Ami := TModelIterator.Create(tempAMi, TAttribute, FMinVisibility, ioVisibility);
  end
  else begin
    Omi := TModelIterator.Create(tempOMi, ioVisibility);
    Ami := TModelIterator.Create(tempAMi, ioVisibility);
  end;
  try
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
  finally
    AMi.Free;
    OMi.Free;
    tempAMi.Free;
    tempOMi.Free;
  end;
  for I := 0 to ControlCount-1 do
    if Controls[I] is TRtfdCustomLabel then
      NeedW := Max( TRtfdCustomLabel(Controls[I]).WidthNeeded, NeedW);
  Width := Max(NeedW,cDefaultWidth);

  Height := Max(NeedH, cDefaultHeight);
  Visible := WasVisible;
end;


{ TRtfdUnitPackage }

constructor TRtfdUnitPackage.Create(Owner: TComponent; Entity: TModelEntity; Frame: TDiagramFrame; MinVisibility : TVisibility);
begin
  inherited Create(Owner, Entity, Frame, MinVisibility);
  PopupMenu := Frame.PackagePopupMenu;
  OnDblClick := DblClick;
  P := Entity as TUnitPackage;
  RefreshEntities;
end;

procedure TRtfdUnitPackage.DblClick(Sender: TObject);
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

constructor TRtfdCustomLabel.Create(Owner: TComponent; Entity: TModelEntity);
begin
  inherited Create(Owner);
  Self.Entity := Entity;
  Parent := Owner as TWinControl;
  Align := alTop;
  Height := Abs(Font.Height);
  FAlignment := taLeftJustify;
  FTransparent := True;
  //Top must be assigned so that all labels appears beneath each other when align=top
  Top := MaxInt;
end;

destructor TRtfdCustomLabel.Destroy;
begin
  inherited;
end;

function TRtfdCustomLabel.WidthNeeded: integer;
begin
  Result := Width + 4 + (2 * ClassShadowWidth);
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
//    taLeftJustify:  Al := DT_LEFT;
    taRightJustify: Al := DT_RIGHT;
    taCenter:       Al := DT_CENTER;
  end;
  r := ClientRect;
  DrawText(Canvas.Handle,PChar(Caption),Length(Caption),r,Al);
  Canvas.Font := oldFont;
end;

procedure TRtfdCustomLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  Adjustbounds;
end;

procedure TRtfdCustomLabel.AdjustBounds;
//const WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
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
//ToDo!!!  Flags := DrawTextBiDiModeFlags(Flags);
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


{ TVisibilityLabel }

const
  IconW = 10;

{ToDo!!! procedure TVisibilityLabel.Paint;
var
  Rect : TRect;
  Pic : Graphics.TBitmap;
begin  }
{ifdef WIN32}
{  Rect := ClientRect;

  case Entity.Visibility of
    viPrivate : Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisPrivateImage.Picture.Bitmap;
    viProtected : Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisProtectedImage.Picture.Bitmap;
    viPublic : Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisPublicImage.Picture.Bitmap;
  else
    Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisPublicImage.Picture.Bitmap;
  end;
  Canvas.Draw(Rect.Left,Rect.Top + 1, Pic );

  Canvas.Font := Font;
  Canvas.TextOut(Rect.Left + IconW + 4, Rect.Top, Caption);  }
{endif}
//end;


function TVisibilityLabel.WidthNeeded: integer;
begin
  Result := Width + IconW;
end;


{ TRtfdClassNameListener }

constructor TRtfdClassNameListener.Create(ARtfdOwner: TRtfdClassName);
begin
  FRtfdOwner := ARtfdOwner;
end;

procedure TRtfdClassNameListener.AfterEntityChange(Sender: TModelEntity);
var
  Mi : TBaseModelIterator;
begin
  with FRtfdOwner do begin
    Mi := (Entity as TMdlClass).GetOperations;
    try
      while Mi.HasNext do
      begin
        if (Mi.Next as TMdlOperation).IsAbstract then
        begin
          Font.Style := Font.Style + [fsItalic];
          Break;
        end;
      end;
    finally
      Mi.Free;
    end;
    if ((Owner as TRtfdBox).Frame as TDiagramFrame).Diagram.xPackage<>Entity.Owner then
      Caption := Entity.FullName
    else
      Caption := Entity.Name;
  end;
end;

{ TRtfdClassName }

constructor TRtfdClassName.Create(Owner: TComponent; Entity: TModelEntity);
begin
  inherited Create(Owner, Entity);
  Font.Style := [fsBold];
  Alignment := taCenter;
  FListener := TRtfdClassNameListener.Create(Self);
  Entity.AddListener(FListener); // IAfterClassListener(
  FListener.AfterEntityChange(nil);
end;

destructor TRtfdClassName.Destroy;
begin
  Entity.RemoveListener(FListener); // IAfterClassListener(
  FListener.Free;
  inherited;
end;


{ TRtfdClassNameListener }

constructor TRtfdInterfaceNameListener.Create(ARtfdOwner: TRtfdInterfaceName);
begin
  FRtfdOwner := ARtfdOwner;
end;

procedure TRtfdInterfaceNameListener.AfterEntityChange(Sender: TModelEntity);
begin
  with FRtfdOwner do begin
    if ((Owner as TRtfdBox).Frame as TDiagramFrame).Diagram.xPackage<>Entity.Owner then
      FRtfdOwner.Caption := Entity.FullName
    else
      FRtfdOwner.Caption := Entity.Name;
  end;
end;

{ TRtfdInterfaceName }

constructor TRtfdInterfaceName.Create(Owner: TComponent; Entity: TModelEntity);
begin
  inherited Create(Owner, Entity);
  Font.Style := [fsBold];
  Alignment := taCenter;
  FListener := TRtfdInterfaceNameListener.Create(Self);
  Entity.AddListener(FListener); // IAfterInterfaceListener(
  FListener.AfterEntityChange(nil);
end;

destructor TRtfdInterfaceName.Destroy;
begin
  Entity.RemoveListener(FListener); // IAfterInterfaceListener(
  FListener.Free;
  inherited;
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


{ TRtfdStereotype }

constructor TRtfdStereotype.Create(Owner: TComponent; Entity: TModelEntity; Caption: string);
begin
  inherited Create(Owner, Entity);
  Alignment := taCenter;
  Caption := '<<' + Caption + '>>';
end;


{ TRtfdOperationListener }

constructor TRtfdOperationListener.Create(ARtfdOwner: TRtfdOperation);
begin
  FRtfdOwner := ARtfdOwner;
end;

procedure TRtfdOperationListener.AfterEntityChange(Sender: TModelEntity);
const
  ColorMap: array[TOperationType] of TColor = (clGreen, clRed, clBlack, clGray);
  //   otConstructor,otDestructor,otProcedure,otFunction);
begin
  //Default uml-syntax
  //visibility name ( parameter-list ) : return-type-expression { property-string }
  { TODO : show parameters and returntype for operation }
  with FRtfdOwner do begin
    FRtfdOwner.Caption := O.Name + '(...)';
    FRtfdOwner.Font.Style := [];
    FRtfdOwner.Font.Color := ColorMap[O.OperationType];
    if O.IsAbstract then
      FRtfdOwner.Font.Style := [fsItalic];
  end;
end;

{ TRtfdOperation }

constructor TRtfdOperation.Create(Owner: TComponent; Entity: TModelEntity);
begin
  inherited Create(Owner, Entity);
  O := Entity as TMdlOperation;
  FListener := TRtfdOperationListener.Create(Self);
  O.AddListener(FListener); // IAfterOperationListener(
  FListener.AfterEntityChange(nil);
end;

destructor TRtfdOperation.Destroy;
begin
  O.RemoveListener(FListener); // IAfterOperationListener(
  FListener.Free;
  inherited;
end;


{ TRtfdAttributeListener }

constructor TRtfdAttributeListener.Create(ARtfdOwner: TRtfdAttribute);
begin
  FRtfdOwner := ARtfdOwner;
end;

procedure TRtfdAttributeListener.AfterEntityChange(Sender: TModelEntity);
begin
  //uml standard syntax is:
  //visibility name [ multiplicity ] : type-expression = initial-value { property-string }
  with FRtfdOwner do begin
    if Assigned(A.TypeClassifier) then
      FRtfdOwner.Caption := A.Name + ' : ' + A.TypeClassifier.Name
    else
      FRtfdOwner.Caption := A.Name;
  end;
end;

{ TRtfdAttribute }

constructor TRtfdAttribute.Create(Owner: TComponent; Entity: TModelEntity);
begin
  inherited Create(Owner, Entity);
  A := Entity as TAttribute;
  FListener := TRtfdAttributeListener.Create(Self);
  A.AddListener(FListener); // IAfterAttributeListener(
  FListener.AfterEntityChange(nil);
end;

destructor TRtfdAttribute.Destroy;
begin
  A.RemoveListener(FListener); // IAfterAttributeListener(
  FListener.Free;
  inherited;
end;


{ TRtfdUnitPackageNameListener }

constructor TRtfdUnitPackageNameListener.Create(ARtfdOwner: TRtfdUnitPackageName);
begin
  FRtfdOwner := ARtfdOwner;
end;

procedure TRtfdUnitPackageNameListener.AfterEntityChange(Sender: TModelEntity);
begin
  with FRtfdOwner do
    Caption := P.Name;
end;

{ TRtfdUnitPackageName }

constructor TRtfdUnitPackageName.Create(Owner: TComponent; Entity: TModelEntity);
begin
  inherited Create(Owner, Entity);
  Font.Style := [fsBold];
  Alignment := taCenter;
  P := Entity as TUnitPackage;
  FListener := TRtfdUnitPackageNameListener.Create(Self);
  P.AddListener(FListener); // IAfterUnitPackageListener(
  FListener.AfterEntityChange(nil);
end;

destructor TRtfdUnitPackageName.Destroy;
begin
  P.RemoveListener(FListener); // IAfterUnitPackageListener(
  FListener.Free;
  inherited;
end;


{ TRtfdUnitPackageDiagramListener }

constructor TRtfdUnitPackageDiagramListener.Create(ARtfdOwner: TRtfdUnitPackageDiagram);
begin
  FRtfdOwner := ARtfdOwner;
end;

procedure TRtfdUnitPackageDiagramListener.AfterEntityChange(Sender: TModelEntity);
begin
  FRtfdOwner.Caption := '   ' + FRtfdOwner.P.FullName;
end;

{ TRtfdUnitPackageDiagram }

constructor TRtfdUnitPackageDiagram.Create(Owner: TComponent; Entity: TModelEntity);
begin
  //This class is the caption in upper left corner for a unitdiagram
  inherited Create(Owner, Entity);
  Color := clBtnFace;
  Font.Name := 'Times New Roman';
  Font.Style := [fsBold];
  Font.Size := 12;
  Alignment := taLeftJustify;
  P := Entity as TUnitPackage;
  FListener := TRtfdUnitPackageDiagramListener.Create(Self);
  P.AddListener(FListener); // IAfterUnitPackageListener(
  FListener.AfterEntityChange(nil);
end;

destructor TRtfdUnitPackageDiagram.Destroy;
begin
  P.RemoveListener(FListener); // IAfterUnitPackageListener(
  FListener.Free;
  inherited;
end;


{ TRtfdBoxList }

function TRtfdBoxList.GetItems(AIndex: integer): TRtfdBox;
begin
  Result := (inherited Items[AIndex]) as TRtfdBox;
end;

procedure TRtfdBoxList.SetItems(AIndex: integer; const AValue: TRtfdBox);
begin
  Items[AIndex] := AValue;
end;


end.