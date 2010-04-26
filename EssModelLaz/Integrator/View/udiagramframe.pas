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

unit uDiagramFrame;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, uViewIntegrator, StdCtrls, ExtCtrls, uListeners, uModelEntity,
  uModel, Buttons;

const
  WM_ChangePackage = WM_USER + 1;
type
  TDiagramFrame = class(TFrame) // IBeforeObjectModelListener,IAfterObjectModelListener
    ActionList: TActionList;
    OpenSelectedPackageAction: TAction;
    PackagePopupMenu: TPopupMenu;
    Openselectedpackageindiagram1: TMenuItem;
    Panel1: TPanel;
    VisibilityCombo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    OpenButton: TSpeedButton;
    LayoutButton: TSpeedButton;
    HideDiagramElementAction: TAction;
    Hide1: TMenuItem;
    ClassInterfacePopupMenu: TPopupMenu;
    Hide2: TMenuItem;
    ConnectionsCombo: TComboBox;
    procedure VisibilityComboChange(Sender: TObject);
    procedure HideDiagramElementActionExecute(Sender: TObject);
    procedure ConnectionsComboChange(Sender: TObject);
  private
    Bypass : boolean;
//    procedure IBeforeObjectModelListener.Change = ModelBeforeChange;
//    procedure IAfterObjectModelListener.Change = ModelAfterChange;
  protected
  public
    procedure PackageChange(var M: TMessage); message WM_ChangePackage;

    constructor Create(AOwner: TComponent; Model : TObjectModel); reintroduce;
    destructor Destroy; override;
    procedure OnUpdateToolbar(Sender : TObject);
    //Model listener
    procedure BeforeChange(Sender: TModelEntity);
    procedure AfterChange(Sender: TModelEntity);
  public
    Diagram : TDiagramIntegrator;
    Model : TObjectModel;
    ScrollBox : TScrollBox;
  end;

{
diagramframe
  lägg upp toolbar
  visibilitydropdown
    on change
      if not bypass
        diagram.VisibilityFilter=value
    OnUpdateToolbar
      bypass=true
      visibilitydropdown.itemindex
  diagram
    property VisiblityFilter
      set FVisiblityFilter=value
      if changed
        DoOnUpdateToolbar
    OnUpdateToolbar event
}

implementation

uses uError, uMainModule;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

type
  TScrollBoxWithNotify = class(TScrollBox)
  protected
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  public
    constructor Create(AOwner: TComponent); override;
  end;


{ TDiagramFrame }

constructor TDiagramFrame.Create(AOwner: TComponent; Model : TObjectModel);
begin
  inherited Create(AOwner);
  Self.Model := Model;
  Model.AddListener(Self); // IAfterObjectModelListener(
  VisibilityCombo.ItemIndex := 0;
  //Hindra att caption från FileOpenAction syns på knapp, vi vill bara ha glyfen
  OpenButton.Caption:='';
  LayoutButton.Caption:='';
  ScrollBox := TScrollBoxWithNotify.Create(Self);
  ScrollBox.Parent := Self;
  ScrollBox.AutoScroll:=True; // JuMa
end;

destructor TDiagramFrame.Destroy;
begin
  if Assigned(Model) and not Application.Terminated then
    Model.RemoveListener(Self); // IAfterObjectModelListener(
  inherited;
end;


procedure TDiagramFrame.OnUpdateToolbar(Sender: TObject);
const
  C : array[boolean] of integer = (1,0);
begin
  Bypass := True;
  VisibilityCombo.ItemIndex := Integer(Diagram.VisibilityFilter);
  ConnectionsCombo.ItemIndex := C[ Diagram.ShowAssoc ];
  Bypass := False;
end;

//Message för att en gui-komponent skall kunna begära packagechange
//utan att riskera krash p.g.a. komponenten blir destroyed.
procedure TDiagramFrame.PackageChange(var M: TMessage);
begin
  OpenSelectedPackageAction.Execute;
end;

procedure TDiagramFrame.VisibilityComboChange(Sender: TObject);
var
  I : integer;
begin
  I := VisibilityCombo.ItemIndex;
  if (not Bypass) and (I<>-1) then
  begin
    if I=VisibilityCombo.Items.Count-1 then
      //Hide all members, set minimum to highest + 1
      Diagram.VisibilityFilter := TVisibility( Integer(High(TVisibility)) + 1 )
    else
      Diagram.VisibilityFilter := TVisibility(VisibilityCombo.ItemIndex);
  end;
end;

procedure TDiagramFrame.BeforeChange(Sender: TModelEntity);
begin
  //Someone have to do a reset to currententity, so it's done here.
  uViewIntegrator.SetCurrentEntity(nil);
end;

procedure TDiagramFrame.AfterChange(Sender: TModelEntity);
var
  HasModel : boolean;
begin
  HasModel := Model.ModelRoot.GetAllUnitPackages.Count > 0;
  VisibilityCombo.Enabled := HasModel;
  ConnectionsCombo.Enabled := HasModel;
end;

procedure TDiagramFrame.HideDiagramElementActionExecute(Sender: TObject);
begin
  Diagram.HideSelectedDiagramElements;
end;


{ TScrollBoxWithNotify }

constructor TScrollBoxWithNotify.Create(AOwner: TComponent);
begin
  inherited;
  HorzScrollBar.Smooth := True;
  HorzScrollBar.Tracking := True;
  VertScrollBar.Smooth := True;
  VertScrollBar.Tracking := True;
  Align := alClient;
end;

procedure TScrollBoxWithNotify.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  if (Message.ScrollBar = 0) and (HorzScrollBar.Visible) and Assigned(OnResize) then
    OnResize(nil);
end;

procedure TScrollBoxWithNotify.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  if (Message.ScrollBar = 0) and (VertScrollBar.Visible) and Assigned(OnResize) then
    OnResize(nil);
end;

procedure TDiagramFrame.ConnectionsComboChange(Sender: TObject);
var
  P : TAbstractPackage;
begin
  if not Bypass then
  begin
    Diagram.ShowAssoc := ConnectionsCombo.ItemIndex=0;
    //We have to do a full refresh with store/fetch, this is only done at setpackage
    P := Diagram.Package;
    Diagram.Package := nil;
    Diagram.Package := P;
    Diagram.InitFromModel;
  end;
end;


end.
