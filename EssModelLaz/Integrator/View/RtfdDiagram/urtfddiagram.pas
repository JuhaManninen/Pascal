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

unit uRtfdDiagram;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses Controls, Graphics, Classes, Forms, Types,
  uDiagramFrame, uRtfdComponents, uFeedback, uViewIntegrator, essConnectPanel,
  uModelEntity, uModel;


type

  { TRtfdDiagram }

  TRtfdDiagram = class(TDiagramIntegrator)
// ,IBeforeObjectModelListener, IAfterObjectModelListener, IAfterUnitPackageListener)
  private
    Panel: TessConnectPanel;
    Frame: TDiagramFrame;
    //Map Entity.fullName -> TRtfdCustomPanel
    BoxNames: TStringList;
    FHasHidden : boolean;
    FHasChanged : boolean;
    IsAllClasses : boolean;
    ZoomFocusW,ZoomFocusH : integer;
    procedure ClearDiagram;
    procedure AddBox(E: TModelEntity);
    function GetBox(const S : string) : TRtfdBox;
    procedure ResolveAssociations;
    //Model listeners
    procedure ModelBeforeChange(Sender: TModelEntity);
    procedure ModelAfterChange(Sender: TModelEntity);
//    procedure IBeforeObjectModelListener.Change = ModelBeforeChange;
//    procedure IAfterObjectModelListener.Change = ModelAfterChange;
    //Unitpackage listeners
    procedure UnitPackageAfterChange(Sender: TModelEntity);
    procedure UnitPackageAfterAddChild(Sender: TModelEntity; NewChild: TModelEntity);
    procedure UnitPackageAfterRemove(Sender: TModelEntity);
    procedure UnitPackageAfterEntityChange(Sender: TModelEntity);
//    procedure IAfterUnitPackageListener.Change = UnitPackageAfterChange;
//    procedure IAfterUnitPackageListener.AddChild = UnitPackageAfterAddChild;
//    procedure IAfterUnitPackageListener.Remove = UnitPackageAfterRemove;
//    procedure IAfterUnitPackageListener.EntityChange = UnitPackageAfterEntityChange;
    procedure OnNeedZoomUpdate(Sender : TObject);
  protected
    procedure StoreDiagram; override;
    function FetchDiagram : integer; override;
    function HasChanged : boolean;
    procedure SetVisibilityFilter(const Value: TVisibility); override;
    procedure CurrentEntityChanged; override;
    procedure SetShowAssoc(const Value: boolean); override;
  public
    constructor Create(om: TObjectModel; Parent: TWinControl; Feedback : TEldeanFeedback = nil); override;
    destructor Destroy; override;
    procedure InitFromModel; override;
    procedure PaintTo(Canvas: TCanvas; X, Y: integer; SelectedOnly : boolean); override;
    procedure GetDiagramSize(var W,H : integer); override;
    procedure SetPackage(const Value: TAbstractPackage); override;
    procedure DoLayout; override;
    function GetClickAreas : TStringList; override;
    procedure OpenSelectedPackage;
    procedure DrawZoom(Canvas : TCanvas; W,H : integer); override;
    procedure SetZoomedScroll(ScrollX,ScrollY,W,H : integer); override;
    procedure HideSelectedDiagramElements; override;
    function HasHiddenElements : boolean; override;
    procedure UnHideAllElements; override;
    function GetSelectedRect : TRect; override;
    procedure ScreenCenterEntity(E : TModelEntity); override;
    // Listener methods distributing to other listeners:
    procedure BeforeChange(Sender: TModelEntity); override;
    procedure AfterChange(Sender: TModelEntity); override;
    procedure AfterAddChild(Sender: TModelEntity; NewChild: TModelEntity); override;
    procedure AfterRemove(Sender: TModelEntity); override;
    procedure AfterEntityChange(Sender: TModelEntity); override;
  end;

implementation

uses uRtfdDiagramFrame, Math, LCLIntf, LCLType, uError, SysUtils,
  uIterators, IniFiles, Dialogs, essLayout, uConfig, contnrs, ExtCtrls, uIntegrator;


{ TRtfdDiagram }

constructor TRtfdDiagram.Create(om: TObjectModel; Parent: TWinControl; Feedback : TEldeanFeedback = nil);
begin
  inherited Create(Om, Parent, Feedback);
  Frame := TRtfdDiagramFrame.Create(Parent, Self);
  Frame.Parent := Parent;

  Panel := TessConnectPanel.Create(Parent);
  if not Config.IsLimitedColors then
    Panel.BackBitmap := TRtfdDiagramFrame(Frame).DiaBackImage.Picture.Bitmap;
  Panel.Parent := Frame.ScrollBox;

  //Both these events triggers refresh of zoomimage
  Panel.OnContentChanged := OnNeedZoomUpdate;
  Frame.ScrollBox.OnResize := OnNeedZoomUpdate;

  BoxNames := TStringList.Create;
  BoxNames.CaseSensitive := True;
  BoxNames.Sorted := True;
  BoxNames.Duplicates := dupIgnore;

  Model.AddListener(Self); // IBeforeObjectModelListener()
  ClearDiagram;
end;

destructor TRtfdDiagram.Destroy;
begin
  //Force listeners to release, and diagram to persist.
  Panel.Hide;
  Package := nil;
  ClearDiagram;
  Model.RemoveListener(Self); // IBeforeObjectModelListener(
  FreeAndNil(BoxNames);
  inherited;
end;

procedure TRtfdDiagram.InitFromModel;
var
  Mi : TBaseModelIterator;
  FetchCount : integer;

  procedure InAddUnit(Up: TUnitPackage);
  var
    Mi : TBaseModelIterator;
  begin
    Mi := Up.GetClassifiers;
    while Mi.HasNext do
      AddBox( Mi.Next );
  end;

begin
  IsAllClasses := Package=AllClassesPackage;
  Panel.Hide;
  if not Assigned(FPackage) then
  begin
    Package := Model.ModelRoot;
    //If there is only one package (except unknown) then show it.
    //Assign with Package-property to trigger listeners
    Mi := (FPackage as TLogicPackage).GetPackages;
    if Mi.Count=2 then
    begin
      Mi.Next;
      Package := Mi.Next as TAbstractPackage;
    end;
  end;

  //Clean old
  ClearDiagram;

  //Create boxes
  if FPackage is TUnitPackage then
  begin
    TRtfdUnitPackageDiagram.Create(Panel, FPackage);
    InAddUnit(FPackage as TUnitPackage);
  end
  else
  begin
    //Logic package
    //Exclude unknown-package, otherwise all temp-classes will be included on showallclasses.
    //Also, unkown-package will be shown on package-overview (including docgen)
    if IsAllClasses then
    begin
      //These lines show all members of a package on one diagram
      Mi := TModelIterator.Create( (Model.ModelRoot as TLogicPackage).GetPackages, TEntitySkipFilter.Create(Model.UnknownPackage) );
      while Mi.HasNext do
        InAddUnit( Mi.Next as TUnitPackage )
    end
    else
    begin
      Mi := TModelIterator.Create( (FPackage as TLogicPackage).GetPackages, TEntitySkipFilter.Create(Model.UnknownPackage) );
      while Mi.HasNext do
        AddBox( Mi.Next );
    end;
  end;

  //Fetch layout for this diagram
  FetchCount := FetchDiagram;

  //Create arrow between boxes
  //This must be done after fetchdiagram because Connection-setting might be stored
  ResolveAssociations;

  //Make auto-layout
  if FetchCount=0 then
    DoLayout
  else if FetchCount<BoxNames.Count-2 then
      //if MessageDlg('Model has changed since diagram was saved.'#13'Re-layout?',mtConfirmation,mbOKCancel,0) = mrOk then
      DoLayout
  else
  begin
    with GetStorage(False) do
    begin
      Feedback.Message('Diagram layout and settings was read from file: ' + FileName);
      Free;
    end;
  end;

  Panel.RecalcSize;
  Panel.IsModified := False;

  DoOnUpdateToolbar;
//  DoOnUpdateZoom;
  Panel.Show;
  Panel.SetFocus;
  FHasChanged := False;
end;


procedure TRtfdDiagram.BeforeChange(Sender: TModelEntity);
begin
//  if xxx then
    ModelBeforeChange(Sender);
end;

procedure TRtfdDiagram.AfterChange(Sender: TModelEntity);
begin
//  if xxx then
    ModelAfterChange(Sender);
//  else if xxx then
    UnitPackageAfterChange(Sender);
end;

procedure TRtfdDiagram.AfterAddChild(Sender: TModelEntity;
  NewChild: TModelEntity);
begin
//  if xxx then
    UnitPackageAfterAddChild(Sender, NewChild);
end;

procedure TRtfdDiagram.AfterRemove(Sender: TModelEntity);
begin
//  if xxx then
    UnitPackageAfterRemove(Sender);
end;

procedure TRtfdDiagram.AfterEntityChange(Sender: TModelEntity);
begin
//  if xxx then
    UnitPackageAfterEntityChange(Sender);
end;

procedure TRtfdDiagram.ModelBeforeChange(Sender: TModelEntity);
begin
  ErrorHandler.Trace(Format('ModelBeforeChange: %s', [ClassName]));
  Package := nil;
  IsAllClasses := False;
  ClearDiagram;
end;


procedure TRtfdDiagram.ModelAfterChange(Sender: TModelEntity);
begin
  ErrorHandler.Trace(Format('ModelAfterChange: %s', [ClassName]));
  InitFromModel;
end;


procedure TRtfdDiagram.PaintTo(Canvas: TCanvas; X, Y: integer; SelectedOnly : boolean);
var
  OldBit : Graphics.TBitmap;
begin
  OldBit := Panel.BackBitmap;
  Panel.BackBitmap := nil;
  if SelectedOnly then
  begin
    if (Panel.GetFirstSelected<>nil) then
      Panel.SelectedOnly := True;
  end
  else
    //Selection-markers should not be visible in the saved picture
    Panel.ClearSelection;
  Panel.PaintTo(Canvas.Handle, X, Y);
  Panel.SelectedOnly := False;
  Panel.BackBitmap := OldBit;
end;


procedure TRtfdDiagram.ClearDiagram;
begin
  if not (csDestroying in Panel.ComponentState) then
  begin
    Panel.ClearManagedObjects;
    Panel.DestroyComponents;
  end;
  BoxNames.Clear;
  FHasHidden := False;
  FHasChanged := False;
end;


//Add a 'Box' to the diagram (class/interface/package).
procedure TRtfdDiagram.AddBox(E: TModelEntity);
var
  Mi : TBaseModelIterator;
  Int : TMdlInterface;
  C : TMdlClass;
  A : TAttribute;

  function InCreateBox(E: TModelEntity; BoxT: TRtfdBoxClass): TRtfdBox;
  begin
    Result := BoxT.Create(Panel, E, Frame, VisibilityFilter);
    BoxNames.AddObject(E.FullName, Result);
  end;

begin
  if E is TUnitPackage then
    Panel.AddManagedObject( InCreateBox(E,TRtfdUnitPackage) )
  else if E is TMdlClass then
  //Class
  begin
    //Insert related boxes from other packages
    //This should not be done if IsAllClasses, because then all boxes are inserted anyway
    if not IsAllClasses then
    begin
      //Ancestor that is in another package and that is not already inserted
      //is added to the diagram.
      C := (E as TMdlClass);
      if Assigned(C.Ancestor) and
        (C.Ancestor.Owner<>E.Owner) and
        ( GetBox(C.Ancestor.FullName)=nil ) then
        Panel.AddManagedObject( InCreateBox(C.Ancestor,TRtfdClass) );
      //Implementing interface that is in another package and is not already inserted
      //is added to the diagram.
      Mi := C.GetImplements;
      while Mi.HasNext do
      begin
        Int := Mi.Next as TMdlInterface;
        if (Int.Owner<>E.Owner) and
          ( GetBox( Int.FullName )=nil ) then
          Panel.AddManagedObject( InCreateBox(Int,TRtfdInterface) );
      end;
      //Attribute associations that are in other packages are added
      if ShowAssoc then
      begin
        Mi := C.GetAttributes;
        while Mi.HasNext do
        begin
          A := TAttribute(Mi.Next);
          if Assigned(A.TypeClassifier) and (GetBox(A.TypeClassifier.FullName)=nil) and
            (A.TypeClassifier<>C) and (A.TypeClassifier<>C.Ancestor) and
            (A.TypeClassifier.Owner<>Model.UnknownPackage) then //Avoid getting temp-types from unknown (java 'int' for example)
          begin
            if A.TypeClassifier is TMdlClass then
              Panel.AddManagedObject( InCreateBox(A.TypeClassifier,TRtfdClass) );
            if A.TypeClassifier is TMdlInterface then
              Panel.AddManagedObject( InCreateBox(A.TypeClassifier,TRtfdInterface) );
          end;
        end;
      end;
    end;
    if GetBox(E.FullName)=nil then
      Panel.AddManagedObject( InCreateBox(E,TRtfdClass) );
  end
  else if E is TMdlInterface then
  //Interface
  begin
    //Ancestor that is in another package and that is not already inserted
    //is added to the diagram.
    if (not IsAllClasses) and
      Assigned((E as TMdlInterface).Ancestor) and
      (TMdlInterface(E).Ancestor.Owner<>E.Owner) and
      ( GetBox(TMdlInterface(E).Ancestor.FullName)=nil ) then
      Panel.AddManagedObject( InCreateBox((E as TMdlInterface).Ancestor,TRtfdInterface) );
    if GetBox(E.FullName)=nil then
      Panel.AddManagedObject( InCreateBox(E,TRtfdInterface) );
  end;
end;


//Make arrows between boxes
procedure TRtfdDiagram.ResolveAssociations;
var
  I : integer;
  CBox: TRtfdClass;
  IBox : TRtfdInterface;
  A : TAttribute;

  UBox : TRtfdUnitPackage;
  U : TUnitPackage;
  Dep : TUnitDependency;

  Mi : TBaseModelIterator;
  DestBox: TRtfdBox;
begin
  for I := 0 to BoxNames.Count - 1 do
    if (BoxNames.Objects[I] is TRtfdClass) then
    begin //Class
      CBox := (BoxNames.Objects[I] as TRtfdClass);
      //Ancestor
      if Assigned((CBox.Entity as TMdlClass).Ancestor) then
      begin
        DestBox := GetBox( (CBox.Entity as TMdlClass).Ancestor.FullName );
        if Assigned(DestBox) then
          Panel.ConnectObjects(CBox,DestBox);
      end;
      //Implements
      Mi := (CBox.Entity as TMdlClass).GetImplements;
      while Mi.HasNext do
      begin
        DestBox := GetBox( Mi.Next.FullName );
        if Assigned(DestBox) then
          Panel.ConnectObjects(CBox,DestBox,csThinDash);
      end;
      //Attributes associations
      if ShowAssoc then
      begin
        Mi := (CBox.Entity as TMdlClass).GetAttributes;
        while Mi.HasNext do
        begin
          A := TAttribute(Mi.Next);
          //Avoid arrows that points to themselves, also associations to ancestor (double arrows)
          if Assigned(A.TypeClassifier) and
            (A.TypeClassifier<>CBox.Entity) and
            (A.TypeClassifier<>(CBox.Entity as TMdlClass).Ancestor) then
          begin
            DestBox := GetBox( A.TypeClassifier.FullName );
            //Test for same entity, this will filter out TDatatype that can have same name as a class
            if Assigned(DestBox) and (DestBox.Entity=A.TypeClassifier) then
              Panel.ConnectObjects(CBox,DestBox,csThin,asEmptyOpen);
          end;
        end;
      end;
    end else if (BoxNames.Objects[I] is TRtfdInterface) then
    begin //Interface
      IBox := (BoxNames.Objects[I] as TRtfdInterface);
      //Ancestor
      if Assigned((IBox.Entity as TMdlInterface).Ancestor) then
      begin
        DestBox := GetBox( (IBox.Entity as TMdlInterface).Ancestor.FullName );
        if Assigned(DestBox) then
          Panel.ConnectObjects(IBox,DestBox);
      end;
    end else if (BoxNames.Objects[I] is TRtfdUnitPackage) then
    begin //Unit
      UBox := (BoxNames.Objects[I] as TRtfdUnitPackage);
      U := UBox.Entity as TUnitPackage;
      Mi := U.GetUnitDependencies;
      while Mi.HasNext do
      begin
        Dep := Mi.Next as TUnitDependency;
        if Dep.Visibility=viPublic then
        begin
          DestBox := GetBox( Dep.Package.FullName );
          if Assigned(DestBox) then
            Panel.ConnectObjects(UBox,DestBox,csThinDash,asEmptyOpen);
        end;
      end;
    end;
end;


procedure TRtfdDiagram.SetPackage(const Value: TAbstractPackage);
begin
  if Assigned(FPackage) and HasChanged then
    StoreDiagram;
  if Assigned(FPackage) and (FPackage is TUnitPackage) then
    FPackage.RemoveListener(Self); // IAfterUnitPackageListener(
  inherited SetPackage(Value);
  if Assigned(FPackage) and (FPackage is TUnitPackage) then
    FPackage.AddListener(Self); // IAfterUnitPackageListener(
  if Assigned(Frame.ScrollBox) then
    if not Config.IsTerminating then begin
      Frame.ScrollBox.HorzScrollBar.Position := 0;
      Frame.ScrollBox.VertScrollBar.Position := 0;
    end;
end;


procedure TRtfdDiagram.UnitPackageAfterAddChild(Sender, NewChild: TModelEntity);
begin
  Assert(Assigned(Sender), 'Sender not assigned in TRtfdDiagram.UnitPackageAfterAddChild.');
  ErrorHandler.Trace(Format('UnitPackageAfterAddChild: %s : %s', [ClassName, Sender.Name]));
  if (NewChild is TMdlClass) or (NewChild is TMdlInterface) then
  begin
    AddBox(NewChild);
    ResolveAssociations;
  end;
end;

procedure TRtfdDiagram.UnitPackageAfterChange(Sender: TModelEntity);
var
  s: String;
begin
  s:='';
  if Assigned(Sender) then
    s:=Sender.Name;
  ErrorHandler.Trace(Format('UnitPackageAfterChange: %s : %s', [ClassName, s]));
end;

procedure TRtfdDiagram.UnitPackageAfterEntityChange(Sender: TModelEntity);
begin
  Assert(Assigned(Sender), 'Sender not assigned in TRtfdDiagram.UnitPackageAfterEntityChange.');
  ErrorHandler.Trace(Format('UnitPackageAfterEntityChange: %s : %s', [ClassName, Sender.Name]));
end;

procedure TRtfdDiagram.UnitPackageAfterRemove(Sender: TModelEntity);
begin
  Assert(Assigned(Sender), 'Sender not assigned in TRtfdDiagram.UnitPackageAfterRemove.');
  ErrorHandler.Trace(Format('UnitPackageAfterRemove: %s : %s', [ClassName, Sender.Name]));
end;

procedure TRtfdDiagram.OpenSelectedPackage;
var
  B: TRtfdBox;
begin
  //Anropas av frame action
  B := Panel.GetFirstSelected;
  if Assigned(B) and (B is TRtfdUnitPackage) then
  begin
    Package := (B as TRtfdUnitPackage).P;
    InitFromModel;
    CurrentEntity := Package;
  end;
end;

function TRtfdDiagram.HasChanged: boolean;
begin
  Result := FHasChanged or Panel.IsModified;
end;

procedure TRtfdDiagram.StoreDiagram;
var
  Ini : TCustomIniFile;
  I,OldMode : integer;
  Box : TRtfdBox;
  S : string;
  DoSave : boolean;
begin
  DoSave:=False;
  case Config.DiSave of
    dsAsk : DoSave := MessageDlg('Save changed layout?',mtConfirmation, [mbYes,mbNo] , 0)=mrYes;
    dsNever : ;
    dsAlways : DoSave := True;
  end;
  if DoSave then
  begin
    Ini := GetStorage(True);
    if Assigned(Ini) then
      try
        //Boxes
        for I := 0 to BoxNames.Count - 1 do
        begin
          Box := BoxNames.Objects[I] as TRtfdBox;
          S := 'Box: ' + Package.FullName + ' - ' + Box.Entity.FullName;
          Ini.EraseSection(S);
          Ini.WriteInteger(S,'X', Box.Left);
          Ini.WriteInteger(S,'Y', Box.Top);
          if not Box.Visible then
            Ini.WriteBool(S,'Visible', Box.Visible);
          //Ini.WriteInteger(S,'W', Box.Width);
          //Ini.WriteInteger(S,'H', Box.Height);
        end;

        //Diagram stuff
        S := 'Diagram: ' + Package.FullName;
        Ini.EraseSection(S);
        Ini.WriteInteger(S,'OffsetX',Frame.ScrollBox.VertScrollBar.Position);
        Ini.WriteInteger(S,'OffsetY',Frame.ScrollBox.HorzScrollBar.Position);
        Ini.WriteInteger(S,'Visibility', Integer(VisibilityFilter)  );
        Ini.WriteBool(S,'ShowAssoc', ShowAssoc);

        //Commit
//ToDo!!!        OldMode:=SetErrorMode(SEM_FAILCRITICALERRORS);
        try
          try
            Ini.UpdateFile;
          except
            ErrorHandler.Trace('Could not write layout to disk');
          end;
        finally
//!!!          SetErrorMode(OldMode);
        end;

      finally
        Ini.Free;
      end;
  end;
end;

function TRtfdDiagram.FetchDiagram : integer;
var
  Ini : TCustomIniFile;
  I,NextX,NextY : integer;
  Box : TRtfdBox;
  S : string;
begin
  Result := 0;
  NextX := 50;
  NextY := 50;
  Ini := GetStorage(False);
  if Assigned(Ini) then
    try
      //Boxar
      for I := 0 to BoxNames.Count - 1 do
      begin
        Box := BoxNames.Objects[I] as TRtfdBox;
        S := 'Box: ' + Package.FullName + ' - ' + Box.Entity.FullName;
        if Ini.SectionExists(S) then
        begin
          Inc(Result);
          Box.Left := Ini.ReadInteger(S,'X',Box.Left);
          Box.Top := Ini.ReadInteger(S,'Y',Box.Top);
          Box.Visible := Ini.ReadBool(S,'Visible', True);
          if (not Box.Visible) and (not FHasHidden) then
            FHasHidden := True;
        end
        else
        begin
          //Boxes not in file will get a default postion in upper left corner
          Box.BoundsRect := Rect(NextX, NextY, NextX+Box.Width, NextY+Box.Height);
          Inc(NextX,25);
          Inc(NextY,25);
        end;
      end;

      //Diagram stuff
      S := 'Diagram: ' + Package.FullName;
      if Ini.SectionExists(S) then
      begin
        Frame.ScrollBox.VertScrollBar.Position := Ini.ReadInteger(S,'OffsetX',Frame.ScrollBox.VertScrollBar.Position);
        Frame.ScrollBox.HorzScrollBar.Position := Ini.ReadInteger(S,'OffsetY',Frame.ScrollBox.HorzScrollBar.Position);;
        VisibilityFilter := TVisibility(Ini.ReadInteger(S,'Visibility', Integer( Low(TVisibility) ) ));
        ShowAssoc := Ini.ReadBool(S,'ShowAssoc', ShowAssoc);
      end;

    finally
      Ini.Free;
    end;
end;


procedure TRtfdDiagram.DoLayout;
var
  Layout : TEssLayout;
begin
  if BoxNames.Count>0 then
  begin
    Panel.Hide;
    Layout := TEssLayout.CreateLayout( Panel );
    try
      Layout.Execute;
    finally
      Panel.Show;
      Layout.Free
    end;
    Panel.IsModified := True;
    Panel.RecalcSize;
    Panel.Refresh;
  end;
end;


function TRtfdDiagram.GetBox(const S: string): TRtfdBox;
var
  I : integer;
begin
  I := BoxNames.IndexOf( S );
  if I=-1 then
    Result := nil
  else
    Result := BoxNames.Objects[I] as TRtfdBox;
end;


procedure TRtfdDiagram.SetVisibilityFilter(const Value: TVisibility);
var
  I : integer;
begin
  if Value<>VisibilityFilter then
  begin
    Panel.Hide;
    for I := 0 to BoxNames.Count - 1 do
      (BoxNames.Objects[I] as TRtfdBox).MinVisibility := Value;
    Panel.RecalcSize;
    Panel.Show;
    FHasChanged := True;
    inherited;
  end;
end;


procedure TRtfdDiagram.GetDiagramSize(var W, H: integer);
begin
  W := Panel.Width;
  H := Panel.Height;
end;

//Returns list with str = 'x1,y1,x2,y2', obj = modelentity
function TRtfdDiagram.GetClickAreas: TStringList;
var
  I : integer;
  Box : TRtfdBox;
  S : string;
begin
  Result := TStringList.Create;
  for I := 0 to BoxNames.Count-1 do
  begin
    Box := BoxNames.Objects[I] as TRtfdBox;
    S := IntToStr(Box.Left) + ',' + IntToStr(Box.Top) + ',' +
         IntToStr(Box.Left+Box.Width) + ',' + IntToStr(Box.Top+Box.Height);
    Result.AddObject(S,Box.Entity);
  end;
end;


procedure TRtfdDiagram.HideSelectedDiagramElements;
var
  C: TControl;
  L: TControlList;
  I: integer;
begin
  //Called from frame action
  L := Panel.GetSelectedControls;
  try
    if L.Count>0 then
      for I := 0 to L.Count-1 do
      begin
        C := L[I];
        if (C is TRtfdBox) and Assigned(GetBox( (C as TRtfdBox).Entity.FullName )) then
        begin
          C.Visible := False;
          FHasHidden := True;
          FHasChanged := True;
        end;
      end;
      Panel.ClearSelection;
      Panel.RecalcSize;
      Panel.Refresh;
  finally
    L.Free;
  end;
end;

function TRtfdDiagram.HasHiddenElements: boolean;
begin
  Result := FHasHidden;
end;

procedure TRtfdDiagram.UnHideAllElements;
var
  I : integer;
  Box : TRtfdBox;
begin
  for I := 0 to BoxNames.Count - 1 do
  begin
    Box := BoxNames.Objects[I] as TRtfdBox;
    if not Box.Visible then
      Box.Visible := True;
  end;
  Panel.RecalcSize;
  Panel.Refresh;
  FHasHidden := False;
  FHasChanged := True;
end;

procedure TRtfdDiagram.DrawZoom(Canvas: TCanvas; W,H : integer);
var
  I,ZoomW,ZoomH : integer;
  Box : TRtfdBox;
  ScaleX,ScaleY,Scale : double;
  R : TRect;
begin
  if Panel.Width=0 then
    Exit;
  ScaleX := W / Panel.Width;
  ScaleY := H / Panel.Height;
  Scale := Min(ScaleX,ScaleY);
  //Clear whole area
  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(Rect(0,0,W,H));
  //Fill area for zoomcanvas
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := clBlack;
  ZoomW := Round(Panel.Width * Scale);
  ZoomH := Round(Panel.Height * Scale);
  Canvas.Rectangle( Rect(0,0, ZoomW,ZoomH ) );
  if not Config.IsLimitedColors then
    Canvas.Brush.Color := $EAF4F8
  else
    Canvas.Brush.Color := clWhite;
  //Draw boxes
  for I := 0 to BoxNames.Count-1 do
  begin
    Box := BoxNames.Objects[I] as TRtfdBox;
    if not Box.Visible then
      Continue;
    R := Box.BoundsRect;
    R.Left := Round(R.Left * Scale);
    R.Top := Round(R.Top * Scale);
    R.Right := Round(R.Right * Scale);
    R.Bottom := Round(R.Bottom * Scale);
    Canvas.Rectangle(R);
  end;
  //Draw zoomfocus-box
  ZoomFocusW := Round(Frame.ScrollBox.Width * Scale);
  ZoomFocusH := Round(Frame.ScrollBox.Height * Scale);
  R.Left := Round(Frame.ScrollBox.HorzScrollBar.Position * Scale);
  R.Top := Round(Frame.ScrollBox.VertScrollBar.Position * Scale);
  R.Right := R.Left + ZoomFocusW;
  R.Bottom := R.Top + ZoomFocusH;
  if not ((R.Left=0) and (R.Right>=ZoomW) and (R.Top=0) and (R.Bottom>=ZoomH)) then
  begin
    Canvas.Pen.Mode := pmXor;
    Canvas.Rectangle(R);
    Canvas.Pen.Mode := pmCopy;
  end;
end;


procedure TRtfdDiagram.SetZoomedScroll(ScrollX, ScrollY, W, H: integer);
var
  ScaleX,ScaleY,Scale : double;
begin
  ScaleX := Panel.Width / W;
  ScaleY := Panel.Height / H ;
  Scale := Max(ScaleX,ScaleY);

  //Modify coords to put mousearrow in center of zoomfocus-box
  Dec(ScrollX,ZoomFocusW div 2);
  Dec(ScrollY,ZoomFocusH div 2);

  Frame.ScrollBox.HorzScrollBar.Position := Min(Frame.ScrollBox.HorzScrollBar.Range-
                                                Frame.ScrollBox.Width, Round(ScrollX * Scale));
  Frame.ScrollBox.VertScrollBar.Position := Min(Frame.ScrollBox.VertScrollBar.Range-
                                                Frame.ScrollBox.Height, Round(ScrollY * Scale));
end;

procedure TRtfdDiagram.OnNeedZoomUpdate(Sender: TObject);
begin
  DoOnUpdateZoom;
end;

procedure TRtfdDiagram.CurrentEntityChanged;
var
  P : TModelEntity;
begin
  inherited;
  P := CurrentEntity;
  while Assigned(P) and (not (P is TAbstractPackage)) do
    P := P.Owner;
  if Assigned(P) and (P<>Package) then
  begin
    Package := P as TAbstractPackage;
    InitFromModel
  end;
  if (CurrentEntity is TMdlClass) or (CurrentEntity is TMdlInterface) then
    ScreenCenterEntity(CurrentEntity);
end;

function TRtfdDiagram.GetSelectedRect: TRect;
var
  C: TControl;
  L : TObjectList;
  I : integer;
  R : TRect;
begin
  L := Panel.GetSelectedControls;
  if L.Count=0 then
    Result := Rect(0,0,0,0)
  else
  begin
    Result := Rect(MaxInt,MaxInt,0,0);
    for I := 0 to L.Count-1 do
    begin
      C := TControl(L[I]);
      R := C.BoundsRect;
      if R.Top<Result.Top then
        Result.Top := R.Top;
      if R.Left<Result.Left then
        Result.Left := R.Left;
      if R.Bottom>Result.Bottom then
        Result.Bottom := R.Bottom;
      if R.Right>Result.Right then
        Result.Right := R.Right;
    end;
  end;
  L.Free;
end;


procedure TRtfdDiagram.ScreenCenterEntity(E: TModelEntity);
var
  I : integer;
  Box : TRtfdBox;
begin
  for I := 0 to BoxNames.Count-1 do
    if (BoxNames.Objects[I] as TRtfdBox).Entity=E then
    begin
      Box := BoxNames.Objects[I] as TRtfdBox;
//      Frame.ScrollBox.ScrollInView(Box); // !!! Replaced with AutoScroll.
      Break;
    end;
end;

procedure TRtfdDiagram.SetShowAssoc(const Value: boolean);
begin
  if Value<>ShowAssoc then
    FHasChanged := True;
  inherited;
end;

end.