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

unit uTreeViewIntegrator;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Controls, ComCtrls, contnrs,
  uViewIntegrator, uTreeViewFrame, uModel, uModelEntity, uFeedback;


type
//  TTreeNodeClass = class of TTreeNode;

  TTreeViewIntegrator = class(TViewIntegrator) // , IAfterObjectModelListener
  private
    Frame: TTreeViewFrame;
  protected
    procedure BuildAllClassesView(ATreeRoot: TTreeNode; AEntity: TLogicPackage);
    procedure BuildLogicPackageView(ATreeRoot: TTreeNode; AEntity: TLogicPackage);
    procedure BuildUnitPackageView(ATreeRoot: TTreeNode; AEntity: TUnitPackage);
    procedure BuildClassView(ATreeRoot: TTreeNode; AEntity: TMdlClass);
    procedure BuildInterfaceView(ATreeRoot: TTreeNode; AEntity: TMdlInterface);

    procedure tvModelCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure tvModelChange(Sender: TObject; Node: TTreeNode);
    procedure tvModelAddition(Sender: TObject; Node: TTreeNode);
    procedure CurrentEntityChanged; override;
  public
    constructor Create(om: TObjectModel; Parent: TWinControl; Feedback: IEldeanFeedback = nil); override;
    destructor Destroy; override;
    procedure InitFromModel; override;
    // Listener methods
    procedure BeforeChange(Sender: TModelEntity); override;
    procedure AfterChange(Sender: TModelEntity); override;
  end;

  TViewNode = class(TTreeNode)
  private
    FIsImplementation: Boolean;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function LocateNode(data: Pointer; IsImplState: Boolean): TViewNode;

    property IsImplementation: Boolean read FIsImplementation;
  end;

  { TViewNodeList }

  TViewNodeList = class(TObjectList)
  private
    function GetItems(AIndex: integer): TViewNode;
    procedure SetItems(AIndex: integer; const AValue: TViewNode);
  public
    property Items[AIndex: integer]: TViewNode read GetItems write SetItems; default;
  end;


implementation
uses uIntegrator, uIterators, Classes;

const
  ALL_CLASSES_TEXT: string = 'All classes';
  PACKAGES_TEXT: string = 'Packages';
var
  NodesList: TViewNodeList;

procedure TViewNode.AfterConstruction;
begin
  inherited;
  FIsImplementation := False;
  if not Assigned(NodesList) then
    NodesList := TViewNodeList.Create(False);
  NodesList.Add(Self);
end;

procedure TViewNode.BeforeDestruction;
begin
  inherited;
  NodesList.Remove(Self);
  if NodesList.Count = 0 then
    FreeAndNil(NodesList);
end;

function TViewNode.LocateNode(data: Pointer; IsImplState: Boolean): TViewNode;
var
  i: Integer;
begin
  for i := 0 to NodesList.Count - 1 do
  begin
    Result := NodesList[i];
    if (Result.Data = data) and (Result.IsImplementation = IsImplState) then exit;
  end;
  Result := nil;
end;

{ TTreeViewIntegrator }

procedure TTreeViewIntegrator.tvModelCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TViewNode;
end;

procedure TTreeViewIntegrator.tvModelChange(Sender: TObject; Node: TTreeNode);
var
  chkNode: TViewNode;
begin
  chkNode := (Node as TViewNode);
  if Assigned(chkNode) and (chkNode.IsImplementation) then
  begin
    CurrentEntity := TObject(chkNode.Data) as TModelEntity;
    Exit;
  end
end;

procedure TTreeViewIntegrator.tvModelAddition(Sender: TObject; Node: TTreeNode);
var
  ent: TModelEntity;
  imIndex: Integer;
begin
  if Node.Data = nil then
    Node.ImageIndex := 0
  else begin
    ent := TModelEntity(Node.Data);
    imIndex := 0;
    if ent is TAbstractPackage then
      imIndex := 1
    else if ent is TMdlClass then
      imIndex := 2
    else if ent is TMdlInterface then
      imIndex := 3;
//    Node.ImageIndex := imIndex;
//    Node.SelectedIndex := Node.ImageIndex;
  end;
end;

procedure TTreeViewIntegrator.BuildAllClassesView(ATreeRoot: TTreeNode;
  AEntity: TLogicPackage);
var
  Ci: TBaseModelIterator;
  cent: TModelEntity;
  newRoot: TTreeNode;
begin
  Ci := TModelIterator.Create(Model.ModelRoot.GetAllClassifiers, ioAlpha);
  while Ci.HasNext do
  begin
    cent := Ci.Next;
    if not ((cent is TMdlClass) or (cent is TMdlInterface)) then continue;

    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, cent.Name, cent);
    if cent is TMdlClass then
      BuildClassView(newRoot, cent as TMdlClass)
    else if cent is TMdlInterface then
      BuildInterfaceView(newRoot, cent as TMdlInterface);
  end;
end;

procedure TTreeViewIntegrator.BuildLogicPackageView(ATreeRoot: TTreeNode;
  AEntity: TLogicPackage);
var
  Mi: TBaseModelIterator;
  ent: TModelEntity;
  newRoot: TTreeNode;
begin
  Mi := TModelIterator.Create(AEntity.GetPackages, ioAlpha);
  while Mi.HasNext do
  begin
    ent := Mi.Next;
    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, (ent as TAbstractPackage).Name, ent);
    (newRoot as TViewNode).FIsImplementation := True;
    if ent is TUnitPackage then
      BuildUnitPackageView(newRoot, ent as TUnitPackage)
    else
      BuildLogicPackageView(newRoot, ent as TLogicPackage);
  end;
  ATreeRoot.Expand(False);
end;

procedure TTreeViewIntegrator.BuildUnitPackageView(ATreeRoot: TTreeNode;
  AEntity: TUnitPackage);
var
  Mi: TBaseModelIterator;
  ent: TModelEntity;
  newRoot: TTreeNode;
begin
  Mi := TModelIterator.Create(AEntity.GetUnitDependencies, ioAlpha);
  if Mi.Count > 0 then
  begin
    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, 'dependencies', nil);
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      ATreeRoot.Owner.AddChildObject(newRoot, (ent as TUnitDependency).Package.Name, (ent as TUnitDependency).Package);
    end;
  end;
  Mi := TModelIterator.Create(AEntity.GetClassifiers, ioAlpha);
  while Mi.HasNext do
  begin
    ent := Mi.Next as TClassifier;
    if (ent is TMdlClass) or (ent is TMdlInterface) then
    begin
      newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, ent.Name, ent);
      (newRoot as TViewNode).FIsImplementation := True;
      if ent is TMdlClass then
        BuildClassView(newRoot, ent as TMdlClass)
      else
        BuildInterfaceView(newRoot, ent as TMdlInterface)
    end;
  end;
end;

procedure TTreeViewIntegrator.BuildClassView(ATreeRoot: TTreeNode; AEntity: TMdlClass);
var
  Mi: TBaseModelIterator;
  newRoot: TTreeNode;
  ent: TModelEntity;
begin
  if Assigned(AEntity.Ancestor) then
    ATreeRoot.Owner.AddChildObject(ATreeRoot, 'Ancestor: ' + AEntity.Ancestor.Name, AEntity.Ancestor);
  Mi := TModelIterator.Create(AEntity.GetImplements, ioAlpha);
  if Mi.Count > 0 then
  begin
    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, 'interfaces', nil);
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      ATreeRoot.Owner.AddChildObject(newRoot, ent.Name, ent);
    end;
  end;
  Mi := TModelIterator.Create(AEntity.GetDescendants, ioAlpha);
  if Mi.Count > 0 then
  begin
    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, 'subclasses', nil);
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      ATreeRoot.Owner.AddChildObject(newRoot, ent.Name, ent);
    end;
  end;
end;

procedure TTreeViewIntegrator.BuildInterfaceView(ATreeRoot: TTreeNode; AEntity: TMdlInterface);
var
  Mi: TBaseModelIterator;
  newRoot: TTreeNode;
  ent: TModelEntity;
begin
  if Assigned(AEntity.Ancestor) then
    ATreeRoot.Owner.AddChildObject(ATreeRoot, 'Ancestor: ' + AEntity.Ancestor.Name, AEntity.Ancestor);
  Mi := TModelIterator.Create(AEntity.GetImplementingClasses, ioAlpha);
  if Mi.Count > 0 then
  begin
    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, 'implementors', nil);
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      ATreeRoot.Owner.AddChildObject(newRoot, ent.Name, ent);
    end;
  end;
end;

constructor TTreeViewIntegrator.Create(om: TObjectModel;
  Parent: TWinControl; Feedback: IEldeanFeedback);
begin
  inherited Create(Om, Parent, Feedback);
  Frame := TTreeViewFrame.Create(Parent);
  Frame.Parent := Parent;
  Model.AddListener(Self); // IAfterObjectModelListener()

  Frame.tvModel.OnCreateNodeClass := tvModelCreateNodeClass; // Frame.tvModel.OnCustomCreateItem := tvModelCreateItem;
  Frame.tvModel.OnChange := tvModelChange;
  Frame.tvModel.OnAddition := tvModelAddition;
end;

destructor TTreeViewIntegrator.Destroy;
begin
  Model.RemoveListener(Self); // IAfterObjectModelListener()
  inherited;
end;

procedure TTreeViewIntegrator.InitFromModel;
var
  node: TViewNode;
begin
  Frame.tvModel.Items.Clear;
//  BuildUnitPackageView(Frame.tvModel.Items.Add(nil,'Unknown'),Model.UnknownPackage);
  node := Frame.tvModel.Items.AddObject(nil, PACKAGES_TEXT, Model.ModelRoot) as TViewNode;
  node.FIsImplementation := True;
  BuildLogicPackageView(node, Model.ModelRoot);

  node := Frame.tvModel.Items.AddObject(nil, ALL_CLASSES_TEXT, AllClassesPackage) as TViewNode;
  node.FIsImplementation := True;
  BuildAllClassesView(node, nil);
end;

procedure TTreeViewIntegrator.CurrentEntityChanged;
begin
  inherited;
  Frame.tvModel.Selected := TViewNode(Frame.tvModel.Items.GetFirstNode).LocateNode(CurrentEntity, True);
end;


procedure TTreeViewIntegrator.BeforeChange(Sender: TModelEntity);
begin
  InitFromModel;
end;

procedure TTreeViewIntegrator.AfterChange(Sender: TModelEntity);
begin
  InitFromModel;
end;

{ TViewNodeList }

function TViewNodeList.GetItems(AIndex: integer): TViewNode;
begin
  Result := (inherited Items[AIndex]) as TViewNode;
end;

procedure TViewNodeList.SetItems(AIndex: integer; const AValue: TViewNode);
begin
  Items[AIndex] := AValue;
end;

end.
