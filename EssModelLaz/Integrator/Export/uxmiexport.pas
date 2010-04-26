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

{
  XMI-export.
  See UML-specen page 591 for a description of XMI mapping of UML
}
unit uXmiExport;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses uIntegrator, uModelEntity, Classes, uModel, uFeedback;

type
  TXMIExporter = class(TExportIntegrator)
  private
    Ids,
    LaterList : TStringList;
    Output : TMemoryStream;
    NextId : integer;
    Feedback : IEldeanFeedback;
    procedure WritePackage(P : TAbstractPackage);
    procedure WriteLogicPackage(L : TLogicPackage);
    procedure WriteUnitPackage(U : TUnitPackage);
    procedure WriteClass(C : TClass);
    procedure WriteInterface(I : TInterface);
    procedure WriteEntityHeader(E : TModelEntity; const XmiName : string);
    procedure WriteFeatures(C : TClassifier);
    procedure WriteDataType(T : TDataType);
    function MakeTypeRef(C : TClassifier) : string;
    procedure FlushLaterList;
    procedure MakeGeneral(Child,Parent : TClassifier);
    procedure MakeAbstract(Client,Supplier : TClassifier);
    function MakeId(const S : string) : string;
    function XmlClose(const S : string) : string;
    function XmlOpen(const S : string) : string;
    function Xml(const S : string) : string;
    procedure Write(const S : string);
  public
    constructor Create(om: TObjectModel; Feedback : IEldeanFeedback = nil); reintroduce;
    destructor Destroy; override;
    procedure InitFromModel; override;
    procedure ShowSaveDialog;
    procedure SaveTo(const FileName : string);
    function GetXmi : string;
    function GetXMIId(E : TModelEntity) : string;
  end;

implementation
uses SysUtils, uIterators, Dialogs, uConst;

const
  Core = 'Foundation.Core.';
  CoreModelElement = Core + 'ModelElement.';

  XmiHeader =
'<?xml version="1.0" encoding="UTF-8"?>'#13#10 +
'<XMI xmi.version="1.0">'#13#10 +
'<XMI.header>'#13#10 +
'<XMI.metamodel xmi.name="UML" xmi.version="1.3"/>'#13#10 +
'<XMI.documentation>'#13#10 +
'<XMI.exporter>' + uConst.ProgName + '</XMI.exporter>'#13#10 +
'<XMI.exporterVersion>' + uConst.ProgVersion + '</XMI.exporterVersion>'#13#10 +
'</XMI.documentation>'#13#10 +
'</XMI.header>'#13#10 +
'<XMI.content>';

  XmiFooter =
'</XMI.content>'#13#10 +
'</XMI>';


{ TXMIExporter }

constructor TXMIExporter.Create(om: TObjectModel; Feedback : IEldeanFeedback = nil);
begin
  inherited Create(om);
  Output := TMemoryStream.Create;
  LaterList := TStringList.Create;
  Ids := TStringList.Create;
  Ids.Sorted := True;
  Ids.Duplicates := dupIgnore;
  NextId := 0;
  Self.Feedback := Feedback;
  if Feedback=nil then
    Self.Feedback := NilFeedback
end;

destructor TXMIExporter.Destroy;
begin
  FreeAndNil(Output);
  FreeAndNil(Ids);
  FreeAndNil(LaterList);
  inherited;
end;

procedure TXMIExporter.InitFromModel;
begin
  Write(XmiHeader);
  Write( XmlOpen('Model_Management.Model') );
  WritePackage(Model.ModelRoot);
  Write( XmlClose('Model_Management.Model') );
  Write(XmiFooter);
  Feedback.Message('XMI finished.');
end;

function TXMIExporter.MakeId(const S : string): string;
var
  I : integer;
begin
  I := Ids.IndexOf(S);
  if I=-1 then
  begin
    Inc(NextId);
    I := Ids.AddObject(S,pointer(NextId));
  end;
  Result := 'xmi_' + IntToStr( integer(Ids.Objects[ I ]) );
end;


procedure TXMIExporter.ShowSaveDialog;
var
  D : TSaveDialog;
  Dir : string;
begin
  D := TSaveDialog.Create(nil);
  try
    Dir := ExtractFilePath( Model.ModelRoot.GetConfigFile );
    D.DefaultExt := 'xmi';
    D.InitialDir := Dir;
    D.Filter := 'Xmi files (*.xmi)|*.xmi|All files (*.*)|*.*';
    D.Options := D.Options + [ofOverwritePrompt];
    if D.Execute then
      SaveTo( D.FileName );
  finally
    D.Free;
  end;
end;

procedure TXMIExporter.Write(const S: string);
begin
  Output.Write(S[1],Length(S));
  Output.Write(#13#10,2);
end;


procedure TXMIExporter.WriteClass(C: TClass);
var
  Mi : TBaseModelIterator;
begin
  WriteEntityHeader(C, Core + 'Class');

  WriteFeatures(C);

  if Assigned(C.Ancestor) then
  begin
    Write( XmlOpen( Core + 'GeneralizableElement.generalization') );
    MakeGeneral(C,C.Ancestor);
    Write( XmlClose( Core + 'GeneralizableElement.generalization') );
  end;

  //Implements
  Mi := C.GetImplements;
  if Mi.HasNext then
  begin
    Write( XmlOpen( CoreModelElement + 'clientDependency') );
    while Mi.HasNext do
      MakeAbstract(C, Mi.Next as TClassifier);
    Write( XmlClose( CoreModelElement + 'clientDependency') );
  end;

  Mi := C.GetDescendants;
  if Mi.HasNext then
  begin
    Write( XmlOpen( Core + 'GeneralizableElement.specialization') );
    while Mi.HasNext do
      MakeGeneral( Mi.Next as TClassifier, C);
    Write( XmlClose( Core + 'GeneralizableElement.specialization') );
  end;

  Write( XmlClose(Core + 'Class') );
end;



procedure TXMIExporter.WriteFeatures(C: TClassifier);
var
  Mi : TBaseModelIterator;
  F : TModelEntity;

  procedure WriteAttribute(A : TAttribute);
  begin
    WriteEntityHeader(A, Core + 'Attribute');
    if Assigned(A.TypeClassifier) then
    begin
      Write( XmlOpen(Core + 'StructuralFeature.type') );
        Write( MakeTypeRef(A.TypeClassifier) );
      Write( XmlClose(Core + 'StructuralFeature.type') );
    end;
    Write( XmlClose(Core + 'Attribute') );
  end;

  procedure WriteOperation(O : TOperation);
  var
    Mio : TBaseModelIterator;
    P : TParameter;
  begin
    WriteEntityHeader(O, Core + 'Operation');
      Write( XmlOpen(Core + 'BehavioralFeature.parameter') );
      if Assigned(O.ReturnValue) then
      begin
        Write( XmlOpen(Core + 'Parameter') );
        Write( '<' + Core + 'Parameter.kind xmi.value="return"/>');
        Write( XmlOpen(Core + 'Parameter.type') );
          Write( MakeTypeRef( O.ReturnValue ) );
        Write( XmlClose(Core + 'Parameter.type') );
        Write( XmlClose(Core + 'Parameter') );
      end;
      Mio := O.GetParameters;
      while Mio.HasNext do
      begin
        P := Mio.Next as TParameter;
        WriteEntityHeader(P, Core + 'Parameter');
        if Assigned(P.TypeClassifier) then
        begin
          Write( XmlOpen(Core + 'Parameter.type') );
            Write( MakeTypeRef(P.TypeClassifier) );
          Write( XmlClose(Core + 'Parameter.type') );
        end;
        Write( XmlClose(Core + 'Parameter') );
      end;
      Write( XmlClose(Core + 'BehavioralFeature.parameter') );
    Write( XmlClose(Core + 'Operation') );
  end;

begin
  Mi := C.GetFeatures;
  if Mi.HasNext then
  begin
    Write( XmlOpen(Core + 'Classifier.feature') );
    while Mi.HasNext do
    begin
      F := Mi.Next;
      if F is TAttribute then
        WriteAttribute(F as TAttribute)
      else if F is TOperation then
        WriteOperation(F as TOperation);
    end;
    Write( XmlClose(Core + 'Classifier.feature') );
  end;
end;


procedure TXMIExporter.WriteEntityHeader(E: TModelEntity; const XmiName: string);
const
  VisibilityMap: array[TVisibility] of string = ('private', 'protected', 'public', 'public');
  //(viPrivate,viProtected,viPublic,viPublished);
begin
{
  <Foundation.Core.Attribute xmi.id="xmi.3">
    <Foundation.Core.ModelElement.name>x</Foundation.Core.ModelElement.name>
    <Foundation.Core.ModelElement.visibility xmi.value="private"/>
}
  Write( '<' + XmiName + ' xmi.id="' + MakeId(E.FullName) + '">' );
  Write( XmlOpen(CoreModelElement + 'name') + Xml(E.Name) + XmlClose(CoreModelElement + 'name') );
  Write( '<' + CoreModelElement + 'visibility xmi.value="' + VisibilityMap[E.Visibility] + '"/>');
end;



procedure TXMIExporter.WritePackage(P: TAbstractPackage);
begin
  Feedback.Message('XMI generating package ' + P.Name + '...');
  WriteEntityHeader(P,'Model_Management.Package');
    if P is TLogicPackage then
      WriteLogicPackage(P as TLogicPackage)
    else if P is TUnitPackage then
      WriteUnitPackage(P as TUnitPackage);
    //Laterlist contains generalizations etc that belongs to this package
    FlushLaterList;
  Write( XmlClose('Model_Management.Package') );
end;


procedure TXMIExporter.WriteLogicPackage(L: TLogicPackage);
var
  Mi : TBaseModelIterator;
begin
  Mi := L.GetPackages;
  while Mi.HasNext do
    WritePackage( Mi.Next as TAbstractPackage );
end;


procedure TXMIExporter.WriteUnitPackage(U: TUnitPackage);
var
  Mi : TBaseModelIterator;
  C : TModelEntity;
begin
  Mi := U.GetClassifiers;
  while Mi.HasNext do
  begin
    C := Mi.Next;
    if C is TClass then
      WriteClass(C as TClass)
    else if C is TInterface then
      WriteInterface(C as TInterface)
    else if C is TDataType then
      WriteDataType(C as TDataType);
  end;
end;

function TXMIExporter.XmlClose(const S: string): string;
begin
  Result := '</' + S + '>';
end;

function TXMIExporter.XmlOpen(const S: string): string;
begin
  Result := '<' + S + '>';
end;


//Writes a reference to a classifier
function TXMIExporter.MakeTypeRef(C: TClassifier) : string;
var
  S : string;
begin
  if C is TClass then
    S := 'Class'
  else if C is TDataType then
    S := 'DataType'
  else if C is TInterface then
    S := 'Interface';
  Result := '<' + Core + S +' xmi.idref="' + MakeId(C.FullName) + '"/>';
end;


//Check that string does not contain xml-chars like < and >
function TXMIExporter.Xml(const S: string): string;
var
  I : integer;
begin
  Result := S;
  for I:=1 to Length(Result) do
    case Result[I] of
      '<' : Result[I]:='(';
      '>' : Result[I]:=')';
    end;
end;

procedure TXMIExporter.WriteInterface(I: TInterface);
{
          <Foundation.Core.ModelElement.supplierDependency>
            <Foundation.Core.Abstraction xmi.idref="xmi.37"/>
          </Foundation.Core.ModelElement.supplierDependency>
}
var
  Mi : TBaseModelIterator;
begin
  WriteEntityHeader(I, Core + 'Interface');
  WriteFeatures(I);

  //Implementing classes
  Mi := I.GetImplementingClasses;
  if Mi.HasNext then
  begin
    Write( XmlOpen( CoreModelElement + 'supplierDependency') );
    while Mi.HasNext do
      MakeAbstract(Mi.Next as TClassifier,I);
    Write( XmlClose( CoreModelElement + 'supplierDependency') );
  end;

  Write( XmlClose(Core + 'Interface') );
end;

procedure TXMIExporter.WriteDataType(T: TDataType);
begin
  WriteEntityHeader(T, Core + 'DataType');
  Write( XmlClose(Core + 'DataType') );
end;

procedure TXMIExporter.FlushLaterList;
var
  I : integer;
begin
  for I := 0 to LaterList.Count-1 do
    Write(LaterList[I]);
  LaterList.Clear;
end;


//Creates a reference to a generalization.
//Also create the generalization if it did not already exist.
procedure TXMIExporter.MakeGeneral(Child, Parent: TClassifier);
var
  ID,S : string;
begin
{
      <Foundation.Core.Generalization xmi.id="xmi.12">
        <Foundation.Core.Generalization.child>
          <Foundation.Core.Class xmi.idref="xmi.11"/>
        </Foundation.Core.Generalization.child>
        <Foundation.Core.Generalization.parent>
          <Foundation.Core.Class xmi.idref="xmi.36"/>
        </Foundation.Core.Generalization.parent>
      </Foundation.Core.Generalization>
}
  S := 'General ' + Child.FullName + ' - ' + Parent.FullName;
  if Ids.IndexOf(S)=-1 then
  begin
    //Crate generalization
    ID := MakeId(S);
    LaterList.Add( '<' + Core + 'Generalization xmi.id="' + ID + '">');
      LaterList.Add( XmlOpen(Core + 'Generalization.child') );
      LaterList.Add( MakeTypeRef(Child) );
      LaterList.Add( XmlClose(Core + 'Generalization.child') );
      LaterList.Add( XmlOpen(Core + 'Generalization.parent') );
      LaterList.Add( MakeTypeRef(Parent) );
      LaterList.Add( XmlClose(Core + 'Generalization.parent') );
    LaterList.Add( XmlClose(Core + 'Generalization') );
  end
  else
    ID := MakeId(S);
  //Write reference
  Write( '<' + Core + 'Generalization xmi.idref="' + ID + '"/>');
end;


//Creates a reference to an Abstraction.
//Also create the Abstraction if it did not already exist.
procedure TXMIExporter.MakeAbstract(Client, Supplier: TClassifier);
{
        <Foundation.Core.Abstraction xmi.id="xmi.37">
          <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
          <Foundation.Core.ModelElement.namespace>
            <Model_Management.Model xmi.idref="xmi.1"/>
          </Foundation.Core.ModelElement.namespace>
          <Foundation.Core.Dependency.client>
            <Foundation.Core.Class xmi.idref="xmi.36"/>
          </Foundation.Core.Dependency.client>
          <Foundation.Core.Dependency.supplier>
            <Foundation.Core.Interface xmi.idref="xmi.47"/>
          </Foundation.Core.Dependency.supplier>
        </Foundation.Core.Abstraction>
}
var
  ID,S : string;
begin
  S := 'Abstract ' + Client.FullName + ' - ' + Supplier.FullName;
  if Ids.IndexOf(S)=-1 then
  begin
    //Create the Abstraction
    ID := MakeId(S);
    LaterList.Add( '<' + Core + 'Abstraction xmi.id="' + ID + '">');
      LaterList.Add( XmlOpen(Core + 'Dependency.client') );
      LaterList.Add( MakeTypeRef(Client) );
      LaterList.Add( XmlClose(Core + 'Dependency.client') );
      LaterList.Add( XmlOpen(Core + 'Dependency.supplier') );
      LaterList.Add( MakeTypeRef(Supplier) );
      LaterList.Add( XmlClose(Core + 'Dependency.supplier') );
    LaterList.Add( XmlClose(Core + 'Abstraction') );
  end
  else
    ID := MakeId(S);
  //Write reference
  Write( '<' + Core + 'Abstraction xmi.idref="' + ID + '"/>');
end;


procedure TXMIExporter.SaveTo(const FileName: string);
var
  F : TFileStream;
begin
  F := TFileStream.Create( FileName ,fmCreate);
  try
    F.CopyFrom(Output, 0);
  finally
    F.Free;
  end;
end;

//Returns the whole xmi-file as a string.
function TXMIExporter.GetXmi: string;
begin
  SetLength(Result,Output.Size);
  Move(Output.Memory^,Result[1],Output.Size);
end;

//Used by htmldoc to get id of packages.
function TXMIExporter.GetXMIId(E: TModelEntity): string;
begin
  Result := MakeID(E.FullName);
end;


end.