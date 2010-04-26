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
  See UML-spec page 591 for a description of XMI mapping of UML

  This unit makes an xmi-representation that is compatible with Argo UML v0.10
  Code contributed by Fernando Montenegro.
  Build the project with ARGO_XMI defined to use this unit.
}
unit uXmiExportArgoUML;

interface

uses uIntegrator, uModelEntity, Classes, uModel, uFeedback;

type

  TTypeClassifier = (tcAll, tcDataType, tcNotDataType);

  TXMIExporterArgoUML = class(TExportIntegrator)
  private
    Ids,
    LaterList : TStringList;
    IdModel : string;
    Output : TMemoryStream;
    NextId : integer;
    Feedback : IEldeanFeedback;
    //Fernando
    bolWritePackage : boolean;
    dataTypeVoid : TDataType;
    procedure WritePackage(P : TAbstractPackage; TC:TTypeClassifier = tcAll;IdPai : string='');
    procedure WriteLogicPackage(L : TLogicPackage; TC:TTypeClassifier = tcAll;IdPai : string='');
    procedure WriteUnitPackage(U : TUnitPackage; TC:TTypeClassifier = tcAll;IdPai : string='');
    procedure WriteClass(C : TClass;IdPai : string='');
    procedure WriteInterface(I : TInterface;IdPai : string='');
//Fernando    procedure WriteEntityHeader(E : TModelEntity; const XmiName : string);
    function WriteEntityHeader(E : TModelEntity; const XmiName : string) : string;
    procedure WriteFeatures(C : TClassifier; pID: string);
    procedure WriteDataType(T : TDataType;IdPai : string='');
    function MakeTypeRef(C : TClassifier) : string;
    procedure FlushLaterList;
    procedure MakeGeneral(Child,Parent : TClassifier;IdPai : string);
    procedure MakeAbstract(Client,Supplier : TClassifier);
    function MakeId(const S : string) : string;
    function XmlClose(const S : string) : string;
    function XmlOpen(const S : string) : string;
    function Xml(const S : string) : string;
    procedure Write(const S : string);
    function getNamespace(IdPai : string=''): string;
    procedure complementoHeader(xmiCore: string; IdPai : string='';
                                          bolPrintNameSpace : boolean = True);
    function xmiUUID(Id: string): string;
  public
    constructor Create(om: TObjectModel; Feedback : IEldeanFeedback = nil); reintroduce;
    destructor Destroy; override;
    procedure InitFromModel; override;
    procedure ShowSaveDialog;
    procedure SaveTo(const FileName : string);
    function GetXmi : string;
    function GetXMIId(E : TModelEntity) : string;
    procedure SetbolWritePackage(valor : boolean);
  end;


implementation
uses SysUtils, uIterators, Dialogs, uConst;

const
  Core = 'Foundation.Core.';
  CoreModelElement = Core + 'ModelElement.';
//begin Fernando Montenegro
  CoreModelGeneralizableElement = Core + 'GeneralizableElement.';

  headerCoreClass = 'Class';
  headerCoreAttribute = 'Attribute';
  headerCoreOperation = 'Operation';
  headerCoreParameter = 'Parameter';
  headerCorePackage = 'Model_Management.Package';
  headerCoreInterface = 'Interface';
  headerCoreDataType = 'DataType';

//end Fernando Montenegro

  XmiHeader =
'<?xml version="1.0" encoding="UTF-8"?>'#13#10 +
'<XMI xmi.version="1.0">'#13#10 +
'<XMI.header>'#13#10 +
'<XMI.documentation>'#13#10 +
'<XMI.exporter>' + uConst.ProgName + '</XMI.exporter>'#13#10 +
'<XMI.exporterVersion>' + uConst.ProgVersion + '</XMI.exporterVersion>'#13#10 +
'</XMI.documentation>'#13#10 +
'<XMI.metamodel xmi.name="UML" xmi.version="1.3"/>'#13#10 +
'</XMI.header>'#13#10 +
'<XMI.content>';

  XmiFooter =
'</XMI.content>'#13#10 +
'</XMI>';


{ TXMIExporterArgoUML }

constructor TXMIExporterArgoUML.Create(om: TObjectModel; Feedback : IEldeanFeedback = nil);
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

destructor TXMIExporterArgoUML.Destroy;
begin
  FreeAndNil(Output);
  FreeAndNil(Ids);
  FreeAndNil(LaterList);
  inherited;
end;

procedure TXMIExporterArgoUML.InitFromModel;
//begin Fernando Montenegro
//inclusão do ID de modelo
const
  S : string = 'Model_Management.Model';
var
  lbolWritePackage : boolean;
begin
  IdModel := MakeId(S);
  Write(XmiHeader);

  Write( XmlOpen(S + ' xmi.id="' + IdModel + '" xmi.uuid="'+
        xmiUUID(IdModel) + '"'));

  Write( XmlOpen(CoreModelElement + 'name') + Xml('Modelo') + XmlClose(CoreModelElement + 'name') );
  Write( '<' + CoreModelElement + 'isSpecification xmi.value="false" />');
  complementoHeader(CoreModelGeneralizableElement, '',false);

  Write( XmlOpen(Core + 'Namespace.ownedElement'));
  lbolWritePackage := bolWritePackage;
  bolWritePackage := false;
  WritePackage(Model.ModelRoot, tcDataType );
  bolWritePackage := lbolWritePackage;
  WritePackage(Model.ModelRoot, tcNotDataType);
  Write( XmlClose(Core + 'Namespace.ownedElement'));

  Write( XmlClose(S) );
  Write(XmiFooter);
  Feedback.Message('XMI finished.');
end;

function TXMIExporterArgoUML.MakeId(const S : string): string;
var
  I : integer;
begin
  I := Ids.IndexOf(S);
  if I=-1 then
  begin
    Inc(NextId);
    I := Ids.AddObject(S,pointer(NextId));
  end;
//begin Fernando Montenegro
  Result := 'xmi.' + IntToStr( integer(Ids.Objects[ I ]) );
//  Result := 'xmi_' + IntToStr( integer(Ids.Objects[ I ]) );
//end Fernando Montenegro
end;


procedure TXMIExporterArgoUML.ShowSaveDialog;
var
  D : TSaveDialog;
  Dir : string;
begin
  D := TSaveDialog.Create(nil);
  try
    Dir := ExtractFilePath( Model.ModelRoot.GetConfigFile );
    D.InitialDir := Dir;
    D.DefaultExt := 'xmi';
    D.Filter := 'Xmi files (*.xmi)|*.xmi|All files (*.*)|*.*';
    D.Options := D.Options + [ofOverwritePrompt];
    if D.Execute then
      SaveTo( D.FileName );
  finally
    D.Free;
  end;
end;

procedure TXMIExporterArgoUML.Write(const S: string);
begin
  Output.Write(S[1],Length(S));
  Output.Write(#13#10,2);
end;


procedure TXMIExporterArgoUML.WriteClass(C: TClass;IdPai : string);
var
  ID : string;
  Mi : TBaseModelIterator;
begin
  ID := WriteEntityHeader(C, headerCoreClass);
  Write( '<' + Core + headerCoreClass + '.isActive xmi.value="false" />');
  complementoHeader(CoreModelGeneralizableElement,IdPai);
  WriteFeatures(C, ID);

  if Assigned(C.Ancestor) then
  begin
    Write( XmlOpen( CoreModelGeneralizableElement + 'generalization') );
    MakeGeneral(C,C.Ancestor, IdPai);
    Write( XmlClose( CoreModelGeneralizableElement + 'generalization') );
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
    Write( XmlOpen( CoreModelGeneralizableElement + 'specialization') );
    while Mi.HasNext do
      MakeGeneral( Mi.Next as TClassifier, C, IdPai);
    Write( XmlClose( CoreModelGeneralizableElement + 'specialization') );
  end;

  Write( XmlClose(Core + headerCoreClass) );
end;



procedure TXMIExporterArgoUML.WriteFeatures(C: TClassifier; pID: string);
var
  Mi : TBaseModelIterator;
  F : TModelEntity;

  procedure WriteFeatureOwner(pID: string);
  begin
    Write( XmlOpen(Core + 'Feature.owner') );
    Write( '<' + Core + 'Classifier xmi.idref="'+ pID +'"/>');
    Write( XmlClose(Core + 'Feature.owner') );
  end;

  procedure WriteAttribute(A : TAttribute; pID: string);
  begin
    WriteEntityHeader(A, headerCoreAttribute);
    Write ( getNamespace());
    WriteFeatureOwner(pID);
    if Assigned(A.TypeClassifier) then
    begin
      Write( XmlOpen(Core + 'StructuralFeature.type') );
        Write( MakeTypeRef(A.TypeClassifier) );
      Write( XmlClose(Core + 'StructuralFeature.type') );
    end;
    Write( XmlClose(Core + headerCoreAttribute) );
  end;

  procedure WriteOperation(O : TOperation; pID: string);
  var
    Mio : TBaseModelIterator;
    P : TParameter;
    IdO : string;
  begin
    IdO := WriteEntityHeader(O, headerCoreOperation);
    WriteFeatureOwner(pID);
    Write ('<' + Core + 'BehavioralFeature.isQuery xmi.value="false"/>');
    complementoHeader(Core + headerCoreOperation +'.');
      Write( XmlOpen(Core + 'BehavioralFeature.parameter') );
      Write( XmlOpen(Core + headerCoreParameter) );
      Write( '<' + Core + headerCoreParameter + '.kind xmi.value="return"/>');
      Write( XmlOpen(Core + 'Parameter.behavioralFeature'));
         Write( '<' + Core + 'BehavioralFeature xmi.idref="' + IdO +'"/>');
      Write( XmlClose(Core + 'Parameter.behavioralFeature'));
      Write( XmlOpen(Core + headerCoreParameter + '.type') );
      if Assigned(O.ReturnValue) then
         Write( MakeTypeRef( O.ReturnValue ) )
      else
        //changed C and Java compability
         Write( MakeTypeRef(dataTypeVoid));
//         Write( '<' + Core + headerCoreDataType +' xmi.idref="' + MakeId('void') + '"/>');
      Write( XmlClose(Core + headerCoreParameter + '.type') );
      Write( XmlClose(Core + headerCoreParameter) );


      Mio := O.GetParameters;
      while Mio.HasNext do
      begin
        P := Mio.Next as TParameter;
        WriteEntityHeader(P, headerCoreParameter);
        if Assigned(P.TypeClassifier) then
        begin
          Write( '<' + Core + headerCoreParameter + '.kind xmi.value="in"/>');
          Write( XmlOpen(Core + 'Parameter.behavioralFeature'));
             Write( '<' + Core + 'BehavioralFeature xmi.idref="' + IdO +'"/>');
          Write( XmlClose(Core + 'Parameter.behavioralFeature'));
          Write( XmlOpen(Core + headerCoreParameter + '.type') );
            Write( MakeTypeRef(P.TypeClassifier) );
          Write( XmlClose(Core + headerCoreParameter + '.type') );
        end;
        Write( XmlClose(Core + headerCoreParameter) );
      end;
      Write( XmlClose(Core + 'BehavioralFeature.parameter') );
    Write( XmlClose(Core + headerCoreOperation) );
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
        WriteAttribute(F as TAttribute, pId)
      else if F is TOperation then
        WriteOperation(F as TOperation, pId);
    end;
    Write( XmlClose(Core + 'Classifier.feature') );
  end;
end;


function TXMIExporterArgoUML.WriteEntityHeader(E: TModelEntity; const XmiName: string) : string;
const
  VisibilityMap: array[TVisibility] of string = ('private', 'protected', 'public', 'public');
  //(viPrivate,viProtected,viPublic,viPublished);
var
 ID, lXmiName: string;

begin
{
  <Foundation.Core.Attribute xmi.id="xmi.3">
    <Foundation.Core.ModelElement.name>x</Foundation.Core.ModelElement.name>
    <Foundation.Core.ModelElement.visibility xmi.value="private"/>
}
  ID := MakeId(XmiName+'_'+E.FullName);
  if XmiName <> headerCorePackage then
    lXmiName :=  Core + XmiName
  else
    lXmiName :=  XmiName;
  Write( '<' + lXmiName + ' xmi.id="' + ID + '"');
  if (XmiName <> headerCoreDataType) then
    Write(' xmi.uuid="'+ xmiUUID(ID) + '"');
  Write('>');

  if E.Name <> '' then
    Write( XmlOpen(CoreModelElement + 'name') + Xml(E.Name) + XmlClose(CoreModelElement + 'name') )
  else
    if (XmiName = headerCorePackage) then
      Write( XmlOpen(CoreModelElement + 'name') + Xml('Pacote'+ID) + XmlClose(CoreModelElement + 'name') );


//begin Fernando Montenegro
  if (XmiName = headerCoreAttribute) OR (XmiName = headerCoreOperation) then
    Write( '<' + CoreModelElement + 'visibility xmi.value="' + VisibilityMap[E.Visibility] + '"/>');

  Write( '<' + CoreModelElement + 'isSpecification xmi.value="false" />');

  result := ID;
//end Fernando Montenegro
end;

procedure TXMIExporterArgoUML.complementoHeader(xmiCore : string; IdPai : string;
                                          bolPrintNameSpace : boolean);
begin
  Write( '<' + xmiCore + 'isRoot xmi.value="false" />');
  Write( '<' + xmiCore + 'isLeaf xmi.value="false" />');
  Write( '<' + xmiCore + 'isAbstract xmi.value="false" />');
  if bolPrintNameSpace then
    Write(getNamespace(IdPai));
end;

procedure TXMIExporterArgoUML.WritePackage(P: TAbstractPackage;
                                       TC:TTypeClassifier; IdPai : string);
var
  lId : string;
begin
  Feedback.Message('XMI generating package ' + P.Name + '...');
  if bolWritePackage  then
   begin
    lId := WriteEntityHeader(P,headerCorePackage);
    complementoHeader(CoreModelGeneralizableElement, IdPai,(IdPai <> lId) );
   end
  else
   lId := IdPai;

  if (TC <> tcDataType)  and (bolWritePackage) then
    Write( XmlOpen(Core + 'Namespace.ownedElement'));

  if P is TLogicPackage then
    WriteLogicPackage(P as TLogicPackage, TC, lId)
  else
    if P is TUnitPackage then
       WriteUnitPackage(P as TUnitPackage, TC, lId);
     //Laterlist contains generalizations etc that belongs to this package
  FlushLaterList;

  if (TC <> tcDataType)  and (bolWritePackage) then
    Write( XmlClose(Core + 'Namespace.ownedElement'));

  if bolWritePackage  then
    Write( XmlClose(headerCorePackage) );
end;


procedure TXMIExporterArgoUML.WriteLogicPackage(L: TLogicPackage;
                                       TC:TTypeClassifier; IdPai : string);
var
  Mi : TBaseModelIterator;
begin
  Mi := L.GetPackages;
  while Mi.HasNext do
    WritePackage( Mi.Next as TAbstractPackage,TC, IdPai);
end;


procedure TXMIExporterArgoUML.WriteUnitPackage(U: TUnitPackage;
                                       TC:TTypeClassifier; IdPai : string);
const
  tName: string = 'void';
var
  Mi : TBaseModelIterator;
  C : TModelEntity;
  ex: TClassifier;
begin
  if tName <> '' then
    begin
      ex := U.FindClassifier(tName);
      if not Assigned(ex) then dataTypeVoid := U.AddDatatype(tName);
      tName := '';
    end;

  Mi := U.GetClassifiers;
  while Mi.HasNext do
  begin
    C := Mi.Next;
    if (C is TClass) AND (TC in [tcAll,tcNotDataType]) then
      WriteClass(C as TClass,IdPai)
    else if (C is TInterface) AND (TC in [tcAll,tcNotDataType]) then
      WriteInterface(C as TInterface,IdPai)
    else if (C is TDataType) AND (TC in [tcAll,tcDataType]) then
      WriteDataType(C as TDataType,IdPai);
  end;
end;

function TXMIExporterArgoUML.XmlClose(const S: string): string;
begin
  Result := '</' + S + '>';
end;

function TXMIExporterArgoUML.XmlOpen(const S: string): string;
begin
  Result := '<' + S + '>';
end;


//Writes a reference to a classifier
function TXMIExporterArgoUML.MakeTypeRef(C: TClassifier) : string;
var
  S : string;
begin
  S := '';
  if (C is TClass)   then
    S := headerCoreClass
  else if (C is TDataType ) then
    S := headerCoreDataType
  else if (C is TInterface ) then
    S := headerCoreInterface;

  Result := '<' + Core + S +' xmi.idref="' + MakeId(S+'_'+C.FullName) + '"/>'
end;


//Check that string does not contain xml-chars like < and >
function TXMIExporterArgoUML.Xml(const S: string): string;
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

procedure TXMIExporterArgoUML.WriteInterface(I: TInterface;IdPai : string);
{
          <Foundation.Core.ModelElement.supplierDependency>
            <Foundation.Core.Abstraction xmi.idref="xmi.37"/>
          </Foundation.Core.ModelElement.supplierDependency>
}
var
  Mi : TBaseModelIterator;
  ID : string;
begin
  ID := WriteEntityHeader(I, headerCoreInterface);
  complementoHeader(CoreModelGeneralizableElement, IdPai);
  WriteFeatures(I, ID);

  //Implementing classes
  Mi := I.GetImplementingClasses;
  if Mi.HasNext then
  begin
    Write( XmlOpen( CoreModelElement + 'supplierDependency') );
    while Mi.HasNext do
      MakeAbstract(Mi.Next as TClassifier,I);
    Write( XmlClose( CoreModelElement + 'supplierDependency') );
  end;

  Write( XmlClose(Core + headerCoreInterface) );
end;

procedure TXMIExporterArgoUML.WriteDataType(T: TDataType;IdPai : string);
begin
  WriteEntityHeader(T, headerCoreDataType);
  complementoHeader(CoreModelGeneralizableElement,IDPai);
  Write( XmlClose(Core + headerCoreDataType) );
end;

procedure TXMIExporterArgoUML.FlushLaterList;
var
  I : integer;
begin
  for I := 0 to LaterList.Count-1 do
    Write(LaterList[I]);
  LaterList.Clear;
end;


//Creates a reference to a generalization.
//Also create the generalization if it did not already exist.
procedure TXMIExporterArgoUML.MakeGeneral(Child, Parent: TClassifier;IdPai : string);
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
    //Create generalization
    ID := MakeId(S);
      LaterList.Add( '<' + Core + 'Generalization xmi.id="' + ID
           + '" xmi.uuid="'+    xmiUUID(ID) + '">');
      LaterList.Add( '<' + CoreModelElement + 'isSpecification xmi.value="false" />');
      LaterList.Add( getNamespace(IdPai));
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
procedure TXMIExporterArgoUML.MakeAbstract(Client, Supplier: TClassifier);
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


procedure TXMIExporterArgoUML.SaveTo(const FileName: string);
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
function TXMIExporterArgoUML.GetXmi: string;
begin
  SetLength(Result,Output.Size);
  Move(Output.Memory^,Result[1],Output.Size);
end;

//Used by htmldoc to get id of packages.
function TXMIExporterArgoUML.GetXMIId(E: TModelEntity): string;
begin
  Result := MakeID(E.FullName);
end;


function TXMIExporterArgoUML.getNamespace(IdPai : string) : string;
var
  lId : string;
begin
  lId := IdPai;
  if lId = '' then
    lId := IdModel;
  result := XmlOpen(CoreModelElement + 'namespace');
  result := result + '<' + Core + 'Namespace xmi.idref="'+ lId +'" />';
  result := result + XmlClose(CoreModelElement + 'namespace');
end;

procedure TXMIExporterArgoUML.SetbolWritePackage(valor: boolean);
begin
  bolWritePackage := valor;
end;

function TXMIExporterArgoUML.xmiUUID(Id : string) : string;
var
 provID: string;
 iID : integer;
begin
    provID := ID;
    delete(provID,1,length('xmi.'));
    iID := strToInt(provID);
    provID := IntToHex(32769-iID,4);
    result := '-106--94-51-41-5c9766:ee6479a4ca:-' + provID;
end;


end.