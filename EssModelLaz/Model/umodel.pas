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

unit uModel;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
  Classes to represent the object model.
}

interface

uses Contnrs, Classes, SysUtils, uModelEntity, uIterators;

const
  UNKNOWNPACKAGE_NAME = '<<Unknown>>';
  ConfigFileExt = '.essModel';

type
  TLogicPackage = class;
  TUnitPackage = class;

  TOperationType = (otConstructor, otDestructor, otProcedure, otFunction);

  TObjectModel = class
  private
    FListeners: TListenerList; // TInterfaceList;
    FModelRoot: TLogicPackage;
    FUnknownPackage: TUnitPackage;
    FLocked: boolean;
    procedure CreatePackages;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Fire(Method: TListenerMethodType; Info: TModelEntity = nil);
    procedure AddListener(NewListener: TListenerBase);
    procedure RemoveListener(Listener: TListenerBase);
    procedure Clear;
    procedure Lock;
    procedure Unlock;
    property ModelRoot: TLogicPackage read FModelRoot;
    property Locked: boolean read FLocked;
    property UnknownPackage: TUnitPackage read FUnknownPackage;
  end;

  // Class definition.
  TFeature = class(TModelEntity);

  TClassifier = class(TModelEntity)
  private
    FFeatures: TModelEntityList;
    FIsPlaceholder: boolean;
  public
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    property IsPlaceholder: boolean read FIsPlaceHolder write FIsPlaceholder;
    function GetFeatures : TBaseModelIterator;
  end;

  TParameter = class(TModelEntity)
  private
    FTypeClassifier : TClassifier;
  protected
//    class function GetBeforeListener: TGUID; override;
//    class function GetAfterListener: TGUID; override;
  public
    property TypeClassifier : TClassifier read FTypeClassifier write FTypeClassifier;
  end;

  TMdlOperation = class(TFeature)
  protected
//    class function GetBeforeListener: TGUID; override;
//    class function GetAfterListener: TGUID; override;
  private
    FOperationType: TOperationType;
    FParameters: TModelEntityList;
    FIsAbstract: boolean;
    FReturnValue: TClassifier;
    procedure SetOperationType(const Value: TOperationType);
    procedure SetIsAbstract(const Value: boolean);
    procedure SetReturnValue(const Value: TClassifier);
  public
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    function AddParameter(const NewName: string): TParameter;
    property OperationType: TOperationType read FOperationType write SetOperationType;
    property IsAbstract: boolean read FIsAbstract write SetIsAbstract;
    property ReturnValue: TClassifier read FReturnValue write SetReturnValue;
    function GetParameters : TBaseModelIterator;
  end;

  TAttribute = class(TFeature)
  private
    FTypeClassifier: TClassifier;
    procedure SetTypeClassifier(const Value: TClassifier);
  protected
//    class function GetBeforeListener: TGUID; override;
//    class function GetAfterListener: TGUID; override;
  public
    property TypeClassifier : TClassifier read FTypeClassifier write SetTypeClassifier;
  end;

  TProperty = class(TAttribute)
  { TODO : to be specified later }
  protected
//    class function GetBeforeListener: TGUID; override;
//    class function GetAfterListener: TGUID; override;
  end;

  TDataType = class(TClassifier)
    {From UML-spec: A descriptor of a set of values that lack identity and whose
    operations do not have side effects. Datatypes include
    primitive pre-defined types and user-definable types. Pre-defined
    types include numbers, string and time. User-definable
    types include enumerations.}
  protected
//    class function GetBeforeListener: TGUID; override;
//    class function GetAfterListener: TGUID; override;
  end;

  TMdlInterface = class(TClassifier)
  private
    FAncestor: TMdlInterface;
    procedure SetAncestor(const Value: TMdlInterface);
  protected
//    class function GetBeforeListener: TGUID; override;
//    class function GetAfterListener: TGUID; override;
  public
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    function AddOperation(const NewName: string): TMdlOperation;
    function AddAttribute(const NewName: string): TAttribute;
    function GetOperations : TBaseModelIterator;
    function GetAttributes : TBaseModelIterator;
    property Ancestor: TMdlInterface read FAncestor write SetAncestor;
    function GetImplementingClasses : TBaseModelIterator;
  end;

  TMdlClass = class(TClassifier) // , IBeforeClassListener
  protected
//    class function GetBeforeListener: TGUID; override;
//    class function GetAfterListener: TGUID; override;
  private
    FAncestor: TMdlClass;
    FImplements: TModelEntityList;
    procedure SetAncestor(const Value: TMdlClass);
//    procedure IBeforeClassListener.Change = AncestorChange;
//    procedure IBeforeClassListener.EntityChange = AncestorEntityChange;
//    procedure IBeforeClassListener.AddChild = AncestorAddChild;
//    procedure IBeforeClassListener.Remove = AncestorRemove;
  public
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    function AddOperation(const NewName: string): TMdlOperation;
    function AddAttribute(const NewName: string): TAttribute;
    function AddProperty(const NewName: string): TProperty;
    function AddImplements(I: TMdlInterface): TMdlInterface;
    property Ancestor: TMdlClass read FAncestor write SetAncestor;
    function GetOperations : TBaseModelIterator;
    function GetAttributes : TBaseModelIterator;
    function GetImplements : TBaseModelIterator;
    function GetDescendants : TBaseModelIterator;
    function FindOperation(O : TMdlOperation) : TMdlOperation;
    //Ancestorlisteners
    procedure BeforeChange(Sender: TModelEntity); override;
    procedure BeforeAddChild(Sender: TModelEntity; NewChild: TModelEntity); override;
    procedure BeforeRemove(Sender: TModelEntity); override;
    procedure BeforeEntityChange(Sender: TModelEntity); override;
  public
    property xImplements: TModelEntityList read FImplements;
  end;


  TAbstractPackage = class(TModelEntity)
  private
    ConfigFile : string;
  public
    procedure SetConfigFile(const Value : string);
    function GetConfigFile : string;
  end;

  //Represents the link between one package that uses another
  TUnitDependency = class(TModelEntity)
  public
    xPackage : TUnitPackage;
  end;

  TUnitPackage = class(TAbstractPackage)
  protected
//    class function GetBeforeListener: TGUID; override;
//    class function GetAfterListener: TGUID; override;
  private
    FClassifiers: TModelEntityList;
    FUnitDependencies: TModelEntityList;
  public
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    function AddClass(const NewName: string): TMdlClass;
    function AddInterface(const NewName: string): TMdlInterface;
    function AddDatatype(const NewName: string): TDataType;
    function AddUnitDependency(U : TUnitPackage; Visibility : TVisibility): TUnitDependency;
    function FindClassifier(const CName: string; RaiseException: boolean = False;
        TheClass : TModelEntityClass = nil; CaseSense : boolean = False): TClassifier;
    function GetClassifiers : TBaseModelIterator;
    function GetUnitDependencies : TBaseModelIterator;
  public
    property Classifiers: TModelEntityList read FClassifiers;
    property UnitDependencies: TModelEntityList read FUnitDependencies;
  end;


  TLogicPackage = class(TAbstractPackage)
  private
    FPackages: TModelEntityList;
  protected
//    class function GetBeforeListener: TGUID; override;
//    class function GetAfterListener: TGUID; override;
  public
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    function AddUnit(const NewUnitName: string): TUnitPackage;
    //Might need a AddLogicPackage also
    function FindUnitPackage(const PName: string; RaiseException: boolean = False;
                             CaseSense : boolean = False): TUnitPackage;
    function GetPackages : TBaseModelIterator;
    function GetAllUnitPackages : TBaseModelIterator;
    function GetAllClassifiers : TBaseModelIterator;
  public
    property Packages: TModelEntityList read FPackages;
  end;


  { TAttributeList }
  TAttributeList = class(TObjectList)
  private
    function GetItems(AIndex: integer): TAttribute;
    procedure SetItems(AIndex: integer; const AValue: TAttribute);
  public
    property Items[AIndex: integer]: TAttribute read GetItems write SetItems; default;
  end;

  { TParameterList }
  TParameterList = class(TObjectList)
  private
    function GetItems(AIndex: integer): TParameter;
    procedure SetItems(AIndex: integer; const AValue: TParameter);
  public
    property Items[AIndex: integer]: TParameter read GetItems write SetItems; default;
  end;

  function AllClassesPackage : TAbstractPackage;


implementation

uses uError, uViewIntegrator;

type
  //Used by Class.GetDescendant
  TClassDescendantFilter = class(TIteratorFilter)
  private
    Ancestor : TMdlClass;
  public
    constructor Create(Ancestor : TMdlClass);
    function Accept(M : TModelEntity) : boolean; override;
  end;

  //Used by Interface.GetImplementingClasses
  TInterfaceImplementsFilter = class(TIteratorFilter)
  private
    Int : TMdlInterface;
  public
    constructor Create(I : TMdlInterface);
    function Accept(M : TModelEntity) : boolean; override;
  end;

  TStrCompare = function(const S1, S2: string): Integer;

const
  CompareFunc : array[boolean] of TStrCompare = (CompareText, CompareStr);

var
  _AllClassesPackage : TAbstractPackage = nil;

{ TObjectModel }

constructor TObjectModel.Create;
begin
  FListeners := TListenerList.Create(False);
  CreatePackages;
end;

destructor TObjectModel.Destroy;
begin
  FreeAndNil(FListeners);
  FreeAndNil(FModelRoot);
// FUnknownPackage will be freed by FModelRoot who owns it
  inherited;
end;

procedure TObjectModel.Clear;
begin
  //Model must be locked, otherwise events will be fired back to
  //backend and diagram.
  if not FLocked then
  begin
    Lock;
    FreeAndNil(FModelRoot);
    CreatePackages;
    UnLock;
  end
  else begin
    FreeAndNil(FModelRoot);
    CreatePackages;
  end;
end;

procedure TObjectModel.Fire(Method: TListenerMethodType; Info: TModelEntity = nil);
var
  I: integer;
  L: TListenerBase;
begin
  if not Locked then
    for I := 0 to FListeners.Count - 1 do
    begin
      L := FListeners[I];
      Assert(L is TListenerBase, 'TObjectModel.Fire: Listener is not TDispatchBase.');
      case Method of
        //BeforeChange is triggered when the model will be changed from the root-level.
        mtBeforeChange: L.BeforeChange(nil);
        //AfterChange is triggered when the model has been changed from the root-level.
        mtAfterChange:  L.AfterChange(nil);
      else raise Exception.Create(ClassName + ' Eventmethod not recognized.');
      end;
    end;
end;


procedure TObjectModel.Lock;
begin
  Fire(mtBeforeChange);
  FLocked := True;
  ModelRoot.Locked := True;
end;

procedure TObjectModel.Unlock;
begin
  FLocked := False;
  ModelRoot.Locked := False;
  Fire(mtAfterChange);
end;

procedure TObjectModel.CreatePackages;
begin
  //Creates the default packages that must exist
  FModelRoot := TLogicPackage.Create(nil);
  FUnknownPackage := FModelRoot.AddUnit(UNKNOWNPACKAGE_NAME);
end;

procedure TObjectModel.AddListener(NewListener: TListenerBase);
begin
  if FListeners.IndexOf(NewListener) = -1 then
    FListeners.Add(NewListener);
end;

procedure TObjectModel.RemoveListener(Listener: TListenerBase);
begin
  FListeners.Remove(Listener);
end;

{ TLogicPackage }

constructor TLogicPackage.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FPackages := TModelEntityList.Create(True);
end;

destructor TLogicPackage.Destroy;
begin
  FreeAndNil(FPackages);
  inherited;
end;

function TLogicPackage.AddUnit(const NewUnitName: string): TUnitPackage;
begin
  Result := TUnitPackage.Create(Self);
  Result.FName := NewUnitName;
  FPackages.Add(Result);
  try
    Fire(mtBeforeAddChild, Result)
  except
    FPackages.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result)
end;
{
class function TLogicPackage.GetAfterListener: TGUID;
begin
  Result := IAfterLogicPackageListener;
end;

class function TLogicPackage.GetBeforeListener: TGUID;
begin
  Result := IBeforeLogicPackageListener;
end;
}
//Searches in this and dependant logic packages after a unit with name PName.
function TLogicPackage.FindUnitPackage(const PName: string; RaiseException: boolean = False; CaseSense : boolean = False): TUnitPackage;
var
  I: integer;
  P: TAbstractPackage;
  F : TStrCompare;
begin
  F := CompareFunc[CaseSense];
  Result := nil;
  for I := 0 to FPackages.Count - 1 do
  begin
    P := FPackages[I] as TAbstractPackage;
    if (P is TLogicPackage) then
    begin
      Result := (P as TLogicPackage).FindUnitPackage(PName, RaiseException);
      if Assigned(Result) then
        Exit;
    end
    else if (P is TUnitPackage) then
    begin
      if F(P.Name,PName)=0 then
      begin
        Result := P as TUnitPackage;
        Exit;
      end;
    end;
  end;
  if not Assigned(Result) and RaiseException then
    raise Exception.Create(ClassName + '.FindUnitPackage failed: ' + PName);
end;

function TLogicPackage.GetPackages: TBaseModelIterator;
begin
  Result := TModelIterator.Create(FPackages);
end;

//Returns all unitpackages in and below this logic package.
//Unknownpackage is excluded.
function TLogicPackage.GetAllUnitPackages: TBaseModelIterator;
var
  List : TModelEntityList;

  procedure InAddNested(L : TLogicPackage);
  var
    Mi : TBaseModelIterator;
    P : TModelEntity;
  begin
    Mi := L.GetPackages;
    try
      while Mi.HasNext do
      begin
        P := Mi.Next;
        if P is TLogicPackage then
          InAddNested(P as TLogicPackage)
        else //Not logicpackage, must be unitpackage.
          if (P.Name<>UNKNOWNPACKAGE_NAME) then
            List.Add( P );
      end;
    finally
      Mi.Free;
    end;
  end;

begin
  List := TModelEntityList.Create(False);
  try
    InAddNested(Self);
    Result := TModelIterator.Create(List,True);
  finally
    List.Free;
  end;
end;

//Returns all classifiers in and below this logic package.
function TLogicPackage.GetAllClassifiers: TBaseModelIterator;
var
  Pmi,Cmi : TBaseModelIterator;
  List : TModelEntityList;
begin
  List := TModelEntityList.Create(False);
  try
    Pmi := GetAllUnitPackages;
    try
      while Pmi.HasNext do
      begin
        Cmi := (Pmi.Next as TUnitPackage).GetClassifiers;
        try
          while Cmi.HasNext do
            List.Add( Cmi.Next );
        finally
          Cmi.Free;
        end;
      end;
    finally
      Pmi.Free;
    end;
    Result := TModelIterator.Create(List,True);
  finally
    List.Free;
  end;
end;

{ TUnitPackage }

constructor TUnitPackage.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FClassifiers := TModelEntityList.Create(True);
  FUnitDependencies := TModelEntityList.Create(True);
end;

destructor TUnitPackage.Destroy;
begin
  FreeAndNil(FClassifiers);
  FreeAndNil(FUnitDependencies);
  inherited;
end;

function TUnitPackage.AddClass(const NewName: string): TMdlClass;
begin
  Result := TMdlClass.Create(Self);
  Result.FName := NewName;
  FClassifiers.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FClassifiers.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

function TUnitPackage.AddInterface(const NewName: string): TMdlInterface;
begin
  Result := TMdlInterface.Create(Self);
  Result.FName := NewName;
  FClassifiers.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FClassifiers.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

function TUnitPackage.AddDatatype(const NewName: string): TDataType;
begin
  Result := TDataType.Create(Self);
  Result.FName := NewName;
  FClassifiers.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FClassifiers.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;
{
class function TUnitPackage.GetAfterListener: TGUID;
begin
  Result := IAfterUnitPackageListener;
end;

class function TUnitPackage.GetBeforeListener: TGUID;
begin
  Result := IBeforeUnitPackageListener;
end;
}

{
  Search for classifier in this unit, then looks in UnitDependencies if necessary.
  Used by the parser to find ancestorclass within current scope.
}
function TUnitPackage.FindClassifier(const CName: string;
  RaiseException: boolean = False;
  TheClass : TModelEntityClass = nil;
  CaseSense : boolean = False): TClassifier;
var
  C : TClassifier;
  Mim : TBaseModelIterator;
  P : TUnitPackage;
  F : TStrCompare;

  function InFind(P : TUnitPackage) : TClassifier;
  var
    Mi, tempMi : TBaseModelIterator;
  begin
    Result := nil;
    tempMi := nil;
    //Search in this unit
    if Assigned(TheClass) then begin
      tempMi := TModelIterator.Create(P.Classifiers);
      Mi := TModelIterator.Create(tempMi, TheClass);
    end
    else
      Mi := P.GetClassifiers;
    try
      while Mi.HasNext do
      begin
        C := Mi.Next as TClassifier;
        if F(C.Name,CName)=0 then
        begin
          Result := C;
          Break;
        end;
      end;
    finally
      Mi.Free;
      if Assigned(tempMi) then
        tempMi.Free;
    end;
  end;

begin
  F := CompareFunc[CaseSense];
  //Search in this unit
  Result := InFind(Self);
  //If nil search in public dependencies
  if not Assigned(Result) then
  begin
    Mim := GetUnitDependencies;
    try
      while Mim.HasNext do
      begin
        P := (Mim.Next as TUnitDependency).xPackage;
        Result := InFind(P);
        if Assigned(Result) then
          Break;
      end;
    finally
      Mim.Free;
    end;
  end;
  if not Assigned(Result) and RaiseException then
    raise Exception.Create(ClassName + '.FindClassifier failed: ' + CName);
end;

function TUnitPackage.GetClassifiers: TBaseModelIterator;
begin
  Result := TModelIterator.Create( FClassifiers );
end;

function TUnitPackage.AddUnitDependency(U: TUnitPackage; Visibility: TVisibility): TUnitDependency;
begin
  Assert( (U<>Self) and (U<>nil) ,ClassName + '.AddUnitDependency invalid parameter');
  Result := TUnitDependency.Create( Self );
  Result.xPackage := U;
  Result.Visibility := Visibility;
  FUnitDependencies.Add( Result );
end;

function TUnitPackage.GetUnitDependencies: TBaseModelIterator;
begin
  Result := TModelIterator.Create( FUnitDependencies );
end;

{ TMdlClass }

constructor TMdlClass.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FImplements := TModelEntityList.Create(False); // Only reference
end;

destructor TMdlClass.Destroy;
begin
  // Don't touch listeners if the model is locked.
  if not Locked then
  begin
    Fire(mtBeforeRemove);
    //    if Assigned(FAncestor) then
    //      FAncestor.RemoveListener(IBeforeClassListener(Self));
  end;
  FreeAndNil(FImplements);
  inherited;
end;

function TMdlClass.AddAttribute(const NewName: string): TAttribute;
begin
  Result := TAttribute.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

function TMdlClass.AddProperty(const NewName: string): TProperty;
begin
  Result := TProperty.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
end;

function TMdlClass.AddOperation(const NewName: string): TMdlOperation;
begin
  Result := TMdlOperation.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;
{
class function TMdlClass.GetAfterListener: TGUID;
begin
  Result := IAfterClassListener;
end;

class function TMdlClass.GetBeforeListener: TGUID;
begin
  Result := IBeforeClassListener;
end;
}
function TMdlClass.AddImplements(I: TMdlInterface): TMdlInterface;
begin
  Result := I;
  FImplements.Add(I);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FImplements.Remove(I);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

procedure TMdlClass.SetAncestor(const Value: TMdlClass);
var
  Old: TMdlClass;
begin
  Assert(Value <> Self, 'Tried to set self to ancestor.');
  if Value <> FAncestor then
  begin
    Old := FAncestor;
    FAncestor := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FAncestor := Old;
      raise;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

procedure TMdlClass.BeforeAddChild(Sender, NewChild: TModelEntity);
begin
  Assert(Assigned(Sender), 'Sender not assigned in TMdlClass.BeforeAddChild.');
  ErrorHandler.Trace(Format('AncestorAddChild: %s : %s : %s', [ClassName, FName, Sender.Name]));
end;

procedure TMdlClass.BeforeChange(Sender: TModelEntity);
begin
  Assert(Assigned(Sender), 'Sender not assigned in TRtfdDiagram.BeforeChange.');
  ErrorHandler.Trace(Format('AncestorChange: %s : %s : %s', [ClassName, FName, Sender.Name]));
end;

procedure TMdlClass.BeforeEntityChange(Sender: TModelEntity);
begin
  Assert(Assigned(Sender), 'Sender not assigned in TRtfdDiagram.BeforeEntityChange.');
  ErrorHandler.Trace(Format('AncestorEntityChange: %s : %s : %s', [ClassName, FName, Sender.Name]));
  Fire(mtBeforeEntityChange);
  Fire(mtAfterEntityChange);
end;

procedure TMdlClass.BeforeRemove(Sender: TModelEntity);
begin
  ErrorHandler.Trace(Format('AncestorRemove: %s : %s : %s', [ClassName, FName, Sender.Name]));
  FAncestor.RemoveListener(Self); // IBeforeClassListener()
  Ancestor := nil;
end;

function TMdlClass.GetOperations: TBaseModelIterator;
var
  tempMi: TBaseModelIterator;
begin
  tempMi := GetFeatures;
  try
    Result := TModelIterator.Create( tempMi , TMdlOperation);
  finally
    tempMi.Free;
  end;
end;

function TMdlClass.GetAttributes: TBaseModelIterator;
var
  tempMi: TBaseModelIterator;
begin
  tempMi := GetFeatures;
  try
    Result := TModelIterator.Create( tempMi , TAttribute);
  finally
    tempMi.Free;
  end;
end;

function TMdlClass.GetImplements: TBaseModelIterator;
begin
  Result := TModelIterator.Create( FImplements );
end;

//Returns a list of classes that inherits from this class.
function TMdlClass.GetDescendants: TBaseModelIterator;
var
  tempMi: TBaseModelIterator;
begin
  tempMi := (Root as TLogicPackage).GetAllClassifiers;
  try
    Result := TModelIterator.Create(tempMi, TClassDescendantFilter.Create(Self));
  finally
    tempMi.Free;
  end;
end;


{
  Finds an operation with same name and signature as parameter.
  Used by Delphi-parser to find a modelentity for a method implementation.
}
function TMdlClass.FindOperation(O: TMdlOperation): TMdlOperation;
var
  Mi, Omi1, Omi2 : TBaseModelIterator;
  O2 : TMdlOperation;
  IsMatch: Boolean;
begin
  Assert(O<>nil,ClassName + '.FindOperation invalid parameter');
  Result := nil;
  Mi := GetOperations;
  try
    while Mi.HasNext do
    begin
      O2 := Mi.Next as TMdlOperation;
      //Compare nr of parameters
      if O.FParameters.Count <> O2.FParameters.Count then
        Continue;
      { TODO -ovk : case sensitive match? java/delphi. only delphi-parser calls this method. }
      //Compare operation name
      if CompareText(O.Name,O2.Name)<>0 then
        Continue;
      //Compare parameters
      Omi1 := O.GetParameters;
      Omi2 := O2.GetParameters;
      try
        IsMatch := True;
        while Omi1.HasNext do
        begin
          if CompareText((Omi1.Next as TParameter).Name,
                         (Omi2.Next as TParameter).Name)<>0 then
            IsMatch := False;
            Break;
        end;
        if IsMatch then  //Ok, match
        begin
          Result := O2;
          Break;
        end;
      finally
        Omi2.Free;
        Omi1.Free;
      end;
    end;
  finally
    Mi.Free;
  end;
end;


{ TParameter }
{
class function TParameter.GetAfterListener: TGUID;
begin
  Result := IAfterParameterListener;
end;

class function TParameter.GetBeforeListener: TGUID;
begin
  Result := IBeforeParameterListener;
end;
}
{ TMdlOperation }


constructor TMdlOperation.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FParameters := TModelEntityList.Create(True);
end;

destructor TMdlOperation.Destroy;
begin
  FreeAndNil(FParameters);
  inherited;
end;

function TMdlOperation.AddParameter(const NewName: string): TParameter;
begin
  Result := TParameter.Create(Self);
  Result.FName := NewName;
  FParameters.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FParameters.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;
{
class function TMdlOperation.GetAfterListener: TGUID;
begin
  Result := IAfterOperationListener;
end;

class function TMdlOperation.GetBeforeListener: TGUID;
begin
  Result := IBeforeOperationListener;
end;
}
procedure TMdlOperation.SetOperationType(const Value: TOperationType);
var
  Old: TOperationType;
begin
  Old := FOperationType;
  if Old <> Value then
  begin
    FOperationType := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FOperationType := Old;
      raise;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

procedure TMdlOperation.SetIsAbstract(const Value: boolean);
var
  Old: boolean;
begin
  Old := FIsAbstract;
  if Old <> Value then
  begin
    FIsAbstract := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FIsAbstract := Old;
      raise;
    end {try};
    Fire(mtAfterEntityChange);
  end;
end;

procedure TMdlOperation.SetReturnValue(const Value: TClassifier);
var
  Old: TClassifier;
begin
  Old := FReturnValue;
  if Old <> Value then
  begin
    FReturnValue := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FReturnValue := Old;
      raise;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

function TMdlOperation.GetParameters: TBaseModelIterator;
begin
  Result := TModelIterator.Create(FParameters);
end;

{ TAttribute }
{
class function TAttribute.GetAfterListener: TGUID;
begin
  Result := IAfterAttributeListener;
end;

class function TAttribute.GetBeforeListener: TGUID;
begin
  Result := IBeforeAttributeListener;
end;
}
procedure TAttribute.SetTypeClassifier(const Value: TClassifier);
var
  Old: TClassifier;
begin
  Old := FTypeClassifier;
  if Old <> Value then
  begin
    FTypeClassifier := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FTypeClassifier := Old;
      raise;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

{ TProperty }
{
class function TProperty.GetAfterListener: TGUID;
begin
  Result := IAfterPropertyListener;
end;

class function TProperty.GetBeforeListener: TGUID;
begin
  Result := IBeforePropertyListener;
end;
}
{ TClassifier }

constructor TClassifier.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FFeatures := TModelEntityList.Create(True);
end;

destructor TClassifier.Destroy;
begin
  FFeatures.Free;
  inherited;
end;

function TClassifier.GetFeatures: TBaseModelIterator;
begin
  Result := TModelIterator.Create(FFeatures);
end;

{ TMdlInterface }

constructor TMdlInterface.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
end;

destructor TMdlInterface.Destroy;
begin
  inherited;
end;

function TMdlInterface.AddOperation(const NewName: string): TMdlOperation;
begin
  Result := TMdlOperation.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;
{
class function TMdlInterface.GetAfterListener: TGUID;
begin
  Result := IAfterInterfaceListener;
end;

class function TMdlInterface.GetBeforeListener: TGUID;
begin
  Result := IBeforeInterfaceListener;
end;
}
function TMdlInterface.GetOperations: TBaseModelIterator;
var
  tempMi: TBaseModelIterator;
begin
  tempMi := GetFeatures;
  try
    Result := TModelIterator.Create( tempMi , TMdlOperation);
  finally
    tempMi.Free;
  end;
end;

procedure TMdlInterface.SetAncestor(const Value: TMdlInterface);
begin
  Assert(Value <> Self, 'Tried to set self to ancestor.');
  FAncestor := Value;
end;

//Returns a list of classes that implements this interface.
function TMdlInterface.GetImplementingClasses: TBaseModelIterator;
var
//  List: TModelEntityList;
  TempMi: TBaseModelIterator;
begin
//  List := (Root as TLogicPackage).GetAllClassifierList;
  TempMi := (Root as TLogicPackage).GetAllClassifiers;
  try
    Result := TModelIterator.Create(TempMi, TInterfaceImplementsFilter.Create(Self));
  finally
    TempMi.Free;
  end;
{  Result := TModelIterator.Create(
    (Root as TLogicPackage).GetAllClassifiers,
    TInterfaceImplementsFilter.Create(Self) );  }
end;

function TMdlInterface.AddAttribute(const NewName: string): TAttribute;
begin
  Result := TAttribute.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

function TMdlInterface.GetAttributes : TBaseModelIterator;
var
  tempMi: TBaseModelIterator;
begin
  tempMi := GetFeatures;
  try
    Result := TModelIterator.Create( GetFeatures , TAttribute);
  finally
    tempMi.Free;
  end;
end;

{ TDataType }
{
class function TDataType.GetAfterListener: TGUID;
begin
  Result := IBeforeInterfaceListener;
end;

class function TDataType.GetBeforeListener: TGUID;
begin
  Result := IAfterInterfaceListener;
end;
}
{ TAbstractPackage }

function TAbstractPackage.GetConfigFile: string;
begin
  Result := ConfigFile;
  if (Result='') and Assigned(FOwner) then
    Result := (Owner as TAbstractPackage).GetConfigFile;
end;

procedure TAbstractPackage.SetConfigFile(const Value: string);
begin
  if Value<>'' then
    ConfigFile := ChangeFileExt(Value,ConfigFileExt);
end;


{ TClassDescendantFilter }

constructor TClassDescendantFilter.Create(Ancestor: TMdlClass);
begin
  inherited Create;
  Self.Ancestor := Ancestor;
end;

//Returns true if M inherits from ancestor
function TClassDescendantFilter.Accept(M: TModelEntity): boolean;
begin
  Result := (M is TMdlClass) and ((M as TMdlClass).Ancestor = Ancestor);
end;

{ TInterfaceImplementsFilter }

constructor TInterfaceImplementsFilter.Create(I: TMdlInterface);
begin
  inherited Create;
  Int := I;
end;

//Returns true if M implements interface Int
function TInterfaceImplementsFilter.Accept(M: TModelEntity): boolean;
var
  MC: TMdlClass;
begin
  MC := M as TMdlClass;
  Result := MC.FImplements.IndexOf(Int) <> -1;
end;


//Unique Flag-instance, if Integrator.CurrentEntity=AllClassesPackage then show all classes
function AllClassesPackage : TAbstractPackage;
begin
  if _AllClassesPackage=nil then
    _AllClassesPackage := TAbstractPackage.Create(nil);
  Result := _AllClassesPackage;
end;

{ TAttributeList }

function TAttributeList.GetItems(AIndex: integer): TAttribute;
begin
  Result := (inherited Items[AIndex]) as TAttribute;
end;

procedure TAttributeList.SetItems(AIndex: integer; const AValue: TAttribute);
begin
  Items[AIndex] := AValue;
end;

{ TParameterList }

function TParameterList.GetItems(AIndex: integer): TParameter;
begin
  Result := (inherited Items[AIndex]) as TParameter;
end;

procedure TParameterList.SetItems(AIndex: integer; const AValue: TParameter);
begin
  Items[AIndex] := AValue;
end;


initialization

finalization
  if Assigned(_AllClassesPackage) then
    _AllClassesPackage.Free;

end.
