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

unit uJavaClassImport;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses uCodeParser, Classes, uModel, uIntegrator, uModelEntity;

type
  TJavaClassImporter = class(TImportIntegrator)
  private
    procedure NeedPackageHandler(const AName: string; var AStream: TStream; OnlyLookUp: Boolean = False);
  public
    procedure ImportOneFile(const FileName : string); override;
    class function GetFileExtensions : TStringList; override;
  end;


implementation

uses uJavaClass, SysUtils, uError;

type
  TJavaClassParser = class(TCodeParser)
  private
    OM : TObjectModel;
    function GetVisibility(flags : integer) : TVisibility;
    function ExtractPackageName(CName : string) : string;
    function ExtractClassName(CName : string) : string;
    function GetFieldType(const Field : string; var Index : integer) : TClassifier;
    function NeedClassifier(CName :  string; TheClass : TModelEntityClass = nil) : TClassifier;
  public
    procedure ParseStream(AStream: TStream; AModel: TAbstractPackage; AOM: TObjectModel); override;
  end;

{ TJavaClassImporter }


procedure TJavaClassImporter.ImportOneFile(const FileName : string);
var
  Str : TStream;
  Parser : TJavaClassParser;
begin
  Str := CodeProvider.LoadStream(FileName);
  if Assigned(Str) then
  begin
    Parser := TJavaClassParser.Create;
    try
      Parser.NeedPackage := NeedPackageHandler;
      Parser.ParseStream(Str, Model.ModelRoot, Model);
    finally
      Parser.Free;
    end;
  end;
end;

procedure TJavaClassImporter.NeedPackageHandler(const AName: string; var AStream: TStream; OnlyLookUp: Boolean = False);
var
  FileName : string;
begin
  AStream := nil;
  FileName := AName + '.class';
  FileName := CodeProvider.LocateFile(FileName);
  //Dont read same file twice
  if (not OnlyLookUp) and (FileName<>'') and (FilesRead.IndexOf(FileName)=-1) then
  begin
    AStream := CodeProvider.LoadStream(FileName);
    FilesRead.Add(FileName);
  end;
end;

class function TJavaClassImporter.GetFileExtensions: TStringList;
begin
  Result := TStringList.Create;
  Result.Values['.class'] := 'Java Class';
end;

{ TJavaClassParser }

procedure TJavaClassParser.ParseStream(AStream: TStream; AModel: TAbstractPackage; AOM: TObjectModel);
var
  JC : TClassFile;
  U : TUnitPackage;
  C : TMdlClass;
  Int : TMdlInterface;
  I : integer;
  PName,S : string;

  procedure ParseOp(Op : TMdlOperation; Met : TMethodInfo);
  var
    Desc : string;
    I,J : integer;
  begin
    Op.Visibility := GetVisibility(Met.access_flags);
    Op.IsAbstract := TAccData.isAbstract(Met.access_flags);
    Desc := Met.descriptor.GetString;
    I := 1;
    if Desc[I]='(' then
    begin
      //parameters
      J := 0;
      Inc(I);
      while (Desc[I]<>')') and (I<Length(Desc)) do
        Op.AddParameter( char( Ord('a') + J ) ).TypeClassifier := GetFieldType(Desc,I);
      Inc(I);
    end;
    if Desc[I]<>'V' then
      Op.ReturnValue := GetFieldType(Desc,I);
    if Met.isConstructor then
    begin
      Op.OperationType := otConstructor;
      Op.Name := C.Name;
    end else if Assigned(Op.ReturnValue) then
      Op.OperationType := otFunction
    else
      Op.OperationType := otProcedure
  end;

  procedure ParseAttr(Attr : TAttribute; Fi : TFieldInfo);
  var
    I : integer;
  begin
    Attr.Visibility := GetVisibility(Fi.access_flags);
    I := 1;
    Attr.TypeClassifier := GetFieldType(Fi.descriptor.getString,I);
  end;

begin
  FModel := AModel;
  OM := AOM;

  JC := TClassFile.Create(AStream);
  try
    PName := ExtractPackageName( JC.ClassName );
    U := OM.ModelRoot.FindUnitPackage(PName);
    if not Assigned(U) then
      U := (FModel as TLogicPackage).AddUnit( PName );
    if TAccData.isInterface( JC.classDecl.accessFlags ) then
    begin
      //interface
      Int := U.AddInterface( ExtractClassName(JC.ClassName) );
      Int.Visibility := GetVisibility( JC.classDecl.accessFlags );

      for I:=0 to Length(JC.classFields.classFields)-1 do
      begin
        S := JC.classFields.classFields[I].name.GetString;
        if (Length(S)>0) and (S[1]<>'$') then
          ParseAttr( Int.AddAttribute( S ),JC.classFields.classFields[I] );
      end;

      for I:=0 to Length(JC.classMethods.classMethods)-1 do
      begin
        S := JC.classMethods.classMethods[I].name.GetString;
        if (Length(S)>0) and (not (S[1] in ['<','$'])) then
          ParseOp( Int.AddOperation( S ) ,JC.classMethods.classMethods[I]);
      end;

    end
    else
    begin
      //class
      C := U.AddClass( ExtractClassName(JC.ClassName) );
      //ancestor
      if Assigned(JC.classDecl.superClass) then
      begin
        S := TObjNameFormat.ToDotSeparator(JC.classDecl.superClass.getString);
        if S<>'java.lang.Object' then
          C.Ancestor := NeedClassifier(S, TMdlClass) as TMdlClass;
      end;
      //implements
      for I := 0 to Length(JC.classDecl.interfaces)-1 do
        C.AddImplements(NeedClassifier(
            TObjNameFormat.toDotSeparator(JC.classDecl.interfaces[I].getString),
                                       TMdlInterface) as TMdlInterface);
      C.Visibility := GetVisibility( JC.classDecl.accessFlags );
      for I:=0 to Length(JC.classFields.classFields)-1 do
      begin
        S := JC.classFields.classFields[I].name.GetString;
        if (Length(S)>0) and (S[1]<>'$') then
          ParseAttr( C.AddAttribute( S ),JC.classFields.classFields[I] );
      end;
      for I:=0 to Length(JC.classMethods.classMethods)-1 do
      begin
        S := JC.classMethods.classMethods[I].name.GetString;
        if S='<init>' then  //Constructor has internal name '<init>'
          S := C.Name;
        if (Length(S)>0) and (not (S[1] in ['<','$'])) then
          ParseOp( C.AddOperation( S ) ,JC.classMethods.classMethods[I]);
      end;
    end;

  finally
    JC.Free;
  end;
end;

//Translate java-visibility
function TJavaClassParser.GetVisibility(flags: integer): TVisibility;
begin
  Result := viPrivate;
  if TAccData.isPublic( flags ) then
    Result := viPublic
  else if TAccData.isPrivate( flags ) then
    Result := viPrivate
  else if TAccData.isProtected( flags ) then
    Result := viProtected;
end;

function TJavaClassParser.ExtractPackageName(CName: string): string;
var
  I : integer;
begin
  I := LastDelimiter('.',CName);
  if I=0 then
    Result := 'Default'
  else
    Result := Copy(CName,1,I-1);
end;

//Extract short class name
function TJavaClassParser.ExtractClassName(CName: string): string;
var
  I : integer;
begin
  I := LastDelimiter('.',CName);
  if I=0 then
    Result := CName
  else
    Result := Copy(CName,I+1,255);
end;


function TJavaClassParser.NeedClassifier(CName: string; TheClass : TModelEntityClass = nil): TClassifier;
var
  PName,ShortName : string;
  U : TUnitPackage;
  Parser : TJavaClassParser;
  Str : TStream;
begin
  Result := nil;
  PName := ExtractPackageName(CName);
  ShortName := ExtractClassName(CName);

  //First look in model
  U := OM.ModelRoot.FindUnitPackage(PName);
  if Assigned(U) then
    Result := U.FindClassifier(ShortName,False,TheClass,True);

  if not Assigned(Result) then
  begin
    //See if we can find the file that we need
    Str := nil;
    if Assigned(NeedPackage) then
      NeedPackage( ShortName ,str);
    if Assigned(Str) then
    begin
      Parser := TJavaClassParser.Create;
      try
        Parser.NeedPackage := NeedPackage;
        Parser.ParseStream(Str, OM.ModelRoot, OM);
      finally
        Parser.Free;
      end;
      U := OM.ModelRoot.FindUnitPackage(PName);
      if Assigned(U) then
        Result := U.FindClassifier(ShortName,False,TheClass,True);
    end;
  end;

  if not Assigned(Result) then
  begin
    //Look in unknown package
    Result := OM.UnknownPackage.FindClassifier(CName, False, TheClass, True);
    if not Assigned(Result) then
    begin
      if (TheClass=nil) or (TheClass=TMdlClass) then
        Result := OM.UnknownPackage.AddClass(CName)
      else if TheClass=TMdlInterface then
        Result := OM.UnknownPackage.AddInterface(CName)
      else if TheClass=TDataType then
        Result := OM.UnknownPackage.AddDataType(CName)
    end;
  end;

  if not Assigned(Result) then
    raise Exception.Create(ClassName + ' failed to locate ' + Cname);
end;

function TJavaClassParser.GetFieldType(const Field: string; var Index: integer): TClassifier;
var
  DimCount,I : integer;
  S : string;
  IsPrimitive : boolean;
begin
  Result := nil;
  DimCount := 0;
  while Field[Index]='[' do
  begin
    Inc(DimCount);
    Inc(Index);
  end;
  IsPrimitive := True;
  case Field[Index] of
    'B' : S := 'byte';
    'C' : S := 'char';
    'D' : S := 'double';
    'F' : S := 'float';
    'I' : S := 'int';
    'J' : S := 'long';
    'L' :
      begin
        Inc(Index);
        I := Index;
        while (Field[I]<>';') and (I<Length(Field)) do
          Inc(I);
        S := TObjNameFormat.toDotSeparator( Copy(Field,Index,I-Index) );
        Index := I;
        IsPrimitive := False;
      end;
    'S' : S := 'short';
    'Z' : S := 'boolean';
  end;
  Inc(Index);
  for I := 0 to DimCount-1 do
    S := S + '[]';

  if S='' then
    ErrorHandler.Trace(ClassName + ' getfieldtype: ' + Field)
  else
  begin
    if IsPrimitive then
      Result := NeedClassifier( S , TDataType)
    else
      Result := NeedClassifier( S );
  end;
end;

initialization

  Integrators.Register(TJavaClassImporter);

end.