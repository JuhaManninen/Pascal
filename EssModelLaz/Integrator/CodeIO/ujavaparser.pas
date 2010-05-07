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

unit uJavaParser;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes, uCodeParser, uModel, uModelEntity, uIntegrator;

type
  TJavaImporter = class(TImportIntegrator)
  private
    procedure NeedPackageHandler(const AName: string; var AStream: TStream; OnlyLookUp: Boolean = False);
  public
    procedure ImportOneFile(const FileName : string); override;
    class function GetFileExtensions : TStringList; override;
  end;


  TJavaParser = class(TCodeParser)
  private
    FStream: TMemoryStream;
    FCurrPos: PChar;
    Token: string;
    FOM: TObjectModel;
    FUnit: TUnitPackage;
    Comment: string; // Accumulated comment string used for documentation of entities.
    ModAbstract : boolean;
    ModVisibility: TVisibility;
    ClassImports,FullImports : TStringList;
    NameCache : TStringList;

    function SkipToken(const what: string): Boolean;
    function SkipPair(const open, close: string): Boolean;
    function GetChar: char;

    procedure EatWhiteSpace;
    function GetNextToken: string;

    procedure ParseCompilationUnit;
    procedure ParseTypeDeclaration;
    procedure ParseModifiersOpt;
    procedure ParseClassDeclaration(IsInner : boolean = False; const ParentName : string = '');
    procedure ParseInterfaceDeclaration;

    procedure DoOperation(O: TMdlOperation; const ParentName, TypeName: string);
    procedure DoAttribute(A: TAttribute; const TypeName: string);
    function GetTypeName : string;

    procedure SetVisibility(M: TModelEntity);
    function NeedClassifier(const CName: string; Force : boolean = True; TheClass: TModelEntityClass = nil): TClassifier;
    function NeedSource(const SourceName : string) : boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseStream(AStream: TStream; AModel: TAbstractPackage; AOM: TObjectModel); overload; override;
  end;

implementation

uses LCLIntf, LCLType, Dialogs, SysUtils, uError;


function ExtractPackageName(const CName: string): string;
var
  I : integer;
begin
  I := LastDelimiter('.',CName);
  if I=0 then
    Result := ''
  else
    Result := Copy(CName,1,I-1);
end;

function ExtractClassName(const CName: string): string;
var
  I : integer;
begin
  I := LastDelimiter('.',CName);
  if I=0 then
    Result := CName
  else
    Result := Copy(CName,I+1,255);
end;


{ TJavaImporter }

procedure TJavaImporter.ImportOneFile(const FileName : string);
var
  Str : TStream;
  Parser: TCodeParser;
begin
  Str := CodeProvider.LoadStream(FileName);
  if Assigned(Str) then
  begin
    Parser := TJavaParser.Create;
    try
      Parser.NeedPackage := NeedPackageHandler;
      Parser.ParseStream(Str, Model.ModelRoot, Model);
    finally
      Parser.Free;
    end;
  end;
end;

procedure TJavaImporter.NeedPackageHandler(const AName: string; var AStream: TStream; OnlyLookUp: Boolean = False);
var
  FileName: string;
begin
  AStream := nil;
  FileName := AName + '.java';
  FileName := CodeProvider.LocateFile(FileName);
  //Dont read same file twice
  if (not OnlyLookUp) and (FileName<>'') and (FilesRead.IndexOf(FileName)=-1) then
  begin
    AStream := CodeProvider.LoadStream(FileName);
    FilesRead.Add(FileName);
  end;
end;

class function TJavaImporter.GetFileExtensions: TStringList;
begin
  Result := TStringList.Create;
  Result.Values['.java'] := 'Java';
end;

{ TJavaParser }

constructor TJavaParser.Create;
begin
  inherited Create;
  ClassImports := TStringList.Create;
  FullImports := TStringList.Create;
  NameCache := TStringList.Create;
  NameCache.Sorted := True;
  NameCache.Duplicates := dupIgnore;
end;

destructor TJavaParser.Destroy;
begin
  inherited;
  if Assigned(FStream) then FreeAndNil(FStream);
  ClassImports.Free;
  FullImports.Free;
  NameCache.Free;
end;

function TJavaParser.SkipToken(const what: string): Boolean;
begin
  Result := False;
  GetNextToken;
  if Token = what then
  begin
    GetNextToken;
    Result := True;
  end;
end;

function TJavaParser.SkipPair(const open, close: string): Boolean;

  procedure InternalSkipPair(const open, close: string);
  begin
    while (Token <> close) and (Token<>'') do
    begin
      GetNextToken;
      while Token = open do
        InternalSkipPair(open, close);
    end;
    GetNextToken;
  end;

begin
  Result := False;
  InternalSkipPair(open, close);
  if Token <> '' then Result := True;
end;


procedure TJavaParser.EatWhiteSpace;
var
  inComment, continueLastComment, State: Boolean;

  procedure EatOne;
  begin
    if inComment then
      Comment := Comment + GetChar
    else
      GetChar;
  end;

  function EatWhite: Boolean;
  begin
    Result := False;
    while not (FCurrPos^ in [#0, #33..#255]) do
    begin
      Result := True;
      EatOne;
    end;
  end;

  function EatStarComment: Boolean;
  begin
    Result := True;
    while (not ((FCurrPos^ = '*') and ((FCurrPos + 1)^ = '/'))) or (FCurrPos^=#0) do
    begin
      Result := True;
      EatOne;
    end;
    continueLastComment := False;
    inComment := False;
    EatOne; EatOne;
  end;

  function EatSlashComment: Boolean;
  begin
    Result := True;
    while (FCurrPos^ <> #13) and (FCurrPos^ <> #10) and (FCurrPos^ <> #0) do
    begin
      Result := True;
      EatOne;
    end;
    continueLastComment := True;
    inComment := False;
    while FCurrPos^ in [#13,#10] do
      EatOne;
  end;

begin
  inComment := False;
  continueLastComment := False;
  State := True;
  while State do
  begin
    State := False;
    if (FCurrPos^ = #10) or ((FCurrPos^ = #13) and ((FCurrPos + 1)^ = #10)) then continueLastComment := False;
    if not (FCurrPos^ in [#0,#33..#255]) then State := EatWhite;
    if (FCurrPos^ = '/') and ((FCurrPos + 1)^ = '*') then
    begin
      Comment := '';
      EatOne; EatOne; // Skip slash star
      inComment := True;
      State := EatStarComment;
      inComment := False;
    end;
    if (FCurrPos^ = '/') and ((FCurrPos + 1)^ = '/') then
    begin
      if not continueLastComment then
        Comment := ''
      else
        Comment := Comment + #13#10;
      EatOne; EatOne; // Skip the double slashes
      inComment := True;
      State := EatSlashComment;
      inComment := False;
    end;
  end;
end;

function TJavaParser.GetNextToken: string;

  procedure AddOne;
  begin
    Token := Token + GetChar;
  end;

begin
//Hantera qualified id som en token
//'.' är en del av ett namn
//om första tecken efter namn är [ så hitta matchande ] och returnera som en token
//raise ifall end of file
  Token := '';

  EatWhiteSpace;

  if FCurrPos^ = '"' then // Parse String
  begin
    AddOne;
    while not (FCurrPos^ in ['"',#0]) do
    begin
      if ((FCurrPos^ = '\') and ((FCurrPos + 1)^ in ['"','\'])) then AddOne;
      AddOne;
    end;
    AddOne;
  end
  else if FCurrPos^ = '''' then // Parse char
  begin
    AddOne;
    while not (FCurrPos^ in ['''',#0]) do
    begin
      if ((FCurrPos^ = '\') and ((FCurrPos + 1)^ in ['''','\'])) then AddOne;
      AddOne;
    end;
    AddOne;
  end
  else if FCurrPos^ in ['A'..'Z', 'a'..'z', '_', '$'] then
  begin //Identifier
    AddOne;
    while True do
    begin
      while FCurrPos^ in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do AddOne;
      if FCurrPos^ = '.' then
      begin
        AddOne;
        Continue;
      end;
      Break;
    end;
    while FCurrPos^ in ['[', ']'] do AddOne;
  end
  else if FCurrPos^ in [';', '{', '}', '(', ')', ',', '='] then
  begin //Enskilda tecken som vi testar på
    AddOne;
  end
  else if FCurrPos^ = '[' then  //Lösa hakar
    while FCurrPos^ in ['[', ']'] do AddOne
  else //Allt annat, spola fram till whitespace eller intressant tecken
  begin
    while not (FCurrPos^ in [#0, #9, #10, #12, #13, #32, ',', '=', ';', '{', '}', '(', ')', '"', '''']) do AddOne;
    //**antagligen otillräckligt
  end;

  Result := Token;
end;

procedure TJavaParser.ParseStream(AStream: TStream; AModel: TAbstractPackage; AOM: TObjectModel);
begin
  if Assigned(FStream) then
    FreeAndNil(FStream);

  FStream := StreamToMemory(AStream);
  FCurrPos := FStream.Memory;

  FModel := AModel;
  FOM := AOM;

  ParseCompilationUnit;

//  ErrorHandler.Trace(FUnit.Name);
end;

(*
QualifiedIdentifier:
    Identifier { . Identifier }
*)

procedure TJavaParser.ParseModifiersOpt;
(*
ModifiersOpt:
    { Modifier }

Modifier:
    public
    protected
    private
    static
    abstract
    final
    native
    synchronized
    transient
    volatile
    strictfp
*)
begin
  //nollställ flaggor
  ModVisibility := viPublic;
  ModAbstract := False;
  while True do
  begin
    //Sätt flaggor baserat på visibility
    if Token = 'public' then
      ModVisibility := viPublic
    else if Token = 'protected' then
      ModVisibility := viProtected
    else if Token = 'private' then
      ModVisibility := viPrivate
    else if Token = 'abstract' then
      ModAbstract := True
    else if (Token = 'static') or (Token = 'final') or (Token = 'native') or
      (Token = 'synchronized') or (Token = 'transient') or (Token = 'volatile') or (Token = 'strictfp') then
    else
      Break;
    GetNextToken;
  end;
end;


procedure TJavaParser.ParseCompilationUnit;
(*
CompilationUnit:
 [package QualifiedIdentifier   ;  ]
        {ImportDeclaration}
        {TypeDeclaration}
*)
var
  UnitName: string;
  S : string;
begin
  GetNextToken;

  if Token = 'package' then
  begin
    UnitName := GetNextToken;
    SkipToken(';');
  end
  else
    UnitName := 'Default';

  FUnit := (FModel as TLogicPackage).FindUnitPackage(UnitName);
  if not Assigned(FUnit) then
    FUnit := (FModel as TLogicPackage).AddUnit(UnitName);

  while Token = 'import' do
  begin
    (*
     ImportDeclaration
        import Identifier {   .   Identifier } [   .     *   ] ;
    *)
    S := GetNextToken;
    if GetNextToken = '*' then
    begin
      FullImports.Add( ExtractPackageName(S) );
      GetNextToken;
    end
    else
    begin
      ClassImports.Values[ ExtractClassName(S) ] := ExtractPackageName(S);
//      NeedClassifier(S);
    end;
    GetNextToken;
  end;

  while Token<>'' do
    ParseTypeDeclaration;
end;


procedure TJavaParser.ParseTypeDeclaration;
(*
TypeDeclaration:
    ClassOrInterfaceDeclaration
    ;

ClassOrInterfaceDeclaration:
    ModifiersOpt (ClassDeclaration | InterfaceDeclaration)

InterfaceDeclaration:
    interface Identifier [extends TypeList] InterfaceBody
*)
begin
  ParseModifiersOpt;
  if Token = 'class' then
    ParseClassDeclaration
  else if Token = 'interface' then
    ParseInterfaceDeclaration
  else if Token = ';' then
    GetNextToken
  else
    //**error
//    raise Exception.Create('JavaParser error')
    GetNextToken
    ;
end;

procedure TJavaParser.ParseClassDeclaration(IsInner : boolean = False; const ParentName : string = '');
(*
ClassDeclaration:
    class Identifier [extends Type] [implements TypeList] ClassBody

ClassBody:
    { {ClassBodyDeclaration} }

ClassBodyDeclaration:
    ;
    [static] Block
    ModifiersOpt MemberDecl

MemberDecl:
    MethodOrFieldDecl
    void Identifier MethodDeclaratorRest
    Identifier ConstructorDeclaratorRest
    ClassOrInterfaceDeclaration

MethodOrFieldDecl:
    Type Identifier MethodOrFieldRest

MethodOrFieldRest:
    VariableDeclaratorRest
    MethodDeclaratorRest
*)
var
  C: TMdlClass;
  Int: TMdlInterface;
  TypeName, Ident: string;
begin
  GetNextToken;
  C := FUnit.AddClass(Token);
  SetVisibility(C);
  GetNextToken;

  if Token = 'extends' then
  begin
    C.Ancestor := NeedClassifier(GetNextToken, True, TMdlClass) as TMdlClass;
    GetNextToken;
  end;

  if Token = 'implements' then
  begin
    repeat
      Int := NeedClassifier(GetNextToken, True, TMdlInterface) as TMdlInterface;
      if Assigned(Int) then
        C.AddImplements(Int);
      GetNextToken;
    until Token <> ',';
  end;

  if Token = '{' then
  begin
    GetNextToken;
    while True do
    begin
      ParseModifiersOpt;
      if Token = '{' then
        //Static initializer
        SkipPair('{', '}')
      else if Token = ';' then
        //ensamt semikolon
        GetNextToken
      else if Token = 'class' then
        //Inner class
        ParseClassDeclaration(True,C.Name)
      else if Token = 'interface' then
        //Inner interface
        ParseInterfaceDeclaration
      else if (Token = '}') or (Token='') then
      begin
        //Slut på klassdeklaration
        GetNextToken;
        Break;
      end
      else
      begin
        //Måste vara typnamn för attr eller operation
        //Eller konstruktor
        TypeName := GetTypeName;
        if (TypeName = C.Name) and (Token = '(') then
        begin
          Ident := TypeName; //Konstruktor
          TypeName := '';
        end
        else
        begin
          Ident := Token;
          GetNextToken;
        end;
        if Token = '(' then
        begin
          //Operation
          DoOperation(C.AddOperation(Ident), C.Name, TypeName);
          GetNextToken; //')'
          //Skippa ev Throws
          while (Token<>';') and (Token <> '{') and (Token <> '') do
            GetNextToken;
          //Antingen ; för abstract metod eller { för body
          if Token='{' then
            SkipPair('{', '}');
        end
        else
        begin
          //Attributes
          DoAttribute(C.AddAttribute(Ident), TypeName);
          while Token = ',' do
          begin
            DoAttribute(C.AddAttribute(GetNextToken), TypeName);
            GetNextToken;
          end;
          Comment := '';
        end;
      end;
    end;
  end;

  //**Räcker detta?
  //Parent läggs till sist för att konstruktorer etc skall fungera
  if IsInner then
    C.Name := ParentName + '.' + C.Name;
end;

procedure TJavaParser.ParseInterfaceDeclaration;
(*
InterfaceDeclaration:
	interface Identifier [extends TypeList] InterfaceBody

InterfaceBody:
	{ {InterfaceBodyDeclaration} }

InterfaceBodyDeclaration:
	;
	ModifiersOpt InterfaceMemberDecl

InterfaceMemberDecl:
	InterfaceMethodOrFieldDecl
	void Identifier VoidInterfaceMethodDeclaratorRest
	ClassOrInterfaceDeclaration

InterfaceMethodOrFieldDecl:
	Type Identifier InterfaceMethodOrFieldRest

InterfaceMethodOrFieldRest:
	ConstantDeclaratorsRest ;
	InterfaceMethodDeclaratorRest

InterfaceMethodDeclaratorRest:
	FormalParameters BracketsOpt [throws QualifiedIdentifierList]   ;

VoidInterfaceMethodDeclaratorRest:
	FormalParameters [throws QualifiedIdentifierList]   ;
*)
var
  Int: TMdlInterface;
  TypeName, Ident: string;
begin
  GetNextToken;
  Int := FUnit.AddInterface(Token);
  SetVisibility(Int);
  GetNextToken;

  if Token = 'extends' then
  begin
    Int.Ancestor := NeedClassifier(GetNextToken, True, TMdlInterface) as TMdlInterface;
    //**limitation: an java interface can extend several interfaces, but our model only support one ancestor
    GetNextToken;
    while Token=',' do
    begin
      GetNextToken;
      GetNextToken;
    end;
  end;

  if Token = '{' then
  begin
    GetNextToken;
    while True do
    begin
      ParseModifiersOpt;
      if Token = ';' then
        //empty
        GetNextToken
      else if Token = 'class' then
        //Inner class
        ParseClassDeclaration
      else if Token = 'interface' then
        //Inner interface
        ParseInterfaceDeclaration
      else if (Token = '}')  or (Token='') then
      begin
        //End of interfacedeclaration
        GetNextToken;
        Break;
      end
      else
      begin
        //Must be type of attr or return type of operation
        TypeName := GetTypeName;
        Ident := Token;
        if GetNextToken = '(' then
        begin
          //Operation
          DoOperation(Int.AddOperation(Ident), Int.Name, TypeName);
          GetNextToken;
          //Skip Throws if present
          while (Token<>';') and (Token <> '') do
            GetNextToken;
        end
        else
        begin
          DoAttribute(Int.AddAttribute(Ident) , TypeName);
          while Token = ',' do
          begin
            DoAttribute(Int.AddAttribute(GetNextToken), TypeName);
            GetNextToken;
          end;
          Comment := '';
        end;
      end;
    end;
  end;
end;



function TJavaParser.NeedClassifier(const CName: string; Force :  boolean = True; TheClass: TModelEntityClass = nil): TClassifier;
var
  PName,ShortName : string;
  CacheI : integer;

  function InLookInModel : TClassifier;
  var
    U : TUnitPackage;
    I : integer;
  begin
    Result := nil;
    if PName='' then
    //Inget packagenamn, kolla i aktuell unit samt imports
    begin
      //Classimports ( java.util.HashTable )
      for I := 0 to ClassImports.Count-1 do
        //Kan ej göra indexofname pga casesensetivity
        if ClassImports.Names[I]=ShortName then
        begin
          Result := NeedClassifier( ClassImports.Values[ShortName] + '.' + ShortName, False, TheClass );
          if Assigned(Result) then
            Break;
        end;
      //Fullimports ( java.util.* )
      if not Assigned(Result) then
      begin
        for I := 0 to FullImports.Count-1 do
        begin
          Result := NeedClassifier( FullImports[I] + '.' + ShortName, False, TheClass );
          if Assigned(Result) then
            Break;
        end;
      end;
      //Kolla i aktuell unit
      if not Assigned(Result) then
        Result := FUnit.FindClassifier(ShortName,False,TheClass,True);
    end
    else
    //Packagenamn angivet, leta efter package
    begin
      U := FOM.ModelRoot.FindUnitPackage(PName);
      if not Assigned(U) then
        //Försök hitta shortname.java fil i alla kända sökvägar
        //Leta sedan i model på nytt
        //**Ej tillräckligt, hittar tex List.java först i awt när det är List.java i util som behövs
        //**Borde iterera alla .java filer som har heter shortname
        if NeedSource(ShortName) then
          U := FOM.ModelRoot.FindUnitPackage(PName);
      if Assigned(U) then
        Result := U.FindClassifier(ShortName,False,TheClass,True);
    end;
  end;

begin
  //Allra först titta i cache över namn vi redan kört lookup på
  //Optimering som sparar mycket tid när man läser in stora projekt
  CacheI := NameCache.IndexOf(CName);
  if (CacheI<>-1) and (NameCache[CacheI]=CName) and ((TheClass=nil) or (NameCache.Objects[CacheI] is TheClass)) then
   //Stringlist indexof är ej casesensitive så vi måste göra det igen
  begin
    Result := TClassifier(NameCache.Objects[CacheI]);
    Exit;
  end;

  PName := ExtractPackageName(CName);
  ShortName := ExtractClassName(CName);

  //Kolla i modellen
  Result := InLookInModel;

  //Annars se om vi hittar den fil vi behöver
  if not Assigned(Result) then
    if NeedSource(ShortName) then
      Result := InLookInModel;

  if not Assigned(Result) then
  begin
    //Leta i unknown
    Result := FOM.UnknownPackage.FindClassifier(CName,False,TheClass,True);
    if Force and (not Assigned(Result)) then
    begin
      //Saknas, skapa i unknown (om Force)
      if (TheClass=nil) or (TheClass=TMdlClass) then
        Result := FOM.UnknownPackage.AddClass(CName)
      else if TheClass=TMdlInterface then
        Result := FOM.UnknownPackage.AddInterface(CName)
      else if TheClass=TDataType then
        Result := FOM.UnknownPackage.AddDataType(CName)
    end;
  end;

  if Assigned(Result) and (CacheI=-1) then
    NameCache.AddObject(CName,Result);

  if Force and (not Assigned(Result)) then
    raise Exception.Create(ClassName + ' failed to locate ' + Cname);
end;



//Sätt visibility baserat på flaggor som parseOptModifier tilldelat

procedure TJavaParser.SetVisibility(M: TModelEntity);
begin
  M.Visibility := ModVisibility;
  if ModAbstract and (M is TMdlOperation) then
    (M as TMdlOperation).IsAbstract := ModAbstract;
end;

function TJavaParser.GetChar: char;
begin
  Result := FCurrPos^;
  if Result<>#0 then
    Inc(FCurrPos);
end;

procedure TJavaParser.DoOperation(O: TMdlOperation; const ParentName, TypeName: string);
var
  ParType: string;
begin
  SetVisibility(O);
  if (TypeName <> '') and (TypeName <> 'void') then
    O.ReturnValue := NeedClassifier(TypeName);
  if Assigned(O.ReturnValue) then
    O.OperationType := otFunction
  else if ParentName = O.Name then
    O.OperationType := otConstructor
  else
    O.OperationType := otProcedure;
  //Parametrar
  GetNextToken;
  while (Token<>'') and (Token <> ')') do
  begin
    if Token = 'final' then
      GetNextToken;
    ParType := GetTypeName;
    O.AddParameter(Token).TypeClassifier := NeedClassifier(ParType);
    GetNextToken;
    if Token=',' then
      GetNextToken;
  end;
  O.Documentation.Description := Comment;
  Comment := '';
end;

procedure TJavaParser.DoAttribute(A: TAttribute; const TypeName: string);
begin
  SetVisibility(A);
  if Token = '=' then
    while (Token <> ';') and (Token<>'') do
    begin
      GetNextToken;
      //Attribute initializer kan innehålla hela inner classdeklarationer
      if Token='{' then
        SkipPair('{','}');
    end;
  A.TypeClassifier := NeedClassifier(TypeName);
  A.Documentation.Description := Comment;
  if Token=';' then
    GetNextToken;
end;

//Hanterar att typnamn kan följas av ett [] som kommer efteråt
function TJavaParser.GetTypeName: string;
(*
Type:
	Identifier {   .   Identifier } BracketsOpt
	BasicType

*)
begin
  Result := Token;
  GetNextToken;
  if (Length(Token)>0) and (Token[1]='[') then
  begin
    Result := Result + Token;
    GetNextToken;
  end;
end;


//Anropar needpackage.
//Obs att 'package' i javaparser avser en .java-fil.
function TJavaParser.NeedSource(const SourceName: string): boolean;
var
  Str : TStream;
  Parser : TJavaParser;
begin
  Result := False;
  if Assigned(NeedPackage) then
  begin
    NeedPackage(SourceName,Str);
    if Assigned(Str) then
    begin
      Parser := TJavaParser.Create;
      try
        Parser.NeedPackage := NeedPackage;
        Parser.ParseStream(Str, FOM.ModelRoot, FOM);
      finally
        Parser.Free;
      end;
      Result := True;
    end;
  end;
end;


initialization

  Integrators.Register(TJavaImporter);

end.