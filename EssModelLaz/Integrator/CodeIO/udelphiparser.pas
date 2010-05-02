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
  Implementation of the Delphi parser.

  Known issues

  - The parser now parses each unit from the start to the end. It should really
    first parse the interface part of all units and then parse the
    implementation part of the units.

  - In some cases the parser is order dependent with regards to wich files it can
    find.
    Possible solution would be to allow for external means of supplying a
    searchpath for files the codeprovider cannot find, but that can easily become
    cumbersome and/or unconvenient.

  bugs arising from the above mentioned issues
    delphiparser
      uplayer ärver ifrån unknown::sprite i dduo trots att tsprite finns
        detta är pga att egentligen borde interface delar av alla used units parsas först
        sedan impl delarna
      tdelphiintegrator ärver unknown i essmodel trots att tcodeintegrator finns
        detta är för att när parser är på delphiintegrator så har den inte sökvägen till codeintegrator
        när den tar uses i .dpr filen så ligger sökvägen med
}
unit uDelphiParser;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses
  Classes, LCLIntf, LCLType, Dialogs, SysUtils,
  uCodeParser, uModel, uModelEntity;

type

  TDelphiParser = class(TCodeParser)
  private
    FStream: TMemoryStream;
    FCurrPos: PChar;
    FToken: string;
    FNextToken: string;

    FMarkCurrPos: PChar;
    FMarkToken: string;
    FMarkNextToken: string;

    //FThisNode: TDelphiParseTree;
    //FNextNode: TDelphiParseTree;
    //FParseTreeRoot: TDelphiParseTree;
    //FParsed: TDelphiParseTree;
    FOM: TObjectModel;
    FUnit: TUnitPackage;

    FGlobalDefines: TStringList;
    FLocalDefines: TStringList;
    {$IFNDEF Release}
    DebugLog : TStringList;
    {$ENDIF}

    FComment: String;  // Accumulated comment string used for documentation of entities.

    function SkipToken(const what: string): Boolean;
    function SkipPair(const open, close: string): Boolean;
    function GetLowerCaseToken: string;

//    procedure IndentAfter;
//    procedure UndentAfter;

    function IsCallingConvention(const s: string): Boolean;
    function IsFunctionDecoration(const s: string): Boolean;
    function IsHintDirective(const S : string) : boolean;

    procedure LocalDefine(const symbol: String );
    procedure LocalUndefine(const symbol: String );
    function IsDefined(const symbol: String ): Boolean;

    procedure Mark;
    procedure Recall;
  protected
    procedure EatWhiteSpace;
    function GetNextToken: Boolean;

    procedure ParseProgram;
    procedure ParseLibrary;
    procedure ParseUnit;

    procedure ParseProgramBlock;
    procedure ParseInterfaceSection;
    procedure ParseImplementation;

    procedure ParseUses(Visibility: TVisibility = viPublic; Recurse: Boolean = True);
    procedure ParseLabelSection;
    procedure ParseTypeSection(Visibility: TVisibility = viPublic);
    procedure ParseConstSection;
    procedure ParseVarSection;
    procedure ParseResourcestringSection;

    procedure ParseClass(AClass: TMdlClass);
    procedure ParseInterface(AClass: TMdlInterface);
    procedure ParseRecord(ARecord: TDataType; DoSkip : boolean = True);

    procedure ParseFunction(AOperation: TMdlOperation; Visibility: TVisibility = viPublic);
    procedure ParseFunctionImplementation;
    procedure ParseParamList(AOperation: TMdlOperation);
    procedure ParseReturnType(AOperation: TMdlOperation);
    procedure ParseAttribute(AAttribute: TAttribute; Visibility: TVisibility = viPublic);
    procedure ParseProperty(AProperty: TProperty; Visibility: TVisibility = viPublic);

    function ParseType: TClassifier;

  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseStream(AStream: TStream; AModel: TAbstractPackage; AOM: TObjectModel); override;
    procedure ParseStreamWithDefines(AStream: TStream; AModel: TAbstractPackage; AOM: TObjectModel; const GlobalDefines: TStringList );

    property Token: string read FToken;
    property NextToken: string read FNextToken;
    property lToken: string read GetLowerCaseToken;
  end;

implementation

uses uError;

{ TDelphiParser }

constructor TDelphiParser.Create;
begin
  inherited;
  FLocalDefines := TStringList.Create;
  {$IFNDEF Release}
  DebugLog := TStringList.Create;
  {$ENDIF}
end;

destructor TDelphiParser.Destroy;
begin
  inherited;
  FreeAndNil(FLocalDefines);
  if Assigned(FStream) then FreeAndNil(FStream);
  {$IFNDEF Release}
  DebugLog.Free;
  {$ENDIF}
end;

function TDelphiParser.IsCallingConvention(const S: string): Boolean;
begin
  //'local', 'varargs' new in Kylix/Delphi 6
  Result := (s = 'register') or (s = 'pascal') or (s = 'cdecl') or
    (s = 'stdcall') or (s = 'safecall') or (s = 'assembler') or (s = 'export') or
    (s = 'local') or (s = 'varargs');
end;

function TDelphiParser.IsFunctionDecoration(const s: string): Boolean;
begin
  Result := (s = 'overload') or (s = 'near') or (s = 'far');
end;

function TDelphiParser.GetLowerCaseToken: string;
begin
   { TODO : We shoudl be able to optimise this. It is called often. }
  Result := LowerCase(Token);
end;

function TDelphiParser.SkipToken(const what: string): Boolean;
begin
  Result := False;
  GetNextToken;
  if LowerCase(Token) = LowerCase(what) then
  begin
    GetNextToken;
    Result := True;
  end;
end;

function TDelphiParser.SkipPair(const open, close: string): Boolean;
  procedure InternalSkipPair(open, close: string);
  begin
    while GetNextToken and (lToken <> close) do
      if lToken = open then
        InternalSkipPair(open, close)
      else
        { TODO : Find a better way to manage that 'end' can be started in several ways. }
        if open = 'begin' then
        begin
          if (lToken = 'try') or (lToken = 'case') or (lToken = 'asm') then
            InternalSkipPair(open, close);
        end;
  end;
begin
  Result := False;
  if lToken <> close then
    InternalSkipPair(open, close);
  GetNextToken;
  if Token <> '' then Result := True;
end;

procedure TDelphiParser.EatWhiteSpace;
type
  PPEnum = (PP_EOF, PP_IF, PP_IFDEF, PP_IFNDEF, PP_IFOPT,
    PP_DEFINE, PP_UNDEF, PP_ELSE, PP_ELSEIF, PP_ENDIF, PP_IFEND, PP_INCLUDE, PP_UNKNOWN);
  PPEnumSet = set of PPEnum;
var
  inComment, continueLastComment, State: Boolean;

  procedure EatOne;
  begin
    if inComment then
      FComment := FComment + FCurrPos^;
    inc(FCurrPos);
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

  function EatBraceComment: Boolean;
  begin
    Result := True;
    while not (FCurrPos^ in [#0,'}']) do
    begin
      Result := True;
      EatOne;
    end;
    continueLastComment := False;
    inComment := False;
    if FCurrPos^ <> #0 then
      EatOne;
  end;

  function EatParenComment: Boolean;
  begin
    Result := True;
    while not ( (FCurrPos^ = '*') and ((FCurrPos + 1)^ = ')') ) and (FCurrPos^ <> #0) do
    begin
      Result := True;
      EatOne;
    end;
    continueLastComment := False;
    inComment := False;
    if FCurrPos^ <> #0 then
    begin
      EatOne;
      EatOne;
    end;
  end;

  function EatSlashComment: Boolean;
  begin
    Result := True;
    while not (FCurrPos^ in [#13,#10,#0]) do
    begin
      Result := True;
      EatOne;
    end;
    continueLastComment := True;
    inComment := False;
  end;

  procedure HandleInclude(const IncludeFile : string);
  var
    IncStr : TStream;
    OffSet : integer;
    Buf : TMemoryStream;
  begin
    if Assigned(NeedPackage) then
    begin
      NeedPackage(IncludeFile, IncStr);
      if Assigned(IncStr) then
      begin
        Buf := TMemoryStream.Create;
        try
          Offset := integer(FCurrPos) - integer(FStream.Memory);

          Buf.Write(FStream.Memory^,Offset);
          Buf.CopyFrom(IncStr,IncStr.Size);

          FStream.Position := Offset;
          Buf.CopyFrom(FStream,FStream.Size - Offset);

          FStream.LoadFromStream(Buf);
          FStream.Position := 0;
          FCurrPos := PCHar(integer(FStream.Memory) + Offset);
        finally
          IncStr.Free;
          Buf.Free;
        end;
      end;
    end;
  end;

  function EvalPPExpression(const E : string) : boolean;
  begin
    Result := True;
  end;

  function GetPPExp : string;
  begin
    Result := '';
    while True do
    begin
      if FCurrPos^ in [#0,'}'] then
        Break;
      if FCurrPos^>#31 then
        Result := Result + FCurrPos^;
      Inc(FCurrPos);
    end;
    Result := LowerCase( Trim(Result) );
    if FCurrPos^='}' then
      Inc(FCurrPos);
  end;

  function GetNextPP : PPEnum;
  var
    S : string;
  begin
    Result := PP_EOF;
    while True do
    begin
      case FCurrPos^ of
        #0 : Break;
        '(' :
          if ((FCurrPos + 1)^ = '*') then
            EatParenComment
          else
            Inc(FCurrPos);
        '/' :
          if ((FCurrPos + 1)^ = '/') then
            EatSlashComment
          else
            Inc(FCurrPos);
        '''' :
          begin
            Inc(FCurrPos);
            while not (FCurrPos^ in ['''', #10, #13]) or ((FCurrPos^ = '''') and ((FCurrPos + 1)^ = '''')) do
            begin
              if ((FCurrPos^ = '''') and ((FCurrPos + 1)^ = '''')) then
                Inc(FCurrPos);
              Inc(FCurrPos);
            end;
            Inc(FCurrPos);
          end;
        '{' :
          if ((FCurrPos+1)^ = '$') then
          begin
            Inc(FCurrPos,2);
            S := '';
            while not (FCurrPos^ in [#0,#9,#32,#10,#13,'}']) do
            begin
              S := S + FCurrPos^;
              Inc(FCurrPos);
            end;
            if FCurrPos^<>#0 then
            begin
              S := LowerCase(S);
              if      S='if'    then Result := PP_IF
              else if S='ifdef' then Result := PP_IFDEF
              else if S='ifndef' then Result := PP_IFNDEF
              else if S='ifopt'  then Result := PP_IFOPT
              else if S='define' then Result := PP_DEFINE
              else if S='undef'  then Result := PP_UNDEF
              else if S='else'   then Result := PP_ELSE
              else if S='elseif' then Result := PP_ELSEIF
              else if S='endif'  then Result := PP_ENDIF
              else if S='ifend'  then Result := PP_IFEND
              else if S='include'  then Result := PP_INCLUDE
              else Result := PP_UNKNOWN;
            end;
            Break;
          end
          else
            EatBraceComment;
      else
        Inc(FCurrPos);
      end;
    end;
  end;

  function FindMatchingPP(const EndSet : PPEnumSet) : PPEnum;
  var
    Nest : integer;
  begin
    Result := PP_EOF;
    Nest:=0;
    while True do
    begin
      Result := GetNextPP;
      if (Nest=0) and (Result in EndSet) then
        Break;
      case Result of
        PP_EOF :
          Break;
        PP_IF, PP_IFDEF, PP_IFNDEF, PP_IFOPT :
          Inc(Nest);
        PP_IFEND, PP_ENDIF :
          if Nest>0 then Dec(Nest);
      end;
    end;
  end;

  procedure SkipToElseOrEndif;
  begin
    while True do
      case FindMatchingPP( [PP_ELSE, PP_ELSEIF, PP_ENDIF, PP_IFEND] ) of
        PP_ELSEIF :
          if EvalPPExpression( GetPPExp ) then
            Break;
        PP_EOF, PP_ELSE, PP_IFEND, PP_ENDIF :
          begin
            EatBraceComment;
            Break;
          end;
      end;
  end;

begin
  inComment := False;
  continueLastComment := False;
  State := True;
  while State do
  begin
    State := False;

    if (FCurrPos^ = #13) and ((FCurrPos + 1)^ = #10) then
      continueLastComment := False;

    if not (FCurrPos^ in [#33..#255]) then
      State := EatWhite;

    if (FCurrPos^ = '{') and ((FCurrPos + 1)^ = '$') then
    begin
      // Handle compiler directive
      case GetNextPP of
        PP_EOF : Break;
        PP_IF :
          if not EvalPPExpression( GetPPExp ) then
            SkipToElseOrEndif;
        PP_IFDEF :
          if not IsDefined( GetPPExp ) then
            SkipToElseOrEndif;
        PP_IFNDEF :
          if IsDefined( GetPPExp ) then
            SkipToElseOrEndif;
        PP_DEFINE :
          LocalDefine( GetPPExp );
        PP_UNDEF :
          LocalUnDefine( GetPPExp );
        PP_ELSE, PP_ELSEIF :
          //If we get here it means we have parsed the 'then' part and should skip
          //the else part
          begin
            FindMatchingPP( [PP_ENDIF, PP_IFEND] );
            EatBraceComment;
          end;
        PP_INCLUDE :
          HandleInclude( GetPPExp );
      else
        //PP_IFOPT, PP_ENDIF, PP_IFEND
        EatBraceComment; //Ignore
      end;
      State := True;
    end
    else if FCurrPos^ = '{' then
    begin
      FComment := '';
      EatOne; // Skip the curlybrace
      inComment := True;
      State := EatBraceComment;
      inComment := False;
    end;

    if (FCurrPos^ = '(') and ((FCurrPos + 1)^ = '*') then
    begin
      FComment := '';
      EatOne; EatOne; // Skip paren star
      inComment := True;
      State := EatParenComment;
      inComment := False;
    end;

    if (FCurrPos^ = '/') and ((FCurrPos + 1)^ = '/') then
    begin
      if not continueLastComment then
        FComment := ''
      else
        FComment := FComment + #13#10;
      EatOne; EatOne; // Skip the double slashes
      inComment := True;
      State := EatSlashComment;
      inComment := False;
    end;

  end;
end;

function TDelphiParser.GetNextToken: Boolean;

  procedure AddOne;
  begin
    FNextToken := FNextToken + FCurrPos^;
    Inc(FCurrPos);
  end;

begin
  FToken := FNextToken;
  FNextToken := '';
  EatWhiteSpace;

  case FCurrPos^ of
    #0 : // End of File
      begin
        if FToken <> '' then
          Result := True
        else
          Result := False;
        Exit;
      end;
    '''' : // Parse String
      begin
        AddOne;
        while not (FCurrPos^ in ['''', #10, #13]) or ((FCurrPos^ = '''') and ((FCurrPos + 1)^ = '''')) do
        begin
          if ((FCurrPos^ = '''') and ((FCurrPos + 1)^ = '''')) then AddOne;
          AddOne;
        end;
        AddOne;
      end;
    '"' : // Parse String (asm-statements can have " strings)
      begin
        AddOne;
        while not (FCurrPos^ in ['"', #10, #13]) or ((FCurrPos^ = '"') and ((FCurrPos + 1)^ = '"')) do
        begin
          if ((FCurrPos^ = '"') and ((FCurrPos + 1)^ = '"')) then AddOne;
          AddOne;
        end;
        AddOne;
      end;
    '#' : // Parse char
      begin
        AddOne;
        while FCurrPos^ in ['0'..'9'] do AddOne;
      end;
    '$' : // Parse hexnum
      begin
        AddOne;
        while FCurrPos^ in ['0'..'9', 'A'..'F', 'a'..'f'] do AddOne;
      end;
    ':' :
      begin
        AddOne;
        if FCurrPos^ = '=' then AddOne;
      end;
    '+', '-', '0'..'9' : // Parse numerics
      begin
        AddOne;
        while FCurrPos^ in ['0'..'9'] do AddOne;
        if ((FCurrPos - 1)^ in ['0'..'9']) and (FCurrPos^ = '.') then
        begin
          AddOne;
          while FCurrPos^ in ['0'..'9'] do AddOne;
        end;
        if ((FCurrPos - 1)^ in ['0'..'9']) and (FCurrPos^ in ['e', 'E']) then
        begin
          AddOne;
          if FCurrPos^ in ['-', '+'] then AddOne;
          while FCurrPos^ in ['0'..'9'] do AddOne;
        end;
      end;
    '<', '>' :
      begin
        if (FCurrPos + 1)^ = '=' then
        begin
          AddOne; AddOne;
        end
        else if (FCurrPos^ = '<') and ((FCurrPos + 1)^ = '>') then
        begin
          AddOne; AddOne;
        end
        else AddOne;
      end;
    '(', ')', '[', ']', ';', ',', '.', '^', '*', '/', '=' :
      begin
        if (FCurrPos^ = '.') and ((FCurrPos + 1)^ = '.') then AddOne;
        AddOne;
      end;
    'A'..'Z', 'a'..'z', '_', '@' :
      begin
        AddOne;
        while FCurrPos^ in ['A'..'Z', 'a'..'z', '0'..'9', '_', '@'] do AddOne;
      end;
  else
    raise EParseError.Create(FUnit.Name + ': Unhandled parser state character:'+FCurrPos^);
  end;
  {$IFNDEF Release}
  DebugLog.Add(FNextToken);
  {$ENDIF}
  Result := True;
end;

procedure TDelphiParser.ParseStream(AStream: TStream; AModel: TAbstractPackage; AOM: TObjectModel);
begin
  ParseStreamWithDefines(AStream, AModel, AOM, nil);
end;


procedure TDelphiParser.ParseStreamWithDefines(AStream: TStream; AModel: TAbstractPackage; AOM: TObjectModel; const GlobalDefines: TStringList );
begin
  {$IFNDEF Release}
  try
  {$ENDIF}
    FGlobalDefines := GlobalDefines;
    if Assigned(FGlobalDefines) then
      FLocalDefines.Assign(FGlobalDefines);

    if Assigned(FStream) then FreeAndNil(FStream);
    FStream := StreamToMemory(AStream);
    FCurrPos := FStream.Memory;

    FModel := AModel;
    FOM := AOM;
    GetNextToken;

    GetNextToken;
    if lToken = 'program' then
      ParseProgram
    else if lToken = 'library' then
      ParseLibrary
    else if lToken = 'unit' then
      ParseUnit;
  {$IFNDEF Release}
  except
    on E : EParseError do
    begin
      DebugLog.SaveToFile( 'c:\essModelDump.txt' );
      raise;
    end;
  end;
  {$ENDIF}
end;

procedure TDelphiParser.ParseLibrary;
begin
  if GetNextToken then
  begin
    FUnit := (FModel as TLogicPackage).AddUnit(Token);
    FUnit.Documentation.Description := FComment;
    FComment := '';
    SkipToken(';');
    if lToken = 'uses' then
      ParseUses;
  end;
end;

procedure TDelphiParser.ParseProgram;
begin
  if GetNextToken then
  begin
    FUnit := (FModel as TLogicPackage).AddUnit(Token);
    FUnit.Documentation.Description := FComment;
    FComment := '';
    SkipToken(';');
    if lToken = 'uses' then
    begin
      Mark;
      ParseUses(viPublic,False);
      Recall;
      ParseUses;
    end;
    ParseProgramBlock;
  end;
end;

procedure TDelphiParser.ParseProgramBlock;
begin
  while (lToken <> 'begin') and (lToken <> 'asm') and (Token <> '') do
  begin
    if lToken = 'label' then
      ParseLabelSection
    else if lToken = 'type' then
      ParseTypeSection(viPrivate)
    else if lToken = 'const' then
      ParseConstSection
    else if lToken = 'var' then
      ParseVarSection
    else if lToken = 'resourcestring' then
      ParseResourcestringSection
    else if (lToken = 'procedure') or (lToken = 'function') or
      (lToken = 'constructor') or (lToken = 'destructor')
      or (lToken = 'class') {Implementation of class functions} then
    begin
      if (lToken = 'class') then GetNextToken;
      ParseFunctionImplementation;
    end
    else
      raise EParseError.Create(FUnit.Name + ': Parseprogramblock: Token:' + Token + ' Next:' + NextToken+' Context:'+Copy(FCurrPos,-100,200));
  end;
  if (lToken = 'begin') or (lToken = 'asm') then
  begin
    SkipPair('begin', 'end');
  end;
   // Token should now be   ; or .
end;


procedure TDelphiParser.ParseUnit;
var
  uName: string;
begin
  if GetNextToken then
  begin
    uName := Token;
    FUnit := (FModel as TLogicPackage).AddUnit(uName);
    FUnit.Documentation.Description := FComment;
    FComment := '';
    SkipToken(';');

    if lToken = 'interface' then
      ParseInterfaceSection;
    if lToken = 'implementation' then
      ParseImplementation;
      { TODO : finalization initialization end. }
  end;
end;

procedure TDelphiParser.ParseImplementation;
begin
  if GetNextToken then
  begin
    if lToken = 'uses' then
      ParseUses(viPrivate);
    while (lToken <> 'finalization') and (lToken <> 'initialization') and
      (lToken <> 'begin') and (lToken <> 'end') do
    begin
      if lToken = 'type' then
        ParseTypeSection(viPrivate)
      else if lToken = 'const' then
        ParseConstSection
      else if (lToken = 'var') or (lToken = 'threadvar') then
        ParseVarSection
      else if lToken = 'resourcestring' then
        ParseResourcestringSection
      else if (lToken = 'procedure') or (lToken = 'function') or
        (lToken = 'constructor') or (lToken = 'destructor')
        or (lToken = 'class') {Implementation of class functions} then
      begin
        if (lToken = 'class') then GetNextToken;
        ParseFunctionImplementation;
      end
      else
        raise EParseError.Create(FUnit.Name + ' : ParseImplementation: Token:' + Token + ' Next:' + NextToken);
    end;
  end;
end;

procedure TDelphiParser.ParseInterfaceSection;
begin
  if GetNextToken then
  begin
    if lToken = 'uses' then
      ParseUses;
    while lToken <> 'implementation' do
    begin
      if lToken = 'type' then
        ParseTypeSection(viPublic)
      else if lToken = 'const' then
        ParseConstSection
      else if lToken = 'var' then
        ParseVarSection
      else if lToken = 'resourcestring' then
        ParseResourcestringSection
      else if (lToken = 'procedure') or (lToken = 'function') then
      begin
            // Skip forward declaration
        while GetNextToken and (Token <> ';') do
          if Token = '(' then
          begin
            SkipPair('(', ')');
            if Token = ';' then break;
          end;
        GetNextToken;
        //'forward' is valid but not neccessary
        while IsCallingConvention(lToken) or IsFunctionDecoration(lToken) or IsHintDirective(lToken) or (lToken = 'forward') do
          if GetNextToken and (Token = ';') then GetNextToken;
      end
      else
        raise EParseError.Create(FUnit.Name + ' : Parse Interface Section: Token:' + Token + ' Next:' + NextToken);
    end;
  end;
end;

procedure TDelphiParser.ParseUses(Visibility: TVisibility = viPublic; Recurse: Boolean = True);
var
  str: TStream;
  prs: TDelphiParser;
  uName, fName: string;
  p: PChar;
begin
  if GetNextToken then
  begin
    while Token <> ';' do
    begin
      uName := Token;

      if LowerCase(NextToken) = 'in' then
        SkipToken('in');

      fName := Token;
      p := PChar(fName);
      fName := AnsiExtractQuotedStr(p, ''''); ;
      if fName = '' then fName := uName;

      if Assigned(NeedPackage) and (FOM.ModelRoot.FindUnitPackage(uName) = nil) then
      begin
        NeedPackage(fName, str, not Recurse);
        if Assigned(str) and Recurse then
        begin
          prs := TDelphiParser.Create;
          prs.NeedPackage := NeedPackage;
          try
            try
              prs.ParseStreamWithDefines(str, FModel, FOM, FGlobalDefines);
            except
              on E : EParseError do
                ShowMessage(E.Message);
            end;
            { TODO : Keep the parsed list somewhere to be able to implement
                     a twoway integrator }
          finally
            FreeAndNil(prs);
          end;
        end;
      end;
      if (FOM.ModelRoot.FindUnitPackage(uName) <> nil) and Recurse then
        FUnit.AddUnitDependency(FOM.ModelRoot.FindUnitPackage(uName),Visibility);
      SkipToken(',');
    end;
    GetNextToken;
  end;
end;

procedure TDelphiParser.ParseLabelSection;
begin // Token = label
  while GetNextToken and (Token <> ';') do ;
  GetNextToken; // Skip ;
end;

procedure TDelphiParser.ParseConstSection;
begin
  if GetNextToken then
  begin
    while (NextToken = '=') or (NextToken = ':') do
    begin
      while GetNextToken and (Token <> ';') do
      begin
        if lToken = '(' then SkipPair('(', ')');
        if lToken = 'record' then ParseRecord(nil);
        if Token = ';' then break;
      end;
      GetNextToken;
    end;
  end;
end;

procedure TDelphiParser.ParseResourcestringSection;
begin
  if GetNextToken then
  begin
    while NextToken = '=' do
    begin
      while GetNextToken and (Token <> ';') do ;
      GetNextToken;
    end;
  end;
end;


procedure TDelphiParser.ParseTypeSection(Visibility: TVisibility);
var
  tName: string;
  ex: TClassifier;
begin
  GetNextToken;
  while NextToken = '=' do
  begin
    tName := Token;
    GetNextToken; GetNextToken;
    if lToken = 'packed' then GetNextToken; { TODO : Add packed to teh model? }

    if lToken = 'class' then // Handle a class declaration
    begin
      if LowerCase(NextToken) <> 'of' then
      begin
        ex := FUnit.FindClassifier(tName,False,TMdlClass);
        //Have to be in the current unit, it is ok to decalre a new class with the
        // same name as an existing class
        if (not Assigned(ex)) or (ex.Owner<>FUnit) then
          ex := FUnit.AddClass(tName);
        ex.Visibility := Visibility;
        ParseClass(ex as TMdlClass);
      end
      else
      begin
            { TODO : Manage 'class of ' declarations }
        while GetNextToken and (Token <> ';') do ;
        GetNextToken;
      end;
    end
    else if (lToken = 'interface') or (lToken = 'dispinterface') then
    begin
      ex := FUnit.FindClassifier(tName,False,TMdlInterface);
      if (not Assigned(ex)) or (ex.Owner<>FUnit)  then
        ex := FUnit.AddInterface(tName);
      ex.Visibility := Visibility;
      ParseInterface(ex as TMdlInterface)
    end
    else if Token = '(' then // Enum
    begin
      ex := FUnit.FindClassifier(tName);
      if not Assigned(ex) then {ex := }FUnit.AddDatatype(tName);
//      ex.Visibility := Visibility;
      // Skip the enum declaration for now
      while GetNextToken and (Token <> ';') do ;
      GetNextToken;
    end
    else if lToken = 'record' then
    begin
      ex := FUnit.FindClassifier(tName);
      if not Assigned(ex) then ex := FUnit.AddDatatype(tName);
      ParseRecord(ex as TDataType);
    end
    else if lToken = 'set' then
    begin
      ex := FUnit.FindClassifier(tName);
      if not Assigned(ex) then {ex := }FUnit.AddDatatype(tName);
         { TODO : MAnage sets }
         //   set of (x,y,z)
      while GetNextToken and (Token <> ';') do ;
      GetNextToken;
    end
    else if NextToken = '..' then
    begin
      ex := FUnit.FindClassifier(tName);
      if not Assigned(ex) then {ex := }FUnit.AddDatatype(tName);
         { TODO : Manage subranges }
         //   xxx..yyy
      while GetNextToken and (Token <> ';') do ;
      GetNextToken;
    end
    else if (lToken = 'procedure') or (lToken = 'function') then
    begin // Procedural type
      ParseFunction(nil); // Ignore for now
      if lToken = 'of' then
      begin
            // Skip 'of Object;'
        GetNextToken; GetNextToken; GetNextToken;
      end;
    end
    else
    begin
      ex := FUnit.FindClassifier(tName);
      if not Assigned(ex) then {ex := }FUnit.AddDatatype(tName);
         { TODO : Manage the 'rest' of the types eg. procedural types..}
         // Best guess skip
      while GetNextToken and (Token <> ';') do
      begin
        if Token = '(' then SkipPair('(', ')');
      end;
      GetNextToken;
    end;
  end; // while
end;

procedure TDelphiParser.ParseVarSection;
begin // Token = var
  if GetNextToken then
  begin // Token = varname
    while (NextToken = ':') or (NextToken = ',') do
    begin
      while GetNextToken and (Token <> ';') do
        if (lToken = 'procedure') or (lToken = 'function') then
        begin
          ParseFunction(nil);
          if Token <> '=' then break;
        end
        else if lToken = 'record' then { TODO : Handle records in the var section of functions }
        begin
          ParseRecord(nil);
          break; // Break out to the outer loop
        end;

      if Token = ';' then GetNextToken;
    end;
  end;
end;

procedure TDelphiParser.ParseClass(AClass: TMdlClass);
var
  Visibility: TVisibility;
  ancestor: TMdlClass;
  interf: TMdlInterface;
  classif: TClassifier;
begin // Token is class
  AClass.Documentation.Description := FComment;
  FComment := '';
  if GetNextToken then
  begin { TODO : Class }
    if Token = '(' then
    begin
      GetNextToken; // Should be Ancestor
      ancestor := FUnit.FindClassifier(Token, False, TMdlClass) as TMdlClass;
      if not Assigned(ancestor) then
      begin
        classif := FOM.UnknownPackage.FindClassifier(Token, False, TMdlClass);
        if Assigned(classif) and (classif is TMdlClass) then
          ancestor := classif as TMdlClass;
      end;
      if Assigned(ancestor) then
        AClass.Ancestor := ancestor
      else
        AClass.Ancestor := FOM.UnknownPackage.AddClass(Token);
         { TODO : Parse implemented interfaces }

      GetNextToken;
      while Token = ',' do
      begin
        GetNextToken;
        // Should be an implemented interface
        interf := FUnit.FindClassifier(Token, False, TMdlInterface) as TMdlInterface;
        if not Assigned(interf) then
        begin
          classif := FOM.UnknownPackage.FindClassifier(Token, False, TMdlInterface);
          if Assigned(classif) then
            interf := classif as TMdlInterface;
        end;
        if Assigned(interf) then
          AClass.AddImplements(interf)
        else
          AClass.AddImplements(FOM.UnknownPackage.AddInterface(Token));
        GetNextToken;
      end;
      SkipPair('(', ')');
    end;
    if Token = ';' then
    begin
      // It was only a forward declaration
      GetNextToken;
    end
    else
    begin
      Visibility := viPublished;
      while lToken <> 'end' do
      begin
        if lToken = 'private' then
        begin
          Visibility := viPrivate;
          GetNextToken;
        end
        else if lToken = 'protected' then
        begin
          Visibility := viProtected;
          GetNextToken;
        end
        else if lToken = 'public' then
        begin
          Visibility := viPublic;
          GetNextToken;
        end
        else if lToken = 'published' then
        begin
          Visibility := viPublished;
          GetNextToken;
        end
        else if (lToken = 'function') or (lToken = 'procedure') or
          (lToken = 'constructor') or (lToken = 'destructor') or
          (lToken = 'class') then
        begin
          if lToken = 'class' then GetNextToken; { TODO : Proper handling of 'class function/procedure' }
          ParseFunction(AClass.AddOperation(NextToken), Visibility);
        end
        else if lToken = 'property' then
        begin
          ParseProperty(AClass.AddProperty(NextToken), Visibility);
        end
        else
        begin
          ParseAttribute(AClass.AddAttribute(Token), Visibility);
        end;
      end;
    end;
    if lToken = 'end' then
    begin
      GetNextToken;
      while IsHintDirective(lToken) do
        GetNextToken;
      if Token=';' then
        GetNextToken;
    end;
  end;
end;

procedure TDelphiParser.ParseInterface(AClass: TMdlInterface);
var
  interf: TMdlInterface;
  classif: TClassifier;
begin // Token = interface|dispinterface
  if GetNextToken then
  begin { TODO : Interface }
    if Token = '(' then
    begin
      // This interface inherits from...
      GetNextToken; // Should be Ancestor
      interf := FUnit.FindClassifier(Token,False,TMdlInterface) as TMdlInterface;
      if not Assigned(interf) then
      begin
        classif := FOM.UnknownPackage.FindClassifier(Token,False,TMdlInterface);
        if Assigned(classif) and (classif is TMdlInterface) then
          interf := classif as TMdlInterface;
      end;
      if Assigned(interf) then
        AClass.Ancestor := interf
      else
        AClass.Ancestor := FOM.UnknownPackage.AddInterface(Token);

      SkipPair('(', ')');
    end;
    if Token = ';' then
    begin
      // Only a forward declaration
      GetNextToken;
    end
    else
    begin
      while lToken <> 'end' do
        if (lToken = 'procedure') or (lToken = 'function') then
          ParseFunction(AClass.AddOperation(NextToken))
        else
          GetNextToken;
    end;
    if lToken = 'end' then
      SkipToken(';');
  end;
end;

procedure TDelphiParser.ParseRecord(ARecord: TDataType; DoSkip : boolean = True);
begin
   // ARecord CAN be nil
  while GetNextToken and (lToken <> 'end') do
  begin
    if lToken = 'record' then
    begin
      ParseRecord(ARecord);
      if lToken = 'end' then break;
    end;
  end;
  while IsHintDirective(LowerCase(NextToken)) do
    GetNextToken;
  if DoSkip then
    SkipToken(';')
  else
    GetNextToken;
  // Depending on DoSkip current is either after ';' or on ';'
end;

procedure TDelphiParser.ParseFunction(AOperation: TMdlOperation; Visibility: TVisibility);
begin // Token = function|procedure|constructor|destructor
  if Assigned(AOperation) then
  begin
    AOperation.Documentation.Description := FComment;
    FComment := '';
    if lToken = 'procedure' then
      AOperation.OperationType := otProcedure
    else if lToken = 'function' then
      AOperation.OperationType := otFunction
    else if lToken = 'constructor' then
      AOperation.OperationType := otConstructor
    else if lToken = 'destructor' then
      AOperation.OperationType := otDestructor;
    AOperation.Visibility := Visibility;
  end;

  if (NextToken <> '(') and (NextToken <> ';') then GetNextToken;
  while GetNextToken and (Token <> '(') and (Token <> ';') and (Token <> ':') do ; // Skippa mysko interface grejor
  if Assigned(AOperation) then
  begin
    if Token = '(' then ParseParamList(AOperation);
    if Token = ':' then ParseReturnType(AOperation);
  end
  else
  begin
    if Token = '(' then SkipPair('(', ')');
    while (Token <> ';') do GetNextToken;
  end;

  while ((NextToken = ';') and (lToken <> 'end')) or (Token = ';') or (lToken = 'of') do
  begin
    if Token = ';' then GetNextToken;
    while IsHintDirective(lToken) do
      GetNextToken;
    if IsCallingConvention(lToken) then
    begin
      GetNextToken; GetNextToken;
    end
    else if (lToken = 'virtual') or (lToken = 'override') or (lToken = 'dynamic') then
    begin
         { TODO : Set IsPolymorphic }
      GetNextToken; GetNextToken;
    end
    else if (lToken = 'abstract') then
    begin
      if Assigned(AOperation) then AOperation.IsAbstract := True;
      GetNextToken; GetNextToken;
    end
    else if (lToken = 'overload') or (lToken = 'reintroduce') then
    begin
      GetNextToken; GetNextToken;
    end
    else if (lToken = 'message') then
    begin
         { TODO : Handle message? }
      while GetNextToken and (Token <> ';') do ;
    end
    else if (lToken = 'of') and (LowerCase(NextToken) = 'object') then
    begin
         { TODO : Handle 'of object'? }
      GetNextToken; GetNextToken;
    end;
  end;
  if Token = ';' then GetNextToken;
end;

procedure TDelphiParser.ParseFunctionImplementation;
var
  IsClass: TMdlClass;
  IsOperation, IterOperation: TMdlOperation;
begin // Token = procedure|function|constructor|destructor
   // Skip forward declaration
  GetNextToken;
  if NextToken = '.' then
  begin
    IsClass := FUnit.FindClassifier(Token, False, TMdlClass) as TMdlClass;
    IsOperation := TMdlOperation.Create(nil);
    try
      //Parse to a temporary TMdlOperation to be able to locate a matching operation in a class.
      GetNextToken; GetNextToken; // Skip the '.'
      IsOperation.Name := Token;
      GetNextToken;
      if Token = '(' then ParseParamList(IsOperation);
      if Token = ':' then ParseReturnType(IsOperation);

      if Token = ';' then GetNextToken;

      // Find IsOperation  in IsClass
      if Assigned(isClass) then
      begin
        IterOperation := IsClass.FindOperation(IsOperation);
        if Assigned(IterOperation) then
        begin
          if IterOperation.Documentation.Description <> '' then
            IterOperation.Documentation.Description := IterOperation.Documentation.Description + #13#10;
          IterOperation.Documentation.Description := IterOperation.Documentation.Description +FComment;
          FComment := '';
        end;
      end;
    finally
      FreeAndNil(IsOperation);
    end;
  end else
  begin
    while GetNextToken and (Token <> ';') do
      if Token = '(' then
      begin
        SkipPair('(', ')');
        if Token = ';' then break;
      end;
    GetNextToken;
  end;

  while IsCallingConvention(lToken) or IsFunctionDecoration(lToken) do
    if GetNextToken and (Token = ';') then GetNextToken;

  if (lToken = 'external') then
  begin
    //Manage 'external xxx name yyy;'
    //Mess ups:
    //  procedure QWidget_destroy; external QtShareName name QtNamePrefix + 'QWidget_destroy';  <--Stringexpression
    //  function OffsetWindowOrgEx(): BOOL; external gdi32 name 'OffsetWindowOrgEx'   <--NO semicolon!
    //  procedure Sleep; external kernel32 name 'Sleep'; stdcall;  <-- calling convention after external
    while GetNextToken and (Token <> ';') do
      //Weird delphi compiler bug? Semicolon is NOT necessary after NAME, see Windows.pas
      if lToken='name' then
      begin
        repeat
          GetNextToken;
          GetNextToken;
        until Token<>'+';
        Break;
      end;
    if Token=';' then
      SkipToken(';');
    while IsCallingConvention(lToken) do
      if GetNextToken and (Token = ';') then GetNextToken;
  end
  else if (lToken = 'forward') then
    SkipToken(';')
  else
  begin
    //Skip function body
    ParseProgramBlock;
    if Token = ';' then GetNextToken;
  end;
end;


procedure TDelphiParser.ParseAttribute(AAttribute: TAttribute; Visibility: TVisibility);
var
  attribs: TAttributeList;
  AA: TAttribute;
  i: Integer;
  classifier: TClassifier;
begin // Token = Attributename
  AAttribute.Visibility := Visibility;
  attribs := TAttributeList.Create(False);
  attribs.Add(AAttribute);
  try
    while NextToken = ',' do
    begin
      GetNextToken; GetNextToken;
      AA := (AAttribute.Owner as TMdlClass).AddAttribute(Token);
      attribs.Add(AA);
    end;
    if NextToken = ':' then
    begin
      {Token = : }
      GetNextToken; GetNextToken;
      classifier := ParseType;
      for i:=0 to attribs.Count -1 do
        attribs[i].TypeClassifier := classifier;
    end
    else
      // The line below is needed to step ahead if ':' is missing (an error)
      // OR do a parseerror here
      raise EParseError.Create(FUnit.Name + ': Bad attribute:' + FCurrPos^);
    if Token=';' then
      GetNextToken;
  finally
    FreeAndNil(attribs);
  end;
end;

procedure TDelphiParser.ParseParamList(AOperation: TMdlOperation);
var
  params: TParameterList;
  param: TParameter;
  i,parenLevel: Integer;
  classifier: TClassifier;
begin // Token = (
  params := TParameterList.Create(False);
  parenLevel := 0;
  try
    GetNextToken;
    while ((Token <> ')') or (parenLevel > 0)) do
    begin
      if Token = '(' then
        inc(parenLevel);

      if Token = ')' then
        dec(parenLevel);

      if (NextToken = ',') or (NextToken = ':') then
      begin
        param := AOperation.AddParameter(Token);
        params.Add(param);
      end;

      if Token = ':' then
      begin
        GetNextToken;
        classifier := ParseType;

        for i:= 0 to params.Count -1 do
          params[i].TypeClassifier := classifier;
        params.Clear;
        continue;
      end;
      GetNextToken;
    end;

    GetNextToken;
  finally
    FreeAndNil(params);
  end;
end;

procedure TDelphiParser.ParseReturnType(AOperation: TMdlOperation);
var
  return: string;
  dt: TClassifier;
begin // Token =
  return := '';
  // Do not include Token ':'
  while GetNextToken and (Token <> ';') and (not IsCallingConvention(lToken)) do
    return := return + Token;
  if (Token <> ';') and IsCallingConvention(lToken) then
  begin
      { TODO : Set calling convention }
    GetNextToken;
  end;
  if return <> '' then
  begin
    dt := FUnit.FindClassifier(return);
    if not Assigned(dt) then
      dt := FOM.UnknownPackage.FindClassifier(return);
    if not Assigned(dt) then
      dt := FOM.UnknownPackage.AddDatatype(return);
    AOperation.ReturnValue := dt;
  end;
  GetNextToken;
end;

{procedure TDelphiParser.IndentAfter;
begin
  FParsed := FThisNode;
  FThisNode.IndentAfter;
end;}

{procedure TDelphiParser.UndentAfter;
begin
  FParsed := FThisNode.Parent.Parent as TDelphiParseTree;
  FThisNode.UndentAfter;
end;}

procedure TDelphiParser.ParseProperty(AProperty: TProperty;
  Visibility: TVisibility);
begin // Token = Propertyname
  AProperty.Visibility := Visibility;

  AProperty.Documentation.Description := FComment;
  FComment := '';

  //Mess ups:
  //  property ActionModes default [amInsert, amEdit, amBrowse];  <-- Override on default value

  //** Don't we set the type of a property? Bug?

  while (NextToken <> ':') and (NextToken <> ';') do
  begin
    GetNextToken;
    if Token='[' then
    begin
      SkipPair('[',']');
      if Token=';' then
        Break;
    end;
  end;

  while Token <> ';' do
    GetNextToken;
  if LowerCase(NextToken) = 'default' then
  begin
    GetNextToken;
    GetNextToken;
  end;
  while (Token<>';') do
    GetNextToken;
  GetNextToken;
end;

procedure TDelphiParser.LocalDefine(const symbol: String);
begin
  FLocalDefines.Add(symbol);
end;

procedure TDelphiParser.LocalUndefine(const symbol: String);
var
  i, index: Integer;
begin
  index := -1;
  for i:=0 to FLocalDefines.Count -1 do
  begin
    if CompareText(symbol,FLocalDefines[i]) = 0 then
    begin
      index := i;
      break;
    end;
  end;

  if index <> -1 then
    FLocalDefines.Delete(index);
end;

function TDelphiParser.IsDefined(const symbol: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i:=0 to FLocalDefines.Count -1 do
  begin
    if CompareText(symbol,FLocalDefines[i]) = 0 then
    begin
      Result := True;
      break;
    end;
  end;
end;

function TDelphiParser.ParseType: TClassifier;
var
  name: String;
begin // Interprets and returns a datatype.
  // Token should be the first token after the ':' in a var declaration or paramlist.
  name := '';
  Result := nil;
  while (Token <> ';') and (Token <> ')') and (Token <> '=') and (lToken<>'end') do
  begin
    if Token = '(' then
    begin
      SkipPair('(',')');
      Continue; //Token could already be at the end.
    end;

    if lToken = 'array' then
    begin
      while lToken <> 'of' do GetNextToken;
      Result := ParseType;
      exit;
    end;
    if lToken = 'record' then
    begin
      ParseRecord(nil,False);
      exit;
    end;
    if name = '' then
      name := Token
    else
      name := name+' '+Token;
    GetNextToken;
  end;

  Result := FUnit.FindClassifier(name);
  if not Assigned(Result) then
  begin
    if name = '' then name := 'Unidentified datatype';
    { TODO : MAybe a little smarter. }
    Result := FOM.UnknownPackage.FindClassifier(name);
    if not Assigned(Result) then
      Result := FOM.UnknownPackage.AddDatatype(name);
  end;

  if Token = '=' then
  begin
    { TODO : Skip default values for now }
    while (Token <> ';') and (Token <> ')') and (Token <> '=') do
    begin
      if Token = '(' then
        SkipPair('(',')')
      else
        GetNextToken;
    end;
  end;
end;

function TDelphiParser.IsHintDirective(const S: string): boolean;
//D6 hjälpen:
//  The 'hint' directives platform, deprecated, and library may be appended to
//  any declaration, except that units cannot be declared with deprecated. In the
//  case of a procedure or function declaration, the hint directive should be separated
//  from the rest of the declaration with a semicolon.
begin
  Result := (s = 'platform') or (s = 'deprecated') or (s = 'library');
end;

procedure TDelphiParser.Mark;
begin
  FMarkCurrPos   := FCurrPos;
  FMarkToken     := FToken;
  FMarkNextToken := FNextToken;
end;

procedure TDelphiParser.Recall;
begin
  FCurrPos   := FMarkCurrPos;
  FToken     := FMarkToken;
  FNextToken :=FMarkNextToken;
end;

end.
