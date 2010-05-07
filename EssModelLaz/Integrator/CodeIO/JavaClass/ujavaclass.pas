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

unit uJavaClass;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes;


type
  TConstBase = class
  private
    tag : integer;
  public
    procedure Read(Input : TStream); virtual; abstract;
    procedure set_ref(const objAry : array of TConstBase); virtual;
    function getString : string; virtual;
  end;

  TConstPool = class
  private
    constPoolCnt : integer;
    constPool : array of TConstBase;
  public
    constructor Create(Input : TStream);
    destructor Destroy; override;
    function ConstPoolElem(ix : integer) : TConstBase;
  private
    function allocConstEntry(tag  : integer) : TConstBase;
    procedure resolveConstPool;
    procedure readConstPool(Input : TStream);
  end;

  TConstUtf8 = class(TConstBase)
  private
    str : string;
  public
    procedure Read(Input : TStream); override;
    function GetString : string; override;
  end;

  TConstClass_or_String = class(TConstBase)
  private
    index : integer;
    Utf8 : TConstUtf8;
  public
    procedure Read(Input : TStream); override;
    procedure set_ref(const objAry : array of TConstBase); override;
    function GetString : string; override;
  end;

  TConstLongConvert = class(TConstBase)
  private
    function toLong(h,l : integer) : int64;
  protected
    function readLong(Input : TStream) : int64;
  end;

  TConstDouble = class(TConstLongConvert)
  private
    d : double;
  public
    procedure Read(Input : TStream); override;
  end;

  TConstFloat = class(TConstBase)
  private
    f : single;
  public
    procedure Read(Input : TStream); override;
  end;

  TConstInt = class(TConstBase)
  private
    val : integer;
  public
    procedure Read(Input : TStream); override;
  end;

  TConstLong = class(TConstLongConvert)
  private
    longVal : int64;
  public
    procedure Read(Input : TStream); override;
  end;

  TConstName_and_Type_info = class(TConstClass_or_String)
  private
    descriptor_index : integer;
    descriptor_Utf8 : TConstUtf8;
  public
    procedure Read(Input : TStream); override;
    procedure set_ref(const objAry : array of TConstBase); override;
  end;

  TConstRef = class(TConstBase)
  private
    index,name_and_type_index : integer;
    class_ref : TConstClass_or_String;
    name_ref : TConstName_and_Type_info;
  public
    procedure Read(Input : TStream); override;
    procedure set_ref(const objAry : array of TConstBase); override;
  end;

  TAccData = class
  public
    class function isPublic(Val : integer) : boolean;
    class function isPrivate(Val : integer) : boolean;
    class function isProtected(Val : integer) : boolean;
    class function isStatic(Val : integer) : boolean;
    class function isFinal(Val : integer) : boolean;
    class function isSync(Val : integer) : boolean;
    class function isSuper(Val : integer) : boolean;
    class function isVolatile(Val : integer) : boolean;
    class function isTransient(Val : integer) : boolean;
    class function isNative(Val : integer) : boolean;
    class function isInterface(Val : integer) : boolean;
    class function isAbstract(Val : integer) : boolean;
    class function isStrict(Val : integer) : boolean;
  end;

  TObjNameFormat = class
  public
    class function ToDotSeparator(SlashName : string) : string;
  end;

  TAttrInfo = class
  private
    AttrName : string;
    Len : integer;
  public
    constructor Create(Name : string; Length  : integer);
    function GetName : string;
  end;

  TAttrFactory = class
  private
    class procedure Skip_data(len : integer; Input : TStream);
  public
    class function AllocAttr(Input : TStream; constPoolSec : TConstPool) : TAttrInfo;
  end;

  TClassFileHeader = class
  private
    magic : longword;
    minor_version : shortint;
    major_version : shortint;
  public
    constructor Create(Input : TStream);
  end;

  TClassDeclSec = class
  public
    accessFlags : integer;
    thisClass : TConstBase;
    superClass : TConstBase;
    interfaces : array of TConstBase;
  public
    constructor Create(Input : TStream; ConstPoolSec : TConstPool);
    function GetClassName : string;
  end;

  TFieldInfo = class
  public
    access_flags : integer;
    name : TConstUtf8;
    descriptor : TConstUtf8 ;
    attributes : array of TAttrInfo;
  public
    constructor Create(Input : TStream; constPoolSec : TConstPool);
  end;

  TClassFieldSec = class
  public
    classFields : array of TFieldInfo;
  public
    constructor Create(Input : TStream; constPoolSec : TConstPool);
  end;

  TMethodInfo = class
  public
    access_flags : integer;
    name : TConstUtf8 ;
    descriptor : TConstUtf8 ;
    attributes : array of TAttrInfo;
  public
    constructor Create(Input : TStream; constPoolSec : TConstPool);
    function isConstructor : boolean;
  end;

  TClassMethodSec = class
  public
    classMethods : array of TMethodInfo;
  public
    constructor Create(Input : TStream; constPoolSec : TConstPool; className : string);
    destructor Destroy; override;
  end;

  TClassAttrSec = class
  private
    classAttrTab : array of TAttrInfo;
  public
    constructor Create(Input : TStream; constPoolSec : TConstPool);
  end;

  TClassFile = class
  public
    header : TClassFileHeader;
    classConstPool : TConstPool;
    classDecl : TClassDeclSec;
    classFields : TClassFieldSec;
    classMethods : TClassMethodSec;
    classAttrs : TClassAttrSec;
    className : string;
  public
    constructor Create(Input : TStream);
    destructor Destroy; override;
  end;


implementation

uses SysUtils, uError;

const
  ACC_PUBLIC    : word = $0001;
  ACC_PRIVATE   : word = $0002;
  ACC_PROTECTED : word = $0004;
  ACC_STATIC    : word = $0008;
  ACC_FINAL     : word = $0010;
  ACC_SYNC      : word = $0020;
  ACC_VOLATILE  : word = $0040;
  ACC_TRANSIENT : word = $0080;
  ACC_NATIVE    : word = $0100;
  ACC_INTERFACE : word = $0200;
  ACC_ABSTRACT  : word = $0400;
  ACC_STRICT    : word = $0800;

  CONSTANT_Class = 7;
  CONSTANT_Fieldref = 9;
  CONSTANT_Methodref = 10;
  CONSTANT_InterfaceMethodref = 11;
  CONSTANT_String = 8;
  CONSTANT_Integer = 3;
  CONSTANT_Float = 4;
  CONSTANT_Long = 5;
  CONSTANT_Double = 6;
  CONSTANT_NameAndType = 12;
  CONSTANT_Utf8 = 1;


function ReadU1(Input: TStream): integer;
var
  ByteVal : byte;
begin
  Input.Read(ByteVal,1);
  Result := ByteVal;
end;

function ReadU2(Input: TStream): integer;
var
  tmp : array[0..1] of byte;
begin
  Input.Read(tmp,2);
  Result := (tmp[0] shl 8) or tmp[1];
end;

function ReadU4(Input: TStream): longword;
var
  tmp : array[0..3] of byte;
begin
  //$BEBAFECA
  Input.Read(tmp,4);
  Result := (tmp[0] shl 24) or (tmp[1] shl 16) or (tmp[2] shl 8) or tmp[3];
end;



{ TClassFileHeader }

constructor TClassFileHeader.Create(Input: TStream);
begin
  inherited Create;
  magic := readU4( Input );
  Assert(Magic=$CAFEBABE);
  minor_version := readU2( Input );
  major_version := readU2( Input );
end;

{ TClassDeclSec }

constructor TClassDeclSec.Create(Input: TStream; ConstPoolSec: TConstPool);
var
  thisClassIx, superClassIx, interfaceCnt, ix, i : integer;
begin
  inherited Create;
  accessFlags := readU2( Input );
  thisClassIx := readU2( Input );
  superClassIx := readU2( Input );

  thisClass := constPoolSec.constPoolElem( thisClassIx );
  superClass := constPoolSec.constPoolElem( superClassIx );

  interfaceCnt := readU2( Input );

  if (interfaceCnt > 0) then
  begin
    SetLength(interfaces,interfaceCnt);
    for I := 0 to interfaceCnt-1 do
    begin
      ix := readU2( Input );
      interfaces[ i ] := constPoolSec.constPoolElem( ix );
    end;
  end;
end;

function TClassDeclSec.GetClassName: string;
var
  name : string;
begin
  if Assigned(thisClass) then
    if (thisClass is TConstClass_or_String) then
      name := TObjNameFormat.toDotSeparator( thisClass.getString );
  Result := Name;
end;

{ TFieldInfo }

constructor TFieldInfo.Create(Input: TStream; constPoolSec: TConstPool);
var
  name_index,desc_index,attr_cnt,I : integer;
  obj : TConstBase;
begin
  inherited Create;
  access_flags := readU2( Input );
  name_index   := readU2( Input );
  desc_index   := readU2( Input );
  attr_cnt     := readU2( Input );

  obj := constPoolSec.constPoolElem( name_index );
  if Assigned(obj) and (obj is TConstUtf8) then
    Name := obj as TConstUtf8;

  obj := constPoolSec.constPoolElem( desc_index );
  if Assigned(obj) and (obj is TConstUtf8) then
    descriptor := obj as TConstUtf8;

  if (attr_cnt > 0) then
  begin
    SetLength(attributes,attr_cnt);
    for I := 0 to attr_cnt-1 do
      attributes[i] := TAttrFactory.allocAttr( Input, constPoolSec );
  end;
end;

{ TClassFieldSec }

constructor TClassFieldSec.Create(Input: TStream; constPoolSec: TConstPool);
var
  field_cnt,i : integer;
begin
  inherited Create;
  field_cnt := readU2( Input );
  if (field_cnt > 0) then
    SetLength(classFields,field_cnt);

  for I := 0 to field_cnt-1 do
    classFields[i] := TFieldInfo.Create( Input, constPoolSec );
end;

{ TMethodInfo }

constructor TMethodInfo.Create(Input: TStream; constPoolSec: TConstPool);
var
  name_index,desc_index,attr_cnt,I : integer;
  obj : TConstBase;
begin
  inherited Create;
  access_flags := readU2( Input );
  name_index   := readU2( Input );
  desc_index   := readU2( Input );
  attr_cnt     := readU2( Input );

  obj := constPoolSec.constPoolElem( name_index );
  if Assigned(obj) and  (obj is TConstUtf8) then
    name := obj as TConstUtf8;

  obj := constPoolSec.constPoolElem( desc_index );
  if Assigned(obj) and  (obj is TConstUtf8) then
    descriptor := obj as TConstUtf8;

  if (attr_cnt > 0) then
  begin
    SetLength(attributes,attr_cnt);
    for I := 0 to attr_cnt-1 do
      attributes[i] := TAttrFactory.allocAttr( Input, constPoolSec );
  end;
end;

function TMethodInfo.isConstructor: boolean;
begin
  Result := (name.getString()='<init>');
end;


{ TClassMethodSec }

constructor TClassMethodSec.Create(Input: TStream; constPoolSec: TConstPool; className: string);
var
  methodCnt,I : integer;
begin
  inherited Create;
  methodCnt := readU2(Input);
  if (methodCnt > 0) then
    SetLength(classMethods,methodCnt);
  for I := 0 to methodCnt-1 do
    classMethods[i] := TMethodInfo.Create( Input, constPoolSec );
end;

destructor TClassMethodSec.Destroy;
var
  I : integer;
begin
  for I := 0 to High(classMethods) do
    if Assigned(ClassMethods[I]) then FreeAndNil(ClassMethods[I]);
  inherited;
end;

{ TClassAttrSec }

constructor TClassAttrSec.Create(Input: TStream; constPoolSec: TConstPool);
var
  numAttr,I : integer;
begin
  inherited Create;
  numAttr := readU2( Input );
  if (numAttr > 0) then
  begin
    SetLength(classAttrTab,numAttr);
    for I := 0 to numAttr-1 do
      classAttrTab[i] := TAttrFactory.allocAttr( Input, constPoolSec );
  end;
end;

{ TClassFile }

constructor TClassFile.Create(Input: TStream);
begin
  inherited Create;
  try
    header := TClassFileHeader.Create( Input );
    classConstPool := TConstPool.Create( Input );
    classDecl := TClassDeclSec.Create( Input, classConstPool );
    classFields := TClassFieldSec.Create(Input, classConstPool);
    className := classDecl.getClassName;
    classMethods := TClassMethodSec.Create(Input, classConstPool, className );
    classAttrs := TClassAttrSec.Create(Input, classConstPool);
  finally
    Input.Free;
  end;
end;


destructor TClassFile.Destroy;
begin
  if Assigned(Header) then FreeAndNil(header);
  if Assigned(classConstPool) then FreeAndNil(classConstPool);
  if Assigned(classDecl) then FreeAndNil(classDecl);
  if Assigned(classFields) then FreeAndNil(classFields);
  if Assigned(classMethods) then FreeAndNil(classMethods);
  if Assigned(classAttrs) then FreeAndNil(classAttrs);
  inherited;
end;

{ TAttrInfo }

constructor TAttrInfo.Create(Name: string; Length: integer);
begin
  inherited Create;
  attrName := Name;
  len := length;
end;

function TAttrInfo.GetName: string;
begin
  Result := attrName;
end;

{ TAttrFactory }

class function TAttrFactory.allocAttr(Input: TStream; constPoolSec: TConstPool): TAttrInfo;
var
  length : integer;
  retObj : TAttrInfo;
begin
  retObj := nil;

  ReadU2(Input);
  length := ReadU4(Input);

  //Skip all attributes
  skip_data(length,Input);

  Result := retObj;
end;

class procedure TAttrFactory.skip_data(len: integer; Input: TStream);
begin
  if (Input.Position>=Input.Size) and (Len<>0) then
    raise Exception.Create('Unexpected end of file');
  Input.Position := Input.Position + Len;
end;



{ TConstBase }


function TConstBase.getString: string;
begin
  Result := '**noname';
end;

procedure TConstBase.set_ref(const objAry: array of TConstBase);
begin
  //nothing
end;

{ TConstPool }


function TConstPool.allocConstEntry(tag: integer): TConstBase;
begin
  Result := nil;
  case Tag of
    CONSTANT_Utf8:
      Result := TConstUtf8.Create;
    CONSTANT_Integer:
      Result := TConstInt.Create;
    CONSTANT_Float:
      Result := TConstFloat.Create;
    CONSTANT_Long:
      Result := TConstLong.Create;
    CONSTANT_Double:
      Result := TConstDouble.Create;
    CONSTANT_Class,
    CONSTANT_String:
      Result := TConstClass_or_String.Create;
    CONSTANT_Fieldref,
    CONSTANT_Methodref,
    CONSTANT_InterfaceMethodref:
      Result := TConstRef.Create;
    CONSTANT_NameAndType:
      Result := TConstName_and_Type_info.Create;
  else
    ErrorHandler.Trace('allocConstEntry: bad tag value = ' + IntToStr(tag));
  end;

  if Assigned(Result) then
    Result.Tag := Tag;
end;

function TConstPool.ConstPoolElem(ix: integer): TConstBase;
begin
  Result := nil;
  if (ix>0) and (ix<Length(constPool)) then
    Result := constPool[ix];
end;

constructor TConstPool.Create(Input: TStream);
begin
  inherited Create;
  constPoolCnt := readU2(Input);
  SetLength(constPool,constPoolCnt);
  readConstPool(Input);
  resolveConstPool;
end;

destructor TConstPool.Destroy;
var
  I : integer;
begin
  for I:=0 to High(ConstPool) do
    if Assigned(ConstPool[I]) then FreeAndNil(ConstPool[I]);
  inherited;
end;

procedure TConstPool.readConstPool(Input: TStream);
var
  i,tag : integer;
  constObj : TConstBase;
begin
  I := 1;
  while I<constPoolCnt do
  begin
    Tag := ReadU1(Input);
    if (Tag > 0) then
    begin
      constObj := allocConstEntry( tag );
      constObj.read( Input );
      constPool[i] := constObj;
      if (constObj is TConstLong) or (constObj is TConstDouble) then
      begin
        Inc(I);
        constPool[i] := nil;
      end;
    end
    else
      ; //ErrorHandler.Trace('tag == 0');
    Inc(I);
  end;
end;

procedure TConstPool.resolveConstPool;
var
  I : integer;
begin
  //Index 0 is not used
  for I:=1 to constPoolCnt-1 do
    if Assigned(constPool[I]) then
      constPool[I].set_ref(constPool);
end;


{ TConstClass_or_String }

function TConstClass_or_String.GetString: string;
begin
  Result := Utf8.GetString;
end;

procedure TConstClass_or_String.Read(Input: TStream);
begin
  index := readU2( Input );
end;

procedure TConstClass_or_String.set_ref(const objAry: array of TConstBase);
var
  tmp : TConstBase;
begin
  tmp := objAry[ index ];
  if (tmp is TConstUtf8) then
    Utf8 := tmp as TConstUtf8
  else
    ;//ErrorHandler.Trace('not utf8');
end;


{ TConstLongConvert }

function TConstLongConvert.readLong(Input: TStream): int64;
var
  h,l : integer;
begin
  h := readU4(Input);
  l := readU4(Input);
  Result := toLong(h,l);
end;

function TConstLongConvert.toLong(h, l: integer): int64;
begin
  Result := (h shl 32) or l;
end;

{ TConstDouble }

procedure TConstDouble.Read(Input: TStream);
var
  I : int64;
begin
  //Is this cast ok?
  I := ReadLong(Input);
  Move(I,D,SizeOf(D));
end;

{ TConstFloat }

procedure TConstFloat.Read(Input: TStream);
var
  L : longword;
begin
  L := ReadU4(Input);
  //Is this cast ok?
  Move(L,F,SizeOf(F));
end;

{ TConstInt }

procedure TConstInt.Read(Input: TStream);
var
  L : longword;
begin
  L := ReadU4(Input);
  Val := L;
end;

{ TConstLong }

procedure TConstLong.Read(Input: TStream);
begin
  longVal := ReadLong(Input);
end;

{ TConstName_and_Type_info }

procedure TConstName_and_Type_info.Read(Input: TStream);
begin
  inherited read(Input);
  descriptor_index := readU2(Input);
end;

procedure TConstName_and_Type_info.set_ref(const objAry: array of TConstBase);
var
  tmp : TConstBase;
begin
  inherited set_ref( objAry );
  Tmp := objAry[ descriptor_index ];
  if (tmp is TConstUtf8) then
    descriptor_Utf8 := tmp as TConstUtf8
  else
    ; //ErrorHandler.Trace('utf8');
end;

{ TConstRef }

procedure TConstRef.Read(Input: TStream);
begin
  index := readU2( Input );
  name_and_type_index := readU2( Input );
end;

procedure TConstRef.set_ref(const objAry: array of TConstBase);
var
  tmp : TConstBase;
begin
  Tmp := objAry[ index ];
  if (tmp is TConstClass_or_String) then
    class_ref := tmp as TconstClass_or_String
  else
    ; //ErrorHandler.Trace('nix');

  Tmp := objAry[ name_and_type_index ];
  if (tmp is TConstName_and_Type_info) then
    name_ref := tmp as TConstName_and_Type_info
  else
    ;//ErrorHandler.Trace('nix');
end;

{ TConstUtf8 }


procedure TConstUtf8.Read(Input: TStream);
var
  one_char : word;
  len, charCnt : integer;
  one_byte,first_byte : byte;
  tmp : word;
begin
  len := readU2( Input );

  charCnt := 0;
  while (charCnt < len) do
  begin
    one_byte := readU1( Input );
    Inc(charCnt);
    if ((one_byte shr 7) = 1) then
    begin
      tmp := (one_byte and $3f);  // Bits 5..0 (six bits)
      first_byte := one_byte;
      one_byte := readU1( Input );
      Inc(charCnt);
      tmp := (tmp or ((one_byte and $3f) shl 6));
      if ((first_byte shr 4) = 2+4+8) then
      begin
        one_byte := readU1( Input );
        Inc(charCnt);
        one_byte := (one_byte and $0F);
        tmp := (tmp or (one_byte shl 12));
      end;
      one_char := tmp;
    end
    else
      one_char := one_byte;
    Str := Str + char(Lo(One_Char));
  end;
end;

function TConstUtf8.GetString: string;
begin
  Result := str;
end;

{ TAccData }

class function TAccData.isAbstract(Val: integer): boolean;
begin
  Result := (Val and ACC_ABSTRACT) <> 0;
end;

class function TAccData.isFinal(Val: integer): boolean;
begin
  Result := (Val and ACC_FINAL) <> 0;
end;

class function TAccData.isInterface(Val: integer): boolean;
begin
  Result := (Val and ACC_INTERFACE) <> 0;
end;

class function TAccData.isNative(Val: integer): boolean;
begin
  Result := (Val and ACC_NATIVE) <> 0;
end;

class function TAccData.isPrivate(Val: integer): boolean;
begin
  Result := (Val and ACC_PRIVATE) <> 0;
end;

class function TAccData.isProtected(Val: integer): boolean;
begin
  Result := (Val and ACC_PROTECTED) <> 0;
end;

class function TAccData.isPublic(Val: integer): boolean;
begin
  Result := (Val and ACC_PUBLIC) <> 0;
end;

class function TAccData.isStatic(Val: integer): boolean;
begin
  Result := (Val and ACC_STATIC) <> 0;
end;

class function TAccData.isStrict(Val: integer): boolean;
begin
  Result := (Val and ACC_STRICT) <> 0;
end;

class function TAccData.isSuper(Val: integer): boolean;
begin
  Result := (Val and ACC_SYNC) <> 0;  //sync and super share the same bit-flag
end;

class function TAccData.isSync(Val: integer): boolean;
begin
  Result := (Val and ACC_SYNC) <> 0;
end;

class function TAccData.isTransient(Val: integer): boolean;
begin
  Result := (Val and ACC_TRANSIENT) <> 0;
end;

class function TAccData.isVolatile(Val: integer): boolean;
begin
  Result := (Val and ACC_VOLATILE) <> 0;
end;


{ TObjNameFormat }


class function TObjNameFormat.toDotSeparator(slashName: string): string;
var
  I : integer;
  Ch : char;
begin
  Result := '';
  for I:=1 to Length(SlashName) do
  begin
    ch := SlashName[I];
    if ch='/' then
      Result := Result + '.'
    else if ch<>';' then
      Result := Result + ch;
  end;
end;

end.