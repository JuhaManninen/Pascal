{
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *

  Author: Juha Manninen

  Abstract:
    A list container for integers. Does for integers what TStringList does for strings.
}
unit JmIntList;

interface

uses Classes, SysUtils;

type

  IntArray = array of integer;

  // Sort compare function type.
  // Ind1 & 2 are values in the index array. They are offsets for the real data array.
//  TSortCompare = function (Ind1, Ind2: integer): Integer of Object;

  // Different sort functions are selected based on these values.
  TSortType = (stStr, stText, stStrNum);

  { TIntList }

  TIntList = class
  private
    fdata: IntArray;
    fCount: integer;
    fSorted: Boolean;
    // QuickSort from TList, modified little.
    procedure QuickSort(L, R: Integer);
    procedure Grow;
    // For setter / getter :
    function GetItem(Index: Integer): Integer;
    procedure SetItem(Index: Integer; const Value: Integer);
    function GetCapacity: integer;
    procedure SetCapacity(const Value: integer);
    procedure SetSorted(const Value: Boolean);
    procedure SetCount(const Value: integer);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(aList: TIntList);
    function Add(const aI: integer): Integer;
    procedure AddList(aList: TIntList);
    procedure Insert(Index: Integer; const aI: integer);
    procedure Delete(Index: Integer);
    procedure Clear;
    procedure Sort;
    function Find(const ai: integer; var Index: Integer): Boolean;
    function IndexOf(const ai: integer): Integer;
    function Equals(aList: TIntList): Boolean;

    property Count: integer read fCount write SetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Sorted: Boolean read fSorted write SetSorted;
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
  end;


implementation

{ TIntList }

// Contructor
constructor TIntList.Create;
begin
  inherited Create;
  Clear;
end;

destructor TIntList.Destroy;
begin
  Capacity := 0;
  inherited;
end;

procedure TIntList.Grow;
var
  Delta, Capa: Integer;
begin
  Capa := Capacity;
{ if Capa > 64 then Delta := Capa div 4 else
    if Capa > 8 then Delta := 16 else
      Delta := 4;
}
  if Capa > 1024 then
    Delta := Capa div 4
  else
    Delta := 512;
  SetCapacity(Capa + Delta);
end;

procedure TIntList.Assign(aList: TIntList);
var
  I: Integer;
begin
  Clear;
  for I := 0 to aList.Count-1 do
    Add(aList[I]);
end;

function TIntList.Add(const aI: integer): Integer;
begin
  Result := fCount;
  if Result = Capacity then
    Grow;
  fdata[Result] := aI;
  Inc(fCount);
  fSorted := False;
end;

procedure TIntList.AddList(aList: TIntList);
var
  i: Integer;
begin
  for i := 0 to aList.Count-1 do
    Add(aList[i]);
end;

procedure TIntList.Delete(Index: Integer);
var
  I: integer;
begin
  if (Index < 0) or (Index >= fCount) then
    raise Exception.Create(Format('Index %s out of range in TIntList!', [Index]));;
  Dec(fCount);
  if Index < fCount then
    for I := Index to fCount do
      fdata[I] := fdata[I+1];
end;

procedure TIntList.Clear;
begin
  Capacity := 0;
  fCount := 0;
end;

procedure TIntList.Insert(Index: Integer; const aI: integer);
var
  I: integer;
begin
  if (Index < 0) or (Index > fCount) then
    raise Exception.Create(Format('Index %s out of range in TIntList!', [Index]));
  if fCount = Capacity then
    Grow;
  if Index < fCount then
    for I := fCount-1 downto Index do
      fdata[I+1] := fdata[I];
//    System.Move(FList^[Index], FList^[Index + 1],
//      (fCount - Index) * SizeOf(Pointer));
  fdata[Index] := aI;
  Inc(fCount);
  fSorted := False;
end;

procedure TIntList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P, T: Integer; // Pointer;
begin
  repeat
    I := L;
    J := R;
    P := fdata[(L + R) shr 1];
    repeat
      while fdata[I] < P do
        Inc(I);
      while fdata[J] > P do
        Dec(J);
      if I <= J then
      begin
        T := fdata[I];
        fdata[I] := fdata[J];
        fdata[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TIntList.Sort;
begin
  if (fdata <> nil) and (Count > 0) then
    QuickSort(0, Count - 1);
  fSorted := True;
end;

function TIntList.Find(const ai: integer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := fCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := fData[I] - ai;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
        Result := True;
    end;
  end;
  Index := L;
end;

function TIntList.IndexOf(const ai: integer): Integer;
begin
  Result := 0;
  while (Result < fCount) and (fdata[Result] <> ai) do
    Inc(Result);
  if Result = fCount then
    Result := -1;
end;

function TIntList.Equals(aList: TIntList): Boolean;
var
  i: Integer;
begin
  Result := False;
  if fCount <> aList.Count then exit;
  for i := 0 to fCount-1 do
    if Items[i] <> aList[i] then
      exit;
  Result := True;
end;


// Setter / Getter :

// Capacity
function TIntList.GetCapacity: integer;
begin
  Result := Length(fdata);
end;

procedure TIntList.SetCapacity(const Value: integer);
begin
  SetLength(fdata, Value);
end;

// Count
procedure TIntList.SetCount(const Value: integer);
var
  I: Integer;
begin
  if (Value < 0) or (Value > MaxListSize) then
    raise Exception.Create(Format('List count out of bounds (%d)', [Value]));
  if Value > Capacity then
    SetCapacity(Value);
  if Value > FCount then
  begin
    for I := FCount to Value-1 do
      Items[I] := 0;
//    FillChar(FList^[FCount], (Value - FCount) * SizeOf(Pointer), 0)
  end
  else
    for I := FCount - 1 downto Value do
      Delete(I);
  FCount := Value;
end;

// Sorted
procedure TIntList.SetSorted(const Value: Boolean);
begin
  if fSorted <> Value then
  begin
    fSorted := Value;
    if fSorted then
      Sort();
  end;
end;

// Item
function TIntList.GetItem(Index: Integer): Integer;
//var ErrorI: integer;
begin
//  if Index >= Length(fdata) then
//    ErrorI := 0;
  Result := fdata[Index];
end;

procedure TIntList.SetItem(Index: Integer; const Value: Integer);
begin
  fdata[Index] := Value;
end;


end.

