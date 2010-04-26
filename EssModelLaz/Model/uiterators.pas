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

unit uIterators;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{
  Iterators and filters for model navigation.
}

uses Contnrs, Classes, SysUtils, uModelEntity;


type

  //Baseclass for iterators
  TModelIterator = class(TBaseModelIterator)
  private
    FItems : TObjectList;
    OwnsItems : boolean;
    NextI : integer;
    FNext : TModelEntity;
    FHasNext : boolean;
  private
    procedure Init(List : TBaseModelIterator; Filter : TBaseIteratorFilter; Order : TIteratorOrder);
  protected
    procedure Advance; virtual;
  public
    constructor Create(ObList : TObjectList; MakeCopy : boolean = False); overload;
    constructor Create(List : TBaseModelIterator; Filter : TBaseIteratorFilter; Order : TIteratorOrder = ioNone); overload;
    constructor Create(List : TBaseModelIterator;
      OneClass : TModelEntityClass;
      MinVisibility : TVisibility = Low(TVisibility);
      Order : TIteratorOrder = ioNone); overload;
    constructor Create(List : TBaseModelIterator; Order : TIteratorOrder = ioNone); overload;
    constructor Create(List : TBaseModelIterator; MinVisibility : TVisibility); overload;
    destructor Destroy; override;
    // TBaseModelIterator
    function HasNext : boolean; override;
    function Next : TModelEntity; override;
    procedure Reset; override;
    function Count : integer; override;
  end;

  //Baseclass for filter used in iterators
  TIteratorFilter = class(TBaseIteratorFilter)
  public
    function Accept(M : TModelEntity) : boolean; override; abstract;
  end;

  //Filters on a class and a minimum visibilty
  TClassAndVisibilityFilter = class(TIteratorFilter)
  private
    OneClass : TModelEntityClass;
    MinVisibility : TVisibility;
  public
    constructor Create(OneClass : TModelEntityClass; MinVisibility : TVisibility = Low(TVisibility));
    function Accept(M : TModelEntity) : boolean; override;
  end;

  //Excludes an entity
  TEntitySkipFilter = class(TIteratorFilter)
  private
    SkipEntity : TModelEntity;
  public
    constructor Create(SkipEntity : TModelEntity);
    function Accept(M : TModelEntity) : boolean; override;
  end;

implementation


{ TModelIterator }

//Creates iterator as a direct copy of an objectlist.
//If makecopy=false then reference oblist, else copy all items.
constructor TModelIterator.Create(ObList: TObjectList; MakeCopy : boolean = False);
var
  I : integer;
begin
  inherited Create;
  if MakeCopy then
  begin
    //Copy oblist to items
    OwnsItems := True;
    FItems := TObjectList.Create(False);
    for I:=0 to ObList.Count-1 do
      FItems.Add(ObList[I]);
  end
  else
  begin
    //Reference same list instead of copy
    OwnsItems := False;
    FItems := ObList;
  end;
  Advance;
end;


//Creates an iterator based on another iterator, filter with Filter, sort on Order.
constructor TModelIterator.Create(List: TBaseModelIterator; Filter : TBaseIteratorFilter; Order : TIteratorOrder = ioNone);
begin
  inherited Create;
  Init(List, Filter, Order);
end;


//Creates an iterator based on another iterator, filter on class and
//visibility, sort result.
constructor TModelIterator.Create(List: TBaseModelIterator;
  OneClass: TModelEntityClass;
  MinVisibility : TVisibility = Low(TVisibility);
  Order : TIteratorOrder = ioNone);
begin
  inherited Create;
  Init(List, TClassAndVisibilityFilter.Create(OneClass,MinVisibility), Order);
end;

//Creates an iterator based on another iterator, sort result.
constructor TModelIterator.Create(List: TBaseModelIterator; Order: TIteratorOrder);
begin
  inherited Create;
  Init(List, nil, Order);
end;


//Creates an iterator based on another iterator, filtered on visibility.
constructor TModelIterator.Create(List: TBaseModelIterator; MinVisibility: TVisibility);
begin
  inherited Create;
  //TModelEntity as classfilter = always true
  Init(List, TClassAndVisibilityFilter.Create(TModelEntity,MinVisibility), ioNone);
end;



destructor TModelIterator.Destroy;
begin
  if OwnsItems then
    FreeAndNil(FItems);
  inherited;
end;

function SortVisibility(Item1, Item2: Pointer): Integer;
var
  E1,E2 : TModelEntity;
begin
  //Visibility, then alpha
  E1 := TModelEntity(Item1);
  E2 := TModelEntity(Item2);
  if (E1.Visibility<E2.Visibility) then
    Result:=-1  //Lower
  else if (E1.Visibility=E2.Visibility) then
    Result := CompareText(E1.Name,E2.Name)
  else
    Result:=1; //Higher
end;

function SortAlpha(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText( TModelEntity(Item1).Name , TModelEntity(Item2).Name );
end;


//Called by all iterator constructors that has an iterator as a parameter
//Initializes iterator
procedure TModelIterator.Init(List: TBaseModelIterator; Filter: TBaseIteratorFilter; Order : TIteratorOrder);
var
  E : TModelEntity;
begin
  OwnsItems := True;
  FItems := TObjectList.Create(False);
  if Assigned(Filter) then
    while List.HasNext do
    begin
      E := List.Next;
      if Filter.Accept( E ) then
        FItems.Add( E );
    end
  else//Not filtered
    while List.HasNext do
      FItems.Add( List.Next );
  //Sort
  case Order of
    ioNone : ;
    ioVisibility : FItems.Sort( SortVisibility );
    ioAlpha      : FItems.Sort( SortAlpha );
  end;
  Advance;
end;


procedure TModelIterator.Advance;
begin
  FHasNext := NextI < FItems.Count;
  if FHasNext then
  begin
    FNext := FItems[NextI] as TModelEntity;
    Inc(NextI);
  end;
end;

function TModelIterator.HasNext: boolean;
begin
  Result := FHasNext;
end;

function TModelIterator.Next: TModelEntity;
begin
  if not FHasNext then
    raise Exception.Create(ClassName + '.Next at end');
  Result := FNext;
  Advance;
end;

procedure TModelIterator.Reset;
begin
  NextI := 0;
  Advance;
end;

{
  Returns nr of elements.
}
function TModelIterator.Count: integer;
begin
  Result := FItems.Count;
end;


{ TClassAndVisibilityFilter }

constructor TClassAndVisibilityFilter.Create(OneClass: TModelEntityClass;
  MinVisibility: TVisibility);
begin
  inherited Create;
  Self.OneClass := OneClass;
  Self.MinVisibility := MinVisibility;
end;

function TClassAndVisibilityFilter.Accept(M: TModelEntity): boolean;
begin
  Result := (M is OneClass) and (M.Visibility>=MinVisibility);
end;


{ TEntitySkipFilter }

constructor TEntitySkipFilter.Create(SkipEntity: TModelEntity);
begin
  Self.SkipEntity := SkipEntity;
end;

function TEntitySkipFilter.Accept(M: TModelEntity): boolean;
begin
  Result := M<>SkipEntity;
end;


end.