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

unit uParseTree;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses uModel, uModelEntity, Contnrs;

type

  {
    Baseclass for a parsetree
  }
  TParseTree = class(TObject)
  private
    FOwner: TParseTree;
    FEntity: TModelEntity;
    FChildren: TObjectList;
    FModelEntity: TModelEntity;
    FIsImportant: Boolean; // If false the node is 'whitespace'
    function GetChildCount: Integer;
    function GetChildren(i: Integer): TParseTree;
    procedure SetChildren(i: Integer; const Value: TParseTree);
    procedure SetOwner(AChild: TParseTree);
  public
    constructor Create(AOwner: TPArseTree; AEntity: TModelEntity;
      IsImportant: Boolean = False);
    destructor Destroy; override;

    function Add(NewChild: TParseTree): Integer; overload;
    procedure Insert(index: Integer; NewChild: TParseTree); overload;

    property Parent: TParseTree read FOwner write FOwner;
    property ModelEntity: TModelEntity read FModelEntity write FModelEntity;
    property Important: Boolean read FIsImportant write FIsImportant;
    property ChildCount: Integer read GetChildCount;
    property Children[i: Integer]: TParseTree read GetChildren write SetChildren;
  end;


  TDelphiParseTree = class(TParseTree)
  private
    FCode: string; // Holds the source att this node
  public
    constructor Create(AOwner: TDelphiParseTree; AName: string; AEntity: TModelEntity;
      IsImportant: Boolean = False);
    destructor Destroy; override;

    procedure SaveToFile(Name: string);
    function IndentAfter: Boolean;
    function UndentAfter: Boolean;
    function Add(NewCode: string; NewEntity: TModelEntity; IsImportant: Boolean = False): TDelphiParseTree; overload;
    procedure Insert(index: Integer; NewCode: string; NewEntity: TModelEntity); overload;

    property Code: string read FCode write FCode;
  end;

implementation
uses Classes, SysUtils;

{ TParseTree }

function TParseTree.Add(NewChild: TParseTree): Integer;
begin
  if Self.ClassType <> NewChild.ClassType then
    raise Exception.Create('Parsetree can only hold ' + Self.ClassName);
  SetOwner(NewChild);
  Result := FChildren.Add(NewChild);
end;

constructor TParseTree.Create(AOwner: TPArseTree; AEntity: TModelEntity; IsImportant: Boolean);
begin
  inherited Create;
  FOwner := AOwner;
  FEntity := AEntity;
  FIsImportant := IsImportant;
  FChildren := TObjectList.Create(True);
end;

destructor TParseTree.Destroy;
begin
  inherited;
  FreeAndNil(FChildren);
end;

function TParseTree.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

function TParseTree.GetChildren(i: Integer): TParseTree;
begin
  Result := FChildren.Items[i] as TParseTree;
end;

procedure TParseTree.Insert(index: Integer; NewChild: TParseTree);
begin
  if Self.ClassType <> NewChild.ClassType then
    raise Exception.Create('Parsetree can only hold ' + Self.ClassName);
  SetOwner(NewChild);
  FChildren.Insert(index, NewChild);
end;

procedure TParseTree.SetChildren(i: Integer; const Value: TParseTree);
begin
  if Self.ClassType <> Value.ClassType then
    raise Exception.Create('Parsetree can only hold ' + Self.ClassName);
  SetOwner(Value);
  FChildren.Items[i] := Value;
end;

procedure TParseTree.SetOwner(AChild: TParseTree);
begin
  if (AChild.FOwner <> Self) and (AChild.FOwner <> nil) then
    FOwner.FChildren.Extract(AChild);
  AChild.FOwner := Self;
end;

{ TDelphiParseTree }

function TDelphiParseTree.Add(NewCode: string; NewEntity: TModelEntity; IsImportant: Boolean): TDelphiParseTree;
begin
  Result := TDelphiParseTree.Create(nil, NewCode, NewEntity, IsImportant);
  inherited Add(Result);
end;

constructor TDelphiParseTree.Create(AOwner: TDelphiParseTree; AName: string; AEntity: TModelEntity;
  IsImportant: Boolean);
begin
  inherited Create(AOwner, AEntity, IsImportant);
  FCode := AName;
end;

destructor TDelphiParseTree.Destroy;
begin
  inherited;
end;

function TDelphiParseTree.IndentAfter: Boolean;
var
  i: Integer;
begin
  Result := False;
  if Assigned(FOwner) then
  begin
    i := FOwner.FChildren.IndexOf(Self);
    while (FOwner.FChildren.Count - 1) > i do // Add succeding childs
      Add(FOwner.FChildren.Items[i + 1] as TDelphiParseTree); // as children to Self
  end;
end;

function TDelphiParseTree.UndentAfter: Boolean;
var
  i: Integer;
  Target, c: TDelphiParseTree;
begin
  Result := False;
  if Assigned(FOwner) and Assigned(FOwner.FOwner) then
  begin
    Target := FOwner.FOwner as TDelphiParseTree;
    i := FOwner.FChildren.IndexOf(Self);
    while (FOwner.FChildren.Count - 1) > i do // Move children to the level above.
    begin
      c := FOwner.FChildren.Items[i + 1] as TDelphiParseTree;
      FOwner.FChildren.Extract(c);
      c.FOwner := nil;
      Target.Add(c);
    end;
    Result := True;
  end;
end;

procedure TDelphiParseTree.Insert(index: Integer; NewCode: string; NewEntity: TModelEntity);
begin
  inherited Insert(index, TDelphiParseTree.Create(Self, NewCode, NewEntity));
end;

procedure TDelphiParseTree.SaveToFile(Name: string);
var
  sl: TStringList;
  procedure Build(it: TDelphiParseTree; ind: Integer);
  var
    i: Integer;
  begin
    sl.Add(StringOfChar('-', ind) + it.code);
    for i := 0 to it.FChildren.Count - 1 do
      Build(it.FChildren.Items[i] as TDelphiParseTree, ind + 1);
  end;
begin
  sl := TStringList.Create;
  try
    Build(Self, 0);
    sl.SaveToFile(Name);
  finally
    FreeAndNil(sl);
  end;
end;

end.
