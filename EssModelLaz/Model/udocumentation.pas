{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter S�derman, Ville Krumlinde

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

unit uDocumentation;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

type
  //Instance of this class is a member of TModelEntity
  TDocumentation = class
  private
    FDescription : string;
    procedure SetDescription(const Value: string);
  public
    { TODO : add SeeAlso etc here }
    function ShortDescription : string;
    property Description : string read FDescription write SetDescription;
  end;

implementation

uses SysUtils;

{ TDocumentation }

procedure TDocumentation.SetDescription(const Value: string);
var
  I : integer;
begin
  { TODO : Maybe use some kind of TCommentaryParser-class to extract javadoc style tags here }
  I := 1;
  while (I<Length(Value)+1) and (Value[I] in ['*','_','/',' ',#13,#10]) do Inc(I);
  if I>1 then
    FDescription := Copy(Value,I,MAXINT)
  else
    FDescription := Value;
end;

function TDocumentation.ShortDescription: string;
var
  I : integer;
begin
  I := Pos('.',Description);
  if (I=0) or (I>80) then
    I:=80;
  Result := Copy(Description,1,I);
end;

end.
