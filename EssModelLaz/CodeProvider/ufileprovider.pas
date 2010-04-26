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

unit uFileProvider;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses Classes, uCodeProvider;

type

  {
    Implementation of a TCodeProvider using the physical filesystem.
  }
  TFileProvider = class(TCodeProvider)
  protected
    procedure HookChanges; override;
    procedure UnhookChanges; override;
  public
    function LoadStream(const AName: string): TStream; override;
    procedure SaveStream(const AName: string; AStream: TStream); override;
    function LocateFile(const AName: string): string; override;
  end;

implementation

uses SysUtils, uConfig, uConst;

{ TFileProvider }

function TFileProvider.LoadStream(const AName: string): TStream;
begin
  if AName <> '' then
  begin
    Feedback.Message('Reading ' + AName);
    Inc(LoadedCount);
    Result := TFileStream.Create(AName, fmOpenRead);
    AddChangeWatch(AName);
  end
  else
    Result := nil;
end;

procedure TFileProvider.SaveStream(const AName: string; AStream: TStream);
var
  fs: TFileStream;
begin
   //inherited;
   { TODO : Make a backup before we save the file. }
  fs := TFileStream.Create(AName, fmOpenWrite + fmShareDenyWrite);
  try
    fs.CopyFrom(AStream, 0);
  finally
    FreeAndNil(fs);
  end;
end;

procedure TFileProvider.HookChanges;
begin
   { TODO : Attach a filesystem listener. }

end;

procedure TFileProvider.UnhookChanges;
begin
   { TODO : Dettach the filesystem listener. }
end;

function TFileProvider.LocateFile(const AName: string): string;
var
  i: Integer;
  p: string;
begin
  Result := '';
  if ( Pos(Copy(AName,1,1),'\/')>0 ) or (Copy(AName,2,1)=':') then
  begin
    //Filename with an absolute path
    if FileExists(AName) then
      Result := AName;
  end
  else
  begin
    //Filename without a path, use searchpath to locate it.
    for I := 0 to SearchPath.Count - 1 do
    begin
      if FileExists(SearchPath[I] + AName) then
      begin
        Result := SearchPath[I] + AName;
        Break;
      end;
    end;
  end;

  // Add the searchpath of the file to searchpaths.
  // It will only be added if it doesn't already exist in searchpaths
  P := ExtractFilePath(Result);
  if (P <> '') and (SearchPath.IndexOf(P) < 0) then
    SearchPath.Add(P);
end;

end.

