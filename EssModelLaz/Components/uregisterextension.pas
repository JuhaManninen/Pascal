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
  Functions to register essModel as a handler of supplied extensions.
}
unit uRegisterExtension;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

function ShellRegisterExtension( ext:String; menustring: String; command: String ): Boolean;
function ShellIsExtensionRegistered( ext:String; menustring: String ): Boolean;
function ShellUnregisterExtension( ext:String; menustring: String ): Boolean;

function DelphiRegisterTool( Title, Prog, WorkingDir, Parameters:String): Boolean;
function DelphiIsToolRegistered( Title: String ): Boolean;
function DelphiUnregisterTool( Title: String ): Boolean;

implementation
uses SysUtils, LCLIntf, LCLType, classes, registry;

function OpenDocEntry(ext:String; reg:TRegistry; Force: Boolean=False): Boolean;
var
  currKey: String;
begin
  Result := False;
  reg.RootKey := HKEY_CLASSES_ROOT;
  if not reg.OpenKey(ext,Force) then
  begin
    // Failed to open the key bail out.
    exit;
  end;

  currKey := reg.ReadString('');
  if currKey = '' then
  begin
    currKey := StringReplace(ext,'.','',[])+'file';
    reg.WriteString('',currKey);
  end;

  currKey := '\'+currKey + '\Shell';
  if not reg.OpenKey(currKey,True) then
  begin
    // Failed to open the key bail out.
    exit;
  end;
  Result := True;
end;

function ShellRegisterExtension( ext:String; menustring: String; command: String ): Boolean;
var
  reg: TRegistry;
  currKey: String;
begin
  Result := False;
  reg := TRegistry.Create;
  try
  if not OpenDocEntry(ext,reg,True) then
  begin
    // Failed to open the key bail out.
    exit;
  end;
  currKey := '\'+reg.CurrentPath;

  currKey := currKey + '\'+menustring;
  if not reg.OpenKey(currKey,True) then
  begin
    // Failed to open the key bail out.
    exit;
  end;
  reg.WriteString('',menustring);

  currKey := currKey + '\command';
  if not reg.OpenKey(currKey,True) then
  begin
    // Failed to open the key bail out.
    exit;
  end;
  reg.WriteString('',command);
  Result := True;
  finally
    FreeAndNil(reg);
  end;
end;

function ShellIsExtensionRegistered( ext:String; menustring: String ): Boolean;
var
  reg: TRegistry;
  currKey: String;
begin
  Result := False;
  reg := TRegistry.Create;
  try
  if not OpenDocEntry(ext,reg) then
  begin
    // Failed to open the key bail out.
    exit;
  end;
  currKey := '\'+reg.CurrentPath;

  currKey := currKey + '\'+menustring;
  if not reg.OpenKey(currKey,False) then
  begin
    // Failed to open the key bail out.
    exit;
  end;
  Result := True;
  finally
    FreeAndNil(reg);
  end;
end;

function ShellUnregisterExtension( ext:String; menustring: String ): Boolean;
var
  reg: TRegistry;
  docRoot, currKey: String;
  ourOwn: Boolean;
begin
  Result := False;

  reg := TRegistry.Create;
  try
  if not OpenDocEntry(ext,reg) then
  begin
    // Failed to open the key bail out.
    exit;
  end;
  currKey := '\'+reg.CurrentPath;
  docRoot := currKey;
  docRoot := StringReplace(docRoot,'\Shell','',[]);

  if not reg.OpenKey(currKey,False) then
    exit;

  ourOwn := not reg.KeyExists('DefaultIcon');

  currKey := currKey + '\'+menustring;
  if not reg.OpenKey(currKey,False) then
  begin
    // Failed to open the key bail out.
    exit;
  end;

  if reg.DeleteKey(currKey+'\command') and
     reg.DeleteKey(currKey) then
  begin
    if ourOwn then
    begin
//      Result := reg.DeleteKey(docRoot+'\Shell') and reg.DeleteKey(docRoot) and
//                reg.DeleteKey('\'+ext);}
    end;
  end else
    Result := True;
  finally
    FreeAndNil(reg);
  end;
end;


function DelphiRegisterTool( Title, Prog, WorkingDir, Parameters:String): Boolean;
var
  reg: TRegistry;
  currKey: String;
  names: TStringList;
  function RegisterIt( verStr: String ):Boolean;
  var
    count: Integer;
  begin
    Result := False;
    if not reg.OpenKey(currKey+verStr+'\Transfer',False) then
      exit;
    if reg.GetDataType('Count') = rdString then
      count := StrToInt(reg.ReadString('Count'))
    else
      count := reg.ReadInteger('Count');

    reg.WriteString('Title'+IntToStr(count),Title);
    reg.WriteString('WorkingDir'+IntToStr(count),WorkingDir);
    reg.WriteString('Path'+IntToStr(count),Prog);
    reg.WriteString('Params'+IntToStr(count),Parameters);
    if reg.GetDataType('Count') = rdString then
      reg.WriteString('Count',IntToStr(count+1))
    else
      reg.WriteInteger('Count',count+1);
  end;
begin
  Result := False;
  reg := TRegistry.Create;
  names := TStringList.Create;
  try
  reg.RootKey := HKEY_CURRENT_USER;
  currKey := '\Software\Borland\Delphi\';
  if not reg.OpenKey(currKey,False) then
    exit;

  RegisterIt('3.0');
  RegisterIt('4.0');
  RegisterIt('5.0');
  RegisterIt('6.0');
  RegisterIt('7.0');

  Result := True;
  finally
    FreeAndNil(reg);
    FreeAndNil(names);
  end;
end;

function DelphiIsToolRegistered( Title: String ): Boolean;
var
  reg: TRegistry;
  currKey: String;
  names: TStringList;
  function FindIt( verStr: String ):Boolean;
  var
    i: Integer;
  begin
    Result := False;
    if not reg.OpenKey(currKey+verStr+'\Transfer',False) then
      exit;
    names.Clear;
    reg.GetValueNames(names);
    for i:=0 to names.Count -1 do
    begin
      if Pos('Title',names[i]) = 1 then
      begin
        if reg.ReadString(names[i]) = Title then
        begin
          // This is the one we wanted
          Result := True
        end;
      end;
    end;
  end;
begin
  Result := False;
  reg := TRegistry.Create;
  names := TStringList.Create;
  try
  reg.RootKey := HKEY_CURRENT_USER;
  currKey := '\Software\Borland\Delphi\';
  if not reg.OpenKey(currKey,False) then
    exit;

  Result := FindIt('3.0') or FindIt('4.0') or FindIt('5.0') or FindIt('6.0');

  finally
    FreeAndNil(reg);
    FreeAndNil(names);
  end;
end;


function DelphiUnregisterTool( Title: String ): Boolean;
var
  reg: TRegistry;
  currKey: String;
  target: Integer;
  names: TStringList;
  function DoUnregister( verStr: String ):Boolean;
  var
    i,x, count: Integer;
  begin
    Result := False;
    if not reg.OpenKey(currKey+verStr+'\Transfer',False) then
      exit;
    names.Clear;
    reg.GetValueNames(names);
    for i:=0 to names.Count -1 do
    begin
      if Pos('Title',names[i]) = 1 then
      begin
        if reg.ReadString(names[i]) = Title then
        begin
          // This is the one we want to delete
          target := StrToInt(StringReplace(names[i],'Title','',[]));

          if reg.GetDataType('Count') = rdString then
            count := StrToInt(reg.ReadString('Count'))
          else
            count := reg.ReadInteger('Count');
          if Count < 1 then exit;

          reg.DeleteValue('Title'+IntToStr(target));
          reg.DeleteValue('WorkingDir'+IntToStr(target));
          reg.DeleteValue('Path'+IntToStr(target));
          reg.DeleteValue('Params'+IntToStr(target));
          if reg.GetDataType('Count') = rdString then
            reg.WriteString('Count',IntToStr(count-1))
          else
            reg.WriteInteger('Count',count-1);
          for x := (target+1) to count -1 do
          begin
            reg.RenameValue('Title'+IntToStr(x),     'Title'+IntToStr(x-1));
            reg.RenameValue('WorkingDir'+IntToStr(x),'WorkingDir'+IntToStr(x-1));
            reg.RenameValue('Path'+IntToStr(x),      'Path'+IntToStr(x-1));
            reg.RenameValue('Params'+IntToStr(x),    'Params'+IntToStr(x-1));
          end;
          Result := True;
          exit;
        end;
      end;
    end;
  end;
begin
  Result := False;
  reg := TRegistry.Create;
  names := TStringList.Create;
  try
  reg.RootKey := HKEY_CURRENT_USER;
  currKey := '\Software\Borland\Delphi\';
  if not reg.OpenKey(currKey,False) then
    exit;

  DoUnregister('3.0');
  DoUnregister('4.0');
  DoUnregister('5.0');
  DoUnregister('6.0');

  finally
    FreeAndNil(reg);
    FreeAndNil(names);
  end;
end;


end.