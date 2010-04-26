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

unit uIntegrator;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses uModel, Classes, Contnrs, uCodeProvider;

type
  {
    Baseclass for integrators
  }
  TIntegrator = class(TComponent)
  private
    FModel: TObjectModel;
  public
    constructor Create(om: TObjectModel); reintroduce;
    destructor Destroy; override;

    // THE objectmodel
    property Model: TObjectModel read FModel;
  end;

  {
    Baseclass for an integrator where the model can be changed from the source and
    changes in the model can change the source.
  }
  TTwowayIntegrator = class(TIntegrator)
  public
    procedure InitFromModel; virtual;
    procedure BuildModelFrom(FileName : string); virtual;
  end;

  {
    Baseclass for an integrator where the model is used to generate 'something'.
  }
  TExportIntegrator = class(TIntegrator)
  public
    procedure InitFromModel; virtual; abstract;
  end;

  {
    Baseclass for an integrator where 'something' (probably some sourcecode) is
    used to generate a model.
  }
  TImportIntegrator = class(TIntegrator)
  protected
    CodeProvider: TCodeProvider;

    // List of files that have been read in this Lista importsession
    FilesRead : TStringList;

    procedure ImportOneFile(const FileName : string); virtual; abstract;
  public
    constructor Create(om: TObjectModel; CodeProvider: TCodeProvider); reintroduce;
    destructor Destroy; override;
    procedure BuildModelFrom(FileName : string; ResetModel : boolean = True; Lock : boolean = True); overload;
    procedure BuildModelFrom(FileNames : TStrings); overload;
    class function GetFileExtensions : TStringList; virtual; abstract;
  end;

  TIntegratorClass = class of TIntegrator;
  TImportIntegratorClass = class of TImportIntegrator;

  TIntegrators = class
  private
    List : TClassList;
  public
    constructor Create;
    destructor Destroy; override;

    {
     Should be called by all integrator implementations to register it with
     the masterlist of integrators.
     eg. Integrators.Register( TEiffelIntegrator );
    }
    procedure Register(T: TIntegratorClass);

    // Retrieves a list of available integrators of a given kind
    function Get(Kind : TIntegratorClass) : TClassList;
  end;

{
  Used to retrieve _the_ instance of TIntegrators.
}
function Integrators : TIntegrators;

implementation

uses SysUtils, uError, uUseful, uCodeParser, Dialogs;

var
  _Integrators : TIntegrators = nil;

{ TIntegrator }

constructor TIntegrator.Create(om: TObjectModel);
begin
  inherited Create(nil);
  FModel := om;
end;

destructor TIntegrator.Destroy;
begin
  inherited;
  FModel := nil;
end;

{ TTwowayIntegrator }

procedure TTwowayIntegrator.BuildModelFrom(FileName : string);
begin
//Stub
end;

procedure TTwowayIntegrator.InitFromModel;
begin
//Stub
end;

{ TImportIntegrator }

procedure TImportIntegrator.BuildModelFrom(FileName: string; ResetModel: boolean; Lock : boolean);
begin
  CodeProvider.AddSearchPath(ExtractFilePath(FileName));

  if Lock then
    Model.Lock;

  if ResetModel then
  begin
    Model.Clear;
    Model.ModelRoot.SetConfigFile(FileName);
  end;

  FilesRead.Add(FileName);
  try
    try
      ImportOneFile(FileName);
    except
      on E : EParseError do
        ShowMessage(E.Message);
    end;
  finally
    if Lock then
      Model.Unlock;
  end;
end;


procedure TImportIntegrator.BuildModelFrom(FileNames: TStrings);
var
  I : integer;
  P : IEldeanProgress;
begin
  Model.Lock;
  try
    // Add all searchpaths first so the units can find eachother.
    for I := 0 to FileNames.Count-1 do
      CodeProvider.AddSearchPath(ExtractFilePath(FileNames[I]));
    // 'Build' all files..
    if FileNames.Count>3 then
      P := TEldeanProgress.Create('Reading files...',FileNames.Count);
    for I := 0 to FileNames.Count-1 do
    begin
      if FilesRead.IndexOf( FileNames[I] )=-1 then
        BuildModelFrom(FileNames[I], I=0, False)
      else
        ErrorHandler.Trace('Skipping file, already parsed: ' + FileNames[I]);
      if P<>nil then
        P.Tick;
    end;
  finally
    Model.UnLock;
  end;
end;

constructor TImportIntegrator.Create(om: TObjectModel; CodeProvider: TCodeProvider);
begin
  inherited Create(Om);
  Self.CodeProvider := CodeProvider;

  FilesRead := TStringList.Create;
  FilesRead.Sorted := True;
  FilesRead.Duplicates := dupIgnore;
end;

destructor TImportIntegrator.Destroy;
begin
  CodeProvider.Free;
  FilesRead.Free;
  inherited;
end;

{ TIntegrators }

constructor TIntegrators.Create;
begin
  List := TClassList.Create;
end;

destructor TIntegrators.Destroy;
begin
  List.Free;
end;

function TIntegrators.Get(Kind: TIntegratorClass): TClassList;
var
  I : integer;
begin
  Result := TClassList.Create;
  for I := 0 to List.Count - 1 do
    if List[I].InheritsFrom(Kind) then
      Result.Add(List[I]);
end;

procedure TIntegrators.Register(T: TIntegratorClass);
begin
  List.Add(T);
end;


function Integrators : TIntegrators;
begin
  if _Integrators=nil then
    _Integrators := TIntegrators.Create;
  Result := _Integrators;
end;

initialization

finalization

  if Assigned(_Integrators) then
    _Integrators.Free;

end.
