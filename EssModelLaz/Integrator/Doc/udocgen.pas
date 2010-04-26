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

unit uDocGen;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses uIntegrator, uModel, uModelEntity;

type
  //Baseclass for documenation generators.
  TDocGen = class(TExportIntegrator)
  protected
    Packages : TBaseModelIterator;
    procedure TraverseModel; virtual;
    procedure WriteOverview; virtual;
    procedure WritePackageDetail(P : TUnitPackage); virtual;
    procedure WriteClassDetail(C : TClass); virtual;
    procedure WriteInterfaceDetail(I: TInterface); virtual;
    procedure DocStart; virtual; abstract;
    procedure DocFinished; virtual; abstract;
    procedure SelectDestPath;
  public
    DestPath : string;     //Let user select path in dialog if not set
    IsPreview : boolean;   //If true generate doc in tempdir
    procedure InitFromModel; override;
  end;

  //Factory function, create instance of tdocgen
  function CreateDocGen(Om : TObjectModel) : TDocGen;


implementation

uses uIterators,
  uHtmlDocGen,
  uUseful,
  SysUtils,
  Forms,
  uConst;

{ TDocGen }

procedure TDocGen.InitFromModel;
begin
  if IsPreview then
    DestPath := uUseful.MakeTempDir
  else
    if DestPath='' then
      SelectDestPath;

  if not (DestPath[ Length(DestPath) ] in [PathDelim,':']) then
    DestPath := DestPath + PathDelim;

  //Get all unitpackages sorted in name order
  Packages := TModelIterator.Create(Model.ModelRoot.GetAllUnitPackages,ioAlpha);
  DocStart;
  TraverseModel;
  DocFinished;
end;

procedure TDocGen.SelectDestPath;
var
  Di : TBrowseForFolderDialog;
begin
  Di := TBrowseForFolderDialog.Create;
  try
    Di.Path := ExtractFilePath( Model.ModelRoot.GetConfigFile );
    if not Di.Execute then
      Abort;
    DestPath := Di.Path;
  finally
    Di.Free;
  end;
end;

procedure TDocGen.TraverseModel;
var
  P : TUnitPackage;
  Mi : TBaseModelIterator;
  Pro : IEldeanProgress;
  PCount : integer;
begin
  //Overview with packagenames
  WriteOverview;
  Packages.Reset;

  //Init progressbar
  PCount := 0;
  while Packages.HasNext do
  begin
    Inc(PCount);
    Packages.Next;
  end;
  Packages.Reset;
  Pro := TEldeanProgress.Create('Generating documentation...',PCount);

  while Packages.HasNext do
  begin
    //Packagedetails
    P := Packages.Next as TUnitPackage;
    WritePackageDetail(P);
    //Class details
    Mi := TModelIterator.Create(P.GetClassifiers,TClass,Low(TVisibility),ioAlpha);
    while Mi.HasNext do
      WriteClassDetail(Mi.Next as TClass);
    //Interface details
    Mi := TModelIterator.Create(P.GetClassifiers,TInterface,Low(TVisibility),ioAlpha);
    while Mi.HasNext do
      WriteInterfaceDetail(Mi.Next as TInterface);
    Pro.Tick;
  end;
end;

/////////////////////

function CreateDocGen(Om : TObjectModel) : TDocGen;
begin
  //Use html
  Result := THtmlDocGen.Create(Om);
end;

procedure TDocGen.WriteClassDetail(C: TClass);
begin

end;

procedure TDocGen.WriteInterfaceDetail(I: TInterface);
begin

end;

procedure TDocGen.WriteOverview;
begin

end;

procedure TDocGen.WritePackageDetail(P: TUnitPackage);
begin

end;

end.