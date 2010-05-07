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

unit uHtmlDocGen;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses

{$IFNDEF FPC}
  Windows, ComObj,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
Forms, SysUtils, Graphics, Classes, Dialogs,
uDocGen, uModel, uModelEntity, uXmiExport;

type
  //Html documentation generator
  THtmlDocGen = class(TDocGen)
  private
    Xmi : TXmiExporter;
    Source,Detail : variant;
    procedure MakeDiagram(P : TAbstractPackage);
  protected
    procedure DocStart; override;
    procedure DocFinished; override;
    procedure WriteOverview; override;
    procedure WritePackageDetail(P : TUnitPackage); override;
  public
    destructor Destroy; override;
  end;

implementation

uses uConfig, uViewIntegrator;


{$IFDEF Win32}
function MakeDOM : variant;
begin
  try
    //This should make it work with xml 3 and 4
    //MSXML no longer have version independent progids
    try
      Result := CreateOleObject('MSXML2.DOMDocument.4.0');
    except
      Result := CreateOleObject('MSXML2.DOMDocument.3.0');
    end;
  except
    ShowMessage('MS XML 4 or later is required.'#13#10'Download from http://msdn.microsoft.com/xml');
    Abort;
  end;
  Result.async := false;
  Result.resolveExternals := false;
  Result.setProperty('SelectionNamespaces','xmlns:xsl=''http://www.w3.org/1999/XSL/Transform''');
end;
{$ENDIF}

{$IFDEF Linux}
function MakeDOM : variant;
begin
{ TODO : linux xsl-processor }
end;
{$ENDIF}


{ THtmlDocGen }

destructor THtmlDocGen.Destroy;
begin
  if Assigned(Xmi) then Xmi.Free;
  inherited;
end;


procedure THtmlDocGen.DocStart;
begin
  //Establish stylesheet
  with Config.GetResourceStream('css_file') do
  begin
    SaveToFile(DestPath +  'styles.css');
    Free;
  end;
  Xmi := TXmiExporter.Create(Model);
  Xmi.InitFromModel;
  Source := MakeDOM;
  Source.loadXML( Xmi.GetXmi );
  Detail := MakeDOM;
  if not Detail.loadXML( Config.GetResourceText('p_detail_xsl_file') ) then
    raise exception.Create('nix');
end;


procedure THtmlDocGen.DocFinished;
begin
  {$IFDEF Win32}
  if Assigned(MainForm) then
    ShellExecute(MainForm.Handle,'open',PChar( DestPath +  'overview.html' ),'',
      PChar( DestPath ),SW_SHOWDEFAULT);
  {$ENDIF}
end;

procedure THtmlDocGen.WriteOverview;
var
  Sheet : variant;
  S : string;
  F : TFileStream;
begin
  Sheet := MakeDOM;
  Sheet.loadXML( Config.GetResourceText('p_overview_xsl_file') );
  S := Source.TransformNode(Sheet);
  F := TFileStream.Create( DestPath +  'overview.html' , fmCreate);
  try
    F.Write(S[1],Length(S));
  finally
    F.Free;
  end;
  MakeDiagram( Model.ModelRoot );
end;

procedure THtmlDocGen.WritePackageDetail(P: TUnitPackage);
var
  F : TFileStream;
  Id,FileName,S : string;
begin
  Id := Xmi.GetXMIId(P);
  // Tell the styesheet which package we want to generate html for
//ToDo!!!  Detail.SelectSingleNode('//xsl:param').Text := Id;
  S := Source.TransformNode(Detail);
  FileName := DestPath + 'p_' + id + '.html';
  F := TFileStream.Create( FileName , fmCreate);
  try
    F.Write(S[1],Length(S));
  finally
    F.Free;
  end;
  MakeDiagram( P );
end;

procedure THtmlDocGen.MakeDiagram(P: TAbstractPackage);
var
  Di : TDiagramIntegrator;
  TempForm : TForm;
  W,H : integer;
  FilePrefix,ImageFileName : string;
  Html,Clicks : TStringList;
  Target,Coords : string;
  E : TModelEntity;
  I : integer;
  IsRoot : boolean;
begin
  IsRoot := P=Model.ModelRoot;
  if IsRoot then
    FilePrefix := DestPath + 'p_overview'
  else
    FilePrefix := DestPath + 'p_' + Xmi.GetXMIId(P);

  ImageFileName := FilePrefix + '.png';

  TempForm := TForm.CreateNew(nil);
  Di := TDiagramIntegrator.CreateDiagram(Model, TempForm);
  try
    Di.xPackage := P;
    Di.InitFromModel;
    Di.GetDiagramSize(W,H);
    Clicks := Di.GetClickAreas;
    TempForm.Height:=0;
    TempForm.Width:=0;
    TempForm.SendToBack;
    TempForm.Top := Screen.Height;
    TempForm.Show;
    Di.SaveAsPicture(ImageFileName);
    TempForm.Hide;
  finally
    Di.Free;
    TempForm.Free;
  end;

  Html := TStringList.Create;
  try
    Html.Add( '<html> <body>' );
    Html.Add( '<img src="' + ExtractFileName(ImageFileName) + '" usemap="#map1">' );
    Html.Add( '<map name="map1">' );
    for I := 0 to Clicks.Count-1 do
    begin
      E := Clicks.Objects[I] as TModelEntity;
      if IsRoot then
        Target := 'p_' + Xmi.GetXMIId( E ) + '.html'
      else
        Target := 'p_' + Xmi.GetXMIId( E.Owner ) + '.html' + '#' + Xmi.GetXMIId(E);
      Coords := Clicks[I];
      Html.Add( '<area href="' + Target + '" shape="rect" coords="' + Coords + '">' );
    end;
    Html.Add( '</map>' );
    Html.Add( '</body> </html>' );
    Html.SaveToFile(FilePrefix + '_diagram.html');
  finally
    Html.Free;
    Clicks.Free;
  end;
end;


end.
