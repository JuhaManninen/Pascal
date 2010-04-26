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

unit uMainModule;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  uModel, uIntegrator, uCodeIntegrator, uDelphiIntegrator, ActnList, uViewIntegrator,
  {$IFDEF DRAG_SUPPORT}DropSource, DropTarget, {$ENDIF}Menus,
  uFeedback, uTreeViewIntegrator,ExtCtrls;

type
  TMainModule = class(TDataModule)
    ActionList: TActionList;
    CopyDiagramClipboardAction: TAction;
    PrintDiagramAction: TAction;
    DocGenAction: TAction;
    AboutAction: TAction;
    ExportXmiAction: TAction;
    LayoutDiagramAction: TAction;
    FileOpenAction: TAction;
    ExitAction: TAction;
    SettingsAction: TAction;
    UnhideElementsAction: TAction;
    SaveDiagramAction: TAction;
    DocGenPreviewAction: TAction;
    CloseTimer: TTimer;
    OpenFolderAction: TAction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure CopyDiagramClipboardActionExecute(Sender: TObject);
    procedure DocGenActionExecute(Sender: TObject);
    procedure DropFileTargetDrop(Sender: TObject;  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure AboutActionExecute(Sender: TObject);
    procedure ExportXmiActionExecute(Sender: TObject);
    procedure LayoutDiagramActionExecute(Sender: TObject);
    procedure FileOpenActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
    procedure SettingsActionExecute(Sender: TObject);
    procedure UnhideElementsActionUpdate(Sender: TObject);
    procedure UnhideElementsActionExecute(Sender: TObject);
    procedure SaveDiagramActionExecute(Sender: TObject);
    procedure DocGenPreviewActionExecute(Sender: TObject);
    procedure CloseTimerTimer(Sender: TObject);
    procedure OpenFolderActionExecute(Sender: TObject);
  private
    { Private declarations }
    FModel: TObjectModel;
    FDiagram: TDiagramIntegrator;
    //FBackEnd: TCodeIntegrator;
    RecentFiles : TStringList;
    FTreeView : TTreeViewIntegrator;
    {$if Defined(DRAG_SUPPORT)}
    Drop : TDropFileTarget;
    {$ifend}
    Feedback : IEldeanFeedback;
    procedure ProcessCommandLine;
    procedure DoDocGen(IsPreview : boolean; const DestPath : string = '');
    procedure DoXmiFile(const DestFile : string = '');
    procedure RefreshRecentFiles;
    procedure OnRecentFilesClicked(Sender : TObject);
  protected
    //property BackEnd: TCodeIntegrator read FBackEnd;
    property Diagram: TDiagramIntegrator read FDiagram;
  public
    { Public declarations }
    procedure LoadProject(FileNames : TStrings); overload;
    procedure LoadProject(FileName : string); overload;

    property Model: TObjectModel read FModel;
  end;

var
  MainModule: TMainModule;

implementation

uses uMainForm,
  Clipbrd,
  Printers,
  uFileProvider,
  uDocGen,
  uConfig,
  uJavaClassImport,
  uJavaParser,
  {$IFDEF ARGO_XMI}uXmiExportArgoUML, {$ELSE}uXmiExport, {$ENDIF}
  uConst,
  uError,
  Contnrs,
  uAboutForm,
  uSettingsForm,
  uZoomFrame,
  uOpenFolderForm;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TMainModule.DataModuleCreate(Sender: TObject);
begin
  RecentFiles := TStringList.Create;
  RecentFiles.CommaText := Config.ReadStr('RecentFiles','');
  RefreshRecentFiles;

  Feedback := TGuiFeedback.Create(MainForm.StatusPanel);

  FModel := TObjectModel.Create;
  FDiagram := TDiagramIntegrator.CreateDiagram(FModel, MainForm.DiagramPanel, Feedback);
//  FBackEnd := TDelphiIntegrator.Create(FModel);
//  FBackend.CodeProvider := TFileProvider.Create;

  FTreeView := TTreeViewIntegrator.Create(FModel,MainForm.TreePanel,Feedback);

  TZoomFrame.Create(MainForm.ZoomPanel,Diagram);

  {$if Defined(DRAG_SUPPORT)}
  Drop := TDropFileTarget.Create(Self);
  Drop.DragTypes := [dtCopy];
  Drop.OnDrop := DropFileTargetDrop;
  Drop.Register(MainForm);
  {$ifend}

  if ParamCount>0 then
    ProcessCommandLine;
end;

procedure TMainModule.DataModuleDestroy(Sender: TObject);
begin
  Config.IsTerminating := True;
  Config.WriteStr('RecentFiles',RecentFiles.CommaText);

  {$if Defined(DRAG_SUPPORT)}
  if Assigned(Drop) then
    Drop.Unregister;
  {$ifend}

  FreeAndNil(FDiagram);
//  FreeAndNil(FBackEnd);
  FreeAndNil(FTreeView);
  FreeAndNil(FModel);

  FreeAndNil(RecentFiles);
end;

procedure TMainModule.LoadProject(FileNames : TStrings);
var
  Ext : string;
  Imp : TImportIntegrator;
  Ints : TClassList;
  Exts : TStringList;
  I,J : integer;
begin
  // Examine fileextension and call the correct integrator
//!!!  Screen.Cursor := crHourGlass;
  Ext := LowerCase(ExtractFileExt(FileNames[0]));
  Imp := nil;
  Ints := Integrators.Get(TImportIntegrator);
  try
    for I := 0 to Ints.Count - 1 do
    begin
      Exts := TImportIntegratorClass(Ints[I]).GetFileExtensions;
      try
        if Exts.IndexOfName(Ext)>-1 then
        begin
          Imp := TImportIntegratorClass(Ints[I]).Create(Model, TFileProvider.Create(Feedback) );
          J := 0;
          while J<FileNames.Count do
            if Exts.IndexOfName(LowerCase(ExtractFileExt(FileNames[J])))=-1 then
              FileNames.Delete(J)
            else
              Inc(J);
          Break;
        end;
      finally
        Exts.Free;
      end;
    end;

    if Imp<>nil then
      try
        Imp.BuildModelFrom(FileNames);
      finally
        Imp.Free;
      end;
  finally
    Ints.Free;
    Screen.Cursor := crArrow;
  end;

  //Add to RecentFiles
  for I := 0 to FileNames.Count-1 do
  begin
    J := RecentFiles.IndexOf(FileNames[I]);
    if J<>-1 then
      RecentFiles.Delete(J);
    RecentFiles.Insert(0,FileNames[I]);
  end;
  RefreshRecentFiles;

  // These actions are intially disabled
  DocGenAction.Enabled := True;
  DocGenPreviewAction.Enabled := True;
  ExportXmiAction.Enabled := True;
  LayoutDiagramAction.Enabled := True;
  SaveDiagramAction.Enabled := True;
  CopyDiagramClipboardAction.Enabled := True;
end;

procedure TMainModule.LoadProject(FileName : string);
var
  L : TStringList;
begin
  L := TStringList.Create;
  L.Add(FileName);
  LoadProject(L);
  L.Free;
end;


procedure TMainModule.CopyDiagramClipboardActionExecute(Sender: TObject);
var
//!!!  Wmf, Wmf1: TMetaFile;
//!!!  WmfCanvas, WmfCanvas1: TMetaFileCanvas;
  W,H : integer;
  SrcRect: TRect;
begin
  Diagram.GetDiagramSize(W,H);
  SrcRect := Diagram.GetSelectedRect;
{!!!  Wmf := TMetafile.Create;
  Wmf1 := TMetafile.Create;
  try
    if (SrcRect.Right - SrcRect.Left) <> 0 then
    begin
      Wmf1.Width := SrcRect.Right - SrcRect.Left;
      Wmf1.Height := SrcRect.Bottom - SrcRect.Top;
    end;
    Wmf.Width := W;
    Wmf.Height := H;
    WmfCanvas := TMetafileCanvas.Create(Wmf, 0);
    try
      //**This works in w2k since we have fixed controls.pas
      Diagram.PaintTo(WmfCanvas, 0, 0, True);
    finally
      WmfCanvas.Free;
    end;
    if (SrcRect.Right - SrcRect.Left) <> 0 then
    begin
      WmfCanvas1 := TMetafileCanvas.Create(Wmf1, 0);
      try
        WmfCanvas1.Draw(-SrcRect.Left,-SrcRect.Top,Wmf);
      finally
        WmfCanvas1.Free;
      end;
    end else
    begin
      FreeAndNil(Wmf1);
      Wmf1 := Wmf;
      Wmf := nil;
    end;
    Clipboard.Assign(Wmf1);
  finally
    if Assigned(Wmf) then FreeAndNil(Wmf);
    if Assigned(Wmf1) then FreeAndNil(Wmf1);
  end;
}
end;

//end;

procedure TMainModule.DocGenActionExecute(Sender: TObject);
begin
  DoDocGen(False);
end;

procedure TMainModule.DropFileTargetDrop(Sender: TObject;  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
{$if Defined(DRAG_SUPPORT)}
  LoadProject(Drop.Files);
{$ifend}
end;


procedure TMainModule.AboutActionExecute(Sender: TObject);
var
  F : TAboutForm;
begin
  F := TAboutForm.Create(nil);
  try
    F.IconImage.Picture.Icon.LoadFromFile('');
//    F.IconImage.Picture.Icon.Handle := LoadIcon(HInstance,'MAINICON');
    F.NameLabel.Caption := uConst.ProgName + ' ' + uConst.ProgVersion;
    F.ShowModal;
  finally
    F.Free;
  end;
end;


procedure TMainModule.ExportXmiActionExecute(Sender: TObject);
begin
  DoXmiFile;
end;

procedure TMainModule.LayoutDiagramActionExecute(Sender: TObject);
begin
  Diagram.DoLayout;
end;


procedure TMainModule.ProcessCommandLine;
var
  I : integer;
  Files : TStringList;
  S : string;
  DocGenDir : string;
  IsDocGen : boolean;
  XmiFile : string;
  IsXmi : boolean;

  procedure InShowHelp;
  begin
    ShowMessage(
     'Syntax: '#9 + ChangeFileExt(ExtractFileName(Application.ExeName),'') + ' [options] [@list] [files...]'#13#10#13#10 +
     'Valid options are:'#13#10#13#10+
     '    -d[path]'#9'Generate documentation to the path specified'#13#10 +
     '    -a[+/-]'#9'Show associations on generated diagrams on/off'#13#10 +
     '    -v[0-3]'#9'Visibilty filter for generated diagrams'#13#10 +
     '    -x[file]'#9'Export model to xmi file')
  end;

  procedure InOne(S : string);
  var
    IsAbsolute : boolean;
    DirInfo: TSearchRec;
    Code,I : integer;
    L : TStringList;
  begin
    if Pos('@',S)=1 then
    begin
      //File with a list of filenames
      L := TStringList.Create;
      try
        L.LoadFromFile( Copy(S,2,255) );
        for I := 0 to L.Count - 1 do
          InOne(L[I]);
      finally
        L.Free;
      end;
    end;
    //Files added on the command line needs their home directory added to searchpaths
    IsAbsolute := ( Pos(Copy(S,1,1),'\/')>0 ) or (Copy(S,2,1)=':');
    if not IsAbsolute then
      S := GetCurrentDir + PathDelim + S;
    if LastDelimiter('*?',S)>0 then
    begin
      //Expand wildcards
      Code := FindFirst(S, 0, DirInfo);
      while Code = 0 do
      begin
        Files.Add( ExtractFilePath(S) + DirInfo.Name );
        Code := FindNext(DirInfo);
      end;
      FindClose(DirInfo);
    end
    else
      Files.Add( S );
  end;

begin
  Files := TStringList.Create;
  try
    IsDocGen := False;
    IsXmi := False;
    for I := 1 to ParamCount do
    begin
      S := ParamStr(I);
      if Copy(S,1,2)='-d' then
      begin //-d Generate documentation
        DocGenDir := Copy(S,3,255);
        IsDocGen := True;
      end
      else if Copy(S,1,2)='-x' then
      begin //-x Generate xmi-file
        XmiFile := Copy(S,3,255);
        IsXmi := True;
      end else if Copy(S,1,2)='-a' then
      begin //Show associations on/off
        Config.DiShowAssoc := Copy(S,3,1)<>'-';
      end else if Copy(S,1,2)='-v' then
      begin //Visibility filter
        Config.DiVisibilityFilter := StrToIntDef(Copy(S,3,1),0 );
      end
      else if (S='-?') or (S='-help') then
        InShowHelp
      else if (S='-traceon') then
        //ignore
      else if (S<>'') and (S[1]='-') then
        ShowMessage('Ignoring unknown switch: ' + S)
      else if (S<>'') and (S[1]<>'-') then
        InOne( ParamStr(I) );
    end;
    if Files.Count>0 then
      LoadProject(Files);
    // Do stuff after the model is read in
    // the program will exit automatically
    if IsDocGen then
      DoDocGen(False,DocGenDir);
    if IsXmi then
      DoXmiFile(XmiFile);
    if IsDocGen or IsXmi then
      //Delayed exit by using a timer, this is so that all global objects have
      //time to initialize (MainForm, MainModule). Otherwise this would be a
      //special case exit.
      CloseTimer.Enabled := True;
  finally
    Files.Free;
  end;
end;


procedure TMainModule.FileOpenActionExecute(Sender: TObject);
const
  // Keep dialog alive between calls so the searchpath is retained
  D : TOpenDialog = nil;
var
  Ints : TClassList;
  Exts : TStringList;
  I,J : integer;
  AnyFilter,
  Filter : string;
begin
  Filter := '';
  Ints := Integrators.Get(TImportIntegrator);
  try
    for I := 0 to Ints.Count - 1 do
    begin
      Exts := TImportIntegratorClass(Ints[I]).GetFileExtensions;
      try
        for J := 0 to Exts.Count - 1 do
        begin
          if Filter<>'' then
            Filter := Filter + '|';
          Filter := Filter + Exts.Values[ Exts.Names[J] ] + ' (*' + Exts.Names[J] + ')|*' + Exts.Names[J];
          if AnyFilter<>'' then
            AnyFilter := AnyFilter + ';';
          AnyFilter := AnyFilter + '*' + Exts.Names[J];
        end;
      finally
        Exts.Free;
      end;
    end;
  finally
    Ints.Free;
  end;
  Filter := 'All types (' + AnyFilter + ')|' + AnyFilter + '|' + Filter;
  if not Assigned(D) then
  begin
    D := TOpenDialog.Create(MainForm);
    D.Filter := Filter;
    D.Options := D.Options + [ofAllowMultiSelect];
  end;
  if D.Execute then
    LoadProject(D.Files);
end;


procedure TMainModule.ExitActionExecute(Sender: TObject);
begin
  Application.MainForm.Close;
end;


procedure TMainModule.DoDocGen(IsPreview : boolean; const DestPath: string = '');
var
  Doc : TDocGen;
begin
//!!!  Screen.Cursor := crHourGlass;
  Doc := uDocGen.CreateDocGen(Model);
  try
    Doc.DestPath := DestPath;
    Doc.IsPreview := IsPreview;
    Doc.InitFromModel;
  finally
    Screen.Cursor := crArrow;
    Doc.Free;
  end;
end;


procedure TMainModule.SettingsActionExecute(Sender: TObject);
var
  F : TForm;
begin
  F := TSettingsForm.Create(Self);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;


procedure TMainModule.UnhideElementsActionUpdate(Sender: TObject);
begin
  UnhideElementsAction.Enabled := Diagram.HasHiddenElements;
end;

procedure TMainModule.UnhideElementsActionExecute(Sender: TObject);
begin
  Diagram.UnHideAllElements;
end;


procedure TMainModule.DoXmiFile(const DestFile: string);
var
  Xmi : {$IFDEF ARGO_XMI}TXMIExporterArgoUML{$ELSE}TXmiExporter{$ENDIF};
begin
  Xmi := {$IFDEF ARGO_XMI}TXMIExporterArgoUML{$ELSE}TXmiExporter{$ENDIF}.Create(Model,Feedback);
  try
//!!!    Screen.Cursor := crHourGlass;
    Xmi.InitFromModel;
    if DestFile='' then
      Xmi.ShowSaveDialog
    else
      Xmi.SaveTo(DestFile);
  finally
    Screen.Cursor := crArrow;
    Xmi.Free;
  end;
end;

procedure TMainModule.RefreshRecentFiles;
var
//  Items : array of TMenuItem;
  M : TMenuItem;
  I : integer;
begin
  //MAke sure the list never grows beyond 10 items
  while RecentFiles.Count>10 do
    RecentFiles.Delete(RecentFiles.Count-1);
  // Erase old menuitems
  while MainForm.ReopenMenuItem.Count>0 do
    MainForm.ReopenMenuItem.Items[0].Free;
//!!!  SetLength(Items,RecentFiles.Count);
  for I := 0 to RecentFiles.Count-1 do
  begin
    M := TMenuItem.Create(MainForm);
    M.Caption := '&' + IntToStr(I) + ' ' + RecentFiles[I];
    M.OnClick := OnRecentFilesClicked;
    M.Tag := I;
    MainForm.ReopenMenuItem.Add(M);
//!!!    Items[I]:= M;
  end;
//!!!  MainForm.ReopenMenuItem.Add(Items);
end;

procedure TMainModule.OnRecentFilesClicked(Sender: TObject);
begin
  LoadProject( RecentFiles[ (Sender as TMenuItem).Tag ] );
end;

procedure TMainModule.SaveDiagramActionExecute(Sender: TObject);
const
  D : TSaveDialog = nil;
begin
  if not Assigned(D) then
  begin
    D := TSaveDialog.Create(MainForm);
    D.InitialDir := ExtractFilePath( Model.ModelRoot.GetConfigFile );
    D.Filter := 'PNG files (*.png)|*.gif|WMF files (*.wmf)|*.wmf|All files (*.*)|*.*';
    D.Options := D.Options + [ofOverwritePrompt];
  end;
  if D.Execute then
  begin
    if ExtractFileExt(D.FileName)='' then
    begin
      if D.FilterIndex=1 then
        D.FileName := ChangeFileExt(D.FileName,'.png')
      else D.FileName := ChangeFileExt(D.FileName,'.wmf');
    end;
    Diagram.SaveAsPicture( D.FileName );
  end;
end;

procedure TMainModule.DocGenPreviewActionExecute(Sender: TObject);
begin
  DoDocGen(True);
end;

procedure TMainModule.CloseTimerTimer(Sender: TObject);
begin
  ExitAction.Execute;
end;

procedure TMainModule.OpenFolderActionExecute(Sender: TObject);
const
  F : TOpenFolderForm = nil;
var
  L : TStringList;
  Ints : TClassList;
  Exts : TStringList;
  I : integer;

  procedure _AddFileNames(Files : TStringList; Path,Ext : string);
  var
    Sr : TSearchRec;
  begin
    if FindFirst(Path + '\*.*', faReadOnly or faDirectory, Sr)=0 then
    begin
      repeat
        if (Sr.Name<>'.') and (Sr.Name<>'..') then
        begin
          if (Sr.Attr and faDirectory=faDirectory) then
            _AddFileNames(Files,Path + '\' + Sr.Name,Ext)
          else
            if CompareText(ExtractFileExt(Sr.Name),Ext)=0 then
              Files.Add(Path + '\' + Sr.Name);
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  end;

begin
  L := TStringList.Create;
  Ints := Integrators.Get(TImportIntegrator);
  try
    if not Assigned(F) then
    begin
      F := TOpenFolderForm.Create(MainForm);
      for I := 0 to Ints.Count - 1 do
      begin
        Exts := TImportIntegratorClass(Ints[I]).GetFileExtensions;
        F.FileTypeCombo.Items.Add( '*' + Exts.Names[0]);
      end;
      F.FileTypeCombo.ItemIndex := F.FileTypeCombo.Items.Count-1;
    end;
    if F.ShowModal=mrOk then
    begin
      _AddFileNames(L, {F.PathTreeView.Path} './', //!!!
              Copy(F.FileTypeCombo.Items[ F.FileTypeCombo.ItemIndex ],2,10) );
      if L.Count>0 then
        LoadProject(L)
      else
        ShowMessage('No files found');
    end;
  finally
    L.Free;
    Ints.Free;
  end;
end;


end.
