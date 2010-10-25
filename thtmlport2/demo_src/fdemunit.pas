unit FDemUnit;
{A program to demonstrate the TFrameViewer component}

{$mode objfpc}{$H+}

interface

uses
  LclIntf, LMessages, LclType, LResources, FPimage, HtmlMisc,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, FramView, ExtCtrls, StdCtrls, Menus,
  Clipbrd, HTMLView, HTMLsubs, HTMLun2, // ShellAPI, Unix,
  {PreviewForm,} PrintersDlgs, FontDlg, HTMLAbt, Submit, ImgForm, Readhtml, ComCtrls;

const
  MaxHistories = 6;  // size of History list

  type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog: TOpenDialog;                 
    Edit1: TMenuItem;
    Find1: TMenuItem;
    Panel2: TPanel;
    Copy1: TMenuItem;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    FindDialog: TFindDialog;
    Options1: TMenuItem;
    Showimages: TMenuItem;
    About1: TMenuItem;
    HistoryMenuItem: TMenuItem;
    PrintDialog: TPrintDialog;
    Print1: TMenuItem;
    Fonts: TMenuItem;
    FrameViewer: TFrameViewer;
    Panel1: TPanel;
    ReloadButton: TButton;
    FwdButton: TButton;
    BackButton: TButton;
    Edit2: TEdit;
    PopupMenu1: TPopupMenu;
    CopyImagetoclipboard: TMenuItem;
{   MediaPlayer: TMediaPlayer;
}
    ViewImage: TMenuItem;
    N3: TMenuItem;
    OpenInNewWindow: TMenuItem;
    PrintPreview1: TMenuItem;
    Timer1: TTimer;
    ProgressBar: TProgressBar;
    SetPrintScale: TMenuItem;
    PrinterSetupDialog: TPrinterSetupDialog;
    PrinterSetup: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ReloadClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure ShowimagesClick(Sender: TObject);
    procedure HistoryClick(Sender: TObject);
    procedure HistoryChange(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure FontsClick(Sender: TObject);
    procedure SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: string;
      Results: TStringList);
    procedure HotSpotTargetClick(Sender: TObject; const Target,
      URL: string; var Handled: Boolean);
    procedure HotSpotTargetChange(Sender: TObject; const Target,
      URL: string);
    procedure ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
    procedure WindowRequest(Sender: TObject; const Target,
      URL: string);
    procedure BackButtonClick(Sender: TObject);
    procedure FwdButtonClick(Sender: TObject);
    procedure CopyImagetoclipboardClick(Sender: TObject);
    procedure MediaPlayerNotify(Sender: TObject);
    procedure SoundRequest(Sender: TObject; const SRC: String;
      Loop: Integer; Terminate: Boolean);
    procedure FrameViewerObjectClick(Sender, Obj: TObject; const AOnClickMsg: String);
    procedure ViewImageClick(Sender: TObject);
    procedure FrameViewerInclude(Sender: TObject; const Command: String;
      Params: TStrings; var S: string);
    procedure FormDestroy(Sender: TObject);
    procedure FrameViewerRightClick(Sender: TObject;
      Parameters: TRightClickParameters);
    procedure OpenInNewWindowClick(Sender: TObject);
    procedure PrintPreview1Click(Sender: TObject);
    procedure FrameViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FrameViewerProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Integer);
    procedure SetPrintScaleClick(Sender: TObject);
    procedure ViewerPrintHTMLFooter(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
    procedure ViewerPrintHTMLHeader(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
    procedure PrinterSetupClick(Sender: TObject);
  private
{$IFDEF LCLCarbon}
    AppMenu : TMenuItem;
{$ENDIF}
    Histories: array[0..MaxHistories-1] of TMenuItem;
    FoundObject: TImageObj;
    NewWindowFile: string;
    MediaCount: integer;
    ThePlayer: TOBject;
    TimerCount: integer;
    OldTitle: string;
    HintWindow: THintWindow;
    HintVisible: boolean;
    TitleViewer: ThtmlViewer;

    procedure wmDropFiles(var Message: TLMessage); message LM_DROPFILES;
    procedure CloseAll;
  public

  end;

var
  Form1: TForm1;

implementation

{$IFNDEF LCL}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: integer;
begin
  Left := Left div 2;
  Top := Top div 2;
  Width := (Screen.Width * 8) div 10;
  Height := (Screen.Height * 6) div 8;
  FrameViewer.HistoryMaxCount := MaxHistories;  {defines size of history list}

  for I := 0 to MaxHistories-1 do
  begin      {create the MenuItems for the history list}
    Histories[I] := TMenuItem.Create(HistoryMenuItem);
    HistoryMenuItem.Insert(I, Histories[I]);
    with Histories[I] do
    begin
      OnClick := @HistoryClick;
      Caption := 'XX';
      Tag := I;
    end;
  end;

{$IFDEF LCLCarbon}
  AppMenu := TMenuItem.Create(Self);  //Application menu
  AppMenu.Caption := #$EF#$A3#$BF;  //Unicode Apple logo char
  MainMenu1.Items.Insert(0, AppMenu);
  MainMenu1.Items.Remove(About1);  //Remove About as separate menu
  AppMenu.Add(About1);  //Add About as item in application menu
{$ENDIF}

{$IFDEF MSWINDOWS}
  DragAcceptFiles(Handle, True);
{$ENDIF}
  HintWindow := THintWindow.Create(Self);
  HintWindow.Color := $CCFFFF;
end;

procedure TForm1.HotSpotTargetClick(Sender: TObject; const Target, URL: string;
          var Handled: boolean);
{This routine handles what happens when a hot spot is clicked.  The assumption
 is made that DOS filenames are being used. .EXE, .WAV, .MID, and .AVI files are
 handled here, but other file types could be easily added.

 If the URL is handled here, set Handled to True.  If not handled here, set it
 to False and ThtmlViewer will handle it.}
const
  snd_Async = $0001;  { play asynchronously }
var
  PC: array[0..255] of char;
  PC2: array[0..255] of char;
  S, Params: string[255];
  Ext: string[5];
  I, J, K: integer;
  Viewer: ThtmlViewer;
  ID: string;

begin
  Handled := False;
  {The following looks for a link of the form, "IDExpand_XXX".  This is interpreted
   as meaning a block with an ID="XXXPlus" or ID="XXXMinus" attribute should
   have its Display property toggled.  }
  I := Pos('IDEXPAND_', Uppercase(URL));
  if I=1 then
  begin
    Viewer := FrameViewer.ActiveViewer;
    if Assigned(Viewer) then
    begin
      ID := Copy(URL, 10, Length(URL)-9);
      Viewer.IDDisplay[ID+'Plus'] := not Viewer.IDDisplay[ID+'Plus'];
      Viewer.IDDisplay[ID+'Minus'] := not Viewer.IDDisplay[ID+'Minus'];
      Viewer.Reformat;
    end;
    Handled := True;
    Exit;
  end;
  
  {check for various file types}
  I := Pos(':', URL);
  J := Pos('FILE:', UpperCase(URL));
  if (I <= 2) or (J > 0) then
  begin                      {apparently the URL is a filename}
    S := URL;
    K := Pos(' ', S);     {look for parameters}
    if K = 0 then K := Pos('?', S);  {could be '?x,y' , etc}
    if K > 0 then
    begin
      Params := Copy(S, K+1, 255); {save any parameters}
      S[0] := chr(K-1);            {truncate S}
    end
    else Params := '';
    S := (Sender as TFrameViewer).HTMLExpandFileName(S);
    Ext := Uppercase(ExtractFileExt(S));
    if Ext = '.WAV' then
    begin
      Handled := True;
{     sndPlaySound(StrPCopy(PC, S), snd_ASync);
}
    end
    else if Ext = '.EXE' then
    begin
      Handled := True;
{$IFDEF MSWINDOWS}
      ShellExecute(Handle, nil, StrPCopy(PC, S), StrPCopy(PC2, Params),
                   nil, SW_SHOWNORMAL);
{$ELSE}
 {$IFDEF LCLCarbon}
      OpenDocument(S + '.app"'); // Shell('Open "' + S + '.app"');
 {$ELSE}
      OpenDocument(S);
 {$ENDIF}
{$ENDIF}
    end
    else if (Ext = '.MID') or (Ext = '.AVI')  then
    begin
      Handled := True;
{$IFDEF MSWINDOWS}
      ShellExecute(Handle, nil, 'MPlayer.exe', '/play /close',
                   nil, SW_SHOWNORMAL);
{$ELSE}  //No equivalent to MPlayer?
 {$IFDEF LCLCarbon}
      OpenDocument(S + '.app"'); // Shell('Open "' + S + '.app"');
 {$ELSE}
      OpenDocument(S);
 {$ENDIF}
{$ENDIF}
    end;
    {else ignore other extensions}
    Edit2.Text := URL;
    Exit;
  end;
  I := Pos('MAILTO:', UpperCase(URL));
  J := Pos('HTTP://', UpperCase(URL));
  if (I > 0) or (J > 0) then
  begin
    OpenURL(URL);
    Handled := True;
    Exit;
  end;
  Edit2.Text := URL;   {other protocall}
end;

procedure TForm1.HotSpotTargetChange(Sender: TObject; const Target, URL: string);
{mouse moved over or away from a hot spot.  Change the status line}
begin
  if URL = '' then
    Panel2.Caption := ''
  else if Target <> '' then
    Panel2.Caption := 'Target: '+Target+'  URL: '+URL
  else
    Panel2.Caption := 'URL: '+URL
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  if FrameViewer.CurrentFile <> '' then
    OpenDialog.InitialDir := ExtractFilePath(FrameViewer.CurrentFile)
  else
{$IFNDEF LCLCarbon}
    OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
{$ELSE}  //Don't default to within app bundle.
    OpenDialog.InitialDir := ExtractFilePath(ParamStr(0)) + '../../../';
{$ENDIF}
  OpenDialog.FilterIndex := 1;
  if OpenDialog.Execute then
  begin
    FrameViewer.LoadFromFile(OpenDialog.Filename);
    Caption := FrameViewer.DocumentTitle;
  end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Find1Click(Sender: TObject);
begin
  FindDialog.Execute;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  S: string;
  I: integer;
begin
{$IFNDEF LCLCarbon}  //Launched file name not passed via command line with app bundle.
  if (ParamCount >= 1) then
  begin            {Parameter is file to load}
    S := ParamStr(1);
    FrameViewer.LoadFromFile(HtmlToDos(Trim(S)));
  end else
{$ENDIF}
  begin
    S := ExtractFilePath(ParamStr(0)) + 'demo.htm';
    if FileExists(S) then
      FrameViewer.LoadFromFile(S)
    { If run from Lazarus IDE, HTML files probably won't be in executable's folder,
     so look for them one level up (or 4 levels up with Carbon app bundle).}
    else begin
      S := ExtractFilePath(ParamStr(0)) + '..' + PathDelim +
           {$IFDEF LCLCarbon} '../../../' + {$ENDIF} 'demo.htm';
      if FileExists(S) then
        FrameViewer.LoadFromFile(S);
    end;
  end;
end;

procedure TForm1.ReloadClick(Sender: TObject);
{the Reload button was clicked}
begin
  ReloadButton.Enabled := False;
  FrameViewer.Reload;   {load again}
  ReloadButton.Enabled := FrameViewer.CurrentFile <> '';
  FrameViewer.SetFocus;
end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
  FrameViewer.CopyToClipboard;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  Copy1.Enabled := FrameViewer.SelLength <> 0;
  SelectAll1.Enabled := (FrameViewer.ActiveViewer <> Nil)
                    and (FrameViewer.ActiveViewer.CurrentFile <> '');
  Find1.Enabled := SelectAll1.Enabled;
end;

procedure TForm1.SelectAll1Click(Sender: TObject);
begin
  FrameViewer.SelectAll;
end;

procedure TForm1.FindDialogFind(Sender: TObject);
begin
  with FindDialog do
    if not FrameViewer.FindEx(FindText, frMatchCase in Options, not (frDown in Options)) then
      MessageDlg('No further occurances of "'+FindText+'"', mtInformation, [mbOK], 0);
end;

procedure TForm1.ShowimagesClick(Sender: TObject);
begin
  FrameViewer.ViewImages := not FrameViewer.ViewImages;
  ShowImages.Checked := FrameViewer.ViewImages;
end;

procedure TForm1.HistoryChange(Sender: TObject);
{This event occurs when something changes history list}
var
  I: integer;
  Cap: string[80];
begin
  with Sender as TFrameViewer do
  begin
    {check to see which buttons are to be enabled}
    FwdButton.Enabled := FwdButtonEnabled;
    BackButton.Enabled := BackButtonEnabled;

    {Enable and caption the appropriate history menuitems}
    HistoryMenuItem.Visible := History.Count > 0;
    for I := 0 to MaxHistories-1 do
      with Histories[I] do
        if I < History.Count then
        Begin
          Cap := History.Strings[I];
          if TitleHistory[I] <> '' then
            Cap := Cap + '--' + TitleHistory[I];
          Caption := Cap;    {Cap limits string to 80 char}
          Visible := True;
          Checked := I = HistoryIndex;
        end
        else
          Histories[I].Visible := False;
    Caption := DocumentTitle;    {keep the caption updated}
    FrameViewer.SetFocus;
  end;
end;

procedure TForm1.HistoryClick(Sender: TObject);
{A history list menuitem got clicked on}
begin
  {Changing the HistoryIndex loads and positions the appropriate document}
  FrameViewer.HistoryIndex := (Sender as TMenuItem).Tag;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  AboutBox := TAboutBox.CreateIt(Self, 'FrameDem', 'TFrameViewer');
  try
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;

procedure TForm1.Print1Click(Sender: TObject);
begin
  with PrintDialog do
    if Execute then
      if PrintRange = prAllPages then
        FrameViewer.Print(1, 9999)
      else
        FrameViewer.Print(FromPage, ToPage);
end;

procedure TForm1.File1Click(Sender: TObject);
begin
  Print1.Enabled := FrameViewer.ActiveViewer <> Nil;
  PrintPreview1.Enabled := Print1.Enabled;
end;

procedure TForm1.FontsClick(Sender: TObject);
var
  FontForm: TFontForm;
begin
  FontForm := TFontForm.Create(Self);
  try
    with FontForm do
    begin
      FontName := FrameViewer.DefFontName;
      FontColor := FrameViewer.DefFontColor;
      FontSize := FrameViewer.DefFontSize;
      HotSpotColor := FrameViewer.DefHotSpotColor;
      Background := FrameViewer.DefBackground;
      if ShowModal = mrOK then
      begin
        FrameViewer.DefFontName := FontName;
        FrameViewer.DefFontColor := FontColor;
        FrameViewer.DefFontSize := FontSize;
        FrameViewer.DefHotSpotColor := HotSpotColor;
        FrameViewer.DefBackground := Background;
        ReloadClick(Self);    {reload to see how it looks}
      end;
    end;
  finally
    FontForm.Free;
  end;
end;

procedure TForm1.SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: String;
  Results: TStringList);
begin
  with SubmitForm do
  begin
    ActionText.Text := AnAction;
    MethodText.Text := Method;
    ResultBox.Items := Results;
    Results.Free;
    Show;
  end;
end;

procedure TForm1.ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
begin
  if ProcessingOn then
  begin    {disable various buttons and menuitems during processing}
    FwdButton.Enabled := False;
    BackButton.Enabled := False;
    ReloadButton.Enabled := False;
    Print1.Enabled := False;
    PrintPreview1.Enabled := False;
    Find1.Enabled := False;
    SelectAll1.Enabled := False;
    Open1.Enabled := False;
    CloseAll;    {in case hint window is open}
  end
  else begin
    FwdButton.Enabled := FrameViewer.FwdButtonEnabled;
    BackButton.Enabled := FrameViewer.BackButtonEnabled;
    ReloadButton.Enabled := FrameViewer.CurrentFile <> '';
    Print1.Enabled := (FrameViewer.CurrentFile <> '') and (FrameViewer.ActiveViewer <> Nil);
    PrintPreview1.Enabled := Print1.Enabled;
    Find1.Enabled := Print1.Enabled;
    SelectAll1.Enabled := Print1.Enabled;
    Open1.Enabled := True;
  end;
end;

procedure TForm1.FwdButtonClick(Sender: TObject);
begin
  FrameViewer.GoFwd;
end;

procedure TForm1.BackButtonClick(Sender: TObject);
begin
  FrameViewer.GoBack;
end;

procedure TForm1.WindowRequest(Sender: TObject; const Target, URL: string);
var
  S, Dest: string[255];
  I: integer;
  PC: array[0..255] of char;
  PC2: array[0..255] of char;
begin
  S := URL;
  I := Pos('#', S);
  if I >= 1 then
  begin
    Dest := System.Copy(S, I, 255);  {local destination}
    S := System.Copy(S, 1, I-1);     {the file name}
  end
  else
    Dest := '';    {no local destination}
  S := FrameViewer.HTMLExpandFileName(S);
  if FileExists(S) then
//    WinExec(StrPCopy(PC, ParamStr(0)+' "'+S+Dest+'"'), sw_Show);
 {$IFDEF MSWINDOWS}
    ShellExecute(Handle, nil, StrPCopy(PC, ParamStr(0)),
                 StrPCopy(PC2, S+Dest), nil, SW_SHOWNORMAL);
 {$ELSE}
  {$IFDEF LCLCarbon}
    OpenDocument(ParamStr(0) + '.app"'); // Shell('Open "' + S + '.app"');
  {$ELSE}
    OpenDocument(ParamStr(0)); // Shell('"' + ParamStr(0) + '" "' + S+Dest + '"');
  {$ENDIF}
 {$ENDIF}
end;

procedure TForm1.wmDropFiles(var Message: TLMessage);
var
  S: string[200];
  Count: integer;
begin
{$IFDEF MSWINDOWS}
  Count := DragQueryFile(Message.WParam, 0, @S[1], 200);
  S[0] := Char(Count);
  DragFinish(Message.WParam);
  if Count >0 then
    FrameViewer.LoadFromFile(S);
{$ENDIF}
  Message.Result := 0;
end;

procedure TForm1.CopyImagetoclipboardClick(Sender: TObject);
begin
  Clipboard.Assign(FoundObject.Bitmap);
end;

procedure TForm1.ViewImageClick(Sender: TObject);
var
  AForm: TImageForm;
begin
  AForm := TImageForm.Create(Self);
  AForm.ImageFormBitmap := FoundObject.Bitmap;
  AForm.Caption := '';
  AForm.Show;
end;

procedure TForm1.MediaPlayerNotify(Sender: TObject);
begin
{$IFNDEF LCL}
  With MediaPlayer do
  try
    if NotifyValue = nvSuccessful then
    begin
      if MediaCount > 0 then
      begin
        Play;
        Dec(MediaCount);
      end
      else Begin
        Close;
        ThePlayer := Nil;
      end;
    end;
  except
  end;
{$ENDIF}
end;

procedure TForm1.SoundRequest(Sender: TObject; const SRC: String;
  Loop: Integer; Terminate: Boolean);
begin
{$IFNDEF LCL}
  with MediaPlayer do
  try
    if Terminate then
    begin
      if (Sender = ThePlayer) then
      begin
        Close;
        ThePlayer := Nil;
      end;
    end
    else if ThePlayer = Nil then
    begin
      if Sender is ThtmlViewer then
        Filename := ThtmlViewer(Sender).HTMLExpandFilename(SRC)
      else
        Filename := (Sender as TFrameViewer).HTMLExpandFilename(SRC);
      Notify := True;
      Open;
      ThePlayer := Sender;
      if Loop < 0 then
        MediaCount := 9999
      else if Loop = 0 then
        MediaCount := 1
      else
        MediaCount := Loop;
    end;
  except
  end;
{$ENDIF}
end;

procedure TForm1.FrameViewerObjectClick(Sender, Obj: TObject; const AOnClickMsg: String);
var
  S: string;
begin
  if AOnClickMsg = 'display' then
  begin
    if Obj is TFormControlObj then
      with TFormControlObj(Obj) do
      begin
        if TheControl is TCheckBox then
          with TCheckBox(TheControl) do
          begin
            S := Value + ' is ';
            if Checked then
              S := S + 'checked'
            else
              S := S + 'unchecked';
            MessageDlg(S, mtCustom, [mbOK], 0);
          end
        else if TheControl is TRadioButton then
          with TRadioButton(TheControl) do
          begin
            S := Value + ' is checked';
            MessageDlg(S, mtCustom, [mbOK], 0);
          end;
      end;
  end
  else if AOnClickMsg <> '' then
    MessageDlg(AOnClickMsg, mtCustom, [mbOK], 0);
end;

procedure TForm1.FrameViewerInclude(Sender: TObject; const Command: String;
  Params: TStrings; var S: string);
{OnInclude handler}  
var
  Filename: string;
  I: integer;
  MS: TMemoryStream;
begin
  if CompareText(Command, 'Date') = 0 then
    S := DateToStr(Date) { <!--#date --> }
  else if CompareText(Command, 'Time') = 0 then
    S := TimeToStr(Time)   { <!--#time -->  }
  else if CompareText(Command, 'Include') = 0 then
  begin   {an include file <!--#include FILE="filename" -->  }
    if (Params.count >= 1) then
    begin
      I := Pos('file=', Lowercase(Params[0]));
      if I > 0 then
      begin
        Filename := copy(Params[0],  6, Length(Params[0])-5);
        MS := TMemoryStream.Create;
        try
          try
            MS.LoadFromFile(Filename);
            SetString(S, PChar(MS.Memory), MS.Size);
          finally
            MS.Free;
          end;
        except
        end;
      end;
    end;
  end;
  Params.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  HintWindow.Free;
end;

procedure TForm1.FrameViewerRightClick(Sender: TObject; Parameters: TRightClickParameters);
var
  Pt: TPoint;
  S, Dest: string;
  I: integer;
  Viewer: ThtmlViewer;
  HintWin: THintWindow;
  ARect: TRect;
begin
  Viewer := Sender as ThtmlViewer;
  with Parameters do
  begin
    FoundObject := Image;
    ViewImage.Enabled := (FoundObject <> Nil) and (FoundObject.Bitmap <> Nil);
    CopyImageToClipboard.Enabled := (FoundObject <> Nil) and (FoundObject.Bitmap <> Nil);
    if URL <> '' then
    begin
      S := URL;
      I := Pos('#', S);
      if I >= 1 then
      begin
        Dest := System.Copy(S, I, 255);  {local destination}
        S := System.Copy(S, 1, I-1);     {the file name}
      end
      else
        Dest := '';    {no local destination}
      if S = '' then
        S := Viewer.CurrentFile
      else
        S := Viewer.HTMLExpandFileName(S);
      NewWindowFile := S+Dest;
      OpenInNewWindow.Enabled := FileExists(S);
    end
    else
      OpenInNewWindow.Enabled := False;

    GetCursorPos(Pt);
    if Length(CLickWord) > 0 then
    begin
      HintWin := THintWindow.Create(Self);
      try
        ARect := Rect(0,0,0,0);
        DrawTextW(HintWin.Canvas.Handle, @ClickWord[1], Length(ClickWord), ARect, DT_CALCRECT);
        with ARect do
          HintWin.ActivateHint(Rect(Pt.X+20, Pt.Y-(Bottom-Top)-15, Pt.x+30+Right, Pt.Y-15), ClickWord);
        PopupMenu1.Popup(Pt.X, Pt.Y);
      finally
        HintWin.Free;
      end;
    end
    else
      PopupMenu1.Popup(Pt.X, Pt.Y);
  end;
end;
  
procedure TForm1.OpenInNewWindowClick(Sender: TObject);
var
  PC: array[0..255] of char;
  PC2: array[0..255] of char;
begin
//  WinExec(StrPCopy(PC, ParamStr(0)+' "'+NewWindowFile+'"'), sw_Show);
{$IFDEF MSWINDOWS}
  ShellExecute(Handle, nil, StrPCopy(PC, ParamStr(0)), 
               StrPCopy(PC2, NewWindowFile), nil, SW_SHOWNORMAL); 
{$ELSE}
 {$IFDEF LCLCarbon}
  OpenDocument(NewWindowFile);
 {$ELSE}
  OpenDocument(NewWindowFile); // Shell('"' + ParamStr(0) + '" "' + NewWindowFile + '"');
 {$ENDIF}
{$ENDIF}
end;

procedure TForm1.PrinterSetupClick(Sender: TObject);
begin
{$IFNDEF LCLCarbon}
  PrinterSetupDialog.Execute;
{$ELSE}
  MessageDlg('Not yet supported with Carbon widgetset.', mtError, [mbOK], 0);
{$ENDIF}
end;

procedure TForm1.PrintPreview1Click(Sender: TObject);
{$IFNDEF LCL}
var
  pf: TPreviewForm;
  Viewer: ThtmlViewer;
  Abort: boolean;
{$ENDIF}
begin
{$IFNDEF LCL}
  Viewer := FrameViewer.ActiveViewer;
  if Assigned(Viewer) then
  begin
    pf := TPreviewForm.CreateIt(Self, Viewer, Abort);
    try
      if not Abort then
        pf.ShowModal;
    finally
      pf.Free;
    end;
  end;
{$ENDIF}
end;

procedure TForm1.FrameViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TitleStr: string;
begin
  if not Timer1.Enabled and Assigned(ActiveControl) and ActiveControl.Focused
         and (Sender is ThtmlViewer) then  
  begin
    TitleViewer := ThtmlViewer(Sender);
    TitleStr := TitleViewer.TitleAttr;
    if TitleStr = '' then
      OldTitle := ''
    else if TitleStr <> OldTitle then
    begin
      TimerCount := 0;
      Timer1.Enabled := True;
      OldTitle := TitleStr;
    end;
  end;
end;

procedure TForm1.CloseAll;
begin
  Timer1.Enabled := False;
  HintWindow.ReleaseHandle;
  HintVisible := False;
  TitleViewer := Nil;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
  StartCount = 2; {timer counts before hint window opens}
  EndCount = 20;  {after this many timer counts, hint window closes}
var
  Pt, Pt1: TPoint;
  ARect: TRect;
  TitleStr: string;

begin
  if not Assigned(TitleViewer) then
  begin
    CloseAll;
    Exit;
  end;
  Inc(TimerCount);
  GetCursorPos(Pt);
  try       {in case TitleViewer becomes corrupted}
    Pt1 := TitleViewer.ScreenToClient(Pt);
    TitleStr := TitleViewer.TitleAttr;
    if (TitleStr = '') or not PtInRect(TitleViewer.ClientRect, Pt1)then
    begin
      OldTitle := '';
      CloseAll;
      Exit;
    end;
    if TitleStr <> OldTitle then
    begin
      TimerCount := 0;
      OldTitle := TitleStr;
      HintWindow.ReleaseHandle;
      HintVisible := False;
      Exit;
    end;
    if TimerCount > EndCount then
      CloseAll
    else if (TimerCount >= StartCount) and not HintVisible then
    begin
      ARect := HintWindow.CalcHintRect(300, TitleStr, Nil);
      with ARect do
        HintWindow.ActivateHint(Rect(Pt.X, Pt.Y+18, Pt.X+Right, Pt.Y+18+Bottom), TitleStr);
      HintVisible := True;
    end;
  except
    CloseAll;
  end;
end;

procedure TForm1.FrameViewerProgress(Sender: TObject;
  Stage: TProgressStage; PercentDone: Integer);
begin
  ProgressBar.Position := PercentDone;
  case Stage of
    psStarting:
      ProgressBar.Visible := True;
    psRunning:;
    psEnding:
      ProgressBar.Visible := False;
  end;
  ProgressBar.Update;
end;

procedure TForm1.SetPrintScaleClick(Sender: TObject);
var
  S: string;
begin
  S := FloatToStr(FrameViewer.PrintScale);
  try
    if InputQuery('PrintScale', 'Enter desired print scale value', S) then
      FrameViewer.PrintScale := StrToFloat(S);
  except
  end;
end;

{HTML for print header and footer}
const
  HFText: string =  '<html><head><style>'+
            'body  {font: Arial 8pt;}'+
          '</style></head>'+
          '<body marginwidth="0">'+
          '<table border="0" cellspacing="2" cellpadding="1" width="100%">'+
            '<tr>'+
              '<td>#left</td><td align="right">#right</td>'+
            '</tr>'+
          '</table></body></html>';

function ReplaceStr(Const S, FromStr, ToStr: string): string;
{replace FromStr with ToStr in string S.}
begin
  Result := StringReplace(S, FromStr, ToStr, []); // rfReplaceAll
{  I := Pos(FromStr, S);
  if I > 0 then
  begin
    Result := S;
    Delete(Result, I, Length(FromStr));
    Insert(ToStr, Result, I);
  end; }
end;

procedure TForm1.ViewerPrintHTMLHeader(Sender: TObject;
  HFViewer: THTMLViewer; NumPage: Integer; LastPage: boolean; var XL, XR: integer; var StopPrinting: Boolean);
var
  S: string;
begin
  S := ReplaceStr(HFText, '#left', FrameViewer.DocumentTitle);
  S := ReplaceStr(S, '#right', FrameViewer.CurrentFile);
  HFViewer.LoadFromString(S);
end;

procedure TForm1.ViewerPrintHTMLFooter(Sender: TObject;
  HFViewer: THTMLViewer; NumPage: Integer; LastPage: boolean; var XL, XR: integer; var StopPrinting: Boolean);
var
  S: string;
begin
  S := ReplaceStr(HFText, '#left', DateToStr(Date));
  S := ReplaceStr(S, '#right', 'Page '+IntToStr(NumPage));
  HFViewer.LoadFromString(S);
end;


end.
