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

unit uError;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
  Errorhandler.
  Use command line switch '-traceon' to display trace messages runtime.
}

interface

{$IFNDEF Release}
uses SysUtils, Forms, StdCtrls;
{$ENDIF}


type
  TTraceMode = (trOff, trShowWindow);

  TErrorHandler = class
  private
    {$IFNDEF Release}
    TraceMode: TTraceMode;
    TraceWindow: TForm;
    TraceMemo: TMemo;
    {$ENDIF}
  public
    constructor Create;
    procedure SetTraceMode(Mode: TTraceMode);
    procedure Trace(const Msg: string);
  end;


var
  ErrorHandler: TErrorHandler;


implementation

{$IFNDEF Release}
uses LCLIntf, LCLType, LMessages, Controls;
{$ENDIF}


{-------------------}
{ ErrorHandler }

constructor TErrorHandler.Create;
begin
  inherited Create;
  {$IFNDEF Release}
  SetTraceMode(trOff);
  {$ENDIF}
end;


procedure TErrorHandler.SetTraceMode(Mode: TTraceMode);
{
  Avgör hur tracemeddelanden visas.
  trOff
    Stänger av trace
  trWinDebug
    Meddelande skrivs till eventlog
    Använd View - Debug Windows - Event log i delphi för att visa dessa meddelanden
  trShowWindow
    Tracemeddelanden visas i ett eget fönster
}
begin
{$IFNDEF Release}
  TraceMode := Mode;
  if (TraceMode = trShowWindow) and (TraceWindow = nil) then
  begin
    TraceWindow := TForm.Create(Application);
    //Fönstret skapas nere i högra hörnet, med full bredd.
    with TraceWindow do
    begin
      FormStyle := fsStayOnTop;
      Left := Screen.Width - 300;
      Top := (Screen.Height div 3) * 2;
      Width := Screen.Width - 100;
      Height := Screen.Height div 3;
    end;
    TraceMemo := TMemo.Create(TraceWindow);
    with TraceMemo do
    begin
      Parent := TraceWindow;
      Align := alClient;
      ScrollBars := ssVertical;
    end;
    TraceWindow.Show;
  end;
{$ENDIF}
end;


procedure TErrorHandler.Trace(const Msg: string);
begin
{$IFNDEF Release}
  case TraceMode of
    trOff: ;
    trShowWindow: TraceMemo.Lines.Add(Msg);
  end;
{$ENDIF}
end;

{--------------------}


initialization

  ErrorHandler := TErrorHandler.Create;
  {$IFNDEF Release}
  if FindCmdLineSwitch('traceon', ['-', '/'], True) then
    ErrorHandler.SetTraceMode(trShowWindow);
  {$ENDIF}

finalization

  ErrorHandler.Free;

end.