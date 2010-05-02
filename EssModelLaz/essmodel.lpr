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

  For more info about ESS-Model please visit:
    www.essmodel.com
    www.sourceforge.net/projects/essmodel
}

program EssModel;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$R 'Res\files\essModelFiles.res' 'Res\files\essModelFiles.rc'}
{%File 'EssModelReadme.txt'}

{.$R 'components\WindowsXP.res'}

uses
  Forms, Interfaces,
  uMainForm in 'System/uMainForm.pas' {MainForm},
  uListeners in 'Model/uListeners.pas',
  uDelphiIntegrator in 'Integrator/Code/Delphi/uDelphiIntegrator.pas',
  uIntegrator in 'Integrator/uIntegrator.pas',
  uModel in 'Model/uModel.pas',
  uCodeProvider in 'CodeProvider/uCodeProvider.pas',
  uFileProvider in 'CodeProvider/uFileProvider.pas',
  uCodeParser in 'Integrator/CodeIO/uCodeParser.pas',
  uDelphiParser in 'Integrator/CodeIO/uDelphiParser.pas',
  uCodeIntegrator in 'Integrator/Code/uCodeIntegrator.pas',
  uViewIntegrator in 'Integrator/View/uViewIntegrator.pas',
  uRtfdComponents in 'Integrator/View/RtfdDiagram/uRtfdComponents.pas',
  uRtfdDiagram in 'Integrator/View/RtfdDiagram/uRtfdDiagram.pas',
  uError in 'System/uError.pas',
  uUseful in 'System/uUseful.pas',
  uModelEntity in 'Model/uModelEntity.pas',
  uParseTree in 'Integrator/CodeIO/uParseTree.pas',
  uConfig in 'System/uConfig.pas',
  uDiagramFrame in 'Integrator/View/uDiagramFrame.pas' {DiagramFrame: TFrame},
  uRtfdDiagramFrame in 'Integrator/View/RtfdDiagram/uRtfdDiagramFrame.pas' {RtfdDiagramFrame: TFrame},
  uIterators in 'Model/uIterators.pas',
  uDocGen in 'Integrator/Doc/uDocGen.pas',
  uDocumentation in 'Model/uDocumentation.pas',
  uJavaClassImport in 'Integrator/CodeIO/JavaClass/uJavaClassImport.pas',
  uJavaClass in 'Integrator/CodeIO/JavaClass/uJavaClass.pas',
  essConnectPanel in 'Components/essConnectPanel.pas',
  uXmiExport in 'Integrator/Export/uXmiExport.pas',
  essLayout in 'Components/essLayout.pas',
  uHtmlDocGen in 'Integrator/Doc/uHtmlDocGen.pas',
  uJavaParser in 'Integrator/CodeIO/uJavaParser.pas',
  SugiyamaLayout in 'Components/SugiyamaLayout.pas',
  uConst in 'System/uConst.pas',
  uAboutForm in 'System/uAboutForm.pas' {AboutForm},
  uRegisterExtension in 'Components/uRegisterExtension.pas',
  uSettingsForm in 'System/uSettingsForm.pas' {SettingsForm},
  uFeedback in 'System/uFeedback.pas',
  uTreeViewFrame in 'Integrator/View/uTreeViewFrame.pas' {TreeViewFrame: TFrame},
  uTreeViewIntegrator in 'Integrator/View/uTreeViewIntegrator.pas',
  {$IFDEF ARGO_XMI} uXmiExportArgoUML in 'Integrator\Export\uXmiExportArgoUML.pas', {$ENDIF}
  uZoomFrame in 'System/uZoomFrame.pas' {ZoomFrame: TFrame},
  uOpenFolderForm in 'System/uOpenFolderForm.pas' {OpenFolderForm};

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
