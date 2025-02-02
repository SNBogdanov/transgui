

{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2019 by Yury Sidorov and Transmission Remote GUI working group.

  Transmission Remote GUI is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Transmission Remote GUI is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Transmission Remote GUI; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

  In addition, as a special exception, the copyright holders give permission to 
  link the code of portions of this program with the
  OpenSSL library under certain conditions as described in each individual
  source file, and distribute linked combinations including the two.

  You must obey the GNU General Public License in all respects for all of the
  code used other than OpenSSL.  If you modify file(s) with this exception, you
  may extend this exception to your version of the file(s), but you are not
  obligated to do so.  If you do not wish to do so, delete this exception
  statement from your version.  If you delete this exception statement from all
  source files in the program, then also delete it here.
*************************************************************************************}

Unit About;

{$mode objfpc}{$H+}

Interface

Uses 
BaseForm, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
Dialogs, StdCtrls, ComCtrls, ExtCtrls, ButtonPanel, lclversion,
ssl_openssl, ssl_openssl_lib;

resourcestring
SErrorCheckingVersion = 'Error checking for new version.';
SNewVersionFound = 'A new version of %s is available.' + LineEnding +
                   'Your current version: %s' + LineEnding +
                   'The new version: %s' + LineEnding + LineEnding +
                   'Do you wish to open the Downloads web page?';
SLatestVersion = 'No updates have been found.' + LineEnding +
                 'You are running the latest version of %s.';

Type 

  { TAboutForm }

  TAboutForm = Class(TBaseForm)
    Bevel1: TBevel;
    Buttons: TButtonPanel;
    edLicense: TMemo;
    imgTransmission: TImage;
    imgSynapse: TImage;
    imgLazarus: TImage;
    txHomePage: TLabel;
    txAuthor: TLabel;
    txVersion: TLabel;
    txAppName: TLabel;
    Page: TPageControl;
    tabAbout: TTabSheet;
    tabLicense: TTabSheet;
    txVersFPC: TLabel;
    Procedure FormCreate(Sender: TObject);
    Procedure imgDonateClick(Sender: TObject);
    Procedure imgLazarusClick(Sender: TObject);
    Procedure imgSynapseClick(Sender: TObject);
    Procedure txHomePageClick(Sender: TObject);
    Private 
    { private declarations }
    Public 
    { public declarations }
  End;

Procedure CheckNewVersion(Async: boolean = True);
Procedure GoHomePage;
Procedure GoGitHub;

Implementation

Uses Main, utils, httpsend;

Type 

  { TCheckVersionThread }

  TCheckVersionThread = Class(TThread)
    Private 
      FHttp: THTTPSend;
      FError: string;
      FVersion: string;
      FExit: boolean;

      Procedure CheckResult;
      Function GetIntVersion(Const Ver: String): integer;
    Protected 
      Procedure Execute;
      override;
  End;

Var 
  CheckVersionThread: TCheckVersionThread;

Procedure CheckNewVersion(Async: boolean);
Begin
  If CheckVersionThread <> Nil Then
    exit;
  Ini.WriteInteger('Interface', 'LastNewVersionCheck', Trunc(Now));
  CheckVersionThread := TCheckVersionThread.Create(True);
  CheckVersionThread.FreeOnTerminate := True;
  If Async Then
    CheckVersionThread.Suspended := False
  Else
    Begin
      CheckVersionThread.Execute;
      CheckVersionThread.FExit := True;
      CheckVersionThread.Suspended := False;
    End;
End;

Procedure GoHomePage;
Begin
  AppBusy;
  OpenURL('https://github.com/transmission-remote-gui/transgui/releases');
  AppNormal;
End;

Procedure GoGitHub;
Begin
  AppBusy;
  OpenURL('https://github.com/transmission-remote-gui/transgui');
  AppNormal;
End;

{ TCheckVersionThread }

Procedure TCheckVersionThread.CheckResult;
Begin
  ForceAppNormal;
  If FError <> '' Then
    Begin
      MessageDlg(SErrorCheckingVersion + LineEnding + FError, mtError, [mbOK], 0
      );
      exit;
    End;

  If GetIntVersion(AppVersion) >= GetIntVersion(FVersion)  Then
    Begin
      MessageDlg(Format(SLatestVersion, [AppName]), mtInformation, [mbOK], 0);
      exit;
    End;

  If MessageDlg(Format(SNewVersionFound, [AppName, AppVersion, FVersion]),
     mtConfirmation, mbYesNo, 0) <> mrYes Then
    exit;

  Application.ProcessMessages;
  AppBusy;
  OpenURL('https://github.com/transmission-remote-gui/transgui/releases');
  AppNormal;
End;

Function TCheckVersionThread.GetIntVersion(Const Ver: String): integer;

Var 
  v: string;
  vi, i, j: integer;
Begin
  Result := 0;
  v := Ver;
  For i:=1 To 3 Do
    Begin
      If v = '' Then
        vi := 0
      Else
        Begin
          j := Pos('.', v);
          If j = 0 Then
            j := MaxInt;
          vi := StrToIntDef(Copy(v, 1, j - 1), 0);
          Delete(v, 1, j);
        End;
      Result := Result shl 8 Or vi;
    End;
End;

Procedure TCheckVersionThread.Execute;
Begin
  If Not FExit Then
    Begin
      Try
        FHttp := THTTPSend.Create;
        Try
          If RpcObj.Http.ProxyHost <> '' Then
            Begin
              FHttp.ProxyHost := RpcObj.Http.ProxyHost;
              FHttp.ProxyPort := RpcObj.Http.ProxyPort;
              FHttp.ProxyUser := RpcObj.Http.ProxyUser;
              FHttp.ProxyPass := RpcObj.Http.ProxyPass;
            End;
          If FHttp.HTTPMethod('GET',

'https://raw.githubusercontent.com/transmission-remote-gui/transgui/master/VERSION.txt'
             ) Then
            Begin
              If FHttp.ResultCode = 200 Then
                Begin
                  SetString(FVersion, FHttp.Document.Memory, FHttp.Document.Size
                  );
                  FVersion := Trim(FVersion);
                End
              Else
                FError := Format('HTTP error: %d', [FHttp.ResultCode]);
            End
          Else
            FError := FHttp.Sock.LastErrorDesc;
        Finally
          FHttp.Free;
    End;
Except
  FError := Exception(ExceptObject).Message;
End;
If (FError <> '') Or (GetIntVersion(FVersion) > GetIntVersion(AppVersion)) Or
   Suspended Then
  If Suspended Then
    CheckResult
Else
  Synchronize(@CheckResult);
End;
If Not Suspended Then
  CheckVersionThread := Nil;
End;

{ TAboutForm }

Procedure TAboutForm.imgSynapseClick(Sender: TObject);
Begin
  AppBusy;
  OpenURL('http://synapse.ararat.cz');
  AppNormal;
End;

Procedure TAboutForm.txHomePageClick(Sender: TObject);
Begin
  GoHomePage;
End;

Procedure TAboutForm.FormCreate(Sender: TObject);
{$ifdef lclcarbon}

Var 
  s: string;
{$endif lclcarbon}
Begin
  bidiMode := GetBiDi();
  txAppName.Font.Size := Font.Size + 2;
  txHomePage.Font.Size := Font.Size;
  BorderStyle := bsSizeable;
  txAppName.Caption := AppName;
  txVersion.Caption := Format(txVersion.Caption, [AppVersion]);
  Page.ActivePageIndex := 0;

  txVersFPC.caption := 'Fpc : ' + {$I %FPCVERSION%} + '   Lazarus : ' +
                       lcl_version;

{$ifdef lclcarbon}
  s := edLicense.Text;
  edLicense.Text := '';
  edLicense.HandleNeeded;
  edLicense.Text := s;
  Buttons.BorderSpacing.Right := Buttons.BorderSpacing.Right + ScaleInt(12);
{$endif lclcarbon}
End;

Procedure TAboutForm.imgDonateClick(Sender: TObject);
Begin
  GoGitHub;
End;

Procedure TAboutForm.imgLazarusClick(Sender: TObject);
Begin
  AppBusy;
  OpenURL('https://www.lazarus-ide.org');
  AppNormal;
End;

initialization
  {$I about.lrs}

End.
