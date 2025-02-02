

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

Unit download;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls,
  httpsend, synsock, ExtCtrls, BaseForm, utils;

Resourcestring
  SDownloadProgress = '%s of %s downloaded';
  SDownloadProgress2 = '%s downloaded';

Type
  TDownloadThread = Class;

  { TDownloadForm }

  TDownloadForm = Class(TBaseForm)
    btCancel: TButton;
    UpdateTimer: TTimer;
    txPercent: TLabel;
    txBytes: TLabel;
    txFileName: TLabel;
    pbDownload: TProgressBar;
    Procedure btCancelClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure UpdateTimerTimer(Sender: TObject);
  Private
    FThread: TDownloadThread;
    FTotalSize: Int64;
    FDownloaded: Int64;
    FError: String;

    Procedure UpdateStatus(Data: PtrInt);
  Public
    { public declarations }
  End;

  { TDownloadThread }

  TDownloadThread = Class(TThread)
  Private
    FHttp: THTTPSend;
    FForm: TDownloadForm;
    FUrl: String;
    FDestFileName: String;
    FOut: TFileStreamUTF8;

    Procedure DoMonitor(Sender: TObject; Writing: Boolean;
      Const Buffer: TMemory; Len: Integer);
    Procedure WriteToFile;
  Protected
    Procedure Execute; Override;
  End;

Function DownloadFile(Const URL, DestFolder: String; Const DestFileName: String = '';
  Const DisplayName: String = ''): Boolean;

Implementation

Uses Main, rpc;

Function DownloadFile(Const URL, DestFolder: String;
  Const DestFileName, DisplayName: String): Boolean;
Var
  s: String;
Begin
  With TDownloadForm.Create(Application) Do
  Try
    s := ExtractFileName(StringReplace(URL, '/', DirectorySeparator,
      [rfReplaceAll]));
    If DisplayName <> '' Then
      txFileName.Caption := DisplayName
    Else
      txFileName.Caption := s;
    If DestFileName <> '' Then
      s := DestFileName;
    FThread.FUrl := URL;
    FThread.FDestFileName := IncludeTrailingPathDelimiter(DestFolder) + s;
    FThread.Suspended := False;
    Result := ShowModal = mrOk;
  Finally
    Free;
  End;
End;

{ TDownloadThread }

Procedure TDownloadThread.DoMonitor(Sender: TObject; Writing: Boolean;
  Const Buffer: TMemory; Len: Integer);
Begin
  If Terminated Then
  Begin
    FHttp.Abort;
    exit;
  End;

  If FHttp.DownloadSize <> 0 Then
  Begin
    FForm.FTotalSize := FHttp.DownloadSize;
    Inc(FForm.FDownloaded, Len);
    WriteToFile;
  End;
End;

Procedure TDownloadThread.WriteToFile;
Begin
  If FOut = nil Then
    FOut := TFileStreamUTF8.Create(FDestFileName, fmCreate);

  FHttp.Document.Position := 0;
  FOut.CopyFrom(FHttp.Document, FHttp.Document.Size);
  FHttp.Document.Clear;
End;

Procedure TDownloadThread.Execute;
Var
  res: PtrInt;
Begin
  res := 1;
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
      FHttp.Sock.OnMonitor := @DoMonitor;
      If FHttp.HTTPMethod('GET', FUrl) Then
      Begin
        If FHttp.ResultCode = 200 Then
        Begin
          FForm.FDownloaded := FHttp.DownloadSize;
          WriteToFile;
          res := 2;
        End
        Else
          If Not Terminated Then
            FForm.FError := Format('HTTP error: %d', [FHttp.ResultCode]);
      End
      Else
        If Not Terminated Then
          FForm.FError := FHttp.Sock.LastErrorDesc;
    Finally
      FHttp.Free;
    End;
  Except
    FForm.FError := Exception(ExceptObject).Message;
  End;
  FOut.Free;
  If res = 1 Then
    DeleteFile(FDestFileName);
  Application.QueueAsyncCall(@FForm.UpdateStatus, res);
  FForm.FThread := nil;
End;

{ TDownloadForm }

Procedure TDownloadForm.FormCreate(Sender: TObject);
Begin
  bidiMode := GetBiDi();
  FThread := TDownloadThread.Create(True);
  FThread.FreeOnTerminate := True;
  FThread.FForm := Self;
  UpdateTimerTimer(nil);
End;

Procedure TDownloadForm.btCancelClick(Sender: TObject);
Begin
  btCancel.Enabled := False;
  FThread.Terminate;
End;

Procedure TDownloadForm.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  If FThread <> nil Then
  Begin
    CloseAction := caNone;
    btCancel.Click;
  End;
End;

Procedure TDownloadForm.FormDestroy(Sender: TObject);
Begin
End;

Procedure TDownloadForm.FormResize(Sender: TObject);
Begin
  btCancel.Left := (ClientWidth - btCancel.Width) Div 2;
End;

Procedure TDownloadForm.FormShow(Sender: TObject);
Begin
  FormResize(nil);
End;

Procedure TDownloadForm.UpdateTimerTimer(Sender: TObject);
Begin
  If FTotalSize <> 0 Then
  Begin
    pbDownload.Max := FTotalSize;
    pbDownload.Position := FDownloaded;
    txPercent.Caption := Format('%.1f%%', [FDownloaded * 100 / FTotalSize]);
    txBytes.Caption := Format(SDownloadProgress,
      [GetHumanSize(FDownloaded), GetHumanSize(FTotalSize)]);
    txPercent.Show;
  End
  Else
  Begin
    txBytes.Caption := Format(SDownloadProgress2, [GetHumanSize(FDownloaded)]);
    txPercent.Hide;
  End;
End;

Procedure TDownloadForm.UpdateStatus(Data: PtrInt);
Begin
  If Data <> 0 Then
  Begin
    If Data = 2 Then
      ModalResult := mrOk
    Else
    Begin
      If FError <> '' Then
        MessageDlg(FError, mtError, [mbOK], 0);
      ModalResult := mrCancel;
    End;
    exit;
  End;
End;

Initialization
  {$I download.lrs}

End.
