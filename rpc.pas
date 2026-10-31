

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

  You should have received a copy of the GNU General Public License along with
  Transmission Remote GUI; if not, write to the Free Software Foundation, Inc.,
  51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

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

Unit rpc;

{$mode objfpc}{$H+}

Interface

Uses
  {$ifdef windows}
windows,
  {$endif windows}
  Classes, SysUtils, Forms, httpsend, syncobjs, fpjson, jsonparser, variants,
  ssl_openssl, dbugintf,
  ZStream, jsonscanner;

Resourcestring
  sTransmissionAt = 'Transmission%s at %s:%s';

Const
  DefaultRpcPath = '/transmission/rpc';
  ClientFeaturesHeader = 'X-Transmission-Client-Features: status/v2';

Type
  TAdvInfoType = (aiNone, aiGeneral, aiFiles, aiPeers, aiTrackers, aiStats);
  TRefreshTypes = (rtTorrents, rtDetails, rtSession);
  TRefreshType = Set Of TRefreshTypes;

  TRpc = Class;

  { TRpcThread }

  TRpcThread = Class(TThread)
  Private
    ResultData: TJSONData;
    FRpc: TRpc;
    FGeneration: Integer;

    Function IsCurrent: Boolean;
    Function GetAdvInfo: TAdvInfoType;
    Function GetCurTorrentId: Cardinal;
    Function GetRefreshInterval: TDateTime;
    Function GetStatus: String;
    Procedure SetStatus(Const AValue: String);

    Function GetTorrents: Boolean;
    Procedure GetPeers(TorrentId: Integer);
    Procedure GetFiles(TorrentId: Integer);
    Procedure GetTrackers(TorrentId: Integer);
    Procedure GetStats;
    Procedure GetInfo(TorrentId: Integer);
    Procedure GetSessionInfo;
    Procedure GetFreeSpace;

    Procedure DoFillTorrentsList;
    Procedure DoFillPeersList;
    Procedure DoFillFilesList;
    Procedure DoFillInfo;
    Procedure DoFillTrackersList;
    Procedure DoFillStats;
    Procedure DoFillSessionInfo;
    Procedure NotifyCheckStatus;
    Procedure CheckStatusHandler(Data: PtrInt);
  Protected
    Procedure Execute; Override;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Property Status: String read GetStatus write SetStatus;
    Property RefreshInterval: TDateTime read GetRefreshInterval;
    Property CurTorrentId: Cardinal read GetCurTorrentId;
    Property AdvInfo: TAdvInfoType read GetAdvInfo;
  End;

  TRpc = Class
  Private
    FLock: TCriticalSection;
    FStatus: String;
    FInfoStatus: String;
    FConnected: Boolean;
    FGeneration: Integer;
    FMainThreadRequestActive: Boolean;
    FTorrentFields: String;
    FRPCVersion: Integer;
    FForkFeatures: TStringList;
    FForkFeaturesLoaded: Boolean;
    XTorrentSession: String;
    FMainThreadId: TThreadID;
    FRpcPath: String;

    Function GetConnected: Boolean;
    Function GetConnecting: Boolean;
    Function GetInfoStatus: String;
    Function GetStatus: String;
    Function GetTorrentFields: String;
    Procedure SetInfoStatus(Const AValue: String);
    Procedure SetStatus(Const AValue: String);
    Procedure SetTorrentFields(Const AValue: String);
    Procedure CreateHttp;
    Function SendRequestRaw(req: TJSONObject; ReturnArguments: Boolean;
      ATimeOut: Integer): TJSONObject;
  Public
    Http: THTTPSend;
    HttpLock: TCriticalSection;
    RpcThread: TRpcThread;
    Url: String;
    RefreshInterval: TDateTime;
    CurTorrentId: Cardinal;
    AdvInfo: TAdvInfoType;
    RefreshNow: TRefreshType;
    RequestFullInfo: Boolean;
    ReconnectAllowed: Boolean;
    RequestStartTime: TDateTime;
    IncompleteDir: String;
    ForceRequest: Integer;

    Constructor Create;
    Destructor Destroy; Override;
    Procedure InitSSL;

    Procedure Lock;
    Procedure Unlock;

    Procedure Connect;
    Procedure Disconnect;

    Procedure ResetForkFeatures;
    Procedure DetectForkFeatures;
    Function HasForkFeature(Const AName: String; AVersion: Integer = 1): Boolean;

    Function SendRequest(req: TJSONObject; ReturnArguments: Boolean = True;
      ATimeOut: Integer = -1): TJSONObject;
    Function RequestInfo(TorrentId: Integer; Const Fields: Array Of Const;
      Const ExtraFields: Array Of String): TJSONObject;
    Function RequestInfo(TorrentId: Integer;
      Const Fields: Array Of Const): TJSONObject;
    Function RequestInfos(TorrentIds: Variant; Const Fields: Array Of Const;
      Const ExtraFields: Array Of String): TJSONObject;
    Function RequestInfos(TorrentIds: Variant;
      Const Fields: Array Of Const): TJSONObject;

    Property Status: String read GetStatus write SetStatus;
    Property InfoStatus: String read GetInfoStatus write SetInfoStatus;
    Property Connected: Boolean read GetConnected;
    Property Connecting: Boolean read GetConnecting;
    Property TorrentFields: String read GetTorrentFields write SetTorrentFields;
    Property RPCVersion: Integer read FRPCVersion;
    Property RpcPath: String read FRpcPath write FRpcPath;
  End;

Var
  RemotePathDelimiter: Char = '/';

Implementation

Uses Main, ssl_openssl_lib, synafpc, blcksock;

Function TranslateTableToObjects(reply: TJSONObject): TJSONObject;
Var
  array_tor, fields, out_torrents: TJSONArray;
  object_tor: TJSONObject;
  i, j: Integer;
Begin
  out_torrents := TJSONArray.Create;
  If (reply = nil) Or (reply.Arrays['torrents'] = nil) Or
    (reply.Arrays['torrents'].Count = 0) Then
    Result := TJSONObject.Create(['torrents', out_torrents])
  Else
  Begin
    fields := reply.Arrays['torrents'].Arrays[0];
    For i := 1 To reply.Arrays['torrents'].Count - 1 Do
    Begin
      array_tor := reply.Arrays['torrents'].Arrays[i];
      object_tor := TJSONObject.Create;
      For j := 0 To fields.Count - 1 Do
        object_tor.Add(fields.Items[j].AsString,
          array_tor.Items[j].Clone);

      out_torrents.Add(object_tor);
    End;
    Result := TJSONObject.Create(['torrents', out_torrents]);
    reply.Free;
  End;
End;

{ TRpcThread }

Procedure TRpcThread.Execute;
Var
  t, tt: TDateTime;
  i: Integer;
  ai: TAdvInfoType;
Begin
  Try
    Try
      GetSessionInfo;
      NotifyCheckStatus;
    Except
      OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
        '(GetSessionInfo)'));
    End;
    If Not FRpc.FConnected Then
      Terminate;

    t := Now - RefreshInterval;
    tt := Now - RefreshInterval * 5;
    While Not Terminated Do
    Begin
      If (Now - t >= RefreshInterval) Or (FRpc.ForceRequest > 0) Then
      Begin
        FRpc.RefreshNow := FRpc.RefreshNow + [rtTorrents, rtDetails];
        t := Now;

        If FRpc.ForceRequest > 0 Then
          FRpc.ForceRequest := FRpc.ForceRequest - 1;
      End;
      If Now - tt >= RefreshInterval * 5 Then
      Begin
        Include(FRpc.RefreshNow, rtSession);
        tt := Now;
      End;

      If Status = '' Then
        If rtSession In FRpc.RefreshNow Then
        Begin
          Try
            GetSessionInfo;
          Except
            OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
              '(GetSessionInfo)'));
          End;
          Try
            GetFreeSpace;
          Except
            OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
              '(GetFreeSpace)'));
          End;
          Exclude(FRpc.RefreshNow, rtSession);
          //FRpc.RefreshNow:=FRpc.RefreshNow + [rtTorrents];
        End
        Else
          If rtDetails In FRpc.RefreshNow Then
          Begin
            i := CurTorrentId;
            ai := AdvInfo;
            Try
              If i <> 0 Then
              Begin
                Case ai Of
                  aiGeneral:
                    GetInfo(i);
                  aiPeers:
                    GetPeers(i);
                  aiFiles:
                    GetFiles(i);
                  aiTrackers:
                    GetTrackers(i);
                End;
                Case ai Of
                  aiStats:
                    GetStats;
                End;

              End;
            Except
              OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
                '(rtDetails)'));
            End;
            If (i = CurTorrentId) And (ai = AdvInfo) Then
              Exclude(FRpc.RefreshNow, rtDetails);
          End
          Else
            If rtTorrents In FRpc.RefreshNow Then
            Begin
              Try
                GetTorrents;
              Except
                OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
                  '(GetTorrents)'));
              End;

              Exclude(FRpc.RefreshNow, rtTorrents);
              t := Now;
            End;

      If Status <> '' Then
      Begin
        NotifyCheckStatus;
        Sleep(100);
      End;

      If FRpc.RefreshNow = [] Then
        Sleep(50);
    End;
  Except
    Status := Exception(ExceptObject).Message;
    FRpc.ReconnectAllowed := True;
    If Status <> '' Then OutputDebugString(LPCSTR(Status));
    //    FRpc.RpcThread.destroy;
    NotifyCheckStatus;
  End;
  // Do not let a finishing old thread clear the state of a newer connection.
  If FRpc.RpcThread = Self Then
  Begin
    FRpc.RpcThread := nil;
    FRpc.FConnected := False;
    FRpc.FRPCVersion := 0;
    FRpc.ResetForkFeatures;
  End;
  Sleep(20);
End;

Procedure TRpcThread.GetFreeSpace;
Var
  i: Integer;
  req, args2: TJSONObject;
Begin
  If FreeSpacePaths = nil Then
    exit;
  For i := 0 To FreeSpacePaths.Count - 1 Do
  Begin
    //    Application.ProcessMessages;
    req := TJSONObject.Create;
    req.Add('method', 'free-space');
    args2 := TJSONObject.Create;
    args2.Add('path', FreeSpacePaths.Keys[i]);
    req.Add('arguments', args2);
    Try
      args2 := RpcObj.SendRequest(req, True, 20000);
      //args2:=RpcObj.SendRequest(req);
      If args2 <> nil Then
        FreeSpacePaths[FreeSpacePaths.Keys[i]] :=
          '(' + Format(SFreeSpace,
          [GetHumanSize(args2.Floats['size-bytes'])]) + ')';
    Finally
      args2.Free;
      RpcObj.Status := '';
      req.Free;
      MainForm.CheckStatus(True);
    End;
  End;

End;

Constructor TRpcThread.Create;
Begin
  Inherited Create(True);
End;

Destructor TRpcThread.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TRpcThread.SetStatus(Const AValue: String);
Begin
  FRpc.Status := AValue;
End;

Procedure TRpcThread.DoFillTorrentsList;
Begin
  If Not IsCurrent Then
    exit;
  MainForm.FillTorrentsList(ResultData As TJSONArray);
End;

Procedure TRpcThread.DoFillPeersList;
Begin
  If Not IsCurrent Then
    exit;
  MainForm.FillPeersList(ResultData As TJSONArray);
End;

Procedure TRpcThread.DoFillFilesList;
Var
  t: TJSONObject;
  dir: WideString;
Begin
  If Not IsCurrent Then
    exit;
  If ResultData = nil Then
  Begin
    MainForm.ClearDetailsInfo;
    exit;
  End;
  t := ResultData As TJSONObject;
  If RpcObj.RPCVersion >= 4 Then
    dir := WideString(t.Strings['downloadDir'])
  Else
    dir := '';
  MainForm.FillFilesList(t.Integers['id'], t.Arrays['files'],
    t.Arrays['priorities'], t.Arrays['wanted'], dir);
End;

Procedure TRpcThread.DoFillInfo;
Begin
  If Not IsCurrent Then
    exit;
  MainForm.FillGeneralInfo(ResultData As TJSONObject);
End;

Procedure TRpcThread.DoFillTrackersList;
Begin
  If Not IsCurrent Then
    exit;
  MainForm.FillTrackersList(ResultData As TJSONObject);
End;

Procedure TRpcThread.DoFillStats;
Begin
  If Not IsCurrent Then
    exit;
  MainForm.FillStatistics(ResultData As TJSONObject);
End;

Procedure TRpcThread.DoFillSessionInfo;
Begin
  If Not IsCurrent Then
    exit;
  MainForm.FillSessionInfo(ResultData As TJSONObject);
End;

Procedure TRpcThread.NotifyCheckStatus;
Begin
  If Not Terminated Then
    Application.QueueAsyncCall(@CheckStatusHandler, 0);
End;

Procedure TRpcThread.CheckStatusHandler(Data: PtrInt);
Begin
  If Not IsCurrent Then exit;
  If csDestroying In MainForm.ComponentState Then exit;
  MainForm.CheckStatus(True);
End;

Procedure TRpcThread.GetSessionInfo;
Var
  req, args, args2: TJSONObject;
  s: String;
Begin
  req := TJSONObject.Create;
  Try
    req.Add('method', 'session-get');
    //req.Add('jsonrpc', '1.0');
    args := FRpc.SendRequest(req);
    If args <> nil Then
    Try
      FRpc.FConnected := True;
      FRpc.IncompleteDir := '';
      If args.IndexOfName('incomplete-dir-enabled') >= 0 Then
        If args.Booleans['incomplete-dir-enabled'] Then
          If args.IndexOfName('incomplete-dir') >= 0 Then
            FRpc.IncompleteDir := args.Strings['incomplete-dir'];
      If args.IndexOfName('rpc-version') >= 0 Then
        FRpc.FRPCVersion := args.Integers['rpc-version']
      Else
        FRpc.FRPCVersion := 0;
      FRpc.DetectForkFeatures;
      If args.IndexOfName('version') >= 0 Then
        s := ' ' + args.Strings['version']
      Else
        s := '';
      FRpc.InfoStatus := Format(sTransmissionAt,
        [s, FRpc.Http.TargetHost, FRpc.Http.TargetPort]);
      If FRpc.RPCVersion >= 15 Then
      Begin
        // Requesting free space in download dir
        req.Free;
        req := TJSONObject.Create;
        req.Add('method', 'free-space');
        args2 := TJSONObject.Create;
        Try
          args2.Add('path', args.Strings['download-dir']);
          req.Add('arguments', args2);
          args2 := FRpc.SendRequest(req);
          If args2 <> nil Then
            args.Floats['download-dir-free-space'] :=
              args2.Floats['size-bytes']
          Else
          Begin
            args.Floats['download-dir-free-space'] := -1;
            FRpc.Status := '';
          End;
          If FRpc.IncompleteDir <> '' Then
          Begin
            req.Free;
            req := TJSONObject.Create;
            req.Add('method', 'free-space');
            args2 := TJSONObject.Create;
            args2.Add('path', FRpc.IncompleteDir);
            req.Add('arguments', args2);
            args2 := FRpc.SendRequest(req);
            If args2 <> nil Then
              args.Floats['incomplete-dir-free-space'] :=
                args2.Floats['size-bytes']
            Else
            Begin
              args.Floats['incomplete-dir-free-space'] := -1;
              FRpc.Status := '';
            End;
          End;
        Finally
          args2.Free;
        End;
      End;
      ResultData := args;
      If Not Terminated Then
        Synchronize(@DoFillSessionInfo);
    Finally
      args.Free;
    End
    Else
      ASSERT(FRpc.Status <> '');
  Finally
    req.Free;
  End;
End;

Function TRpcThread.GetTorrents: Boolean;
Var
  args: TJSONObject;
  ExtraFields: Array Of String;
  sl: TStringList;
  i: Integer;
Begin
  Result := False;
  sl := TStringList.Create;
  Try
    FRpc.Lock;
    Try
      sl.CommaText := FRpc.FTorrentFields;
    Finally
      FRpc.Unlock;
    End;

    If FRpc.RPCVersion < 7 Then
    Begin
      i := sl.IndexOf('trackers');
      If FRpc.RequestFullInfo Then
      Begin
        If i < 0 Then
          sl.Add('trackers');
      End
      Else
        If i >= 0 Then
          sl.Delete(i);
    End;

    //i := sl.IndexOf('downloadDir');
    //If FRpc.RequestFullInfo Then
    //Begin
    //  If i < 0 Then
    //    sl.Add('downloadDir');
    //End
    //Else
    //  If i >= 0 Then
    //    sl.Delete(i);

    SetLength(ExtraFields, sl.Count);
    For i := 0 To sl.Count - 1 Do
      ExtraFields[i] := sl[i];
  Finally
    sl.Free;
  End;

  args := FRpc.RequestInfo(0, ['id', 'name', 'status', 'error', 'errorString',
    'announceResponse', 'recheckProgress', 'sizeWhenDone', 'leftUntilDone',
    'percentDone', 'rateDownload', 'rateUpload', 'trackerStats','downloadDir',  'percentComplete',
    'metadataPercentComplete'], ExtraFields);
  Try
    If (args <> nil) And Not Terminated Then
    Begin
      Try
        FRpc.RequestFullInfo := False;
        ResultData := args.Arrays['torrents'];
        Synchronize(@DoFillTorrentsList);
        Result := True;
      Except
        OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
          '(GetTorrents 0)'));
      End;
    End;
  Finally
    args.Free;
  End;
End;

Procedure TRpcThread.GetPeers(TorrentId: Integer);
Var
  args: TJSONObject;
  t: TJSONArray;
Begin
  args := FRpc.RequestInfo(TorrentId, ['peers']);
  Try
    If args <> nil Then
    Begin
      t := args.Arrays['torrents'];
      If t.Count > 0 Then
        ResultData := t.Objects[0].Arrays['peers']
      Else
        ResultData := nil;
      If Not Terminated Then
        Synchronize(@DoFillPeersList);
    End;
  Finally
    args.Free;
  End;
End;

Procedure TRpcThread.GetFiles(TorrentId: Integer);
Var
  args: TJSONObject;
  t: TJSONArray;
Begin
  args := FRpc.RequestInfo(TorrentId, ['id', 'files', 'priorities',
    'wanted', 'downloadDir']);
  Try
    If args <> nil Then
    Begin
      t := args.Arrays['torrents'];
      If t.Count > 0 Then
        ResultData := t.Objects[0]
      Else
        ResultData := nil;
      If Not Terminated Then
        Synchronize(@DoFillFilesList);
    End;
  Finally
    args.Free;
  End;
End;

Procedure TRpcThread.GetTrackers(TorrentId: Integer);
Var
  args: TJSONObject;
  t: TJSONArray;
Begin
  args := FRpc.RequestInfo(TorrentId, ['id', 'trackers', 'trackerStats',
    'nextAnnounceTime']);
  Try
    If args <> nil Then
    Begin
      t := args.Arrays['torrents'];
      If t.Count > 0 Then
        ResultData := t.Objects[0]
      Else
        ResultData := nil;
      If Not Terminated Then
        Synchronize(@DoFillTrackersList);
    End;
  Finally
    args.Free;
  End;
End;

Procedure TRpcThread.GetStats;
Var
  req, args: TJSONObject;
Begin
  req := TJSONObject.Create;
  Try
    req.Add('method', 'session-stats');
    args := FRpc.SendRequest(req);
    If args <> nil Then
    Try
      ResultData := args;
      If Not Terminated Then
        Synchronize(@DoFillStats);
    Finally
      args.Free;
    End;
  Finally
    req.Free;
  End;
End;

Procedure TRpcThread.GetInfo(TorrentId: Integer);
Var
  args: TJSONObject;
  s1: TJSONObject;
  r: TJSONObject;
  t: TJSONArray;
  s:string;
Begin
  args := FRpc.RequestInfo(TorrentId, ['totalSize', 'sizeWhenDone',
    'leftUntilDone', 'pieceCount', 'pieceSize', 'haveValid', 'hashString',
    'comment', 'downloadedEver', 'uploadedEver', 'corruptEver',
    'error', 'errorString', 'announceResponse', 'downloadLimit', 'downloadLimitMode',
    'uploadLimit', 'uploadLimitMode', 'maxConnectedPeers', 'nextAnnounceTime',
    'dateCreated', 'creator', 'eta', 'peersSendingToUs', 'seeders',
    'peersGettingFromUs', 'leechers', 'uploadRatio', 'addedDate',
    'doneDate', 'activityDate', 'downloadLimited', 'uploadLimited',
    'downloadDir', 'incomplete_dir', 'incomplete-dir', 'id', 'pieces',
    'trackerStats', 'secondsDownloading', 'secondsSeeding', 'magnetLink',
    'isPrivate', 'labels', 'sequentialDownload']);
  Try
    If args <> nil Then
    Begin
      t := args.Arrays['torrents'];
      If t.Count > 0 Then
      Begin
        //r := t.Objects[0];
        //s:=r.AsJSON;
        //s1 := GetJSON(s)  as TJSONObject;
        //r.Add('downloadDir',s1.Strings['download-dir']);
        //ResultData:=r;
        ResultData:=t.Objects[0];
      end
      Else
        ResultData := nil;
      If Not Terminated Then
        Synchronize(@DoFillInfo);
    End;
  Finally
    args.Free;
  End;
End;

Function TRpcThread.IsCurrent: Boolean;
Begin
  Result := Assigned(FRpc) And (FRpc.RpcThread = Self) And
    (FGeneration = FRpc.FGeneration) And Not Terminated;
End;

Function TRpcThread.GetAdvInfo: TAdvInfoType;
Begin
  FRpc.Lock;
  Try
    Result := FRpc.AdvInfo;
  Finally
    FRpc.Unlock;
  End;
End;

Function TRpcThread.GetCurTorrentId: Cardinal;
Begin
  FRpc.Lock;
  Try
    Result := FRpc.CurTorrentId;
  Finally
    FRpc.Unlock;
  End;
End;

Function TRpcThread.GetRefreshInterval: TDateTime;
Begin
  FRpc.Lock;
  Try
    Result := FRpc.RefreshInterval;
  Finally
    FRpc.Unlock;
  End;
End;

Function TRpcThread.GetStatus: String;
Begin
  Result := FRpc.Status;
End;

{ TRpc }

Type
  TAsyncRpcRequest = Class(TThread)
  Private
    FRpc: TRpc;
    FReq: TJSONObject;
    FReturnArguments: Boolean;
    FTimeOut: Integer;
    FResult: TJSONObject;
  Protected
    Procedure Execute; Override;
  Public
    Constructor Create(ARpc: TRpc; AReq: TJSONObject;
      AReturnArguments: Boolean; ATimeOut: Integer);
    Destructor Destroy; Override;
  End;

Constructor TAsyncRpcRequest.Create(ARpc: TRpc; AReq: TJSONObject;
  AReturnArguments: Boolean; ATimeOut: Integer);
Begin
  FRpc := ARpc;
  FReq := AReq;
  FReturnArguments := AReturnArguments;
  FTimeOut := ATimeOut;
  Inherited Create(False);
End;

Destructor TAsyncRpcRequest.Destroy;
Begin
  FResult.Free;
  Inherited Destroy;
End;

Procedure TAsyncRpcRequest.Execute;
Begin
  Try
    FResult := FRpc.SendRequestRaw(FReq, FReturnArguments, FTimeOut);
  Except
    FResult := nil;
  End;
End;

Function TRpc.SendRequest(req: TJSONObject; ReturnArguments: Boolean;
  ATimeOut: Integer): TJSONObject;
Var
  T: TAsyncRpcRequest;
Begin
  If GetCurrentThreadId <> FMainThreadId Then
    Exit(SendRequestRaw(req, ReturnArguments, ATimeOut));
  If FMainThreadRequestActive Then
  Begin
    Status := 'Request already in progress';
    Exit(nil);
  End;

  FMainThreadRequestActive := True;
  T := TAsyncRpcRequest.Create(Self, req, ReturnArguments, ATimeOut);
  Try
    While Not T.Finished Do
    Begin
      Application.ProcessMessages;
      Sleep(10);
    End;
    T.WaitFor;
    Result := T.FResult;
    T.FResult := nil;
  Finally
    FMainThreadRequestActive := False;
    T.Free;
  End;
End;

Constructor TRpc.Create;
Begin
  Inherited;
  FMainThreadId := GetCurrentThreadId;
  FLock := TCriticalSection.Create;
  HttpLock := TCriticalSection.Create;
  FForkFeatures := TStringList.Create;
  ForceRequest := 0;
  RefreshNow := [];
  CreateHttp;
End;

Destructor TRpc.Destroy;
Begin
  //If RpcThread <> nil Then
  //  RpcThread.Terminate;
  Disconnect;
  Http.Free;
  FForkFeatures.Free;
  HttpLock.Free;
  FreeAndNil(FLock);//.Free;
  Inherited Destroy;
End;

Procedure TRpc.InitSSL;
{$ifdef unix}
{$ifndef darwin}
Procedure CheckOpenSSL;

Const 
  OpenSSLVersions: array[1..4] Of string = 
                                           ('0.9.8', '1.0.0', '1.0.2', '1.1.0');

Var 
  hLib1, hLib2: TLibHandle;
  i: integer;
Begin
  For i:=Low(OpenSSLVersions) To High(OpenSSLVersions) Do
    Begin
      hlib1 := LoadLibrary(PChar('libssl.so.' + OpenSSLVersions[i]));
      hlib2 := LoadLibrary(PChar('libcrypto.so.' + OpenSSLVersions[i]));
      If hLib2 <> 0 Then
        FreeLibrary(hLib2);
      If hLib1 <> 0 Then
        FreeLibrary(hLib1);
      If (hLib1 <> 0) And (hLib2 <> 0) Then
        Begin
          DLLSSLName := 'libssl.so.' + OpenSSLVersions[i];
          DLLUtilName := 'libcrypto.so.' + OpenSSLVersions[i];
          break;
        End;
    End;
End;
{$endif darwin}
{$endif unix}
Begin
  If IsSSLloaded Then exit;
  {$ifdef unix}
{$ifndef darwin}
  CheckOpenSSL;
{$endif darwin}
  {$endif unix}
  If InitSSLInterface Then
    SSLImplementation := TSSLOpenSSL;
  CreateHttp;
End;

Type
  TGzipDecompressionStream = Class(TDecompressionStream)
  Public
    Constructor Create(Asource: TStream);
  End;

Constructor TGzipDecompressionStream.Create(Asource: TStream);
Var
  gzHeader: Array[1..10] Of Byte;
Begin

{
    paszlib is based on a relatively old zlib version that didn't implement
    reading the gzip header. we "implement" this ourselves by skipping the first
    10 bytes which is just enough for the data Transmission sends.
  }
  Inherited Create(Asource, True);
  Asource.Read(gzHeader, sizeof(gzHeader));
End;

Function DecompressGzipContent(Source: TStream): TMemoryStream;
Var
  buf: Array[1..16384] Of Byte;
  numRead: Integer;
  decomp: TGzipDecompressionStream;
Begin
  decomp := TGzipDecompressionStream.Create(Source);
  Result := TMemoryStream.Create;
  Repeat
  Try
    numRead := decomp.Read(buf, sizeof(buf));
    Result.Write(buf, numRead);
  Except
    Result.Clear;
  End;
  Until numRead < sizeof(buf);
  Result.Position := 0;
  decomp.Free;
End;

Function CreateJsonParser(serverResp: THTTPSend): TJSONParser;
Var
  decompressed: TMemoryStream;
    s : string;
     Resp :TStringList;
Begin
  Try
    If serverResp.Headers.IndexOf('Content-Encoding: gzip') <> -1 Then
    Begin
      { need to fully decompress as the parser relies on a working Seek() }
      decompressed := DecompressGzipContent(serverResp.Document);
      //Resp.LoadFromStream(decompressed);
      //s:=StringReplace(Resp.Strings[0],'"tag"}','"tag":0}', []);
      //Result := TJSONParser.Create(s, [joUTF8]);
      Result := TJSONParser.Create(decompressed, [joUTF8]);
      decompressed.Free;
    End
    Else
    Begin
       //Resp := TStringList.Create;
       //Resp.LoadFromStream(serverResp.Document);
       //s:=Resp.Strings[0];
       ////s:=StringReplace(Resp.Strings[0],'"tag"}','"tag":0}', []);
       //Result := TJSONParser.Create(s, [joUTF8]);
      Result := TJSONParser.Create(serverResp.Document, [joUTF8]);
    End;
  Except
    OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
      '(CreateJsonParser 0)'));
  End;
End;

Function TRpc.SendRequestRaw(req: TJSONObject; ReturnArguments: Boolean;
  ATimeOut: Integer): TJSONObject;
Var
  obj: TJSONData;
  res: TJSONObject;
  jp: TJSONParser;
  s: String;
  i, j, OldTimeOut, RetryCnt: Integer;
  locked, r: Boolean;
Begin
  If FRpcPath = '' Then
    FRpcPath := DefaultRpcPath;
  Status := '';
  Result := nil;
  req.Add('tag',0);
  RetryCnt := 3;
  i := 0;
  Repeat
    Inc(i);
    HttpLock.Enter;
    locked := True;
  Try
    Try
      OldTimeOut := Http.Timeout;
      RequestStartTime := Now;
      Http.Document.Clear;
      s := req.AsJSON;
      Http.Document.Write(PChar(s)^, Length(s));
      //s := '';
      Http.Headers.Clear;
      Http.Headers.Add('Accept-Encoding: gzip');
      Http.Headers.Add(ClientFeaturesHeader);
      Http.MimeType := 'application/json';
      If XTorrentSession <> '' Then
        Http.Headers.Add(XTorrentSession);
      If ATimeOut >= 0 Then
        Http.Timeout := ATimeOut;
      Try
        Try
          r := Http.HTTPMethod('POST', Url + FRpcPath);
        Except
          //OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
          //  '(SendRequest 3)'));
          r := False;
        End;

      Finally
        Http.Timeout := OldTimeOut;
      End;
      If Not r Then
      Begin
        If FMainThreadId <> GetCurrentThreadId Then
          ReconnectAllowed := True;
        Status := Http.Sock.LastErrorDesc;
        //break;
        continue;
      End
      Else
      Begin
        If Http.ResultCode = 409 Then
        Begin
          XTorrentSession := '';
          For j := 0 To Http.Headers.Count - 1 Do
            If Pos('x-transmission-session-id:', AnsiLowerCase(Http.Headers[j])) >
              0 Then
            Begin
              XTorrentSession := Http.Headers[j];
              break;
            End;
          If XTorrentSession <> '' Then
          Begin
            If i = RetryCnt Then
            Begin
              If FMainThreadId <> GetCurrentThreadId Then
                ReconnectAllowed := True;
              Status := 'Session ID error.';
            End;
            continue;
          End;
        End;

        If Http.ResultCode = 301 Then
        Begin
          s := Trim(Http.Headers.Values['Location']);
          If (s <> '') And (i = 1) Then
          Begin
            j := Length(s);
            If Copy(s, j - 4, MaxInt) = '/web/' Then
              SetLength(s, j - 4)
            Else
              If Copy(s, j - 3, MaxInt) = '/web' Then
                SetLength(s, j - 3);
            FRpcPath := s + 'rpc';
            Inc(RetryCnt);
            continue;
          End;
        End;

        If Http.ResultCode <> 200 Then
        Begin
          If Http.Headers.Count > 0 Then
          Begin
            SetString(s, Http.Document.Memory, Http.Document.Size);
            j := Pos('<body>', LowerCase(s));
            If j > 0 Then
              System.Delete(s, 1, j - 1);
            s := StringReplace(s, #13#10, '', [rfReplaceAll]);
            s := StringReplace(s, #13, '', [rfReplaceAll]);
            s := StringReplace(s, #10, '', [rfReplaceAll]);
            s := StringReplace(s, #9, ' ', [rfReplaceAll]);
            s := StringReplace(s, '&quot;', '"', [rfReplaceAll, rfIgnoreCase]);
            s := StringReplace(s, '<br>', LineEnding,
              [rfReplaceAll, rfIgnoreCase]);
            s := StringReplace(s, '</p>', LineEnding,
              [rfReplaceAll, rfIgnoreCase]);
            s := StringReplace(s, '</h1>', LineEnding,
              [rfReplaceAll, rfIgnoreCase]);
            s := StringReplace(s, '<li>', LineEnding + '* ',
              [rfReplaceAll, rfIgnoreCase]);
            j := 1;
            While j <= Length(s) Do
            Begin
              If s[j] = '<' Then
              Begin
                While (j <= Length(s)) And (s[j] <> '>') Do
                  System.Delete(s, j, 1);
                System.Delete(s, j, 1);
              End
              Else
                Inc(j);
            End;
            While Pos('  ', s) > 0 Do
              s := StringReplace(s, '  ', ' ', [rfReplaceAll]);
            While Pos(LineEnding + ' ', s) > 0 Do
              s := StringReplace(s, LineEnding + ' ', LineEnding,
                [rfReplaceAll]);
            s := Trim(s);
          End
          Else
            s := '';
          If s = '' Then
          Begin
            s := Http.ResultString;
            If s = '' Then
              If Http.ResultCode = 0 Then
                s := 'Invalid server response.'
              Else
                s := Format('HTTP error: %d', [Http.ResultCode]);
          End;
          Status := s;
          break;
        End;
        Http.Document.Position := 0;
        Try
          jp := CreateJsonParser(Http);

        Except
          OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
            '(CreateJsonParser)'));
        End;
        HttpLock.Leave;
        locked := False;
        RequestStartTime := 0;
        Try
          Try
            obj := jp.Parse;
            //s := obj.AsJSON;
            Http.Document.Clear;
          Finally
            jp.Free;
          End;
        Except
          on E: Exception Do
          Begin
            OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
              '(SendRequest 2)'));
            Status := e.Message;
            break;
          End;
        End;
        Try
          Try
            If obj Is TJSONObject Then
            Begin
              res := obj As TJSONObject;
              s := res.Strings['result'];
              If AnsiCompareText(s, 'success') <> 0 Then
              Begin
                If Trim(s) = '' Then
                  s := 'Unknown error.';
                Status := s;
              End
              Else
              Begin
                If ReturnArguments Then
                Begin
                  Result := res.Objects['arguments'];
                  If Result = nil Then
                    Status := 'Arguments object not found.'
                  Else
                  Begin
                    //                res.Extract(Result); // lazarus 1.2.6 ok
                    res.Extract(res.IndexOf(Result));
                    // fix Tample :) lazarus 1.4.0 and high!
                    FreeAndNil(obj);
                  End;
                End
                Else
                  Result := res;
                If Result <> nil Then
                  obj := nil;
              End;
              break;
            End
            Else
            Begin
              Status := 'Invalid server response.';
              break;
            End;
          Except
            OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
              '(SendRequest 1)'));
          End;

        Finally
          obj.Free;
        End;
      End;

    Except
      OutputDebugString(LPCSTR(Exception(ExceptObject).Message + '(SendRequest 0)'));
    End;
  Finally
    RequestStartTime := 0;
    If locked Then
    Try
      HttpLock.Leave;

    Finally
    End;
  End;
  Until i >= RetryCnt;
  If Status <> '' Then OutputDebugString(LPCSTR(Status));
End;

Procedure DeleteIfRpcLessThan(Fields: TStringList; Field: String;
  RpcVer: Integer; NeededRpcVer: Integer);
Var
  idx: Integer;
Begin
  idx := Fields.IndexOf(Field);
  If (idx <> -1) And (RpcVer < NeededRpcVer) Then
    Fields.Delete(idx);
End;

Function TRpc.RequestInfo(TorrentId: Integer; Const Fields: Array Of Const;
  Const ExtraFields: Array Of String): TJSONObject;
Var
  req, args: TJSONObject;
  _fields: TJSONArray;
  i: Integer;
  sl: TStringList;
Begin
  Result := nil;
  req := TJSONObject.Create;
  sl := TStringList.Create;
  Try
    Try

      req.Add('method', 'torrent-get');
      args := TJSONObject.Create;
      If TorrentId <> 0 Then
        args.Add('ids', TJSONArray.Create([TorrentId]));
      _fields := TJSONArray.Create;
      For i := Low(Fields) To High(Fields) Do
        If (Fields[i].VType = vtAnsiString) Then
          sl.Add(String(Fields[i].VAnsiString));
      sl.AddStrings(ExtraFields);
      sl.Sort;

      DeleteIfRpcLessThan(sl, 'labels', FRPCVersion, 16);

      For i := sl.Count - 2 Downto 0 Do
        If (sl[i] = sl[i + 1]) Then
          sl.Delete(i + 1);
      For i := 0 To sl.Count - 1 Do
        _fields.Add(sl[i]);
      args.Add('fields', _fields);
      //if FRPCVersion >= 16 then
      //args.Add('format', 'table');

      req.Add('arguments', args);
      //if FRPCVersion >= 16 then
      //Result:=TranslateTableToObjects(SendRequest(req))
      //else
      Try
        Result := SendRequest(req);
      Except
        OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
          '(SendRequest)'));
      End;
    Except
      OutputDebugString(LPCSTR(Exception(ExceptObject).Message + '(RequestInfo)'));
    End;
  Finally
    sl.Free;
    req.Free;
  End;
End;

Function TRpc.RequestInfos(TorrentIds: Variant;
  Const Fields: Array Of Const): TJSONObject;
Begin
  Result := RequestInfos(TorrentIds, Fields, []);
End;

Function TRpc.RequestInfos(TorrentIds: Variant; Const Fields: Array Of Const;
  Const ExtraFields: Array Of String): TJSONObject;
Var
  req, args: TJSONObject;
  _fields, ids: TJSONArray;
  i: Integer;
  sl: TStringList;
Begin
  Result := nil;
  req := TJSONObject.Create;
  sl := TStringList.Create;
  Try
    req.Add('method', 'torrent-get');
    args := TJSONObject.Create;
    ids := TJSONArray.Create;
    For i := VarArrayLowBound(TorrentIds, 1) To VarArrayHighBound(TorrentIds, 1) Do
      ids.Add(Integer(TorrentIds[i]));
    args.Add('ids', ids);
    _fields := TJSONArray.Create;
    For i := Low(Fields) To High(Fields) Do
      If (Fields[i].VType = vtAnsiString) Then
        sl.Add(String(Fields[i].VAnsiString));
    sl.AddStrings(ExtraFields);
    sl.Sort;
    For i := sl.Count - 2 Downto 0 Do
      If (sl[i] = sl[i + 1]) Then
        sl.Delete(i + 1);
    For i := 0 To sl.Count - 1 Do
      _fields.Add(sl[i]);
    args.Add('fields', _fields);
    If FRPCVersion >= 16 Then
      args.Add('format', 'table');

    req.Add('arguments', args);
    If FRPCVersion >= 16 Then
      Result := TranslateTableToObjects(SendRequest(req))
    Else
      Result := SendRequest(req);
  Finally
    sl.Free;
    req.Free;
  End;
End;

Function TRpc.RequestInfo(TorrentId: Integer; Const Fields: Array Of Const): TJSONObject;
Begin
  Result := RequestInfo(TorrentId, Fields, []);
End;


Function TRpc.GetStatus: String;
Begin
  Lock;
  Try
    Result := FStatus;
    UniqueString(Result);
  Finally
    Unlock;
  End;
End;

Function TRpc.GetTorrentFields: String;
Begin
  Lock;
  Try
    Result := FTorrentFields;
    UniqueString(Result);
  Finally
    Unlock;
  End;
End;

Procedure TRpc.SetInfoStatus(Const AValue: String);
Begin
  Lock;
  Try
    FInfoStatus := AValue;
    UniqueString(FStatus);
  Finally
    Unlock;
  End;
End;

Function TRpc.GetConnected: Boolean;
Begin
  Result := Assigned(RpcThread) And FConnected;
End;

Function TRpc.GetConnecting: Boolean;
Begin
  Result := Not FConnected And Assigned(RpcThread);
End;

Function TRpc.GetInfoStatus: String;
Begin
  Lock;
  Try
    Result := FInfoStatus;
    UniqueString(Result);
  Finally
    Unlock;
  End;
End;

Procedure TRpc.SetStatus(Const AValue: String);
Begin
  Lock;
  Try
    FStatus := AValue;
    UniqueString(FStatus);
  Finally
    Unlock;
  End;
End;

Procedure TRpc.SetTorrentFields(Const AValue: String);
Begin
  Lock;
  Try
    FTorrentFields := AValue;
    UniqueString(FTorrentFields);
  Finally
    Unlock;
  End;
End;

Procedure TRpc.CreateHttp;
Var
  i: Integer;
Begin
  Http.Free;
  Http := THTTPSend.Create;
  Http.Protocol := '1.1';

  i := Ini.ReadInteger('NetWork', 'HttpTimeout', 30);
  If (i < 2) Or (i > 999) Then i := 30;
  // default
  Ini.WriteInteger('NetWork', 'HttpTimeout', i);
  Http.Timeout := i * 1000;

  i := Ini.ReadInteger('NetWork', 'ConnectTimeout', 0);
  If (i < 0) Or (i > 999) Then i := 0;
  // default
  Ini.WriteInteger('NetWork', 'ConnectTimeout', i);
  Http.FSock.ConnectionTimeout := i * 1000;

  Http.Headers.NameValueSeparator := ':';
End;

Procedure TRpc.Lock;
Begin
  Try
    FLock.Enter;

  Finally
  End;
End;

Procedure TRpc.Unlock;
Begin
  FLock.Leave;
End;

Procedure TRpc.ResetForkFeatures;
Begin
  Lock;
  Try
    FForkFeatures.Clear;
    FForkFeaturesLoaded := False;
  Finally
    Unlock;
  End;
End;

Procedure TRpc.DetectForkFeatures;

  Procedure AddFeature(Const AName: String);
  Begin
    If FForkFeatures.IndexOf(AName) < 0 Then
      FForkFeatures.Add(AName);
  End;

Var
  req, args, params: TJSONObject;
  fields, feats: TJSONArray;
  d: TJSONData;
  i: Integer;
  OldStatus: String;
  HasMarker, HasMinFree, HasMargin: Boolean;
  ResponseOk: Boolean;
Begin
  Lock;
  Try
    If FForkFeaturesLoaded Then
      exit;
  Finally
    Unlock;
  End;

  ResponseOk := False;
  OldStatus := Status;
  Try
    // Ask for the fork feature marker.
    req := TJSONObject.Create;
    Try
      req.Add('method', 'session-get');
      params := TJSONObject.Create;
      fields := TJSONArray.Create;
      fields.Add('fork-features');
      params.Add('fields', fields);
      req.Add('arguments', params);
      args := SendRequest(req);
      If args <> nil Then
      Try
        feats := nil;
        d := args.Find('fork-features');
        If d = nil Then
          d := args.Find('fork_features');
        If (d <> nil) And (d Is TJSONArray) Then
          feats := TJSONArray(d);
        Lock;
        Try
          If feats <> nil Then
            For i := 0 To feats.Count - 1 Do
              If feats.Items[i].JSONType = jtString Then
                AddFeature(AnsiLowerCase(feats.Strings[i]));
        Finally
          Unlock;
        End;
      Finally
        args.Free;
      End;
    Finally
      req.Free;
    End;

    Lock;
    Try
      HasMarker := FForkFeatures.Count > 0;
    Finally
      Unlock;
    End;
    If HasMarker Then
      ResponseOk := True;

    // Old fork builds may not expose the marker. Probe the settings directly.
    If Not HasMarker Then
    Begin
      HasMinFree := False;
      HasMargin := False;
      req := TJSONObject.Create;
      Try
        req.Add('method', 'session-get');
        params := TJSONObject.Create;
        fields := TJSONArray.Create;
        fields.Add('min-free-space');
        fields.Add('min-free-space-resume-margin');
        params.Add('fields', fields);
        req.Add('arguments', params);
        args := SendRequest(req);
        If args <> nil Then
        Try
          ResponseOk := True;
          d := args.Find('min-free-space');
          If d = nil Then
            d := args.Find('min_free_space');
          HasMinFree := d <> nil;
          d := args.Find('min-free-space-resume-margin');
          If d = nil Then
            d := args.Find('min_free_space_resume_margin');
          HasMargin := d <> nil;
          Lock;
          Try
            If HasMinFree Then
              AddFeature('min_free_space/v1');
            If HasMargin Then
              AddFeature('min_free_space_resume_margin/v1');
          Finally
            Unlock;
          End;
        Finally
          args.Free;
        End;
      Finally
        req.Free;
      End;
    End;
  Finally
    If ResponseOk Then
    Begin
      Lock;
      Try
        FForkFeaturesLoaded := True;
      Finally
        Unlock;
      End;
    End;
    Status := OldStatus;
  End;
End;

Function TRpc.HasForkFeature(Const AName: String; AVersion: Integer): Boolean;
Var
  s: String;
Begin
  DetectForkFeatures;
  s := AnsiLowerCase(AName) + '/v' + IntToStr(AVersion);
  Lock;
  Try
    Result := FForkFeatures.IndexOf(s) >= 0;
  Finally
    Unlock;
  End;
End;

Procedure TRpc.Connect;
Begin
  CurTorrentId := 0;
  XTorrentSession := '';
  FConnected := False;
  ResetForkFeatures;
  RequestFullInfo := True;
  ReconnectAllowed := False;
  RefreshNow := [];
  Inc(FGeneration);
  RpcThread := TRpcThread.Create;
  With RpcThread Do
  Begin
    FreeOnTerminate := True;
    FRpc := Self;
    FGeneration := FRpc.FGeneration;
    Suspended := False;
  End;
End;

Procedure TRpc.Disconnect;
Var
  retry, i: Integer;
Begin
  Inc(FGeneration);
  If Assigned(RpcThread) Then
  Begin
    RpcThread.Terminate;
    retry := 20;
    i := 0;
    While Assigned(RpcThread) And (i < retry) Do
    Begin
      Application.ProcessMessages;
      Inc(i);
      Try
        //        RpcThread.WaitFor;
        Http.Sock.CloseSocket;
      Except
      End;
      Sleep(20);
    End;
  End;
  Status := '';
  RequestStartTime := 0;
  FRpcPath := '';
  ResetForkFeatures;
End;

End.
