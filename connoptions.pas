

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

Unit ConnOptions;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ComCtrls, Buttons, ButtonPanel, ExtCtrls, BaseForm,
  ResTranslator;

Const
  DefSpeeds = '0,10,25,50,100,250,500,750,1000,2500,5000,7000';

Resourcestring
  sNoHost = 'No host name specified.';
  sNoProxy = 'No proxy server specified.';
  SDelConnection = 'Are you sure to delete connection ''%s''?';
  SNewConnection = 'New connection to Transmission';

Type

  { TConnOptionsForm }

  TConnOptionsForm = Class(TBaseForm)
    btNew: TButton;
    btDel: TButton;
    btRename: TButton;
    Buttons: TButtonPanel;
    cbProxyAuth: TCheckBox;
    cbUseProxy: TCheckBox;
    cbUseSocks5: TCheckBox;
    cbAuth: TCheckBox;
    cbShowAdvanced: TCheckBox;
    cbAskPassword: TCheckBox;
    edMaxFolder: TSpinEdit;
    edIniFileName: TEdit;
    edLanguage: TEdit;
    edTranslateForm: TCheckBox;
    edTranslateMsg: TCheckBox;
    edRpcPath: TEdit;
    edUpSpeeds: TEdit;
    edHost: TEdit;
    cbSSL: TCheckBox;
    cbAutoReconnect: TCheckBox;
    cbConnection: TComboBox;
    edDownSpeeds: TEdit;
    edProxy: TEdit;
    edProxyPassword: TEdit;
    edProxyPort: TSpinEdit;
    edProxyUserName: TEdit;
    edUserName: TEdit;
    edPassword: TEdit;
    edPaths: TMemo;
    gbSpeed: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    txRpcPath: TLabel;
    txConName: TLabel;
    txConnHelp: TLabel;
    txDownSpeeds: TLabel;
    panTop: TPanel;
    tabProxy: TTabSheet;
    tabMisc: TTabSheet;
    txUpSpeeds: TLabel;
    txPaths: TLabel;
    tabPaths: TTabSheet;
    Page: TPageControl;
    tabConnection: TTabSheet;
    txProxy: TLabel;
    txProxyPassword: TLabel;
    txProxyPort: TLabel;
    txProxyUserName: TLabel;
    txUserName: TLabel;
    txPort: TLabel;
    edPort: TSpinEdit;
    txHost: TLabel;
    txPassword: TLabel;
    txCertFile: TLabel;
    edCertFile: TEdit;
    txCertPass: TLabel;
    edCertPass: TEdit;
    Procedure btDelClick(Sender: TObject);
    Procedure btNewClick(Sender: TObject);
    Procedure btOKClick(Sender: TObject);
    Procedure btRenameClick(Sender: TObject);
    Procedure cbAskPasswordClick(Sender: TObject);
    Procedure cbAuthClick(Sender: TObject);
    Procedure cbConnectionSelect(Sender: TObject);
    Procedure cbProxyAuthClick(Sender: TObject);
    Procedure cbShowAdvancedClick(Sender: TObject);
    Procedure cbUseProxyClick(Sender: TObject);
    Procedure edHostChange(Sender: TObject);
    Procedure edIniFileOpen(Sender: TObject);
    Procedure edLanguageDoubleClick(Sender: TObject);
    Procedure edTranslateFormChange(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure tabPathsShow(Sender: TObject);
    Procedure cbSSLClick(Sender: TObject);
  Private
    FCurConn: String;
    FCurHost: String;
    edConnection: TEdit;

    Function Validate: Boolean;
    Procedure BeginEdit;
    Procedure EndEdit;
    Procedure SaveConnectionsList;
  Public
    ActiveConnection: String;
    ActiveSettingChanged: Boolean;

    Procedure LoadConnSettings(Const ConnName: String);
    Procedure SaveConnSettings(Const ConnName: String);
    Function IsConnSettingsChanged(Const ConnName: String): Boolean;
  End;

Implementation

Uses Main, synacode, utils, rpc, LCLIntf;

  { TConnOptionsForm }

Procedure TConnOptionsForm.btOKClick(Sender: TObject);
Begin
  If Not Validate Then
    exit;
  EndEdit;
  SaveConnSettings(FCurConn);
  SaveConnectionsList;
  ModalResult := mrOk;
End;

Procedure TConnOptionsForm.btRenameClick(Sender: TObject);
Begin
  If edConnection.Visible Then
  Begin
    If Trim(edConnection.Text) = '' Then exit;
    EndEdit;
    exit;
  End;
  If cbConnection.Text = '' Then exit;
  BeginEdit;
  ActiveControl := edConnection;
  edConnection.SelectAll;
End;

Procedure TConnOptionsForm.cbAskPasswordClick(Sender: TObject);
Begin
  EnableControls(Not cbAskPassword.Checked And cbAskPassword.Enabled,
    [txPassword, edPassword]);
End;

Procedure TConnOptionsForm.cbAuthClick(Sender: TObject);
Begin
  EnableControls(cbAuth.Checked, [txUserName, edUserName, txPassword, cbAskPassword]);
  cbAskPasswordClick(nil);
End;

Procedure TConnOptionsForm.cbSSLClick(Sender: TObject);
Begin
  {$ifndef windows}
  EnableControls(cbSSL.Checked, [txCertFile, edCertFile, txCertPass, edCertPass]
    );
  {$else}
  EnableControls(False, [txCertFile, edCertFile, txCertPass, edCertPass]);
{$endif windows}
End;

Procedure TConnOptionsForm.cbConnectionSelect(Sender: TObject);
Var
  i: Integer;
  s: String;
Begin
  If edConnection.Visible Then
    exit;
  i := cbConnection.ItemIndex;
  If i >= 0 Then
    s := cbConnection.Items[i]
  Else
    s := '';

  If (FCurConn <> s) And (FCurConn <> '') Then
  Begin
    If Not Validate Then
    Begin
      cbConnection.ItemIndex := cbConnection.Items.IndexOf(FCurConn);
      exit;
    End;
    SaveConnSettings(FCurConn);
  End;
  If s <> '' Then
    LoadConnSettings(s);
End;

Procedure TConnOptionsForm.cbProxyAuthClick(Sender: TObject);
Begin
  EnableControls(cbProxyAuth.Checked And cbProxyAuth.Enabled,
    [txProxyUserName, edProxyUserName, txProxyPassword, edProxyPassword]);
End;

Procedure TConnOptionsForm.cbShowAdvancedClick(Sender: TObject);
Begin
  txRpcPath.Visible := cbShowAdvanced.Checked;
  edRpcPath.Visible := cbShowAdvanced.Checked;
  txCertFile.Visible := cbShowAdvanced.Checked;
  edCertFile.Visible := cbShowAdvanced.Checked;
  txCertPass.Visible := cbShowAdvanced.Checked;
  edCertPass.Visible := cbShowAdvanced.Checked;
  {$ifndef LCLCocoa}
  {$ifndef LCLgtk2}
  tabConnection.TabVisible := cbShowAdvanced.Checked;
  {$endif LCLgtk2}
  tabProxy.TabVisible := cbShowAdvanced.Checked;
  tabPaths.TabVisible := cbShowAdvanced.Checked;
  tabMisc.TabVisible := cbShowAdvanced.Checked;
  {$endif LCLCocoa}
  {$ifdef LCLCocoa}
  Page.ShowTabs := cbShowAdvanced.Checked;
{$endif LCLCocoa}
  cbShowAdvanced.Visible := Not cbShowAdvanced.Checked;
  Page.ActivePage := tabConnection;
End;

Procedure TConnOptionsForm.btNewClick(Sender: TObject);
Begin
  EndEdit;
  If (FCurConn <> '') And Not Validate Then
    exit;
  SaveConnSettings(FCurConn);
  LoadConnSettings('');
  BeginEdit;
  edConnection.Text := '';
  Page.ActivePage := tabConnection;
  ActiveControl := edHost;
End;

Procedure TConnOptionsForm.btDelClick(Sender: TObject);
Var
  i: Integer;
Begin
  If edConnection.Visible Or (cbConnection.Text = '') Then
    exit;
  If MessageDlg('', Format(SDelConnection, [cbConnection.Text]),
    mtConfirmation, mbYesNo, 0, mbNo) <> mrYes Then exit;
  If FCurConn <> '' Then
  Begin
    Ini.EraseSection('Connection.' + FCurConn);
    Ini.EraseSection('Connection');
    Ini.EraseSection('AddTorrent.' + FCurConn);

    i := cbConnection.ItemIndex;
    If i >= 0 Then
    Begin
      cbConnection.Items.Delete(i);
      If i >= cbConnection.Items.Count Then
      Begin
        i := cbConnection.Items.Count - 1;
        If i < 0 Then
          i := 0;
      End;
    End
    Else
      i := 0;
    If i < cbConnection.Items.Count Then
      cbConnection.ItemIndex := i
    Else
      cbConnection.ItemIndex := -1;
  End
  Else
    cbConnection.ItemIndex := -1;
  If cbConnection.ItemIndex >= 0 Then
  Begin
    If FCurConn = ActiveConnection Then
      ActiveConnection := '';
    LoadConnSettings(cbConnection.Items[cbConnection.ItemIndex]);
    If ActiveConnection = '' Then
      ActiveConnection := FCurConn;
  End
  Else
  Begin
    FCurConn := '';
    btNewClick(nil);
  End;
  SaveConnectionsList;
End;

Procedure TConnOptionsForm.cbUseProxyClick(Sender: TObject);
Begin
  EnableControls(cbUseProxy.Checked, [txProxy, edProxy, txProxyPort,
    edProxyPort, cbUseSocks5, cbProxyAuth]);
  cbProxyAuthClick(nil);
End;

Procedure TConnOptionsForm.edHostChange(Sender: TObject);
Begin
  If edConnection.Visible And (edConnection.Text = FCurHost) Then
    edConnection.Text := edHost.Text;
  FCurHost := edHost.Text;
End;

Procedure TConnOptionsForm.edIniFileOpen(Sender: TObject);
Begin
  If edIniFileName.Text <> '' Then
  Begin
    AppBusy;
    OpenURL(Main.FHomeDir + ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),
      '.ini'));
    AppNormal;
    exit;
  End;

  ForceAppNormal;
  MessageDlg(sNoPathMapping, mtInformation, [mbOK], 0);
End;

Procedure TConnOptionsForm.edLanguageDoubleClick(Sender: TObject);
Var
  Sl: TStringList;
Begin
  Sl := GetAvailableTranslations(DefaultLangDir);
  GetTranslationFileName(Main.FTranslationLanguage, Sl);
  ShowMessage(
    sLanguagePathFile + ': ' + IniFileName + sLineBreak + sLineBreak +
    sLanguagePath + ': ' + DefaultLangDir + sLineBreak + sLineBreak +
    sLanguageList + ':' + sLineBreak + Sl.Text
    );
  edLanguage.Text := IniFileName;
  If IniFileName <> '' Then
  Begin
    AppBusy;
    OpenURL(DefaultLangDir);
    AppNormal;
    exit;
  End;
  ForceAppNormal;
  MessageDlg(sNoPathMapping, mtInformation, [mbOK], 0);
End;

Procedure TConnOptionsForm.edTranslateFormChange(Sender: TObject);
Begin
  If edTranslateForm.Checked Then
    edTranslateMsg.Enabled := True
  Else
  Begin
    edTranslateMsg.Enabled := False;
    edLanguage.Text := '';
  End;

End;

Procedure TConnOptionsForm.FormCreate(Sender: TObject);
Var
  i, cnt: Integer;
  s: String;
Begin
  bidiMode := GetBiDi();
  Page.ActivePageIndex := 0;
  txConnHelp.Caption := Format(txConnHelp.Caption, [AppName]);
  ActiveControl := edHost;
  Buttons.OKButton.ModalResult := mrNone;
  Buttons.OKButton.OnClick := @btOKClick;

  edConnection := TEdit.Create(cbConnection.Parent);
  edConnection.Visible := False;
  edConnection.BoundsRect := cbConnection.BoundsRect;
  edConnection.Parent := cbConnection.Parent;

  cnt := Ini.ReadInteger('Hosts', 'Count', 0);
  For i := 1 To cnt Do
  Begin
    s := Ini.ReadString('Hosts', Format('Host%d', [i]), '');
    If s <> '' Then
      cbConnection.Items.Add(s);
  End;

  cbShowAdvanced.Top := edRpcPath.Top;

  If edTranslateForm.Checked Then
    edTranslateMsg.Enabled := True
  Else
    edTranslateMsg.Enabled := False;
  Main.LoadTranslation;
  edLanguage.Text := Main.FTranslationLanguage;
  edIniFileName.Text := Main.FHomeDir + ChangeFileExt(
    ExtractFileName(ParamStrUTF8(0)), '.ini');
End;

Procedure TConnOptionsForm.FormShow(Sender: TObject);
Begin
  If edConnection.Visible Then
    exit;
  If cbConnection.Items.Count = 0 Then
  Begin
    btNewClick(nil);
    exit;
  End;
  cbConnection.ItemIndex := cbConnection.Items.IndexOf(ActiveConnection);
  If cbConnection.ItemIndex < 0 Then
    cbConnection.ItemIndex := 0;
  LoadConnSettings(cbConnection.Text);
End;

Procedure TConnOptionsForm.tabPathsShow(Sender: TObject);
Var
  R: TRect;
Begin
  R := edPaths.BoundsRect;
  R.Top := txPaths.BoundsRect.Bottom + 8;
  edPaths.BoundsRect := R;
End;

Function TConnOptionsForm.Validate: Boolean;
Begin
  Result := False;
  edHost.Text := Trim(edHost.Text);
  If Trim(edHost.Text) = '' Then
  Begin
    Page.ActivePage := tabConnection;
    edHost.SetFocus;
    MessageDlg(sNoHost, mtError, [mbOK], 0);
    exit;
  End;
  edProxy.Text := Trim(edProxy.Text);
  If tabProxy.TabVisible And cbUseProxy.Checked And (edProxy.Text = '') Then
  Begin
    Page.ActivePage := tabProxy;
    edProxy.SetFocus;
    MessageDlg(sNoProxy, mtError, [mbOK], 0);
    exit;
  End;
  Result := True;
End;

Procedure TConnOptionsForm.EndEdit;

  Procedure RenameSection(Const OldName, NewName: String);
  Var
    i: Integer;
    sl: TStringList;
  Begin
    sl := TStringList.Create;
    With Ini Do
    Try
      ReadSectionValues(OldName, sl);
      For i := 0 To sl.Count - 1 Do
        WriteString(NewName, sl.Names[i], sl.ValueFromIndex[i]);
      EraseSection(OldName);
    Finally
      sl.Free;
    End;
  End;

Var
  NewName, s: String;
  i, p: Integer;
Begin
  If Not edConnection.Visible Then exit;
  NewName := Trim(edConnection.Text);
  If NewName = '' Then
    NewName := Trim(edHost.Text);
  If NewName <> FCurConn Then
  Begin
    If FCurConn <> '' Then
    Begin
      p := cbConnection.Items.IndexOf(FCurConn);
      If p >= 0 Then
        cbConnection.Items.Delete(p);
    End
    Else
      p := -1;

    i := 1;
    s := NewName;
    While cbConnection.Items.IndexOf(NewName) >= 0 Do
    Begin
      Inc(i);
      NewName := Format('%s (%d)', [s, i]);
    End;

    If FCurConn <> '' Then
    Begin
      RenameSection('Connection.' + FCurConn, 'Connection.' + NewName);
      RenameSection('AddTorrent.' + FCurConn, 'AddTorrent.' + NewName);
    End;

    If p >= 0 Then
      cbConnection.Items.Insert(p, NewName)
    Else
      cbConnection.Items.Add(NewName);
    If (FCurConn = ActiveConnection) Or (FCurConn = '') Then
      ActiveConnection := NewName;
    FCurConn := NewName;
    SaveConnectionsList;
  End;
  cbConnection.ItemIndex := cbConnection.Items.IndexOf(NewName);
  cbConnection.Visible := True;
  edConnection.Visible := False;
End;

Procedure TConnOptionsForm.SaveConnectionsList;
Var
  i: Integer;
Begin
  With Ini Do
  Begin
    WriteString('Hosts', 'CurHost', ActiveConnection);
    WriteInteger('Hosts', 'Count', cbConnection.Items.Count);
    For i := 0 To cbConnection.Items.Count - 1 Do
      WriteString('Hosts', Format('Host%d', [i + 1]), cbConnection.Items[i]);
    UpdateFile;
  End;
End;

Procedure TConnOptionsForm.BeginEdit;
Var
  i: Integer;
Begin
  i := cbConnection.ItemIndex;
  If i >= 0 Then
    edConnection.Text := cbConnection.Items[i]
  Else
    edConnection.Text := '';
  edConnection.Visible := True;
  cbConnection.Visible := False;
End;

Procedure TConnOptionsForm.LoadConnSettings(Const ConnName: String);
Var
  Sec, s: String;
Begin
  With Ini Do
  Begin
    Sec := 'Connection.' + ConnName;
    If (ConnName <> '') And Not SectionExists(Sec) Then
      Sec := 'Connection';
    edHost.Text := ReadString(Sec, 'Host', '');
    FCurHost := edHost.Text;
    edPort.Value := ReadInteger(Sec, 'Port', 9091);
    cbSSL.Checked := ReadBool(Sec, 'UseSSL', False);
    edCertFile.Text := ReadString(Sec, 'CertFile', '');
    If cbSSL.Checked Then
      If ReadString(Sec, 'CertPass', '') <> '' Then
        edCertPass.Text := '******'
      Else
        edCertPass.Text := '';
    cbAutoReconnect.Checked := ReadBool(Sec, 'Autoreconnect', False);
    edUserName.Text := ReadString(Sec, 'UserName', '');
    s := ReadString(Sec, 'Password', '');
    cbAuth.Checked := (edUserName.Text <> '') Or (s <> '');
    If cbAuth.Checked Then
    Begin
      cbAskPassword.Checked := s = '-';
      If Not cbAskPassword.Checked Then
        If s <> '' Then
          edPassword.Text := '******'
        Else
          edPassword.Text := '';
    End;
    cbAuthClick(nil);
    cbSSLClick(nil);
    edRpcPath.Text := ReadString(Sec, 'RpcPath', DefaultRpcPath);
    cbUseProxy.Checked := ReadBool(Sec, 'UseProxy', False);
    cbUseSocks5.Checked := ReadBool(Sec, 'UseSockProxy', False);
    edProxy.Text := ReadString(Sec, 'ProxyHost', '');
    edProxyPort.Value := ReadInteger(Sec, 'ProxyPort', 8080);
    edProxyUserName.Text := ReadString(Sec, 'ProxyUser', '');
    cbProxyAuth.Checked := edProxyUserName.Text <> '';
    If cbProxyAuth.Checked Then
      If ReadString(Sec, 'ProxyPass', '') <> '' Then
        edProxyPassword.Text := '******'
      Else
        edProxyPassword.Text := '';
    edPaths.Text := StringReplace(ReadString(Sec, 'PathMap', ''),
      '|', LineEnding, [rfReplaceAll]);
    edDownSpeeds.Text := ReadString(Sec, 'DownSpeeds', DefSpeeds);
    edUpSpeeds.Text := ReadString(Sec, 'UpSpeeds', DefSpeeds);
    edTranslateMsg.Checked := ReadBool('Translation', 'TranslateMsg', True);
    edTranslateForm.Checked := ReadBool('Translation', 'TranslateForm', True);
    cbUseProxyClick(nil);
  End;

  edMaxFolder.Value := Ini.ReadInteger('Interface', 'MaxFoldersHistory', 50);
  // PETROV

  FCurConn := ConnName;
  FCurHost := edHost.Text;
End;

Procedure TConnOptionsForm.SaveConnSettings(Const ConnName: String);
Var
  Sec: String;
  i: Integer;
  s, ss: String;
Begin
  If ConnName = '' Then
    exit;
  If ConnName = ActiveConnection Then
    If IsConnSettingsChanged(ConnName) Then
      ActiveSettingChanged := True;

  Ini.WriteInteger('Interface', 'MaxFoldersHistory', edMaxFolder.Value);
  With Ini Do
  Begin
    Sec := 'Connection.' + ConnName;
    WriteString(Sec, 'Host', Trim(edHost.Text));
    WriteBool(Sec, 'UseSSL', cbSSL.Checked);
    If Not cbSSL.Checked Then
    Begin
      edCertFile.Text := '';
      edCertPass.Text := '';
    End;
    WriteString(Sec, 'CertFile', edCertFile.Text);
    If edCertPass.Text <> '******' Then
    Begin
      If edCertPass.Text = '' Then
        s := ''
      Else
        s := EncodeBase64(edCertPass.Text);
      WriteString(Sec, 'CertPass', s);
    End;
    WriteBool(Sec, 'Autoreconnect', cbAutoReconnect.Checked);
    WriteInteger(Sec, 'Port', edPort.Value);
    If Not cbAuth.Checked Then
    Begin
      edUserName.Text := '';
      edPassword.Text := '';
      cbAskPassword.Checked := False;
    End;
    WriteString(Sec, 'UserName', edUserName.Text);
    If cbAskPassword.Checked Then
      WriteString(Sec, 'Password', '-')
    Else
      If edPassword.Text <> '******' Then
      Begin
        ss := edPassword.Text;
        If (Pos('{', ss) > 0) Or (Pos('}', ss) > 0) Then
        Begin
          MessageDlg('The password can''t contain the characters: { }',
            mtError, [mbOK], 0);
        End;

        If edPassword.Text = '' Then
          s := ''
        Else
          s := EncodeBase64(edPassword.Text);
        WriteString(Sec, 'Password', s);
      End;

    If (edRpcPath.Text = DefaultRpcPath) Or (edRpcPath.Text = '') Then
      DeleteKey(Sec, 'RpcPath')
    Else
      WriteString(Sec, 'RpcPath', edRpcPath.Text);

    WriteBool('Translation', 'TranslateMsg', edTranslateMsg.Checked);
    WriteBool('Translation', 'TranslateForm', edTranslateForm.Checked);
    WriteBool(Sec, 'UseProxy', cbUseProxy.Checked);
    WriteBool(Sec, 'UseSockProxy', cbUseSocks5.Checked);
    WriteString(Sec, 'ProxyHost', Trim(edProxy.Text));
    WriteInteger(Sec, 'ProxyPort', edProxyPort.Value);
    If Not cbProxyAuth.Checked Then
    Begin
      edProxyUserName.Text := '';
      edProxyPassword.Text := '';
    End;
    WriteString(Sec, 'ProxyUser', edProxyUserName.Text);
    If edProxyPassword.Text <> '******' Then
    Begin
      If edProxyPassword.Text = '' Then
        s := ''
      Else
        s := EncodeBase64(edProxyPassword.Text);
      WriteString(Sec, 'ProxyPass', s);
    End;
    WriteString(Sec, 'PathMap', StringReplace(edPaths.Text, LineEnding,
      '|', [rfReplaceAll]));
    WriteString(Sec, 'DownSpeeds', Trim(edDownSpeeds.Text));
    WriteString(Sec, 'UpSpeeds', Trim(edUpSpeeds.Text));

    i := cbConnection.Items.IndexOf(ConnName);
    If i < 0 Then
      cbConnection.Items.Insert(0, ConnName);
    UpdateFile;
  End;
End;

Function TConnOptionsForm.IsConnSettingsChanged(Const ConnName: String): Boolean;
Var
  Sec: String;
Begin
  With Ini Do
  Begin
    Sec := 'Connection.' + ConnName;
    If Not SectionExists(Sec) Then
      Sec := 'Connection';
    Result := (edPort.Value <> ReadInteger(Sec, 'Port', 9091)) Or
      (edHost.Text <> ReadString(Sec, 'Host', '')) Or
      (cbSSL.Checked <> ReadBool(Sec, 'UseSSL', False)) Or
      (edCertFile.Text <> ReadString(Sec, 'CertFile', '')) Or
      ((ReadString(Sec, 'CertPass', '') = '') And (edCertPass.Text <> '')) Or
      ((ReadString(Sec, 'CertPass', '') <> '') And (edCertPass.Text <> '******')) Or
      (cbAutoReconnect.Checked <> ReadBool(Sec, 'Autoreconnect', False)) Or
      (edUserName.Text <> ReadString(Sec, 'UserName', '')) Or
      ((ReadString(Sec, 'Password', '') = '') And (edPassword.Text <> '')) Or
      ((ReadString(Sec, 'Password', '') <> '') And (edPassword.Text <> '******')) Or
      (edRpcPath.Text <> ReadString(Sec, 'RpcPath', DefaultRpcPath)) Or
      (cbUseProxy.Checked <> ReadBool(Sec, 'UseProxy', False)) Or
      (edTranslateMsg.Checked <> ReadBool('Translation', 'TranslateMsg', True)) Or
      (edTranslateForm.Checked <> ReadBool('Translation', 'TranslateForm', True)) Or
      (edProxy.Text <> ReadString(Sec, 'ProxyHost', '')) Or
      (edProxyPort.Value <> ReadInteger(Sec, 'ProxyPort', 8080)) Or
      (edProxyUserName.Text <> ReadString(Sec, 'ProxyUser', '')) Or
      ((ReadString(Sec, 'ProxyPass', '') = '') And (edProxyPassword.Text <> '')) Or
      ((ReadString(Sec, 'ProxyPass', '') <> '') And
      (edProxyPassword.Text <> '******')) Or
      (edPaths.Text <> StringReplace(ReadString(Sec, 'PathMap', ''),
      '|', LineEnding, [rfReplaceAll])) Or (edDownSpeeds.Text <>
      ReadString(Sec, 'DownSpeeds', '')) Or (edUpSpeeds.Text <>
      ReadString(Sec, 'UpSpeeds', ''));
  End;
End;

Initialization
  {$I connoptions.lrs}

End.
