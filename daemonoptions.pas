

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

Unit DaemonOptions;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LazUTF8, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls, CheckLst, EditBtn, MaskEdit,
  ButtonPanel, BaseForm, Grids, fpjson, ScriptRunnerUI;

Resourcestring
  sPortTestSuccess = 'Incoming port tested successfully.';
  sPortTestFailed = 'Incoming port is closed. Check your firewall settings.';
  sEncryptionDisabled = 'Encryption disabled';
  sEncryptionEnabled = 'Encryption enabled';
  sEncryptionRequired = 'Encryption required';
  SNoDownloadDir = 'The downloads directory was not specified.';
  SNoIncompleteDir = 'The directory for incomplete files was not specified.';
  // SNoBlocklistURL = 'The blocklist URL was not specified.';
  SInvalidTime = 'The invalid time value was entered.';
  SInvalidFreeSpaceValue = 'Invalid free space value "%s".';
  SInvalidFreeSpaceUnit = 'Invalid free space unit "%s".';

Type

  { TDaemonOptionsForm }

  TDaemonOptionsForm = Class(TBaseForm)
    btTestPort: TButton;
    Buttons: TButtonPanel;
    cbBlocklist: TCheckBox;
    cbDHT: TCheckBox;
    cbUpQueue: TCheckBox;
    cbEncryption: TComboBox;
    cbMaxDown: TCheckBox;
    cbSequentialDownload: TCheckBox;
    cbMaxUp: TCheckBox;
    cbPEX: TCheckBox;
    cbPortForwarding: TCheckBox;
    cbRandomPort: TCheckBox;
    cbIncompleteDir: TCheckBox;
    cbPartExt: TCheckBox;
    cbSeedRatio: TCheckBox;
    cbLPD: TCheckBox;
    cbIdleSeedLimit: TCheckBox;
    cbAltEnabled: TCheckBox;
    cbAutoAlt: TCheckBox;
    cbStalled: TCheckBox;
    cbUTP: TCheckBox;
    cbDownQueue: TCheckBox;
    edAltTimeEnd: TMaskEdit;
    edDownQueue: TSpinEdit;
    edUpQueue: TSpinEdit;
    edStalledTime: TSpinEdit;
    tabQueue: TTabSheet;
    tabFreeSpace: TTabSheet;
    txMinFreeSpace: TLabel;
    gFreeSpace: TStringGrid;
    btAddFreeSpace: TButton;
    btDelFreeSpace: TButton;
    txFreeSpaceMargin: TLabel;
    edFreeSpaceMargin: TFloatSpinEdit;
    cbFreeSpaceMarginUnit: TComboBox;
    tabScripts: TTabSheet;
    txScriptsHint: TLabel;
    gbScriptAdded: TGroupBox;
    cbScriptAddedEnabled: TCheckBox;
    edScriptAddedFilename: TEdit;
    gbScriptDone: TGroupBox;
    cbScriptDoneEnabled: TCheckBox;
    edScriptDoneFilename: TEdit;
    gbScriptDoneSeeding: TGroupBox;
    cbScriptDoneSeedingEnabled: TCheckBox;
    edScriptDoneSeedingFilename: TEdit;
    txDays: TLabel;
    txFrom: TLabel;
    edDownloadDir: TEdit;
    edIncompleteDir: TEdit;
    edBlocklistURL: TEdit;
    edMaxDown: TSpinEdit;
    edAltDown: TSpinEdit;
    edMaxPeers: TSpinEdit;
    edMaxUp: TSpinEdit;
    edAltUp: TSpinEdit;
    edPort: TSpinEdit;
    edSeedRatio: TFloatSpinEdit;
    gbBandwidth: TGroupBox;
    edIdleSeedLimit: TSpinEdit;
    gbAltSpeed: TGroupBox;
    edAltTimeBegin: TMaskEdit;
    txAltUp: TLabel;
    txAltDown: TLabel;
    txMinutes1: TLabel;
    txTo: TLabel;
    txKbs3: TLabel;
    txKbs4: TLabel;
    txMinutes: TLabel;
    txMB: TLabel;
    txCacheSize: TLabel;
    Page: TPageControl;
    edCacheSize: TSpinEdit;
    tabNetwork: TTabSheet;
    tabBandwidth: TTabSheet;
    tabDownload: TTabSheet;
    txDownloadDir: TLabel;
    txEncryption: TLabel;
    txKbs1: TLabel;
    txKbs2: TLabel;
    txPeerLimit: TLabel;
    txPort: TLabel;
    Procedure btOKClick(Sender: TObject);
    Procedure btTestPortClick(Sender: TObject);
    Procedure btAddFreeSpaceClick(Sender: TObject);
    Procedure btDelFreeSpaceClick(Sender: TObject);
    Procedure gFreeSpaceSelectEditor(Sender: TObject; aCol, aRow: Integer;
      Var Editor: TWinControl);
    Procedure gFreeSpacePrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    Procedure gFreeSpaceDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    Procedure FormDestroy(Sender: TObject);
    Procedure cbScriptEnabledClick(Sender: TObject);
    Procedure cbAutoAltClick(Sender: TObject);
    Procedure cbBlocklistClick(Sender: TObject);
    Procedure cbIdleSeedLimitClick(Sender: TObject);
    Procedure cbIncompleteDirClick(Sender: TObject);
    Procedure cbMaxDownClick(Sender: TObject);
    Procedure cbMaxUpClick(Sender: TObject);
    Procedure cbRandomPortClick(Sender: TObject);
    Procedure cbSeedRatioClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  Private
    FScriptSettingsFrame: TScriptSettingsFrame;
    FScriptSettingsTab: TTabSheet;
    FScriptSettingsLoaded: Boolean;
    Function ValidateFreeSpace: Boolean;
    Procedure LoadScriptSettings;
    Function SaveScriptSettings: Boolean;
  Public
    Procedure SetFreeSpaceData(Rules: TJSONArray; Const Margin: String);
    Function GetFreeSpaceRules: TStringList;
    Function GetResumeMargin: String;
  End;

Implementation

Uses main, utils, StrUtils;

  { TDaemonOptionsForm }

Function NormalizeUnit(Const AUnit: String): String;
Begin
  Result := UpperCase(Trim(AUnit));
  If Result = 'B' Then
    Result := '';
End;

Function IsValidSizeUnit(Const AUnit: String): Boolean;
Begin
  Result := (AUnit = '') Or (AUnit = 'K') Or (AUnit = 'M') Or
    (AUnit = 'G') Or (AUnit = 'T');
End;

Function NormalizeSizeValue(Const S: String; Out AValue: String): Boolean;
Var
  i, SepCnt: Integer;
  c: Char;
  d: Double;
Begin
  AValue := Trim(S);
  Result := False;
  If AValue = '' Then
    exit;
  SepCnt := 0;
  For i := 1 To Length(AValue) Do
  Begin
    c := AValue[i];
    If (c >= '0') And (c <= '9') Then
      Continue;
    If (c = '.') Or (c = ',') Then
    Begin
      Inc(SepCnt);
      If SepCnt > 1 Then
        exit;
      Continue;
    End;
    exit;
  End;
  AValue := StringReplace(AValue, ',', '.', [rfReplaceAll]);
  Result := TryStrToFloat(StringReplace(AValue, '.',
    FormatSettings.DecimalSeparator, [rfReplaceAll]), d) And (d >= 0);
End;

Function ExtractSizeValueUnit(Const S: String; Out AValue, AUnit: String): Boolean;
Var
  i: Integer;
  v: String;
Begin
  v := Trim(S);
  AUnit := '';
  i := Length(v);
  While (i > 0) And Not ((v[i] >= '0') And (v[i] <= '9')) And
    (v[i] <> '.') And (v[i] <> ',') Do
    Dec(i);
  If i < Length(v) Then
  Begin
    AUnit := NormalizeUnit(Copy(v, i + 1, MaxInt));
    v := Copy(v, 1, i);
  End;
  Result := NormalizeSizeValue(v, AValue) And IsValidSizeUnit(AUnit);
End;

Function SplitFreeSpaceRule(Const S: String; Out APath, AValue,
  AUnit: String): Boolean;
Var
  p: Integer;
  sz: String;
Begin
  APath := Trim(S);
  AValue := '';
  AUnit := '';
  p := RPos('=', S);
  If p = 0 Then
    Exit(False);
  APath := Trim(Copy(S, 1, p - 1));
  sz := Trim(Copy(S, p + 1, MaxInt));
  Result := (APath <> '') And ExtractSizeValueUnit(sz, AValue, AUnit);
End;

Function IsWindowsPath(Const APath: String): Boolean;
Var
  c: Char;
Begin
  Result := False;
  If Length(APath) < 2 Then
    exit;
  c := APath[1];
  If (APath[2] = ':') And
    (((c >= 'A') And (c <= 'Z')) Or ((c >= 'a') And (c <= 'z'))) Then
    Exit(True);
  Result := (Copy(APath, 1, 2) = '\\') Or (Copy(APath, 1, 2) = '//');
End;

Function FreeSpaceDisplayPath(Const APath: String): String;
Begin
  Result := Trim(APath);
  If IsWindowsPath(Result) Then
    Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
End;

Function FreeSpaceRemotePath(Const APath: String): String;
Begin
  Result := Trim(APath);
  If IsWindowsPath(Result) Then
    Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
End;

Function UnitDisplayName(Const AUnit: String): String;
Begin
  Case UpperCase(AUnit) Of
    'K': Result := sKByte;
    'M': Result := sMByte;
    'G': Result := sGByte;
    'T': Result := sTByte;
    Else Result := sByte;
  End;
End;

Function TryUnitDisplayToSuffix(Const S: String; Out AUnit: String): Boolean;
Var
  t: String;
Begin
  t := Trim(S);
  AUnit := '';
  If (t = '') Or (t = sByte) Then
    Exit(True);
  If t = sKByte Then
    AUnit := 'K'
  Else
    If t = sMByte Then
      AUnit := 'M'
    Else
      If t = sGByte Then
        AUnit := 'G'
      Else
        If t = sTByte Then
          AUnit := 'T'
        Else
          Exit(False);
  Result := True;
End;

Procedure TDaemonOptionsForm.SetFreeSpaceData(Rules: TJSONArray;
  Const Margin: String);
Var
  i: Integer;
  s, path, val, szUnit: String;

  Procedure AddRow(const P, V, U: String);
  Begin
    gFreeSpace.RowCount := gFreeSpace.RowCount + 1;
    gFreeSpace.Cells[0, gFreeSpace.RowCount - 1] := P;
    gFreeSpace.Cells[1, gFreeSpace.RowCount - 1] := V;
    gFreeSpace.Cells[2, gFreeSpace.RowCount - 1] := U;
  End;

Begin
  gFreeSpace.RowCount := 1;
  If Rules <> nil Then
    For i := 0 To Rules.Count - 1 Do
      If Rules.Items[i].JSONType = jtString Then
      Begin
        s := Rules.Strings[i];
        If Not SplitFreeSpaceRule(s, path, val, szUnit) Then
        Begin
          path := s;
          val := '';
          szUnit := '';
        End;
        If val = '' Then
          AddRow(FreeSpaceDisplayPath(path), '', '')
        Else
          AddRow(FreeSpaceDisplayPath(path), val, UnitDisplayName(szUnit));
      End;
  If gFreeSpace.RowCount = 1 Then
    AddRow('', '', '');

  If ExtractSizeValueUnit(Margin, val, szUnit) Then
  Begin
    edFreeSpaceMargin.Value := StrToFloatDef(
      StringReplace(val, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]), 0);
    If cbFreeSpaceMarginUnit.Items.IndexOf(UnitDisplayName(szUnit)) >= 0 Then
      cbFreeSpaceMarginUnit.ItemIndex :=
        cbFreeSpaceMarginUnit.Items.IndexOf(UnitDisplayName(szUnit))
    Else
      cbFreeSpaceMarginUnit.ItemIndex := 2;
  End
  Else
  Begin
    edFreeSpaceMargin.Value := 0;
    cbFreeSpaceMarginUnit.ItemIndex := 2;
  End;
End;

Function TDaemonOptionsForm.GetFreeSpaceRules: TStringList;
Var
  i: Integer;
  path, val, szUnit: String;
Begin
  Result := TStringList.Create;
  If Not gFreeSpace.Visible Then
    exit;
  For i := 1 To gFreeSpace.RowCount - 1 Do
  Begin
    path := FreeSpaceRemotePath(gFreeSpace.Cells[0, i]);
    If path = '' Then
      Continue;
    If Not NormalizeSizeValue(gFreeSpace.Cells[1, i], val) Then
      Continue;
    If Not TryUnitDisplayToSuffix(gFreeSpace.Cells[2, i], szUnit) Then
      Continue;
    Result.Add(path + '=' + val + szUnit);
  End;
End;

Function TDaemonOptionsForm.GetResumeMargin: String;
Var
  s, szUnit: String;
Begin
  If edFreeSpaceMargin.Value <= 0 Then
    Exit('0');
  s := FormatFloat('0.####', edFreeSpaceMargin.Value);
  s := StringReplace(s, FormatSettings.DecimalSeparator, '.', [rfReplaceAll]);
  If Not TryUnitDisplayToSuffix(cbFreeSpaceMarginUnit.Text, szUnit) Then
    szUnit := '';
  Result := s + szUnit;
End;

Function TDaemonOptionsForm.ValidateFreeSpace: Boolean;
Var
  i: Integer;
  path, val, szUnit, s: String;
Begin
  Result := False;
  If Not tabFreeSpace.TabVisible Then
    Exit(True);
  If gFreeSpace.Visible Then
    For i := 1 To gFreeSpace.RowCount - 1 Do
    Begin
      path := FreeSpaceDisplayPath(gFreeSpace.Cells[0, i]);
      If path = '' Then
        Continue;
      gFreeSpace.Cells[0, i] := path;
      s := gFreeSpace.Cells[1, i];
      If Not NormalizeSizeValue(s, val) Then
      Begin
        Page.ActivePage := tabFreeSpace;
        gFreeSpace.Row := i;
        gFreeSpace.Col := 1;
        gFreeSpace.SetFocus;
        MessageDlg(Format(SInvalidFreeSpaceValue, [s]), mtError, [mbOK], 0);
        exit;
      End;
      s := gFreeSpace.Cells[2, i];
      If Not TryUnitDisplayToSuffix(s, szUnit) Then
      Begin
        Page.ActivePage := tabFreeSpace;
        gFreeSpace.Row := i;
        gFreeSpace.Col := 2;
        gFreeSpace.SetFocus;
        MessageDlg(Format(SInvalidFreeSpaceUnit, [s]), mtError, [mbOK], 0);
        exit;
      End;
      gFreeSpace.Cells[1, i] := val;
      gFreeSpace.Cells[2, i] := UnitDisplayName(szUnit);
    End;
  Result := True;
End;

Procedure TDaemonOptionsForm.gFreeSpaceSelectEditor(Sender: TObject;
  aCol, aRow: Integer; Var Editor: TWinControl);
Begin
  If (aCol = 2) And (aRow >= gFreeSpace.FixedRows) And
    (Editor Is TPickListCellEditor) Then
  Begin
    TPickListCellEditor(Editor).Style := csDropDownList;
    Editor.Height := gFreeSpace.RowHeights[aRow];
  End;
End;

Procedure TDaemonOptionsForm.gFreeSpacePrepareCanvas(Sender: TObject;
  aCol, aRow: Integer; aState: TGridDrawState);
Begin
  If gFreeSpace.EditorMode Or (aRow < gFreeSpace.FixedRows) Then
    exit;
  If (gdFocused In aState) Or (gdSelected In aState) Then
  Begin
    gFreeSpace.Canvas.Brush.Color := clHighlight;
    gFreeSpace.Canvas.Font.Color := clHighlightText;
  End
  Else
    If aRow = gFreeSpace.Row Then
    Begin
      gFreeSpace.Canvas.Brush.Color := $00F2E9DB;
      gFreeSpace.Canvas.Font.Color := clWindowText;
    End;
End;

Procedure TDaemonOptionsForm.gFreeSpaceDrawCell(Sender: TObject;
  aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
Begin
  If (aRow < gFreeSpace.FixedRows) Or Not (gdFocused In aState) Then
    exit;
  gFreeSpace.Canvas.Brush.Style := bsClear;
  gFreeSpace.Canvas.Pen.Color := clHighlightText;
  gFreeSpace.Canvas.Pen.Width := 2;
  gFreeSpace.Canvas.Rectangle(aRect.Left + 1, aRect.Top + 1,
    aRect.Right - 1, aRect.Bottom - 1);
  gFreeSpace.Canvas.Brush.Style := bsSolid;
End;

Procedure TDaemonOptionsForm.btAddFreeSpaceClick(Sender: TObject);
Begin
  gFreeSpace.RowCount := gFreeSpace.RowCount + 1;
  gFreeSpace.Row := gFreeSpace.RowCount - 1;
  gFreeSpace.Col := 0;
  gFreeSpace.SetFocus;
End;

Procedure TDaemonOptionsForm.btDelFreeSpaceClick(Sender: TObject);
Begin
  If gFreeSpace.Row < 1 Then
    exit;
  If gFreeSpace.RowCount > 2 Then
    gFreeSpace.DeleteRow(gFreeSpace.Row)
  Else
  Begin
    gFreeSpace.Cells[0, gFreeSpace.Row] := '';
    gFreeSpace.Cells[1, gFreeSpace.Row] := '';
    gFreeSpace.Cells[2, gFreeSpace.Row] := '';
  End;
End;

Procedure TDaemonOptionsForm.cbMaxDownClick(Sender: TObject);
Begin
  edMaxDown.Enabled := cbMaxDown.Checked;
End;

Procedure TDaemonOptionsForm.btTestPortClick(Sender: TObject);
Var
  req, res: TJSONObject;
Begin
  AppBusy;
  req := TJSONObject.Create;
  Try
    req.Add('method', 'port-test');
    res := RpcObj.SendRequest(req, False);
    AppNormal;
    If res = nil Then
      MainForm.CheckStatus(False)
    Else
      If res.Objects['arguments'].Integers['port-is-open'] <> 0 Then
        MessageDlg(sPortTestSuccess, mtInformation, [mbOK], 0)
      Else
        MessageDlg(sPortTestFailed, mtError, [mbOK], 0);
    res.Free;
  Finally
    req.Free;
  End;
End;

Procedure TDaemonOptionsForm.cbScriptEnabledClick(Sender: TObject);
Begin
  edScriptAddedFilename.Enabled := cbScriptAddedEnabled.Checked;
  edScriptDoneFilename.Enabled := cbScriptDoneEnabled.Checked;
  edScriptDoneSeedingFilename.Enabled := cbScriptDoneSeedingEnabled.Checked;
End;

Procedure TDaemonOptionsForm.cbAutoAltClick(Sender: TObject);
Var
  i: Integer;
Begin
  edAltTimeBegin.Enabled := cbAutoAlt.Checked;
  edAltTimeEnd.Enabled := cbAutoAlt.Checked;
  txFrom.Enabled := cbAutoAlt.Checked;
  txTo.Enabled := cbAutoAlt.Checked;
  txDays.Enabled := cbAutoAlt.Checked;
  For i := 1 To 7 Do
    gbAltSpeed.FindChildControl(Format('cbDay%d', [i])).Enabled :=
      cbAutoAlt.Checked;
End;

Procedure TDaemonOptionsForm.cbBlocklistClick(Sender: TObject);
Begin
  If Not edBlocklistURL.Visible Then
    exit;
  edBlocklistURL.Enabled := cbBlocklist.Checked;
  If edBlocklistURL.Enabled Then
    edBlocklistURL.Color := clWindow
  Else
    edBlocklistURL.ParentColor := True;
End;

Procedure TDaemonOptionsForm.cbIdleSeedLimitClick(Sender: TObject);
Begin
  edIdleSeedLimit.Enabled := cbIdleSeedLimit.Checked;
End;

Procedure TDaemonOptionsForm.btOKClick(Sender: TObject);
Begin
  edDownloadDir.Text := Trim(edDownloadDir.Text);
  If edDownloadDir.Text = '' Then
  Begin
    Page.ActivePage := tabDownload;
    edDownloadDir.SetFocus;
    MessageDlg(SNoDownloadDir, mtError, [mbOK], 0);
    exit;
  End;
  edIncompleteDir.Text := Trim(edIncompleteDir.Text);
  If cbIncompleteDir.Checked And (edIncompleteDir.Text = '') Then
  Begin
    Page.ActivePage := tabDownload;
    edIncompleteDir.SetFocus;
    MessageDlg(SNoIncompleteDir, mtError, [mbOK], 0);
    exit;
  End;
  edBlocklistURL.Text := Trim(edBlocklistURL.Text);
  If cbAutoAlt.Checked Then
  Begin
    If StrToTimeDef(edAltTimeBegin.Text, -1) < 0 Then
    Begin
      Page.ActivePage := tabBandwidth;
      edAltTimeBegin.SetFocus;
      MessageDlg(SInvalidTime, mtError, [mbOK], 0);
      exit;
    End;
    If StrToTimeDef(edAltTimeEnd.Text, -1) < 0 Then
    Begin
      Page.ActivePage := tabBandwidth;
      edAltTimeEnd.SetFocus;
      MessageDlg(SInvalidTime, mtError, [mbOK], 0);
      exit;
    End;
  End;
  If Not ValidateFreeSpace Then
    exit;
  If Not SaveScriptSettings Then
    exit;
  ModalResult := mrOk;
End;

Procedure TDaemonOptionsForm.cbIncompleteDirClick(Sender: TObject);
Begin
  edIncompleteDir.Enabled := cbIncompleteDir.Checked;
  If edIncompleteDir.Enabled Then
    edIncompleteDir.Color := clWindow
  Else
    edIncompleteDir.ParentColor := True;
End;

Procedure TDaemonOptionsForm.cbMaxUpClick(Sender: TObject);
Begin
  edMaxUp.Enabled := cbMaxUp.Checked;
End;

Procedure TDaemonOptionsForm.cbRandomPortClick(Sender: TObject);
Begin
  edPort.Enabled := Not cbRandomPort.Checked;
End;

Procedure TDaemonOptionsForm.cbSeedRatioClick(Sender: TObject);
Begin
  edSeedRatio.Enabled := cbSeedRatio.Checked;
End;

Procedure TDaemonOptionsForm.FormDestroy(Sender: TObject);
Begin
  If Not Assigned(Ini) Then
    exit;
  Ini.WriteInteger('DaemonOptions', 'FreeSpacePathWidth',
    gFreeSpace.Columns[0].Width);
  Ini.WriteInteger('DaemonOptions', 'FreeSpaceValueWidth',
    gFreeSpace.Columns[1].Width);
  Ini.WriteInteger('DaemonOptions', 'FreeSpaceUnitWidth',
    gFreeSpace.Columns[2].Width);
  Ini.UpdateFile;
End;

Procedure TDaemonOptionsForm.FormShow(Sender: TObject);
Begin
  If Not FScriptSettingsLoaded Then
    LoadScriptSettings;
End;

Procedure LogScriptSettingsError(Const AWhere: String; E: Exception);
Var
  sl: TStringList;
  fn: String;
  Frames: PPointer;
  FrameCount, FrameNumber: Integer;
Begin
  Try
    If FHomeDir <> '' Then
      fn := IncludeTrailingPathDelimiter(FHomeDir) + 'script-settings-error.log'
    Else
      fn := IncludeTrailingPathDelimiter(GetTempDir) + 'script-settings-error.log';
    sl := TStringList.Create;
    Try
      sl.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
      sl.Add(AWhere + ': ' + E.ClassName + ': ' + E.Message);
      Try
        sl.Add('  ' + BackTraceStrFunc(ExceptAddr));
        Frames := ExceptFrames;
        FrameCount := ExceptFrameCount;
        For FrameNumber := 0 To FrameCount - 1 Do
          sl.Add('  ' + BackTraceStrFunc(Frames[FrameNumber]));
      Except
      End;
      sl.SaveToFile(fn);
    Finally
      sl.Free;
    End;
  Except
  End;
End;

Procedure TDaemonOptionsForm.LoadScriptSettings;
Var
  req, args, res, settings, sr: TJSONObject;
  d: TJSONData;
  fields, scripts, hosts: TJSONArray;
Begin
  FScriptSettingsLoaded := True;
  Try
  If Not Assigned(RpcObj) Or Not RpcObj.Connected Then
    exit;
  If Not RpcObj.HasForkFeature('script_runner') Or
    Not RpcObj.HasForkFeature('script_runner_settings') Then
    exit;

  If FScriptSettingsTab = nil Then
  Begin
    FScriptSettingsTab := Page.AddTabSheet;
    FScriptSettingsTab.Caption := SScriptParameters;
    FScriptSettingsFrame := TScriptSettingsFrame.Create(FScriptSettingsTab);
    FScriptSettingsFrame.Parent := FScriptSettingsTab;
    FScriptSettingsFrame.Align := alClient;
  End;
  If FScriptSettingsFrame = nil Then
  Begin
    FScriptSettingsFrame := TScriptSettingsFrame.Create(FScriptSettingsTab);
    FScriptSettingsFrame.Parent := FScriptSettingsTab;
    FScriptSettingsFrame.Align := alClient;
  End;
  FScriptSettingsTab.TabVisible := False;

  req := TJSONObject.Create;
  Try
    req.Add('method', 'session-get');
    args := TJSONObject.Create;
    fields := TJSONArray.Create;
    fields.Add('script_runner');
    args.Add('fields', fields);
    req.Add('arguments', args);
    res := RpcObj.SendRequest(req);
  Finally
    req.Free;
  End;
  If res = nil Then
    exit;

  settings := nil;
  Try
    d := res.Find('script_runner');
    If Not (d Is TJSONObject) Then
      exit;
    sr := TJSONObject(d);
    scripts := nil;
    hosts := nil;
    d := sr.Find('scripts');
    If d Is TJSONArray Then
      scripts := TJSONArray(d);
    d := sr.Find('external_hosts');
    If d Is TJSONArray Then
      hosts := TJSONArray(d);

    req := TJSONObject.Create;
    Try
      req.Add('method', 'script_settings_get');
      req.Add('arguments', TJSONObject.Create);
      settings := RpcObj.SendRequest(req);
    Finally
      req.Free;
    End;

    FScriptSettingsFrame.LoadData(scripts, settings, hosts);
    FScriptSettingsTab.TabVisible := FScriptSettingsFrame.HasHostParams;
    If FScriptSettingsTab.TabVisible And (settings = nil) Then
      FScriptSettingsFrame.SetHintText(SScriptSettingsUnavailable);
  Finally
    settings.Free;
    res.Free;
  End;
  Except
    On E: Exception Do
    Begin
      LogScriptSettingsError('LoadScriptSettings', E);
      If FScriptSettingsTab <> nil Then
        FScriptSettingsTab.TabVisible := False;
    End;
  End;
End;

Function TDaemonOptionsForm.SaveScriptSettings: Boolean;
Var
  req, args, res: TJSONObject;
  err, msg: String;
Begin
  Result := True;
  Try
  If Not FScriptSettingsLoaded Or (FScriptSettingsTab = nil) Or
    (FScriptSettingsFrame = nil) Or Not FScriptSettingsTab.TabVisible Then
    exit;
  If Not FScriptSettingsFrame.BuildRequest(args, err) Then
  Begin
    Page.ActivePage := FScriptSettingsTab;
    MessageDlg('', err, mtError, [mbOK], 0);
    exit(False);
  End;
  If args = nil Then
    exit;

  req := TJSONObject.Create;
  res := nil;
  Try
    req.Add('method', 'script_settings_set');
    req.Add('arguments', args);
    args := nil;
    AppBusy;
    Try
      res := RpcObj.SendRequest(req);
    Finally
      AppNormal;
    End;
    If res = nil Then
    Begin
      msg := RpcObj.Status;
      If msg = '' Then
        msg := SScriptSettingsUnavailable;
      MessageDlg('', msg, mtError, [mbOK], 0);
      exit(False);
    End;
    Result := True;
  Finally
    res.Free;
    args.Free;
    req.Free;
  End;
  Except
    On E: Exception Do
    Begin
      LogScriptSettingsError('SaveScriptSettings', E);
      MessageDlg('', E.Message, mtError, [mbOK], 0);
      Result := False;
    End;
  End;
End;

Procedure TDaemonOptionsForm.FormCreate(Sender: TObject);
Var
  i, j, x, wd: Integer;
  cb: TCheckBox;
Begin
  bidiMode := GetBiDi();
  Page.ActivePageIndex := 0;
  cbEncryption.Items.Add(sEncryptionDisabled);
  cbEncryption.Items.Add(sEncryptionEnabled);
  cbEncryption.Items.Add(sEncryptionRequired);
  cbFreeSpaceMarginUnit.Items.Add(sByte);
  cbFreeSpaceMarginUnit.Items.Add(sKByte);
  cbFreeSpaceMarginUnit.Items.Add(sMByte);
  cbFreeSpaceMarginUnit.Items.Add(sGByte);
  cbFreeSpaceMarginUnit.Items.Add(sTByte);
  cbFreeSpaceMarginUnit.ItemIndex := 2;
  gFreeSpace.Columns[2].ButtonStyle := cbsPickList;
  gFreeSpace.Columns[2].PickList.Clear;
  gFreeSpace.Columns[2].PickList.Add(sByte);
  gFreeSpace.Columns[2].PickList.Add(sKByte);
  gFreeSpace.Columns[2].PickList.Add(sMByte);
  gFreeSpace.Columns[2].PickList.Add(sGByte);
  gFreeSpace.Columns[2].PickList.Add(sTByte);
  gFreeSpace.Options := gFreeSpace.Options + [goColSizing];
  If Assigned(Ini) Then
  Begin
    gFreeSpace.Columns[0].Width :=
      Ini.ReadInteger('DaemonOptions', 'FreeSpacePathWidth', 300);
    gFreeSpace.Columns[1].Width :=
      Ini.ReadInteger('DaemonOptions', 'FreeSpaceValueWidth', 90);
    gFreeSpace.Columns[2].Width :=
      Ini.ReadInteger('DaemonOptions', 'FreeSpaceUnitWidth', 90);
  End;
  OnDestroy := @FormDestroy;
  cbScriptEnabledClick(nil);
  gFreeSpace.OnSelectEditor := @gFreeSpaceSelectEditor;
  gFreeSpace.OnPrepareCanvas := @gFreeSpacePrepareCanvas;
  gFreeSpace.OnDrawCell := @gFreeSpaceDrawCell;
  gFreeSpace.DefaultRowHeight := Abs(gFreeSpace.Font.Height) + 8;
  If gFreeSpace.DefaultRowHeight < 28 Then
    gFreeSpace.DefaultRowHeight := 28;
  If gFreeSpace.DefaultRowHeight < cbFreeSpaceMarginUnit.Height + 2 Then
    gFreeSpace.DefaultRowHeight := cbFreeSpaceMarginUnit.Height + 2;
  gFreeSpace.RowCount := 2;
  For i := 0 To gFreeSpace.RowCount - 1 Do
    gFreeSpace.RowHeights[i] := gFreeSpace.DefaultRowHeight;
  gFreeSpace.Cells[0, 1] := '';
  gFreeSpace.Cells[1, 1] := '';
  gFreeSpace.Cells[2, 1] := '';
  Buttons.OKButton.ModalResult := mrNone;
  Buttons.OKButton.OnClick := @btOKClick;

  OnShow := @FormShow;

  x := edAltTimeBegin.Left;
  wd := (gbAltSpeed.ClientWidth - x - BorderWidth) Div 7;
  For i := 1 To 7 Do
  Begin
    cb := TCheckBox.Create(gbAltSpeed);
    cb.Parent := gbAltSpeed;
    j := i + 1;
    If j > 7 Then
      Dec(j, 7);
    cb.Caption := SysToUTF8(FormatSettings.ShortDayNames[j]);
    cb.Name := Format('cbDay%d', [j]);
    cb.Left := x;
    cb.Top := txDays.Top - (cb.Height - txDays.Height) Div 2;
    Inc(x, wd);
  End;
End;

Initialization
  {$I daemonoptions.lrs}

End.
