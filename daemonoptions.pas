

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
ButtonPanel, BaseForm;

resourcestring
sPortTestSuccess = 'Incoming port tested successfully.';
sPortTestFailed = 'Incoming port is closed. Check your firewall settings.';
sEncryptionDisabled = 'Encryption disabled';
sEncryptionEnabled = 'Encryption enabled';
sEncryptionRequired = 'Encryption required';
SNoDownloadDir = 'The downloads directory was not specified.';
SNoIncompleteDir = 'The directory for incomplete files was not specified.';
// SNoBlocklistURL = 'The blocklist URL was not specified.';
SInvalidTime = 'The invalid time value was entered.';

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
    Procedure cbAutoAltClick(Sender: TObject);
    Procedure cbBlocklistClick(Sender: TObject);
    Procedure cbIdleSeedLimitClick(Sender: TObject);
    Procedure cbIncompleteDirClick(Sender: TObject);
    Procedure cbMaxDownClick(Sender: TObject);
    Procedure cbMaxUpClick(Sender: TObject);
    Procedure cbRandomPortClick(Sender: TObject);
    Procedure cbSeedRatioClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Private 
    { private declarations }
    Public 
    { public declarations }
  End;

Implementation

Uses main, utils, fpjson;

{ TDaemonOptionsForm }

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
    If res = Nil Then
      MainForm.CheckStatus(False)
    Else
      If res.Objects['arguments'].Integers['port-is-open'] <> 0 Then
        MessageDlg(sPortTestSuccess, mtInformation, [mbOk], 0)
    Else
      MessageDlg(sPortTestFailed, mtError, [mbOK], 0);
    res.Free;
  Finally
    req.Free;
End;
End;

Procedure TDaemonOptionsForm.cbAutoAltClick(Sender: TObject);

Var 
  i: integer;
Begin
  edAltTimeBegin.Enabled := cbAutoAlt.Checked;
  edAltTimeEnd.Enabled := cbAutoAlt.Checked;
  txFrom.Enabled := cbAutoAlt.Checked;
  txTo.Enabled := cbAutoAlt.Checked;
  txDays.Enabled := cbAutoAlt.Checked;
  For i:=1 To 7 Do
    gbAltSpeed.FindChildControl(Format('cbDay%d', [i])).Enabled := cbAutoAlt.
                                                                   Checked;
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
  ModalResult := mrOK;
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

Procedure TDaemonOptionsForm.FormCreate(Sender: TObject);

Var 
  i, j, x, wd: integer;
  cb: TCheckBox;
Begin
  bidiMode := GetBiDi();
  Page.ActivePageIndex := 0;
  cbEncryption.Items.Add(sEncryptionDisabled);
  cbEncryption.Items.Add(sEncryptionEnabled);
  cbEncryption.Items.Add(sEncryptionRequired);
  Buttons.OKButton.ModalResult := mrNone;
  Buttons.OKButton.OnClick := @btOKClick;

  x := edAltTimeBegin.Left;
  wd := (gbAltSpeed.ClientWidth - x - BorderWidth) Div 7;
  For i:=1 To 7 Do
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

initialization
  {$I daemonoptions.lrs}

End.
