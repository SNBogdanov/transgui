

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

Unit Options;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Spin, Buttons, ButtonPanel, BaseForm,
  ConnOptions;

Type

  { TOptionsForm }

  TOptionsForm = Class(TBaseForm)
    Buttons: TButtonPanel;
    cbDeleteTorrentFile: TCheckBox;
    cbLanguage: TComboBox;
    cbLangLeftRight: TComboBox;
    cbLinksFromClipboard: TCheckBox;
    cbShowAddTorrentWindow: TCheckBox;
    cbTrayClose: TCheckBox;
    cbTrayIconAlways: TCheckBox;
    cbTrayMinimize: TCheckBox;
    cbCheckNewVersion: TCheckBox;
    cbCalcAvg: TCheckBox;
    cbRegExt: TCheckBox;
    cbRegMagnet: TCheckBox;
    cbTrayNotify: TCheckBox;
    edIntfScale: TSpinEdit;
    edCheckVersionDays: TSpinEdit;
    edRefreshInterval: TSpinEdit;
    edRefreshIntervalMin: TSpinEdit;
    gbTray: TGroupBox;
    gbNewTorrent: TGroupBox;
    gbData: TGroupBox;
    gbSysInt: TGroupBox;
    txDays: TLabel;
    tabGeneral: TTabSheet;
    txPerc: TLabel;
    Page: TPageControl;
    tabAdvanced: TTabSheet;
    txLanguage: TLabel;
    txIntfScale: TLabel;
    txRefreshInterval: TLabel;
    txRefreshIntervalMin: TLabel;
    txSeconds: TLabel;
    txSeconds2: TLabel;
    Procedure cbCheckNewVersionClick(Sender: TObject);
    Procedure cbLanguageEnter(Sender: TObject);
    Procedure cbLanguageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure cbLanguageMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure OKButtonClick(Sender: TObject);
  Private
    FLangList: TStringList;
    {$ifdef mswindows}
    FRegExt: Boolean;
    FRegMagnet: Boolean;
    {$endif mswindows}

    Procedure FillLanguageItems;
  Public
    ConnForm: TConnOptionsForm;
  End;

Implementation

Uses
  {$ifdef mswindows}
  registry,
  {$endif mswindows}
  main, utils, ResTranslator, Math;

  { TOptionsForm }

Procedure TOptionsForm.FormCreate(Sender: TObject);
Var
  i: Integer;
  pg: TTabSheet;
  {$ifdef mswindows}
  reg: TRegistry;
  s: String;
  {$endif mswindows}
Begin
  bidiMode := GetBiDi();
  cbRegExt.Caption := Format(cbRegExt.Caption, [AppName]);
  cbRegMagnet.Caption := Format(cbRegMagnet.Caption, [AppName]);

  ConnForm := TConnOptionsForm.Create(Self);
  While ConnForm.Page.ControlCount > 0 Do
  Begin
    pg := ConnForm.Page.Pages[0];
    pg.Parent := Page;
    pg.TabVisible := True;
  End;
  ConnForm.Page.Free;
  ConnForm.Page := Page;

  Page.ActivePageIndex := 0;
  Buttons.OKButton.ModalResult := mrNone;
  Buttons.OKButton.OnClick := @OKButtonClick;

  cbLangLeftRight.Items.Add(sBiDiDefault);
  cbLangLeftRight.Items.Add(sBiDiLeftRight);
  cbLangLeftRight.Items.Add(sBiDiRightLeft);
  cbLangLeftRight.Items.Add(sBiDiRightLeftNoAlign);
  cbLangLeftRight.Items.Add(sBiDiRightLeftReadOnly);
  cbLangLeftRight.ItemIndex :=
    Ini.ReadInteger('Interface', 'IgnoreRightLeft', 0);

  cbLanguage.Items.Add(FTranslationLanguage);
  cbLanguage.ItemIndex := 0;
  i := 80 * 100 Div (ScaleInt(100) * 100 Div IntfScale);
  i := i - i Mod 5;
  If i < 10 Then
    i := 10;
  edIntfScale.MinValue := i;
  {$ifdef LCLgtk2}
  cbLanguage.OnDropDown := @cbLanguageEnter;
  cbLanguage.OnMouseMove := @cbLanguageMouseMove;
{$endif LCLgtk2}

  {$ifdef mswindows}
  gbSysInt.Visible := True;
  reg := TRegistry.Create;
  Try
    If reg.OpenKeyReadOnly('Software\Classes\.torrent') Then
    Begin
      If reg.ReadString('') = AppName Then
      Begin
        reg.CloseKey;
        If reg.OpenKeyReadOnly(Format(
          'Software\Classes\%s\shell\open\command', [AppName])) Then
        Begin
          s := reg.ReadString('');
          FRegExt := CompareFilePath(s, Format('"%s" "%%1"',
            [ParamStr(0)])) = 0;
        End;
      End;
    End;
    reg.CloseKey;
    If reg.OpenKeyReadOnly('Software\Classes\Magnet\shell\open\command') Then
    Begin
      s := reg.ReadString('');
      FRegMagnet := CompareFilePath(s, Format('"%s" "%%1"', [ParamStr(0)])) = 0;
    End;
  Finally
    reg.Free;
  End;
  cbRegExt.Checked := FRegExt;
  cbRegMagnet.Checked := FRegMagnet;
  {$endif mswindows}
End;

Procedure TOptionsForm.cbLanguageEnter(Sender: TObject);
Begin
  If Not Assigned(FLangList) Then
    FillLanguageItems;
End;

Procedure TOptionsForm.cbCheckNewVersionClick(Sender: TObject);
Begin
  edCheckVersionDays.Enabled := cbCheckNewVersion.Checked;
End;

Procedure TOptionsForm.cbLanguageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  cbLanguageEnter(cbLanguage);
End;

Procedure TOptionsForm.cbLanguageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Begin
  cbLanguageEnter(cbLanguage);
End;

Procedure TOptionsForm.FormActivate(Sender: TObject);
Var
  cnv: TControlCanvas;
  w: Integer;
  s: String;
Begin
  w := 0;
  cnv := TControlCanvas.Create;
  Try
    cnv.Control := cbLangLeftRight;
    cnv.Font.Assign(cbLangLeftRight.Font);
    For s In cbLangLeftRight.Items Do
      w := max(w, cnv.TextWidth(s));
    cbLangLeftRight.ItemWidth := w + 16;
  Finally
    cnv.Free;
  End;
End;

Procedure TOptionsForm.FillLanguageItems;
Var
  i: Integer;
Begin
  AppBusy;
  cbLanguage.Items.BeginUpdate;
  Try
    cbLanguage.Items.Clear;
    cbLanguage.Items.Add('English');
    FLangList := GetAvailableTranslations;
    FLangList.Sort;
    With FLangList Do
      For i := 0 To Count - 1 Do
        cbLanguage.Items.Add(Names[i]);
    With cbLanguage Do
      ItemIndex := Items.IndexOf(FTranslationLanguage);
  Finally
    cbLanguage.Items.EndUpdate;
  End;
  AppNormal;
End;

Procedure TOptionsForm.FormDestroy(Sender: TObject);
Begin
  FLangList.Free;
End;

Procedure TOptionsForm.FormShow(Sender: TObject);
Begin
  ConnForm.FormShow(nil);
  If ConnForm.edHost.Text = '' Then
  Begin
    tabGeneral.Hide;
    ActiveControl := ConnForm.edHost;
  End;
End;

Procedure TOptionsForm.OKButtonClick(Sender: TObject);
Var
  s: String;
  bd, idx: Integer;
  restart: Boolean;
  {$ifdef mswindows}
  reg: TRegistry;
  {$endif mswindows}
Begin
  ConnForm.ModalResult := mrNone;
  ConnForm.btOKClick(nil);
  If ConnForm.ModalResult = mrNone Then
    exit;
  restart := False;
  If cbLanguage.Text <> FTranslationLanguage Then
  Begin
    If cbLanguage.Text = 'English' Then
      s := '-'
    Else
      s := FLangList.Values[cbLanguage.Text];
    Ini.WriteString('Interface', 'TranslationFile', s);
    restart := True;
  End;

  // bidi
  idx := cbLangLeftRight.ItemIndex;
  bd := Ini.ReadInteger('Interface', 'IgnoreRightLeft', 0);
  Ini.WriteInteger('Interface', 'IgnoreRightLeft', idx);
  If idx <> bd Then restart := True;

  {$ifdef mswindows}
  reg := TRegistry.Create;
  Try
    If cbRegExt.Checked <> FRegExt Then
      If cbRegExt.Checked Then
      Begin
        If reg.OpenKey('Software\Classes\.torrent', True) Then
        Begin
          reg.WriteString('', AppName);
          reg.CloseKey;
          If reg.OpenKey(Format('Software\Classes\%s\DefaultIcon',
            [AppName]), True) Then
          Begin
            reg.WriteString('', Format('"%s",0', [ParamStr(0)]));
            reg.CloseKey;
            If reg.OpenKey(Format('Software\Classes\%s\shell\open\command',
              [AppName]), True) Then
            Begin
              reg.WriteString('', Format('"%s" "%%1"', [ParamStr(0)]));
              reg.CloseKey;
            End;
          End;
        End;
      End
      Else
      Begin
        If reg.OpenKey('Software\Classes\.torrent', False) Then
        Begin
          reg.DeleteValue('');
          reg.CloseKey;
        End;
        s := Format('Software\Classes\%s', [AppName]);
        reg.DeleteKey(s + '\DefaultIcon');
        reg.DeleteKey(s + '\shell\open\command');
        reg.DeleteKey(s + '\shell\open');
        reg.DeleteKey(s + '\shell');
        reg.DeleteKey(s);
      End;

    If cbRegMagnet.Checked <> FRegMagnet Then
      If cbRegMagnet.Checked Then
      Begin
        If reg.OpenKey('Software\Classes\Magnet', True) Then
        Begin
          If Not reg.ValueExists('') Then
            reg.WriteString('', 'Magnet URI');
          reg.WriteString('Content Type', 'application/x-magnet');
          reg.WriteString('URL Protocol', '');
          reg.CloseKey;
          If reg.OpenKey('Software\Classes\Magnet\DefaultIcon', True) Then
          Begin
            reg.WriteString('', Format('"%s",0', [ParamStr(0)]));
            reg.CloseKey;
            If reg.OpenKey('Software\Classes\Magnet\shell', True) Then
            Begin
              reg.WriteString('', 'open');
              reg.CloseKey;
              If reg.OpenKey(
                'Software\Classes\Magnet\shell\open\command',
                True) Then
              Begin
                reg.WriteString('', Format('"%s" "%%1"',
                  [ParamStr(0)]));
                reg.CloseKey;
              End;
            End;
          End;
        End;
      End
      Else
      Begin
        reg.DeleteKey('Software\Classes\Magnet\DefaultIcon');
        reg.DeleteKey('Software\Classes\Magnet\shell\open\command');
      End;
  Finally
    reg.Free;
  End;
  {$endif mswindows}

  If edIntfScale.Value <> IntfScale Then
    restart := True;

  If restart Then
    MessageDlg(sRestartRequired, mtInformation, [mbOK], 0);
  ModalResult := mrOk;
End;

Initialization
  {$I options.lrs}

End.
