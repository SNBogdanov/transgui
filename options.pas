

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
  ComCtrls, StdCtrls, Spin, Buttons, ButtonPanel, Grids, BaseForm;

Resourcestring
  SWhisparrSettingsRequired = 'Enter both the Whisparr URL and API key.';
  SWhisparrConnectionOk = 'Connection to Whisparr succeeded. Version: %s.';
  SWhisparrConnectionFailed = 'Connection to Whisparr failed: %s';
  SWhisparrSaveAnyway = 'Whisparr connection check failed: %s' + LineEnding +
    'Save the settings anyway?';
  SWhisparrUnknownVersion = 'Unknown';
  SWindowSizesTab = 'Window size settings';
  SFieldColumn = 'Field';
  SWidthColumn = 'Width';
  SInfoColGap = 'Column gap';
  SInfoCol1Left = 'Left offset';
  SInfoCol1Width = 'Label 1 width';
  SInfoCol2Width = 'Value 1 width';
  SInfoCol3Width = 'Label 2 width';
  SInfoCol4Width = 'Value 2 width';
  SStatusBarPanel = 'Status bar panel %d';

Type

  { TOptionsForm }

  TOptionsForm = Class(TBaseForm)
    Buttons: TButtonPanel;
    btEditFileMenu: TButton;
    btEditTorrentMenu: TButton;
    btWhisparrTest: TButton;
    cbAskScriptParams: TCheckBox;
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
    cbWhisparrVerifySsl: TCheckBox;
    edIntfScale: TSpinEdit;
    edCheckVersionDays: TSpinEdit;
    edRefreshInterval: TSpinEdit;
    edRefreshIntervalMin: TSpinEdit;
    edArchivePageSize: TSpinEdit;
    edWhisparrApiKey: TEdit;
    edWhisparrUrl: TEdit;
    gbArchive: TGroupBox;
    gbFileMenu: TGroupBox;
    gbTorrentMenu: TGroupBox;
    gbTray: TGroupBox;
    gbWhisparr: TGroupBox;
    gbNewTorrent: TGroupBox;
    gbData: TGroupBox;
    gbSysInt: TGroupBox;
    txArchivePageSize: TLabel;
    txDays: TLabel;
    tabGeneral: TTabSheet;
    tabUserMenus: TTabSheet;
    tabWhisparr: TTabSheet;
    txPerc: TLabel;
    txTorrentMenu: TLabel;
    txFileMenu: TLabel;
    txUserMenuHint: TLabel;
    txWhisparrApiKey: TLabel;
    txWhisparrHint: TLabel;
    txWhisparrUrl: TLabel;
    Page: TPageControl;
    tabAdvanced: TTabSheet;
    txLanguage: TLabel;
    txIntfScale: TLabel;
    txRefreshInterval: TLabel;
    txRefreshIntervalMin: TLabel;
    txSeconds: TLabel;
    txSeconds2: TLabel;
    Procedure btEditFileMenuClick(Sender: TObject);
    Procedure btEditTorrentMenuClick(Sender: TObject);
    Procedure btWhisparrTestClick(Sender: TObject);
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
    FWhisparrDirty: Boolean;
    FWidthsTab: TTabSheet;
    FWidthsGrid: TStringGrid;
    {$ifdef mswindows}
    FRegExt: Boolean;
    FRegMagnet: Boolean;
    {$endif mswindows}

    Procedure FillLanguageItems;
    Function TestWhisparr(Out Version, Error: String): Boolean;
    Procedure WhisparrChanged(Sender: TObject);
    Procedure InitWindowSizesTab;
    Procedure LoadWindowSizes;
    Procedure SaveWindowSizes;
    Procedure WidthsSelectEditor(Sender: TObject; ACol, ARow: Integer;
      Var Editor: TWinControl);
    Procedure WidthsEditingDone(Sender: TObject);
    Procedure ApplyWidthsRow(ARow: Integer);
  End;

Implementation

Uses
  {$ifdef mswindows}
  registry,
  {$endif mswindows}
  main, UserMenuForm, utils, ResTranslator, Math, WhisparrClient;

  { TOptionsForm }

Const
  WindowSizesInfoKeys: Array[0..5] Of String = (
    'ColGap', 'Col1Left', 'Col1Width', 'Col2Width', 'Col3Width', 'Col4Width');
  WindowSizesInfoDefaults: Array[0..5] Of Integer = (
    16, 4, 200, 719, 459, 464);

Procedure TOptionsForm.FormCreate(Sender: TObject);
Var
  i: Integer;
  {$ifdef mswindows}
  reg: TRegistry;
  s: String;
  {$endif mswindows}
Begin
  bidiMode := GetBiDi();
  cbRegExt.Caption := Format(cbRegExt.Caption, [AppName]);
  cbRegMagnet.Caption := Format(cbRegMagnet.Caption, [AppName]);

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

  cbAskScriptParams.Checked :=
    Ini.ReadBool('Interface', 'AskScriptParams', True);
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
  {$ifndef mswindows}
  tabUserMenus.TabVisible := False;
  {$endif}

  edWhisparrUrl.OnChange := @WhisparrChanged;
  edWhisparrApiKey.OnChange := @WhisparrChanged;
  cbWhisparrVerifySsl.OnClick := @WhisparrChanged;
  InitWindowSizesTab;
End;

Procedure TOptionsForm.WhisparrChanged(Sender: TObject);
Begin
  FWhisparrDirty := True;
End;

Procedure TOptionsForm.WidthsSelectEditor(Sender: TObject; ACol, ARow: Integer;
  Var Editor: TWinControl);
Begin
  If (ACol = 0) Or (ARow = 0) Then
    Editor := nil;
End;

Procedure TOptionsForm.WidthsEditingDone(Sender: TObject);
Begin
  If (FWidthsGrid <> nil) And (FWidthsGrid.Col = 1) And (FWidthsGrid.Row > 0)
    Then
    ApplyWidthsRow(FWidthsGrid.Row);
End;

Procedure TOptionsForm.ApplyWidthsRow(ARow: Integer);
Var
  i, W, Def: Integer;
Begin
  If (FWidthsGrid = nil) Or (MainForm = nil) Or (ARow <= 0) Then
    exit;
  If ARow <= High(WindowSizesInfoKeys) + 1 Then
  Begin
    i := ARow - 1;
    Def := Ini.ReadInteger('GeneralInfo', WindowSizesInfoKeys[i],
      WindowSizesInfoDefaults[i]);
    W := StrToIntDef(Trim(FWidthsGrid.Cells[1, ARow]), Def);
    If (WindowSizesInfoKeys[i] = 'ColGap') Or
      (WindowSizesInfoKeys[i] = 'Col1Left') Then
    Begin
      If W < 0 Then W := 0;
    End
    Else If W < 20 Then
      W := 20;
    FWidthsGrid.Cells[1, ARow] := IntToStr(W);
    Ini.WriteInteger('GeneralInfo', WindowSizesInfoKeys[i], W);
    MainForm.ReloadInfoLayout;
  End
  Else
  Begin
    i := ARow - 1 - Length(WindowSizesInfoKeys);
    If (MainForm.StatusBar = nil) Or (i < 0) Or
      (i >= MainForm.StatusBar.Panels.Count) Then
      exit;
    Def := MainForm.StatusBar.Panels[i].Width;
    W := StrToIntDef(Trim(FWidthsGrid.Cells[1, ARow]), Def);
    If W < 1 Then
      W := Def;
    If W < 1 Then
      W := 20;
    FWidthsGrid.Cells[1, ARow] := IntToStr(W);
    Ini.WriteInteger('StatusBarPanels', IntToStr(i), W);
    Ini.WriteInteger('StatusBarPanels', 'ScreenWidth', Screen.Width);
    MainForm.StatusBar.Panels[i].Width := W;
    MainForm.StatusBar.Update;
  End;
End;

Procedure TOptionsForm.InitWindowSizesTab;
Var
  PanelCount: Integer;
Begin
  If FWidthsTab <> nil Then
    exit;
  FWidthsTab := TTabSheet.Create(Page);
  FWidthsTab.PageControl := Page;
  FWidthsTab.Caption := SWindowSizesTab;

  FWidthsGrid := TStringGrid.Create(FWidthsTab);
  FWidthsGrid.Parent := FWidthsTab;
  FWidthsGrid.Align := alClient;
  FWidthsGrid.BorderSpacing.Around := 8;
  FWidthsGrid.ColCount := 2;
  FWidthsGrid.FixedCols := 1;
  FWidthsGrid.FixedRows := 1;
  FWidthsGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine,
    goHorzLine, goColSizing, goEditing, goTabs, goThumbTracking];
  FWidthsGrid.Cells[0, 0] := SFieldColumn;
  FWidthsGrid.Cells[1, 0] := SWidthColumn;
  FWidthsGrid.ColWidths[0] := 360;
  FWidthsGrid.ColWidths[1] := 120;
  FWidthsGrid.OnSelectEditor := @WidthsSelectEditor;
  FWidthsGrid.OnEditingDone := @WidthsEditingDone;

  If (MainForm <> nil) And (MainForm.StatusBar <> nil) Then
    PanelCount := MainForm.StatusBar.Panels.Count
  Else
    PanelCount := 8;
  FWidthsGrid.RowCount := 1 + Length(WindowSizesInfoKeys) + PanelCount;
  LoadWindowSizes;
End;

Procedure TOptionsForm.LoadWindowSizes;
Var
  i, Row, PanelCount, W: Integer;

  Function InfoCaption(Const Key: String): String;
  Begin
    If Key = 'ColGap' Then
      Result := SInfoColGap
    Else If Key = 'Col1Left' Then
      Result := SInfoCol1Left
    Else If Key = 'Col1Width' Then
      Result := SInfoCol1Width
    Else If Key = 'Col2Width' Then
      Result := SInfoCol2Width
    Else If Key = 'Col3Width' Then
      Result := SInfoCol3Width
    Else If Key = 'Col4Width' Then
      Result := SInfoCol4Width
    Else
      Result := 'GeneralInfo.' + Key;
  End;

Begin
  If (FWidthsGrid = nil) Or (MainForm = nil) Or (MainForm.StatusBar = nil) Then
    exit;
  Row := 1;
  For i := Low(WindowSizesInfoKeys) To High(WindowSizesInfoKeys) Do
  Begin
    FWidthsGrid.Cells[0, Row] := InfoCaption(WindowSizesInfoKeys[i]);
    FWidthsGrid.Cells[1, Row] := IntToStr(
      Ini.ReadInteger('GeneralInfo', WindowSizesInfoKeys[i],
      WindowSizesInfoDefaults[i]));
    Inc(Row);
  End;
  PanelCount := MainForm.StatusBar.Panels.Count;
  For i := 0 To PanelCount - 1 Do
  Begin
    FWidthsGrid.Cells[0, Row] := Format(SStatusBarPanel, [i]);
    W := Ini.ReadInteger('StatusBarPanels', IntToStr(i),
      MainForm.StatusBar.Panels[i].Width);
    If W <= 0 Then
      W := MainForm.StatusBar.Panels[i].Width;
    FWidthsGrid.Cells[1, Row] := IntToStr(W);
    Inc(Row);
  End;
End;

Procedure TOptionsForm.SaveWindowSizes;
Var
  i, Row, PanelCount, W, Def: Integer;
Begin
  If FWidthsGrid <> nil Then
    FWidthsGrid.EditorMode := False;
  If (FWidthsGrid = nil) Or (MainForm = nil) Or (MainForm.StatusBar = nil) Then
    exit;
  Row := 1;
  For i := Low(WindowSizesInfoKeys) To High(WindowSizesInfoKeys) Do
  Begin
    Def := Ini.ReadInteger('GeneralInfo', WindowSizesInfoKeys[i],
      WindowSizesInfoDefaults[i]);
    W := StrToIntDef(Trim(FWidthsGrid.Cells[1, Row]), Def);
    If (WindowSizesInfoKeys[i] = 'ColGap') Or
      (WindowSizesInfoKeys[i] = 'Col1Left') Then
    Begin
      If W < 0 Then W := 0;
    End
    Else If W < 20 Then
      W := 20;
    Ini.WriteInteger('GeneralInfo', WindowSizesInfoKeys[i], W);
    Inc(Row);
  End;

  PanelCount := MainForm.StatusBar.Panels.Count;
  Ini.WriteInteger('StatusBarPanels', 'ScreenWidth', Screen.Width);
  For i := 0 To PanelCount - 1 Do
  Begin
    Def := MainForm.StatusBar.Panels[i].Width;
    W := StrToIntDef(Trim(FWidthsGrid.Cells[1, Row]), Def);
    If W < 1 Then
      W := Def;
    If W < 1 Then
      W := 20;
    Ini.WriteInteger('StatusBarPanels', IntToStr(i), W);
    MainForm.StatusBar.Panels[i].Width := W;
    Inc(Row);
  End;
  MainForm.ReloadInfoLayout;
  MainForm.StatusBar.Update;
End;

Procedure TOptionsForm.btEditTorrentMenuClick(Sender: TObject);
Begin
  With TUserMenuForm.Create(Self) Do
  Try
    Section := 'UserTorrentMenu';
    ShowParamType := False;
    Caption := SUserTorrentMenuTitle;
    ShowModal;
  Finally
    Free;
  End;
End;

Procedure TOptionsForm.btEditFileMenuClick(Sender: TObject);
Begin
  With TUserMenuForm.Create(Self) Do
  Try
    Section := 'UserFileMenu';
    ShowParamType := True;
    Caption := SUserFileMenuTitle;
    ShowModal;
  Finally
    Free;
  End;
End;

Function TOptionsForm.TestWhisparr(Out Version, Error: String): Boolean;
Var
  Config: TWhisparrConfig;
Begin
  Version := '';
  Config.Url := Trim(edWhisparrUrl.Text);
  Config.ApiKey := Trim(edWhisparrApiKey.Text);
  Config.VerifySsl := cbWhisparrVerifySsl.Checked;
  If Not WhisparrConfigured(Config) Then
  Begin
    Error := SWhisparrSettingsRequired;
    Result := False;
    exit;
  End;
  AppBusy;
  Try
    Result := WhisparrTestConnection(Config, Version, Error);
  Finally
    AppNormal;
  End;
End;

Procedure TOptionsForm.btWhisparrTestClick(Sender: TObject);
Var
  Version, Error: String;
Begin
  If TestWhisparr(Version, Error) Then
  Begin
    FWhisparrDirty := False;
    If Version = '' Then
      Version := SWhisparrUnknownVersion;
    MessageDlg('', Format(SWhisparrConnectionOk, [Version]),
      mtInformation, [mbOK], 0);
  End
  Else
  Begin
    FWhisparrDirty := True;
    MessageDlg('', Format(SWhisparrConnectionFailed, [Error]),
      mtError, [mbOK], 0);
  End;
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
  FWhisparrDirty := False;
End;

Procedure TOptionsForm.OKButtonClick(Sender: TObject);
Var
  s, Version, Error: String;
  bd, idx: Integer;
  restart: Boolean;
  {$ifdef mswindows}
  reg: TRegistry;
  {$endif mswindows}
Begin
  If (Trim(edWhisparrUrl.Text) <> '') Or (Trim(edWhisparrApiKey.Text) <> '') Then
  Begin
    If (Trim(edWhisparrUrl.Text) = '') Or (Trim(edWhisparrApiKey.Text) = '') Then
    Begin
      MessageDlg('', SWhisparrSettingsRequired, mtError, [mbOK], 0);
      exit;
    End;
    If FWhisparrDirty Then
      If Not TestWhisparr(Version, Error) Then
        If MessageDlg('', Format(SWhisparrSaveAnyway, [Error]),
          mtWarning, [mbYes, mbNo], 0) <> mrYes Then
          exit;
  End;
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

  Ini.WriteBool('Interface', 'AskScriptParams', cbAskScriptParams.Checked);

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

  SaveWindowSizes;

  If restart Then
    MessageDlg(sRestartRequired, mtInformation, [mbOK], 0);
  ModalResult := mrOk;
End;

Initialization
  {$I options.lrs}

End.