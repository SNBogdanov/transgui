
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
*************************************************************************************}

Unit UserMenuForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, BaseForm;

Resourcestring
  SUserMenuCaptionRequired = 'Enter a caption for the menu item.';
  SUserMenuExeRequired = 'Enter the program or command to run.';
  SUserMenuSeparator = '(separator)';
  SUserMenuNoCaption = '(no caption)';
  SUserMenuRestart = 'Restart the application to apply the menu changes.';
  SUserMenuSelectProgram = 'Select program';
  SUserTorrentMenuTitle = 'Torrent menu';
  SUserFileMenuTitle = 'File menu';
  SUserMenuTorrentHint = 'Parameters: %s is the RPC URL, %s is the torrent ID(s). Use - as caption for a separator.';
  SUserMenuFileHint = 'Parameters: %s is the file path(s), %s is the RPC URL, %s is the torrent ID. Type id passes file indices instead of paths. Use - as caption for a separator.';
  SUserMenuSelectItem = 'Select an item to edit or press New item.';
  SUserMenuEntryInfo = 'Use New item to clear the fields. Add appends a new item; Apply applies changes to the selected item.';

Type
  TUserMenuEntry = Record
    Caption: String;
    ExeName: String;
    Params: String;
    ParamType: String;
  End;

  { TUserMenuForm }

  TUserMenuForm = Class(TBaseForm)
    btAdd: TButton;
    btBrowse: TButton;
    btDelete: TButton;
    btDown: TButton;
    btNew: TButton;
    btUpdate: TButton;
    btUp: TButton;
    Buttons: TButtonPanel;
    cbType: TComboBox;
    edCaption: TEdit;
    edExe: TEdit;
    edParams: TEdit;
    lbItems: TListBox;
    OpenDialog1: TOpenDialog;
    txCaption: TLabel;
    txExe: TLabel;
    txHint: TLabel;
    txInfo: TLabel;
    txParams: TLabel;
    txType: TLabel;
    Procedure btAddClick(Sender: TObject);
    Procedure btBrowseClick(Sender: TObject);
    Procedure btDeleteClick(Sender: TObject);
    Procedure btDownClick(Sender: TObject);
    Procedure btNewClick(Sender: TObject);
    Procedure btOKClick(Sender: TObject);
    Procedure btUpClick(Sender: TObject);
    Procedure btUpdateClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure lbItemsSelectionChange(Sender: TObject; User: Boolean);
  Private
    FEntries: Array Of TUserMenuEntry;
    FUpdating: Boolean;
    Procedure AppendEntry;
    Function CommitFields: Boolean;
    Procedure LoadEntries;
    Procedure LoadSelected;
    Procedure RefreshList;
    Procedure SaveEntries;
    Procedure StoreFields(Index: Integer);
    Procedure UpdateButtons;
    Function ValidateEntries: Boolean;
    Function ValidateFields: Boolean;
  Public
    Section: String;
    ShowParamType: Boolean;
  End;

Implementation

Uses main;

  { TUserMenuForm }

Procedure TUserMenuForm.FormCreate(Sender: TObject);
Begin
  bidiMode := GetBiDi();
  cbType.Items.Add('file');
  cbType.Items.Add('id');
  cbType.ItemIndex := 0;
  Buttons.OKButton.ModalResult := mrNone;
  Buttons.OKButton.OnClick := @btOKClick;
  OpenDialog1.Filter := 'Programs (*.exe)|*.exe|All files (*.*)|*.*';
End;

Procedure TUserMenuForm.FormShow(Sender: TObject);
Begin
  txType.Visible := ShowParamType;
  cbType.Visible := ShowParamType;
  txInfo.Caption := SUserMenuEntryInfo;
  If Section = 'UserTorrentMenu' Then
    txHint.Caption := SUserMenuTorrentHint
  Else
    txHint.Caption := SUserMenuFileHint;
  LoadEntries;
  RefreshList;
  If Length(FEntries) > 0 Then
    lbItems.ItemIndex := 0
  Else
    btNewClick(nil);
End;

Procedure TUserMenuForm.LoadEntries;
Var
  i: Integer;
Begin
  SetLength(FEntries, 0);
  i := 1;
  While Ini.ValueExists(Section, 'Caption' + IntToStr(i)) Do
  Begin
    SetLength(FEntries, Length(FEntries) + 1);
    With FEntries[High(FEntries)] Do
    Begin
      Caption := Ini.ReadString(Section, 'Caption' + IntToStr(i), '');
      ExeName := Ini.ReadString(Section, 'ExeName' + IntToStr(i), '');
      Params := Ini.ReadString(Section, 'Params' + IntToStr(i), '');
      ParamType := Ini.ReadString(Section, 'ParamType' + IntToStr(i), 'file');
    End;
    Inc(i);
  End;
  If Section = 'UserTorrentMenu' Then
    For i := 0 To High(FEntries) Do
      FEntries[i].ParamType := 'id';
End;

Procedure TUserMenuForm.RefreshList;
Var
  i, OldIndex: Integer;
  AName: String;
Begin
  OldIndex := lbItems.ItemIndex;
  FUpdating := True;
  Try
    lbItems.Items.BeginUpdate;
    Try
      lbItems.Items.Clear;
      For i := 0 To High(FEntries) Do
      Begin
        AName := FEntries[i].Caption;
        If AName = '-' Then
          AName := SUserMenuSeparator
        Else If AName = '' Then
          AName := SUserMenuNoCaption;
        lbItems.Items.Add(AName);
      End;
    Finally
      lbItems.Items.EndUpdate;
    End;
    If Length(FEntries) = 0 Then
      lbItems.ItemIndex := -1
    Else If OldIndex < 0 Then
      lbItems.ItemIndex := 0
    Else If OldIndex >= Length(FEntries) Then
      lbItems.ItemIndex := High(FEntries)
    Else
      lbItems.ItemIndex := OldIndex;
  Finally
    FUpdating := False;
  End;
  LoadSelected;
End;

Procedure TUserMenuForm.LoadSelected;
Var
  i: Integer;
Begin
  i := lbItems.ItemIndex;
  FUpdating := True;
  Try
    If (i >= 0) And (i < Length(FEntries)) Then
    Begin
      edCaption.Text := FEntries[i].Caption;
      edExe.Text := FEntries[i].ExeName;
      edParams.Text := FEntries[i].Params;
      If ShowParamType Then
        cbType.ItemIndex := cbType.Items.IndexOf(FEntries[i].ParamType)
      Else
        cbType.ItemIndex := 0;
    End
    Else
    Begin
      edCaption.Text := '';
      edExe.Text := '';
      edParams.Text := '';
      cbType.ItemIndex := 0;
    End;
  Finally
    FUpdating := False;
  End;
  UpdateButtons;
End;

Procedure TUserMenuForm.UpdateButtons;
Var
  i: Integer;
Begin
  i := lbItems.ItemIndex;
  btUpdate.Enabled := (i >= 0) And (i < Length(FEntries));
  btDelete.Enabled := btUpdate.Enabled;
  btUp.Enabled := i > 0;
  btDown.Enabled := (i >= 0) And (i < High(FEntries));
End;

Function TUserMenuForm.ValidateFields: Boolean;
Var
  AName: String;
Begin
  Result := False;
  AName := Trim(edCaption.Text);
  If AName = '' Then
  Begin
    MessageDlg('', SUserMenuCaptionRequired, mtError, [mbOK], 0);
    edCaption.SetFocus;
    exit;
  End;
  If (AName <> '-') And (Trim(edExe.Text) = '') Then
  Begin
    MessageDlg('', SUserMenuExeRequired, mtError, [mbOK], 0);
    edExe.SetFocus;
    exit;
  End;
  Result := True;
End;

Procedure TUserMenuForm.StoreFields(Index: Integer);
Begin
  With FEntries[Index] Do
  Begin
    Caption := Trim(edCaption.Text);
    ExeName := Trim(edExe.Text);
    Params := edParams.Text;
    If ShowParamType Then
    Begin
      If cbType.ItemIndex >= 0 Then
        ParamType := cbType.Text
      Else
        ParamType := 'file';
    End
    Else
      ParamType := 'id';
  End;
End;

Procedure TUserMenuForm.AppendEntry;
Begin
  SetLength(FEntries, Length(FEntries) + 1);
  StoreFields(High(FEntries));
End;

Function TUserMenuForm.CommitFields: Boolean;
Var
  i: Integer;
Begin
  Result := False;
  i := lbItems.ItemIndex;
  If (i >= 0) And (i < Length(FEntries)) Then
  Begin
    If Not ValidateFields Then
      exit;
    StoreFields(i);
  End
  Else If (Trim(edCaption.Text) <> '') Or (Trim(edExe.Text) <> '') Then
  Begin
    If Not ValidateFields Then
      exit;
    AppendEntry;
  End;
  Result := True;
End;

Function TUserMenuForm.ValidateEntries: Boolean;
Var
  i: Integer;
Begin
  Result := False;
  For i := 0 To High(FEntries) Do
    With FEntries[i] Do
    Begin
      If Caption = '' Then
      Begin
        MessageDlg('', SUserMenuCaptionRequired, mtError, [mbOK], 0);
        lbItems.ItemIndex := i;
        edCaption.SetFocus;
        exit;
      End;
      If (Caption <> '-') And (ExeName = '') Then
      Begin
        MessageDlg('', SUserMenuExeRequired, mtError, [mbOK], 0);
        lbItems.ItemIndex := i;
        edExe.SetFocus;
        exit;
      End;
    End;
  Result := True;
End;

Procedure TUserMenuForm.SaveEntries;
Var
  i: Integer;
  AName: String;
Begin
  Ini.EraseSection(Section);
  For i := 0 To High(FEntries) Do
  Begin
    Ini.WriteString(Section, 'Caption' + IntToStr(i + 1), FEntries[i].Caption);
    Ini.WriteString(Section, 'ExeName' + IntToStr(i + 1), FEntries[i].ExeName);
    Ini.WriteString(Section, 'Params' + IntToStr(i + 1), FEntries[i].Params);
    If Section = 'UserFileMenu' Then
    Begin
      AName := FEntries[i].ParamType;
      If AName = '' Then
        AName := 'file';
      Ini.WriteString(Section, 'ParamType' + IntToStr(i + 1), AName);
    End;
  End;
  Ini.UpdateFile;
End;

Procedure TUserMenuForm.btNewClick(Sender: TObject);
Begin
  lbItems.ItemIndex := -1;
  LoadSelected;
  edCaption.SetFocus;
End;

Procedure TUserMenuForm.btAddClick(Sender: TObject);
Begin
  If Not ValidateFields Then
    exit;
  AppendEntry;
  RefreshList;
  lbItems.ItemIndex := High(FEntries);
  LoadSelected;
End;

Procedure TUserMenuForm.btUpdateClick(Sender: TObject);
Var
  i: Integer;
Begin
  i := lbItems.ItemIndex;
  If (i < 0) Or (i >= Length(FEntries)) Then
  Begin
    MessageDlg('', SUserMenuSelectItem, mtInformation, [mbOK], 0);
    exit;
  End;
  If Not ValidateFields Then
    exit;
  StoreFields(i);
  RefreshList;
End;

Procedure TUserMenuForm.btDeleteClick(Sender: TObject);
Var
  i, j: Integer;
Begin
  i := lbItems.ItemIndex;
  If (i < 0) Or (i >= Length(FEntries)) Then
    exit;
  For j := i To High(FEntries) - 1 Do
    FEntries[j] := FEntries[j + 1];
  SetLength(FEntries, Length(FEntries) - 1);
  RefreshList;
End;

Procedure TUserMenuForm.btUpClick(Sender: TObject);
Var
  i: Integer;
  E: TUserMenuEntry;
Begin
  i := lbItems.ItemIndex;
  If i <= 0 Then
    exit;
  E := FEntries[i - 1];
  FEntries[i - 1] := FEntries[i];
  FEntries[i] := E;
  RefreshList;
  lbItems.ItemIndex := i - 1;
  LoadSelected;
End;

Procedure TUserMenuForm.btDownClick(Sender: TObject);
Var
  i: Integer;
  E: TUserMenuEntry;
Begin
  i := lbItems.ItemIndex;
  If (i < 0) Or (i >= High(FEntries)) Then
    exit;
  E := FEntries[i + 1];
  FEntries[i + 1] := FEntries[i];
  FEntries[i] := E;
  RefreshList;
  lbItems.ItemIndex := i + 1;
  LoadSelected;
End;

Procedure TUserMenuForm.btBrowseClick(Sender: TObject);
Var
  AName: String;
Begin
  AName := Trim(edExe.Text);
  If (AName <> '') And DirectoryExists(ExtractFilePath(AName)) Then
    OpenDialog1.InitialDir := ExtractFilePath(AName);
  OpenDialog1.Title := SUserMenuSelectProgram;
  If OpenDialog1.Execute Then
    edExe.Text := OpenDialog1.FileName;
End;

Procedure TUserMenuForm.btOKClick(Sender: TObject);
Begin
  If Not CommitFields Then
    exit;
  If Not ValidateEntries Then
    exit;
  SaveEntries;
  MessageDlg('', SUserMenuRestart, mtInformation, [mbOK], 0);
  ModalResult := mrOk;
End;

Procedure TUserMenuForm.lbItemsSelectionChange(Sender: TObject; User: Boolean);
Begin
  If Not FUpdating Then
    LoadSelected;
End;

Initialization
  {$I usermenuform.lrs}

End.
