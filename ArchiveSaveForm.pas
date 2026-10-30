
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

Unit ArchiveSaveForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, Variants, BaseForm;

Resourcestring
  SArchiveNameRequired = 'Please enter an archive name.';
  SArchiveNameInvalid = 'The archive name contains characters that are not allowed.';
  SNoTorrentsSelected = 'There are no selected torrents to archive.';
  SConfirmDeleteData = 'Delete the downloaded files of the %d selected torrent(s) after archiving?';
  SArchiveSaved = 'Archived %d torrent(s) to archive "%s".';
  SArchivePartial = 'Archived %d torrent(s) to archive "%s", but some entries failed:';

Type

  { TArchiveSaveForm }

  TArchiveSaveForm = Class(TBaseForm)
    Buttons: TButtonPanel;
    cbArchive: TComboBox;
    cbDeleteLocalData: TCheckBox;
    cbOverwrite: TCheckBox;
    cbUseWhisparrSeries: TCheckBox;
    txArchive: TLabel;
    txInfo: TLabel;
    Procedure btOKClick(Sender: TObject);
    Procedure cbUseWhisparrSeriesClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  Private
    FWhisparrName: String;
    Function GetArchiveName: String;
    Function SelectedCount: Integer;
    Function SuggestArchiveName: Boolean;
    Procedure UpdateArchiveNameLock;
  Public
    SelectedIds: Variant;
    Procedure SetArchiveName(Const AName: String);
    Property ArchiveName: String read GetArchiveName;
  End;

Implementation

Uses fpjson, utils, rpc, ArchiveCommon, WhisparrClient, main;

  { TArchiveSaveForm }

Procedure TArchiveSaveForm.FormCreate(Sender: TObject);
Begin
  bidiMode := GetBiDi();
  LoadArchiveNames(cbArchive);
  Buttons.OKButton.ModalResult := mrNone;
  Buttons.OKButton.OnClick := @btOKClick;
End;

Procedure TArchiveSaveForm.FormShow(Sender: TObject);
Var
  Config: TWhisparrConfig;
Begin
  LoadArchiveNamesFromServer(cbArchive);
  FWhisparrName := '';
  Config := MainForm.GetWhisparrConfig;
  cbUseWhisparrSeries.Visible := WhisparrConfigured(Config);
  cbUseWhisparrSeries.Enabled := False;
  cbUseWhisparrSeries.Checked := False;
  If cbUseWhisparrSeries.Visible And SuggestArchiveName Then
  Begin
    cbUseWhisparrSeries.Enabled := True;
    cbUseWhisparrSeries.Checked := True;
  End;
  UpdateArchiveNameLock;
End;

Procedure TArchiveSaveForm.UpdateArchiveNameLock;
Begin
  cbArchive.Enabled := Not cbUseWhisparrSeries.Checked;
End;

Procedure TArchiveSaveForm.cbUseWhisparrSeriesClick(Sender: TObject);
Begin
  If cbUseWhisparrSeries.Checked Then
  Begin
    If FWhisparrName = '' Then
      cbUseWhisparrSeries.Checked := False
    Else
      SetArchiveName(FWhisparrName);
  End;
  UpdateArchiveNameLock;
End;

Function TArchiveSaveForm.SuggestArchiveName: Boolean;
Var
  Config: TWhisparrConfig;
  Args: TJSONObject;
  List: TJSONData;
  Item: TJSONObject;
  Names: TStringList;
  NewArchiveName, Error, s: String;
  i: Integer;
Begin
  Result := False;
  If VarIsEmpty(SelectedIds) Or VarIsNull(SelectedIds) Then
    exit;
  Config := MainForm.GetWhisparrConfig;
  If Not WhisparrConfigured(Config) Then
    exit;
  AppBusy;
  Try
    Args := RpcObj.RequestInfos(SelectedIds, ['name']);
    If Args = nil Then
      exit;
    Try
      List := Args.Find('torrents');
      If Not (List Is TJSONArray) Then
        exit;
      Names := TStringList.Create;
      Try
        For i := 0 To TJSONArray(List).Count - 1 Do
        Begin
          Item := TJSONArray(List).Objects[i];
          If Item = nil Then
            continue;
          s := '';
          If Item.Find('name') <> nil Then
            s := Item.Find('name').AsString;
          If s <> '' Then
            Names.Add(s);
        End;
        If WhisparrSuggestArchiveName(Config, Names, NewArchiveName, Error) Then
        Begin
          FWhisparrName := NewArchiveName;
          SetArchiveName(NewArchiveName);
          Result := True;
        End;
      Finally
        Names.Free;
      End;
    Finally
      Args.Free;
    End;
  Finally
    AppNormal;
  End;
End;

Procedure TArchiveSaveForm.SetArchiveName(Const AName: String);
Begin
  If AName = '' Then
    exit;
  If cbArchive.Items.IndexOf(AName) < 0 Then
    cbArchive.Items.Insert(0, AName);
  cbArchive.Text := AName;
End;

Function TArchiveSaveForm.GetArchiveName: String;
Begin
  Result := Trim(cbArchive.Text);
End;

Function TArchiveSaveForm.SelectedCount: Integer;
Begin
  Result := 0;
  If VarIsEmpty(SelectedIds) Or VarIsNull(SelectedIds) Then
    exit;
  If VarIsArray(SelectedIds) Then
    Result := VarArrayHighBound(SelectedIds, 1) - VarArrayLowBound(SelectedIds, 1) + 1
  Else
    Result := 1;
End;

Procedure TArchiveSaveForm.btOKClick(Sender: TObject);
Var
  AName, s: String;
  Args, Res: TJSONObject;
  Errors: TJSONData;
  ErrObj: TJSONObject;
  i, Saved: Integer;
Begin
  AName := Trim(cbArchive.Text);
  If AName = '' Then
  Begin
    MessageDlg('', SArchiveNameRequired, mtError, [mbOK], 0);
    cbArchive.SetFocus;
    exit;
  End;
  If Not IsValidArchiveName(AName) Then
  Begin
    MessageDlg('', SArchiveNameInvalid, mtError, [mbOK], 0);
    cbArchive.SetFocus;
    exit;
  End;
  If SelectedCount = 0 Then
  Begin
    MessageDlg('', SNoTorrentsSelected, mtError, [mbOK], 0);
    exit;
  End;
  If cbDeleteLocalData.Checked Then
    If MessageDlg('', Format(SConfirmDeleteData, [SelectedCount]), mtWarning,
      mbYesNo, 0, mbNo) <> mrYes Then
      exit;

  AppBusy;
  Try
    Args := TJSONObject.Create;
    Args.Add('action', 'save');
    Args.Add('archive', UTF8Decode(AName));
    Args.Add('ids', TorrentIdsToJSON(SelectedIds));
    Args.Add('delete_local_data', cbDeleteLocalData.Checked);
    Args.Add('overwrite', cbOverwrite.Checked);
    Res := ArchiveRequest(Args, 300000);
  Finally
    AppNormal;
  End;

  If Res = nil Then
  Begin
    MainForm.CheckStatus(False);
    exit;
  End;

  Try
    Saved := JsonInt64(Res, 'saved');
    s := '';
    Errors := Res.Find('errors');
    If (Errors <> nil) And (Errors Is TJSONArray) Then
      For i := 0 To TJSONArray(Errors).Count - 1 Do
      Begin
        ErrObj := TJSONArray(Errors).Objects[i];
        If ErrObj = nil Then
          continue;
        If s <> '' Then
          s := s + LineEnding;
        s := s + Format('#%d: %s', [JsonInt64(ErrObj, 'id'), JsonStr(ErrObj, 'error')]);
      End;
    If s <> '' Then
      MessageDlg('', Format(SArchivePartial, [Saved, AName]) + LineEnding +
        LineEnding + s, mtWarning, [mbOK], 0)
    Else
      MessageDlg('', Format(SArchiveSaved, [Saved, AName]), mtInformation, [mbOK], 0);
  Finally
    Res.Free;
  End;

  RememberArchiveName(AName);
  RpcObj.RefreshNow := RpcObj.RefreshNow + [rtTorrents];
  ModalResult := mrOk;
End;

Initialization
  {$I archivesaveform.lrs}

End.
