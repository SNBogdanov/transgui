
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

Unit ArchiveForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Types, FileUtil, LResources, Forms, Controls, Graphics,
  LCLType, Dialogs, StdCtrls, ExtCtrls, Variants, fpjson, Grids, BaseForm, VarGrid;

Resourcestring
  SArchiveNameRequiredMsg = 'Please enter an archive name.';
  SArchiveNameInvalidMsg = 'The archive name contains characters that are not allowed.';
  SArchiveNoEntries = 'The archive does not contain any entries.';
  SArchiveVerifyRestore = 'Verify local data after restore';
  SArchiveRestored = 'Restored %d torrent(s).';
  SArchiveRestoredPartial = 'Restored %d of %d torrent(s).';
  SArchiveMoveTarget = 'Target archive name:';
  SArchiveMoveSame = 'The source and target archives must be different.';
  SArchiveMoveOverwrite = 'Replace existing entries in the target archive?';
  SArchiveMoved = 'Moved %d entr(ies) to archive "%s".';
  SArchiveDownloadSaved = 'Downloaded %d entr(ies) to:';
  SArchiveImportOverwrite = 'Replace existing entries in the destination archive?';
  SArchiveImported = 'Imported %d entr(ies), skipped %d.';
  SArchiveImportDialogTitle = 'Import archive bundle';
  SArchiveBundleTooLarge = 'The selected bundle is too large to upload.';
  SArchiveDownloadDialogTitle = 'Download archive bundle';
  SArchiveMoveTitle = 'Move archive entries';
  SArchiveEntryCount = 'Entries: %d';
  SArchiveMetaTorrent = 'torrent';
  SArchiveMetaResume = 'resume';
  SArchiveMetaMagnet = 'magnet';
  SArchiveMetaNone = 'none';
  SArchiveLocalDeleted = 'deleted';
  SArchiveLocalPresent = 'present';
  SArchiveBundleFile = 'Bundle: %s';
  SArchiveNewTitle = 'New archive';
  SArchiveNewPrompt = 'New archive name:';
  SArchiveCreated = 'Archive "%s" was created.';
  SArchiveAlreadyExists = 'Archive "%s" already exists.';
  SArchiveDeleteConfirm = 'Delete archive "%s"? Only empty archives can be deleted.';
  SArchiveDeleteNotEmpty = 'The archive is not empty. Remove or move its entries first.';
  SArchiveDeleted = 'Archive "%s" was deleted.';
  SArchiveDeleteNotDeleted = 'Archive "%s" was not deleted.';
  SArchiveAllArchives = 'All archives';
  SArchiveSingleArchiveRequired = 'Select entries from a single archive for this operation.';
  SArchivePage = 'Page %d of %d';
  SArchivePrevPage = 'Previous';
  SArchiveNextPage = 'Next';
  SArchiveViewSingle = 'Select a single entry to view.';
  SArchiveDeleteEntryConfirm = 'Delete the selected torrent(s) from the archive? Downloaded data will not be deleted.';
  SArchiveDeleteEntryDone = 'Deleted %d entry(ies) from the archive.';
  SArchiveDeleteEntryPartial = 'Deleted %d of %d entry(ies).';
  SArchiveDeleteEntryFailed = 'The archive entry could not be deleted.';

Type

  { TArchiveForm }

  TArchiveForm = Class(TBaseForm)
    btClose: TButton;
    btDeleteArchive: TButton;
    btDeleteEntry: TButton;
    btDownload: TButton;
    btImport: TButton;
    btMove: TButton;
    btNewArchive: TButton;
    btNextPage: TButton;
    btPrevPage: TButton;
    btRefresh: TButton;
    btRestore: TButton;
    btView: TButton;
    cbArchive: TComboBox;
    cbVerifyRestore: TCheckBox;
    lvEntries: TVarGrid;
    OpenDialog1: TOpenDialog;
    pnActions: TPanel;
    pnTop: TPanel;
    SaveDialog1: TSaveDialog;
    txArchive: TLabel;
    txCount: TLabel;
    txPage: TLabel;
    Procedure btCloseClick(Sender: TObject);
    Procedure btDeleteArchiveClick(Sender: TObject);
    Procedure btDeleteEntryClick(Sender: TObject);
    Procedure btDownloadClick(Sender: TObject);
    Procedure btImportClick(Sender: TObject);
    Procedure btMoveClick(Sender: TObject);
    Procedure btNewArchiveClick(Sender: TObject);
    Procedure btNextPageClick(Sender: TObject);
    Procedure btPrevPageClick(Sender: TObject);
    Procedure btRefreshClick(Sender: TObject);
    Procedure btRestoreClick(Sender: TObject);
    Procedure btViewClick(Sender: TObject);
    Procedure cbArchiveChange(Sender: TObject);
    Procedure cbArchiveSelect(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure lvEntriesClick(Sender: TObject);
    Procedure lvEntriesDblClick(Sender: TObject);
    Procedure lvEntriesKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
  Private
    FEntries: TJSONArray;
    FOffset: Integer;
    FTotal: Integer;
    FPageSize: Integer;
    FSortBy: String;
    FSortAsc: Boolean;
    FSortCol: Integer;
    Procedure ClearEntries;
    Function EnsureArchiveName: Boolean;
    Function EntryArchive(Obj: TJSONObject): String;
    Procedure FillEntries;
    Function GetSelectedEntries: TStringList;
    Function IsAllArchivesMode: Boolean;
    Procedure LoadSortSettings;
    Procedure lvEntriesHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    Function SortKeyForColumn(ADataCol: Integer): String;
    Function SortColumnForKey(Const AKey: String): Integer;
    Function UniqueArchives(Entries: TStringList): TStringList;
    Procedure RefreshArchiveNames;
    Procedure RememberName(Const AName: String);
    Procedure UpdateButtons;
    Procedure UpdatePaging;
  Public
    Procedure RefreshList;
    Procedure SetArchiveName(Const AName: String);
    Destructor Destroy; Override;
  End;

Implementation

Uses synacode, utils, rpc, AddTorrent, ArchiveCommon, ArchiveViewForm, ArchiveRestoreForm, main;

Const
  DefaultArchivePageSize = 100;
  ArchiveSortByKey = 'SortBy';
  ArchiveSortAscKey = 'SortAsc';

  { TArchiveForm }

Procedure TArchiveForm.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  bidiMode := GetBiDi();
  Width := Ini.ReadInteger(ArchiveSection, 'WindowWidth', Width);
  Height := Ini.ReadInteger(ArchiveSection, 'WindowHeight', Height);
  FPageSize := Ini.ReadInteger(ArchiveSection, 'PageSize', DefaultArchivePageSize);
  If FPageSize < 1 Then
    FPageSize := DefaultArchivePageSize;
  For i := 0 To lvEntries.Columns.Count - 1 Do
    lvEntries.Columns[i].Width := Ini.ReadInteger(ArchiveSection,
      Format('ColWidth%d', [i]), lvEntries.Columns[i].Width);
  lvEntries.SortColumn := -1;
  lvEntries.OnHeaderClick := @lvEntriesHeaderClick;
  LoadSortSettings;
  cbVerifyRestore.Caption := SArchiveVerifyRestore;
  cbVerifyRestore.Checked := Ini.ReadBool(ArchiveSection, ArchiveVerifyKey, False);
  LoadArchiveNames(cbArchive);
  UpdateButtons;
End;

Function TArchiveForm.SortKeyForColumn(ADataCol: Integer): String;
Begin
  Case ADataCol Of
    1: Result := 'name';
    2: Result := 'size';
    3: Result := 'added_date';
    4: Result := 'archived_date';
    5: Result := 'folder';
    Else
      Result := '';
  End;
End;

Function TArchiveForm.SortColumnForKey(Const AKey: String): Integer;
Begin
  If AKey = 'name' Then
    Result := 1
  Else If AKey = 'size' Then
    Result := 2
  Else If AKey = 'added_date' Then
    Result := 3
  Else If AKey = 'archived_date' Then
    Result := 4
  Else If AKey = 'folder' Then
    Result := 5
  Else
    Result := -1;
End;

Procedure TArchiveForm.LoadSortSettings;
Begin
  FSortBy := Ini.ReadString(ArchiveServerSection, ArchiveSortByKey, '');
  FSortCol := SortColumnForKey(FSortBy);
  If FSortCol < 0 Then
    FSortBy := '';
  FSortAsc := Ini.ReadBool(ArchiveServerSection, ArchiveSortAscKey, True);
  lvEntries.SortArrowColumn := FSortCol;
  lvEntries.SortArrowAscending := FSortAsc;
End;

Procedure TArchiveForm.lvEntriesHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
Var
  ADataCol: Integer;
  Key: String;
Begin
  If Not IsColumn Then
    exit;
  ADataCol := lvEntries.ColToDataCol(Index);
  Key := SortKeyForColumn(ADataCol);
  If Key = '' Then
    exit;
  If (FSortBy = Key) And (FSortCol = ADataCol) Then
  Begin
    If FSortAsc Then
      FSortAsc := False
    Else
    Begin
      FSortBy := '';
      FSortCol := -1;
      FSortAsc := True;
    End;
  End
  Else
  Begin
    FSortBy := Key;
    FSortCol := ADataCol;
    FSortAsc := True;
  End;
  Ini.WriteString(ArchiveServerSection, ArchiveSortByKey, FSortBy);
  Ini.WriteBool(ArchiveServerSection, ArchiveSortAscKey, FSortAsc);
  lvEntries.SortArrowColumn := FSortCol;
  lvEntries.SortArrowAscending := FSortAsc;
  FOffset := 0;
  lvEntries.Invalidate;
  RefreshList;
End;

Procedure TArchiveForm.FormShow(Sender: TObject);
Begin
  FOffset := 0;
  FTotal := 0;
  RefreshArchiveNames;
  If Trim(cbArchive.Text) <> '' Then
    RefreshList;
End;

Procedure TArchiveForm.RefreshArchiveNames;
Begin
  If Not LoadArchiveNamesFromServer(cbArchive) Then
    exit;
  If cbArchive.Items.IndexOf(SArchiveAllArchives) < 0 Then
    cbArchive.Items.Insert(0, SArchiveAllArchives);
  If (cbArchive.Items.Count > 0) And
    ((Trim(cbArchive.Text) = '') Or
     ((cbArchive.Items.IndexOf(Trim(cbArchive.Text)) < 0) And
      Not IsAllArchivesMode)) Then
    cbArchive.Text := cbArchive.Items[0];
  UpdateButtons;
End;

Destructor TArchiveForm.Destroy;
Var
  i: Integer;
Begin
  Ini.WriteBool(ArchiveSection, ArchiveVerifyKey, cbVerifyRestore.Checked);
  If WindowState <> wsMinimized Then
  Begin
    Ini.WriteInteger(ArchiveSection, 'WindowWidth', Width);
    Ini.WriteInteger(ArchiveSection, 'WindowHeight', Height);
    For i := 0 To lvEntries.Columns.Count - 1 Do
      Ini.WriteInteger(ArchiveSection, Format('ColWidth%d', [i]),
        lvEntries.Columns[i].Width);
  End;
  FEntries.Free;
  Inherited Destroy;
End;

Procedure TArchiveForm.SetArchiveName(Const AName: String);
Begin
  If AName <> '' Then
  Begin
    FOffset := 0;
    cbArchive.Text := AName;
  End;
End;

Procedure TArchiveForm.RememberName(Const AName: String);
Begin
  RememberArchiveName(AName);
  If cbArchive.Items.IndexOf(AName) < 0 Then
    cbArchive.Items.Insert(0, AName);
  cbArchive.Text := AName;
End;

Procedure TArchiveForm.UpdateButtons;
Var
  HasEntries, AllArchives: Boolean;
Begin
  HasEntries := (FEntries <> nil) And (FEntries.Count > 0);
  AllArchives := IsAllArchivesMode;
  btRestore.Enabled := HasEntries;
  btMove.Enabled := HasEntries;
  btView.Enabled := HasEntries;
  btDeleteEntry.Enabled := HasEntries;
  btDownload.Enabled := HasEntries;
  btImport.Enabled := Not AllArchives;
  btNewArchive.Enabled := True;
  btDeleteArchive.Enabled := (Not AllArchives) And (Trim(cbArchive.Text) <> '');
  btPrevPage.Enabled := FOffset > 0;
  btNextPage.Enabled := (FEntries <> nil) And
    (FOffset + FEntries.Count < FTotal);
End;

Procedure TArchiveForm.UpdatePaging;
Var
  Page, Pages: Integer;
Begin
  Pages := (FTotal + FPageSize - 1) Div FPageSize;
  If Pages < 1 Then
    Pages := 1;
  Page := FOffset Div FPageSize + 1;
  If Page > Pages Then
    Page := Pages;
  txPage.Caption := Format(SArchivePage, [Page, Pages]);
  UpdateButtons;
End;

Procedure TArchiveForm.ClearEntries;
Begin
  FreeAndNil(FEntries);
  FOffset := 0;
  FTotal := 0;
  lvEntries.Items.Clear;
  lvEntries.Items.RowCnt := 0;
  txCount.Caption := '';
  UpdatePaging;
End;

Function TArchiveForm.EnsureArchiveName: Boolean;
Var
  s: String;
Begin
  Result := False;
  s := Trim(cbArchive.Text);
  If s = '' Then
  Begin
    MessageDlg('', SArchiveNameRequiredMsg, mtError, [mbOK], 0);
    cbArchive.SetFocus;
    exit;
  End;
  If Not IsValidArchiveName(s) Then
  Begin
    MessageDlg('', SArchiveNameInvalidMsg, mtError, [mbOK], 0);
    cbArchive.SetFocus;
    exit;
  End;
  Result := True;
End;

Function TArchiveForm.IsAllArchivesMode: Boolean;
Var
  s: String;
Begin
  s := Trim(cbArchive.Text);
  Result := (s = 'all') Or (s = SArchiveAllArchives);
End;

Function TArchiveForm.EntryArchive(Obj: TJSONObject): String;
Begin
  Result := JsonStr(Obj, 'archive');
  If (Result = '') And Not IsAllArchivesMode Then
    Result := Trim(cbArchive.Text);
End;

Procedure TArchiveForm.FillEntries;
Var
  i: Integer;
  o: TJSONObject;
  s: String;
Begin
  lvEntries.BeginUpdate;
  Try
    lvEntries.Items.Clear;
    If FEntries = nil Then
    Begin
      lvEntries.Items.RowCnt := 0;
      UpdateButtons;
      exit;
    End;
    lvEntries.Items.RowCnt := FEntries.Count;
    For i := 0 To FEntries.Count - 1 Do
    Begin
      o := FEntries.Objects[i];
      If o = nil Then
        continue;
      lvEntries.Items[0, i] := EntryArchive(o);
      lvEntries.Items[1, i] := JsonStr(o, 'name');
      lvEntries.Items[2, i] := GetHumanSize(JsonInt64(o, 'total_size'));
      lvEntries.Items[3, i] := UnixTimeToText(JsonInt64(o, 'added_date'));
      lvEntries.Items[4, i] := UnixTimeToText(JsonInt64(o, 'archived_date'));
      lvEntries.Items[5, i] := JsonStr(o, 'download_dir');
      lvEntries.Items[6, i] := JsonStrArray(o, 'labels');
      s := '';
      If JsonBool(o, 'has_torrent') Then
        s := SArchiveMetaTorrent;
      If JsonBool(o, 'has_resume') Then
      Begin
        If s <> '' Then
          s := s + ', ';
        s := s + SArchiveMetaResume;
      End;
      If JsonBool(o, 'has_magnet') Then
      Begin
        If s <> '' Then
          s := s + ', ';
        s := s + SArchiveMetaMagnet;
      End;
      If s = '' Then
        s := SArchiveMetaNone;
      lvEntries.Items[7, i] := s;
      If JsonBool(o, 'local_data_deleted') Then
        lvEntries.Items[8, i] := SArchiveLocalDeleted
      Else
        lvEntries.Items[8, i] := SArchiveLocalPresent;
    End;
  Finally
    lvEntries.EndUpdate;
    If FEntries <> nil Then
      txCount.Caption := Format(SArchiveEntryCount, [FTotal])
    Else
      txCount.Caption := '';
  End;
  UpdatePaging;
End;

Function TArchiveForm.GetSelectedEntries: TStringList;
Var
  i: Integer;
  L: TStringList;

  Procedure AddEntry(Index: Integer);
  Var
    o: TJSONObject;
    AName, Hash: String;
  Begin
    If (Index < 0) Or (Index >= FEntries.Count) Then
      exit;
    o := FEntries.Objects[Index];
    If o = nil Then
      exit;
    Hash := JsonStr(o, 'hash_string');
    If Hash = '' Then
      exit;
    AName := EntryArchive(o);
    L.Add(AName + #9 + Hash);
  End;

Begin
  L := TStringList.Create;
  Result := L;
  If FEntries = nil Then
    exit;
  If lvEntries.SelCount = 0 Then
  Begin
    AddEntry(lvEntries.Row);
    exit;
  End;
  For i := 0 To FEntries.Count - 1 Do
    If lvEntries.RowSelected[i] Then
      AddEntry(i);
End;

Function TArchiveForm.UniqueArchives(Entries: TStringList): TStringList;
Var
  i, j: Integer;
  AName: String;
Begin
  Result := TStringList.Create;
  If Entries = nil Then
    exit;
  For i := 0 To Entries.Count - 1 Do
  Begin
    j := Pos(#9, Entries[i]);
    If j = 0 Then
      continue;
    AName := Copy(Entries[i], 1, j - 1);
    If AName = '' Then
      continue;
    If Result.IndexOf(AName) < 0 Then
      Result.Add(AName);
  End;
End;

Procedure TArchiveForm.RefreshList;
Var
  AName: String;
  Args, Res: TJSONObject;
  Entries: TJSONData;
  Failed, Retry: Boolean;
Begin
  If IsAllArchivesMode Then
    AName := 'all'
  Else
  Begin
    If Not EnsureArchiveName Then
      exit;
    AName := Trim(cbArchive.Text);
  End;
  If FTotal <= 0 Then
    FOffset := 0
  Else If FOffset >= FTotal Then
    FOffset := ((FTotal - 1) Div FPageSize) * FPageSize;
  Failed := False;
  Retry := False;
  AppBusy;
  Try
    Args := TJSONObject.Create;
    Args.Add('action', 'list');
    Args.Add('archive', UTF8Decode(AName));
    Args.Add('offset', FOffset);
    Args.Add('limit', FPageSize);
    If FSortBy <> '' Then
    Begin
      Args.Add('sort_by', FSortBy);
      If FSortAsc Then
        Args.Add('sort_order', 'asc')
      Else
        Args.Add('sort_order', 'desc');
    End;
    Res := ArchiveRequest(Args, 60000);
    If Res = nil Then
    Begin
      ClearEntries;
      Failed := True;
    End
    Else
    Try
      FreeAndNil(FEntries);
      FTotal := Integer(JsonInt64(Res, 'total'));
      Entries := Res.Find('entries');
      If (Entries <> nil) And (Entries Is TJSONArray) Then
        FEntries := TJSONArray(Entries.Clone);
      If (FTotal > 0) And (FOffset >= FTotal) Then
      Begin
        FOffset := ((FTotal - 1) Div FPageSize) * FPageSize;
        Retry := True;
      End
      Else
        FillEntries;
    Finally
      Res.Free;
    End;
  Finally
    AppNormal;
  End;
  If Retry Then
  Begin
    RefreshList;
    exit;
  End;
  If Failed Then
  Begin
    MainForm.CheckStatus(False);
    exit;
  End;
  If Not IsAllArchivesMode Then
    RememberName(AName);
End;

Procedure TArchiveForm.btPrevPageClick(Sender: TObject);
Begin
  Dec(FOffset, FPageSize);
  If FOffset < 0 Then
    FOffset := 0;
  RefreshList;
End;

Procedure TArchiveForm.btNextPageClick(Sender: TObject);
Begin
  Inc(FOffset, FPageSize);
  If (FTotal > 0) And (FOffset >= FTotal) Then
    FOffset := ((FTotal - 1) Div FPageSize) * FPageSize;
  RefreshList;
End;

Procedure TArchiveForm.btRefreshClick(Sender: TObject);
Begin
  RefreshArchiveNames;
  RefreshList;
End;

Procedure TArchiveForm.btNewArchiveClick(Sender: TObject);
Var
  AName: String;
  Args, Res: TJSONObject;
  Created: Boolean;
Begin
  AName := Trim(cbArchive.Text);
  If IsAllArchivesMode Then
    AName := '';
  If Not InputQuery(SArchiveNewTitle, SArchiveNewPrompt, AName) Then
    exit;
  AName := Trim(AName);
  If Not IsValidArchiveName(AName) Then
  Begin
    MessageDlg('', SArchiveNameInvalidMsg, mtError, [mbOK], 0);
    exit;
  End;
  AppBusy;
  Try
    Args := TJSONObject.Create;
    Args.Add('action', 'create_archive');
    Args.Add('archive', UTF8Decode(AName));
    Res := ArchiveRequest(Args, 60000);
  Finally
    AppNormal;
  End;
  If Res = nil Then
  Begin
    MainForm.CheckStatus(False);
    exit;
  End;
  Try
    Created := JsonBool(Res, 'created');
  Finally
    Res.Free;
  End;
  RememberName(AName);
  If Created Then
    MessageDlg('', Format(SArchiveCreated, [AName]), mtInformation, [mbOK], 0)
  Else
    MessageDlg('', Format(SArchiveAlreadyExists, [AName]), mtInformation, [mbOK], 0);
  RefreshList;
End;

Procedure TArchiveForm.btDeleteArchiveClick(Sender: TObject);
Var
  AName: String;
  Args, Res: TJSONObject;
  Deleted: Boolean;
  i: Integer;
Begin
  If IsAllArchivesMode Then
    exit;
  If Not EnsureArchiveName Then
    exit;
  AName := Trim(cbArchive.Text);
  If (FEntries <> nil) And (FEntries.Count > 0) Then
  Begin
    MessageDlg('', SArchiveDeleteNotEmpty, mtWarning, [mbOK], 0);
    exit;
  End;
  If MessageDlg('', Format(SArchiveDeleteConfirm, [AName]), mtConfirmation,
    mbYesNo, 0, mbNo) <> mrYes Then
    exit;
  AppBusy;
  Try
    Args := TJSONObject.Create;
    Args.Add('action', 'delete_archive');
    Args.Add('archive', UTF8Decode(AName));
    Res := ArchiveRequest(Args, 60000);
  Finally
    AppNormal;
  End;
  If Res = nil Then
  Begin
    MainForm.CheckStatus(False);
    exit;
  End;
  Try
    Deleted := JsonBool(Res, 'deleted');
  Finally
    Res.Free;
  End;
  If Not Deleted Then
  Begin
    MessageDlg('', Format(SArchiveDeleteNotDeleted, [AName]), mtError, [mbOK], 0);
    exit;
  End;
  i := cbArchive.Items.IndexOf(AName);
  If i >= 0 Then
    cbArchive.Items.Delete(i);
  cbArchive.Text := '';
  ClearEntries;
  RefreshArchiveNames;
  If Trim(cbArchive.Text) <> '' Then
    RefreshList;
  MessageDlg('', Format(SArchiveDeleted, [AName]), mtInformation, [mbOK], 0);
End;

Procedure TArchiveForm.cbArchiveChange(Sender: TObject);
Begin
  UpdateButtons;
End;

Procedure TArchiveForm.cbArchiveSelect(Sender: TObject);
Begin
  If cbArchive.ItemIndex >= 0 Then
  Begin
    FOffset := 0;
    RefreshList;
  End;
End;

Procedure TArchiveForm.lvEntriesClick(Sender: TObject);
Begin
  UpdateButtons;
End;

Procedure TArchiveForm.lvEntriesDblClick(Sender: TObject);
Begin
  If (FEntries <> nil) And (lvEntries.Row >= 0) And
    (lvEntries.Row < FEntries.Count) Then
    btViewClick(nil);
End;

Procedure TArchiveForm.lvEntriesKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If (Key = VK_A) And (ssCtrl In Shift) Then
  Begin
    lvEntries.SelectAll;
    Key := 0;
    UpdateButtons;
  End;
End;

Procedure TArchiveForm.btRestoreClick(Sender: TObject);
Var
  Entries: TStringList;
  Frm: TArchiveRestoreForm;
  AName, Arch, Hash, Dir, ErrMsg, ErrorText: String;
  Remove, Verify: Boolean;
  i, j, Restored: Integer;
  Args, Res, Item: TJSONObject;
  Items: TJSONArray;
  Data: TJSONData;
Begin
  If (Not IsAllArchivesMode) And (Not EnsureArchiveName) Then
    exit;
  Entries := GetSelectedEntries;
  Try
    If Entries.Count = 0 Then
    Begin
      MessageDlg('', SArchiveNoEntries, mtInformation, [mbOK], 0);
      exit;
    End;
    Frm := TArchiveRestoreForm.Create(Self);
    Try
      If Frm.ShowModal <> mrOk Then
        exit;
      Dir := '';
      If Frm.ChangePath Then
        Dir := Frm.DownloadDir;
      Remove := Frm.RemoveFromArchive;
    Finally
      Frm.Free;
    End;
    Verify := cbVerifyRestore.Checked;

    Items := TJSONArray.Create;
    For i := 0 To Entries.Count - 1 Do
    Begin
      j := Pos(#9, Entries[i]);
      If j = 0 Then
        continue;
      Arch := Copy(Entries[i], 1, j - 1);
      Hash := Copy(Entries[i], j + 1, MaxInt);
      Item := TJSONObject.Create;
      Item.Add('archive', UTF8Decode(Arch));
      Item.Add('hash_string', Hash);
      Items.Add(Item);
    End;
    If Items.Count = 0 Then
    Begin
      Items.Free;
      exit;
    End;

    Args := TJSONObject.Create;
    Args.Add('action', 'restore');
    Args.Add('items', Items);
    If Trim(Dir) <> '' Then
      Args.Add('download_dir', UTF8Decode(Trim(Dir)));
    Args.Add('remove', Remove);
    Args.Add('verify', Verify);

    Restored := 0;
    ErrMsg := '';
    AppBusy;
    Try
      Res := ArchiveRequest(Args, 300000);
    Finally
      AppNormal;
    End;

    If Res = nil Then
    Begin
      ErrMsg := RpcObj.Status;
      RpcObj.Status := '';
    End
    Else
    Try
      Restored := Integer(JsonInt64(Res, 'restored', 0));
      Data := Res.Find('errors');
      If (Data <> nil) And (Data Is TJSONArray) Then
        For i := 0 To TJSONArray(Data).Count - 1 Do
        Begin
          Item := TJSONArray(Data).Objects[i];
          If Item = nil Then
            continue;
          Arch := JsonStr(Item, 'archive');
          Hash := JsonStr(Item, 'hash_string');
          ErrorText := JsonStr(Item, 'error');
          If ErrMsg <> '' Then
            ErrMsg := ErrMsg + LineEnding;
          If IsAllArchivesMode And (Arch <> '') Then
            ErrMsg := ErrMsg + Arch + ': ';
          If Hash <> '' Then
            ErrMsg := ErrMsg + Hash + ': ';
          ErrMsg := ErrMsg + ErrorText;
        End;
    Finally
      Res.Free;
    End;

    If Not IsAllArchivesMode Then
    Begin
      AName := Trim(cbArchive.Text);
      RememberArchiveName(AName);
    End;
    RpcObj.RefreshNow := RpcObj.RefreshNow + [rtTorrents];
    If ErrMsg <> '' Then
      MessageDlg('', Format(SArchiveRestoredPartial, [Restored, Entries.Count]) +
        LineEnding + LineEnding + ErrMsg, mtWarning, [mbOK], 0)
    Else
      MessageDlg('', Format(SArchiveRestored, [Restored]), mtInformation, [mbOK], 0);
    RefreshList;
  Finally
    Entries.Free;
  End;
End;

Procedure TArchiveForm.btViewClick(Sender: TObject);
Var
  Entries: TStringList;
  Frm: TAddTorrentForm;
  Files: TJSONArray;
  Args, Res: TJSONObject;
  Arch, Hash, TorrentName, ErrMsg, Encoded: String;
  Data: RawByteString;
  TotalSize: Int64;
  i: Integer;
Begin
  Entries := GetSelectedEntries;
  Try
    If Entries.Count <> 1 Then
    Begin
      MessageDlg('', SArchiveViewSingle, mtInformation, [mbOK], 0);
      exit;
    End;
    i := Pos(#9, Entries[0]);
    If i = 0 Then
      exit;
    Arch := Copy(Entries[0], 1, i - 1);
    Hash := Copy(Entries[0], i + 1, MaxInt);
    If (Arch = '') Or (Hash = '') Then
      exit;
    AppBusy;
    Try
      Args := TJSONObject.Create;
      Args.Add('action', 'view');
      Args.Add('archive', UTF8Decode(Arch));
      Args.Add('hash_string', Hash);
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
      Encoded := JsonStr(Res, 'data_base64');
    Finally
      Res.Free;
    End;
    If Encoded = '' Then
    Begin
      MessageDlg('', SArchiveViewNoTorrent, mtError, [mbOK], 0);
      exit;
    End;
    Data := DecodeBase64(Encoded);
    Files := TArchiveViewForm.ExtractTorrentFiles(Data, Hash, TorrentName, TotalSize, ErrMsg);
    If Files = nil Then
    Begin
      MessageDlg('', ErrMsg, mtError, [mbOK], 0);
      exit;
    End;
    Try
      Frm := TAddTorrentForm.Create(Self);
      Try
        Frm.ShowContents(TorrentName, Files);
      Finally
        Frm.Free;
      End;
    Finally
      Files.Free;
    End;
  Finally
    Entries.Free;
  End;
End;

Procedure TArchiveForm.btDeleteEntryClick(Sender: TObject);
Var
  Entries: TStringList;
  Args, Res: TJSONObject;
  i, j, Deleted, Failed: Integer;
  Arch, Hash, ErrMsg, Status: String;
Begin
  Entries := GetSelectedEntries;
  Try
    If Entries.Count = 0 Then
    Begin
      MessageDlg('', SArchiveNoEntries, mtInformation, [mbOK], 0);
      exit;
    End;
    If MessageDlg('', SArchiveDeleteEntryConfirm, mtConfirmation, mbYesNo,
      0, mbNo) <> mrYes Then
      exit;
    Deleted := 0;
    Failed := 0;
    ErrMsg := '';
    AppBusy;
    Try
      For i := 0 To Entries.Count - 1 Do
      Begin
        j := Pos(#9, Entries[i]);
        If j = 0 Then
          continue;
        Arch := Copy(Entries[i], 1, j - 1);
        Hash := Copy(Entries[i], j + 1, MaxInt);
        Args := TJSONObject.Create;
        Args.Add('action', 'delete');
        Args.Add('archive', UTF8Decode(Arch));
        Args.Add('hash_string', Hash);
        Res := ArchiveRequest(Args, 60000);
        If Res = nil Then
        Begin
          Inc(Failed);
          Status := RpcObj.Status;
          RpcObj.Status := '';
          If Status = '' Then
            Status := SArchiveDeleteEntryFailed;
          If ErrMsg <> '' Then
            ErrMsg := ErrMsg + LineEnding;
          If IsAllArchivesMode And (Arch <> '') Then
            ErrMsg := ErrMsg + Arch + ': ';
          ErrMsg := ErrMsg + Hash + ': ' + Status;
          If Not RpcObj.Connected Then
            break;
        End
        Else
        Try
          If JsonBool(Res, 'deleted') Then
            Inc(Deleted)
          Else
          Begin
            Inc(Failed);
            If ErrMsg <> '' Then
              ErrMsg := ErrMsg + LineEnding;
            If IsAllArchivesMode And (Arch <> '') Then
              ErrMsg := ErrMsg + Arch + ': ';
            ErrMsg := ErrMsg + Hash + ': ' + SArchiveDeleteEntryFailed;
          End;
        Finally
          Res.Free;
        End;
      End;
    Finally
      AppNormal;
    End;
    RefreshList;
    If Failed = 0 Then
      MessageDlg('', Format(SArchiveDeleteEntryDone, [Deleted]), mtInformation, [mbOK], 0)
    Else
      MessageDlg('', Format(SArchiveDeleteEntryPartial, [Deleted, Entries.Count]) +
        LineEnding + LineEnding + ErrMsg, mtWarning, [mbOK], 0);
  Finally
    Entries.Free;
  End;
End;

Procedure TArchiveForm.btMoveClick(Sender: TObject);
Var
  Entries, Archives, Hashes: TStringList;
  Target, Arch, ErrMsg: String;
  Overwrite: Boolean;
  Args, Res: TJSONObject;
  i, j, k: Integer;
  Moved: Int64;
Begin
  If (Not IsAllArchivesMode) And (Not EnsureArchiveName) Then
    exit;
  Entries := GetSelectedEntries;
  Try
    If Entries.Count = 0 Then
    Begin
      MessageDlg('', SArchiveNoEntries, mtInformation, [mbOK], 0);
      exit;
    End;
    Archives := UniqueArchives(Entries);
    Try
      If Archives.Count = 0 Then
      Begin
        MessageDlg('', SArchiveNoEntries, mtInformation, [mbOK], 0);
        exit;
      End;
      Target := '';
      If Not InputQuery(SArchiveMoveTitle, SArchiveMoveTarget, Target) Then
        exit;
      Target := Trim(Target);
      If Not IsValidArchiveName(Target) Then
      Begin
        MessageDlg('', SArchiveNameInvalidMsg, mtError, [mbOK], 0);
        exit;
      End;
      If (Archives.Count = 1) And (CompareText(Target, Archives[0]) = 0) Then
      Begin
        MessageDlg('', SArchiveMoveSame, mtError, [mbOK], 0);
        exit;
      End;
      Overwrite := MessageDlg('', SArchiveMoveOverwrite, mtConfirmation, mbYesNo,
        0, mbNo) = mrYes;
      Moved := 0;
      ErrMsg := '';
      AppBusy;
      Try
        For i := 0 To Archives.Count - 1 Do
        Begin
          Arch := Archives[i];
          If CompareText(Arch, Target) = 0 Then
            continue;
          Hashes := TStringList.Create;
          Try
            For j := 0 To Entries.Count - 1 Do
            Begin
              k := Pos(#9, Entries[j]);
              If k = 0 Then
                continue;
              If Copy(Entries[j], 1, k - 1) = Arch Then
                Hashes.Add(Copy(Entries[j], k + 1, MaxInt));
            End;
            Args := TJSONObject.Create;
            Args.Add('action', 'move');
            Args.Add('archive', UTF8Decode(Arch));
            Args.Add('target_archive', UTF8Decode(Target));
            Args.Add('overwrite', Overwrite);
            Args.Add('hashes', StringsToJSON(Hashes));
            Res := ArchiveRequest(Args, 300000);
          Finally
            Hashes.Free;
          End;
          If Res = nil Then
          Begin
            If ErrMsg <> '' Then
              ErrMsg := ErrMsg + LineEnding;
            ErrMsg := ErrMsg + Arch + ': ' + RpcObj.Status;
            RpcObj.Status := '';
            If Not RpcObj.Connected Then
              break;
          End
          Else
          Begin
            Moved := Moved + JsonInt64(Res, 'moved');
            Res.Free;
          End;
        End;
      Finally
        AppNormal;
      End;
      RememberArchiveName(Target);
      If ErrMsg <> '' Then
        MessageDlg('', Format(SArchiveMoved, [Moved, Target]) + LineEnding +
          LineEnding + ErrMsg, mtWarning, [mbOK], 0)
      Else
        MessageDlg('', Format(SArchiveMoved, [Moved, Target]), mtInformation, [mbOK], 0);
      RefreshList;
    Finally
      Archives.Free;
    End;
  Finally
    Entries.Free;
  End;
End;

Procedure TArchiveForm.btDownloadClick(Sender: TObject);
Var
  Entries, Archives, Hashes: TStringList;
  AName, Encoded: String;
  Args, Res: TJSONObject;
  Data: RawByteString;
  Stream: TFileStream;
  Count: Int64;
  i, j: Integer;
Begin
  If (Not IsAllArchivesMode) And (Not EnsureArchiveName) Then
    exit;
  Entries := GetSelectedEntries;
  Try
    If Entries.Count = 0 Then
    Begin
      MessageDlg('', SArchiveNoEntries, mtInformation, [mbOK], 0);
      exit;
    End;
    Archives := UniqueArchives(Entries);
    Try
      If Archives.Count = 0 Then
      Begin
        MessageDlg('', SArchiveNoEntries, mtInformation, [mbOK], 0);
        exit;
      End;
      If Archives.Count > 1 Then
      Begin
        MessageDlg('', SArchiveSingleArchiveRequired, mtInformation, [mbOK], 0);
        exit;
      End;
      AName := Archives[0];
      Hashes := TStringList.Create;
      Try
        For i := 0 To Entries.Count - 1 Do
        Begin
          j := Pos(#9, Entries[i]);
          If j = 0 Then
            continue;
          If Copy(Entries[i], 1, j - 1) = AName Then
            Hashes.Add(Copy(Entries[i], j + 1, MaxInt));
        End;
        SaveDialog1.FileName := 'archive-' + FormatDateTime('yyyymmdd-hhnnss', Now) + '.zip';
        If Not SaveDialog1.Execute Then
          exit;
        AppBusy;
        Try
          Args := TJSONObject.Create;
          Args.Add('action', 'export');
          Args.Add('archive', UTF8Decode(AName));
          Args.Add('overwrite', True);
          Args.Add('inline', True);
          Args.Add('hashes', StringsToJSON(Hashes));
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
          Encoded := JsonStr(Res, 'data_base64');
          Count := JsonInt64(Res, 'count');
        Finally
          Res.Free;
        End;
        If Encoded = '' Then
        Begin
          MessageDlg('', SArchiveNoEntries, mtError, [mbOK], 0);
          exit;
        End;
        Data := DecodeBase64(Encoded);
        Try
          Stream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
          Try
            If Length(Data) > 0 Then
              Stream.WriteBuffer(Data[1], Length(Data));
          Finally
            Stream.Free;
          End;
        Except
          On E: Exception Do
          Begin
            MessageDlg('', E.Message, mtError, [mbOK], 0);
            exit;
          End;
        End;
        MessageDlg('', Format(SArchiveDownloadSaved, [Count]) + LineEnding +
          SaveDialog1.FileName, mtInformation, [mbOK], 0);
      Finally
        Hashes.Free;
      End;
    Finally
      Archives.Free;
    End;
  Finally
    Entries.Free;
  End;
End;

Procedure TArchiveForm.btImportClick(Sender: TObject);
Var
  AName, Detail: String;
  Data: AnsiString;
  Overwrite: Boolean;
  Stream: TFileStream;
  Args, Res: TJSONObject;
  Imported, Skipped: Int64;
Begin
  If IsAllArchivesMode Then
    exit;
  If Not EnsureArchiveName Then
    exit;
  AName := Trim(cbArchive.Text);
  OpenDialog1.Title := SArchiveImportDialogTitle;
  OpenDialog1.FileName := '';
  If Not OpenDialog1.Execute Then
    exit;
  Try
    Stream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead Or fmShareDenyWrite);
    Try
      If Stream.Size > MaxInt Then
      Begin
        MessageDlg('', SArchiveBundleTooLarge, mtError, [mbOK], 0);
        exit;
      End;
      SetLength(Data, Stream.Size);
      If Length(Data) > 0 Then
        Stream.ReadBuffer(Data[1], Length(Data));
    Finally
      Stream.Free;
    End;
  Except
    On E: Exception Do
    Begin
      MessageDlg('', E.Message, mtError, [mbOK], 0);
      exit;
    End;
  End;
  Overwrite := MessageDlg('', SArchiveImportOverwrite, mtConfirmation, mbYesNo,
    0, mbNo) = mrYes;
  AppBusy;
  Try
    Args := TJSONObject.Create;
    Args.Add('action', 'import');
    Args.Add('data_base64', EncodeBase64(Data));
    Args.Add('archive', UTF8Decode(AName));
    Args.Add('overwrite', Overwrite);
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
    Imported := JsonInt64(Res, 'imported');
    Skipped := JsonInt64(Res, 'skipped');
  Finally
    Res.Free;
  End;
  RememberArchiveName(AName);
  Detail := Format(SArchiveImported, [Imported, Skipped]) + LineEnding +
    LineEnding + Format(SArchiveBundleFile, [OpenDialog1.FileName]);
  MessageDlg('', Detail, mtInformation, [mbOK], 0);
  RefreshList;
End;

Procedure TArchiveForm.btCloseClick(Sender: TObject);
Begin
  ModalResult := mrClose;
End;

Initialization
  {$I archiveform.lrs}

End.
