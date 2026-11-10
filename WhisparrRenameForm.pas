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
*************************************************************************************}

Unit WhisparrRenameForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, StdCtrls, Variants, ButtonPanel, BaseForm, VarGrid;

Resourcestring
  SWhisparrRenamePartial = 'Renamed %d of %d torrent(s).';
  SWhisparrNotRecognized = 'Not recognized';
  SWhisparrNotRecognizedReason = 'Not recognized (%s)';

Type

  { TWhisparrRenameForm }

  TWhisparrRenameForm = Class(TBaseForm)
    Buttons: TButtonPanel;
    gRename: TVarGrid;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormShow(Sender: TObject);
    Procedure gRenameCellAttributes(Sender: TVarGrid; ACol, ARow,
      ADataCol: Integer; AState: TGridDrawState; Var CellAttribs: TCellAttributes);
    Procedure gRenameCheckBoxClick(Sender: TVarGrid; ACol, ARow,
      ADataCol: Integer);
    Procedure OKButtonClick(Sender: TObject);
  Private
    FIds: Array Of Integer;
    FOldNames: TStringList;
    FNewNames: TStringList;
    FPaths: TStringList;
    FBaseNames: TStringList;
    FErrors: Array Of String;
    FRecognized: Array Of Boolean;
    FChecked: Array Of Boolean;
    FBusy: Boolean;
    Procedure LoadColumnWidths;
    Procedure LoadItems;
    Procedure SaveColumnWidths;
    Function ShortWhisparrError(Const S: String): String;
    Procedure UpdateButtons;
  Public
    SelectedIds: Variant;
  End;

Implementation

Uses fpjson, utils, rpc, main, WhisparrClient;

Const
  WhisparrRenameSection = 'WhisparrRename';

  { TWhisparrRenameForm }

Procedure TWhisparrRenameForm.FormCreate(Sender: TObject);
Begin
  bidiMode := GetBiDi();
  FOldNames := TStringList.Create;
  FNewNames := TStringList.Create;
  FPaths := TStringList.Create;
  FBaseNames := TStringList.Create;
  LoadColumnWidths;
  Buttons.OKButton.ModalResult := mrNone;
  Buttons.OKButton.OnClick := @OKButtonClick;
  OnCloseQuery := @FormCloseQuery;
End;

Procedure TWhisparrRenameForm.FormCloseQuery(Sender: TObject;
  Var CanClose: Boolean);
Begin
  If FBusy Then
    CanClose := False;
End;

Procedure TWhisparrRenameForm.FormDestroy(Sender: TObject);
Begin
  SaveColumnWidths;
  FOldNames.Free;
  FNewNames.Free;
  FPaths.Free;
  FBaseNames.Free;
End;

Procedure TWhisparrRenameForm.LoadColumnWidths;
Var
  i, W: Integer;
Begin
  For i := 0 To gRename.Columns.Count - 1 Do
  Begin
    W := Ini.ReadInteger(WhisparrRenameSection, Format('Width%d', [i]),
      gRename.Columns[i].Width);
    If W > 0 Then
      gRename.Columns[i].Width := W;
  End;
End;

Procedure TWhisparrRenameForm.SaveColumnWidths;
Var
  i: Integer;
Begin
  For i := 0 To gRename.Columns.Count - 1 Do
    Ini.WriteInteger(WhisparrRenameSection, Format('Width%d', [i]),
      gRename.Columns[i].Width);
  Ini.UpdateFile;
End;

Procedure TWhisparrRenameForm.FormShow(Sender: TObject);
Begin
  LoadItems;
  UpdateButtons;
End;

Function TWhisparrRenameForm.ShortWhisparrError(Const S: String): String;
Var
  P: Integer;
Begin
  Result := Trim(S);
  P := Pos(#10, Result);
  If P > 0 Then
    Result := Trim(Copy(Result, 1, P - 1));
  P := Pos(#13, Result);
  If P > 0 Then
    Result := Trim(Copy(Result, 1, P - 1));
  If Pos('Whisparr ', Result) = 1 Then
    Delete(Result, 1, 9);
  If Pos('returned HTTP ', Result) = 1 Then
    Delete(Result, 1, 9);
  While (Result <> '') And (Result[Length(Result)] In ['.', ' ']) Do
    Delete(Result, Length(Result), 1);
  If Length(Result) > 60 Then
    Result := Copy(Result, 1, 57) + '...';
End;

Function JsonString(Obj: TJSONObject; Const Name: String): String;
Var
  Data: TJSONData;
Begin
  Result := '';
  If Obj = nil Then
    exit;
  Data := Obj.Find(Name);
  If Data <> nil Then
    Result := Data.AsString;
End;

Function TopicNameFromComment(Const Comment: String): String;
Const
  Marker = '[topic-name]';
Var
  Lines: TStringList;
  i: Integer;
  s: String;
Begin
  Result := '';
  If Trim(Comment) = '' Then
    exit;
  Lines := TStringList.Create;
  Try
    Lines.Text := Comment;
    For i := 0 To Lines.Count - 1 Do
    Begin
      s := Trim(Lines[i]);
      If (Length(s) >= Length(Marker)) And
        (CompareText(Copy(s, 1, Length(Marker)), Marker) = 0) Then
      Begin
        Result := Trim(Copy(s, Length(Marker) + 1, MaxInt));
        If Result <> '' Then
          break;
      End;
    End;
  Finally
    Lines.Free;
  End;
End;

Procedure TWhisparrRenameForm.LoadItems;
Var
  Config: TWhisparrConfig;
  Args: TJSONObject;
  List, Files: TJSONArray;
  ListData: TJSONData;
  Item, FileObj: TJSONObject;
  i, j, k, Count, Id, MediaCount: Integer;
  TorrentName, Comment, TopicName, OldName, NewName, NewBase, OldBase, Ext,
    FilePath: String;
  SeriesName, SeriesError, BaseTitle: String;
  Error, Reason: String;
  Parsed, CanRename, SingleFile: Boolean;

  Procedure AddRow(AId: Integer; Const APath, ADisplayOld, ADisplayNew,
    ABaseName, AError: String; ARecognized: Boolean);
  Var
    n: Integer;
  Begin
    n := Length(FIds);
    SetLength(FIds, n + 1);
    SetLength(FErrors, n + 1);
    SetLength(FRecognized, n + 1);
    SetLength(FChecked, n + 1);
    FIds[n] := AId;
    FErrors[n] := AError;
    FOldNames.Add(ADisplayOld);
    FNewNames.Add(ADisplayNew);
    FPaths.Add(APath);
    FBaseNames.Add(ABaseName);
    FRecognized[n] := ARecognized;
    FChecked[n] := ARecognized;
  End;

  Function WithExtension(Const Base, OriginalExt: String): String;
  Begin
    Result := Base;
    If (OriginalExt <> '') And (Base <> '') And
      (Not SameText(ExtractFileExt(Base), OriginalExt)) Then
      Result := Base + OriginalExt;
  End;

Begin
  FIds := nil;
  FErrors := nil;
  FRecognized := nil;
  FChecked := nil;
  FOldNames.Clear;
  FNewNames.Clear;
  FPaths.Clear;
  FBaseNames.Clear;
  gRename.Items.RowCnt := 0;
  If VarIsEmpty(SelectedIds) Or VarIsNull(SelectedIds) Then
    exit;
  Config := MainForm.GetWhisparrConfig;
  If Not WhisparrConfigured(Config) Then
    exit;
  AppBusy;
  Try
    Args := RpcObj.RequestInfos(SelectedIds, ['id', 'name', 'comment', 'files']);
    If Args = nil Then
    Begin
      MainForm.CheckStatus(False);
      exit;
    End;
    Try
      ListData := Args.Find('torrents');
      If Not (ListData Is TJSONArray) Then
        exit;
      List := TJSONArray(ListData);
      Count := List.Count;
      For i := 0 To Count - 1 Do
      Begin
        Id := 0;
        TorrentName := '';
        Comment := '';
        TopicName := '';
        Item := List.Objects[i];
        If Item <> nil Then
        Begin
          If Item.Find('id') <> nil Then
            Id := Item.Find('id').AsInteger;
          TorrentName := JsonString(Item, 'name');
          Comment := JsonString(Item, 'comment');
          TopicName := TopicNameFromComment(Comment);
        End;

        Files := nil;
        If Item <> nil Then
          If Item.Find('files') Is TJSONArray Then
            Files := TJSONArray(Item.Find('files'));

        SingleFile := (Files = nil) Or (Files.Count = 0);
        If (Files <> nil) And (Files.Count = 1) Then
          SingleFile := SameText(JsonString(Files.Objects[0], 'name'),
            TorrentName);

        If SingleFile Then
        Begin
          OldName := TorrentName;
          NewName := '';
          NewBase := '';
          Error := '';
          Parsed := False;
          If OldName <> '' Then
          Begin
            Ext := ExtractFileExt(OldName);
            Parsed := WhisparrParseEpisode(Config, OldName, NewName, Error);
            If (Not Parsed) And (TopicName <> '') Then
              Parsed := WhisparrParseEpisode(Config, TopicName, NewName, Error);
            If (Not Parsed) And (TopicName <> '') Then
            Begin
              SeriesName := '';
              SeriesError := '';
              If WhisparrParseSeriesName(Config, TopicName, SeriesName,
                SeriesError) And (SeriesName <> '') Then
                Parsed := WhisparrParseEpisode(Config,
                  SeriesName + ' ' + ChangeFileExt(OldName, ''), NewName, Error,
                  SeriesName);
            End;
            If Parsed Then
              NewBase := WithExtension(NewName, Ext);
          End
          Else
            Error := 'The torrent name is empty.';
          CanRename := Parsed And (NewBase <> '') And (NewBase <> OldName);
          AddRow(Id, OldName, OldName, NewBase, NewBase, Error, CanRename);
          continue;
        End;

        SeriesName := '';
        SeriesError := '';
        If TorrentName <> '' Then
          WhisparrParseSeriesName(Config, TorrentName, SeriesName, SeriesError);
        If (SeriesName = '') And (TopicName <> '') Then
          WhisparrParseSeriesName(Config, TopicName, SeriesName, SeriesError);

        MediaCount := 0;
        For j := 0 To Files.Count - 1 Do
        Begin
          FileObj := Files.Objects[j];
          FilePath := JsonString(FileObj, 'name');
          If Not WhisparrIsMediaFile(FilePath) Then
            continue;
          Inc(MediaCount);
          OldBase := ExtractFileName(FilePath);
          Ext := ExtractFileExt(OldBase);
          Error := '';
          NewName := '';
          BaseTitle := ChangeFileExt(OldBase, '');
          Parsed := WhisparrParseEpisode(Config, BaseTitle, NewName, Error,
            SeriesName, True);
          If (Not Parsed) And (SeriesName <> '') Then
            Parsed := WhisparrParseEpisode(Config,
              SeriesName + ' ' + BaseTitle, NewName, Error, SeriesName, True);
          If (Not Parsed) And (TopicName <> '') And
            (TopicName <> SeriesName) Then
            Parsed := WhisparrParseEpisode(Config,
              TopicName + ' ' + BaseTitle, NewName, Error, SeriesName, True);
          NewBase := '';
          If Parsed Then
            NewBase := WithExtension(NewName, Ext);
          CanRename := Parsed And (NewBase <> '') And (NewBase <> OldBase);
          If NewBase <> '' Then
            NewName := Copy(FilePath, 1, Length(FilePath) - Length(OldBase)) +
              NewBase
          Else
            NewName := '';
          AddRow(Id, FilePath, FilePath, NewName, NewBase, Error, CanRename);
        End;

        If MediaCount = 0 Then
          AddRow(Id, TorrentName, TorrentName, '', '',
            'No media files found.', False);
      End;

      For i := 0 To High(FIds) Do
        If FRecognized[i] Then
          For k := 0 To i - 1 Do
            If FRecognized[k] And (FNewNames[i] = FNewNames[k]) Then
            Begin
              FRecognized[i] := False;
              FChecked[i] := False;
              FErrors[i] := 'Duplicate name.';
              break;
            End;

      gRename.Items.RowCnt := Length(FIds);
      For i := 0 To High(FIds) Do
      Begin
        gRename.Items[0, i] := '';
        gRename.Items[1, i] := FOldNames[i];
        If FNewNames[i] <> '' Then
          gRename.Items[2, i] := FNewNames[i]
        Else
        Begin
          Reason := ShortWhisparrError(FErrors[i]);
          If Reason = '' Then
            gRename.Items[2, i] := SWhisparrNotRecognized
          Else
            gRename.Items[2, i] :=
              Format(SWhisparrNotRecognizedReason, [Reason]);
        End;
      End;
    Finally
      Args.Free;
    End;
  Finally
    AppNormal;
  End;
End;

Procedure TWhisparrRenameForm.gRenameCellAttributes(Sender: TVarGrid;
  ACol, ARow, ADataCol: Integer; AState: TGridDrawState;
  Var CellAttribs: TCellAttributes);
Begin
  If (ARow < 0) Or (ARow >= Length(FRecognized)) Then
    exit;
  If ADataCol <> 0 Then
    exit;
  CellAttribs.Options := [coDrawCheckBox];
  If FChecked[ARow] Then
    CellAttribs.State := cbChecked
  Else
    CellAttribs.State := cbUnchecked;
End;

Procedure TWhisparrRenameForm.gRenameCheckBoxClick(Sender: TVarGrid;
  ACol, ARow, ADataCol: Integer);
Begin
  If (ADataCol <> 0) Or (ARow < 0) Or (ARow >= Length(FRecognized)) Then
    exit;
  If Not FRecognized[ARow] Then
    exit;
  FChecked[ARow] := Not FChecked[ARow];
  Sender.Invalidate;
  UpdateButtons;
End;

Procedure TWhisparrRenameForm.UpdateButtons;
Var
  i, Count: Integer;
Begin
  Count := 0;
  For i := 0 To High(FChecked) Do
    If FRecognized[i] And FChecked[i] Then
      Inc(Count);
  Buttons.OKButton.Enabled := Count > 0;
End;

Procedure TWhisparrRenameForm.OKButtonClick(Sender: TObject);
Var
  i, Renamed, Failed: Integer;
  Args: TJSONObject;
Begin
  If FBusy Then
    exit;
  FBusy := True;
  Buttons.OKButton.Enabled := False;
  Buttons.CancelButton.Enabled := False;
  Renamed := 0;
  Failed := 0;
  AppBusy;
  MainForm.SuppressErrorDialogs;
  Try
    For i := 0 To High(FChecked) Do
      If FRecognized[i] And FChecked[i] Then
      Begin
        Args := TJSONObject.Create;
        Args.Add('path', UTF8Decode(FPaths[i]));
        Args.Add('name', UTF8Decode(FBaseNames[i]));
        If MainForm.TorrentAction(FIds[i], 'torrent-rename-path', Args,
          False) Then
          Inc(Renamed)
        Else
          Inc(Failed);
      End;
  Finally
    MainForm.RestoreErrorDialogs;
    FBusy := False;
    Buttons.CancelButton.Enabled := True;
    AppNormal;
  End;
  If Failed > 0 Then
    MessageDlg('', Format(SWhisparrRenamePartial, [Renamed, Renamed + Failed]),
      mtWarning, [mbOK], 0);
  ModalResult := mrOk;
End;

Initialization
  {$I whisparrrenameform.lrs}

End.
