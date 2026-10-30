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
    FErrors: Array Of String;
    FRecognized: Array Of Boolean;
    FChecked: Array Of Boolean;
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
  LoadColumnWidths;
  Buttons.OKButton.ModalResult := mrNone;
  Buttons.OKButton.OnClick := @OKButtonClick;
End;

Procedure TWhisparrRenameForm.FormDestroy(Sender: TObject);
Begin
  SaveColumnWidths;
  FOldNames.Free;
  FNewNames.Free;
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

Procedure TWhisparrRenameForm.LoadItems;
Var
  Config: TWhisparrConfig;
  Args: TJSONObject;
  List: TJSONData;
  Item: TJSONObject;
  i, Count, Id: Integer;
  OldName, NewName, Error, Reason: String;
  Parsed, CanRename: Boolean;
Begin
  FIds := nil;
  FErrors := nil;
  FRecognized := nil;
  FChecked := nil;
  FOldNames.Clear;
  FNewNames.Clear;
  gRename.Items.RowCnt := 0;
  If VarIsEmpty(SelectedIds) Or VarIsNull(SelectedIds) Then
    exit;
  Config := MainForm.GetWhisparrConfig;
  If Not WhisparrConfigured(Config) Then
    exit;
  AppBusy;
  Try
    Args := RpcObj.RequestInfos(SelectedIds, ['id', 'name']);
    If Args = nil Then
    Begin
      MainForm.CheckStatus(False);
      exit;
    End;
    Try
      List := Args.Find('torrents');
      If Not (List Is TJSONArray) Then
        exit;
      Count := TJSONArray(List).Count;
      SetLength(FIds, Count);
      SetLength(FErrors, Count);
      SetLength(FRecognized, Count);
      SetLength(FChecked, Count);
      For i := 0 To Count - 1 Do
      Begin
        Id := 0;
        OldName := '';
        NewName := '';
        Error := '';
        Parsed := False;
        Item := TJSONArray(List).Objects[i];
        If Item <> nil Then
        Begin
          If Item.Find('id') <> nil Then
            Id := Item.Find('id').AsInteger;
          If Item.Find('name') <> nil Then
            OldName := Item.Find('name').AsString;
          If OldName <> '' Then
            Parsed := WhisparrParseEpisode(Config, OldName, NewName, Error)
          Else
            Error := 'The torrent name is empty.';
        End;
        CanRename := Parsed And (NewName <> '') And (NewName <> OldName);
        FIds[i] := Id;
        FErrors[i] := Error;
        FOldNames.Add(OldName);
        If Parsed Then
          FNewNames.Add(NewName)
        Else
          FNewNames.Add('');
        FRecognized[i] := CanRename;
        FChecked[i] := CanRename;
      End;
      gRename.Items.RowCnt := Count;
      For i := 0 To Count - 1 Do
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
  Renamed := 0;
  Failed := 0;
  AppBusy;
  Try
    For i := 0 To High(FChecked) Do
      If FRecognized[i] And FChecked[i] Then
      Begin
        Args := TJSONObject.Create;
        Args.Add('path', UTF8Decode(FOldNames[i]));
        Args.Add('name', UTF8Decode(FNewNames[i]));
        If MainForm.TorrentAction(FIds[i], 'torrent-rename-path', Args) Then
          Inc(Renamed)
        Else
          Inc(Failed);
      End;
  Finally
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
