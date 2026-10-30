
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

Unit ArchiveViewForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, BaseForm, VarGrid, fpjson, AddTorrent, BEncode;

Resourcestring
  SArchiveViewNoTorrent = 'The archive entry does not contain torrent metadata.';
  SArchiveViewParseError = 'Could not read the torrent metadata: %s';
  SArchiveViewNoFiles = 'The torrent does not contain any files.';
  SArchiveViewInfo = '%d file(s), %s';

Type

  { TArchiveViewForm }

  TArchiveViewForm = Class(TBaseForm)
    btClose: TButton;
    lvFiles: TVarGrid;
    txInfo: TLabel;
    txName: TLabel;
    Procedure FormCreate(Sender: TObject);
  Private
    FFilesTree: TFilesTree;
    Class Function DecodeName(Const S: RawByteString): String;
    Class Function DictValue(Obj: TBEncoded; Const Key: String): TBEncoded;
    Class Function GetInt(Obj: TBEncoded; Const Key: String; Default: Int64): Int64;
    Class Function GetRawStr(Obj: TBEncoded; Const Key: String): RawByteString;
    Class Function PathToString(Obj: TBEncoded): String;
    Class Function ParseTorrent(Const TorrentData, ResumeData: RawByteString;
      Const Hash: String; Out AName: String; Out ATotalSize: Int64;
      Out AError: String): TJSONArray;
    Class Function ExtractStoredEntries(Const BundleData: RawByteString;
      Out TorrentData, ResumeData: RawByteString; Out AError: String): Boolean;
  Public
    Class Function ExtractTorrentFiles(Const BundleData: RawByteString;
      Const Hash: String; Out AName: String; Out ATotalSize: Int64;
      Out AError: String): TJSONArray;
    Procedure SetFiles(Const AName: String; Files: TJSONArray; ATotalSize: Int64);
  End;

Implementation

Uses utils, main;

  { TArchiveViewForm }

Procedure TArchiveViewForm.FormCreate(Sender: TObject);
Begin
  If Assigned(Ini) Then
    bidiMode := GetBiDi();
  lvFiles.Items.ExtraColumns := FilesExtraColumns;
  If Assigned(MainForm) Then
    lvFiles.Images := MainForm.ImageList16;
  FFilesTree := TFilesTree.Create(lvFiles);
  FFilesTree.Checkboxes := True;
  FFilesTree.CheckboxesReadOnly := True;
End;

Class Function TArchiveViewForm.DecodeName(Const S: RawByteString): String;
Begin
  Result := '';
  If S = '' Then
    exit;
  Try
    Result := UTF8Encode(UTF8Decode(S));
  Except
    Result := S;
  End;
End;

Class Function TArchiveViewForm.DictValue(Obj: TBEncoded; Const Key: String): TBEncoded;
Begin
  Result := nil;
  If (Obj = nil) Or (Obj.Format <> befDictionary) Or (Obj.ListData = nil) Then
    exit;
  Result := TBEncoded(Obj.ListData.FindElement(AnsiString(Key), False));
End;

Class Function TArchiveViewForm.GetInt(Obj: TBEncoded; Const Key: String;
  Default: Int64): Int64;
Var
  V: TBEncoded;
Begin
  Result := Default;
  V := DictValue(Obj, Key);
  If (V <> nil) And (V.Format = befInteger) Then
    Result := V.IntegerData;
End;

Class Function TArchiveViewForm.GetRawStr(Obj: TBEncoded; Const Key: String): RawByteString;
Var
  V: TBEncoded;
Begin
  Result := '';
  V := DictValue(Obj, Key);
  If (V <> nil) And (V.Format = befString) Then
    Result := V.StringData;
End;

Class Function TArchiveViewForm.PathToString(Obj: TBEncoded): String;
Var
  i: Integer;
  Part: TBEncoded;
  S: String;
Begin
  Result := '';
  If (Obj = nil) Or (Obj.Format <> befList) Or (Obj.ListData = nil) Then
    exit;
  For i := 0 To Obj.ListData.Count - 1 Do
  Begin
    Part := TBEncoded(Obj.ListData[i].Data);
    If (Part = nil) Or (Part.Format <> befString) Then
      continue;
    S := DecodeName(Part.StringData);
    If S = '' Then
      continue;
    If Result <> '' Then
      Result := Result + '/';
    Result := Result + S;
  End;
End;

Class Function TArchiveViewForm.ParseTorrent(Const TorrentData,
  ResumeData: RawByteString; Const Hash: String; Out AName: String;
  Out ATotalSize: Int64; Out AError: String): TJSONArray;
Var
  Stream: TMemoryStream;
  Root, Info, FilesNode, FileNode, PathNode: TBEncoded;
  ResumeStream: TMemoryStream;
  ResumeRoot, ResumeFiles, ResumeFileNode, ResumeNode: TBEncoded;
  i: Integer;
  TopName, ResumeName, RelName, FullName: String;
  FileSize: Int64;
  Item: TJSONObject;
  Names, ResumeNames: TStringList;
  Sizes: Array Of Int64;
  ResumeWanted, ResumePriority: Array Of Integer;

  Procedure AddTorrentFile(Const AFileName: String; ASize: Int64);
  Begin
    Names.Add(AFileName);
    SetLength(Sizes, Names.Count);
    Sizes[Names.Count - 1] := ASize;
    Inc(ATotalSize, ASize);
  End;

Begin
  Result := nil;
  AName := '';
  ATotalSize := 0;
  AError := '';
  If TorrentData = '' Then
  Begin
    AError := SArchiveViewNoTorrent;
    exit;
  End;
  Names := TStringList.Create;
  ResumeNames := TStringList.Create;
  Try
    Stream := TMemoryStream.Create;
    Try
      Stream.WriteBuffer(TorrentData[1], Length(TorrentData));
      Stream.Position := 0;
      Root := nil;
      Try
        Try
          Root := TBEncoded.Create(Stream);
        Except
          On E: Exception Do
          Begin
            AError := Format(SArchiveViewParseError, [E.Message]);
            exit;
          End;
        End;
        Info := DictValue(Root, 'info');
        If Info = nil Then
        Begin
          AError := SArchiveViewNoTorrent;
          exit;
        End;
        TopName := DecodeName(GetRawStr(Info, 'name.utf-8'));
        If TopName = '' Then
          TopName := DecodeName(GetRawStr(Info, 'name'));
        If TopName = '' Then
          TopName := Hash;
        AName := TopName;
        FilesNode := DictValue(Info, 'files');
        If (FilesNode <> nil) And (FilesNode.Format = befList) And
          (FilesNode.ListData <> nil) And (FilesNode.ListData.Count > 0) Then
        Begin
          For i := 0 To FilesNode.ListData.Count - 1 Do
          Begin
            FileNode := TBEncoded(FilesNode.ListData[i].Data);
            If (FileNode = nil) Or (FileNode.Format <> befDictionary) Then
              continue;
            FileSize := GetInt(FileNode, 'length', -1);
            If FileSize < 0 Then
              continue;
            PathNode := DictValue(FileNode, 'path.utf-8');
            If PathNode = nil Then
              PathNode := DictValue(FileNode, 'path');
            RelName := PathToString(PathNode);
            If RelName = '' Then
              continue;
            If TopName <> '' Then
              FullName := TopName + '/' + RelName
            Else
              FullName := RelName;
            AddTorrentFile(FullName, FileSize);
          End;
        End
        Else
        Begin
          FileSize := GetInt(Info, 'length', -1);
          If FileSize >= 0 Then
            AddTorrentFile(TopName, FileSize);
        End;
      Finally
        Root.Free;
      End;
    Finally
      Stream.Free;
    End;

    If Names.Count = 0 Then
    Begin
      AError := SArchiveViewNoFiles;
      exit;
    End;

    SetLength(ResumeWanted, Names.Count);
    SetLength(ResumePriority, Names.Count);
    For i := 0 To Names.Count - 1 Do
    Begin
      ResumeWanted[i] := 1;
      ResumePriority[i] := 0;
    End;

    ResumeName := '';
    If ResumeData <> '' Then
    Begin
      ResumeStream := TMemoryStream.Create;
      Try
        ResumeStream.WriteBuffer(ResumeData[1], Length(ResumeData));
        ResumeStream.Position := 0;
        ResumeRoot := nil;
        Try
          Try
            ResumeRoot := TBEncoded.Create(ResumeStream);
          Except
            ResumeRoot := nil;
          End;
          If (ResumeRoot <> nil) And (ResumeRoot.Format = befDictionary) Then
          Begin
            ResumeName := DecodeName(GetRawStr(ResumeRoot, 'name'));
            ResumeFiles := DictValue(ResumeRoot, 'files');
            If (ResumeFiles <> nil) And (ResumeFiles.Format = befList) And
              (ResumeFiles.ListData <> nil) Then
            Begin
              For i := 0 To ResumeFiles.ListData.Count - 1 Do
              Begin
                ResumeFileNode := TBEncoded(ResumeFiles.ListData[i].Data);
                If (ResumeFileNode <> nil) And (ResumeFileNode.Format = befString) Then
                  ResumeNames.Add(DecodeName(ResumeFileNode.StringData))
                Else
                  ResumeNames.Add('');
              End;
            End;
            ResumeNode := DictValue(ResumeRoot, 'dnd');
            If (ResumeNode <> nil) And (ResumeNode.Format = befList) And
              (ResumeNode.ListData <> nil) Then
            Begin
              For i := 0 To ResumeNode.ListData.Count - 1 Do
                If i < Names.Count Then
                Begin
                  ResumeFileNode := TBEncoded(ResumeNode.ListData[i].Data);
                  If (ResumeFileNode <> nil) And (ResumeFileNode.Format = befInteger) Then
                    If ResumeFileNode.IntegerData = 0 Then
                      ResumeWanted[i] := 1
                    Else
                      ResumeWanted[i] := 0;
                End;
            End;
            ResumeNode := DictValue(ResumeRoot, 'priority');
            If (ResumeNode <> nil) And (ResumeNode.Format = befList) And
              (ResumeNode.ListData <> nil) Then
            Begin
              For i := 0 To ResumeNode.ListData.Count - 1 Do
                If i < Names.Count Then
                Begin
                  ResumeFileNode := TBEncoded(ResumeNode.ListData[i].Data);
                  If (ResumeFileNode <> nil) And (ResumeFileNode.Format = befInteger) Then
                    ResumePriority[i] := ResumeFileNode.IntegerData;
                End;
            End;
          End;
        Finally
          ResumeRoot.Free;
        End;
      Finally
        ResumeStream.Free;
      End;
    End;

    If ResumeName <> '' Then
      AName := ResumeName;

    Result := TJSONArray.Create;
    For i := 0 To Names.Count - 1 Do
    Begin
      FullName := Names[i];
      If (i < ResumeNames.Count) And (ResumeNames[i] <> '') Then
        FullName := ResumeNames[i]
      Else If (ResumeName <> '') And (TopName <> '') Then
      Begin
        If FullName = TopName Then
          FullName := ResumeName
        Else If Copy(FullName, 1, Length(TopName) + 1) = TopName + '/' Then
          FullName := ResumeName + Copy(FullName, Length(TopName) + 1, MaxInt);
      End;
      Item := TJSONObject.Create;
      Item.Add('name', FullName);
      Item.Add('length', Sizes[i]);
      Item.Add('bytesCompleted', 0.0);
      Item.Add('wanted', ResumeWanted[i]);
      Item.Add('priority', ResumePriority[i]);
      Result.Add(Item);
    End;
  Finally
    Names.Free;
    ResumeNames.Free;
  End;
End;

Class Function TArchiveViewForm.ExtractStoredEntries(
  Const BundleData: RawByteString; Out TorrentData, ResumeData: RawByteString;
  Out AError: String): Boolean;
Var
  Pos, NamePos, DataPos: Integer;
  NameLen, ExtraLen, Flags, Method: Word;
  CompSize, UncompSize: Cardinal;
  EntryName: RawByteString;

  Function ReadU16(Offset: Integer): Word;
  Begin
    Move(BundleData[Offset + 1], Result, SizeOf(Result));
  End;

  Function ReadU32(Offset: Integer): Cardinal;
  Begin
    Move(BundleData[Offset + 1], Result, SizeOf(Result));
  End;

  Function EndsWith(const S, Suffix: RawByteString): Boolean;
  Begin
    Result := (Length(S) >= Length(Suffix)) And
      (Copy(S, Length(S) - Length(Suffix) + 1, Length(Suffix)) = Suffix);
  End;

Begin
  Result := False;
  TorrentData := '';
  ResumeData := '';
  AError := '';
  Pos := 0;
  While Pos + 30 <= Length(BundleData) Do
  Begin
    If ReadU32(Pos) <> $04034B50 Then
      break;
    Flags := ReadU16(Pos + 6);
    Method := ReadU16(Pos + 8);
    CompSize := ReadU32(Pos + 18);
    UncompSize := ReadU32(Pos + 22);
    NameLen := ReadU16(Pos + 26);
    ExtraLen := ReadU16(Pos + 28);
    If (Flags And 8) <> 0 Then
    Begin
      AError := Format(SArchiveViewParseError, ['unsupported ZIP data descriptor']);
      exit;
    End;
    If (Method <> 0) Or (CompSize <> UncompSize) Then
    Begin
      AError := Format(SArchiveViewParseError, ['unsupported ZIP compression']);
      exit;
    End;
    NamePos := Pos + 30;
    DataPos := NamePos + NameLen + ExtraLen;
    If (DataPos < NamePos) Or
      (Int64(DataPos) + CompSize > Length(BundleData)) Then
    Begin
      AError := Format(SArchiveViewParseError, ['invalid ZIP entry']);
      exit;
    End;
    EntryName := Copy(BundleData, NamePos + 1, NameLen);
    If EndsWith(EntryName, '.torrent') And (TorrentData = '') Then
    Begin
      SetLength(TorrentData, CompSize);
      If CompSize > 0 Then
        Move(BundleData[DataPos + 1], TorrentData[1], CompSize);
    End
    Else
      If EndsWith(EntryName, '.resume') And (ResumeData = '') Then
      Begin
        SetLength(ResumeData, CompSize);
        If CompSize > 0 Then
          Move(BundleData[DataPos + 1], ResumeData[1], CompSize);
      End;
    Pos := DataPos + Integer(CompSize);
    If (TorrentData <> '') And (ResumeData <> '') Then
      break;
  End;
  If TorrentData = '' Then
  Begin
    AError := SArchiveViewNoTorrent;
    exit;
  End;
  Result := True;
End;

Class Function TArchiveViewForm.ExtractTorrentFiles(Const BundleData: RawByteString;
  Const Hash: String; Out AName: String; Out ATotalSize: Int64;
  Out AError: String): TJSONArray;
Var
  TorrentData, ResumeData: RawByteString;
Begin
  Result := nil;
  AName := '';
  ATotalSize := 0;
  AError := '';
  If BundleData = '' Then
  Begin
    AError := SArchiveViewNoTorrent;
    exit;
  End;
  Try
    If Not ExtractStoredEntries(BundleData, TorrentData, ResumeData, AError) Then
      exit;
    Result := ParseTorrent(TorrentData, ResumeData, Hash, AName, ATotalSize, AError);
  Except
    On E: Exception Do
    Begin
      FreeAndNil(Result);
      AError := Format(SArchiveViewParseError, [E.Message]);
    End;
  End;
End;

Procedure TArchiveViewForm.SetFiles(Const AName: String; Files: TJSONArray;
  ATotalSize: Int64);
Var
  i, FileIndex: Integer;
Begin
  Caption := AName;
  txName.Caption := AName;
  If Files <> nil Then
    txInfo.Caption := Format(SArchiveViewInfo, [Files.Count, GetHumanSize(ATotalSize)])
  Else
    txInfo.Caption := '';
  If Files = nil Then
  Begin
    FFilesTree.FillTree(0, nil, nil, nil, True);
    exit;
  End;
  FFilesTree.FillTree(0, Files, nil, nil, True);
  FFilesTree.SetStateAll(cbChecked);
  For i := 0 To lvFiles.Items.Count - 1 Do
    If Not FFilesTree.IsFolder(i) Then
    Begin
      FileIndex := Integer(lvFiles.Items[idxFileId, i]);
      If (FileIndex >= 0) And (FileIndex < Files.Count) And
        (Files.Objects[FileIndex].Integers['wanted'] = 0) Then
        FFilesTree.Checked[i] := cbUnchecked;
    End;
  FFilesTree.UpdateSummary;
End;

Initialization
  {$I archiveviewform.lrs}

End.

