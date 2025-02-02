

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

Unit AddTorrent;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
StdCtrls, Spin, VarGrid, Grids, ButtonPanel, ExtCtrls, Buttons, BaseForm,
varlist, fpjson, StrUtils, DateUtils, LazUTF8;

resourcestring
SSize = 'Size';
SSelectDownloadFolder = 'Select a folder for download';
SInvalidName = 'Invalid name specified.';

Type 
  TFilesTree = Class;

  { TAddTorrentForm }

    TAddTorrentForm = Class(TBaseForm)
      DelButton: TBitBtn;
      btSelectAll: TButton;
      btSelectNone: TButton;
      btBrowse: TButton;
      Buttons: TButtonPanel;
      cbStartTorrent: TCheckBox;
      cbSequentialDownload: TCheckBox;
      cbDestFolder: TComboBox;
      edSaveAs: TEdit;
      edLabel: TEdit;
      edExtension: TEdit;
      gbSaveAs: TGroupBox;
      gbContents: TGroupBox;
      edPeerLimit: TSpinEdit;
      DiskSpaceTimer: TTimer;
      txSaveAs: TLabel;
      txLabel: TLabel;
      txSaveAs1: TLabel;
      txSize: TLabel;
      txDiskSpace: TLabel;
      txPeerLimit: TLabel;
      lvFiles: TVarGrid;
      txDestFolder: TLabel;
      Procedure btBrowseClick(Sender: TObject);
      Procedure btSelectAllClick(Sender: TObject);
      Procedure btSelectNoneClick(Sender: TObject);
      Procedure cbDestFolderChange(Sender: TObject);
      Procedure cbStartTorrentChange(Sender: TObject);
      Procedure cbSequentialDownloadChange(Sender: TObject);
      Procedure DelButtonClick(Sender: TObject);
      Procedure DiskSpaceTimerTimer(Sender: TObject);
      Procedure edSaveAsChange(Sender: TObject);
      Procedure edLabelChange(Sender: TObject);
      Procedure FormCreate(Sender: TObject);
      Procedure FormShow(Sender: TObject);
      Procedure OKButtonClick(Sender: TObject);
      Procedure SearchGoodExtension ();
      Function  GetTempate (ext:String; Var e: Array Of String): integer;
      Function  IsFileTemplate(filename:String; cntE : integer; e: Array Of
                               String): boolean;
      Function  CorrectPath (path: String): string;
      Procedure DeleteDirs(maxdel : Integer);

      Private 
        FDiskSpaceCaption: string;
        FTree: TFilesTree;
        Procedure TreeStateChanged(Sender: TObject);
        Procedure UpdateSize;
      Public 
        OrigCaption: string;
        Extension : string;
        property FilesTree: TFilesTree read FTree;
    End;

    TFolderInfo = Record
      Size: double;
      DoneSize: double;
      Priority: integer;
      chk: TCheckBoxState;
    End;

  { TFilesTree }

    TFilesTree = Class(TComponent)
      Private 
        FCheckboxes: boolean;
        FDownloadDir: string;
        FGrid: TVarGrid;
        FHasFolders: boolean;
        FIsPlain: boolean;
        FOnStateChange: TNotifyEvent;
        FFiles: TVarList;
        FTorrentId: integer;
        FLastFileCount: integer;
        //    LastFilterIdx:integer;
        FCommonPathLen: integer;
        FHasDone: boolean;
        FHasPriority: boolean;

        Procedure CollapseFolder(ARow: integer);
        Procedure DoCellAttributes(Sender: TVarGrid; ACol, ARow, ADataCol:
                                   integer; AState: TGridDrawState; Var
                                   CellAttribs: TCellAttributes);
        Procedure DoCheckBoxClick(Sender: TVarGrid; ACol, ARow, ADataCol:
                                  integer);
        Procedure DoDrawCell(Sender: TVarGrid; ACol, ARow, ADataCol: integer;
                             AState: TGridDrawState; Const R: TRect; Var
                             ADefaultDrawing: boolean);
        Procedure DoQuickSearch(Sender: TVarGrid; Var SearchText: String; Var
                                ARow: integer);
        Procedure DoTreeButtonClick(Sender: TVarGrid; ACol, ARow, ADataCol:
                                    integer);
        Procedure DoAfterSort(Sender: TObject);
        Procedure ExpandFolder(ARow: integer);
        Function GetChecked(ARow: integer): TCheckBoxState;
        Function GetExpanded(ARow: integer): boolean;
        Function GetLevel(ARow: integer): integer;
        Procedure SetCheckboxes(Const AValue: boolean);
        Procedure IntSetChecked(ARow: integer; Const AValue: TCheckBoxState);
        Procedure SetChecked(ARow: integer; Const AValue: TCheckBoxState);
        Procedure SetExpanded(ARow: integer; Const AValue: boolean);
        Procedure SetIsPlain(Const AValue: boolean);
        Procedure TreeChanged;
        Procedure DoOnStateChange;
        Function DoCompareVarRows(Sender: TVarList; Row1, Row2: PVariant;
                                  DescendingSort: boolean): integer;
        Procedure SetRowOption(ARow, AOption: integer; DoSet: boolean);
      Public 
        constructor Create(AGrid: TVarGrid);
        reintroduce;
        destructor Destroy;
        override;
        Function IsFolder(ARow: integer): boolean;
        Procedure CollapseAll;
        Procedure FillTree(ATorrentId: integer; files, priorities, wanted:
                           TJSONArray;RebuildTree:boolean);
        Procedure SetStateAll(AState: TCheckBoxState);
        Procedure EnsureRowVisible(ARow: integer);
        Function GetFullPath(ARow: integer; AbsolutePath: boolean = True):

                                                                          string
        ;
        Function GetIncompleteFullPath(ARow: integer; AbsolutePath: boolean =
                                       True): string;
        Function UpdateSummary: TFolderInfo;
        Procedure Clear;
        property Grid: TVarGrid read FGrid;
        property HasFolders: boolean read FHasFolders;
        property Checkboxes: boolean read FCheckboxes write SetCheckboxes;
        property IsPlain: boolean read FIsPlain write SetIsPlain;
        property DownloadDir: string read FDownloadDir write FDownloadDir;
        property Expanded[ARow: integer]: boolean read GetExpanded write
                                          SetExpanded;
        property Checked[ARow: integer]: TCheckBoxState read GetChecked write
                                         SetChecked;
        property RowLevel[ARow: integer]: integer read GetLevel;
        property OnStateChange: TNotifyEvent read FOnStateChange write
                                FOnStateChange;
    End;

    Const 
      // Files list columns
      idxFileName      = 0;
      idxFileSize      = 1;
      idxFileDone      = 2;
      idxFileProgress  = 3;
      idxFilePriority  = 4;
      idxFileId        = -4;
      idxFileId1       = 5;
      idxFileFullPath  = -1;
      idxFileLevel     = -2;
      idxFileIndex     = -3;

      FilesExtraColumns = 4;

    Implementation

    Uses lclintf, lcltype, main, variants, Utils, rpc, lclproc;

    Const 
      roChecked   = $030000;
      roCollapsed = $040000;
      roHidden    = $080000;
      roTag       = $100000;

      roCheckedShift = 16;

      TR_PRI_MIXED = -1001;
      // psedudo priority

{ TFilesTree }

      constructor TFilesTree.Create(AGrid: TVarGrid);
    Begin
      inherited Create(AGrid);
      FGrid := AGrid;
      FFiles := FGrid.Items;
      FGrid.OnCheckBoxClick := @DoCheckBoxClick;
      FGrid.OnTreeButtonClick := @DoTreeButtonClick;
      FGrid.OnCellAttributes := @DoCellAttributes;
      FGrid.OnAfterSort := @DoAfterSort;
      FGrid.OnQuickSearch := @DoQuickSearch;
      FGrid.OnDrawCell := @DoDrawCell;
    End;

    destructor TFilesTree.Destroy;
    Begin
      inherited Destroy;
    End;

    Function TFilesTree.IsFolder(ARow: integer): boolean;
    Begin
      Result := VarIsEmpty(FGrid.Items[idxFileId, ARow]);
    End;

    Procedure TFilesTree.CollapseAll;

    Var 
      i: integer;
    Begin
      FGrid.BeginUpdate;
      Try
        For i:=0 To FGrid.Items.Count - 1 Do
          Begin
            If IsFolder(i) Then
              SetRowOption(i, roCollapsed, True);
            If integer(FGrid.Items[idxFileLevel, i]) > 0 Then
              Begin
                FGrid.RowVisible[i] := False;
                FGrid.RowSelected[i] := False;
                SetRowOption(i, roHidden, True);
              End;
          End;
        TreeChanged;
      Finally
        FGrid.EndUpdate;
    End;
  End;

Procedure TFilesTree.FillTree(ATorrentId: integer; files, priorities, wanted:
                              TJSONArray;RebuildTree:boolean);

Procedure _AddFolders(list: TVarList; Const path: String; Var idx: integer; cnt,
                      level: integer);

Var 
  s, ss: string;
  j: integer;
  p: PChar;
  ww: widestring;
Begin
  While idx < cnt Do
    Begin

      ww := widestring(list[idxFileFullPath, idx]);

      s  := StringReplace(UTF8Encode(ww), ':', '_', [rfReplaceAll, rfIgnoreCase]
            );
      s  := ExtractFilePath(s);
      // fixed an incorrect search if there is a ":"

      If s = '' Then
        Begin
          Inc(idx);
          continue;
        End;
      If (path <> '') And (Pos(path, s) <> 1)  Then
        break;
      If s = path Then
        Begin
          list[idxFileLevel, idx] := level;
          Inc(idx);
        End
      Else
        Begin
          ss := Copy(s, Length(path) + 1, MaxInt);
          p := PChar(ss);
          While (p^ <> #0) And Not (p^ In ['/','\']) Do
            Inc(p);
          If p^ <> #0 Then
            Begin
              SetLength(ss, p - PChar(ss) + 1);
              j := list.Count;
              list[idxFileLevel, j] := level;
              list[idxFileFullPath, j] := UTF8Decode(path + ss);
              _AddFolders(list, path + ss, idx, cnt, level + 1);
              ss := ExcludeTrailingPathDelimiter(ss);
              list[idxFileName, j] := UTF8Decode(ExtractFileName(ss));
            End;
        End;
    End;
End;

Var 
  i, row: integer;
  FullRefresh: boolean;
  f: TJSONObject;
  s, ss, path: string;
  ff: double;
Begin
  If files = Nil Then
    Begin
      FGrid.Items.Clear;
      exit;
    End;
  FHasDone := FGrid.Columns.Count > idxFileDone;
  FHasPriority := FHasDone And (priorities <> Nil) And (wanted <> Nil);
  FullRefresh := (FTorrentId <> ATorrentId) Or (FLastFileCount <> files.Count)
                 Or RebuildTree;
  FLastFileCount := files.Count;

  FTorrentId := ATorrentId;
  FIsPlain := FGrid.SortColumn <> idxFileName;
  FFiles.BeginUpdate;
  Try
    FFiles.OnCompareVarRows := Nil;
    If FullRefresh Then
      FFiles.Clear
    Else
      Begin
        For i:=0 To FFiles.Count - 1 Do
          SetRowOption(i, roTag, False);
        FFiles.Sort(idxFileId);
      End;

    // Detecting top level folder to be removed
    FCommonPathLen := 0;
    path := '';
    If files.Count > 0 Then
      Begin
        s := UTF8Encode(files.Objects[0].Strings['name']);
        s := ExcludeInvalidChar(s);
        // petrov - Exclude prohibited characters
        FCommonPathLen := Pos(RemotePathDelimiter, s);
        If FCommonPathLen > 0 Then
          path := Copy(s, 1, FCommonPathLen);
      End;

    FHasFolders := False;
    For i:=0 To files.Count - 1 Do
      Begin
        f := files.Objects[i];
        If FullRefresh Then
          Begin
            row := i;
            FFiles[idxFileLevel, row] := 0;
          End
        Else
          If Not FFiles.Find(idxFileId, i, row) Then
            Begin
              FFiles.InsertRow(row);
              FFiles[idxFileLevel, row] := 0;
            End;
        SetRowOption(row, roTag, True);
        FFiles[idxFileId, row] := i;
        If FFiles.ColCnt>5 Then FFiles[idxFileId1, row] := i;

        s := UTF8Encode(f.Strings['name']);
        s := ExcludeInvalidChar(s);
        // petrov - Exclude prohibited characters
        ss := UTF8Decode(ExtractFilePath(s));
        FFiles[idxFileFullPath, row] := UTF8Decode(ExtractFilePath(s));
        If FCommonPathLen > 0 Then
          s := Copy(s, FCommonPathLen + 1, MaxInt);
        ss := ExtractFileName(s);
        If ss <> s Then
          FHasFolders := True;

        FFiles[idxFileName, row] := UTF8Decode(ss);
        ff := f.Floats['length'];
        FFiles[idxFileSize, row] := ff;

        If FHasDone Then
          Begin
            FFiles[idxFileDone, row] := f.Floats['bytesCompleted'];
            If ff = 0 Then
              ff := 100.0
            Else
              ff := double(FFiles[idxFileDone, row])*100.0/ff;
            FFiles[idxFileProgress, row] := Int(ff*10.0)/10.0;

            If FHasPriority Then
              Begin
                If wanted.Integers[i] = 0 Then
                  Begin
                    FFiles[idxFilePriority, row] := TR_PRI_SKIP;
                    IntSetChecked(row, cbUnchecked);
                  End
                Else
                  Begin
                    FFiles[idxFilePriority, row] := priorities.Integers[i];
                    IntSetChecked(row, cbChecked);
                  End;
              End;
          End;
      End;

    If Not FullRefresh Then
      Begin
        i := 0;
        While i < FFiles.Count Do
          If Not IsFolder(i) And Not LongBool(FFiles.RowOptions[i] And roTag)
            Then
            FFiles.Delete(i)
          Else
            Inc(i);
      End;

    If HasFolders And FullRefresh Then
      Begin
        FFiles.Sort(idxFileFullPath);
        i := 0;
        _AddFolders(FFiles, path, i, FFiles.Count, 0);
      End;

    FFiles.OnCompareVarRows := @DoCompareVarRows;
    FGrid.Sort;
    If FullRefresh And (FFiles.Count > 0) Then
      Begin
        FGrid.Row := 0;
        If HasFolders Then
          Begin
            i := FFiles.RowCnt + FGrid.FixedRows;
            If FGrid.RowCount <> i Then
              FGrid.RowCount := i;
            CollapseAll;
          End
        Else
          TreeChanged;
      End
    Else
      TreeChanged;
    If Not IsPlain Then
      UpdateSummary;
  Finally
    FFiles.EndUpdate;
End;
End;

Procedure TFilesTree.SetStateAll(AState: TCheckBoxState);

Var 
  i: integer;
Begin
  FFiles.BeginUpdate;
  Try
    For i:=0 To FFiles.Count - 1 Do
      IntSetChecked(i, AState);
  Finally
    FFiles.EndUpdate;
End;
DoOnStateChange;
End;

Procedure TFilesTree.EnsureRowVisible(ARow: integer);

Var 
  i, level: integer;
Begin
  If Not FGrid.RowVisible[ARow] Then
    Begin
      FGrid.BeginUpdate;
      Try
        level := FFiles[idxFileLevel, ARow] - 1;
        For i:=ARow Downto 0 Do
          Begin
            If IsFolder(i) And (FFiles[idxFileLevel, i] = level) Then
              Begin
                ExpandFolder(i);
                If level = 0 Then
                  break;
                Dec(level);
              End;
          End;
      Finally
        FGrid.EndUpdate;
    End;
End;
FGrid.EnsureRowVisible(ARow);
End;

Function TFilesTree.GetFullPath(ARow: integer; AbsolutePath: boolean): string;
Begin
  If AbsolutePath Then
    Begin
      Result := FDownloadDir;
      If Copy(Result, Length(Result), 1) <> RemotePathDelimiter Then
        Result := Result + RemotePathDelimiter;
    End
  Else
    Result := '';
  Result := Result + UTF8Encode(widestring(FFiles[idxFileFullPath, ARow]));

  If IsFolder(ARow) Then
    Result := Copy(Result, 1, Length(Result) - 1)
  Else
    Result := Result + UTF8Encode(widestring(FFiles[idxFileName, ARow]));
End;
Function TFilesTree.GetIncompleteFullPath(ARow: integer; AbsolutePath: boolean):

                                                                          string
;
Begin
  If AbsolutePath Then
    Begin
      Result := RpcObj.IncompleteDir;
      If Copy(Result, Length(Result), 1) <> RemotePathDelimiter Then
        Result := Result + RemotePathDelimiter;
    End
  Else
    Result := '';
  Result := Result + UTF8Encode(widestring(FFiles[idxFileFullPath, ARow]));

  If IsFolder(ARow) Then
    Result := Copy(Result, 1, Length(Result) - 1)
  Else
    Result := Result + UTF8Encode(widestring(FFiles[idxFileName, ARow]));
End;

Function TFilesTree.UpdateSummary: TFolderInfo;

Function _UpdateSummary(Var idx: integer; cnt, level: integer): TFolderInfo;

Var 
  i, j: integer;
  IsFirst: boolean;
Begin
  FillChar(Result, SizeOf(Result), 0);
  IsFirst := True;
  While idx < cnt Do
    Begin
      If FFiles[idxFileLevel, idx] <> level Then
        break;
      i := idx;
      Inc(idx);

      If IsFolder(i) Then
        Begin
          With _UpdateSummary(idx, cnt, level + 1) Do
            Begin
              FFiles[idxFileSize, i] := Size;
              If FHasDone Then
                Begin
                  FFiles[idxFileDone, i] := DoneSize;
                  If Size = 0 Then
                    DoneSize := 100.0
                  Else
                    DoneSize := DoneSize*100.0/Size;
                  FFiles[idxFileProgress, i] := Int(DoneSize*10.0)/10.0;
                End;
              If FHasPriority Then
                Begin
                  FFiles[idxFilePriority, i] := Priority;
                  IntSetChecked(i, chk);
                End;
            End;
        End;

      With Result Do
        Begin
          Size := Size + FFiles[idxFileSize, i];
          If FHasDone Then
            DoneSize := DoneSize + FFiles[idxFileDone, i];
          If FHasPriority Then
            Begin
              j := FFiles[idxFilePriority, i];
              If IsFirst Then
                Begin
                  IsFirst := False;
                  Priority := j;
                  chk := Checked[i];
                End
              Else
                Begin
                  If Priority <> j Then
                    Priority := TR_PRI_MIXED;
                  If chk <> Checked[i] Then
                    chk := cbGrayed;
                End;
            End;
        End;
    End;
End;

Var 
  i: integer;
Begin
  FFiles.BeginUpdate;
  Try
    i := 0;
    Result := _UpdateSummary(i, FFiles.Count, 0);
  Finally
    FFiles.EndUpdate;
End;
End;

Procedure TFilesTree.Clear;
Begin
  FLastFileCount := 0;
  FTorrentId := 0;
  FFiles.Clear;
End;

Procedure TFilesTree.DoCheckBoxClick(Sender: TVarGrid; ACol, ARow, ADataCol:
                                     integer);
Begin
  If Checked[ARow] = cbChecked Then
    Checked[ARow] := cbUnchecked
  Else
    Checked[ARow] := cbChecked;
End;

Procedure TFilesTree.DoTreeButtonClick(Sender: TVarGrid; ACol, ARow, ADataCol:
                                       integer);
Begin
  Expanded[ARow] := Not Expanded[ARow];
End;

Procedure TFilesTree.DoAfterSort(Sender: TObject);

Var 
  p: boolean;
Begin
  p := FGrid.SortColumn <> idxFileName;
  If p <> IsPlain Then
    IsPlain := p
  Else
    TreeChanged;
End;

Procedure TFilesTree.CollapseFolder(ARow: integer);

Var 
  i, lev: integer;
Begin
  AppBusy;
  FGrid.BeginUpdate;
  Try
    lev := FGrid.Items[idxFileLevel, ARow];
    SetRowOption(ARow, roCollapsed, True);
    For i:=ARow + 1 To FGrid.Items.Count - 1 Do
      If integer(FGrid.Items[idxFileLevel, i]) > lev Then
        Begin
          FGrid.RowVisible[i] := False;
          FGrid.RowSelected[i] := False;
          SetRowOption(i, roHidden, True);
        End
      Else
        break;
    TreeChanged;
  Finally
    FGrid.EndUpdate;
End;
AppNormal;
End;

Procedure TFilesTree.ExpandFolder(ARow: integer);

Var 
  i, j, lev: integer;
Begin
  AppBusy;
  FGrid.BeginUpdate;
  Try
    lev := FGrid.Items[idxFileLevel, ARow] + 1;
    SetRowOption(ARow, roCollapsed, False);
    For i:=ARow + 1 To FGrid.Items.Count - 1 Do
      Begin
        j := integer(FGrid.Items[idxFileLevel, i]);
        If j = lev Then
          Begin
            FGrid.RowVisible[i] := True;
            SetRowOption(i, roHidden, False);
            If IsFolder(i) And Expanded[i] Then
              ExpandFolder(i);
          End
        Else
          If j <= lev Then
            break;
      End;
    TreeChanged;
  Finally
    FGrid.EndUpdate;
End;
AppNormal;
End;

Function TFilesTree.GetChecked(ARow: integer): TCheckBoxState;
Begin
  Result := TCheckBoxState((FFiles.RowOptions[ARow] And roChecked) shr
            roCheckedShift);
End;

Function TFilesTree.GetExpanded(ARow: integer): boolean;
Begin
  Result := Not LongBool(FFiles.RowOptions[ARow] And roCollapsed);
End;

Function TFilesTree.GetLevel(ARow: integer): integer;
Begin
  Result := FFiles[idxFileLevel, ARow];
End;

Procedure TFilesTree.SetCheckboxes(Const AValue: boolean);
Begin
  If FCheckboxes = AValue Then exit;
  FCheckboxes := AValue;
End;

Procedure TFilesTree.IntSetChecked(ARow: integer; Const AValue: TCheckBoxState);
Begin
  FFiles.RowOptions[ARow] := (FFiles.RowOptions[ARow] And Not roChecked) Or (
                             integer(AValue) shl roCheckedShift);
End;

Procedure TFilesTree.SetChecked(ARow: integer; Const AValue: TCheckBoxState);

Var 
  i, lev: integer;
  st: TCheckBoxState;
Begin
  st := AValue;
  If st = cbGrayed Then
    st := cbUnchecked;
  If Checked[ARow] = st Then
    exit;
  IntSetChecked(ARow, st);
  FGrid.InvalidateRow(ARow + FGrid.FixedRows);

  If Not IsPlain Then
    Begin
      lev := integer(FFiles[idxFileLevel, ARow]);

      If IsFolder(ARow) Then
        Begin
          FFiles.BeginUpdate;
          For i:=ARow + 1 To FFiles.Count - 1 Do
            If integer(FFiles[idxFileLevel, i]) <= lev Then
              break
            Else
              IntSetChecked(i, st);
          FFiles.EndUpdate;
        End;

      If lev > 0 Then
        Begin
          i := ARow + 1;
          While (i < FFiles.Count) And (integer(FFiles[idxFileLevel, i]) >= lev)
            Do
            Inc(i);

          For i:=i - 1 Downto 0 Do
            Begin
              If IsFolder(i) And (integer(FFiles[idxFileLevel, i]) < lev) Then
                Begin
                  IntSetChecked(i, st);
                  FGrid.InvalidateRow(i + FGrid.FixedRows);
                  Dec(lev);
                  If lev = 0 Then
                    break;
                End
              Else
                If Checked[i] <> st Then
                  st := cbGrayed;
            End;
        End;
    End;
  DoOnStateChange;
End;

Procedure TFilesTree.SetExpanded(ARow: integer; Const AValue: boolean);
Begin
  If GetExpanded(ARow) <> AValue Then
    If AValue Then
      ExpandFolder(ARow)
  Else
    CollapseFolder(ARow);
End;

Procedure TFilesTree.SetIsPlain(Const AValue: boolean);
Begin
  If FIsPlain = AValue Then exit;
  FIsPlain := AValue;
  FFiles.BeginUpdate;
  Try
    TreeChanged;
    If Not FIsPlain Then
      UpdateSummary;
  Finally
    FFiles.EndUpdate;
End;
If FFiles.Count > 0 Then
  FGrid.Row := 0;
End;

Procedure TFilesTree.TreeChanged;

Var 
  i, j: integer;
  f: boolean;
Begin
  FGrid.Items.BeginUpdate;
  Try
    FGrid.RowCount := FFiles.RowCnt + FGrid.FixedRows;
    j := 0;
    For i:=0 To FGrid.Items.Count - 1 Do
      Begin
        If IsPlain Then
          f := Not IsFolder(i)
        Else
          f := Not LongBool(FFiles.RowOptions[i] And roHidden);
        FGrid.RowVisible[i] := f;
        If f Then
          Begin
            FGrid.Items[idxFileIndex, i] := j;
            Inc(j);
          End;
      End;
  Finally
    FGrid.Items.EndUpdate;
End;
End;

Procedure TFilesTree.DoOnStateChange;
Begin
  If Assigned(FOnStateChange) Then
    FOnStateChange(Self);
End;

Function TFilesTree.DoCompareVarRows(Sender: TVarList; Row1, Row2: PVariant;
                                     DescendingSort: boolean): integer;
//var
//v:variant;
Begin
  If FGrid.SortColumn <> idxFileName Then
    Begin


//    Result:=(integer(VarIsEmpty(Sender.GetRowItem(Row1, idxFileId))) and 1) - (integer(VarIsEmpty(Sender.GetRowItem(Row2, idxFileId))) and 1);
      Result := CompareVariants(Sender.GetRowItem(Row1, FGrid.SortColumn),
                Sender.GetRowItem(Row2, FGrid.SortColumn));
      If FGrid.SortColumn = idxFilePriority Then
        Result := -Result;
      If DescendingSort Then
        Result := -Result;
      If Result <> 0 Then
        exit;
      Result := CompareVariants(Sender.GetRowItem(Row1, idxFileFullPath), Sender
                .GetRowItem(Row2, idxFileFullPath));
      If DescendingSort Then
        Result := -Result;
      exit;
    End;

  Result := CompareVariants(Sender.GetRowItem(Row1, idxFileFullPath), Sender.
            GetRowItem(Row2, idxFileFullPath));
  If DescendingSort Then
    Result := -Result;
  If Result <> 0 Then
    exit;
  Result := (integer(VarIsEmpty(Sender.GetRowItem(Row2, idxFileId))) And 1) - (
            integer(VarIsEmpty(Sender.GetRowItem(Row1, idxFileId))) And 1);
  If DescendingSort Then
    Result := -Result;
  If Result <> 0 Then
    exit;
  Result := CompareVariants(Sender.GetRowItem(Row1, idxFileName), Sender.
            GetRowItem(Row2, idxFileName));
  If DescendingSort Then
    Result := -Result;
End;

Procedure TFilesTree.SetRowOption(ARow, AOption: integer; DoSet: boolean);

Var 
  i: integer;
Begin
  i := FFiles.RowOptions[ARow];
  If DoSet Then
    FFiles.RowOptions[ARow] := i Or AOption
  Else
    FFiles.RowOptions[ARow] := i And Not AOption;
End;

Procedure TFilesTree.DoCellAttributes(Sender: TVarGrid; ACol, ARow, ADataCol:
                                      integer; AState: TGridDrawState; Var
                                      CellAttribs: TCellAttributes);

Var 
  i: integer;
Begin
  If ARow < 0 Then exit;
  With CellAttribs Do
    Begin
      If Not (gdSelected In AState) And (integer(Sender.Items[idxFileIndex, ARow
         ]) And 1 = 1) Then
        Sender.Canvas.Brush.Color := FAlterColor;
      If Text = '' Then exit;
      Case ADataCol Of 
        0:
           Begin


//        Text:=(Sender.Items[idxFileFullPath, ARow]) + ' (' + Text + ')';// Lazarus 1.4.4

             If Checkboxes Then
               Begin
                 Options := [coDrawCheckBox];
                 State := Checked[ARow];
               End;
             If IsPlain Then
               Begin
                 Text := Copy(UTF8Encode(widestring(Sender.Items[idxFileFullPath
                         , ARow])), FCommonPathLen + 1, MaxInt) + Text;
               End
             Else
               Begin
                 Indent := integer(Sender.Items[idxFileLevel, ARow])*16;
                 If IsFolder(ARow) Then
                   Begin
                     Include(Options, coDrawTreeButton);
                     Expanded := Self.Expanded[ARow];
                     ImageIndex := 22;
                   End
                 Else
                   If HasFolders Then
                     Inc(Indent, Sender.RowHeights[ARow + Sender.FixedRows]);
               End;
           End;
        idxFileSize, idxFileDone:
                                  Text := (GetHumanSize(double(Sender.Items[
                                          ADataCol, ARow])));
        idxFileProgress:
                         Text := (Format('%.1f%%', [double(Sender.Items[ADataCol
                                 , ARow])]));
        idxFilePriority:
                         Begin
                           i := Sender.Items[idxFilePriority, ARow];
                           If i = TR_PRI_MIXED Then
                             Text := ''
                           Else
                             Text := (PriorityToStr(i, ImageIndex));
                         End;
      End;
    End;
End;

Procedure TFilesTree.DoDrawCell(Sender: TVarGrid; ACol, ARow, ADataCol: integer;
                                AState: TGridDrawState; Const R: TRect;
                                Var ADefaultDrawing: boolean);
Begin
  If ARow < 0 Then exit;
  If ADataCol = idxFileProgress Then
    Begin
      ADefaultDrawing := False;
      DrawProgressCell(Sender, ACol, ARow, ADataCol, AState, R);
    End;
End;

Procedure TFilesTree.DoQuickSearch(Sender: TVarGrid; Var SearchText: String; Var
                                   ARow: integer);

Var 
  i: integer;
  s: string;
  v: variant;
Begin
  s := LazUTF8.UTF8UpperCase(SearchText);
  For i:=ARow To Sender.Items.Count - 1 Do
    Begin
      v := Sender.Items[idxFileName, i];
      If VarIsEmpty(v) Or VarIsNull(v) Or (IsPlain And IsFolder(i)) Then
        continue;
      If Pos(s, UTF8Trim(LazUTF8.UTF8UpperCase((widestring(v))))) > 0 Then
        Begin
          ARow := i;
          EnsureRowVisible(ARow);
          break;
        End;
    End;
End;

{ TAddTorrentForm }

Procedure TAddTorrentForm.FormShow(Sender: TObject);
Begin
  AppBusy;
  lvFiles.BeginUpdate;
  Try
    btSelectAllClick(Nil);
{
    lvFiles.Sort;
    if lvFiles.Items.Count > 0 then
      lvFiles.Row:=0;
}
    //    FTree.CollapseAll;
  Finally
    lvFiles.EndUpdate;
End;

// Search good extension
SearchGoodExtension ();

DiskSpaceTimerTimer(Nil);
AppNormal;
End;

Function TAddTorrentForm.CorrectPath (path: String): string;

Var 
  l_old: integer;
Begin
  path  := StringReplace(path, '//', '/', [rfReplaceAll, rfIgnoreCase]);
  Result := path;
  l_old := length(path);
  If l_old >= 1 Then
    Begin
      If path[l_old]='/' Then
        path := MidStr(path,1,l_old-1);
      Result := path;
    End;
End;

Procedure TAddTorrentForm.DeleteDirs(maxdel : Integer);

Var 
  i,min,max,indx, fldr: integer;
  pFD : FolderData;
Begin
  max := Ini.ReadInteger('Interface', 'MaxFoldersHistory',  50);
  Ini.WriteInteger    ('Interface', 'MaxFoldersHistory', max);
  // PETROV

  Try
    While (cbDestFolder.Items.Count+maxdel) > max Do
      Begin
        min := 9999999;
        indx := -1;
        For i:=0 To cbDestFolder.Items.Count - 1 Do
          Begin
            pFD := cbDestFolder.Items.Objects[i] as FolderData;

            fldr := DaysBetween(SysUtils.Date,pFD.Lst);
            If SysUtils.Date > pFD.Lst Then
              fldr := 0- fldr;

            fldr := fldr + pFD.Hit;
            If fldr < min Then
              Begin
                min := fldr;
                indx := i;
              End;
          End;

        If indx > -1 Then
          cbDestFolder.Items.Delete(indx);
      End;
  Except
    MessageDlg('Error: LS-001. Please contact the developer', mtError, [mbOK], 0
    );
End;
End;

Procedure TAddTorrentForm.OKButtonClick(Sender: TObject);

Var 
  s,e : string;
  i : integer;
  pFD : FolderData;
Begin

  s := CorrectPath(cbDestFolder.Text);
  e := edExtension.Text;
  Try
    DeleteDirs (0);
    // check count items
    i := cbDestFolder.Items.IndexOf(s);
    If i < 0 Then
      Begin
        DeleteDirs (1);
        // prepare for new item
        cbDestFolder.Items.Add (s);
        i := cbDestFolder.Items.IndexOf(s);
        cbDestFolder.ItemIndex := i;


// Re-set item index in case DeleteDirs actually deleted a dir and removed the text from the combobox
        pFD    := FolderData.create;
        pFD.Hit := 1;
        pFD.Ext := e;
        pFD.Txt := s;
        pFD.Lst := SysUtils.Date;
        cbDestFolder.Items.Objects[i] := pFD;
      End
    Else
      Begin
        pFD    := cbDestFolder.Items.Objects[i] as FolderData;
        pFD.Hit := pFD.Hit + 1;
        pFD.Ext := e;
        pFD.Txt := s;
        pFD.Lst := SysUtils.Date;
        cbDestFolder.Items.Objects[i] := pFD;
        If cbDestFolder.ItemIndex < 0 Then
          Begin
            // as above, if DeleteDirs ended up deleting stuff...
            cbDestFolder.ItemIndex := i;
          End;
      End;
  Except
    MessageDlg('Error: LS-002. Please contact the developer', mtError, [mbOK], 0
    );
End;

// petrov - Exclude prohibited characters
edSaveAs.Text := ExcludeInvalidChar(edSaveAs.Text);

If edSaveAs.Enabled Then
  Begin
    edSaveAs.Text := Trim(edSaveAs.Text);

    If edSaveAs.Text = '' Then
      Begin
        edSaveAs.SetFocus;
        MessageDlg(SInvalidName, mtError, [mbOK], 0);
        exit;
      End;
  End;
ModalResult := mrOK;
End;

Procedure TAddTorrentForm.UpdateSize;

Var 
  i: integer;
  d, sz, tsz: double;
  s: string;
Begin
  sz := 0;
  tsz := 0;
  For i:=0 To lvFiles.Items.Count - 1 Do
    If Not FTree.IsFolder(i) Then
      Begin
        d := double(lvFiles.Items[idxFileSize, i]);
        tsz := tsz + d;
        If FTree.Checked[i] = cbChecked Then
          sz := sz + d;
      End;

  s := GetHumanSize(sz);
  If s <> GetHumanSize(tsz) Then
    s := s + ' / ' + GetHumanSize(tsz);
  txSize.Caption := Format('%s: %s', [SSize, s]);
End;

Procedure TAddTorrentForm.btSelectAllClick(Sender: TObject);
Begin
  FTree.SetStateAll(cbChecked);
End;

Procedure TAddTorrentForm.btBrowseClick(Sender: TObject);

Var 
  s: string;
Begin
  s := MainForm.SelectRemoteFolder(cbDestFolder.Text, SSelectDownloadFolder);
  If s <> '' Then
    Begin
      cbDestFolder.Text := s;
      cbDestFolderChange (Nil);
    End;
End;

Procedure TAddTorrentForm.btSelectNoneClick(Sender: TObject);
Begin
  FTree.SetStateAll(cbUnchecked);
End;

Procedure TAddTorrentForm.cbDestFolderChange(Sender: TObject);

Var 
  s : string;
  i : integer;
  pFD : FolderData;
Begin
  s := cbDestFolder.Text;
  i := cbDestFolder.Items.IndexOf(s);
  If i < 0 Then
    Begin
      edExtension.Text := '';
    End
  Else
    Begin
      pFD             := cbDestFolder.Items.Objects[i] as FolderData;
      edExtension.Text := pFD.Ext;
    End;
  DiskSpaceTimer.Enabled := True;
End;

Procedure TAddTorrentForm.cbStartTorrentChange(Sender: TObject);
Begin
  Ini.WriteBool('Interface', 'StartTorrentOnAdd', cbStartTorrent.Checked);
  If (cbStartTorrent.Checked = false) Then
    Begin
      cbStartTorrent.Font.Style := [fsbold];
      Buttons.OKButton.Font.Style := [fsbold]
    End
  Else
    Begin
      cbStartTorrent.Font.Style := [];
      Buttons.OKButton.Font.Style := []
    End;
End;
Procedure TAddTorrentForm.cbSequentialDownloadChange(Sender: TObject);
Begin
  Ini.WriteBool('Interface', 'SequentialDownload', cbSequentialDownload.Checked)
  ;
  If (cbSequentialDownload.Checked = false) Then
    Begin
      cbSequentialDownload.Font.Style := [fsbold];
      Buttons.OKButton.Font.Style := [fsbold]
    End
  Else
    Begin
      cbSequentialDownload.Font.Style := [];
      Buttons.OKButton.Font.Style := []
    End;
End;

Procedure TAddTorrentForm.DelButtonClick(Sender: TObject);

Var 
  i: integer;
  s : string;
Begin
  If cbDestFolder.Items.Count > 1 Then
    Begin
      s := CorrectPath(cbDestFolder.Text);
      i := cbDestFolder.Items.IndexOf(s);
      If i > -1 Then
        Begin
          cbDestFolder.Items.Delete(i);
          cbDestFolder.ItemIndex := 0;
        End;
    End;
End;

Procedure TAddTorrentForm.DiskSpaceTimerTimer(Sender: TObject);

Var 
  f: double;
  req, args: TJSONObject;
Begin
  DiskSpaceTimer.Enabled := False;
  If RpcObj.RPCVersion < 15 Then
    exit;
  AppBusy;
  f := -1;
  Try
    req := TJSONObject.Create;
    args := TJSONObject.Create;
    Try
      req.Add('method', 'free-space');
      args.Add('path', UTF8Decode(cbDestFolder.Text));
      req.Add('arguments', args);
      args := RpcObj.SendRequest(req);
      If args <> Nil Then
        f := args.Floats['size-bytes'];
      RpcObj.Status := '';
    Finally
      args.Free;
      req.Free;
End;
Except
  f := -1;
End;
txDiskSpace.Caption := FDiskSpaceCaption + ' ' + GetHumanSize(f);
AppNormal;
End;

Procedure TAddTorrentForm.edSaveAsChange(Sender: TObject);
Begin
  Caption := OrigCaption + ' - ' + edSaveAs.Text;
End;
Procedure TAddTorrentForm.edLabelChange(Sender: TObject);
Begin
  //     Ini.WriteString('Interface', 'Label', edLabel.Text);
End;

Procedure TAddTorrentForm.TreeStateChanged(Sender: TObject);
Begin
  UpdateSize;
End;

Function  TAddTorrentForm.GetTempate (ext:String; Var e: Array Of String):

                                                                         integer
;

Var 
  tmp,exten : string;
  i,n : integer;
Begin
  tmp   := ext;
  exten := ext;
  n     := 0;
  While tmp <> '' Do
    Begin
      i := Pos (' ', tmp);
      If (i <> 0) Then
        Begin
          exten := Trim (Copy (tmp, 1, i-1));
          tmp  := Trim (Copy (tmp, i, 999));
        End
      Else
        Begin
          exten := Trim (tmp);
          tmp  := '';
        End;
      e[n] := exten;
      n   := n+1;
    End;
  GetTempate := n;
End;

Function  TAddTorrentForm.IsFileTemplate(filename:String; cntE : integer; e:
                                         Array Of String): boolean;

Var 
  tmp, tmpExt, tmp_Name, sstr: string;
  i,n,lstr,j,k, total_sstr, total_templ : integer;
  ok : boolean;
  re : boolean;
Begin
  IsFileTemplate := false;

  tmp := filename;
  lstr := Length(tmp);
  For i:=1 To lstr-1 Do
    Begin
      If (tmp[lstr-i]= '/') Or (tmp[lstr-i]= '\') Then
        Begin
          tmp := Copy (tmp, lstr-i+1, 999);
          Break;
        End;
    End;

  For i:=0 To cntE-1 Do
    Begin
      tmpExt     := e[i];
      tmp_Name   := tmp;
      total_sstr := 0;
      total_templ := 0;
      While tmpExt <> '' Do
        Begin
          j := Pos ('*', tmpExt);
          If j <> 0 Then
            Begin
              sstr  := Copy (tmpExt, 1, j-1);
              tmpExt := Copy (tmpExt, j+1, 999);
              re    := false;
            End
          Else
            Begin
              sstr  := Trim (tmpExt);
              tmpExt := '';
              re    := true;
            End;
          If sstr = '' Then continue;

          total_templ := total_templ +1;
          n           := Length(sstr);
          ok          := false;
          While 1 = 1 Do
            Begin
              If tmp_Name = '' Then break;
              k := Pos (sstr, tmp_Name);
              If k <> 0 Then
                Begin
                  tmp_Name    := Copy (tmp_Name, k+n, 999);
                  If ((tmpExt ='') And (re=true)) And (tmp_Name <> '') Then
                    Begin
                      continue;
                    End
                  Else
                    Begin
                      total_sstr := total_sstr +1;
                      ok         := true;
                      Break;
                    End;
                End
              Else
                Begin
                  Break;
                End;
            End;
          If ok = true Then break;
        End;

      If total_sstr = total_templ Then
        Begin
          IsFileTemplate := true;
          Break;
        End;
    End;
End;


Procedure TAddTorrentForm.SearchGoodExtension ();

Var 
  i,j, jMax, torrMax : integer;
  s, filename : string;
  filesize, dTotal,dTotalMax : double;
  pFD : FolderData;
  e : array [0..50] Of string;
  n : integer;
Begin
  dTotalMax := 0;
  jMax      := -1;

  Try
    For j:=0 To cbDestFolder.Items.Count-1 Do
      Begin
        pFD  := cbDestFolder.Items.Objects[j] as FolderData;
        s    := Trim(pFD.Ext);
        If s = '' Then continue;

        n      := GetTempate (AnsiLowerCase(s), e);
        dTotal := 0;
        torrMax := lvFiles.Items.Count;
        If torrMax > 100 Then torrMax := 100;

        For i:=0 To torrMax - 1 Do
          Begin
            If Not FTree.IsFolder(i) Then
              Begin
                filename := lvFiles.Items[idxFileName, i];
                filesize := double(lvFiles.Items[idxFileSize, i]);
                If IsFileTemplate(filename, n,e) = true Then
                  dTotal := dTotal + filesize;
              End;
          End;

        If (dTotal > 0) And (dTotal > dTotalMax) Then
          Begin
            dTotalMax := dTotal;
            jMax      := j;
          End;
      End;
  Except
    MessageDlg('Error: LS-003. Please contact the developer', mtError, [mbOK], 0
    );
End;

Try
  If jMax <> -1 Then
    Begin
      pFD  := cbDestFolder.Items.Objects[jMax] as FolderData;
      cbDestFolder.ItemIndex := jMax;
      cbDestFolder.Text := pFD.Txt;
      cbDestFolderChange(Nil);
    End
  Else
    Begin
      cbDestFolderChange(Nil);
    End;
Except
  MessageDlg('Error: LS-004. Please contact the developer', mtError, [mbOK], 0);
End;
End;

Procedure TAddTorrentForm.FormCreate(Sender: TObject);
Begin
  OrigCaption := Caption;
  FDiskSpaceCaption := txDiskSpace.Caption;
  lvFiles.Items.ExtraColumns := FilesExtraColumns;
  FTree := TFilesTree.Create(lvFiles);
  FTree.Checkboxes := True;
  FTree.OnStateChange := @TreeStateChanged;
  Buttons.OKButton.ModalResult := mrNone;
  bidiMode := GetBiDi();
  cbStartTorrent.Checked := Ini.ReadBool('Interface', 'StartTorrentOnAdd', true)
  ;
  cbSequentialDownload.Checked := Ini.ReadBool('Interface', 'SequentialDownload'
                                  , true);
  //  edLabel.Text:=Ini.ReadString('Interface', 'Label', '');
  If RpcObj.RPCVersion < 18 Then
    cbSequentialDownload.Visible := false
  Else
    cbSequentialDownload.Visible := true;
  If (cbStartTorrent.Checked = false) Then
    Begin
      cbStartTorrent.Font.Style := [fsbold];
      Buttons.OKButton.Font.Style := [fsbold]
    End
  Else
    Begin
      cbStartTorrent.Font.Style := [];
      Buttons.OKButton.Font.Style := []
    End;

{$ifdef windows}
  gbSaveAs.Caption := '';
{$endif windows}
{$ifdef darwin}
  Buttons.BorderSpacing.Right := Buttons.BorderSpacing.Right + ScaleInt(12);
{$endif darwin}
End;

initialization
  {$I addtorrent.lrs}

End.
