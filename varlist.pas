

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

Unit varlist;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, variants;

Type
  TVarList = Class;

  TCompareVarRowsEvent = Function(Sender: TVarList; Row1, Row2: PVariant;
    DescendingSort: Boolean): Integer Of Object;

  { TVarList }

  TVarList = Class(TList)
  Private
    FColCnt: Integer;
    FExtraColumns: Integer;
    FOnCompareVarRows: TCompareVarRowsEvent;
    FOnDataChanged: TNotifyEvent;
    FUpdateLockCnt: Integer;
    Function GetItemPtr(ACol, ARow: Integer): PVariant;
    Function GetItems(ACol, ARow: Integer): Variant;
    Function GetRowCnt: Integer;
    Function GetRowOptions(ARow: Integer): Integer;
    Function GetRows(ARow: Integer): PVariant;
    Function GetRow(ARow: Integer): PVariant;
    Procedure SetColCnt(Const AValue: Integer);
    Procedure SetExtraColumns(Const AValue: Integer);
    Procedure SetItems(ACol, ARow: Integer; Const AValue: Variant);
    Procedure SetRowCnt(Const AValue: Integer);
    Procedure SetRowOptions(ARow: Integer; Const AValue: Integer);
    Function IntCols: Integer;
    Procedure CheckColIndex(ColIndex: Integer);

  Protected
    Procedure DoDataChanged; Virtual;
  Public
    Constructor Create(AColCnt, ARowCnt: Integer);
    Destructor Destroy; Override;
    Procedure Clear; Override;
    Procedure Delete(Index: Integer);
    Procedure Sort(ACol: Integer; Descending: Boolean = False); Reintroduce;
    Function IndexOf(ACol: Integer; Const Value: Variant): Integer;
    Function SortedIndexOf(ACol: Integer; Const Value: Variant): Integer;
    Function Find(ACol: Integer; Const Value: Variant; Var Index: Integer): Boolean;
    Procedure BeginUpdate;
    Procedure EndUpdate;
    Procedure InsertRow(ARow: Integer);
    Function IsUpdating: Boolean;
    Function GetRowItem(ARow: PVariant; ACol: Integer): Variant;
    Property Items[ACol, ARow: Integer]: Variant read GetItems write SetItems; Default;
    Property ItemPtrs[ACol, ARow: Integer]: PVariant read GetItemPtr;
    Property Rows[ARow: Integer]: PVariant read GetRows;
    Property RowOptions[ARow: Integer]: Integer read GetRowOptions write SetRowOptions;
    Property ColCnt: Integer read FColCnt write SetColCnt;
    Property RowCnt: Integer read GetRowCnt write SetRowCnt;
    Property Count: Integer read GetRowCnt;
    Property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
    Property OnCompareVarRows: TCompareVarRowsEvent
      read FOnCompareVarRows write FOnCompareVarRows;
    Property ExtraColumns: Integer read FExtraColumns write SetExtraColumns;
  End;

Function CompareVariants(Const v1, v2: Variant): Integer;

Implementation

Uses Math;

  { TVarList }

Function TVarList.GetItems(ACol, ARow: Integer): Variant;
Begin
  CheckColIndex(ACol);
  Result := GetRow(ARow)[ACol + IntCols];
End;

Function TVarList.GetItemPtr(ACol, ARow: Integer): PVariant;
Begin
  CheckColIndex(ACol);
  Result := GetRow(ARow) + (ACol + IntCols);
End;

Function TVarList.GetRowCnt: Integer;
Begin
  Result := Inherited GetCount;
End;

Function TVarList.GetRowOptions(ARow: Integer): Integer;
Begin
  Result := GetRow(ARow)[0];
End;

Function TVarList.GetRows(ARow: Integer): PVariant;
Begin
  Result := GetRow(ARow);
End;

Function TVarList.GetRow(ARow: Integer): PVariant;
Var
  v: PVariant;
  sz: Integer;
Begin

  If ARow < 0 Then ARow := 0;

  If ARow >= Count Then
    SetRowCnt(ARow + 1);
  v := Get(ARow);
  If v = nil Then
  Begin
    sz := SizeOf(Variant) * (FColCnt + IntCols);
    v := GetMem(sz);
    FillChar(v^, sz, 0);
    v[0] := 0;
    Put(ARow, v);
  End;
  Result := v;
End;

Procedure TVarList.SetColCnt(Const AValue: Integer);
Var
  i, j, ocnt, ncnt: Integer;
  p: PVariant;
Begin
  If FColCnt = AValue Then exit;
  ocnt := FColCnt + IntCols;
  FColCnt := AValue;
  ncnt := FColCnt + IntCols;
  For i := 0 To Count - 1 Do
  Begin
    p := GetRow(i);
    For j := ncnt To ocnt - 1 Do
      VarClear(p[j]);
    ReAllocMem(p, ncnt * SizeOf(Variant));
    If ncnt > ocnt Then
      FillChar(p[ocnt], (ncnt - ocnt) * SizeOf(Variant), 0);
  End;
End;

Procedure TVarList.SetExtraColumns(Const AValue: Integer);
Begin
  If FExtraColumns = AValue Then exit;
  If RowCnt <> 0 Then
    Raise Exception.Create('Unable to set extra columns.');
  FExtraColumns := AValue;
End;

Procedure TVarList.SetItems(ACol, ARow: Integer; Const AValue: Variant);
Begin
  GetRow(ARow)[ACol + IntCols] := AValue;
  DoDataChanged;
End;

Procedure TVarList.SetRowCnt(Const AValue: Integer);
Begin
  BeginUpdate;
  Try
    While Count > AValue Do
      Delete(Count - 1);
    SetCount(AValue);
  Finally
    EndUpdate;
  End;
End;

Procedure TVarList.SetRowOptions(ARow: Integer; Const AValue: Integer);
Begin
  GetRow(ARow)[0] := AValue;
End;

Function TVarList.IntCols: Integer;
Begin
  Result := FExtraColumns + 1;
End;

Procedure TVarList.CheckColIndex(ColIndex: Integer);
Begin
  If (ColIndex + IntCols < 0) Or (ColIndex >= ColCnt) Then
    Raise Exception.CreateFmt('Invalid column index (%d).', [ColIndex]);
End;

Procedure TVarList.DoDataChanged;
Begin
  If Assigned(FOnDataChanged) And (FUpdateLockCnt = 0) Then
    FOnDataChanged(Self);
End;

Constructor TVarList.Create(AColCnt, ARowCnt: Integer);
Begin
  Inherited Create;
  FColCnt := AColCnt;
  RowCnt := ARowCnt;
End;

Destructor TVarList.Destroy;
Begin
  FOnDataChanged := nil;
  Inherited Destroy;
End;

Procedure TVarList.Clear;
Var
  i: Integer;
  v: PVariant;
Begin
  For i := 0 To Count - 1 Do
  Begin
    v := Inherited Get(i);
    If v <> nil Then
    Begin
      VarClear(v^);
      FreeMem(v);
    End;
  End;
  Inherited Clear;
  DoDataChanged;
End;

Procedure TVarList.Delete(Index: Integer);
Var
  v: PVariant;
  i: Integer;
Begin
  v := Inherited Get(Index);
  If v <> nil Then
  Begin
    For i := 0 To ColCnt + IntCols - 1 Do
      VarClear(v[i]);
    FreeMem(v);
  End;
  Inherited Delete(Index);
  DoDataChanged;
End;

Function CompareVariants(Const v1, v2: Variant): Integer;
Var
  v1e, v2e: Boolean;
  d1, d2: Double;
Begin
  v1e := VarIsNull(v1) Or VarIsEmpty(v1);
  v2e := VarIsNull(v2) Or VarIsEmpty(v2);
  If v1e And v2e Then
    Result := 0
  Else
    If v1e And Not v2e Then
      Result := -1
    Else
      If Not v1e And v2e Then
        Result := 1
      Else
      Begin
        Case VarType(v1) Of
          varInteger, varsmallint, varshortint, varbyte, varword,
          varlongword, varint64,
          varqword:
          Begin
            d1 := Int64(v1);
          End;
          varDouble, varSingle, varDate:
          Begin
            d1 := Double(v1);
            Result := Sign(Double(v1) - Double(v2));
          End
          Else
            Result := AnsiCompareText(v1, v2);
            exit;
        End;
        Case VarType(v2) Of
          varInteger, varsmallint, varshortint, varbyte, varword,
          varlongword, varint64,
          varqword:
          Begin
            d2 := Int64(v2);
            Result := Sign(d1 - d2);
          End;
          varDouble, varSingle, varDate:
          Begin
            d2 := Double(v2);
            Result := Sign(d1 - d2);
          End
          Else
            Result := AnsiCompareText(v1, v2);
        End;
      End;

End;

Var
  _SortColumn: Integer;
  _SortDesc: Boolean;
  _IntCols: Integer;
  _List: TVarList;

Function CompareItems(Item1, Item2: Pointer): Integer;
Var
  v1, v2: PVariant;
  i: Integer;
Begin
  If Item1 = Item2 Then
  Begin
    Result := 0;
    exit;
  End;
  v1 := Item1;
  v2 := Item2;
  If Assigned(_List.OnCompareVarRows) Then
    Result := _List.OnCompareVarRows(_List, v1, v2, _SortDesc)
  Else
    Result := 0;
  If Result = 0 Then
  Begin
    Result := CompareVariants(v1[_SortColumn], v2[_SortColumn]);
    i := _IntCols;
    While (Result = 0) And (i < _List.ColCnt + _IntCols) Do
    Begin
      If i <> _SortColumn Then
        Result := CompareVariants(v1[i], v2[i]);
      Inc(i);
    End;
    If _SortDesc Then
      Result := -Result;
  End;
End;

Procedure TVarList.Sort(ACol: Integer; Descending: Boolean);
Begin
  _SortColumn := ACol + IntCols;
  _SortDesc := Descending;
  _IntCols := IntCols;
  _List := Self;
  Inherited Sort(@CompareItems);
  DoDataChanged;
End;

Function TVarList.IndexOf(ACol: Integer; Const Value: Variant): Integer;
Var
  i: Integer;
Begin
  For i := 0 To RowCnt - 1 Do
    If CompareVariants(Items[ACol, i], Value) = 0 Then
    Begin
      Result := i;
      exit;
    End;
  Result := -1;
End;

Function TVarList.SortedIndexOf(ACol: Integer; Const Value: Variant): Integer;
Begin
  Result := -1;
  If Not Find(ACol, Value, Result) Then
    Result := -1;
End;

Function TVarList.Find(ACol: Integer; Const Value: Variant; Var Index: Integer): Boolean;
Var
  L, R, I: Integer;
  CompareRes: PtrInt;
Begin
  Result := False;
  L := 0;
  R := Count - 1;
  While (L <= R) Do
  Begin
    I := L + (R - L) Div 2;
    CompareRes := CompareVariants(Value, Items[ACol, I]);
    If (CompareRes > 0) Then
      L := I + 1
    Else
    Begin
      R := I - 1;
      If (CompareRes = 0) Then
      Begin
        Result := True;
        L := I;
        // forces end of while loop
      End;
    End;
  End;
  Index := L;
End;

Procedure TVarList.BeginUpdate;
Begin
  Inc(FUpdateLockCnt);
End;

Procedure TVarList.EndUpdate;
Begin
  Dec(FUpdateLockCnt);
  If FUpdateLockCnt = 0 Then
    DoDataChanged;
End;

Procedure TVarList.InsertRow(ARow: Integer);
Begin
  Inherited Insert(ARow, nil);
End;

Function TVarList.IsUpdating: Boolean;
Begin
  Result := FUpdateLockCnt > 0;
End;

Function TVarList.GetRowItem(ARow: PVariant; ACol: Integer): Variant;
Begin
  CheckColIndex(ACol);
  Result := ARow[ACol + IntCols];
End;

End.
