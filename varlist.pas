

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

    TCompareVarRowsEvent = Function (Sender: TVarList; Row1, Row2: PVariant;
                                     DescendingSort: boolean): integer Of object
    ;

  { TVarList }

    TVarList = Class(TList)
      Private 
        FColCnt: integer;
        FExtraColumns: integer;
        FOnCompareVarRows: TCompareVarRowsEvent;
        FOnDataChanged: TNotifyEvent;
        FUpdateLockCnt: integer;
        Function GetItemPtr(ACol, ARow: integer): PVariant;
        Function GetItems(ACol, ARow: integer): variant;
        Function GetRowCnt: integer;
        Function GetRowOptions(ARow: integer): integer;
        Function GetRows(ARow: integer): PVariant;
        Function GetRow(ARow: integer): PVariant;
        Procedure SetColCnt(Const AValue: integer);
        Procedure SetExtraColumns(Const AValue: integer);
        Procedure SetItems(ACol, ARow: integer; Const AValue: variant);
        Procedure SetRowCnt(Const AValue: integer);
        Procedure SetRowOptions(ARow: integer; Const AValue: integer);
        Function IntCols: integer;
        Procedure CheckColIndex(ColIndex: integer);

      Protected 
        Procedure DoDataChanged;
        virtual;

      Public 
        constructor Create(AColCnt, ARowCnt: integer);
        destructor Destroy;
        override;
        Procedure Clear;
        override;
        Procedure Delete(Index: Integer);
        Procedure Sort(ACol: integer; Descending: boolean = False);
        reintroduce;
        Function IndexOf(ACol: integer; Const Value: variant): integer;
        Function SortedIndexOf(ACol: integer; Const Value: variant): integer;
        Function Find(ACol: integer; Const Value: variant; Var Index: Integer):

                                                                         Boolean
        ;
        Procedure BeginUpdate;
        Procedure EndUpdate;
        Procedure InsertRow(ARow: integer);
        Function IsUpdating: boolean;
        Function GetRowItem(ARow: PVariant; ACol: integer): variant;
        property Items[ACol, ARow: integer]: variant read GetItems write
                                             SetItems;
        default;
        property ItemPtrs[ACol, ARow: integer]: PVariant read GetItemPtr;
        property Rows[ARow: integer]: PVariant read GetRows;
        property RowOptions[ARow: integer]: integer read GetRowOptions write
                                            SetRowOptions;
        property ColCnt: integer read FColCnt write SetColCnt;
        property RowCnt: integer read GetRowCnt write SetRowCnt;
        property Count: integer read GetRowCnt;
        property OnDataChanged: TNotifyEvent read FOnDataChanged write
                                FOnDataChanged;
        property OnCompareVarRows: TCompareVarRowsEvent read FOnCompareVarRows
                                   write FOnCompareVarRows;
        property ExtraColumns: integer read FExtraColumns write SetExtraColumns;
    End;

    Function CompareVariants(Const v1, v2: variant): integer;

    Implementation

    Uses Math;

{ TVarList }

    Function TVarList.GetItems(ACol, ARow: integer): variant;
    Begin
      CheckColIndex(ACol);
      Result := GetRow(ARow)[ACol + IntCols];
    End;

    Function TVarList.GetItemPtr(ACol, ARow: integer): PVariant;
    Begin
      CheckColIndex(ACol);
      Result := GetRow(ARow) + (ACol + IntCols);
    End;

    Function TVarList.GetRowCnt: integer;
    Begin
      Result := Inherited GetCount;
    End;

    Function TVarList.GetRowOptions(ARow: integer): integer;
    Begin
      Result := GetRow(ARow)[0];
    End;

    Function TVarList.GetRows(ARow: integer): PVariant;
    Begin
      Result := GetRow(ARow);
    End;

    Function TVarList.GetRow(ARow: integer): PVariant;

    Var 
      v: PVariant;
      sz: integer;
    Begin

      If ARow < 0 Then ARow := 0;

      If ARow >= Count Then
        SetRowCnt(ARow + 1);
      v := Get(ARow);
      If v = Nil Then
        Begin
          sz := SizeOf(variant)*(FColCnt + IntCols);
          v := GetMem(sz);
          FillChar(v^, sz, 0);
          v[0] := 0;
          Put(ARow, v);
        End;
      Result := v;
    End;

    Procedure TVarList.SetColCnt(Const AValue: integer);

    Var 
      i, j, ocnt, ncnt: integer;
      p: PVariant;
    Begin
      If FColCnt = AValue Then exit;
      ocnt := FColCnt + IntCols;
      FColCnt := AValue;
      ncnt := FColCnt + IntCols;
      For i:=0 To Count - 1 Do
        Begin
          p := GetRow(i);
          For j:=ncnt To ocnt - 1 Do
            VarClear(p[j]);
          ReAllocMem(p, ncnt*SizeOf(variant));
          If ncnt > ocnt Then
            FillChar(p[ocnt], (ncnt - ocnt)*SizeOf(variant), 0);
        End;
    End;

    Procedure TVarList.SetExtraColumns(Const AValue: integer);
    Begin
      If FExtraColumns=AValue Then exit;
      If RowCnt <> 0 Then
        raise Exception.Create('Unable to set extra columns.');
      FExtraColumns := AValue;
    End;

    Procedure TVarList.SetItems(ACol, ARow: integer; Const AValue: variant);
    Begin
      GetRow(ARow)[ACol + IntCols] := AValue;
      DoDataChanged;
    End;

    Procedure TVarList.SetRowCnt(Const AValue: integer);
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

Procedure TVarList.SetRowOptions(ARow: integer; Const AValue: integer);
Begin
  GetRow(ARow)[0] := AValue;
End;

Function TVarList.IntCols: integer;
Begin
  Result := FExtraColumns + 1;
End;

Procedure TVarList.CheckColIndex(ColIndex: integer);
Begin
  If (ColIndex + IntCols < 0) Or (ColIndex >= ColCnt) Then
    raise Exception.CreateFmt('Invalid column index (%d).', [ColIndex]);
End;

Procedure TVarList.DoDataChanged;
Begin
  If Assigned(FOnDataChanged) And (FUpdateLockCnt = 0) Then
    FOnDataChanged(Self);
End;

constructor TVarList.Create(AColCnt, ARowCnt: integer);
Begin
  inherited Create;
  FColCnt := AColCnt;
  RowCnt := ARowCnt;
End;

destructor TVarList.Destroy;
Begin
  FOnDataChanged := Nil;
  inherited Destroy;
End;

Procedure TVarList.Clear;

Var 
  i: integer;
  v: PVariant;
Begin
  For i:=0 To Count - 1 Do
    Begin
      v := Inherited Get(i);
      If v <> Nil Then
        Begin
          VarClear(v^);
          FreeMem(v);
        End;
    End;
  inherited Clear;
  DoDataChanged;
End;

Procedure TVarList.Delete(Index: Integer);

Var 
  v: PVariant;
  i: integer;
Begin
  v := Inherited Get(Index);
  If v <> Nil Then
    Begin
      For i:=0 To ColCnt + IntCols - 1 Do
        VarClear(v[i]);
      FreeMem(v);
    End;
  inherited Delete(Index);
  DoDataChanged;
End;

Function CompareVariants(Const v1, v2: variant): integer;

Var 
  v1e, v2e: boolean;
  d1,d2: double;
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
        varInteger,varsmallint,varshortint,varbyte,varword,varlongword,varint64,
        varqword:
                  Begin
                    d1 := Int64(v1);
                  End;
        varDouble,varSingle,varDate:
                                     Begin
                                       d1 := double(v1);
                                       Result := Sign(double(v1) - double(v2));
                                     End
                                     Else
                                       Result := AnsiCompareText(v1, v2);
        exit;
      End;
      Case VarType(v2) Of 
        varInteger,varsmallint,varshortint,varbyte,varword,varlongword,varint64,
        varqword:
                  Begin
                    d2 := Int64(v2);
                    Result := Sign(d1-d2);
                  End;
        varDouble,varSingle,varDate:
                                     Begin
                                       d2 := double(v2);
                                       Result := Sign(d1-d2);
                                     End
                                     Else
                                       Result := AnsiCompareText(v1, v2);
      End;
    End;

End;

Var 
  _SortColumn: integer;
  _SortDesc: boolean;
  _IntCols: integer;
  _List: TVarList;

Function CompareItems(Item1, Item2: Pointer): Integer;

Var 
  v1, v2: PVariant;
  i: integer;
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

Procedure TVarList.Sort(ACol: integer; Descending: boolean);
Begin
  _SortColumn := ACol + IntCols;
  _SortDesc := Descending;
  _IntCols := IntCols;
  _List := Self;
  inherited Sort(@CompareItems);
  DoDataChanged;
End;

Function TVarList.IndexOf(ACol: integer; Const Value: variant): integer;

Var 
  i: integer;
Begin
  For i:=0 To RowCnt - 1 Do
    If CompareVariants(Items[ACol, i], Value) = 0 Then
      Begin
        Result := i;
        exit;
      End;
  Result := -1;
End;

Function TVarList.SortedIndexOf(ACol: integer; Const Value: variant): integer;
Begin
  Result := -1;
  If Not Find(ACol, Value, Result) Then
    Result := -1;
End;

Function TVarList.Find(ACol: integer; Const Value: variant; Var Index: Integer):

                                                                         Boolean
;

Var 
  L, R, I: Integer;
  CompareRes: PtrInt;
Begin
  Result := false;
  L := 0;
  R := Count - 1;
  While (L<=R) Do
    Begin
      I := L + (R - L) Div 2;
      CompareRes := CompareVariants(Value, Items[ACol, I]);
      If (CompareRes>0) Then
        L := I+1
      Else
        Begin
          R := I-1;
          If (CompareRes=0) Then
            Begin
              Result := true;
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

Procedure TVarList.InsertRow(ARow: integer);
Begin
  inherited Insert(ARow, Nil);
End;

Function TVarList.IsUpdating: boolean;
Begin
  Result := FUpdateLockCnt > 0;
End;

Function TVarList.GetRowItem(ARow: PVariant; ACol: integer): variant;
Begin
  CheckColIndex(ACol);
  Result := ARow[ACol + IntCols];
End;

End.
