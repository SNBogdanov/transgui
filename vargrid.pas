

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

Unit VarGrid;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, Grids, VarList, Graphics, Controls, LMessages, Forms,
StdCtrls, LCLType, ExtCtrls,LazUTF8, LCLVersion;

Type 
  TVarGrid = Class;

    TCellOption = (coDrawCheckBox, coDrawTreeButton);
    TCellOptions = set Of TCellOption;

    TCellAttributes = Record
      Text: string;
      ImageIndex: integer;
      Indent: integer;
      Options: TCellOptions;
      State: TCheckBoxState;
      Expanded: boolean;
    End;

    TOnCellAttributes = Procedure (Sender: TVarGrid; ACol, ARow, ADataCol:
                                   integer; AState: TGridDrawState; Var
                                   CellAttribs: TCellAttributes) Of object;
    TOnDrawCellEvent = Procedure (Sender: TVarGrid; ACol, ARow, ADataCol:
                                  integer; AState: TGridDrawState; Const R:
                                  TRect; Var ADefaultDrawing: boolean) Of object
    ;
    TOnSortColumnEvent = Procedure (Sender: TVarGrid; Var ASortCol: integer) Of 
                         object;
    TCellNotifyEvent = Procedure (Sender: TVarGrid; ACol, ARow, ADataCol:
                                  integer) Of object;
    TOnQuickSearch = Procedure (Sender: TVarGrid; Var SearchText: String; Var
                                ARow: integer) Of object;

  { TVarGridStringEditor }

    TVarGridStringEditor = Class(TStringCellEditor)
      Protected 
        Procedure msg_SetGrid(Var Msg: TGridMessage);
        message GM_SETGRID;
        Procedure msg_SetBounds(Var Msg: TGridMessage);
        message GM_SETBOUNDS;
    End;

  { TVarGrid }

    TVarGrid = Class(TCustomDrawGrid)
      Private 
        FFirstVisibleColumn: integer;
        FHideSelection: boolean;
        FImages: TImageList;
        FItems: TVarList;
        FItemsChanging: boolean;
        FColumnsMap: array Of integer;
        FMultiSelect: boolean;
        FOnAfterSort: TNotifyEvent;
        FOnCellAttributes: TOnCellAttributes;
        FOnCheckBoxClick: TCellNotifyEvent;
        FOnDrawCell: TOnDrawCellEvent;
        FOnEditorHide: TNotifyEvent;
        FOnEditorShow: TNotifyEvent;
        FOnQuickSearch: TOnQuickSearch;
        FOnTreeButtonClick: TCellNotifyEvent;
        FSelCount: integer;
        FAnchor: integer;
        FSortColumn: integer;
        FOnSortColumn: TOnSortColumnEvent;
        FRow: integer;
        FHintCell: TPoint;
        FCurSearch: string;
        FSearchTimer: TTimer;
        FOldOpt: TGridOptions;
        FNoDblClick: boolean;
        FStrEditor: TVarGridStringEditor;

        Function GetRow: integer;
        Function GetRowSelected(RowIndex: integer): boolean;
        Function GetRowVisible(RowIndex: integer): boolean;
        Function GetSortOrder: TSortOrder;
        Procedure ItemsChanged(Sender: TObject);
        Procedure SetHideSelection(Const AValue: boolean);
        Procedure SetRow(Const AValue: integer);
        Procedure SetRowSelected(RowIndex: integer; Const AValue: boolean);
        Procedure SetRowVisible(RowIndex: integer; Const AValue: boolean);
        Procedure SetSortColumn(Const AValue: integer);
        Procedure SetSortOrder(Const AValue: TSortOrder);
        Procedure UpdateColumnsMap;
        Procedure UpdateSelCount;
        Procedure SelectRange(OldRow, NewRow: integer);
        Procedure CMHintShow(Var Message: TCMHintShow);
        message CM_HINTSHOW;
        Function CellNeedsCheckboxBitmaps(Const aCol,aRow: Integer): boolean;
        Procedure DrawCellCheckboxBitmaps(Const aCol,aRow: Integer; Const aRect:
                                          TRect);
        Function FindRow(Const SearchStr: String; StartRow: integer): integer;
        Procedure DoSearchTimer(Sender: TObject);

      Protected 
        Procedure SizeChanged(OldColCount, OldRowCount: Integer);
        override;
        Procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:
                           TGridDrawState);
        override;
        Procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer);
        override;
        Procedure PrepareCanvas(aCol,aRow: Integer; aState:TGridDrawState);
        override;
        Procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer
        );
        override;
        Procedure MouseMove(Shift: TShiftState; X,Y: Integer);
        override;
        Procedure KeyDown(Var Key : Word; Shift : TShiftState);
        override;
        Procedure UTF8KeyPress(Var UTF8Key: TUTF8Char);
        override;
        Procedure DoOnCellAttributes(ACol, ARow, ADataCol: integer; AState:
                                     TGridDrawState; Var CellAttribs:
                                     TCellAttributes);
        Procedure HeaderClick(IsColumn: Boolean; index: Integer);
        override;
        Procedure AutoAdjustColumn(aCol: Integer);
        override;
        Procedure DrawColumnText(aCol,aRow: Integer; aRect: TRect; aState:
                                 TGridDrawState);
        override;
        Procedure DblClick;
        override;
        Procedure VisualChange;
        override;
        Procedure Click;
        override;
        Procedure GetCheckBoxState(Const aCol, aRow:Integer; Var aState:
                                   TCheckboxState);
        override;
        Procedure SetCheckboxState(Const aCol, aRow:Integer; Const aState:
                                   TCheckboxState);
        override;
        Procedure SetupCell(ACol, ARow: integer; AState: TGridDrawState; out
                            CellAttribs: TCellAttributes);
        Procedure DoOnCheckBoxClick(ACol, ARow: integer);
        Procedure DoOnTreeButtonClick(ACol, ARow: integer);
        Function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint):

                                                                         Boolean
        ;
        override;
        Function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
        override;
        Function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos
                               : TPoint): Boolean;
        override;
        Procedure DrawRow(aRow: Integer);
        override;
        Function  GetCells(ACol, ARow: Integer): string;
        override;
        Function  GetEditText(ACol, ARow: Longint): string;
        override;
        Procedure SetEditText(ACol, ARow: Longint; Const Value: String);
        override;
        Procedure DoEditorShow;
        override;
        Procedure DoEditorHide;
        override;

      Public 
        Procedure VisualChangeNew;
        constructor Create(AOwner: TComponent);
        override;
        destructor Destroy;
        override;
        Function EditorByStyle(Style: TColumnButtonStyle): TWinControl;
        override;
        Procedure RemoveSelection;
        Procedure SelectAll;
        Procedure Sort;
        reintroduce;
        Function ColToDataCol(ACol: integer): integer;
        Function DataColToCol(ADataCol: integer): integer;
        Procedure EnsureSelectionVisible;
        Procedure EnsureRowVisible(ARow: integer);
        Procedure BeginUpdate;
        reintroduce;
        Procedure EndUpdate(aRefresh: boolean = true);
        reintroduce;
        Procedure EditCell(ACol, ARow: integer);

        property Items: TVarList read FItems;
        property RowSelected[RowIndex: integer]: boolean read GetRowSelected
                                                 write SetRowSelected;
        property RowVisible[RowIndex: integer]: boolean read GetRowVisible write
                                                SetRowVisible;
        property SelCount: integer read FSelCount;
        property Row: integer read GetRow write SetRow;
        property FirstVisibleColumn: integer read FFirstVisibleColumn;
      Published 
        property RowCount;
        property Align;
        property AlternateColor;
        property Anchors;
        property BorderSpacing;
        property BorderStyle;
        property Color;
        property Columns;
        property Constraints;
        property DragCursor;
        property DragKind;
        property DragMode;
        property Enabled;
        property FixedCols;
        property FixedRows;
        property Font;
        property GridLineWidth;
        property Options;
        property ParentColor default false;
        property ParentFont;
        property ParentShowHint default false;
        property PopupMenu;
        property ScrollBars;
        property ShowHint default True;
        property TabOrder;
        property TabStop;
        property TitleFont;
        property TitleImageList;
        property TitleStyle default tsNative;
        property Visible;

        property OnClick;
        property OnDblClick;
        property OnEnter;
        property OnExit;
        property OnHeaderClick;
        property OnHeaderSized;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnMouseWheelDown;
        property OnMouseWheelUp;
        property OnContextPopup;
        property OnDragDrop;
        property OnDragOver;
        property OnEndDock;
        property OnEndDrag;
        property OnStartDock;
        property OnStartDrag;
        property OnUTF8KeyPress;
        property OnResize;
        property OnGetEditText;
        property OnSetEditText;

        property Images: TImageList read FImages write FImages;
        property MultiSelect: boolean read FMultiSelect write FMultiSelect
                              default False;
        property SortColumn: integer read FSortColumn write SetSortColumn
                             default -1;
        property SortOrder: TSortOrder read GetSortOrder write SetSortOrder
                            default soAscending;
        property HideSelection: boolean read FHideSelection write
                                SetHideSelection default False;

        property OnCellAttributes: TOnCellAttributes read FOnCellAttributes
                                   write FOnCellAttributes;
        property OnDrawCell: TOnDrawCellEvent read FOnDrawCell write FOnDrawCell
        ;
        property OnSortColumn: TOnSortColumnEvent read FOnSortColumn write
                               FOnSortColumn;
        property OnAfterSort: TNotifyEvent read FOnAfterSort write FOnAfterSort;
        property OnCheckBoxClick: TCellNotifyEvent read FOnCheckBoxClick write
                                  FOnCheckBoxClick;
        property OnTreeButtonClick: TCellNotifyEvent read FOnTreeButtonClick
                                    write FOnTreeButtonClick;
        property OnQuickSearch: TOnQuickSearch read FOnQuickSearch write
                                FOnQuickSearch;
        property OnEditorShow: TNotifyEvent read FOnEditorShow write
                               FOnEditorShow;
        property OnEditorHide: TNotifyEvent read FOnEditorHide write
                               FOnEditorHide;
    End;

    Procedure Register;

    Implementation

    Uses Variants, Math, GraphType, lclintf, Themes, types, lclproc
    {$ifdef LCLcarbon} , carbonproc {$endif LCLcarbon};

    Const 
      roSelected = 1;
      roCurRow   = 2;

    Procedure Register;
    Begin
      RegisterComponents('TransGUI', [TVarGrid]);
    End;

{ TVarGridStringEditor }

    Procedure TVarGridStringEditor.msg_SetGrid(Var Msg: TGridMessage);
    Begin
      inherited;
      Msg.Options := Msg.Options And Not EO_AUTOSIZE;
    End;

    Procedure TVarGridStringEditor.msg_SetBounds(Var Msg: TGridMessage);

    Var 
      ca: TCellAttributes;
    Begin
      With Msg Do
        Begin
          TVarGrid(Grid).SetupCell(Col, Row, [], ca);
          With CellRect Do
            Begin
              Inc(Left, ca.Indent);
              If coDrawTreeButton In ca.Options Then
                Inc(Left, Bottom - Top);
              If coDrawCheckBox In ca.Options Then
                Inc(Left, Bottom - Top);
              If (ca.ImageIndex <> -1) And Assigned(TVarGrid(Grid).Images) Then
                Inc(Left, TVarGrid(Grid).Images.Width + 2);
              Dec(Left, 3);
              Dec(Top, 1);
              SetBounds(Left, Top, Right-Left, Bottom-Top);
            End;
        End;
    End;

{ TVarGrid }

    Procedure TVarGrid.ItemsChanged(Sender: TObject);

    Var 
      i, OldRows, OldCols: integer;
      pt: TPoint;
    Begin
      FItemsChanging := True;
      Try
        Perform(CM_MouseLeave, 0, 0);
        // Hack to call ResetHotCell to workaround a bug
        OldRows := RowCount;
        OldCols := Columns.Count;
        i := FItems.RowCnt + FixedRows;
        If (FRow = -1) And (Inherited Row >= i) And (i > FixedRows) Then
          inherited Row := i - 1;
        RowCount := i;
        If FRow <> -1 Then
          Begin
            Row := FRow;
            FRow := -1;
          End;
        UpdateSelCount;
        While Columns.Count > FItems.ColCnt Do
          Columns.Delete(Columns.Count - 1);
        If Columns.Count <> FItems.ColCnt Then
          Begin
            Columns.BeginUpdate;
            Try
              For i:=Columns.Count To FItems.ColCnt - 1 Do
                Columns.Add;
            Finally
              Columns.EndUpdate;
          End;
    End;
    If (OldRows <> RowCount) Or (OldCols <> Columns.Count) Then
      Begin
        If Parent <> Nil Then
          HandleNeeded;
        ResetSizes;
      End
    Else
      Invalidate;
    pt := ScreenToClient(Mouse.CursorPos);
    If PtInRect(ClientRect, pt) Then
      MouseMove([], pt.x, pt.y);
  Finally
    FItemsChanging := False;
End;
End;

Procedure TVarGrid.SetHideSelection(Const AValue: boolean);
Begin
  If FHideSelection=AValue Then exit;
  FHideSelection := AValue;
  Invalidate;
End;

Procedure TVarGrid.SetRow(Const AValue: integer);

Var 
  i, r: integer;
Begin
  If FItems.IsUpdating Then
    FRow := AValue
  Else
    Begin
      r := AValue + FixedRows;
      If r <> Inherited Row Then
        Begin
          i := LeftCol;
          inherited Row := r;
          LeftCol := i;
        End;
    End;
End;

Function TVarGrid.GetRowSelected(RowIndex: integer): boolean;
Begin
  Result := LongBool(FItems.RowOptions[RowIndex] And roSelected);
End;

Function TVarGrid.GetRowVisible(RowIndex: integer): boolean;
Begin
  Result := RowHeights[RowIndex + FixedRows] > 0;
End;

Function TVarGrid.GetSortOrder: TSortOrder;
Begin
  Result := Inherited SortOrder;
End;

Function TVarGrid.GetRow: integer;
Begin
  If FItems.IsUpdating And (FRow <> -1) Then
    Result := FRow
  Else
    Begin
      Result := Inherited Row - FixedRows;
    End;
End;

Procedure TVarGrid.SetRowSelected(RowIndex: integer; Const AValue: boolean);

Var 
  i, j: integer;
Begin
  i := FItems.RowOptions[RowIndex];
  If AValue Then
    Begin
      j := i Or roSelected;
      If j <> i Then
        Inc(FSelCount);
    End
  Else
    Begin
      j := i And Not roSelected;
      If j <> i Then
        Dec(FSelCount);
    End;
  FItems.RowOptions[RowIndex] := j;
  InvalidateRow(RowIndex + FixedRows);
  If FSelCount <= 1 Then
    InvalidateRow(Inherited Row);
End;

Procedure TVarGrid.SetRowVisible(RowIndex: integer; Const AValue: boolean);
Begin
  If AValue Then
    RowHeights[RowIndex + FixedRows] := DefaultRowHeight
  Else
    RowHeights[RowIndex + FixedRows] := 0;
End;

Procedure TVarGrid.SetSortColumn(Const AValue: integer);
Begin
  //  if FItems.Count=0 then exit;
  If FSortColumn=AValue Then exit;
  FSortColumn := AValue;
  If FSortColumn >= 0 Then

    Options := Options + [goHeaderPushedLook, goHeaderHotTracking]
  Else
    Options := Options - [goHeaderPushedLook, goHeaderHotTracking];
  Sort;
End;

Procedure TVarGrid.SetSortOrder(Const AValue: TSortOrder);
Begin
  If SortOrder = AValue Then exit;
  inherited SortOrder := AValue;
  Sort;
End;

Procedure TVarGrid.UpdateColumnsMap;

Var 
  i, j: integer;
Begin
  FFirstVisibleColumn := -1;
  SetLength(FColumnsMap, Columns.Count);
  j := 0;
  For i:=0 To Columns.Count - 1 Do
    With Columns[i] Do
      Begin
        If (FFirstVisibleColumn < 0) And Visible Then
          FFirstVisibleColumn := i;
        FColumnsMap[j] := ID - 1;
        Inc(j);
      End;
  SetLength(FColumnsMap, j);
End;

Procedure TVarGrid.UpdateSelCount;

Var 
  i: integer;
Begin
  FSelCount := 0;
  For i:=0 To FItems.Count - 1 Do
    If RowSelected[i] Then
      Inc(FSelCount);
End;

Procedure TVarGrid.SelectRange(OldRow, NewRow: integer);

Var 
  dir: integer;
  sel: boolean;
Begin
  If OldRow = NewRow Then
    exit;
  If FAnchor = -1 Then
    FAnchor := OldRow;
  dir := Sign(NewRow - OldRow);
  If Sign(FAnchor - OldRow) <> Sign(FAnchor - NewRow) Then
    While OldRow <> FAnchor Do
      Begin
        RowSelected[OldRow] := False;
        Inc(OldRow, dir);
      End;
  sel := Abs(FAnchor - OldRow) < Abs(FAnchor - NewRow);
  While OldRow <> NewRow Do
    Begin
      RowSelected[OldRow] := sel;
      Inc(OldRow, dir);
    End;
  RowSelected[NewRow] := True;
End;

Procedure TVarGrid.CMHintShow(Var Message: TCMHintShow);

Var 
  ca: TCellAttributes;
  pt: TPoint;
  wd: integer;
  R: TRect;
Begin
  With Message.HintInfo^ Do
    Begin
      pt := MouseToCell(CursorPos);
      If (pt.x >= FixedCols) And (pt.y >= 0) Then
        Begin
          R := CellRect(pt.x, pt.y);
          If PtInRect(R, CursorPos) Then
            Begin
              SetupCell(pt.x, pt.y, [], ca);
              If ca.Text <> '' Then
                Begin
                  wd := Canvas.TextWidth(ca.Text);
                  Inc(R.Left, ca.Indent);
                  If coDrawTreeButton In ca.Options Then
                    Inc(R.Left, R.Bottom - R.Top);
                  If coDrawCheckBox In ca.Options Then
                    Inc(R.Left, R.Bottom - R.Top);
                  If (ca.ImageIndex <> -1) And Assigned(FImages) Then
                    Inc(R.Left, FImages.Width + 2);
                  If (R.Right <= R.Left) Or (R.Right - R.Left < wd + 5) Then
                    Begin
                      HintStr := ca.Text;
                      R.Top := (R.Top + R.Bottom - Canvas.TextHeight(ca.Text))
                               Div 2 - 4;
                      Dec(R.Left);
                      HintPos := ClientToScreen(R.TopLeft);
                    End;
                  FHintCell := pt;
                End
              Else
                Message.Result := 1;
            End
          Else
            Message.Result := 1;
        End;
    End;
End;

Function TVarGrid.CellNeedsCheckboxBitmaps(Const aCol, aRow: Integer): boolean;

Var 
  C: TGridColumn;
Begin
  Result := false;
  If (aRow>=FixedRows) And Columns.Enabled Then
    Begin
      C := ColumnFromGridColumn(aCol);
      result := (C<>Nil) And (C.ButtonStyle=cbsCheckboxColumn)
    End;
End;

Procedure TVarGrid.DrawCellCheckboxBitmaps(Const aCol, aRow: Integer; Const
                                           aRect: TRect);

Var 
  AState: TCheckboxState;
Begin
  AState := cbUnchecked;
  GetCheckBoxState(aCol, aRow, aState);
  DrawGridCheckboxBitmaps(aCol, aRow, aRect, aState);
End;

Function TVarGrid.FindRow(Const SearchStr: String; StartRow: integer): integer;

Var 
  i, c: integer;
  s, ss: string;
  v: variant;
Begin
  Result := -1;
  If Columns.Count = 0 Then
    exit;
  c := SortColumn;
  If (c < 0) Or (c >= Items.ColCnt) Then
    c := 0;
  ss := LazUTF8.UTF8UpperCase(SearchStr);
  For i:=StartRow To Items.Count - 1 Do
    Begin
      v := Items[c, i];
      If VarIsNull(v) Or VarIsEmpty(v) Then
        s := ''
      Else
        s := LazUTF8.UTF8UpperCase(UTF8Encode(widestring(v){%H-}));
      If Copy(s, 1, Length(ss)) = ss Then
        Begin
          Result := i;
          break;
        End;
    End;
End;

Procedure TVarGrid.DoSearchTimer(Sender: TObject);
Begin
  FSearchTimer.Enabled := False;
  FCurSearch := '';
End;

Procedure TVarGrid.SizeChanged(OldColCount, OldRowCount: Integer);
Begin
  If Not FItemsChanging And (FItems <> Nil) Then
    Begin
      FItems.ColCnt := Columns.Count;
      FItems.RowCnt := RowCount - FixedRows;
      UpdateColumnsMap;
    End;
  inherited;
End;

Procedure TVarGrid.DrawCell(aCol, aRow: Integer; aRect: TRect; aState:
                            TGridDrawState);

Var 
  ca: TCellAttributes;
  //  ts: TTextStyle;
  dd, IsHeader: boolean;
  R, RR: TRect;
  det: TThemedElementDetails;
  sz: TSize;
  i: integer;
Begin
  RR := aRect;
  IsHeader := (gdFixed In aState) And (aRow=0) And (aCol>=FirstGridColumn);
  If Not IsHeader And MultiSelect And (FSelCount > 0) Then
    If (aRow >= FixedRows) And (aCol >= FixedCols) And RowSelected[aRow -
       FixedRows] Then
      Include(aState, gdSelected)
  Else
    Exclude(aState, gdSelected);

  PrepareCanvas(aCol, aRow, aState);
  If DefaultDrawing Then
    SetupCell(aCol, aRow, aState, ca);
  If Not IsHeader Or (TitleStyle<>tsNative) Then
    Canvas.FillRect(aRect);

  If Not IsHeader Then
    Begin
      dd := True;
      If Assigned(FOnDrawCell) Then
        Begin
          R := CellRect(aCol, aRow);
          If goVertLine In Options Then
            Dec(R.Right, 1);
          If goHorzLine In Options Then
            Dec(R.Bottom, 1);
          FOnDrawCell(Self, aCol, aRow - FixedRows, ColToDataCol(aCol), aState,
          R, dd);
        End;

      If DefaultDrawing And dd Then
        Begin
          If CellNeedsCheckboxBitmaps(aCol,aRow) Then
            DrawCellCheckboxBitmaps(aCol,aRow,aRect)
          Else
            Begin
              Inc(aRect.Left, ca.Indent);
              If coDrawTreeButton In ca.Options Then
                Begin
                  R := aRect;
                  R.Right := R.Left + (R.Bottom - R.Top);
                  aRect.Left := R.Right;
                  If ThemeServices.ThemesEnabled Then
                    Begin
                      If ca.Expanded Then
                        det := ThemeServices.GetElementDetails(ttGlyphOpened)
                      Else
                        det := ThemeServices.GetElementDetails(ttGlyphClosed);
                      sz := ThemeServices.GetDetailSizeForPPI(det,96);
                      With R Do
                        Begin
                          Left := (Left + Right - sz.cx) Div 2;
                          Top := (Top + Bottom - sz.cy) Div 2;
                          R := Bounds(Left, Top, sz.cx, sz.cy);
                        End;
                      ThemeServices.DrawElement(Canvas.Handle, det, R, Nil);
                    End
                  Else
                    With Canvas Do
                      Begin
                        i := (R.Bottom - R.Top) Div 4;
                        InflateRect(R, -i, -i);
                        If (R.Right - R.Left) And 1 = 0 Then
                          Dec(R.Right);
                        If (R.Bottom - R.Top) And 1 = 0 Then
                          Dec(R.Bottom);
                        Pen.Color := clWindowText;
                        Rectangle(R);
                        InflateRect(R, -1, -1);
                        Brush.Color := clWindow;
                        FillRect(R);
                        InflateRect(R, -1, -1);
                        i := (R.Top + R.Bottom) Div 2;
                        MoveTo(R.Left, i);
                        LineTo(R.Right, i);
                        If Not ca.Expanded Then
                          Begin
                            i := (R.Left + R.Right) Div 2;
                            MoveTo(i, R.Top);
                            LineTo(i, R.Bottom);
                          End;
                      End;
                End;
              If coDrawCheckBox In ca.Options Then
                Begin
                  R := aRect;
                  R.Right := R.Left + (R.Bottom - R.Top);
                  aRect.Left := R.Right;
                  DrawGridCheckboxBitmaps(aCol, aRow, R, ca.State);
                End;
              If (ca.ImageIndex <> -1) And Assigned(FImages) Then
                Begin
                  FImages.Draw(Canvas, aRect.Left + 2, (aRect.Bottom + aRect.Top
                               - FImages.Height) div 2, ca.ImageIndex, gdeNormal
                  );
                  Inc(aRect.Left, FImages.Width + 2);
                End;
              If ca.Text <> '' Then
                Begin


{
          if Canvas.TextStyle.Alignment <> taLeftJustify then
            if (aRect.Right <= aRect.Left) or (aRect.Right - aRect.Left < Canvas.TextWidth(ca.Text) + 9) then begin
              ts:=Canvas.TextStyle;
              ts.Alignment:=taLeftJustify;
              Canvas.TextStyle:=ts;
            end;
          DrawCellText(aCol, aRow, aRect, aState, ca.Text);
}
                  With aRect Do
                    Begin
                      Inc(Top, 2);
                      Inc(Left, constCellPadding);
                      Dec(Right, constCellPadding);
                      If Right<Left Then
                        Right := Left;
                      If Left>Right Then
                        Left := Right;
                      If Bottom<Top Then
                        Bottom := Top;
                      If Top>Bottom Then
                        Top := Bottom;
                      Top := (Top + Bottom - Canvas.TextHeight(ca.Text)) Div 2 ;
                      If (Left <> Right) And (Top <> Bottom) Then
                        Begin
                          If Canvas.TextStyle.Alignment <> taLeftJustify Then
                            Begin
                              i := Canvas.TextWidth(ca.Text);
                              If i < Right - Left Then
                                Case Canvas.TextStyle.Alignment Of 
                                  taRightJustify:
                                                  Left := Right - i;
                                  taCenter:
                                            Left := (Left + Right - i) Div 2;
                                End;
                            End;
                          ExtUTF8Out(Canvas.Handle, Left, Top, ETO_OPAQUE Or
                                     ETO_CLIPPED, @aRect, PChar(ca.Text), Length
                          (ca.Text), nil);
                        End;
                    End;

                End;
            End;
        End;
    End;
  If gdFixed In aState Then
    DefaultDrawCell(aCol, aRow, RR, aState)
  Else
    DrawCellGrid(aCol, aRow, RR, aState);
End;

Procedure TVarGrid.ColRowMoved(IsColumn: Boolean; FromIndex, ToIndex: Integer);
Begin
  inherited ColRowMoved(IsColumn, FromIndex, ToIndex);
  UpdateColumnsMap;
End;

Procedure TVarGrid.PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);

Var 
  F: TCustomForm;
Begin
  If FHideSelection And (FSelCount = 0) Then
    Begin
      F := GetParentForm(Self);
      If (F <> Nil) And (F.ActiveControl <> Self) Then
        aState := aState - [gdSelected];
    End;
  inherited PrepareCanvas(aCol, aRow, aState);
  With Canvas Do
    If (Font.Color = clWindow) And (Brush.Color = clHighlight) Then
      Begin
        Font.Color := clHighlightText;
{$ifdef LCLgtk2}
        Brush.Color := ColorToRGB(Brush.Color);
        // Workaround for LCL bug
{$endif LCLgtk2}
      End;
End;

Procedure TVarGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
                             Integer);

Var 
  pt: TPoint;
  IsCtrl, CheckBoxClicked: boolean;
  ca: TCellAttributes;
  R, RR: TRect;
Begin
{$ifdef LCLcarbon}
  IsCtrl := ssMeta In GetCarbonShiftState;
{$else}
{$ifdef LCLCocoa}
  IsCtrl := ssMeta In Shift;
{$else}
  IsCtrl := ssCtrl In Shift;
{$endif LCLCocoa}
{$endif LCLcarbon}
  CheckBoxClicked := False;
  pt := MouseToCell(Point(X,Y));
  If ssLeft In Shift Then
    Begin
      SetupCell(pt.x, pt.y, [], ca);
      RR := CellRect(pt.x, pt.y);
      Inc(RR.Left, ca.Indent);
      If (RR.Left <= RR.Right) And (coDrawTreeButton In ca.Options) Then
        Begin
          R := RR;
          R.Right := R.Left + (R.Bottom - R.Top);
          If R.Right > RR.Right Then
            R.Right := RR.Right;
          If PtInRect(R, Point(X,Y)) Then
            Begin
              DoOnTreeButtonClick(pt.x, pt.y);
              InvalidateCell(pt.x, pt.y);
              If Assigned(OnDblClick) And (ssDouble In Shift) Then
                FNoDblClick := True;
            End;
          Inc(RR.Left, RR.Bottom - RR.Top);
        End;
      If (RR.Left <= RR.Right) And (coDrawCheckBox In ca.Options) Then
        Begin
          R := RR;
          R.Right := R.Left + (R.Bottom - R.Top);
          If R.Right > RR.Right Then
            R.Right := RR.Right;
          If PtInRect(R, Point(X,Y)) Then
            Begin
              DoOnCheckBoxClick(pt.x, pt.y);
              InvalidateCell(pt.x, pt.y);
              CheckBoxClicked := True;
              If Assigned(OnDblClick) And (ssDouble In Shift) Then
                FNoDblClick := True;
            End;
        End;
    End;
  If (ssRight In Shift) {$ifdef darwin} Or (Shift*[ssLeft, ssCtrl] = [ssLeft,
     ssCtrl]) {$endif} Then
    Begin
      SetFocus;
      If (pt.x >= FixedCols) And (pt.y >= FixedRows) Then
        Begin
          If MultiSelect And (SelCount > 0) And Not RowSelected[pt.y - FixedRows
             ] Then
            RemoveSelection;
          Row := pt.y - FixedRows;
        End;
    End
  Else
    If MultiSelect And (ssLeft In Shift) And (pt.x >= FixedCols) And (pt.y >=
       FixedRows) Then
      Begin
        If IsCtrl Then
          Begin
            If SelCount = 0 Then
              RowSelected[Row] := True;
            RowSelected[pt.y - FixedRows] := Not RowSelected[pt.y - FixedRows];
            FAnchor := -1;
          End
        Else
          If ssShift In Shift Then
            SelectRange(Row, pt.y - FixedRows)
        Else
          Begin
            If (SelCount > 0) And Not CheckBoxClicked Then
              RemoveSelection;
            FAnchor := -1;
          End;
      End;
  inherited MouseDown(Button, Shift, X, Y);
End;

Procedure TVarGrid.MouseMove(Shift: TShiftState; X, Y: Integer);

Var 
  pt: TPoint;
Begin
  NullStrictConvert := False;
  inherited MouseMove(Shift, X, Y);
  pt := MouseToCell(Point(x, y));
  If (FHintCell.x <> -1) And ((FHintCell.x <> pt.x) Or (FHintCell.y <> pt.y))
    Then
    Begin
      Application.CancelHint;
      FHintCell.x := -1;
    End;
End;

Procedure TVarGrid.KeyDown(Var Key: Word; Shift: TShiftState);

Var 
  r, k: integer;
  ca: TCellAttributes;
Begin
  If EditorMode Then
    Begin
      If Key = VK_ESCAPE Then
        Begin
          EditorHide;
          SetFocus;
        End;
      exit;
    End;

  r := Row;
  k := Key;

  If (Shift = []) And ( (k = VK_SPACE) Or (k = VK_LEFT) Or (k = VK_RIGHT) Or (k
     = VK_ADD) Or (k = VK_SUBTRACT) ) Then
    Begin
      SetupCell(FixedCols, Inherited Row, [], ca);
      Case k Of 
        VK_SPACE:
                  If coDrawCheckBox In ca.Options Then
                    Begin
                      DoOnCheckBoxClick(FixedCols, Inherited Row);
                      Key := 0;
                      exit;
                    End;
        VK_LEFT, VK_SUBTRACT:
                              If (coDrawTreeButton In ca.Options) And ca.
                                 Expanded Then
                                Begin
                                  DoOnTreeButtonClick(FixedCols, Inherited Row);
                                  Key := 0;
                                  exit;
                                End;
        VK_RIGHT, VK_ADD:
                          If (coDrawTreeButton In ca.Options) And Not ca.
                             Expanded Then
                            Begin
                              DoOnTreeButtonClick(FixedCols, Inherited Row);
                              Key := 0;
                              exit;
                            End;
      End;
    End;

  inherited KeyDown(Key, Shift);

  If MultiSelect Then
    Begin
      If ssCtrl In Shift Then
        Begin
          If k = VK_SPACE Then
            RowSelected[Row] := Not RowSelected[Row];
          FAnchor := -1;
        End
      Else
        If ssShift In Shift Then
          Begin
            SelectRange(r, Row);
          End
      Else
        If k In [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_NEXT,
           VK_PRIOR] Then
          Begin
            If SelCount > 0 Then
              RemoveSelection;
            FAnchor := -1;
          End;
    End;
  If (Key = VK_RETURN) And (Shift = []) And Assigned(OnDblClick) Then
    OnDblClick(Self);
End;

Procedure TVarGrid.UTF8KeyPress(Var UTF8Key: TUTF8Char);

Var 
  i, r: integer;
Begin
  inherited UTF8KeyPress(UTF8Key);
  exit;
  If UTF8Key = #0 Then
    exit;
  FSearchTimer.Enabled := False;
  FSearchTimer.Enabled := True;
  If FCurSearch = '' Then
    i := 0
  Else
    i := Row;
  FCurSearch := FCurSearch + UTF8Key;
  If Assigned(FOnQuickSearch) Then
    Begin
      r := i;
      FOnQuickSearch(Self, FCurSearch, r);
      If r <> i Then
        Row := r;
    End
  Else
    Begin
      i := FindRow(FCurSearch, i);
      If i >= 0 Then
        Row := i;
    End;
End;

Procedure TVarGrid.DoOnCellAttributes(ACol, ARow, ADataCol: integer; AState:
                                      TGridDrawState; Var CellAttribs:
                                      TCellAttributes);
Begin
  If Assigned(FOnCellAttributes) Then
    FOnCellAttributes(Self, ACol, ARow, ADataCol, AState, CellAttribs);
End;

Procedure TVarGrid.HeaderClick(IsColumn: Boolean; index: Integer);

Var 
  i: integer;
Begin
  inherited HeaderClick(IsColumn, index);
  If FItems.Count = 0 Then
    exit;
  If IsColumn And (FSortColumn >= 0) Then
    Begin
      fGridState := gsNormal;
      i := ColToDataCol(index);
      If FSortColumn = i Then
        Begin
          If SortOrder = soAscending Then
            SortOrder := soDescending
          Else
            SortOrder := soAscending;
        End
      Else
        Begin
          SortOrder := soAscending;
          SortColumn := i;
        End;
    End;
End;

Procedure TVarGrid.AutoAdjustColumn(aCol: Integer);

Var 
  i, j, wd, h, fr: integer;
  ca: TCellAttributes;
Begin
  wd := 4;
  fr := FixedRows;
  For i:=0 To FItems.Count - 1 Do
    Begin
      h := RowHeights[i + fr];
      If h > 0 Then
        Begin
          SetupCell(aCol, i + fr, [], ca);
          j := Canvas.TextWidth(ca.Text) + 6;
          Inc(j, ca.Indent);
          If coDrawTreeButton In ca.Options Then
            Inc(j, h);
          If coDrawCheckBox In ca.Options Then
            Inc(j, h);
          If (ca.ImageIndex <> -1) And Assigned(FImages) Then
            Inc(j, FImages.Width + 2);
          If j > wd Then
            wd := j;
        End;
    End;
  ColumnFromGridColumn(aCol).Width := wd;
End;
Procedure TVarGrid.VisualChangeNew;
Begin
  VisualChange;
End;

Procedure TVarGrid.VisualChange;

Var i: integer;
Begin
  inherited VisualChange;
  If Images <> Nil Then i := Images.Height;
  If HandleAllocated Then
    DefaultRowHeight := Max(Canvas.TextHeight('Xy') + 5,i);
  UpdateColumnsMap;
End;

Procedure TVarGrid.DrawColumnText(aCol, aRow: Integer; aRect: TRect; aState:
                                  TGridDrawState);

Var 
  R: TRect;
  i: integer;
Begin
  If (gdFixed In aState) And (aRow=0) And (aCol>=FirstGridColumn) Then
    Begin
      R := aRect;
      If FSortColumn = ColToDataCol(aCol) Then
        Begin
          R.Right := R.Left + R.Bottom - R.Top;
          InflateRect(R, -5, -5);
          Types.OffsetRect(R, -3, 0);
          Dec(R.Bottom, 2);
          aRect.Left := R.Right + 2;
        End;
      inherited DrawColumnText(aCol, aRow, aRect, aState);
      If FSortColumn = ColToDataCol(aCol) Then
        With Canvas Do
          Begin
            Pen.Color := clGrayText;
            i := (R.Left + R.Right ) Div 2;
            If SortOrder = soAscending Then
              Begin
                MoveTo(i + (i - R.Left) - 1, R.Bottom - 1);
                LineTo(i, R.Top - 1);
                MoveTo(i, R.Top);
                LineTo(i - (R.Right - i) + 1, R.Bottom);
                LineTo(R.Right, R.Bottom);
              End
            Else
              Begin
                MoveTo(i + (i - R.Left) - 1, R.Top + 1);
                LineTo(i, R.Bottom + 1);
                MoveTo(i, R.Bottom);
                LineTo(i - (R.Right - i) + 1, R.Top);
                LineTo(R.Right, R.Top);
              End;
          End;
    End;
End;


Procedure TVarGrid.DblClick;

Var 
  pt: TPoint;
Begin
  If FNoDblClick Then
    Begin
      FNoDblClick := False;
      exit;
    End;
  pt := MouseToCell(ScreenToClient(Mouse.CursorPos));
  If (pt.y < FixedRows) And (pt.y = 0) And (Cursor <> crHSplit) Then
    exit;
  inherited DblClick;
End;

Procedure TVarGrid.Click;
Begin
  If Assigned(OnClick) Then
    OnClick(Self);
End;

Procedure TVarGrid.GetCheckBoxState(Const aCol, aRow: Integer; Var aState:
                                    TCheckboxState);

Var 
  s: string;
Begin
  If (aCol >= FixedCols) And (aRow >= FixedRows) Then
    Begin
      s := Items[ColToDataCol(aCol), aRow - FixedRows];
      With Columns[GridColumnFromColumnIndex(aCol)] Do
        If s = ValueChecked Then
          aState := cbChecked
        Else
          If s = ValueUnchecked Then
            aState := cbUnchecked
        Else
          aState := cbGrayed;
    End;
  inherited GetCheckBoxState(aCol, aRow, aState);
End;

Procedure TVarGrid.SetCheckboxState(Const aCol, aRow: Integer; Const aState:
                                    TCheckboxState);

Var 
  s: string;
Begin
  If (aCol >= FixedCols) And (aRow >= FixedRows) Then
    Begin
      With Columns[GridColumnFromColumnIndex(aCol)] Do
        Case aState Of 
          cbUnchecked:
                       s := ValueUnchecked;
          cbChecked:
                     s := ValueChecked;
          Else
            s := '?';
        End;
      Items[ColToDataCol(aCol), aRow - FixedRows] := s;
    End;
  inherited SetCheckboxState(aCol, aRow, aState);
End;

Procedure TVarGrid.SetupCell(ACol, ARow: integer; AState: TGridDrawState; out
                             CellAttribs: TCellAttributes);

Var 
  v: variant;
  dc: integer;
Begin
  If (ACol < 0) Or (ARow < 0) Then
    exit;
  CellAttribs.ImageIndex := -1;
  CellAttribs.Indent := 0;
  CellAttribs.Options := [];
  CellAttribs.State := cbUnchecked;
  CellAttribs.Expanded := True;
  If ACol >= FixedCols Then
    Begin
      dc := ColToDataCol(ACol);
      If ARow >= FixedRows Then
        Begin
          v := Items[dc, ARow - FixedRows];
          If Not VarIsNull(v) And Not VarIsEmpty(v) Then
            CellAttribs.Text := UTF8Encode(WideString(v))
          Else
            CellAttribs.Text := '';
        End
      Else
        CellAttribs.Text := ColumnFromGridColumn(ACol).Title.Caption;
    End
  Else
    dc := -1;
  DoOnCellAttributes(ACol - FixedCols, ARow - FixedRows, dc, AState, CellAttribs
  );
End;

Procedure TVarGrid.DoOnCheckBoxClick(ACol, ARow: integer);

Var 
  i, dc, c: integer;
  ca: TCellAttributes;
  st: TCheckBoxState;
Begin
  If Assigned(FOnCheckBoxClick) Then
    Begin
      dc := ColToDataCol(ACol);
      c := ACol - FixedCols;
      FOnCheckBoxClick(Self, c, ARow - FixedRows, dc);
      If (SelCount > 0) And RowSelected[ARow - FixedRows] Then
        Begin
          SetupCell(ACol, ARow, [], ca);
          st := ca.State;
          For i:=0 To Items.Count - 1 Do
            If RowSelected[i] Then
              Begin
                SetupCell(ACol, i + FixedRows, [], ca);
                If (coDrawCheckBox In ca.Options) And (ca.State <> st) Then
                  FOnCheckBoxClick(Self, c, i, dc);
              End;
        End;
    End;
End;

Procedure TVarGrid.DoOnTreeButtonClick(ACol, ARow: integer);
Begin
  If Assigned(FOnTreeButtonClick) Then
    FOnTreeButtonClick(Self, ACol - FixedCols, ARow - FixedRows, ColToDataCol(
                       ACol));
End;

Function TVarGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint):

                                                                         Boolean
;
Begin
  Result := False;
  If Assigned(OnMouseWheelDown) Then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
End;

Function TVarGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
Begin
  Result := False;
  If Assigned(OnMouseWheelUp) Then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
End;

Function TVarGrid.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos
                               : TPoint): Boolean;
Begin
  Result := Inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  If Not Result Then
    Begin
      If Mouse.WheelScrollLines = -1 Then
  {$IF LCL_FULLVERSION < 1080000}
        GridMouseWheel(Shift, -WheelDelta*VisibleRowCount Div 120)
      Else
        GridMouseWheel(Shift, -WheelDelta*Mouse.WheelScrollLines Div 120);
  {$ENDIF}
  {$IF LCL_FULLVERSION >= 1080000}
      GridMouseWheel(Shift, WheelDelta*VisibleRowCount Div 120)
      Else
        GridMouseWheel(Shift, -WheelDelta Div 120);
  {$ENDIF}
      Result := True;
    End;
End;

constructor TVarGrid.Create(AOwner: TComponent);
Begin
  FRow := -1;
  FHintCell.x := -1;
  inherited Create(AOwner);
  FixedRows := 1;
  FixedCols := 0;
  Options := [goRowSelect, goThumbTracking, goVertLine, goHorzLine, goColSizing,
             goColMoving, goDblClickAutoSize, goFixedHorzLine, goFixedVertLine];
  MouseWheelOption := mwGrid;
  FItems := TVarList.Create(1, 0);
  FItems.OnDataChanged := @ItemsChanged;
  ItemsChanged(Nil);
  TitleStyle := tsNative;
  FAnchor := -1;
  FSortColumn := -1;
  ShowHint := True;
  SetLength(FColumnsMap, 1);
  FColumnsMap[0] := 0;
  FSearchTimer := TTimer.Create(Self);
  With FSearchTimer Do
    Begin
      Enabled := False;
      Interval := 1500;
      OnTimer := @DoSearchTimer;
    End;
  FastEditing := False;
  EditorBorderStyle := bsSingle;
End;

destructor TVarGrid.Destroy;
Begin
  inherited Destroy;
  FItems.Free;
End;

Function TVarGrid.EditorByStyle(Style: TColumnButtonStyle): TWinControl;
Begin
  If Style = cbsAuto Then
    Begin
      If FStrEditor = Nil Then
        Begin
          FStrEditor := TVarGridStringEditor.Create(Self);
          FStrEditor.Name := 'VGStringEditor';
          FStrEditor.Text := '';
          FStrEditor.Visible := False;
          FStrEditor.Align := alNone;
          FStrEditor.BorderStyle := bsSingle;
        End;
      Result := FStrEditor;
    End
  Else
    Result := Inherited EditorByStyle(Style);
End;

Procedure TVarGrid.RemoveSelection;

Var 
  i: integer;
Begin
  For i:=0 To FItems.Count - 1 Do
    RowSelected[i] := False;
  FSelCount := 0;
End;

Procedure TVarGrid.SelectAll;

Var 
  i: integer;
Begin
  For i:=0 To FItems.Count - 1 Do
    RowSelected[i] := True;
End;

Procedure TVarGrid.Sort;

Var 
  i, c: integer;
Begin
  If (FSortColumn >= 0) And (FItems.Count > 0) Then
    Begin
      c := FSortColumn;
      If Assigned(FOnSortColumn) Then
        FOnSortColumn(Self, c);
      If Not FItems.IsUpdating And (Row >= 0) And (Row < FItems.Count) Then
        FItems.RowOptions[Row] := FItems.RowOptions[Row] Or roCurRow;
      FItems.Sort(c, SortOrder = soDescending);
      If Not FItems.IsUpdating Then
        Begin
          If Assigned(FOnAfterSort) Then
            FOnAfterSort(Self);
          For i:=0 To FItems.Count - 1 Do
            If LongBool(FItems.RowOptions[i] And roCurRow) Then
              Begin
                FItems.RowOptions[i] := FItems.RowOptions[i] And Not roCurRow;
                Row := i;
                break;
              End;
          Invalidate;
        End;
    End;
End;

Function TVarGrid.ColToDataCol(ACol: integer): integer;
Begin
  If (ACol >= FixedCols) And (ACol <= High(FColumnsMap)) Then
    Result := FColumnsMap[ACol]
  Else
    Result := -1;
End;

Function TVarGrid.DataColToCol(ADataCol: integer): integer;

Var 
  i: integer;
Begin
  For i:=FixedCols To High(FColumnsMap) Do
    If FColumnsMap[i] = ADataCol Then
      Begin
        Result := i;
        exit;
      End;
  Result := -1;
End;

Procedure TVarGrid.EnsureSelectionVisible;

Var 
  i: integer;
Begin
  If FSelCount > 0 Then
    For i:=0 To FItems.Count - 1 Do
      If RowSelected[i] Then
        Begin
          Row := i;
          break;
        End;
  EnsureRowVisible(Row);
End;

Procedure TVarGrid.EnsureRowVisible(ARow: integer);
Begin
  ARow := ARow + FixedRows;
  If ARow < TopRow Then
    TopRow := ARow
  Else
    If ARow > GCache.FullVisibleGrid.Bottom Then
      TopRow := ARow - (GCache.FullVisibleGrid.Bottom - GCache.FullVisibleGrid.
                Top);
End;

Procedure TVarGrid.BeginUpdate;
Begin
  inherited BeginUpdate;
  Items.BeginUpdate;
End;

Procedure TVarGrid.EndUpdate(aRefresh: boolean);
Begin
  inherited EndUpdate(aRefresh);
  Items.EndUpdate;
End;

Procedure TVarGrid.EditCell(ACol, ARow: integer);
Begin
  SetFocus;
  FOldOpt := Options;
  Options := Options + [goEditing];
  EditorShowInCell(DataColToCol(ACol), ARow + FixedRows);
End;

Procedure TVarGrid.DrawRow(aRow: Integer);

Var 
  Gds: TGridDrawState;
  aCol: Integer;
  Rs: Boolean;
  R: TRect;
  ClipArea: Trect;
{$ifdef LCLgtk2}
  Rgn: HRGN;
{$endif LCLgtk2}

Procedure DoDrawCell;
Begin
  With GCache Do
    Begin
      If (aCol=HotCell.x) And (aRow=HotCell.y) And Not ((PushedCell.X<>-1) And (
         PushedCell.Y<>-1)) Then
        Begin
          Include(gds, gdHot);
          HotCellPainted := True;
        End;
      If ClickCellPushed And (aCol=PushedCell.x) And (aRow=PushedCell.y) Then
        Begin
          Include(gds, gdPushed);
        End;
    End;
{$ifdef LCLgtk2}
  Rgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
  SelectClipRgn(Canvas.Handle, Rgn);
  DeleteObject(Rgn);
{$endif LCLgtk2}
  DrawCell(aCol, aRow, R, gds);
End;

Function HorizontalIntersect(Const aRect,bRect: TRect): boolean;
Begin
  result := (aRect.Left < bRect.Right) And (aRect.Right > bRect.Left);
End;

Begin
  Rs := false;
  // Upper and Lower bounds for this row
  R.Left := 0;
  ColRowToOffSet(False, True, aRow, R.Top, R.Bottom);
  If R.Bottom <= R.Top Then
    exit;
  // is this row within the ClipRect?
  ClipArea := Canvas.ClipRect;
  If (R.Top >= ClipArea.Bottom) Or (R.Bottom < ClipArea.Top) Then
    exit;
  // Draw columns in this row
  With GCache.VisibleGrid Do
    Begin
      For aCol:=left To Right Do
        Begin
          ColRowToOffset(True, True, aCol, R.Left, R.Right);
          If (R.Right <= R.Left) Or Not HorizontalIntersect(R, ClipArea) Then
            continue;
          gds := [];
          Rs := (goRowSelect In Options);
          If ARow<FixedRows Then
            include(gds, gdFixed)
          Else
            Begin
              If (aCol=Col)And(aRow=Inherited Row) Then
                gds := gds + [gdFocused, gdSelected]
              Else
                If IsCellSelected[aCol, aRow] Then
                  include(gds, gdSelected);
            End;

          DoDrawCell;
        End;

      // Draw Fixed Columns
      For aCol:=0 To FixedCols-1 Do
        Begin
          gds := [gdFixed];
          ColRowToOffset(True, True, aCol, R.Left, R.Right);
          // is this column within the ClipRect?
          If (R.Right > R.Left) And HorizontalIntersect(R, ClipArea) Then
            DoDrawCell;
        End;

{$ifdef LCLgtk2}
      With ClipArea Do
        Rgn := CreateRectRgn(Left, Top, Right, Bottom);
      SelectClipRgn(Canvas.Handle, Rgn);
      DeleteObject(Rgn);
{$endif LCLgtk2}

      // Draw the focus Rect
      If FocusRectVisible And (ARow=Inherited Row) And
         ((Rs And (ARow>=Top) And (ARow<=Bottom)) Or IsCellVisible(Col,ARow))
        Then
        Begin
          If EditorMode Then
            Begin


        //if EditorAlwaysShown and (FEditor<>nil) and FEditor.Visible then begin
              //DebugLn('No Draw Focus Rect');
            End
          Else
            Begin
              ColRowToOffset(True, True, Col, R.Left, R.Right);
              // is this column within the ClipRect?
              If HorizontalIntersect(R, ClipArea) Then
                DrawFocusRect(Col,Inherited Row, R);
            End;
        End;
    End;
End;

Function TVarGrid.GetCells(ACol, ARow: Integer): string;

Var 
  dc: integer;
  v: variant;
Begin
  Result := '';
  dc := ColToDataCol(ACol);
  If ARow >= FixedRows Then
    Begin
      v := Items[dc, ARow - FixedRows];
      If Not VarIsNull(v) And Not VarIsEmpty(v) Then
        Result := UTF8Encode(WideString(v));
    End;
End;

Function TVarGrid.GetEditText(ACol, ARow: Longint): string;
Begin
  Result := GetCells(ACol, ARow);
  If Assigned(OnGetEditText) Then
    OnGetEditText(self, aCol - FixedCols, aRow - FixedRows, Result);
End;

Procedure TVarGrid.SetEditText(ACol, ARow: Longint; Const Value: String);

Var 
  dc: integer;
Begin
  If Not (gfEditingDone In GridFlags) Then
    exit;
  If Assigned(OnSetEditText) Then
    OnSetEditText(Self, aCol - FixedCols, aRow - FixedRows, Value)
  Else
    Begin
      dc := ColToDataCol(ACol);
      If ARow >= FixedRows Then
        Items[dc, ARow - FixedRows] := UTF8Decode(Value);
    End;
End;

Procedure TVarGrid.DoEditorShow;
Begin
  inherited DoEditorShow;
  If Assigned(OnEditorShow) Then
    OnEditorShow(Self);
End;

Procedure TVarGrid.DoEditorHide;
Begin
  Try
    inherited DoEditorHide;
  Finally
    Options := FOldOpt;
End;
If Assigned(OnEditorHide) Then
  OnEditorHide(Self);
End;

End.
