

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

Unit ColSetup;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, CheckLst, StdCtrls, ButtonPanel, ExtCtrls, VarGrid, BaseForm;

Type

  { TColSetupForm }

  TColSetupForm = Class(TBaseForm)
    btDown: TButton;
    btUp: TButton;
    Buttons: TButtonPanel;
    lstColumns: TCheckListBox;
    Panel1: TPanel;
    Procedure btDownClick(Sender: TObject);
    Procedure btOkClick(Sender: TObject);
    Procedure btUpClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure lstColumnsClick(Sender: TObject);
    Procedure lstColumnsClickCheck(Sender: TObject);
  Private
    FPersistentColumnId: Integer;

    Procedure UpdateUI;
    Procedure MoveItem(Delta: Integer);
  Public
    { public declarations }
  End;

Function SetupColumns(LV: TVarGrid; PersistentColumnId: Integer;
  Const GridName: String): Boolean;

Implementation

Uses main;

Function SetupColumns(LV: TVarGrid; PersistentColumnId: Integer;
  Const GridName: String): Boolean;
Var
  i, j: Integer;
Begin
  With TColSetupForm.Create(Application) Do
  Try
    If GridName <> '' Then
      Caption := Caption + ' - ' + GridName;
    FPersistentColumnId := PersistentColumnId;
    For i := 0 To LV.Columns.Count - 1 Do
      With LV.Columns[i] Do
      Begin
        j := lstColumns.Items.Add(Title.Caption);
        lstColumns.Items.Objects[j] := TObject(ptrint(ID));
        If Width = 0 Then
          Visible := False;
        lstColumns.Checked[j] := Visible;
        If ID = PersistentColumnId Then
          lstColumns.Checked[j] := True;
      End;
    UpdateUI;
    Result := ShowModal = mrOk;
    If Result Then
    Begin
      LV.BeginUpdate;
      Try
        For i := 0 To lstColumns.Items.Count - 1 Do
          For j := 0 To LV.Columns.Count - 1 Do
            With LV.Columns[j] Do
              If ID = ptrint(lstColumns.Items.Objects[i]) Then
              Begin
                Index := i;
                If ID - 1 = PersistentColumnId Then
                  lstColumns.Checked[i] := True;
                If Not Visible And (Visible <> lstColumns.Checked[i]) Then
                Begin
                  Visible := True;
                  If Width < 32 Then
                    Width := 70;
                End
                Else
                  Visible := lstColumns.Checked[i];
                If Not Visible And (LV.SortColumn = ID - 1) And
                  (PersistentColumnId >= 0) Then
                  LV.SortColumn := PersistentColumnId;
                break;
              End;
      Finally
        LV.EndUpdate;
      End;
    End;
  Finally
    Free;
  End;
  Application.ProcessMessages;
End;

{ TColSetupForm }

Procedure TColSetupForm.btOkClick(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To lstColumns.Items.Count - 1 Do
    If lstColumns.Checked[i] Then
    Begin
      ModalResult := mrOk;
      exit;
    End;
  MessageDlg('At least single column must be visible.', mtError, [mbOK], 0);
End;

Procedure TColSetupForm.btDownClick(Sender: TObject);
Begin
  MoveItem(1);
End;

Procedure TColSetupForm.btUpClick(Sender: TObject);
Begin
  MoveItem(-1);
End;

Procedure TColSetupForm.FormCreate(Sender: TObject);
Begin
  Buttons.OKButton.ModalResult := mrNone;
  Buttons.OKButton.OnClick := @btOKClick;
End;

Procedure TColSetupForm.lstColumnsClick(Sender: TObject);
Begin
  UpdateUI;
End;

Procedure TColSetupForm.lstColumnsClickCheck(Sender: TObject);
Var
  i: Integer;
Begin
  If FPersistentColumnId >= 0 Then
    For i := 0 To lstColumns.Items.Count - 1 Do
      If ptrint(lstColumns.Items.Objects[i]) = FPersistentColumnId Then
      Begin
        lstColumns.Checked[i] := True;
        break;
      End;
End;

Procedure TColSetupForm.UpdateUI;
Begin
  btUp.Enabled := lstColumns.ItemIndex > 0;
  btDown.Enabled := (lstColumns.ItemIndex >= 0) And
    (lstColumns.ItemIndex < lstColumns.Items.Count - 1);
End;

Procedure TColSetupForm.MoveItem(Delta: Integer);
Var
  c: Boolean;
  OldIdx: Integer;
Begin
  OldIdx := lstColumns.ItemIndex;
  c := lstColumns.Checked[OldIdx];
  lstColumns.Items.Move(OldIdx, OldIdx + Delta);
  lstColumns.Checked[OldIdx + Delta] := c;
  lstColumns.ItemIndex := OldIdx + Delta;
  UpdateUI;
End;

Initialization
  {$I colsetup.lrs}

End.
