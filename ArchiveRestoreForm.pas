
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

Unit ArchiveRestoreForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, BaseForm;

Resourcestring
  SArchiveRestoreChangePath = 'Change download folder';
  SArchiveRestoreNewPath = 'New download folder';
  SArchiveRestoreRemove = 'Remove restored torrents from the archive';
  SArchiveRestorePathRequired = 'Please enter a download folder.';

Type

  { TArchiveRestoreForm }

  TArchiveRestoreForm = Class(TBaseForm)
    btBrowse: TButton;
    Buttons: TButtonPanel;
    cbChangePath: TCheckBox;
    cbRemove: TCheckBox;
    edPath: TEdit;
    txPath: TLabel;
    Procedure btBrowseClick(Sender: TObject);
    Procedure cbChangePathClick(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  Private
    Function GetChangePath: Boolean;
    Function GetDownloadDir: String;
    Function GetRemoveFromArchive: Boolean;
  Public
    Property ChangePath: Boolean read GetChangePath;
    Property DownloadDir: String read GetDownloadDir;
    Property RemoveFromArchive: Boolean read GetRemoveFromArchive;
  End;

Implementation

Uses main;

  { TArchiveRestoreForm }

Procedure TArchiveRestoreForm.FormCreate(Sender: TObject);
Begin
  bidiMode := GetBiDi();
  cbChangePath.Caption := SArchiveRestoreChangePath;
  txPath.Caption := SArchiveRestoreNewPath;
  cbRemove.Caption := SArchiveRestoreRemove;
  cbChangePath.Checked := False;
  edPath.Enabled := False;
  btBrowse.Enabled := False;
  cbRemove.Checked := True;
End;

Procedure TArchiveRestoreForm.cbChangePathClick(Sender: TObject);
Begin
  edPath.Enabled := cbChangePath.Checked;
  btBrowse.Enabled := cbChangePath.Checked;
  If edPath.Enabled Then
    edPath.SetFocus;
End;

Procedure TArchiveRestoreForm.btBrowseClick(Sender: TObject);
Var
  s: String;
Begin
  s := MainForm.SelectRemoteFolder(edPath.Text, SArchiveRestoreNewPath);
  If s <> '' Then
    edPath.Text := s;
End;

Procedure TArchiveRestoreForm.FormCloseQuery(Sender: TObject;
  Var CanClose: Boolean);
Begin
  If (ModalResult = mrOk) And cbChangePath.Checked And
    (Trim(edPath.Text) = '') Then
  Begin
    MessageDlg('', SArchiveRestorePathRequired, mtError, [mbOK], 0);
    edPath.SetFocus;
    CanClose := False;
  End;
End;

Function TArchiveRestoreForm.GetChangePath: Boolean;
Begin
  Result := cbChangePath.Checked;
End;

Function TArchiveRestoreForm.GetDownloadDir: String;
Begin
  Result := Trim(edPath.Text);
End;

Function TArchiveRestoreForm.GetRemoveFromArchive: Boolean;
Begin
  Result := cbRemove.Checked;
End;

Initialization
  {$I archiverestoreform.lrs}

End.
