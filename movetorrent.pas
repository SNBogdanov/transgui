

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

Unit MoveTorrent;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ExtCtrls, BaseForm;

Resourcestring
  SNoTorrentDir = 'No torrent location was specified.';
  SSelectFolder = 'Select torrent location';

Type

  { TMoveTorrentForm }

  TMoveTorrentForm = Class(TBaseForm)
    btBrowse: TButton;
    Buttons: TButtonPanel;
    cbMoveData: TCheckBox;
    edTorrentDir: TComboBox;
    Panel1: TPanel;
    txTorrentDir: TLabel;
    Procedure btBrowseClick(Sender: TObject);
    Procedure btOKClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  Private
    { private declarations }
  Public
    { public declarations }
  End;

Implementation

Uses main;

  { TMoveTorrentForm }

Procedure TMoveTorrentForm.btOKClick(Sender: TObject);
Begin
  edTorrentDir.Text := Trim(edTorrentDir.Text);
  If edTorrentDir.Text = '' Then
  Begin
    edTorrentDir.SetFocus;
    MessageDlg(SNoTorrentDir, mtError, [mbOK], 0);
    exit;
  End;
  ModalResult := mrOk;
End;

Procedure TMoveTorrentForm.btBrowseClick(Sender: TObject);
Var
  s: String;
Begin
  s := MainForm.SelectRemoteFolder(edTorrentDir.Text, SSelectFolder);
  If s <> '' Then
    edTorrentDir.Text := s;
End;

Procedure TMoveTorrentForm.FormCreate(Sender: TObject);
Begin
  Buttons.OKButton.ModalResult := mrNone;
  Buttons.OKButton.OnClick := @btOKClick;
  bidiMode := GetBiDi();
  btBrowse.Left := edTorrentDir.Left + edTorrentDir.Width + 8;
  // fix button
End;

Initialization
  {$I movetorrent.lrs}

End.
