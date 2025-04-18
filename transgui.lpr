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
  
  In addition, as a special exception, the copyright holders give permission to 
  link the code of portions of this program with the
  OpenSSL library under certain conditions as described in each individual
  source file, and distribute linked combinations including the two.

  You should have received a copy of the GNU General Public License
  along with Transmission Remote GUI; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*************************************************************************************}

{$ifdef windows}
{$apptype gui}
{$endif windows}

Program transgui;

{$mode objfpc}{$H+}

Uses
  {$ifdef UNIX}
  cthreads,
  {$ifdef darwin}
  maclocale,
  {$else}
  clocale,
  {$endif}
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms { you can add units after this },
  BaseForm,
  Main,
  rpc,
  AddTorrent,
  ConnOptions,
  varlist,
  TorrProps,
  DaemonOptions,
  About,
  IpResolver,
  download,
  ColSetup,
  utils,
  ResTranslator,
  AddLink,
  MoveTorrent,
  AddTracker,
  Options,
  passwcon;

  //{$ifdef windows}
  {$R *.res}
  //{$endif}

Begin
  //Application.Scaled:=True; //travis doesnt compile

  If Not CheckAppParams Then exit;

  //  Application.Scaled:=True;
  Application.Title := '';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
End.
