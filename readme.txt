Transmission Remote GUI.
Copyright (c) 2008-2019 by Yury Sidorov and Transmission Remote GUI working group.
Copyright (c) 2026 by Bogdanov Sergey.

This is a fork of Transmission Remote GUI maintained by Bogdanov Sergey.

Transmission Remote GUI is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Transmission Remote GUI is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

In addition, as a special exception, OpenVPN Technologies, Inc. gives
permission to link the code of this program with the OpenSSL Library (or with
modified versions of OpenSSL that use the same license as OpenSSL), and
distribute linked combinations including the two. You must obey the GNU General
Public License in all respects for all of the code used other than OpenSSL. If
you modify this file, you may extend this exception to your version of the
file, but you are not obligated to do so. If you do not wish to do so, delete
this exception statement from your version.
*********************************************************************************

Transmission Remote GUI is a feature rich Windows front-end to remotely control Transmission daemon via its RPC protocol. It is faster and has more functionality than builtin Transmission web interface.

Transmission Remote GUI is developed using Lazarus RAD and Free Pascal compiler.

Features:
 * Native application for Windows
 * uTorrent-like interface
 * Select files to download
 * Choose files priority
 * View details about connected peers
 * Full information about each torrent
 * Per torrent options
 * Torrent archive manager
 * Server-side script runner integration
 * Runtime and per-host script parameters
 * Live script output
 * Whisparr integration

Project home:
https://github.com/SNBogdanov/transgui

INSTALLATION

WINDOWS:

Portable zip tarball (recommended).

- Download the zip tarball from the GitHub Releases page.
- Extract it to any folder.
- Run transgui.exe or create a shortcut for it.
- If SSL/TLS support is needed, install the Visual C++ Redistributable Package and OpenSSL libraries.

Installer.

- Download the installer from the GitHub Releases page.
- Run the installer and follow the installation steps.

COMMAND LINE PARAMETERS

You can specify path to a .torrent file or a magnet link as a command line parameter. The program will add the specified torrent.

-hidden : Start the program hidden. Only the program's tray icon will be visible.
--home=<home_dir> : Specifies a home directory for the program. All program's settings are stored in the home directory. You can run multiple instances of the program by specifying different home directories.

PORTABLE MODE

If the program finds the transgui.ini file in the same folder as the binary file, then it will store all configuration and data files in the program's folder, instead of the folder in a user profile.

FIXED SHORTCUTS

Alt + 1 : All Torrents
Alt + 2 : Downloading
Alt + 3 : Completed
Alt + 4 : Active
Alt + 5 : Inactive
Alt + 6 : Stopped
Alt + 7 : Error
Alt + 8 : Waiting
Alt + S : Searchbox (filter torrents by keywords) - Esc cancel filter and clean the box.
Alt + G : Info Pane - General Tab
Alt + K : Info Pane - Trackers Tab
Alt + P : Info Pane - Peers Tab
Alt + F : Info Pane - Files Tab


ADVANCED PARAMETERS

There are some parameters in the transgui.ini file, that can not be modified via the GUI.
More info on: https://github.com/transmission-remote-gui/transgui/issues/924  (File Manager & Shortcuts)
, https://github.com/transmission-remote-gui/transgui/issues/1020  (User Defined Menu -Windows Only-)
and https://github.com/transmission-remote-gui/transgui/issues/1070  (.torrent Auto Opening)

[Interface]
; Maximum number of elements in the folder history list
MaxFoldersHistory=10

[Interface]
;Alternate File Manager (Windows Only)
FileManagerDefault={Full path to your File Manager .exe}
FileManagerDefaultParam={Alternate parameters, could be left blank}

[Interface]
;System Wide Shortcut key (Windows Only)
GlobalHotkey={Virtual Key Code} full list here http://docwiki.embarcadero.com/RADStudio/Seattle/en/Virtual_Key_Codes (Plus VK_A...VK_Z and VK_0..VK_9)
GlobalHotkeyMod={Modifier Key} [MOD_SHIFT , MOD_CONTROL , MOD_ALT , MOD_WIN alone or combined with + sign]

[Interface]
WatchLocalFolder= {LOCAL Folder to watch for torrent files}
WatchDestinationFolder= {REMOTE destination where the data would be saved if missing or empty last destination folder is used}
WatchInterval=1 {Time period in MINUTES between folder scans for torrents, may be fractional values 0,50 = 30 seconds}


[Shortcuts]
;Modify all the shortcuts of the GUI here

[Usermenu]
Caption1={Caption in the menu}
ExeName1={Full path to the program .exe you want to add to menu}
Params1="%s" {Usually "%s" but some programs may require additional parameters}
Caption2={same for item 2}
ExeName2={same for item 2}
Params2={same for item 2}

[StatusBarPanels]
;Customize the width of the statusbar panels to fit your language
;0 is the left most panel and 7 is the right most panel.
0=327
1=152
2=152
3=130
4=130
5=130
6=130
7=130

[MainForm]
BigToolBarHeight=48 {Height of the Big Icon Toolbar if missing default is 64}

[MainForm]
FromNow=1 {1=Shows the dates relatives to now , 0=Absolute Dates MM/DD/YY HH:MM:SS}
*********************************************************************************
Big Icons

Farm-Fresh Fatcow Web Hosting
http://www.fatcow.com/
License Creative Commons (Attribution 3.0 United States)
https://creativecommons.org/licenses/by/3.0/us/legalcode

Visual Farm
http://icons8.com/
License Creative Commons Attribution-No Derivative Works 3.0 Unported
https://creativecommons.org/licenses/by-nd/3.0/legalcode
