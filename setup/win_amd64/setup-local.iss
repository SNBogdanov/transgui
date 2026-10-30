#ifndef AppVersion
  #define AppVersion GetFileVersion(SourcePath+'..\..\units\transgui.exe')
  #define AppVersion Copy(AppVersion, 1, RPos('.', AppVersion) - 1)
  #define tmpvar Copy(AppVersion, RPos('.', AppVersion) + 1, 3)
  #if tmpvar == "0"
    #define AppVersion Copy(AppVersion, 1, RPos('.', AppVersion) - 1)
  #endif
  #undef tmpvar
#endif

#define AppName "Transmission Remote GUI"
#define AppVerName AppName + " " + AppVersion
#define AppPublisher "Bogdanov Sergey"
#define AppURL "https://github.com/SNBogdanov/transgui"
#define AppExeName "transgui.exe"

[Setup]
AppId=transgui
AppName={#AppName}
AppVerName={#AppVerName}
AppCopyright=Copyright (c) 2026 by Bogdanov Sergey
AppPublisher={#AppPublisher}
AppPublisherURL={#AppURL}
AppSupportURL={#AppURL}
AppUpdatesURL={#AppURL}
UninstallDisplayIcon={app}\transgui.exe
SetupIconFile=..\..\transgui.ico
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
VersionInfoVersion={#GetFileVersion(SourcePath+'..\..\units\transgui.exe')}
VersionInfoTextVersion={#GetFileVersion(SourcePath+'..\..\units\transgui.exe')}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
AllowNoIcons=yes
LicenseFile=..\..\LICENSE
OutputDir=..\..\Release
OutputBaseFilename=transgui-{#AppVersion}-setup_64bit
Compression=lzma2
SolidCompression=yes
PrivilegesRequired=admin
ChangesAssociations=yes
CloseApplications=yes
UsePreviousAppDir=yes
UsePreviousGroup=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: "regfileext"; Description: "{cm:AssocFileExtension,{#AppName},.torrent}"; Flags: unchecked
Name: "regmagnet"; Description: "Handle magnet links by {#AppName}"; Flags: unchecked

[Files]
Source: "..\..\units\transgui.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\LICENSE"; DestDir: "{app}"; DestName: "LICENSE.txt"; Flags: ignoreversion
Source: "..\..\README.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\history.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\lang\transgui.*"; DestDir: "{app}\lang"; Flags: ignoreversion
Source: "openssl\libcrypto-1_1-x64.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "openssl\libssl-1_1-x64.dll"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\{#AppExeName}"; WorkingDir: "{app}"
Name: "{group}\History"; Filename: "{app}\history.txt"; WorkingDir: "{app}"
Name: "{group}\Read me"; Filename: "{app}\README.md"; WorkingDir: "{app}"
Name: "{group}\License"; Filename: "{app}\LICENSE.txt"; WorkingDir: "{app}"
Name: "{group}\GitHub repository"; Filename: "{#AppURL}"; WorkingDir: "{app}"
Name: "{group}\{cm:UninstallProgram,{#AppName}}"; Filename: "{uninstallexe}"; WorkingDir: "{app}"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\{#AppExeName}"; WorkingDir: "{app}"; Tasks: desktopicon

[Registry]
Root: HKCU; Subkey: "Software\Classes\.torrent"; ValueType: string; ValueName: ""; ValueData: "{#AppName}"; Flags: uninsdeletevalue; Tasks: regfileext
Root: HKCU; Subkey: "Software\Classes\{#AppName}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppExeName}"",0"; Flags: uninsdeletevalue; Tasks: regfileext
Root: HKCU; Subkey: "Software\Classes\{#AppName}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppExeName}"" ""%1"""; Flags: uninsdeletevalue; Tasks: regfileext
Root: HKCU; Subkey: "Software\Classes\Magnet"; ValueType: string; ValueName: ""; ValueData: "Magnet URI"; Flags: createvalueifdoesntexist uninsdeletevalue; Tasks: regmagnet
Root: HKCU; Subkey: "Software\Classes\Magnet"; ValueType: string; ValueName: "Content Type"; ValueData: "application/x-magnet"; Flags: uninsdeletevalue; Tasks: regmagnet
Root: HKCU; Subkey: "Software\Classes\Magnet"; ValueType: string; ValueName: "URL Protocol"; ValueData: ""; Flags: uninsdeletevalue; Tasks: regmagnet
Root: HKCU; Subkey: "Software\Classes\Magnet\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppExeName}"",0"; Flags: uninsdeletevalue; Tasks: regmagnet
Root: HKCU; Subkey: "Software\Classes\Magnet\shell"; ValueType: string; ValueName: ""; ValueData: "open"; Flags: uninsdeletevalue; Tasks: regmagnet
Root: HKCU; Subkey: "Software\Classes\Magnet\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppExeName}"" ""%1"""; Flags: uninsdeletevalue; Tasks: regmagnet

[Run]
Filename: "{app}\{#AppExeName}"; Description: "{cm:LaunchProgram,{#AppName}}"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: filesandordirs; Name: "{localappdata}\{#AppName}"
