

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

Unit utils;

{$mode objfpc}{$H+}

Interface

Uses 
SysUtils, Classes, Controls, Forms, IniFiles,
{$ifdef windows}
Windows, win32int, InterfaceBase
{$endif}
{$ifdef unix}
baseunix, unix, unixutil, process
{$endif}
;

Type 

  { TFileStreamUTF8 }

  TFileStreamUTF8 = Class(THandleStream)
    Private 
      FFileName: utf8string;
    Public 
      constructor Create(Const AFileName: utf8string; Mode: Word);
      constructor Create(Const AFileName: utf8string; Mode: Word; Rights:
                         Cardinal);
      destructor Destroy;
      override;
      property FileName: utf8string Read FFilename;
  End;


  { TIniFileUtf8 }

  TIniFileUtf8 = Class(TIniFile)
    Private 
      FStream: TFileStreamUTF8;
      FFileName: string;
    Public 
      constructor Create(Const AFileName: String; AEscapeLineFeeds : Boolean =
                         False);
      override;
      destructor Destroy;
      override;
      Procedure UpdateFile;
      override;
      Function getFileName() : string;
  End;



Function FileOpenUTF8(Const FileName : String; Mode : Integer) : THandle;
Function FileCreateUTF8(Const FileName : String) : THandle;
Function FileCreateUTF8(Const FileName : String; Rights: Cardinal) : THandle;

Function GetTimeZoneDelta: TDateTime;

Procedure ShowTaskbarButton;
Procedure HideTaskbarButton;
Function IsTaskbarButtonVisible: boolean;

Procedure CenterOnParent(C: TControl);
Procedure EnableControls(AEnable: boolean; Const AControls: Array Of TControl);

Function OpenURL(Const URL: String; Const Params: String = ''): boolean;

Function CompareFilePath(Const p1, p2: String): integer;

Procedure AppBusy;
Procedure AppNormal;
Procedure ForceAppNormal;

Function ParamStrUTF8(Param: Integer): utf8string;
Function ParamCount: integer;
Function GetCmdSwitchValue(Const Switch: String): string;

Function Base32Decode(Const s: ansistring): ansistring;

{$ifdef CALLSTACK}
Function GetLastExceptionCallStack: string;
{$endif CALLSTACK}

{$ifdef mswindows}
Procedure AllowSetForegroundWindow(dwProcessId: DWORD);
{$endif mswindows}

Implementation

Uses 
{$ifdef CALLSTACK}
lineinfo2,
{$endif CALLSTACK}
FileUtil, LazUTF8, LazFileUtils, StdCtrls, Graphics;

{$ifdef windows}
Function FileOpenUTF8(Const FileName : String; Mode : Integer) : THandle;

Const 
  AccessMode: array[0..2] Of Cardinal  = (
                                          GENERIC_READ,
                                          GENERIC_WRITE,
                                          GENERIC_READ Or GENERIC_WRITE);
  ShareMode: array[0..4] Of Integer = (
                                       0,
                                       0,
                                       FILE_SHARE_READ,
                                       FILE_SHARE_WRITE,
                                       FILE_SHARE_READ Or FILE_SHARE_WRITE);
Begin
  Result := CreateFileW(PWideChar(UTF8Decode(FileName)), dword(AccessMode[Mode
            And 3]),
            dword(ShareMode[(Mode And $F0) shr 4]), Nil, OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL, 0);
  //if fail api return feInvalidHandle (INVALIDE_HANDLE=feInvalidHandle=-1)
End;

Function FileCreateUTF8(Const FileName : String) : THandle;
Begin
  Result := CreateFileW(PWideChar(UTF8Decode(FileName)), GENERIC_READ Or
            GENERIC_WRITE,
            0, Nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
End;

Function FileCreateUTF8(Const FileName : String; Rights: Cardinal) : THandle;
Begin
  Result := CreateFileW(PWideChar(UTF8Decode(FileName)), GENERIC_READ Or
            GENERIC_WRITE,
            0, Nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
End;

Var 
  FParams: TStringList;

Function ParamStrUTF8(Param: Integer): utf8string;

Function SkipSpaces( P: PWideChar ): PWideChar;
Begin
  While True Do
    Begin
      While (P[0] <> #0) And (P[0] <= ' ') Do
        Inc(P);
      If (P[0] = '"') And (P[1] = '"') Then Inc(P, 2)
      Else Break;
    End;
  Result := P;
End;

Function SkipParam(P: PWideChar): PWideChar;
Begin
  P := SkipSpaces( P );
  While P[0] > ' ' Do
    If P[0] = '"' Then
      Begin
        Inc(P);
        While (P[0] <> #0) And (P[0] <> '"') Do
          Inc(P);
        If P[0] <> #0 Then Inc(P);
      End
    Else
      Inc(P);
  Result := P;
End;

Var 
  P, P1: PWideChar;
  s: widestring;
Begin
  If Win32Platform <> VER_PLATFORM_WIN32_NT Then
    Begin
      Result := LazUTF8.ParamStrUTF8(Param);
      exit;
    End;

  If FParams = Nil Then
    Begin
      FParams := TStringList.Create;
      P := GetCommandLineW;
      While True Do
        Begin
          P := SkipSpaces( P );
          P1 := P;
          P := SkipParam(P);
          If P = P1 Then
            break;
          s := Copy( P1, 1, P - P1 );
          If Length(s) >= 2 Then
            If (s[1] = '"') And (s[Length(s)] = '"') Then
              s := Copy(s, 2, Length(s) - 2);
          FParams.Add(UTF8Encode(s));
        End;
      // Getting real executable name
      SetLength(s, 1000);
      SetLength(s, GetModuleFileNameW(HINSTANCE, PWideChar(s), Length(s) + 1));
      If s <> '' Then
        If FParams.Count > 0 Then
          FParams[0] := UTF8Encode(s)
      Else
        FParams.Add(UTF8Encode(s));
    End;

  If Param >= FParams.Count Then
    Result := ''
  Else
    Result := FParams[Param];
End;

Function ParamCount: integer;
Begin
  If Win32Platform <> VER_PLATFORM_WIN32_NT Then
    Result := System.ParamCount
  Else
    Begin
      If FParams = Nil Then
        ParamStrUTF8(0);
      Result := FParams.Count - 1;
    End;
End;

{$else}
// Non-Windows targets

Function FileOpenUTF8(Const FileName : String; Mode : Integer) : THandle;
Begin
  Result := FileOpen(FileName, Mode);
End;

Function FileCreateUTF8(Const FileName : String) : THandle;
Begin
  Result := FileCreate(FileName);
End;

Function FileCreateUTF8(Const FileName : String; Rights: Cardinal) : THandle;
Begin
  Result := FileCreate(FileName, Rights);
End;

Function ParamStrUTF8(Param: Integer): utf8string;
Begin
  Result := LazUTF8.ParamStrUTF8(Param);
End;

Function ParamCount: integer;
Begin
  Result := System.ParamCount;
End;

{$endif windows}

{ TFileStreamUTF8 }

constructor TFileStreamUTF8.Create(Const AFileName: utf8string; Mode: Word);

Var 
  lHandle: THandle;
Begin
  FFileName := AFileName;
  If Mode = fmcreate Then
    lHandle := FileCreateUTF8(AFileName)
  Else
    lHandle := FileOpenUTF8(AFileName, Mode);

  If (THandle(lHandle)=feInvalidHandle) Then
    Begin
      If Mode = fmCreate Then
        raise EFCreateError.createfmt({SFCreateError}
                                      'Unable to create file "%s"', [AFileName])
      Else
        raise EFOpenError.Createfmt({SFOpenError}'Unable to open file "%s"', [
                                    AFilename]);
    End
  Else
    inherited Create(lHandle);
End;

constructor TFileStreamUTF8.Create(Const AFileName: utf8string; Mode: Word;
                                   Rights: Cardinal);

Var 
  lHandle: THandle;
Begin
  FFileName := AFileName;
  If Mode=fmcreate Then
    lHandle := FileCreateUTF8(AFileName,Rights)
  Else
    lHandle := FileOpenUTF8(AFileName,Mode);

  If (THandle(lHandle)=feInvalidHandle) Then
    Begin
      If Mode=fmcreate Then
        raise EFCreateError.createfmt({SFCreateError}
                                      'Unable to create file "%s"',[AFileName])
      Else
        raise EFOpenError.Createfmt({SFOpenError}'Unable to open file "%s"',[
                                    AFilename]);
    End
  Else
    inherited Create(lHandle);
End;

destructor TFileStreamUTF8.Destroy;
Begin
  FileClose(Handle);
End;

{ TIniFileUtf8 }

constructor TIniFileUtf8.Create(Const AFileName: String; AEscapeLineFeeds:
                                Boolean);

Var 
  m: integer;
Begin
  FFileName := AFileName;
  If FileExistsUTF8(FFileName) Then
    m := fmOpenRead Or fmShareDenyNone
  Else
    m := fmCreate;
  FStream := TFileStreamUTF8.Create(AFileName, m);
  inherited Create(FStream, AEscapeLineFeeds);
  FileClose(FStream.Handle);
  THandle(pointer(@FStream.Handle)^) := 0;
End;

destructor TIniFileUtf8.Destroy;
Begin
  inherited Destroy;
  FStream.Free;
End;

Procedure TIniFileUtf8.UpdateFile;

Var 
  h: THANDLE;
Begin
  If FileExistsUTF8(FFileName) Then
    h := FileOpenUTF8(FFileName, fmOpenWrite Or fmShareDenyWrite)
  Else
    h := FileCreateUTF8(FFileName);
  If h = THandle(-1) Then
    raise Exception.Create('Unable to write to INI file.' + LineEnding +
                           SysErrorMessageUTF8(GetLastOSError));
  THandle(pointer(@FStream.Handle)^) := h;
  Try
    inherited UpdateFile;
    FStream.Size := FStream.Position;
  Finally
    FileClose(FStream.Handle);
    THandle(pointer(@FStream.Handle)^) := 0;
End;
End;

// ---------------------------------------------

Function GetTimeZoneDelta: TDateTime;
{$ifdef windows}

Var 
  t: TIME_ZONE_INFORMATION;
  res: dword;
{$endif}
Begin
  Result := 0;
{$ifdef windows}
  res := GetTimeZoneInformation(t);
  If res<> TIME_ZONE_ID_INVALID Then
    Begin
      Case res Of 
        TIME_ZONE_ID_STANDARD:
                               Result := -t.StandardBias;
        TIME_ZONE_ID_DAYLIGHT:
                               Result := -t.DaylightBias;
      End;
      Result := (-t.Bias + Result)/MinsPerDay;
    End;
{$endif}
{$ifdef unix}
  Result := Tzseconds/SecsPerDay;
{$endif}
End;

Procedure ShowTaskbarButton;
Begin
{$ifdef mswindows}
  ShowWindow(TWin32WidgetSet(WidgetSet).AppHandle, SW_SHOW);
{$else}
  Application.MainForm.Visible := True;
{$endif mswindows}
End;

Procedure HideTaskbarButton;
Begin
{$ifdef mswindows}
  ShowWindow(TWin32WidgetSet(WidgetSet).AppHandle, SW_HIDE);
{$else}
  Application.MainForm.Visible := False;
{$endif mswindows}
End;

Function IsTaskbarButtonVisible: boolean;
Begin
{$ifdef mswindows}
  Result := IsWindowVisible(TWin32WidgetSet(WidgetSet).AppHandle);
{$else}
  Result := Application.MainForm.Visible;
{$endif mswindows}
End;

{$ifdef unix}
Function UnixOpenURL(Const FileName: String): Integer;

Var 
  WrkProcess: TProcess;
  cmd, fn: String;
Begin
  Result := -1;
  WrkProcess := TProcess.Create(Nil);
  WrkProcess.Options := [poNoConsole,poWaitOnExit];

  cmd := FindDefaultExecutablePath('xdg-open');
  If cmd = '' Then
    Begin
      cmd := FindDefaultExecutablePath('gnome-open');
      If cmd = '' Then
        Begin
          cmd := FindDefaultExecutablePath('kioclient');
          If cmd <> '' Then
            Wrkprocess.Parameters.Add('exec')
          Else
            Begin
              cmd := FindDefaultExecutablePath('kfmclient');
              If cmd = '' Then
                exit;
              Wrkprocess.Parameters.Add('exec')
            End;
        End;
    End;

  fn := FileName;
  If Pos('://', fn) > 0 Then
    fn := StringReplace(fn, '#', '%23', [rfReplaceAll]);
  Wrkprocess.Parameters.Add(fn);
  WrkProcess.Executable := cmd;

  Try
    WrkProcess.Execute;
    Result := WrkProcess.ExitStatus;
  Finally
    WrkProcess.Free;
End;
End;
{$endif unix}

Function OpenURL(Const URL, Params: String): boolean;
{$ifdef mswindows}

Var 
  s, p: widestring;
{$endif mswindows}
Begin
{$ifdef mswindows}
  s := UTF8Decode(URL);
  p := UTF8Decode(Params);
  If Win32Platform = VER_PLATFORM_WIN32_NT Then
    Result := ShellExecuteW(0, 'open', PWideChar(s), PWideChar(p), Nil,
              SW_SHOWNORMAL) > 32
  Else
    Result := ShellExecuteA(0, 'open', PChar(ansistring(s)), PChar(ansistring(p)
              ), Nil, SW_SHOWNORMAL) > 32;
{$endif mswindows}

{$ifdef darwin}
  Result := fpSystem('open "' + URL + '"') = 0;
{$else darwin}

  {$ifdef unix}
  Result := UnixOpenURL(URL) = 0;
  {$endif unix}

{$endif darwin}
End;

Var 
  BusyCount: integer = 0;

Procedure AppBusy;
Begin
  Inc(BusyCount);
  Screen.Cursor := crHourGlass;
End;

Procedure AppNormal;
Begin
  Dec(BusyCount);
  If BusyCount <= 0 Then
    Begin
      BusyCount := 0;
      Screen.Cursor := crDefault;
    End;
End;

Procedure ForceAppNormal;
Begin
  BusyCount := 0;
  AppNormal;
End;

{$ifdef mswindows}
Procedure AllowSetForegroundWindow(dwProcessId: DWORD);

Type 
  TAllowSetForegroundWindow = Function (dwProcessId: DWORD): BOOL;
  stdcall;

Var 
  _AllowSetForegroundWindow: TAllowSetForegroundWindow;
Begin
  _AllowSetForegroundWindow := TAllowSetForegroundWindow(GetProcAddress(
                               GetModuleHandle('user32.dll'),
                               'AllowSetForegroundWindow'));
  If Assigned(_AllowSetForegroundWindow) Then
    _AllowSetForegroundWindow(dwProcessId);
End;
{$endif mswindows}

Function CompareFilePath(Const p1, p2: String): integer;
Begin
{$ifdef windows}
  Result := AnsiCompareText(UTF8Decode(p1), UTF8Decode(p2));
{$else}
  Result := AnsiCompareStr(UTF8Decode(p1), UTF8Decode(p2));
{$endif windows}
End;

Function GetCmdSwitchValue(Const Switch: String): string;

Var 
  i, len: integer;
  s, ss: string;
Begin
  Result := '';
  ss := '--' + Switch + '=';
  len := Length(ss);
  For i:=1 To ParamCount Do
    Begin
      s := ParamStrUTF8(i);
      If Copy(s, 1, len) = ss Then
        Begin
          Result := Copy(s, len + 1, MaxInt);
          break;
        End;
    End;
End;

{$ifdef CALLSTACK}
Function GetLastExceptionCallStack: string;

Function GetAddrInfo(addr: pointer): string;

Var 
  func,
  source : shortstring;
  line   : longint;
Begin
  GetLineInfo(ptruint(addr), func, source, line);
  Result := '$' + HexStr(ptruint(addr), sizeof(ptruint) * 2);
  If func<>'' Then
    Result := Result + '  ' + func;
  If source<>'' Then
    Begin
      If func<>'' Then
        Result := Result + ', ';
      If line<>0 Then
        Result := Result + ' line ' + IntToStr(line);
      Result := Result + ' of ' + source;
    End;
End;

Var 
  I: Integer;
  Frames: PPointer;
Begin
  Result := GetAddrInfo(ExceptAddr);
  Frames := ExceptFrames;
  For I := 0 To ExceptFrameCount - 1 Do
    Result := Result + LineEnding + GetAddrInfo(Frames[I]);
End;
{$endif CALLSTACK}

Procedure CenterOnParent(C: TControl);

Var 
  R: TRect;
Begin
  R := C.BoundsRect;
  R.Left := (C.Parent.ClientWidth - C.Width) Div 2;
  R.Top := (C.Parent.ClientHeight - C.Height) Div 2;
  C.BoundsRect := R;
End;

{$PUSH}
{$RANGECHECKS OFF}
Function Base32Decode(Const s: ansistring): ansistring;

Var 
  optr, len, bcnt: integer;
  Output: PByteArray;
  Input: PAnsiChar;
  w: word;
  c: ansichar;
  b: byte;
Begin
  len := Length(s);
  Input := PAnsiChar(s);
  SetLength(Result, len);
  Output := PByteArray(PAnsiChar(Result));
  optr := 0;
  w := 0;
  bcnt := 0;
  While len > 0 Do
    Begin
      Repeat
        c := Input^;
        If c = '=' Then
          Begin
            len := 0;
            break;
          End;
        If c In ['A'..'Z'] Then
          b := Ord(c) - Ord('A')
        Else
          If c In ['2'..'7'] Then
            b := Ord(c) - 24
        Else
          raise Exception.Create('Invalid base32 string.');
        w := (w shl 5) Or b;
        Inc(bcnt, 5);
        Inc(Input);
        Dec(len);
      Until (bcnt >= 8) Or (len <= 0);
      If bcnt < 8 Then
        Output^[optr] := w shl (8 - bcnt)
      Else
        Begin
          Dec(bcnt, 8);
          Output^[optr] := w shr bcnt;
        End;
      Inc(optr);
    End;
  SetLength(Result, optr);
End;
{$POP}

Procedure EnableControls(AEnable: boolean; Const AControls: Array Of TControl);

Var 
  i: integer;
  C: TControl;
Begin
  For i:=Low(AControls) To High(AControls) Do
    Begin
      C := AControls[i];
      C.Enabled := AEnable;
      If (C is TCustomEdit) Or (C is TCustomListBox) Or (C is TCustomComboBox)
        Then
        Begin
          If AEnable Then
            C.Color := clDefault
          Else
            C.Color := clBtnFace;
        End;
    End;
End;


Function TIniFileUtf8.getFileName() : string;
Begin
  result := FFileName;
End;


finalization
{$ifdef windows}
FreeAndNil(FParams);
{$endif windows}

End.
