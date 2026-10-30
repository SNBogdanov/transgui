
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

Unit ArchiveCommon;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, StdCtrls, Variants, fpjson;

Const
  ArchiveSection = 'Archive';
  ArchiveNamesKey = 'Names';
  ArchiveLastKey = 'Last';
  ArchiveVerifyKey = 'VerifyRestore';

Function IsValidArchiveName(Const Name: String): Boolean;
Function ArchiveServerSection: String;
Procedure LoadArchiveNames(CB: TComboBox);
Function LoadArchiveNamesFromServer(CB: TComboBox): Boolean;
Procedure RememberArchiveName(Const Name: String);
Function ArchiveRequest(Args: TJSONObject; ATimeOut: Integer = 120000): TJSONObject;
Function TorrentIdsToJSON(Const Ids: Variant): TJSONArray;
Function StringsToJSON(Values: TStrings): TJSONArray;
Function JsonStr(Obj: TJSONObject; Const Name: String; Const ADefault: String = ''): String;
Function JsonInt64(Obj: TJSONObject; Const Name: String; ADefault: Int64 = 0): Int64;
Function JsonBool(Obj: TJSONObject; Const Name: String; ADefault: Boolean = False): Boolean;
Function JsonStrArray(Obj: TJSONObject; Const Name: String): String;
Function UnixTimeToText(Value: Int64): String;

Implementation

Uses DateUtils, utils, main;

Function IsValidArchiveName(Const Name: String): Boolean;
Var
  i: Integer;
  c: Char;
Begin
  Result := False;
  If (Name = '') Or (Length(Name) > 128) Or (Name = '.') Or (Name = '..') Then
    exit;
  For i := 1 To Length(Name) Do
  Begin
    c := Name[i];
    If (Ord(c) < 32) Or (c In ['/', '\', ':', '*', '?', '"', '<', '>', '|']) Then
      exit;
  End;
  Result := True;
End;

Function ArchiveServerSection: String;
Var
  s: String;
  i: Integer;
Begin
  Result := ArchiveSection;
  If Not Assigned(RpcObj) Then
    exit;
  s := Trim(LowerCase(RpcObj.Url));
  If s = '' Then
    s := Trim(LowerCase(RpcObj.Http.TargetHost));
  If s = '' Then
    s := 'default';
  If Trim(RpcObj.Http.UserName) <> '' Then
    s := s + '_' + Trim(RpcObj.Http.UserName);
  For i := 1 To Length(s) Do
    If s[i] In [':', '/', '[', ']', '=', #13, #10] Then
      s[i] := '_';
  Result := ArchiveSection + '.' + s;
End;

Procedure LoadArchiveNames(CB: TComboBox);
Var
  sl: TStringList;
  s: String;
  Sec: String;
Begin
  Sec := ArchiveServerSection;
  sl := TStringList.Create;
  Try
    sl.CommaText := Ini.ReadString(Sec, ArchiveNamesKey, '');
    CB.Items.Assign(sl);
  Finally
    sl.Free;
  End;
  s := Ini.ReadString(Sec, ArchiveLastKey, '');
  If s <> '' Then
    CB.Text := s;
End;

Procedure RememberArchiveName(Const Name: String);
Var
  sl: TStringList;
  i: Integer;
  Sec: String;
Begin
  If Name = '' Then
    exit;
  Sec := ArchiveServerSection;
  sl := TStringList.Create;
  Try
    sl.CommaText := Ini.ReadString(Sec, ArchiveNamesKey, '');
    i := sl.IndexOf(Name);
    If i >= 0 Then
      sl.Delete(i);
    sl.Insert(0, Name);
    While sl.Count > 20 Do
      sl.Delete(sl.Count - 1);
    Ini.WriteString(Sec, ArchiveNamesKey, sl.CommaText);
    Ini.WriteString(Sec, ArchiveLastKey, Name);
  Finally
    sl.Free;
  End;
End;

Function LoadArchiveNamesFromServer(CB: TComboBox): Boolean;
Var
  args, res: TJSONObject;
  d: TJSONData;
  arr: TJSONArray;
  i: Integer;
  current: String;
Begin
  Result := False;
  If CB = nil Then
    exit;
  current := CB.Text;
  args := TJSONObject.Create;
  args.Add('action', 'list_archives');
  res := ArchiveRequest(args, 60000);
  If res = nil Then
  Begin
    RpcObj.Status := '';
    exit;
  End;
  Try
    d := res.Find('archives');
    If (d = nil) Or Not (d Is TJSONArray) Then
      exit;
    arr := TJSONArray(d);
    CB.Items.BeginUpdate;
    Try
      CB.Items.Clear;
      For i := 0 To arr.Count - 1 Do
        If arr.Items[i].JSONType = jtString Then
          CB.Items.Add(arr.Items[i].AsString);
    Finally
      CB.Items.EndUpdate;
    End;
    CB.Text := current;
    Result := True;
  Finally
    res.Free;
  End;
End;

Function ArchiveRequest(Args: TJSONObject; ATimeOut: Integer): TJSONObject;
Var
  req: TJSONObject;
Begin
  Result := nil;
  If Args = nil Then
    exit;
  req := TJSONObject.Create;
  Try
    req.Add('method', 'torrent_archive');
    req.Add('arguments', Args);
    Result := RpcObj.SendRequest(req, True, ATimeOut);
  Finally
    req.Free;
  End;
End;

Function TorrentIdsToJSON(Const Ids: Variant): TJSONArray;
Var
  i: Integer;
Begin
  Result := TJSONArray.Create;
  If VarIsEmpty(Ids) Or VarIsNull(Ids) Then
    exit;
  If VarIsArray(Ids) Then
    For i := VarArrayLowBound(Ids, 1) To VarArrayHighBound(Ids, 1) Do
      Result.Add(Integer(Ids[i]))
  Else
    Result.Add(Integer(Ids));
End;

Function StringsToJSON(Values: TStrings): TJSONArray;
Var
  i: Integer;
Begin
  Result := TJSONArray.Create;
  If Values = nil Then
    exit;
  For i := 0 To Values.Count - 1 Do
    Result.Add(Values[i]);
End;

Function FindArchiveValue(Obj: TJSONObject; Const Name: String): TJSONData;
Begin
  Result := nil;
  If Obj = nil Then
    exit;
  Result := Obj.Find(Name);
  If Result <> nil Then
    exit;
  // Legacy (Transmission 4 style) RPC responses use different key names.
  If Name = 'hash_string' Then
    Result := Obj.Find('hashString')
  Else If Name = 'added_date' Then
    Result := Obj.Find('addedDate')
  Else If Name = 'total_size' Then
    Result := Obj.Find('totalSize')
  Else If Name = 'download_dir' Then
    Result := Obj.Find('download-dir');
End;

Function JsonStr(Obj: TJSONObject; Const Name: String; Const ADefault: String): String;
Var
  d: TJSONData;
Begin
  Result := ADefault;
  If Obj = nil Then
    exit;
  d := FindArchiveValue(Obj, Name);
  If (d <> nil) And (d.JSONType <> jtNull) Then
    Result := d.AsString;
End;

Function JsonInt64(Obj: TJSONObject; Const Name: String; ADefault: Int64): Int64;
Var
  d: TJSONData;
Begin
  Result := ADefault;
  If Obj = nil Then
    exit;
  d := FindArchiveValue(Obj, Name);
  If (d <> nil) And (d.JSONType = jtNumber) Then
    Result := d.AsInt64;
End;

Function JsonBool(Obj: TJSONObject; Const Name: String; ADefault: Boolean): Boolean;
Var
  d: TJSONData;
Begin
  Result := ADefault;
  If Obj = nil Then
    exit;
  d := FindArchiveValue(Obj, Name);
  If (d <> nil) And (d.JSONType = jtBoolean) Then
    Result := d.AsBoolean;
End;

Function JsonStrArray(Obj: TJSONObject; Const Name: String): String;
Var
  d: TJSONData;
  arr: TJSONArray;
  i: Integer;
Begin
  Result := '';
  If Obj = nil Then
    exit;
  d := FindArchiveValue(Obj, Name);
  If (d = nil) Or Not (d Is TJSONArray) Then
    exit;
  arr := TJSONArray(d);
  For i := 0 To arr.Count - 1 Do
  Begin
    If Result <> '' Then
      Result := Result + ', ';
    Result := Result + arr.Items[i].AsString;
  End;
End;

Function UnixTimeToText(Value: Int64): String;
Begin
  If Value = 0 Then
    Result := ''
  Else
    Result := DateTimeToStr(UnixToDateTime(Value) + GetTimeZoneDelta, True);
End;

End.
