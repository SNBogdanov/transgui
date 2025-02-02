

{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2011-2013 by Yury Sidorov
  Copyright (c) 2010 Alex Cherednichenko, aka Alex7Che.

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

Unit ResTranslator;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, StrUtils, SysUtils, FileUtil, LazFileUtils, LResources, TypInfo,
  LCLProc, LazUTF8;

Type

  TWordDelimitersOptions = Set Of (wdIgnoreLeading, wdIgnoreTrailing);

  TResTranslator = Class;

  TTranslateStringEvent = Procedure(Sender: TResTranslator;
    Const ResourceName: Ansistring; Var Accept: Boolean);

  TTranslateStringOption = (tsoNew, tsoUsed, tsoExternal);
  TTranslateStringOptions = Set Of TTranslateStringOption;

  { TTranslateStringList }

  TTranslateStringList = Class(TStringList)
  Private
    Function CorrectGetName(Index: Integer): String;
    Function CorrectGetValue(Const Name: String): String;
    Function GetOptions(Index: Integer): TTranslateStringOptions;
    Function NormaliseQuotedStr(Const S: String): String;
    Function ScanQuotSep(P: PChar): Integer;
    Procedure SetOptions(Index: Integer; Const AValue: TTranslateStringOptions);
  Protected
    Function DoCompareText(Const s1, s2: String): PtrInt; Override;
  Public
    Constructor Create(Const FileName: String); Overload;
    Function IndexOfName(Const Name: String; Var Offset: Integer): Integer;
    Function IndexOfName(Const Name: String): Integer; Override;
    Procedure LoadFromFile(Const FileName: String); Override;
    Procedure SaveToFile(Const FileName: String); Override;
    Procedure Merge(Source: TTranslateStringList; Const NamesOnly: Boolean = False);
    Property CValues[Const Name: String]: String read CorrectGetValue;
    Property CNames[Index: Integer]: String read CorrectGetName;
    Property Options[Index: Integer]: TTranslateStringOptions
      read GetOptions write SetOptions;
  End;

  { TResTranslator }

  TResTranslator = Class(TAbstractTranslator)
  Private
    FIgnoreDelimiters: TWordDelimitersOptions;
    FOnTranslateString: TTranslateStringEvent;
    FStrResLst: TTranslateStringList;
    FModified: Boolean;
    FTranslationLanguage: Ansistring;
    FWordDelims: TSysCharset;
    Function GetStrings: TStringList;
    Procedure SetIgnoreDelimiters(Const AValue: TWordDelimitersOptions);
    Procedure SetOnTranslateString(Const AValue: TTranslateStringEvent);
    Procedure SetWordDelims(Const AValue: TSysCharset);
    Function InternalTranslateString(Const Value: Ansistring;
      IsExternal: Boolean = False): Ansistring;
  Public
    FTranslationFile: String;
    Constructor Create(TranslationFile: Ansistring);
    Destructor Destroy; Override;
    Procedure TranslateStringProperty(Sender: TObject;
      Const Instance: TPersistent; PropInfo: PPropInfo; Var Content: String);
      Override;
    Procedure SaveFile; Overload;
    Procedure SaveFile(Const aFileName: String); Overload;
    Property Modified: Boolean read FModified;
    Property Strings: TStringList read GetStrings;
    Property IgnoreDelimiters: TWordDelimitersOptions
      read FIgnoreDelimiters write SetIgnoreDelimiters;
    Property WordDelims: TSysCharset read FWordDelims write SetWordDelims;
    Property TranslationLanguage: Ansistring read FTranslationLanguage;
    Property OnTranslateString: TTranslateStringEvent
      read FOnTranslateString write SetOnTranslateString;
  End;

Function LoadTranslationFile(Const TranslationFile: Ansistring;
  Const OnTranslate: TTranslateStringEvent = nil): Ansistring;
Procedure SaveTranslationFile; Overload;
Procedure SaveTranslationFile(Const FileName: Ansistring); Overload;
Procedure MakeTranslationFile; Overload;
Procedure MakeTranslationFile(Language: Ansistring); Overload;
Procedure MakeTranslationFile(Const FileName, Language: Ansistring); Overload;
Procedure SupplementTranslationFile(Const FileName: Ansistring);
Procedure SupplementTranslationFiles; Overload;
Procedure SupplementTranslationFiles(Const TranslationFilesPath: Ansistring); Overload;
Function LoadDefaultTranslationFile(Const OnTranslate: TTranslateStringEvent =
  nil): TFileName;
Function LoadDefaultTranslationFile(Const TranslationFilesPath: Ansistring;
  Const OnTranslate: TTranslateStringEvent = nil): TFileName;
Function LoadLanguageTranslation(Const Language: Ansistring;
  Const OnTranslate: TTranslateStringEvent = nil): TFileName;
Function LoadLanguageTranslation(Const Language, TranslationFilesPath: Ansistring;
  Const OnTranslate: TTranslateStringEvent = nil): TFileName;
Function TranslateString(Const Value: Ansistring;
  IsExternal: Boolean = False): Ansistring;
Function ExtractLangName(Const FileName: TFilename): Ansistring;
Function GetAvailableTranslations: TStringList;
Function GetAvailableTranslations(Const SearchPath: Ansistring): TStringList;
Function GetTranslationFileName(Const Language: Ansistring;
  AvailableTranslations: TStringList): Ansistring;
Function DefaultLangDir: Ansistring;
Function IsTranslationFileValid(Const TranslationFile: Ansistring): Boolean;

Const
  sLanguageIDName = 'TranslationLanguage';

Var
  IniFileName: String;

Implementation

Uses
  Forms, utils;

Const
  LineSeparator = '###################';

  { procedures and functions }

Function IsQuoted(Const S: Ansistring; QuoteChar: Char): Boolean; Inline;
Var
  L: Integer;
Begin
  L := Length(S);
  If L > 1 Then
    Result := (S[1] = QuoteChar) And (S[L] = QuoteChar)
  Else
    Result := False;
End;

Function HasSeparator(Const S: Ansistring; Separator: Char): Boolean; Inline;
Begin
  Result := Pos(Separator, S) > 0;
End;

Function ExtractLangName(Const FileName: TFilename): Ansistring;
Begin
  With TTranslateStringList.Create(FileName) Do
  Try
    Result := AnsiDequotedStr(CValues[sLanguageIDName], QuoteChar);
  Finally
    Free;
  End;
End;

Function GetAvailableTranslations: TStringList;
Begin
  Result := GetAvailableTranslations(DefaultLangDir);
End;

Function GetAvailableTranslations(Const SearchPath: Ansistring): TStringList;
Var
  Sr: TSearchRec;
  LangName, s: Ansistring;
Begin
  Result := TStringList.Create;
  If FindFirstUTF8(IncludeTrailingPathDelimiter(SearchPath) + '*',
    faArchive Or faReadOnly, Sr) = 0 Then
    With Result Do
    Begin
      NameValueSeparator := '=';
      QuoteChar := '"';
      Repeat
        If ExtractFileExt(Sr.Name) = '.template' Then
          continue;
        s := IncludeTrailingPathDelimiter(ExtractFilePath(SearchPath)) +
          Sr.Name;
        If IsTranslationFileValid(s) Then
        Begin
          LangName := ExtractLangName(s);
          If LangName <> '' Then
            Add(LangName + NameValueSeparator + Sr.Name);
        End;
      Until FindNextUTF8(Sr) <> 0;
      FindClose(Sr);
    End;
End;

Var
  FDefaultLangDir: Ansistring;

Function DefaultLangDir: Ansistring;
  {$ifdef unix}
    Function _IsLangDir(Const dir: String): boolean;

    Var 
      sr: TSearchRec;
    Begin
      Result := FindFirstUtf8(dir + ExtractFileNameOnly(ParamStrUtf8(0)) + '.*',
                faAnyFile, sr) = 0;
      FindClose(sr);
    End;

    Var 
      s: string;
{$endif unix}
Begin
  If FDefaultLangDir = '' Then
  Begin
    FDefaultLangDir := ExtractFilePath(ParamStrUtf8(0)) + 'lang' +
      DirectorySeparator;
    {$ifdef unix}
          If Not _IsLangDir(FDefaultLangDir) Then
            Begin
              s := '/usr/share/' + ExtractFileNameOnly(ParamStrUtf8(0)) +
                   '/lang/';
              If _IsLangDir(s) Then
                FDefaultLangDir := s
              Else
                Begin
                  s := '/usr/local/share/' + ExtractFileNameOnly(ParamStrUtf8(0)
                       ) + '/lang/';
                  If _IsLangDir(s) Then
                    FDefaultLangDir := s;
                End;
            End;
{$endif unix}
  End;
  Result := FDefaultLangDir;
End;

Function GetResStrings(Name, Value: Ansistring; Hash: Longint; P: pointer): Ansistring;
Var
  Accept: Boolean;
Begin
  With TResTranslator(P) Do
  Begin
    Accept := True;
    If Assigned(OnTranslateString) Then
      OnTranslateString(TResTranslator(P), Name, Accept);
    If Accept Then
      Result := InternalTranslateString(Value)
    Else
      Result := Value;
  End;
End;

Function LoadTranslationFile(Const TranslationFile: Ansistring;
  Const OnTranslate: TTranslateStringEvent = nil): Ansistring;
Begin
  LRSTranslator := TResTranslator.Create(TranslationFile);
  TResTranslator(LRSTranslator).OnTranslateString := OnTranslate;
  SetResourceStrings(@GetResStrings, LRSTranslator);
  Result := TResTranslator(LRSTranslator).TranslationLanguage;
End;

Procedure SupplementTranslationFiles; Overload;
Begin
  SupplementTranslationFiles(DefaultLangDir);
End;

Procedure MakeTranslationFile; Overload;
Begin
  MakeTranslationFile('???');
End;

Procedure MakeTranslationFile(Language: Ansistring); Overload;
Var
  lLang, sLang, s: String;
Begin
  LazGetLanguageIDs(lLang, sLang);
  sLang := AnsiLowerCase(sLang);
  s := ExtractFileNameOnly(ParamStrUtf8(0));
  If (sLang <> '') And Not FileExistsUTF8(DefaultLangDir + s + '.' + sLang) Then
    s := s + '.' + sLang
  Else
    s := s + '.lng';
  MakeTranslationFile(DefaultLangDir + s, Language);
End;

Procedure MakeTranslationFile(Const FileName, Language: Ansistring);
Var
  Dst: TTranslateStringList;
Begin
  If Assigned(LRSTranslator) And (LRSTranslator Is TResTranslator) Then
  Begin
    Dst := TTranslateStringList.Create;
    Try
      Dst.Values[sLanguageIDName] := Language;
      With LRSTranslator As TResTranslator Do
        Dst.Merge(Strings As TTranslateStringList, True);
      ForceDirectories(ExtractFilePath(FileName));
      Dst.SaveToFile(FileName);
    Finally
      Dst.Free;
    End;
  End;
End;

Procedure SupplementTranslationFile(Const FileName: Ansistring);
Var
  Dst: TTranslateStringList;
Begin
  If Assigned(LRSTranslator) And (LRSTranslator Is TResTranslator) Then
  Begin
    Dst := TTranslateStringList.Create(FileName);
    Try
      With LRSTranslator As TResTranslator Do
        Dst.Merge(Strings As TTranslateStringList, True);
      Dst.SaveToFile(FileName);
    Finally
      Dst.Free;
    End;
  End;
End;

Procedure SupplementTranslationFiles(Const TranslationFilesPath: Ansistring);
Var
  Sl: TStringList;
  i: Integer;
  s: String;
Begin
  If Assigned(LRSTranslator) And (LRSTranslator Is TResTranslator) Then
  Begin
    Sl := GetAvailableTranslations(TranslationFilesPath);
    With Sl Do
      For i := 0 To Count - 1 Do
        SupplementTranslationFile(IncludeTrailingPathDelimiter(
          TranslationFilesPath) + ValueFromIndex[i]);
    // Supplement template file
    s := IncludeTrailingPathDelimiter(TranslationFilesPath) +
      ExtractFileNameOnly(ParamStrUtf8(0)) + '.template';
    If FileExistsUTF8(s) Then
      SupplementTranslationFile(s);
  End;
End;

Const
  InvalidLangExt: Array[1..6] Of String = ('ua', 'by', 'cn', 'cz', 'se', 'tw');

Function IsTranslationFileValid(Const TranslationFile: Ansistring): Boolean;
Var
  s: String;
  i: Integer;
Begin
  Result := FileExistsUTF8(TranslationFile);
  If Not Result Then
    exit;
  s := LowerCase(ExtractFileExt(TranslationFile));
  Delete(s, 1, 1);
  For i := Low(InvalidLangExt) To High(InvalidLangExt) Do
    If s = InvalidLangExt[i] Then
    Begin
      Result := False;
      exit;
    End;
End;

Function LoadDefaultTranslationFile(Const OnTranslate: TTranslateStringEvent): TFileName;
Begin
  Result := LoadDefaultTranslationFile(DefaultLangDir, OnTranslate);
End;

Function LoadDefaultTranslationFile(Const TranslationFilesPath: Ansistring;
  Const OnTranslate: TTranslateStringEvent): TFileName;
Var
  lLang, sLang, s: String;
  i: Integer;
Begin
  LazGetLanguageIDs(lLang, sLang);
  lLang := LowerCase(lLang);
  sLang := LowerCase(sLang);
  {$ifdef windows}
  If sLang = 'ch' Then
    Begin
      sLang := 'zh';
      lLang := StringReplace(lLang, 'ch_', 'zh_', []);
    End;
{$endif windows}
  i := Pos('.', lLang);
  If i > 0 Then
    SetLength(lLang, i - 1);
  s := IncludeTrailingPathDelimiter(TranslationFilesPath) +
    ExtractFileNameOnly(ParamStrUtf8(0)) + '.';
  Result := s + lLang;
  // First check full language name (uk_ua)
  If Not IsTranslationFileValid(Result) Then
  Begin
    Result := s + sLang;
    // Check fallback language name (uk)
    If Not IsTranslationFileValid(Result) Then
    Begin
      // Finally use country name (ua)
      i := Pos('_', lLang);
      If i > 0 Then
        lLang := Copy(lLang, i + 1, MaxInt);
      Result := s + lLang;
      If Not IsTranslationFileValid(Result) Then
      Begin
        Result := '';
        exit;
      End;
    End;
  End;
  IniFileName := Result;
  Result := LoadTranslationFile(Result, OnTranslate);
End;

Function LoadLanguageTranslation(Const Language: Ansistring;
  Const OnTranslate: TTranslateStringEvent): TFileName;
Begin
  Result := LoadLanguageTranslation(Language, DefaultLangDir, OnTranslate);
End;

Function LoadLanguageTranslation(Const Language, TranslationFilesPath: Ansistring;
  Const OnTranslate: TTranslateStringEvent): TFileName;
Var
  Sl: TStringList;
Begin
  Sl := GetAvailableTranslations(TranslationFilesPath);
  Result := GetTranslationFileName(Language, Sl);
  If Result <> '' Then
    Result := IncludeTrailingPathDelimiter(TranslationFilesPath) + Result;
  If FileExistsUTF8(Result) Then
    LoadTranslationFile(Result, OnTranslate);
End;

Function GetTranslationFileName(Const Language: Ansistring;
  AvailableTranslations: TStringList): Ansistring;
Var
  i: Integer;
  aName, aValue: String;
Begin
  Result := '';
  If Assigned(AvailableTranslations) Then
    With AvailableTranslations Do
      For i := 0 To Count - 1 Do
      Begin
        GetNameValue(i, aName, aValue);
        If AnsiSameText(AnsiDequotedStr(Language, QuoteChar),
          AnsiDequotedStr(aName, QuoteChar)) Then
        Begin
          Result := AnsiDequotedStr(aValue, QuoteChar);
          Break;
        End;
      End;
End;

Procedure SaveTranslationFile; Overload;
Begin
  If Assigned(LRSTranslator) And (LRSTranslator Is TResTranslator) Then
    With LRSTranslator As TResTranslator Do
      If Modified Then
        SaveFile;
End;

Procedure SaveTranslationFile(Const FileName: Ansistring); Overload;
Begin
  If Assigned(LRSTranslator) And (LRSTranslator Is TResTranslator) Then
    With LRSTranslator As TResTranslator Do
      If Modified Then
        SaveFile(FileName);
End;

Function TranslateString(Const Value: Ansistring; IsExternal: Boolean): Ansistring;
Begin
  If Assigned(LRSTranslator) And (LRSTranslator Is TResTranslator) Then
    With LRSTranslator As TResTranslator Do
      Result := InternalTranslateString(Value, IsExternal)
  Else
    Result := Value;
End;

{ TTranslateStringList }

Function TTranslateStringList.CorrectGetValue(Const Name: String): String;
Var
  Index: Integer;
  offset: Integer;
Begin
  Index := IndexOfName(Name, offset);
  If Index >= 0 Then
  Begin
    Result := Copy(Strings[Index], offset, MaxInt);
    Options[Index] := Options[Index] + [tsoUsed];
  End
  Else
    Result := '';
End;

Function TTranslateStringList.GetOptions(Index: Integer): TTranslateStringOptions;
Begin
  Result := TTranslateStringOptions(Cardinal(ptruint(Objects[Index])));
End;

Function TTranslateStringList.CorrectGetName(Index: Integer): String;
Var
  Offset: Integer;
  s: String;
Begin
  CheckSpecialChars;
  Result := '';
  s := Strings[Index];
  Offset := ScanQuotSep(PChar(s));
  If (Offset > 0) Then
    Result := NormaliseQuotedStr(LeftStr(s, offset));
End;

Function TTranslateStringList.ScanQuotSep(P: PChar): Integer;
Var
  i, len: Integer;
  QuoteCount: Integer;
Begin
  Result := 0;
  QuoteCount := 0;
  i := 0;
  len := strlen(P);
  While (i < len) And (Result = 0) Do
  Begin
    If P[i] = QuoteChar Then
      Inc(QuoteCount)
    Else If (P[i] = NameValueSeparator) And Not odd(QuoteCount) Then
        Result := i;
    Inc(i);
  End;
End;

Procedure TTranslateStringList.SetOptions(Index: Integer;
  Const AValue: TTranslateStringOptions);
Begin
  Objects[Index] := TObject(ptruint(Cardinal(AValue)));
End;

Function TTranslateStringList.DoCompareText(Const s1, s2: String): PtrInt;
Begin
  If CaseSensitive Then
    Result := AnsiCompareText(s1, s2)
  Else
    Result := AnsiCompareText(UTF8UpperCase(s1), UTF8UpperCase(s2));
End;

Constructor TTranslateStringList.Create(Const FileName: String);
Begin
  Inherited Create;
  CheckSpecialChars;
  LoadFromFile(FileName);
End;

Function TTranslateStringList.NormaliseQuotedStr(Const S: String): String;
Begin
  If Not HasSeparator(S, NameValueSeparator) Then
    Result := AnsiDequotedStr(S, QuoteChar)
  Else If Not IsQuoted(S, QuoteChar) Then
      Result := AnsiQuotedStr(S, QuoteChar)
    Else
      Result := S;
End;

Function TTranslateStringList.IndexOfName(Const Name: String;
  Var Offset: Integer): Integer;
Var
  s, n: String;
Begin
  CheckSpecialChars;
  Result := 0;
  n := NormaliseQuotedStr(Name);
  While (Result < Count) Do
  Begin
    s := Strings[Result];
    Offset := ScanQuotSep(PChar(s));
    If (Offset > 0) And (n = Copy(s, 1, Offset)) Then
    Begin
      Inc(Offset, 2);
      exit;
    End;
    Inc(Result);
  End;
  Result := -1;
End;

Function TTranslateStringList.IndexOfName(Const Name: String): Integer;
Var
  i: Integer;
Begin
  Result := IndexOfName(Name, i);
End;

Procedure TTranslateStringList.LoadFromFile(Const FileName: String);
Var
  FS: TFileStreamUTF8;
  buff: Array[1..3] Of Char;
  i, j, k: Integer;
  s, esep: String;
Begin
  FS := TFileStreamUTF8.Create(FileName, fmOpenRead);
  Try
    // Skip UTF8 header
    buff := '';
    FS.Read(buff, SizeOf(UTF8FileHeader));
    If buff <> UTF8FileHeader Then
      FS.Position := 0;
    LoadFromStream(FS);
  Finally
    FS.Free;
  End;

  i := IndexOf(LineSeparator);
  If i >= 0 Then
    Delete(i);

  // Normalize quotations
  esep := NameValueSeparator + NameValueSeparator;
  For i := 0 To Count - 1 Do
  Begin
    s := Strings[i];
    j := ScanQuotSep(PChar(s));
    If j > 0 Then
    Begin
      k := j + 2;
      If Copy(s, j + 1, 2) = esep Then
      Begin
        Options[i] := [tsoExternal];
        Inc(k);
      End;
      Strings[i] := NormaliseQuotedStr(Copy(s, 1, j)) + NameValueSeparator +
        NormaliseQuotedStr(Copy(s, k, MaxInt));
    End;
  End;
End;

Procedure TTranslateStringList.SaveToFile(Const FileName: String);
Var
  FS: TFileStreamUTF8;
  i, j: Integer;
  s, esep: String;
Begin
  ForceDirectories(ExtractFilePath(FileName));
  FS := TFileStreamUTF8.Create(FileName, fmCreate);
  Try
    FS.WriteBuffer(UTF8FileHeader, SizeOf(UTF8FileHeader));
    esep := NameValueSeparator + NameValueSeparator;
    For i := 0 To Count - 1 Do
    Begin
      s := Strings[i];
      If tsoExternal In Options[i] Then
      Begin
        j := ScanQuotSep(PChar(s));
        If j > 0 Then
          s := NormaliseQuotedStr(Copy(s, 1, j)) + esep +
            NormaliseQuotedStr(Copy(s, j + 2, MaxInt));
      End;
      If s <> '' Then
        FS.WriteBuffer(s[1], Length(s));
      s := LineEnding;
      FS.WriteBuffer(s[1], Length(s));
    End;
  Finally
    FS.Free;
  End;
End;

Procedure TTranslateStringList.Merge(Source: TTranslateStringList;
  Const NamesOnly: Boolean = False);
Var
  i, j: Integer;
  n: String;
Begin
  CheckSpecialChars;
  Source.Sort;
  For i := 0 To Count - 1 Do
    Options[i] := [];
  For i := 0 To Source.Count - 1 Do
  Begin
    If Source.Options[i] * [tsoUsed, tsoExternal] = [] Then
      continue;
    n := Source.CNames[i];
    If n <> '' Then
    Begin
      j := IndexOfName(n);
      If j < 0 Then
      Begin
        // New string
        If NamesOnly Then
          j := Add(n + NameValueSeparator + n)
        Else
          j := Add(Source.Strings[i]);
      End;
      Options[j] := Source.Options[i] + [tsoUsed];
    End;
  End;
  // Delete unused strings
  i := 0;
  While i < Count Do
  Begin
    n := CNames[i];
    If (Options[i] = []) And (n <> '') And
      (CompareText(n, sLanguageIDName) <> 0) Then
      Delete(i)
    Else
      Inc(i);
  End;
End;

{ TResTranslator }

Constructor TResTranslator.Create(TranslationFile: Ansistring);
Begin
  Inherited Create;
  FTranslationFile := TranslationFile;
  FIgnoreDelimiters := [wdIgnoreTrailing];
  FWordDelims := ['.', ',', ':'];

  FStrResLst := TTranslateStringList.Create;
  With FStrResLst Do
  Begin
    Duplicates := dupIgnore;
    CaseSensitive := False;
    CheckSpecialChars;
    If FileExistsUTF8(FTranslationFile) Then
    Begin
      LoadFromFile(FTranslationFile);
      FTranslationLanguage :=
        AnsiDequotedStr(CValues[AnsiQuotedStr(sLanguageIDName, QuoteChar)],
        QuoteChar);
    End;
  End;
End;

Destructor TResTranslator.Destroy;
Begin
  FStrResLst.Free;
  Inherited Destroy;
End;

Function TResTranslator.InternalTranslateString(Const Value: Ansistring;
  IsExternal: Boolean): Ansistring;

  Function IsAlpha(Ch: Char): Boolean; Inline;
  Begin
    Result := Ch In ['A'..'Z', 'a'..'z'];
  End;

  Function HasAlpha: Boolean;
  Var
    i: Integer;
  Begin
    Result := False;
    i := 1;
    While Not Result And (i <= Length(Value)) Do
    Begin
      Result := IsAlpha(Value[i]);
      Inc(i);
    End;
  End;

Var
  ClearValue: Ansistring;
  Original, s, n: Ansistring;
  i: Integer;
Begin
  Original := Value;
  ClearValue := StringReplace(AdjustLineBreaks(Value), LineEnding,
    '~', [rfReplaceAll]);
  Result := ClearValue;

  If wdIgnoreLeading In IgnoreDelimiters Then
    RemoveLeadingChars(ClearValue, FWordDelims);

  If wdIgnoreTrailing In IgnoreDelimiters Then
    RemoveTrailingChars(ClearValue, FWordDelims);
  If HasAlpha Then
  Begin
    With FStrResLst Do
    Begin
      If HasSeparator(ClearValue, NameValueSeparator) Then
        n := AnsiQuotedStr(ClearValue, QuoteChar)
      Else
        n := ClearValue;

      s := CValues[n];
      If (s = '') Then
      Begin
        i := Add(n + NameValueSeparator + n);
        Options[i] := [tsoNew, tsoUsed];
        FModified := True;
        Result := Original;
      End
      Else
      Begin
        Result := StringReplace(Result, ClearValue,
          AnsiDequotedStr(s, QuoteChar), [rfReplaceAll]);
        Result := StringReplace(Result, '~', LineEnding, [rfReplaceAll]);
      End;
      If IsExternal Then
      Begin
        i := IndexOfName(n);
        If i >= 0 Then
          Options[i] := Options[i] + [tsoExternal];
      End;
    End;
  End;
End;

Procedure TResTranslator.SetIgnoreDelimiters(Const AValue: TWordDelimitersOptions);
Begin
  If FIgnoreDelimiters = AValue Then
    exit;
  FIgnoreDelimiters := AValue;
End;

Function TResTranslator.GetStrings: TStringList;
Begin
  Result := FStrResLst;
End;

Procedure TResTranslator.SetOnTranslateString(Const AValue: TTranslateStringEvent);
Begin
  If FOnTranslateString = AValue Then
    exit;
  FOnTranslateString := AValue;
End;

Procedure TResTranslator.SetWordDelims(Const AValue: TSysCharset);
Begin
  If FWordDelims = AValue Then
    exit;
  FWordDelims := AValue;
End;

Procedure TResTranslator.TranslateStringProperty(Sender: TObject;
  Const Instance: TPersistent; PropInfo: PPropInfo; Var Content: String);
Var
  Accept: Boolean;
  ResourceName: Ansistring;
  OwnerName: Ansistring;
Begin
  If Sender Is TReader And Assigned(TReader(Sender).Owner) Then
    OwnerName := TReader(Sender).Owner.GetNamePath;

  If Instance.InheritsFrom(TForm) Then
    ResourceName := OwnerName + '.' + PropInfo^.Name
  Else
    ResourceName := OwnerName + '.' + Instance.GetNamePath + '.' + PropInfo^.Name;

  Accept := True;

  If Assigned(OnTranslateString) Then
    OnTranslateString(Self, ResourceName, Accept);

  If (PropInfo^.Name = 'Caption') And (Instance.GetNamePath = Content) Then
    Accept := False
  Else
    If PropInfo^.Name = 'Name' Then
      Accept := False;

  If Accept Then
    Content := InternalTranslateString(Content);
End;

Procedure TResTranslator.SaveFile;
Begin
  SaveTranslationFile(FTranslationFile);
End;

Procedure TResTranslator.SaveFile(Const aFileName: String);
Begin
  FStrResLst.SaveToFile(aFileName);
End;

Finalization
  FreeAndNil(LRSTranslator);

End.
