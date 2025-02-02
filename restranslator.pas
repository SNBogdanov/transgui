

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

  TWordDelimitersOptions = set Of (wdIgnoreLeading, wdIgnoreTrailing);

  TResTranslator = Class;

    TTranslateStringEvent = Procedure (Sender: TResTranslator; Const
                                       ResourceName: AnsiString; Var Accept:
                                       boolean);

    TTranslateStringOption = (tsoNew, tsoUsed, tsoExternal);
    TTranslateStringOptions = set Of TTranslateStringOption;

  { TTranslateStringList }

    TTranslateStringList = Class(TStringList)
      Private 
        Function CorrectGetName(Index: integer): string;
        Function CorrectGetValue(Const Name: String): string;
        Function GetOptions(Index: integer): TTranslateStringOptions;
        Function NormaliseQuotedStr(Const S: String): string;
        Function ScanQuotSep(P: PChar): integer;
        Procedure SetOptions(Index: integer; Const AValue:
                             TTranslateStringOptions);
      Protected 
        Function DoCompareText(Const s1,s2 : String) : PtrInt;
        override;
      Public 
        constructor Create(Const FileName: String);
        overload;
        Function IndexOfName(Const Name: String; Var Offset: integer): integer;
        Function IndexOfName(Const Name: String): integer;
        override;
        Procedure LoadFromFile(Const FileName: String);
        override;
        Procedure SaveToFile(Const FileName: String);
        override;
        Procedure Merge(Source: TTranslateStringList; Const NamesOnly: boolean =
                        false);
        property CValues[

        Const Name: string]: string read CorrectGetValue;
          property CNames[Index: integer]: string read CorrectGetName;
          property Options[Index: integer]: TTranslateStringOptions read
                                            GetOptions write SetOptions;
        End;

  { TResTranslator }

        TResTranslator = Class(TAbstractTranslator)
          Private 
            FIgnoreDelimiters: TWordDelimitersOptions;
            FOnTranslateString: TTranslateStringEvent;
            FStrResLst:    TTranslateStringList;
            FModified:     boolean;
            FTranslationLanguage: AnsiString;
            FWordDelims:   TSysCharset;
            Function GetStrings: TStringList;
            Procedure SetIgnoreDelimiters(Const AValue: TWordDelimitersOptions);
            Procedure SetOnTranslateString(Const AValue: TTranslateStringEvent);
            Procedure SetWordDelims(Const AValue: TSysCharset);
            Function InternalTranslateString(Const Value: AnsiString; IsExternal
                                             : boolean = False): AnsiString;
          Public 
            FTranslationFile: string;
            constructor Create(TranslationFile: AnsiString);
            destructor Destroy;
            override;
            Procedure TranslateStringProperty(Sender: TObject; Const Instance:
                                              TPersistent; PropInfo: PPropInfo;
                                              Var Content: String);
            override;
            Procedure SaveFile;
            overload;
            Procedure SaveFile(Const aFileName: String);
            overload;
            property Modified: boolean Read FModified;
            property Strings: TStringList Read GetStrings;
            property IgnoreDelimiters: TWordDelimitersOptions Read
                                       FIgnoreDelimiters Write
                                       SetIgnoreDelimiters;
            property WordDelims: TSysCharset Read FWordDelims Write
                                 SetWordDelims;
            property TranslationLanguage: AnsiString read FTranslationLanguage;
            property OnTranslateString: TTranslateStringEvent Read
                                        FOnTranslateString Write
                                        SetOnTranslateString;
        End;

        Function LoadTranslationFile(Const TranslationFile: AnsiString; Const
                                     OnTranslate: TTranslateStringEvent = Nil):

                                                                      AnsiString
        ;
        Procedure SaveTranslationFile;
        overload;
        Procedure SaveTranslationFile(Const FileName: AnsiString);
        overload;
        Procedure MakeTranslationFile;
        overload;
        Procedure MakeTranslationFile(Language: AnsiString);
        overload;
        Procedure MakeTranslationFile(Const FileName, Language: AnsiString);
        overload;
        Procedure SupplementTranslationFile(Const FileName: AnsiString);
        Procedure SupplementTranslationFiles;
        overload;
        Procedure SupplementTranslationFiles(Const TranslationFilesPath:
                                             AnsiString);
        overload;
        Function LoadDefaultTranslationFile(Const OnTranslate:
                                            TTranslateStringEvent = Nil):

                                                                       TFileName
        ;
        Function LoadDefaultTranslationFile(Const TranslationFilesPath:
                                            AnsiString; Const OnTranslate:
                                            TTranslateStringEvent = Nil):

                                                                       TFileName
        ;
        Function LoadLanguageTranslation(Const Language: AnsiString; Const
                                         OnTranslate: TTranslateStringEvent =
                                         Nil): TFileName;
        Function LoadLanguageTranslation(Const Language, TranslationFilesPath:
                                         AnsiString; Const OnTranslate:
                                         TTranslateStringEvent = Nil): TFileName
        ;
        Function TranslateString(Const Value: AnsiString; IsExternal: boolean =
                                 False): AnsiString;
        Function ExtractLangName(Const FileName: TFilename): AnsiString;
        Function GetAvailableTranslations: TStringList;
        Function GetAvailableTranslations(Const SearchPath: AnsiString):

                                                                     TStringList
        ;
        Function GetTranslationFileName(Const Language: AnsiString;
                                        AvailableTranslations: TStringList):

                                                                      AnsiString
        ;
        Function DefaultLangDir: AnsiString;
        Function IsTranslationFileValid(Const TranslationFile: AnsiString):

                                                                         boolean
        ;

        Const 
          sLanguageIDName = 'TranslationLanguage';

        Var 
          IniFileName: string;

        Implementation

        Uses 
        Forms, utils;

        Const 
          LineSeparator = '###################';

{ procedures and functions }

        Function IsQuoted(Const S: AnsiString; QuoteChar: char): boolean;
        inline;

        Var 
          L: integer;
        Begin
          L := Length(S);
          If L > 1 Then
            Result := (S[1] = QuoteChar) And (S[L] = QuoteChar)
          Else
            Result := false;
        End;

        Function HasSeparator(Const S: AnsiString; Separator: char): boolean;
        inline;
        Begin
          Result := Pos(Separator, S) > 0;
        End;

        Function ExtractLangName(Const FileName: TFilename): AnsiString;
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

    Function GetAvailableTranslations(Const SearchPath: AnsiString): TStringList
    ;

    Var 
      Sr: TSearchRec;
      LangName, s: AnsiString;
    Begin
      Result := TStringList.Create;
      If FindFirstUTF8(IncludeTrailingPathDelimiter(SearchPath) + '*', faArchive
         Or faReadOnly, Sr) = 0 Then
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
      FDefaultLangDir: AnsiString;

    Function DefaultLangDir: AnsiString;
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

    Function GetResStrings(Name, Value: AnsiString; Hash: longint; P: pointer):

                                                                      AnsiString
    ;

    Var 
      Accept: boolean;
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

    Function LoadTranslationFile(Const TranslationFile: AnsiString; Const
                                 OnTranslate: TTranslateStringEvent = Nil):

                                                                      AnsiString
    ;
    Begin
      LRSTranslator := TResTranslator.Create(TranslationFile);
      TResTranslator(LRSTranslator).OnTranslateString := OnTranslate;
      SetResourceStrings(@GetResStrings, LRSTranslator);
      Result := TResTranslator(LRSTranslator).TranslationLanguage;
    End;

    Procedure SupplementTranslationFiles;
    overload;
    Begin
      SupplementTranslationFiles(DefaultLangDir);
    End;

    Procedure MakeTranslationFile;
    overload;
    Begin
      MakeTranslationFile('???');
    End;

    Procedure MakeTranslationFile(Language: AnsiString);
    overload;

    Var 
      lLang, sLang, s: string;
    Begin
      LazGetLanguageIDs(lLang, sLang);
      sLang := AnsiLowerCase(sLang);
      s := ExtractFileNameOnly(ParamStrUtf8(0));
      If (sLang <> '') And Not FileExistsUTF8(DefaultLangDir + s + '.' + sLang)
        Then
        s := s + '.' + sLang
      Else
        s := s + '.lng';
      MakeTranslationFile(DefaultLangDir + s, Language);
    End;

    Procedure MakeTranslationFile(Const FileName, Language: AnsiString);

    Var 
      Dst: TTranslateStringList;
    Begin
      If Assigned(LRSTranslator) And (LRSTranslator is TResTranslator) Then
        Begin
          Dst := TTranslateStringList.Create;
          Try
            Dst.Values[sLanguageIDName] := Language;
            With LRSTranslator as TResTranslator Do
              Dst.Merge(Strings as TTranslateStringList, true);
            ForceDirectories(ExtractFilePath(FileName));
            Dst.SaveToFile(FileName);
          Finally
            Dst.Free;
        End;
    End;
  End;

Procedure SupplementTranslationFile(Const FileName: AnsiString);

Var 
  Dst: TTranslateStringList;
Begin
  If Assigned(LRSTranslator) And (LRSTranslator is TResTranslator) Then
    Begin
      Dst := TTranslateStringList.Create(FileName);
      Try
        With LRSTranslator as TResTranslator Do
          Dst.Merge(Strings as TTranslateStringList, true);
        Dst.SaveToFile(FileName);
      Finally
        Dst.Free;
    End;
End;
End;

Procedure SupplementTranslationFiles(Const TranslationFilesPath: AnsiString);

Var 
  Sl: TStringList;
  i: integer;
  s: string;
Begin
  If Assigned(LRSTranslator) And (LRSTranslator is TResTranslator) Then
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
  InvalidLangExt: array[1..6] Of string = ('ua', 'by', 'cn', 'cz', 'se', 'tw');

Function IsTranslationFileValid(Const TranslationFile: AnsiString): boolean;

Var 
  s: string;
  i: integer;
Begin
  Result := FileExistsUTF8(TranslationFile);
  If Not Result Then
    exit;
  s := LowerCase(ExtractFileExt(TranslationFile));
  Delete(s, 1, 1);
  For i:=Low(InvalidLangExt) To High(InvalidLangExt) Do
    If s = InvalidLangExt[i] Then
      Begin
        Result := False;
        exit;
      End;
End;

Function LoadDefaultTranslationFile(Const OnTranslate: TTranslateStringEvent):

                                                                       TFileName
;
Begin
  Result := LoadDefaultTranslationFile(DefaultLangDir, OnTranslate);
End;

Function LoadDefaultTranslationFile(Const TranslationFilesPath: AnsiString;
                                    Const OnTranslate: TTranslateStringEvent):

                                                                       TFileName
;

Var 
  lLang, sLang, s: string;
  i: integer;
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
  s := IncludeTrailingPathDelimiter(TranslationFilesPath) + ExtractFileNameOnly(
       ParamStrUtf8(0))+ '.';
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

Function LoadLanguageTranslation(Const Language: AnsiString; Const OnTranslate:
                                 TTranslateStringEvent): TFileName;
Begin
  Result := LoadLanguageTranslation(Language, DefaultLangDir, OnTranslate);
End;

Function LoadLanguageTranslation(Const Language, TranslationFilesPath:
                                 AnsiString; Const OnTranslate:
                                 TTranslateStringEvent): TFileName;

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

Function GetTranslationFileName(Const Language: AnsiString;
                                AvailableTranslations: TStringList): AnsiString;

Var 
  i: integer;
  aName, aValue: string;
Begin
  Result := '';
  If Assigned(AvailableTranslations) Then
    With AvailableTranslations Do
      For i := 0 To Count - 1 Do
        Begin
          GetNameValue(i, aName, aValue);
          If AnsiSameText(AnsiDequotedStr(Language, QuoteChar), AnsiDequotedStr(
             aName, QuoteChar)) Then
            Begin
              Result := AnsiDequotedStr(aValue, QuoteChar);
              Break;
            End;
        End;
End;

Procedure SaveTranslationFile;
overload;
Begin
  If Assigned(LRSTranslator) And (LRSTranslator is TResTranslator) Then
    With LRSTranslator as TResTranslator Do
      If Modified Then
        SaveFile;
End;

Procedure SaveTranslationFile(Const FileName: AnsiString);
overload;
Begin
  If Assigned(LRSTranslator) And (LRSTranslator is TResTranslator) Then
    With LRSTranslator as TResTranslator Do
      If Modified Then
        SaveFile(FileName);
End;

Function TranslateString(Const Value: AnsiString; IsExternal: boolean):

                                                                      AnsiString
;
Begin
  If Assigned(LRSTranslator) And (LRSTranslator is TResTranslator) Then
    With LRSTranslator as TResTranslator Do
      result := InternalTranslateString(Value, IsExternal)
      Else
        result := Value;
End;

{ TTranslateStringList }

Function TTranslateStringList.CorrectGetValue(Const Name: String): string;

Var 
  Index: integer;
  offset: integer;
Begin
  Index := IndexOfName(Name, offset);
  If Index >= 0  Then
    Begin
      Result := Copy(Strings[Index], offset, MaxInt);
      Options[Index] := Options[Index] + [tsoUsed];
    End
  Else
    result := '';
End;

Function TTranslateStringList.GetOptions(Index: integer):

                                                         TTranslateStringOptions
;
Begin
  Result := TTranslateStringOptions(cardinal(ptruint(Objects[Index])));
End;

Function TTranslateStringList.CorrectGetName(Index: integer): string;

Var 
  Offset: integer;
  s: string;
Begin
  CheckSpecialChars;
  Result := '';
  s := Strings[Index];
  Offset := ScanQuotSep(PChar(s));
  If (Offset > 0) Then
    Result := NormaliseQuotedStr(LeftStr(s, offset));
End;

Function TTranslateStringList.ScanQuotSep(P: PChar): integer;

Var 
  i, len: integer;
  QuoteCount: integer;
Begin
  result := 0;
  QuoteCount := 0;
  i := 0;
  len := strlen(P);
  While (i < len) And (result = 0) Do
    Begin
      If P[i] = QuoteChar Then
        inc(QuoteCount)
      Else If (P[i] = NameValueSeparator) And Not odd(QuoteCount) Then
             result := i;
      inc(i);
    End;
End;

Procedure TTranslateStringList.SetOptions(Index: integer; Const AValue:
                                          TTranslateStringOptions);
Begin
  Objects[Index] := TObject(ptruint(cardinal(AValue)));
End;

Function TTranslateStringList.DoCompareText(Const s1, s2: String): PtrInt;
Begin
  If CaseSensitive Then
    result := AnsiCompareText(s1,s2)
  Else
    result := AnsiCompareText(UTF8UpperCase(s1),UTF8UpperCase(s2));
End;

constructor TTranslateStringList.Create(Const FileName: String);
Begin
  inherited Create;
  CheckSpecialChars;
  LoadFromFile(FileName);
End;

Function TTranslateStringList.NormaliseQuotedStr(Const S: String): string;
Begin
  If Not HasSeparator(S, NameValueSeparator) Then
    Result := AnsiDequotedStr(S, QuoteChar)
  Else If Not IsQuoted(S, QuoteChar) Then
         Result := AnsiQuotedStr(S, QuoteChar)
  Else
    Result := S;
End;

Function TTranslateStringList.IndexOfName(Const Name: String; Var Offset:
                                          integer): integer;

Var 
  s, n: string;
Begin
  CheckSpecialChars;
  result := 0;
  n := NormaliseQuotedStr(Name);
  While (result < Count) Do
    Begin
      s := Strings[result];
      Offset := ScanQuotSep(PChar(s));
      If (Offset > 0) And (n = Copy(s, 1, Offset)) Then
        Begin
          inc(Offset, 2);
          exit;
        End;
      inc(result);
    End;
  result := -1;
End;

Function TTranslateStringList.IndexOfName(Const Name: String): integer;

Var 
  i: integer;
Begin
  Result := IndexOfName(Name, i);
End;

Procedure TTranslateStringList.LoadFromFile(Const FileName: String);

Var 
  FS: TFileStreamUTF8;
  buff: array[1..3] Of char;
  i, j, k: integer;
  s, esep: string;
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
For i:=0 To Count - 1 Do
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
  i, j: integer;
  s, esep: string;
Begin
  ForceDirectories(ExtractFilePath(FileName));
  FS := TFileStreamUTF8.Create(FileName, fmCreate);
  Try
    FS.WriteBuffer(UTF8FileHeader, SizeOf(UTF8FileHeader));
    esep := NameValueSeparator + NameValueSeparator;
    For i:=0 To Count - 1 Do
      Begin
        s := Strings[i];
        If tsoExternal In Options[i] Then
          Begin
            j := ScanQuotSep(PChar(s));
            If j > 0 Then
              s := NormaliseQuotedStr(Copy(s, 1, j)) + esep + NormaliseQuotedStr
                   (Copy(s, j + 2, MaxInt));
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

Procedure TTranslateStringList.Merge(Source: TTranslateStringList; Const
                                     NamesOnly: boolean = false);

Var 
  i, j: integer;
  n: string;
Begin
  CheckSpecialChars;
  Source.Sort;
  For i:=0 To Count - 1 Do
    Options[i] := [];
  For i:=0 To Source.Count - 1 Do
    Begin
      If Source.Options[i]*[tsoUsed, tsoExternal] = [] Then
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
      If (Options[i] = []) And (n <> '') And (CompareText(n, sLanguageIDName) <>
         0) Then
        Delete(i)
      Else
        Inc(i);
    End;
End;

{ TResTranslator }

constructor TResTranslator.Create(TranslationFile: AnsiString);
Begin
  inherited Create;
  FTranslationFile := TranslationFile;
  FIgnoreDelimiters := [wdIgnoreTrailing];
  FWordDelims := ['.', ',', ':'];

  FStrResLst  := TTranslateStringList.Create;
  With FStrResLst Do
    Begin
      Duplicates := dupIgnore;
      CaseSensitive := False;
      CheckSpecialChars;
      If FileExistsUTF8(FTranslationFile) Then
        Begin
          LoadFromFile(FTranslationFile);
          FTranslationLanguage := AnsiDequotedStr(CValues[AnsiQuotedStr(
                                  sLanguageIDName, QuoteChar)], QuoteChar);
        End;
    End;
End;

destructor TResTranslator.Destroy;
Begin
  FStrResLst.Free;
  inherited Destroy;
End;

Function TResTranslator.InternalTranslateString(Const Value: AnsiString;
                                                IsExternal: boolean): AnsiString
;

Function IsAlpha(Ch: char): boolean;
inline;
Begin
  Result := Ch In ['A'..'Z', 'a'..'z'];
End;

Function HasAlpha: boolean;

Var 
  i: integer;
Begin
  Result := False;
  i      := 1;
  While Not Result And (i <= Length(Value)) Do
    Begin
      Result := IsAlpha(Value[i]);
      Inc(i);
    End;
End;

Var 
  ClearValue: AnsiString;
  Original, s, n: AnsiString;
  i: integer;
Begin
  Original := Value;
  ClearValue := StringReplace(AdjustLineBreaks(Value), LineEnding, '~', [
                rfReplaceAll]);
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
              Result := StringReplace(Result, ClearValue, AnsiDequotedStr(s,
                        QuoteChar), [rfReplaceAll]);
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

Procedure TResTranslator.SetIgnoreDelimiters(Const AValue:
                                             TWordDelimitersOptions);
Begin
  If FIgnoreDelimiters = AValue Then
    exit;
  FIgnoreDelimiters := AValue;
End;

Function TResTranslator.GetStrings: TStringList;
Begin
  Result := FStrResLst;
End;

Procedure TResTranslator.SetOnTranslateString(Const AValue:
                                              TTranslateStringEvent);
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

Procedure TResTranslator.TranslateStringProperty(Sender: TObject; Const Instance
                                                 : TPersistent; PropInfo:
                                                 PPropInfo; Var Content: String)
;

Var 
  Accept: boolean;
  ResourceName: AnsiString;
  OwnerName: AnsiString;
Begin
  If Sender is TReader And Assigned(TReader(Sender).Owner) Then
    OwnerName := TReader(Sender).Owner.GetNamePath;

  If Instance.InheritsFrom(TForm) Then
    ResourceName := OwnerName + '.' + PropInfo^.Name
  Else
    ResourceName := OwnerName + '.' + Instance.GetNamePath + '.' + PropInfo^.
                    Name;

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

finalization
FreeAndNil(LRSTranslator);

End.
