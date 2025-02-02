

{
    This file is part of the Free Pascal packages library.
    Copyright (c) 2013 by Yury Sidorov, member of the
    Free Pascal development team

    This unit is used to get format settings of the current
    Mac OS UI locale. Works on Mac OS X 10.3 or later.

    Function CFStringToStr() has been taken from CarbonProc LCL unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit MacLocale;

{$mode objfpc}{$H+}

Interface

Uses 
SysUtils, MacOSAll;

Procedure GetMacFormatSettings(Var ASettings: TFormatSettings);

Implementation



{------------------------------------------------------------------------------
  Name:    CFStringToStr
  Params:  AString  - Core Foundation string ref
          Encoding - Result data encoding format
  Returns: UTF-8 string

  Converts Core Foundation string to string
 ------------------------------------------------------------------------------}
Function CFStringToStr(AString: CFStringRef; Encoding: CFStringEncoding =
                       kCFStringEncodingUTF8): String;

Var 
  Str: Pointer;
  StrSize: CFIndex;
  StrRange: CFRange;
Begin
  If AString = Nil Then
    Begin
      Result := '';
      Exit;
    End;

  // Try the quick way first
  Str := CFStringGetCStringPtr(AString, Encoding);
  If Str <> Nil Then
    Result := PChar(Str)
  Else
    Begin
      // if that doesn't work this will
      StrRange.location := 0;
      StrRange.length := CFStringGetLength(AString);

      CFStringGetBytes(AString, StrRange, Encoding,
                       Ord('?'), False, nil, 0, StrSize{%H-});
      SetLength(Result, StrSize);

      If StrSize > 0 Then
        CFStringGetBytes(AString, StrRange, Encoding,
                         Ord('?'), False, @Result[1], StrSize, StrSize);
    End;
End;

Function StrOfChar(c: AnsiChar; Count: integer): ansistring;
Begin
  SetLength(Result, Count);
  FillChar(PAnsiChar(Result)^, Count, c);
End;

Function ConvertFormatStr(Const fmt: ansistring): ansistring;

Var 
  cnt: integer;
  c, q: AnsiChar;
  p: PAnsiChar;
  s: ansistring;
Begin
  Result := '';
  q := #0;
  cnt := 1;
  p := PAnsiChar(fmt);
  While p^ <> #0 Do
    Begin
      s := '';
      c := p^;
      If c In ['''', '"'] Then
        Begin
          If q = #0 Then
            q := c
          Else
            If c = q Then
              Begin
                q := #0;
                cnt := 1;
              End;
          s := c;
        End
      Else
        If q <> #0 Then
          s := c
      Else
        Begin
          If (p + 1)^ = c Then
            Inc(cnt)
          Else
            Begin
              Case c Of 
                'y', 'Y':
                          Begin
                            c := 'y';
                            If cnt > 2 Then
                              cnt := 4
                            Else
                              cnt := 2;
                          End;
                'M', 'L':
                          Begin
                            c := 'm';
                            If cnt > 4 Then
                              cnt := 3;
                          End;
                'd':
                     If cnt > 2 Then
                       cnt := 2;
                'E', 'e', 'c':
                               Begin
                                 c := 'd';
                                 If (cnt < 3) Or (cnt > 4) Then
                                   cnt := 3;
                               End;
                'a':
                     Begin
                       cnt := 0;
                       s := 'ampm';
                     End;
                'h', 'H', 'k', 'K':
                                    Begin
                                      c := 'h';
                                      If cnt > 2 Then
                                        cnt := 2;
                                    End;
                'm':
                     Begin
                       c := 'n';
                       If cnt > 2 Then
                         cnt := 2;
                     End;
                's':
                     If cnt > 2 Then
                       cnt := 2;
                'S':
                     Begin
                       c := 'z';
                       cnt := 1;
                     End;
                'G','u','Q','q','w','W','D','F','g','A','z','Z','v':
                                                                     cnt := 0;
              End;
              If cnt > 0 Then
                s := StrOfChar(c, cnt);
              cnt := 1;
            End;
        End;
      Inc(p);
      If s <> '' Then
        Result := Result + s;
    End;
End;

Procedure GetMacFormatSettings(Var ASettings: TFormatSettings);

Var 
  loc: CFLocaleRef;

Function _GetFormat(dateStyle: CFDateFormatterStyle; timeStyle:
                    CFDateFormatterStyle; Const DefFormat: String): string;

Var 
  fmt: CFDateFormatterRef;
Begin
  Result := '';
  fmt := CFDateFormatterCreate(Nil, loc, dateStyle, timeStyle);
  If fmt <> Nil Then
    Begin
      Result := ConvertFormatStr(CFStringToStr(CFDateFormatterGetFormat(fmt)));
      CFRelease(fmt);
    End;
  If Result = '' Then
    Result := DefFormat;
End;

Function _DateToStr(fmt: CFDateFormatterRef; Const AFormat: ansistring; AYear:
                    integer; AMonth, ADay, AHour: byte;
                    Const ADefault: ansistring): ansistring;

Var 
  cs: CFStringRef;
  gd: CFGregorianDate;
  at: CFAbsoluteTime;
  tz: CFTimeZoneRef;
Begin
  cs := CFStringCreateWithCString(Nil, Pointer(PAnsiChar(AFormat)),
        kCFStringEncodingUTF8);
  CFDateFormatterSetFormat(fmt, cs);
  CFRelease(cs);
  FillChar(gd, SIzeOf(gd), 0);
  gd.year := AYear;
  gd.month := AMonth;
  gd.day := ADay;
  gd.hour := AHour;
  tz := CFTimeZoneCopyDefault;
  at := CFGregorianDateGetAbsoluteTime(gd, tz);
  CFRelease(tz);
  cs := CFDateFormatterCreateStringWithAbsoluteTime(Nil, fmt, at);
  Result := CFStringToStr(cs);
  CFRelease(cs);
  If Result = '' Then
    Result := ADefault;
End;

Function _GetSeparator(dateStyle: CFDateFormatterStyle; timeStyle:
                       CFDateFormatterStyle; DefSep: char): char;

Var 
  fmt: CFDateFormatterRef;
  s: ansistring;
  p: PAnsiChar;
Begin
  Result := DefSep;
  fmt := CFDateFormatterCreate(Nil, loc, dateStyle, timeStyle);
  If fmt <> Nil Then
    Begin
      s := _DateToStr(fmt, CFStringToStr(CFDateFormatterGetFormat(fmt)), 2000, 1
           , 1, 0, DefSep);
      s := Trim(s);
      p := PAnsiChar(s);
      While p^ <> #0 Do
        If (p^ > ' ') And (p^ < 'A') And Not (p^ In ['0'..'9']) Then
          Begin
            Result := p^;
            break;
          End
        Else
          Inc(p);
      CFRelease(fmt);
    End;
End;

Var 
  s: string;
  fmt: CFDateFormatterRef;
  i: integer;
Begin
  With ASettings Do
    Begin
      loc := CFLocaleCopyCurrent;
      If loc = Nil Then
        exit;
      s := CFStringToStr(CFLocaleGetValue(loc, kCFLocaleDecimalSeparator));
      If Length(s) = 1 Then
        DecimalSeparator := s[1];
      s := CFStringToStr(CFLocaleGetValue(loc, kCFLocaleGroupingSeparator));
      If Length(s) = 1 Then
        ThousandSeparator := s[1]
      Else
        ThousandSeparator := ' ';
      // Unicode char has been returned. Probably it is a whitespace
      CurrencyString := CFStringToStr(CFLocaleGetValue(loc,
                        kCFLocaleCurrencySymbol));

      DateSeparator := _GetSeparator(kCFDateFormatterShortStyle,
                       kCFDateFormatterNoStyle, DateSeparator);
      TimeSeparator := _GetSeparator(kCFDateFormatterNoStyle,
                       kCFDateFormatterShortStyle, TimeSeparator);

      LongDateFormat := _GetFormat(kCFDateFormatterLongStyle,
                        kCFDateFormatterNoStyle, LongDateFormat);
      ShortDateFormat := _GetFormat(kCFDateFormatterShortStyle,
                         kCFDateFormatterNoStyle, ShortDateFormat);
      LongTimeFormat := _GetFormat(kCFDateFormatterNoStyle,
                        kCFDateFormatterLongStyle, LongTimeFormat);
      ShortTimeFormat := _GetFormat(kCFDateFormatterNoStyle,
                         kCFDateFormatterShortStyle, ShortTimeFormat);

      fmt := CFDateFormatterCreate(Nil, loc, kCFDateFormatterNoStyle,
             kCFDateFormatterNoStyle);
      If fmt <> Nil Then
        Begin
          For i:=1 To 12 Do
            Begin
              LongMonthNames[i] := _DateToStr(fmt, 'LLLL', 2006, i, 1, 0,
                                   LongMonthNames[i]);
              ShortMonthNames[i] := _DateToStr(fmt, 'LLL', 2006, i, 1, 0,
                                    ShortMonthNames[i]);
            End;
          For i:=1 To 7 Do
            Begin
              LongDayNames[i] := _DateToStr(fmt, 'cccc', 2006, 1, i, 0,
                                 LongDayNames[i]);
              ShortDayNames[i] := _DateToStr(fmt, 'ccc', 2006, 1, i, 0,
                                  ShortDayNames[i]);
            End;
          TimeAMString := _DateToStr(fmt, 'a', 2006, 1, 1, 1, TimeAMString);
          TimePMString := _DateToStr(fmt, 'a', 2006, 1, 1, 13, TimePMString);
          CFRelease(fmt);
        End;
      CFRelease(loc);
    End;
End;

initialization
GetMacFormatSettings(DefaultFormatSettings);

End.
