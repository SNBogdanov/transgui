

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

Unit BEncode;

Interface

Uses 
Classes, Contnrs, SysUtils;

Type 
  TBEncodedFormat = (befEmpty, befString, befInteger, befList, befDictionary);

  TBEncodedData = Class(TObject)
    Public 
      Header: AnsiString;
      Data: TObject;
      // actually TBEncoded
      destructor Destroy;
      override;
      constructor Create(bData: TObject);
  End;

  { TBEncodedDataList }

  TBEncodedDataList = Class(TObjectList)
    Protected 
      Function GetItems(Index: Integer): TBEncodedData;
      Procedure SetItems(Index: Integer; AClass: TBEncodedData);
    Public 
      Function FindElement(Header: AnsiString; RaiseException: boolean = True):

                                                                         TObject
      ;
      Function Add(AClass: TBEncodedData): Integer;
      Function Extract(Item: TBEncodedData): TBEncodedData;
      Function Remove(AClass: TBEncodedData): Integer;
      Function IndexOf(AClass: TBEncodedData): Integer;
      Function First: TBEncodedData;
      Function Last: TBEncodedData;
      Procedure Insert(Index: Integer; AClass: TBEncodedData);
      property Items[Index: Integer]: TBEncodedData read GetItems write SetItems
      ;
      default;
  End;

  TBEncoded = Class(TObject)
    Private 
      FFormat: TBEncodedFormat;
      Procedure SetFormat(Format: TBEncodedFormat);
    Public 
      StringData: AnsiString;
      IntegerData: int64;
      ListData: TBEncodedDataList;
      property Format: TBEncodedFormat read FFormat write SetFormat;
      Class Procedure Encode(Encoded: TObject; Var Output: AnsiString);
        destructor Destroy;
        override;
        constructor Create(Stream: TStream);
      End;

      Implementation

      destructor TBEncodedData.Destroy;
      Begin
        Data.Free;

        inherited Destroy;
      End;

      constructor TBEncodedData.Create(bData: TObject);
      Begin
        inherited Create;

        Self.Data := bData;
      End;

      destructor TBEncoded.Destroy;
      Begin
        If ListData <> Nil Then
          ListData.Free;

        inherited Destroy;
      End;

      constructor TBEncoded.Create(Stream: TStream);

      Procedure InvalidInput;
      Begin
        raise Exception.Create('Invalid torrent file.');
      End;

      Function GetString(Buffer: AnsiString): AnsiString;

      Var 
        X: char;
        lngth: Integer;
      Begin
        // loop until we come across it
        Repeat
          Stream.ReadBuffer(X, 1);
          If Not ((X In ['0'..'9']) Or (x = ':')) Then
            InvalidInput;
          If X = ':' Then
            Begin
              If Buffer = '' Then
                InvalidInput;
              If Length(Buffer) > 6 Then
                InvalidInput;
              lngth := StrToInt(Buffer);
              SetLength(Result, lngth);
              Stream.ReadBuffer(Result[1], lngth);
              Break;
            End
          Else
            Buffer := Buffer + X;
        Until False;
      End;

      Var 
        X: char;
        Buffer: AnsiString;
        Data: TBEncodedData;
        Encoded: TBEncoded;

      Begin
        inherited Create;

        // get first character to determine the format of the proceeding data
        Stream.ReadBuffer(X, 1);

        // is it an integer?
        If X = 'i' Then
          Begin
            // yes it is, let's read until we come across e
            Buffer := '';
            Repeat
              Stream.ReadBuffer(X, 1);
              If Not ((X In ['0'..'9']) Or (X = 'e')) Then
                InvalidInput;
              If X = 'e' Then
                Begin
                  If Buffer = '' Then
                    InvalidInput
                  Else
                    Begin
                      Format := befInteger;
                      IntegerData := StrToInt64(Buffer);
                      Break;
                    End;
                End
              Else
                Buffer := Buffer + X;
            Until False;
          End
          // is it a list?
        Else If X = 'l' Then
               Begin
                 // its a list
                 Format := befList;

                 // loop until we come across e
                 Repeat
                   // have a peek around and see if theres an e
                   Stream.ReadBuffer(X, 1);
                   // is it an e?
                   If X = 'e' Then
                     Break;
                   // otherwise move the cursor back
                   Stream.Seek(-1, soFromCurrent);
                   // create the element
                   Encoded := TBEncoded.Create(Stream);
                   // add it to the list
                   ListData.Add(TBEncodedData.Create(Encoded));
                 Until False;
               End
               // is it a dictionary?
        Else If X = 'd' Then
               Begin
                 // its a dictionary :>
                 Format := befDictionary;

                 // loop until we come across e
                 Repeat
                   // have a peek around and see if theres an e
                   Stream.ReadBuffer(X, 1);
                   // is it an e?
                   If X = 'e' Then
                     Break;
                   // if it isnt an e it has to be numerical!
                   If Not (X In ['0'..'9']) Then
                     Begin
                       InvalidInput;
                     End;
                   // now read the string data
                   Buffer := GetString(AnsiString(X));

                   // create the element
                   Encoded := TBEncoded.Create(Stream);
                   // create the data element
                   Data := TBEncodedData.Create(Encoded);
                   Data.Header := Buffer;
                   // add it to the list
                   ListData.Add(Data);
                 Until False;
               End
               // is it a string?
        Else If X In ['0'..'9'] Then
               Begin
                 StringData := GetString(AnsiString(X));
                 Format := befString;
               End
        Else
          InvalidInput;
      End;

      Class Procedure TBEncoded.Encode(Encoded: TObject; Var Output: AnsiString)
        ;

        Var 
          i: integer;
        Begin
          With TBEncoded(Encoded) Do
            Begin
              // what type of member is it?
              Case Format Of 
                befString: Output := Output + IntToStr(Length(StringData)) + ':'
                                     +
                                     StringData;
                befInteger: Output := Output + 'i' + IntToStr(IntegerData) + 'e'
                ;
                befList:
                         Begin
                           Output := Output + 'l';
                           For i := 0 To ListData.Count - 1 Do
                             Encode(TBEncoded(ListData[i].Data), Output);
                           Output := Output + 'e';
                         End;
                befDictionary:
                               Begin
                                 Output := Output + 'd';
                                 For i := 0 To ListData.Count - 1 Do
                                   Begin
                                     Output := Output + IntToStr(Length(ListData
                                               [i].Header)) + ':' +
                                               ListData[i].Header;
                                     Encode(TBEncoded(ListData[i].Data), Output)
                                     ;
                                   End;
                                 Output := Output + 'e';
                               End;
              End;
            End;
        End;

        Procedure TBEncoded.SetFormat(Format: TBEncodedFormat);
        Begin
          If Format In [befList, befDictionary] Then
            ListData := TBEncodedDataList.Create;
          FFormat := Format;
        End;

        Function TBEncodedDataList.FindElement(Header: AnsiString;
                                               RaiseException: boolean): TObject
        ;

        Var 
          i: integer;
        Begin
          Header := LowerCase(Header);
          For i := 0 To Count - 1 Do
            If LowerCase(Items[i].Header) = Header Then
              Begin
                Result := Items[i].Data;
                Exit;
              End;

          If RaiseException Then
            raise Exception.CreateFmt('Element ''%s'' not found.', [Header])
          Else
            Result := Nil;
        End;

        Function TBEncodedDataList.Add(AClass: TBEncodedData): Integer;
        Begin
          Result := Inherited Add(AClass);
        End;

        Function TBEncodedDataList.Extract(Item: TBEncodedData): TBEncodedData;
        Begin
          Result := TBEncodedData(Inherited Extract(Item));
        End;

        Function TBEncodedDataList.First: TBEncodedData;
        Begin
          Result := TBEncodedData(Inherited First);
        End;

        Function TBEncodedDataList.GetItems(Index: Integer): TBEncodedData;
        Begin
          Result := TBEncodedData(Inherited Items[Index]);
        End;

        Function TBEncodedDataList.IndexOf(AClass: TBEncodedData): Integer;
        Begin
          Result := Inherited IndexOf(AClass);
        End;

        Procedure TBEncodedDataList.Insert(Index: Integer; AClass: TBEncodedData
        );
        Begin
          inherited Insert(Index, AClass);
        End;

        Function TBEncodedDataList.Last: TBEncodedData;
        Begin
          Result := TBEncodedData(Inherited First);
        End;

        Function TBEncodedDataList.Remove(AClass: TBEncodedData): Integer;
        Begin
          Result := Inherited Remove(AClass);
        End;

        Procedure TBEncodedDataList.SetItems(Index: Integer; AClass:
                                             TBEncodedData);
        Begin
          inherited Items[Index] := AClass;
        End;

      End.
