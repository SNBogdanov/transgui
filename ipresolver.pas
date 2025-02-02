

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

Unit IpResolver;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, GeoIP, syncobjs;

Type
  PHostEntry = ^THostEntry;

  THostEntry = Record
    IP: String;
    HostName: String;
    CountryName: String;
    CountryCode: String;
    ImageIndex: Integer;
  End;

  TResolverOption = (roResolveIP, roResolveCountry);
  TResolverOptions = Set Of TResolverOption;

  { TIpResolverThread }

  TIpResolver = Class(TThread)
  Private
    FLock: TCriticalSection;
    FResolveEvent: TEvent;
    FCache: TList;
    FResolveIp: TStringList;
    FGeoIp: TGeoIP;
    FOptions: TResolverOptions;
    FGeoIpCounryDB: String;
  Protected
    Procedure Execute; Override;
    Function NewEntry(Const IpAddress: String): PHostEntry;
    Function FindEntry(Const IpAddress: String): PHostEntry;
  Public
    Constructor Create(Const GeoIpCounryDB: String; AOptions: TResolverOptions);
      Reintroduce;
    Destructor Destroy; Override;
    Function Resolve(Const IpAddress: String): PHostEntry;
  End;

Implementation

Uses synsock;

  { TIpResolver }

Procedure TIpResolver.Execute;
Var
  ip, s: String;
  c: PHostEntry;
Begin
  Try
    While Not Terminated Do
    Begin
      If FResolveEvent.WaitFor(200) = wrSignaled Then
      Begin
        FResolveEvent.ResetEvent;

        While True Do
        Begin
          FLock.Enter;
          Try
            ip := '';
            If FResolveIp.Count > 0 Then
            Begin
              ip := FResolveIp[0];
              FResolveIp.Delete(0);
            End;
            UniqueString(ip);
          Finally
            FLock.Leave;
          End;

          If ip = '' Then
            break;

          If roResolveIP In FOptions Then
          Begin
            s := synsock.ResolveIPToName(ip, AF_INET, IPPROTO_IP, 0);
            c := FindEntry(ip);
            FLock.Enter;
            Try
              c^.HostName := s;
              UniqueString(c^.HostName);
            Finally
              FLock.Leave;
            End;
          End;
        End;

      End;
    End;
  Except
  End;
  Sleep(20);
End;

Function TIpResolver.NewEntry(Const IpAddress: String): PHostEntry;
Begin
  FLock.Enter;
  Try
    New(Result);
    Result^.ImageIndex := 0;
    Result^.IP := IpAddress;
    UniqueString(Result^.IP);
    Result^.HostName := IpAddress;
    UniqueString(Result^.HostName);
    FCache.Add(Result);
  Finally
    FLock.Leave;
  End;
End;

Function TIpResolver.FindEntry(Const IpAddress: String): PHostEntry;
Var
  i: Integer;
Begin
  FLock.Enter;
  Try
    For i := 0 To FCache.Count - 1 Do
    Begin
      Result := FCache[i];
      If Result^.IP = IpAddress Then
        exit;
    End;
    Result := nil;
  Finally
    FLock.Leave;
  End;
End;

Constructor TIpResolver.Create(Const GeoIpCounryDB: String; AOptions: TResolverOptions);
Begin
  FOptions := AOptions;
  FLock := TCriticalSection.Create;
  FResolveEvent := TEvent.Create(nil, True, False, '');
  FCache := TList.Create;
  FResolveIp := TStringList.Create;
  FGeoIpCounryDB := GeoIpCounryDB;
  If (roResolveCountry In FOptions) And (FGeoIpCounryDB <> '') Then
    FGeoIp := TGeoIP.Create(GeoIpCounryDB);
  Inherited Create(Not (roResolveIP In FOptions));
End;

Destructor TIpResolver.Destroy;
Var
  i: Integer;
Begin
  Terminate;
  If Not Suspended Then
    WaitFor;
  FResolveIp.Free;
  FResolveEvent.Free;
  FLock.Free;
  FGeoIp.Free;
  For i := 0 To FCache.Count - 1 Do
    Dispose(PHostEntry(FCache[i]));
  FCache.Free;
  Inherited Destroy;
End;

Function TIpResolver.Resolve(Const IpAddress: String): PHostEntry;
Var
  GeoCountry: TGeoIPCountry;
Begin
  Result := FindEntry(IpAddress);
  If Result <> nil Then
    exit;

  Result := NewEntry(IpAddress);
  If roResolveIP In FOptions Then
  Begin
    FLock.Enter;
    Try
      If FResolveIp.IndexOf(IpAddress) < 0 Then
      Begin
        FResolveIp.Add(IpAddress);
        FResolveEvent.SetEvent;
      End;
    Finally
      FLock.Leave;
    End;
  End;

  If FGeoIp <> nil Then
  Try
    If FGeoIp.GetCountry(IpAddress, GeoCountry) = GEOIP_SUCCESS Then
    Begin
      FLock.Enter;
      Try
        Result^.CountryName := GeoCountry.CountryName;
        UniqueString(Result^.CountryName);
        Result^.CountryCode := AnsiLowerCase(GeoCountry.CountryCode);
        UniqueString(Result^.CountryCode);
      Finally
        FLock.Leave;
      End;
    End;
  Except
    FreeAndNil(FGeoIp);
    DeleteFile(FGeoIpCounryDB);
    Result := nil;
  End;
End;

End.
