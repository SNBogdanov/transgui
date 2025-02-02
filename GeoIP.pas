

{
 * Copyright (C) 2005 MaxMind LLC  All Rights Reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 * ChangeLog
 * 2003-04-09 Translation of C# class to Pascal provided by W. Tracz
 * 2005-07-20 Added support for GeoIP Region, City, ISP and Organization (Yvan Schwab/esoftys)
}

{ Thanks to W. Tracz/Yvan Schwab for contributing this class }

{$mode delphi}

Unit GeoIP;

Interface

Uses Classes, SysUtils, utils, {$ifdef FPC} sockets {$else} WinSock {$endif};

Type 
  TGeoIPResult = (
                  GEOIP_SUCCESS                 = 0,
                  GEOIP_NODATA                  = 1,
                  GEOIP_ERROR_IPADDR            = 2,
                  GEOIP_ERROR_DBTYPE            = 3,
                  GEOIP_ERROR_IO                = 4
                 );

  TGeoIPDBTypes = (
                   GEOIP_COUNTRY_EDITION     = 1,
                   GEOIP_CITY_EDITION_REV1   = 2,
                   GEOIP_REGION_EDITION_REV1 = 3,
                   GEOIP_ISP_EDITION         = 4,
                   GEOIP_ORG_EDITION         = 5,
                   GEOIP_CITY_EDITION_REV0   = 6,
                   GEOIP_REGION_EDITION_REV0 = 7,
                   GEOIP_PROXY_EDITION       = 8,
                   GEOIP_ASNUM_EDITION       = 9
                  );

  TGeoIPCountry = Record
    CountryCode: string;
    CountryName: string;
  End;

  TGeoIPRegion = Record
    CountryCode: string;
    Region: string;
  End;

  TGeoIPCity = Record
    CountryCode: string;
    CountryName: string;
    Region: string;
    City: string;
    PostalCode: string;
    Latitude: Double;
    Longitude: Double;
    DmaCode: Integer;
    AreaCode: Integer;
  End;

  TGeoIPOrg = Record
    Name: string;
  End;

  TGeoIP = Class
    Private 
      FInputFile: TFileStreamUTF8;
      FDatabaseType: TGeoIPDBTypes;
      FDatabaseSegments: array Of Cardinal;
      FDatabaseInfo: string;
      FRecordLength: Cardinal;
      Function _GetCity(IPNum: Cardinal; Var GeoIPCity: TGeoIPCity):

                                                                    TGeoIPResult
      ;
      Function _GetCountry(IPNum: Cardinal; Var GeoIPCountry: TGeoIPCountry):

                                                                    TGeoIPResult
      ;
      Function _GetOrg(IPNum: Cardinal; Var GeoIPOrg: TGeoIPOrg): TGeoIPResult;
      Function _GetRegion(IPNum: Cardinal; Var GeoIPRegion: TGeoIPRegion):

                                                                    TGeoIPResult
      ;
      Function AddrToNum(Const IPAddr: String): Cardinal;
      Procedure InitDBFile;
      Function SeekRecord(IPNum: Cardinal): Cardinal;
    Public 
      constructor Create(Const FileName: String);
      destructor Destroy;
      override;
      Function GetCity(Const IPAddr: String; Var GeoIPCity: TGeoIPCity):

                                                                    TGeoIPResult
      ;
      Function GetCountry(Const IPAddr: String; Var GeoIPCountry: TGeoIPCountry)
      : TGeoIPResult;
      Function GetDatabaseInfo: string;
      Function GetOrg(Const IPAddr: String; Var GeoIPOrg: TGeoIPOrg):

                                                                    TGeoIPResult
      ;
      Function GetRegion(Const IPAddr: String; Var GeoIPRegion: TGeoIPRegion):

                                                                    TGeoIPResult
      ;
  End;

Const 
  CountryCodes: array [0..252] Of string = ('--','AP','EU','AD','AE','AF','AG',
                                            'AI','AL','AM','AN','AO','AQ','AR',
                                            'AS','AT','AU','AW','AZ','BA','BB',
                                            'BD','BE','BF','BG','BH','BI','BJ',
                                            'BM','BN','BO','BR','BS','BT','BV',
                                            'BW','BY','BZ','CA','CC','CD','CF',
                                            'CG','CH','CI','CK','CL','CM','CN',
                                            'CO','CR','CU','CV','CX','CY','CZ',
                                            'DE','DJ','DK','DM','DO','DZ','EC',
                                            'EE','EG','EH','ER','ES','ET','FI',
                                            'FJ','FK','FM','FO','FR','FX','GA',
                                            'GB','GD','GE','GF','GH','GI','GL',
                                            'GM','GN','GP','GQ','GR','GS','GT',
                                            'GU','GW',
                                            'GY','HK','HM','HN','HR','HT','HU',
                                            'ID','IE','IL','IN','IO','IQ','IR',
                                            'IS','IT','JM','JO','JP','KE','KG',
                                            'KH','KI','KM','KN','KP','KR','KW',
                                            'KY','KZ','LA','LB','LC','LI','LK',
                                            'LR','LS','LT','LU','LV','LY','MA',
                                            'MC','MD','MG','MH','MK','ML','MM',
                                            'MN','MO','MP','MQ','MR','MS','MT',
                                            'MU','MV','MW','MX','MY','MZ','NA',
                                            'NC','NE','NF','NG','NI','NL','NO',
                                            'NP','NR','NU','NZ','OM','PA','PE',
                                            'PF','PG','PH','PK','PL','PM','PN',
                                            'PR','PS','PT','PW','PY','QA','RE',
                                            'RO','RU',
                                            'RW','SA','SB','SC','SD','SE','SG',
                                            'SH','SI','SJ','SK','SL','SM','SN',
                                            'SO','SR','ST','SV','SY','SZ','TC',
                                            'TD','TF','TG','TH','TJ','TK','TM',
                                            'TN','TO','TL','TR','TT','TV','TW',
                                            'TZ','UA','UG','UM','US','UY','UZ',
                                            'VA','VC','VE','VG','VI','VN','VU',
                                            'WF','WS','YE','YT','RS','ZA','ZM',
                                            'ME','ZW','A1','A2','O1','AX','GG',
                                            'IM','JE','BL','MF');

  CountryNames: array [0..252] Of string = ('N/A','Asia/Pacific Region','Europe'
                                            ,'Andorra','United Arab Emirates',
                                            'Afghanistan','Antigua and Barbuda',
                                            'Anguilla','Albania','Armenia',
                                            'Netherlands Antilles','Angola',
                                            'Antarctica','Argentina',
                                            'American Samoa','Austria',
                                            'Australia','Aruba','Azerbaijan',
                                            'Bosnia and Herzegovina','Barbados',
                                            'Bangladesh','Belgium',
                                            'Burkina Faso','Bulgaria','Bahrain',
                                            'Burundi','Benin','Bermuda',
                                            'Brunei Darussalam','Bolivia',
                                            'Brazil','Bahamas','Bhutan',
                                            'Bouvet Island','Botswana',
                                            'Belarus','Belize','Canada',
                                            'Cocos (Keeling) Islands',

                                         'Congo, The Democratic Republic of the'
                                            ,'Central African Republic','Congo',
                                            'Switzerland','Cote D''Ivoire',
                                            'Cook Islands','Chile','Cameroon',
                                            'China','Colombia','Costa Rica',
                                            'Cuba','Cape Verde',
                                            'Christmas Island','Cyprus',
                                            'Czech Republic','Germany',
                                            'Djibouti','Denmark','Dominica',
                                            'Dominican Republic','Algeria',
                                            'Ecuador','Estonia','Egypt',
                                            'Western Sahara','Eritrea','Spain',
                                            'Ethiopia','Finland','Fiji',
                                            'Falkland Islands (Malvinas)',
                                            'Micronesia, Federated States of',
                                            'Faroe Islands','France',
                                            'France, Metropolitan','Gabon',
                                            'United Kingdom','Grenada','Georgia'
                                            ,'French Guiana','Ghana','Gibraltar'
                                            ,'Greenland','Gambia','Guinea',
                                            'Guadeloupe','Equatorial Guinea',
                                            'Greece',

                                  'South Georgia and the South Sandwich Islands'
                                            ,'Guatemala','Guam','Guinea-Bissau',
                                            'Guyana','Hong Kong',
                                            'Heard Island and McDonald Islands',
                                            'Honduras','Croatia','Haiti',
                                            'Hungary','Indonesia','Ireland',
                                            'Israel','India',
                                            'British Indian Ocean Territory',
                                            'Iraq','Iran, Islamic Republic of',
                                            'Iceland','Italy','Jamaica','Jordan'
                                            ,'Japan','Kenya','Kyrgyzstan',
                                            'Cambodia','Kiribati','Comoros',
                                            'Saint Kitts and Nevis',

                                       'Korea, Democratic People''s Republic of'
                                            ,'Korea, Republic of','Kuwait',
                                            'Cayman Islands','Kazakstan',
                                            'Lao People''s Democratic Republic',
                                            'Lebanon','Saint Lucia',
                                            'Liechtenstein','Sri Lanka',
                                            'Liberia','Lesotho','Lithuania',
                                            'Luxembourg','Latvia',
                                            'Libyan Arab Jamahiriya','Morocco',
                                            'Monaco','Moldova, Republic of',
                                            'Madagascar','Marshall Islands',

                                    'Macedonia, the Former Yugoslav Republic of'
                                            ,'Mali','Myanmar','Mongolia','Macao'
                                            ,'Northern Mariana Islands',
                                            'Martinique','Mauritania',
                                            'Montserrat','Malta','Mauritius',
                                            'Maldives','Malawi','Mexico',
                                            'Malaysia','Mozambique','Namibia',
                                            'New Caledonia','Niger',
                                            'Norfolk Island','Nigeria',
                                            'Nicaragua','Netherlands','Norway',
                                            'Nepal','Nauru','Niue','New Zealand'
                                            ,'Oman',
                                            'Panama','Peru','French Polynesia',
                                            'Papua New Guinea','Philippines',
                                            'Pakistan','Poland',
                                            'Saint Pierre and Miquelon',
                                            'Pitcairn','Puerto Rico',
                                            'Palestinian Territory, Occupied',
                                            'Portugal','Palau','Paraguay',
                                            'Qatar','Reunion','Romania',
                                            'Russian Federation','Rwanda',
                                            'Saudi Arabia','Solomon Islands',
                                            'Seychelles','Sudan','Sweden',
                                            'Singapore','Saint Helena',
                                            'Slovenia','Svalbard and Jan Mayen',
                                            'Slovakia','Sierra Leone',
                                            'San Marino','Senegal','Somalia',
                                            'Suriname',
                                            'Sao Tome and Principe',
                                            'El Salvador','Syrian Arab Republic'
                                            ,'Swaziland',
                                            'Turks and Caicos Islands','Chad',
                                            'French Southern Territories','Togo'
                                            ,'Thailand','Tajikistan','Tokelau',
                                            'Turkmenistan','Tunisia','Tonga',
                                            'Timor-Leste','Turkey',
                                            'Trinidad and Tobago','Tuvalu',
                                            'Taiwan',
                                            'Tanzania, United Republic of',
                                            'Ukraine','Uganda',

                                          'United States Minor Outlying Islands'
                                            ,'United States','Uruguay',
                                            'Uzbekistan',
                                            'Holy See (Vatican City State)',
                                            'Saint Vincent and the Grenadines',
                                            'Venezuela',
                                            'Virgin Islands, British',
                                            'Virgin Islands, U.S.','Vietnam',
                                            'Vanuatu','Wallis and Futuna',
                                            'Samoa','Yemen','Mayotte','Serbia',
                                            'South Africa','Zambia','Montenegro'
                                            ,'Zimbabwe','Anonymous Proxy',
                                            'Satellite Provider','Other',
                                            'Aland Islands','Guernsey',
                                            'Isle of Man','Jersey',
                                            'Saint Barthelemy','Saint Martin');


Implementation

Const 
  COUNTRY_BEGIN = 16776960;
  STATE_BEGIN_REV0 = 16700000;
  STATE_BEGIN_REV1  = 16000000;
  STRUCTURE_INFO_MAX_SIZE = 20;
  DATABASE_INFO_MAX_SIZE = 100;
  SEGMENT_RECORD_LENGTH = 3;
  STANDARD_RECORD_LENGTH = 3;
  ORG_RECORD_LENGTH = 4;
  MAX_RECORD_LENGTH = 4;
  MAX_ORG_RECORD_LENGTH = 300;
  FULL_RECORD_LENGTH = 50;
  US_OFFSET = 1;
  CANADA_OFFSET = 677;
  WORLD_OFFSET = 1353;
  FIPS_RANGE = 360;

{ TGeoIP }

  constructor TGeoIP.Create(Const FileName: String);
Begin
  inherited Create;
  FInputFile := TFileStreamUTF8.Create(FileName, fmOpenRead Or fmShareDenyNone);
  InitDBFile;
End;

destructor TGeoIP.Destroy;
Begin
  If Assigned(FInputFile) Then
    FInputFile.Free;
  inherited Destroy;
End;

Function TGeoIP._GetCity(IPNum: Cardinal; Var GeoIPCity: TGeoIPCity):

                                                                    TGeoIPResult
;

Var 
  SeekCity: Cardinal;
  RecordPointer: Cardinal;
  StrLen: Cardinal;
  buf: array[0..FULL_RECORD_LENGTH-1] Of Byte;
  p: PChar;
  i: Integer;
  DmaAreaCombo: Integer;
Begin
  If (FDatabaseType <> GEOIP_CITY_EDITION_REV0) And (FDatabaseType <>
     GEOIP_CITY_EDITION_REV1) Then
    Begin
      Result := GEOIP_ERROR_DBTYPE;
      Exit;
    End;
  SeekCity := SeekRecord(IPNum);
  If SeekCity = FDatabaseSegments[0] Then
    Begin
      Result := GEOIP_NODATA;
      Exit;
    End;
  RecordPointer := SeekCity + (2 * FRecordLength - 1) * FDatabaseSegments[0];
  FInputFile.Seek(RecordPointer, soFromBeginning);
  FInputFile.Read(buf, FULL_RECORD_LENGTH);

  // get country
  GeoIPCity.CountryCode := CountryCodes[buf[0]];
  GeoIPCity.CountryName := CountryNames[buf[0]];

  // get region
  p := @buf[1];
  StrLen := 0;
  While (p[StrLen] <> #0) Do
    Inc(StrLen);
  GeoIPCity.Region := Copy(p, 0, StrLen);

  // get city
  Inc(p, StrLen + 1);
  StrLen := 0;
  While (p[StrLen] <> #0) Do
    Inc(StrLen);
  GeoIPCity.City := Copy(p, 0, StrLen);

  // get postal code
  Inc(p, StrLen + 1);
  StrLen := 0;
  While (p[StrLen] <> #0) Do
    Inc(StrLen);
  GeoIPCity.PostalCode := Copy(p, 0, StrLen);

  // get latitude
  Inc(p, StrLen + 1);
  GeoIPCity.Latitude := 0.0;
  For i:=0 To 2 Do
    Begin
      GeoIPCity.Latitude := GeoIPCity.Latitude + (Integer(p[i]) shl (i*8));
    End;
  GeoIPCity.Latitude := GeoIPCity.Latitude/10000 - 180;

  // get longitude
  Inc(p, 3);
  GeoIPCity.Longitude := 0.0;
  For i:=0 To 2 Do
    Begin
      GeoIPCity.Longitude := GeoIPCity.Longitude + (Integer(p[i]) shl (i*8));
    End;
  GeoIPCity.Longitude := GeoIPCity.Longitude/10000 - 180;



// get area code and dma code for post April 2002 databases and for US locations
  GeoIPCity.DmaCode := 0;
  GeoIPCity.AreaCode := 0;
  If FDatabaseType = GEOIP_CITY_EDITION_REV1 Then
    Begin
      If GeoIPCity.CountryCode = 'US' Then
        Begin
          Inc(p, 3);
          DmaAreaCombo := 0;
          For i:=0 To 2 Do
            Begin
              DmaAreaCombo := DmaAreaCombo + (Integer(p[i]) shl (i*8));
            End;
          GeoIPCity.DmaCode := DmaAreaCombo Div 1000;
          GeoIPCity.AreaCode := DmaAreaCombo Mod 1000;
        End;
    End;
  Result := GEOIP_SUCCESS;
End;

Function TGeoIP._GetCountry(IPNum: Cardinal; Var GeoIPCountry: TGeoIPCountry):

                                                                    TGeoIPResult
;

Var 
  ret: Cardinal;
Begin
  If (FDatabaseType <> GEOIP_COUNTRY_EDITION) And (FDatabaseType <>
     GEOIP_PROXY_EDITION) Then
    Begin
      Result := GEOIP_ERROR_DBTYPE;
      Exit;
    End;
  ret := SeekRecord(IPNum) - COUNTRY_BEGIN;
  If ret > 0 Then
    Begin
      GeoIPCountry.CountryCode := CountryCodes[ret];
      GeoIPCountry.CountryName := CountryNames[ret];
      Result := GEOIP_SUCCESS;
    End
  Else
    Begin
      Result := GEOIP_NODATA;
    End;
End;

Function TGeoIP._GetOrg(IPNum: Cardinal; Var GeoIPOrg: TGeoIPOrg): TGeoIPResult;

Var 
  SeekOrg: Cardinal;
  RecordPointer: Cardinal;
  StrLen: Cardinal;
  buf: array[0..MAX_ORG_RECORD_LENGTH-1] Of Byte;
  p: PChar;
Begin
  If (FDatabaseType <> GEOIP_ORG_EDITION) And (FDatabaseType <>
     GEOIP_ISP_EDITION) And (FDatabaseType <> GEOIP_ASNUM_EDITION) Then
    Begin
      Result := GEOIP_ERROR_DBTYPE;
      Exit;
    End;
  SeekOrg := SeekRecord(IPNum);
  If SeekOrg = FDatabaseSegments[0] Then
    Begin
      Result := GEOIP_NODATA;
      Exit;
    End;
  RecordPointer := SeekOrg + (2 * FRecordLength - 1) * FDatabaseSegments[0];
  FInputFile.Seek(RecordPointer, soFromBeginning);
  FInputFile.Read(buf, FULL_RECORD_LENGTH);

  p := @buf[0];
  StrLen := 0;
  While (p[StrLen] <> #0) Do
    Inc(StrLen);
  GeoIPOrg.Name := Copy(p, 0, StrLen);
  Result := GEOIP_SUCCESS;
End;

Function TGeoIP._GetRegion(IPNum: Cardinal; Var GeoIPRegion: TGeoIPRegion):

                                                                    TGeoIPResult
;

Var 
  SeekRegion: Cardinal;
Begin
  If (FDatabaseType <> GEOIP_REGION_EDITION_REV0) And (FDatabaseType <>
     GEOIP_REGION_EDITION_REV1) Then
    Begin
      Result := GEOIP_ERROR_DBTYPE;
      Exit;
    End;
  SeekRegion := SeekRecord(IPNum);
  If FDatabaseType = GEOIP_REGION_EDITION_REV0 Then
    Begin
      // Region Edition, pre June 2003
      Dec(SeekRegion, STATE_BEGIN_REV0);
      If SeekRegion >= 1000 Then
        Begin
          GeoIPRegion.CountryCode := 'US';
          GeoIPRegion.Region := Chr((SeekRegion - 1000) Div 26 + 65) + Chr((
                                SeekRegion - 1000) Mod 26 + 65);
        End
      Else
        Begin
          GeoIPRegion.CountryCode := CountryCodes[SeekRegion];
          GeoIPRegion.Region := '';
        End;
    End
  Else If FDatabaseType = GEOIP_REGION_EDITION_REV1 Then
         Begin
           // Region Edition, post June 2003
           Dec(SeekRegion, STATE_BEGIN_REV1);
           If SeekRegion < US_OFFSET Then
             Begin
               // Unknown
               GeoIPRegion.CountryCode := '';
               GeoIPRegion.Region := '';
             End
           Else If SeekRegion < CANADA_OFFSET Then
                  Begin
                    // USA State
                    GeoIPRegion.CountryCode := 'US';
                    GeoIPRegion.Region := Chr((SeekRegion - US_OFFSET) Div 26 +
                                          65) + Chr((SeekRegion - US_OFFSET) Mod
                                          26 + 65);
                  End
           Else If SeekRegion < WORLD_OFFSET Then
                  Begin
                    // Canada Province
                    GeoIPRegion.CountryCode := 'CA';
                    GeoIPRegion.Region := Chr((SeekRegion - CANADA_OFFSET) Div
                                          26 + 65) + Chr((SeekRegion -
                                          CANADA_OFFSET) Mod 26 + 65);
                  End
           Else
             Begin
               // Not US or Canada
               GeoIPRegion.CountryCode := CountryCodes[(SeekRegion -
                                          WORLD_OFFSET) Div FIPS_RANGE];
               GeoIPRegion.Region := '';
             End;
         End;
  Result := GEOIP_SUCCESS;
End;

Function TGeoIP.AddrToNum(Const IPAddr: String): Cardinal;
{$ifdef FPC}
Begin
  Result := StrToHostAddr(IPAddr).s_addr;
End;
{$else}

Var 
  netlong: LongInt;
Begin
  netlong := inet_addr(PChar(IPAddr));
  If netlong <> INADDR_NONE Then
    Result := ntohl(netlong)
  Else
    Result := 0;
End;
{$endif}

Function TGeoIP.GetCity(Const IPAddr: String; Var GeoIPCity: TGeoIPCity):

                                                                    TGeoIPResult
;

Var 
  IPNum: Cardinal;
Begin
  IPNum := AddrToNum(IPAddr);
  If IPNum = 0 Then
    Begin
      Result := GEOIP_ERROR_IPADDR;
      Exit;
    End;
  Result := _GetCity(IPNum, GeoIPCity);
End;

Function TGeoIP.GetCountry(Const IPAddr: String; Var GeoIPCountry: TGeoIPCountry
): TGeoIPResult;

Var 
  IPNum: Cardinal;
Begin
  IPNum := AddrToNum(IPAddr);
  If IPNum = 0 Then
    Begin
      Result := GEOIP_ERROR_IPADDR;
      Exit;
    End;
  Result := _GetCountry(IPNum, GeoIPCountry);
End;

Function TGeoIP.GetDatabaseInfo: string;

Var 
  i: Integer;
  delim: array[0..2] Of Byte;
  HasStructureInfo: Boolean;
Begin
  FDatabaseInfo := '';
  HasStructureInfo := False;
  FInputFile.Seek(-3, soFromEnd);
  For i:=0 To STRUCTURE_INFO_MAX_SIZE-1 Do
    Begin
      FInputFile.Read(delim, 3);
      If (delim[0] = 255) And (delim[1] = 255) And (delim[2] = 255) Then
        Begin
          HasStructureInfo := True;
          Break;
        End;
      FInputFile.Seek(-4, soFromCurrent);
    End;
  If HasStructureInfo Then
    FInputFile.Seek(-3, soFromCurrent)
  Else
    // no structure info, must be pre Sep 2002 database, go back to end
    FInputFile.Seek(-3, soFromEnd);
  For i:=0 To DATABASE_INFO_MAX_SIZE-1 Do
    Begin
      FInputFile.Read(delim, 3);
      If (delim[0] = 0) And (delim[1] = 0) And (delim[2] = 0) Then
        Begin
          SetLength(FDatabaseInfo, i);
          FInputFile.Read(PChar(FDatabaseInfo)^, i);
          Break;
        End;
      FInputFile.Seek(-4, soFromCurrent);
    End;
  Result := FDatabaseInfo;
End;

Function TGeoIP.GetOrg(Const IPAddr: String; Var GeoIPOrg: TGeoIPOrg):

                                                                    TGeoIPResult
;

Var 
  IPNum: Cardinal;
Begin
  IPNum := AddrToNum(IPAddr);
  If IPNum = 0 Then
    Begin
      Result := GEOIP_ERROR_IPADDR;
      Exit;
    End;
  Result := _GetOrg(IPNum, GeoIPOrg);
End;

Function TGeoIP.GetRegion(Const IPAddr: String; Var GeoIPRegion: TGeoIPRegion):

                                                                    TGeoIPResult
;

Var 
  IPNum: Cardinal;
Begin
  IPNum := AddrToNum(IPAddr);
  If IPNum = 0 Then
    Begin
      Result := GEOIP_ERROR_IPADDR;
      Exit;
    End;
  Result := _GetRegion(IPNum, GeoIPRegion);
End;

Procedure TGeoIP.InitDBFile;

Var 
  i,j: Integer;
  delim: array[0..2] Of Byte;
  buf: array[0..SEGMENT_RECORD_LENGTH-1] Of Byte;
Begin
  // default to GeoIP Country Edition
  FDatabaseType := GEOIP_COUNTRY_EDITION;
  FRecordLength := STANDARD_RECORD_LENGTH;
  FInputFile.Seek(-3, soFromEnd);
  For i:=0 To STRUCTURE_INFO_MAX_SIZE-1 Do
    Begin
      FInputFile.Read(delim, 3);
      If (delim[0] = 255) And (delim[1] = 255) And (delim[2] = 255) Then
        Begin
          FInputFile.Read(FDatabaseType, 1);
          If Byte(FDatabaseType) >= 106 Then
            Begin


            // Backward compatibility with databases from April 2003 and earlier
              Dec(FDatabaseType, 105);
            End;
          If FDatabaseType = GEOIP_REGION_EDITION_REV0 Then
            Begin
              // Region Edition, pre June 2003
              SetLength(FDatabaseSegments, 1);
              FDatabaseSegments[0] := STATE_BEGIN_REV0;
            End
          Else If FDatabaseType = GEOIP_REGION_EDITION_REV1 Then
                 Begin
                   // Region Edition, post June 2003
                   SetLength(FDatabaseSegments, 1);
                   FDatabaseSegments[0] := STATE_BEGIN_REV1;
                 End
          Else If (FDatabaseType = GEOIP_CITY_EDITION_REV0) Or
                  (FDatabaseType = GEOIP_CITY_EDITION_REV1) Or
                  (FDatabaseType = GEOIP_ORG_EDITION) Or
                  (FDatabaseType = GEOIP_ISP_EDITION) Or
                  (FDatabaseType = GEOIP_ASNUM_EDITION) Then
                 Begin


           // City/Org Editions have two segments, read offset of second segment
                   SetLength(FDatabaseSegments, 1);
                   FDatabaseSegments[0] := 0;
                   FInputFile.Read(buf, SEGMENT_RECORD_LENGTH);
                   For j:=0 To SEGMENT_RECORD_LENGTH-1 Do
                     Begin
                       Inc(FDatabaseSegments[0], Integer(buf[j]) shl (j*8));
                     End;
                   If (FDatabaseType = GEOIP_ORG_EDITION) Or
                      (FDatabaseType = GEOIP_ISP_EDITION) Then
                     FRecordLength := ORG_RECORD_LENGTH;
                 End;
          Break;
        End
      Else
        Begin
          FInputFile.Seek(-4, soFromCurrent);
        End;
    End;
  If (FDatabaseType = GEOIP_COUNTRY_EDITION) Or
     (FDatabaseType = GEOIP_PROXY_EDITION) Then
    Begin
      SetLength(FDatabaseSegments, 1);
      FDatabaseSegments[0] := COUNTRY_BEGIN;
    End;
End;

Function TGeoIP.SeekRecord(IPNum: Cardinal): Cardinal;

Var 
  depth: Cardinal;
  offset: Cardinal;
  i,j: Cardinal;
  x: array[0..1] Of Cardinal;
  y: Cardinal;
  buf: array[0..2*MAX_RECORD_LENGTH-1] Of Byte;
Begin
  offset := 0;
  For depth:=31 Downto 0 Do
    Begin
      FInputFile.Seek(2 * FRecordLength * offset, soFromBeginning);
      FInputFile.Read(buf, 2 * FRecordLength);
      For i:=0 To 1 Do
        Begin
          x[i] := 0;
          For j:=0 To FRecordLength-1 Do
            Begin
              y := buf[i*FRecordLength+j];
              x[i] := x[i] + (y shl (j*8));
            End;
        End;
      If (IPNum And (1 shl depth)) <> 0 Then
        Begin
          If x[1] >= FDatabaseSegments[0] Then
            Begin
              Result := x[1];
              Exit;
            End
          Else
            Begin
              Offset := x[1];
            End;
        End
      Else
        Begin
          If x[0] >= FDatabaseSegments[0] Then
            Begin
              Result := x[0];
              Exit;
            End
          Else
            Begin
              Offset := x[0];
            End;
        End;
    End;
  Result := 0;
End;

End.
