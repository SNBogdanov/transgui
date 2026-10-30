Unit WhisparrClient;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type
  TWhisparrConfig = Record
    Url: String;
    ApiKey: String;
    VerifySsl: Boolean;
  End;

Function WhisparrConfigured(Const Config: TWhisparrConfig): Boolean;
Function WhisparrTestConnection(Const Config: TWhisparrConfig;
  Out Version: String; Out Error: String): Boolean;
Function WhisparrParseSeriesName(Const Config: TWhisparrConfig;
  Const Title: String; Out SeriesName: String; Out Error: String): Boolean;
Function WhisparrParseEpisode(Const Config: TWhisparrConfig;
  Const Title: String; Out EpisodeName: String; Out Error: String): Boolean;
Function WhisparrSuggestArchiveName(Const Config: TWhisparrConfig;
  TorrentNames: TStrings; Out ArchiveName: String; Out Error: String): Boolean;
Function WhisparrCleanPathName(Const Value: String): String;

Implementation

Uses
  httpsend, ssl_openssl, ssl_openssl_lib, blcksock, fpjson, jsonparser,
  jsonscanner, synacode, LazUTF8;

Function NormalizedUrl(Const Config: TWhisparrConfig): String;
Begin
  Result := Trim(Config.Url);
  If Result = '' Then
    exit;
  If Pos('://', Result) = 0 Then
    Result := 'http://' + Result;
  While (Result <> '') And (Result[Length(Result)] = '/') Do
    Delete(Result, Length(Result), 1);
End;

Function WhisparrConfigured(Const Config: TWhisparrConfig): Boolean;
Begin
  Result := (NormalizedUrl(Config) <> '') And (Trim(Config.ApiKey) <> '');
End;

Function FindObject(Obj: TJSONObject; Const Name: String): TJSONObject;
Var
  Data: TJSONData;
Begin
  Result := nil;
  If Obj = nil Then
    exit;
  Data := Obj.Find(Name);
  If Data Is TJSONObject Then
    Result := TJSONObject(Data);
End;

Function FindArray(Obj: TJSONObject; Const Name: String): TJSONArray;
Var
  Data: TJSONData;
Begin
  Result := nil;
  If Obj = nil Then
    exit;
  Data := Obj.Find(Name);
  If Data Is TJSONArray Then
    Result := TJSONArray(Data);
End;

Function FindString(Obj: TJSONObject; Const Name: String): String;
Var
  Data: TJSONData;
Begin
  Result := '';
  If Obj = nil Then
    exit;
  Data := Obj.Find(Name);
  If Data <> nil Then
    Result := Data.AsString;
End;

Function FindInt(Obj: TJSONObject; Const Name: String; ADefault: Integer): Integer;
Var
  Data: TJSONData;
Begin
  Result := ADefault;
  If Obj = nil Then
    exit;
  Data := Obj.Find(Name);
  If Data <> nil Then
    Result := Data.AsInteger;
End;

Procedure EnsureSSLLoaded;
Begin
  If IsSSLloaded Then
  Begin
    SSLImplementation := TSSLOpenSSL;
    exit;
  End;
  If InitSSLInterface Then
  Begin
    SSLImplementation := TSSLOpenSSL;
    exit;
  End;
  {$IFDEF MSWINDOWS}
  DLLSSLName := 'libssl-1_1-x64.dll';
  DLLUtilName := 'libcrypto-1_1-x64.dll';
  If InitSSLInterface Then
    SSLImplementation := TSSLOpenSSL;
  {$ENDIF MSWINDOWS}
End;

Function RequestJson(Const Config: TWhisparrConfig; Const Endpoint: String;
  Out Error: String): TJSONData;
Var
  Http: THTTPSend;
  Parser: TJSONParser;
  Url: String;
Begin
  Result := nil;
  Error := '';
  Url := NormalizedUrl(Config);
  If Url = '' Then
  Begin
    Error := 'Whisparr URL is not configured.';
    exit;
  End;
  If Trim(Config.ApiKey) = '' Then
  Begin
    Error := 'Whisparr API key is not configured.';
    exit;
  End;
  If Pos('https://', UTF8LowerCase(Url)) = 1 Then
  Begin
    EnsureSSLLoaded;
    If Not IsSSLloaded Then
    Begin
      Error := 'Unable to initialize SSL/TLS. Install OpenSSL 1.1 x64 ' +
        '(libssl-1_1-x64.dll and libcrypto-1_1-x64.dll).';
      exit;
    End;
  End;
  Http := THTTPSend.Create;
  Try
    Http.Timeout := 15000;
    Http.Headers.Add('X-Api-Key: ' + Trim(Config.ApiKey));
    Http.Headers.Add('Accept: application/json');
    Http.Sock.SSL.VerifyCert := Config.VerifySsl;
    If Not Http.HTTPMethod('GET', Url + Endpoint) Then
    Begin
      Error := 'Unable to connect to Whisparr: ' + Http.Sock.LastErrorDesc;
      exit;
    End;
    If (Http.ResultCode < 200) Or (Http.ResultCode >= 300) Then
    Begin
      Error := Format('Whisparr returned HTTP %d %s.',
        [Http.ResultCode, Http.ResultString]);
      exit;
    End;
    Http.Document.Position := 0;
    Parser := TJSONParser.Create(Http.Document, [joUTF8]);
    Try
      Result := Parser.Parse;
    Except
      On E: Exception Do
      Begin
        Error := 'Invalid JSON from Whisparr: ' + E.Message;
        FreeAndNil(Result);
      End;
    End;
    Parser.Free;
  Finally
    Http.Free;
  End;
End;

Function WhisparrTestConnection(Const Config: TWhisparrConfig;
  Out Version: String; Out Error: String): Boolean;
Var
  Data: TJSONData;
Begin
  Result := False;
  Version := '';
  Error := '';
  If Not WhisparrConfigured(Config) Then
  Begin
    Error := 'Whisparr URL and API key are not configured.';
    exit;
  End;
  Data := RequestJson(Config, '/api/v3/system/status', Error);
  If Data = nil Then
    exit;
  Try
    If Data Is TJSONObject Then
      Version := FindString(TJSONObject(Data), 'version');
    Result := True;
  Finally
    Data.Free;
  End;
End;

Function SeriesTitleFromParseData(Data: TJSONData): String;
Var
  Obj, Series, ParsedInfo: TJSONObject;
  Arr, SeriesArr: TJSONArray;
  IdData: TJSONData;
  i: Integer;
Begin
  Result := '';
  If Data = nil Then
    exit;
  If Data Is TJSONObject Then
  Begin
    Obj := TJSONObject(Data);
    Series := FindObject(Obj, 'series');
    Result := FindString(Series, 'title');
    If Result = '' Then
    Begin
      SeriesArr := FindArray(Obj, 'series');
      If (SeriesArr <> nil) And (SeriesArr.Count > 0) Then
        Result := FindString(SeriesArr.Objects[0], 'title');
    End;
    If Result = '' Then
    Begin
      ParsedInfo := FindObject(Obj, 'parsedEpisodeInfo');
      Result := FindString(ParsedInfo, 'seriesTitle');
    End;
    If Result = '' Then
      Result := FindString(Obj, 'seriesTitle');
    If (Result = '') And ((Obj.Find('titleSlug') <> nil) Or
      (Obj.Find('sortTitle') <> nil) Or (Obj.Find('tvdbId') <> nil)) Then
      Result := FindString(Obj, 'title');
    IdData := Obj.Find('id');
    If (Result = '') And (Obj.Find('parsedEpisodeInfo') = nil) And
      (IdData <> nil) And (IdData.JSONType = jtNumber) And
      (IdData.AsInteger > 0) Then
      Result := FindString(Obj, 'title');
  End
  Else If Data Is TJSONArray Then
  Begin
    Arr := TJSONArray(Data);
    For i := 0 To Arr.Count - 1 Do
    Begin
      Result := SeriesTitleFromParseData(Arr.Items[i]);
      If Result <> '' Then
        break;
    End;
  End;
End;

Function WhisparrParseSeriesName(Const Config: TWhisparrConfig;
  Const Title: String; Out SeriesName: String; Out Error: String): Boolean;
Var
  Data: TJSONData;
Begin
  Result := False;
  SeriesName := '';
  If Trim(Title) = '' Then
  Begin
    Error := 'The torrent name is empty.';
    exit;
  End;
  Data := RequestJson(Config,
    '/api/v3/parse/series?title=' + EncodeURLElement(UTF8Encode(Title)), Error);
  If Data = nil Then
  Begin
    If Pos('HTTP 404', Error) = 0 Then
      exit;
    Data := RequestJson(Config,
      '/api/v3/parse?title=' + EncodeURLElement(UTF8Encode(Title)), Error);
    If Data = nil Then
      exit;
  End;
  Try
    SeriesName := SeriesTitleFromParseData(Data);
    Result := SeriesName <> '';
    If Not Result Then
      Error := 'Whisparr did not recognize the torrent as a series.';
  Finally
    Data.Free;
  End;
End;

Function WhisparrSuggestArchiveName(Const Config: TWhisparrConfig;
  TorrentNames: TStrings; Out ArchiveName: String; Out Error: String): Boolean;
Var
  Matches: TStringList;
  i: Integer;
  SeriesName: String;
Begin
  Result := False;
  ArchiveName := '';
  Error := '';
  If (TorrentNames = nil) Or (TorrentNames.Count = 0) Then
    exit;
  Matches := TStringList.Create;
  Try
    For i := 0 To TorrentNames.Count - 1 Do
    Begin
      If Not WhisparrParseSeriesName(Config, TorrentNames[i], SeriesName,
        Error) Then
        exit;
      SeriesName := WhisparrCleanPathName(SeriesName);
      If SeriesName = '' Then
        exit;
      If Matches.IndexOf(SeriesName) < 0 Then
        Matches.Add(SeriesName);
    End;
    If Matches.Count = 1 Then
    Begin
      ArchiveName := Matches[0];
      Result := True;
    End;
  Finally
    Matches.Free;
  End;
End;

Function WhisparrCleanPathName(Const Value: String): String;
Var
  i: Integer;
  c: Char;
Begin
  Result := '';
  For i := 1 To Length(Value) Do
  Begin
    c := Value[i];
    If (Ord(c) < 32) Or (c In ['<', '>', ':', '"', '/', '\', '|', '?', '*']) Then
      continue;
    Result := Result + c;
  End;
  Result := Trim(Result);
  While (Result <> '') And (Result[Length(Result)] In ['.', ' ']) Do
    Delete(Result, Length(Result), 1);
  While Length(Result) > 128 Do
    Result := UTF8Copy(Result, 1, UTF8Length(Result) - 1);
End;

Function WhisparrParseEpisode(Const Config: TWhisparrConfig;
  Const Title: String; Out EpisodeName: String; Out Error: String): Boolean;
Var
  Data: TJSONData;
  Obj, Series, Episode, ParsedInfo, Quality, QualityValue, FormatObj: TJSONObject;
  Episodes, Formats: TJSONArray;
  FormatNames: TStringList;
  s, SeriesTitle, AirDate, EpisodeTitle, QualityName: String;
  SeriesName, SeriesError: String;
  SeasonNumber, EpisodeNumber, i: Integer;
  EpisodeFound: Boolean;
Begin
  Result := False;
  EpisodeName := '';
  If Trim(Title) = '' Then
  Begin
    Error := 'The torrent name is empty.';
    exit;
  End;
  Data := RequestJson(Config,
    '/api/v3/parse?title=' + EncodeURLElement(UTF8Encode(Title)), Error);
  If Data = nil Then
    exit;
  Try
    If Not (Data Is TJSONObject) Then
    Begin
      Error := 'Unexpected parse response from Whisparr.';
      exit;
    End;
    Obj := TJSONObject(Data);
    Series := FindObject(Obj, 'series');
    ParsedInfo := FindObject(Obj, 'parsedEpisodeInfo');
    SeriesTitle := FindString(Series, 'title');
    If SeriesTitle = '' Then
      SeriesTitle := FindString(ParsedInfo, 'seriesTitle');
    SeasonNumber := -1;
    EpisodeNumber := -1;
    AirDate := '';
    EpisodeTitle := '';
    EpisodeFound := False;
    Episodes := FindArray(Obj, 'episodes');
    If (Episodes <> nil) And (Episodes.Count > 0) Then
    Begin
      Episode := Episodes.Objects[0];
      If Episode <> nil Then
      Begin
        EpisodeFound := True;
        SeasonNumber := FindInt(Episode, 'seasonNumber', -1);
        EpisodeNumber := FindInt(Episode, 'episodeNumber', -1);
        AirDate := FindString(Episode, 'airDate');
        If AirDate = '' Then
          AirDate := FindString(Episode, 'releaseDate');
        EpisodeTitle := FindString(Episode, 'title');
      End;
    End;
    If AirDate = '' Then
      AirDate := FindString(ParsedInfo, 'airDate');
    If Not EpisodeFound Then
    Begin
      If SeriesTitle = '' Then
      Begin
        If WhisparrParseSeriesName(Config, Title, SeriesName, SeriesError) Then
          Error := 'Whisparr did not recognize the torrent as an episode.'
        Else
          Error := 'Whisparr could not determine the series for the torrent.';
      End
      Else
        Error := 'Whisparr did not recognize the torrent as an episode.';
      exit;
    End;
    s := SeriesTitle;
    If (SeasonNumber >= 0) And (EpisodeNumber >= 0) Then
      s := s + Format(' - S%.2dE%.2d', [SeasonNumber, EpisodeNumber])
    Else If AirDate <> '' Then
      s := s + ' - ' + AirDate;
    If EpisodeTitle <> '' Then
      s := s + ' - ' + EpisodeTitle;
    Quality := FindObject(ParsedInfo, 'quality');
    QualityValue := FindObject(Quality, 'quality');
    QualityName := FindString(QualityValue, 'name');
    If (QualityName <> '') And (QualityName <> 'Unknown') Then
      s := s + ' [' + QualityName + ']';
    FormatNames := TStringList.Create;
    Try
      Formats := FindArray(Obj, 'customFormats');
      If Formats <> nil Then
        For i := 0 To Formats.Count - 1 Do
        Begin
          FormatObj := Formats.Objects[i];
          If FormatObj = nil Then
            continue;
          QualityName := FindString(FormatObj, 'name');
          If QualityName <> '' Then
            FormatNames.Add(QualityName);
        End;
      For i := 0 To FormatNames.Count - 1 Do
        s := s + ' ' + FormatNames[i];
    Finally
      FormatNames.Free;
    End;
    EpisodeName := WhisparrCleanPathName(s);
    Result := EpisodeName <> '';
    If Not Result Then
      Error := 'Whisparr did not recognize the torrent as an episode.';
  Finally
    Data.Free;
  End;
End;

End.
