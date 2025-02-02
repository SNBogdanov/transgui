{==============================================================================|
| Project : Ararat Synapse                                       | 003.012.009 |
|==============================================================================|
| Content: HTTP client                                                         |
|==============================================================================|
| Copyright (c)1999-2015, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c) 1999-2015.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(HTTP protocol client)

Used RFC: RFC-1867, RFC-1947, RFC-2388, RFC-2616
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
//old Delphi does not have MSWINDOWS define.
{$IFDEF WIN32}
  {$IFNDEF MSWINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

Unit httpsend;

Interface

Uses
  SysUtils, Classes,
  blcksock, synautil, synaip, synacode, synsock;

Const
  cHttpProtocol = '80';

Type
  {:These encoding types are used internally by the THTTPSend object to identify
   the transfer data types.}
  TTransferEncoding = (TE_UNKNOWN, TE_IDENTITY, TE_CHUNKED);

  {:abstract(Implementation of HTTP protocol.)}
  THTTPSend = Class(TSynaClient)
  Public
    FSock: TTCPBlockSocket;
  Protected
    FTransferEncoding: TTransferEncoding;
    FAliveHost: String;
    FAlivePort: String;
    FHeaders: TStringList;
    FDocument: TMemoryStream;
    FMimeType: String;
    FProtocol: String;
    FKeepAlive: Boolean;
    FKeepAliveTimeout: Integer;
    FStatus100: Boolean;
    FProxyHost: String;
    FProxyPort: String;
    FProxyUser: String;
    FProxyPass: String;
    FResultCode: Integer;
    FResultString: String;
    FUserAgent: String;
    FCookies: TStringList;
    FDownloadSize: Integer;
    FUploadSize: Integer;
    FRangeStart: Integer;
    FRangeEnd: Integer;
    FAddPortNumberToHost: Boolean;
    Function ReadUnknown: Boolean; Virtual;
    Function ReadIdentity(Size: Integer): Boolean; Virtual;
    Function ReadChunked: Boolean; Virtual;
    Procedure ParseCookies;
    Function PrepareHeaders: Ansistring;
    Function InternalDoConnect(needssl: Boolean): Boolean;
    Function InternalConnect(needssl: Boolean): Boolean;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    {:Reset headers, document and Mimetype.}
    Procedure Clear;

    {:Decode ResultCode and ResultString from Value.}
    Procedure DecodeStatus(Const Value: String);

    {:Connects to host defined in URL and accesses resource defined in URL by
     method. If Document is not empty, send it to the server as part of the HTTP
     request. Server response is in Document and headers. Connection may be
     authorised by username and password in URL. If you define proxy properties,
     connection is made by this proxy.
     If all OK, result is @true, else result is @false.

     If you use 'https:' instead of 'http:' in the URL, your request is made
     by SSL/TLS connection (if you do not specify port, then port 443 is used
     instead of standard port 80). If you use SSL/TLS request and you have
     defined HTTP proxy, then HTTP-tunnel mode is automatically used .}
    Function HTTPMethod(Const Method, URL: String): Boolean;

    {:You can call this method from OnStatus event to break current data
     transfer. (or from another thread.)}
    Procedure Abort;
  Published
    {:Before HTTP operation you may define any non-standard headers for HTTP
     request, except: 'Expect: 100-continue', 'Content-Length', 'Content-Type',
     'Connection', 'Authorization', 'Proxy-Authorization' and 'Host' headers.
     After HTTP operation, it contains full headers of the returned document.}
    Property Headers: TStringList read FHeaders;

    {:Stringlist with name-value stringlist pairs. Each pair is one cookie.
     After the HTTP request is returned, cookies are parsed to this stringlist.
     You can leave these cookies untouched for next HTTP requests. You can also
     save this stringlist for later use.}
    Property Cookies: TStringList read FCookies;

    {:Stream with document to send (before request), or with document received
     from HTTP server (after request).}
    Property Document: TMemoryStream read FDocument;

    {:If you need to download only part of a requested document, specify here
     the position of subpart begin. If 0, the full document is requested.}
    Property RangeStart: Integer read FRangeStart write FRangeStart;

    {:If you need to download only part of a requested document, specify here
     the position of subpart end. If 0, the document from rangeStart to end of
     document is requested.
     (Useful for resuming broken downloads, for example.)}
    Property RangeEnd: Integer read FRangeEnd write FRangeEnd;

    {:Mime type of sending data. Default is: 'text/html'.}
    Property MimeType: String read FMimeType write FMimeType;

    {:Define protocol version. Possible values are: '1.1', '1.0' (default)
     and '0.9'.}
    Property Protocol: String read FProtocol write FProtocol;

    {:If @true (default value), keepalives in HTTP protocol 1.1 is enabled.}
    Property KeepAlive: Boolean read FKeepAlive write FKeepAlive;

    {:Define timeout for keepalives in seconds!}
    Property KeepAliveTimeout: Integer read FKeepAliveTimeout write FKeepAliveTimeout;

    {:if @true, then the server is requested for 100status capability when
     uploading data. Default is @false (off).}
    Property Status100: Boolean read FStatus100 write FStatus100;

    {:Address of proxy server (IP address or domain name) where you want to
     connect in @link(HTTPMethod) method.}
    Property ProxyHost: String read FProxyHost write FProxyHost;

    {:Port number for proxy connection. Default value is 8080.}
    Property ProxyPort: String read FProxyPort write FProxyPort;

    {:Username for connection to proxy server used in HTTPMethod method.}
    Property ProxyUser: String read FProxyUser write FProxyUser;

    {:Password for connection to proxy server used in HTTPMethod method.}
    Property ProxyPass: String read FProxyPass write FProxyPass;

    {:Here you can specify custom User-Agent identification.
     Default: 'Mozilla/4.0 (compatible; Synapse)'}
    Property UserAgent: String read FUserAgent write FUserAgent;

    {:Operation result code after successful @link(HTTPMethod) method.}
    Property ResultCode: Integer read FResultCode;

    {:Operation result string after successful @link(HTTPMethod) method.}
    Property ResultString: String read FResultString;

    {:if this value is not 0, then data download is pending. In this case you
     have here the total size of downloaded data. Useful for drawing download
     progressbar from OnStatus event.}
    Property DownloadSize: Integer read FDownloadSize;

    {:if this value is not 0, then data upload is pending. In this case you have
     here the total size of uploaded data. Useful for drawing upload progressbar
     from OnStatus event.}
    Property UploadSize: Integer read FUploadSize;

    {:Socket object used for TCP/IP operation.
     Good for setting OnStatus hook, etc.}
    Property Sock: TTCPBlockSocket read FSock;

    {:Allows to switch off port number in 'Host:' HTTP header. By default @TRUE.
     Some buggy servers do not like port informations in this header.}
    Property AddPortNumberToHost: Boolean read FAddPortNumberToHost
      write FAddPortNumberToHost;
  End;

{:A very useful function, and example of use can be found in the THTTPSend
 object. It implements the GET method of the HTTP protocol. This function sends
 the GET method for URL document to an HTTP server. Returned document is in the
 "Response" stringlist (without any headers). Returns boolean TRUE if all went
 well.}
Function HttpGetText(Const URL: String; Const Response: TStrings): Boolean;

{:A very useful function, and example of use can be found in the THTTPSend
 object. It implements the GET method of the HTTP protocol. This function sends
 the GET method for URL document to an HTTP server. Returned document is in the
 "Response" stream. Returns boolean TRUE if all went well.}
Function HttpGetBinary(Const URL: String; Const Response: TStream): Boolean;

{:A very useful function, and example of use can be found in the THTTPSend
 object. It implements the POST method of the HTTP protocol. This function sends
 the SEND method for a URL document to an HTTP server. The document to be sent
 is located in the "Data" stream. The returned document is in the "Data" stream.
 Returns boolean TRUE if all went well.}
Function HttpPostBinary(Const URL: String; Const Data: TStream): Boolean;

{:A very useful function, and example of use can be found in the THTTPSend
 object. It implements the POST method of the HTTP protocol. This function is
 good for POSTing form data. It sends the POST method for a URL document to
 an HTTP server. You must prepare the form data in the same manner as you would
 the URL data, and pass this prepared data to "URLdata". The following is
 a sample of how the data would appear: 'name=Lukas&field1=some%20data'.
 The information in the field must be encoded by the EncodeURLElement function.
 The returned document is in the "Data" stream. Returns boolean TRUE if all
 went well.}
Function HttpPostURL(Const URL, URLData: String; Const Data: TStream): Boolean;

{:A very useful function, and example of use can be found in the THTTPSend
 object. It implements the POST method of the HTTP protocol. This function sends
 the POST method for a URL document to an HTTP server. This function simulates
 posting of file by HTML form using the 'multipart/form-data' method. The posted
 file is in the DATA stream. Its name is Filename string. Fieldname is for the
 name of the form field with the file. (simulates HTML INPUT FILE) The returned
 document is in the ResultData Stringlist. Returns boolean TRUE if all
 went well.}
Function HttpPostFile(Const URL, FieldName, FileName: String;
  Const Data: TStream; Const ResultData: TStrings): Boolean;

Implementation

Constructor THTTPSend.Create;
Begin
  Inherited Create;
  FHeaders := TStringList.Create;
  FCookies := TStringList.Create;
  FDocument := TMemoryStream.Create;
  FSock := TTCPBlockSocket.Create;
  FSock.Owner := self;
  FSock.ConvertLineEnd := True;
  FSock.SizeRecvBuffer := c64k;
  FSock.SizeSendBuffer := c64k;
  FTimeout := 90000;
  FTargetPort := cHttpProtocol;
  FProxyHost := '';
  FProxyPort := '8080';
  FProxyUser := '';
  FProxyPass := '';
  FAliveHost := '';
  FAlivePort := '';
  FProtocol := '1.1';
  FKeepAlive := True;
  FStatus100 := False;
  FUserAgent := 'Mozilla/4.0 (compatible; transmission-remote-gui)';
  FDownloadSize := 0;
  FUploadSize := 0;
  FAddPortNumberToHost := True;
  FKeepAliveTimeout := 300;
  Clear;
End;

Destructor THTTPSend.Destroy;
Begin
  FSock.Free;
  FDocument.Free;
  FCookies.Free;
  FHeaders.Free;
  Inherited Destroy;
End;

Procedure THTTPSend.Clear;
Begin
  FRangeStart := 0;
  FRangeEnd := 0;
  FDocument.Clear;
  FHeaders.Clear;
  FMimeType := 'text/html';
End;

Procedure THTTPSend.DecodeStatus(Const Value: String);
Var
  s, su: String;
Begin
  s := Trim(SeparateRight(Value, ' '));
  su := Trim(SeparateLeft(s, ' '));
  FResultCode := StrToIntDef(su, 0);
  FResultString := Trim(SeparateRight(s, ' '));
  If FResultString = s Then
    FResultString := '';
End;

Function THTTPSend.PrepareHeaders: Ansistring;
Begin
  If FProtocol = '0.9' Then
    Result := FHeaders[0] + CRLF
  Else
    {$IFNDEF MSWINDOWS}
    Result := {$IFDEF UNICODE}AnsiString{$ENDIF}(AdjustLineBreaks(FHeaders.Text, tlbsCRLF));
    {$ELSE}
    Result := FHeaders.Text;
  {$ENDIF}
End;

Function THTTPSend.InternalDoConnect(needssl: Boolean): Boolean;
Begin
  Result := False;
  FSock.CloseSocket;
  FSock.Bind(FIPInterface, cAnyPort);
  If FSock.LastError <> 0 Then
    Exit;
  FSock.Connect(FTargetHost, FTargetPort);
  If FSock.LastError <> 0 Then
    Exit;
  If needssl Then
  Begin
    If (FSock.SSL.SNIHost = '') Then
      FSock.SSL.SNIHost := FTargetHost;
    FSock.SSLDoConnect;
    FSock.SSL.SNIHost := '';
    //don't need it anymore and don't wan't to reuse it in next connection
    If FSock.LastError <> 0 Then
      Exit;
  End;
  FAliveHost := FTargetHost;
  FAlivePort := FTargetPort;
  Result := True;
End;

Function THTTPSend.InternalConnect(needssl: Boolean): Boolean;
Begin
  If FSock.Socket = INVALID_SOCKET Then
    Result := InternalDoConnect(needssl)
  Else
    If (FAliveHost <> FTargetHost) Or (FAlivePort <> FTargetPort) Or
      FSock.CanRead(0) Then
      Result := InternalDoConnect(needssl)
    Else
      Result := True;
End;

Function THTTPSend.HTTPMethod(Const Method, URL: String): Boolean;
Var
  Sending, Receiving: Boolean;
  status100: Boolean;
  status100error: String;
  ToClose: Boolean;
  Size: Integer;
  Prot, User, Pass, Host, Port, Path, Para, URI: String;
  s, su: Ansistring;
  HttpTunnel: Boolean;
  n: Integer;
  pp: String;
  UsingProxy: Boolean;
  l: TStringList;
  x: Integer;
Begin
  {initial values}
  Result := False;
  FResultCode := 500;
  FResultString := '';
  FDownloadSize := 0;
  FUploadSize := 0;

  URI := ParseURL(URL, Prot, User, Pass, Host, Port, Path, Para);
  User := DecodeURL(user);
  Pass := DecodeURL(pass);
  If User = '' Then
  Begin
    User := FUsername;
    Pass := FPassword;
  End;
  If UpperCase(Prot) = 'HTTPS' Then
  Begin
    HttpTunnel := FProxyHost <> '';
    FSock.HTTPTunnelIP := FProxyHost;
    FSock.HTTPTunnelPort := FProxyPort;
    FSock.HTTPTunnelUser := FProxyUser;
    FSock.HTTPTunnelPass := FProxyPass;
  End
  Else
  Begin
    HttpTunnel := False;
    FSock.HTTPTunnelIP := '';
    FSock.HTTPTunnelPort := '';
    FSock.HTTPTunnelUser := '';
    FSock.HTTPTunnelPass := '';
  End;
  UsingProxy := (FProxyHost <> '') And Not (HttpTunnel);
  Sending := FDocument.Size > 0;
  {Headers for Sending data}
  status100 := FStatus100 And Sending And (FProtocol = '1.1');
  If status100 Then
    FHeaders.Insert(0, 'Expect: 100-continue');
  If Sending Then
  Begin
    FHeaders.Insert(0, 'Content-Length: ' + IntToStr(FDocument.Size));
    If FMimeType <> '' Then
      FHeaders.Insert(0, 'Content-Type: ' + FMimeType);
  End;
  { setting User-agent }
  If FUserAgent <> '' Then
    FHeaders.Insert(0, 'User-Agent: ' + FUserAgent);
  { setting Ranges }
  If (FRangeStart > 0) Or (FRangeEnd > 0) Then
  Begin
    If FRangeEnd >= FRangeStart Then
      FHeaders.Insert(0, 'Range: bytes=' + IntToStr(FRangeStart) +
        '-' + IntToStr(FRangeEnd))
    Else
      FHeaders.Insert(0, 'Range: bytes=' + IntToStr(FRangeStart) + '-');
  End;
  { setting Cookies }
  s := '';
  For n := 0 To FCookies.Count - 1 Do
  Begin
    If s <> '' Then
      s := s + '; ';
    s := s + FCookies[n];
  End;
  If s <> '' Then
    FHeaders.Insert(0, 'Cookie: ' + s);
  { setting KeepAlives }
  pp := '';
  If UsingProxy Then
    pp := 'Proxy-';
  If FKeepAlive Then
  Begin
    FHeaders.Insert(0, pp + 'Connection: keep-alive');
    FHeaders.Insert(0, 'Keep-Alive: ' + IntToStr(FKeepAliveTimeout));
  End
  Else
    FHeaders.Insert(0, pp + 'Connection: close');
  { set target servers/proxy, authorizations, etc... }
  If (User <> '') Or (Pass <> '') Then
    FHeaders.Insert(0, 'Authorization: Basic ' + EncodeBase64(User + ':' + Pass));
  If UsingProxy And (FProxyUser <> '') Then
    FHeaders.Insert(0, 'Proxy-Authorization: Basic ' +
      EncodeBase64(FProxyUser + ':' + FProxyPass));
  If isIP6(Host) Then
    s := '[' + Host + ']'
  Else
    s := Host;
  If FAddPortNumberToHost And (((Port <> '80') And (UpperCase(Prot) = 'HTTP')) Or
    ((Port <> '443') And (UpperCase(Prot) = 'HTTPS'))) Then
    FHeaders.Insert(0, 'Host: ' + s + ':' + Port)
  Else
    FHeaders.Insert(0, 'Host: ' + s);
  If UsingProxy Then
    URI := Prot + '://' + s + ':' + Port + URI;
  If URI = '/*' Then
    URI := '*';
  If FProtocol = '0.9' Then
    FHeaders.Insert(0, UpperCase(Method) + ' ' + URI)
  Else
    FHeaders.Insert(0, UpperCase(Method) + ' ' + URI + ' HTTP/' + FProtocol);
  If UsingProxy Then
  Begin
    FTargetHost := FProxyHost;
    FTargetPort := FProxyPort;
  End
  Else
  Begin
    FTargetHost := Host;
    FTargetPort := Port;
  End;
  If FHeaders[FHeaders.Count - 1] <> '' Then
    FHeaders.Add('');

  { connect }
  If Not InternalConnect(UpperCase(Prot) = 'HTTPS') Then
  Begin
    FAliveHost := '';
    FAlivePort := '';
    Exit;
  End;

  { reading Status }
  FDocument.Position := 0;
  Status100Error := '';
  If status100 Then
  Begin
    { send Headers }
    FSock.SendString(PrepareHeaders);
    If FSock.LastError <> 0 Then
      Exit;
    Repeat
      s := FSock.RecvString(FTimeout);
      If s <> '' Then
        Break;
    Until FSock.LastError <> 0;
    DecodeStatus(s);
    Status100Error := s;
    Repeat
      s := FSock.recvstring(FTimeout);
      If s = '' Then
        Break;
    Until FSock.LastError <> 0;
    If (FResultCode >= 100) And (FResultCode < 200) Then
    Begin
      { we can upload content }
      Status100Error := '';
      FUploadSize := FDocument.Size;
      FSock.SendBuffer(FDocument.Memory, FDocument.Size);
    End;
  End
  Else
  { upload content }
    If sending Then
    Begin
      If FDocument.Size >= c64k Then
      Begin
        FSock.SendString(PrepareHeaders);
        FUploadSize := FDocument.Size;
        FSock.SendBuffer(FDocument.Memory, FDocument.Size);
      End
      Else
      Begin
        s := PrepareHeaders + ReadStrFromStream(FDocument, FDocument.Size);
        FUploadSize := Length(s);
        FSock.SendString(s);
      End;
    End
    Else
    Begin
      { we not need to upload document, send headers only }
      FSock.SendString(PrepareHeaders);
    End;

  If FSock.LastError <> 0 Then
    Exit;

  Clear;
  Size := -1;
  FTransferEncoding := TE_UNKNOWN;

  { read status }
  If Status100Error = '' Then
  Begin
    Repeat
      Repeat
        s := FSock.RecvString(FTimeout);
        If s <> '' Then
          Break;
      Until FSock.LastError <> 0;
      If Pos('HTTP/', UpperCase(s)) = 1 Then
      Begin
        FHeaders.Add(s);
        DecodeStatus(s);
      End
      Else
      Begin
        { old HTTP 0.9 and some buggy servers not send result }
        s := s + CRLF;
        WriteStrToStream(FDocument, s);
        FResultCode := 0;
      End;
    Until (FSock.LastError <> 0) Or (FResultCode <> 100);
  End
  Else
    FHeaders.Add(Status100Error);

  { if need receive headers, receive and parse it }
  ToClose := FProtocol <> '1.1';
  If FHeaders.Count > 0 Then
  Begin
    l := TStringList.Create;
    Try
      Repeat
        s := FSock.RecvString(FTimeout);
        l.Add(s);
        If s = '' Then
          Break;
      Until FSock.LastError <> 0;
      x := 0;
      While l.Count > x Do
      Begin
        s := NormalizeHeader(l, x);
        FHeaders.Add(s);
        su := UpperCase(s);
        If Pos('CONTENT-LENGTH:', su) = 1 Then
        Begin
          Size := StrToIntDef(Trim(SeparateRight(s, ':')), -1);
          If (Size <> -1) And (FTransferEncoding = TE_UNKNOWN) Then
            FTransferEncoding := TE_IDENTITY;
        End;
        If Pos('CONTENT-TYPE:', su) = 1 Then
          FMimeType := Trim(SeparateRight(s, ':'));
        If Pos('TRANSFER-ENCODING:', su) = 1 Then
        Begin
          s := Trim(SeparateRight(su, ':'));
          If Pos('CHUNKED', s) > 0 Then
            FTransferEncoding := TE_CHUNKED;
        End;
        If UsingProxy Then
        Begin
          If Pos('PROXY-CONNECTION:', su) = 1 Then
            If Pos('CLOSE', su) > 0 Then
              ToClose := True;
        End
        Else
        Begin
          If Pos('CONNECTION:', su) = 1 Then
            If Pos('CLOSE', su) > 0 Then
              ToClose := True;
        End;
      End;
    Finally
      l.Free;
    End;
  End;

  Result := FSock.LastError = 0;
  If Not Result Then
  Begin
    FSock.CloseSocket;
    FAliveHost := '';
    FAlivePort := '';
    Exit;
  End;

  {if need receive response body, read it}
  Receiving := Method <> 'HEAD';
  Receiving := Receiving And (FResultCode <> 204);
  Receiving := Receiving And (FResultCode <> 304);
  If Receiving Then
    Case FTransferEncoding Of
      TE_UNKNOWN:
        Result := ReadUnknown;
      TE_IDENTITY:
        Result := ReadIdentity(Size);
      TE_CHUNKED:
        Result := ReadChunked;
    End;

  FDocument.Seek(0, soFromBeginning);
  If ToClose Then
  Begin
    FSock.CloseSocket;
    FAliveHost := '';
    FAlivePort := '';
  End;
  ParseCookies;
End;

Function THTTPSend.ReadUnknown: Boolean;
Var
  s: Ansistring;
Begin
  Result := False;
  Repeat
    s := FSock.RecvPacket(FTimeout);
    If FSock.LastError = 0 Then
      WriteStrToStream(FDocument, s);
  Until FSock.LastError <> 0;
  If FSock.LastError = WSAECONNRESET Then
  Begin
    Result := True;
    FSock.ResetLastError;
  End;
End;

Function THTTPSend.ReadIdentity(Size: Integer): Boolean;
Begin
  If Size > 0 Then
  Begin
    FDownloadSize := Size;
    FSock.RecvStreamSize(FDocument, FTimeout, Size);
    FDocument.Position := FDocument.Size;
    Result := FSock.LastError = 0;
  End
  Else
    Result := True;
End;

Function THTTPSend.ReadChunked: Boolean;
Var
  s: Ansistring;
  Size: Integer;
Begin
  Repeat
    Repeat
      s := FSock.RecvString(FTimeout);
    Until (s <> '') Or (FSock.LastError <> 0);
    If FSock.LastError <> 0 Then
      Break;
    s := Trim(SeparateLeft(s, ' '));
    s := Trim(SeparateLeft(s, ';'));
    Size := StrToIntDef('$' + s, 0);
    If Size = 0 Then
      Break;
    If Not ReadIdentity(Size) Then
      break;
  Until False;
  Result := FSock.LastError = 0;
End;

Procedure THTTPSend.ParseCookies;
Var
  n: Integer;
  s: String;
  sn, sv: String;
Begin
  For n := 0 To FHeaders.Count - 1 Do
    If Pos('set-cookie:', lowercase(FHeaders[n])) = 1 Then
    Begin
      s := SeparateRight(FHeaders[n], ':');
      s := trim(SeparateLeft(s, ';'));
      sn := trim(SeparateLeft(s, '='));
      sv := trim(SeparateRight(s, '='));
      FCookies.Values[sn] := sv;
    End;
End;

Procedure THTTPSend.Abort;
Begin
  FSock.StopFlag := True;
End;

{==============================================================================}

Function HttpGetText(Const URL: String; Const Response: TStrings): Boolean;
Var
  HTTP: THTTPSend;
Begin
  HTTP := THTTPSend.Create;
  Try
    Result := HTTP.HTTPMethod('GET', URL);
    If Result Then
      Response.LoadFromStream(HTTP.Document);
  Finally
    HTTP.Free;
  End;
End;

Function HttpGetBinary(Const URL: String; Const Response: TStream): Boolean;
Var
  HTTP: THTTPSend;
Begin
  HTTP := THTTPSend.Create;
  Try
    Result := HTTP.HTTPMethod('GET', URL);
    If Result Then
    Begin
      Response.Seek(0, soFromBeginning);
      Response.CopyFrom(HTTP.Document, 0);
    End;
  Finally
    HTTP.Free;
  End;
End;

Function HttpPostBinary(Const URL: String; Const Data: TStream): Boolean;
Var
  HTTP: THTTPSend;
Begin
  HTTP := THTTPSend.Create;
  Try
    HTTP.Document.CopyFrom(Data, 0);
    HTTP.MimeType := 'Application/octet-stream';
    Result := HTTP.HTTPMethod('POST', URL);
    Data.Size := 0;
    If Result Then
    Begin
      Data.Seek(0, soFromBeginning);
      Data.CopyFrom(HTTP.Document, 0);
    End;
  Finally
    HTTP.Free;
  End;
End;

Function HttpPostURL(Const URL, URLData: String; Const Data: TStream): Boolean;
Var
  HTTP: THTTPSend;
Begin
  HTTP := THTTPSend.Create;
  Try
    WriteStrToStream(HTTP.Document, URLData);
    HTTP.MimeType := 'application/x-www-form-urlencoded';
    Result := HTTP.HTTPMethod('POST', URL);
    If Result Then
      Data.CopyFrom(HTTP.Document, 0);
  Finally
    HTTP.Free;
  End;
End;

Function HttpPostFile(Const URL, FieldName, FileName: String;
  Const Data: TStream; Const ResultData: TStrings): Boolean;
Var
  HTTP: THTTPSend;
  Bound, s: String;
Begin
  Bound := IntToHex(Random(MaxInt), 8) + '_Synapse_boundary';
  HTTP := THTTPSend.Create;
  Try
    s := '--' + Bound + CRLF;
    s := s + 'content-disposition: form-data; name="' + FieldName + '";';
    s := s + ' filename="' + FileName + '"' + CRLF;
    s := s + 'Content-Type: Application/octet-string' + CRLF + CRLF;
    WriteStrToStream(HTTP.Document, s);
    HTTP.Document.CopyFrom(Data, 0);
    s := CRLF + '--' + Bound + '--' + CRLF;
    WriteStrToStream(HTTP.Document, s);
    HTTP.MimeType := 'multipart/form-data; boundary=' + Bound;
    Result := HTTP.HTTPMethod('POST', URL);
    If Result Then
      ResultData.LoadFromStream(HTTP.Document);
  Finally
    HTTP.Free;
  End;
End;

End.
