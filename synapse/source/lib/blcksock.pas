{==============================================================================|
| Project : Ararat Synapse                                       | 009.010.000 |
|==============================================================================|
| Content: Library base                                                        |
|==============================================================================|
| Copyright (c)1999-2017, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)1999-2017.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{
Special thanks to Gregor Ibic <gregor.ibic@intelicom.si>
 (Intelicom d.o.o., http://www.intelicom.si)
 for good inspiration about SSL programming.
}

{$DEFINE ONCEWINSOCK}
{Note about define ONCEWINSOCK:
If you remove this compiler directive, then socket interface is loaded and
initialized on constructor of TBlockSocket class for each socket separately.
Socket interface is used only if your need it.

If you leave this directive here, then socket interface is loaded and
initialized only once at start of your program! It boost performace on high
count of created and destroyed sockets. It eliminate possible small resource
leak on Windows systems too.
}

//{$DEFINE RAISEEXCEPT}
{When you enable this define, then is Raiseexcept property is on by default
}

{:@abstract(Synapse's library core)

Core with implementation basic socket classes.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE BCB}
{$ENDIF}
{$IFDEF BCB}
  {$ObjExportAll On}
{$ENDIF}
{$Q-}
{$H+}
{$M+}
{$TYPEDADDRESS OFF}


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

Unit blcksock;

Interface

Uses
  SysUtils, Classes,
  synafpc,
  synsock, synautil, synacode, synaip
  {$IFDEF CIL}
  ,System.Net
  ,System.Net.Sockets
  ,System.Text
  {$ENDIF}
  ;

Const

  SynapseRelease = '40';

  cLocalhost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  cBroadcast = '255.255.255.255';
  c6Localhost = '::1';
  c6AnyHost = '::0';
  c6Broadcast = 'ffff::1';
  cAnyPort = '0';
  CR = #$0d;
  LF = #$0a;
  CRLF = CR + LF;
  c64k = 65536;

Type

  {:@abstract(Exception clas used by Synapse)
   When you enable generating of exceptions, this exception is raised by
   Synapse's units.}
  ESynapseError = Class(Exception)
  Private
    FErrorCode: Integer;
    FErrorMessage: String;
  Published
    {:Code of error. Value depending on used operating system}
    Property ErrorCode: Integer read FErrorCode write FErrorCode;
    {:Human readable description of error.}
    Property ErrorMessage: String read FErrorMessage write FErrorMessage;
  End;

  {:Types of OnStatus events}
  THookSocketReason = (
    {:Resolving is begin. Resolved IP and port is in parameter in format like:
     'localhost.somewhere.com:25'.}
    HR_ResolvingBegin,
    {:Resolving is done. Resolved IP and port is in parameter in format like:
     'localhost.somewhere.com:25'. It is always same as in HR_ResolvingBegin!}
    HR_ResolvingEnd,
    {:Socket created by CreateSocket method. It reporting Family of created
     socket too!}
    HR_SocketCreate,
    {:Socket closed by CloseSocket method.}
    HR_SocketClose,
    {:Socket binded to IP and Port. Binded IP and Port is in parameter in format
     like: 'localhost.somewhere.com:25'.}
    HR_Bind,
    {:Socket connected to IP and Port. Connected IP and Port is in parameter in
     format like: 'localhost.somewhere.com:25'.}
    HR_Connect,
    {:Called when CanRead method is used with @True result.}
    HR_CanRead,
    {:Called when CanWrite method is used with @True result.}
    HR_CanWrite,
    {:Socket is swithed to Listen mode. (TCP socket only)}
    HR_Listen,
    {:Socket Accepting client connection. (TCP socket only)}
    HR_Accept,
    {:report count of bytes readed from socket. Number is in parameter string.
     If you need is in integer, you must use StrToInt function!}
    HR_ReadCount,
    {:report count of bytes writed to socket. Number is in parameter string. If
     you need is in integer, you must use StrToInt function!}
    HR_WriteCount,
    {:If is limiting of bandwidth on, then this reason is called when sending or
     receiving is stopped for satisfy bandwidth limit. Parameter is count of
     waiting milliseconds.}
    HR_Wait,
    {:report situation where communication error occured. When raiseexcept is
     @true, then exception is called after this Hook reason.}
    HR_Error
    );

  {:Procedural type for OnStatus event. Sender is calling TBlockSocket object,
   Reason is one of set Status events and value is optional data.}
  THookSocketStatus = Procedure(Sender: TObject; Reason: THookSocketReason;
    Const Value: String) Of Object;

  {:This procedural type is used for DataFilter hooks.}
  THookDataFilter = Procedure(Sender: TObject; Var Value: Ansistring) Of Object;

  {:This procedural type is used for hook OnCreateSocket. By this hook you can
   insert your code after initialisation of socket. (you can set special socket
   options, etc.)}
  THookCreateSocket = Procedure(Sender: TObject) Of Object;

  {:This procedural type is used for monitoring of communication.}
  THookMonitor = Procedure(Sender: TObject; Writing: Boolean;
    Const Buffer: TMemory; Len: Integer) Of Object;

  {:This procedural type is used for hook OnAfterConnect. By this hook you can
   insert your code after TCP socket has been sucessfully connected.}
  THookAfterConnect = Procedure(Sender: TObject) Of Object;

  {:This procedural type is used for hook OnVerifyCert. By this hook you can
   insert your additional certificate verification code. Usefull to verify server
   CN against URL. }

  THookVerifyCert = Function(Sender: TObject): Boolean Of Object;

 {:This procedural type is used for hook OnHeartbeat. By this hook you can
   call your code repeately during long socket operations.
   You must enable heartbeats by @Link(HeartbeatRate) property!}
  THookHeartbeat = Procedure(Sender: TObject) Of Object;

  {:Specify family of socket.}
  TSocketFamily = (
    {:Default mode. Socket family is defined by target address for connection.
     It allows instant access to IPv4 and IPv6 nodes. When you need IPv6 address
     as destination, then is used IPv6 mode. othervise is used IPv4 mode.
     However this mode not working properly with preliminary IPv6 supports!}
    SF_Any,
    {:Turn this class to pure IPv4 mode. This mode is totally compatible with
     previous Synapse releases.}
    SF_IP4,
    {:Turn to only IPv6 mode.}
    SF_IP6
    );

  {:specify possible values of SOCKS modes.}
  TSocksType = (
    ST_Socks5,
    ST_Socks4
    );

  {:Specify requested SSL/TLS version for secure connection.}
  TSSLType = (
    LT_all,
    LT_TLSv1,
    LT_TLSv1_1,
    LT_TLSv1_2,
    LT_SSHv2
    );

  {:Specify type of socket delayed option.}
  TSynaOptionType = (
    SOT_Linger,
    SOT_RecvBuff,
    SOT_SendBuff,
    SOT_NonBlock,
    SOT_RecvTimeout,
    SOT_SendTimeout,
    SOT_Reuse,
    SOT_TTL,
    SOT_Broadcast,
    SOT_MulticastTTL,
    SOT_MulticastLoop
    );

  {:@abstract(this object is used for remember delayed socket option set.)}
  TSynaOption = Class(TObject)
  Public
    Option: TSynaOptionType;
    Enabled: Boolean;
    Value: Integer;
  End;

  TCustomSSL = Class;
  TSSLClass = Class Of TCustomSSL;

  {:@abstract(Basic IP object.)
   This is parent class for other class with protocol implementations. Do not
   use this class directly! Use @link(TICMPBlockSocket), @link(TRAWBlockSocket),
   @link(TTCPBlockSocket) or @link(TUDPBlockSocket) instead.}
  TBlockSocket = Class(TObject)
  Private
    FOnStatus: THookSocketStatus;
    FOnReadFilter: THookDataFilter;
    FOnCreateSocket: THookCreateSocket;
    FOnMonitor: THookMonitor;
    FOnHeartbeat: THookHeartbeat;
    FLocalSin: TVarSin;
    FRemoteSin: TVarSin;
    FTag: Integer;
    FBuffer: Ansistring;
    FRaiseExcept: Boolean;
    FNonBlockMode: Boolean;
    FMaxLineLength: Integer;
    FMaxSendBandwidth: Integer;
    FNextSend: Longword;
    FMaxRecvBandwidth: Integer;
    FNextRecv: Longword;
    FConvertLineEnd: Boolean;
    FLastCR: Boolean;
    FLastLF: Boolean;
    FBinded: Boolean;
    FFamily: TSocketFamily;
    FFamilySave: TSocketFamily;
    FIP6used: Boolean;
    FPreferIP4: Boolean;
    FDelayedOptions: TList;
    FInterPacketTimeout: Boolean;
    {$IFNDEF CIL}
    FFDSet: TFDSet;
    {$ENDIF}
    FRecvCounter: Integer;
    FSendCounter: Integer;
    FSendMaxChunk: Integer;
    FStopFlag: Boolean;
    FNonblockSendTimeout: Integer;
    FHeartbeatRate: Integer;
    FConnectionTimeout: Integer;
    {$IFNDEF ONCEWINSOCK}
    FWsaDataOnce: TWSADATA;
    {$ENDIF}
    Function GetSizeRecvBuffer: Integer;
    Procedure SetSizeRecvBuffer(Size: Integer);
    Function GetSizeSendBuffer: Integer;
    Procedure SetSizeSendBuffer(Size: Integer);
    Procedure SetNonBlockMode(Value: Boolean);
    Procedure SetTTL(TTL: Integer);
    Function GetTTL: Integer;
    Procedure SetFamily(Value: TSocketFamily); Virtual;
    Procedure SetSocket(Value: TSocket); Virtual;
    Function GetWsaData: TWSAData;
    Function FamilyToAF(f: TSocketFamily): TAddrFamily;
  Protected
    FSocket: TSocket;
    FLastError: Integer;
    FLastErrorDesc: String;
    FOwner: TObject;
    Procedure SetDelayedOption(Const Value: TSynaOption);
    Procedure DelayedOption(Const Value: TSynaOption);
    Procedure ProcessDelayedOptions;
    Procedure InternalCreateSocket(Sin: TVarSin);
    Procedure SetSin(Var Sin: TVarSin; IP, Port: String);
    Function GetSinIP(Sin: TVarSin): String;
    Function GetSinPort(Sin: TVarSin): Integer;
    Procedure DoStatus(Reason: THookSocketReason; Const Value: String);
    Procedure DoReadFilter(Buffer: TMemory; Var Len: Integer);
    Procedure DoMonitor(Writing: Boolean; Const Buffer: TMemory; Len: Integer);
    Procedure DoCreateSocket;
    Procedure DoHeartbeat;
    Procedure LimitBandwidth(Length: Integer; MaxB: Integer; Var Next: Longword);
    Procedure SetBandwidth(Value: Integer);
    Function TestStopFlag: Boolean;
    Procedure InternalSendStream(Const Stream: TStream;
      WithSize, Indy: Boolean); Virtual;
    Function InternalCanRead(Timeout: Integer): Boolean; Virtual;
    Function InternalCanWrite(Timeout: Integer): Boolean; Virtual;
  Public
    Constructor Create;

    {:Create object and load all necessary socket library. What library is
     loaded is described by STUB parameter. If STUB is empty string, then is
     loaded default libraries.}
    Constructor CreateAlternate(Stub: String);
    Destructor Destroy; Override;

    {:If @link(family) is not SF_Any, then create socket with type defined in
     @link(Family) property. If family is SF_Any, then do nothing! (socket is
     created automaticly when you know what type of socket you need to create.
     (i.e. inside @link(Connect) or @link(Bind) call.) When socket is created,
     then is aplyed all stored delayed socket options.}
    Procedure CreateSocket;

    {:It create socket. Address resolving of Value tells what type of socket is
     created. If Value is resolved as IPv4 IP, then is created IPv4 socket. If
     value is resolved as IPv6 address, then is created IPv6 socket.}
    Procedure CreateSocketByName(Const Value: String);

    {:Destroy socket in use. This method is also automatically called from
     object destructor.}
    Procedure CloseSocket; Virtual;

    {:Abort any work on Socket and destroy them.}
    Procedure AbortSocket; Virtual;

    {:Connects socket to local IP address and PORT. IP address may be numeric or
     symbolic ('192.168.74.50', 'cosi.nekde.cz', 'ff08::1'). The same for PORT
     - it may be number or mnemonic port ('23', 'telnet').

     If port value is '0', system chooses itself and conects unused port in the
     range 1024 to 4096 (this depending by operating system!). Structure
     LocalSin is filled after calling this method.

     Note: If you call this on non-created socket, then socket is created
     automaticly.

     Warning: when you call : Bind('0.0.0.0','0'); then is nothing done! In this
     case is used implicit system bind instead.}
    Procedure Bind(IP, Port: String);

    {:Connects socket to remote IP address and PORT. The same rules as with
     @link(BIND) method are valid. The only exception is that PORT with 0 value
     will not be connected!

     Structures LocalSin and RemoteSin will be filled with valid values.

     When you call this on non-created socket, then socket is created
     automaticly. Type of created socket is by @link(Family) property. If is
     used SF_IP4, then is created socket for IPv4. If is used SF_IP6, then is
     created socket for IPv6. When you have family on SF_Any (default!), then
     type of created socket is determined by address resolving of destination
     address. (Not work properly on prilimitary winsock IPv6 support!)}
    Procedure Connect(IP, Port: String); Virtual;

    {:Sets socket to receive mode for new incoming connections. It is necessary
     to use @link(TBlockSocket.BIND) function call before this method to select
     receiving port!}
    Procedure Listen; Virtual;

    {:Waits until new incoming connection comes. After it comes a new socket is
     automatically created (socket handler is returned by this function as
     result).}
    Function Accept: TSocket; Virtual;

    {:Sends data of LENGTH from BUFFER address via connected socket. System
     automatically splits data to packets.}
    Function SendBuffer(Buffer: Tmemory; Length: Integer): Integer; Virtual;

    {:One data BYTE is sent via connected socket.}
    Procedure SendByte(Data: Byte); Virtual;

    {:Send data string via connected socket. Any terminator is not added! If you
     need send true string with CR-LF termination, you must add CR-LF characters
     to sended string! Because any termination is not added automaticly, you can
     use this function for sending any binary data in binary string.}
    Procedure SendString(Data: Ansistring); Virtual;

    {:Send integer as four bytes to socket.}
    Procedure SendInteger(Data: Integer); Virtual;

    {:Send data as one block to socket. Each block begin with 4 bytes with
     length of data in block. This 4 bytes is added automaticly by this
     function.}
    Procedure SendBlock(Const Data: Ansistring); Virtual;

    {:Send data from stream to socket.}
    Procedure SendStreamRaw(Const Stream: TStream); Virtual;

    {:Send content of stream to socket. It using @link(SendBlock) method}
    Procedure SendStream(Const Stream: TStream); Virtual;

    {:Send content of stream to socket. It using @link(SendBlock) method and
    this is compatible with streams in Indy library.}
    Procedure SendStreamIndy(Const Stream: TStream); Virtual;

    {:Note: This is low-level receive function. You must be sure if data is
     waiting for read before call this function for avoid deadlock!

     Waits until allocated buffer is filled by received data. Returns number of
     data received, which equals to LENGTH value under normal operation. If it
     is not equal the communication channel is possibly broken.

     On stream oriented sockets if is received 0 bytes, it mean 'socket is
     closed!"

     On datagram socket is readed first waiting datagram.}
    Function RecvBuffer(Buffer: TMemory; Length: Integer): Integer; Virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions!

     Method waits until data is received. If no data is received within TIMEOUT
     (in milliseconds) period, @link(LastError) is set to WSAETIMEDOUT. Methods
     serves for reading any size of data (i.e. one megabyte...). This method is
     preffered for reading from stream sockets (like TCP).}
    Function RecvBufferEx(Buffer: Tmemory; Len: Integer; Timeout: Integer): Integer;
      Virtual;

    {:Similar to @link(RecvBufferEx), but readed data is stored in binary
     string, not in memory buffer.}
    Function RecvBufferStr(Len: Integer; Timeout: Integer): Ansistring; Virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Waits until one data byte is received which is also returned as function
     result. If no data is received within TIMEOUT (in milliseconds)period,
     @link(LastError) is set to WSAETIMEDOUT and result have value 0.}
    Function RecvByte(Timeout: Integer): Byte; Virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Waits until one four bytes are received and return it as one Ineger Value.
     If no data is received within TIMEOUT (in milliseconds)period,
     @link(LastError) is set to WSAETIMEDOUT and result have value 0.}
    Function RecvInteger(Timeout: Integer): Integer; Virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Method waits until data string is received. This string is terminated by
     CR-LF characters. The resulting string is returned without this termination
     (CR-LF)! If @link(ConvertLineEnd) is used, then CR-LF sequence may not be
     exactly CR-LF. See @link(ConvertLineEnd) description. If no data is
     received within TIMEOUT (in milliseconds) period, @link(LastError) is set
     to WSAETIMEDOUT. You may also specify maximum length of reading data by
     @link(MaxLineLength) property.}
    Function RecvString(Timeout: Integer): Ansistring; Virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Method waits until data string is received. This string is terminated by
     Terminator string. The resulting string is returned without this
     termination. If no data is received within TIMEOUT (in milliseconds)
     period, @link(LastError) is set to WSAETIMEDOUT. You may also specify
     maximum length of reading data by @link(MaxLineLength) property.}
    Function RecvTerminated(Timeout: Integer; Const Terminator: Ansistring): Ansistring;
      Virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Method reads all data waiting for read. If no data is received within
     TIMEOUT (in milliseconds) period, @link(LastError) is set to WSAETIMEDOUT.
     Methods serves for reading unknown size of data. Because before call this
     function you don't know size of received data, returned data is stored in
     dynamic size binary string. This method is preffered for reading from
     stream sockets (like TCP). It is very goot for receiving datagrams too!
     (UDP protocol)}
    Function RecvPacket(Timeout: Integer): Ansistring; Virtual;

    {:Read one block of data from socket. Each block begin with 4 bytes with
     length of data in block. This function read first 4 bytes for get lenght,
     then it wait for reported count of bytes.}
    Function RecvBlock(Timeout: Integer): Ansistring; Virtual;

    {:Read all data from socket to stream until socket is closed (or any error
     occured.)}
    Procedure RecvStreamRaw(Const Stream: TStream; Timeout: Integer); Virtual;
    {:Read requested count of bytes from socket to stream.}
    Procedure RecvStreamSize(Const Stream: TStream; Timeout: Integer; Size: Integer);

    {:Receive data to stream. It using @link(RecvBlock) method.}
    Procedure RecvStream(Const Stream: TStream; Timeout: Integer); Virtual;

    {:Receive data to stream. This function is compatible with similar function
    in Indy library. It using @link(RecvBlock) method.}
    Procedure RecvStreamIndy(Const Stream: TStream; Timeout: Integer); Virtual;

    {:Same as @link(RecvBuffer), but readed data stays in system input buffer.
    Warning: this function not respect data in @link(LineBuffer)! Is not
    recommended to use this function!}
    Function PeekBuffer(Buffer: TMemory; Length: Integer): Integer; Virtual;

    {:Same as @link(RecvByte), but readed data stays in input system buffer.
     Warning: this function not respect data in @link(LineBuffer)! Is not
    recommended to use this function!}
    Function PeekByte(Timeout: Integer): Byte; Virtual;

    {:On stream sockets it returns number of received bytes waiting for picking.
     0 is returned when there is no such data. On datagram socket it returns
     length of the first waiting datagram. Returns 0 if no datagram is waiting.}
    Function WaitingData: Integer; Virtual;

    {:Same as @link(WaitingData), but if exists some of data in @link(Linebuffer),
     return their length instead.}
    Function WaitingDataEx: Integer;

    {:Clear all waiting data for read from buffers.}
    Procedure Purge;

    {:Sets linger. Enabled linger means that the system waits another LINGER
     (in milliseconds) time for delivery of sent data. This function is only for
     stream type of socket! (TCP)}
    Procedure SetLinger(Enable: Boolean; Linger: Integer);

    {:Actualize values in @link(LocalSin).}
    Procedure GetSinLocal;

    {:Actualize values in @link(RemoteSin).}
    Procedure GetSinRemote;

    {:Actualize values in @link(LocalSin) and @link(RemoteSin).}
    Procedure GetSins;

    {:Reset @link(LastError) and @link(LastErrorDesc) to non-error state.}
    Procedure ResetLastError;

    {:If you "manually" call Socket API functions, forward their return code as
     parameter to this function, which evaluates it, eventually calls
     GetLastError and found error code returns and stores to @link(LastError).}
    Function SockCheck(SockResult: Integer): Integer; Virtual;

    {:If @link(LastError) contains some error code and @link(RaiseExcept)
     property is @true, raise adequate exception.}
    Procedure ExceptCheck;

    {:Returns local computer name as numerical or symbolic value. It try get
     fully qualified domain name. Name is returned in the format acceptable by
     functions demanding IP as input parameter.}
    Function LocalName: String;

    {:Try resolve name to all possible IP address. i.e. If you pass as name
     result of @link(LocalName) method, you get all IP addresses used by local
     system.}
    Procedure ResolveNameToIP(Name: String; Const IPList: TStrings);

    {:Try resolve name to primary IP address. i.e. If you pass as name result of
     @link(LocalName) method, you get primary IP addresses used by local system.}
    Function ResolveName(Name: String): String;

    {:Try resolve IP to their primary domain name. If IP not have domain name,
     then is returned original IP.}
    Function ResolveIPToName(IP: String): String;

    {:Try resolve symbolic port name to port number. (i.e. 'Echo' to 8)}
    Function ResolvePort(Port: String): Word;

    {:Set information about remote side socket. It is good for seting remote
     side for sending UDP packet, etc.}
    Procedure SetRemoteSin(IP, Port: String);

    {:Picks IP socket address from @link(LocalSin).}
    Function GetLocalSinIP: String; Virtual;

    {:Picks IP socket address from @link(RemoteSin).}
    Function GetRemoteSinIP: String; Virtual;

    {:Picks socket PORT number from @link(LocalSin).}
    Function GetLocalSinPort: Integer; Virtual;

    {:Picks socket PORT number from @link(RemoteSin).}
    Function GetRemoteSinPort: Integer; Virtual;

    {:Return @TRUE, if you can read any data from socket or is incoming
     connection on TCP based socket. Status is tested for time Timeout (in
     milliseconds). If value in Timeout is 0, status is only tested and
     continue. If value in Timeout is -1, run is breaked and waiting for read
     data maybe forever.

     This function is need only on special cases, when you need use
     @link(RecvBuffer) function directly! read functioms what have timeout as
     calling parameter, calling this function internally.}
    Function CanRead(Timeout: Integer): Boolean; Virtual;

    {:Same as @link(CanRead), but additionally return @TRUE if is some data in
     @link(LineBuffer).}
    Function CanReadEx(Timeout: Integer): Boolean; Virtual;

    {:Return @TRUE, if you can to socket write any data (not full sending
     buffer). Status is tested for time Timeout (in milliseconds). If value in
     Timeout is 0, status is only tested and continue. If value in Timeout is
     -1, run is breaked and waiting for write data maybe forever.

     This function is need only on special cases!}
    Function CanWrite(Timeout: Integer): Boolean; Virtual;

    {:Same as @link(SendBuffer), but send datagram to address from
     @link(RemoteSin). Usefull for sending reply to datagram received by
     function @link(RecvBufferFrom).}
    Function SendBufferTo(Buffer: TMemory; Length: Integer): Integer; Virtual;

    {:Note: This is low-lever receive function. You must be sure if data is
     waiting for read before call this function for avoid deadlock!

     Receives first waiting datagram to allocated buffer. If there is no waiting
     one, then waits until one comes. Returns length of datagram stored in
     BUFFER. If length exceeds buffer datagram is truncated. After this
     @link(RemoteSin) structure contains information about sender of UDP packet.}
    Function RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer; Virtual;
    {$IFNDEF CIL}
    {:This function is for check for incoming data on set of sockets. Whitch
    sockets is checked is decribed by SocketList Tlist with TBlockSocket
    objects. TList may have maximal number of objects defined by FD_SETSIZE
    constant. Return @TRUE, if you can from some socket read any data or is
    incoming connection on TCP based socket. Status is tested for time Timeout
    (in milliseconds). If value in Timeout is 0, status is only tested and
    continue. If value in Timeout is -1, run is breaked and waiting for read
    data maybe forever. If is returned @TRUE, CanReadList TList is filled by all
    TBlockSocket objects what waiting for read.}
    Function GroupCanRead(Const SocketList: TList; Timeout: Integer;
      Const CanReadList: TList): Boolean;
    {$ENDIF}
    {:By this method you may turn address reuse mode for local @link(bind). It
     is good specially for UDP protocol. Using this with TCP protocol is
     hazardous!}
    Procedure EnableReuse(Value: Boolean);

    {:Try set timeout for all sending and receiving operations, if socket
     provider can do it. (It not supported by all socket providers!)}
    Procedure SetTimeout(Timeout: Integer);

    {:Try set timeout for all sending operations, if socket provider can do it.
     (It not supported by all socket providers!)}
    Procedure SetSendTimeout(Timeout: Integer);

    {:Try set timeout for all receiving operations, if socket provider can do
     it. (It not supported by all socket providers!)}
    Procedure SetRecvTimeout(Timeout: Integer);

    {:Return value of socket type.}
    Function GetSocketType: Integer; Virtual;

    {:Return value of protocol type for socket creation.}
    Function GetSocketProtocol: Integer; Virtual;

    {:WSA structure with information about socket provider. On non-windows 
     platforms this structure is simulated!}
    Property WSAData: TWSADATA read GetWsaData;

    {:FDset structure prepared for usage with this socket.}
    Property FDset: TFDSet read FFDset;

    {:Structure describing local socket side.}
    Property LocalSin: TVarSin read FLocalSin write FLocalSin;

    {:Structure describing remote socket side.}
    Property RemoteSin: TVarSin read FRemoteSin write FRemoteSin;

    {:Socket handler. Suitable for "manual" calls to socket API or manual
     connection of socket to a previously created socket (i.e by Accept method
     on TCP socket)}
    Property Socket: TSocket read FSocket write SetSocket;

    {:Last socket operation error code. Error codes are described in socket
     documentation. Human readable error description is stored in
     @link(LastErrorDesc) property.}
    Property LastError: Integer read FLastError;

    {:Human readable error description of @link(LastError) code.}
    Property LastErrorDesc: String read FLastErrorDesc;

    {:Buffer used by all high-level receiving functions. This buffer is used for
     optimized reading of data from socket. In normal cases you not need access
     to this buffer directly!}
    Property LineBuffer: Ansistring read FBuffer write FBuffer;

    {:Size of Winsock receive buffer. If it is not supported by socket provider,
     it return as size one kilobyte.}
    Property SizeRecvBuffer: Integer read GetSizeRecvBuffer write SetSizeRecvBuffer;

    {:Size of Winsock send buffer. If it is not supported by socket provider, it
     return as size one kilobyte.}
    Property SizeSendBuffer: Integer read GetSizeSendBuffer write SetSizeSendBuffer;

    {:If @True, turn class to non-blocking mode. Not all functions are working
     properly in this mode, you must know exactly what you are doing! However
     when you have big experience with non-blocking programming, then you can
     optimise your program by non-block mode!}
    Property NonBlockMode: Boolean read FNonBlockMode write SetNonBlockMode;

    {:Set Time-to-live value. (if system supporting it!)}
    Property TTL: Integer read GetTTL write SetTTL;

    {:If is @true, then class in in IPv6 mode.}
    Property IP6used: Boolean read FIP6used;

    {:Return count of received bytes on this socket from begin of current
     connection.}
    Property RecvCounter: Integer read FRecvCounter;

    {:Return count of sended bytes on this socket from begin of current
     connection.}
    Property SendCounter: Integer read FSendCounter;
  Published
    {:Return descriptive string for given error code. This is class function.
     You may call it without created object!}
    Class Function GetErrorDesc(ErrorCode: Integer): String;

    {:Return descriptive string for @link(LastError).}
    Function GetErrorDescEx: String; Virtual;

    {:this value is for free use.}
    Property Tag: Integer read FTag write FTag;

    {:If @true, winsock errors raises exception. Otherwise is setted
    @link(LastError) value only and you must check it from your program! Default
    value is @false.}
    Property RaiseExcept: Boolean read FRaiseExcept write FRaiseExcept;

    {:Define maximum length in bytes of @link(LineBuffer) for high-level
     receiving functions. If this functions try to read more data then this
     limit, error is returned! If value is 0 (default), no limitation is used.
     This is very good protection for stupid attacks to your server by sending
     lot of data without proper terminator... until all your memory is allocated
     by LineBuffer!

     Note: This maximum length is checked only in functions, what read unknown
     number of bytes! (like @link(RecvString) or @link(RecvTerminated))}
    Property MaxLineLength: Integer read FMaxLineLength write FMaxLineLength;

    {:Define maximal bandwidth for all sending operations in bytes per second.
     If value is 0 (default), bandwidth limitation is not used.}
    Property MaxSendBandwidth: Integer read FMaxSendBandwidth write FMaxSendBandwidth;

    {:Define maximal bandwidth for all receiving operations in bytes per second.
     If value is 0 (default), bandwidth limitation is not used.}
    Property MaxRecvBandwidth: Integer read FMaxRecvBandwidth write FMaxRecvBandwidth;

    {:Define maximal bandwidth for all sending and receiving operations in bytes
     per second. If value is 0 (default), bandwidth limitation is not used.}
    Property MaxBandwidth: Integer write SetBandwidth;

    {:Do a conversion of non-standard line terminators to CRLF. (Off by default)
     If @True, then terminators like sigle CR, single LF or LFCR are converted
     to CRLF internally. This have effect only in @link(RecvString) method!}
    Property ConvertLineEnd: Boolean read FConvertLineEnd write FConvertLineEnd;

    {:Specified Family of this socket. When you are using Windows preliminary
     support for IPv6, then I recommend to set this property!}
    Property Family: TSocketFamily read FFamily write SetFamily;

    {:When resolving of domain name return both IPv4 and IPv6 addresses, then
     specify if is used IPv4 (dafault - @true) or IPv6.}
    Property PreferIP4: Boolean read FPreferIP4 write FPreferIP4;

    {:By default (@true) is all timeouts used as timeout between two packets in
     reading operations. If you set this to @false, then Timeouts is for overall
     reading operation!}
    Property InterPacketTimeout: Boolean read FInterPacketTimeout
      write FInterPacketTimeout;

    {:All sended datas was splitted by this value.}
    Property SendMaxChunk: Integer read FSendMaxChunk write FSendMaxChunk;

    {:By setting this property to @true you can stop any communication. You can
     use this property for soft abort of communication.}
    Property StopFlag: Boolean read FStopFlag write FStopFlag;

    {:Timeout for data sending by non-blocking socket mode.}
    Property NonblockSendTimeout: Integer read FNonblockSendTimeout
      write FNonblockSendTimeout;

    {:Timeout for @link(Connect) call. Default value 0 means default system timeout.
     Non-zero value means timeout in millisecond.}
    Property ConnectionTimeout: Integer read FConnectionTimeout write FConnectionTimeout;

    {:This event is called by various reasons. It is good for monitoring socket,
     create gauges for data transfers, etc.}
    Property OnStatus: THookSocketStatus read FOnStatus write FOnStatus;

    {:this event is good for some internal thinks about filtering readed datas.
     It is used by telnet client by example.}
    Property OnReadFilter: THookDataFilter read FOnReadFilter write FOnReadFilter;

    {:This event is called after real socket creation for setting special socket
     options, because you not know when socket is created. (it is depended on
     Ipv4, IPv6 or automatic mode)}
    Property OnCreateSocket: THookCreateSocket
      read FOnCreateSocket write FOnCreateSocket;

    {:This event is good for monitoring content of readed or writed datas.}
    Property OnMonitor: THookMonitor read FOnMonitor write FOnMonitor;

    {:This event is good for calling your code during long socket operations.
      (Example, for refresing UI if class in not called within the thread.)
      Rate of heartbeats can be modified by @link(HeartbeatRate) property.}
    Property OnHeartbeat: THookHeartbeat read FOnHeartbeat write FOnHeartbeat;

    {:Specify typical rate of @link(OnHeartbeat) event and @link(StopFlag) testing.
      Default value 0 disabling heartbeats! Value is in milliseconds.
      Real rate can be higher or smaller then this value, because it depending
      on real socket operations too!
      Note: Each heartbeat slowing socket processing.}
    Property HeartbeatRate: Integer read FHeartbeatRate write FHeartbeatRate;
    {:What class own this socket? Used by protocol implementation classes.}
    Property Owner: TObject read FOwner write FOwner;
  End;

  {:@abstract(Support for SOCKS4 and SOCKS5 proxy)
   Layer with definition all necessary properties and functions for
   implementation SOCKS proxy client. Do not use this class directly.}
  TSocksBlockSocket = Class(TBlockSocket)
  Protected
    FSocksIP: String;
    FSocksPort: String;
    FSocksTimeout: Integer;
    FSocksUsername: String;
    FSocksPassword: String;
    FUsingSocks: Boolean;
    FSocksResolver: Boolean;
    FSocksLastError: Integer;
    FSocksResponseIP: String;
    FSocksResponsePort: String;
    FSocksLocalIP: String;
    FSocksLocalPort: String;
    FSocksRemoteIP: String;
    FSocksRemotePort: String;
    FBypassFlag: Boolean;
    FSocksType: TSocksType;
    Function SocksCode(IP, Port: String): Ansistring;
    Function SocksDecode(Value: Ansistring): Integer;
  Public
    Constructor Create;

    {:Open connection to SOCKS proxy and if @link(SocksUsername) is set, do
     authorisation to proxy. This is needed only in special cases! (it is called
     internally!)}
    Function SocksOpen: Boolean;

    {:Send specified request to SOCKS proxy. This is needed only in special
     cases! (it is called internally!)}
    Function SocksRequest(Cmd: Byte; Const IP, Port: String): Boolean;

    {:Receive response to previosly sended request. This is needed only in
     special cases! (it is called internally!)}
    Function SocksResponse: Boolean;

    {:Is @True when class is using SOCKS proxy.}
    Property UsingSocks: Boolean read FUsingSocks;

    {:If SOCKS proxy failed, here is error code returned from SOCKS proxy.}
    Property SocksLastError: Integer read FSocksLastError;
  Published
    {:Address of SOCKS server. If value is empty string, SOCKS support is
     disabled. Assingning any value to this property enable SOCKS mode.
     Warning: You cannot combine this mode with HTTP-tunneling mode!}
    Property SocksIP: String read FSocksIP write FSocksIP;

    {:Port of SOCKS server. Default value is '1080'.}
    Property SocksPort: String read FSocksPort write FSocksPort;

    {:If you need authorisation on SOCKS server, set username here.}
    Property SocksUsername: String read FSocksUsername write FSocksUsername;

    {:If you need authorisation on SOCKS server, set password here.}
    Property SocksPassword: String read FSocksPassword write FSocksPassword;

    {:Specify timeout for communicatin with SOCKS server. Default is one minute.}
    Property SocksTimeout: Integer read FSocksTimeout write FSocksTimeout;

    {:If @True, all symbolic names of target hosts is not translated to IP's
     locally, but resolving is by SOCKS proxy. Default is @True.}
    Property SocksResolver: Boolean read FSocksResolver write FSocksResolver;

    {:Specify SOCKS type. By default is used SOCKS5, but you can use SOCKS4 too.
     When you select SOCKS4, then if @link(SOCKSResolver) is enabled, then is
     used SOCKS4a. Othervise is used pure SOCKS4.}
    Property SocksType: TSocksType read FSocksType write FSocksType;
  End;

  {:@abstract(Implementation of TCP socket.)
   Supported features: IPv4, IPv6, SSL/TLS or SSH (depending on used plugin),
   SOCKS5 proxy (outgoing connections and limited incomming), SOCKS4/4a proxy
   (outgoing connections and limited incomming), TCP through HTTP proxy tunnel.}
  TTCPBlockSocket = Class(TSocksBlockSocket)
  Protected
    FOnAfterConnect: THookAfterConnect;
    FSSL: TCustomSSL;
    FHTTPTunnelIP: String;
    FHTTPTunnelPort: String;
    FHTTPTunnel: Boolean;
    FHTTPTunnelRemoteIP: String;
    FHTTPTunnelRemotePort: String;
    FHTTPTunnelUser: String;
    FHTTPTunnelPass: String;
    FHTTPTunnelTimeout: Integer;
    Procedure SocksDoConnect(IP, Port: String);
    Procedure HTTPTunnelDoConnect(IP, Port: String);
    Procedure DoAfterConnect;
  Public
    {:Create TCP socket class with default plugin for SSL/TSL/SSH implementation
    (see @link(SSLImplementation))}
    Constructor Create;

    {:Create TCP socket class with desired plugin for SSL/TSL/SSH implementation}
    Constructor CreateWithSSL(SSLPlugin: TSSLClass);
    Destructor Destroy; Override;

    {:See @link(TBlockSocket.CloseSocket)}
    Procedure CloseSocket; Override;

    {:See @link(TBlockSocket.WaitingData)}
    Function WaitingData: Integer; Override;

    {:Sets socket to receive mode for new incoming connections. It is necessary
     to use @link(TBlockSocket.BIND) function call before this method to select
     receiving port!

     If you use SOCKS, activate incoming TCP connection by this proxy. (By BIND
     method of SOCKS.)}
    Procedure Listen; Override;

    {:Waits until new incoming connection comes. After it comes a new socket is
     automatically created (socket handler is returned by this function as
     result).

     If you use SOCKS, new socket is not created! In this case is used same
     socket as socket for listening! So, you can accept only one connection in
     SOCKS mode.}
    Function Accept: TSocket; Override;

    {:Connects socket to remote IP address and PORT. The same rules as with
     @link(TBlockSocket.BIND) method are valid. The only exception is that PORT
     with 0 value will not be connected. After call to this method
     a communication channel between local and remote socket is created. Local
     socket is assigned automatically if not controlled by previous call to
     @link(TBlockSocket.BIND) method. Structures @link(TBlockSocket.LocalSin)
     and @link(TBlockSocket.RemoteSin) will be filled with valid values.

     If you use SOCKS, activate outgoing TCP connection by SOCKS proxy specified
     in @link(TSocksBlockSocket.SocksIP). (By CONNECT method of SOCKS.)

     If you use HTTP-tunnel mode, activate outgoing TCP connection by HTTP
     tunnel specified in @link(HTTPTunnelIP). (By CONNECT method of HTTP
     protocol.)

     Note: If you call this on non-created socket, then socket is created
     automaticly.}
    Procedure Connect(IP, Port: String); Override;

    {:If you need upgrade existing TCP connection to SSL/TLS (or SSH2, if plugin
     allows it) mode, then call this method. This method switch this class to
     SSL mode and do SSL/TSL handshake.}
    Procedure SSLDoConnect;

    {:By this method you can downgrade existing SSL/TLS connection to normal TCP
     connection.}
    Procedure SSLDoShutdown;

    {:If you need use this component as SSL/TLS TCP server, then after accepting
     of inbound connection you need start SSL/TLS session by this method. Before
     call this function, you must have assigned all neeeded certificates and
     keys!}
    Function SSLAcceptConnection: Boolean;

    {:See @link(TBlockSocket.GetLocalSinIP)}
    Function GetLocalSinIP: String; Override;

    {:See @link(TBlockSocket.GetRemoteSinIP)}
    Function GetRemoteSinIP: String; Override;

    {:See @link(TBlockSocket.GetLocalSinPort)}
    Function GetLocalSinPort: Integer; Override;

    {:See @link(TBlockSocket.GetRemoteSinPort)}
    Function GetRemoteSinPort: Integer; Override;

    {:See @link(TBlockSocket.SendBuffer)}
    Function SendBuffer(Buffer: TMemory; Length: Integer): Integer; Override;

    {:See @link(TBlockSocket.RecvBuffer)}
    Function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; Override;

    {:Return value of socket type. For TCP return SOCK_STREAM.}
    Function GetSocketType: Integer; Override;

    {:Return value of protocol type for socket creation. For TCP return
     IPPROTO_TCP.}
    Function GetSocketProtocol: Integer; Override;

    {:Class implementing SSL/TLS support. It is allways some descendant
     of @link(TCustomSSL) class. When programmer not select some SSL plugin
     class, then is used @link(TSSLNone)}
    Property SSL: TCustomSSL read FSSL;

    {:@True if is used HTTP tunnel mode.}
    Property HTTPTunnel: Boolean read FHTTPTunnel;
  Published
    {:Return descriptive string for @link(LastError). On case of error
     in SSL/TLS subsystem, it returns right error description.}
    Function GetErrorDescEx: String; Override;

    {:Specify IP address of HTTP proxy. Assingning non-empty value to this
     property enable HTTP-tunnel mode. This mode is for tunnelling any outgoing
     TCP connection through HTTP proxy server. (If policy on HTTP proxy server
     allow this!) Warning: You cannot combine this mode with SOCK5 mode!}
    Property HTTPTunnelIP: String read FHTTPTunnelIP write FHTTPTunnelIP;

    {:Specify port of HTTP proxy for HTTP-tunneling.}
    Property HTTPTunnelPort: String read FHTTPTunnelPort write FHTTPTunnelPort;

    {:Specify authorisation username for access to HTTP proxy in HTTP-tunnel
     mode. If you not need authorisation, then let this property empty.}
    Property HTTPTunnelUser: String read FHTTPTunnelUser write FHTTPTunnelUser;

    {:Specify authorisation password for access to HTTP proxy in HTTP-tunnel
     mode.}
    Property HTTPTunnelPass: String read FHTTPTunnelPass write FHTTPTunnelPass;

    {:Specify timeout for communication with HTTP proxy in HTTPtunnel mode.}
    Property HTTPTunnelTimeout: Integer read FHTTPTunnelTimeout write FHTTPTunnelTimeout;

    {:This event is called after sucessful TCP socket connection.}
    Property OnAfterConnect: THookAfterConnect
      read FOnAfterConnect write FOnAfterConnect;
  End;

  {:@abstract(Datagram based communication)
   This class implementing datagram based communication instead default stream
   based communication style.}
  TDgramBlockSocket = Class(TSocksBlockSocket)
  Public
    {:Fill @link(TBlockSocket.RemoteSin) structure. This address is used for
     sending data.}
    Procedure Connect(IP, Port: String); Override;

    {:Silently redirected to @link(TBlockSocket.SendBufferTo).}
    Function SendBuffer(Buffer: TMemory; Length: Integer): Integer; Override;

    {:Silently redirected to @link(TBlockSocket.RecvBufferFrom).}
    Function RecvBuffer(Buffer: TMemory; Length: Integer): Integer; Override;
  End;

  {:@abstract(Implementation of UDP socket.)
   NOTE: in this class is all receiving redirected to RecvBufferFrom. You can
   use for reading any receive function. Preffered is RecvPacket! Similary all
   sending is redirected to SendbufferTo. You can use for sending UDP packet any
   sending function, like SendString.

   Supported features: IPv4, IPv6, unicasts, broadcasts, multicasts, SOCKS5
   proxy (only unicasts! Outgoing and incomming.)}
  TUDPBlockSocket = Class(TDgramBlockSocket)
  Protected
    FSocksControlSock: TTCPBlockSocket;
    Function UdpAssociation: Boolean;
    Procedure SetMulticastTTL(TTL: Integer);
    Function GetMulticastTTL: Integer;
  Public
    Destructor Destroy; Override;

    {:Enable or disable sending of broadcasts. If seting OK, result is @true.
     This method is not supported in SOCKS5 mode! IPv6 does not support
     broadcasts! In this case you must use Multicasts instead.}
    Procedure EnableBroadcast(Value: Boolean);

    {:See @link(TBlockSocket.SendBufferTo)}
    Function SendBufferTo(Buffer: TMemory; Length: Integer): Integer; Override;

    {:See @link(TBlockSocket.RecvBufferFrom)}
    Function RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer; Override;
    {$IFNDEF CIL}
    {:Add this socket to given multicast group. You cannot use Multicasts in
     SOCKS mode!}
    Procedure AddMulticast(MCastIP: String);

    {:Remove this socket from given multicast group.}
    Procedure DropMulticast(MCastIP: String);
    {$ENDIF}
    {:All sended multicast datagrams is loopbacked to your interface too. (you
     can read your sended datas.) You can disable this feature by this function.
     This function not working on some Windows systems!}
    Procedure EnableMulticastLoop(Value: Boolean);

    {:Return value of socket type. For UDP return SOCK_DGRAM.}
    Function GetSocketType: Integer; Override;

    {:Return value of protocol type for socket creation. For UDP return
     IPPROTO_UDP.}
    Function GetSocketProtocol: Integer; Override;

    {:Set Time-to-live value for multicasts packets. It define number of routers
     for transfer of datas. If you set this to 1 (dafault system value), then
     multicasts packet goes only to you local network. If you need transport
     multicast packet to worldwide, then increase this value, but be carefull,
     lot of routers on internet does not transport multicasts packets!}
    Property MulticastTTL: Integer read GetMulticastTTL write SetMulticastTTL;
  End;

  {:@abstract(Implementation of RAW ICMP socket.)
   For this object you must have rights for creating RAW sockets!}
  TICMPBlockSocket = Class(TDgramBlockSocket)
  Public
    {:Return value of socket type. For RAW and ICMP return SOCK_RAW.}
    Function GetSocketType: Integer; Override;

    {:Return value of protocol type for socket creation. For ICMP returns
     IPPROTO_ICMP or IPPROTO_ICMPV6}
    Function GetSocketProtocol: Integer; Override;
  End;

  {:@abstract(Implementation of RAW socket.)
   For this object you must have rights for creating RAW sockets!}
  TRAWBlockSocket = Class(TBlockSocket)
  Public
    {:Return value of socket type. For RAW and ICMP return SOCK_RAW.}
    Function GetSocketType: Integer; Override;

    {:Return value of protocol type for socket creation. For RAW returns
     IPPROTO_RAW.}
    Function GetSocketProtocol: Integer; Override;
  End;

  {:@abstract(Implementation of PGM-message socket.)
   Not all systems supports this protocol!}
  TPGMMessageBlockSocket = Class(TBlockSocket)
  Public
    {:Return value of socket type. For PGM-message return SOCK_RDM.}
    Function GetSocketType: Integer; Override;

    {:Return value of protocol type for socket creation. For PGM-message returns
     IPPROTO_RM.}
    Function GetSocketProtocol: Integer; Override;
  End;

  {:@abstract(Implementation of PGM-stream socket.)
   Not all systems supports this protocol!}
  TPGMStreamBlockSocket = Class(TBlockSocket)
  Public
    {:Return value of socket type. For PGM-stream return SOCK_STREAM.}
    Function GetSocketType: Integer; Override;

    {:Return value of protocol type for socket creation. For PGM-stream returns
     IPPROTO_RM.}
    Function GetSocketProtocol: Integer; Override;
  End;

  {:@abstract(Parent class for all SSL plugins.)
   This is abstract class defining interface for other SSL plugins.

   Instance of this class will be created for each @link(TTCPBlockSocket).

   Warning: not all methods and propertis can work in all existing SSL plugins!
   Please, read documentation of used SSL plugin.}
  TCustomSSL = Class(TObject)
  Private
  Protected
    FOnVerifyCert: THookVerifyCert;
    FSocket: TTCPBlockSocket;
    FSSLEnabled: Boolean;
    FLastError: Integer;
    FLastErrorDesc: String;
    FSSLType: TSSLType;
    FKeyPassword: String;
    FCiphers: String;
    FCertificateFile: String;
    FPrivateKeyFile: String;
    FCertificate: Ansistring;
    FPrivateKey: Ansistring;
    FPFX: Ansistring;
    FPFXfile: String;
    FCertCA: Ansistring;
    FCertCAFile: String;
    FTrustCertificate: Ansistring;
    FTrustCertificateFile: String;
    FVerifyCert: Boolean;
    FUsername: String;
    FPassword: String;
    FSSHChannelType: String;
    FSSHChannelArg1: String;
    FSSHChannelArg2: String;
    FCertComplianceLevel: Integer;
    FSNIHost: String;
    Procedure ReturnError;
    Procedure SetCertCAFile(Const Value: String); Virtual;
    Function DoVerifyCert: Boolean;
    Function CreateSelfSignedCert(Host: String): Boolean; Virtual;
  Public
    {: Create plugin class. it is called internally from @link(TTCPBlockSocket)}
    Constructor Create(Const Value: TTCPBlockSocket); Virtual;

    {: Assign settings (certificates and configuration) from another SSL plugin
     class.}
    Procedure Assign(Const Value: TCustomSSL); Virtual;

    {: return description of used plugin. It usually return name and version
     of used SSL library.}
    Function LibVersion: String; Virtual;

    {: return name of used plugin.}
    Function LibName: String; Virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for start SSL connection.}
    Function Connect: Boolean; Virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for acept new SSL connection.}
    Function Accept: Boolean; Virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for hard shutdown of SSL connection. (for example,
     before socket is closed)}
    Function Shutdown: Boolean; Virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for soft shutdown of SSL connection. (for example,
     when you need to continue with unprotected connection.)}
    Function BiShutdown: Boolean; Virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for sending some datas by SSL connection.}
    Function SendBuffer(Buffer: TMemory; Len: Integer): Integer; Virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for receiving some datas by SSL connection.}
    Function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; Virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for getting count of datas what waiting for read.
     If SSL plugin not allows this, then it should return 0.}
    Function WaitingData: Integer; Virtual;

    {:Return string with identificator of SSL/TLS version of existing
     connection.}
    Function GetSSLVersion: String; Virtual;

    {:Return subject of remote SSL peer.}
    Function GetPeerSubject: String; Virtual;

    {:Return Serial number if remote X509 certificate.}
    Function GetPeerSerialNo: Integer; Virtual;

    {:Return issuer certificate of remote SSL peer.}
    Function GetPeerIssuer: String; Virtual;

    {:Return peer name from remote side certificate. This is good for verify,
     if certificate is generated for remote side IP name.}
    Function GetPeerName: String; Virtual;

    {:Returns has of peer name from remote side certificate. This is good
     for fast remote side authentication.}
    Function GetPeerNameHash: Cardinal; Virtual;

    {:Return fingerprint of remote SSL peer.}
    Function GetPeerFingerprint: String; Virtual;

    {:Return all detailed information about certificate from remote side of
     SSL/TLS connection. Result string can be multilined! Each plugin can return
     this informations in different format!}
    Function GetCertInfo: String; Virtual;

    {:Return currently used Cipher.}
    Function GetCipherName: String; Virtual;

    {:Return currently used number of bits in current Cipher algorythm.}
    Function GetCipherBits: Integer; Virtual;

    {:Return number of bits in current Cipher algorythm.}
    Function GetCipherAlgBits: Integer; Virtual;

    {:Return result value of verify remote side certificate. Look to OpenSSL
     documentation for possible values. For example 0 is successfuly verified
     certificate, or 18 is self-signed certificate.}
    Function GetVerifyCert: Integer; Virtual;

    {: Resurn @true if SSL mode is enabled on existing cvonnection.}
    Property SSLEnabled: Boolean read FSSLEnabled;

    {:Return error code of last SSL operation. 0 is OK.}
    Property LastError: Integer read FLastError;

    {:Return error description of last SSL operation.}
    Property LastErrorDesc: String read FLastErrorDesc;
  Published
    {:Here you can specify requested SSL/TLS mode. Default is autodetection, but
     on some servers autodetection not working properly. In this case you must
     specify requested SSL/TLS mode by your hand!}
    Property SSLType: TSSLType read FSSLType write FSSLType;

    {:Password for decrypting of encoded certificate or key.}
    Property KeyPassword: String read FKeyPassword write FKeyPassword;

    {:Username for possible credentials.}
    Property Username: String read FUsername write FUsername;

    {:password for possible credentials.}
    Property Password: String read FPassword write FPassword;

    {:By this property you can modify default set of SSL/TLS ciphers.}
    Property Ciphers: String read FCiphers write FCiphers;

    {:Used for loading certificate from disk file. See to plugin documentation
     if this method is supported and how!}
    Property CertificateFile: String read FCertificateFile write FCertificateFile;

    {:Used for loading private key from disk file. See to plugin documentation
     if this method is supported and how!}
    Property PrivateKeyFile: String read FPrivateKeyFile write FPrivateKeyFile;

    {:Used for loading certificate from binary string. See to plugin documentation
     if this method is supported and how!}
    Property Certificate: Ansistring read FCertificate write FCertificate;

    {:Used for loading private key from binary string. See to plugin documentation
     if this method is supported and how!}
    Property PrivateKey: Ansistring read FPrivateKey write FPrivateKey;

    {:Used for loading PFX from binary string. See to plugin documentation
     if this method is supported and how!}
    Property PFX: Ansistring read FPFX write FPFX;

    {:Used for loading PFX from disk file. See to plugin documentation
     if this method is supported and how!}
    Property PFXfile: String read FPFXfile write FPFXfile;

    {:Used for loading trusted certificates from disk file. See to plugin documentation
     if this method is supported and how!}
    Property TrustCertificateFile: String read FTrustCertificateFile
      write FTrustCertificateFile;

    {:Used for loading trusted certificates from binary string. See to plugin documentation
     if this method is supported and how!}
    Property TrustCertificate: Ansistring read FTrustCertificate write FTrustCertificate;

    {:Used for loading CA certificates from binary string. See to plugin documentation
     if this method is supported and how!}
    Property CertCA: Ansistring read FCertCA write FCertCA;

    {:Used for loading CA certificates from disk file. See to plugin documentation
     if this method is supported and how!}
    Property CertCAFile: String read FCertCAFile write SetCertCAFile;

    {:If @true, then is verified client certificate. (it is good for writing
     SSL/TLS servers.) When you are not server, but you are client, then if this
     property is @true, verify servers certificate.}
    Property VerifyCert: Boolean read FVerifyCert write FVerifyCert;

    {:channel type for possible SSH connections}
    Property SSHChannelType: String read FSSHChannelType write FSSHChannelType;

    {:First argument of channel type for possible SSH connections}
    Property SSHChannelArg1: String read FSSHChannelArg1 write FSSHChannelArg1;

    {:Second argument of channel type for possible SSH connections}
    Property SSHChannelArg2: String read FSSHChannelArg2 write FSSHChannelArg2;

    {: Level of standards compliance level
      (CryptLib: values in cryptlib.pas, -1: use default value )  }
    Property CertComplianceLevel: Integer read FCertComplianceLevel
      write FCertComplianceLevel;

    {:This event is called when verifying the server certificate immediatally after
     a successfull verification in the ssl library.}
    Property OnVerifyCert: THookVerifyCert read FOnVerifyCert write FOnVerifyCert;

    {: Server Name Identification. Host name to send to server. If empty the host name
       found in URL will be used, which should be the normal use (http Header Host = SNI Host).
       The value is cleared after the connection is established.
      (SNI support requires OpenSSL 0.9.8k or later. Cryptlib not supported, yet )  }
    Property SNIHost: String read FSNIHost write FSNIHost;
  End;

  {:@abstract(Default SSL plugin with no SSL support.)
   Dummy SSL plugin implementation for applications without SSL/TLS support.}
  TSSLNone = Class(TCustomSSL)
  Public
    {:See @inherited}
    Function LibVersion: String; Override;
    {:See @inherited}
    Function LibName: String; Override;
  End;

  {:@abstract(Record with definition of IP packet header.)
   For reading data from ICMP or RAW sockets.}
  TIPHeader = Record
    VerLen: Byte;
    TOS: Byte;
    TotalLen: Word;
    Identifer: Word;
    FragOffsets: Word;
    TTL: Byte;
    Protocol: Byte;
    CheckSum: Word;
    SourceIp: Longword;
    DestIp: Longword;
    Options: Longword;
  End;

  {:@abstract(Parent class of application protocol implementations.)
   By this class is defined common properties.}
  TSynaClient = Class(TObject)
  Protected
    FTargetHost: String;
    FTargetPort: String;
    FIPInterface: String;
    FTimeout: Integer;
    FUserName: String;
    FPassword: String;
  Public
    Constructor Create;
  Published
    {:Specify terget server IP (or symbolic name). Default is 'localhost'.}
    Property TargetHost: String read FTargetHost write FTargetHost;

    {:Specify terget server port (or symbolic name).}
    Property TargetPort: String read FTargetPort write FTargetPort;

    {:Defined local socket address. (outgoing IP address). By default is used
     '0.0.0.0' as wildcard for default IP.}
    Property IPInterface: String read FIPInterface write FIPInterface;

    {:Specify default timeout for socket operations.}
    Property Timeout: Integer read FTimeout write FTimeout;

    {:If protocol need user authorization, then fill here username.}
    Property UserName: String read FUserName write FUserName;

    {:If protocol need user authorization, then fill here password.}
    Property Password: String read FPassword write FPassword;
  End;

Var
  {:Selected SSL plugin. Default is @link(TSSLNone).

   Do not change this value directly!!!

   Just add your plugin unit to your project uses instead. Each plugin unit have
   initialization code what modify this variable.}
  SSLImplementation: TSSLClass = TSSLNone;

Implementation

{$IFDEF ONCEWINSOCK}
Var
  WsaDataOnce: TWSADATA;
  e: ESynapseError;
  {$ENDIF}


Constructor TBlockSocket.Create;
Begin
  CreateAlternate('');
End;

Constructor TBlockSocket.CreateAlternate(Stub: String);
  {$IFNDEF ONCEWINSOCK}
var
  e: ESynapseError;
  {$ENDIF}
Begin
  Inherited Create;
  FDelayedOptions := TList.Create;
  FRaiseExcept := False;
  {$IFDEF RAISEEXCEPT}
  FRaiseExcept := True;
  {$ENDIF}
  FSocket := INVALID_SOCKET;
  FBuffer := '';
  FLastCR := False;
  FLastLF := False;
  FBinded := False;
  FNonBlockMode := False;
  FMaxLineLength := 0;
  FMaxSendBandwidth := 0;
  FNextSend := 0;
  FMaxRecvBandwidth := 0;
  FNextRecv := 0;
  FConvertLineEnd := False;
  FFamily := SF_Any;
  FFamilySave := SF_Any;
  FIP6used := False;
  FPreferIP4 := True;
  FInterPacketTimeout := True;
  FRecvCounter := 0;
  FSendCounter := 0;
  FSendMaxChunk := c64k;
  FStopFlag := False;
  FNonblockSendTimeout := 15000;
  FHeartbeatRate := 0;
  FConnectionTimeout := 0;
  FOwner := nil;
  {$IFNDEF ONCEWINSOCK}
  if Stub = '' then
    Stub := DLLStackName;
  if not InitSocketInterface(Stub) then
  begin
    e := ESynapseError.Create('Error loading Socket interface (' + Stub + ')!');
    e.ErrorCode := 0;
    e.ErrorMessage := 'Error loading Socket interface (' + Stub + ')!';
    raise e;
  end;
  SockCheck(synsock.WSAStartup(WinsockLevel, FWsaDataOnce));
  ExceptCheck;
  {$ENDIF}
End;

Destructor TBlockSocket.Destroy;
Var
  n: Integer;
  p: TSynaOption;
Begin
  CloseSocket;
  {$IFNDEF ONCEWINSOCK}
  synsock.WSACleanup;
  DestroySocketInterface;
  {$ENDIF}
  For n := FDelayedOptions.Count - 1 Downto 0 Do
  Begin
    p := TSynaOption(FDelayedOptions[n]);
    p.Free;
  End;
  FDelayedOptions.Free;
  Inherited Destroy;
End;

Function TBlockSocket.FamilyToAF(f: TSocketFamily): TAddrFamily;
Begin
  Case f Of
    SF_ip4:
      Result := AF_INET;
    SF_ip6:
      Result := AF_INET6;
    Else
      Result := AF_UNSPEC;
  End;
End;

Procedure TBlockSocket.SetDelayedOption(Const Value: TSynaOption);
Var
  li: TLinger;
  x: Integer;
  buf: TMemory;
  {$IFNDEF MSWINDOWS}
  timeval: TTimeval;
  {$ENDIF}
Begin
  Case Value.Option Of
    SOT_Linger:
    Begin
      {$IFDEF CIL}
        li := TLinger.Create(Value.Enabled, Value.Value div 1000);
        synsock.SetSockOptObj(FSocket, integer(SOL_SOCKET), integer(SO_LINGER), li);
      {$ELSE}
      li.l_onoff := Ord(Value.Enabled);
      li.l_linger := Value.Value Div 1000;
      buf := @li;
      synsock.SetSockOpt(FSocket, Integer(SOL_SOCKET), Integer(SO_LINGER),
        buf, SizeOf(li));
      {$ENDIF}
    End;
    SOT_RecvBuff:
    Begin
      {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
      {$ELSE}
      buf := @Value.Value;
      {$ENDIF}
      synsock.SetSockOpt(FSocket, Integer(SOL_SOCKET), Integer(SO_RCVBUF),
        buf, SizeOf(Value.Value));
    End;
    SOT_SendBuff:
    Begin
      {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
      {$ELSE}
      buf := @Value.Value;
      {$ENDIF}
      synsock.SetSockOpt(FSocket, Integer(SOL_SOCKET), Integer(SO_SNDBUF),
        buf, SizeOf(Value.Value));
    End;
    SOT_NonBlock:
    Begin
      FNonBlockMode := Value.Enabled;
      x := Ord(FNonBlockMode);
      synsock.IoctlSocket(FSocket, FIONBIO, x);
    End;
    SOT_RecvTimeout:
    Begin
      {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_RCVTIMEO),
          buf, SizeOf(Value.Value));
      {$ELSE}
      {$IFDEF MSWINDOWS}
      buf := @Value.Value;
      synsock.SetSockOpt(FSocket, Integer(SOL_SOCKET), Integer(SO_RCVTIMEO),
        buf, SizeOf(Value.Value));
      {$ELSE}
        timeval.tv_sec:=Value.Value div 1000;
        timeval.tv_usec:=(Value.Value mod 1000) * 1000;
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_RCVTIMEO),
          @timeval, SizeOf(timeval));
      {$ENDIF}
      {$ENDIF}
    End;
    SOT_SendTimeout:
    Begin
      {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
      {$ELSE}
      {$IFDEF MSWINDOWS}
      buf := @Value.Value;
      synsock.SetSockOpt(FSocket, Integer(SOL_SOCKET), Integer(SO_SNDTIMEO),
        buf, SizeOf(Value.Value));
      {$ELSE}
        timeval.tv_sec:=Value.Value div 1000;
        timeval.tv_usec:=(Value.Value mod 1000) * 1000;
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_SNDTIMEO),
          @timeval, SizeOf(timeval));
      {$ENDIF}
      {$ENDIF}
    End;
    SOT_Reuse:
    Begin
      x := Ord(Value.Enabled);
      {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(x);
      {$ELSE}
      buf := @x;
      {$ENDIF}
      synsock.SetSockOpt(FSocket, Integer(SOL_SOCKET), Integer(SO_REUSEADDR),
        buf, SizeOf(x));
    End;
    SOT_TTL:
    Begin
      {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
      {$ELSE}
      buf := @Value.Value;
      {$ENDIF}
      If FIP6Used Then
        synsock.SetSockOpt(FSocket, Integer(IPPROTO_IPV6), Integer(IPV6_UNICAST_HOPS),
          buf, SizeOf(Value.Value))
      Else
        synsock.SetSockOpt(FSocket, Integer(IPPROTO_IP), Integer(IP_TTL),
          buf, SizeOf(Value.Value));
    End;
    SOT_Broadcast:
    Begin
      //#todo1 broadcasty na IP6
      x := Ord(Value.Enabled);
      {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(x);
      {$ELSE}
      buf := @x;
      {$ENDIF}
      synsock.SetSockOpt(FSocket, Integer(SOL_SOCKET), Integer(SO_BROADCAST),
        buf, SizeOf(x));
    End;
    SOT_MulticastTTL:
    Begin
      {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
      {$ELSE}
      buf := @Value.Value;
      {$ENDIF}
      If FIP6Used Then
        synsock.SetSockOpt(FSocket, Integer(IPPROTO_IPV6),
          Integer(IPV6_MULTICAST_HOPS),
          buf, SizeOf(Value.Value))
      Else
        synsock.SetSockOpt(FSocket, Integer(IPPROTO_IP), Integer(IP_MULTICAST_TTL),
          buf, SizeOf(Value.Value));
    End;
    SOT_MulticastLoop:
    Begin
      x := Ord(Value.Enabled);
      {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(x);
      {$ELSE}
      buf := @x;
      {$ENDIF}
      If FIP6Used Then
        synsock.SetSockOpt(FSocket, Integer(IPPROTO_IPV6),
          Integer(IPV6_MULTICAST_LOOP), buf, SizeOf(x))
      Else
        synsock.SetSockOpt(FSocket, Integer(IPPROTO_IP),
          Integer(IP_MULTICAST_LOOP), buf, SizeOf(x));
    End;
  End;
  Value.Free;
End;

Procedure TBlockSocket.DelayedOption(Const Value: TSynaOption);
Begin
  If FSocket = INVALID_SOCKET Then
  Begin
    FDelayedOptions.Insert(0, Value);
  End
  Else
    SetDelayedOption(Value);
End;

Procedure TBlockSocket.ProcessDelayedOptions;
Var
  n: Integer;
  d: TSynaOption;
Begin
  For n := FDelayedOptions.Count - 1 Downto 0 Do
  Begin
    d := TSynaOption(FDelayedOptions[n]);
    SetDelayedOption(d);
  End;
  FDelayedOptions.Clear;
End;

Procedure TBlockSocket.SetSin(Var Sin: TVarSin; IP, Port: String);
Var
  f: TSocketFamily;
Begin
  DoStatus(HR_ResolvingBegin, IP + ':' + Port);
  ResetLastError;
  //if socket exists, then use their type, else use users selection
  f := SF_Any;
  If (FSocket = INVALID_SOCKET) And (FFamily = SF_any) Then
  Begin
    If IsIP(IP) Then
      f := SF_IP4
    Else
      If IsIP6(IP) Then
        f := SF_IP6;
  End
  Else
    f := FFamily;
  FLastError := synsock.SetVarSin(sin, ip, port, FamilyToAF(f),
    GetSocketprotocol, GetSocketType, FPreferIP4);
  DoStatus(HR_ResolvingEnd, GetSinIP(sin) + ':' + IntToStr(GetSinPort(sin)));
End;

Function TBlockSocket.GetSinIP(Sin: TVarSin): String;
Begin
  Result := synsock.GetSinIP(sin);
End;

Function TBlockSocket.GetSinPort(Sin: TVarSin): Integer;
Begin
  Result := synsock.GetSinPort(sin);
End;

Procedure TBlockSocket.CreateSocket;
Var
  sin: TVarSin;
Begin
  //dummy for SF_Any Family mode
  ResetLastError;
  If (FFamily <> SF_Any) And (FSocket = INVALID_SOCKET) Then
  Begin
    {$IFDEF CIL}
    if FFamily = SF_IP6 then
      sin := TVarSin.Create(IPAddress.Parse('::0'), 0)
    else
      sin := TVarSin.Create(IPAddress.Parse('0.0.0.0'), 0);
    {$ELSE}
    FillChar(Sin, Sizeof(Sin), 0);
    If FFamily = SF_IP6 Then
      sin.sin_family := AF_INET6
    Else
      sin.sin_family := AF_INET;
    {$ENDIF}
    InternalCreateSocket(Sin);
  End;
End;

Procedure TBlockSocket.CreateSocketByName(Const Value: String);
Var
  sin: TVarSin;
Begin
  ResetLastError;
  If FSocket = INVALID_SOCKET Then
  Begin
    SetSin(sin, Value, '0');
    If FLastError = 0 Then
      InternalCreateSocket(Sin);
  End;
End;

Procedure TBlockSocket.InternalCreateSocket(Sin: TVarSin);
Begin
  FStopFlag := False;
  FRecvCounter := 0;
  FSendCounter := 0;
  ResetLastError;
  If FSocket = INVALID_SOCKET Then
  Begin
    FBuffer := '';
    FBinded := False;
    FIP6Used := Sin.AddressFamily = AF_INET6;
    FSocket := synsock.Socket(Integer(Sin.AddressFamily), GetSocketType,
      GetSocketProtocol);
    If FSocket = INVALID_SOCKET Then
      FLastError := synsock.WSAGetLastError;
    {$IFNDEF CIL}
    FD_ZERO(FFDSet);
    FD_SET(FSocket, FFDSet);
    {$ENDIF}
    ExceptCheck;
    If FIP6used Then
      DoStatus(HR_SocketCreate, 'IPv6')
    Else
      DoStatus(HR_SocketCreate, 'IPv4');
    ProcessDelayedOptions;
    DoCreateSocket;
  End;
End;

Procedure TBlockSocket.CloseSocket;
Begin
  AbortSocket;
End;

Procedure TBlockSocket.AbortSocket;
Var
  n: Integer;
  p: TSynaOption;
Begin
  If FSocket <> INVALID_SOCKET Then
    synsock.CloseSocket(FSocket);
  FSocket := INVALID_SOCKET;
  For n := FDelayedOptions.Count - 1 Downto 0 Do
  Begin
    p := TSynaOption(FDelayedOptions[n]);
    p.Free;
  End;
  FDelayedOptions.Clear;
  FFamily := FFamilySave;
  DoStatus(HR_SocketClose, '');
End;

Procedure TBlockSocket.Bind(IP, Port: String);
Var
  Sin: TVarSin;
Begin
  ResetLastError;
  If (FSocket <> INVALID_SOCKET) Or Not ((FFamily = SF_ANY) And
    (IP = cAnyHost) And (Port = cAnyPort)) Then
  Begin
    SetSin(Sin, IP, Port);
    If FLastError = 0 Then
    Begin
      If FSocket = INVALID_SOCKET Then
        InternalCreateSocket(Sin);
      SockCheck(synsock.Bind(FSocket, Sin));
      GetSinLocal;
      FBuffer := '';
      FBinded := True;
    End;
    ExceptCheck;
    DoStatus(HR_Bind, IP + ':' + Port);
  End;
End;

Procedure TBlockSocket.Connect(IP, Port: String);
Var
  Sin: TVarSin;
  b: Boolean;
Begin
  SetSin(Sin, IP, Port);
  If FLastError = 0 Then
  Begin
    If FSocket = INVALID_SOCKET Then
      InternalCreateSocket(Sin);
    If FConnectionTimeout > 0 Then
    Begin
      // connect in non-blocking mode
      b := NonBlockMode;
      NonBlockMode := True;
      SockCheck(synsock.Connect(FSocket, Sin));
      If (FLastError = WSAEINPROGRESS) Or (FLastError = WSAEWOULDBLOCK) Then
        If Not CanWrite(FConnectionTimeout) Then
          FLastError := WSAETIMEDOUT;
      NonBlockMode := b;
    End
    Else
      SockCheck(synsock.Connect(FSocket, Sin));
    If FLastError = 0 Then
      GetSins;
    FBuffer := '';
    FLastCR := False;
    FLastLF := False;
  End;
  ExceptCheck;
  DoStatus(HR_Connect, IP + ':' + Port);
End;

Procedure TBlockSocket.Listen;
Begin
  SockCheck(synsock.Listen(FSocket, SOMAXCONN));
  GetSins;
  ExceptCheck;
  DoStatus(HR_Listen, '');
End;

Function TBlockSocket.Accept: TSocket;
Begin
  Result := synsock.Accept(FSocket, FRemoteSin);
  ///    SockCheck(Result);
  ExceptCheck;
  DoStatus(HR_Accept, '');
End;

Procedure TBlockSocket.GetSinLocal;
Begin
  synsock.GetSockName(FSocket, FLocalSin);
End;

Procedure TBlockSocket.GetSinRemote;
Begin
  synsock.GetPeerName(FSocket, FRemoteSin);
End;

Procedure TBlockSocket.GetSins;
Begin
  GetSinLocal;
  GetSinRemote;
End;

Procedure TBlockSocket.SetBandwidth(Value: Integer);
Begin
  MaxSendBandwidth := Value;
  MaxRecvBandwidth := Value;
End;

Procedure TBlockSocket.LimitBandwidth(Length: Integer; MaxB: Integer;
  Var Next: Longword);
Var
  x: Longword;
  y: Longword;
  n: Integer;
Begin
  If FStopFlag Then
    exit;
  If MaxB > 0 Then
  Begin
    y := GetTick;
    If Next > y Then
    Begin
      x := Next - y;
      If x > 0 Then
      Begin
        DoStatus(HR_Wait, IntToStr(x));
        sleep(x Mod 250);
        For n := 1 To x Div 250 Do
          If FStopFlag Then
            Break
          Else
            sleep(250);
      End;
    End;
    Next := GetTick + Trunc((Length / MaxB) * 1000);
  End;
End;

Function TBlockSocket.TestStopFlag: Boolean;
Begin
  DoHeartbeat;
  Result := FStopFlag;
  If Result Then
  Begin
    FStopFlag := False;
    FLastError := WSAECONNABORTED;
    ExceptCheck;
  End;
End;


Function TBlockSocket.SendBuffer(Buffer: TMemory; Length: Integer): Integer;
  {$IFNDEF CIL}
Var
  x, y: Integer;
  l, r: Integer;
  p: Pointer;
  {$ENDIF}
Begin
  Result := 0;
  If TestStopFlag Then
    Exit;
  DoMonitor(True, Buffer, Length);
  {$IFDEF CIL}
  Result := synsock.Send(FSocket, Buffer, Length, 0);
  {$ELSE}
  l := Length;
  x := 0;
  While x < l Do
  Begin
    y := l - x;
    If y > FSendMaxChunk Then
      y := FSendMaxChunk;
    If y > 0 Then
    Begin
      LimitBandwidth(y, FMaxSendBandwidth, FNextsend);
      p := IncPoint(Buffer, x);
      r := synsock.Send(FSocket, p, y, MSG_NOSIGNAL);
      SockCheck(r);
      If FLastError = WSAEWOULDBLOCK Then
      Begin
        If CanWrite(FNonblockSendTimeout) Then
        Begin
          r := synsock.Send(FSocket, p, y, MSG_NOSIGNAL);
          SockCheck(r);
        End
        Else
          FLastError := WSAETIMEDOUT;
      End;
      If FLastError <> 0 Then
        Break;
      Inc(x, r);
      Inc(Result, r);
      Inc(FSendCounter, r);
      DoStatus(HR_WriteCount, IntToStr(r));
    End
    Else
      break;
  End;
  {$ENDIF}
  ExceptCheck;
End;

Procedure TBlockSocket.SendByte(Data: Byte);
{$IFDEF CIL}
var
  buf: TMemory;
{$ENDIF}
Begin
  {$IFDEF CIL}
  setlength(buf, 1);
  buf[0] := Data;
  SendBuffer(buf, 1);
  {$ELSE}
  SendBuffer(@Data, 1);
  {$ENDIF}
End;

Procedure TBlockSocket.SendString(Data: Ansistring);
Var
  buf: TMemory;
Begin
  {$IFDEF CIL}
  buf := BytesOf(Data);
  {$ELSE}
  buf := Pointer(Data);
  {$ENDIF}
  SendBuffer(buf, Length(Data));
End;

Procedure TBlockSocket.SendInteger(Data: Integer);
Var
  buf: TMemory;
Begin
  {$IFDEF CIL}
  buf := System.BitConverter.GetBytes(Data);
  {$ELSE}
  buf := @Data;
  {$ENDIF}
  SendBuffer(buf, SizeOf(Data));
End;

Procedure TBlockSocket.SendBlock(Const Data: Ansistring);
Var
  i: Integer;
Begin
  i := SwapBytes(Length(Data));
  SendString(Codelongint(i) + Data);
End;

Procedure TBlockSocket.InternalSendStream(Const Stream: TStream;
  WithSize, Indy: Boolean);
Var
  l: Integer;
  yr: Integer;
  s: Ansistring;
  b: Boolean;
  {$IFDEF CIL}
  buf: TMemory;
  {$ENDIF}
Begin
  b := True;
  l := 0;
  If WithSize Then
  Begin
    l := Stream.Size - Stream.Position;
    ;
    If Not Indy Then
      l := synsock.HToNL(l);
  End;
  Repeat
    {$IFDEF CIL}
    Setlength(buf, FSendMaxChunk);
    yr := Stream.read(buf, FSendMaxChunk);
    if yr > 0 then
    begin
      if WithSize and b then
      begin
        b := false;
        SendString(CodeLongInt(l));
      end;
      SendBuffer(buf, yr);
      if FLastError <> 0 then
        break;
    end
    {$ELSE}
    Setlength(s, FSendMaxChunk);
    yr := Stream.Read(Pointer(s)^, FSendMaxChunk);
    If yr > 0 Then
    Begin
      SetLength(s, yr);
      If WithSize And b Then
      Begin
        b := False;
        SendString(CodeLongInt(l) + s);
      End
      Else
        SendString(s);
      If FLastError <> 0 Then
        break;
    End
    {$ENDIF}
  Until yr <= 0;
End;

Procedure TBlockSocket.SendStreamRaw(Const Stream: TStream);
Begin
  InternalSendStream(Stream, False, False);
End;

Procedure TBlockSocket.SendStreamIndy(Const Stream: TStream);
Begin
  InternalSendStream(Stream, True, True);
End;

Procedure TBlockSocket.SendStream(Const Stream: TStream);
Begin
  InternalSendStream(Stream, True, False);
End;

Function TBlockSocket.RecvBuffer(Buffer: TMemory; Length: Integer): Integer;
Begin
  Result := 0;
  If TestStopFlag Then
    Exit;
  LimitBandwidth(Length, FMaxRecvBandwidth, FNextRecv);
  //  Result := synsock.Recv(FSocket, Buffer^, Length, MSG_NOSIGNAL);
  Result := synsock.Recv(FSocket, Buffer, Length, MSG_NOSIGNAL);
  If Result = 0 Then
    FLastError := WSAECONNRESET
  Else
    SockCheck(Result);
  ExceptCheck;
  If Result > 0 Then
  Begin
    Inc(FRecvCounter, Result);
    DoStatus(HR_ReadCount, IntToStr(Result));
    DoMonitor(False, Buffer, Result);
    DoReadFilter(Buffer, Result);
  End;
End;

Function TBlockSocket.RecvBufferEx(Buffer: TMemory; Len: Integer;
  Timeout: Integer): Integer;
Var
  s: Ansistring;
  rl, l: Integer;
  ti: Longword;
  {$IFDEF CIL}
  n: integer;
  b: TMemory;
  {$ENDIF}
Begin
  ResetLastError;
  Result := 0;
  If Len > 0 Then
  Begin
    rl := 0;
    Repeat
      ti := GetTick;
      s := RecvPacket(Timeout);
      l := Length(s);
      If (rl + l) > Len Then
        l := Len - rl;
      {$IFDEF CIL}
      b := BytesOf(s);
      for n := 0 to l do
        Buffer[rl + n] := b[n];
      {$ELSE}
      Move(Pointer(s)^, IncPoint(Buffer, rl)^, l);
      {$ENDIF}
      rl := rl + l;
      If FLastError <> 0 Then
        Break;
      If rl >= Len Then
        Break;
      If Not FInterPacketTimeout Then
      Begin
        Timeout := Timeout - Integer(TickDelta(ti, GetTick));
        If Timeout <= 0 Then
        Begin
          FLastError := WSAETIMEDOUT;
          Break;
        End;
      End;
    Until False;
    Delete(s, 1, l);
    FBuffer := s;
    Result := rl;
  End;
End;

Function TBlockSocket.RecvBufferStr(Len: Integer; Timeout: Integer): Ansistring;
Var
  x: Integer;
  {$IFDEF CIL}
  buf: Tmemory;
  {$ENDIF}
Begin
  Result := '';
  If Len > 0 Then
  Begin
    {$IFDEF CIL}
    Setlength(Buf, Len);
    x := RecvBufferEx(buf, Len , Timeout);
    if FLastError = 0 then
    begin
      SetLength(Buf, x);
      Result := StringOf(buf);
    end
    else
      Result := '';
    {$ELSE}
    Setlength(Result, Len);
    x := RecvBufferEx(Pointer(Result), Len, Timeout);
    If FLastError = 0 Then
      SetLength(Result, x)
    Else
      Result := '';
    {$ENDIF}
  End;
End;

Function TBlockSocket.RecvPacket(Timeout: Integer): Ansistring;
Var
  x: Integer;
  {$IFDEF CIL}
  buf: TMemory;
  {$ENDIF}
Begin
  Result := '';
  ResetLastError;
  If FBuffer <> '' Then
  Begin
    Result := FBuffer;
    FBuffer := '';
  End
  Else
  Begin
    {$IFDEF MSWINDOWS}
    //not drain CPU on large downloads...
    Sleep(0);
    {$ENDIF}
    x := WaitingData;
    If x > 0 Then
    Begin
      {$IFDEF CIL}
      SetLength(Buf, x);
      x := RecvBuffer(Buf, x);
      if x >= 0 then
      begin
        SetLength(Buf, x);
        Result := StringOf(Buf);
      end;
      {$ELSE}
      SetLength(Result, x);
      x := RecvBuffer(Pointer(Result), x);
      If x >= 0 Then
        SetLength(Result, x);
      {$ENDIF}
    End
    Else
    Begin
      If CanRead(Timeout) Then
      Begin
        x := WaitingData;
        If x = 0 Then
          FLastError := WSAECONNRESET;
        If x > 0 Then
        Begin
          {$IFDEF CIL}
          SetLength(Buf, x);
          x := RecvBuffer(Buf, x);
          if x >= 0 then
          begin
            SetLength(Buf, x);
            result := StringOf(Buf);
          end;
          {$ELSE}
          SetLength(Result, x);
          x := RecvBuffer(Pointer(Result), x);
          If x >= 0 Then
            SetLength(Result, x);
          {$ENDIF}
        End;
      End
      Else
        FLastError := WSAETIMEDOUT;
    End;
  End;
  If FConvertLineEnd And (Result <> '') Then
  Begin
    If FLastCR And (Result[1] = LF) Then
      Delete(Result, 1, 1);
    If FLastLF And (Result[1] = CR) Then
      Delete(Result, 1, 1);
    FLastCR := False;
    FLastLF := False;
  End;
  ExceptCheck;
End;


Function TBlockSocket.RecvByte(Timeout: Integer): Byte;
Begin
  Result := 0;
  ResetLastError;
  If FBuffer = '' Then
    FBuffer := RecvPacket(Timeout);
  If (FLastError = 0) And (FBuffer <> '') Then
  Begin
    Result := Ord(FBuffer[1]);
    Delete(FBuffer, 1, 1);
  End;
  ExceptCheck;
End;

Function TBlockSocket.RecvInteger(Timeout: Integer): Integer;
Var
  s: Ansistring;
Begin
  Result := 0;
  s := RecvBufferStr(4, Timeout);
  If FLastError = 0 Then
    Result := (Ord(s[1]) + Ord(s[2]) * 256) + (Ord(s[3]) + Ord(s[4]) * 256) * 65536;
End;

Function TBlockSocket.RecvTerminated(Timeout: Integer;
  Const Terminator: Ansistring): Ansistring;
Var
  x: Integer;
  s: Ansistring;
  l: Integer;
  CorCRLF: Boolean;
  t: Ansistring;
  tl: Integer;
  ti: Longword;
Begin
  ResetLastError;
  Result := '';
  l := Length(Terminator);
  If l = 0 Then
    Exit;
  tl := l;
  CorCRLF := FConvertLineEnd And (Terminator = CRLF);
  s := '';
  x := 0;
  Repeat
    //get rest of FBuffer or incomming new data...
    ti := GetTick;
    s := s + RecvPacket(Timeout);
    If FLastError <> 0 Then
      Break;
    x := 0;
    If Length(s) > 0 Then
      If CorCRLF Then
      Begin
        t := '';
        x := PosCRLF(s, t);
        tl := Length(t);
        If t = CR Then
          FLastCR := True;
        If t = LF Then
          FLastLF := True;
      End
      Else
      Begin
        x := pos(Terminator, s);
        tl := l;
      End;
    If (FMaxLineLength <> 0) And (Length(s) > FMaxLineLength) Then
    Begin
      FLastError := WSAENOBUFS;
      Break;
    End;
    If x > 0 Then
      Break;
    If Not FInterPacketTimeout Then
    Begin
      Timeout := Timeout - Integer(TickDelta(ti, GetTick));
      If Timeout <= 0 Then
      Begin
        FLastError := WSAETIMEDOUT;
        Break;
      End;
    End;
  Until False;
  If x > 0 Then
  Begin
    Result := Copy(s, 1, x - 1);
    Delete(s, 1, x + tl - 1);
  End;
  FBuffer := s;
  ExceptCheck;
End;

Function TBlockSocket.RecvString(Timeout: Integer): Ansistring;
Var
  s: Ansistring;
Begin
  Result := '';
  s := RecvTerminated(Timeout, CRLF);
  If FLastError = 0 Then
    Result := s;
End;

Function TBlockSocket.RecvBlock(Timeout: Integer): Ansistring;
Var
  x: Integer;
Begin
  Result := '';
  x := RecvInteger(Timeout);
  If FLastError = 0 Then
    Result := RecvBufferStr(x, Timeout);
End;

Procedure TBlockSocket.RecvStreamRaw(Const Stream: TStream; Timeout: Integer);
Var
  s: Ansistring;
Begin
  Repeat
    s := RecvPacket(Timeout);
    If FLastError = 0 Then
      WriteStrToStream(Stream, s);
  Until FLastError <> 0;
End;

Procedure TBlockSocket.RecvStreamSize(Const Stream: TStream; Timeout: Integer;
  Size: Integer);
Var
  s: Ansistring;
  n: Integer;
  {$IFDEF CIL}
  buf: TMemory;
  {$ENDIF}
Begin
  For n := 1 To (Size Div FSendMaxChunk) Do
  Begin
    {$IFDEF CIL}
    SetLength(buf, FSendMaxChunk);
    RecvBufferEx(buf, FSendMaxChunk, Timeout);
    if FLastError <> 0 then
      Exit;
    Stream.Write(buf, FSendMaxChunk);
    {$ELSE}
    s := RecvBufferStr(FSendMaxChunk, Timeout);
    If FLastError <> 0 Then
      Exit;
    WriteStrToStream(Stream, s);
    {$ENDIF}
  End;
  n := Size Mod FSendMaxChunk;
  If n > 0 Then
  Begin
    {$IFDEF CIL}
    SetLength(buf, n);
    RecvBufferEx(buf, n, Timeout);
    if FLastError <> 0 then
      Exit;
    Stream.Write(buf, n);
    {$ELSE}
    s := RecvBufferStr(n, Timeout);
    If FLastError <> 0 Then
      Exit;
    WriteStrToStream(Stream, s);
    {$ENDIF}
  End;
End;

Procedure TBlockSocket.RecvStreamIndy(Const Stream: TStream; Timeout: Integer);
Var
  x: Integer;
Begin
  x := RecvInteger(Timeout);
  x := synsock.NToHL(x);
  If FLastError = 0 Then
    RecvStreamSize(Stream, Timeout, x);
End;

Procedure TBlockSocket.RecvStream(Const Stream: TStream; Timeout: Integer);
Var
  x: Integer;
Begin
  x := RecvInteger(Timeout);
  If FLastError = 0 Then
    RecvStreamSize(Stream, Timeout, x);
End;

Function TBlockSocket.PeekBuffer(Buffer: TMemory; Length: Integer): Integer;
Begin
  {$IFNDEF CIL}
  //  Result := synsock.Recv(FSocket, Buffer^, Length, MSG_PEEK + MSG_NOSIGNAL);
  Result := synsock.Recv(FSocket, Buffer, Length, MSG_PEEK + MSG_NOSIGNAL);
  SockCheck(Result);
  ExceptCheck;
  {$ENDIF}
End;

Function TBlockSocket.PeekByte(Timeout: Integer): Byte;
Var
  s: String;
Begin
  {$IFNDEF CIL}
  Result := 0;
  If CanRead(Timeout) Then
  Begin
    SetLength(s, 1);
    PeekBuffer(Pointer(s), 1);
    If s <> '' Then
      Result := Ord(s[1]);
  End
  Else
    FLastError := WSAETIMEDOUT;
  ExceptCheck;
  {$ENDIF}
End;

Procedure TBlockSocket.ResetLastError;
Begin
  FLastError := 0;
  FLastErrorDesc := '';
End;

Function TBlockSocket.SockCheck(SockResult: Integer): Integer;
Begin
  ResetLastError;
  If SockResult = Integer(SOCKET_ERROR) Then
  Begin
    FLastError := synsock.WSAGetLastError;
    FLastErrorDesc := GetErrorDescEx;
  End;
  Result := FLastError;
End;

Procedure TBlockSocket.ExceptCheck;
Var
  e: ESynapseError;
Begin
  FLastErrorDesc := GetErrorDescEx;
  If (LastError <> 0) And (LastError <> WSAEINPROGRESS) And
    (LastError <> WSAEWOULDBLOCK) Then
  Begin
    DoStatus(HR_Error, IntToStr(FLastError) + ',' + FLastErrorDesc);
    If FRaiseExcept Then
    Begin
      e := ESynapseError.Create(Format('Synapse TCP/IP Socket error %d: %s',
        [FLastError, FLastErrorDesc]));
      e.ErrorCode := FLastError;
      e.ErrorMessage := FLastErrorDesc;
      Raise e;
    End;
  End;
End;

Function TBlockSocket.WaitingData: Integer;
Var
  x: Integer;
Begin
  Result := 0;
  If synsock.IoctlSocket(FSocket, FIONREAD, x) = 0 Then
    Result := x;
  If Result > c64k Then
    Result := c64k;
End;

Function TBlockSocket.WaitingDataEx: Integer;
Begin
  If FBuffer <> '' Then
    Result := Length(FBuffer)
  Else
    Result := WaitingData;
End;

Procedure TBlockSocket.Purge;
Begin
  Sleep(1);
  Try
    While (Length(FBuffer) > 0) Or (WaitingData > 0) Do
    Begin
      RecvPacket(0);
      If FLastError <> 0 Then
        break;
    End;
  Except
    on Exception Do ;
  End;
  ResetLastError;
End;

Procedure TBlockSocket.SetLinger(Enable: Boolean; Linger: Integer);
Var
  d: TSynaOption;
Begin
  d := TSynaOption.Create;
  d.Option := SOT_Linger;
  d.Enabled := Enable;
  d.Value := Linger;
  DelayedOption(d);
End;

Function TBlockSocket.LocalName: String;
Begin
  Result := synsock.GetHostName;
  If Result = '' Then
    Result := '127.0.0.1';
End;

Procedure TBlockSocket.ResolveNameToIP(Name: String; Const IPList: TStrings);
Begin
  IPList.Clear;
  synsock.ResolveNameToIP(Name, FamilyToAF(FFamily), GetSocketprotocol,
    GetSocketType, IPList);
  If IPList.Count = 0 Then
    IPList.Add(cAnyHost);
End;

Function TBlockSocket.ResolveName(Name: String): String;
Var
  l: TStringList;
Begin
  l := TStringList.Create;
  Try
    ResolveNameToIP(Name, l);
    Result := l[0];
  Finally
    l.Free;
  End;
End;

Function TBlockSocket.ResolvePort(Port: String): Word;
Begin
  Result := synsock.ResolvePort(Port, FamilyToAF(FFamily),
    GetSocketProtocol, GetSocketType);
End;

Function TBlockSocket.ResolveIPToName(IP: String): String;
Begin
  If Not IsIP(IP) And Not IsIp6(IP) Then
    IP := ResolveName(IP);
  Result := synsock.ResolveIPToName(IP, FamilyToAF(FFamily),
    GetSocketProtocol, GetSocketType);
End;

Procedure TBlockSocket.SetRemoteSin(IP, Port: String);
Begin
  SetSin(FRemoteSin, IP, Port);
End;

Function TBlockSocket.GetLocalSinIP: String;
Begin
  Result := GetSinIP(FLocalSin);
End;

Function TBlockSocket.GetRemoteSinIP: String;
Begin
  Result := GetSinIP(FRemoteSin);
End;

Function TBlockSocket.GetLocalSinPort: Integer;
Begin
  Result := GetSinPort(FLocalSin);
End;

Function TBlockSocket.GetRemoteSinPort: Integer;
Begin
  Result := GetSinPort(FRemoteSin);
End;

Function TBlockSocket.InternalCanRead(Timeout: Integer): Boolean;
  {$IFDEF CIL}
begin
  Result := FSocket.Poll(Timeout * 1000, SelectMode.SelectRead);
  {$ELSE}
Var
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
  FDSet: TFDSet;
Begin
  TimeV.tv_usec := (Timeout Mod 1000) * 1000;
  TimeV.tv_sec := Timeout Div 1000;
  TimeVal := @TimeV;
  If Timeout = -1 Then
    TimeVal := nil;
  FDSet := FFdSet;
  x := synsock.Select(FSocket + 1, @FDSet, nil, nil, TimeVal);
  SockCheck(x);
  If FLastError <> 0 Then
    x := 0;
  Result := x > 0;
  {$ENDIF}
End;

Function TBlockSocket.CanRead(Timeout: Integer): Boolean;
Var
  ti, tr: Integer;
  n: Integer;
Begin
  If (FHeartbeatRate <> 0) And (Timeout <> -1) Then
  Begin
    ti := Timeout Div FHeartbeatRate;
    tr := Timeout Mod FHeartbeatRate;
  End
  Else
  Begin
    ti := 0;
    tr := Timeout;
  End;
  Result := InternalCanRead(tr);
  If Not Result Then
    For n := 0 To ti Do
    Begin
      DoHeartbeat;
      If FStopFlag Then
      Begin
        Result := False;
        FStopFlag := False;
        Break;
      End;
      Result := InternalCanRead(FHeartbeatRate);
      If Result Then
        break;
    End;
  ExceptCheck;
  If Result Then
    DoStatus(HR_CanRead, '');
End;

Function TBlockSocket.InternalCanWrite(Timeout: Integer): Boolean;
  {$IFDEF CIL}
begin
  Result := FSocket.Poll(Timeout * 1000, SelectMode.SelectWrite);
  {$ELSE}
Var
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
  FDSet: TFDSet;
Begin
  TimeV.tv_usec := (Timeout Mod 1000) * 1000;
  TimeV.tv_sec := Timeout Div 1000;
  TimeVal := @TimeV;
  If Timeout = -1 Then
    TimeVal := nil;
  FDSet := FFdSet;
  x := synsock.Select(FSocket + 1, nil, @FDSet, nil, TimeVal);
  SockCheck(x);
  If FLastError <> 0 Then
    x := 0;
  Result := x > 0;
  {$ENDIF}
End;

Function TBlockSocket.CanWrite(Timeout: Integer): Boolean;
Var
  ti, tr: Integer;
  n: Integer;
Begin
  If (FHeartbeatRate <> 0) And (Timeout <> -1) Then
  Begin
    ti := Timeout Div FHeartbeatRate;
    tr := Timeout Mod FHeartbeatRate;
  End
  Else
  Begin
    ti := 0;
    tr := Timeout;
  End;
  Result := InternalCanWrite(tr);
  If Not Result Then
    For n := 0 To ti Do
    Begin
      DoHeartbeat;
      If FStopFlag Then
      Begin
        Result := False;
        FStopFlag := False;
        Break;
      End;
      Result := InternalCanWrite(FHeartbeatRate);
      If Result Then
        break;
    End;
  ExceptCheck;
  If Result Then
    DoStatus(HR_CanWrite, '');
End;

Function TBlockSocket.CanReadEx(Timeout: Integer): Boolean;
Begin
  If FBuffer <> '' Then
    Result := True
  Else
    Result := CanRead(Timeout);
End;

Function TBlockSocket.SendBufferTo(Buffer: TMemory; Length: Integer): Integer;
Begin
  Result := 0;
  If TestStopFlag Then
    Exit;
  DoMonitor(True, Buffer, Length);
  LimitBandwidth(Length, FMaxSendBandwidth, FNextsend);
  Result := synsock.SendTo(FSocket, Buffer, Length, MSG_NOSIGNAL, FRemoteSin);
  SockCheck(Result);
  ExceptCheck;
  Inc(FSendCounter, Result);
  DoStatus(HR_WriteCount, IntToStr(Result));
End;

Function TBlockSocket.RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer;
Begin
  Result := 0;
  If TestStopFlag Then
    Exit;
  LimitBandwidth(Length, FMaxRecvBandwidth, FNextRecv);
  Result := synsock.RecvFrom(FSocket, Buffer, Length, MSG_NOSIGNAL, FRemoteSin);
  SockCheck(Result);
  ExceptCheck;
  Inc(FRecvCounter, Result);
  DoStatus(HR_ReadCount, IntToStr(Result));
  DoMonitor(False, Buffer, Result);
End;

Function TBlockSocket.GetSizeRecvBuffer: Integer;
Var
  l: Integer;
  {$IFDEF CIL}
  buf: TMemory;
  {$ENDIF}
Begin
  {$IFDEF CIL}
  setlength(buf, 4);
  SockCheck(synsock.GetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_RCVBUF), buf, l));
  Result := System.BitConverter.ToInt32(buf,0);
  {$ELSE}
  l := SizeOf(Result);
  SockCheck(synsock.GetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, @Result, l));
  If FLastError <> 0 Then
    Result := 1024;
  ExceptCheck;
  {$ENDIF}
End;

Procedure TBlockSocket.SetSizeRecvBuffer(Size: Integer);
Var
  d: TSynaOption;
Begin
  d := TSynaOption.Create;
  d.Option := SOT_RecvBuff;
  d.Value := Size;
  DelayedOption(d);
End;

Function TBlockSocket.GetSizeSendBuffer: Integer;
Var
  l: Integer;
  {$IFDEF CIL}
  buf: TMemory;
  {$ENDIF}
Begin
  {$IFDEF CIL}
  setlength(buf, 4);
  SockCheck(synsock.GetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_SNDBUF), buf, l));
  Result := System.BitConverter.ToInt32(buf,0);
  {$ELSE}
  l := SizeOf(Result);
  SockCheck(synsock.GetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, @Result, l));
  If FLastError <> 0 Then
    Result := 1024;
  ExceptCheck;
  {$ENDIF}
End;

Procedure TBlockSocket.SetSizeSendBuffer(Size: Integer);
Var
  d: TSynaOption;
Begin
  d := TSynaOption.Create;
  d.Option := SOT_SendBuff;
  d.Value := Size;
  DelayedOption(d);
End;

Procedure TBlockSocket.SetNonBlockMode(Value: Boolean);
Var
  d: TSynaOption;
Begin
  d := TSynaOption.Create;
  d.Option := SOT_nonblock;
  d.Enabled := Value;
  DelayedOption(d);
End;

Procedure TBlockSocket.SetTimeout(Timeout: Integer);
Begin
  SetSendTimeout(Timeout);
  SetRecvTimeout(Timeout);
End;

Procedure TBlockSocket.SetSendTimeout(Timeout: Integer);
Var
  d: TSynaOption;
Begin
  d := TSynaOption.Create;
  d.Option := SOT_sendtimeout;
  d.Value := Timeout;
  DelayedOption(d);
End;

Procedure TBlockSocket.SetRecvTimeout(Timeout: Integer);
Var
  d: TSynaOption;
Begin
  d := TSynaOption.Create;
  d.Option := SOT_recvtimeout;
  d.Value := Timeout;
  DelayedOption(d);
End;

{$IFNDEF CIL}
Function TBlockSocket.GroupCanRead(Const SocketList: TList; Timeout: Integer;
  Const CanReadList: TList): Boolean;
Var
  FDSet: TFDSet;
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x, n: Integer;
  Max: Integer;
Begin
  TimeV.tv_usec := (Timeout Mod 1000) * 1000;
  TimeV.tv_sec := Timeout Div 1000;
  TimeVal := @TimeV;
  If Timeout = -1 Then
    TimeVal := nil;
  FD_ZERO(FDSet);
  Max := 0;
  For n := 0 To SocketList.Count - 1 Do
    If TObject(SocketList.Items[n]) Is TBlockSocket Then
    Begin
      If TBlockSocket(SocketList.Items[n]).Socket > Max Then
        Max := TBlockSocket(SocketList.Items[n]).Socket;
      FD_SET(TBlockSocket(SocketList.Items[n]).Socket, FDSet);
    End;
  x := synsock.Select(Max + 1, @FDSet, nil, nil, TimeVal);
  SockCheck(x);
  ExceptCheck;
  If FLastError <> 0 Then
    x := 0;
  Result := x > 0;
  CanReadList.Clear;
  If Result Then
    For n := 0 To SocketList.Count - 1 Do
      If TObject(SocketList.Items[n]) Is TBlockSocket Then
        If FD_ISSET(TBlockSocket(SocketList.Items[n]).Socket, FDSet) Then
          CanReadList.Add(TBlockSocket(SocketList.Items[n]));
End;
{$ENDIF}

Procedure TBlockSocket.EnableReuse(Value: Boolean);
Var
  d: TSynaOption;
Begin
  d := TSynaOption.Create;
  d.Option := SOT_reuse;
  d.Enabled := Value;
  DelayedOption(d);
End;

Procedure TBlockSocket.SetTTL(TTL: Integer);
Var
  d: TSynaOption;
Begin
  d := TSynaOption.Create;
  d.Option := SOT_TTL;
  d.Value := TTL;
  DelayedOption(d);
End;

Function TBlockSocket.GetTTL: Integer;
Var
  l: Integer;
Begin
  {$IFNDEF CIL}
  l := SizeOf(Result);
  If FIP6Used Then
    synsock.GetSockOpt(FSocket, IPPROTO_IPV6, IPV6_UNICAST_HOPS, @Result, l)
  Else
    synsock.GetSockOpt(FSocket, IPPROTO_IP, IP_TTL, @Result, l);
  {$ENDIF}
End;

Procedure TBlockSocket.SetFamily(Value: TSocketFamily);
Begin
  FFamily := Value;
  FFamilySave := Value;
End;

Procedure TBlockSocket.SetSocket(Value: TSocket);
Begin
  FRecvCounter := 0;
  FSendCounter := 0;
  FSocket := Value;
  {$IFNDEF CIL}
  FD_ZERO(FFDSet);
  FD_SET(FSocket, FFDSet);
  {$ENDIF}
  GetSins;
  FIP6Used := FRemoteSin.AddressFamily = AF_INET6;
End;

Function TBlockSocket.GetWsaData: TWSAData;
Begin
  {$IFDEF ONCEWINSOCK}
  Result := WsaDataOnce;
  {$ELSE}
  Result := FWsaDataOnce;
  {$ENDIF}
End;

Function TBlockSocket.GetSocketType: Integer;
Begin
  Result := 0;
End;

Function TBlockSocket.GetSocketProtocol: Integer;
Begin
  Result := Integer(IPPROTO_IP);
End;

Procedure TBlockSocket.DoStatus(Reason: THookSocketReason; Const Value: String);
Begin
  If assigned(OnStatus) Then
    OnStatus(Self, Reason, Value);
End;

Procedure TBlockSocket.DoReadFilter(Buffer: TMemory; Var Len: Integer);
Var
  s: Ansistring;
Begin
  If assigned(OnReadFilter) Then
    If Len > 0 Then
    Begin
      {$IFDEF CIL}
        s := StringOf(Buffer);
      {$ELSE}
      SetLength(s, Len);
      Move(Buffer^, Pointer(s)^, Len);
      {$ENDIF}
      OnReadFilter(Self, s);
      If Length(s) > Len Then
        SetLength(s, Len);
      Len := Length(s);
      {$IFDEF CIL}
        Buffer := BytesOf(s);
      {$ELSE}
      Move(Pointer(s)^, Buffer^, Len);
      {$ENDIF}
    End;
End;

Procedure TBlockSocket.DoCreateSocket;
Begin
  If assigned(OnCreateSocket) Then
    OnCreateSocket(Self);
End;

Procedure TBlockSocket.DoMonitor(Writing: Boolean; Const Buffer: TMemory; Len: Integer);
Begin
  If assigned(OnMonitor) Then
  Begin
    OnMonitor(Self, Writing, Buffer, Len);
  End;
End;

Procedure TBlockSocket.DoHeartbeat;
Begin
  If assigned(OnHeartbeat) And (FHeartbeatRate <> 0) Then
  Begin
    OnHeartbeat(Self);
  End;
End;

Function TBlockSocket.GetErrorDescEx: String;
Begin
  Result := GetErrorDesc(FLastError);
End;

Class Function TBlockSocket.GetErrorDesc(ErrorCode: Integer): String;
Begin
  {$IFDEF CIL}
  if ErrorCode = 0 then
    Result := ''
  else
  begin
    Result := WSAGetLastErrorDesc;
    if Result = '' then
      Result := 'Other Winsock error (' + IntToStr(ErrorCode) + ')';
  end;
  {$ELSE}
  Case ErrorCode Of
    0:
      Result := '';
    WSAEINTR: {10004}
      Result := 'Interrupted system call';
    WSAEBADF: {10009}
      Result := 'Bad file number';
    WSAEACCES: {10013}
      Result := 'Permission denied';
    WSAEFAULT: {10014}
      Result := 'Bad address';
    WSAEINVAL: {10022}
      Result := 'Invalid argument';
    WSAEMFILE: {10024}
      Result := 'Too many open files';
    WSAEWOULDBLOCK: {10035}
      Result := 'Operation would block';
    WSAEINPROGRESS: {10036}
      Result := 'Operation now in progress';
    WSAEALREADY: {10037}
      Result := 'Operation already in progress';
    WSAENOTSOCK: {10038}
      Result := 'Socket operation on nonsocket';
    WSAEDESTADDRREQ: {10039}
      Result := 'Destination address required';
    WSAEMSGSIZE: {10040}
      Result := 'Message too long';
    WSAEPROTOTYPE: {10041}
      Result := 'Protocol wrong type for Socket';
    WSAENOPROTOOPT: {10042}
      Result := 'Protocol not available';
    WSAEPROTONOSUPPORT: {10043}
      Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT: {10044}
      Result := 'Socket not supported';
    WSAEOPNOTSUPP: {10045}
      Result := 'Operation not supported on Socket';
    WSAEPFNOSUPPORT: {10046}
      Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT: {10047}
      Result := 'Address family not supported';
    WSAEADDRINUSE: {10048}
      Result := 'Address already in use';
    WSAEADDRNOTAVAIL: {10049}
      Result := 'Can''t assign requested address';
    WSAENETDOWN: {10050}
      Result := 'Network is down';
    WSAENETUNREACH: {10051}
      Result := 'Network is unreachable';
    WSAENETRESET: {10052}
      Result := 'Network dropped connection on reset';
    WSAECONNABORTED: {10053}
      Result := 'Software caused connection abort';
    WSAECONNRESET: {10054}
      Result := 'Connection reset by peer';
    WSAENOBUFS: {10055}
      Result := 'No Buffer space available';
    WSAEISCONN: {10056}
      Result := 'Socket is already connected';
    WSAENOTCONN: {10057}
      Result := 'Socket is not connected';
    WSAESHUTDOWN: {10058}
      Result := 'Can''t send after Socket shutdown';
    WSAETOOMANYREFS: {10059}
      Result := 'Too many references:can''t splice';
    WSAETIMEDOUT: {10060}
      Result := 'Connection timed out';
    WSAECONNREFUSED: {10061}
      Result := 'Connection refused';
    WSAELOOP: {10062}
      Result := 'Too many levels of symbolic links';
    WSAENAMETOOLONG: {10063}
      Result := 'File name is too long';
    WSAEHOSTDOWN: {10064}
      Result := 'Host is down';
    WSAEHOSTUNREACH: {10065}
      Result := 'No route to host';
    WSAENOTEMPTY: {10066}
      Result := 'Directory is not empty';
    WSAEPROCLIM: {10067}
      Result := 'Too many processes';
    WSAEUSERS: {10068}
      Result := 'Too many users';
    WSAEDQUOT: {10069}
      Result := 'Disk quota exceeded';
    WSAESTALE: {10070}
      Result := 'Stale NFS file handle';
    WSAEREMOTE: {10071}
      Result := 'Too many levels of remote in path';
    WSASYSNOTREADY: {10091}
      Result := 'Network subsystem is unusable';
    WSAVERNOTSUPPORTED: {10092}
      Result := 'Winsock DLL cannot support this application';
    WSANOTINITIALISED: {10093}
      Result := 'Winsock not initialized';
    WSAEDISCON: {10101}
      Result := 'Disconnect';
    WSAHOST_NOT_FOUND: {11001}
      Result := 'Host not found';
    WSATRY_AGAIN: {11002}
      Result := 'Non authoritative - host not found';
    WSANO_RECOVERY: {11003}
      Result := 'Non recoverable error';
    WSANO_DATA: {11004}
      Result := 'Valid name, no data record of requested type'
    Else
      Result := 'Other Winsock error (' + IntToStr(ErrorCode) + ')';
  End;
  {$ENDIF}
End;

{======================================================================}

Constructor TSocksBlockSocket.Create;
Begin
  Inherited Create;
  FSocksIP := '';
  FSocksPort := '1080';
  FSocksTimeout := 60000;
  FSocksUsername := '';
  FSocksPassword := '';
  FUsingSocks := False;
  FSocksResolver := True;
  FSocksLastError := 0;
  FSocksResponseIP := '';
  FSocksResponsePort := '';
  FSocksLocalIP := '';
  FSocksLocalPort := '';
  FSocksRemoteIP := '';
  FSocksRemotePort := '';
  FBypassFlag := False;
  FSocksType := ST_Socks5;
End;

Function TSocksBlockSocket.SocksOpen: Boolean;
Var
  Buf: Ansistring;
  n: Integer;
Begin
  Result := False;
  FUsingSocks := False;
  If FSocksType <> ST_Socks5 Then
  Begin
    FUsingSocks := True;
    Result := True;
  End
  Else
  Begin
    FBypassFlag := True;
    Try
      If FSocksUsername = '' Then
        Buf := #5 + #1 + #0
      Else
        Buf := #5 + #2 + #2 + #0;
      SendString(Buf);
      Buf := RecvBufferStr(2, FSocksTimeout);
      If Length(Buf) < 2 Then
        Exit;
      If Buf[1] <> #5 Then
        Exit;
      n := Ord(Buf[2]);
      Case n Of
        0: //not need authorisation
          ;
        2:
        Begin
          Buf := #1 + Ansichar(Length(FSocksUsername)) +
            FSocksUsername + Ansichar(Length(FSocksPassword)) + FSocksPassword;
          SendString(Buf);
          Buf := RecvBufferStr(2, FSocksTimeout);
          If Length(Buf) < 2 Then
            Exit;
          If Buf[2] <> #0 Then
            Exit;
        End;
        Else
          //other authorisation is not supported!
          Exit;
      End;
      FUsingSocks := True;
      Result := True;
    Finally
      FBypassFlag := False;
    End;
  End;
End;

Function TSocksBlockSocket.SocksRequest(Cmd: Byte; Const IP, Port: String): Boolean;
Var
  Buf: Ansistring;
Begin
  FBypassFlag := True;
  Try
    If FSocksType <> ST_Socks5 Then
      Buf := #4 + Ansichar(Cmd) + SocksCode(IP, Port)
    Else
      Buf := #5 + Ansichar(Cmd) + #0 + SocksCode(IP, Port);
    SendString(Buf);
    Result := FLastError = 0;
  Finally
    FBypassFlag := False;
  End;
End;

Function TSocksBlockSocket.SocksResponse: Boolean;
Var
  Buf, s: Ansistring;
  x: Integer;
Begin
  Result := False;
  FBypassFlag := True;
  Try
    FSocksResponseIP := '';
    FSocksResponsePort := '';
    FSocksLastError := -1;
    If FSocksType <> ST_Socks5 Then
    Begin
      Buf := RecvBufferStr(8, FSocksTimeout);
      If FLastError <> 0 Then
        Exit;
      If Buf[1] <> #0 Then
        Exit;
      FSocksLastError := Ord(Buf[2]);
    End
    Else
    Begin
      Buf := RecvBufferStr(4, FSocksTimeout);
      If FLastError <> 0 Then
        Exit;
      If Buf[1] <> #5 Then
        Exit;
      Case Ord(Buf[4]) Of
        1:
          s := RecvBufferStr(4, FSocksTimeout);
        3:
        Begin
          x := RecvByte(FSocksTimeout);
          If FLastError <> 0 Then
            Exit;
          s := Ansichar(x) + RecvBufferStr(x, FSocksTimeout);
        End;
        4:
          s := RecvBufferStr(16, FSocksTimeout);
        Else
          Exit;
      End;
      Buf := Buf + s + RecvBufferStr(2, FSocksTimeout);
      If FLastError <> 0 Then
        Exit;
      FSocksLastError := Ord(Buf[2]);
    End;
    If ((FSocksLastError <> 0) And (FSocksLastError <> 90)) Then
      Exit;
    SocksDecode(Buf);
    Result := True;
  Finally
    FBypassFlag := False;
  End;
End;

Function TSocksBlockSocket.SocksCode(IP, Port: String): Ansistring;
Var
  ip6: TIp6Bytes;
  n: Integer;
Begin
  If FSocksType <> ST_Socks5 Then
  Begin
    Result := CodeInt(ResolvePort(Port));
    If Not FSocksResolver Then
      IP := ResolveName(IP);
    If IsIP(IP) Then
    Begin
      Result := Result + IPToID(IP);
      Result := Result + FSocksUsername + #0;
    End
    Else
    Begin
      Result := Result + IPToID('0.0.0.1');
      Result := Result + FSocksUsername + #0;
      Result := Result + IP + #0;
    End;
  End
  Else
  Begin
    If Not FSocksResolver Then
      IP := ResolveName(IP);
    If IsIP(IP) Then
      Result := #1 + IPToID(IP)
    Else
      If IsIP6(IP) Then
      Begin
        ip6 := StrToIP6(IP);
        Result := #4;
        For n := 0 To 15 Do
          Result := Result + Ansichar(ip6[n]);
      End
      Else
        Result := #3 + Ansichar(Length(IP)) + IP;
    Result := Result + CodeInt(ResolvePort(Port));
  End;
End;

Function TSocksBlockSocket.SocksDecode(Value: Ansistring): Integer;
Var
  Atyp: Byte;
  y, n: Integer;
  w: Word;
  ip6: TIp6Bytes;
Begin
  FSocksResponsePort := '0';
  Result := 0;
  If FSocksType <> ST_Socks5 Then
  Begin
    If Length(Value) < 8 Then
      Exit;
    Result := 3;
    w := DecodeInt(Value, Result);
    FSocksResponsePort := IntToStr(w);
    FSocksResponseIP := Format('%d.%d.%d.%d',
      [Ord(Value[5]), Ord(Value[6]), Ord(Value[7]), Ord(Value[8])]);
    Result := 9;
  End
  Else
  Begin
    If Length(Value) < 4 Then
      Exit;
    Atyp := Ord(Value[4]);
    Result := 5;
    Case Atyp Of
      1:
      Begin
        If Length(Value) < 10 Then
          Exit;
        FSocksResponseIP :=
          Format('%d.%d.%d.%d', [Ord(Value[5]), Ord(Value[6]),
          Ord(Value[7]), Ord(Value[8])]);
        Result := 9;
      End;
      3:
      Begin
        y := Ord(Value[5]);
        If Length(Value) < (5 + y + 2) Then
          Exit;
        For n := 6 To 6 + y - 1 Do
          FSocksResponseIP := FSocksResponseIP + Value[n];
        Result := 5 + y + 1;
      End;
      4:
      Begin
        If Length(Value) < 22 Then
          Exit;
        For n := 0 To 15 Do
          ip6[n] := Ord(Value[n + 5]);
        FSocksResponseIP := IP6ToStr(ip6);
        Result := 21;
      End;
      Else
        Exit;
    End;
    w := DecodeInt(Value, Result);
    FSocksResponsePort := IntToStr(w);
    Result := Result + 2;
  End;
End;

{======================================================================}

Procedure TDgramBlockSocket.Connect(IP, Port: String);
Begin
  SetRemoteSin(IP, Port);
  InternalCreateSocket(FRemoteSin);
  FBuffer := '';
  DoStatus(HR_Connect, IP + ':' + Port);
End;

Function TDgramBlockSocket.RecvBuffer(Buffer: TMemory; Length: Integer): Integer;
Begin
  Result := RecvBufferFrom(Buffer, Length);
End;

Function TDgramBlockSocket.SendBuffer(Buffer: TMemory; Length: Integer): Integer;
Begin
  Result := SendBufferTo(Buffer, Length);
End;

{======================================================================}

Destructor TUDPBlockSocket.Destroy;
Begin
  If Assigned(FSocksControlSock) Then
    FSocksControlSock.Free;
  Inherited;
End;

Procedure TUDPBlockSocket.EnableBroadcast(Value: Boolean);
Var
  d: TSynaOption;
Begin
  d := TSynaOption.Create;
  d.Option := SOT_Broadcast;
  d.Enabled := Value;
  DelayedOption(d);
End;

Function TUDPBlockSocket.UdpAssociation: Boolean;
Var
  b: Boolean;
Begin
  Result := True;
  FUsingSocks := False;
  If FSocksIP <> '' Then
  Begin
    Result := False;
    If Not Assigned(FSocksControlSock) Then
      FSocksControlSock := TTCPBlockSocket.Create;
    FSocksControlSock.CloseSocket;
    FSocksControlSock.CreateSocketByName(FSocksIP);
    FSocksControlSock.Connect(FSocksIP, FSocksPort);
    If FSocksControlSock.LastError <> 0 Then
      Exit;
    // if not assigned local port, assign it!
    If Not FBinded Then
      Bind(cAnyHost, cAnyPort);
    //open control TCP connection to SOCKS
    FSocksControlSock.FSocksUsername := FSocksUsername;
    FSocksControlSock.FSocksPassword := FSocksPassword;
    b := FSocksControlSock.SocksOpen;
    If b Then
      b := FSocksControlSock.SocksRequest(3, GetLocalSinIP, IntToStr(GetLocalSinPort));
    If b Then
      b := FSocksControlSock.SocksResponse;
    If Not b And (FLastError = 0) Then
      FLastError := WSANO_RECOVERY;
    FUsingSocks := FSocksControlSock.UsingSocks;
    FSocksRemoteIP := FSocksControlSock.FSocksResponseIP;
    FSocksRemotePort := FSocksControlSock.FSocksResponsePort;
    Result := b And (FLastError = 0);
  End;
End;

Function TUDPBlockSocket.SendBufferTo(Buffer: TMemory; Length: Integer): Integer;
Var
  SIp: String;
  SPort: Integer;
  Buf: Ansistring;
Begin
  Result := 0;
  FUsingSocks := False;
  If (FSocksIP <> '') And (Not UdpAssociation) Then
    FLastError := WSANO_RECOVERY
  Else
  Begin
    If FUsingSocks Then
    Begin
      {$IFNDEF CIL}
      Sip := GetRemoteSinIp;
      SPort := GetRemoteSinPort;
      SetRemoteSin(FSocksRemoteIP, FSocksRemotePort);
      SetLength(Buf, Length);
      Move(Buffer^, Pointer(Buf)^, Length);
      Buf := #0 + #0 + #0 + SocksCode(Sip, IntToStr(SPort)) + Buf;
      Result := Inherited SendBufferTo(Pointer(Buf), System.Length(buf));
      SetRemoteSin(Sip, IntToStr(SPort));
      {$ENDIF}
    End
    Else
      Result := Inherited SendBufferTo(Buffer, Length);
  End;
End;

Function TUDPBlockSocket.RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer;
Var
  Buf: Ansistring;
  x: Integer;
Begin
  Result := Inherited RecvBufferFrom(Buffer, Length);
  If FUsingSocks Then
  Begin
    {$IFNDEF CIL}
    SetLength(Buf, Result);
    Move(Buffer^, Pointer(Buf)^, Result);
    x := SocksDecode(Buf);
    Result := Result - x + 1;
    Buf := Copy(Buf, x, Result);
    Move(Pointer(Buf)^, Buffer^, Result);
    SetRemoteSin(FSocksResponseIP, FSocksResponsePort);
    {$ENDIF}
  End;
End;

{$IFNDEF CIL}
Procedure TUDPBlockSocket.AddMulticast(MCastIP: String);
Var
  Multicast: TIP_mreq;
  Multicast6: TIPv6_mreq;
  n: Integer;
  ip6: Tip6bytes;
Begin
  If FIP6Used Then
  Begin
    ip6 := StrToIp6(MCastIP);
    For n := 0 To 15 Do
      Multicast6.ipv6mr_multiaddr.u6_addr8[n] := Ip6[n];
    Multicast6.ipv6mr_interface := 0;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IPV6, IPV6_JOIN_GROUP,
      Pansichar(@Multicast6), SizeOf(Multicast6)));
  End
  Else
  Begin
    Multicast.imr_multiaddr.S_addr := swapbytes(strtoip(MCastIP));
    //    Multicast.imr_interface.S_addr := INADDR_ANY;
    Multicast.imr_interface.S_addr := FLocalSin.sin_addr.S_addr;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_ADD_MEMBERSHIP,
      Pansichar(@Multicast), SizeOf(Multicast)));
  End;
  ExceptCheck;
End;

Procedure TUDPBlockSocket.DropMulticast(MCastIP: String);
Var
  Multicast: TIP_mreq;
  Multicast6: TIPv6_mreq;
  n: Integer;
  ip6: Tip6bytes;
Begin
  If FIP6Used Then
  Begin
    ip6 := StrToIp6(MCastIP);
    For n := 0 To 15 Do
      Multicast6.ipv6mr_multiaddr.u6_addr8[n] := Ip6[n];
    Multicast6.ipv6mr_interface := 0;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IPV6, IPV6_LEAVE_GROUP,
      Pansichar(@Multicast6), SizeOf(Multicast6)));
  End
  Else
  Begin
    Multicast.imr_multiaddr.S_addr := swapbytes(strtoip(MCastIP));
    //    Multicast.imr_interface.S_addr := INADDR_ANY;
    Multicast.imr_interface.S_addr := FLocalSin.sin_addr.S_addr;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_DROP_MEMBERSHIP,
      Pansichar(@Multicast), SizeOf(Multicast)));
  End;
  ExceptCheck;
End;
{$ENDIF}

Procedure TUDPBlockSocket.SetMulticastTTL(TTL: Integer);
Var
  d: TSynaOption;
Begin
  d := TSynaOption.Create;
  d.Option := SOT_MulticastTTL;
  d.Value := TTL;
  DelayedOption(d);
End;

Function TUDPBlockSocket.GetMulticastTTL: Integer;
Var
  l: Integer;
Begin
  {$IFNDEF CIL}
  l := SizeOf(Result);
  If FIP6Used Then
    synsock.GetSockOpt(FSocket, IPPROTO_IPV6, IPV6_MULTICAST_HOPS, @Result, l)
  Else
    synsock.GetSockOpt(FSocket, IPPROTO_IP, IP_MULTICAST_TTL, @Result, l);
  {$ENDIF}
End;

Procedure TUDPBlockSocket.EnableMulticastLoop(Value: Boolean);
Var
  d: TSynaOption;
Begin
  d := TSynaOption.Create;
  d.Option := SOT_MulticastLoop;
  d.Enabled := Value;
  DelayedOption(d);
End;

Function TUDPBlockSocket.GetSocketType: Integer;
Begin
  Result := Integer(SOCK_DGRAM);
End;

Function TUDPBlockSocket.GetSocketProtocol: Integer;
Begin
  Result := Integer(IPPROTO_UDP);
End;

{======================================================================}
Constructor TTCPBlockSocket.CreateWithSSL(SSLPlugin: TSSLClass);
Begin
  Inherited Create;
  FSSL := SSLPlugin.Create(self);
  FHTTPTunnelIP := '';
  FHTTPTunnelPort := '';
  FHTTPTunnel := False;
  FHTTPTunnelRemoteIP := '';
  FHTTPTunnelRemotePort := '';
  FHTTPTunnelUser := '';
  FHTTPTunnelPass := '';
  FHTTPTunnelTimeout := 30000;
End;

Constructor TTCPBlockSocket.Create;
Begin
  CreateWithSSL(SSLImplementation);
End;

Destructor TTCPBlockSocket.Destroy;
Begin
  Inherited Destroy;
  FSSL.Free;
End;

Function TTCPBlockSocket.GetErrorDescEx: String;
Begin
  Result := Inherited GetErrorDescEx;
  If (FLastError = WSASYSNOTREADY) And (self.SSL.LastError <> 0) Then
  Begin
    Result := self.SSL.LastErrorDesc;
  End;
End;

Procedure TTCPBlockSocket.CloseSocket;
Begin
  If FSSL.SSLEnabled Then
    FSSL.Shutdown;
  If (FSocket <> INVALID_SOCKET) And (FLastError = 0) Then
  Begin
    Synsock.Shutdown(FSocket, 1);
    Purge;
  End;
  Inherited CloseSocket;
End;

Procedure TTCPBlockSocket.DoAfterConnect;
Begin
  If assigned(OnAfterConnect) Then
  Begin
    OnAfterConnect(Self);
  End;
End;

Function TTCPBlockSocket.WaitingData: Integer;
Begin
  Result := 0;
  If FSSL.SSLEnabled And (FSocket <> INVALID_SOCKET) Then
    Result := FSSL.WaitingData;
  If Result = 0 Then
    Result := Inherited WaitingData;
End;

Procedure TTCPBlockSocket.Listen;
Var
  b: Boolean;
  Sip, SPort: String;
Begin
  If FSocksIP = '' Then
  Begin
    Inherited Listen;
  End
  Else
  Begin
    Sip := GetLocalSinIP;
    If Sip = cAnyHost Then
      Sip := LocalName;
    SPort := IntToStr(GetLocalSinPort);
    Inherited Connect(FSocksIP, FSocksPort);
    b := SocksOpen;
    If b Then
      b := SocksRequest(2, Sip, SPort);
    If b Then
      b := SocksResponse;
    If Not b And (FLastError = 0) Then
      FLastError := WSANO_RECOVERY;
    FSocksLocalIP := FSocksResponseIP;
    If FSocksLocalIP = cAnyHost Then
      FSocksLocalIP := FSocksIP;
    FSocksLocalPort := FSocksResponsePort;
    FSocksRemoteIP := '';
    FSocksRemotePort := '';
    ExceptCheck;
    DoStatus(HR_Listen, '');
  End;
End;

Function TTCPBlockSocket.Accept: TSocket;
Begin
  If FUsingSocks Then
  Begin
    If Not SocksResponse And (FLastError = 0) Then
      FLastError := WSANO_RECOVERY;
    FSocksRemoteIP := FSocksResponseIP;
    FSocksRemotePort := FSocksResponsePort;
    Result := FSocket;
    ExceptCheck;
    DoStatus(HR_Accept, '');
  End
  Else
  Begin
    Result := Inherited Accept;
  End;
End;

Procedure TTCPBlockSocket.Connect(IP, Port: String);
Begin
  If FSocksIP <> '' Then
    SocksDoConnect(IP, Port)
  Else
    If FHTTPTunnelIP <> '' Then
      HTTPTunnelDoConnect(IP, Port)
    Else
      Inherited Connect(IP, Port);
  If FLasterror = 0 Then
    DoAfterConnect;
End;

Procedure TTCPBlockSocket.SocksDoConnect(IP, Port: String);
Var
  b: Boolean;
Begin
  Inherited Connect(FSocksIP, FSocksPort);
  If FLastError = 0 Then
  Begin
    b := SocksOpen;
    If b Then
      b := SocksRequest(1, IP, Port);
    If b Then
      b := SocksResponse;
    If Not b And (FLastError = 0) Then
      FLastError := WSASYSNOTREADY;
    FSocksLocalIP := FSocksResponseIP;
    FSocksLocalPort := FSocksResponsePort;
    FSocksRemoteIP := IP;
    FSocksRemotePort := Port;
  End;
  ExceptCheck;
  DoStatus(HR_Connect, IP + ':' + Port);
End;

Procedure TTCPBlockSocket.HTTPTunnelDoConnect(IP, Port: String);
//bugfixed by Mike Green (mgreen@emixode.com)
Var
  s: String;
Begin
  Port := IntToStr(ResolvePort(Port));
  Inherited Connect(FHTTPTunnelIP, FHTTPTunnelPort);
  If FLastError <> 0 Then
    Exit;
  FHTTPTunnel := False;
  If IsIP6(IP) Then
    IP := '[' + IP + ']';
  SendString('CONNECT ' + IP + ':' + Port + ' HTTP/1.0' + CRLF);
  If FHTTPTunnelUser <> '' Then
    Sendstring('Proxy-Authorization: Basic ' +
      EncodeBase64(FHTTPTunnelUser + ':' + FHTTPTunnelPass) + CRLF);
  SendString(CRLF);
  Repeat
    s := RecvTerminated(FHTTPTunnelTimeout, #$0a);
    If FLastError <> 0 Then
      Break;
    If (Pos('HTTP/', s) = 1) And (Length(s) > 11) Then
      FHTTPTunnel := s[10] = '2';
  Until (s = '') Or (s = #$0d);
  If (FLasterror = 0) And Not FHTTPTunnel Then
    FLastError := WSAECONNREFUSED;
  FHTTPTunnelRemoteIP := IP;
  FHTTPTunnelRemotePort := Port;
  ExceptCheck;
End;

Procedure TTCPBlockSocket.SSLDoConnect;
Begin
  ResetLastError;
  If Not FSSL.Connect Then
    FLastError := WSASYSNOTREADY;
  ExceptCheck;
End;

Procedure TTCPBlockSocket.SSLDoShutdown;
Begin
  ResetLastError;
  FSSL.BiShutdown;
End;

Function TTCPBlockSocket.GetLocalSinIP: String;
Begin
  If FUsingSocks Then
    Result := FSocksLocalIP
  Else
    Result := Inherited GetLocalSinIP;
End;

Function TTCPBlockSocket.GetRemoteSinIP: String;
Begin
  If FUsingSocks Then
    Result := FSocksRemoteIP
  Else
    If FHTTPTunnel Then
      Result := FHTTPTunnelRemoteIP
    Else
      Result := Inherited GetRemoteSinIP;
End;

Function TTCPBlockSocket.GetLocalSinPort: Integer;
Begin
  If FUsingSocks Then
    Result := StrToIntDef(FSocksLocalPort, 0)
  Else
    Result := Inherited GetLocalSinPort;
End;

Function TTCPBlockSocket.GetRemoteSinPort: Integer;
Begin
  If FUsingSocks Then
    Result := ResolvePort(FSocksRemotePort)
  Else
    If FHTTPTunnel Then
      Result := StrToIntDef(FHTTPTunnelRemotePort, 0)
    Else
      Result := Inherited GetRemoteSinPort;
End;

Function TTCPBlockSocket.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
Begin
  If FSSL.SSLEnabled Then
  Begin
    Result := 0;
    If TestStopFlag Then
      Exit;
    ResetLastError;
    LimitBandwidth(Len, FMaxRecvBandwidth, FNextRecv);
    Result := FSSL.RecvBuffer(Buffer, Len);
    If FSSL.LastError <> 0 Then
      FLastError := WSASYSNOTREADY;
    ExceptCheck;
    Inc(FRecvCounter, Result);
    DoStatus(HR_ReadCount, IntToStr(Result));
    DoMonitor(False, Buffer, Result);
    DoReadFilter(Buffer, Result);
  End
  Else
    Result := Inherited RecvBuffer(Buffer, Len);
End;

Function TTCPBlockSocket.SendBuffer(Buffer: TMemory; Length: Integer): Integer;
Var
  x, y: Integer;
  l, r: Integer;
  {$IFNDEF CIL}
  p: Pointer;
  {$ENDIF}
Begin
  If FSSL.SSLEnabled Then
  Begin
    Result := 0;
    If TestStopFlag Then
      Exit;
    ResetLastError;
    DoMonitor(True, Buffer, Length);
    {$IFDEF CIL}
    Result := FSSL.SendBuffer(Buffer, Length);
    if FSSL.LastError <> 0 then
      FLastError := WSASYSNOTREADY;
    Inc(FSendCounter, Result);
    DoStatus(HR_WriteCount, IntToStr(Result));
    {$ELSE}
    l := Length;
    x := 0;
    While x < l Do
    Begin
      y := l - x;
      If y > FSendMaxChunk Then
        y := FSendMaxChunk;
      If y > 0 Then
      Begin
        LimitBandwidth(y, FMaxSendBandwidth, FNextsend);
        p := IncPoint(Buffer, x);
        r := FSSL.SendBuffer(p, y);
        If FSSL.LastError <> 0 Then
          FLastError := WSASYSNOTREADY;
        If Flasterror <> 0 Then
          Break;
        Inc(x, r);
        Inc(Result, r);
        Inc(FSendCounter, r);
        DoStatus(HR_WriteCount, IntToStr(r));
      End
      Else
        break;
    End;
    {$ENDIF}
    ExceptCheck;
  End
  Else
    Result := Inherited SendBuffer(Buffer, Length);
End;

Function TTCPBlockSocket.SSLAcceptConnection: Boolean;
Begin
  ResetLastError;
  If Not FSSL.Accept Then
    FLastError := WSASYSNOTREADY;
  ExceptCheck;
  Result := FLastError = 0;
End;

Function TTCPBlockSocket.GetSocketType: Integer;
Begin
  Result := Integer(SOCK_STREAM);
End;

Function TTCPBlockSocket.GetSocketProtocol: Integer;
Begin
  Result := Integer(IPPROTO_TCP);
End;

{======================================================================}

Function TICMPBlockSocket.GetSocketType: Integer;
Begin
  Result := Integer(SOCK_RAW);
End;

Function TICMPBlockSocket.GetSocketProtocol: Integer;
Begin
  If FIP6Used Then
    Result := Integer(IPPROTO_ICMPV6)
  Else
    Result := Integer(IPPROTO_ICMP);
End;

{======================================================================}

Function TRAWBlockSocket.GetSocketType: Integer;
Begin
  Result := Integer(SOCK_RAW);
End;

Function TRAWBlockSocket.GetSocketProtocol: Integer;
Begin
  Result := Integer(IPPROTO_RAW);
End;

{======================================================================}

Function TPGMmessageBlockSocket.GetSocketType: Integer;
Begin
  Result := Integer(SOCK_RDM);
End;

Function TPGMmessageBlockSocket.GetSocketProtocol: Integer;
Begin
  Result := Integer(IPPROTO_RM);
End;

{======================================================================}

Function TPGMstreamBlockSocket.GetSocketType: Integer;
Begin
  Result := Integer(SOCK_STREAM);
End;

Function TPGMstreamBlockSocket.GetSocketProtocol: Integer;
Begin
  Result := Integer(IPPROTO_RM);
End;

{======================================================================}

Constructor TSynaClient.Create;
Begin
  Inherited Create;
  FIPInterface := cAnyHost;
  FTargetHost := cLocalhost;
  FTargetPort := cAnyPort;
  FTimeout := 5000;
  FUsername := '';
  FPassword := '';
End;

{======================================================================}

Constructor TCustomSSL.Create(Const Value: TTCPBlockSocket);
Begin
  Inherited Create;
  FSocket := Value;
  FSSLEnabled := False;
  FUsername := '';
  FPassword := '';
  FLastError := 0;
  FLastErrorDesc := '';
  FVerifyCert := False;
  FSSLType := LT_all;
  FKeyPassword := '';
  FCiphers := '';
  FCertificateFile := '';
  FPrivateKeyFile := '';
  FCertCAFile := '';
  FCertCA := '';
  FTrustCertificate := '';
  FTrustCertificateFile := '';
  FCertificate := '';
  FPrivateKey := '';
  FPFX := '';
  FPFXfile := '';
  FSSHChannelType := '';
  FSSHChannelArg1 := '';
  FSSHChannelArg2 := '';
  FCertComplianceLevel := -1; //default
  FSNIHost := '';
End;

Procedure TCustomSSL.Assign(Const Value: TCustomSSL);
Begin
  FUsername := Value.Username;
  FPassword := Value.Password;
  FVerifyCert := Value.VerifyCert;
  FSSLType := Value.SSLType;
  FKeyPassword := Value.KeyPassword;
  FCiphers := Value.Ciphers;
  FCertificateFile := Value.CertificateFile;
  FPrivateKeyFile := Value.PrivateKeyFile;
  FCertCAFile := Value.CertCAFile;
  FCertCA := Value.CertCA;
  FTrustCertificate := Value.TrustCertificate;
  FTrustCertificateFile := Value.TrustCertificateFile;
  FCertificate := Value.Certificate;
  FPrivateKey := Value.PrivateKey;
  FPFX := Value.PFX;
  FPFXfile := Value.PFXfile;
  FCertComplianceLevel := Value.CertComplianceLevel;
  FSNIHost := Value.FSNIHost;
End;

Procedure TCustomSSL.ReturnError;
Begin
  FLastError := -1;
  FLastErrorDesc := 'SSL/TLS support is not compiled!';
End;

Function TCustomSSL.LibVersion: String;
Begin
  Result := '';
End;

Function TCustomSSL.LibName: String;
Begin
  Result := '';
End;

Function TCustomSSL.CreateSelfSignedCert(Host: String): Boolean;
Begin
  Result := False;
End;

Function TCustomSSL.Connect: Boolean;
Begin
  ReturnError;
  Result := False;
End;

Function TCustomSSL.Accept: Boolean;
Begin
  ReturnError;
  Result := False;
End;

Function TCustomSSL.Shutdown: Boolean;
Begin
  ReturnError;
  Result := False;
End;

Function TCustomSSL.BiShutdown: Boolean;
Begin
  ReturnError;
  Result := False;
End;

Function TCustomSSL.SendBuffer(Buffer: TMemory; Len: Integer): Integer;
Begin
  ReturnError;
  Result := Integer(SOCKET_ERROR);
End;

Procedure TCustomSSL.SetCertCAFile(Const Value: String);
Begin
  FCertCAFile := Value;
End;

Function TCustomSSL.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
Begin
  ReturnError;
  Result := Integer(SOCKET_ERROR);
End;

Function TCustomSSL.WaitingData: Integer;
Begin
  ReturnError;
  Result := 0;
End;

Function TCustomSSL.GetSSLVersion: String;
Begin
  Result := '';
End;

Function TCustomSSL.GetPeerSubject: String;
Begin
  Result := '';
End;

Function TCustomSSL.GetPeerSerialNo: Integer;
Begin
  Result := -1;
End;

Function TCustomSSL.GetPeerName: String;
Begin
  Result := '';
End;

Function TCustomSSL.GetPeerNameHash: Cardinal;
Begin
  Result := 0;
End;

Function TCustomSSL.GetPeerIssuer: String;
Begin
  Result := '';
End;

Function TCustomSSL.GetPeerFingerprint: String;
Begin
  Result := '';
End;

Function TCustomSSL.GetCertInfo: String;
Begin
  Result := '';
End;

Function TCustomSSL.GetCipherName: String;
Begin
  Result := '';
End;

Function TCustomSSL.GetCipherBits: Integer;
Begin
  Result := 0;
End;

Function TCustomSSL.GetCipherAlgBits: Integer;
Begin
  Result := 0;
End;

Function TCustomSSL.GetVerifyCert: Integer;
Begin
  Result := 1;
End;

Function TCustomSSL.DoVerifyCert: Boolean;
Begin
  If assigned(OnVerifyCert) Then
  Begin
    Result := OnVerifyCert(Self);
  End
  Else
    Result := True;
End;


{======================================================================}

Function TSSLNone.LibVersion: String;
Begin
  Result := 'Without SSL support';
End;

Function TSSLNone.LibName: String;
Begin
  Result := 'ssl_none';
End;

{======================================================================}

Initialization
  Begin
    {$IFDEF ONCEWINSOCK}
    If Not InitSocketInterface(DLLStackName) Then
    Begin
      e := ESynapseError.Create('Error loading Socket interface (' +
        DLLStackName + ')!');
      e.ErrorCode := 0;
      e.ErrorMessage := 'Error loading Socket interface (' + DLLStackName + ')!';
      Raise e;
    End;
    synsock.WSAStartup(WinsockLevel, WsaDataOnce);
    {$ENDIF}
  End;

Finalization
  Begin
    {$IFDEF ONCEWINSOCK}
    synsock.WSACleanup;
    DestroySocketInterface;
    {$ENDIF}
  End;

End.
