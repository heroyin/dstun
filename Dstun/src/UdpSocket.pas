unit UdpSocket;

interface

uses
  SysUtils, Windows, Classes, WinSock;

type
  IDrtpSocket = interface
    function GetActive: Boolean;
    function GetLastRecvAddr: string;
    function GetLastRecvPort: Integer;
    function GetLocalIP: string;
    function GetLocalPort: Integer;
    function GetHandle: Integer; stdcall;
    function GetStopTransfer: Boolean; stdcall;
    function GetTimeout: Integer;
    function RecvBuffer(ABuffer: Pointer; ASize: Integer): Integer;
    function ReplyBuffer(ABuffer: Pointer; ASize: Integer): Integer;
    function SendBuffer(ABuffer: Pointer; ASize: Integer; Addr: string; APort:
        Integer): Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetLocalIP(const Value: string);
    procedure SetLocalPort(const Value: Integer);
    procedure SetStopTransfer(const Value: Boolean); stdcall;
    procedure SetTimeout(const Value: Integer);
    function WaitForData(ATimeOut: Integer): Boolean;
    property Active: Boolean read GetActive write SetActive;
    property LastRecvAddr: string read GetLastRecvAddr;
    property LastRecvPort: Integer read GetLastRecvPort;
    property LocalIP: string read GetLocalIP write SetLocalIP;
    property LocalPort: Integer read GetLocalPort write SetLocalPort;
    property Handle: Integer read GetHandle;
    property StopTransfer: Boolean read GetStopTransfer write SetStopTransfer;
    property Timeout: Integer read GetTimeout write SetTimeout;
  end;


  TDRtpUdpSocket = class(TInterfacedObject, IDrtpSocket)
  private
    FActive: Boolean;
    FBufferSize: Integer;
    FLastRecvAddress: TSockAddr;
    FLastRecvAddressLen: Integer;
    FLocalIP: string;
    FLocalPort: Integer;
    FSocket: TSocket;
    FStopTransfer: Boolean;
    FTimeOut: Integer;
    procedure Close;
    function GetActive: Boolean;
    function GetLastRecvAddr: string;
    function GetLastRecvPort: Integer;
    function GetLocalIP: string;
    function GetLocalPort: Integer;
    function GetReuseAddress: Boolean;
    function GetHandle: Integer; stdcall;
    function GetTimeOut: Integer;
    procedure Open;
    procedure SetActive(const Value: Boolean);
    procedure SetBufferSize(const Value: Integer);
    procedure SetLocalIP(const Value: string);
    procedure SetLocalPort(const Value: Integer);
    procedure SetReuseAddress(const Value: Boolean);
    procedure SetTimeOut(const Value: Integer);
  protected
    function GetStopTransfer: Boolean; stdcall;
    procedure SetStopTransfer(const Value: Boolean); stdcall;
  public
    constructor Create;
    destructor Destroy; override;
    function RecvBuffer(ABuffer: Pointer; ASize: Integer): Integer;
    function ReplyBuffer(ABuffer: Pointer; ASize: Integer): Integer;
    function SendBuffer(ABuffer: Pointer; ASize: Integer; Addr: string; APort:
        Integer): Integer;
    function WaitForData(ATimeOut: Integer): Boolean;
    property Active: Boolean read GetActive write SetActive;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
    property LastRecvAddr: string read GetLastRecvAddr;
    property LastRecvPort: Integer read GetLastRecvPort;
    property LocalIP: string read GetLocalIP write SetLocalIP;
    property LocalPort: Integer read GetLocalPort write SetLocalPort;
    property ReuseAddress: Boolean read GetReuseAddress write SetReuseAddress;
    property Handle: Integer read GetHandle;
    property TimeOut: Integer read GetTimeOut write SetTimeOut;
    property StopTransfer: Boolean read GetStopTransfer write SetStopTransfer;
  end;

procedure DebugMsg(valMsg: string; Aarg: array of const);

implementation

function Select(H: THandle; ReadReady, WriteReady, ExceptFlag: PBoolean;
    TimeOut: Integer): Boolean;
var
  ReadFds: TFDset;
  ReadFdsptr: PFDset;
  WriteFds: TFDset;
  WriteFdsptr: PFDset;
  ExceptFds: TFDset;
  ExceptFdsptr: PFDset;
  tv: timeval;
  Timeptr: PTimeval;
begin
  if Assigned(ReadReady) then
  begin
    ReadFdsptr := @ReadFds;
    FD_ZERO(ReadFds);
    FD_SET(H, ReadFds);
  end else
    ReadFdsptr := nil;
  if Assigned(WriteReady) then
  begin
    WriteFdsptr := @WriteFds;
    FD_ZERO(WriteFds);
    FD_SET(H, WriteFds);
  end else
    WriteFdsptr := nil;
  if Assigned(ExceptFlag) then
  begin
    ExceptFdsptr := @ExceptFds;
    FD_ZERO(ExceptFds);
    FD_SET(H, ExceptFds);
  end else
    ExceptFdsptr := nil;
  if TimeOut >= 0 then
  begin
    tv.tv_sec := TimeOut div 1000;
    tv.tv_usec :=  1000 * (TimeOut mod 1000);
    Timeptr := @tv;
  end else
    Timeptr := nil;
  Try
    Result := WinSock.select(H + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr) > 0;
  except
    Result := False;
  end;
  if Assigned(ReadReady) then
    ReadReady^ := FD_ISSET(H, ReadFds);
  if Assigned(WriteReady) then
    WriteReady^ := FD_ISSET(H, WriteFds);
  if Assigned(ExceptFlag) then
    ExceptFlag^ := FD_ISSET(H, ExceptFds);
end;

function GetSocketAddr(h: String; p: Integer): TSockAddr;

  function LookupHostAddr(const hn: string): string;
  var
    h: PHostEnt;
  begin
    Result := '';
    if hn <> '' then
    begin
      if hn[1] in ['0'..'9'] then
      begin
        if inet_addr(pansichar(hn)) <> INADDR_NONE then
          Result := hn;
      end
      else
      begin
        h := gethostbyname(pansichar(hn));
        if h <> nil then
          with h^ do
          Result := format('%d.%d.%d.%d', [ord(h_addr^[0]), ord(h_addr^[1]),
              ord(h_addr^[2]), ord(h_addr^[3])]);
      end;
    end
    else Result := '0.0.0.0';
  end;

  function LookupPort(const sn: string; pn: pchar = nil): word;
  var
    se: PServent;
  begin
    Result := 0;
    if sn <> '' then
    begin
      se := getservbyname(pansichar(sn), pansichar(pn));
      if se <> nil then
        Result := ntohs(se^.s_port)
      else
        Result := StrToInt(sn);
    end;
  end;

begin
  Result.sin_family := AF_INET;
  Result.sin_addr.s_addr := inet_addr(pansichar(LookupHostAddr(h)));
  Result.sin_port := htons(LookupPort(IntToStr(p))); 
end;

function BindFreePort(hSocket: THandle; Address: string; MinPort: integer =
    10000; MaxPort: integer = 20000): Integer;
var
  tmpAddr: TSockAddr;
begin
  Randomize;
  MinPort := MinPort + Random(5)*Random(100)+Random(10);

  Result := MinPort;
  while Result <= MaxPort do
  begin
    tmpAddr := GetSocketAddr(Address, Result);
    if bind(hSocket, tmpAddr, SizeOf(tmpAddr)) <> SOCKET_ERROR then
      Exit;
    Inc(Result);
  end;

  Result := -1;
end;

function InAddrToString(AInAddr: TInAddr): string;
begin
  with AInAddr.S_un_b do
  begin
    Result := IntToStr(Byte(s_b1)) + '.' + IntToStr(Byte(s_b2)) + '.' +
      IntToStr(Byte(s_b3)) + '.' + IntToStr(Byte(s_b4));
  end;
end;

var
  WSAData: TWSAData;

procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    raise Exception.Create('init socket error');
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise Exception.Create('cleanup socket error');
end;


procedure DebugMsg(valMsg: string; Aarg: array of const);
begin
  OutputDebugString(PChar(Format('UDP SOCKET: ' + valMsg, Aarg)));
end;

{TUdpSendChannel}

constructor TDRtpUdpSocket.Create;
begin
  inherited Create;

  FTimeOut := 100;
  FBufferSize := 64 * 1024;

  FSocket := 0;
  FStopTransfer := False;
end;

destructor TDRtpUdpSocket.Destroy;
begin
  if Active then
    Active := False;
  inherited;
end;

procedure TDRtpUdpSocket.Close;
begin
  if FSocket <> 0 then
  begin
    shutdown(FSocket, SD_BOTH);
    closesocket(FSocket);
    FSocket := 0;
  end;
end;

function TDRtpUdpSocket.GetActive: Boolean;
begin
  Result := FActive;
end;

function TDRtpUdpSocket.GetLastRecvAddr: string;
begin
  Result := inet_ntoa(FLastRecvAddress.sin_addr);
end;

function TDRtpUdpSocket.GetLastRecvPort: Integer;
begin
  Result := ntohs(FLastRecvAddress.sin_port);
end;

function TDRtpUdpSocket.GetLocalIP: string;
begin
  Result := FLocalIP;
end;

function TDRtpUdpSocket.GetLocalPort: Integer;
begin
  Result := FLocalPort;
end;

function TDRtpUdpSocket.GetReuseAddress: Boolean;
var
  bReListen, tmpLen: Integer;
begin
  Result := False;
  if FSocket = 0 then Exit;

  GetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR,  @bReListen,  tmpLen);

  Result := bReListen <> 0;
end;

function TDRtpUdpSocket.GetHandle: Integer;
begin
  Result := FSocket;
end;

function TDRtpUdpSocket.GetStopTransfer: Boolean;
begin
  Result := FStopTransfer;
end;

function TDRtpUdpSocket.GetTimeOut: Integer;
begin
  Result := FTimeOut;
end;

procedure TDRtpUdpSocket.Open;
var
  tmpAddr: TSockAddr;
begin
  FSocket := socket(AF_INET, SOCK_DGRAM, 0);

  if FLocalPort <> 0 then
  begin
    tmpAddr := GetSocketAddr(FLocalIP, FLocalPort);
    if bind(FSocket, tmpAddr, SizeOf(tmpAddr)) = SOCKET_ERROR then
    begin
      DebugMsg('bind error: ' + SysErrorMessage(GetLastError), []);
      RaiseLastWin32Error;
    end;
  end else
    FLocalPort := BindFreePort(FSocket, FLocalIP);

  TimeOut := FTimeOut;
  BufferSize := FBufferSize;
end;

function TDRtpUdpSocket.RecvBuffer(ABuffer: Pointer; ASize: Integer): Integer;
begin
  Result := 0;
  if not Active then Exit;
  if FStopTransfer then Exit;

  DebugMsg('Recv udp packet', []);

  FLastRecvAddressLen := SizeOf(FLastRecvAddress);
  FillChar(FLastRecvAddress, SizeOf(FLastRecvAddress), #0);
  Result := recvfrom(FSocket, ABuffer^, ASize, 0, FLastRecvAddress,
    FLastRecvAddressLen);

{  if Result = SOCKET_ERROR then
    DebugMsg('recv error: ' + SysErrorMessage(GetLastError) + ' size = ' + IntToStr(ASize), []);  }
end;

function TDRtpUdpSocket.ReplyBuffer(ABuffer: Pointer; ASize: Integer): Integer;
begin
  Result := -1;
  if not Active then Exit;
  if FStopTransfer then Exit;
  
  DebugMsg('Reply udp packet', []);

 if FLastRecvAddressLen <> 0 then
    Result := sendto(FSocket, ABuffer^, ASize, 0,
      FLastRecvAddress, SizeOf(FLastRecvAddress));
end;

function TDRtpUdpSocket.SendBuffer(ABuffer: Pointer; ASize: Integer; Addr:
    string; APort: Integer): Integer;
var
  tmpAddr: TSockAddr;
begin
  Result := -1;
  if not Active then Exit;
  if FStopTransfer then Exit;

  DebugMsg('send udp packet to %s %d', [Addr, APort]);

  tmpAddr := GetSocketAddr(Addr, APort);
  Result := sendto(FSocket, ABuffer^, ASize, 0, tmpAddr, SizeOf(tmpAddr));

  if Result = SOCKET_ERROR then
    DebugMsg('send error [%s:%d] ' + SysErrorMessage(GetLastError), [Addr,
      APort]);
end;

procedure TDRtpUdpSocket.SetActive(const Value: Boolean);
begin
  if FActive = Value then Exit;

  FActive := Value;

  if FActive then
    Open
  else
    Close;
end;

procedure TDRtpUdpSocket.SetBufferSize(const Value: Integer);
begin
  FBufferSize := Value;

  if not Active then Exit;

  setsockopt(FSocket, SOL_SOCKET, SO_RCVBUF,
      @FBufferSize, SizeOf(FBufferSize));
end;

procedure TDRtpUdpSocket.SetLocalIP(const Value: string);
begin
  FLocalIP := Value;
end;

procedure TDRtpUdpSocket.SetLocalPort(const Value: Integer);
begin
  FLocalPort := Value;
end;

procedure TDRtpUdpSocket.SetReuseAddress(const Value: Boolean);
begin
  if FSocket = 0 then Exit;

  SetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR,  @Value,  SizeOf(Value));
end;

procedure TDRtpUdpSocket.SetStopTransfer(const Value: Boolean);
begin
  FStopTransfer := Value;
end;

procedure TDRtpUdpSocket.SetTimeOut(const Value: Integer);
begin
  FTimeOut := Value;

  if not Active then Exit;

  setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO,
      @FTimeOut, SizeOf(FTimeOut));
end;

function TDRtpUdpSocket.WaitForData(ATimeOut: Integer): Boolean;
var
  ReadReady, ExceptFlag: Boolean;
begin
  Result := False;
  if Select(FSocket, @ReadReady, nil, @ExceptFlag, ATimeOut) then
    Result := ReadReady and not ExceptFlag;
end;

end.
