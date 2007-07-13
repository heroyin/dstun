{
DStun

Description:
 A delphi librry for stun(rfc3489).

License:
 The contents of this file are subject to the Mozilla Public License
 Version 1.1 (the "License"); you may not use this file except in compliance
 with the License. You may obtain a copy of the License at
 http://www.mozilla.org/MPL/

Contact Details:
 EMail: heroyin@gmail.com

How to use:
  var
    DSClient: TDStunClient;
    DSResult: TDSResult;
  begin
    DSClient := TDStunClient.Create;
    try
      DSResult := DSClient.Query('stunserver.org', 3478);
    finally
      FreeAndNil(DSClient);
    end;
  end;


Change log:
 (2007-6-11):
  - First version by heroyin@gmail.com.
 (2007-7-13):
  - First BindingRequest add changeip Attribute, with false and false
}
unit DStun;

interface

uses Windows, SysUtils, Classes, WinSock, DSMessage, DSSocket;

type
  TDStunClient = class(TObject)
  private
    FBindAddr: TSockAddrIn;
    FLocalIP: string;
    FLocalPort: word;
    FStartLocalPort: word;
    FTimeOut: Integer;
    FUdpSocket: THandle;
    procedure BindSocket;
    function IsSameLocalAddress(AIP: TDSIPAddress): Boolean;
    function ReciveStream(AStream: TStream; AServer: String; APort: Integer):
        Boolean;
    function SendStream(AStream: TStream; AServer: String; APort: Integer): Boolean;
    procedure SetLocalPort(const Value: word);
  public
    constructor Create;
    destructor Destroy; override;
    function Query(AServer: string; APort: integer): TDSResult;
    function SendCommand(AMessage: IDSMessage; AServer: string; APort: Integer):
        IDSMessage; overload;
    function SendCommand(AMessage: IDSMessage; AIP: TDSIPAddress): IDSMessage;
        overload;
    property LocalIP: string read FLocalIP write FLocalIP;
    property LocalPort: word read FLocalPort write SetLocalPort;
    property TimeOut: Integer read FTimeOut write FTimeOut;
  end;


function IPAddressToString(AIP: TDSIPAddress): string;

function IPAdressToPort(AIP: TDSIPAddress): Word;

implementation

function IPAddressToString(AIP: TDSIPAddress): string;
begin
  Result := Format('%d.%d.%d.%d', [AIP.IP[0], AIP.IP[1], AIP.IP[2], AIP.IP[3]]);
end;

function IPAdressToPort(AIP: TDSIPAddress): Word;
begin
  Result := AIP.Port_hi shl 8 + AIP.Port_lo;
end;



{TDStunClient}

constructor TDStunClient.Create;
begin
  inherited;
  FStartLocalPort := 3000;
  FLocalIP := '0.0.0.0';
  FUdpSocket := Socket(AF_INET,SOCK_DGRAM,0);
  TimeOut := 3000;
end;

destructor TDStunClient.Destroy;
begin
  closesocket(FUdpSocket);
  inherited;
end;

procedure TDStunClient.BindSocket;
begin
  FBindAddr.sin_family := AF_INET;

  if LocalIP <> '' then
    FBindAddr.sin_addr.S_addr := Inet_Addr(PChar(FLocalIP))
  else
    FBindAddr.sin_addr.S_addr := INADDR_ANY;

  
  FLocalPort := FStartLocalPort;
  while FLocalPort < 65535 do
  begin
    FBindAddr.sin_port := htons(FLocalPort);
    if bind(FUdpSocket, FBindAddr, sizeof(FBindAddr)) <> SOCKET_ERROR then
    begin
      setsockopt(FUdpSocket, SOL_SOCKET, SO_RCVTIMEO,
        @FTimeOut, SizeOf(FTimeOut));
      Exit;
    end;
    inc(FLocalPort);
  end;

  raise Exception.Create('bind faild!');
end;

function TDStunClient.IsSameLocalAddress(AIP: TDSIPAddress): Boolean;
type
  TaPInAddr = array [0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  tmpBuf: array [0..255] of char;
  tmpHostEnt: PHostEnt;
  tmpPptr : PaPInAddr;
  I: Integer;
  tmpStr1, tmpStr2: String;
begin
  GetHostName(tmpBuf, SizeOf(tmpBuf));
  tmpHostEnt := GetHostByName(tmpBuf);
  tmpPptr := PaPInAddr(tmpHostEnt^.h_addr_list);

  I := 0;
  tmpStr1 := IPAddressToString(AIP);
  while tmpPptr^[I] <> nil do
  begin
    tmpStr2 := StrPas(inet_ntoa(tmpPptr^[I]^));
    if tmpStr1 = tmpStr2 then
    begin
      Result := True;
      Exit;
    end;
    Inc(I);
  end;

  Result := False;
end;

{
    In test I, the client sends a STUN Binding Request to a server, without any flags set in the
    CHANGE-REQUEST attribute, and without the RESPONSE-ADDRESS attribute. This causes the server
    to send the response back to the address and port that the request came from.

    In test II, the client sends a Binding Request with both the "change IP" and "change port" flags
    from the CHANGE-REQUEST attribute set.

    In test III, the client sends a Binding Request with only the "change port" flag set.

                        +--------+
                        |  Test  |
                        |   I    |
                        +--------+
                             |
                             |
                             V
                            /\              /\
                         N /  \ Y          /  \ Y             +--------+
          UDP     <-------/Resp\--------->/ IP \------------->|  Test  |
          Blocked         \ ?  /          \Same/              |   II   |
                           \  /            \? /               +--------+
                            \/              \/                    |
                                             | N                  |
                                             |                    V
                                             V                    /\
                                         +--------+  Sym.      N /  \
                                         |  Test  |  UDP    <---/Resp\
                                         |   II   |  Firewall   \ ?  /
                                         +--------+              \  /
                                             |                    \/
                                             V                     |Y
                  /\                         /\                    |
   Symmetric  N  /  \       +--------+   N  /  \                   V
      NAT  <--- / IP \<-----|  Test  |<--- /Resp\               Open
                \Same/      |   I    |     \ ?  /               Internet
                 \? /       +--------+      \  /
                  \/                         \/
                  |                           |Y
                  |                           |
                  |                           V
                  |                           Full
                  |                           Cone
                  V              /\
              +--------+        /  \ Y
              |  Test  |------>/Resp\---->Restricted
              |   III  |       \ ?  /
              +--------+        \  /
                                 \/
                                  |N
                                  |       Port
                                  +------>Restricted
}

function TDStunClient.Query(AServer: string; APort: integer): TDSResult;
var
  tmpRequest1, tmpRequest2, tmpRequest12, tmpRequest3,
  tmpResponse1, tmpResponse2, tmpResponse12, tmpResponse3: IDSMessage;
begin
  BindSocket;

  Result.NetType := dsntUdpBlocked;

  ///test 1(1)
  tmpRequest1 := TDSMessage.Create;
  tmpRequest1.MessageType := DSMT_BindingRequest;
  tmpRequest1.ChangeRequestAttribute := TDSChangeRequestAttribute.Create;
  (tmpRequest1.ChangeRequestAttribute as IDSAttribute).AttributeType := DSAT_ChangeRequest;
  tmpRequest1.ChangeRequestAttribute.ChangeIP := False;
  tmpRequest1.ChangeRequestAttribute.ChangePort := False;
  tmpResponse1 := SendCommand(tmpRequest1, AServer, APort);

  if tmpResponse1 <> nil then
  begin
    ///test 2
    tmpRequest2 := TDSMessage.Create;
    tmpRequest2.MessageType := DSMT_BindingRequest;
    tmpRequest2.ChangeRequestAttribute := TDSChangeRequestAttribute.Create;
    (tmpRequest2.ChangeRequestAttribute as IDSAttribute).AttributeType := DSAT_ChangeRequest;
    tmpRequest2.ChangeRequestAttribute.ChangeIP := True;
    tmpRequest2.ChangeRequestAttribute.ChangePort := True;

    if IsSameLocalAddress(tmpResponse1.MappedAddress.IPAddress) then
    begin
      ///no nat
      tmpResponse2 := SendCommand(tmpRequest2, AServer, APort);

      if tmpResponse2 <> nil then
      begin
        ///Open Internet
        Result.NetType := dsntOpenInternet;
        Result.PublicIP := tmpResponse2.MappedAddress.IPAddress;
      end else
      begin
        ///Symmetric UDP firewall
        Result.NetType := dsntSymmetricUdpFirewall;
        Result.PublicIP := tmpResponse1.MappedAddress.IPAddress;
      end;
    end else
    begin
      tmpResponse2 := SendCommand(tmpRequest2, AServer, APort);
     // if SameIPAddress(tmpResponse2.MappedAddress.IPAddress,
     //   tmpResponse1.MappedAddress.IPAddress) then
      if tmpResponse2 <> nil then 
      begin
        /// full cone nat
        Result.NetType := dsntFullCone;
        Result.PublicIP := tmpResponse2.MappedAddress.IPAddress;
      end else
      begin
        ///TEST 1(2)
        tmpRequest12 := TDSMessage.Create;
        tmpRequest12.MessageType := DSMT_BindingRequest;
        tmpRequest12.ChangeRequestAttribute := TDSChangeRequestAttribute.Create;
        (tmpRequest12.ChangeRequestAttribute as IDSAttribute).AttributeType := DSAT_ChangeRequest;
        tmpRequest12.ChangeRequestAttribute.ChangeIP := False;
        tmpRequest12.ChangeRequestAttribute.ChangePort := False;
        tmpResponse12 := SendCommand(tmpRequest12,
          tmpResponse1.ChangedAddress.IPAddress);
        if tmpResponse12 <> nil then
        begin
          ///Symmetric NAT
          if not SameIPAddress(tmpResponse12.MappedAddress.IPAddress,
            tmpResponse1.MappedAddress.IPAddress) then
          begin
            Result.NetType := dsntSymmetric;
            Result.PublicIP := tmpResponse1.MappedAddress.IPAddress;
          end else
          begin
            tmpRequest3 := TDSMessage.Create;
            tmpRequest3.MessageType := DSMT_BindingRequest;
            tmpRequest3.ChangeRequestAttribute := TDSChangeRequestAttribute.Create;
            (tmpRequest3.ChangeRequestAttribute as IDSAttribute).AttributeType := DSAT_ChangeRequest;
            tmpRequest3.ChangeRequestAttribute.ChangeIP := False;
            tmpRequest3.ChangeRequestAttribute.ChangePort := True;

            tmpResponse3 := SendCommand(tmpRequest3,
              tmpResponse1.ChangedAddress.IPAddress);
            if SameIPAddress(tmpResponse3.MappedAddress.IPAddress,
              tmpResponse1.MappedAddress.IPAddress) then
            begin
              /// Restricted
              Result.NetType := dsntRestrictedCone;
              Result.PublicIP := tmpResponse1.MappedAddress.IPAddress;
            end else
            begin
              ///map Restricted
              Result.NetType := dsntPortRestrictedCone;
              Result.PublicIP := tmpResponse1.MappedAddress.IPAddress;
            end;
          end;
        end;

      end;
    end;
  end;   
end;

function TDStunClient.ReciveStream(AStream: TStream; AServer: String; APort:
    Integer): Boolean;
var
  tmpBuf: array [0..512] of Byte;
  tmpSize: Integer;
  tmpAddr: TSockAddrIn;
  tmpAddrLength: Integer;
begin
  Result := False;
  FillChar(tmpBuf, SizeOf(tmpBuf), #0);
  tmpAddr := GetSocketAddr(AServer, APort);
  tmpAddrLength := SizeOf(tmpAddr);
  tmpSize := recvfrom(FUdpSocket, tmpBuf, Length(tmpBuf), 0, tmpAddr, tmpAddrLength);
  if tmpSize = SOCKET_ERROR then Exit;
  AStream.Write(tmpBuf, tmpSize);
  Result := tmpSize <> 0;
end;

function TDStunClient.SendStream(AStream: TStream; AServer: String; APort:
    Integer): Boolean;
var
  tmpBuf: array [0..511] of Char;
  tmpAddr: TSockAddrIn;
  tmpAddrLength: Integer;
begin
  AStream.Position := 0;
  AStream.Read(tmpBuf, AStream.Size);

    tmpAddr := GetSocketAddr(AServer, APort);
    tmpAddrLength := SizeOf(tmpAddr);
    Result := sendto(FUdpSocket, tmpBuf, AStream.Size, 0, tmpAddr, tmpAddrLength)
      <> SOCKET_ERROR;
//    FSocket.SendBuf(tmpBuf, AStream.Size);
end;

function TDStunClient.SendCommand(AMessage: IDSMessage; AServer: string; APort:
    Integer): IDSMessage;
var
  tmpStream: TStream;
  tmpStart: Cardinal;
  tmpMessage: IDSMessage;
begin
  Result := nil;


  tmpStream := TMemoryStream.Create;
  try
    tmpStart := GetTickCount;

    tmpStream.Size := 0;

    AMessage.Build(tmpStream);
    if not SendStream(tmpStream, AServer, APort) then Exit;

    while GetTickCount - tmpStart < 2000 do
    begin
      if WaitForData(FUdpSocket, 100, AServer, APort) then
      begin
        tmpStream.Size := 0;
        if not ReciveStream(tmpStream, AServer, APort) then continue;

        tmpMessage := TDSMessage.Create;
        tmpMessage.Parser(tmpStream);

        if SameGUID(AMessage.TransactionID, tmpMessage.TransactionID) then
        begin
          Result := tmpMessage;
          Exit;
        end;
      end;
    end;
  finally
    FreeAndNil(tmpStream);
  end;
end;

function TDStunClient.SendCommand(AMessage: IDSMessage; AIP: TDSIPAddress):
    IDSMessage;
begin
  Result := SendCommand(AMessage, IPAddressToString(AIP), IPAdressToPort(AIP));
end;

procedure TDStunClient.SetLocalPort(const Value: word);
begin
  FStartLocalPort := Value;
  FLocalPort := Value;
end;





end.
