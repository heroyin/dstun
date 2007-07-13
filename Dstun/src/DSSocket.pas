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

unit:
 stun socket 

Change log:
 (2007-6-11):
  - First version by heroyin@gmail.com.
}
unit DSSocket;

interface

uses
  SysUtils, WinSock;

function GetSocketAddr(h: String; p: Integer): TSockAddr;

function WaitForData(H: THandle; ATimeOut: Integer; AServer: String; APort:
    Integer): Boolean;

implementation

var
  WSAData: TWSAData;


procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    raise Exception.Create('WSAStartup');
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise Exception.Create('WSACleanup');
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
        if inet_addr(pchar(hn)) <> INADDR_NONE then
          Result := hn;
      end
      else
      begin
        h := gethostbyname(pchar(hn));
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
      se := getservbyname(pchar(sn), pchar(pn));
      if se <> nil then
        Result := ntohs(se^.s_port)
      else
        Result := StrToInt(sn);
    end;
  end;

begin
  Result.sin_family := AF_INET;
  Result.sin_addr.s_addr := inet_addr(pchar(LookupHostAddr(h)));
  Result.sin_port := htons(LookupPort(IntToStr(p)));
end;

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

function WaitForData(H: THandle; ATimeOut: Integer; AServer: String; APort:
    Integer): Boolean;
var
  ReadReady, ExceptFlag: Boolean;
begin
  Result := False;
  if Select(H, @ReadReady, nil, @ExceptFlag, ATimeOut) then
    Result := ReadReady and not ExceptFlag;
end;

initialization
  Startup;

finalization
  Cleanup;


end.
