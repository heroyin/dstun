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
 stun message 

Change log:
 (2007-6-11):
  - First version by heroyin@gmail.com.
}
unit DSMessage;

interface

uses Windows, SysUtils, Classes;

const
  ///********** STUN message type **********///

  /// STUN message is binding request.
  DSMT_BindingRequest            = $0001;
  /// STUN message is binding request response.
  DSMT_BindingResponse           = $0101;
  /// STUN message is binding requesr error response.
  DSMT_BindingErrorResponse      = $0111;
  /// STUN message is "shared secret" request.
  DSMT_SharedSecretRequest       = $0002;
  /// STUN message is "shared secret" request response.
  DSMT_SharedSecretResponse      = $0102;
  /// STUN message is "shared secret" request error response.
  DSMT_SharedSecretErrorResponse = $0112;


  ///********** STUN attribute type **********///

  DSAT_MappedAddress    = $0001;
  DSAT_ResponseAddress  = $0002;
  DSAT_ChangeRequest    = $0003;
  DSAT_SourceAddress    = $0004;
  DSAT_ChangedAddress   = $0005;
  DSAT_Username         = $0006;
  DSAT_Password         = $0007;
  DSAT_MessageIntegrity = $0008;
  DSAT_ErrorCode        = $0009;
  DSAT_UnknownAttribute = $000A;
  DSAT_ReflectedFrom    = $000B;
  DSAT_XorMappedAddress = $8020;
  DSAT_XorOnly          = $0021;
  DSAT_ServerName       = $8022;

  ///********** STUN IPFamily **********///

  DSIF_IPV4 = $01;
  DSIF_IPV6 = $02;

  
type

  TDSNetType = (
    /// UDP is always blocked.
    dsntUdpBlocked,
    /// No NAT, public IP, no firewall.
    dsntOpenInternet,
    /// No NAT, public IP, but symmetric UDP firewall.
    dsntSymmetricUdpFirewall,
    /// A full cone NAT is one where all requests from the same internal IP address and port are
    /// mapped to the same external IP address and port. Furthermore, any external host can send
    /// a packet to the internal host, by sending a packet to the mapped external address.
    dsntFullCone,
    /// A restricted cone NAT is one where all requests from the same internal IP address and
    /// port are mapped to the same external IP address and port. Unlike a full cone NAT, an external
    /// host (with IP address X) can send a packet to the internal host only if the internal host
    /// had previously sent a packet to IP address X.
    dsntRestrictedCone,
    /// A port restricted cone NAT is like a restricted cone NAT, but the restriction
    /// includes port numbers. Specifically, an external host can send a packet, with source IP
    /// address X and source port P, to the internal host only if the internal host had previously
    /// sent a packet to IP address X and port P.
    dsntPortRestrictedCone,
    /// A symmetric NAT is one where all requests from the same internal IP address and port,
    /// to a specific destination IP address and port, are mapped to the same external IP address and
    /// port.  If the same host sends a packet with the same source address and port, but to
    /// a different destination, a different mapping is used. Furthermore, only the external host that
    /// receives a packet can send a UDP packet back to the internal host.
    dsntSymmetric
  );

  TDSIPAddress = packed record
    UnUsed: Byte;
    Family: Byte;
    Port_hi: Byte;
    Port_lo: Byte;
    IP: array [0..3] of byte;
  end;

  TDSErrorCode = record
    Code: integer;
    Reason: string;
  end;

  TDSResult = record
    NetType: TDSNetType;
    PublicIP: TDSIPAddress;
  end;


type

  IDSAttribute = interface
  ['{D30F83F3-90CC-49D9-9BAF-80667D22E56A}']
    procedure Build(AStream: TStream); stdcall;
    procedure Parser(AStream: TStream); stdcall;
    function GetAttributeLength: Word; stdcall;
    function GetAttributeType: Word; stdcall;
    function GetHeadLength: Integer; stdcall;
    procedure SetAttributeType(const Value: Word); stdcall;
    property AttributeLength: Word read GetAttributeLength;
    property AttributeType: Word read GetAttributeType write SetAttributeType;
    property HeadLength: Integer read GetHeadLength;
  end;

  IDSAddressAttribute = interface
  ['{720336AC-9AFB-47DE-9248-5C38F35E2259}']
    function GetIPAddress: TDSIPAddress; stdcall;
    procedure SetIPAddress(const Value: TDSIPAddress); stdcall;
    property IPAddress: TDSIPAddress read GetIPAddress write SetIPAddress;
  end;

  IDSChangeRequestAttribute = interface
  ['{4292B9AB-591E-4D9F-BD1E-F855D230B475}']
    function GetChangeIP: Boolean; stdcall;
    function GetChangePort: Boolean; stdcall;
    procedure SetChangeIP(const Value: Boolean); stdcall;
    procedure SetChangePort(const Value: Boolean); stdcall;
    property ChangeIP: Boolean read GetChangeIP write SetChangeIP;
    property ChangePort: Boolean read GetChangePort write SetChangePort;
  end;

  IDSStringAttribute = interface
  ['{79DD3703-3879-49C5-B252-07F1BDCD30D5}']
    function GetStringValue: string; stdcall;
    procedure SetStringValue(const Value: string); stdcall;
    property StringValue: string read GetStringValue write SetStringValue;

  end;

  IDSErrorAttribute = interface
  ['{E74044FA-35F6-4230-BBF0-948091289FB5}']
    function GetClasses: Byte; stdcall;
    function GetNumber: Word; stdcall;
    function GetReason: string; stdcall;
    procedure SetClasses(const Value: Byte); stdcall;
    procedure SetNumber(const Value: Word); stdcall;
    procedure SetReason(const Value: string); stdcall;
    property Classes: Byte read GetClasses write SetClasses;
    property Number: Word read GetNumber write SetNumber;
    property Reason: string read GetReason write SetReason;
  end;

  IDSMessage = interface
  ['{2229958A-01EA-4693-BD17-806274EF370F}']
    procedure Build(AStream: TStream);
    function GetChangedAddress: IDSAddressAttribute; stdcall;
    function GetChangeRequestAttribute: IDSChangeRequestAttribute; stdcall;
    function GetErrorAttribute: IDSErrorAttribute; stdcall;
    function GetHeadLength: Integer;
    function GetMappedAddress: IDSAddressAttribute; stdcall;
    function GetMessageLength: Integer; stdcall;
    function GetMessageType: Word; stdcall;
    function GetPassword: IDSStringAttribute; stdcall;
    function GetReflectedFrom: IDSAddressAttribute; stdcall;
    function GetResponseAddress: IDSAddressAttribute; stdcall;
    function GetServerName: IDSStringAttribute; stdcall;
    function GetSourceAddress: IDSAddressAttribute; stdcall;
    function GetTransactionID: TGUID; stdcall;
    function GetUserName: IDSStringAttribute; stdcall;
    function GetXorMappedAddress: IDSAddressAttribute; stdcall;
    function GetXorOnly: IDSStringAttribute; stdcall;
    procedure Parser(AStream: TStream);
    procedure SetChangedAddress(const Value: IDSAddressAttribute); stdcall;
    procedure SetChangeRequestAttribute(const Value: IDSChangeRequestAttribute);
        stdcall;
    procedure SetErrorAttribute(const Value: IDSErrorAttribute); stdcall;
    procedure SetMappedAddress(const Value: IDSAddressAttribute); stdcall;
    procedure SetMessageType(const Value: Word); stdcall;
    procedure SetPassword(const Value: IDSStringAttribute); stdcall;
    procedure SetReflectedFrom(const Value: IDSAddressAttribute); stdcall;
    procedure SetResponseAddress(const Value: IDSAddressAttribute); stdcall;
    procedure SetServerName(const Value: IDSStringAttribute); stdcall;
    procedure SetSourceAddress(const Value: IDSAddressAttribute); stdcall;
    procedure SetUserName(const Value: IDSStringAttribute); stdcall;
    procedure SetXorMappedAddress(const Value: IDSAddressAttribute); stdcall;
    procedure SetXorOnly(const Value: IDSStringAttribute); stdcall;
    property ChangedAddress: IDSAddressAttribute read GetChangedAddress write
        SetChangedAddress;
    property ChangeRequestAttribute: IDSChangeRequestAttribute read
        GetChangeRequestAttribute write SetChangeRequestAttribute;
    property ErrorAttribute: IDSErrorAttribute read GetErrorAttribute write
        SetErrorAttribute;
    property HeadLength: Integer read GetHeadLength;
    property MappedAddress: IDSAddressAttribute read GetMappedAddress write
        SetMappedAddress;
    property MessageLength: Integer read GetMessageLength;
    property MessageType: Word read GetMessageType write SetMessageType;
    property Password: IDSStringAttribute read GetPassword write SetPassword;
    property ReflectedFrom: IDSAddressAttribute read GetReflectedFrom write
        SetReflectedFrom;
    property ResponseAddress: IDSAddressAttribute read GetResponseAddress write
        SetResponseAddress;
    property ServerName: IDSStringAttribute read GetServerName write SetServerName;
    property SourceAddress: IDSAddressAttribute read GetSourceAddress write
        SetSourceAddress;
    property TransactionID: TGUID read GetTransactionID;
    property UserName: IDSStringAttribute read GetUserName write SetUserName;
    property XorMappedAddress: IDSAddressAttribute read GetXorMappedAddress write
        SetXorMappedAddress;
    property XorOnly: IDSStringAttribute read GetXorOnly write SetXorOnly;

  end;

  ///////////////////////////////classes///////////////////////////

  TDSAttribute = class(TInterfacedObject, IDSAttribute)
  private
    FAttributeLength: Word;
    FAttributeType: Word;
  protected
    function GetAttributeLength: Word; stdcall;
    function GetAttributeType: Word; stdcall;
    function GetHeadLength: Integer; stdcall;
    procedure SetAttributeType(const Value: Word); stdcall;
    procedure Build(AStream: TStream); virtual; stdcall;
    procedure Parser(AStream: TStream); virtual; stdcall;
    property AttributeLength: Word read GetAttributeLength;
    property AttributeType: Word read GetAttributeType write SetAttributeType;
    property HeadLength: Integer read GetHeadLength;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TDSAddressAttribute = class(TDSAttribute, IDSAddressAttribute)
  private
    FIPAddress: TDSIPAddress;
  protected
    function GetIPAddress: TDSIPAddress; stdcall;
    procedure SetIPAddress(const Value: TDSIPAddress); stdcall;
    procedure Build(AStream: TStream); override;
    procedure Parser(AStream: TStream); override;
    property IPAddress: TDSIPAddress read GetIPAddress write SetIPAddress;
  public
    constructor Create; override;
  end;

  TDSChangeRequestAttribute = class(TDSAttribute, IDSChangeRequestAttribute)
  private
    FChangeIP: Boolean;
    FChangePort: Boolean;
  protected
    function GetChangeIP: Boolean; stdcall;
    function GetChangePort: Boolean; stdcall;
    procedure SetChangeIP(const Value: Boolean); stdcall;
    procedure SetChangePort(const Value: Boolean); stdcall;
    procedure Build(AStream: TStream); override;
    procedure Parser(AStream: TStream); override;
    property ChangeIP: Boolean read GetChangeIP write SetChangeIP;
    property ChangePort: Boolean read GetChangePort write SetChangePort;
  public
    constructor Create; override;
  end;

  TDSStringAttribute = class(TDSAttribute, IDSStringAttribute)
  private
    FStringValue: string;
  protected
    function GetStringValue: string; stdcall;
    procedure SetStringValue(const Value: string); stdcall;
    procedure Build(AStream: TStream); override;
    procedure Parser(AStream: TStream); override;
    property StringValue: string read GetStringValue write SetStringValue;
  public
    constructor Create; override;
  end;
  
  TDSErrorAttribute = class(TDSAttribute, IDSErrorAttribute)
  private
    FClasses: Byte;
    FNumber: Word;
    FReason: string;
  protected
    function GetClasses: Byte; stdcall;
    function GetNumber: Word; stdcall;
    function GetReason: string; stdcall;
    procedure SetClasses(const Value: Byte); stdcall;
    procedure SetNumber(const Value: Word); stdcall;
    procedure SetReason(const Value: string); stdcall;
    procedure Build(AStream: TStream); override;
    procedure Parser(AStream: TStream); override;
    property Classes: Byte read GetClasses write SetClasses;
    property Number: Word read GetNumber write SetNumber;
    property Reason: string read GetReason write SetReason;
  public
    constructor Create; override;
  end;


  TDSMessage = class(TInterfacedObject, IDSMessage)
  private
    FUserName: IDSStringAttribute;
    FPassword: IDSStringAttribute;
    FReflectedFrom: IDSAddressAttribute;
    FServerName: IDSStringAttribute;
    FXorMappedAddress: IDSAddressAttribute;
    FXorOnly: IDSStringAttribute;
    FResponseAddress: IDSAddressAttribute;
    FSourceAddress: IDSAddressAttribute;
    FChangedAddress: IDSAddressAttribute;
    FChangeRequestAttribute: IDSChangeRequestAttribute;
    FErrorAttribute: IDSErrorAttribute;
    FMappedAddress: IDSAddressAttribute;
    FMessageType: Word;
    FTransactionID: TGUID;
    function GetHeadLength: Integer;
  protected
    function GetChangedAddress: IDSAddressAttribute; stdcall;
    function GetChangeRequestAttribute: IDSChangeRequestAttribute; stdcall;
    function GetErrorAttribute: IDSErrorAttribute; stdcall;
    function GetMappedAddress: IDSAddressAttribute; stdcall;
    function GetMessageLength: Integer; stdcall;
    function GetMessageType: Word; stdcall;
    function GetPassword: IDSStringAttribute; stdcall;
    function GetReflectedFrom: IDSAddressAttribute; stdcall;
    function GetResponseAddress: IDSAddressAttribute; stdcall;
    function GetServerName: IDSStringAttribute; stdcall;
    function GetSourceAddress: IDSAddressAttribute; stdcall;
    function GetTransactionID: TGUID; stdcall;
    function GetUserName: IDSStringAttribute; stdcall;
    function GetXorMappedAddress: IDSAddressAttribute; stdcall;
    function GetXorOnly: IDSStringAttribute; stdcall;
    procedure SetChangedAddress(const Value: IDSAddressAttribute); stdcall;
    procedure SetChangeRequestAttribute(const Value: IDSChangeRequestAttribute);
        stdcall;
    procedure SetErrorAttribute(const Value: IDSErrorAttribute); stdcall;
    procedure SetMappedAddress(const Value: IDSAddressAttribute); stdcall;
    procedure SetMessageType(const Value: Word); stdcall;
    procedure SetPassword(const Value: IDSStringAttribute); stdcall;
    procedure SetReflectedFrom(const Value: IDSAddressAttribute); stdcall;
    procedure SetResponseAddress(const Value: IDSAddressAttribute); stdcall;
    procedure SetServerName(const Value: IDSStringAttribute); stdcall;
    procedure SetSourceAddress(const Value: IDSAddressAttribute); stdcall;
    procedure SetUserName(const Value: IDSStringAttribute); stdcall;
    procedure SetXorMappedAddress(const Value: IDSAddressAttribute); stdcall;
    procedure SetXorOnly(const Value: IDSStringAttribute); stdcall;
  public
    constructor Create;
    procedure Build(AStream: TStream);
    procedure Parser(AStream: TStream);
    property ChangedAddress: IDSAddressAttribute read GetChangedAddress write
        SetChangedAddress;
    property ChangeRequestAttribute: IDSChangeRequestAttribute read
        GetChangeRequestAttribute write SetChangeRequestAttribute;
    property ErrorAttribute: IDSErrorAttribute read GetErrorAttribute write
        SetErrorAttribute;
    property HeadLength: Integer read GetHeadLength;
    property MappedAddress: IDSAddressAttribute read GetMappedAddress write
        SetMappedAddress;
    property MessageLength: Integer read GetMessageLength;
    property MessageType: Word read GetMessageType write SetMessageType;
    property Password: IDSStringAttribute read GetPassword write SetPassword;
    property ReflectedFrom: IDSAddressAttribute read GetReflectedFrom write
        SetReflectedFrom;
    property ResponseAddress: IDSAddressAttribute read GetResponseAddress write
        SetResponseAddress;
    property ServerName: IDSStringAttribute read GetServerName write SetServerName;
    property SourceAddress: IDSAddressAttribute read GetSourceAddress write
        SetSourceAddress;
    property TransactionID: TGUID read GetTransactionID;
    property UserName: IDSStringAttribute read GetUserName write SetUserName;
    property XorMappedAddress: IDSAddressAttribute read GetXorMappedAddress write
        SetXorMappedAddress;
    property XorOnly: IDSStringAttribute read GetXorOnly write SetXorOnly;
  end;


procedure WriteWord(AStream: TStream; AWord: Word);

function ReadWord(AStream: TStream): Word;

function ReadIPAddress(AStream: TStream): TDSIPAddress;

function ReadString(AStream: TStream): string;

function SameIPAddress(IP1, IP2: TDSIPAddress): Boolean;

function SameGUID(GUID1, GUID2: TGUID): Boolean;

function IPAddressToString(AIP: TDSIPAddress): string;

function IPAdressToPort(AIP: TDSIPAddress): Word;

implementation

procedure WriteWord(AStream: TStream; AWord: Word);
var
  tmpByte: Byte;
begin
  tmpByte := Hi(AWord);
  AStream.Write(tmpByte, SizeOf(tmpByte));
  tmpByte := Lo(AWord);
  AStream.Write(tmpByte, SizeOf(tmpByte));
end;

function ReadWord(AStream: TStream): Word;
var
  tmpLowByte, tmpHighByte: Byte;
begin
  AStream.Read(tmpHighByte, SizeOf(tmpHighByte));
  AStream.Read(tmpLowByte, SizeOf(tmpLowByte));
  Result := (tmpHighByte shl 8) or tmpLowByte;
end;

function ReadIPAddress(AStream: TStream): TDSIPAddress;
begin
  FillChar(Result, SizeOf(Result), #0);
  AStream.Position := 1;
  AStream.Read(Result, SizeOf(Result));
end;

function ReadString(AStream: TStream): string;
var
  tmpChar: array [0..255] of char;
begin
  AStream.Read(tmpChar, Length(tmpChar));
  Result := tmpChar;
end;

function SameIPAddress(IP1, IP2: TDSIPAddress): Boolean;
begin
  Result := (IP1.Family = IP2.Family) and
            (IP1.Port_hi = IP2.Port_hi) and
            (IP1.Port_lo = IP2.Port_lo) and
            (IP1.IP[0] = IP2.IP[0]) and
            (IP1.IP[1] = IP2.IP[1]) and
            (IP1.IP[2] = IP2.IP[2]) and
            (IP1.IP[3] = IP2.IP[3]);
end;

function SameGUID(GUID1, GUID2: TGUID): Boolean;
begin
  Result := (GUID1.D1 = GUID2.D1) and
            (GUID1.D2 = GUID2.D2) and
            (GUID1.D3 = GUID2.D3) and
            (GUID1.D4[0] = GUID2.D4[0]) and
            (GUID1.D4[1] = GUID2.D4[1]) and
            (GUID1.D4[2] = GUID2.D4[2]) and
            (GUID1.D4[3] = GUID2.D4[3]) and
            (GUID1.D4[4] = GUID2.D4[4]) and
            (GUID1.D4[5] = GUID2.D4[5]) and
            (GUID1.D4[6] = GUID2.D4[6]) and
            (GUID1.D4[7] = GUID2.D4[7]);
end;

function IPAddressToString(AIP: TDSIPAddress): string;
begin
  Result := Format('%d.%d.%d.%d', [AIP.IP[0], AIP.IP[1], AIP.IP[2], AIP.IP[3]]);
end;

function IPAdressToPort(AIP: TDSIPAddress): Word;
begin
  Result := AIP.Port_hi shl 8 + AIP.Port_lo;
end;

{
  TDSAttribute

  RFC 3489 11.2.
      Each attribute is TLV encoded, with a 16 bit type, 16 bit AttrLength, and variable value:

      0                   1                   2                   3
      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     |         Type                  |            AttrLength             |
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     |                             Value                             ....
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

constructor TDSAttribute.Create;
begin
  inherited;
  FAttributeLength := 0;
end;

destructor TDSAttribute.Destroy;
begin
  inherited;
end;

procedure TDSAttribute.Build(AStream: TStream);
begin
  WriteWord(AStream, AttributeType);
  WriteWord(AStream, AttributeLength);
end;

function TDSAttribute.GetAttributeLength: Word;
begin
  Result := FAttributeLength;
end;

function TDSAttribute.GetAttributeType: Word;
begin
  Result := FAttributeType;
end;

function TDSAttribute.GetHeadLength: Integer;
begin
  Result := 4;
end;

procedure TDSAttribute.SetAttributeType(const Value: Word);
begin
  FAttributeType := Value;
end;

procedure TDSAttribute.Parser(AStream: TStream);
begin
  FAttributeType := ReadWord(AStream);
  FAttributeLength := ReadWord(AStream);
end;

{

  TDSAddressAttribute

      It consists of an eight bit address family, and a sixteen bit
      port, followed by a fixed length value representing the IP address.

      0                   1                   2                   3
      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |x x x x x x x x|    Family     |           Port                |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |                             Address                           |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

constructor TDSAddressAttribute.Create;
begin
  inherited;
  FillChar(FIPAddress, SizeOf(FIPAddress), #0);
  FAttributeLength := FAttributeLength + SizeOf(FIPAddress);
end;

procedure TDSAddressAttribute.Build(AStream: TStream);
begin
  inherited;
  AStream.Write(FIPAddress, SizeOf(FIPAddress));
end;

function TDSAddressAttribute.GetIPAddress: TDSIPAddress;
begin
  Result := FIPAddress;
end;

procedure TDSAddressAttribute.SetIPAddress(const Value: TDSIPAddress);
begin
  FIPAddress := Value;
end;

procedure TDSAddressAttribute.Parser(AStream: TStream);
begin
  inherited;
  AStream.Read(FIPAddress, SizeOf(FIPAddress));

end;

{
  TDSChangeRequestAttribute

      The CHANGE-REQUEST attribute is used by the client to request that
      the server use a different address and/or port when sending the
      response.  The attribute is 32 bits long, although only two bits (A
      and B) are used:

       0                   1                   2                   3
       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 A B 0|
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

      The meaning of the flags is:

      A: This is the "change IP" flag.  If true, it requests the server
         to send the Binding Response with a different IP address than the
         one the Binding Request was received on.

      B: This is the "change port" flag.  If true, it requests the
         server to send the Binding Response with a different port than the
         one the Binding Request was received on.

}

constructor TDSChangeRequestAttribute.Create;
begin
  inherited;
  FChangeIP := False;
  FChangePort := False;
  FAttributeLength := FAttributeLength + 4;
end;

procedure TDSChangeRequestAttribute.Build(AStream: TStream);
var
  tmpWord: Integer;
begin
  inherited;
  tmpWord := 0;
  if FChangeIP then
    tmpWord := tmpWord or $4000000;      //0000 0000 0000 0100
  if FChangePort then
    tmpWord := tmpWord or $2000000;      //0000 0000 0000 0010
  AStream.Write(tmpWord, SizeOf(tmpWord));
end;

function TDSChangeRequestAttribute.GetChangeIP: Boolean;
begin
  Result := FChangeIP;
end;

function TDSChangeRequestAttribute.GetChangePort: Boolean;
begin
  Result := FChangePort;
end;

procedure TDSChangeRequestAttribute.Parser(AStream: TStream);
var
  tmpWord: Integer;
begin
  inherited;

  AStream.Read(tmpWord, SizeOf(tmpWord));
  FChangeIP := (tmpWord and $40000000) <> 0;
  FChangePort := (tmpWord and $20000000) <> 0;
end;

procedure TDSChangeRequestAttribute.SetChangeIP(const Value: Boolean);
begin
  FChangeIP := Value;
end;

procedure TDSChangeRequestAttribute.SetChangePort(const Value: Boolean);
begin
  FChangePort := Value;
end;

{
  TDSStringAttribute

}

constructor TDSStringAttribute.Create;
begin
  inherited;
  FStringValue := '';
end;

procedure TDSStringAttribute.Build(AStream: TStream);
begin
  inherited;
  AStream.Write(FStringValue, Length(FStringValue) + 1);
end;

function TDSStringAttribute.GetStringValue: string;
begin
  Result := FStringValue;
end;

procedure TDSStringAttribute.Parser(AStream: TStream);
begin
  inherited;
  StringValue := ReadString(AStream);
end;

procedure TDSStringAttribute.SetStringValue(const Value: string);
begin
  FStringValue := Value;
  FAttributeLength := Length(FStringValue) + 1;
end;


{
  TDSErrorAttribute

}

constructor TDSErrorAttribute.Create;
begin
  inherited;
  FClasses := 0;
  FNumber := 0;
  FReason := '';

  /// 3+length(FReason)
  FAttributeLength := FAttributeLength + 3;
end;

procedure TDSErrorAttribute.Build(AStream: TStream);
begin
  inherited;

  AStream.Write(FClasses, SizeOf(FClasses));
  WriteWord(AStream, FNumber);
  AStream.Write(FReason, Length(FReason) + 1);
end;

procedure TDSErrorAttribute.Parser(AStream: TStream);
begin
  inherited;

  AStream.Read(FClasses, SizeOf(FClasses));
  FNumber := ReadWord(AStream);
  Reason := ReadString(AStream);
end;

function TDSErrorAttribute.GetClasses: Byte;
begin
  Result := FClasses;
end;

function TDSErrorAttribute.GetNumber: Word;
begin
  Result := FNumber;
end;

function TDSErrorAttribute.GetReason: string;
begin
  Result := FReason;
end;

procedure TDSErrorAttribute.SetClasses(const Value: Byte);
begin
  FClasses := Value;
end;

procedure TDSErrorAttribute.SetNumber(const Value: Word);
begin
  FNumber := Value;
end;

procedure TDSErrorAttribute.SetReason(const Value: string);
begin
  FReason := Value;

  FAttributeLength := 3 + Length(FReason) + 1;
end;


{
  TDSMessage

      RFC 3489 11.1.
      All STUN messages consist of a 20 byte header:

      0                   1                   2                   3
      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     |      STUN Message Type        |         Message Length        |
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     |
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
                              Transaction ID
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
                                                                     |
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

     The message length is the count, in bytes, of the size of the
     message, not including the 20 byte header.  
}

constructor TDSMessage.Create;
begin
  inherited;
  CreateGUID(FTransactionID);
  FMessageType := 0;
end;

function TDSMessage.GetHeadLength: Integer;
begin
  Result := 20;
end;

procedure TDSMessage.Build(AStream: TStream);

  procedure WriteAttribute(AInterface: IInterface);
  var
    tmpAttr: IDSAttribute;
  begin
    if AInterface.QueryInterface(IDSAttribute, tmpAttr) = S_OK then
      tmpAttr.Build(AStream);
  end;
  
begin
  ///null stream
  AStream.Size := 0;

  WriteWord(AStream, MessageType);
  ///write length at last
  WriteWord(AStream, 0);
  AStream.Write(FTransactionID, SizeOf(FTransactionID));

  if FUserName <> nil then
    (FUserName as IDSAttribute).Build(AStream);
  if FPassword <> nil then
    (FPassword as IDSAttribute).Build(AStream);
  if FReflectedFrom <> nil then
    (FReflectedFrom as IDSAttribute).Build(AStream);
  if FServerName <> nil then
    (FServerName as IDSAttribute).Build(AStream);
  if FXorMappedAddress <> nil then
    (FXorMappedAddress as IDSAttribute).Build(AStream);
  if FXorOnly <> nil then
    (FXorOnly as IDSAttribute).Build(AStream);
  if FResponseAddress <> nil then
    (FResponseAddress as IDSAttribute).Build(AStream);
  if FSourceAddress <> nil then
    (FSourceAddress as IDSAttribute).Build(AStream);
  if FChangedAddress <> nil then
    (FChangedAddress as IDSAttribute).Build(AStream);
  if FChangeRequestAttribute <> nil then
    (FChangeRequestAttribute as IDSAttribute).Build(AStream);
  if FErrorAttribute <> nil then
    (FErrorAttribute as IDSAttribute).Build(AStream);
  if FMappedAddress <> nil then
    (FMappedAddress as IDSAttribute).Build(AStream);

  AStream.Position := 2;
  WriteWord(AStream, MessageLength);
end;

function TDSMessage.GetChangedAddress: IDSAddressAttribute;
begin
  Result := FChangedAddress;
end;

function TDSMessage.GetChangeRequestAttribute: IDSChangeRequestAttribute;
begin
  Result := FChangeRequestAttribute;
end;

function TDSMessage.GetErrorAttribute: IDSErrorAttribute;
begin
  Result := FErrorAttribute;
end;

function TDSMessage.GetMappedAddress: IDSAddressAttribute;
begin
  Result := FMappedAddress;
end;

function TDSMessage.GetMessageLength: Integer;
begin
  Result := 0;

  if FUserName <> nil then
    Result := Result +
      (FUserName as IDSAttribute).HeadLength +
      (FUserName as IDSAttribute).AttributeLength;
  if FPassword <> nil then
    Result := Result +
      (FPassword as IDSAttribute).HeadLength +
      (FPassword as IDSAttribute).AttributeLength;
  if FReflectedFrom <> nil then
    Result := Result +
      (FReflectedFrom as IDSAttribute).HeadLength +
      (FReflectedFrom as IDSAttribute).AttributeLength;
  if FServerName <> nil then
    Result := Result +
      (FServerName as IDSAttribute).HeadLength +
      (FServerName as IDSAttribute).AttributeLength;
  if FXorMappedAddress <> nil then
    Result := Result +
      (FXorMappedAddress as IDSAttribute).HeadLength +
      (FXorMappedAddress as IDSAttribute).AttributeLength;
  if FXorOnly <> nil then
    Result := Result +
      (FXorOnly as IDSAttribute).HeadLength +
      (FXorOnly as IDSAttribute).AttributeLength;
  if FResponseAddress <> nil then
    Result := Result +
      (FResponseAddress as IDSAttribute).HeadLength +
      (FResponseAddress as IDSAttribute).AttributeLength;
  if FSourceAddress <> nil then
    Result := Result +
      (FSourceAddress as IDSAttribute).HeadLength +
      (FSourceAddress as IDSAttribute).AttributeLength;
  if FChangedAddress <> nil then
    Result := Result +
      (FChangedAddress as IDSAttribute).HeadLength +
      (FChangedAddress as IDSAttribute).AttributeLength;
  if FChangeRequestAttribute <> nil then
    Result := Result +
      (FChangeRequestAttribute as IDSAttribute).HeadLength +
      (FChangeRequestAttribute as IDSAttribute).AttributeLength;
  if FErrorAttribute <> nil then
    Result := Result +
      (FErrorAttribute as IDSAttribute).HeadLength +
      (FErrorAttribute as IDSAttribute).AttributeLength;
  if FMappedAddress <> nil then
    Result := Result +
      (FMappedAddress as IDSAttribute).HeadLength +
      (FMappedAddress as IDSAttribute).AttributeLength;
end;

function TDSMessage.GetMessageType: Word;
begin
  Result := FMessageType;
end;

function TDSMessage.GetPassword: IDSStringAttribute;
begin
  Result := FPassword;
end;

function TDSMessage.GetReflectedFrom: IDSAddressAttribute;
begin
  Result := FReflectedFrom;
end;

function TDSMessage.GetResponseAddress: IDSAddressAttribute;
begin
  Result := FResponseAddress;
end;

function TDSMessage.GetServerName: IDSStringAttribute;
begin
  Result := FServerName;
end;

function TDSMessage.GetSourceAddress: IDSAddressAttribute;
begin
  Result := SourceAddress;
end;

function TDSMessage.GetTransactionID: TGUID;
begin
  Result := FTransactionID;
end;

function TDSMessage.GetUserName: IDSStringAttribute;
begin
  Result := FUserName;
end;

function TDSMessage.GetXorMappedAddress: IDSAddressAttribute;
begin
  Result := FXorMappedAddress;
end;

function TDSMessage.GetXorOnly: IDSStringAttribute;
begin
  Result := FXorOnly;
end;

procedure TDSMessage.Parser(AStream: TStream);
var
  tmpAttrType: Word;
  tmpAddrAttr: TDSAddressAttribute;
  tmpRequestAttr: TDSChangeRequestAttribute;
  tmpStringAttr: TDSStringAttribute;
  tmpErrorAttr: TDSErrorAttribute;
  tmpAttrLenth, tmpMessageLength: Word;
begin
  AStream.Position := 0;
  FMessageType := ReadWord(AStream);
  tmpMessageLength := ReadWord(AStream);
  AStream.Read(FTransactionID, SizeOf(FTransactionID));

  while AStream.Position < (tmpMessageLength + HeadLength) do
  begin
    tmpAttrType := ReadWord(AStream);
    tmpAttrLenth := ReadWord(AStream);
    AStream.Position := AStream.Position - SizeOf(tmpAttrType) - SizeOf(tmpAttrLenth);

    case tmpAttrType of
      DSAT_MappedAddress:
      begin
        tmpAddrAttr := TDSAddressAttribute.Create;
        tmpAddrAttr.Parser(AStream);
        FMappedAddress := tmpAddrAttr;
      end;
      DSAT_ResponseAddress:
      begin
        tmpAddrAttr := TDSAddressAttribute.Create;
        tmpAddrAttr.Parser(AStream);
        FResponseAddress := tmpAddrAttr;
      end;
      DSAT_ChangeRequest:
      begin
        tmpRequestAttr := TDSChangeRequestAttribute.Create;
        tmpRequestAttr.Parser(AStream);
        FChangeRequestAttribute := tmpRequestAttr;
      end;
      DSAT_SourceAddress:
      begin
        tmpAddrAttr := TDSAddressAttribute.Create;
        tmpAddrAttr.Parser(AStream);
        FSourceAddress := tmpAddrAttr;
      end;
      DSAT_ChangedAddress: 
      begin
        tmpAddrAttr := TDSAddressAttribute.Create;
        tmpAddrAttr.Parser(AStream);
        FChangedAddress := tmpAddrAttr;
      end;
      DSAT_Username:
      begin
        tmpStringAttr := TDSStringAttribute.Create;
        tmpStringAttr.Parser(AStream);
        FUserName := tmpStringAttr;
      end;
      DSAT_Password:
      begin
        tmpStringAttr := TDSStringAttribute.Create;
        tmpStringAttr.Parser(AStream);
        FPassword := tmpStringAttr;
      end;
      DSAT_MessageIntegrity:
        ///ignore
        AStream.Position := AStream.Position + tmpAttrLenth;
      DSAT_ErrorCode:
      begin
        tmpErrorAttr := TDSErrorAttribute.Create;
        tmpErrorAttr.Parser(AStream);
        FErrorAttribute := tmpErrorAttr;
      end;
      DSAT_UnknownAttribute:
        ///ignore
        AStream.Position := AStream.Position + tmpAttrLenth;
      DSAT_ReflectedFrom:
      begin
        tmpAddrAttr := TDSAddressAttribute.Create;
        tmpAddrAttr.Parser(AStream);
        FReflectedFrom := tmpAddrAttr;
      end;
      DSAT_XorMappedAddress:
      begin
        tmpAddrAttr := TDSAddressAttribute.Create;
        tmpAddrAttr.Parser(AStream);
        FXorMappedAddress := tmpAddrAttr;
      end;
      DSAT_XorOnly:
      begin
        tmpStringAttr := TDSStringAttribute.Create;
        tmpStringAttr.Parser(AStream);
        FXorOnly := tmpStringAttr;
      end;
      DSAT_ServerName:
      begin
        tmpStringAttr := TDSStringAttribute.Create;
        tmpStringAttr.Parser(AStream);
        FServerName := tmpStringAttr;
      end;
      else
        ///ignore
        AStream.Position := AStream.Position + tmpAttrLenth;
    end;
  end;    
end;

procedure TDSMessage.SetChangedAddress(const Value: IDSAddressAttribute);
begin
  FChangedAddress := Value;
end;

procedure TDSMessage.SetChangeRequestAttribute(const Value:
    IDSChangeRequestAttribute);
begin
  FChangeRequestAttribute := Value;
end;

procedure TDSMessage.SetErrorAttribute(const Value: IDSErrorAttribute);
begin
  FErrorAttribute := Value;
end;

procedure TDSMessage.SetMappedAddress(const Value: IDSAddressAttribute);
begin
  FMappedAddress := Value;
end;

procedure TDSMessage.SetMessageType(const Value: Word);
begin
  FMessageType := Value;
end;

procedure TDSMessage.SetPassword(const Value: IDSStringAttribute);
begin
  FPassword := Value;
end;

procedure TDSMessage.SetReflectedFrom(const Value: IDSAddressAttribute);
begin
  FReflectedFrom := Value;
end;

procedure TDSMessage.SetResponseAddress(const Value: IDSAddressAttribute);
begin
  FResponseAddress := Value;
end;

procedure TDSMessage.SetServerName(const Value: IDSStringAttribute);
begin
  FServerName := Value;
end;

procedure TDSMessage.SetSourceAddress(const Value: IDSAddressAttribute);
begin
  FSourceAddress := Value;
end;

procedure TDSMessage.SetUserName(const Value: IDSStringAttribute);
begin
  FUserName := Value;
end;

procedure TDSMessage.SetXorMappedAddress(const Value: IDSAddressAttribute);
begin
  FXorMappedAddress := Value;
end;

procedure TDSMessage.SetXorOnly(const Value: IDSStringAttribute);
begin
  FXorOnly := Value;
end;


end.
