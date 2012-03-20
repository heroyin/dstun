unit TestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DStun, DSMessage, Sockets, IdBaseComponent,
  IdComponent, IdUDPBase, IdUDPClient;

type
  TForm1 = class(TForm)
    btnTest: TButton;
    edtServer: TLabeledEdit;
    edtPort: TLabeledEdit;
    edtNatType: TLabeledEdit;
    edtPublicIP: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  if ParamStr(1) <> '' then
    edtServer.Text := ParamStr(1);

  if ParamStr(2) <> '' then
    edtPort.Text := ParamStr(2);

end;

procedure TForm1.btnTestClick(Sender: TObject);
const
  NetArray: array [dsntUdpBlocked..dsntSymmetric] of string =
    ('UdpBlocked', 'OpenInternet', 'SymmetricUdpFirewall',
     'FullCone', 'RestrictedCone', 'PortRestrictedCone',
     'Symmetric');
var
  DSClient: TDStunClient;
  DSResult: TDSResult;
  UDP: TIdUDPClient;
begin
  btnTest.Enabled := False;

  UDP := TIdUDPClient.Create(Self);


  DSClient := TDStunClient.Create(UDP.Binding.Handle);
  try
    edtNatType.Text := '';
    edtPublicIP.Text := '';
    DSResult := DSClient.Query(edtServer.Text, StrToInt(edtPort.Text));
   // edtLocal.Text := Format('%s:%d', [DSClient.LocalIP, DSClient.LocalPort]);
    edtNatType.Text := NetArray[DSResult.NetType];
    edtPublicIP.Text := Format('%s:%d', [IPAddressToString(DSResult.PublicIP),
      IPAdressToPort(DSResult.PublicIP)]);
  finally
    FreeAndNil(DSClient);
    btnTest.Enabled := True;
    UDP.free;
  end;
end;

end.
