unit TestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DStun;

type
  TForm1 = class(TForm)
    btnTest: TButton;
    edtServer: TLabeledEdit;
    edtPort: TLabeledEdit;
    edtNatType: TLabeledEdit;
    edtPublicIP: TLabeledEdit;
    edtLocal: TLabeledEdit;
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

procedure TForm1.btnTestClick(Sender: TObject);
const
  NetArray: array [dsntUdpBlocked..dsntSymmetric] of string =
    ('UdpBlocked', 'OpenInternet', 'SymmetricUdpFirewall',
     'FullCone', 'RestrictedCone', 'PortRestrictedCone',
     'Symmetric');
var
  DSClient: TDSClient;
  DSResult: TDSResult;
begin
  btnTest.Enabled := False;
  DSClient := TDSClient.Create;
  try
    edtNatType.Text := '';
    edtPublicIP.Text := '';
    DSResult := DSClient.Query(edtServer.Text, StrToInt(edtPort.Text));
    edtLocal.Text := Format('%s:%d', [DSClient.LocalIP, DSClient.LocalPort]);
    edtNatType.Text := NetArray[DSResult.NetType];
    edtPublicIP.Text := IPAddressToString(DSResult.PublicIP);
  finally
    FreeAndNil(DSClient);
    btnTest.Enabled := True;
  end;
end;

end.
