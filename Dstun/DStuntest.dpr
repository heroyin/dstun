program DStuntest;

uses
  Forms,
  TestMain in 'TestMain.pas' {Form1},
  DStun in 'src\DStun.pas',
  DSMessage in 'src\DSMessage.pas',
  DSSocket in 'src\DSSocket.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
