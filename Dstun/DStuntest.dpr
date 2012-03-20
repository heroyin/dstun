program DStuntest;

uses
  Forms,
  TestMain in 'TestMain.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
