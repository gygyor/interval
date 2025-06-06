program Interval;

uses
  Vcl.Forms,
  UFormInterval in 'UFormInterval.pas' {Form1},
  UAvlTree in 'UAvlTree.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
