program Interval;

uses
  Vcl.Forms,
  UFormInterval in 'UFormInterval.pas' {FormInterval},
  UAvlTree in 'UAvlTree.pas',
  UDisjointInterval in 'UDisjointInterval.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInterval, FormInterval);
  Application.Run;
end.
