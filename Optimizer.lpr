program Optimizer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  UEventArray,
  UMidiEvent,
  UMidiSaveStream,
  UMyMemoryStream,
  UMyMidiStream,
  UGriffEvent,
  Forms, UOpt, UMidiDataStream;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TOpt, Opt);
  Application.Run;
end.

