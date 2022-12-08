program StackTraceSample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Classes,
  Ice.Utils, Ice.Debug;

// Demo subs

procedure Nested2;
begin
  Abort;
end;

procedure Nested1;
begin
  Nested2;
end;

procedure Nested0;
begin
  Nested1;
end;

var
  sl: TStringList;
  ai: TMapFileAddrInfo;
begin
  InstallExceptionCallStack;
  sl := TStringList.Create;
  sl.LoadFromFile(ChangeFileExt(ParamStr(0), '.map'), TEncoding.UTF8);
  ReadMapFile(sl.Text);
  sl.Free;
  GetAddrInfo(@Nested2, ai);
  Writeln(AddrInfoToString(ai));
  Readln;
  try
    Nested0;
  except on E: Exception do
    begin
      Writeln(ExceptionInfo(E, True));
      Readln;
    end;
  end;
  UninstallExceptionCallStack;
end.
