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

procedure Nested1_1;
var CallStack: TDbgInfoStack;
begin
  GetCallStackOS(CallStack, 0);
  Writeln('** Random callstack (no exception involved):', NL, CallStackToStr(CallStack));
end;

procedure Nested0_1;
begin
  Nested1_1;
end;

// Demo class
type
  {$IFOPT M-} {$DEFINE NO_TYPEINFO} {$ENDIF} // Save current value of TYPEINFO option
  {$M+} // ! Necessary for GetMethodName to work
  TDemoClass = class
    procedure TestMethod;
  end;
  {$IFDEF NO_TYPEINFO} {$M-} {$ENDIF}        // Revert value of TYPEINFO option

{ TDemoClass }

procedure TDemoClass.TestMethod;
begin
  Writeln('** Get method name by address', NL, GetMethodName(Self, GetCurrentAddress));
end;

var
  sl: TStringList;
  ai: TMapFileAddrInfo;
  cl: TDemoClass;

begin
  // Setup our custom exception call stack mechanism
  InstallExceptionCallStack;

  // Load MAP file
  sl := TStringList.Create;
  sl.LoadFromFile(ChangeFileExt(ParamStr(0), '.map'), TEncoding.UTF8);
  ReadMapFile(sl.Text);
  sl.Free;

  // Demo1: get info about some address
  GetAddrInfo(@Nested2, ai);
  Writeln('** Address info of Nested2 proc:', NL, AddrInfoToString(ai));

  // Demo2: exception stack trace info
  try
    Nested0;
  except on E: Exception do
    begin
      Writeln('** Exception info & stack trace:', NL, ExceptionInfo(E, True));
    end;
  end;

  // Demo3: random callstack (no exception involved)
  Nested0_1;

  // Remove our custom exception call stack mechanism
  UninstallExceptionCallStack;

  // Demo4: get method name by address
  cl := TDemoClass.Create();
  cl.TestMethod;
  cl.Free;

  Readln;
end.
