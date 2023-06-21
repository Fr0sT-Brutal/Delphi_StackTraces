program StackTraceSample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Classes, Zlib, Windows,
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

// Load MAP file from <exename>.map or, if not exists, from RT_RCDATA 'MapFileZ' resource (compressed)
// Raises exception if fails to read from file, silently passes if fails to read from resource.
// Idea is:
//   - By default built-in MAP is loaded, the binary is standalone
//   - Release configs without MAP file raise no errors, user can check result with MapFileLoaded
//   - If something changes, external MAP file has priority to not have to setup the new binary.
//   - Existing binary built without MAP file could still load the external file if provided
procedure LoadMapFile;
var
  sl: TStringList;
  rs: TResourceStream;
  zds: TZDecompressionStream;
  ss: TStringStream;
begin
  if FileExists(ChangeFileExt(ParamStr(0), '.map')) then
  begin
    sl := TStringList.Create;
    sl.LoadFromFile(ChangeFileExt(ParamStr(0), '.map'), TEncoding.UTF8);
    ReadMapFile(sl.Text);
    sl.Free;
  end
  else
  begin
    rs := nil; zds := nil; ss := nil;
    try
      rs := TResourceStream.Create(HInstance, 'MapFileZ', RT_RCDATA);
      zds := TZDecompressionStream.Create(rs);
      ss := TStringStream.Create('', TEncoding.ASCII);
      ss.LoadFromStream(zds);
      ReadMapFile(ss.DataString);
    except
    end;
    FreeAndNil(zds);
    FreeAndNil(rs);
    FreeAndNil(ss);
  end;
end;

// Setup call stack handler and load MAP file
procedure PrepareCallStacksAndMapFile;
begin
  InstallExceptionCallStack;
  LoadMapFile;
end;

var
  ai: TMapFileAddrInfo;
  cl: TDemoClass;

begin
  // Init call stack and load MAP file
  PrepareCallStacksAndMapFile;

  if not MapFileLoaded then
  begin
    Writeln('Can''t load MAP file from file or resource!');
    Exit;
  end
  else
    Writeln('MAP file loaded');

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
