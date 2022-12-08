(*******************************************************
  Call stacks, MAP files
  Compatibility:
    Compiler: D2010+, FPC
    Platform: x86, x64
    ОС: Windows
  (c) Fr0sT-Brutal https://github.com/Fr0sT-Brutal
  LICENSE MPL-2.0
*******************************************************)

unit Ice.Debug;

interface

uses {$IFDEF MSWINDOWS}
     Windows,
     {$ENDIF}
     SysUtils, StrUtils,
     Ice.Utils;

type
  // ***************************************************************************
  // MAP file reading
  // ***************************************************************************
  TMapFileLineAddrInfo = record
    UnitName: string;
    LineNum: Integer;
    Addr: Pointer;
  end;

  TMapFilePublicAddrInfo = record
    Name: string;
    Addr: Pointer;
  end;

  TMapFileAddrInfo = record
    UnitName: string;
    LineNum: Integer;
    Exact: Boolean;
    PublicName: string;
  end;

// Read MAP file from string and fill given lists of address/symbols info
procedure ReadMapFile(const MapFile: string; out LineAddrs: TArray<TMapFileLineAddrInfo>;
  out PublicAddrs: TArray<TMapFilePublicAddrInfo>); overload;
// Read MAP file from string and fill global lists of address/symbols info
procedure ReadMapFile(const MapFile: string); overload;
// Return info about the address from given lists of address/symbols info
function GetAddrInfo(Addr: Pointer; const LineAddrs: TArray<TMapFileLineAddrInfo>;
  const PublicAddrs: TArray<TMapFilePublicAddrInfo>; out AddrInfo: TMapFileAddrInfo): Boolean; overload;
// Return info about the address from global lists of address/symbols info
function GetAddrInfo(Addr: Pointer; out AddrInfo: TMapFileAddrInfo): Boolean; overload;
// Format addr info into string with all info available.
// Format is:
//   $Address Unit:LOC [Unit.Routine]
//     - LOC - if exact, number of line of code; not exact values are marked with ~
function AddrInfoToString(const AddrInfo: TMapFileAddrInfo): string;
// Return name of currently executed function/method
function CurrentFuncName: string;

// Call stack
const
  DBG_STACK_LENGTH = 32;
type
  TDbgInfoStack = array[0..DBG_STACK_LENGTH - 1] of Pointer;
  PDbgInfoStack = ^TDbgInfoStack;

{$IFDEF MSWINDOWS}
function RtlCaptureStackBackTrace(FramesToSkip: ULONG; FramesToCapture: ULONG; BackTrace: Pointer;
  BackTraceHash: PULONG): USHORT; stdcall; external 'kernel32.dll';
{$ENDIF}

{$IFDEF MSWINDOWS}
// Return call stack pointers. Removes itself from call stack by increasing FramesToSkip by 1
procedure GetCallStackOS(const Stack: TDbgInfoStack; FramesToSkip: Integer);
{$ENDIF}
function CallStackToStr(const Stack: TDbgInfoStack): string;
procedure InstallExceptionCallStack;
procedure UninstallExceptionCallStack;

implementation

var
  MapFileAvailable: Boolean;
  LineAddrs: TArray<TMapFileLineAddrInfo>;
  PublicAddrs: TArray<TMapFilePublicAddrInfo>;

const
  S_EMF_Reading = 'Error reading map file: "%s" at line #%d "%s"';

{$REGION 'MAP file'}

const
  SSegmentsHeader = ' Start         Length     Name                   Class';
  SPublicsHeader = '  Address             Publics by Name';
  SLineNumbersHeaderStart = 'Line numbers for ';
  MaxSegments = 10; // there could unlikely be more segments in EXE

type
  TMapFileSegmentStartAddrs = array[1..MaxSegments] of Pointer; // segment number => starting address

// Read address of a "0002:0001FDEF" kind
procedure ReadAddr(const StrAddr: string; out Segment: Integer; out Addr: Pointer);
begin
  with SplitPair(StrAddr, ':') do
  begin
    Segment := StrToInt(Left);
    Addr := Pointer(StrToInt('$'+Right));
  end;
end;

// Restore full absolute address from segment address and relative address.
// It's possible to have lines like "0004:FFE70000       SysInit.__ImageBase"
// so function checks if result will overflow and returns nil in this case
function AbsAddr(SegmentAddr, RelAddr: Pointer): Pointer;
var uRes: UInt64;
begin
  uRes := UInt64(SegmentAddr) + UInt64(RelAddr);
  if uRes > High(NativeUInt) then
    uRes := 0;
  Result := Pointer(uRes);
end;

// Read starting addrs of segments
procedure ReadSegments(const arr: TStrArray; var CurrIdx: Integer; var SegmentInfo: TMapFileSegmentStartAddrs);
var
  s: string;
  Segment: Integer;
  Addr: Pointer;
begin
  while arr[CurrIdx] <> SSegmentsHeader do
  begin
    Inc(CurrIdx);
    if CurrIdx = High(arr) then Exit;
  end;
  Inc(CurrIdx);
  // " 0001:00401000 002AF040H .text                   CODE"
  try
    while arr[CurrIdx] <> '' do
    begin
      s := GetElement(arr[CurrIdx], 1, ' ');
      ReadAddr(s, Segment, Addr);
      SegmentInfo[Segment] := Addr;
      Inc(CurrIdx);
    end;
  except on E: Exception do
    raise Err(S_EMF_Reading, [E.Message, CurrIdx + 1, arr[CurrIdx]]);
  end;
end;

// Read starting addrs of publics
procedure ReadPublics(const arr: TStrArray; var CurrIdx: Integer;
  const SegmentInfo: TMapFileSegmentStartAddrs; var PublicAddrs: TArray<TMapFilePublicAddrInfo>);
var
  sAddr, sPub: string;
  Segment: Integer;
  Addr: Pointer;
  pai: TMapFilePublicAddrInfo;
begin
  while arr[CurrIdx] <> SPublicsHeader do
  begin
    Inc(CurrIdx);
    if CurrIdx = High(arr) then Exit;
  end;
  Inc(CurrIdx);
  Inc(CurrIdx);
  // " 0001:0028BC18       <Unit>.<PublicName>"
  try
    while arr[CurrIdx] <> '' do
    begin
      SplitPair(arr[CurrIdx], '       ', sAddr, sPub);
      pai := Default(TMapFilePublicAddrInfo);
      ReadAddr(Trim(sAddr), Segment, Addr);
      pai.Addr := AbsAddr(SegmentInfo[Segment], Addr);
      if pai.Addr <> nil then
      begin
        pai.Name := sPub;
        TArrHelper<TMapFilePublicAddrInfo>.Add(PublicAddrs, pai);
      end;
      Inc(CurrIdx);
    end;
  except on E: Exception do
    raise Err(S_EMF_Reading, [E.Message, CurrIdx + 1, arr[CurrIdx]]);
  end;
end;

// Loop until line numbers for a unit is encountered
function LocateLineNumbersSection(const arr: TStrArray; var CurrIdx: Integer): Boolean;
begin
  Result := False;
  repeat
    if FirstChar(arr[CurrIdx]) = SLineNumbersHeaderStart[1] then
      if StrIsStartingFrom(arr[CurrIdx], SLineNumbersHeaderStart) then
        Exit(True);
    Inc(CurrIdx);
    if CurrIdx = High(arr) then
      Exit;
  until False;
end;

// "Line numbers for <Unit1>(<Unit2>.pas) segment <segm>"
// <Unit1> could be the same as <Unit2> or differ in case of generics
procedure ReadLineNumbersHeader(const Header: string; out UnitName: string);
var
  s, Unit1st, Unit2nd: string;
begin
  s := Header;
  Delete(s, 1, Length(SLineNumbersHeaderStart));  // "<Unit1>(<Unit2>.pas) segment <segm>"
  UnitName := GetElement(s, 0, ' ');              // "<Unit1>(<Unit2>.pas)"
  // Check if unit is the same
  SplitPair(UnitName, '(', Unit1st, Unit2nd);
  if StrIsStartingFrom(Unit2nd, Unit1st) then
    UnitName := Copy(Unit2nd, 1, Length(Unit2nd) - 1);  // get rid of trailing ')'
end;

// Read line numbers section and fill LineAddrs
procedure ReadLineNumbersSection(const arr: TStrArray; var CurrIdx: Integer; const UnitName: string;
  const SegmentInfo: TMapFileSegmentStartAddrs; var LineAddrs: TArray<TMapFileLineAddrInfo>);
var
  linearr: TStrArray;
  s: string;
  lai: TMapFileLineAddrInfo;
  Segment: Integer;
  Addr: Pointer;
  Reading1stPart: Boolean;
begin
  Inc(CurrIdx); // empty
  Inc(CurrIdx); // 1st line
  try
    while arr[CurrIdx] <> '' do
    begin
      // set of "LLL 000S:AAAAAAAA" pairs separated by 1 or more spaces
      // LLL - line number
      // S - segment number
      // A - address
      // Elements will be trimmed because separator is single space and allowEmpty = false
      linearr := Split(arr[CurrIdx], ' ', False);
      Reading1stPart := True;
      for s in linearr do
        if Reading1stPart then
        begin
          lai := Default(TMapFileLineAddrInfo);
          lai.UnitName := UnitName;
          lai.LineNum := StrToInt(s);
          Reading1stPart := False;
        end
        else
        begin
          ReadAddr(s, Segment, Addr);
          lai.Addr := AbsAddr(SegmentInfo[Segment], Addr);
          if lai.Addr <> nil then
            TArrHelper<TMapFileLineAddrInfo>.Add(LineAddrs, lai);
          Reading1stPart := True;
        end;
      Inc(CurrIdx);
    end;
  except on E: Exception do
    raise Err(S_EMF_Reading, [E.Message, CurrIdx + 1, arr[CurrIdx]]);
  end;
end;

procedure ReadMapFile(const MapFile: string; out LineAddrs: TArray<TMapFileLineAddrInfo>;
  out PublicAddrs: TArray<TMapFilePublicAddrInfo>);
var
  arr: TStrArray;
  i: Integer;
  UnitName: string;
  SegmentInfo: TMapFileSegmentStartAddrs;
begin
  LineAddrs := nil;
  PublicAddrs := nil;

  arr := Split(MapFile, NL);
  i := Low(arr);

  ReadSegments(arr, i, SegmentInfo);
  ReadPublics(arr, i, SegmentInfo, PublicAddrs);

  repeat
    if not LocateLineNumbersSection(arr, i) then
      Break;
    ReadLineNumbersHeader(arr[i], UnitName);
    ReadLineNumbersSection(arr, i, UnitName, SegmentInfo, LineAddrs);
  until i = High(arr);

  TArrHelper<TMapFileLineAddrInfo>.Sort(LineAddrs,
    function(const Item1, Item2: TMapFileLineAddrInfo): TCompareRes
    begin
      Result := Compare(Item1.Addr, Item2.Addr);
    end);
  TArrHelper<TMapFilePublicAddrInfo>.Sort(PublicAddrs,
    function(const Item1, Item2: TMapFilePublicAddrInfo): TCompareRes
    begin
      Result := Compare(Item1.Addr, Item2.Addr);
    end);
end;

procedure ReadMapFile(const MapFile: string);
begin
  ReadMapFile(MapFile, LineAddrs, PublicAddrs);
  MapFileAvailable := True;
end;

function GetAddrInfo(Addr: Pointer; const LineAddrs: TArray<TMapFileLineAddrInfo>;
  const PublicAddrs: TArray<TMapFilePublicAddrInfo>; out AddrInfo: TMapFileAddrInfo): Boolean;
var
  laIdx, paIdx: Integer;
  Found: Boolean;
begin
  Found := False; laIdx := 0;
  while laIdx <= Length(LineAddrs) - 2 do
    if (LineAddrs[laIdx].Addr = Addr) or
       ((PByte(LineAddrs[laIdx].Addr) < PByte(Addr)) and (PByte(Addr) < PByte(LineAddrs[laIdx + 1].Addr))) then
    begin
      Found := True;
      Break;
    end
    else
      Inc(laIdx);
  if not Found then laIdx := -1;

  Found := False; paIdx := 0;
  while paIdx <= Length(PublicAddrs) - 2 do
    if (PByte(PublicAddrs[paIdx].Addr) <= PByte(Addr)) and (PByte(Addr) < PByte(PublicAddrs[paIdx + 1].Addr)) then
    begin
      Found := True;
      Break;
    end
    else
      Inc(paIdx);
  if not Found then paIdx := -1;

  if (laIdx = -1) and (paIdx = -1) then Exit(False);

  AddrInfo := Default(TMapFileAddrInfo);
  if laIdx <> -1 then
  begin
    AddrInfo.UnitName := LineAddrs[laIdx].UnitName;
    AddrInfo.LineNum := LineAddrs[laIdx].LineNum;
    AddrInfo.Exact := LineAddrs[laIdx].Addr = Addr;
  end;
  if paIdx <> -1 then
    AddrInfo.PublicName := PublicAddrs[paIdx].Name;

  Result := True;
end;

function GetAddrInfo(Addr: Pointer; out AddrInfo: TMapFileAddrInfo): Boolean;
begin
  if not MapFileAvailable
    then Result := False
    else Result := GetAddrInfo(Addr, LineAddrs, PublicAddrs, AddrInfo);
end;

function AddrInfoToString(const AddrInfo: TMapFileAddrInfo): string;
const
  SPattMain = '%s:%s%d';
  SPattPub = ' [%s]';
  Patterns: array[Boolean] of string =
    (SPattMain, SPattMain + SPattPub);
begin
  Result := Format(Patterns[AddrInfo.PublicName <> ''],
    [AddrInfo.UnitName, IfTh(not AddrInfo.Exact, '~'), AddrInfo.LineNum,
     AddrInfo.PublicName]);
end;

function CurrentFuncName: string;
var AddrInfo: TMapFileAddrInfo;
begin
  if GetAddrInfo(ReturnAddress, AddrInfo)
    then Result := AddrInfo.PublicName
    else Result := '';
end;

{$ENDREGION}

procedure GetCallStackOS(const Stack: TDbgInfoStack; FramesToSkip: Integer);
begin
  ZeroMemory(@Stack, SizeOf(Stack));
  {$IFDEF MSWINDOWS}
  // Remove itself from call stack by adding 1
  RtlCaptureStackBackTrace(1 + FramesToSkip, Length(Stack), @Stack, nil);
  {$ENDIF}
end;

function CallStackToStr(const Stack: TDbgInfoStack): string;
var
  Ptr: Pointer;
  AddrInfo: TMapFileAddrInfo;
begin
  Result := '';
  for Ptr in Stack do
    if Ptr <> nil then
      if MapFileAvailable and GetAddrInfo(Ptr, AddrInfo) then
        AddStr(Result, Format('$%p', [Ptr]) + ' ' + AddrInfoToString(AddrInfo), NL)
      else
        AddStr(Result, Format('$%p', [Ptr]), NL)
    else
      Break;
end;

function GetExceptionStackInfo(P: PExceptionRecord): Pointer;
var
  pStack: PDbgInfoStack;
  CurrIdx: Integer;
begin
  pStack := AllocMem(SizeOf(TDbgInfoStack));
  // ! This call stack could include nested functions:
  //    - GetExceptionStackInfo
  //    - System.SysUtils.Exception.RaisingException
  // or could not. Let them be for now.
  GetCallStackOS(pStack^, 0);
  // Shift the stack by one and add the address of the exception itself
  for CurrIdx := High(pStack^) downto Low(pStack^) + 1 do
    pStack[CurrIdx] := pStack[CurrIdx - 1];
  pStack[0] := P.ExceptionAddress;
  Result := pStack;
end;

function GetStackInfoStringProc(Info: Pointer): string;
begin
  Result := CallStackToStr(PDbgInfoStack(Info)^);
end;

procedure CleanUpStackInfoProc(Info: Pointer);
begin
  Dispose(PDbgInfoStack(Info));
end;

procedure InstallExceptionCallStack;
begin
  SysUtils.Exception.GetExceptionStackInfoProc := GetExceptionStackInfo;
  SysUtils.Exception.GetStackInfoStringProc := GetStackInfoStringProc;
  SysUtils.Exception.CleanUpStackInfoProc := CleanUpStackInfoProc;
end;

procedure UninstallExceptionCallStack;
begin
  SysUtils.Exception.GetExceptionStackInfoProc := nil;
  SysUtils.Exception.GetStackInfoStringProc := nil;
  SysUtils.Exception.CleanUpStackInfoProc := nil;
end;

end.
