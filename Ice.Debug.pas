(*******************************************************
  Call stacks, MAP files
  Compatibility:
    Compiler: D2010+, FPC
    Platform: x86, x64
    OS: Windows
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
  // Line numbers info: unit, number, address. This is loaded from MAP file
  TMapFileLineAddrInfo = record
    UnitName: string;
    LineNum: Integer;
    Addr: Pointer;
  end;

  // Public (procedures, methods) info: name, address. This is loaded from MAP file
  TMapFilePublicAddrInfo = record
    Name: string;
    Addr: Pointer;
  end;

  // Info about an address returned by GetAddrInfo: unit, line number, public name, flag of exact match
  TMapFileAddrInfo = record
    UnitName: string;
    LineNum: Integer;
    Exact: Boolean;
    PublicName: string;
  end;

// Read MAP file from string and fill given lists of address/symbols info
//   @param MapFile - contents of MAP file
// @raises Exception on error
procedure ReadMapFile(const MapFile: string; out LineAddrs: TArray<TMapFileLineAddrInfo>;
  out PublicAddrs: TArray<TMapFilePublicAddrInfo>); overload;
// Read MAP file from string and fill global lists of address/symbols info
//   @param MapFile - contents of MAP file
// @raises Exception on error
procedure ReadMapFile(const MapFile: string); overload;
// Return info about the address from given lists of address/symbols info.
// @returns Whether info was found
function GetAddrInfo(Addr: Pointer; const LineAddrs: TArray<TMapFileLineAddrInfo>;
  const PublicAddrs: TArray<TMapFilePublicAddrInfo>; out AddrInfo: TMapFileAddrInfo): Boolean; overload;
// Return info about the address from global lists of address/symbols info
// @returns Whether info was found
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
// Format callstack to string using AddrInfoToString
function CallStackToStr(const Stack: TDbgInfoStack): string;
// Setup custom exception call stack hooks
procedure InstallExceptionCallStack;
// Remove custom exception call stack hooks
procedure UninstallExceptionCallStack;

implementation

var
  MapFileAvailable: Boolean;
  LineAddrs: TArray<TMapFileLineAddrInfo>;
  PublicAddrs: TArray<TMapFilePublicAddrInfo>;

resourcestring
  S_EMF_ReadingNoSection = 'Error reading map file: no section "%s" found';
  S_EMF_Reading = 'Error reading map file: "%s" at line #%d "%s"';
  S_EMF_ReadingUnexpContents = 'unexpected contents';
  S_EMF_ReadAddr = 'String "%s" doesn''t contain valid address (expect HHHH:HHHHHHHH)';
  S_EMF_ReadAddrIdx = 'String "%s" starting from index %d doesn''t contain valid address (expect HHHH:HHHHHHHH)';

{$REGION 'MAP file'}

//~ ** Internals **

const
  SSegmentsHeader = ' Start         Length     Name                   Class';
  SPublicsHeader = '  Address             Publics by Name';
  SLineNumbersHeaderStart = 'Line numbers for ';
  MaxSegments = 10; // there could unlikely be more segments in EXE
  // Address format: HHHH:HHHHHHHH where HHHH is hex number of segment, HHHHHHHH
  // is hex offset inside that segment
  AddrSegmLen = 4;
  AddrOfsLen = 8;
  AddrSep: Char = ':';
  AddrLen = AddrSegmLen + 1 + AddrOfsLen;

type
  TMapFileSegmentStartAddrs = array[1..MaxSegments] of Pointer; // segment number => starting address

// Read address of a "0002:0001FDEF" kind from string starting at index
//   StartIdx - [IN] Index to start at, [OUT] Index rigth after the address
// @raises Exception if address is invalid
procedure ReadAddr(const S: string; var StartIdx: Integer; out Segment: Integer; out Addr: Pointer); overload;
var
  ErrIdx: Integer;
begin
  if Length(S) < StartIdx - 1 + AddrLen then
    raise Err(S_EMF_ReadAddrIdx, [S, StartIdx]);
  Segment := HexToUInt(PChar(Pointer(S)) + StartIdx - 1, AddrSegmLen, ErrIdx);
  if ErrIdx <> 0 then
    raise Err(S_EMF_ReadAddrIdx, [S, StartIdx]);
  if S[StartIdx - 1 + AddrSegmLen + 1] <> AddrSep then
    raise Err(S_EMF_ReadAddrIdx, [S, StartIdx]);
  Addr := Pointer(HexToUInt(PChar(Pointer(S)) + StartIdx - 1 + AddrSegmLen + 1, AddrOfsLen, ErrIdx));
  if ErrIdx <> 0 then
    raise Err(S_EMF_ReadAddrIdx, [S, StartIdx]);
  Inc(StartIdx, AddrLen);
end;

// Skip spaces in string starting from an index, return whether a non-space char
// was found (False is the string has ended by space). Idx is allowed to be greater
// than string length
function SkipSpaces(const S: string; Len: Integer; var Idx: Integer): Boolean;
begin
  repeat
    if Idx >= Len then
      Exit(False);
    if S[Idx] <> ' ' then
      Exit(True);
    Inc(Idx);
  until False;
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

// Loops through array of strings starting from an index until a string to search is found
// @returns Whether search string is found and is not the last element in array
function LocateSection(const arr: TStrArray; var CurrIdx: Integer; const SectionHeader: string): Boolean;
var HighArr: Integer;
begin
  HighArr := High(arr);
  while CurrIdx < HighArr do // we want at least one line after stopline
  begin
    if arr[CurrIdx] = SectionHeader then
    begin
      Inc(CurrIdx); // Move to next line after stopline
      Exit(True);
    end;
    Inc(CurrIdx);
  end;
  Result := False;
end;

// Loops through array of strings starting from an index until a string to search is found
// @returns Whether search string is found and is not the last element in array
function LocateSectionPartial(const arr: TStrArray; var CurrIdx: Integer; const SectionHeader: string): Boolean;
var HighArr: Integer;
begin
  HighArr := High(arr);
  while CurrIdx < HighArr do // we want at least one line after stopline
  begin
    if FirstChar(arr[CurrIdx]) = SectionHeader[1] then  // fast check
      if StrIsStartingFrom(arr[CurrIdx], SectionHeader) then
      begin
        Inc(CurrIdx); // Move to next line after stopline
        Exit(True);
      end;
    Inc(CurrIdx);
  end;
  Result := False;
end;

// Read starting addrs of segments
// @raises Exception section is not found or some line is invalid
procedure ReadSegments(const arr: TStrArray; var CurrIdx: Integer; var SegmentInfo: TMapFileSegmentStartAddrs);
var
  Line: string;
  Segment, LineIdx, HighArr: Integer;
  Addr: Pointer;
begin
  if not LocateSection(arr, CurrIdx, SSegmentsHeader) then
    raise Err(S_EMF_ReadingNoSection, [SSegmentsHeader]);
  // Read all lines until empty
  try
    HighArr := High(arr);
    // " 0001:00401000 002AF040H .text                   CODE"
    while CurrIdx <= HighArr do
    begin
      Line := arr[CurrIdx];
      if Line = '' then Break;
      LineIdx := 1;
      SkipSpaces(Line, Length(Line), LineIdx);
      ReadAddr(Line, LineIdx, Segment, Addr);
      SegmentInfo[Segment] := Addr;
      Inc(CurrIdx);
    end;
  except on E: Exception do
    raise Err(S_EMF_Reading, [E.Message, CurrIdx + 1, arr[CurrIdx]]);
  end;
end;

// Read starting addrs of publics
// @raises Exception section is not found or some line is invalid
procedure ReadPublics(const arr: TStrArray; var CurrIdx: Integer;
  const SegmentInfo: TMapFileSegmentStartAddrs; var PublicAddrs: TArray<TMapFilePublicAddrInfo>);
var
  Line: string;
  Segment, LineIdx, HighArr: Integer;
  Addr: Pointer;
  pai: TMapFilePublicAddrInfo;
begin
  if not LocateSection(arr, CurrIdx, SPublicsHeader) then
    raise Err(S_EMF_ReadingNoSection, [SPublicsHeader]);
  Inc(CurrIdx); // empty line
  // Read all lines until empty
  try
    HighArr := High(arr);
    // " 0001:0028BC18       <Unit>.<PublicName>"
    while CurrIdx <= HighArr do
    begin
      Line := arr[CurrIdx];
      if Line = '' then Break;
      LineIdx := 1;
      SkipSpaces(Line, Length(Line), LineIdx);
      ReadAddr(Line, LineIdx, Segment, Addr);
      pai := Default(TMapFilePublicAddrInfo);
      pai.Addr := AbsAddr(SegmentInfo[Segment], Addr);
      if pai.Addr <> nil then
      begin
        if not SkipSpaces(Line, Length(Line), LineIdx) then
          raise Err(S_EMF_ReadingUnexpContents);
        pai.Name := Copy(Line, LineIdx, MaxInt);
        TArrHelper<TMapFilePublicAddrInfo>.Add(PublicAddrs, pai);
      end;
      Inc(CurrIdx);
    end;
  except on E: Exception do
    raise Err(S_EMF_Reading, [E.Message, CurrIdx + 1, arr[CurrIdx]]);
  end;
end;

// Read header of unit line numbers section, move to next line.
// "Line numbers for <Unit1>(<Unit2>.pas) segment <segm>"
// <Unit1> could be the same as <Unit2> or differ in case of generics
procedure ReadLineNumbersHeader(const arr: TStrArray; var CurrIdx: Integer; out UnitName: string);
const
  HeaderPrefixLen = Length(SLineNumbersHeaderStart);
var
  SpPos: Integer;
  UnitParts: TStrArray;
begin
  SpPos := Pos(' ', arr[CurrIdx], HeaderPrefixLen + 1);
  if SpPos = 0 then
    raise Err(S_EMF_Reading, [S_EMF_ReadingUnexpContents, CurrIdx + 1, arr[CurrIdx]]);
  UnitName := Copy(arr[CurrIdx], HeaderPrefixLen + 1, SpPos - HeaderPrefixLen - 1);  // "<Unit1>(<Unit2>.pas)"
  // Check if <Unit1> = <Unit2> and shrink it then
  UnitParts := Split(UnitName, '(');
  if StrIsStartingFrom(UnitParts[1], UnitParts[0]) then
    UnitName := Copy(UnitParts[1], 1, Length(UnitParts[1]) - 1);  // get rid of trailing ')';
  Inc(CurrIdx);
end;

// Read line numbers section and fill LineAddrs
procedure ReadLineNumbersSection(const arr: TStrArray; var CurrIdx: Integer; const UnitName: string;
  const SegmentInfo: TMapFileSegmentStartAddrs; var LineAddrs: TArray<TMapFileLineAddrInfo>);
var
  Line: string;
  lai: TMapFileLineAddrInfo;
  Segment, LineIdx, LineLen, HighArr: Integer;
  Addr: Pointer;
begin
  // Read all lines until empty
  try
    HighArr := High(arr);
    while CurrIdx <= HighArr do
    begin
      Line := arr[CurrIdx];
      if Line = '' then Break;
      // set of "  L 000S:AAAAAAAA" pairs separated by 1 or more spaces
      // L(LL...) - line number
      // 000S - segment number
      // AAAAAAAA - address
      LineLen := Length(Line);
      LineIdx := 1;

      repeat
        if not SkipSpaces(Line, LineLen, LineIdx) then Break; // line has ended
        // Read line number
        lai := Default(TMapFileLineAddrInfo);
        lai.UnitName := UnitName;
        lai.LineNum := ReadNumber(Line, LineIdx);  // reads until non-digit char
        if lai.LineNum = 0 then
          raise Err(S_EMF_ReadingUnexpContents);
        // Check delimiting space
        if Line[LineIdx] <> ' ' then
          raise Err(S_EMF_ReadingUnexpContents);
        Inc(LineIdx);
        // Read address
        ReadAddr(Line, LineIdx, Segment, Addr);
        lai.Addr := AbsAddr(SegmentInfo[Segment], Addr);
        if lai.Addr <> nil then
          TArrHelper<TMapFileLineAddrInfo>.Add(LineAddrs, lai);
      until False;

      Inc(CurrIdx);
    end;
  except on E: Exception do
    raise Err(S_EMF_Reading, [E.Message, CurrIdx + 1, arr[CurrIdx]]);
  end;
end;

//~ ** Publics **

procedure ReadMapFile(const MapFile: string; out LineAddrs: TArray<TMapFileLineAddrInfo>;
  out PublicAddrs: TArray<TMapFilePublicAddrInfo>);
var
  arr: TStrArray;
  CurrIdx, HighArr: Integer;
  UnitName: string;
  SegmentInfo: TMapFileSegmentStartAddrs;
begin
  LineAddrs := nil;
  PublicAddrs := nil;

  arr := Split(MapFile, NL);
  CurrIdx := Low(arr);

  ReadSegments(arr, CurrIdx, SegmentInfo);
  ReadPublics(arr, CurrIdx, SegmentInfo, PublicAddrs);

  HighArr := High(arr);
  repeat
    // Here we read obligatory sections so don't raise exception if not found
    if not LocateSectionPartial(arr, CurrIdx, SLineNumbersHeaderStart) then
      Break;
    Dec(CurrIdx); // We'll need section header
    ReadLineNumbersHeader(arr, CurrIdx, UnitName);
    Inc(CurrIdx); // skip empty line
    ReadLineNumbersSection(arr, CurrIdx, UnitName, SegmentInfo, LineAddrs);
  until CurrIdx = HighArr;

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
  ExactMarks:array[Boolean] of string =
    ('~', '');
begin
  Result := Format(Patterns[AddrInfo.PublicName <> ''],
    [AddrInfo.UnitName, ExactMarks[AddrInfo.Exact], AddrInfo.LineNum,
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
