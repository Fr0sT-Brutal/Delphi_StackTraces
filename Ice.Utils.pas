(*******************************************************
              Useful functions and classes
  COMPATIBILITY:
    Compiler: D2010+, FPC
    Platform: x86, x64
    OS: Windows, Posix (partially)

  (c) Fr0sT-Brutal https://github.com/Fr0sT-Brutal
  LICENSE MPL-2.0
*******************************************************)

unit Ice.Utils;

interface

{$IFDEF FPC}{$Mode Delphi}{$ENDIF}

{$IF CompilerVersion >= 24} // System.Pos with offset starting from XE3
  {$DEFINE CAPS_POS_WITH_OFFSET}
{$IFEND}

uses
  SysUtils;

// Comparation results - not a copy from System.Types b/c there's mysterious bug
// when using in TRecordList<T>.Sort
type
  TCompareRes = Integer;

const
  ValLess    = -1;
  ValEqual   = 0;
  ValGreater = 1;

type
  TStrArray = TArray<string>;

  // For lists and arrays
  TCompareFn<T> = reference to function(Item1, Item2: T): TCompareRes;
  TCompareConstFn<T> = reference to function(const Item1, Item2: T): TCompareRes;

type
  // ***************************************************************************
  // Static class that just typecasts pointers to generic types
  // ***************************************************************************
  // Ex.: TSomeClass<T>.GetItemPtr: Ptr<T>.P
  // Background: you can't just use `PSomeType = ^TSomeType` with generics containers
  // like `TArray<TSomeType>`.
  Ptr<T> = class
  public
  type
    P = ^T;
  end;

  // ***************************************************************************
  // Static class that helps perform some actions over dynamic arrays
  // ***************************************************************************
  TArrHelper<T> = class
  private
    class procedure QuickSort(var Arr: TArray<T>; const CompareFn: TCompareConstFn<T>; L, R: Integer);
  public
    // Move elements in array Arr to the left removing 0th one and leaving last one empty
    class procedure Shift(var Arr: TArray<T>);
    // Remove Index-th element from array Arr and decrease array length
    class procedure Delete(var Arr: TArray<T>; Index: Integer);
    // Insert element Item into array Arr at index Index
    class procedure Insert(var Arr: TArray<T>; Index: Integer; const Item: T);
    // Increase length of array Arr by 1
    class procedure Grow(var Arr: TArray<T>); inline;
    // Decrease length of array Arr by 1
    class procedure Trunc(var Arr: TArray<T>); inline;
    // Add element Item to the end of array Arr
    class procedure Add(var Arr: TArray<T>; const Item: T); overload; inline;
    // Add all elements from array ArrToAdd to the end of array Arr
    class procedure Add(var Arr: TArray<T>; const ArrToAdd: array of T); overload;
    // Enlarge array Arr by 1 and return pointer to the last element
    class function Add(var Arr: TArray<T>): Ptr<T>.P; overload;
    // Add element Item to the end of array Arr and extract the 1st element
    class procedure Push(var Arr: TArray<T>; const Item: T); inline;
    // Sort items in array Arr with compare function CompareFn
    class procedure Sort(var Arr: TArray<T>; const CompareFn: TCompareConstFn<T>);
    // Return copy of 1st element of array Arr. Empty array will raise exception.
    class function First(const Arr: TArray<T>): T; inline;
    // Return pointer to the 1st element of array Arr. Empty array will raise exception.
    class function FirstPtr(const Arr: TArray<T>): Ptr<T>.P; inline;
    // Return copy of last element of array Arr. Empty array will raise exception.
    class function Last(const Arr: TArray<T>): T; inline;
    // Return pointer to last element of array Arr. Empty array will raise exception.
    class function LastPtr(const Arr: TArray<T>): Ptr<T>.P; inline;
  end;

const
  // Platform-dependent end of line
  NL = sLineBreak;

  // comparation
  function Compare(Item1, Item2: Pointer): TCompareRes; inline;
  // Read decimal number from the string starting from Idx up to first non-digit char
  function ReadNumber(const Str: string; var Idx: Integer): Integer;
  // Read DigitCnt hex digits from Src into a number
  function HexToUInt(Src: PChar; DigitCnt: Cardinal; out ErrIdx: Integer): NativeUInt;
  // Get 1st char of string or #0 if it's empty
  function FirstChar(const Str: string): Char; inline;
  {$IFNDEF CAPS_POS_WITH_OFFSET} // System.Pos with offset starting from XE3
  function Pos(const SubStr, Str: string; Offset: Integer): Integer; overload;
  {$ENDIF}
  // Check if Str starts from SubStr without copying by direct memory comparison
  function StrIsStartingFrom(const Str, SubStr: string): Boolean;
  // Add substring to string with delimiter.
  procedure AddStr(var Str: string; const AddStr, Delim: string; AllowEmpty: Boolean = True); inline;
  // Split string to array of elements.
  function Split(const Str: string; const Delim: string; AllowEmpty: Boolean = True;
    LastIdx: Integer = MaxInt): TStrArray;
  // Get size of string in bytes
  function StrSize(const Str: UnicodeString): Int64; inline;
  // Fill buffer with zero's. Differs from ZeroMemory: inline and other param types
  procedure ZeroMem(var Dest; Count: NativeUInt); inline;
  // Return exception info: message, class and address.
  function ExceptionInfo(E: Exception = nil; CallStacks: Boolean = False): string;
  // Create exception to raise - just a short form
  function Err(const Msg: string): Exception; overload;
  // Create exception to raise - just a short form (formatting included)
  function Err(const Msg: string; const Vars: array of const): Exception; overload;
  // Get address of currently executed code
  function GetCurrentAddress: Pointer;
  // Get name of class method that contains the given address.
  // Class must be compiled with M+/TYPEINFO ON option!
  function GetMethodName(AClass: TClass; Address: Pointer): string; overload;
  // Get name of object's method that contains the given address.
  // Class must be compiled with M+/TYPEINFO ON option!
  function GetMethodName(AObject: TObject; Address: Pointer): string; overload;

implementation

const
  // Messages
  S_E_NoExceptInfo         = 'No exception data or error occurred';
  S_M_ExceptInfoPatt       = '%s (%s @ $%x)';
  S_M_ExceptInfoStackPatt  = 'Stack trace:';
  S_M_ExceptInfoBaseExc    = 'Base exception: %s';
  S_EHex_TooMuchDigits     = 'HexToUInt: number of digits %d exceeds limit (16)';
  S_EHex_UnsuppCharSize    = 'Hex: char size  %d is not supported';

const
  Digits = ['0'..'9'];

{$REGION 'TArrHelper'}

class procedure TArrHelper<T>.Shift(var Arr: TArray<T>);
var Tail: Integer;
begin
  Finalize(Arr[0]); // Obligatory for managed types (strings, dyn arrays, interfaces)
  Tail := High(Arr);
  Move(Arr[1], Arr[0], Tail*SizeOf(T));
  // In the case of managed types (strings, dyn arrays, interfaces) now two last
  // cells reference the same object but ref counter is not incremented.
  // Just shred the last cell.
  ZeroMem(Arr[High(Arr)], SizeOf(T));
end;

class procedure TArrHelper<T>.Delete(var Arr: TArray<T>; Index: Integer);
var Tail: Integer;
begin
  Finalize(Arr[Index]); // Obligatory for managed types (strings, dyn arrays, interfaces)
  Tail := High(Arr) - Index;
  if Tail > 0 then
  begin
    Move(Arr[Index+1], Arr[Index], Tail*SizeOf(T));
    // Required for managed types - see comment in Shift
    ZeroMem(Arr[High(Arr)], SizeOf(T));
  end;
  Trunc(Arr);
end;

class procedure TArrHelper<T>.Insert(var Arr: TArray<T>; Index: Integer; const Item: T);
var Tail: Integer;
begin
  Grow(Arr);

  // Calc the number of elements to shift to avoid range check and excess move
  // if inserting at the end.
  Tail := High(Arr) - Index;
  if Tail > 0 then
  begin
    Move(Arr[Index], Arr[Index+1], Tail*SizeOf(T));
    // Required for managed types - see comment in Shift
    ZeroMem(Arr[Index], SizeOf(T));
  end;
  Arr[Index] := Item;
end;

class procedure TArrHelper<T>.Grow(var Arr: TArray<T>);
begin
  SetLength(Arr, Length(Arr) + 1);
end;

class procedure TArrHelper<T>.Trunc(var Arr: TArray<T>);
begin
  SetLength(Arr, Length(Arr) - 1);
end;

class procedure TArrHelper<T>.Add(var Arr: TArray<T>; const Item: T);
var
  Len: Integer;
begin
  Len := Length(Arr);
  SetLength(Arr, Len + 1);
  Arr[Len] := Item;
end;

class procedure TArrHelper<T>.Add(var Arr: TArray<T>; const ArrToAdd: array of T);
var
  Len, i: Integer;
begin
  Len := Length(Arr);
  SetLength(Arr, Len + Length(ArrToAdd));
  for i := Low(ArrToAdd) to High(ArrToAdd) do
    Arr[Len + i] := ArrToAdd[i];
end;

// Enlarge array Arr by 1 and return pointer to the last element
// Examples:
//   1) pItem := TArrHelper<TItem>.Add(ItemArray);
//      pItem.Field := ...
//   2) TArrHelper<TItem>.Add(ItemArray).Field := ...
class function TArrHelper<T>.Add(var Arr: TArray<T>): Ptr<T>.P;
var
  Len: Integer;
begin
  Len := Length(Arr);
  SetLength(Arr, Len + 1);
  Result := @Arr[Len];
end;

class procedure TArrHelper<T>.Push(var Arr: TArray<T>; const Item: T);
begin
  Shift(Arr);
  Arr[High(Arr)] := Item;
end;

class procedure TArrHelper<T>.QuickSort(var Arr: TArray<T>; const CompareFn: TCompareConstFn<T>; L, R: Integer);
var
  I, J: Integer;
  P, Tmp: T;
begin
  repeat
    I := L;
    J := R;
    P := Arr[(L + R) shr 1];
    repeat
      while CompareFn(Arr[I], P) <= ValLess do
        Inc(I);
      while CompareFn(Arr[J], P) >= ValGreater do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Tmp := Arr[I];
          Arr[I] := Arr[J];
          Arr[J] := Tmp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(Arr, CompareFn, L, J);
    L := I;
  until I >= R;
end;

class procedure TArrHelper<T>.Sort(var Arr: TArray<T>; const CompareFn: TCompareConstFn<T>);
begin
  if Length(Arr) > 1 then
    QuickSort(Arr, CompareFn, 0, Length(Arr) - 1);
end;

class function TArrHelper<T>.First(const Arr: TArray<T>): T;
begin
  Result := Arr[Low(Arr)];
end;

class function TArrHelper<T>.FirstPtr(const Arr: TArray<T>): Ptr<T>.P;
begin
  Result := @Arr[Low(Arr)];
end;

class function TArrHelper<T>.Last(const Arr: TArray<T>): T;
begin
  Result := Arr[High(Arr)];
end;

class function TArrHelper<T>.LastPtr(const Arr: TArray<T>): Ptr<T>.P;
begin
  Result := @Arr[High(Arr)];
end;

{$ENDREGION}

// ********* Comparation ********* \\

function Compare(Item1, Item2: Pointer): TCompareRes;
begin
  if PByte(Item1) < PByte(Item2) then
    Result := ValLess
  else if PByte(Item1) > PByte(Item2) then
    Result := ValGreater
  else
    Result := ValEqual;
end;

// ********* Strings ********* \\

{$IFNDEF CAPS_POS_WITH_OFFSET}
// PosEx analog, implemented to not use StrUtils
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
function Pos(const SubStr, Str: string; Offset: Integer): Integer;
var
  I, LIterCnt, L, J: Integer;
  PSubStr, PS: PChar;
  LCh: Char;
begin
  PSubStr := Pointer(SubStr);
  PS := Pointer(Str);
  if (PSubStr = nil) or (PS = nil) or (Offset < 1) then
    Exit(0);
  L := Length(SubStr);
  { Calculate the number of possible iterations. }
  LIterCnt := Length(Str) - Offset - L + 2;
  if (L > 0) and (LIterCnt > 0) then
  begin
    Inc(PS, Offset - 1);
    I := 0;
    LCh := PSubStr[0];
    if L = 1 then   // Special case when Substring length is 1
      repeat
        if PS[I] = LCh then
          Exit(I + Offset);
        Inc(I);
      until I = LIterCnt
    else
      repeat
        if PS[I] = LCh then
        begin
          J := 1;
          repeat
            if PS[I + J] = PSubStr[J] then
            begin
              Inc(J);
              if J = L then
                Exit(I + Offset);
            end
            else
              Break;
          until False;
        end;
        Inc(I);
      until I = LIterCnt;
  end;

  Result := 0;
end;
{$ENDIF}

// Digit char to digit
function CharToDigit(C: Char): Byte;
begin
  Result := Ord(C) - Ord('0');
end;

// Read a number from string starting from index Idx. Reading ends at first non-digit char.
// Index of that char is returned in Idx.
function ReadNumber(const Str: string; var Idx: Integer): Integer;
var Len: Integer;
begin
  Result := 0; Len := Length(Str);
  while (Idx <= Len) and (CharInSet(Str[Idx], Digits)) do
  begin
    Result := Result*10 + CharToDigit(Str[Idx]);
    Inc(Idx);
  end;
end;

// ********* Hex ********* \\

const
  HexDigitValues: array['0'..'f'] of ShortInt =
  (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,                               // 0..9
    -1, -1, -1, -1, -1, -1, -1,                                 // :..@
    10, 11, 12, 13, 14, 15,                                     // A..F
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // G..`
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    10, 11, 12, 13, 14, 15                                      // a..f
  );

  HexDigits: array[0..15] of Char =
    '0123456789ABCDEF';

function HexDigitToInt(Digit: Char): ShortInt; inline;
begin
  case Digit of
    Low(HexDigitValues)..High(HexDigitValues):
      Result := HexDigitValues[Digit];
    else
      Result := -1;
  end;
end;

// Convert characters from hex string to a number.
//   Src - pointer to hex string without spaces, accepted chars are 0..9, a..f, A..F
//   DigitCnt - how much hex digits to read
//   ErrIdx - index of first char that is not hex digit (0 on success)
function InternalHexToUInt(Src: Pointer; CharSize: Byte; DigitCnt: Byte; out ErrIdx: Integer): NativeUInt; inline;
var
  i: Cardinal;
  Digit: ShortInt;
begin
  if DigitCnt > 2*SizeOf(NativeUInt) then
    raise Err(S_EHex_TooMuchDigits, [DigitCnt]);

  ErrIdx := 0; Result := 0; // !! 0 because it will be shifted

  case CharSize of
    {$IFDEF UNICODE}
    SizeOf(AnsiChar):
      for i := 1 to DigitCnt do
      begin
        Result := Result shl 4; // ! doesn't matter at 1st loop as the value is 0
        Digit := HexDigitToInt(Char(PAnsiChar(Src)^));
        if Digit = -1 then
        begin
          ErrIdx := i;
          Exit(0);
        end;
        Result := Result or Byte(Digit);
        Inc(PByte(Src), CharSize);
      end;
    {$ENDIF}
    SizeOf(Char):
      for i := 1 to DigitCnt do
      begin
        Result := Result shl 4; // ! doesn't matter at 1st loop as the value is 0
        Digit := HexDigitToInt(Char(PWideChar(Src)^));
        if Digit = -1 then
        begin
          ErrIdx := i;
          Exit(0);
        end;
        Result := Result or Byte(Digit);
        Inc(PByte(Src), CharSize);
      end;
    else raise Err(S_EHex_UnsuppCharSize, [CharSize]);
  end;
end;

function HexToUInt(Src: PChar; DigitCnt: Cardinal; out ErrIdx: Integer): NativeUInt;
begin
  Result := InternalHexToUInt(Src, SizeOf(Char), DigitCnt, ErrIdx);
end;

// Add substring to string with delimiter.
//   @param Str - accumulator string
//   @param AddStr - substring to add. If empty and AllowEmpty = False, Str is not modified.
//   @param Delim - delimiter. It is added before AddStr if Str is not empty or AllowEmpty = True
//   @param AllowEmpty - if True, allows adding empty substrings
procedure AddStr(var Str: string; const AddStr, Delim: string; AllowEmpty: Boolean);
begin
  if (AddStr = '') and not AllowEmpty then Exit;
  if Str = ''
    then Str := AddStr
    else Str := Str + Delim + AddStr;
end;

// Split string to array of elements.
//   @param Str - source string
//   @param Delim - delimiter of elements (any but non-empty)
//   @param AllowEmpty - if empty elements should be added ('elem1;elem2;;elem3').
//     Default setting is True. Set to False if source string is a set of
//     homogenic items and empty elements are ignored: `Split(Text, NL, False)`
//   @param LastIdx - 0-based last index for splitting. After LastIdx-th element is found,
//     search is stopped and remain of source string is returned in one piece:
//     `Split('password = abc=def', '=', False, 1) = ['password', 'abc=def']`.
//     * LastIdx = 0 - array consisting of the whole source string is returned
//     * LastIdx = MaxInt - split everything
// Sample:
//   HTMLlines := Split(HTMLPageSource, #13#10, False)
//   BananaProperties := Split('banana;yellow;;Africa', ';', True)
// ! Escaping of delimiters is not supported !
function Split(const Str: string; const Delim: string; AllowEmpty: Boolean; LastIdx: Integer): TStrArray;
const
  LengthStep = 16;
var
  CurrDelim, NextDelim, CurrIdx, StrLen, DelimLen: Integer;
begin
  CurrDelim := 1; CurrIdx := 0; SetLength(Result, LengthStep);
  StrLen := Length(Str); DelimLen := Length(Delim);

  repeat
    if CurrIdx = Length(Result) then
      SetLength(Result, CurrIdx + LengthStep); // expand the array if needed

    if CurrIdx = LastIdx then // last index reached - write up to the end
    begin
      NextDelim := StrLen + 1;
    end
    else
    begin
      NextDelim := Pos(Delim, Str, CurrDelim);
      if NextDelim = 0 then        // string is over - write up to the end
        NextDelim := StrLen + 1;
    end;
    Result[CurrIdx] := Copy(Str, CurrDelim, NextDelim - CurrDelim);
    CurrDelim := NextDelim + DelimLen;

    if (Result[CurrIdx] <> '') or AllowEmpty
      then Inc(CurrIdx)
      else Continue;
  until NextDelim > StrLen;

  SetLength(Result, CurrIdx); // cut the array down
end;

function StrSize(const Str: UnicodeString): Int64;
begin
  {$IF DECLARED(StringElementSize)}
  Assert(StringElementSize(Str) = SizeOf(WideChar)); // StringElementSize adds excess code, just check in debug that we guess right
  {$IFEND}
  Result := Length(Str)*SizeOf(WideChar);
end;

function StrIsStartingFrom(const Str, SubStr: string): Boolean;
begin
  Result := False;
  if ((Str = '') or (SubStr = '')) or (Length(SubStr) > Length(Str)) then Exit;
  Result := CompareMem(Pointer(Str), Pointer(SubStr), StrSize(SubStr));
end;

function FirstChar(const Str: string): Char;
begin
  if Str = '' then Result := #0 else Result := Str[1];
end;

procedure ZeroMem(var Dest; Count: NativeUInt);
begin
  FillChar(Dest, Count, 0);
end;

// Return exception info: message, class and address.
// If E = nil, takes exception from ExceptObject (i.e. function could be used ouside of "on E: Exc do" block)
function ExceptionInfo(E: Exception; CallStacks: Boolean): string;
const
  Separator: array[Boolean] of string = (' ', NL); // Nested exc's delimiter depending
                                                   // on call stacks presence
var
  ExcAddr: Pointer;
  stack: string;
begin
  // Take object from ExceptObject if E is nil
  if not Assigned(E) then
    if (ExceptObject <> nil) and (ExceptObject is Exception) then
      E := Exception(ExceptObject);
  // Still nil, quitting
  if not Assigned(E) then
    Exit(S_E_NoExceptInfo);
  ExcAddr := ExceptAddr;
  // Try to determine exc addr from the record.
  // FPC has these fields only on Windows
  {$WARN SYMBOL_PLATFORM OFF} // get rid of "Symbol 'ExceptionRecord' is specific to a platform" warning
  if (E is EExternal) {$IFDEF MSWINDOWS}and (EExternal(E).ExceptionRecord <> nil){$ENDIF} then
    {$IFDEF MSWINDOWS}
      ExcAddr := EExternal(E).ExceptionRecord^.ExceptionAddress;
    {$ELSE}
      {$IF DEFINED(DCC) AND (DEFINED(LINUX) OR DEFINED(MACOS))}
      ExcAddr := Pointer(EExternal(E).ExceptionAddress);
      {$IFEND}
    {$ENDIF}

  Result := Format(S_M_ExceptInfoPatt, [E.Message, E.ClassName, NativeUInt(ExcAddr)]);
  if CallStacks then
  begin
    stack := E.StackTrace;
    if stack <> '' then
      AddStr(Result, (S_M_ExceptInfoStackPatt + NL + stack), NL);
  end;
  // Docs: "If there are no inner exceptions that triggered the current one, BaseException returns
  // the current exception; otherwise BaseException inspects the inner exceptions until it
  // finds the first one that triggered the chain of exceptions."
  if E <> E.BaseException then
    AddStr(Result, Format(S_M_ExceptInfoBaseExc, [ExceptionInfo(E.BaseException)]), Separator[CallStacks]);
end;

function Err(const Msg: string): Exception;
begin
  Result := Exception.Create(Msg);
end;

function Err(const Msg: string; const Vars: array of const): Exception;
begin
  Result := Exception.CreateFmt(Msg, Vars);
end;

function GetCurrentAddress: Pointer;
begin
  Result := ReturnAddress;
end;

function GetMethodName(AClass: TClass; Address: Pointer): string;
type   // copy declaration from System's impl section
  PMethRec = ^MethRec;
  MethRec = packed record
    recSize: Word;
    methAddr: Pointer;
    nameLen: Byte;
    { nameChars[nameLen]: AnsiChar }
  end;
var
  LMethTablePtr: Pointer;
  LMethCount: Word;
  LMethEntry, LResultMethEntry: PMethRec;
begin
  Result := '';

  { Obtain the method table and count }
  LMethTablePtr := PPointer(PByte(AClass) + vmtMethodTable)^;
  if LMethTablePtr = nil then // no methods...
    Exit;
  LMethCount := PWord(LMethTablePtr)^;
  if LMethCount = 0 then // no methods...
    Exit;

  Inc(PWord(LMethTablePtr));
  // Get all method entries and find max method entry addr that is less (or equal - very unlikely tho) than Address
  LMethEntry := LMethTablePtr;
  LResultMethEntry := nil;
  while LMethCount > 0 do
  begin
    // Only consider methods starting before the Address
    if PByte(LMethEntry.methAddr) <= PByte(Address) then
    begin
      // Not assigned yet
      if (LResultMethEntry = nil) or
        // Current entry is closer to Address, reassign the variable
        (PByte(LMethEntry.methAddr) > PByte(LResultMethEntry.methAddr)) then
        LResultMethEntry := LMethEntry;
    end;
    Dec(LMethCount);
    LMethEntry := Pointer(PByte(LMethEntry) + LMethEntry.recSize); // get next
  end;

  if LResultMethEntry <> nil then
    Result := string(PShortString(@LResultMethEntry.nameLen)^);
end;

function GetMethodName(AObject: TObject; Address: Pointer): string;
begin
  Result := GetMethodName(AObject.ClassType, Address);
end;

end.
