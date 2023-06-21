program ResAdd;

{$APPTYPE CONSOLE}

uses
  Windows, Classes, SysUtils, System.ZLib;

function GetResData(const ResFile: string; Compress: Boolean): TMemoryStream;
var
  ZCompr: TZCompressionStream;
  ComprData: TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    Result.LoadFromFile(ResFile);
  except
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
  if not Compress then Exit; // Return as is

  try try
    ComprData := TMemoryStream.Create;
    ZCompr := TZCompressionStream.Create(clMax, ComprData);
    ZCompr.CopyFrom(Result, Result.Size);
    FreeAndNil(Result);
    Result := ComprData;
  except
    begin
      FreeAndNil(Result);
      FreeAndNil(ComprData);
      raise;
    end;
  end;
  finally
    FreeAndNil(ZCompr);
  end;
end;

var
  DestExe, ResName, ResFile: string;
  Compress: Boolean;
  hUpdRes: THandle;
  ResData: TMemoryStream;
begin
  ReportMemoryLeaksOnShutdown := True;
  if not (ParamCount in [3, 4]) then
  begin
    Writeln('Resource BLOB adder. Usage:'+sLineBreak+
      'ResAdd.exe <DestExe> <ResName> <ResFile> [compress]');
    Readln;
    Exit;
  end;
  hUpdRes := 0; ResData := nil;
  try try
    DestExe := ParamStr(1);
    ResName := ParamStr(2);
    ResFile := ParamStr(3);
    Compress := ParamStr(4) = 'compress';
    hUpdRes := BeginUpdateResource(PChar(DestExe), False);
    if hUpdRes = 0 then
      RaiseLastOSError;
    ResData := GetResData(ResFile, Compress);
    if not UpdateResource(hUpdRes, RT_RCDATA, PChar(ResName),
      MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), ResData.Memory, ResData.Size) then
      RaiseLastOSError;
  finally
    EndUpdateResource(hUpdRes, False);
    FreeAndNil(ResData);
  end;
  except on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
