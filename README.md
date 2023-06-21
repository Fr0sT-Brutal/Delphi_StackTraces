Delphi call stacks with function names and line numbers
=======================================================

This project consisting of two small units shows how to get call stacks for exceptions (and even for any random line)
and, with the help of MAP file, get all info corresponding to each address of call stack: unit, function and even number
of line in source code.
Bonus abilities include:

- Getting line number of currently executed instruction
- Getting name of currently executed function/method

Demo project shows sample of use.

Built-in MAP files
------------------

Separate MAP files could be inconvenient - they're big, they must be kept in sync with binary, they could be deleted and so on.
Solution is to inject MAP file into binary.

1. Compile small utility called ResAdd from the corresponding folder of this repo. I copied it to `$(BDS)\Tools\` but that's optional

2. In the options of your project go to Build events > Post-build events > Commands and add the following:

    `$(BDS)\Tools\ResAdd.exe $(OUTPUTPATH) MapFileZ $(OUTPUTDIR)$(OUTPUTNAME).map compress`

    where `$(BDS)\Tools\ResAdd.exe` is path to ResAdd for my setup, you can modify this for yours.

3. Add following code to your project:

   ```pascal
   uses Zlib, Windows;

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
   ```

   and call `PrepareCallStacksAndMapFile` somewhere at start point.

4. Cool, now you have slim binary containing synced MAP file that occupies just a small space.
