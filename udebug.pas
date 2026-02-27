unit uDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Math;  // Add SyncObjs for TCriticalSection

type
  TDebugLevel = (
    dlNone,
    dlError,
    dlWarning,
    dlInfo,
    dlDetail,
    dlTrace
  );

var
  DebugLevel: TDebugLevel = dlInfo;
  DebugToConsole: Boolean = True;
  DebugToFile: Boolean = True;
  DebugFilename: String = 'debug.log';

procedure DebugInit;
procedure DebugDone;
procedure DebugLog(Level: TDebugLevel; const Msg: String);
procedure DebugError(const Msg: String);
procedure DebugWarning(const Msg: String);
procedure DebugInfo(const Msg: String);
procedure DebugDetail(const Msg: String);
procedure DebugTrace(const Msg: String);

procedure DebugHexDump(const Data: array of Byte; Count: Integer; const Prefix: String);
procedure DebugStreamDump(Stream: TStream; Count: Integer; const Prefix: String);
procedure DebugFileInfo(const Filename: String);

implementation

var
  DebugFile: TextFile;
  DebugInitialized: Boolean = False;
  DebugCS: TCriticalSection;  // Changed from TRTLCriticalSection

procedure DebugInit;
begin
  if DebugInitialized then
    Exit;

  DebugCS := TCriticalSection.Create;  // Cross-platform critical section

  if DebugToFile then
  begin
    AssignFile(DebugFile, DebugFilename);
    try
      if FileExists(DebugFilename) then
        Append(DebugFile)
      else
        Rewrite(DebugFile);
    except
      DebugToFile := False;
    end;
  end;

  DebugInitialized := True;
  DebugInfo('=== Debug System Initialized ===');
  DebugInfo('Time: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now));
  DebugInfo('Level: ' + IntToStr(Ord(DebugLevel)));
end;

procedure DebugDone;
begin
  if not DebugInitialized then
    Exit;

  DebugCS.Enter;
  try
    DebugInfo('=== Debug System Shutting Down ===');

    if DebugToFile then
      CloseFile(DebugFile);

    DebugInitialized := False;
  finally
    DebugCS.Leave;
    DebugCS.Free;
  end;
end;

procedure WriteDebugMessage(const Msg: String);
var
  Timestamp: String;
begin
  Timestamp := FormatDateTime('hh:nn:ss.zzz', Now);

  DebugCS.Enter;
  try
    if DebugToConsole and IsConsole then
      WriteLn(Timestamp + ' ' + Msg);

    if DebugToFile and DebugInitialized then
    begin
      WriteLn(DebugFile, Timestamp + ' ' + Msg);
      Flush(DebugFile);
    end;
  finally
    DebugCS.Leave;
  end;
end;

procedure DebugLog(Level: TDebugLevel; const Msg: String);
begin
  if not DebugInitialized then
    DebugInit;

  if Level <= DebugLevel then
    WriteDebugMessage(Msg);
end;

procedure DebugError(const Msg: String);
begin
  DebugLog(dlError, '[ERROR] ' + Msg);
end;

procedure DebugWarning(const Msg: String);
begin
  DebugLog(dlWarning, '[WARN]  ' + Msg);
end;

procedure DebugInfo(const Msg: String);
begin
  DebugLog(dlInfo, '[INFO]  ' + Msg);
end;

procedure DebugDetail(const Msg: String);
begin
  DebugLog(dlDetail, '[DETAIL] ' + Msg);
end;

procedure DebugTrace(const Msg: String);
begin
  DebugLog(dlTrace, '[TRACE] ' + Msg);
end;

procedure DebugHexDump(const Data: array of Byte; Count: Integer; const Prefix: String);
var
  I, J: Integer;
  HexStr, AsciiStr: String;
  LineStart: Integer;
begin
  if not DebugInitialized then
    DebugInit;

  if DebugLevel < dlDetail then
    Exit;

  Count := Min(Count, Length(Data));

  for I := 0 to (Count + 15) div 16 - 1 do
  begin
    HexStr := '';
    AsciiStr := '';
    LineStart := I * 16;

    for J := 0 to 15 do
    begin
      if LineStart + J < Count then
      begin
        HexStr := HexStr + IntToHex(Data[LineStart + J], 2) + ' ';

        if (Data[LineStart + J] >= 32) and (Data[LineStart + J] < 127) then
          AsciiStr := AsciiStr + Chr(Data[LineStart + J])
        else
          AsciiStr := AsciiStr + '.';
      end
      else
      begin
        HexStr := HexStr + '   ';
        AsciiStr := AsciiStr + ' ';
      end;

      if J = 7 then
        HexStr := HexStr + ' ';
    end;

    DebugDetail(Prefix + Format('%4.4x: %-50s %s', [LineStart, HexStr, AsciiStr]));
  end;
end;

procedure DebugStreamDump(Stream: TStream; Count: Integer; const Prefix: String);
var
  Buffer: array of Byte;
  OldPos: Int64;
  BytesRead: Integer;
begin
  if not DebugInitialized then
    DebugInit;

  if (Stream = nil) or (DebugLevel < dlDetail) then
    Exit;

  OldPos := Stream.Position;
  try
    Stream.Position := 0;
    Count := Min(Count, Stream.Size);

    SetLength(Buffer, Count);
    BytesRead := Stream.Read(Buffer[0], Count);

    if BytesRead > 0 then
      DebugHexDump(Buffer, BytesRead, Prefix);

  finally
    Stream.Position := OldPos;
  end;
end;

procedure DebugFileInfo(const Filename: String);
var
  F: File;
  Size: Int64;
  FileDate: LongInt;
begin
  if not DebugInitialized then
    DebugInit;

  DebugInfo('File: ' + Filename);

  if FileExists(Filename) then
  begin
    try
      AssignFile(F, Filename);
      Reset(F, 1);
      Size := FileSize(F);
      CloseFile(F);

      FileDate := FileAge(Filename);
      if FileDate <> -1 then
        DebugInfo('  Size: ' + IntToStr(Size) + ' bytes, Modified: ' +
                 DateTimeToStr(FileDateToDateTime(FileDate)))
      else
        DebugInfo('  Size: ' + IntToStr(Size) + ' bytes');

    except
      on E: Exception do
        DebugError('  Cannot get file info: ' + E.Message);
    end;
  end
  else
  begin
    DebugWarning('  File does not exist');
  end;
end;

initialization
  DebugInit;

finalization
  DebugDone;

end.
