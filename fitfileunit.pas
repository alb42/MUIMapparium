unit fitfileunit;

{$mode objfpc}{$H+}
{.$define FITDEBUG}
interface

uses
  Classes, SysUtils, Math;

type
  TFitField = packed record
    ID: Byte;
    Size: Byte;
    Typ: Byte;
  end;

 TFitHeader = packed record
    Version: Byte;
    ProfileVersion: Word;
    Len: LongWord;
    Signatur: array[0..3] of AnsiChar; // ".FIT"
  end;
  PFitHeader = ^TFitHeader;

  TDefMessage = packed record
    unknown: Byte;
    Endian: Byte;
    global_ID: Word;
    NumFields: Byte;
  end;
  PTDefMessage = ^TDefMessage;

  TFitDefinition = record
    DefMsg: TDefMessage;
    Fields: array of TFitField;
  end;
  TFieldDefinition= array of TFitDefinition;

  procedure LoadFITFile(Filename: string);
  function GetDefHeader(F: TStream; Header: Byte): Boolean;
  function GetData(F: TStream; Header: Byte; Offset: Integer = 0): Boolean;

var
  FitDef: array[0..255] of TFitDefinition;
  //
  last_timestamp: Int64;
  TrackIdx: Integer = 0;
  TrkPts: array of record
    lat: double;
    lon: double;
    alt: double;
    HeartRate: Double;
    Cadence: Double;
    Power: Double;
    Temp: Double;
    Time: TDateTime;
  end;


implementation

procedure LoadFITFile(Filename: string);
var
  filestream: TFileStream;
  m: TMemoryStream;
  HLen: Byte;
  HBuffer: Pointer;
  RecordHeader: Byte;
begin
  HBuffer := nil;
  TrackIdx := 0;
  last_timestamp := 0;
  SetLength(TrkPts, 0);
  {$ifdef FITDEBUG}
  writeln('Loading file ' + Filename);
  {$endif}
  filestream := TFileStream.Create(Filename, fmOpenRead);
  m := TMemoryStream.Create;
  try
    m.CopyFrom(filestream, filestream.Size);
    M.Position := 0;
    filestream.Free;
    filestream := nil;
    {$ifdef FITDEBUG}
    writeln('Open, read Header Lengh');
    {$endif}
    if (M.Read(HLen, 1) <> 1) or (HLen < 12) then
    begin
      {$ifdef FITDEBUG}
      writeln('Broken file!');
      {$endif}
      Exit;
    end;
    HBuffer := AllocMem(HLen);
    if M.Read(HBuffer^, HLen - 1) <> HLen - 1 then
    begin
      {$ifdef FITDEBUG}
      writeln('Broken file, too short for Header');
      {$endif}
      Exit;
    end;
    {$ifdef FITDEBUG}
    writeln('Header got, inspect');
    {$endif}
    {$ifdef FITDEBUG}
    writeln('  Version: ' + IntToStr((FH^.Version shr 4) and $F) + '.' + IntToStr(FH^.Version and $F));
    {$endif}
    while M.Position < M.Size - 1 do
    begin
      M.Read(RecordHeader, 1);
      if RecordHeader and $80 <> 0 then
      begin
        {$ifdef FITDEBUG}
        writeln('Compressed message header found: $' + IntToHex(RecordHeader, 1) + ' at ', F.position);
        {$endif}
        GetData(M, (RecordHeader shr 5) and 3);
      end
      else
      if RecordHeader and $40 <> 0 then
      begin
        {$ifdef FITDEBUG}
        writeln('Definition header found: $' + IntToHex(RecordHeader, 1) + ' at ', F.position);
        {$endif}
        GetDefHeader(M, RecordHeader);
      end
      else
      begin
        {$ifdef FITDEBUG}
        writeln('data header found: $' + IntToHex(RecordHeader, 1) + ' at ', F.position);
        {$endif}
        GetData(M, RecordHeader);
      end;
    end;
  finally
    if Assigned(HBuffer) then
      FreeMem(HBuffer);
    M.Free;
    filestream.Free;
  end;
end;

function GetDefHeader(F: TStream; Header: Byte): Boolean;
var
  LocalID: Integer;
  i: Integer;
begin
  Result := False;
  LocalID := Header and $F;
  {$ifdef FITDEBUG}
  writeln('parse definition Header localID: ' + IntToHex(LocalID, 1));
  {$endif}
  if F.Read(FitDef[LocalID].DefMsg, SizeOf(FitDef[LocalID].DefMsg)) <> SizeOf(FitDef[LocalID].DefMsg) then
  begin
    {$ifdef FITDEBUG}
    writeln('file truncated');
    {$endif}
    Exit;
  end;
  {$ifdef ENDIAN_LITTLE}
  if FitDef[LocalID].DefMsg.Endian = 1 then
    FitDef[LocalID].DefMsg.global_ID := SwapEndian(FitDef[LocalID].DefMsg.global_ID);
  {$else}
    if FitDef[LocalID].DefMsg.Endian = 0 then
      FitDef[LocalID].DefMsg.global_ID := SwapEndian(FitDef[LocalID].DefMsg.global_ID);
  {$endif}
  {$ifdef FITDEBUG}
  writeln('  Got definition header');
  writeln('  Endianess: ' + IntToStr(FitDef[LocalID].DefMsg.Endian));
  writeln('  GlobalID: ' + IntToStr(FitDef[LocalID].DefMsg.global_ID));
  writeln('  NumFields: ' + IntToStr(FitDef[LocalID].DefMsg.NumFields));
  {$endif}
  SetLength(FitDef[LocalID].Fields, FitDef[LocalID].DefMsg.NumFields);
  for i := 0 to FitDef[LocalID].DefMsg.NumFields - 1 do
  begin
    F.Read(FitDef[LocalID].Fields[i], SizeOf(FitDef[LocalID].Fields[i]));
    {$ifdef FITDEBUG}
    writeln('    ' + IntToStr(i+1) + '. Field ID: ' + IntToStr(FitDef[LocalID].Fields[i].ID) + ' Size: ' + intToStr(FitDef[LocalID].Fields[i].Size) + ' type: ' + IntToStr(FitDef[LocalID].Fields[i].Typ));
    {$endif}
  end;
  Result := True;
end;

function ReadField(F: TStream; LocalID: Integer; FieldID: Integer): LongInt;
var
  b: Byte;
  w: Word;
  i: Integer;
begin
  case FitDef[LocalID].Fields[FieldID].Typ of
    1,2: begin  // byte
      if FitDef[LocalID].Fields[FieldID].Size = 1 then
      begin
        F.Read(b, 1);
      end
      else
      begin
        for i := 1 to  FitDef[LocalID].Fields[FieldID].Size do
          F.Read(b, 1);
      end;
      Result := b;
    end;
    $83,$84: begin // Word
      if FitDef[LocalID].Fields[FieldID].Size = 2 then
      begin
        F.Read(w, 2);
        {$ifdef ENDIAN_LITTLE}
        if FitDef[LocalID].DefMsg.Endian = 1 then
          Result := SwapEndian(w)
        else
          Result := w;
        {$else}
        if FitDef[LocalID].DefMsg.Endian = 0 then
          Result := SwapEndian(w)
        else
          Result := w;
        {$endif}
      end
      else
      begin
        for i := 1 to  FitDef[LocalID].Fields[FieldID].Size do
          F.Read(b, 1);
        Result := -1;
      end;
    end;
    $85,$86: begin // LongInt
      if FitDef[LocalID].Fields[FieldID].Size = 4 then
      begin
        F.Read(Result, 4);
        {$ifdef ENDIAN_LITTLE}
        if FitDef[LocalID].DefMsg.Endian = 1 then
          Result := SwapEndian(Result)
        else
          Result := Result;
        {$else}
        if FitDef[LocalID].DefMsg.Endian = 0 then
          Result := SwapEndian(Result)
        else
          Result := Result;
        {$endif}
      end
      else
      begin
        for i := 1 to  FitDef[LocalID].Fields[FieldID].Size do
          F.Read(b, 1);
        Result := -1;
      end;
    end;
    else
    begin
      for i := 1 to  FitDef[LocalID].Fields[FieldID].Size do
        F.Read(b, 1);
      Result := -1;
    end;
  end;
end;

function UNIXTimeToDateTimeFAST(UnixTime: Int64): TDateTime;
begin
  Result := (UnixTime / 86400) + 25569;
end;

function GetData(F: TStream; Header: Byte; Offset: Integer = 0): Boolean;
var
  LocalID, i: Integer;
  Val, Lat, Lon, alt, Heart, Cadence, PowerVal, Temp: LongInt;
  d: Single;
  TimeStamp: Int64;
begin
  Val := -1;
  Lat := -1;
  Lon := -1;
  heart := -1;
  Cadence := -1;
  Alt := MaxInt;
  temp := MaxInt;
  Result := False;
  LocalID := Header and $1F;
  TimeStamp := last_timestamp + Offset;
  {$ifdef FITDEBUG}
  writeln('parse data Header localID: ' + IntToHex(LocalID, 2));
  {$endif}
  for i := 0 to High(FitDef[LocalID].Fields) do
  begin
    Val := ReadField(f, LocalID, i);
    if FitDef[LocalID].Fields[i].ID = 253 then
    begin
      //timestamp;
      last_timestamp := val;
      TimeStamp := val;
    end else
    begin
      case FitDef[LocalID].Fields[i].ID of
        0: begin
          Lat := val;
        end;
        1: begin
          Lon := val;
        end;
        2: begin
          alt := val;
        end;
        3: begin
          heart := val;
        end;
        4: begin
          cadence := val;
        end;
        7: begin
          Powerval := val;
        end;
        13: begin
          temp := val;
        end;
        // rest we do not need currently
      end;
    end;
  end;
  d := Double($7fffffff);
  if (lat >= 0) and (lon >= 0) and (FitDef[LocalID].DefMsg.global_ID = 20) then
  begin
    if TrackIdx > High(TrkPts) then
      SetLength(TrkPts, TrackIdx + 101);
    TrkPts[TrackIdx].lat := (lat / d) * 180;
    TrkPts[TrackIdx].lon := (lon / d) * 180;
    TrkPts[TrackIdx].HeartRate := Nan;
    if Heart >= 0 then
      TrkPts[TrackIdx].HeartRate := Heart;
    TrkPts[TrackIdx].Cadence := Nan;
    if Cadence >= 0 then
      TrkPts[TrackIdx].Cadence := Cadence;
    TrkPts[TrackIdx].Power := Nan;
    if PowerVal >= 0 then
      TrkPts[TrackIdx].Power := PowerVal;
    TrkPts[TrackIdx].Temp := Nan;
    if Temp < MaxInt then
      TrkPts[TrackIdx].Temp := Temp;
    //
    TrkPts[TrackIdx].alt := 0;
    if Alt <> MaxInt then
    begin
      TrkPts[TrackIdx].alt := (Alt / 5) - 500;
    end;
    TrkPts[TrackIdx].Time := UNIXTimeToDateTimeFAST(timestamp + 631065600);
    Inc(TrackIdx);
  end;
  Result := True;
end;

end.
