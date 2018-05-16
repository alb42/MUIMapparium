unit Exif;

{$MODE Delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  TIfdTag = packed record
    ID: word; //Tag number
    Typ: word; //Type tag
    Count: cardinal; //tag length
    Offset: cardinal; //Offset / Value
  end;

  TExif = class
  private
    FDateTime: string; //Date and Time of Change
    FValid: boolean;
    fs: TStream;
    gpsifdp: cardinal;
    FSwap: boolean;
    function ReadAsci(const Offset, Count: cardinal): string;
    function ReadRatio(const Offset: cardinal; frac: boolean): string; overload;
    function ReadRatio(const Offset: cardinal): single; overload;
    procedure ReadTag(var tag: TIfdTag);
    procedure Init;
    function ReadLongIntValue(const Offset: cardinal): longint;
  public
    FLatDir: string;
    FLonDir: string;
    FLat: array[0..2] of single;
    FLon: array[0..2] of single;


    constructor Create;
    procedure ReadFromFile(const FileName: ansistring);

    property DateTime: string read FDateTime;
  end;

implementation

uses
  Math;

type
  TMarker = packed record
    Marker: word; //Section marker
    Len: word; //Length Section
    Indefin: array [0..4] of char; //Indefiner - "Exif" 00, "JFIF" 00 and ets
    Pad: char; //0x00
  end;

  TIFDHeader = packed record
    pad: byte; //00h
    ByteOrder: word; //II (4D4D) or MM
    i42: word; //2A00 (magic number from the 'Hitchhikers Guide'
    Offset: cardinal; //0th offset IFD
    Count: word; // number of IFD entries
  end;

function SwapLong(Value: dword): dword;
begin
  Result := SwapEndian(Value);
end;

function SwapWord(Value: word): word;
begin
  Result := SwapEndian(Value);
end;

procedure TExif.ReadTag(var tag: TIfdTag);
begin
  Fs.Read(Tag, 12);
  if FSwap then
    with tag do
    begin // motorola or intel byte order ?
      ID := SwapWord(ID);
      Typ := SwapWord(Typ);
      Count := SwapLong(Count);
      if (Typ = 1) or (Typ = 3) then
        Offset := (Offset shr 8) and $FF
      else
        Offset := SwapLong(Offset);
    end
  else
    with tag do
    begin
      if ID <> $8827 then //ISO Metering Mode not need conversion
        if (Typ = 1) or (Typ = 3) then
          Offset := Offset and $FF; // other bytes are undefined but maybe not zero
    end;
end;


function TExif.ReadAsci(const Offset, Count: cardinal): string;
var
  fp: longint;
  i: word;
begin
  SetLength(Result, Count);
  fp := fs.Position;
  fs.Position := Offset;
  try
    i := 1;
    repeat
      fs.Read(Result[i], 1);
      //BlockRead(f,Result[i],1);
      Inc(i);
    until (i >= Count) or (Result[i - 1] = #0);
    if i <= Count then
      Result := Copy(Result, 1, i - 1);
  except
    Result := '';
  end;
  Result := TrimRight(Result);
  fs.Position := fp;
end;

function TExif.ReadLongIntValue(const Offset: cardinal): longint;
var
  fp: longint;
begin
  fp := fs.Position;
  fs.Position := Offset;
  try
    fs.Read(Result, SizeOf(Result));
    if FSwap then
      Result := SwapLong(Result);
  except
    Result := 0;
  end;
  fs.Position := fp;
end;

function TExif.ReadRatio(const Offset: cardinal; frac: boolean): string;
var
  fp: longint;
  nom, denom: cardinal;
begin
  fp := fs.Position;
  fs.Position := Offset;
  try
    fs.Read(nom, 4);
    fs.Read(denom, 4);
    if FSwap then
    begin // !!!
      nom := SwapLong(nom);
      denom := SwapLong(denom);
    end;
    if frac then
    begin
      str((nom / denom): 1: 2, Result);
      if (length(Result) > 0) and (Result[length(Result)] = '0') then
        Result := copy(Result, 1, length(Result) - 1);
    end
    else
    if denom <> 1000000 then
      Result := IntToStr(nom) + '/' + IntToStr(denom)
    else
      Result := '0';
  except
    Result := '';
  end;
  fs.Position := fp;
end;


function TExif.ReadRatio(const Offset: cardinal): single;
var
  fp: longint;
  nom, denom: cardinal;
begin
  fp := fs.Position;
  fs.Position := Offset;
  try
    fs.Read(nom, 4);
    fs.Read(denom, 4);
    if FSwap then
    begin // !!!
      nom := SwapLong(nom);
      denom := SwapLong(denom);
    end;
    Result := nom / denom;
  except
    Result := 0.0;
  end;
  fs.Position := fp;
end;


procedure TExif.Init;
begin
  FDateTime := '';
  FValid := False;
end;


constructor TExif.Create;
begin
  Init;
end;


procedure DebugMe(Msg: string; Value: Integer = 0);
begin
  {$ifdef HASAMIGA}
  //sysdebugln(MSG + ' $' + HexStr(Pointer(Value)));
  {$else}
  //writeln(MSG + ' $' + HexStr(Pointer(Value)));
  {$endif}
end;

procedure TExif.ReadFromFile(const FileName: ansistring);
var
  j: TMarker;
  ifd: TIFDHeader;
  off0: cardinal; //Null Exif Offset
  tag: TIfdTag;
  i, n1: integer;
  SOI: word; //2 bytes SOI marker. FF D8 (Start Of Image)
  IfdCnt: word;
  fs1: TFilestream;
begin
  DebugMe('load' + FileName);
  if not FileExists(FileName) then
    exit;
  Init;

  System.FileMode := 0; //Read Only open
  fs1 := TFileStream.Create(Filename, fmOpenRead);
  fs := fs1;
  fs.Position := 0;


  fs.Read(SOI, 2);
  {$ifdef ENDIAN_BIG}
  Soi := SwapEndian(SOI);
  {$endif}
  DebugMe('SOI: ', SOI);
  if SOI = $D8FF then
  begin //Is this Jpeg
    fs.Read(j, 9);
    {$ifdef ENDIAN_BIG}
    j.Len := SwapEndian(j.Len);
    j.Marker := SwapEndian(j.Marker);
    {$endif}
    if j.Marker = $E0FF then
    begin //JFIF Marker Found
      fs.Position := 20;
      fs.Read(j, 9);
      {$ifdef ENDIAN_BIG}
      j.Len := SwapEndian(j.Len);
      j.Marker := SwapEndian(j.Marker);
      {$endif}
    end;

    //Search Exif start marker;
    if j.Marker <> $E1FF then
    begin
      i := 0;
      repeat
        fs.Read(SOI, 2);
        Inc(i);
      until ((fs.Position >= fs.Size) or (i > 1000) or (SOI = $E1FF));
      //If we find maker
      {$ifdef ENDIAN_BIG}
      Soi := SwapEndian(SOI);
      {$endif}
      if SOI = $E1FF then
      begin
        fs.Position := fs.Position - 2;
        fs.Read(j, 9);
        {$ifdef ENDIAN_BIG}
        j.Len := SwapEndian(j.Len);
        j.Marker := SwapEndian(j.Marker);
        {$endif}
      end;
    end;

    DebugMe('j.Marker: ', j.Marker);
    if j.Marker = $E1FF then
    begin //If we found Exif Section. j.Indefin='Exif'.
      FValid := True;
      Off0 := fs.Position + 1;
      fs.Read(ifd, 11);
      {$ifdef ENDIAN_LITTLE}
      FSwap := ifd.ByteOrder = $4D4D; // II or MM - if MM we have to swap
      {$else}
      FSwap := not (ifd.ByteOrder = $4D4D); // II or MM - if MM we have to swap
      {$endif}
      if FSwap then
      begin
        ifd.Offset := SwapLong(ifd.Offset);
        ifd.Count := SwapWord(ifd.Count);
      end;
      DebugMe('ifd.Offset: ', ifd.Offset);
      DebugMe('ifd.Count: ', ifd.Count);
      if ifd.Offset <> 8 then
      begin
        fs.Position := fs.Position + abs(ifd.Offset) - 8;
      end;

      if (ifd.Count = 0) then
        ifd.Count := 100;

      for i := 1 to ifd.Count do
      begin
        ReadTag(tag);
        case tag.ID of
          0: break;
          $0132: FDateTime := ReadAsci(tag.Offset + off0, tag.Count);
          $8825: gpsifdp := Tag.Offset;
        end;
      end;

      if gpsifdp > 0 then
      begin
        fs.Position := gpsifdp + off0;
        //Seek(f,gpsifdp+off0);
        fs.Read(IfdCnt, 2);
        //BlockRead(f,IfdCnt,2);
        if FSwap then
          IfdCnt := SwapWord(IfdCnt);
        for i := 1 to IfdCnt do
        begin
          ReadTag(tag);
          case Tag.ID of
            $0001: FLatDir := char(SwapLong(Tag.Offset));
            $0002:
            begin
              FLat[0] := 0;
              ;
              FLat[1] := 0;
              FLat[2] := 0;
              ;
              for n1 := 0 to Min(High(FLat), Tag.Count - 1) do
              begin
                FLat[n1] := ReadRatio(Tag.Offset + Off0 + (n1 * 8));
              end;
            end;
            $0003: FLonDir := char(SwapLong(Tag.Offset));
            $0004:
            begin
              FLon[0] := 0;
              ;
              FLon[1] := 0;
              FLon[2] := 0;
              ;
              for n1 := 0 to Min(High(FLat), Tag.Count - 1) do
              begin
                FLon[n1] := ReadRatio(Tag.Offset + Off0 + (n1 * 8));
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  fs.Free;
  fs := nil;
  //CloseFile(f);
end;

end.
