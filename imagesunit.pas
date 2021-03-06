unit imagesunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dos, fgl, osmhelper, syncobjs, Math, mui,
  fpreadpng, fpimage, FPImgCanv, fpcanvas, prefsunit, fphttpclient,
  MUIWrap, MUIMappariumLocale;

const
  MINPLOTDIST = 5;

type
  TFPAMemImage = class(TFPCompactImgRGBA8Bit)
  public
    property Data: PFPCompactImgRGBA8BitValue read FData;
  end;

  TFPACanvas = class(TFPImageCanvas)
  protected
    procedure DoCopyRect (x,y:integer; canvas:TFPCustomCanvas; Const SourceRect:TRect); override;
    procedure DoDraw (x,y:integer; Const AImage:TFPCustomImage); override;
  public
    procedure FastDraw(x,y: Integer; AImage: TFPAMemImage);
  end;


  { TImageTile }

  TImageTile = class
    Zoom: Integer;
    Posi: Classes.TPoint;
    Pict: TFPAMemImage;
    Requested: Boolean;
    ReadyToUse: Boolean;
    Error: Boolean;
    LastUsed: Int64;
    constructor Create(AZoom: Integer; APos: Classes.TPoint);
    destructor Destroy; override;
  end;

  TImagesList = specialize TFPGObjectList<TImageTile>;
  TBitmapList = specialize TFPGObjectList<TFPAMemImage>;

  { TImageLoadThread }

  TImageLoadThread = class(TThread)
  private
    HTTPClient: TFPHTTPClient;
  protected
    procedure Execute; override;
  end;

  function GetTile(AZoom: Integer; MPos: Classes.TPoint; out IsRealPicture: Boolean): TFPAMemImage;

var
  LNumLoaded: Cardinal = 0;
  LNumLoadedHD: Cardinal = 0;

  ImageList: TImagesList;

  ImagesToFree: TBitmapList;
  ITFMutex: TCriticalSection;

  NoBitmap: TFPAMemImage;

  Mutex: TCriticalSection;
  SomeThingTodo: Boolean = False;
  ImageLoadThread: TImageLoadThread;
  UpdateImage: Boolean = False;
  RedrawImage: Boolean = False;
  ImagesToLoad: Integer = 0;
  //
  MaxImages: Integer = 1000;

  AppDir: string;
  DataDir: string;


implementation

uses
  networkingunit, positionunit, defimageunit,
  icon, workbench;


procedure TFPACanvas.DoCopyRect (x,y:integer; canvas:TFPCustomCanvas; Const SourceRect:TRect);
begin
end;

procedure TFPACanvas.DoDraw (x,y:integer; Const AImage:TFPCustomImage);
begin
end;

// Faster Version of the Draw (only works if both are the same type, but this is in my case!)
procedure TFPACanvas.FastDraw(x,y: Integer; AImage: TFPAMemImage);
var
  SrcPtr, DestPtr: PLongWord;
  i: Integer;
  SrcLen, DestLen: Integer;
begin
  SrcPtr := Pointer(AImage.Data);
  DestPtr := Pointer(TFPAMemImage(Image).Data);
  Inc(DestPtr, x + (y * Width));
  //
  SrcLen := AImage.Width;
  DestLen := Width;
  for i := 0 to AImage.Height - 1 do
  begin
    System.Move(SrcPtr^, DestPtr^, SrcLen * SizeOf(LongWord));
    Inc(SrcPtr, SrcLen);
    Inc(DestPtr, DestLen);
  end;
end;

function LoadBitmap(FName: string; Pic: TFPAMemImage): Boolean;
var
  Reader: TFPReaderPNG;
begin
  Result := False;
  if not FileExists(FName) then
    Exit;
  Reader := TFPReaderPNG.Create;
  Pic.LoadFromFile(FName, Reader);
  Reader.Free;
  InterLockedIncrement(LNumLoadedHD);
  Result := True;
end;

function LoadFromWeb(HTTPClient: TFPHTTPClient; AZoom: Integer; MPos: Classes.TPoint; Pic: TFPAMemImage): Boolean;
var
  Mem: TMemoryStream;
  Url: string;
  Filename: string;
  //t1: Int64;
  Reader: TFPReaderPNG;
begin
  Result := False;
  if not IsOnline then
    Exit;
  //t1 := GetMSCount;
  Mem := TMemoryStream.Create;
  Reader := TFPReaderPNG.Create;
  try
    Url := BuildURL(AZoom, MPos);
    FileName := BuildFilename(AZoom, MPos);
    GetFile(HTTPClient, URL, Mem);
    Mem.Position := 0;
    Mem.SaveToFile(Filename);
    Mem.Position := 0;
    Pic.LoadFromStream(Mem, Reader);
    InterLockedIncrement(LNumLoaded);
    Result := True;
  finally
    Mem.Free;
    Reader.Free;
  end;
end;

{ TImageLoadThread }

function CompareFunc(const aValue, bValue: TImageTile): LongInt;
begin
  Result := 0;
  if Assigned(aValue) and Assigned(bValue) then
  begin
    if aValue.LastUsed > bValue.LastUsed then
      Result := 1;
    if aValue.LastUsed < bValue.LastUsed then
      Result := -1;
  end;
end;



procedure TImageLoadThread.Execute;
var
  i: Integer;
  FileLoaded: Boolean;
  Img: TImageTile;
  t1: Int64;
  ListCount, j: Integer;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  HTTPClient.KeepConnection := True;
  try
  t1 := GetMsCount;
  repeat
    if SomeThingTodo then
    begin
      Mutex.Enter;
      SomeThingTodo := False;
      i := 0;
      ListCount := ImageList.Count;
      while i < ImageList.Count do
      begin
        Img := ImageList[i];
        if (Img.Zoom <> CurZoom) and not Img.ReadyToUse then
        begin
          ImageList.Delete(i);
          Dec(ImagesToLoad);
          Continue;
        end;
        Mutex.Leave;
        if not Img.ReadyToUse and not Img.Error then
        begin
          FileLoaded := False;
          try
            try
              FileLoaded := LoadBitmap(BuildFilename(Img.Zoom, Img.Posi), Img.Pict);
            except
              FileLoaded:= False;
            end;
            if not FileLoaded then
            begin
              try
                FileLoaded := LoadFromWeb(HTTPClient, Img.Zoom, Img.Posi, Img.Pict);
              except
                on E:Exception do
                begin
                  FileLoaded := False;
                  IsOnline := False;
                  OfflineMsg := True;
                  OfflineErrText := E.Message;
                  writeln(OfflineErrText + #10'Unable to load File from WEB: osm_' + IntToStr(Img.Zoom) + '_' + IntToStr(Img.Posi.X) + '_' + IntToStr(Img.Posi.Y) + '.png'#10'Going Offline!');
                end;
              end;
            end;
            Img.Error := not FileLoaded;
            Img.ReadyToUse := True;
            UpdateImage := True;
          except
            on E:Exception do
            begin
              SysDebugLn('Excpetion Loading File: osm_' + IntToStr(Img.Zoom) + '_' + IntToStr(Img.Posi.X) + '_' + IntToStr(Img.Posi.Y) + '.png');
            end;
          end;
          Mutex.Enter;
          Dec(ImagesToLoad);
          Mutex.Leave;
        end;
        // Check if we are killed already
        if Terminated then
          Exit;
        Mutex.Enter;
        try
          if ImageList.Count = ListCount then
            Inc(i)
          else
          begin
            i := 0;
            ListCount := ImageList.Count;
          end;
          if (GetMsCount - t1 > 1000) and (ImageList.Count > MaxImages) then
          begin
            ImageList.Sort(@CompareFunc);
            while (ImageList.Count > MaxImages) do
            begin
              j := 0;
              while ImageList[j].Requested do
                j := j + 1;
              ImageList.Delete(j);
            end;
            t1 := GetMsCount;
          end;
        except
          sysdebugln('Exception in Remove');
        end;
      end;
      Mutex.Leave;
    end
    else
    begin
      Sleep(25);
    end;
  until Terminated;
  finally
    HTTPClient.Free;
  end;
end;

{ TImageTile }

constructor TImageTile.Create(AZoom: Integer; APos: Classes.TPoint);
begin
  Pict := TFPAMemImage.Create(10,10);
  LastUsed := GetMsCount;
  Zoom := AZoom;
  Posi.X := APos.X;
  Posi.Y := APos.Y;
  Requested := False;
  Error := False;
  ReadyToUse := False;
end;

destructor TImageTile.Destroy;
begin
  ITFMutex.Enter;
  if Assigned(ImagesToFree) then
    ImagesToFree.Add(Pict)
  else
    Pict.Free;
  ITFMutex.Leave;
  Pict := nil;
  inherited Destroy;
end;

function GetTile(AZoom: Integer; MPos: Classes.TPoint; out IsRealPicture: Boolean): TFPAMemImage;
var
  i: Integer;
  Found: Boolean;
  InList: Boolean;
  NewImg, Img: TImageTile;
  p: Integer;
begin
  IsRealPicture := False;
  Result := EmptyBitmap;
  Found := False;
  InList := False;
  p := Round(Power(2, AZoom));
  if (MPos.Y < 0) or (MPos.Y >= p) then
  begin
    Result := NoBitmap;
    Exit;
  end;
  if MPos.X < 0 then
  begin
    MPos.X := Max(0, P + MPos.X);
  end;
  if MPos.X >= P then
  begin
    MPos.X := Max(0, P - MPos.X);
  end;
  //
  Mutex.Enter;
  try
    for i := 0 to ImageList.Count - 1 do
    begin
      Img := ImageList[i];
      if (Img.Zoom = AZoom) and (Img.Posi.X = MPos.X) and
        (Img.Posi.Y = MPos.Y) then
      begin
        InList := True;
        Img.LastUsed := GetMsCount;
        if Img.ReadyToUse then
        begin
          IsRealPicture := True;
          Found := True;
          if Img.Error then
          begin
            Result := ErrorBitmap
          end
          else
          begin
            Result :=Img.Pict;
          end;
        end;
        Exit;
      end;
    end;
  finally
    Mutex.Leave;
  end;
  //
  if not Found and not InList then
  begin
    Mutex.Enter;
    NewImg := TImageTile.Create(AZoom, MPos);
    ImageList.Insert(0, NewImg);
    Inc(ImagesToLoad);
    SomeThingTodo := True;
    Mutex.Leave;
  end;
end;

function GetStrToolType(DObj: PDiskObject; Entry: string; Default: string): string;
var
  Res: PChar;
begin
  Result := Default;
  if not assigned(Dobj) then
    Exit;
  if not Assigned(Dobj^.do_Tooltypes) then
    Exit;
  Res := FindToolType(Dobj^.do_Tooltypes, PChar(Entry));
  if Assigned(Res) then
    Result := Res;
end;

procedure GetDirectories;
var
  DObj: PDiskObject;
begin
  AppDir := ExtractFileDir(ParamStr(0));
  DataDir := IncludeTrailingPathDelimiter(AppDir) + 'data';
  DObj := GetDiskObject(PChar(ParamStr(0)));
  if Assigned(DObj) then
  begin
    DataDir := GetStrToolType(DObj, 'DATAPATH', DataDir);
    FreeDiskObject(DObj);
  end;

  if not DirectoryExists(DataDir) then
  begin
    try
      CreateDir(DataDir);
    except
      On E:Exception do
      begin
        writeln('Cannot create directory for image data: ' + DataDir + ' Message: ' + E.Message);
        //ShowMessage('Error', GetLocString(MSG_GENERAL_OK), 'Cannot create directory for image data: ' + DataDir + ' Message: ' + E.Message);
        halt(5);
      end;
    end;
  end;
end;


var
  ic: TFPACanvas;

initialization
  //writeln('enter imagesunit');
  GetDirectories;

  ITFMutex := TCriticalSection.Create;
  Mutex := TCriticalSection.Create;

  ImageList := TImagesList.Create(True);
  ImagesToFree:= TBitmapList.Create(True);

  NoBitmap := TFPAMemImage.Create(256,256);
  ic := TFPACanvas.Create(NoBitmap);
  ic.Brush.FPColor := colBlue;
  ic.FillRect(0,0,255,255);
  ic.Free;

  ImageLoadThread := TImageLoadThread.Create(True);
  ImageLoadThread.Start;
  //writeln('leave imagesunit');
finalization
  ImageLoadThread.Terminate;
  ImageLoadThread.WaitFor;
  ImageLoadThread.Free;
  ITFMutex.Enter;
  ImagesToFree.Free;
  ImagesToFree := nil;
  ITFMutex.Leave;
  ImageList.Free;
  //EmptyBitmap.Free;
  NoBitmap.Free;
  Mutex.Free;
  ITFMutex.Free;
end.
