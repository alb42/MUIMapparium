unit positionunit;
{$mode objfpc}{$H+}
interface

uses
  classes, math, Sysutils,
  FPImgCanv, fpimage,
  osmhelper, imagesunit;
var
  // middle of the image  in real coords
  MiddlePos: TCoord;
  // middle position as pixel
  MiddleCoord: TTileCoord;
  // Current Zoom
  CurZoom: Integer;
  // FullBitmap to hold all the Bitmaps to directly drawto window
  FullBitmap: TFPAMemImage;
  FullCanvas: TFPACanvas;

  // shortcuts for mouseactions
  GResX, GResY: Single;
  Offset: Classes.TPoint;
  GPixOff: Classes.TPoint;

  RecordedSize: Classes.TPoint;
  //
  MoveOffset: Classes.TPoint;
  // Pens for drawing something
  WhitePen,
  BlackPen,
  GrayPen,
  BluePen,
  YellowPen,
  GreenPen,
  RedPen: LongInt;

  procedure DrawFullImage(Width, Height: Integer);
  function BoundingBoxToZoom(BoundString: string): Integer;

implementation

uses
  prefsunit;

var
  FullNumX, FullNumY, FullZoom, FullOffX, FullOffY : Integer;
  FullAvail: array of array of Boolean;

procedure DrawFullImage(Width, Height: Integer);
var
  TileRect: TRectCoord;
  PTMid: Classes.TPoint;
  y, x, OffX, OffY: Integer;
  LMiddleCoord: TTileCoord;
  PixOff: Classes.TPoint;
  NumX, NumY: Integer;
  Tile: TFPAMemImage;
begin
  RecordedSize.X := Width;
  RecordedSize.Y := Height;
  // some images to delete from memory, do it now
  ITFMutex.Enter;
  ImagesToFree.Clear;
  ITFMutex.Leave;
  // do not go over the limits

  if MiddlePos.Lat < -85.0511 then
    MiddlePos.Lat := -85.0511;
  if MiddlePos.Lat > 85.0511 then
    MiddlePos.Lat := 85.0511;

  if MiddlePos.Lon < -180 then
    MiddlePos.Lon := 180 + (MiddlePos.Lon + 180);
  if MiddlePos.Lon > 180 then
    MiddlePos.Lon := -180 - (MiddlePos.Lon - 180);

  LMiddleCoord := CoordToTile(CurZoom, MiddlePos);
  TileRect := GetTileRect(CurZoom, LMiddleCoord.Tile);
  GResX := (TileRect.MaxLon - TileRect.MinLon) / 256; // resolution per pixel
  Offset.X := LMiddleCoord.Pixel.X;
  Offset.Y := LMiddleCoord.Pixel.Y;
  GResY := (TileRect.MaxLat - TileRect.MinLat) / 256; // resolution per pixel

  PTMid := Point((Width div 2), (Height div 2));

  NumX := (Width div 256) + 2;
  NumY := (Height div 256) + 2;
  PixOff := Point(-Ceil((PTMid.X - Offset.X) / 256), - Ceil((PTMid.Y - Offset.Y) / 256));
  OffX := LMiddleCoord.Tile.X + PixOff.X;
  OffY := LMiddleCoord.Tile.Y + PixOff.Y;

  if (FullNumX <> NumX) or (FullNumY <> NumY) or
     (FullZoom <> CurZoom) or
     (FullOffX <> OffX) or (FullOffY <> OffY) then
  begin
    if (FullNumX <> NumX) or (FullNumY <> NumY) then
    begin
      FullBitmap.SetSize(NumX * 256, NumY * 256);
      SetLength(FullAvail, NumX, NumY);
    end;
    FullNumX := NumX;
    FullNumY := NumY;
    FullOffX := OffX;
    FullOffY := OffY;
    FullZoom := CurZoom;
    for y := 0 to NumY - 1 do
    begin
      for x := 0 to NumX - 1 do
      begin
        FullAvail[x, y] := False;
      end;
    end;
  end;

  FullCanvas.LockCanvas;
  for y := 0 to NumY - 1 do
  begin
    for x := 0 to NumX - 1 do
    begin
      if not FullAvail[x, y] then
      begin
        Tile := GetTile(CurZoom, Point(OffX + X, OffY + Y), FullAvail[x, y]);
        FullCanvas.FastDraw(x*256, y*256, Tile);
      end;
    end;
  end;
  FullCanvas.UnLockCanvas;
  MiddleCoord := LMiddleCoord;
  GPixOff := PixOff;
  //writeln('Images: ', ImageList.Count, ' of ', MaxImages);
end;

// calculate the Zoom level, that the Bounding box is completely visible
// needed for search
function BoundingBoxToZoom(BoundString: string): Integer;
var
  SL: TStringList;
  MinLat, MaxLat, MinLon, MaxLon, DiffLat, DiffLon: Double;
  Deg: TCoord;
  i: Integer;
  Pt: Classes.TPoint;
  Rec: TRectCoord;
begin
  Result := 9;
  SL := TStringList.Create;
  try
    ExtractStrings([','], [], PChar(BoundString), SL);
    if SL.Count = 4 then
    begin
      MinLat := StrToFloatDef(SL[0], 0);
      MaxLat := StrToFloatDef(SL[1], 0);
      MinLon := StrToFloatDef(SL[2], 0);
      MaxLon := StrToFloatDef(SL[3], 0);
      DiffLat := Abs(MaxLat - MinLat);
      DiffLon := Abs(MaxLon - MinLon);
      Deg.Lat:= (MaxLat + MinLat) / 2;
      Deg.Lon:= (MaxLon + MinLon) / 2;

      for i := 0 to 18 do
      begin
        Pt := GetTileCoord(i, Deg);
        Rec := GetTileRect(i, Pt);
        if (Abs(Rec.MaxLat - Rec.MinLat) >= DiffLat) and (Abs(Rec.MaxLon - Rec.MinLon) >= DiffLon) then
          Result := i;
      end;
    end;
  finally
    SL.Free;
  end;
end;


initialization
  //writeln('ener position');
  MiddlePos.Lat := Prefs.StartPosLat;
  //writeln(1);
  MiddlePos.Lon := Prefs.StartPosLon;
  //writeln(2);
  CurZoom := Prefs.StartZoom;
  //writeln(3);
  FullBitmap := TFPAMemImage.create(10,10);
  //writeln(4);
  FullCanvas := TFPACanvas.Create(FullBitmap);
  //writeln('leave position');
finalization
  FullCanvas.Free;
  FullBitmap.Free;
end.
