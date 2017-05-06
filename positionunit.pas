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
  Offset: TPoint;
  GPixOff: TPoint;

  RecordedSize: TPoint;
  //
  MoveOffset: TPoint;

  procedure DrawFullImage(Width, Height: Integer);

implementation

uses
  prefsunit;

var
  FullNumX, FullNumY, FullZoom, FullOffX, FullOffY : Integer;
  FullAvail: array of array of Boolean;

procedure DrawFullImage(Width, Height: Integer);
var
  TileRect: TRectCoord;
  PTMid: TPoint;
  y, x, OffX, OffY: Integer;
  LMiddleCoord: TTileCoord;
  PixOff: TPoint;
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


initialization
  MiddlePos.Lat := Prefs.StartPosLat;
  MiddlePos.Lon := Prefs.StartPosLon;
  CurZoom := Prefs.StartZoom;
  FullBitmap := TFPAMemImage.create(10,10);
  FullCanvas := TFPACanvas.Create(FullBitmap);
finalization
  FullCanvas.Free;
  FullBitmap.Free;
end.
