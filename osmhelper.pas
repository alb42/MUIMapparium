unit OSMHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, fgl;

const
  BASEURL = 'http://tile.openstreetmap.org/';
  BASEFILE = 'osm_';

  SEARCHURL = 'http://nominatim.openstreetmap.org/search/';
  REVERSEURL = 'http://nominatim.openstreetmap.org/reverse';

type
  TSearchResult = class
    DisplayName: string;
    Lat, Lon: double;
    Zoom: Integer;
  end;

  TSearchResults = specialize TFPGObjectList<TSearchResult>;

  TCoord = record
    Lat, Lon: Double;
  end;

  TRectCoord = record
    MinLat, MinLon, MaxLat, MaxLon: Double;
  end;

  TTileCoord = record
    Tile: TPoint;
    Pixel: TPoint;
  end;

function GradToDistance(const P1, P2: TCoord): double;

function CoordToTile(Zoom: Integer; Deg: TCoord): TTileCoord;
function TileToCoord(Zoom: Integer; T: TTileCoord): TCoord;

function GetTileCoord(Zoom: Integer; Deg: TCoord): TPoint;
function GetMapPosition(Zoom: Integer; MPos: TPoint): TCoord;
function GetTileRect(Zoom: Integer; MPos: TPoint): TRectCoord;

function GetResolution(Zoom: Integer; Deg: TCoord): Double;
function GetYResolution(Zoom: Integer; Deg: TCoord): Double;

function BuildURL(Zoom: Integer; MPos: TPoint): string;
function BuildFilename(Zoom: Integer; MPos: TPoint): string;

function GetPosbyString(PosStr: string; var Coord: TCoord; var Zoom: Integer): Boolean;

implementation

uses
  ImagesUnit;


const
  WG84_ARC = 111323.872; // km/°

function GradToDistance(const P1, P2: TCoord): double;
var
  dx: Double;
  dy: Double;
begin
  dx := WG84_ARC * cos(degtorad(P1.lat)) * (P2.Lon - P1.Lon);
  dy := WG84_ARC * (P2.Lat - P1.Lat);
  Result := Sqrt(dx**2 + dy**2);
end;

function GetTileCoord(Zoom: Integer; Deg: TCoord): TPoint;
var
  n, LatRad: Double;
begin
  Result.X := 0;
  Result.Y := 0;
  LatRad := DegToRad(Deg.Lat);
  if Cos(LatRad) = 0 then
    Exit;
  n := Power(2, Zoom);
  Result.X := Trunc(((Deg.Lon + 180) / 360) * n);
  Result.Y := Trunc((1 - (ln(Tan(LatRad) + (1 / Cos(LatRad))) / Pi)) / 2 * n);
end;

function CoordToTile(Zoom: Integer; Deg: TCoord): TTileCoord;
var
  x1, y1, n, LatRad: Double;
begin
  Result.Tile.X := 0;
  Result.Tile.Y := 0;
  Result.Pixel.X := 0;
  Result.Pixel.Y := 0;
  LatRad := DegToRad(Deg.Lat);
  if Cos(LatRad) = 0 then
    Exit;
  n := Power(2, Zoom);
  x1 := ((Deg.Lon + 180) / 360) * n;
  y1 := (1 - (ln(Tan(LatRad) + (1 / Cos(LatRad))) / Pi)) / 2 * n;
  Result.Tile.X := Max(0, Floor(x1));
  Result.Tile.Y := Max(0, Floor(y1));
  Result.Pixel.X := Floor(Frac(x1) * 256);
  Result.Pixel.Y := Floor(Frac(y1) * 256);
end;

function TileToCoord(Zoom: Integer; T: TTileCoord): TCoord;
var
  x1, y1, n, LatRad: Double;
begin
  n := Power(2, Zoom);
  //
  x1 := T.Tile.X + (T.Pixel.X / 256);
  y1 := T.Tile.Y + (T.Pixel.Y / 256);

  LatRad := Arctan(Sinh(Pi * (1 - 2 * y1 / n)));
  Result.Lat := RadtoDeg(LatRad);
  Result.Lon := x1 / n * 360.0 - 180.0;
end;

function GetYResolution(Zoom: Integer; Deg: TCoord): Double;
var
  n, LatRad: Double;
  c, LResY, LResY1: Double;
  Pt: TPoint;
  TileRect: TRectCoord;
begin
  Result := 0;
  LatRad := DegToRad(Deg.Lat);
  if Cos(LatRad) = 0 then
    Exit;
  n := Power(2, Zoom);
  Pt.X := Trunc(((Deg.Lon + 180) / 360) * n);
  C := ((1 - (ln(Tan(LatRad) + (1 / Cos(LatRad))) / Pi)) / 2 * n);
  Pt.Y := Trunc(C);
  C := C - Pt.Y;

  TileRect := GetTileRect(Zoom, Pt);
  LResY := (TileRect.MaxLat - TileRect.MinLat) / 256; // resolution per pixel

  Result := LResY;
  if C = 0 then
    Exit;
  Pt.Y := Pt.Y + 1;
  TileRect := GetTileRect(Zoom, Pt);
  LResY1 := (TileRect.MaxLat - TileRect.MinLat) / 256; // resolution per pixel
  Result := ((1- C) * LResY1) + (C * LResY);
end;

function GetMapPosition(Zoom: Integer; MPos: TPoint): TCoord;
var
  n, LatRad: Double;
begin
  n := Power(2, Zoom);
  LatRad := Arctan(Sinh(Pi * (1 - 2 * MPos.Y / n)));
  Result.Lat := RadtoDeg(LatRad);
  Result.Lon := MPos.X / n * 360.0 - 180.0;
end;

function GetTileRect(Zoom: Integer; MPos: TPoint): TRectCoord;
var
  Pt: TCoord;
  p: float;
begin
  if MPos.X < 0 then
    MPos.X := 0;
  if MPos.Y < 0 then
    MPos.Y := 0;
  p := Power(2, Zoom);
  if MPos.X >= p then
    MPos.X := 0;
  if MPos.Y >= p then
    MPos.Y := 0;
  Pt := GetMapPosition(Zoom, MPos);
  Result.MinLat := Pt.Lat;
  Result.MinLon := Pt.Lon;
  MPos.X := MPos.X + 1;
  MPos.Y := MPos.Y + 1;
  Pt := GetMapPosition(Zoom, MPos);
  Result.MaxLat := Pt.Lat;
  Result.MaxLon := Pt.Lon;
end;

function GetResolution(Zoom: Integer; Deg: TCoord): Double;
begin
  Result := 156543.03 * cos(DegToRad(Deg.Lat)) / Power(2, Zoom);
end;

function BuildURL(Zoom: Integer; MPos: TPoint): string;
begin
  Result := BASEURL + IntToStr(Zoom) + '/' + IntToStr(MPos.X) + '/' + intToStr(MPos.Y) + '.png';
end;

function BuildFilename(Zoom: Integer; MPos: TPoint): string;
begin
  Result := IncludeTrailingPathDelimiter(DataDir) + BASEFILE + IntToStr(Zoom) + '_' + IntToStr(MPos.X) + '_' + intToStr(MPos.Y) + '.png';
end;

function GetPosbyString(PosStr: string; var Coord: TCoord; var Zoom: Integer): Boolean;
var
  P1: SizeInt;
  SZoom, SLat, SLon: String;
  Lat, Lon: Extended;
begin
  Result := False;
  P1 := Pos(' ', PosStr);
  if P1 > 0 then
  begin
    SZoom := '';
    SLat := Copy(PosStr, 1, P1 - 1);
    SLon := Copy(PosStr, P1 + 1, Length(PosStr));
    P1 := Pos(' ', SLon);
    if P1 > 0 then
    begin
      SZoom:= Copy(SLon, P1 + 1, Length(SLon));
      Delete(SLon, P1, Length(SLon));
    end;
    Lat := StrToFloatDef(SLat, NaN);
    Lon := StrToFloatDef(SLon, NaN);
    if not IsNan(Lat) and not IsNan(Lon) then
    begin
      Zoom := StrToIntDef(SZoom, Zoom);
      //
      Coord.Lat := Lat;
      Coord.Lon := Lon;
      Result := True;
    end;
  end;
end;


end.

