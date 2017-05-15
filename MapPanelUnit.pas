unit MapPanelUnit;
{$mode objfpc}{$H+}
interface

uses
  Math, Classes, SysUtils, MUIPaintBoxUnit, imagesunit, positionunit,
  osmhelper, mui, agraphics, intuition, utility, prefsunit, types, layers,
  cybergraphics, waypointunit, muiwrap;

type

  TMapPanel = class(TMUIPaintBox)
  private
    DownPos: TPoint;
    FLastMouse: TPoint;
    LeftDown: Boolean;
    StartCoord: TCoord;
    // middle Marker
    MiddleMarker: LongWord;
    MiddleMarkerColor: LongWord;
    MiddleMarkerSize: LongWord;
    FOnUpdateLocationLabel: TProcedure;
  protected
    procedure DrawMiddleMarker(RP: PRastPort; DrawRange: TRect);
    procedure DrawMarker(RP: PRastPort; DrawRange: TRect);
    procedure DrawTracks(RP: PRastPort; DrawRange: TRect);
  public
    constructor Create(const Args: array of PtrUInt); override;
    procedure DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
    //
    procedure MouseDownEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseUpEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseDblClick(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseMoveEvent(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseWheelEvent(Sender: TObject; ScrollUp: Boolean; var EatEvent: Boolean);
    //
    procedure KeyDownEvent(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    //
    procedure ZoomIn(ToPos: Boolean);
    procedure ZoomOut;
    procedure RefreshImage;
    function PixelToPos(T: TPoint): TCoord;
    function PosToPixel(C: TCoord): TPoint;

    property OnUpdateLocationLabel: TProcedure read FOnUpdateLocationLabel write FOnUpdateLocationLabel;
    property LastMouse: TPoint read FLastMouse;
  end;

implementation


// #####################################################################
//   MapPanel.Create
constructor TMapPanel.Create(const Args: array of PtrUInt);
begin
  inherited;
  // Middle Marker Settings
  MiddleMarker := Prefs.MiddleMarker;
  MiddleMarkerColor := Prefs.MiddleMarkerColor;
  MiddleMarkerSize := Prefs.MarkerSize;
  // init the size Parameter
  MinWidth := 170;
  DefWidth := 256;
  MinHeight := 100;
  DefHeight := 256;
  // connect the events
  OnDrawObject := @DrawEvent;
  OnMUIDblClick := @MouseDblClick;
  OnMUIMouseDown := @MouseDownEvent;
  OnMUIMouseUp := @MouseUpEvent;
  OnMUIMouseMove := @MouseMoveEvent;
  OnMUIMouseWheel := @MouseWheelEvent;

  OnMUIKeyDown := @KeyDownEvent;
end;

// Key Down Event
procedure TMapPanel.KeyDownEvent(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
begin
  case Code of
    94: ZoomIn(False); // mouse wheel up and + on numpad
    93: ZoomOut;       // mouse wheel down and - on numpad
    CURSORDOWN:
      begin
        MiddlePos := PixelToPos(Point(Obj_Width(MUIObject) div 2, Obj_Height(MUIObject) div 2 + 10));
        RefreshImage;
      end;
    CURSORUP:
      begin
        MiddlePos := PixelToPos(Point(Obj_Width(MUIObject) div 2, Obj_Height(MUIObject) div 2 - 10));
        RefreshImage;
      end;
    CURSORRIGHT:
      begin
        MiddlePos := PixelToPos(Point(Obj_Width(MUIObject) div 2 + 10, Obj_Height(MUIObject) div 2));
        RefreshImage;
      end;
    CURSORLEFT:
      begin
        MiddlePos := PixelToPos(Point(Obj_Width(MUIObject) div 2 - 10, Obj_Height(MUIObject) div 2));
        RefreshImage;
      end;
  end;
  EatEvent := True;
end;

// Key Down Event
procedure TMapPanel.MouseDownEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
begin
  LeftDown := True;
  DownPos := Point(X, Y);
  FLastMouse := DownPos;
  StartCoord := MiddlePos;
  EatEvent := True;
end;

// KeyUp Event
procedure TMapPanel.MouseUpEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
begin
  if LeftDown then
  begin
    MoveOffset.X := 0;
    MoveOffset.Y := 0;
    LeftDown := False;
    RefreshImage();
  end;
  EatEvent := True;
end;

// Mouse Move
procedure TMapPanel.MouseMoveEvent(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
begin
  FLastMouse := Point(X, Y);
  if LeftDown then
  begin
    MiddlePos.Lon := StartCoord.Lon - (X - DownPos.X) * GResX;
    MiddlePos.Lat := StartCoord.Lat - (Y - DownPos.Y) * GResY;
    MoveOffset.X := X - DownPos.X;
    MoveOffset.Y := Y - DownPos.Y;
    RedrawImage := True;
  end
  else
  begin
    if Assigned(FOnUpdateLocationLabel) then
      FOnUpdateLocationLabel;
  end;
  EatEvent := True;
end;

// Mouse Double Click
procedure TMapPanel.MouseDblClick(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
begin
  ZoomIn(True);
  LeftDown := False;
  EatEvent := True;
end;

// Mouse Wheel
procedure TMapPanel.MouseWheelEvent(Sender: TObject; ScrollUp: Boolean; var EatEvent: Boolean);
begin
  if ScrollUp then
    ZoomIn(False)
  else
    ZoomOut;
  EatEvent := True;
end;

// Draw Event
procedure TMapPanel.DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
var
  PTMid: TPoint;
  LOffset: TPoint;
  LocalRP: PRastPort;
  li: pLayer_Info;
begin
  if (RecordedSize.X <> DrawRect.Width) or (RecordedSize.Y <> DrawRect.Height) then
    DrawFullImage(DrawRect.Width, DrawRect.Height);
  PTMid.X := DrawRect.Width div 2;
  PTMid.Y := DrawRect.Height div 2;
  // Make a temporary Rastport to draw to
  LocalRP := CreateRastPort;
  li := NewLayerInfo(); // Layerinfo we also need
  // Bitmap and layer for the temp rastport
  LocalRP^.Bitmap := AllocBitMap(DrawRect.Width, DrawRect.Height, rp^.Bitmap^.Depth, BMF_MINPLANES or BMF_DISPLAYABLE, rp^.Bitmap);
  LocalRP^.Layer := CreateUpFrontHookLayer(li, LocalRP^.Bitmap, 0, 0, DrawRect.Width - 1, DrawRect.Height - 1, LAYERSIMPLE, nil, nil);
  SetFont(LocalRP, rp^.Font);
  // initialize to background color
  SetAPen(LocalRP, 0);
  SetBPen(LocalRP, 0);
  // fill with background color
  RectFill(LocalRP, 0, 0, DrawRect.Right, DrawRect.Bottom);
  // draw the actual Images to there
  LOffset.X := MiddleCoord.Pixel.X - MoveOffset.X;
  LOffset.Y := MiddleCoord.Pixel.Y - MoveOffset.Y;
  WritePixelArray(FullBitmap.Data, 0, 0, FullBitmap.Width * SizeOf(LongWord), LocalRP, PTMid.X + ((0 + GPixOff.X)*256) - LOffset.X, PTMid.Y + ((0 + GPixOff.Y)*256) - LOffset.Y, FullBitmap.Width, FullBitmap.Height, RECTFMT_RGBA);
  RedrawImage := False;
  // Draw Tracks
  DrawTracks(LocalRP, DrawRect);
  // Draw Marker
  DrawMarker(LocalRP, DrawRect);
  // Draw Middle Marker
  DrawMiddleMarker(LocalRP, DrawRect);
  // blit the temp rastport to the real one, one step so no flickering
  ClipBlit(LocalRP, 0,0, rp, DrawRect.Left, DrawRect.Top, DrawRect.Width, DrawRect.Height, $00C0);
  // delete the layer
  DeleteLayer(0, LocalRP^.layer);
  DisposeLayerInfo(li);
  // delete the bitmap
  FreeBitmap(LocalRP^.Bitmap);
  LocalRP^.Layer := nil;
  LocalRP^.Bitmap := nil;
  // Destroy the temp rastport
  FreeRastPort(LocalRP);
end;

// Draw the middle Marker
procedure TMapPanel.DrawMiddleMarker(RP: PRastPort; DrawRange: TRect);
var
  PTMid: TPoint;
  Pen: LongWord;
begin
  {$ifndef MorphOS}
  PTMid.X := DrawRange.Width div 2;
  PTMid.Y := DrawRange.Height div 2;
  {$else}
  PTMid.X := DrawRange.Width div 2 + 4;
  PTMid.Y := DrawRange.Height div 2 + 4;
  {$endif}
  Pen := SetColor(RP, MiddleMarkerColor);
  case MiddleMarker of
    1: RectFill(RP, PTMid.X - MiddleMarkerSize, PTMid.Y - MiddleMarkerSize, PTMid.X + MiddleMarkerSize, PTMid.Y + MiddleMarkerSize);
    2: begin
      GfxMove(RP, PTMid.X - (MiddleMarkerSize + 2), PTMid.Y);
      Draw(RP, PTMid.X + MiddleMarkerSize + 2, PTMid.Y);
      GfxMove(RP, PTMid.X, PTMid.Y - (MiddleMarkerSize + 2));
      Draw(RP, PTMid.X, PTMid.Y + MiddleMarkerSize + 2);
    end;
    3: begin
      GfxMove(RP, 0, PTMid.Y);
      Draw(RP, DrawRange.Width, PTMid.Y);
      GfxMove(RP, PTMid.X, 0);
      Draw(RP, PTMid.X, DrawRange.Height);
    end;
  end;
  UnSetColor(Pen);
end;

// Draw the Waypoints
procedure TMapPanel.DrawMarker(RP: PRastPort; DrawRange: TRect);
const
  AREA_BYTES = 4000;
var
  WPColor: LongWord = $0000FF;
  i,j: Integer;
  Points: packed array of packed record
    x,y: SmallInt;
  end;
  pt: TPoint;
  Ras: TPlanePtr;
  TRas: TTmpRas;
  WarBuff: array[0..AREA_BYTES] of Word;
  ari: TAreaInfo;
  Pen: LongWord;
  TE: TTextExtent;
  MarkerText: string;
begin
  Pen := SetColor(RP, WPColor);
  SetDrMd(RP, JAM1);
  // make tmprast
  Ras := AllocRaster(DrawRange.Width, DrawRange.Height);
  InitTmpRas(@TRas, ras, DrawRange.Width * DrawRange.Height * 3);
  for i := 0 to MarkerList.Count - 1 do
  begin
    if MarkerList[i].Visible then
    begin
      SetLength(Points, 4);
      pt := PosToPixel(MarkerList[i].Position);
      Points[0].X := pt.x;
      Points[0].Y := pt.y;
      if (Points[0].X >= -20) and (Points[0].Y >= -20) and (Points[0].X <= DrawRange.Width + 20) and (Points[0].Y <= DrawRange.Height + 20) then
      begin
        Points[1].X := Max(0, Points[0].X - 5);
        Points[1].Y := Max(0, Points[0].Y - 20);
        Points[2].X := Max(0, Points[0].X + 5);
        Points[2].Y := Max(0, Points[0].Y - 20);
        Points[3].X := Points[0].X;
        Points[3].Y := Points[0].Y;

        InitArea(@ari, @WarBuff[0], AREA_BYTES div 5);
        RP^.TmpRas := @TRas;
        RP^.AreaInfo := @Ari;
        AreaMove(RP, Points[0].X, Points[0].Y);
        for j := 0 to High(Points) do
          AreaDraw(RP, Points[j].X, Points[j].Y);
        AreaEnd(RP);

        //TE := Ca.TextExtent(MarkerText);
        MarkerText := MarkerList[i].Name;
        TextExtent(Rp, PChar(MarkerText), Length(MarkerText), @TE);

        GFXMove(RP, Points[0].X - (TE.te_Width div 2), Points[1].Y - TE.te_Height div 2);
        GfxText(Rp, PChar(MarkerText), Length(MarkerText));
        //Ca.TextOut(Points[0].X - (TE.CY div 2), Points[1].Y - TE.cy, AnsiToUtf8(MarkerText));
      end;
    end;
  end;
  RP^.TmpRas := nil;
  RP^.AreaInfo := nil;
  FreeRaster(Ras, DrawRange.Width, DrawRange.Height);
  UnSetColor(Pen);
end;

// Draw the tracks
procedure TMapPanel.DrawTracks(RP: PRastPort; DrawRange: TRect);
const
  AREA_BYTES = 4000;
  TrackColor: LongWord = $FF0000;
var
  PT: TPoint;
  i, j: Integer;
  {Points: packed array of packed record
    x,y: SmallInt;
  end;
  Ras: TPlanePtr;
  TRas: TTmpRas;
  WarBuff: array[0..AREA_BYTES] of Word;
  ari: TAreaInfo;}
  TrackPtSize: Integer;
  Pen: LongWord;
begin
  TrackPtSize := Max(2, CurZoom - 10);
  Pen := SetColor(RP, TrackColor);
  SetDrMd(RP, JAM1);
  // make tmprast
  {
  Ras := AllocRaster(DrawRange.Width, DrawRange.Height);
  InitTmpRas(@TRas, ras, DrawRange.Width * DrawRange.Height * 3);
  }
  // Draw Tracks
  for i := 0 to TrackList.Count - 1 do
  begin
    if not TrackList[i].Visible then
      Continue;
    {if TrackList[i] = CurTrack then
    begin
      Ca.Brush.Color := ActiveTrackColor;
      Ca.Pen.Color := ActiveTrackColor;
    end
    else
    begin
      Ca.Brush.Color := TrackColor;
      Ca.Pen.Color := TrackColor;
    end;
    ShowActivePt := (TrackList[i] = CurTrack);}
    //SetLength(Points, 3);
    for j := 0 to High(TrackList[i].Pts) do
    begin
      Pt := PosToPixel(TrackList[i].Pts[j].Position);
      if (Pt.X >= -100) and (Pt.Y >= -100) and (Pt.X <= DrawRange.Width + 100) and (Pt.Y <= DrawRange.Height + 100) then
      begin
        RectFill(RP, Pt.X - TrackPtSize, Pt.Y - TrackPtSize, Pt.X + TrackPtSize, Pt.Y + TrackPtSize);
        if j = 0 then
          GfxMove(RP, PT.X, PT.Y)
        else
          Draw(RP, Pt.X, PT.Y);
      end;
    end;
    {if ShowActivePt and (ActiveTrackPt >=0) and (ActiveTrackPt <= High(TrackList[i].Pts)) then
    begin
      Ca.Brush.Color := clBlue;
      Ca.Brush.Style := bsSolid;
      Ca.Pen.Color := clBlack;
      Ca.Pen.Style := psSolid;
      Pt := PosToPixel(TrackList[i].Pts[ActiveTrackPt].Position);
      if (Pt.X >= -100) and (Pt.Y >= -100) and (Pt.X <= PB.Width + 100) and (Pt.Y <= PB.Height + 100) then
      begin
        Points[0] := Pt;
        Points[1].X := Max(0, Points[0].X - 5);
        Points[1].Y := Max(0, Points[0].Y - 20);
        Points[2].X := Max(0, Points[0].X + 5);
        Points[2].Y := Max(0, Points[0].Y - 20);
        Ca.Polygon(Points);
      end;
    end;}
  end;
  //RP^.TmpRas := nil;
  //RP^.AreaInfo := nil;
  //FreeRaster(Ras, DrawRange.Width, DrawRange.Height);
  UnSetColor(Pen);
end;

// Refresh the FullBitmap
procedure TMapPanel.RefreshImage;
begin
  //MH_Set(app, MUIA_Application_Sleep, AsTag(True));
  DrawFullImage(Width, Height);
  ReDrawImage := True;
  //MH_Set(app, MUIA_Application_Sleep, AsTag(False));
end;

// Calculate Pixel to real coordinate
function TMapPanel.PixelToPos(T: TPoint): TCoord;
var
  PTMid: TPoint;
begin
  PTMid := Point((Width div 2), (Height div 2));
  Result.Lon := MiddlePos.Lon - ((PTMid.X - T.X) * GResX);
  Result.Lat := MiddlePos.Lat - ((PTMid.Y - T.Y) * GResY);
end;

function TMapPanel.PosToPixel(C: TCoord): TPoint;
var
  PTMid: TPoint;
  NCoord: TTileCoord;
  TileDist: TPoint;
  MidDist: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;

  PTMid := Point((Width div 2), (Height div 2));
  NCoord := CoordToTile(CurZoom, C);
  //
  MidDist.X := PTMid.X - MiddleCoord.Pixel.X;
  MidDist.Y := PTMid.Y - MiddleCoord.Pixel.Y;
  //
  TileDist.X := (NCoord.Tile.X - MiddleCoord.Tile.X) * 256;
  TileDist.Y := (NCoord.Tile.Y - MiddleCoord.Tile.Y) * 256;
  //
  Result.X := TileDist.X + NCoord.Pixel.X + MidDist.X + MoveOffset.X;
  Result.Y := TileDist.Y + NCoord.Pixel.Y + MidDist.Y + MoveOffset.Y;
end;

// do a Zoom in (ToPos = using Mouse position to zoom to)
procedure TMapPanel.ZoomIn(ToPos: Boolean);
var
  Dist, OldPosi, NewPosi: TCoord;
  LMiddleCoord: Classes.TPoint;
  TileRect: TRectCoord;
begin
  if (LastMouse.X = -1) and (LastMouse.Y = -1) then
    Exit;
  OldPosi := PixelToPos(LastMouse);
  CurZoom := Min(19, CurZoom + 1);
  //if CurZoom = 19 then
  //  ZoomInEnabled := False;
  //
  if ToPos then
  begin
    LMiddleCoord := GetTileCoord(CurZoom, MiddlePos);
    TileRect := GetTileRect(CurZoom, LMiddleCoord);
    GResX := (TileRect.MaxLon - TileRect.MinLon) / 256;
    GResY := (TileRect.MaxLat - TileRect.MinLat) / 256;
    //
    NewPosi := PixelToPos(LastMouse);
    //
    Dist.Lat := NewPosi.Lat - MiddlePos.Lat;
    Dist.Lon := NewPosi.Lon - MiddlePos.Lon;
    //
    MiddlePos.Lat := OldPosi.Lat - Dist.Lat;
    MiddlePos.Lon := OldPosi.Lon - Dist.Lon;
    //
  end;
  RefreshImage;
end;

// Do a Zoom Out
procedure TMapPanel.ZoomOut;
begin
  CurZoom := Max(2, CurZoom - 1);
  RefreshImage;
end;

end.
