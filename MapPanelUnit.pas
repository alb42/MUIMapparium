unit MapPanelUnit;
{$mode objfpc}{$H+}
interface

uses
  Math, Classes, SysUtils, MUIPaintBoxUnit, imagesunit, positionunit,
  osmhelper, mui, agraphics, intuition, utility, prefsunit, types, layers,
  cybergraphics, waypointunit, muiwrap, strutils;

type
  TMapButton = record
    Area: TRect;
    Pressed: Boolean;
  end;

  TMapPanel = class(TMUIPaintBox)
  private
    DownPos: Classes.TPoint;
    FLastMouse: Classes.TPoint;
    StartCoord: TCoord;
    // middle Marker

    FOnUpdateLocationLabel: TProcedure;
    FOnSidePanelOpen: TProcedure;
    //
    FShowMarker: Boolean;
    FShowTracks: Boolean;
    FShowRoutes: Boolean;
    //
    SidePanelBtn: TMapButton;
    ZoomPanelBtn: TMapButton;
    FShowZoomPanel: Boolean;
    FShowSidePanelBtn: Boolean;
    ZoomInBtn: TMapButton;
    ZoomOutBtn: TMapButton;
  private
    procedure SetShowMarker(AValue: Boolean);
    procedure SetShowTracks(AValue: Boolean);
    procedure SetShowRoutes(AValue: Boolean);
  protected
    procedure DrawMiddleMarker(RP: PRastPort; DrawRange: TRect);
    procedure DrawMarker(RP: PRastPort; DrawRange: TRect; UsePens: Boolean = True);
    procedure DrawTracks(RP: PRastPort; DrawRange: TRect; UsePens: Boolean = True);
    procedure DrawRoute(RP: PRastPort; DrawRange: TRect; ARoute: TRoute; UsePens: Boolean = True);
    procedure DrawRoutes(RP: PRastPort; DrawRange: TRect; UsePens: Boolean = True);
    procedure DrawGUI(RP: PRastPort; DrawRange: TRect);
    procedure SetShowSidePanelBtn(AValue: Boolean);
    //
    function DoSetup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Setup): PtrUInt; override;
    function DoCleanup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Cleanup): PtrUInt; override;
  public
    MiddleMarker: LongWord;
    MiddleMarkerColor: LongWord;
    MiddleMarkerSize: LongWord;
    LeftDown: Boolean;
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
    procedure SaveToFile(AFileName: string);

    function PixelToPos(T: Classes.TPoint): TCoord;
    function PosToPixel(C: TCoord): Classes.TPoint;
    function CoordToPixel(NCoord: TTileCoord): Classes.TPoint;

    property OnUpdateLocationLabel: TProcedure read FOnUpdateLocationLabel write FOnUpdateLocationLabel;
    property OnSidePanelOpen: TProcedure read FOnSidePanelOpen write FOnSidePanelOpen;
    property LastMouse: Classes.TPoint read FLastMouse;
    property ShowSidePanelBtn: Boolean read FShowSidePanelBtn write SetShowSidePanelBtn;

    property ShowMarker: Boolean read FShowMarker write SetShowMarker;
    property ShowTracks: Boolean read FShowTracks write SetShowTracks;
    property ShowRoutes: Boolean read FShowRoutes write SetShowRoutes;
  end;

var
  MUIMapPanel: TMapPanel;

implementation
uses
  TrackPropsUnit, RoutePropsUnit, fpwritepng;


// #####################################################################
//   MapPanel.Create
constructor TMapPanel.Create(const Args: array of PtrUInt);
begin
  inherited;
  FShowZoomPanel := False;
  FShowSidePanelBtn := True;
  // Middle Marker Settings
  MiddleMarker := Prefs.MiddleMarker;
  MiddleMarkerColor := Prefs.MiddleMarkerColor;
  MiddleMarkerSize := Prefs.MarkerSize;
  // init the size Parameter
  MinWidth := 170;
  DefWidth := 256;
  MinHeight := 100;
  DefHeight := 256;
  // visiblity
  FShowMarker:= True;
  FShowTracks := True;
  FShowRoutes := True;
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
  // Tweak the code as we like
  if Key = '+' then
    Code := 94;
  if Key = '-' then
    Code := 93;
  //
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
  if PtInRect(SidePanelBtn.Area, Point(X,Y)) then
  begin
    if Assigned(FOnSidePanelOpen) then
      FOnSidePanelOpen();
    ResetDblClickTime;
    Exit;
  end;
  if PtInRect(ZoomPanelBtn.Area, Point(X,Y)) then
  begin
    FShowZoomPanel := not FShowZoomPanel;
    RedrawObject;
    ResetDblClickTime;
    Exit;
  end;
  if FShowZoomPanel then
  begin
    if PtInRect(ZoomInBtn.Area, Point(X,Y)) then
    begin
      ZoomIn(False);
      ResetDblClickTime;
      Exit;
    end;
    if PtInRect(ZoomOutBtn.Area, Point(X,Y)) then
    begin
      ZoomOut;
      ResetDblClickTime;
      Exit;
    end;
  end;
  LeftDown := True;
  DownPos := Point(X, Y);
  FLastMouse := DownPos;
  StartCoord := MiddlePos;
  EatEvent := False;
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
    EatEvent := True;
  end;
end;

// Mouse Move
procedure TMapPanel.MouseMoveEvent(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
var
  NPos: TTileCoord;
begin
  FLastMouse := Point(X, Y);
  if LeftDown then
  begin
    MoveOffset.X := 0;
    MoveOffset.Y := 0;
    NPos.Pixel.X :=  MiddleCoord.Pixel.X - (X - DownPos.X);
    NPos.Pixel.Y :=  MiddleCoord.Pixel.Y - (Y - DownPos.Y);
    NPos.Tile.X := MiddleCoord.Tile.X;
    NPos.Tile.Y := MiddleCoord.Tile.Y;
    MiddlePos := TileToCoord(CurZoom, NPos);
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

procedure TMapPanel.SaveToFile(AFileName: string);
var
  PTMid: Classes.TPoint;
  LOffset: Classes.TPoint;
  LocalRP: PRastPort;
  li: pLayer_Info;
  DrawRect: Classes.TRect;
  SavePict: TFPAMemImage;
  Writer: TFPWriterPNG;
  RP: PRastPort;
begin
  try
    DrawRect := Rect(0, 0, RecordedSize.X, RecordedSize.Y);
    //
    PTMid.X := DrawRect.Width div 2;
    PTMid.Y := DrawRect.Height div 2;
    // get the Rastport of the image
    RP := nil;
    if Assigned(MUIObject) then
      RP := Obj_Rp(MUIObject);
    if not Assigned(RP) then
      Exit;
    // Make a temporary Rastport to draw to
    LocalRP := CreateRastPort;
    li := NewLayerInfo(); // Layerinfo we also need
    // Bitmap and layer for the temp rastport
    LocalRP^.Bitmap := AllocBitMap(DrawRect.Width, DrawRect.Height, rp^.Bitmap^.Depth, BMF_MINPLANES or BMF_DISPLAYABLE, RP^.Bitmap);
    LocalRP^.Layer := CreateUpFrontLayer(li, LocalRP^.Bitmap, 0, 0, DrawRect.Width - 1, DrawRect.Height - 1, LAYERSIMPLE, nil);
    SetFont(LocalRP, RP^.Font);
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
    if FShowTracks then
      DrawTracks(LocalRP, DrawRect, False);
    // Draw Routes
    if FShowRoutes then
      DrawRoutes(LocalRP, DrawRect, False);
    if Assigned(CurRoute) then
      DrawRoute(LocalRP, DrawRect, CurRoute, False);
    // Draw Marker
    if FShowMarker then
      DrawMarker(LocalRP, DrawRect, False);
    //
    SavePict := TFPAMemImage.Create(RecordedSize.X, RecordedSize.Y);
    ReadPixelArray(SavePict.Data, 0, 0, SavePict.Width * SizeOf(LongWord), LocalRP, 0, 0, SavePict.Width, SavePict.Height, RECTFMT_RGBA);
    Writer := TFPWriterPNG.Create;
    SavePict.SaveToFile(AFileName, Writer);

  finally
    Writer.Free;
    SavePict.Free;
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
end;


// Draw Event
procedure TMapPanel.DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
var
  PTMid: Classes.TPoint;
  LOffset: Classes.TPoint;
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
  LocalRP^.Layer := CreateUpFrontLayer(li, LocalRP^.Bitmap, 0, 0, DrawRect.Width - 1, DrawRect.Height - 1, LAYERSIMPLE, nil);
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
  if FShowTracks then
    DrawTracks(LocalRP, DrawRect);
  // Draw Routes
  if FShowRoutes then
    DrawRoutes(LocalRP, DrawRect);
  if Assigned(CurRoute) and (RouteList.IndexOf(CurRoute) < 0) then
    DrawRoute(LocalRP, DrawRect, CurRoute);
  // Draw Marker
  if FShowMarker then
    DrawMarker(LocalRP, DrawRect);
  // Draw Middle Marker
  DrawMiddleMarker(LocalRP, DrawRect);
  // Draw Zoom and Position GUI
  DrawGUI(LocalRP, DrawRect);
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
  PTMid: Classes.TPoint;
  //Pen: LongWord;
begin
  {$ifndef MorphOS}
  PTMid.X := DrawRange.Width div 2;
  PTMid.Y := DrawRange.Height div 2;
  {$else}
  PTMid.X := DrawRange.Width div 2 + 4;
  PTMid.Y := DrawRange.Height div 2 + 4;
  {$endif}
  //Pen := SetColor(RP, MiddleMarkerColor);
  SetAPen(RP, BlackPen);
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
  //UnSetColor(Pen);
end;

// Draw the Waypoints
procedure TMapPanel.DrawMarker(RP: PRastPort; DrawRange: TRect; UsePens: Boolean = True);
const
  AREA_BYTES = 4000;
var
  //WPColor: LongWord = $0000FF;
  i,j: Integer;
  Points: packed array of packed record
    x,y: SmallInt;
  end;
  pt: Classes.TPoint;
  Ras: TPlanePtr;
  TRas: TTmpRas;
  WarBuff: array[0..AREA_BYTES] of Word;
  ari: TAreaInfo;
  //Pen: LongWord;
  TE: TTextExtent;
  MarkerText: string;
  MarkerPen: LongInt;
begin
  //Pen := SetColor(RP, WPColor);
  SetAPen(RP, BluePen);
  SetDrMd(RP, JAM1);
  // make tmprast
  Ras := AllocRaster(DrawRange.Width, DrawRange.Height);
  InitTmpRas(@TRas, ras, DrawRange.Width * DrawRange.Height * 3);
  for i := 0 to MarkerList.Count - 1 do
  begin
    if MarkerList[i].Visible then
    begin
      MarkerPen := -1;
      if UsePens then
      begin
        case MarkerList[i].Color of
          clRed: SetAPen(RP, RedPen);
          clGreen: SetAPen(RP, GreenPen);
          clBlue: SetAPen(RP, BluePen);
          clBlack: SetAPen(RP, BlackPen);
          else
            MarkerPen := SetColor(RP, MarkerList[i].Color);
        end;
      end
      else
        MarkerPen := SetColor(RP, MarkerList[i].Color);
      //
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
      UnSetColor(MarkerPen);
    end;
  end;
  RP^.TmpRas := nil;
  RP^.AreaInfo := nil;
  FreeRaster(Ras, DrawRange.Width, DrawRange.Height);
end;

// Draw the tracks
procedure TMapPanel.DrawTracks(RP: PRastPort; DrawRange: TRect; UsePens: Boolean = True);
const
  AREA_BYTES = 4000;
var
  PT: Classes.TPoint;
  i, j: Integer;
  LastWasDrawn: Boolean;
  TrackPtSize: Integer;
  Drawn: Integer;
  ShowActivePt: Boolean;
  Points: array[0..3] of record
    X: SmallInt;
    Y: SmallInt;
  end;
  TrackPen: LongInt;
begin
  TrackPtSize := Max(2, CurZoom - 10);
  SetAPen(RP, RedPen);
  SetDrMd(RP, JAM1);
  // Draw Tracks
  for i := 0 to TrackList.Count - 1 do
  begin
    if not TrackList[i].Visible then
      Continue;
    TrackPen := -1;
    if UsePens then
    begin
      case TrackList[i].Color of
        clRed: SetAPen(RP, RedPen);
        clGreen: SetAPen(RP, GreenPen);
        clBlue: SetAPen(RP, BluePen);
        clBlack: SetAPen(RP, BlackPen);
        else
          TrackPen := SetColor(RP, TrackList[i].Color);
      end;
    end
    else
      TrackPen := SetColor(RP, TrackList[i].Color);
    ShowActivePt := (TrackList[i] = CurTrack);
    Drawn := 0;
    LastWasDrawn := False;
    TrackList[i].CalcCoords(CurZoom);
    for j := 0 to High(TrackList[i].Pts) do
    begin
      if (TrackList[i].Coords[j].Tile.X < 0) and (TrackList[i].Coords[j].Tile.Y < 0) then
        Continue;
      Pt := CoordToPixel(TrackList[i].Coords[j]);
      if (Pt.X >= -100) and (Pt.Y >= -100) and (Pt.X <= DrawRange.Width + 100) and (Pt.Y <= DrawRange.Height + 100) then
      begin
        Inc(Drawn);
        RectFill(RP, Pt.X - TrackPtSize, Pt.Y - TrackPtSize, Pt.X + TrackPtSize, Pt.Y + TrackPtSize);
        if not LastWasDrawn then
          GfxMove(RP, PT.X, PT.Y)
        else
          Draw(RP, Pt.X, PT.Y);
        LastWasDrawn := True;
      end
      else
        LastWasDrawn := False;
      UnSetColor(TrackPen);
    end;
    if ShowActivePt and (ActiveTrackPt >=0) and (ActiveTrackPt <= High(TrackList[i].Pts)) then
    begin
      SetAPen(Rp, RedPen);
      Pt := PosToPixel(TrackList[i].Pts[ActiveTrackPt].Position);
      if (Pt.X >= -100) and (Pt.Y >= -100) and (Pt.X <= DrawRange.Width + 100) and (Pt.Y <= DrawRange.Height + 100) then
      begin
        Points[0].X := Pt.X;
        Points[0].Y := Pt.Y;
        Points[1].X := Points[0].X - 5;
        Points[1].Y := Points[0].Y - 20;
        Points[2].X := Points[0].X + 5;
        Points[2].Y := Points[0].Y - 20;
        Points[3].X := Pt.X;
        Points[3].Y := Pt.Y;
        GfxMove(Rp, Pt.X, Pt.Y);
        PolyDraw(RP, 4, @Points[0]);
      end;
    end;
  end;
  //UnSetColor(Pen);
end;

procedure TMapPanel.DrawRoute(RP: PRastPort; DrawRange: TRect; ARoute: TRoute; UsePens: Boolean = True);
const
  AREA_BYTES = 4000;
var
  PT: Classes.TPoint;
  j: Integer;
  LastWasDrawn: Boolean;
  RoutePtSize: Integer;
  //Pen: LongWord;
  Drawn: Integer;
  ShowActivePt: Boolean;
  RoutePen: LongInt;
begin
  if not ARoute.Visible then
    Exit;
  RoutePtSize := Max(2, CurZoom - 10);
  SetAPen(RP, RedPen);
  SetDrMd(RP, JAM1);
  //
  RoutePen := -1;
  if UsePens then
  begin
    case ARoute.Color of
      clRed: SetAPen(RP, RedPen);
      clGreen: SetAPen(RP, GreenPen);
      clBlue: SetAPen(RP, BluePen);
      clBlack: SetAPen(RP, BlackPen);
      else
        RoutePen := SetColor(RP, ARoute.Color);
    end;
  end
  else
      RoutePen := SetColor(RP, ARoute.Color);
  ShowActivePt := (ARoute = CurRoute);
  Drawn := 0;
  LastWasDrawn := False;
  ARoute.CalcCoords(CurZoom);
  for j := 0 to High(ARoute.Pts) do
  begin
    if (ARoute.Coords[j].Tile.X < 0) and (ARoute.Coords[j].Tile.Y < 0) then
      Continue;
    Pt := CoordToPixel(ARoute.Coords[j]);
    if (Pt.X >= -100) and (Pt.Y >= -100) and (Pt.X <= DrawRange.Width + 100) and (Pt.Y <= DrawRange.Height + 100) then
    begin
      Inc(Drawn);
      RectFill(RP, Pt.X - RoutePtSize, Pt.Y - RoutePtSize, Pt.X + RoutePtSize, Pt.Y + RoutePtSize);
      if not LastWasDrawn then
        GfxMove(RP, PT.X, PT.Y)
      else
        Draw(RP, Pt.X, PT.Y);
      LastWasDrawn := True;
    end
    else
      LastWasDrawn := False;
  end;
  UnSetColor(RoutePen);
  LastWasDrawn := False;
  if ShowActivePt and Assigned(CurOrder) then
  begin
    SetAPen(Rp, GreenPen);
    for j := 0 to High(CurOrder.Positions) do
    begin
      Pt := PosToPixel(CurOrder.Positions[j]);
      if (Pt.X >= -100) and (Pt.Y >= -100) and (Pt.X <= DrawRange.Width + 100) and (Pt.Y <= DrawRange.Height + 100) then
      begin
        Inc(Drawn);
        RectFill(RP, Pt.X - RoutePtSize, Pt.Y - RoutePtSize, Pt.X + RoutePtSize, Pt.Y + RoutePtSize);
        if not LastWasDrawn then
          GfxMove(RP, PT.X, PT.Y)
        else
          Draw(RP, Pt.X, PT.Y);
        LastWasDrawn := True;
      end
      else
        LastWasDrawn := False;
    end;
  end;
  SetAPen(RP, RedPen);
  SetDrMd(RP, JAM1);
end;

// Route
// Draw the Routes
procedure TMapPanel.DrawRoutes(RP: PRastPort; DrawRange: TRect; UsePens: Boolean = True);
var
  i: Integer;
begin
  // Draw Tracks
  for i := 0 to RouteList.Count - 1 do
  begin
    DrawRoute(RP, DrawRange, RouteList[i], UsePens);
  end;
end;

procedure TMapPanel.DrawGUI(RP: PRastPort; DrawRange: TRect);
var
  ButtonSize: Classes.TPoint;
  //Pen: LongWord;
begin
  ButtonSize.X := Round(TW(RP, 'W') * 1.2);
  ButtonSize.Y := Round(TH(RP, '|') * 1.2);
  //
  SidePanelBtn.Area := Rect(0, ButtonSize.Y, ButtonSize.X, ButtonSize.Y * 2);
  //
  ZoomPanelBtn.Area := Rect(DrawRange.Width - ButtonSize.X - 1, DrawRange.Height - 2 * ButtonSize.Y, DrawRange.Width - 1, DrawRange.Height - ButtonSize.Y);
  ZoomOutBtn.Area := ZoomPanelBtn.Area;
  ZoomOutBtn.Area.Offset(0, -(ButtonSize.Y + 5));
  ZoomInBtn.Area := ZoomOutBtn.Area;
  ZoomInBtn.Area.Offset(0, -(ButtonSize.Y + 5));

  //Pen := SetColor(RP, clWhite);
  SetAPen(RP, WhitePen);
  //
  RectFill(RP, SidePanelBtn.Area.Left, SidePanelBtn.Area.Top, SidePanelBtn.Area.Right, SidePanelBtn.Area.Bottom);
  if FShowZoomPanel then
  begin
    if CurZoom <= 2 then
      SetAPen(RP, GrayPen)
    else
      SetAPen(RP, WhitePen);
    RectFill(RP, ZoomOutBtn.Area.Left, ZoomOutBtn.Area.Top, ZoomOutBtn.Area.Right, ZoomOutBtn.Area.Bottom);
    if CurZoom >= 19 then
      SetAPen(RP, GrayPen)
    else
      SetAPen(RP, WhitePen);
    RectFill(RP, ZoomInBtn.Area.Left, ZoomInBtn.Area.Top, ZoomInBtn.Area.Right, ZoomInBtn.Area.Bottom);
  end;
  SetAPen(RP, WhitePen);
  RectFill(RP, ZoomPanelBtn.Area.Left, ZoomPanelBtn.Area.Top, ZoomPanelBtn.Area.Right, ZoomPanelBtn.Area.Bottom);
  //
  //UnSetColor(Pen);

  //Pen := SetColor(RP, clBlack);
  SetAPen(RP, BlackPen);
  //
  DrawRect(RP, SidePanelBtn.Area);
  GfxMove(RP, SidePanelBtn.Area.Left + 3, SidePanelBtn.Area.Top + ButtonSize.Y div 2 + 3);
  GfxText(RP, PChar(IfThen(FShowSidePanelBtn, '>', '<')), 1);
  //
  DrawRect(RP, ZoomPanelBtn.Area);
  GfxMove(RP, ZoomPanelBtn.Area.Left + 3, ZoomPanelBtn.Area.Top + ButtonSize.Y div 2 + 3);
  //
  if FShowZoomPanel then
  begin
    GfxText(RP, '>', 1);
    //
    DrawRect(RP, ZoomOutBtn.Area);
    GfxMove(RP, ZoomOutBtn.Area.Left + 3, ZoomOutBtn.Area.Top + ButtonSize.Y div 2 + 3);
    GfxText(RP, '-', 1);
    //
    DrawRect(RP, ZoomInBtn.Area);
    GfxMove(RP, ZoomInBtn.Area.Left + 3, ZoomInBtn.Area.Top + ButtonSize.Y div 2 + 3);
    GfxText(RP, '+', 1);
  end
  else
    GfxText(RP, '<', 1);
  //
  //UnSetColor(Pen);
end;

procedure TMapPanel.SetShowSidePanelBtn(AValue: Boolean);
begin
  if FShowSidePanelBtn = AValue then
    Exit;
  FShowSidePanelBtn := AValue;
  RedrawObject;
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
function TMapPanel.PixelToPos(T: Classes.TPoint): TCoord;
var
  PTMid, MidDist: Classes.TPoint;
  NCoord: TTileCoord;
begin
  PTMid := Point((Width div 2), (Height div 2));

  T.X := T.X - MoveOffset.X;
  T.Y := T.Y - MoveOffset.Y;

  MidDist.X := PTMid.X - MiddleCoord.Pixel.X;
  MidDist.Y := PTMid.Y - MiddleCoord.Pixel.Y;

  NCoord.Tile.X := MiddleCoord.Tile.X;
  NCoord.Tile.Y := MiddleCoord.Tile.Y;

  NCoord.Pixel.X := T.X - MidDist.X;
  NCoord.Pixel.Y := T.Y - MidDist.Y;

  Result := TileToCoord(CurZoom, NCoord);
end;

function TMapPanel.PosToPixel(C: TCoord): Classes.TPoint;
var
  PTMid: Classes.TPoint;
  NCoord: TTileCoord;
  TileDist: Classes.TPoint;
  MidDist: Classes.TPoint;
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

function TMapPanel.CoordToPixel(NCoord: TTileCoord): Classes.TPoint;
var
  PTMid, TileDist, MidDist: Classes.TPoint;
begin
  Result.X := 0;
  Result.Y := 0;

  PTMid := Point((Width div 2), (Height div 2));
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
begin
  if (LastMouse.X = -1) and (LastMouse.Y = -1) then
    Exit;
  OldPosi := PixelToPos(LastMouse);
  CurZoom := Min(19, CurZoom + 1);
  if ToPos then
  begin
    MiddleCoord := CoordToTile(CurZoom, MiddlePos);
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


procedure TMapPanel.SetShowMarker(AValue: Boolean);
begin
  if AValue = FShowMarker then
    Exit;
  FShowMarker := AValue;
  RedrawObject;
end;

procedure TMapPanel.SetShowTracks(AValue: Boolean);
begin
  if AValue = FShowTracks then
    Exit;
  FShowTracks := AValue;
  RedrawObject;
end;

procedure TMapPanel.SetShowRoutes(AValue: Boolean);
begin
  if AValue = FShowRoutes then
    Exit;
  FShowRoutes := AValue;
  RedrawObject;
end;

function TMapPanel.DoSetup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Setup): PtrUInt;
begin
  Result := inherited;
  WhitePen := ObtainPen(clWhite);
  BlackPen := ObtainPen(clBlack);
  GrayPen := ObtainPen(clGray);
  BluePen := ObtainPen(clBlue);
  RedPen := ObtainPen(clRed);
  GreenPen := ObtainPen(clGreen);
  YellowPen := ObtainPen(clYellow);
end;

function TMapPanel.DoCleanup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Cleanup): PtrUInt;
begin
  Result := inherited;
  FreePen(WhitePen);
  FreePen(BlackPen);
  FreePen(GrayPen);
  FreePen(BluePen);
  FreePen(RedPen);
  FreePen(YellowPen);
end;

// Do a Zoom Out
procedure TMapPanel.ZoomOut;
begin
  CurZoom := Max(2, CurZoom - 1);
  RefreshImage;
end;

end.
