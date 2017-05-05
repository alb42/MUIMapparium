program MUIMapparium;
{$mode objfpc}{$H+}

uses
  athreads,
  {$if defined(MorphOS) or defined(Amiga68k)}
  amigalib,
  {$endif}
  imagesunit, positionunit, osmhelper, networkingunit, prefsunit,
  Exec, Utility, intuition, agraphics, Layers, AmigaDos,
  cybergraphics, mui, muihelper, MUIWrap, prefswinunit,
  StatisticsUnit, waypointunit, WPPropsUnit, TrackPropsUnit,
  DOM, XMLRead, XMLWrite, xmlutils, jsonparser, fpjson,
  SysUtils, StrUtils, Types, Classes, Math;

const
  // MainMenu
  MID_QUIT       = 1;
  MID_SIDEPANEL  = 2;
  MID_ZOOMIN     = 3;
  MID_ZOOMOUT    = 4;
  MID_FINDME     = 5;
  MID_Prefs      = 6;
  MID_Statistics = 7;
  MID_Load       = 8;
  MID_Save       = 9;
  // WayMenu
  WID_Toggle = 1;

  TabTitles: array[0..3] of PChar = ('Search'#0, 'WayPoints'#0, 'Tracks'#0, nil);

type
  TMyData = record
    dummy: LongInt;
  end;

var
  Running: Boolean = False;
  EHNode: TMUI_EventHandlerNode;
  DownPos: TPoint;
  LastMouse: TPoint;
  LeftDown: Boolean;
  StartCoord: TCoord;
  MapPanel, CoordsLabel,
  // Search
  SearchEdit, SearchList, SearchListEntry,
  // Waypoints
  WayPointList, WaypointListEntry, WayPointMenu,
  // Tracks
  TracksList, TracksListEntry,
  // Basic
  App, Window: PObject_;
  // MainWindow
  SidePanel, MainBalance, MainMenu: PObject_;
  // Menu
  MenuSidePanel: PObject_;
  //
  SRes: TSearchResults;
  SearchTitleStr: string;
  // middle Marker
  MiddleMarker: LongWord = 1; //
  MiddleMarkerColor: LongWord = 0; //
  MiddleMarkerSize: LongWord = 1;

  WM1: PObject_;

  MyTv: record
    Secs, Micros: LongWord;
  end;

function PixelToPos(T: TPoint): TCoord; forward;
function PosToPixel(C: TCoord): TPoint; forward;
procedure RefreshImage; forward;
procedure ZoomIn(ToPos: Boolean); forward;
procedure ZoomOut; forward;
procedure UpdateLocationLabel; forward;
function BoundingBoxToZoom(BoundString: string): Integer; forward;
procedure ShowSidePanel(ShowIt: Boolean); forward;

procedure DrawMiddleMarker(RP: PRastPort; DrawRange: TRect); forward;
procedure DrawMarker(RP: PRastPort; DrawRange: TRect); forward;
procedure DrawTracks(RP: PRastPort; DrawRange: TRect); forward;
procedure OpenPrefs; forward;
procedure UpdateWayPoints; forward;


//############################################
// SETUP
function mSetup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Setup): PtrUInt;
begin
  Result := DoSuperMethodA(cl, obj, msg);
  EHNode.ehn_Priority := 0;
  EHNode.ehn_Flags := 0;
  EHNode.ehn_Object := obj;
  EHNode.ehn_Class := cl;
  EHNode.ehn_Events := IDCMP_MOUSEBUTTONS or IDCMP_MOUSEMOVE or IDCMP_RAWKEY;
  DoMethod(OBJ_win(obj), [MUIM_Window_AddEventHandler, PtrUInt(@EHNode)]);
end;

//############################################
// CLEANUP
function mCleanup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Cleanup): PtrUInt;
begin
  DoMethod(OBJ_win(obj), [MUIM_Window_RemEventHandler, PtrUInt(@EHNode)]);
  Result := DoSuperMethodA(cl,obj,msg);
end;

//############################################
// ASKMINMAX
//
// AskMinMax method will be called before the window is opened
// and before layout takes place. We need to tell MUI the
// minimum, maximum and default size of our object.
function mAskMinMax(cl: PIClass; Obj: PObject_; Msg: PMUIP_AskMinMax): PtrUInt;
begin
  mAskMinMax := 0;

  // let our superclass first fill in what it thinks about sizes.
  // this will e.g. add the size of frame and inner spacing.

  DoSuperMethodA(cl, obj, msg);

  // now add the values specific to our object. note that we
  // indeed need to *add* these values, not just set them!

  msg^.MinMaxInfo^.MinWidth  := msg^.MinMaxInfo^.MinWidth + 170;
  msg^.MinMaxInfo^.DefWidth  := msg^.MinMaxInfo^.DefWidth + 320;
  msg^.MinMaxInfo^.MaxWidth  := MUI_MAXMAX;

  msg^.MinMaxInfo^.MinHeight := msg^.MinMaxInfo^.MinHeight + 100;
  msg^.MinMaxInfo^.DefHeight := msg^.MinMaxInfo^.DefHeight + 256;
  msg^.MinMaxInfo^.MaxHeight := MUI_MAXMAX;
end;

//############################################
// DRAW
//
// Draw method is called whenever MUI feels we should render
// our object. This usually happens after layout is finished
// or when we need to refresh in a simplerefresh window.
// Note: You may only render within the rectangle
//       OBJ_mleft(obj), OBJ_mtop(obj), OBJ_mwidth(obj), OBJ_mheight(obj).
function mDraw(cl: PIClass; Obj: PObject_; Msg: PMUIP_Draw): PtrUInt;
var
  Clip: Pointer;
  ri: PMUI_RenderInfo;
  rp: PRastPort;
  PTMid: TPoint;
  TileRect: TRectCoord;
  LOffset: TPoint;
  DrawRect: TRect;
  LocalRP: PRastPort;
  li: pLayer_Info;
begin
  mDraw := 0;

  // let our superclass draw itself first, area class would
  // e.g. draw the frame and clear the whole region. What
  // it does exactly depends on msg->flags.
  DoSuperMethodA(cl,obj,msg);
  // if MADF_DRAWOBJECT isn't set, we shouldn't draw anything.
  // MUI just wanted to update the frame or something like that.

  if (Msg^.flags and MADF_DRAWOBJECT) = 0 then
  begin
    Exit;
  end;
  // get render info
  ri := MUIRenderInfo(Obj);
  if not Assigned(ri) then
    Exit;
  // get rastport for drawing
  rp := OBJ_rp(Obj);
  if not Assigned(rp) then
    Exit;
  //
  DrawRect.Left := Obj_mLeft(Obj);
  DrawRect.Top := Obj_mTop(Obj);
  DrawRect.Width := Obj_mWidth(Obj);
  DrawRect.Height := Obj_mHeight(Obj);
  // install the clip region (do not draw over the border)
  clip := MUI_AddClipping(ri, DrawRect.Left, DrawRect.Top, DrawRect.Width, DrawRect.Height);
  // changed size -> redo the image
  if (RecordedSize.X <> DrawRect.Width) or (RecordedSize.Y <> DrawRect.Height) then
    DrawFullImage(DrawRect.Width, DrawRect.Height);
  PTMid.X := DrawRect.Width div 2;
  PTMid.Y := DrawRect.Height div 2;
  TileRect := GetTileRect(CurZoom, MiddleCoord);
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
  if (GResX <> 0) and (GResY <> 0) then
  begin
    LOffset.Y := Round((MiddlePos.Lat - TileRect.MinLat) / GResY);
    LOffset.X := Round((MiddlePos.Lon - TileRect.MinLon) / GResX);
    WritePixelArray(FullBitmap.Data, 0, 0, FullBitmap.Width * SizeOf(LongWord), LocalRP, PTMid.X + ((0 + GPixOff.X)*256) - LOffset.X, PTMid.Y + ((0 + GPixOff.Y)*256) - LOffset.Y, FullBitmap.Width, FullBitmap.Height, RECTFMT_RGBA);
    RedrawImage := False;
  end;
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
  // remove the clip regio again
  MUI_RemoveClipRegion(ri, clip);
end;

//############################################
// Handle Event
//
function mHandleEvent(cl: PIClass; Obj: PObject_; Msg: PMUIP_HandleEvent): PtrUInt;
var
  RelX, RelY: Integer;
begin
  Result := DoSuperMethodA(cl,obj,msg);
  if OBJ_IsInObject(Msg^.Imsg^.MouseX, Msg^.Imsg^.MouseY, Obj) and not Boolean(MH_Get(Window, MUIA_Window_Sleep)) then
  begin
    //writeln('Event: ',Msg^.imsg^.IClass,' ', Msg^.imsg^.Code);
    case Msg^.imsg^.IClass of
      // Mouse Buttons
      IDCMP_MOUSEBUTTONS:
      begin
        RelX := Msg^.imsg^.MouseX - obj_Left(obj);
        RelY := Msg^.imsg^.MouseY - obj_Top(obj);
        case Msg^.imsg^.Code of
          SELECTDOWN:
          begin
            if DoubleClick(MyTv.Secs, MyTv.Micros, Msg^.imsg^.Seconds, Msg^.imsg^.Micros) and
              (DownPos.X - RelX < 2) and (DownPos.Y - RelY < 2) then
            begin
              ZoomIn(True);
              LeftDown := False;
            end
            else
            begin
              MyTv.Secs := Msg^.imsg^.Seconds;
              MyTv.Micros := Msg^.imsg^.Micros;
              LeftDown := True;
              DownPos := Point(RelX, RelY);
              LastMouse := DownPos;
              StartCoord := MiddlePos;
            end;
            Result := MUI_EventHandlerRC_Eat;
          end;
          SELECTUP:
          begin
            if LeftDown then
            begin
              LeftDown := False;
              RefreshImage();
            end;
            Result := MUI_EventHandlerRC_Eat;
          end;
        end;
      end;
      // Mouse Move
      IDCMP_MOUSEMOVE:
      begin
        RelX := Msg^.imsg^.MouseX - obj_Left(obj);
        RelY := Msg^.imsg^.MouseY - obj_Top(obj);
        LastMouse := Point(RelX, RelY);
        if LeftDown then
        begin
          MiddlePos.Lon := StartCoord.Lon - (RelX - DownPos.X) * GResX;
          MiddlePos.Lat := StartCoord.Lat - (RelY - DownPos.Y) * GResY;
          RedrawImage := True;
        end
        else
        begin
          UpdateLocationLabel;
        end;
        Result := MUI_EventHandlerRC_Eat;
      end;
      // Raw Key
      IDCMP_RAWKEY:
      begin
        if (Msg^.imsg^.Code = $7A) or (Msg^.imsg^.Code = $7B) then
        begin
          if Msg^.imsg^.Code = $7A then
            ZoomIn(False)
          else
            if Msg^.imsg^.Code = $7B then
              ZoomOut;
        end;
        Result := MUI_EventHandlerRC_Eat;
      end;
    end;
  end
  else
  begin
    if LeftDown then
    begin
      LeftDown := False;
      RefreshImage();
    end;
  end;
end;


// Here comes the dispatcher for our custom class.
// Unknown/unused methods are passed to the superclass immediately.

function MyDispatcher(cl: PIClass; Obj: PObject_; Msg: intuition.PMsg): PtrUInt;
begin
  case Msg^.MethodID of
    MUIM_Setup: MyDispatcher := mSetup(cl, Obj, Pointer(Msg));
    MUIM_Cleanup: MyDispatcher := mCleanup(cl, Obj, Pointer(Msg));
    //
    MUIM_AskMinMax: MyDispatcher := mAskMinMax(cl, Obj, Pointer(Msg));
    //
    MUIM_Draw: MyDispatcher := mDraw(cl, Obj, Pointer(Msg));
    MUIM_HANDLEEVENT: MyDispatcher := mHandleEvent(cl, Obj, Pointer(Msg));
    else
      MyDispatcher := DoSuperMethodA(cl, obj, msg);
  end;
end;


// *********************************************************************
// other Procedures

// make a new Waypoint
procedure NewWpt;
var
  Marker: TMarker;
begin
  Marker := TMarker.Create;
  Marker.Position.Lat := MiddlePos.Lat;
  Marker.Position.Lon := MiddlePos.Lon;
  Marker.Name := 'Marker ' + IntToStr(MarkerList.Add(Marker));
  UpdateWayPoints;
end;


// Draw the middle Marker
procedure DrawMiddleMarker(RP: PRastPort; DrawRange: TRect);
var
  PTMid: TPoint;
  {$ifdef Amiga}
  Pen: LongWord;
  {$endif}
begin
  {$ifndef MorphOS}
  PTMid.X := DrawRange.Width div 2;
  PTMid.Y := DrawRange.Height div 2;
  {$else}
  PTMid.X := DrawRange.Width div 2 + 4;
  PTMid.Y := DrawRange.Height div 2 + 4;
  {$endif}
  {$ifdef Amiga}
  Pen := ObtainBestPenA(IntuitionBase^.ActiveScreen^.ViewPort.ColorMap, MiddleMarkerColor shl 8,MiddleMarkerColor shl 16,MiddleMarkerColor shl 24, nil);
  SetAPen(RP, Pen);
  {$else}
  SetRPAttrs(RP,[
    RPTAG_PenMode, AsTag(False),
    RPTAG_FGColor, AsTag(MiddleMarkerColor),
    TAG_DONE]);
  {$endif}
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
  {$ifdef Amiga}
  ReleasePen(IntuitionBase^.ActiveScreen^.ViewPort.ColorMap, Pen);
  {$endif}
end;

// Draw the Waypoints
procedure DrawMarker(RP: PRastPort; DrawRange: TRect);
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
  {$ifdef Amiga}
  Pen: LongWord;
  {$endif}
  TE: TTextExtent;
  MarkerText: string;
begin
  {$ifdef Amiga}
  Pen := ObtainBestPenA(IntuitionBase^.ActiveScreen^.ViewPort.ColorMap, WPColor shl 8,WPColor shl 16,WPColor shl 24, nil);
  SetAPen(RP, Pen);
  {$else}
  SetRPAttrs(RP,[
    RPTAG_PenMode, AsTag(False),
    RPTAG_FGColor, AsTag(WPColor),
    TAG_DONE]);
  {$endif}
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
  {$ifdef Amiga}
  ReleasePen(IntuitionBase^.ActiveScreen^.ViewPort.ColorMap, Pen);
  {$endif}
end;

// Draw the tracks
procedure DrawTracks(RP: PRastPort; DrawRange: TRect);
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
  {$ifdef Amiga}
  Pen: LongWord;
  {$endif}
begin
  TrackPtSize := Max(2, CurZoom - 10);
  {$ifdef Amiga}
  Pen := ObtainBestPenA(IntuitionBase^.ActiveScreen^.ViewPort.ColorMap, TrackColor shl 8,TrackColor shl 16,TrackColor shl 24, nil);
  SetAPen(RP, Pen);
  {$else}
  SetRPAttrs(RP,[
    RPTAG_PenMode, AsTag(False),
    RPTAG_FGColor, AsTag(TrackColor),
    TAG_DONE]);
  {$endif}
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
  {$ifdef Amiga}
  ReleasePen(IntuitionBase^.ActiveScreen^.ViewPort.ColorMap, Pen);
  {$endif}
end;

// Refresh the FullBitmap
procedure RefreshImage;
var
  Width, Height: LongInt;
begin
  if Assigned(MapPanel) then
  begin
    //MH_Set(app, MUIA_Application_Sleep, AsTag(True));
    Width := Obj_Width(MapPanel);
    Height := Obj_Height(MapPanel);
    //writeln('Draw Full: ', Width,' x ', Height);
    DrawFullImage(Width, Height);
    ReDrawImage := True;
    //MH_Set(app, MUIA_Application_Sleep, AsTag(False));
  end;
end;

// Calculate Pixel to real coordinate
function PixelToPos(T: TPoint): TCoord;
var
  PTMid: TPoint;
begin
  if Assigned(MapPanel) then
  begin
    PTMid := Point((Obj_Width(MapPanel) div 2), (Obj_Height(MapPanel) div 2));
    Result.Lon := MiddlePos.Lon - ((PTMid.X - T.X) * GResX);
    Result.Lat := MiddlePos.Lat - ((PTMid.Y - T.Y) * GResY);
  end;
end;

function PosToPixel(C: TCoord): TPoint;
var
  PTMid: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
  if (GResX = 0) or (GResY = 0) or not assigned(MapPanel) then
    Exit;
  PTMid := Point((Obj_Width(MapPanel) div 2), (Obj_Height(MapPanel) div 2));
  Result.X := Round(Max(-MaxInt / 2, Min(MaxInt / 2, 0 - (((MiddlePos.Lon - C.Lon) / GResX) - PTMid.X))));
  Result.Y := Round(Max(-MaxInt / 2, Min(MaxInt / 2, 0 - (((MiddlePos.Lat - C.Lat) / GResY) - PTMid.Y))));
end;

// calculate the Zoom level, that the Bounding box is completely visible
// needed for search
function BoundingBoxToZoom(BoundString: string): Integer;
var
  SL: TStringList;
  MinLat, MaxLat, MinLon, MaxLon, DiffLat, DiffLon: Double;
  Deg: TCoord;
  i: Integer;
  Pt: TPoint;
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

// do a Zoom in (ToPos = using Mouse position to zoom to)
procedure ZoomIn(ToPos: Boolean);
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
procedure ZoomOut;
begin
  CurZoom := Max(2, CurZoom - 1);
  RefreshImage;
end;

// Update the Location below the picture
procedure UpdateLocationLabel;
var
  Coord: TCoord;
  StrCoord: string;
begin
  Coord := PixelToPos(LastMouse);
  StrCoord := FloatToStrF(Coord.Lat, ffFixed, 8,6) + ' ; ' + FloatToStrF(Coord.Lon, ffFixed, 8,6) + ' ; ' + IntToStr(CurZoom) + '  ';
  StrCoord := Format('%25s', [StrCoord]);
  MH_Set(CoordsLabel, MUIA_Text_Contents, AsTag(PChar(StrCoord)));
end;

// Update WayPoints;
procedure UpdateWayPoints;
var
  i: Integer;
begin
  DoMethod(WaypointListEntry, [MUIM_List_Clear]);
  for i := 0 to MarkerList.Count - 1 do
  begin
    MarkerList[i].FullName := MarkerList[i].Name;
    if not MarkerList[i].Visible then
      MarkerList[i].FullName := '(' + MarkerList[i].FullName + ')';
    DoMethod(WaypointListEntry, [MUIM_List_InsertSingle, AsTag(PChar(MarkerList[i].FullName)), AsTag(MUIV_List_Insert_Bottom)]);
  end;
  RedrawImage := True;
end;

// Update Tracks;
procedure UpdateTracks;
var
  i: Integer;
begin
  DoMethod(TracksListEntry, [MUIM_List_Clear]);
  for i := 0 to TrackList.Count - 1 do
  begin
    TrackList[i].FullName := TrackList[i].Name + ' (' + IntToStr(Length(TrackList[i].Pts)) + ') ' + TrackList[i].Desc;
    if not TrackList[i].Visible then
      TrackList[i].FullName := '(' + TrackList[i].FullName + ')';
    DoMethod(TracksListEntry, [MUIM_List_InsertSingle, AsTag(PChar(TrackList[i].FullName)), AsTag(MUIV_List_Insert_Bottom)]);
  end;
  RedrawImage := True;
end;

// Search for SearchTerm -> show in sidepanel, open Sidepanel
procedure SearchEntry(SearchTerm: AnsiString);
var
  Mem: TMemoryStream;
  Doc: TXMLDocument;
  Url, EncStr: String;
  Child, Node: TDOMNode;
  i: Integer;
  sr: TSearchResult;
  SText: string;
begin
  DoMethod(SearchListEntry, [MUIM_List_Clear]);
  if not IsOnline then
  begin
    SearchTitleStr :=  'Not Online'#0;
    MH_Set(SearchListEntry, MUIA_List_Title, AsTag(@SearchTitleStr[1]));
    ShowSidePanel(True);
    Exit;
  end;

  SearchTerm := StringReplace(SearchTerm, '/', ' ', [rfReplaceAll]);
  SearchTerm := StringReplace(SearchTerm, '?', ' ', [rfReplaceAll]);
  SearchTerm := StringReplace(SearchTerm, '#', ' ', [rfReplaceAll]);
  SearchTerm := StringReplace(SearchTerm, ':', ' ', [rfReplaceAll]);
  SearchTerm := StringReplace(SearchTerm, ';', ' ', [rfReplaceAll]);
  SearchTerm := AnsiToUTF8(SearchTerm);
  EncStr := '';
  for i := 1 to Length(SearchTerm) do
    EncStr := EncStr + '%' + IntToHex(Ord(SearchTerm[i]),2);
  Url := SEARCHURL + EncStr + '?format=xml&accept-language=' + Prefs.SearchLang;
  Mem := TMemoryStream.Create;
  SText := '';
  try
    if GetFile(Url, Mem) then
    begin
      Mem.Position := 0;
      ReadXMLFile(Doc, Mem);
      SRes.Clear;
      Child := Doc.DocumentElement.FirstChild;
      while Assigned(Child) do
      begin
        Node := Child.Attributes.GetNamedItem('class');
        if Assigned(Node) then
        begin
          if Node.NodeValue = 'boundary' then
          begin
            Child := Child.NextSibling;
            Continue;
          end;
        end;
        Node := Child.Attributes.GetNamedItem('display_name');
        if Assigned(Node) then
        begin
          sr := TSearchResult.Create;
          SRes.Add(Sr);
          Sr.DisplayName := AnsiString(Node.NodeValue);
          Node := Child.Attributes.GetNamedItem('lat');
          if Assigned(Node) then
            Sr.Lat := StrToFloatDef(AnsiString(Node.NodeValue), 0);
          Node := Child.Attributes.GetNamedItem('lon');
          if Assigned(Node) then
            Sr.lon := StrToFloatDef(AnsiString(Node.NodeValue), 0);
          Node := Child.Attributes.GetNamedItem('boundingbox');
          if Assigned(Node) then
          begin
            Sr.Zoom := BoundingBoxToZoom(AnsiString(Node.NodeValue));
          end;
          DoMethod(SearchListEntry, [MUIM_List_InsertSingle, AsTag(PChar(SR.DisplayName)), AsTag(MUIV_List_Insert_Bottom)]);
        end;
        Child := Child.NextSibling;
      end;
      if SRes.Count = 0 then
      begin
        SearchTitleStr :=  'Nothing found for "'+SearchTerm+'"'#0;
        MH_Set(SearchListEntry, MUIA_List_Title, AsTag(@SearchTitleStr[1]));
      end
      else
      begin
        SearchTitleStr :=  IntToStr(SRes.Count) + ' Results for "'+SearchTerm+'"'#0;
        MH_Set(SearchListEntry, MUIA_List_Title, AsTag(@SearchTitleStr[1]));
        if SRes.Count = 1 then
        begin
          MiddlePos.Lat := SR.Lat;
          MiddlePos.Lon := SR.Lon;
          CurZoom:= SR.Zoom;
          RefreshImage;
        end;
      end;
    end else
    begin
      SearchTitleStr :=  'Network error'#0;
      MH_Set(SearchListEntry, MUIA_List_Title, AsTag(@SearchTitleStr[1]));
    end;
  finally
    Mem.Free;
    Doc.Free;
  end;
  MH_Set(SearchListEntry, MUIA_Floattext_Text, AsTag(PChar(SText)));
  if SRes.Count <> 1 then
    ShowSidePanel(True);
end;



procedure SearchIP(ip: string);
var
  St: TStringStream;
  Url: String;
  SLat, SLon: string;
  Lat, Lon: Double;
  jData: TJSONData;
  jObject: TJSONObject;
  c, r: TJSONStringType;
begin
  //Url := 'http://ipinfo.io';
  URL:='http://ip-api.com/json';
  if ip <> '' then
    Url := URL + '/' + ip;
  St := TStringStream.Create('');
  try
    if GetCurlFile(Url, St) then
    begin
      St.Position := 0;
      jData := GetJSON(St);
      jObject := TJSONObject(jData);
      SLat := jObject.Get('lat'); //Copy(Location, 1, P1 - 1);
      SLon := jObject.Get('lon');//Copy(Location, P1 + 1, Length(Location));
      Lat := StrToFloatDef(SLat, Nan);
      Lon := StrToFloatDef(SLon, Nan);
      if not IsNan(Lat) and not IsNan(Lon) then
      begin
        c := jObject.Get('city', '');
        r := jObject.Get('regionName', '');
        //l := jObject.Get('country', '');
        if c <> '' then
          CurZoom := 11
        else
        if r <> '' then
          CurZoom := 8
        else
          CurZoom := 6;
        MiddlePos.Lat := Lat;
        MiddlePos.Lon := Lon;
        RefreshImage;
        //TrackPosition := False;
        //LocationPanel.Caption := AnsiToUtf8(c + ' ' + r + ' ' + l);
      end;
    end;
  finally
    St.Free;
  end;
end;

//###################################
// Search Ack
function SearchAck(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  SText: string;
  P1, NZoom: Integer;
  Lat, Lon: Double;
  SZoom, SLon, SLat: string;
  NMPos: TCoord;
begin
  SText := PChar(MH_Get(SearchEdit, MUIA_String_Contents));
  if SText <> '' then
  begin
    if LowerCase(Copy(SText, 1, 3)) = 'ip:' then
    begin
      Delete(SText, 1, 3);
      SearchIP(SText);
      Exit;
    end;
    if LowerCase(Copy(SText, 1, 4)) = 'geo:' then
    begin
      SZoom := '';
      SLat := '';
      SLon := '';
      Delete(SText, 1, 4);
      P1 := Pos(',', SText);
      if P1 > 0 then
      begin
        SLat := Copy(SText, 1, P1 - 1);
        Delete(SText, 1, P1);
      end;
      P1 := Pos('?', SText);
      if P1 < 1 then
        P1 := Pos(',', SText);
      if P1 < 1 then
        P1 := Pos(';', SText);
      if (P1 > 1) or (Length(SText) > 0) then
      begin
        if P1 > 1 then
          SLon := Copy(SText, 1, P1 - 1)
        else
          SLon := SText;
        Delete(SText, 1, P1);
      end;
      if Length(SText) > 2 then
      begin
        if (SText[1] = 'z') and (SText[2] = '=') then
        begin
          Delete(SText, 1, 2);
          SZoom := SText;
        end;
      end;
      Lat := StrToFloatDef(SLat, NaN);
      Lon := StrToFloatDef(SLon, NaN);
      if not IsNan(Lat) and not IsNan(Lon) then
      begin
        CurZoom := StrToIntDef(SZoom, CurZoom);
        //
        MiddlePos.Lat := Lat;
        MiddlePos.Lon := Lon;
        RefreshImage;
        //TrackPosition := False;
        //LocationPanel.Caption := '';
      end;
      Exit;
    end;
    NZoom := CurZoom;
    NMPos := MiddlePos;
    if GetPosbyString(SText, NMPos, NZoom) then
    begin
      CurZoom := NZoom;
      //
      MiddlePos.Lat := NMPos.Lat;
      MiddlePos.Lon := NMPos.Lon;
      RefreshImage;
      //TrackPosition := False;
      //LocationPanel.Caption := '';
      Exit;
    end;
    SearchEntry(SText);
  end;
  MH_Set(SearchEdit, MUIA_String_Contents, AsTag(''));
  Result := 0;
end;

//###################################
// Menu Event
function MenuEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  //writeln('menu event');
  case MH_Get(App, MUIA_Application_MenuAction) of
    MID_Load: if LoadWayFile then
      begin
        UpdateWayPoints;
        UpdateTracks;
      end;
    MID_Save: SaveWayFile;
    MID_Quit: Running := False;
    MID_SidePanel: ShowSidePanel(Boolean(MH_Get(MenuSidePanel, MUIA_Menuitem_Checked)));
    MID_ZOOMIN: ZoomIn(False);
    MID_ZOOMOUT: ZoomOut;
    MID_FINDME: SearchIP('');
    MID_PREFS: OpenPrefs();
    MID_Statistics: MH_Set(StatWin, MUIA_Window_Open, AsTag(True));
  end;
  Result := 0;
end;

//###################################
// Double Click to Search Result
function SearchDblEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active: LongInt;
  Sr: TSearchResult;
begin
  Result := 0;
  Active := MH_Get(SearchListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < SRes.Count) then
  begin
    Sr := SRes[Active];
    if Assigned(Sr) then
    begin
      MiddlePos.Lat := SR.Lat;
      MiddlePos.Lon := SR.Lon;
      CurZoom:= SR.Zoom;
      RedrawImage := True;
    end;
  end;
end;

//###################################
// Double Click to WayPoint
function DblWayPointEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active: LongInt;
  Ma: TMarker;
begin
  Result := 0;
  Active := MH_Get(WayPointListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < MarkerList.Count) then
  begin
    Ma := MarkerList[Active];
    if Assigned(Ma) then
    begin
      MiddlePos.Lat := Ma.Position.Lat;
      MiddlePos.Lon := Ma.Position.Lon;
      RefreshImage;
    end;
  end;
end;

//###################################
// Double Click to Track
function DblTrackEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active: LongInt;
  Tr: TTrack;
  MinC, MaxC: TCoord;
  DiffLat, DiffLon: ValReal;
  Pt: Classes.TPoint;
  Rec: TRectCoord;
  i: Integer;
begin
  Result := 0;
  Active := MH_Get(TracksListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < TrackList.Count) then
  begin
    Tr := TrackList[Active];
    if Assigned(Tr) then
    begin
      for i := 0 to High(Tr.Pts) do
      begin
        if i = 0 then
        begin
          MinC := Tr.Pts[0].Position;
          MaxC := Tr.Pts[0].Position;
        end else
        begin
          MinC.Lat := Min(MinC.Lat, Tr.Pts[i].Position.Lat);
          MinC.Lon := Min(MinC.Lon, Tr.Pts[i].Position.Lon);
          MaxC.Lat := Max(MaxC.Lat, Tr.Pts[i].Position.Lat);
          MaxC.Lon := Max(MaxC.Lon, Tr.Pts[i].Position.Lon);
        end;
      end;
      if Length(Tr.Pts) > 0 then
      begin
        DiffLat := Abs(MaxC.Lat - MinC.Lat);
        DiffLon := Abs(MaxC.Lon - MinC.Lon);
        MiddlePos.Lat:= (MaxC.Lat + MinC.Lat) / 2;
        MiddlePos.Lon:= (MaxC.Lon + MinC.Lon) / 2;

        for i := 0 to 18 do
        begin
          Pt := GetTileCoord(i, MiddlePos);
          Rec := GetTileRect(i, Pt);
          if (Abs(Rec.MaxLat - Rec.MinLat) >= DiffLat) and (Abs(Rec.MaxLon - Rec.MinLon) >= DiffLon) then
            CurZoom := i;
        end;
        CurZoom := CurZoom + 1;
        RefreshImage;
      end;
    end;
  end;
end;

// AddWayPoint Button
function AddWayEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  NewWpt;
end;

// Remove WayPoint Button
function RemWayEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active: LongInt;
begin
  Result := 0;
  Active := MH_Get(WayPointListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < MarkerList.Count) then
  begin
    MarkerList.Delete(Active);
    DoMethod(WaypointListEntry, [MUIM_List_Remove, Active]);
    MUI_Redraw(MapPanel, MADF_DRAWOBJECT);
  end;
end;

// Edit Marker
function EditWayEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Ma: TMarker;
  Active: Integer;
begin
  Result := 0;
  Active := MH_Get(WayPointListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < MarkerList.Count) then
  begin
    Ma := MarkerList[Active];
    ShowWPProps(Ma);
  end;
end;


// Remove Track Button
function RemTrackEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active: LongInt;
begin
  Result := 0;
  Active := MH_Get(TracksListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < TrackList.Count) then
  begin
    TrackList.Delete(Active);
    DoMethod(TracksListEntry, [MUIM_List_Remove, Active]);
    MUI_Redraw(MapPanel, MADF_DRAWOBJECT);
  end;
end;

// Edit Track
function EditTrackEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Tr: TTrack;
  Active: Integer;
begin
  Result := 0;
  Active := MH_Get(TracksListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < TrackList.Count) then
  begin
    Tr := TrackList[Active];
    ShowTrackProps(Tr);
  end;
end;

//###################################
// WayPoint Menu entries
function WPToggleEvent(Hook: PHook; Obj: PObject_; AMsg: Pointer): NativeInt;
var
  Active: Integer;
  Ma: TMarker;
begin
  Result := 0;
  Active := MH_Get(WayPointListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < MarkerList.Count) then
  begin
    Ma := MarkerList[Active];
    Ma.Visible := not Ma.Visible;
    Ma.FullName := Ma.Name;
    if not Ma.Visible then
      Ma.FullName := '(' + Ma.FullName + ')';
    DoMethod(WaypointListEntry, [MUIM_List_Remove, Active]);
    DoMethod(WaypointListEntry, [MUIM_List_InsertSingle, AsTag(PChar(Ma.FullName)), Active]);
    MUI_Redraw(MapPanel, MADF_DRAWOBJECT);
  end;
end;

//###################################
// Open the SidePanel
procedure ShowSidePanel(ShowIt: Boolean);
begin
  if Boolean(MH_Get(MenuSidePanel, MUIA_Menuitem_Checked)) <> ShowIt then
    MH_Set(MenuSidePanel, MUIA_Menuitem_Checked, AsTag(ShowIt));
  MH_Set(SidePanel, MUIA_ShowMe, AsTag(ShowIt));
  MH_Set(MainBalance, MUIA_ShowMe, AsTag(ShowIt));
end;
// event for the close button
function CloseSideEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  ShowSidePanel(False);
end;

//##################################
// Open Prefs
//
procedure OpenPrefs;
begin
  MH_Set(Window, MUIA_Window_Sleep, AsTag(True));
  OpenPrefsWindow;
end;

//#####################################################
// Update Prefs to local variables for faster access
//
procedure UpdatePrefs;
begin
  MiddleMarker := Prefs.MiddleMarker;
  MiddleMarkerColor := Prefs.MiddleMarkerColor;
  MiddleMarkerSize := Prefs.MarkerSize;
  MaxImages := Prefs.MaxTiles;
  // let redraw to make the marker changes visible
  RedrawImage := True;
end;

// WayPoint Property edit changed something ;-)
procedure WPChangedEvent;
begin
  UpdateWayPoints;
  MUI_Redraw(MapPanel, MADF_DRAWOBJECT);
end;

// Tracks Property edit changed something ;-)
procedure TrackChangedEvent;
begin
  UpdateTracks;
  MUI_Redraw(MapPanel, MADF_DRAWOBJECT);
end;


// *********************************************************************
// ####################################
// Main Routine

procedure StartMe;
var
  Sigs: LongInt;
  MCC: PMUI_CustomClass;
  StrCoord: string;
  MenuHook, SearchAckHook, DblSearchHook,DblWayPointHook, CloseSideHook: THook;
  RemTrack: PObject_;
  RemTrackHook, DblTrackHook: Thook;
  AddWay, RemWay, EditWay, EditTrack: PObject_;
  AddWayHook, RemWayHook, EditWayHook, EditTrackHook: Thook;
  SideCloseButton: PObject_;
  StartTime: Int64;
  WayMenuHooks: array[0..0] of THook;
begin
  SRes := TSearchResults.Create;
  // Prefs
  UpdatePrefs;
  OnUpdatePrefs := @UpdatePrefs;
  //
  OnWPChanged := @WPChangedEvent;
  OnTrackChanged := @TrackChangedEvent;
  try
    // Create the new custom class with a call to MH_CreateCustomClass().
    // Caution: This function returns not a struct IClass, but a
    // TMUI_CustomClass which contains a struct IClass to be
    // used with MH_NewObject() calls.
    // Note well: MUI creates the dispatcher hook for you, you may
    // *not* use its h_Data field! If you need custom data, use the
    // cl_UserData of the IClass structure!

    MCC := MH_CreateCustomClass(nil, MUIC_Area, nil, SizeOf(TMyData), @MyDispatcher);
    if not Assigned(MCC) then
    begin
      writeln('Could not create custom class.');
      Exit;
    end;
    //
    SearchTitleStr :=  'Search Results'#0;
    //
    StrCoord := FloatToStrF(MiddlePos.Lat, ffFixed, 8,6) + ' ; ' + FloatToStrF(MiddlePos.Lon, ffFixed, 8,6) + ' ; ' + IntToStr(CurZoom) + '  ';
    StrCoord := Format('%25s', [StrCoord]);
    //
    // Popupmenu for WayPointList ++++++++++++++++++++++++++++++++++++++
    WayPointMenu := MH_Menustrip([
      Child, AsTag(MH_Menu('Waypoint', [
        Child, AsTag(MH_MenuItem(WM1, [
          MUIA_Menuitem_Title, AsTag(PChar('Toggle visibility')),
          MUIA_UserData, WID_Toggle,
          TAG_DONE])),
        TAG_DONE])),
      TAG_DONE]);
    //
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //
    // Side Panel
    SidePanel := MH_VGroup([
      MUIA_ShowMe, AsTag(False),
      Child, AsTag(MH_HGroup([
        // Header with close button
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Image(SideCloseButton, [
          MUIA_Image_Spec, MUII_ArrowRight,
          MUIA_InputMode, MUIV_InputMode_RelVerify,
          MUIA_ShowSelState, AsTag(True),
          TAG_DONE])),
        TAG_DONE])),
      Child, AsTag(MH_Register([
        MUIA_Register_Titles, AsTag(@TabTitles),
        // Search list
        Child, AsTag(MH_ListView(SearchList, [
          MUIA_Weight, 100,
          MUIA_Listview_Input, MUI_TRUE,
          MUIA_Listview_List, AsTag(MH_List(SearchListEntry, [
            MUIA_Frame, MUIV_Frame_ReadList,
            MUIA_Background, MUII_ReadListBack,
            MUIA_List_PoolThreshSize, 256,
            MUIA_List_Title, AsTag(PChar(SearchTitleStr)),
            TAG_DONE])),
          TAG_DONE])),
        // WayPoints
        Child, AsTag(MH_VGroup([
          Child, AsTag(MH_ListView(WaypointList, [
            MUIA_Weight, 100,
            MUIA_Listview_Input, MUI_TRUE,
            MUIA_Listview_List, AsTag(MH_List(WaypointListEntry, [
              MUIA_Frame, MUIV_Frame_ReadList,
              MUIA_Background, MUII_ReadListBack,
              MUIA_ContextMenu, AsTag(WayPointMenu),
              MUIA_List_PoolThreshSize, 256,
              TAG_DONE])),
            TAG_DONE])),
          Child, AsTag(MH_HGroup([
              Child, AsTag(MH_Button(AddWay, 'Add')),
              Child, AsTag(MH_Button(RemWay, 'Remove')),
              Child, AsTag(MH_Button(EditWay, 'Show/Edit')),
            TAG_DONE])),
          TAG_DONE])),
        Child, AsTag(MH_VGroup([
          Child, AsTag(MH_ListView(TracksList, [
            MUIA_Weight, 100,
            MUIA_Listview_Input, MUI_TRUE,
            MUIA_Listview_List, AsTag(MH_List(TracksListEntry, [
              MUIA_Frame, MUIV_Frame_ReadList,
              MUIA_Background, MUII_ReadListBack,
              //MUIA_ContextMenu, AsTag(WayPointMenu),
              MUIA_List_PoolThreshSize, 256,
              TAG_DONE])),
            TAG_DONE])),
          Child, AsTag(MH_HGroup([
              Child, AsTag(MH_Button(RemTrack, 'Remove')),
              Child, AsTag(MH_Button(EditTrack, 'Show/Edit')),
            TAG_DONE])),
          TAG_DONE])),
        TAG_DONE])),
      TAG_DONE]);
    if not Assigned(SidePanel) then
      writeln('Failed to create SidePanel');
    //
    // Main Menu +++++++++++++++++++++++++++++++++++++++++++++++++++++++
    MainMenu := MH_Menustrip([
      // Project Menu -----------------------------------
      Child, AsTag(MH_Menu('Project',[
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar('Load...')),
          MUIA_Menuitem_Shortcut, AsTag('L'),
          MUIA_UserData, MID_Load,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar('Save...')),
          MUIA_Menuitem_Shortcut, AsTag('S'),
          MUIA_UserData, MID_Save,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar('Quit')),
          MUIA_Menuitem_Shortcut, AsTag('Q'),
          MUIA_UserData, MID_Quit,
          TAG_DONE])),
        TAG_DONE])),
      // Map Menu ----------------------------------
      Child, AsTag(MH_Menu('Map',[
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar('Find me')),
          MUIA_UserData, MID_FINDME,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(-1),
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar('Zoom in')),
          MUIA_UserData, MID_ZOOMIN,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar('Zoom out')),
          MUIA_UserData, MID_ZOOMOUT,
          TAG_DONE])),
        TAG_DONE])),
      // Window Menu ------------------------------------
      Child, AsTag(MH_Menu('Window',[
        Child, AsTag(MH_MenuItem(MenuSidePanel, [
          MUIA_Menuitem_Title, AsTag(PChar('Side Panel')),
          MUIA_Menuitem_Checkit, AsTag(True),
          MUIA_Menuitem_Toggle, AsTag(True),
          MUIA_UserData, MID_SIDEPANEL,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar('Prefs')),
          MUIA_UserData, MID_Prefs,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar('Statistics')),
          MUIA_UserData, MID_Statistics,
          TAG_DONE])),
        TAG_DONE])),
      // About Menu -----------------
      Child, AsTag(MH_Menu('About',[
        TAG_DONE])),
      TAG_DONE]);
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    // Application +++++++++++++++++++++++++++++++++++++++++++++++++++++
    App := MH_Application([
      MUIA_Application_Title,       AsTag('MUIMapparium'),
      MUIA_Application_Version,     AsTag('$VER: MUIMapparium 0.2 (01.05.2017)'),
      MUIA_Application_Copyright,   AsTag('(c)2017, Marcus "ALB" Sackrow'),
      MUIA_Application_Author,      AsTag('Marcus "ALB" Sackrow'),
      MUIA_Application_Description, AsTag('Open Street Map viewer. (MUI)'),
      MUIA_Application_Base,        AsTag('MAPPARIUM'),

      SubWindow, AsTag(MH_Window(Window, [
        MUIA_Window_Title,     AsTag('MUIMapparium 0.1'),
        MUIA_Window_ID,        AsTag(MAKE_ID('M','A','P','P')),
        MUIA_Window_MenuStrip, AsTag(MainMenu),
        WindowContents, AsTag(MH_VGroup([


          Child, AsTag(MH_String(SearchEdit, [
            MUIA_Frame, MUIV_Frame_String,
            TAG_DONE])),
          Child, AsTag(MH_HGroup([
            Child, AsTag(SidePanel),
            Child, AsTag(MH_Balance(MainBalance, [MUIA_CycleChain, 1, MUIA_ShowMe, AsTag(False), TAG_END])),
            Child, AsTag(MH_NewObject(MapPanel, mcc^.mcc_Class, nil, [
              MUIA_Weight, 200,
              MUIA_Frame, MUIV_Frame_Text,
              MUIA_Background, MUII_BACKGROUND,
              MUIA_Font, AsTag(MUIV_Font_Button),
              MUIA_FillArea, AsTag(False),
              MUIA_InnerLeft, 0,
              MUIA_InnerTop, 0,
              MUIA_InnerBottom, 0,
              MUIA_InnerRight, 0,
              TAG_DONE])),
            TAG_DONE])),
          Child, AsTag(MH_HGroup([
            Child, AsTag(MH_Text('Map data '+#169+' OpenStreetMap contributors    ')),
            Child, AsTag(MH_Text(CoordsLabel, PChar(StrCoord))),
            TAG_DONE])),

          TAG_DONE])),

        TAG_DONE])),
      SubWindow, AsTag(PrefsWin),
      SubWindow, AsTag(StatWin),
      SubWindow, AsTag(WPPropsWin),
      SubWindow, AsTag(TrackPropsWin),
      TAG_DONE]);
    if not Assigned(app) then
    begin
      writeln('Failed to create Application');
      Exit;
    end;
    //
    MH_Set(CoordsLabel, MUIA_Text_PreParse, AsTag(PChar(MUIX_R)));
    // Close main window
    DoMethod(Window, [MUIM_Notify, MUIA_Window_CloseRequest, MUI_TRUE,
      AsTag(app), 2, AsTag(MUIM_Application_ReturnID), AsTag(MUIV_Application_ReturnID_Quit)]);
    // Enter Search text
    ConnectHookFunction(MUIA_String_Acknowledge, MUIV_EveryTime, SearchEdit, nil, @SearchAckHook, @SearchAck);
    // double click to a Search entry
    ConnectHookFunction(MUIA_Listview_DoubleClick, MUIV_EveryTime, SearchList, nil, @DblSearchHook, @SearchDblEvent);
    // double click to a WayPoint Entry
    ConnectHookFunction(MUIA_Listview_DoubleClick, MUIV_EveryTime, WayPointList, nil, @DblWayPointHook, @DblWayPointEvent);
    // do any menu thing
    ConnectHookFunction(MUIA_Window_MenuAction, MUIV_EveryTime, Window, nil, @MenuHook, @MenuEvent); // MainMenu
    // WayPointMenu events
    ConnectHookFunction(MUIA_Menuitem_Trigger, MUIV_EveryTime, WM1, nil, @WayMenuHooks[0], @WPToggleEvent);
    // Add WayPoint Hook
    ConnectHookFunction(MUIA_Pressed, AsTag(False), AddWay, nil, @AddWayHook, @AddWayEvent);
    ConnectHookFunction(MUIA_Pressed, AsTag(False), RemWay, nil, @RemWayHook, @RemWayEvent);
    ConnectHookFunction(MUIA_Pressed, AsTag(False), EditWay, nil, @EditWayHook, @EditWayEvent);
    // double click to a Track Entry
    ConnectHookFunction(MUIA_Listview_DoubleClick, MUIV_EveryTime, TracksList, nil, @DblTrackHook, @DblTrackEvent);
    // Remove Track
    ConnectHookFunction(MUIA_Pressed, AsTag(False), RemTrack, nil, @RemTrackHook, @RemTrackEvent);
    ConnectHookFunction(MUIA_Pressed, AsTag(False), EditTrack, nil, @EditTrackHook, @EditTrackEvent);
    // close side panel
    ConnectHookFunction(MUIA_Pressed, AsTag(False), SideCloseButton, nil, @CloseSideHook, @CloseSideEvent);


    DoMethod(PrefsWin, [MUIM_Notify, MUIA_Window_CloseRequest, MUI_TRUE,
      AsTag(PrefsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
    DoMethod(PrefsWin, [MUIM_Notify, MUIA_Window_Open, MUI_FALSE,
      AsTag(Window), 3, MUIM_SET, MUIA_Window_Sleep, AsTag(False)]);
    //

    // Update waypoints before open the window first time
    UpdateWayPoints;
    UpdateTracks;
    //
    // open the window
    MH_Set(Window, MUIA_Window_Open, AsTag(True));
    //
    // This is the ideal input loop for an object oriented MUI application.
    // Everything is encapsulated in classes, no return ids need to be used,
    // we just check if the program shall terminate.
    // Note that MUIM_Application_NewInput expects sigs to contain the result
    // from Wait() (or 0). This makes the input loop significantly faster.

    if MH_Get(Window, MUIA_Window_Open) <> 0 then
    begin
      Running := True;
      // first drawing of the image
      RefreshImage;
      // main cycle
      StartTime := GetMUITime;
      while Integer(DoMethod(app, [MUIM_Application_NewInput, AsTag(@sigs)])) <> MUIV_Application_ReturnID_Quit do
      begin
        if UpdateImage then
        begin
          RefreshImage;
          UpdateImage := False;
          UpdateLocationLabel;
          //writeln(7);
        end;
        if RedrawImage then
        begin
          MUI_Redraw(MapPanel, MADF_DRAWOBJECT);
          ReDrawImage := False;
          //writeln(8);
        end;
        // Update Status things
        if GetMUITime - StartTime > 500 then
        begin
          SetTilesToLoad(ImagesToLoad);
          SetNumTiles(ImageList.Count);
          SetNumFromNet(LNumLoaded);
          SetNumFromHD(LNumLoadedHD);
          AddDownloaded(LBytes, LTimes);
          RedrawList;
        end;
        if Sigs <> 0 then
        begin
          Sigs := Wait(sigs or SIGBREAKF_CTRL_C);
          if (Sigs and SIGBREAKF_CTRL_C) <>0 then
            Break;
        end;
        if not Running then
          Break;
      end;
    end;

    // end of main loop, close window if still open
    MH_Set(Window, MUIA_Window_Open, AsTag(False));

  finally
    if Assigned(WayPointMenu) then
      MUI_DisposeObject(WayPointMenu);
    if Assigned(App) then
      MUI_DisposeObject(app);
    if Assigned(MCC) then
      MUI_DeleteCustomClass(MCC);
    SRes.Free;
  end;
end;

begin
  StartMe;
end.
