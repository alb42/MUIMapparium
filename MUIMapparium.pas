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
  StatisticsUnit,
  DOM, XMLRead, XMLWrite, xmlutils, jsonparser, fpjson,
  SysUtils, StrUtils, Types, Classes, Math;

const
  MID_QUIT       = 1;
  MID_SIDEPANEL  = 2;
  MID_ZOOMIN     = 3;
  MID_ZOOMOUT    = 4;
  MID_FINDME     = 5;
  MID_Prefs      = 6;
  MID_Statistics = 7;

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
  MapPanel, CoordsLabel, SearchEdit, SearchList, SearchListEntry,
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

  MyTv: record
    Secs, Micros: LongWord;
  end;

function PixelToPos(T: TPoint): TCoord; forward;
procedure RefreshImage; forward;
procedure ZoomIn(ToPos: Boolean); forward;
procedure ZoomOut; forward;
procedure UpdateLocationLabel; forward;
function BoundingBoxToZoom(BoundString: string): Integer; forward;
procedure ShowSidePanel(ShowIt: Boolean); forward;

procedure DrawMiddleMarker(RP: PRastPort; DrawRange: TRect); forward;
procedure OpenPrefs; forward;


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
          end;
          SELECTUP:
          begin
            if LeftDown then
            begin
              LeftDown := False;
              RefreshImage();
            end;
          end;
        end;
        Result := MUI_EventHandlerRC_Eat;
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

// Draw the middle Marker
procedure DrawMiddleMarker(RP: PRastPort; DrawRange: TRect);
var
  PTMid: TPoint;
begin
  PTMid.X := DrawRange.Width div 2;
  PTMid.Y := DrawRange.Height div 2;
  {$ifdef Amiga68k}
  SetAPen(RP, ObtainBestPenA(IntuitionBase^.ActiveScreen^.ViewPort.ColorMap, MiddleMarkerColor shl 8,MiddleMarkerColor shl 16,MiddleMarkerColor shl 24, nil));
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
  case MH_Get(App, MUIA_Application_MenuAction) of
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
  Sr := nil;
  if (Active >= 0) and (Active < SRes.Count) then
  begin
    Sr := SRes[Active];
    if Assigned(Sr) then
    begin
      MiddlePos.Lat := SR.Lat;
      MiddlePos.Lon := SR.Lon;
      CurZoom:= SR.Zoom;
      RefreshImage;
    end;
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

// *********************************************************************
// ####################################
// Main Routine

procedure StartMe;
var
  Sigs: LongInt;
  MCC: PMUI_CustomClass;
  StrCoord: string;
  MenuHook, SearchAckHook, DblSearchHook: THook;
  StartTime: Int64;
begin
  SRes := TSearchResults.Create;
  // Prefs
  UpdatePrefs;
  OnUpdatePrefs := @UpdatePrefs;
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
    // Side Panel
    SidePanel := MH_HGroup([
      MUIA_ShowMe, AsTag(False),
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
      TAG_DONE]);
    if not Assigned(SidePanel) then
      writeln('Failed to create SidePanel');
    //
    // Main Menu
    MainMenu := MH_Menustrip([
      // Project Menu
      Child, AsTag(MH_Menu('Project',[
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar('Quit')),
          MUIA_Menuitem_Shortcut, AsTag('Q'),
          MUIA_UserData, MID_Quit,
          TAG_DONE])),
        TAG_DONE])),
      // Map Menu
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
      // Window Menu
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
      // About Menu
      Child, AsTag(MH_Menu('About',[
        TAG_DONE])),
      TAG_DONE]);


    // Application
    App := MH_Application([
      MUIA_Application_Title,       AsTag('MUIMapparium'),
      MUIA_Application_Version,     AsTag('$VER: MUIMapparium 0.1 (22.04.2017)'),
      MUIA_Application_Copyright,   AsTag('(c)2017, Marcus "ALB" Sackrow'),
      MUIA_Application_Author,      AsTag('Marcus "ALB" Sackrow'),
      MUIA_Application_Description, AsTag('Mapparium Open Street Map viewer. MUI-Version'),
      MUIA_Application_Base,        AsTag('MAPPARIUM'),
      MUIA_Application_MenuStrip,   AsTag(MainMenu),

      SubWindow, AsTag(MH_Window(Window, [
        MUIA_Window_Title,     AsTag('MUIMapparium 0.1'),
        MUIA_Window_ID,        AsTag(MAKE_ID('M','A','P','P')),
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
              MUIA_FillArea, AsTag(False),
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
      TAG_DONE]);
    if not Assigned(app) then
    begin
      writeln('Failed to create Application');
      Exit;
    end;
    //
    MH_Set(CoordsLabel, MUIA_Text_PreParse, AsTag(PChar(MUIX_R)));
    //
    DoMethod(Window, [MUIM_Notify, MUIA_Window_CloseRequest, MUI_TRUE,
      AsTag(app), 2, AsTag(MUIM_Application_ReturnID), AsTag(MUIV_Application_ReturnID_Quit)]);
    //
    ConnectHookFunction(MUIA_String_Acknowledge, MUIV_EveryTime, SearchEdit, nil, @SearchAckHook, @SearchAck);
    ConnectHookFunction(MUIA_Application_MenuAction, MUIV_EveryTime, App, nil, @MenuHook, @MenuEvent);
    //
    ConnectHookFunction(MUIA_Listview_DoubleClick, MUIV_EveryTime, SearchList, nil, @DblSearchHook, @SearchDblEvent);

    DoMethod(PrefsWin, [MUIM_Notify, MUIA_Window_CloseRequest, MUI_TRUE,
      AsTag(PrefsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
    DoMethod(PrefsWin, [MUIM_Notify, MUIA_Window_Open, MUI_FALSE,
      AsTag(Window), 3, MUIM_SET, MUIA_Window_Sleep, AsTag(False)]);

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
