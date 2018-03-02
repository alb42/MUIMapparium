program MUIMapparium;
{$mode objfpc}{$H+}

uses
  athreads,
  MUIMappariumlocale, MUIPaintBoxUnit,
  imagesunit, positionunit, osmhelper, networkingunit, prefsunit,
  Exec, Utility, intuition, agraphics, Layers, AmigaDos, icon,
  cybergraphics, mui, muihelper, MUIWrap, PrefsWinUnit,
  Statisticsunit, waypointunit, WPPropsUnit, TrackPropsUnit, RoutePropsUnit,
  DOM, XMLRead, XMLWrite, xmlutils, jsonparser, fpjson,
  SysUtils, StrUtils, Types, Classes, Math, versionunit,
  MapPanelUnit, UpdateUnit, aboutwinunit;

const
  // MainMenu
  MID_QUIT        = 1;
  MID_SIDEPANEL   = 2;
  MID_ZOOMIN      = 3;
  MID_ZOOMOUT     = 4;
  MID_FINDME      = 5;
  MID_Prefs       = 6;
  MID_Statistics  = 7;
  MID_Load        = 8;
  MID_Save        = 9;
  MID_DrawMarker  = 10;
  MID_DrawTracks  = 11;
  MID_DrawRoutes  = 12;
  MID_UpdateCheck = 13;
  MID_Help        = 14;
  MID_About       = 15;
  MID_AboutMUI    = 16;
  // WayMenu
  WID_Toggle = 1;

var
  TabStrings: array[0..4] of string = ('Search', 'WayPoints', 'Tracks', 'Routes', 'Images');
  TabTitles: array[0..5] of PChar = ('Search'#0, 'WayPoints'#0, 'Tracks'#0, 'Routes'#0, 'Images'#0, nil);

var
  Running: Boolean = False;

  CoordsLabel,
  // Search
  SearchEdit, SearchList, SearchListEntry,
  // Waypoints
  WayPointList, WaypointListEntry, WayPointMenu,
  // Tracks
  TracksList, TracksListEntry,
  //Routes
  RoutesList, RoutesListEntry,
  // Basic
  App, Window: PObject_;
  // MainWindow
  SidePanel, MainBalance, MainMenu: PObject_;
  // Menu
  MenuSidePanel, MenuDrawMarker, MenuDrawTracks, MenuDrawRoutes: PObject_;
  //
  SRes: TSearchResults;
  SearchTitleStr: string;

  MUIMapPanel: TMapPanel;

  WM1: PObject_;

  SidePanelOpen: Boolean = FALSE;

procedure UpdateLocationLabel; forward;
procedure SidePanelOpenEvent; forward;
function BoundingBoxToZoom(BoundString: string): Integer; forward;
procedure ShowSidePanel(ShowIt: Boolean); forward;

procedure OpenPrefs; forward;
procedure UpdateWayPoints; forward;



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
  Marker.Name := Format(GetLocString(MSG_DEFAULT_WAYPOINT), [MarkerList.Add(Marker)]); //'Waypoint %d';
  UpdateWayPoints;
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

// Update the Location below the picture
procedure UpdateLocationLabel;
var
  Coord: TCoord;
  StrCoord: string;
begin
  Coord := MUIMapPanel.PixelToPos(MUIMapPanel.LastMouse);
  StrCoord := FloatToStrF(Coord.Lat, ffFixed, 8,6) + ' ; ' + FloatToStrF(Coord.Lon, ffFixed, 8,6) + ' ; ' + IntToStr(CurZoom) + '  ';
  StrCoord := Format('%25s', [StrCoord]);
  MH_Set(CoordsLabel, MUIA_Text_Contents, AsTag(PChar(StrCoord)));
end;

procedure SidePanelOpenEvent;
begin
  ShowSidePanel(not SidePanelOpen);
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

// Update Routes
procedure UpdateRoutes;
var
  i: Integer;
begin
  DoMethod(RoutesListEntry, [MUIM_List_Clear]);
  for i := 0 to RouteList.Count - 1 do
  begin
    RouteList[i].FullName := RouteList[i].Name + ' (' + IntToStr(Length(RouteList[i].Pts)) + ') ' + RouteList[i].Desc;
    if not RouteList[i].Visible then
      RouteList[i].FullName := '(' + RouteList[i].FullName + ')';
    DoMethod(RoutesListEntry, [MUIM_List_InsertSingle, AsTag(PChar(RouteList[i].FullName)), AsTag(MUIV_List_Insert_Bottom)]);
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
    SearchTitleStr :=  GetLocString(MSG_ERROR_ONLINE); // 'Not Online'#0;
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
        SearchTitleStr := Format(GetLocString(MSG_ERROR_NOTHINGFOUND), [SearchTerm]); //'Nothing found for "'+SearchTerm+'"'#0;
        MH_Set(SearchListEntry, MUIA_List_Title, AsTag(@SearchTitleStr[1]));
      end
      else
      begin
        SearchTitleStr :=  Format(GetLocString(MSG_SEARCH_RESULTS), [SRes.Count, SearchTerm]);
        MH_Set(SearchListEntry, MUIA_List_Title, AsTag(@SearchTitleStr[1]));
        if SRes.Count = 1 then
        begin
          MiddlePos.Lat := SR.Lat;
          MiddlePos.Lon := SR.Lon;
          CurZoom:= SR.Zoom;
          MUIMapPanel.RefreshImage;
        end;
      end;
    end else
    begin
      SearchTitleStr :=  GetLocString(MSG_ERROR_NETWORK); //'Network error'
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
  //URL:='http://ip-api.com/json';
  URL:='http://freegeoip.net/json/';
  if ip <> '' then
    Url := URL + ip;
  St := TStringStream.Create('');
  try
    if GetCurlFile(Url, St) then
    begin
      //St.Position := 0;
      //writeln(ST.DataString);
      ST.Position := 0;
      jData := GetJSON(St);
      jObject := TJSONObject(jData);
      //SLat := jObject.Get('lat'); //Copy(Location, 1, P1 - 1);
      //SLon := jObject.Get('lon');//Copy(Location, P1 + 1, Length(Location));
      SLat := jObject.Get('latitude'); //Copy(Location, 1, P1 - 1);
      SLon := jObject.Get('longitude');//Copy(Location, P1 + 1, Length(Location));
      Lat := StrToFloatDef(SLat, Nan);
      Lon := StrToFloatDef(SLon, Nan);
      if not IsNan(Lat) and not IsNan(Lon) then
      begin
        c := jObject.Get('city', '');
        r := jObject.Get('region_name', '');
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
        MUIMapPanel.RefreshImage;
        //TrackPosition := False;
        //LocationPanel.Caption := AnsiToUtf8(c + ' ' + r + ' ' + l);
      end;
    end;
  except
    on E: Exception do
      writeln('Error Getting IP Position: ', E.Message);
  end;
  St.Free;
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
        MUIMapPanel.RefreshImage;
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
      MUIMapPanel.RefreshImage;
      //TrackPosition := False;
      //LocationPanel.Caption := '';
      Exit;
    end;
    SearchEntry(SText);
  end;
  MH_Set(SearchEdit, MUIA_String_Contents, AsTag(''));
  Result := 0;
end;

procedure StartHelp;
begin
  {$ifdef AROS}
    try
      ExecuteProcess('c:run', 'Sys:Utilities/Multiview ' + IncludeTrailingPathdelimiter(AppDir) + 'MuiMapparium.guide');
    except
    end;
  {$else}
    DoMethod(app, [MUIM_Application_ShowHelp, AsTag(Window), 0, 0, 0]);
  {$endif}
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
    MID_ZOOMIN: MUIMapPanel.ZoomIn(False);
    MID_ZOOMOUT: MUIMapPanel.ZoomOut;
    MID_DrawMarker: MUIMapPanel.ShowMarker := Boolean(MH_Get(MenuDrawMarker, MUIA_Menuitem_Checked));
    MID_DrawTracks: MUIMapPanel.ShowTracks := Boolean(MH_Get(MenuDrawTracks, MUIA_Menuitem_Checked));
    MID_DrawRoutes: MUIMapPanel.ShowRoutes := Boolean(MH_Get(MenuDrawRoutes, MUIA_Menuitem_Checked));
    MID_FINDME: SearchIP('');
    MID_PREFS: OpenPrefs();
    MID_Statistics: MH_Set(StatWin, MUIA_Window_Open, AsTag(True));
    MID_UpdateCheck: CheckForUpdate;
    MID_Help: StartHelp;
    MID_About: OpenAboutWindow;
    MID_AboutMUI: DoMethod(App, [MUIM_Application_AboutMUI, AsTag(Window)]);
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
      MUIMapPanel.RefreshImage;
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
      MUIMapPanel.RefreshImage;
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
        MUIMapPanel.RefreshImage;
      end;
    end;
  end;
end;

//###################################
// Double Click to Route
function DblRouteEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active: LongInt;
  Ro: TRoute;
  MinC, MaxC: TCoord;
  DiffLat, DiffLon: ValReal;
  Pt: Classes.TPoint;
  Rec: TRectCoord;
  i: Integer;
begin
  Result := 0;
  Active := MH_Get(RoutesListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < RouteList.Count) then
  begin
    Ro := RouteList[Active];
    if Assigned(Ro) then
    begin
      for i := 0 to High(Ro.Pts) do
      begin
        if i = 0 then
        begin
          MinC := Ro.Pts[0].Position;
          MaxC := Ro.Pts[0].Position;
        end else
        begin
          MinC.Lat := Min(MinC.Lat, Ro.Pts[i].Position.Lat);
          MinC.Lon := Min(MinC.Lon, Ro.Pts[i].Position.Lon);
          MaxC.Lat := Max(MaxC.Lat, Ro.Pts[i].Position.Lat);
          MaxC.Lon := Max(MaxC.Lon, Ro.Pts[i].Position.Lon);
        end;
      end;
      if Length(Ro.Pts) > 0 then
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
        MUIMapPanel.RefreshImage;
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
    MUIMapPanel.RedrawObject;
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
    MUIMapPanel.RedrawObject;
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
    MUIMapPanel.RedrawObject;
  end;
end;

// Add Route
function AddRouteEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
//var
//  Ro: TRoute;
//  Active: Integer;
begin
  Result := 0;
  NewRouteProps;
  {Active := MH_Get(RoutesListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < RouteList.Count) then
  begin
    Ro := RouteList[Active];
    //ShowTrackProps(Tr);
    MUIMapPanel.RedrawObject;
  end;}
end;


// Remove Route Button
function RemRouteEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active: LongInt;
begin
  Result := 0;
  Active := MH_Get(RoutesListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < RouteList.Count) then
  begin
    RouteList.Delete(Active);
    DoMethod(RoutesListEntry, [MUIM_List_Remove, Active]);
    MUIMapPanel.RedrawObject;
  end;
end;

// Edit Route
function EditRouteEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Ro: TRoute;
  Active: Integer;
begin
  Result := 0;
  Active := MH_Get(RoutesListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < RouteList.Count) then
  begin
    Ro := RouteList[Active];
    ShowRouteProps(Ro);
    MUIMapPanel.RedrawObject;
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
    MUIMapPanel.RedrawObject;
  end;
end;

//###################################
// AREXX Hook

function RexxMsgEvent(Msg: string; out ReturnMsg: string): LongInt;
var
  SL: TStringList;
  NLat, NLon: Double;
  NZoom,i : Integer;
  Fail: Boolean;
  WP: TMarker;
  NName: String;
begin
  Result := 20;
  ReturnMsg := GetLocString(MSG_ERROR_REXX_UNKNOWN); // 'unknown command';
  SL := TStringList.Create;
  try
    ExtractStrings([' '], [], PChar(Msg), SL);
    if SL.Count = 0 then
      Exit;
    case UpperCase(SL[0]) of
     'GOTO':
        begin
          if (SL.Count < 3) or (SL.Count > 4) then
          begin
            ReturnMsg := Format(GetLocString(MSG_ERROR_REXX_PARAM), ['GOTO <lat> <lon> [<zoom>]']); //'wrong number of parameter:
            Result := 10;
            Exit;
          end;
          NLat := StrToFloatDef(SL[1], NaN);
          NLon := StrToFloatDef(SL[2], NaN);
          NZoom := -1;
          if SL.Count = 4 then
            NZoom := StrToIntDef(SL[3], -1);
          Fail := False;
          ReturnMsg := '';
          if IsNan(NLat) then
          begin
            Fail := True;
            ReturnMsg := Format(GetLocString(MSG_ERROR_REXX_ILLEGALPARAM), [1]); // ' Illegal 1 Parameter should be a float value'
          end;
          if IsNan(NLat) then
          begin
            Fail := True;
            ReturnMsg := Format(GetLocString(MSG_ERROR_REXX_ILLEGALPARAM), [2]); // ' Illegal 2 Parameter should be a float value'
          end;
          if Fail then
          begin
            Result := 10;
            Exit;
          end;
          MiddlePos.Lat := NLat;
          MiddlePos.Lon := NLon;
          if NZoom >= 0 then
            CurZoom := NZoom;
          UpdateImage := True;
          Result := 0;
          MH_Set(Window, MUIA_Window_Activate, AsTag(True));
        end;
      'ADDWAYPOINT':
        begin
          if (SL.Count < 4) then
          begin
            ReturnMsg := Format(GetLocString(MSG_ERROR_REXX_PARAM), ['WAYPOINT <lat> <lon> "<name>"']); //'wrong number of parameter:
            Result := 10;
            Exit;
          end;
          NLat := StrToFloatDef(SL[1], NaN);
          NLon := StrToFloatDef(SL[2], NaN);
          NName := SL[3];
          for i := 4 to SL.Count - 1 do
            NName := NName + ' ' + SL[i];
          Fail := False;
          ReturnMsg := '';
          if IsNan(NLat) then
          begin
            Fail := True;
            ReturnMsg := Format(GetLocString(MSG_ERROR_REXX_ILLEGALPARAM), [1]); // ' Illegal 1 Parameter should be a float value'
          end;
          if IsNan(NLat) then
          begin
            Fail := True;
            ReturnMsg := Format(GetLocString(MSG_ERROR_REXX_ILLEGALPARAM), [2]); // ' Illegal 2 Parameter should be a float value'
          end;
          if Fail then
          begin
            Result := 10;
            Exit;
          end;
          WP := TMarker.Create;
          WP.Position.lat := NLat;
          WP.Position.Lon := NLon;
          WP.Name := NName;
          MarkerList.Add(WP);
          UpdateWayPoints;
          UpdateImage := True;
          Result := 0;
          MH_Set(Window, MUIA_Window_Activate, AsTag(True));
        end;
      {
      'ADDJPG': // Add Jpg
        begin
          if SL.Count < 2 then
          begin
            ReturnMsg := 'wrong number of parameter: AddJPG "<Filename>"';
            Result := 10;
            Exit;
          end;
          // Parameter: "Path",
          NName := SL[1];
          for i := 2 to SL.Count - 1 do
            NName := NName + ' ' + SL[i];
          Result := ImageWin.AddImage(NName);
          if Result = 0 then
            ImageWin.UpdateImagesAfterAdd;
        end;
      'ADDJPGPOS': // Add Jpg with position
        begin
          if SL.Count < 4 then
          begin
            ReturnMsg := 'wrong number of parameter: AddJPGPos <lat> <lon> "<Filename>"';
            Result := 10;
            Exit;
          end;
          NLat := StrToFloatDef(SL[1], NaN);
          NLon := StrToFloatDef(SL[2], NaN);
          NName := SL[3];
          for i := 4 to SL.Count - 1 do
            NName := NName + ' ' + SL[i];
          Result := ImageWin.AddImage(NName, NLat, NLon);
          if Result = 0 then
            ImageWin.UpdateImagesAfterAdd;
        end;
       'LOCKIMAGELIST':
         begin
           if SL.Count > 1 then
           begin
             ReturnMsg := 'wrong number of parameter: LOCKIMAGELIST';
             Result := 10;
             Exit;
           end;
           ImageWin.LockImgList := True;
           Result := 0;
         end;
       'UNLOCKIMAGELIST':
         begin
           if SL.Count > 1 then
           begin
             ReturnMsg := 'wrong number of parameter: UNLOCKIMAGELIST';
             Result := 10;
             Exit;
           end;
           ImageWin.LockImgList := False;
           ImageWin.UpdateImagesAfterAdd;
           Result := 0;
         end}
      else
        begin
          ReturnMsg := GetLocString(MSG_ERROR_REXX_UNKNOWN) + '''' + SL[0]+ '''';
          Result := 20;
        end;
    end;
  finally
    SL.Free;
  end;

end;


function RexxHookEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  RexxMsg: PRexxMsg;
  Txt: string;
begin
  Result := 20;
  if Assigned(Msg) then
  begin
    RexxMsg := Msg;
    Txt := '';
    Result := RexxMsgEvent(RexxMsg^.rm_Args[0], Txt);
    if Txt <> '' then
    begin
      Txt := Txt + #13#10;
      DosWrite(RexxMsg^.rm_Stdout, PChar(Txt), Length(Txt));
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
  MH_Set(MainBalance, MUIA_Disabled, AsTag(not ShowIt));
  MUIMapPanel.ShowSidePanelBtn := not ShowIt;
  SidePanelOpen := ShowIt;
  //MH_Set(MainBalance, MUIA_ShowMe, AsTag(ShowIt));
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
  MaxImages := Prefs.MaxTiles;
  // let redraw to make the marker changes visible
  RedrawImage := True;
end;

// WayPoint Property edit changed something ;-)
procedure WPChangedEvent;
begin
  UpdateWayPoints;
  MUIMapPanel.RedrawObject;
end;

// Tracks Property edit changed something ;-)
procedure TrackChangedEvent;
begin
  UpdateTracks;
  MUIMapPanel.RedrawObject;
end;

// track wants us to redraw (show active point or so)
procedure TrackRedrawEvent;
begin
  MUIMapPanel.RedrawObject;
end;

procedure GoToPosEvent;
var
  MinC, MaxC: TCoord;
  DiffLat, DiffLon: ValReal;
  Pt: Classes.TPoint;
  Rec: TRectCoord;
  i: Integer;
begin
  if Assigned(CurOrder) then
  begin
    for i := 0 to High(CurOrder.Positions) do
    begin
      if i = 0 then
      begin
        MinC := CurOrder.Positions[i];
        MaxC := CurOrder.Positions[i];
      end else
      begin
        MinC.Lat := Min(MinC.Lat, CurOrder.Positions[i].Lat);
        MinC.Lon := Min(MinC.Lon, CurOrder.Positions[i].Lon);
        MaxC.Lat := Max(MaxC.Lat, CurOrder.Positions[i].Lat);
        MaxC.Lon := Max(MaxC.Lon, CurOrder.Positions[i].Lon);
      end;
    end;
    if Length(CurOrder.Positions) > 0 then
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
      CurZoom := CurZoom;
      MUIMapPanel.RefreshImage;
    end;
  end;
end;



// *********************************************************************
// ####################################
// Main Routine

procedure StartMe;
var
  Sigs: LongInt;
  StrCoord: string;
  MenuHook, SearchAckHook, DblSearchHook,DblWayPointHook: THook;
  RemTrack, EditTrack: PObject_;
  RemTrackHook, DblTrackHook, EditTrackHook: Thook;
  RemRoute, EditRoute, AddRoute: PObject_;
  AddRouteHook, RemRouteHook, DblRouteHook, EditRouteHook: Thook;
  AddWay, RemWay, EditWay: PObject_;
  AddWayHook, RemWayHook, EditWayHook: Thook;
  StartTime: Int64;
  WayMenuHooks: array[0..0] of THook;
  RexxHook: THook;
  ThisAppDiskIcon: Pointer;
  i: Integer;
begin
  //Initialize Frame titles 'Search', 'WayPoints', 'Tracks'
  TabStrings[0] := string(GetLocString(MSG_FRAME_SEARCH));
  TabStrings[1] := string(GetLocString(MSG_FRAME_WAYPOINTS));
  TabStrings[2] := string(GetLocString(MSG_FRAME_TRACKS));
  TabStrings[3] := string(GetLocString(MSG_FRAME_ROUTES));
  TabStrings[4] := string(GetLocString(MSG_FRAME_IMAGES));
  //
  for i := 0 to High(TabStrings) do
    TabTitles[i] := PChar(TabStrings[i]);
  // remove Fotos for now
  TabTitles[High(TabStrings)] := nil;

  SRes := TSearchResults.Create;
  // Prefs
  UpdatePrefs;
  OnUpdatePrefs := @UpdatePrefs;
  //
  OnWPChanged := @WPChangedEvent;
  OnTrackChanged := @TrackChangedEvent;
  OnTrackRedraw := @TrackRedrawEvent;
  OnRouteGoToPos := @GoToPosEvent;
  try
    SearchTitleStr :=  GetLocString(MSG_SEARCH_RESULTS_TITLE); // 'Search Results';
    //
    StrCoord := FloatToStrF(MiddlePos.Lat, ffFixed, 8,6) + ' ; ' + FloatToStrF(MiddlePos.Lon, ffFixed, 8,6) + ' ; ' + IntToStr(CurZoom) + '  ';
    StrCoord := Format('%25s', [StrCoord]);
    //
    // Popupmenu for WayPointList ++++++++++++++++++++++++++++++++++++++
    WayPointMenu := MH_Menustrip([
      Child, AsTag(MH_Menu(GetLocString(MSG_POPUP_WAYPOINT), [                   // 'Waypoint'
        Child, AsTag(MH_MenuItem(WM1, [
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_POPUP_WAYPOINT_TOGGLE)),   // 'Toggle visibility'
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
      Child, AsTag(MH_Register([
        MUIA_Register_Titles, AsTag(@TabTitles),
        //#### Search list
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
        //#### WayPoints
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
              Child, AsTag(MH_Button(AddWay, GetLocString(MSG_BUTTON_WAYPOINT_ADD))),    // 'Add'
              Child, AsTag(MH_Button(RemWay, GetLocString(MSG_BUTTON_WAYPOINT_REMOVE))), // 'Remove'
              Child, AsTag(MH_Button(EditWay, GetLocString(MSG_BUTTON_WAYPOINT_EDIT))),  // 'Edit'
            TAG_DONE])),
          TAG_DONE])),
        //#### Tracks
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
              Child, AsTag(MH_Button(RemTrack, GetLocString(MSG_BUTTON_TRACK_REMOVE))),    // 'Remove'
              Child, AsTag(MH_Button(EditTrack, GetLocString(MSG_BUTTON_WAYPOINT_EDIT))),  //  'Edit'
            TAG_DONE])),
          TAG_DONE])),
        //#### Routes
        Child, AsTag(MH_VGroup([
          Child, AsTag(MH_ListView(RoutesList, [
            MUIA_Weight, 100,
            MUIA_Listview_Input, MUI_TRUE,
            MUIA_Listview_List, AsTag(MH_List(RoutesListEntry, [
              MUIA_Frame, MUIV_Frame_ReadList,
              MUIA_Background, MUII_ReadListBack,
              //MUIA_ContextMenu, AsTag(WayPointMenu),
              MUIA_List_PoolThreshSize, 256,
              TAG_DONE])),
            TAG_DONE])),
          Child, AsTag(MH_HGroup([
              Child, AsTag(MH_Button(AddRoute, GetLocString(MSG_BUTTON_WAYPOINT_ADD))),    // 'Add'
              Child, AsTag(MH_Button(RemRoute, GetLocString(MSG_BUTTON_WAYPOINT_REMOVE))),    // 'Remove'
              Child, AsTag(MH_Button(EditRoute, GetLocString(MSG_BUTTON_WAYPOINT_EDIT))),  //  'Edit'
            TAG_DONE])),
          TAG_DONE])),
        //#### Images
        //Child, AsTag(MH_VGroup([
        //  TAG_DONE])),
        TAG_DONE])),
      TAG_DONE]);
    if not Assigned(SidePanel) then
      writeln(GetLocString(MSG_ERROR_SIDEPANEL)); //  'Failed to create SidePanel.'
    //
    // Main Menu +++++++++++++++++++++++++++++++++++++++++++++++++++++++
    MainMenu := MH_Menustrip([
      // Project Menu -----------------------------------
      Child, AsTag(MH_Menu(GetLocString(MSG_MENU_PROJECT),[             // 'Project'
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_MAIN_LOAD)), //  'Load...'
          MUIA_Menuitem_Shortcut, AsTag(GetLocString(MSG_MENU_MAIN_LOAD_KEY)), // 'L'
          MUIA_UserData, MID_Load,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_MAIN_SAVE)), //  'Save...'
          MUIA_Menuitem_Shortcut, AsTag(GetLocString(MSG_MENU_MAIN_SAVE_KEY)), // 'S'
          MUIA_UserData, MID_Save,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_MAIN_QUIT)), //  'Quit'
          MUIA_Menuitem_Shortcut, AsTag(GetLocString(MSG_MENU_MAIN_QUIT_KEY)), // 'Q'
          MUIA_UserData, MID_Quit,
          TAG_DONE])),
        TAG_DONE])),
      // Map Menu ----------------------------------
      Child, AsTag(MH_Menu(GetLocString(MSG_MENU_MAP),[                 // 'Map'
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_MAP_FINDME)),//  'Find me'
          MUIA_UserData, MID_FINDME,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(-1),
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_MAP_ZOOMIN)),//  'Zoom in'
          MUIA_UserData, MID_ZOOMIN,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_MAP_ZOOMOUT)),  //  'Zoom out'
          MUIA_UserData, MID_ZOOMOUT,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(-1),
          TAG_DONE])),
        Child, AsTag(MH_MenuItem(MenuDrawMarker, [
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_MAP_DRAWMARKER)),  //  'Draw Marker'
          MUIA_UserData, MID_DrawMarker,
          MUIA_Menuitem_Checkit, AsTag(True),
          MUIA_Menuitem_Toggle, AsTag(True),
          MUIA_Menuitem_Checked, AsTag(True),
          TAG_DONE])),
        Child, AsTag(MH_MenuItem(MenuDrawTracks, [
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_MAP_DRAWTRACKS)),  //  'Draw Tracks'
          MUIA_UserData, MID_DrawTracks,
          MUIA_Menuitem_Checkit, AsTag(True),
          MUIA_Menuitem_Toggle, AsTag(True),
          MUIA_Menuitem_Checked, AsTag(True),
          TAG_DONE])),
        Child, AsTag(MH_MenuItem(MenuDrawRoutes, [
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_MAP_DRAWROUTES)),  //  'Draw Routes'
          MUIA_UserData, MID_DrawRoutes,
          MUIA_Menuitem_Checkit, AsTag(True),
          MUIA_Menuitem_Toggle, AsTag(True),
          MUIA_Menuitem_Checked, AsTag(True),
          TAG_DONE])),
        TAG_DONE])),
      // Window Menu ------------------------------------
      Child, AsTag(MH_Menu(GetLocString(MSG_MENU_WINDOW),[              // 'Window'
        Child, AsTag(MH_MenuItem(MenuSidePanel, [
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_WINDOW_SIDEPANEL)), // 'Side Panel'
          MUIA_Menuitem_Checkit, AsTag(True),
          MUIA_Menuitem_Toggle, AsTag(True),
          MUIA_UserData, MID_SIDEPANEL,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_WINDOW_PREFS)),     // 'Prefs ...'
          MUIA_UserData, MID_Prefs,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_WINDOW_STATISTICS)), // 'Statistics'
          MUIA_UserData, MID_Statistics,
          TAG_DONE])),
        TAG_DONE])),
      // About Menu -----------------
      Child, AsTag(MH_Menu(GetLocString(MSG_MENU_ABOUT),[
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar(GetLocString(MSG_MENU_ABOUT_ABOUT))),     // 'About ...'
          MUIA_UserData, MID_About,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar(GetLocString(MSG_MENU_ABOUT_ABOUTMUI))),     // 'About MUI ...'
          MUIA_UserData, MID_AboutMUI,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar(GetLocString(MSG_MENU_ABOUT_HELP))),     // 'Help ...'
          MUIA_UserData, MID_Help,
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([
          MUIA_Menuitem_Title, AsTag(PChar(GetLocString(MSG_MENU_ABOUT_UPDATE))),     // 'Check for Update ...'
          MUIA_UserData, MID_UpdateCheck,
          TAG_DONE])),
        TAG_DONE])),
      TAG_DONE]);
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    MUIMapPanel := TMapPanel.Create([MUIA_Weight, 200, TAG_DONE]);
    MUIMapPanel.OnUpdateLocationLabel := @UpdateLocationLabel;
    MUIMapPanel.OnSidePanelOpen := @SidePanelOpenEvent;
    // writeln(GetLocString(MSG_ERROR_CUSTOM_CLASS)); // 'Could not create custom class.'

    // Application +++++++++++++++++++++++++++++++++++++++++++++++++++++
    MH_SetHook(RexxHook, @RexxHookEvent, nil);
    //
    ThisAppDiskIcon := GetDiskObject(PChar(ParamStr(0)));
    //
    App := MH_Application([
      MUIA_Application_Title,       AsTag('MUIMapparium'),
      MUIA_Application_Version,     AsTag(PChar(VERSIONSTRING)),
      MUIA_Application_Copyright,   AsTag('(c)2017, Marcus "ALB" Sackrow'),
      MUIA_Application_Author,      AsTag('Marcus "ALB" Sackrow'),
      MUIA_Application_Description, AsTag('Open Street Map viewer. (MUI)'),
      MUIA_Application_Base,        AsTag('MAPPARIUM'),
      MUIA_Application_DiskObject,  AsTag(ThisAppDiskIcon),
      MUIA_Application_HelpFile,    AsTag(PChar(IncludeTrailingPathdelimiter(AppDir) + 'MUIMapparium.guide')),
      MUIA_Application_RexxHook,    AsTag(@RexxHook),

      SubWindow, AsTag(MH_Window(Window, [
        MUIA_Window_Title,     AsTag(PChar(WindowTitleTemplate)),
        MUIA_Window_ID,        AsTag(MAKE_ID('M','A','P','P')),
        MUIA_Window_MenuStrip, AsTag(MainMenu),
        MUIA_HelpNode,         AsTag('MainWin'),
        WindowContents, AsTag(MH_VGroup([


          Child, AsTag(MH_String(SearchEdit, [
            MUIA_Frame, MUIV_Frame_String,
            TAG_DONE])),
          Child, AsTag(MH_HGroup([
            Child, AsTag(SidePanel),
            Child, AsTag(MH_Balance(MainBalance, [MUIA_CycleChain, 1, MUIA_Disabled, AsTag(True), TAG_END])),
            Child, AsTag(MUIMapPanel.MUIObject),
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
      SubWindow, AsTag(RoutePropsWin),
      SubWindow, AsTag(AboutWin),
      TAG_DONE]);
    if not Assigned(app) then
    begin
      writeln(GetLocString(MSG_ERROR_APPLICATION));           // 'Failed to create Application'
      Exit;
    end;
    // version app set
    WrapApp := App;
    WrapWin := Window;
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
    // Track Actions
    ConnectHookFunction(MUIA_Pressed, AsTag(False), RemTrack, nil, @RemTrackHook, @RemTrackEvent);
    ConnectHookFunction(MUIA_Pressed, AsTag(False), EditTrack, nil, @EditTrackHook, @EditTrackEvent);
    ConnectHookFunction(MUIA_Listview_DoubleClick, MUIV_EveryTime, TracksList, nil, @DblTrackHook, @DblTrackEvent);
    // Route Actions
    ConnectHookFunction(MUIA_Pressed, AsTag(False), AddRoute, nil, @AddRouteHook, @AddRouteEvent);
    ConnectHookFunction(MUIA_Pressed, AsTag(False), RemRoute, nil, @RemRouteHook, @RemRouteEvent);
    ConnectHookFunction(MUIA_Pressed, AsTag(False), EditRoute, nil, @EditRouteHook, @EditRouteEvent);
    ConnectHookFunction(MUIA_Listview_DoubleClick, MUIV_EveryTime, RoutesList, nil, @DblRouteHook, @DblRouteEvent);



    DoMethod(PrefsWin, [MUIM_Notify, MUIA_Window_CloseRequest, MUI_TRUE,
      AsTag(PrefsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
    DoMethod(PrefsWin, [MUIM_Notify, MUIA_Window_Open, MUI_FALSE,
      AsTag(Window), 3, MUIM_SET, MUIA_Window_Sleep, AsTag(False)]);
    //

    // Update waypoints before open the window first time
    UpdateWayPoints;
    UpdateTracks;
    UpdateRoutes;
    //
    // open the window
    MH_Set(Window, MUIA_Window_Open, AsTag(True));
    // open additionally the stat window if needed
    if Prefs.StatWinOpen then
      MH_Set(StatWin, MUIA_Window_Open, AsTag(True));
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
      MUIMapPanel.RefreshImage;
      // main cycle
      StartTime := GetMUITime;
      while Integer(DoMethod(app, [MUIM_Application_NewInput, AsTag(@sigs)])) <> MUIV_Application_ReturnID_Quit do
      begin
        if UpdateImage and not MUIMapPanel.LeftDown then
        begin
          MUIMapPanel.RefreshImage;
          UpdateImage := False;
          UpdateLocationLabel;
          //writeln(7);
        end;
        if RedrawImage then
        begin
          MUIMapPanel.RedrawObject;
          ReDrawImage := False;
          //writeln(8);
        end;
        if GetMUITime - StartTime > 20 then
        begin
          if LongBool(MH_Get(PlayPanel.MUIObject, MUIA_ShowMe)) then
            PlayPanel.RedrawObject;
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
    Prefs.StatWinOpen := LongBool(MH_Get(StatWin, MUIA_Window_Open));
    if Prefs.StatWinOpen then
      DoMethod(StatWin, [MUIM_Window_Snapshot, 1]);
    // end of main loop, close window if still open
    MH_Set(Window, MUIA_Window_Open, AsTag(False));

  finally
    if Assigned(WayPointMenu) then
      MUI_DisposeObject(WayPointMenu);
    if Assigned(App) then
      MUI_DisposeObject(app);
    if Assigned(ThisAppDiskIcon) then
      FreeDiskObject(ThisAppDiskIcon);
    SRes.Free;
    MUIMapPanel.Free;
  end;
end;

begin
  StartMe;
end.
