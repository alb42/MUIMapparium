program MUIMapparium;
{$mode objfpc}{$H+}
{$NOTES OFF}
uses
  athreads,
  MUIMappariumlocale, MUIPaintBoxUnit,
  imagesunit, positionunit, osmhelper, networkingunit, prefsunit,
  Exec, Utility, intuition, agraphics, Layers, AmigaDos, icon,
  cybergraphics, mui, muihelper, MUIWrap, PrefsWinUnit,
  Statisticsunit, waypointunit, WPPropsUnit, TrackPropsUnit, RoutePropsUnit,
  DOM, XMLRead, XMLWrite, xmlutils, jsonparser, fpjson,
  SysUtils, StrUtils, Types, Classes, Math, versionunit,
  MapPanelUnit, UpdateUnit, aboutwinunit, ASL, workbench,
  PhotoShowUnit, fgl, serialthread;

const
  NM_BarLabel: LongInt = -1;
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
  MID_DrawPhotos  = 13;
  MID_UpdateCheck = 14;
  MID_Help        = 15;
  MID_About       = 16;
  MID_AboutMUI    = 17;
  MID_ExportPNG   = 18;
  MID_Search      = 19;

var
  TabStrings: array[0..5] of string = ('Search', 'WayPoints', 'Tracks', 'Routes', 'Photos', 'Photos');
  TabTitles: array[0..6] of PChar = ('Search'#0, 'WayPoints'#0, 'Tracks'#0, 'Routes'#0, 'Photos'#0, 'Photos'#0, nil);

var
  Running: Boolean = False;

  CoordsLabel,
  // Search
  SearchEdit, SearchList, SearchListEntry,
  // Waypoints
  WayPointList, WaypointListEntry, MapFeatureMenu,
  // Tracks
  TracksList, TracksListEntry,
  //Routes
  RoutesList, RoutesListEntry,
  //Photos
  PhotosList, PhotosListEntry,
  // Basic
  App, Window: PObject_;
  // MainWindow
  ListTabs,
  SidePanel, MainBalance, MainMenu: PObject_;
  // Menu
  MenuSidePanel, MenuDrawMarker, MenuDrawTracks, MenuDrawRoutes, MenuDrawPhotos: PObject_;
  //
  SRes: TSearchResults;
  SearchTitleStr: string;

  WM1, WM2, WM3, WM4: PObject_;

  SidePanelOpen: Boolean = False;
  LockImgList: Boolean = False;

  SerThread: TSerThread = nil;

  WinTitleText: string;

procedure UpdateLocationLabel; forward;
procedure SidePanelOpenEvent; forward;
procedure ShowSidePanel(ShowIt: Boolean); forward;
function EditWayEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt; forward;
function EditTrackEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt; forward;
function EditRouteEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt; forward;
procedure CenterToRoute(Ro: TRoute); forward;
procedure CenterToTrack(Tr: TTrack); forward;

procedure OpenPrefs; forward;
procedure UpdateWayPoints; forward;

procedure CloseGPS;
begin
  {if Assigned(SerThread) then
  begin
    SerThread.Terminate;
    SerThread.WaitFor;
    SerThread.Free;
    SerThread := nil;
  end;}
end;

procedure InitGPS;
var
  ReDo: Boolean;
begin
  {if not Prefs.UseGPS then
  begin
    CloseGPS;
    Exit;
  end;
  // should start gps, check settings
  ReDo := True;
  if Assigned(SerThread) then
  begin
    if (SerThread.Device = Prefs.GPSDevice) and
       (SerThread.UnitNum = Prefs.GPSUnit) and
       (SerThread.Baud = Prefs.GPSBaud) then
      ReDo := False;
  end;
  if ReDo then
  begin
    CloseGPS;
    SerThread := TSerThread.Create(Prefs.GPSDevice, Prefs.GPSUnit, Prefs.GPSBaud);
  end;}
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
  Marker.Name := Format(GetLocString(MSG_DEFAULT_WAYPOINT), [MarkerList.Add(Marker)]); //'Waypoint %d';
  UpdateWayPoints;
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
  MH_Set(WaypointListEntry, MUIA_List_Quiet, LTrue);
  DoMethod(WaypointListEntry, [MUIM_List_Clear]);
  for i := 0 to MarkerList.Count - 1 do
  begin
    MarkerList[i].UpdateFullName;
    DoMethod(WaypointListEntry, [MUIM_List_InsertSingle, AsTag(PChar(MarkerList[i].FullName)), AsTag(MUIV_List_Insert_Bottom)]);
  end;
  RedrawImage := True;
  MH_Set(WaypointListEntry, MUIA_List_Quiet, LFalse);
end;

// Update Tracks;
procedure UpdateTracks;
var
  i: Integer;
begin
  MH_Set(TracksListEntry, MUIA_List_Quiet, LTrue);
  DoMethod(TracksListEntry, [MUIM_List_Clear]);
  for i := 0 to TrackList.Count - 1 do
  begin
    TrackList[i].UpdateFullName;
    DoMethod(TracksListEntry, [MUIM_List_InsertSingle, AsTag(PChar(TrackList[i].FullName)), AsTag(MUIV_List_Insert_Bottom)]);
  end;
  RedrawImage := True;
  MH_Set(TracksListEntry, MUIA_List_Quiet, LFalse);
end;

// Update Routes
procedure UpdateRoutes;
var
  i: Integer;
begin
  MH_Set(RoutesListEntry, MUIA_List_Quiet, LTrue);
  DoMethod(RoutesListEntry, [MUIM_List_Clear]);
  for i := 0 to RouteList.Count - 1 do
  begin
    RouteList[i].UpdateFullName;
    DoMethod(RoutesListEntry, [MUIM_List_InsertSingle, AsTag(PChar(RouteList[i].FullName)), AsTag(MUIV_List_Insert_Bottom)]);
  end;
  RedrawImage := True;
  MH_Set(RoutesListEntry, MUIA_List_Quiet, LFalse);
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
  try
  Doc := nil;
  Mem := nil;
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
  Url := SEARCHURL + EncStr + '&format=xml&limit=20&accept-language=' + Prefs.SearchLang;
  //Url := SEARCHURL + EncStr + '?format=xml&accept-language=' + Prefs.SearchLang;
  //sysdebugln(URL);
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
        if (Pos(',', SearchTerm) = 0) and (Pos(' ', SearchTerm) > 0) then
        begin
          SearchTerm := StringReplace(SearchTerm, ' ', ', ', [rfReplaceAll]);
          SearchEntry(SearchTerm);
          Exit;
        end;
        SearchTitleStr := utf8ToAnsi(Format(GetLocString(MSG_ERROR_NOTHINGFOUND), [SearchTerm])); //'Nothing found for "'+SearchTerm+'"'#0;
        MH_Set(SearchListEntry, MUIA_List_Title, AsTag(@SearchTitleStr[1]));
      end
      else
      begin
        SearchTitleStr :=  Utf8ToAnsi(Format(GetLocString(MSG_SEARCH_RESULTS), [SRes.Count, SearchTerm]));
        MH_Set(SearchListEntry, MUIA_List_Title, AsTag(@SearchTitleStr[1]));
        if SRes.Count > 0 then
        begin
          SR := SRes[0];
          MiddlePos.Lat := SR.Lat;
          MiddlePos.Lon := SR.Lon;
          CurZoom:= SR.Zoom;
          MUIMapPanel.RefreshImage;
          MH_Set(SearchListEntry, MUIA_List_Active, 0);
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
  except
    on E: Exception do
    begin
      SearchTitleStr :=  GetLocString(MSG_ERROR_NETWORK) + ': ' + E.Message; //'Network error'
      MH_Set(SearchListEntry, MUIA_List_Title, AsTag(@SearchTitleStr[1]));
    end;
  end;
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
  //URL:='http://freegeoip.net/json/';
  URL:='http://osm.alb42.de/mmipsearch.php?ip=';
  if ip <> '' then
    Url := URL + ip;
  St := TStringStream.Create('');
  try
    if GetFile(Url, St) then
    begin
      //St.Position := 0;
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
      ShowMessage('Error', GetLocString(MSG_GENERAL_OK), 'Error Getting IP Position: ' + e.Message);
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
  // which by some user, do not delete
  //MH_Set(SearchEdit, MUIA_String_Contents, AsTag(''));
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

procedure ExportAsPng;
var
  fr: PFileRequester;
  OldDrawer: string;
  OldFileName: string;
begin
{$R-}
  OldFilename := 'Map.png';
  OldDrawer := Prefs.LoadPath;
  fr := AllocAslRequestTags(ASL_FileRequest, [
    NativeUInt(ASLFR_TitleText),      NativeUInt(PChar('Choose name  to save the Map as PNG')),
    NativeUInt(ASLFR_InitialFile),    NativeUInt(PChar(OldFileName)),
    NativeUInt(ASLFR_InitialDrawer),  NativeUInt(PChar(OldDrawer)),
    NativeUInt(ASLFR_InitialPattern), NativeUInt(PChar('#?.png')),
    NativeUInt(ASLFR_DoSaveMode),     LTrue,
    NativeUInt(ASLFR_DoPatterns),     LTrue,
    TAG_END]);
  if Assigned(fr) then
  begin
    //
    if AslRequestTags(fr, [TAG_END]) then
    begin
      OldFilename := IncludeTrailingPathDelimiter(string(fr^.fr_drawer)) + string(fr^.fr_file);
      OldFilename := ChangeFileExt(OldFilename, '.png');
      MUIMapPanel.SaveToFile(OldFilename);
    end;
    FreeAslRequest(fr);
  end;
end;

//###################################
// Menu Event
function MenuEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  case MH_Get(App, MUIA_Application_MenuAction) of
    MID_Load: if LoadWayFile then
      begin
        UpdateWayPoints;
        UpdateTracks;
      end;
    MID_Save: SaveWayFile;
    MID_ExportPNG: ExportAsPng;
    MID_Quit: Running := False;
    MID_SidePanel: ShowSidePanel(Boolean(MH_Get(MenuSidePanel, MUIA_Menuitem_Checked)));
    MID_ZOOMIN: MUIMapPanel.ZoomIn(False);
    MID_ZOOMOUT: MUIMapPanel.ZoomOut;
    MID_DrawMarker: MUIMapPanel.ShowMarker := Boolean(MH_Get(MenuDrawMarker, MUIA_Menuitem_Checked));
    MID_DrawTracks: MUIMapPanel.ShowTracks := Boolean(MH_Get(MenuDrawTracks, MUIA_Menuitem_Checked));
    MID_DrawRoutes: MUIMapPanel.ShowRoutes := Boolean(MH_Get(MenuDrawRoutes, MUIA_Menuitem_Checked));
    MID_DrawPhotos: MUIMapPanel.ShowPhotos := Boolean(MH_Get(MenuDrawPhotos, MUIA_Menuitem_Checked));
    MID_FINDME: SearchIP('');
    MID_PREFS: OpenPrefs();
    MID_Statistics: MH_Set(StatWin, MUIA_Window_Open, AsTag(True));
    MID_UpdateCheck: CheckForUpdate;
    MID_Help: StartHelp;
    MID_About: OpenAboutWindow;
    MID_AboutMUI: DoMethod(App, [MUIM_Application_AboutMUI, AsTag(Window)]);
    MID_SEARCH: MH_Set(Window, MUIA_Window_ActiveObject, AsTag(SearchEdit));
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
// Double Click to Photo
function DblPhotoEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active, i: Integer;
  MinPos,MaxPos: TCoord;
  s: TPhoto;
  DiffLat, DiffLon: Double;
  Pt: TPoint;
  Rec: TRectCoord;
begin
  Result := 0;
  Active := MH_Get(PhotosListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < PhotoList.VisList.Count) then
  begin
    CurPhoto := PhotoList.VisList[Active].VisEntry;
    if Assigned(CurPhoto) then
    begin
      MiddlePos.Lat := CurPhoto.Position.Lat;
      MiddlePos.Lon := CurPhoto.Position.Lon;
      MUIMapPanel.RefreshImage;
      PhotoPanel.ShowImage(CurPhoto.Path);
    end
    else
    begin
      MinPos.Lat := NaN;
      for i := Active + 1 to PhotoList.VisList.Count - 1 do
      begin
        s := PhotoList.VisList[i].VisEntry;
        if not Assigned(s) then
          Break;
        if i = (Active + 1) then
        begin
          MinPos := s.Position;
          MaxPos := s.Position;
        end
        else
        begin
          MaxPos.Lat := Max(MaxPos.Lat, s.Position.Lat);
          MaxPos.Lon := Max(MaxPos.Lon, s.Position.Lon);
          //
          MinPos.Lat := Min(MinPos.Lat, s.Position.Lat);
          MinPos.Lon := Min(MinPos.Lon, s.Position.Lon);
        end;
      end;
      //
      if not IsNan(MinPos.Lat) then
      begin
        DiffLat := Abs(MaxPos.Lat - MinPos.Lat);
        DiffLon := Abs(MaxPos.Lon - MinPos.Lon);
        MiddlePos.Lat:= (MaxPos.Lat + MinPos.Lat) / 2;
        MiddlePos.Lon:= (MaxPos.Lon + MinPos.Lon) / 2;

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

function PhotoActiveEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active: Integer;
begin
  Result := 0;
  Active := MH_Get(PhotosListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < PhotoList.VisList.Count) then
  begin
    CurPhoto := PhotoList.VisList[Active].VisEntry;
  end
  else
    CurPhoto := nil;
  MUIMapPanel.RefreshImage;
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
      case Prefs.DClickMode of
        dmCenter: begin
          MiddlePos.Lat := Ma.Position.Lat;
          MiddlePos.Lon := Ma.Position.Lon;
          MUIMapPanel.RefreshImage;
        end;
        dmProperty: begin
          MiddlePos.Lat := Ma.Position.Lat;
          MiddlePos.Lon := Ma.Position.Lon;
          MUIMapPanel.RefreshImage;
          EditWayEvent(nil, nil, nil);
        end;
        dmVisible: begin
          Ma.Visible := not Ma.Visible;
          Ma.UpdateFullName;
          DoMethod(WaypointListEntry, [MUIM_List_Remove, Active]);
          DoMethod(WaypointListEntry, [MUIM_List_InsertSingle, AsTag(PChar(Ma.FullName)), Active]);
          MUIMapPanel.RefreshImage;
        end;
      end;
    end;
  end;
end;

//###################################
// Double Click to Track
function DblTrackEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active: LongInt;
  Tr: TTrack;
begin
  Result := 0;
  Active := MH_Get(TracksListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < TrackList.Count) then
  begin
    Tr := TrackList[Active];
    if Assigned(Tr) then
    begin
      case Prefs.DClickMode of
        dmCenter: CenterToTrack(Tr);
        dmProperty: EditTrackEvent(nil, nil, nil);
        dmVisible: begin
          Tr.Visible := not Tr.Visible;
          Tr.UpdateFullName;
          DoMethod(TracksListEntry, [MUIM_List_Remove, Active]);
          DoMethod(TracksListEntry, [MUIM_List_InsertSingle, AsTag(PChar(Tr.FullName)), Active]);
          MUIMapPanel.RefreshImage;
        end;
      end;
    end;
  end;
end;

procedure CenterToTrack(Tr: TTrack);
var
  MinC, MaxC: TCoord;
  DiffLat, DiffLon: Double;
  Pt: TPoint;
  Rec: TRectCoord;
  i: Integer;
begin
  if not Assigned(Tr) then
    Exit;
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

procedure CenterToRoute(Ro: TRoute);
var
  MinC, MaxC: TCoord;
  DiffLat, DiffLon: Double;
  Pt: TPoint;
  Rec: TRectCoord;
  i: Integer;
begin
  if not Assigned(Ro) then
    Exit;
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

//###################################
// Double Click to Route
function DblRouteEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active: LongInt;
  Ro: TRoute;
begin
  Result := 0;
  Active := MH_Get(RoutesListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active < RouteList.Count) then
  begin
    Ro := RouteList[Active];
    if Assigned(Ro) then
    begin
      case Prefs.DClickMode of
        dmCenter: CenterToRoute(Ro);
        dmProperty: EditRouteEvent(nil, nil, nil);
        dmVisible: begin
          Ro.Visible := not Ro.Visible;
          Ro.UpdateFullName;
          DoMethod(RoutesListEntry, [MUIM_List_Remove, Active]);
          DoMethod(RoutesListEntry, [MUIM_List_InsertSingle, AsTag(PChar(Ro.FullName)), Active]);
          MUIMapPanel.RefreshImage;
        end;
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
    CenterToTrack(Tr);
    ShowTrackProps(Tr);
    MUIMapPanel.RedrawObject;
  end;
end;

// Add Route
function AddRouteEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  NewRouteProps;
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
    CenterToRoute(Ro);
    ShowRouteProps(Ro);
    MUIMapPanel.RedrawObject;
  end;
end;


function GetFilename(FDir, FName: string): string;
begin
  FDir := Trim(FDir);
  if Length(FDir) = 0 then
    Result := FName
  else
  begin
    if (FDir[Length(FDir)] = DIRECTORYSEPARATOR) or (FDir[Length(FDir)] = ':') then
      Result := FDir + FName
    else
      Result := FDir + DIRECTORYSEPARATOR + FName;
  end;
end;

procedure UpdatePhotosList;
var
  i: Integer;
begin
  MH_Set(PhotosListEntry, MUIA_List_Quiet, LTrue);
  DoMethod(PhotosListEntry, [MUIM_List_Clear]);
  PhotoList.UpdateVisList;
  for i := 0 to PhotoList.VisList.Count - 1 do
  begin
    DoMethod(PhotosListEntry, [MUIM_List_InsertSingle, AsTag(PChar(PhotoList.VisList[i].Caption)), AsTag(MUIV_List_Insert_Bottom)]);
  end;
  RedrawImage := True;
  MUIMapPanel.RedrawObject;
  MH_Set(PhotosListEntry, MUIA_List_Quiet, LFalse);
end;

// Add Photos
function AddPhotoEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  OldDrawer: string;
  fr: PFileRequester;
  i, Added, NotAdded: Integer;
begin
  Result := 0;
  OldDrawer := Prefs.LoadPath;
  fr := AllocAslRequestTags(ASL_FileRequest, [
    NativeUInt(ASLFR_TitleText),      NativeUInt(PChar('Choose Photos to add')),
    NativeUInt(ASLFR_InitialDrawer),  NativeUInt(PChar(OldDrawer)),
    NativeUInt(ASLFR_InitialPattern), NativeUInt(PChar('(#?.jpg|#?.jpeg)')),
    NativeUInt(ASLFR_DoPatterns),     LTrue,
    NativeUInt(ASLFR_DoMultiSelect),  LTrue,
    TAG_END]);
  if Assigned(fr) then
  begin
    //
    if AslRequestTags(fr, [TAG_END]) then
    begin
      Added := 0;
      NotAdded := 0;
      for i := 1 to  fr^.fr_NumArgs do
      begin
        if PhotoList.AddPhoto(GetFilename(string(fr^.fr_Drawer), string(PWBArgList(fr^.fr_ArgList)^[i].wa_Name))) = 0 then
          Inc(Added)
        else
          Inc(NotAdded);
      end;
      //OldFilename := IncludeTrailingPathDelimiter(string(fr^.fr_drawer)) + string(fr^.fr_file);
      UpdatePhotosList;
      ShowMessage('Message', GetLocString(MSG_GENERAL_OK), IntToStr(Added) + ' Photos added'#10 + IntToStr(NotAdded) +  ' Photos not added (no GPS informations)');
    end;
    FreeAslRequest(fr);
  end;
end;

function RemPhotoEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
end;

//###################################
// WayPoint Menu entries
function WPToggleEvent(Hook: PHook; Obj: PObject_; AMsg: Pointer): NativeInt;
var
  Active: Integer;
  Ma: TMapFeature;
begin
  Result := 0;
  case MH_Get(ListTabs, MUIA_Group_ActivePage) of
    1: // waypoint
    begin
      Active := MH_Get(WayPointListEntry, MUIA_List_Active);
      if (Active >= 0) and (Active < MarkerList.Count) then
      begin
        Ma := MarkerList[Active];
        Ma.Visible := not Ma.Visible;
        Ma.UpdateFullName;
        DoMethod(WaypointListEntry, [MUIM_List_Remove, Active]);
        DoMethod(WaypointListEntry, [MUIM_List_InsertSingle, AsTag(PChar(Ma.FullName)), Active]);
        MUIMapPanel.RedrawObject;
      end;
    end;
    2: // Track
    begin
      Active := MH_Get(TracksListEntry, MUIA_List_Active);
      if (Active >= 0) and (Active < TrackList.Count) then
      begin
        Ma := TrackList[Active];
        Ma.Visible := not Ma.Visible;
        Ma.UpdateFullName;
        DoMethod(TracksListEntry, [MUIM_List_Remove, Active]);
        DoMethod(TracksListEntry, [MUIM_List_InsertSingle, AsTag(PChar(Ma.FullName)), Active]);
        MUIMapPanel.RedrawObject;
      end;
    end;
    3: // Route
    begin
      Active := MH_Get(RoutesListEntry, MUIA_List_Active);
      if (Active >= 0) and (Active < RouteList.Count) then
      begin
        Ma := RouteList[Active];
        Ma.Visible := not Ma.Visible;
        Ma.UpdateFullName;
        DoMethod(RoutesListEntry, [MUIM_List_Remove, Active]);
        DoMethod(RoutesListEntry, [MUIM_List_InsertSingle, AsTag(PChar(Ma.FullName)), Active]);
        MUIMapPanel.RedrawObject;
      end;
    end;
  end;
end;

function WPGoToEvent(Hook: PHook; Obj: PObject_; AMsg: Pointer): NativeInt;
var
  Active: Integer;
  Ma: TMarker;
begin
  Result := 0;
  case MH_Get(ListTabs, MUIA_Group_ActivePage) of
    1: // WayPoints
    begin
      Active := MH_Get(WayPointListEntry, MUIA_List_Active);
      if (Active >= 0) and (Active < MarkerList.Count) then
      begin
        Ma := MarkerList[Active];
        MiddlePos.Lat := Ma.Position.Lat;
        MiddlePos.Lon := Ma.Position.Lon;
        MUIMapPanel.RefreshImage;
      end;
    end;
    2: // Track
    begin
      Active := MH_Get(TracksListEntry, MUIA_List_Active);
      if (Active >= 0) and (Active < TrackList.Count) then
        CenterToTrack(TrackList[Active]);
    end;
    3: // Route
    begin
      Active := MH_Get(RoutesListEntry, MUIA_List_Active);
      if (Active >= 0) and (Active < RouteList.Count) then
        CenterToRoute(RouteList[Active]);
    end;
  end;
  MUIMapPanel.RedrawObject;
end;

function WPShowAllEvent(Hook: PHook; Obj: PObject_; AMsg: Pointer): NativeInt;
var
  i: Integer;
begin
  Result := 0;
  case MH_Get(ListTabs, MUIA_Group_ActivePage) of
    1: // Waypoint
    begin
      for i := 0 to MarkerList.Count - 1 do
        MarkerList[i].Visible := True;
      UpdateWayPoints;
    end;
    2: // Track
    begin
      for i := 0 to TrackList.Count - 1 do
        TrackList[i].Visible := True;
      UpdateTracks;
    end;
    3: // Route
    begin
      for i := 0 to RouteList.Count - 1 do
        RouteList[i].Visible := True;
      UpdateRoutes;
    end;
  end;
  MUIMapPanel.RedrawObject;
end;

function WPHideAllEvent(Hook: PHook; Obj: PObject_; AMsg: Pointer): NativeInt;
var
  i: Integer;
begin
  Result := 0;
  case MH_Get(ListTabs, MUIA_Group_ActivePage) of
    1: // Waypoint
    begin
      for i := 0 to MarkerList.Count - 1 do
        MarkerList[i].Visible := False;
      UpdateWayPoints;
    end;
    2: // Track
    begin
      for i := 0 to TrackList.Count - 1 do
        TrackList[i].Visible := False;
      UpdateTracks;
    end;
    3: // Route
    begin
      for i := 0 to RouteList.Count - 1 do
        RouteList[i].Visible := False;
      UpdateRoutes;
    end;
  end;
  MUIMapPanel.RedrawObject;
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
          Result := PhotoList.AddPhoto(NName);
          if (Result = 0) and not LockImgList then
            UpdatePhotosList;
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
          Result := PhotoList.AddPhoto(NName, NLat, NLon);
          if (Result = 0) and not LockImgList then
            UpdatePhotosList;
        end;
       'LOCKIMAGELIST',
       'LOCKPHOTOLIST':
         begin
           if SL.Count > 1 then
           begin
             ReturnMsg := 'wrong number of parameter: ' + UpperCase(SL[0]);
             Result := 10;
             Exit;
           end;
           LockImgList := True;
           Result := 0;
         end;
       'UNLOCKIMAGELIST',
       'UNLOCKPHOTOLIST':
         begin
           if SL.Count > 1 then
           begin
             ReturnMsg := 'wrong number of parameter: ' + UpperCase(SL[0]);
             Result := 10;
             Exit;
           end;
           LockImgList := False;
           UpdatePhotosList;
           Result := 0;
         end;
       'QUIT':
         begin
           Running := False;
           Result := 0;
         end;
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
  if Assigned(MUIMapPanel) then
  begin
    MUIMapPanel.MiddleMarker := Prefs.MiddleMarker;
    MUIMapPanel.MiddleMarkerColor := Prefs.MiddleMarkerColor;
    MUIMapPanel.MiddleMarkerSize := Prefs.MarkerSize;
    MUIMapPanel.RedrawObject;
  end;
  // Photos
  if Assigned(PhotoShowWin) then
  begin
    PhotoPanel.UseDataType := Prefs.UseDataTypes;
  end;
  InitGPS;
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
  case GoToMode of
    0: begin
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
    1: CenterToRoute(CurRoute);
    2: begin
      MiddlePos := GoToPos;
      CurZoom := 11;
      MUIMapPanel.RefreshImage;
    end;
  end;
end;

procedure RouteChangedEvent;
begin
  UpdateRoutes;
  MUIMapPanel.RefreshImage;
  MUIMapPanel.RedrawObject;
end;


procedure NextPhotoEvent(Dir: Integer);
var
  Active, Idx, i: Integer;
begin
  Active := MH_Get(PhotosListEntry, MUIA_List_Active);
  if (Active >= 0) and (Active <= PhotoList.VisList.Count) then
  begin
    Idx := PhotoList.VisList[Active].Idx;
    Idx := Min(PhotoList.Count - 1, Max(0, Idx + Dir));
    if (Idx < 0) or (Idx = PhotoList.VisList[Active].Idx) then
      Exit;
    for i := 0 to PhotoList.VisList.Count - 1 do
    begin
      if PhotoList.VisList[i].Idx = Idx then
      begin
        MH_Set(PhotosListEntry, MUIA_List_Active, i);
        DblPhotoEvent(nil, nil, nil);
        Exit;
      end;
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
  MenuHook, SearchAckHook, DblSearchHook,DblWayPointHook, DblPhotoHook, PhotoActiveHook: THook;
  RemTrack, EditTrack: PObject_;
  RemTrackHook, DblTrackHook, EditTrackHook: Thook;
  RemRoute, EditRoute, AddRoute: PObject_;
  AddRouteHook, RemRouteHook, DblRouteHook, EditRouteHook: Thook;
  AddWay, RemWay, EditWay: PObject_;
  AddWayHook, RemWayHook, EditWayHook: Thook;
  RemPhoto, AddPhoto: PObject_;
  AddPhotoHook, RemPhotoHook: Thook;
  StartTime: Int64;
  WayMenuHooks: array[0..3] of THook;
  RexxHook: THook;
  ThisAppDiskIcon: Pointer;
  i: Integer;
  OldDate: TDateTime;
begin
  SidePanelOpen := Prefs.SidePanelOpen;
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
  OnUpdatePrefs := @UpdatePrefs;
  //
  OnWPChanged := @WPChangedEvent;
  OnTrackChanged := @TrackChangedEvent;
  OnTrackRedraw := @TrackRedrawEvent;
  OnRouteGoToPos := @GoToPosEvent;
  OnRouteChanged := @RouteChangedEvent;
  OnNextPhoto := @NextPhotoEvent;
  try
    SearchTitleStr :=  GetLocString(MSG_SEARCH_RESULTS_TITLE); // 'Search Results';
    //
    StrCoord := FloatToStrF(MiddlePos.Lat, ffFixed, 8,6) + ' ; ' + FloatToStrF(MiddlePos.Lon, ffFixed, 8,6) + ' ; ' + IntToStr(CurZoom) + '  ';
    StrCoord := Format('%25s', [StrCoord]);
    //
    // Popupmenu for Lists ++++++++++++++++++++++++++++++++++++++
    MapFeatureMenu := MH_Menustrip([
      Child, AsTag(MH_Menu(GetLocString(MSG_POPUP_FEATURE), [                   // 'Map Feature'
        Child, AsTag(MH_MenuItem(WM2, [
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_POPUP_FEATURE_GOTO)),     // 'GoTo'
          TAG_DONE])),
        Child, AsTag(MH_MenuItem([ MUIA_Menuitem_Title, AsTag(NM_BarLabel), TAG_DONE])),
        Child, AsTag(MH_MenuItem(WM1, [
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_POPUP_FEATURE_TOGGLE)),   // 'Toggle visibility'
          TAG_DONE])),
        Child, AsTag(MH_MenuItem(WM3, [
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_POPUP_FEATURE_SHOWALL)),  // 'Show all'
          TAG_DONE])),
        Child, AsTag(MH_MenuItem(WM4, [
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_POPUP_FEATURE_HIDEALL)),  // 'Hide all'
          TAG_DONE])),
        TAG_DONE])),
      TAG_DONE]);
    //
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //
    // Side Panel
    SidePanel := MH_VGroup([
      MUIA_ObjectID, 1,
      MUIA_ShowMe, AsTag(Prefs.SidePanelOpen),
      Child, AsTag(MH_Register(ListTabs, [
        MUIA_ObjectID, 2,
        MUIA_Group_ActivePage, Prefs.SidePage,
        MUIA_Register_Titles, AsTag(@TabTitles),
        //#### Search list
        Child, AsTag(MH_ListView(SearchList, [
          MUIA_Weight, 100,
          MUIA_ObjectID, 3,
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
              MUIA_ContextMenu, AsTag(MapFeatureMenu),
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
              MUIA_ContextMenu, AsTag(MapFeatureMenu),
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
              MUIA_ContextMenu, AsTag(MapFeatureMenu),
              MUIA_List_PoolThreshSize, 256,
              TAG_DONE])),
            TAG_DONE])),
          Child, AsTag(MH_HGroup([
              Child, AsTag(MH_Button(AddRoute, GetLocString(MSG_BUTTON_WAYPOINT_ADD))),    // 'Add'
              Child, AsTag(MH_Button(RemRoute, GetLocString(MSG_BUTTON_WAYPOINT_REMOVE))),    // 'Remove'
              Child, AsTag(MH_Button(EditRoute, GetLocString(MSG_BUTTON_WAYPOINT_EDIT))),  //  'Edit'
            TAG_DONE])),
          TAG_DONE])),
        //#### Photos
        Child, AsTag(MH_VGroup([
          Child, AsTag(MH_ListView(PhotosList, [
            MUIA_Weight, 100,
            MUIA_Listview_Input, MUI_TRUE,
            MUIA_Listview_List, AsTag(MH_List(PhotosListEntry, [
              MUIA_Frame, MUIV_Frame_ReadList,
              MUIA_Background, MUII_ReadListBack,
              //MUIA_ContextMenu, AsTag(MapFeatureMenu),
              MUIA_List_PoolThreshSize, 256,
              TAG_DONE])),
            TAG_DONE])),
          Child, AsTag(MH_HGroup([
              Child, AsTag(MH_Button(AddPhoto, GetLocString(MSG_BUTTON_WAYPOINT_ADD))),    // 'Add'
              Child, AsTag(MH_Button(RemPhoto, GetLocString(MSG_BUTTON_WAYPOINT_REMOVE))),    // 'Remove'
            TAG_DONE])),
          TAG_DONE])),
        //
        TAG_DONE])),
      TAG_DONE]);
    if not Assigned(SidePanel) then
      writeln(GetLocString(MSG_ERROR_SIDEPANEL));  //  'Failed to create SidePanel.'
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
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_MAIN_EXPORTPNG)), //  'Export as PNG...'
          MUIA_UserData, MID_ExportPNG,
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
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_FRAME_SEARCH)),//  'Search'
          MUIA_Menuitem_Shortcut, AsTag('F'), // 'F'
          MUIA_UserData, MID_SEARCH,
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
        Child, AsTag(MH_MenuItem(MenuDrawPhotos, [
          MUIA_Menuitem_Title, AsTag(GetLocString(MSG_MENU_MAP_DRAWPHOTOS)),  //  'Draw Photos'
          MUIA_UserData, MID_DrawPhotos,
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
    MUIMapPanel.ShowSidePanelBtn := not SidePanelOpen;
    UpdatePrefs;
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
            Child, AsTag(MH_Balance(MainBalance, [MUIA_ObjectID, 4, MUIA_CycleChain, 1, MUIA_Disabled, AsTag(not SidePanelOpen), TAG_END])),
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
      SubWindow, AsTag(PhotoShowWin),
      TAG_DONE]);
    if not Assigned(app) then
    begin
      writeln(GetLocString(MSG_ERROR_APPLICATION));   // 'Failed to create Application'
      Exit;
    end;
    // version app set
    WrapApp := App;
    WrapWin := Window;
    //
    //MH_Set(AddRoute, MUIA_Disabled, MUI_TRUE);
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
    ConnectHookFunction(MUIA_Menuitem_Trigger, MUIV_EveryTime, WM2, nil, @WayMenuHooks[1], @WPGoToEvent);
    ConnectHookFunction(MUIA_Menuitem_Trigger, MUIV_EveryTime, WM3, nil, @WayMenuHooks[2], @WPShowAllEvent);
    ConnectHookFunction(MUIA_Menuitem_Trigger, MUIV_EveryTime, WM4, nil, @WayMenuHooks[3], @WPHideAllEvent);
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
    // Photo Action
    ConnectHookFunction(MUIA_Pressed, AsTag(False), AddPhoto, nil, @AddPhotoHook, @AddPhotoEvent);
    ConnectHookFunction(MUIA_Pressed, AsTag(False), RemPhoto, nil, @RemPhotoHook, @RemPhotoEvent);
    ConnectHookFunction(MUIA_Listview_DoubleClick, MUIV_EveryTime, PhotosList, nil, @DblPhotoHook, @DblPhotoEvent);
    ConnectHookFunction(MUIA_List_Active, MUIV_EveryTime, PhotosListEntry, nil, @PhotoActiveHook, @PhotoActiveEvent);

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
    // open additionally the stat window if needed
    if Prefs.StatWinOpen then
      MH_Set(StatWin, MUIA_Window_Open, AsTag(True));
    // open the window
    MH_Set(Window, MUIA_Window_Open, AsTag(True));
    //
    if Prefs.StatWinOpen then
      DoMethod(StatWin, [MUIM_Window_ToFront]);
    if MH_Get(Window, MUIA_Window_Open) <> 0 then
    begin
      Running := True;
      InitGPS;

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
        end;
        if RedrawImage then
        begin
          MUIMapPanel.RedrawObject;
          ReDrawImage := False;
        end;
        if GetMUITime - StartTime > 20 then
        begin
          if LongBool(MH_Get(PlayPanel.MUIObject, MUIA_ShowMe)) then
            PlayPanel.RedrawObject;
        end;
        // Update Status things
        if GetMUITime - StartTime > 500 then
        begin
          // Update Position
          if Assigned(SerThread) and SerThread.InitDone then
          begin
            OldDate := GPSData.Date;
            SerThread.GetData(GPSData);
            if MUIMapPanel.ShowGPSPos <> GPSData.Valid then
            begin
              MUIMapPanel.ShowGPSPos := GPSData.Valid;
              if not GPSData.Valid then
              begin
                WinTitleText := WindowTitleTemplate + ' GPS position invalid';
                MH_Set(Window, MUIA_Window_Title, AsTag(PChar(WinTitleText)));
              end;
            end;
            if Abs(GPSData.Date - OldDate) * 24 * 60 * 60 > 1 then
            begin
              if GPSData.Valid then
              begin
                WinTitleText := WindowTitleTemplate + ' Sat: ' + IntToStr(GPSData.NumSatelites) + ' / 12   Pos: ' +
                  FloatToStrF(GPSData.Lat, ffFixed, 8,6) + ';' + FloatToStrF(GPSData.Lon, ffFixed, 8,6) + ' Valid: ' + BoolToStr(GPSData.Valid, True);
              end
              else
                WinTitleText := WindowTitleTemplate + ' GPS position invalid';
              MH_Set(Window, MUIA_Window_Title, AsTag(PChar(WinTitleText)));
              writeln(DateTimeToStr(GPSData.Date) + ' Num Sat: ' + IntToStr(GPSData.NumSatelites) + ' / 12   Pos: ' +
                FloatToStrF(GPSData.Lat, ffFixed, 8,6) + ';' + FloatToStrF(GPSData.Lon, ffFixed, 8,6) + ' Valid: ' + BoolToStr(GPSData.Valid, True));
              //MiddlePos.Lat := GPSData.Lat;
              //MiddlePos.Lon := GPSData.Lon;
              MUIMapPanel.RefreshImage;
            end
            else
            begin
              GPSData.Date := OldDate;
            end;
            StartTime := GetMUITime;
          end;
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
    end
    else
    begin
      writeln('Failed to open Window.');
      Exit;
    end;
    //
    Prefs.StatWinOpen := LongBool(MH_Get(StatWin, MUIA_Window_Open));
    if Prefs.StatWinOpen then
      DoMethod(StatWin, [MUIM_Window_Snapshot, 1]);
    Prefs.SidePanelOpen := LongBool(MH_Get(SidePanel, MUIA_ShowMe));
    Prefs.SidePage := MH_Get(ListTabs, MUIA_Group_ActivePage);
    // end of main loop, close window if still open
    MH_Set(Window, MUIA_Window_Open, AsTag(False));
  finally
    CloseGPS;
    if Assigned(MapFeatureMenu) then
      MUI_DisposeObject(MapFeatureMenu);
    if Assigned(App) then
      MUI_DisposeObject(app);
    if Assigned(ThisAppDiskIcon) then
      FreeDiskObject(ThisAppDiskIcon);
    SRes.Free;
    MUIMapPanel.Free;
  end;
end;

{$ifdef AMIGA68k}
procedure TestVampire;
var
  a: Double;
begin
  a := 5;
  if Round(a*1.3) = 0 then
  begin
    writeln('Your FPU is not IEEE 754 compatible, please use the NoFPU Version');
    halt(5);
  end;
end;
{$endif}

begin
  {$ifdef AMIGA68k}
  TestVampire;
  {$endif}
  StartMe;
end.
