unit RoutePropsUnit;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, exec, utility, intuition, agraphics, mui, muihelper, Math, StrUtils,
  prefsunit, osmhelper, MUIWrap, imagesunit, positionunit, waypointunit;

var
  RoutePropsWin: PObject_;
  OnRouteChanged: TProcedure = nil;

  GoToMode: Integer; // = 0 Order, 1 = Route, 2 = Position
  CurRoute: TRoute = nil;
  GoToPos: TCoord;
  OnRouteGoToPos: TProcedure = nil;

  CurOrder: TOrder = nil;
  FromPos, ToPos: TCoord;


const
  YOURBASEURL = 'http://www.yournavigation.org/api/1.0/gosmore.php';
  OPENLSBASEURL = 'http://openls.geog.uni-heidelberg.de/route';

procedure ShowRouteProps(NewRoute: TRoute);
procedure NewRouteProps();
procedure CheckSearchButton;

implementation

uses
  MUIMappariumlocale, classes, networkingunit, dom, xmlread, versionunit,
  ASL, MapPanelUnit;

var
  RouteName, SaveButton, CloseButton, RouteCol, OrderListEntry, OrderList//,
  {WPLat, WPLon, CurPos}: PObject_;
  FromPS, FromBtn, FromEdit, FromLW, FromList, FromText: PObject_;
  ToPS, ToBtn, ToEdit, ToLW, ToList, ToText, DriveDevice, RouteType, CalcRoute, CalcRoutePanel: PObject_;
  GetFromPos, GetToPos, PopMenu, WM1: PObject_;
  SaveHook, DblOrderHook, DblFromHook, DblToHook, OpenHook, CalcRouteHook,
  FromEnterHook, ToEnterHook, CloseHook, GetFromPosHook, GetToPosHook, ExportHTMLHook: THook;
  FreeRoute: Boolean;

  DriveStrings: array[0..2] of string;
  RouteStrings: array[0..1] of string;
  DriveDevices: array of PChar;
  RouteTypes: array of PChar;

procedure UpdateCurRouteOrder;
var
  i: Integer;
begin
  DoMethod(OrderListEntry, [MUIM_List_Clear]);
  MH_Set(OrderListEntry, MUIA_List_Quiet, AsTag(True));
  if Assigned(CurRoute) then
  begin
    for i := 0 to CurRoute.Orders.Count - 1 do
    begin
      CurRoute.Orders[i].FormatOrder;
      DoMethod(OrderListEntry, [MUIM_List_InsertSingle, AsTag(PChar(CurRoute.Orders[i].FOrder)), AsTag(MUIV_List_Insert_Bottom)]);
    end;
  end;
  MH_Set(OrderListEntry, MUIA_List_Quiet, AsTag(False));
end;

procedure NewRouteProps();
var
  MUIRGB: TMUI_RGBcolor;
begin
  CurRoute := nil;
  FromPos.Lat := NaN;
  FromPos.Lon := NaN;
  ToPos.Lat := NaN;
  ToPos.Lon := NaN;
  MH_Set(FromEdit, MUIA_String_Contents, AsTag(PChar('')));
  MH_Set(ToEdit, MUIA_String_Contents, AsTag(PChar('')));
  MH_Set(FromText, MUIA_Text_Contents, AsTag(PChar(GetLocString(MSG_ROUTEPROP_FROM) + ':                            ')));
  MH_Set(ToText, MUIA_Text_Contents, AsTag(PChar(GetLocString(MSG_ROUTEPROP_TO) + ':                            ')));
  MH_Set(CalcRoutePanel, MUIA_ShowMe, AsTag(True));
  MH_Set(RoutePropsWin, MUIA_Window_Open, AsTag(True));
  MUIRGB.Red := $FFFFFFFF;
  MUIRGB.Green := 0;
  MUIRGB.Blue := 0;
  MH_Set(RouteCol, MUIA_Pendisplay_RGBcolor, AsTag(@MUIRGB));
  // Set Name
  MH_Set(RouteName, MUIA_String_Contents, AsTag(PChar(GetLocString(MSG_ROUTEPROP_NEW_NAME)))); //'New Route'
  UpdateCurRouteOrder;
  CheckSearchButton;
  MH_Set(SaveButton, MUIA_Disabled, AsTag(True));
end;

// Open Properties Window
procedure ShowRouteProps(NewRoute: TRoute);
var
  MUIRGB: TMUI_RGBcolor;
begin
  MH_Set(RoutePropsWin, MUIA_Window_Open, AsTag(False));
  if Assigned(NewRoute) then
  begin
    MH_Set(CalcRoutePanel, MUIA_ShowMe, AsTag(False));
    CurRoute := NewRoute;
    MH_Set(RoutePropsWin, MUIA_Window_Open, AsTag(True));
    // Set Name
    MH_Set(RouteName, MUIA_String_Contents, AsTag(PChar(NewRoute.Name)));
    // Set Color
    MUIRGB.Red := NewRoute.Color shl 8;
    MUIRGB.Green := NewRoute.Color shl 16;
    MUIRGB.Blue := NewRoute.Color shl 24;
    MH_Set(RouteCol, MUIA_Pendisplay_RGBcolor, AsTag(@MUIRGB));
    //
    UpdateCurRouteOrder;
    CheckSearchButton;
    MH_Set(SaveButton, MUIA_Disabled, AsTag(False));
  end;
end;

var
  SearchTitleStr: string;
  SRes: TSearchResults;

function SearchEntry(SearchTerm: AnsiString; List: PObject_): Integer;
var
  Mem: TMemoryStream;
  Doc: TXMLDocument;
  Url, EncStr: String;
  Child, Node: TDOMNode;
  i: Integer;
  sr: TSearchResult;
begin
  Result := 0;
  DoMethod(List, [MUIM_List_Clear]);
  if not IsOnline then
  begin
    SearchTitleStr :=  GetLocString(MSG_ERROR_ONLINE); // 'Not Online'#0;
    DoMethod(List, [MUIM_List_InsertSingle, AsTag(PChar(SearchTitleStr)), AsTag(MUIV_List_Insert_Bottom)]);
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
  Url := SEARCHURL + EncStr + '&format=xml&accept-language=' + Prefs.SearchLang;
  Mem := TMemoryStream.Create;
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
          DoMethod(List, [MUIM_List_InsertSingle, AsTag(PChar(SR.DisplayName)), AsTag(MUIV_List_Insert_Bottom)]);
        end;
        Child := Child.NextSibling;
      end;
      if SRes.Count = 0 then
      begin
        SearchTitleStr := Format(GetLocString(MSG_ERROR_NOTHINGFOUND), [SearchTerm]); //'Nothing found for "'+SearchTerm+'"'#0;
        DoMethod(List, [MUIM_List_InsertSingle, AsTag(PChar(SearchTitleStr)), AsTag(MUIV_List_Insert_Bottom)]);
      end
      else
      begin
        SearchTitleStr :=  Format(GetLocString(MSG_SEARCH_RESULTS), [SRes.Count, SearchTerm]);
        //MH_Set(List, MUIA_List_Title, AsTag(@SearchTitleStr[1]));
      end;
    end else
    begin
      SearchTitleStr :=  GetLocString(MSG_ERROR_NETWORK); //'Network error'
      DoMethod(List, [MUIM_List_InsertSingle, AsTag(PChar(SearchTitleStr)), AsTag(MUIV_List_Insert_Bottom)]);
    end;
  finally
    Mem.Free;
    Doc.Free;
  end;
  Result := SRes.Count;
end;

function FromOpenEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  SText: string;
begin
  Result := 0;
  if Obj = FromLW then
  begin
    SText := PChar(MH_Get(FromEdit, MUIA_String_Contents));
    if SText <> '' then
      Result := SearchEntry(SText, FromList);
  end
  else
  begin
    SText := PChar(MH_Get(ToEdit, MUIA_String_Contents));
    if SText <> '' then
      Result := SearchEntry(SText, ToList);
  end;
end;

function FromEnterEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := DoMethod(FromPS, [MUIM_Popstring_Open]);
end;

function ToEnterEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := DoMethod(ToPS, [MUIM_Popstring_Open]);
end;

function DblFromEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Idx: Integer;
begin
  //DoubleClick From List;
  DoMethod(FromPS, [MUIM_Popstring_Close, AsTag(True)]);
  Idx := MH_Get(FromList, MUIA_List_Active);
  if (Idx >= 0) and (Idx < SRes.Count) then
  begin
    FromPos.Lat := SRes[Idx].Lat;
    FromPos.Lon := SRes[Idx].Lon;
    MH_Set(FromText, MUIA_Text_Contents, AsTag(PChar(GetLocString(MSG_ROUTEPROP_FROM) + ': ' + FloatToStrF(FromPos.Lat, ffFixed, 10, 6) + ' ; ' + FloatToStrF(FromPos.Lon, ffFixed, 10, 6))));
    MH_Set(FromEdit, MUIA_String_Contents, AsTag(PChar(SRes[Idx].DisplayName)));
    GoToPos := FromPos;
    CurOrder := nil;
    GoToMode := 2;
    if Assigned(OnRouteGoToPos) then
      OnRouteGoToPos;
  end;
  CheckSearchButton;
  Result := 1;
end;

function DblToEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Idx: Integer;
begin
  //DoubleClick To List
  DoMethod(ToPS, [MUIM_Popstring_Close, AsTag(True)]);
  Idx := MH_Get(ToList, MUIA_List_Active);
  if (Idx >= 0) and (Idx < SRes.Count) then
  begin
    ToPos.Lat := SRes[Idx].Lat;
    ToPos.Lon := SRes[Idx].Lon;
    MH_Set(ToText, MUIA_Text_Contents, AsTag(PChar(GetLocString(MSG_ROUTEPROP_TO) + ': ' + FloatToStrF(ToPos.Lat, ffFixed, 10, 6) + ' ; ' + FloatToStrF(ToPos.Lon, ffFixed, 10, 6))));
    MH_Set(ToEdit, MUIA_String_Contents, AsTag(PChar(SRes[Idx].DisplayName)));
    GoToPos := ToPos;
    CurOrder := nil;
    GoToMode := 2;
    if Assigned(OnRouteGoToPos) then
      OnRouteGoToPos;
  end;
  CheckSearchButton;
  Result := 1;
end;


// Save the Values
function SaveEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  MUIRGB: PMUI_RGBcolor;
begin
  Result := 0;
  if Assigned(CurRoute) then
  begin
    if RouteList.IndexOf(CurRoute) < 0 then
      RouteList.Add(CurRoute);
    CurRoute.Name := PChar(MH_Get(RouteName, MUIA_String_Contents));
    MUIRGB := PMUI_RGBcolor(MH_Get(RouteCol, MUIA_Pendisplay_RGBcolor));
    if Assigned(MUIRGB) then
      CurRoute.Color := (MUIRGB^.Red shr 8 and $ff0000) or (MUIRGB^.Green shr 16 and $FF00) or (MUIRGB^.Blue shr 24 and $FF);
  end;
  FreeRoute := False;
  CurOrder := nil;
  CurRoute := nil;
  MH_Set(RoutePropsWin, MUIA_Window_Open, AsTag(False));
  if Assigned(OnRouteChanged) then
    OnRouteChanged;
end;

function CloseEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  if Assigned(CurRoute) and FreeRoute then
    CurRoute.Free;
  CurRoute := nil;
  CurOrder := nil;
end;


function DblOrderEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Active: LongInt;
  Order: TOrder;
begin
  Result := 0;
  if Assigned(CurRoute) then
  begin
    Active := MH_Get(OrderListEntry, MUIA_List_Active);
    if (Active >= 0) and (Active < CurRoute.Orders.Count) then
    begin
      Order := CurRoute.Orders[Active];
      if Assigned(Order) then
      begin
        GoToMode := 0;
        CurOrder := Order;
        if Assigned(OnRouteGoToPos) then
          OnRouteGoToPos();
      end;
    end;
  end;
end;

function GetTextNode(StartNode: TDomNode; s: string): string;
var
  LNode: TDOMNode;
begin
  Result := '';
  LNode := StartNode.FindNode(WideString(s));
  if Assigned(LNode) then
    Result := string(LNode.TextContent);
end;

procedure CheckSearchButton;
var
  Dis: Boolean;
begin
  Dis := IsNan(FromPos.Lat) or isNan(FromPos.Lon) or IsNan(ToPos.Lat) or isNan(ToPos.Lon);
  MH_Set(CalcRoute, MUIA_Disabled, AsTag(Dis));
end;


procedure DoRouteSearch;
var
  URL, Distance, TravelTime, StrCoords, Desc, Line, SLon, SLat: String;
  StrStream: TStringStream;
  XmlDoc: TXMLDocument;
  KmlNode, DocumentNode, FolderNode, PlaceNode, LineString, RouteNode,
    DistNode, TextNode: TDOMNode;
  SL, SLDesc: TStringList;
  i: Integer;
  P1: SizeInt;
  Idx, j, Hours, Minu, Secs: Integer;
  UseOpenLS: Boolean;
  Order: TOrder;
begin
  UseOpenLS := False;//ServiceType.ItemIndex = 0;
  if UseOpenLS then
  begin
    URL := OPENLSBASEURL + '?distunit=KM&via=&avoidAreas=&useTMC=false&noTollways=false&noFerries=false&instructions=true&lang=' + Prefs.SearchLang;
    URL := URL + '&start=' + FloatToStrF(FromPos.Lon, ffFixed, 8,6) + ',' + FloatToStrF(FromPos.Lat, ffFixed, 8,6);
    URL := URL + '&end=' + FloatToStrF(ToPos.Lon, ffFixed, 8,6) + ',' + FloatToStrF(ToPos.Lat, ffFixed, 8,6);
    URL := URL + '&weighting=' + ifthen(MH_Get(RouteType, MUIA_Cycle_Active) = 0, 'Fastest','Shortest');
    case MH_Get(DriveDevice, MUIA_Cycle_Active) of
      0: begin  // car
        URL := URL + '&routepref=Car&noMotorways=false&noUnpavedroads=true&noSteps=true';
      end;
      1: begin  // bicycle
        URL := URL + '&routepref=Bicycle&noMotorways=true&noUnpavedroads=false&noSteps=true';
      end;
      2: begin // foot
        URL := URL + '&routepref=Pedestrian&noMotorways=false&noUnpavedroads=false&noSteps=false';
      end;
    end;
    URL := URL + '';
  end
  else
  begin
    URL := YOURBASEURL + '?&lang=' + IfThen(Prefs.SearchLang = 'en', 'en_GB', Prefs.SearchLang);
    URL := URL + '&flat=' + FloatToStrF(FromPos.Lat, ffFixed, 8,6);
    URL := URL + '&flon=' + FloatToStrF(FromPos.Lon, ffFixed, 8,6);
    URL := URL + '&tlat=' + FloatToStrF(ToPos.Lat, ffFixed, 8,6);
    URL := URL + '&tlon=' + FloatToStrF(ToPos.Lon, ffFixed, 8,6);
    URL := URL + '&fast=' + ifthen(MH_Get(RouteType, MUIA_Cycle_Active) = 0, '1','0');
    case MH_Get(DriveDevice, MUIA_Cycle_Active) of
      0: begin  // car
        URL := URL + '&v=motorcar';
      end;
      1: begin  // bicycle
        URL := URL + '&v=bicycle';
      end;
      2: begin // foot
        URL := URL + '&v=foot';
      end;
    end;
    URL := URL + '&instructions=1';
  end;
  XmlDoc := nil;
  SL := TStringList.Create;
  StrStream := TStringStream.Create('');
  try
  if UseOpenls then
  begin
    if GetFile(URL, StrStream) then
    begin
      //StrStream.Position:=0;
      StrStream.Position:=0;
      ReadXMLFile(XmlDoc, StrStream);
      KmlNode := XmlDoc.FindNode('xls:XLS');
      if not Assigned(KmlNode) then
        Exit;
      DocumentNode := KmlNode.FindNode('xls:Response');
      if not Assigned(DocumentNode) then
        Exit;
      FolderNode := DocumentNode.FindNode('xls:DetermineRouteResponse');
      if not Assigned(FolderNode) then
        Exit;
      // Route Summary
      RouteNode := FolderNode.FindNode('xls:RouteSummary');
      if not Assigned(RouteNode) then
        Exit;
      TravelTime := GetTextNode(RouteNode, 'xls:TotalTime');
      DistNode := RouteNode.FindNode('xls:TotalDistance');
      if Assigned(DistNode) then
      begin
        DistNode := DistNode.Attributes.GetNamedItem('value');
        if Assigned(DistNode) then
          Distance := string(DistNode.NodeValue);
      end;
      // Route Geometry
      RouteNode := FolderNode.FindNode('xls:RouteGeometry');
      if not Assigned(RouteNode) then
        Exit;
      LineString := RouteNode.FindNode('gml:LineString');
      if not Assigned(LineString) then
        Exit;
      if not Assigned(CurRoute) or not FreeRoute then
        CurRoute := TRoute.Create;
      FreeRoute := True;
      SetLength(CurRoute.Pts, 0);
      CurRoute.Orders.Clear;
      //CurRoute.Name:= FromEdit.Text + ' - ' + ToEdit.Text;
      CurRoute.Name := string(PChar(MH_Get(FromEdit, MUIA_String_Contents))) + ' - ' + string(PChar(MH_Get(ToEdit, MUIA_String_Contents)));
      CurRoute.Distance := StrToFloatDef(Distance, 0);
      Hours := 0;
      Minu := 0;
      Secs := 0;
      if (Length(TravelTime) > 2) and (TravelTime[1] = 'P') and (TravelTime[2] = 'T') then
      begin
        Delete(TravelTime, 1, 2);
        P1 := Pos('H', TravelTime);
        if P1 > 0 then
        begin
          Hours := StrToIntDef(Copy(TravelTime, 1, P1 - 1), 0);
          Delete(TravelTime, 1, P1);
        end;
        P1 := Pos('M', TravelTime);
        if P1 > 0 then
        begin
          Minu := StrToIntDef(Copy(TravelTime, 1, P1 - 1), 0);
          Delete(TravelTime, 1, P1);
        end;
        P1 := Pos('S', TravelTime);
        if P1 > 0 then
        begin
          Secs := StrToIntDef(Copy(TravelTime, 1, P1 - 1), 0);
          Delete(TravelTime, 1, P1);
        end;
      end;
      CurRoute.TravelTime := Secs + (((Hours*60) + Minu)*60);
      SetLength(CurRoute.Pts, LineString.ChildNodes.Count);
      Idx := 0;
      for i := 0 to LineString.ChildNodes.Count - 1 do
      begin
        TextNode := LineString.ChildNodes.Item[i];
        if TextNode.NodeName = 'gml:pos' then
        begin
          Line := string(TextNode.TextContent);
          P1 := Pos(' ', Line);
          SLon := Copy(Line, 1, P1 - 1);
          SLat := Copy(Line, P1 + 1, Length(Line));
          CurRoute.Pts[Idx].Position.Lat:=StrToFloatDef(SLat, Nan);
          CurRoute.Pts[Idx].Position.Lon:=StrToFloatDef(SLon, Nan);
          if not isNan(CurRoute.Pts[Idx].Position.Lat) and not IsNan(CurRoute.Pts[Idx].Position.Lon) then
            Inc(Idx)
        end;
      end;
      SetLength(CurRoute.Pts, Idx);
      // route instructionlist
      RouteNode := FolderNode.FindNode('xls:RouteInstructionsList');
      if not Assigned(RouteNode) then
        Exit;
      for i := 0 to RouteNode.ChildNodes.Count - 1 do
      begin
        TextNode := RouteNode.ChildNodes.Item[i];
        if TextNode.NodeName = 'xls:RouteInstruction' then
        begin
          Order :=  TOrder.Create;
          Order.Order := GetTextNode(TextNode, 'xls:Instruction');
          LineString := TextNode.FindNode('xls:RouteInstructionGeometry');
          if Assigned(LineString) then
          begin
            LineString := LineString.FindNode('gml:LineString');
            if Assigned(LineString) then
            begin
              SetLength(Order.Positions, LineString.ChildNodes.Count);
              Idx := 0;
              for j := 0 to LineString.ChildNodes.Count - 1 do
              begin
                TextNode := LineString.ChildNodes.Item[j];
                if TextNode.NodeName = 'gml:pos' then
                begin
                  Line := string(TextNode.TextContent);
                  P1 := Pos(' ', Line);
                  SLon := Copy(Line, 1, P1 - 1);
                  SLat := Copy(Line, P1 + 1, Length(Line));
                  Order.Positions[Idx].Lat:=StrToFloatDef(SLat, Nan);
                  Order.Positions[Idx].Lon:=StrToFloatDef(SLon, Nan);
                  if not isNan(Order.Positions[Idx].Lat) and not IsNan(Order.Positions[Idx].Lon) then
                    Inc(Idx)
                end;
              end;
              SetLength(Order.Positions, Idx);
            end;
          end;
          CurRoute.Orders.Add(Order);
        end;
      end;
      //ShowRouteInWindow;
      //AddToListButton.Enabled := True;
    end;
  end
  else
    if GetYourFile(URL, StrStream) then
    begin
      //StrStream.Position:=0;
      StrStream.Position:=0;
      ReadXMLFile(XmlDoc, StrStream);
      KmlNode := XmlDoc.FindNode('kml');
      if Assigned(KmlNode) then
      begin
        DocumentNode := KmlNode.FindNode('Document');
        if Assigned(DocumentNode) then
        begin
          if not Assigned(CurRoute) or not FreeRoute then
            CurRoute := TRoute.Create;
          FreeRoute := True;
          SetLength(CurRoute.Pts, 0);
          CurRoute.Orders.Clear;
          CurRoute.Name := string(PChar(MH_Get(FromEdit, MUIA_String_Contents))) + ' - ' + string(PChar(MH_Get(ToEdit, MUIA_String_Contents)));
          Distance := GetTextNode(DocumentNode, 'distance');
          CurRoute.Distance := StrToFloatDef(Distance, 0);
          TravelTime := GetTextNode(DocumentNode, 'traveltime');
          CurRoute.TravelTime := StrToIntDef(TravelTime, 0);
          Desc := GetTextNode(DocumentNode, 'description');
          Desc := StringReplace(Desc, '<br>', '', [rfIgnoreCase, rfReplaceAll]);
          SLDesc := TStringList.Create;
          try
            SLDesc.Text := Desc;
            for i:= 0 to SLDesc.Count - 1 do
            begin
              Order :=  TOrder.Create;
              Order.Order := SLDesc[i];
              SetLength(Order.Positions, 0);
              CurRoute.Orders.Add(Order);
            end;
          finally
            SLDesc.Free;
          end;
          FolderNode := DocumentNode.FindNode('Folder');
          SetLength(CurRoute.Pts,0);
          if Assigned(FolderNode) then
          begin
            PlaceNode := FolderNode.FindNode('Placemark');
            if Assigned(PlaceNode) then
            begin
              LineString := PlaceNode.FindNode('LineString');
              if Assigned(LineString) then
              begin
                StrCoords := GetTextNode(LineString, 'coordinates');
                SL.Text := StrCoords;
                SetLength(CurRoute.Pts, SL.Count);
                Idx := 0;
                for i := 0 to SL.Count - 1 do
                begin
                  Line := SL[i];
                  P1 := Pos(',', Line);
                  SLon := Copy(Line, 1, P1 - 1);
                  SLat := Copy(Line, P1 + 1, Length(Line));
                  CurRoute.Pts[Idx].Position.Lat:=StrToFloatDef(SLat, Nan);
                  CurRoute.Pts[Idx].Position.Lon:=StrToFloatDef(SLon, Nan);
                  if not isNan(CurRoute.Pts[Idx].Position.Lat) and not IsNan(CurRoute.Pts[Idx].Position.Lon) then
                    Inc(Idx)
                end;
                SetLength(CurRoute.Pts, Idx);
                //ShowRouteInWindow;
                //AddToListButton.Enabled := True;
              end;
            end;
          end;
        end;
      end;
    end;
    MH_Set(SaveButton, MUIA_Disabled, AsTag(False));
  finally
    StrStream.Free;
    XmlDoc.Free;
    SL.Free;
  end;
end;

function CalcRouteEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  DoRouteSearch;
  UpdateCurRouteOrder;
  MH_Set(RouteName, MUIA_String_Contents, AsTag(PChar(CurRoute.Name)));
  Result := 0;
  //
  GoToMode := 1;
  if Assigned(OnRouteGoToPos) then
    OnRouteGoToPos();
end;

function GetFromPosEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  FromPos := MiddlePos;
  MH_Set(FromText, MUIA_Text_Contents, AsTag(PChar(GetLocString(MSG_ROUTEPROP_FROM) + ': ' + FloatToStrF(FromPos.Lat, ffFixed, 10, 6) + ' ; ' + FloatToStrF(FromPos.Lon, ffFixed, 10, 6))));
  MH_Set(FromEdit, MUIA_String_Contents, AsTag(PChar(GetLocString(MSG_WAYPROP_CURRENTPOS))));
  CheckSearchButton;
end;

function GetToPosEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  ToPos := MiddlePos;
  MH_Set(ToText, MUIA_Text_Contents, AsTag(PChar(GetLocString(MSG_ROUTEPROP_TO) + ': ' + FloatToStrF(ToPos.Lat, ffFixed, 10, 6) + ' ; ' + FloatToStrF(ToPos.Lon, ffFixed, 10, 6))));
  MH_Set(ToEdit, MUIA_String_Contents, AsTag(PChar(GetLocString(MSG_WAYPROP_CURRENTPOS))));
  CheckSearchButton;
end;

const
  HTMLTemplate = '<html>'#13#10 +
  '<head><title>MUIMapparium Route: %ROUTENAME%</title></head>'#13#10 +
  '<body bgcolor="#FFFFFF">'#13#10 +
  '<h2 align=center>%ROUTENAME%</h2>'#13#10 +
  '<h5 align=center>%VERSION%</h5>'#13#10 +
  '<img src="%PNG%"><br>'#13#10 +
  '<table>'#13#10 +
  '%ORDERS%'#13#10 +
  '</table>'#13#10 +
  '</body></html>';


procedure SaveToHTML(AFileName: string);
var
  PngFile, HtmlFile: string;
  SL: TStringList;
  s, t: string;
  i: Integer;
  OldR, OldT, OldW: Boolean;
begin
  if not Assigned(CurRoute) then
    Exit;
  PngFile := ChangeFileExt(AFileName, '.png');
  HtmlFile := ChangeFileExt(AFileName, '.html');
  SL := TStringList.Create;
  try
    OldR := MUIMapPanel.ShowRoutes;
    MUIMapPanel.ShowRoutes := False;
    OldT := MUIMapPanel.ShowTracks;
    MUIMapPanel.ShowTracks := False;
    OldW := MUIMapPanel.ShowMarker;
    MUIMapPanel.ShowMarker := False;
    MUIMapPanel.SaveToFile(PngFile);
    MUIMapPanel.ShowRoutes := OldR;
    MUIMapPanel.ShowTracks := OldT;
    MUIMapPanel.ShowMarker := OldW;

    s := StringReplace(HTMLTemplate, '%VERSION%', Copy(VERSIONSTRING, 6, Length(VERSIONSTRING)), [rfReplaceAll]);
    s := StringReplace(s, '%ROUTENAME%', CurRoute.Name, [rfReplaceAll]);
    s := StringReplace(s, '%PNG%', ExtractFileName(PngFile), [rfReplaceAll]);
    //
    t := '';
    for i := 0 to CurRoute.Orders.Count - 1 do
    begin
      t := t + '<tr><td>' + CurRoute.Orders[i].Order + '</td></tr>'#13#10;
    end;
    s := StringReplace(s, '%ORDERS%', t, [rfReplaceAll]);
    SL.Text := s;
    SL.SaveToFile(HTMLFile);
  finally
    SL.Free;
  end;
end;


function ExportHTMLEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  fr: PFileRequester;
  OldDrawer: string;
  OldFileName: string;
begin
  Result := 0;
{$R-}
  OldFilename := stringreplace(Copy(CurRoute.Name, 1, 27) + '.html', '/', '', [rfReplaceAll]);
  OldFilename := stringreplace(OldFilename, '\', '', [rfReplaceAll]);
  OldFilename := stringreplace(OldFilename, ':', '', [rfReplaceAll]);
  OldFilename := stringreplace(OldFilename, ';', '', [rfReplaceAll]);
  OldFilename := stringreplace(OldFilename, ',', '', [rfReplaceAll]);
  OldFilename := stringreplace(OldFilename, ' ', '_', [rfReplaceAll]);
  OldDrawer := Prefs.LoadPath;
  fr := AllocAslRequestTags(ASL_FileRequest, [
    NativeUInt(ASLFR_TitleText),      NativeUInt(PChar('Choose name to save the route as HTML')),
    NativeUInt(ASLFR_InitialFile),    NativeUInt(PChar(OldFileName)),
    NativeUInt(ASLFR_InitialDrawer),  NativeUInt(PChar(OldDrawer)),
    NativeUInt(ASLFR_InitialPattern), NativeUInt(PChar('#?.html')),
    NativeUInt(ASLFR_DoSaveMode),     LTrue,
    NativeUInt(ASLFR_DoPatterns),     LTrue,
    TAG_END]);
  if Assigned(fr) then
  begin
    //
    if AslRequestTags(fr, [TAG_END]) then
    begin
      OldFilename := IncludeTrailingPathDelimiter(string(fr^.fr_drawer)) + string(fr^.fr_file);
      SaveToHTML(OldFilename);
    end;
    FreeAslRequest(fr);
  end;
end;

procedure CreateRoutePropsWin;
var
  i: Integer;
begin
  DriveStrings[0] := GetLocString(MSG_ROUTEPROP_CAR);
  DriveStrings[1] := GetLocString(MSG_ROUTEPROP_BIKE);
  DriveStrings[2] := GetLocString(MSG_ROUTEPROP_FOOT);
  SetLength(DriveDevices, Length(DriveStrings) + 1);
  for i := 0 to High(DriveStrings) do
    DriveDevices[i] := PChar(DriveStrings[i]);
  DriveDevices[High(DriveDevices)] := nil;

  RouteStrings[0] := GetLocString(MSG_ROUTEPROP_FASTEST);
  RouteStrings[1] := GetLocString(MSG_ROUTEPROP_SHORTEST);
  SetLength(RouteTypes, Length(RouteStrings) + 1);
  for i := 0 to High(RouteStrings) do
    RouteTypes[i] := PChar(RouteStrings[i]);
  RouteTypes[High(RouteTypes)] := nil;

  PopMenu := MH_Menustrip([
    Child, AsTag(MH_Menu(GetLocString(MSG_ROUTEPOP_EXPORT), [                   // 'Export'
      Child, AsTag(MH_MenuItem(WM1, [
        MUIA_Menuitem_Title, AsTag(GetLocString(MSG_ROUTEPOP_EXPORTHTML)),     // 'Export as HTML'
        TAG_DONE])),
      TAG_DONE])),
    TAG_DONE]);

  MH_SetHook(OpenHook, @FromOpenEvent, nil);
  RoutePropsWin := MH_Window([
    MUIA_Window_Title,     AsTag(GetLocString(MSG_ROUTEPROP_TITLE)), // 'Route Properties'
    MUIA_Window_ID,        AsTag(MAKE_ID('R','O','U','P')),
    MUIA_HelpNode,         AsTag('RouteWin'),
    WindowContents, AsTag(MH_VGroup([
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        MUIA_FrameTitle, AsTag(GetLocString(MSG_ROUTEPROP_NAME)),    // 'Route Title'
        Child, AsTag(MH_String(RouteName, [
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Format, MUIV_String_Format_Left,
          MUIA_Weight, 180,
          MUIA_String_Contents, AsTag('________________________'),
          TAG_END])),
        Child, AsTag(MH_Poppen(RouteCol, [MUIA_Weight, 20, TAG_END])),
        TAG_END])),
        Child, AsTag(MH_Group(CalcRoutePanel, [
          Child, AsTag(MH_HGroup([
              Child, AsTag(MH_Text(FromText, PChar(GetLocString(MSG_ROUTEPROP_FROM) + ':                            '))),
              Child, AsTag(MH_Text(GetFromPos, [
                MUIA_Text_Contents, AsTag(PChar(GetLocString(MSG_WAYPROP_CURRENTPOS))),
                MUIA_FixWidthTxt, AsTag(PChar(GetLocString(MSG_WAYPROP_CURRENTPOS))),
                MUIA_Background, MUII_ButtonBack,
                MUIA_Frame, ButtonFrame,
                MUIA_InputMode, MUIV_InputMode_RelVerify,
                TAG_END])),
              TAG_END])),
          Child, AsTag(MH_PopObject(FromPS, [
            MUIA_Popobject_StrObjHook, AsTag(@OpenHook),
            MUIA_PopString_Button, AsTag(MH_Text(FromBtn, [
              MUIA_Text_Contents, AsTag(PChar(GetLocString(MSG_ROUTEPROP_SEARCH))),
              MUIA_FixWidthTxt, AsTag(PChar(GetLocString(MSG_ROUTEPROP_SEARCH))),
              MUIA_Background, MUII_ButtonBack,
              MUIA_Frame, ButtonFrame,
              MUIA_InputMode, MUIV_InputMode_RelVerify,
              TAG_END])),
            MUIA_Popstring_String, AsTag(MH_String(FromEdit, [
              MUIA_Frame, StringFrame,
              TAG_END])),
            MUIA_PopObject_Object, AsTag(MH_ListView(FromLW, [
              MUIA_ListView_List , AsTag(MH_List(FromList, [
                MUIA_Frame, InputListFrame,
                TAG_END])),
              TAG_END])),
            TAG_END])),
            Child, AsTag(MH_HGroup([
              Child, AsTag(MH_Text(ToText, PChar(GetLocString(MSG_ROUTEPROP_TO) + ':                            '))),
              Child, AsTag(MH_Text(GetToPos, [
                MUIA_Text_Contents, AsTag(PChar(GetLocString(MSG_WAYPROP_CURRENTPOS))),
                MUIA_FixWidthTxt, AsTag(PChar(GetLocString(MSG_WAYPROP_CURRENTPOS))),
                MUIA_Background, MUII_ButtonBack,
                MUIA_Frame, ButtonFrame,
                MUIA_InputMode, MUIV_InputMode_RelVerify,
                TAG_END])),
              TAG_END])),
            Child, AsTag(MH_PopObject(ToPS, [
            MUIA_Popobject_StrObjHook, AsTag(@OpenHook),
            MUIA_PopString_Button, AsTag(MH_Text(ToBtn, [
              MUIA_Text_Contents, AsTag(PChar(GetLocString(MSG_ROUTEPROP_SEARCH))),
              MUIA_FixWidthTxt, AsTag(PChar(GetLocString(MSG_ROUTEPROP_SEARCH))),
              MUIA_Background, MUII_ButtonBack,
              MUIA_Frame, ButtonFrame,
              MUIA_InputMode, MUIV_InputMode_RelVerify,
              TAG_END])),
            MUIA_Popstring_String, AsTag(MH_String(ToEdit, [
              MUIA_Frame, StringFrame,
              TAG_END])),
            MUIA_PopObject_Object, AsTag(MH_ListView(ToLW, [
              MUIA_ListView_List , AsTag(MH_List(ToList, [
                MUIA_Frame, InputListFrame,
                TAG_END])),
              TAG_END])),
            TAG_END])),
            Child, AsTag(MH_HGroup([
              Child, AsTag(MH_Cycle(DriveDevice, [
                MUIA_Cycle_Entries, AsTag(@DriveDevices[0]),
                TAG_END])),
              Child, AsTag(MH_Cycle(RouteType, [
                MUIA_Cycle_Entries, AsTag(@RouteTypes[0]),
                TAG_END])),
              Child, AsTag(MH_Button(CalcRoute, GetLocString(MSG_ROUTEPROP_CALCROUTE))),
              TAG_END])),
            TAG_END])),
          Child, AsTag(MH_ListView(OrderList, [
            MUIA_Listview_Input, MUI_TRUE,
            MUIA_Listview_List, AsTag(MH_List(OrderListEntry, [
              MUIA_Frame, MUIV_Frame_ReadList,
              MUIA_Background, MUII_ReadListBack,
              MUIA_List_PoolThreshSize, 256,
              MUIA_ContextMenu, AsTag(PopMenu),
              TAG_DONE])),
            TAG_DONE])),
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        Child, AsTag(MH_Button(SaveButton, GetLocString(MSG_GENERAL_SAVE))),   // 'Save'
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Button(CloseButton, GetLocString(MSG_GENERAL_CLOSE))), // 'Close'
        TAG_DONE])),
      TAG_END])),
    TAG_END]);
  // set the coord to position
  ConnectHookFunction(MUIA_Listview_DoubleClick, MUIV_EveryTime, FromLW, nil, @DblFromHook, @DblFromEvent);
  ConnectHookFunction(MUIA_Listview_DoubleClick, MUIV_EveryTime, ToLW, nil, @DblToHook, @DblToEvent);
  // double click to a Order Entry
  ConnectHookFunction(MUIA_Listview_DoubleClick, MUIV_EveryTime, OrderList, nil, @DblOrderHook, @DblOrderEvent);

  ConnectHookFunction(MUIA_String_Acknowledge, MUIV_EveryTime, FromEdit, nil, @FromEnterHook, @FromEnterEvent);
  ConnectHookFunction(MUIA_String_Acknowledge, MUIV_EveryTime, ToEdit, nil, @ToEnterHook, @ToEnterEvent);

  ConnectHookFunction(MUIA_Pressed, AsTag(False), CalcRoute, nil, @CalcRouteHook, @CalcRouteEvent);

  //
  ConnectHookFunction(MUIA_Pressed, AsTag(False), GetFromPos, nil, @GetFromPosHook, @GetFromPosEvent);
  ConnectHookFunction(MUIA_Pressed, AsTag(False), GetToPos, nil, @GetToPosHook, @GetToPosEvent);
  // Popupmenu
  ConnectHookFunction(MUIA_Menuitem_Trigger, MUIV_EveryTime, WM1, nil, @ExportHTMLHook, @ExportHTMLEvent);


  // save the changes if any
  ConnectHookFunction(MUIA_Pressed, AsTag(False), SaveButton, nil, @SaveHook, @SaveEvent);
  // just close it
  ConnectHookFunction(MUIA_Window_CloseRequest, MUIV_EveryTime, RoutePropsWin, nil, @CloseHook, @CloseEvent);

  DoMethod(CloseButton, [MUIM_Notify, MUIA_Pressed, AsTag(False),
      AsTag(RoutePropsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
  DoMethod(RoutePropsWin, [MUIM_Notify, MUIA_Window_CloseRequest, AsTag(True),
      AsTag(RoutePropsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
end;


initialization
  //writeln('enter route');
  FromPos.Lat := NaN;
  FromPos.Lon := NaN;
  ToPos.Lat := NaN;
  ToPos.Lon := NaN;
  SRes := TSearchResults.Create;
  CreateRoutePropsWin;
  //writeln('leave route');
finalization
  SRes.Free;
end.
