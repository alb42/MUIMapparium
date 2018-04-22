unit waypointunit;
{$mode objfpc}{$H+}
interface

uses
  imagesunit, gpxunit,
  DOM, XMLRead, XMLWrite, xmlutils, jsonparser, fpjson, zipper, dateutils,
  SysUtils, StrUtils, Types, Classes, Math, utility, exec, dos, asl,
  MUIWrap;


type
  TUnpacker = class
    procedure CreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure UnpackFile(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
  end;

var
  UnPacker: TUnPacker;
  MarkerList: TMarkerList;
  TrackList: TTrackList;
  RouteList: TRouteList;

procedure LoadWayPoints(Filename: string);
procedure SaveWayPoints(Filename: string);
procedure LoadKMLWayPoints(Filename: string);
procedure LoadTCXWayPoints(Filename: string);
procedure LoadFITWayPoints(Filename: string);

function LoadWayFile: boolean;
procedure SaveWayFile;

implementation

uses
  Prefsunit, fitfileunit;

var
  OldFilename: string = '';

function LoadWayFile: Boolean;
var
  fr: PFileRequester;
  OldDrawer: string;
begin
  Result := False;
{$R-}
  OldDrawer := Prefs.LoadPath;
  fr := AllocAslRequestTags(ASL_FileRequest, [
    NativeUInt(ASLFR_TitleText),      NativeUInt(PChar('Choose Waypoints File to load')),
    NativeUInt(ASLFR_InitialFile),    NativeUInt(PChar(OldFilename)),
    NativeUInt(ASLFR_InitialDrawer),  NativeUInt(PChar(OldDrawer)),
    NativeUInt(ASLFR_InitialPattern), NativeUInt(PChar('(#?.gpx|#?.kml|#?.kmz|#?.tcx|#?.fit)')),
    NativeUInt(ASLFR_DoPatterns),     LTrue,
    TAG_END]);
  if Assigned(fr) then
  begin
    //
    if AslRequestTags(fr, [TAG_END]) then
    begin
      {$if defined(VER3_0) or defined(MorphOS) or defined(Amiga68k)}
      OldFilename := IncludeTrailingPathDelimiter(string(fr^.rf_dir)) + string(fr^.rf_file);
      {$else}
      OldFilename := IncludeTrailingPathDelimiter(string(fr^.fr_drawer)) + string(fr^.fr_file);
      {$endif}
      if LowerCase(ExtractFileExt(OldFilename)) = '.gpx' then
        LoadWayPoints(OldFilename);
      if (LowerCase(ExtractFileExt(OldFilename)) = '.kml') or (LowerCase(ExtractFileExt(OldFilename)) = '.kmz') then
        LoadKMLWayPoints(OldFilename);
      if LowerCase(ExtractFileExt(OldFilename)) = '.tcx' then
        LoadTCXWayPoints(OldFilename);
      if LowerCase(ExtractFileExt(OldFilename)) = '.fit' then
        LoadFITWayPoints(OldFilename);
      Result := True;
    end;
    FreeAslRequest(fr);
  end;
end;

procedure SaveWayFile;
var
  fr: PFileRequester;
  OldDrawer: string;
begin
{$R-}
  OldDrawer := Prefs.LoadPath;
  fr := AllocAslRequestTags(ASL_FileRequest, [
    NativeUInt(ASLFR_TitleText),      NativeUInt(PChar('Choose File name Save GPX File')),
    NativeUInt(ASLFR_InitialFile),    NativeUInt(PChar(OldFilename)),
    NativeUInt(ASLFR_InitialDrawer),  NativeUInt(PChar(OldDrawer)),
    NativeUInt(ASLFR_InitialPattern), NativeUInt(PChar('#?.gpx')),
    NativeUInt(ASLFR_DoSaveMode),       LTrue,
    NativeUInt(ASLFR_DoPatterns),     LTrue,
    TAG_END]);
  if Assigned(fr) then
  begin
    //
    if AslRequestTags(fr, [TAG_END]) then
    begin
      {$if defined(VER3_0) or defined(MorphOS) or defined(Amiga68k)}
      OldFilename := IncludeTrailingPathDelimiter(string(fr^.rf_dir)) + string(fr^.rf_file);
      {$else}
      OldFilename := IncludeTrailingPathDelimiter(string(fr^.fr_drawer)) + string(fr^.fr_file);
      {$endif}
      OldFilename := ChangeFileExt(OldFilename, '.gpx');
      SaveWayPoints(OldFilename);
    end;
    FreeAslRequest(fr);
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

function XMLTimeToDateTime(StrTime: string): TDateTime;
var
  Year, Month, Day, Hour, Minu, Sec: LongInt;
begin
  Result := Now;
  if Length(StrTime) = 20 then
  begin
    Year := StrToIntDef(Copy(StrTime, 1, 4), 0);
    Month := StrToIntDef(Copy(StrTime, 6, 2), 0);
    Day := StrToIntDef(Copy(StrTime, 9, 2), 0);
    Hour := StrToIntDef(Copy(StrTime, 12, 2), 0);
    Minu := StrToIntDef(Copy(StrTime, 15, 2), 0);
    Sec := StrToIntDef(Copy(StrTime, 18, 2), 0);
    try
      Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minu, Sec, 0);
    except
      Result := Now;
    end;
  end;
end;

procedure LoadFITWayPoints(Filename: string);
var
  Track: TTrack;
  i, AddIdx, HeartIdx, CadenceIdx, PowerIdx, TempIdx: Integer;
begin
  LoadFITFile(FileName);
  if fitfileunit.TrackIdx > 0 then
  begin
    Track := TTrack.Create;
    AddIdx := 0;
    HeartIdx := -1;
    CadenceIdx := -1;
    PowerIdx := -1;
    TempIdx := -1;
    { // deactivated until file with actual content
    if not IsNan(TrkPts[0].HeartRate) then
    begin
      HeartIdx := AddIdx;
      Inc(AddIdx);
      SetLength(Track.AddFields, AddIdx);
      Track.AddFields[HeartIdx] := 'Heart rate';
    end;
    if not IsNan(TrkPts[0].Cadence) then
    begin
      CadenceIdx := AddIdx;
      Inc(AddIdx);
      SetLength(Track.AddFields, AddIdx);
      Track.AddFields[CadenceIdx] := 'Cadence';
    end;
    if not IsNan(TrkPts[0].Power) then
    begin
      PowerIdx := AddIdx;
      Inc(AddIdx);
      SetLength(Track.AddFields, AddIdx);
      Track.AddFields[PowerIdx] := 'Power';
    end;
    if not IsNan(TrkPts[0].Temp) then
    begin
      TempIdx := AddIdx;
      Inc(AddIdx);
      SetLength(Track.AddFields, AddIdx);
      Track.AddFields[TempIdx] := 'Temperature';
    end; //}

    SetLength(Track.Pts, fitfileunit.TrackIdx);
    Track.Name := ExtractFileName(ExtractFileName(FileName));
    Track.Name := Copy(Track.Name, Length(Track.Name) - 4, 4);
    for i := 0 to fitfileunit.TrackIdx - 1 do
    begin
      SetLength(Track.Pts[i].AddFields, AddIdx);
      Track.Pts[i].Position.Lat:= TrkPts[i].lat;
      Track.Pts[i].Position.Lon:= TrkPts[i].lon;
      Track.Pts[i].Elevation:= TrkPts[i].alt;
      Track.Pts[i].Time := TrkPts[i].Time;
      if HeartIdx >= 0 then
        Track.Pts[i].AddFields[HeartIdx] := TrkPts[i].HeartRate;
      if CadenceIdx >= 0 then
        Track.Pts[i].AddFields[CadenceIdx] := TrkPts[i].Cadence;
      if PowerIdx >= 0 then
        Track.Pts[i].AddFields[PowerIdx] := TrkPts[i].Power;
      if TempIdx >= 0 then
        Track.Pts[i].AddFields[TempIdx] := TrkPts[i].Temp;
    end;
    TrackList.Add(Track);
  end;
end;

//#################################
//  TCX

procedure LoadTCXWayPoints(Filename: string);

  procedure InspectCourse(CNode: TDOMNode);
  var
    TrackNode, TPtNode, TrackPt: TDOMNode;
    Track: TTrack;
    strTime: string;
    i, Idx: Integer;
  begin
    TrackNode := CNode.FindNode('Track');
    if Assigned(TrackNode) and (TrackNode.ChildNodes.Count > 0) then
    begin
      Track := TTrack.Create;
      Track.Name := GetTextNode(CNode, 'Name');
      Track.Desc := GetTextNode(CNode, 'Notes');
      if Track.Name = '' then
        Track.Name:= 'Unnamed Track';
      SetLength(Track.Pts, TrackNode.ChildNodes.Count);
      Idx := 0;
      for i := 0 to TrackNode.ChildNodes.Count - 1 do
      begin
        TrackPt := TrackNode.ChildNodes[i];
        if TrackPt.NodeName = 'Trackpoint' then
        begin
          strTime := GetTextNode(TrackPt, 'Time');
          strTime := Copy(strTime, 1, 20);
          Track.Pts[Idx].Time := XMLTimeToDateTime(strTime);
          TPtNode := TrackPt.FindNode('Position');
          if Assigned(TPtNode) then
          begin
            Track.Pts[Idx].Position.Lat := StrToFloatDef(GetTextNode(TPtNode, 'LatitudeDegrees'), 0);
            Track.Pts[Idx].Position.Lon := StrToFloatDef(GetTextNode(TPtNode, 'LongitudeDegrees'), 0);
          end;
          Track.Pts[Idx].Elevation:= StrToFloatDef(GetTextNode(TrackNode, 'AltitudeMeters'), 0);
          Inc(Idx);
        end;
      end;
      SetLength(Track.Pts, Idx);
      TrackList.Add(Track);
    end;
  end;

  procedure InspectCourses(CoursesNode: TDOMNode);
  var
    i: Integer;
  begin
    for i := 0 to CoursesNode.ChildNodes.Count - 1 do
    begin
      if CoursesNode.ChildNodes[i].NodeName = 'Course' then
        InspectCourse(CoursesNode.ChildNodes[i]);
    end;
  end;

  procedure InspectFolder(FolderNode: TDOMNode);
  var
    i: Integer;
  begin
    for i := 0 to FolderNode.ChildNodes.Count - 1 do
    begin
      if FolderNode.ChildNodes[i].NodeName = 'Folders' then
        InspectFolder(FolderNode.ChildNodes[i])
      else
      if FolderNode.ChildNodes[i].NodeName = 'Courses' then
        InspectCourses(FolderNode.ChildNodes[i]);
    end;
  end;

var
  XMLDoc: TXMLDocument;
  TrainNode: TDOMNode;
begin
  if not FileExists(Filename) then
    Exit;
  try
    ReadXMLFile(XMLDoc, Filename);
    TrainNode := XMLDoc.FindNode('TrainingCenterDatabase');
    if Assigned(TrainNode) then
      InspectFolder(TrainNode);
  finally
    XMLDoc.Free;
  end;
end;


//#################################
//  KML

var
  NXMLDoc: TXMLDocument = nil;

procedure TUnpacker.UnpackFile(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  try
    AStream.Position := 0;
    ReadXMLFile(NXMLDoc, AStream);
  finally
    AStream.Free;
    AStream := nil;
  end;
end;

procedure TUnpacker.CreateStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream := TStream(TMemoryStream.Create);
end;

type
  T3DCoord = record
    Valid: Boolean;
    Lat: Double;
    Lon: Double;
    Elevation: Double
  end;

function KMLtoCoord(a: string): T3DCoord;
var
  SL: TStringList;
begin
  Result.Valid := False;
  SL := TStringList.Create;
  try
    ExtractStrings([',',' '],[], PChar(a), SL);
    if SL.Count >= 2 then
    begin
      Result.Lon := StrToFloatDef(SL[0], NaN);
      Result.Lat := StrToFloatDef(SL[1], NaN);
      Result.Elevation := 0;
      if SL.Count > 2 then
        Result.Elevation := StrToFloatDef(SL[2], 0);
      Result.Valid := (not IsNan(Result.Lon)) and (not IsNan(Result.Lat));
    end;
  finally
    SL.Free;
  end;
end;

procedure LoadKMLWayPoints(Filename: string);

  procedure InspectDocument(BaseNode: TDOMNode); forward;

  procedure InspectFolder(AFolder: TDOMNode);
  var
    i: Integer;
    SubNode, PNode, TrackNode: TDOMNode;
    WName: string;
    Coords, WDesc: string;
    SL: TStringList;
    Marker: TMarker;
    Ele, Lon, Lat: Double;
    IdxP, IdxT, j: Integer;
    Track: TTrack;
    TrkPts: T3DCoord;
  begin
    for i := 0 to AFolder.ChildNodes.Count - 1 do
    begin
      SubNode := AFolder.ChildNodes[i];
      if SubNode.NodeName = 'Folder' then
        InspectFolder(SubNode);
      if SubNode.NodeName = 'Document' then
        InspectDocument(SubNode);
      if SubNode.NodeName = 'Placemark' then
      begin
        WName := GetTextNode(SubNode, 'name');
        WDesc := GetTextNode(SubNode, 'description');
        //
        // its waypoint
        PNode := SubNode.FindNode('Point');
        if Assigned(PNode) then
        begin
          Coords := GetTextNode(PNode, 'coordinates');
          SL := TStringList.Create;
          try
            ExtractStrings([','],[], PChar(Coords), SL);
            if SL.Count >= 2 then
            begin
              Lon := StrToFloatDef(SL[0], NaN);
              Lat := StrToFloatDef(SL[1], NaN);
              Ele := 0;
              if SL.Count > 2 then
                Ele := StrToFloatDef(SL[2], 0);
              if not IsNan(Lon) and not IsNan(Lat) then
              begin
                Marker := TMarker.Create;
                Marker.Name := WName;
                Marker.Elevation := Ele;
                Marker.Position.Lat := Lat;
                Marker.Position.Lon := Lon;
                Marker.Time := Now();
                MarkerList.Add(Marker);
              end;
            end;
          finally
            SL.Free;
          end;
          Continue;
        end;
        PNode := SubNode.FindNode('gx:Track');
        if Assigned(PNode) then
        begin
          Track := TTrack.Create;
          Track.Name := WName;
          Track.Desc := WDesc;
          IdxP := 0;
          IdxT := 0;
          SetLength(Track.Pts, 100);
          for j := 0 to PNode.ChildNodes.Count - 1 do
          begin
            TrackNode := PNode.ChildNodes[j];
            if TrackNode.NodeName = 'when' then
            begin
              Track.Pts[IdxT].Time := XMLTimeToDateTime(string(TrackNode.TextContent));
              Inc(IdxT);
            end;
            if TrackNode.NodeName = 'gx:coord' then
            begin
              TrkPts := KMLtoCoord(string(TrackNode.TextContent));
              if TrkPts.Valid then
              begin
                Track.Pts[IdxP].Position.Lat := TrkPts.Lat;
                Track.Pts[IdxP].Position.Lon := TrkPts.Lon;
                Track.Pts[IdxP].Elevation := TrkPts.Elevation;
                Inc(IdxP);
              end;
            end;
            if (IdxP > High(Track.Pts)) or (IdxT > High(Track.Pts)) then
              SetLength(Track.Pts, Length(Track.Pts) + 100);
          end;
          SetLength(Track.Pts, Max(IdxP, IdxT));
          TrackList.Add(Track);
        end;
      end;
    end;
  end;

  procedure InspectDocument(BaseNode: TDOMNode);
  var
    j: Integer;
    SubNode: TDOMNode;
  begin
    for j := 0 to BaseNode.ChildNodes.Count - 1 do
    begin
      SubNode := BaseNode.ChildNodes[j];
      if SubNode.NodeName = 'Folder' then
      begin
        InspectFolder(SubNode);
      end;
    end;
  end;

var
  XMLDoc: TXMLDocument;
  KMLNode, DocNode: TDOMNode;
  i: Integer;
  Zipper: TUnZipper;
  SL: TStringList;
begin
  if not FileExists(Filename) then
    Exit;
  try
    XMLDoc := nil;
    if LowerCase(ExtractFileExt(Filename)) = '.kml' then
    begin
      ReadXMLFile(XMLDoc, Filename);
    end;
    if LowerCase(ExtractFileExt(Filename)) = '.kmz' then
    begin
      Zipper := TUnZipper.Create;
      SL := TStringList.Create;
      try
        NXMLDoc := nil;
        Zipper.FileName := Filename;
        Zipper.OnCreateStream := @UnPacker.CreateStream;
        Zipper.OnDoneStream := @UnPacker.UnpackFile;
        SL.Text := 'doc.kml';
        Zipper.UnZipFiles(SL);
        XMLDoc := NXMLDoc;
        NXMLDoc := nil;
      finally
        Zipper.Free;
        SL.Free;
      end;
    end;
    if not Assigned(XMLDoc) then
      Exit;
    KMLNode := XMLDoc.FindNode('kml');
    if Assigned(KMLNode) then
    begin
      for i := 0 to KMLNode.ChildNodes.Count - 1 do
      begin
        DocNode := KMLNode.ChildNodes[i];
        if DocNode.NodeName = 'Document' then
        begin
          InspectDocument(DocNode);
        end;
        if DocNode.NodeName = 'Folder' then
        begin
          InspectFolder(DocNode);
        end;
      end;
    end;
  finally
    if Assigned(XMLDoc) then
      XMLDoc.Free;
  end;
end;




//########################################
//  GPX

procedure LoadWayPoints(Filename: string);
var
  XMLDoc: TXMLDocument;
  GPXNode, SubNode, SegNode, PtNode, ExtNode, OrderNode, OrdersNode: TDOMNode;
  i, j: Integer;
  str: string;
  Marker: TMarker;
  Track: TTrack;
  Idx, d: Integer;
  Route: TRoute;
  Order: TOrder;
begin
  if not FileExists(Filename) then
    Exit;
  try
    ReadXMLFile(XMLDoc, Filename);
    GPXNode := XMLDoc.FindNode('gpx');
    if Assigned(GPXNode) then
    begin
      for i := 0 to GPXNode.ChildNodes.Count - 1 do
      begin
        SubNode := GPXNode.ChildNodes[i];

      //##### Waypoint
        if SubNode.NodeName = 'wpt' then
        begin
          Marker := TMarker.Create;
          str := string(SubNode.Attributes.GetNamedItem('lat').NodeValue);
          Marker.Position.Lat := StrToFloatDef(str, 0);
          str := string(SubNode.Attributes.GetNamedItem('lon').NodeValue);
          Marker.Position.Lon := StrToFloatDef(str, 0);
          Marker.Name := GetTextNode(SubNode, 'name');
          Marker.Elevation := StrToFloatDef(GetTextNode(SubNode, 'ele'), Nan);
          Marker.symbol := GetTextNode(SubNode, 'sym');
          str := GetTextNode(SubNode, 'time');
          Marker.Time := XMLTimeToDateTime(str);
          ExtNode := SubNode.FindNode('extensions');
          if Assigned(ExtNode) then
            Marker.Visible := StrToBoolDef(GetTextNode(ExtNode, 'visible'), True);
          MarkerList.Add(Marker);
        end;
      //##### Route
        if SubNode.NodeName = 'rte' then
        begin
          Route := TRoute.Create;
          Route.Name := GetTextNode(SubNode, 'name');
          if Route.Name = '' then
            Route.Name := 'Unnamed Route';
          Route.Desc := GetTextNode(SubNode, 'desc');
          SetLength(Route.Pts, SubNode.ChildNodes.Count);
          Idx := 0;
          for j := 0 to SubNode.ChildNodes.Count - 1 do
          begin
            PtNode := SubNode.ChildNodes[j];
            if PtNode.NodeName = 'rtept' then
            begin
              str := string(PtNode.Attributes.GetNamedItem('lat').NodeValue);
              Route.Pts[Idx].Position.Lat := StrToFloatDef(str, 0);
              str := string(PtNode.Attributes.GetNamedItem('lon').NodeValue);
              Route.Pts[Idx].Position.Lon := StrToFloatDef(str, 0);
              Inc(Idx);
            end;
          end;
          SetLength(Route.Pts, Idx);
          ExtNode := SubNode.FindNode('extensions');
          if Assigned(ExtNode) then
          begin
            Route.Visible := StrToBoolDef(GetTextNode(ExtNode, 'visible'), True);
            Route.Color := StrToIntDef('$' + GetTextNode(ExtNode, 'color'), Route.Color);
            Route.Distance := StrToFloatDef(GetTextNode(ExtNode, 'distance'), 0);
            Route.TravelTime := StrToIntDef(GetTextNode(ExtNode, 'traveltime'), 0);
            OrderNode := nil;
            OrdersNode := ExtNode.FindNode('orders');
            for j := 0 to OrdersNode.ChildNodes.Count - 1 do
            begin
              OrderNode := OrdersNode.ChildNodes[j];
              if OrderNode.NodeName = 'order' then
              begin
                Order := TOrder.Create;
                Order.Order := GetTextNode(OrderNode, 'text');
                Idx := 0;
                SetLength(Order.Positions, OrderNode.ChildNodes.Count);
                for d := 0 to OrderNode.ChildNodes.Count - 1 do
                begin
                  PtNode := OrderNode.ChildNodes[d];
                  if PtNode.NodeName = 'orderpt' then
                  begin
                    str := string(PtNode.Attributes.GetNamedItem('lat').NodeValue);
                    Order.Positions[Idx].Lat := StrToFloatDef(str, 0);
                    str := string(PtNode.Attributes.GetNamedItem('lon').NodeValue);
                    Order.Positions[Idx].Lon := StrToFloatDef(str, 0);
                    Inc(Idx);
                  end;
                end;
                SetLength(Order.Positions, Idx);
                Route.Orders.Add(Order);
              end;
            end;
          end;
          RouteList.Add(Route);
        end;
      //##### Track
        if SubNode.NodeName = 'trk' then
        begin
          Track := TTrack.Create;
          Track.Name := GetTextNode(SubNode, 'name');
          if Track.Name = '' then
            Track.Name := 'Unnamed Track';
          Track.Desc := GetTextNode(SubNode, 'desc');
          SegNode := SubNode.FindNode('trkseg');
          Idx := 0;
          if Assigned(SegNode) then
          begin
            SetLength(Track.Pts, SegNode.ChildNodes.Count);
            for j := 0 to SegNode.ChildNodes.Count - 1 do
            begin
              PtNode := SegNode.ChildNodes[j];
              if PtNode.NodeName = 'trkpt' then
              begin
                str := string(PtNode.Attributes.GetNamedItem('lat').NodeValue);
                Track.Pts[Idx].Position.Lat := StrToFloatDef(str, 0);
                str := string(PtNode.Attributes.GetNamedItem('lon').NodeValue);
                Track.Pts[Idx].Position.Lon := StrToFloatDef(str, 0);
                Track.Pts[Idx].Elevation := StrToFloatDef(GetTextNode(PtNode, 'ele'), Nan);
                Track.Pts[Idx].Time := XMLTimeToDateTime(GetTextNode(PtNode, 'time'));
                Inc(Idx);
              end;
            end;
          end;
          ExtNode := SubNode.FindNode('extensions');
          if Assigned(ExtNode) then
          begin
            Track.Visible := StrToBoolDef(GetTextNode(ExtNode, 'visible'), True);
            Track.Color := StrToIntDef('$' + GetTextNode(ExtNode, 'color'), Track.Color);
          end;
          SetLength(Track.Pts, Idx);
          TrackList.Add(Track);
        end;
      end;
    end;
  finally
    XMLDoc.Free;
  end;
end;

procedure SaveWayPoints(Filename: string);
var
  XMLDoc: TXMLDocument;
  SStream: TStringStream;
  GPXNode: TDOMNode;
  WptNode, TextNode, TrkNode, SegNode, PtNode, RteNode, ExtNode,
    OrderNode, OrdersNode: TDOMElement;
  i, j, d: Integer;
begin
  SStream := TStringStream.Create(GPXTEMPLATE);
  try
    ReadXMLFile(XMLDoc, SStream);
    GPXNode := XMLDoc.FindNode('gpx');
    if Assigned(GPXNode) then
    begin
      for i := 0 to MarkerList.Count - 1 do
      begin
        WptNode := XMLDoc.CreateElement('wpt');
        WptNode.SetAttribute('lat', WideString(FloatToStrF(MarkerList[i].Position.Lat, ffFixed,10,6)));
        WptNode.SetAttribute('lon', WideString(FloatToStrF(MarkerList[i].Position.Lon, ffFixed,10,6)));
        GPXNode.AppendChild(WptNode);
        // Name
        TextNode := XMLDoc.CreateElement('name');
        WptNode.AppendChild(TextNode);
        TextNode.TextContent := WideString(MarkerList[i].Name);
        // Time
        TextNode := XMLDoc.CreateElement('time');
        WptNode.AppendChild(TextNode);
        TextNode.TextContent := WideString(FormatDateTime('yyyy-mm-dd', MarkerList[i].Time) + 'T' + FormatDateTime('hh:nn:ss', MarkerList[i].Time) + 'Z');
        if MarkerList[i].symbol <> '' then
        begin
          TextNode := XMLDoc.CreateElement('sym');
          WptNode.AppendChild(TextNode);
          TextNode.TextContent := WideString(MarkerList[i].symbol);
        end;
        if not IsNan(MarkerList[i].Elevation) then
        begin
          TextNode := XMLDoc.CreateElement('ele');
          WptNode.AppendChild(TextNode);
          TextNode.TextContent := WideString(FloatToStrF(MarkerList[i].Elevation, ffFixed,8,1));
        end;
        ExtNode := XMLDoc.CreateElement('extensions');
        WptNode.AppendChild(ExtNode);
        TextNode := XMLDoc.CreateElement('visible');
        ExtNode.AppendChild(TextNode);
        TextNode.TextContent := WideString(BoolToStr(MarkerList[i].Visible, True));
        GPXNode.AppendChild(WptNode);
      end;
      for i := 0 to RouteList.Count -1 do
      begin
        RteNode := XMLDoc.CreateElement('rte');
        //
        TextNode := XMLDoc.CreateElement('name');
        RteNode.AppendChild(TextNode);
        TextNode.TextContent := WideString(RouteList[i].Name);
        // Desc
        TextNode := XMLDoc.CreateElement('desc');
        RteNode.AppendChild(TextNode);
        TextNode.TextContent := WideString(RouteList[i].Desc);
        //
        for j := 0 to High(RouteList[i].Pts) do
        begin
          PtNode := XMLDoc.CreateElement('rtept');
          PtNode.SetAttribute('lat', WideString(FloatToStrF(RouteList[i].Pts[j].Position.Lat, ffFixed,10,6)));
          PtNode.SetAttribute('lon', WideString(FloatToStrF(RouteList[i].Pts[j].Position.Lon, ffFixed,10,6)));
          RteNode.AppendChild(PtNode);
        end;
        ExtNode := XMLDoc.CreateElement('extensions');
        RteNode.AppendChild(ExtNode);
        //
        TextNode := XMLDoc.CreateElement('visible');
        TextNode.TextContent := WideString(BoolToStr(RouteList[i].Visible, True));
        ExtNode.AppendChild(TextNode);
        //
        // Color
        TextNode := XMLDoc.CreateElement('color');
        TextNode.TextContent := WideString(IntToHex(RouteList[i].Color, 6));
        ExtNode.AppendChild(TextNode);
        //
        TextNode := XMLDoc.CreateElement('distance');
        TextNode.TextContent := WideString(FloatToStrF(RouteList[i].Distance,ffFixed, 8,2));
        ExtNode.AppendChild(TextNode);
        //
        TextNode := XMLDoc.CreateElement('traveltime');
        TextNode.TextContent := WideString(IntToStr(RouteList[i].TravelTime));
        ExtNode.AppendChild(TextNode);
        //
        OrdersNode := XMLDoc.CreateElement('orders');
        ExtNode.AppendChild(OrdersNode);
        for j := 0 to RouteList[i].Orders.Count - 1 do
        begin
          OrderNode := XMLDoc.CreateElement('order');
          TextNode := XMLDoc.CreateElement('text');
          TextNode.TextContent := WideString(RouteList[i].Orders[j].Order);
          OrderNode.AppendChild(TextNode);
          for d := 0 to High(RouteList[i].Orders[j].Positions) do
          begin
            PtNode := XMLDoc.CreateElement('orderpt');
            PtNode.SetAttribute('lat', WideString(FloatToStrF(RouteList[i].Orders[j].Positions[d].Lat, ffFixed,10,6)));
            PtNode.SetAttribute('lon', WideString(FloatToStrF(RouteList[i].Orders[j].Positions[d].Lon, ffFixed,10,6)));
            OrderNode.AppendChild(PtNode);
          end;
          OrdersNode.AppendChild(OrderNode);
        end;
        GPXNode.AppendChild(RteNode);
      end;
      for i := 0 to TrackList.Count - 1 do
      begin
        TrkNode := XMLDoc.CreateElement('trk');
        // Name
        TextNode := XMLDoc.CreateElement('name');
        TrkNode.AppendChild(TextNode);
        TextNode.TextContent := WideString(TrackList[i].Name);
        // Desc
        TextNode := XMLDoc.CreateElement('desc');
        TrkNode.AppendChild(TextNode);
        TextNode.TextContent := WideString(TrackList[i].Desc);
        // sequence
        SegNode := XMLDoc.CreateElement('trkseg');
        for j := 0 to High(TrackList[i].Pts) do
        begin
          PtNode := XMLDoc.CreateElement('trkpt');
          PtNode.SetAttribute('lat', WideString(FloatToStrF(TrackList[i].Pts[j].Position.Lat, ffFixed,10,6)));
          PtNode.SetAttribute('lon', WideString(FloatToStrF(TrackList[i].Pts[j].Position.Lon, ffFixed,10,6)));
          if not IsNan(TrackList[i].Pts[j].Elevation) then
          begin
            TextNode := XMLDoc.CreateElement('ele');
            PtNode.AppendChild(TextNode);
            TextNode.TextContent := WideString(FloatToStrF(TrackList[i].Pts[j].Elevation, ffFixed,10,2));
          end;
          // Time
          TextNode := XMLDoc.CreateElement('time');
          PtNode.AppendChild(TextNode);
          TextNode.TextContent := WideString(FormatDateTime('yyyy-mm-dd', TrackList[i].Pts[j].Time) + 'T' + FormatDateTime('hh:nn:ss', TrackList[i].Pts[j].Time) + 'Z');
          SegNode.AppendChild(PtNode);
        end;
        //
        ExtNode := XMLDoc.CreateElement('extensions');
        TrkNode.AppendChild(ExtNode);
        // visibility
        TextNode := XMLDoc.CreateElement('visible');
        TextNode.TextContent := WideString(BoolToStr(TrackList[i].Visible, True));
        ExtNode.AppendChild(TextNode);
        // Color
        TextNode := XMLDoc.CreateElement('color');
        TextNode.TextContent := WideString(IntToHex(TrackList[i].Color, 6));
        ExtNode.AppendChild(TextNode);
        //
        TrkNode.AppendChild(SegNode);
        GPXNode.AppendChild(TrkNode);
      end;
      WriteXMLFile(XMLDoc, Filename);
    end;
  finally
    XMLDoc.Free;
    SStream.Free;
  end;
end;


initialization
  Unpacker := TUnpacker.Create;
  // TrackList
  TrackList := TTrackList.Create(True);
  // Waypoints
  MarkerList := TMarkerList.Create(True);
  // Routes
  RouteList := TRouteList.Create(True);
  // load defaults.gpx
  LoadWayPoints(AppDir + DirectorySeparator + DEFAULTGPX);
finalization
  // save defaults.gpx
  SaveWayPoints(AppDir + DirectorySeparator + DEFAULTGPX);
  //
  TrackList.Free;
  MarkerList.Free;
  RouteList.Free;
  UnPacker.Free;
end.
