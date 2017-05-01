unit waypointunit;
{$mode objfpc}{$H+}
interface

uses
  imagesunit, gpxunit,
  DOM, XMLRead, XMLWrite, xmlutils, jsonparser, fpjson,
  SysUtils, StrUtils, Types, Classes, Math;

var
  MarkerList: TMarkerList;
  TrackList: TTrackList;
  RouteList: TRouteList;

procedure LoadWayPoints(Filename: string);

implementation

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
  Result := now;
  if Length(StrTime) = 20 then
  begin
    Year := StrToIntDef(Copy(StrTime, 1, 4), 0);
    Month := StrToIntDef(Copy(StrTime, 6, 2), 0);
    Day := StrToIntDef(Copy(StrTime, 9, 2), 0);
    Hour := StrToIntDef(Copy(StrTime, 12, 2), 0);
    Minu := StrToIntDef(Copy(StrTime, 15, 2), 0);
    Sec := StrToIntDef(Copy(StrTime, 18, 2), 0);
    Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minu, Sec, 0);
  end;
end;

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
          if Length(str) = 20 then
          begin
            str[11] := ' ';
            str[20] := ' ';
            Marker.Time := StrToDateTimeDef(str, now());
          end;
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


initialization
  // TrackList
  TrackList := TTrackList.Create(True);
  // Waypoints
  MarkerList := TMarkerList.Create(True);
  // Routes
  RouteList := TRouteList.Create(True);
  //
  LoadWayPoints(AppDir + DirectorySeparator + DEFAULTGPX);

finalization
  TrackList.Free;
  MarkerList.Free;
  RouteList.Free;
end.
