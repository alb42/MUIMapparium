unit RoutePropsUnit;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, exec, utility, intuition, agraphics, mui, muihelper,
  prefsunit, osmhelper, MUIWrap, imagesunit, positionunit, waypointunit;

var
  RoutePropsWin: PObject_;
  OnRouteChanged: TProcedure = nil;
  CurRoute: TRoute = nil;
  CurOrder: TOrder = nil;
  OnRouteGoToPos: TProcedure = nil;


procedure ShowRouteProps(NewRoute: TRoute);
procedure NewRouteProps();

implementation

uses
  MUIMappariumlocale;

var
  RouteName, SaveButton, CloseButton, RouteCol, OrderListEntry, OrderList//,
  {WPLat, WPLon, CurPos}: PObject_;
  SaveHook, DblOrderHook{, CurPosHook}: THook;

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
begin
  CurRoute := nil;
  MH_Set(RoutePropsWin, MUIA_Window_Open, AsTag(True));
  // Set Name
  MH_Set(RouteName, MUIA_String_Contents, AsTag(PChar(GetLocString(MSG_ROUTEPROP_NEW_NAME)))); //'New Route'
end;

// Open Properties Window
procedure ShowRouteProps(NewRoute: TRoute);
var
  MUIRGB: TMUI_RGBcolor;
begin
  if Assigned(NewRoute) then
  begin
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
    // Position
    //MH_Set(WPLat, MUIA_String_Contents, AsTag(PChar(FloatToStrF(NewMarker.Position.Lat, ffFixed, 8,6))));
    //MH_Set(WPLon, MUIA_String_Contents, AsTag(PChar(FloatToStrF(NewMarker.Position.Lon, ffFixed, 8,6))));
  end;
end;

// set the position of Waypoints to the current position
function CurPosEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  //MH_Set(WPLat, MUIA_String_Contents, AsTag(PChar(FloatToStrF(MiddlePos.Lat, ffFixed, 8,6))));
  //MH_Set(WPLon, MUIA_String_Contents, AsTag(PChar(FloatToStrF(MiddlePos.Lon, ffFixed, 8,6))));
end;

// Save the Values
function SaveEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  MUIRGB: PMUI_RGBcolor;
begin
  Result := 0;
  if RouteList.IndexOf(CurRoute) >= 0 then
  begin
    CurRoute.Name := PChar(MH_Get(RouteName, MUIA_String_Contents));
    MUIRGB := PMUI_RGBcolor(MH_Get(RouteCol, MUIA_Pendisplay_RGBcolor));
    if Assigned(MUIRGB) then
      CurRoute.Color := (MUIRGB^.Red shr 8 and $ff0000) or (MUIRGB^.Green shr 16 and $FF00) or (MUIRGB^.Blue shr 24 and $FF);
    //CurMarker.Position.Lat := StrToFloatDef(PChar(MH_Get(WPLat, MUIA_String_Contents)), CurMarker.Position.Lat);
    //CurMarker.Position.Lon := StrToFloatDef(PChar(MH_Get(WPLon, MUIA_String_Contents)), CurMarker.Position.Lon);
    if Assigned(OnRouteChanged) then
      OnRouteChanged;
  end;
  MH_Set(RoutePropsWin, MUIA_Window_Open, AsTag(False));
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
        CurOrder := Order;
        OnRouteGoToPos();
        //MiddlePos.Lat := Ma.Position.Lat;
        //MiddlePos.Lon := Ma.Position.Lon;
        //MUIMapPanel.RefreshImage;
      end;
    end;
  end;
end;

procedure CreateRoutePropsWin;
begin
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
        Child, AsTag(MH_ListView(OrderList, [
          MUIA_Listview_Input, MUI_TRUE,
          MUIA_Listview_List, AsTag(MH_List(OrderListEntry, [
            MUIA_Frame, MUIV_Frame_ReadList,
            MUIA_Background, MUII_ReadListBack,
            //MUIA_ContextMenu, AsTag(WayPointMenu),
            MUIA_List_PoolThreshSize, 256,
            TAG_DONE])),
          TAG_DONE])),
      {Child, AsTag(MH_HGroup([
        MUIA_Group_Columns, 2,
        MUIA_Frame, MUIV_Frame_Group,
        MUIA_FrameTitle, AsTag(GetLocString(MSG_WAYPROP_POS)),    // 'Position'
        Child, AsTag(MH_Text(GetLocString(MSG_GENERAL_LAT))),     // 'Lat:'
        Child, AsTag(MH_String(WPLat,[
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Format, MUIV_String_Format_Right,
          MUIA_String_Accept, AsTag('0123456789.'),
          MUIA_String_Contents, AsTag('0000.000000'),
          TAG_END])),
        Child, AsTag(MH_Text(GetLocString(MSG_GENERAL_LON))),     // 'Lon:'
        Child, AsTag(MH_String(WPLon,[
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Format, MUIV_String_Format_Right,
          MUIA_String_Accept, AsTag('0123456789.'),
          MUIA_String_Contents, AsTag('0000.000000'),
          TAG_END])),
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Button(CurPos, GetLocString(MSG_WAYPROP_CURRENTPOS))),  // 'Current Position'
        TAG_END])),}
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        Child, AsTag(MH_Button(SaveButton, GetLocString(MSG_GENERAL_SAVE))),   // 'Save'
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Button(CloseButton, GetLocString(MSG_GENERAL_CLOSE))), // 'Close'
        TAG_DONE])),
      TAG_END])),
    TAG_END]);
  // set the coord to position
  //ConnectHookFunction(MUIA_Pressed, AsTag(False), CurPos, nil, @CurPosHook, @CurPosEvent);
  // double click to a Order Entry
  ConnectHookFunction(MUIA_Listview_DoubleClick, MUIV_EveryTime, OrderList, nil, @DblOrderHook, @DblOrderEvent);

  // save the changes if any
  ConnectHookFunction(MUIA_Pressed, AsTag(False), SaveButton, nil, @SaveHook, @SaveEvent);
  // just close it
  DoMethod(CloseButton, [MUIM_Notify, MUIA_Pressed, AsTag(False),
      AsTag(RoutePropsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
  DoMethod(RoutePropsWin, [MUIM_Notify, MUIA_Window_CloseRequest, AsTag(True),
      AsTag(RoutePropsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
end;


initialization
  CreateRoutePropsWin;
finalization

end.
