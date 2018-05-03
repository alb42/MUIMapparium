unit WPPropsUnit;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, exec, utility, intuition, agraphics, mui, muihelper,
  prefsunit, osmhelper, MUIWrap, imagesunit, positionunit, waypointunit;

var
  WPPropsWin: PObject_;
  OnWPChanged: TProcedure = nil;

procedure ShowWPProps(NewMarker: TMarker);

implementation

uses
  MUIMappariumlocale;

var
  WPName, SaveButton, CloseButton,
  WPLat, WPLon, CurPos, MarkerCol: PObject_;
  CurMarker: TMarker = nil;
  SaveHook, CurPosHook: THook;

// Open Properties Window
procedure ShowWPProps(NewMarker: TMarker);
var
  MUIRGB: TMUI_RGBcolor;
begin
  if Assigned(NewMarker) then
  begin
    CurMarker := NewMarker;
    MH_Set(WPPropsWin, MUIA_Window_Open, AsTag(True));
    // Set Name
    MH_Set(WPName, MUIA_String_Contents, AsTag(PChar(NewMarker.Name)));
    // Position
    MH_Set(WPLat, MUIA_String_Contents, AsTag(PChar(FloatToStrF(NewMarker.Position.Lat, ffFixed, 8,6))));
    MH_Set(WPLon, MUIA_String_Contents, AsTag(PChar(FloatToStrF(NewMarker.Position.Lon, ffFixed, 8,6))));
    // Color
    // Set Color
    MUIRGB.Red := NewMarker.Color shl 8;
    MUIRGB.Green := NewMarker.Color shl 16;
    MUIRGB.Blue := NewMarker.Color shl 24;
    MH_Set(MarkerCol, MUIA_Pendisplay_RGBcolor, AsTag(@MUIRGB));
  end;
end;

// set the position of Waypoints to the current position
function CurPosEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  MH_Set(WPLat, MUIA_String_Contents, AsTag(PChar(FloatToStrF(MiddlePos.Lat, ffFixed, 8,6))));
  MH_Set(WPLon, MUIA_String_Contents, AsTag(PChar(FloatToStrF(MiddlePos.Lon, ffFixed, 8,6))));
end;

// Save the Values
function SaveEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  MUIRGB: PMUI_RGBcolor;
begin
  Result := 0;
  if MarkerList.IndexOf(CurMarker) >= 0 then
  begin
    CurMarker.Name := PChar(MH_Get(WPName, MUIA_String_Contents));
    CurMarker.Position.Lat := StrToFloatDef(PChar(MH_Get(WPLat, MUIA_String_Contents)), CurMarker.Position.Lat);
    CurMarker.Position.Lon := StrToFloatDef(PChar(MH_Get(WPLon, MUIA_String_Contents)), CurMarker.Position.Lon);
    //
    MUIRGB := PMUI_RGBcolor(MH_Get(MarkerCol, MUIA_Pendisplay_RGBcolor));
    if Assigned(MUIRGB) then
      CurMarker.Color := (MUIRGB^.Red shr 8 and $ff0000) or (MUIRGB^.Green shr 16 and $FF00) or (MUIRGB^.Blue shr 24 and $FF);
    if Assigned(OnWPChanged) then
      OnWPChanged;
  end;
  MH_Set(WPPropsWin, MUIA_Window_Open, AsTag(False));
end;

procedure CreateWPPropsWin;
begin
  WPPropsWin := MH_Window([
    MUIA_Window_Title,     AsTag(GetLocString(MSG_WAYPROP_TITLE)), // 'WayPoint Properties'
    MUIA_Window_ID,        AsTag(MAKE_ID('W','A','Y','P')),
    MUIA_HelpNode,         AsTag('WayWin'),
    WindowContents, AsTag(MH_VGroup([
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        MUIA_FrameTitle, AsTag(GetLocString(MSG_WAYPROP_NAME)),    // 'WayPoint Title'
        Child, AsTag(MH_String(WPName, [
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Format, MUIV_String_Format_Left,
          MUIA_String_Contents, AsTag('________________________'),
          TAG_END])),
          Child, AsTag(MH_Poppen(MarkerCol, [MUIA_Weight, 20, TAG_END])),
        TAG_END])),
      Child, AsTag(MH_HGroup([
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
        TAG_END])),
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        Child, AsTag(MH_Button(SaveButton, GetLocString(MSG_GENERAL_SAVE))),   // 'Save'
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Button(CloseButton, GetLocString(MSG_GENERAL_CLOSE))), // 'Close'
        TAG_DONE])),
      TAG_END])),
    TAG_END]);
  // set the coord to position
  ConnectHookFunction(MUIA_Pressed, AsTag(False), CurPos, nil, @CurPosHook, @CurPosEvent);
  // save the changes if any
  ConnectHookFunction(MUIA_Pressed, AsTag(False), SaveButton, nil, @SaveHook, @SaveEvent);
  // just close it
  DoMethod(CloseButton, [MUIM_Notify, MUIA_Pressed, AsTag(False),
      AsTag(WPPropsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
  DoMethod(WPPropsWin, [MUIM_Notify, MUIA_Window_CloseRequest, AsTag(True),
      AsTag(WPPropsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
end;


initialization
  CreateWPPropsWin;
finalization

end.
