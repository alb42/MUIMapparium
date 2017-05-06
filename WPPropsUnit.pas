unit WPPropsUnit;
{$mode objfpc}{$H+}
interface

uses
  {$if defined(Amiga68k) or defined(MorphOS)}
    amigalib,
  {$endif}
  SysUtils, exec, utility, intuition, agraphics, mui, muihelper,
  prefsunit, osmhelper, MUIWrap, imagesunit, positionunit, waypointunit;

var
  WPPropsWin: PObject_;
  OnWPChanged: TProcedure = nil;

procedure ShowWPProps(NewMarker: TMarker);

implementation

var
  WPName, SaveButton, CloseButton,
  WPLat, WPLon, CurPos: PObject_;
  CurMarker: TMarker = nil;
  SaveHook, CurPosHook: THook;

// Open Properties Window
procedure ShowWPProps(NewMarker: TMarker);
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
begin
  Result := 0;
  if MarkerList.IndexOf(CurMarker) >= 0 then
  begin
    CurMarker.Name := PChar(MH_Get(WPName, MUIA_String_Contents));
    CurMarker.Position.Lat := StrToFloatDef(PChar(MH_Get(WPLat, MUIA_String_Contents)), CurMarker.Position.Lat);
    CurMarker.Position.Lon := StrToFloatDef(PChar(MH_Get(WPLon, MUIA_String_Contents)), CurMarker.Position.Lon);
    if Assigned(OnWPChanged) then
      OnWPChanged;
  end;
  MH_Set(WPPropsWin, MUIA_Window_Open, AsTag(False));
end;

procedure CreateWPPropsWin;
begin
  WPPropsWin := MH_Window([
    MUIA_Window_Title,     AsTag('WayPoint Properties'),
    MUIA_Window_ID,        AsTag(MAKE_ID('W','A','Y','P')),
    WindowContents, AsTag(MH_VGroup([
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        MUIA_FrameTitle, AsTag('WayPoint Title'),
        Child, AsTag(MH_String(WPName, [
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Format, MUIV_String_Format_Left,
          MUIA_String_Contents, AsTag('________________________'),
          TAG_END])),
        TAG_END])),
      Child, AsTag(MH_HGroup([
        MUIA_Group_Columns, 2,
        MUIA_Frame, MUIV_Frame_Group,
        MUIA_FrameTitle, AsTag('Position'),
        Child, AsTag(MH_Text('Lat:')),
        Child, AsTag(MH_String(WPLat,[
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Format, MUIV_String_Format_Right,
          MUIA_String_Accept, AsTag('0123456789.'),
          MUIA_String_Contents, AsTag('0000.000000'),
          TAG_END])),
        Child, AsTag(MH_Text('Lon:')),
        Child, AsTag(MH_String(WPLon,[
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Format, MUIV_String_Format_Right,
          MUIA_String_Accept, AsTag('0123456789.'),
          MUIA_String_Contents, AsTag('0000.000000'),
          TAG_END])),
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Button(CurPos, 'Current Position')),
        TAG_END])),
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        Child, AsTag(MH_Button(SaveButton, 'Save')),
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Button(CloseButton, 'Close')),
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
