unit TrackPropsUnit;
{$mode objfpc}{$H+}
interface

uses
  {$if defined(Amiga68k) or defined(MorphOS)}
    amigalib,
  {$endif}
  SysUtils, exec, utility, intuition, agraphics, mui, muihelper,
  prefsunit, osmhelper, MUIWrap, imagesunit, positionunit, waypointunit;

var
  TrackPropsWin: PObject_;
  OnTrackChanged: TProcedure = nil;

procedure ShowTrackProps(NewTrack: TTrack);

implementation

var
  TrackName, SaveButton, CloseButton: PObject_;
  CurTrack: TTrack = nil;
  SaveHook: THook;

// Open Properties Window
procedure ShowTrackProps(NewTrack: TTrack);
begin
  if Assigned(NewTrack) then
  begin
    CurTrack := NewTrack;
    MH_Set(TrackPropsWin, MUIA_Window_Open, AsTag(True));
    // Set Name
    MH_Set(TrackName, MUIA_String_Contents, AsTag(PChar(NewTrack.Name)));
  end;
end;

// Save the Values
function SaveEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  if TrackList.IndexOf(CurTrack) >= 0 then
  begin
    CurTrack.Name := PChar(MH_Get(TrackName, MUIA_String_Contents));
    if Assigned(OnTrackChanged) then
      OnTrackChanged;
  end;
  MH_Set(TrackPropsWin, MUIA_Window_Open, AsTag(False));
end;

procedure CreateTrackPropsWin;
begin
  TrackPropsWin := MH_Window([
    MUIA_Window_Title,     AsTag('Track Properties'),
    MUIA_Window_ID,        AsTag(MAKE_ID('T','R','A','P')),
    WindowContents, AsTag(MH_VGroup([
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        MUIA_FrameTitle, AsTag('Track Title'),
        Child, AsTag(MH_String(TrackName, [
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Format, MUIV_String_Format_Left,
          MUIA_String_Contents, AsTag('________________________'),
          TAG_END])),
        TAG_END])),
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        Child, AsTag(MH_Button(SaveButton, 'Save')),
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Button(CloseButton, 'Close')),
        TAG_DONE])),
      TAG_END])),
    TAG_END]);
  // save the changes if any
  ConnectHookFunction(MUIA_Pressed, AsTag(False), SaveButton, nil, @SaveHook, @SaveEvent);
  // just close it
  DoMethod(CloseButton, [MUIM_Notify, MUIA_Pressed, AsTag(False),
      AsTag(TrackPropsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
  DoMethod(TrackPropsWin, [MUIM_Notify, MUIA_Window_CloseRequest, AsTag(True),
      AsTag(TrackPropsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
end;


initialization
  CreateTrackPropsWin;
finalization

end.
