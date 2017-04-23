unit PrefsWinUnit;
{$mode objfpc}{$H+}

interface

uses
  {$if defined(Amiga68k) or defined(MorphOS)}
    amigalib,
  {$endif}
  SysUtils, exec, utility, intuition, agraphics, mui, muihelper,
  prefsunit, osmhelper, MUIWrap, imagesunit;

type
  TProcedure = procedure;

var
  PrefsWin: PObject_ = nil;
  CountButton, ClearButton, FilesLabel, ToLevel,
  SaveButton, CancelButton, MarkerType, MarkerSize,
  LangSel, TilesHD: PObject_;
  Bytes, Counter: Int64;

  OnUpdatePrefs: TProcedure;

procedure OpenPrefsWindow;

implementation

var
  CountHook, ClearHook, SaveHook: THook;

procedure OpenPrefsWindow;
begin
  //Marker
  MH_Set(MarkerType, MUIA_Cycle_Active, Prefs.MiddleMarker);
  MH_Set(MarkerSize, MUIA_String_Integer, Prefs.MarkerSize);
  MH_Set(LangSel, MUIA_String_Contents, AsTag(PChar(Prefs.SearchLang)));
  MH_Set(TilesHD, MUIA_String_Integer, Prefs.MaxTiles);
  //
  MH_Set(PrefsWin, MUIA_Window_Open, AsTag(True));
end;

function SaveEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  // Save Prefs
  Prefs.MiddleMarker := MH_Get(MarkerType, MUIA_Cycle_Active);
  Prefs.MarkerSize := MH_Get(MarkerSize, MUIA_String_Integer);
  Prefs.SearchLang := PChar(MH_Get(LangSel, MUIA_String_Contents));
  Prefs.MaxTiles := MH_Get(TilesHD, MUIA_String_Integer);
  //
  if Assigned(OnUpdatePrefs) then
    OnUpdatePrefs();
  //
  MH_Set(PrefsWin, MUIA_Window_Open, AsTag(False));
end;

function ScaleBytes(a: Int64): string;
begin
  Result := IntToStr(a) + ' bytes';
  if a > 1024 then
  begin
    if a > 1024 * 1024 then
    begin
      Result := FloatToStrF(a/1024/1024, ffFixed, 8,1) + ' MiB';
    end else
    begin
      Result := FloatToStrF(a/1024, ffFixed, 8,1) + ' KiB';
    end;
  end;
end;

function CountButtonEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Info: TRawByteSearchRec;
  StartTime: Int64;
begin
  Result := 0;
  Bytes := 0;
  Counter := 0;
  StartTime := GetTickCount64;
  if FindFirst(ExtractFileDir(ParamStr(0)) + DirectorySeparator + BASEFILE + '*.png', faAnyFile, Info) = 0 then
  begin
    repeat
      Inc(Counter);
      Inc(Bytes, Info.Size);
      if GetTickCount64 - StartTime > 200 then
      begin
        MH_Set(FilesLabel, MUIA_Text_Contents, AsTag(PChar('Cached data: ' + ScaleBytes(Bytes) + ' in ' +  IntToStr(Counter) + ' files.')));
        StartTime := GetTickCount64;
      end;
    until FindNext(Info) <> 0;
  end;
  FindClose(Info);
  MH_Set(FilesLabel, MUIA_Text_Contents, AsTag(PChar('Cached data: ' + ScaleBytes(Bytes) + ' in ' +  IntToStr(Counter) + ' files.')));
end;

function ClearButtonEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  Info: TRawByteSearchRec;
  StartTime: Int64;
  Limit: Integer;
  FileName: RawByteString;
  P1: SizeInt;
  ZoomLevel: LongInt;
  DeletedFiles, IgnoredFiles: QWord;
begin
  Result := 0;
  StartTime := GetTickCount64;
  DeletedFiles := 0;
  IgnoredFiles := 0;
  Limit := MH_Get(ToLevel, MUIA_String_Integer);
  //
  if FindFirst(ExtractFileDir(ParamStr(0)) + DirectorySeparator + BASEFILE + '*.png', faAnyFile, Info) = 0 then
  begin
    repeat
      FileName := Info.Name;
      Delete(FileName, 1, 4); // remove 'osm_'
      P1 := Pos('_', FileName);
      ZoomLevel := StrToIntDef(Copy(Filename, 1, P1 - 1), -1);
      if ZoomLevel >= Limit then
      begin
        DeleteFile(ExtractFileDir(ParamStr(0)) + DirectorySeparator + 'data' + DirectorySeparator + Info.Name);
        Inc(DeletedFiles);
      end else
      begin
        Inc(IgnoredFiles);
      end;
      if GetTickCount64 - StartTime > 200 then
      begin
        MH_Set(FilesLabel, MUIA_Text_Contents, AsTag(PChar(IntToStr(DeletedFiles) + ' deleted, ' + IntToStr(IgnoredFiles) + ' kept.')));
        StartTime := GetTickCount64;
      end;
    until FindNext(Info) <> 0;
  end;
  FindClose(Info);
  CountButtonEvent(Hook, nil, nil);
end;


const
  MarkerTypes: array[0..4] of PChar =
    ('None'#0, 'Point'#0, 'Cross'#0, 'Lines'#0, nil);

procedure CreatePrefsWin;
begin
  PrefsWin := MH_Window([
    MUIA_Window_Title,     AsTag('Preferences'),
    MUIA_Window_ID,        AsTag(MAKE_ID('M','P','R','E')),
    WindowContents, AsTag(MH_VGroup([
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        MUIA_FrameTitle, AsTag('Middle position marker'),
        Child, AsTag(MH_Text('Type/Size')),
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Cycle(MarkerType, [
          MUIA_Cycle_Entries, AsTag(@MarkerTypes),
          TAG_DONE])),
        Child, AsTag(MH_String(MarkerSize, [
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Format, MUIV_String_Format_Right,
          MUIA_String_Accept, AsTag('0123456789'),
          MUIA_String_Integer, 1,
          MUIA_String_MaxLen, 2,
          TAG_DONE])),
        TAG_DONE])),
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        MUIA_FrameTitle, AsTag('Language'),
        Child, AsTag(MH_Text('Default search result language')),
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_String(LangSel, [
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Contents, AsTag('en'),
          TAG_DONE])),
        TAG_DONE])),
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        MUIA_FrameTitle, AsTag('Memory'),
        Child, AsTag(MH_Text('Max Tiles in Memory')),
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_String(TilesHD, [
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Format, MUIV_String_Format_Right,
          MUIA_String_Accept, AsTag('0123456789'),
          MUIA_String_Integer, 20,
          TAG_DONE])),
        TAG_DONE])),
      Child, AsTag(MH_VGroup([
        MUIA_Frame, MUIV_Frame_Group,
        MUIA_FrameTitle, AsTag('Files on hard disk'),
        Child, AsTag(MH_HGroup([
          Child, AsTag(MH_Button(CountButton, 'Count')),
          Child, AsTag(MH_Button(ClearButton, 'Clear')),
          Child, AsTag(MH_Text('to Zoom')),
          Child, AsTag(MH_String(ToLevel, [
            MUIA_Frame, MUIV_Frame_String,
            MUIA_String_Format, MUIV_String_Format_Right,
            MUIA_String_Accept, AsTag('0123456789'),
            MUIA_String_Integer, 7,
            TAG_DONE])),
          TAG_DONE])),
        Child, AsTag(MH_Text(FilesLabel, 'Cached Data:')),
        TAG_DONE])),
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        Child, AsTag(MH_Button(SaveButton, 'Save')),
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Button(CancelButton, 'Cancel')),
        TAG_DONE])),
      TAG_DONE])),
    TAG_DONE]);

  ConnectHookFunction(MUIA_Pressed, AsTag(False), CountButton, nil, @CountHook, @CountButtonEvent);
  ConnectHookFunction(MUIA_Pressed, AsTag(False), ClearButton, nil, @ClearHook, @ClearButtonEvent);

  ConnectHookFunction(MUIA_Pressed, AsTag(False), SaveButton, nil, @SaveHook, @SaveEvent);
  DoMethod(CancelButton, [MUIM_Notify, MUIA_Pressed, AsTag(False),
      AsTag(PrefsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
end;

initialization
  CreatePrefsWin;

finalization

end.
