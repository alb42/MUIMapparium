unit StatisticsUnit;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, exec, utility, intuition, agraphics, mui, muihelper,
  prefsunit, osmhelper, MUIWrap, imagesunit,
  MUIMappariumlocale;

var

  StatWin, StatList, StatListEntry: PObject_;

  procedure SetNumTiles(AValue: Integer);
  procedure SetTilesToLoad(AValue: Integer);
  procedure AddDownloaded(ABytes, ATimes: Int64);
  procedure SetNumFromNet(AValue: Integer);
  procedure SetNumFromHD(AValue: Integer);
  procedure RedrawList(Num: Integer = MUIV_List_Redraw_All);

var
  NeedReDraw: Boolean = False;

implementation

var
  DispHook: THook;
  TimesOffset: Int64 = 0;
  BytesOffset: Int64 = 0;
  NumFromNetOffset: Int64 = 0;
  NumFromHDOffset: Int64 = 0;

const
  ColumnTitles: array[0..3] of PChar = ('', 'This Session'#0, 'Overall'#0, nil);
  STAT_TILESMEM  = 0;
  STAT_TILESNET  = 1;
  STAT_TILESHD   = 2;
  STAT_TILESLOAD = 3;
  STAT_DOWNLOAD  = 4;
  STAT_AVERAGE   = 5;
  RowTitles: array[0..6] of PChar = ('Tiles in Memory'#0, 'Tiles loaded from Net'#0, 'Tiles loaded from HD'#0, 'Tiles to load'#0, 'Bytes downloaded'#0, 'Average Speed'#0, nil);

type
  TStatEntry = record
    ID: Integer;
    SessionNumber: Int64;
    OverallNumber: Int64;
    SessionText: string;
    OverAllText: string;
  end;
  PStatEntry = ^TStatEntry;
var
  Entries: array[0..5] of TStatEntry;


procedure RedrawList(Num: Integer = MUIV_List_Redraw_All);
begin
  if Assigned(StatWin) and NeedReDraw then
  begin
    if boolean(MH_Get(StatWin, MUIA_Window_Open)) then
    begin
      DoMethod(StatListEntry, [MUIM_List_Redraw, Num]);
      NeedReDraw := False;
    end;
  end;
end;

procedure SetNumTiles(AValue: Integer);
begin
  NeedReDraw := NeedReDraw or (Entries[STAT_TILESMEM].SessionNumber <> AValue);
  Entries[STAT_TILESMEM].SessionNumber := AValue;
  Entries[STAT_TILESMEM].SessionText := IntToStr(AValue);
end;

procedure SetTilesToLoad(AValue: Integer);
begin
  NeedReDraw := NeedReDraw or (Entries[STAT_TILESLOAD].SessionNumber <> AValue);
  Entries[STAT_TILESLOAD].SessionNumber := AValue;
  Entries[STAT_TILESLOAD].SessionText := IntToStr(AValue);
end;

procedure AddDownloaded(ABytes, ATimes: Int64);
var
  Speed, SpeedAll: Double;
begin
  if (Entries[STAT_DOWNLOAD].SessionNumber <> ABytes) or (Entries[STAT_AVERAGE].SessionNumber <> ATimes) then
  begin
    Speed := 0.0;
    SpeedAll := 0.0;
    if ATimes > 0 then
      Speed := ABytes / (ATimes / 1000);
    if TimesOffset + ATimes  > 0 then
      SpeedAll := ((BytesOffset + ABytes)) / ((TimesOffset + ATimes) / 1000);
    Entries[STAT_DOWNLOAD].SessionNumber := ABytes;
    Entries[STAT_DOWNLOAD].OverallNumber := ABytes + BytesOffset;

    if ABytes < 1024*1024 then
      Entries[STAT_DOWNLOAD].SessionText := FloatToStrF(ABytes / 1024, ffFixed, 8,1) + ' KiB'
    else
      Entries[STAT_DOWNLOAD].SessionText := FloatToStrF(ABytes / 1024 / 1024, ffFixed, 8,1) + ' MiB';

    if (BytesOffset + ABytes) < 1024*1024 then
      Entries[STAT_DOWNLOAD].OverallText := FloatToStrF((BytesOffset + ABytes) / 1024, ffFixed, 8,1) + ' KiB'
    else
      Entries[STAT_DOWNLOAD].OverallText := FloatToStrF((BytesOffset + ABytes) / 1024 / 1024, ffFixed, 8,1) + ' MiB';


    Entries[STAT_AVERAGE].SessionNumber := ATimes;
    //
    Entries[STAT_AVERAGE].SessionText := FloatToStrF(Speed / 1024, ffFixed, 8,1) + ' KiB/s';
    Entries[STAT_AVERAGE].OverallText := FloatToStrF(SpeedAll / 1024, ffFixed, 8,1) + ' KiB/s';
    NeedRedraw := True;
  end;
end;

procedure SetNumFromNet(AValue: Integer);
begin
  NeedReDraw := NeedReDraw or (Entries[STAT_TILESNET].SessionNumber <> AValue);
  Entries[STAT_TILESNET].SessionNumber := AValue;
  Entries[STAT_TILESNET].SessionText := IntToStr(AValue);
  Entries[STAT_TILESNET].OverallText := IntToStr(NumFromNetOffset + AValue);
end;

procedure SetNumFromHD(AValue: Integer);
begin
  NeedReDraw := NeedReDraw or (Entries[STAT_TILESHD].SessionNumber <> AValue);
  Entries[STAT_TILESHD].SessionNumber := AValue;
  Entries[STAT_TILESHD].SessionText := IntToStr(AValue);
  Entries[STAT_TILESHD].OverallText := IntToStr(NumFromHDOffset + AValue);
end;



function DisplayFunc(Hook: PHook; Columns: PPChar; Entry: PStatEntry): Integer;
var
  i: Integer;
begin
  if Entry = nil then
  begin
    for i := 0 to 2 do
      Columns[i] := ColumnTitles[i];
  end
  else
  begin
    Columns[0] := RowTitles[Entry^.ID];
    Columns[1] := PChar(Entry^.SessionText);
    Columns[2] := PChar(Entry^.OverallText);
  end;
  Result := 0;
end;

procedure CreateStatWin;
var
  i: Integer;
begin
  ColumnTitles[1] := GetLocString(MSG_STAT_SESSION);
  ColumnTitles[2] := GetLocString(MSG_STAT_OVERALL);
  //
  RowTitles[0] := GetLocString(MSG_STAT_MEMTILES);
  RowTitles[1] := GetLocString(MSG_STAT_NETTILES);
  RowTitles[2] := GetLocString(MSG_STAT_HDTILES);
  RowTitles[3] := GetLocString(MSG_STAT_PENDINGTILES);
  RowTitles[4] := GetLocString(MSG_STAT_DOWNLOADED);
  RowTitles[5] := GetLocString(MSG_STAT_SPEED);

  MH_SetHook(DispHook, THookFunc(@DisplayFunc), nil);

  StatWin := MH_Window([
    MUIA_Window_Title,     AsTag(GetLocString(MSG_STAT_WINTITLE)), // 'Statistics'
    MUIA_Window_ID,        AsTag(MAKE_ID('M','S','T','A')),
    MUIA_HelpNode,         AsTag('StatWin'),
    WindowContents, AsTag(MH_VGroup([
      Child, AsTag(MH_Text(PChar(GetLocString(MSG_STAT_TITLE) + '                                                                                '))),
      Child, AsTag(MH_List(StatListEntry, [
        MUIA_Background, MUII_ReadListBack,
        MUIA_Frame, MUIV_Frame_ReadList,
        MUIA_List_DisplayHook, AsTag(@DispHook),
        MUIA_List_Format, AsTag('Bar,Bar P='#27'r,P='#27'r'),
        MUIA_List_Title, AsTag(True),
        TAG_DONE])),
      TAG_DONE])),
    TAG_DONE]);
    // Create content lines
    for i := 0 to 5 do
    begin
      Entries[i].ID := i;
      DoMethod(StatListEntry, [MUIM_List_InsertSingle, AsTag(@Entries[i]), AsTag(MUIV_List_Insert_Bottom)]);
    end;
    // Close Window
    DoMethod(StatWin, [MUIM_Notify, MUIA_Window_CloseRequest, MUI_TRUE,
      AsTag(StatWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
end;

initialization
  CreateStatWin;
  NumFromNetOffset := Prefs.DownFiles;
  NumFromHDOffset := Prefs.LoadedFiles;
  TimesOffset := Prefs.DownTime;
  BytesOffset := Prefs.DownBytes;
  Entries[STAT_DOWNLOAD].SessionNumber := -1;
finalization
  Prefs.DownFiles := NumFromNetOffset + Entries[STAT_TILESNET].SessionNumber;
  Prefs.LoadedFiles := NumFromHDOffset + Entries[STAT_TILESHD].SessionNumber;
  Prefs.DownTime := TimesOffset + Entries[STAT_AVERAGE].SessionNumber;
  Prefs.DownBytes := BytesOffset + Entries[STAT_DOWNLOAD].SessionNumber;
end.
