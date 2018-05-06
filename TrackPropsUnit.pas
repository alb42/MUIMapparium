unit TrackPropsUnit;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Classes, exec, utility, intuition, agraphics, locale, mui, muihelper,
  prefsunit, osmhelper, MUIWrap, imagesunit, positionunit, waypointunit,
  MUIPlotBoxUnit;

const
  XAXIS_TIME_S = 0;
  XAXIS_TIME_MIN = 1;
  XAXIS_TIME_HOUR = 2;
  XAXIS_DIST_METER = 3;
  XAXIS_DIST_KM = 4;

  YAXIS_NONE = 0;
  YAXIS_HEIGHT_METER = 1;
  YAXIS_SLOPE_METER = 2;
  YAXIS_SPEED_METERS = 3;
  YAXIS_SPEED_KMS = 4;

var
  XAxisStrings: array[0..4] of string =
    ('Time [s]'#0, 'Time [min]'#0, 'Time [h]'#0, 'Distance [m]'#0, 'Distance [km]'#0);
  YAxisStrings: array[0..4] of string =
    ('None'#0, 'Height [m]'#0, 'Slope [m]', 'Speed [m/s]', 'Speed [km/h]');
  XAxisTitles: array of PChar;
  YAxisTitles: array of PChar;
type
  TTrackPoint = record
    Time: Double;
    Pos: Double;
    Height: Double;
    AddValues: array of Double;
  end;
  TTrackCurve = record
    AddNames: array of string;
    Data: array of TTrackPoint;
  end;

var
  TC: TTrackCurve;
  TrackPropsWin: PObject_;
  OnTrackChanged: TProcedure = nil;
  OnTrackRedraw: TProcedure = nil;
  PlotPanel: TPlotPanel;
  ChooseXAxis, ChooseYLeft, ChooseYRight, DrawButton, HTMLButton, TrackCol: PObject_;
  IsISO: Boolean;
  LengthUnits: array[0..1] of string;
  LengthFactors: array[0..1] of Single;
  CurTrack: TTrack = nil; // current showed track
  ActiveTrackPt: Integer = -1;
  CloseHook: THook;

procedure ShowTrackProps(NewTrack: TTrack);
function DrawButtonEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;

implementation

uses
  ASL, MUIMappariumLocale, MapPanelUnit, versionunit, Math;

var
  TrackName, SaveButton, CloseButton: PObject_;
  SaveHook, DrawButtonHook, HTMLButtonHook: THook;

procedure GetLengthUnit;
begin
  if IsISO then
  begin
    LengthUnits[0] := 'm';
    LengthFactors[0] := 1;
    LengthUnits[1] := 'km';
    LengthFactors[1] := 1;
  end
  else
  begin
    LengthUnits[0] := 'ft';
    LengthFactors[0] := 3.28084;
    LengthUnits[1] := 'ml';
    LengthFactors[1] := 0.621371;
  end;
end;

// Open Properties Window
procedure ShowTrackProps(NewTrack: TTrack);
var
  MUIRGB: TMUI_RGBcolor;
begin
  if Assigned(NewTrack) then
  begin
    CurTrack := NewTrack;
    MH_Set(TrackPropsWin, MUIA_Window_Open, AsTag(True));
    // Set Name
    MH_Set(TrackName, MUIA_String_Contents, AsTag(PChar(NewTrack.Name)));
    // Set Color
    MUIRGB.Red := NewTrack.Color shl 8;
    MUIRGB.Green := NewTrack.Color shl 16;
    MUIRGB.Blue := NewTrack.Color shl 24;
    MH_Set(TrackCol, MUIA_Pendisplay_RGBcolor, AsTag(@MUIRGB));
    //
    SetLength(TC.Data, 0);
    DrawButtonEvent(nil, nil, nil);
  end;
end;

// Save the Values
function SaveEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  MUIRGB: PMUI_RGBcolor;
begin
  Result := 0;
  if TrackList.IndexOf(CurTrack) >= 0 then
  begin
    CurTrack.Name := PChar(MH_Get(TrackName, MUIA_String_Contents));
    MUIRGB := PMUI_RGBcolor(MH_Get(TrackCol, MUIA_Pendisplay_RGBcolor));
    if Assigned(MUIRGB) then
      CurTrack.Color := (MUIRGB^.Red shr 8 and $ff0000) or (MUIRGB^.Green shr 16 and $FF00) or (MUIRGB^.Blue shr 24 and $FF);
    if Assigned(OnTrackChanged) then
      OnTrackChanged;
  end;
  MH_Set(TrackPropsWin, MUIA_Window_Open, AsTag(False));
end;

procedure MakeTrackCurve(CurTrack: TTrack);
var
  Last: TCoord;
  LastTime, DoneWay: Double;
  i, j: Integer;
begin
  SetLength(TC.Data, Length(CurTrack.Pts));
  if Length(CurTrack.Pts) > 0 then
  begin
    Last := CurTrack.Pts[0].Position;
    DoneWay := 0;
    LastTime := CurTrack.Pts[0].Time;
  end;
  for i := 0 to High(TC.Data) do
  begin
    TC.Data[i].Pos := DoneWay + GradToDistance(Last, CurTrack.Pts[i].Position);
    TC.Data[i].Time:= (CurTrack.Pts[i].Time - LastTime) * (24*60*60);
    TC.Data[i].Height:= CurTrack.Pts[i].Elevation;
    //
    Last := CurTrack.Pts[i].Position;
    DoneWay := TC.Data[i].Pos;
    SetLength(TC.Data[i].AddValues, Length(CurTrack.AddFields));
    for j := 0 to High(CurTrack.AddFields) do
      TC.Data[i].AddValues[j] := CurTrack.Pts[i].AddFields[j];
  end;
end;

function GetXAxis(CurTrack: TTrack; Idx: Integer; out Name: string; out AxUnit: string): TDoubleArray;
var
  i: Integer;
begin
  Name := '';
  AxUnit := '';
  SetLength(Result, 0);
  if Assigned(CurTrack) then
  begin
    if Length(TC.Data) = 0 then
    begin
      MakeTrackCurve(CurTrack);
    end;
    SetLength(Result, Length(TC.Data));
    for i := 0 to High(Result) do
    begin
      case Idx of
        XAXIS_TIME_S: begin
          Result[i] := TC.Data[i].Time;           // Time [s]
          Name := GetLocString(MSG_TRACKPROP_TIME);
          AxUnit := 's';
        end;
        XAXIS_TIME_MIN: begin
          Result[i] := TC.Data[i].Time / 60;      // Time [min]
          Name := GetLocString(MSG_TRACKPROP_TIME);
          AxUnit := 'min';
        end;
        XAXIS_TIME_HOUR: begin
          Result[i] := TC.Data[i].Time / 60 / 60; // Time [h]
          Name := GetLocString(MSG_TRACKPROP_TIME);
          AxUnit := 'h';
        end;
        XAXIS_DIST_METER: begin
          Result[i] := TC.Data[i].Pos * LengthFactors[0];            // Distance [m]
          Name := GetLocString(MSG_TRACKPROP_DISTANCE);
          AxUnit := LengthUnits[0];
        end;
        XAXIS_DIST_KM: begin
          Result[i] := (TC.Data[i].Pos / 1000) * LengthFactors[1];     // Distance [km]
          Name := GetLocString(MSG_TRACKPROP_DISTANCE);
          AxUnit := LengthUnits[1];
        end
        else
          Result[i] := 0;
      end;
    end;
  end;
end;

function GetYAxis(CurTrack: TTrack; Idx: Integer): TDoubleArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  if Idx = YAXIS_NONE then
    Exit;
  if Assigned(CurTrack) then
  begin
    if Length(TC.Data) = 0 then
    begin
      MakeTrackCurve(CurTrack);
    end;
    SetLength(Result, Length(TC.Data));
    Result[0] := 0;
    for i := 0 to High(Result) do
    begin
      case Idx of
        YAXIS_HEIGHT_METER:
        begin
          Result[i] := TC.Data[i].Height * LengthFactors[0];         // Height [m]
        end;
        YAXIS_SLOPE_METER : begin
          if i > 0 then
          begin
            Result[i] := (TC.Data[i].Height - TC.Data[i - 1].Height) * LengthFactors[0]; // Slope [m]
          end;
        end;
        YAXIS_SPEED_METERS: begin                                     // speed m/s
          if i > 0 then
          begin
            if (TC.Data[i].time - TC.Data[i - 1].time <> 0) then
              Result[i] := ((TC.Data[i].Pos - TC.Data[i - 1].Pos) * LengthFactors[0]) / (TC.Data[i].time - TC.Data[i - 1].time)
            else
              Result[i] := 0;
          end;
        end;
        YAXIS_SPEED_KMS: begin                                       // Speed km/h
          if i > 0 then
          begin
            if ((TC.Data[i].time - TC.Data[i - 1].time) / 60 / 60) <> 0 then
              Result[i] := (((TC.Data[i].Pos - TC.Data[i - 1].Pos) / 1000) * LengthFactors[1]) / ((TC.Data[i].time - TC.Data[i - 1].time) / 60 / 60)
            else
              Result[i] := 0;
          end;
        end;
        else
        begin
          Result[i] := 0;
          //DataIdx := ChooseYAxis.ItemIndex - 3;
          //if (DataIdx >= 0) and (DataIdx <= High(TC.AddNames)) then
          //begin
          //  Result[i] := TC.Data[i].AddValues[DataIdx];
          //end;
        end;
      end;
    end;
  end;
end;

// Save the Values
function DrawButtonEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  XIdx: Integer;
  YIdx1, YIdx2: Integer;
  XAxis,YAxis1, YAxis2: TDoubleArray;
  Valid: array of Boolean;
  XName, XUnit: string;
begin
  Result := 0;
  SetLength(Valid, 0);
  try
  if TrackList.IndexOf(CurTrack) >= 0 then
  begin
    XIdx := MH_Get(ChooseXAxis, MUIA_Cycle_Active);
    YIdx1 := MH_Get(ChooseYLeft, MUIA_Cycle_Active);
    YIdx2 := MH_Get(ChooseYRight, MUIA_Cycle_Active);
    XAxis := GetXAxis(CurTrack, XIdx, XName, XUnit);
    YAxis1 := GetYAxis(CurTrack, YIdx1);
    YAxis2 := GetYAxis(CurTrack, YIdx2);
    PlotPanel.Clear;
    PlotPanel.AxisLeft.Color := clBlack;
    PlotPanel.AxisRight.Color := clBlack;
    PlotPanel.AxisBottom.AxUnit := XUnit;
    PlotPanel.AxisBottom.Title := XName;
    PlotPanel.AxisBottom.Options := PlotPanel.AxisBottom.Options + [aoForceNoExp, aoMajorGrid, aoMinorGrid];
    if (Length(XAxis) > 0) and (Length(YAxis1) > 0) then
    begin
      PlotPanel.AxisLeft.AxUnit := '';
      case YIdx1 of
        YAXIS_HEIGHT_METER,
        YAXIS_SLOPE_METER:  PlotPanel.AxisLeft.AxUnit := LengthUnits[0];
        YAXIS_SPEED_METERS: PlotPanel.AxisLeft.AxUnit := LengthUnits[0] + '/s';
        YAXIS_SPEED_KMS: PlotPanel.AxisLeft.AxUnit := LengthUnits[1] + '/h';
      end;
      PlotPanel.AddCurve(XAxis, YAxis1, Valid, apBottom, apLeft, clBlue, YAxisStrings[YIdx1]);
      PlotPanel.AxisLeft.Options := PlotPanel.AxisLeft.Options + [aoShowTics, aoShowLabels, aoMajorGrid];
      PlotPanel.AxisLeft.Color := clBlue;
    end
    else
    begin
      PlotPanel.AxisLeft.AxUnit := '';
      PlotPanel.AxisLeft.Options := PlotPanel.AxisLeft.Options - [aoShowTics, aoShowLabels, aoMajorGrid];
    end;
    if (Length(XAxis) > 0) and (Length(YAxis2) > 0) then
    begin
      PlotPanel.AxisRight.AxUnit := '';
      case YIdx2 of
        YAXIS_HEIGHT_METER,
        YAXIS_SLOPE_METER:  PlotPanel.AxisRight.AxUnit := LengthUnits[0];
        YAXIS_SPEED_METERS: PlotPanel.AxisRight.AxUnit := LengthUnits[0] + '/s';
        YAXIS_SPEED_KMS: PlotPanel.AxisRight.AxUnit := LengthUnits[1] + '/h';
      end;
      PlotPanel.AddCurve(XAxis, YAxis2, Valid, apBottom, apRight, clRed, YAxisStrings[YIdx2]);
      PlotPanel.AxisRight.Options := PlotPanel.AxisRight.Options + [aoShowTics, aoShowLabels, aoMajorGrid];
      PlotPanel.AxisRight.Color := clRed;
    end
    else
    begin
      PlotPanel.AxisRight.Options := PlotPanel.AxisRight.Options - [aoShowTics, aoShowLabels, aoMajorGrid];
    end;
    PlotPanel.PlotData;
  end;
  except
    ;
  end;
end;

const
  HTMLTemplate = '<html>'#13#10 +
  '<head><title>MUIMapparium Track: %TRACKNAME%</title></head>'#13#10 +
  '<body bgcolor="#FFFFFF">'#13#10 +
  '<h2 align=center>%TRACKNAME%</h2>'#13#10 +
  '<h5 align=center>%VERSION%</h5>'#13#10 +
  '%DESC%<BR>'#13#10 +
  '<table>'#13#10 +
  '<tr><td>Time</td><td>%DATE%</td></tr>'#13#10 +
  '<tr><td>Distance</td><td>%LENGTH%</td></tr>'#13#10 +
  '<tr><td>Track Time</td><td>%TIME%</td></tr>'#13#10 +
  '<tr><td>Move Time</td><td>%MTIME%</td></tr>'#13#10 +
  '<tr><td>Average Speed</td><td>%ASPEED%</td></tr>'#13#10 +
  '<tr><td>Average Moving Speed</td><td>%MSPEED%</td></tr>'#13#10 +
  '<tr><td>Max Speed</td><td>%MAXSPEED%</td></tr>'#13#10 +
  '<tr><td>Height Diff</td><td>%HEIGHT%</td></tr>'#13#10 +
  '</table><br>'#13#10 +
  '<table><tr><td><img src="%PNG1%"></td></tr><tr><td align=center>Map of the track</td><br>'#13#10 +
  '<table><tr><td><img src="%PNG2%"></td></tr><tr><td align=center>Plot: %X% - %Y%</td><br>'#13#10 +
  '</body></html>';


procedure SaveToHTML(AFileName: string);
var
  PngFile1, PngFile2, HtmlFile: string;
  SL: TStringList;
  s, l: string;
  OldR, OldT, OldW: Boolean;
  M, H, t, i: Integer;
  StopTime: Double;
  Len, MaxSpeed, MinHeight, MaxHeight: Double;
begin
  if not Assigned(CurTrack) then
    Exit;
  PngFile1 := ChangeFileExt(AFileName, '') + '_map.png';
  PngFile2 := ChangeFileExt(AFileName, '') + '_plot.png';
  HtmlFile := ChangeFileExt(AFileName, '.html');
  SL := TStringList.Create;
  try
    OldR := MUIMapPanel.ShowRoutes;
    MUIMapPanel.ShowRoutes := False;
    OldT := MUIMapPanel.ShowTracks;
    MUIMapPanel.ShowTracks := False;
    OldW := MUIMapPanel.ShowMarker;
    MUIMapPanel.ShowMarker := False;
    MUIMapPanel.SaveToFile(PngFile1);
    MUIMapPanel.ShowRoutes := OldR;
    MUIMapPanel.ShowTracks := OldT;
    MUIMapPanel.ShowMarker := OldW;
    //
    PlotPanel.ExportPNG(PngFile2);
    //
    s := StringReplace(HTMLTemplate, '%VERSION%', Copy(VERSIONSTRING, 6, Length(VERSIONSTRING)), [rfReplaceAll]);
    s := StringReplace(s, '%DESC%', CurTrack.Desc, [rfReplaceAll]);
    s := StringReplace(s, '%TRACKNAME%', CurTrack.Name, [rfReplaceAll]);
    s := StringReplace(s, '%PNG1%', ExtractFileName(PngFile1), [rfReplaceAll]);
    s := StringReplace(s, '%PNG2%', ExtractFileName(PngFile2), [rfReplaceAll]);

    i := Min(High(XAxisStrings), Max(0, MH_Get(ChooseXAxis, MUIA_Cycle_Active)));
    s := StringReplace(s, '%X%', XAxisStrings[i], [rfReplaceAll]);

    i := Min(High(YAxisStrings), Max(0, MH_Get(ChooseYLeft, MUIA_Cycle_Active)));
    l := '<font color=#0000FF>' + YAxisStrings[i] + '</font>';
    i := Min(High(YAxisStrings), Max(0, MH_Get(ChooseYRight, MUIA_Cycle_Active)));
    l := l + ', <font color=#FF0000>' + YAxisStrings[i] + '</font>';
    s := StringReplace(s, '%Y%', l, [rfReplaceAll]);

    // Time
    t := Round(TC.Data[High(TC.Data)].Time);
    M := t div 60;
    t := t mod 60;
    H := M div 60;
    M := M mod 60;
    s := StringReplace(s, '%TIME%', Format('%2.2d:%2.2d:%2.2d',[H,M,T]), [rfReplaceAll]);
    // Length
    Len := (TC.Data[High(TC.Data)].Pos / 1000) * LengthFactors[1];
    s := StringReplace(s, '%LENGTH%', FloatToStrF(Len, ffFixed, 8,2) + ' ' + LengthUnits[1], [rfReplaceAll]);
    // Average Speed
    if TC.Data[High(TC.Data)].Time > 0 then
      Len := Len / (TC.Data[High(TC.Data)].Time / 3600)
    else
      Len := 0;
    s := StringReplace(s, '%ASPEED%', FloatToStrF(Len, ffFixed, 8,2) + ' ' + LengthUnits[1] + '/h', [rfReplaceAll]);
    MaxSpeed := 0;
    StopTime := 0;
    for i := 0 to High(TC.Data) do
    begin
      if (i > 0) and ((TC.Data[i].time - TC.Data[i - 1].time) > 0) then
      begin
        Len := (((TC.Data[i].Pos - TC.Data[i - 1].Pos) / 1000) * LengthFactors[1]) / ((TC.Data[i].time - TC.Data[i - 1].time) / 60 / 60);
        if Len > MaxSpeed then
          MaxSpeed := Len;
        // Less than 3 hm/h -> stopped
        if Len < 3 then
          StopTime := StopTime + (TC.Data[i].time - TC.Data[i - 1].time);
      end;
      if i = 0 then
      begin
        MinHeight := TC.Data[i].Height;
        MaxHeight := TC.Data[i].Height;
      end
      else
      begin
        if TC.Data[i].Height < MinHeight then
          MinHeight := TC.Data[i].Height;
        if TC.Data[i].Height > MaxHeight then
          MaxHeight := TC.Data[i].Height;
      end;
    end;
    s := StringReplace(s, '%MAXSPEED%', FloatToStrF(MaxSpeed, ffFixed, 8,2) + ' ' + LengthUnits[1] + '/h', [rfReplaceAll]);
    l := FloatToStrF((MaxHeight - MinHeight) * LengthFactors[0] , ffFixed, 8,2) + ' ' + LengthUnits[0] + ' ('+FloatToStrF(MinHeight * LengthFactors[0] , ffFixed, 8,2) + ' ' + LengthUnits[0] + ' - '+FloatToStrF(MaxHeight * LengthFactors[0] , ffFixed, 8,2) + ' ' + LengthUnits[0] + ')';
    s := StringReplace(s, '%HEIGHT%', l, [rfReplaceAll]);
    //
    // Average Speed without stopping time
    Len := (TC.Data[High(TC.Data)].Pos / 1000) * LengthFactors[1];
    if TC.Data[High(TC.Data)].Time - StopTime > 0 then
      Len := Len / ((TC.Data[High(TC.Data)].Time - StopTime) / 3600)
    else
      Len := 0;
    s := StringReplace(s, '%MSPEED%', FloatToStrF(Len, ffFixed, 8,2) + ' ' + LengthUnits[1] + '/h', [rfReplaceAll]);
    t := Round(TC.Data[High(TC.Data)].Time - StopTime);
    M := t div 60;
    t := t mod 60;
    H := M div 60;
    M := M mod 60;
    s := StringReplace(s, '%MTIME%', Format('%2.2d:%2.2d:%2.2d',[H,M,T]), [rfReplaceAll]);
    //
    l := '-';
    if Length(CurTrack.Pts) > 0 then
      l:= FormatDateTime('yyyy-mm-dd hh:nn:ss',CurTrack.Pts[0].Time);
    s := StringReplace(s, '%DATE%', l, [rfReplaceAll]);
    //
    SL.Text := s;
    SL.SaveToFile(HTMLFile);
  finally
    SL.Free;
  end;
end;


function HTMLButtonEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
var
  fr: PFileRequester;
  OldDrawer: string;
  OldFileName: string;
begin
  Result := 0;
{$R-}
  OldFilename := stringreplace(Copy(CurTrack.Name, 1, 27) + '.html', '/', '', [rfReplaceAll]);
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
      {$if defined(VER3_0) or defined(MorphOS) or defined(Amiga68k)}
      OldFilename := IncludeTrailingPathDelimiter(string(fr^.rf_dir)) + string(fr^.rf_file);
      {$else}
      OldFilename := IncludeTrailingPathDelimiter(string(fr^.fr_drawer)) + string(fr^.fr_file);
      {$endif}
      SaveToHTML(OldFilename);
    end;
    FreeAslRequest(fr);
  end;
end;

procedure CheckLocale;
var
  Loc: PLocale;
begin
  IsISO := True;
  Loc := OpenLocale(nil);
  if Assigned(Loc) then
  begin
    IsISO := Loc^.loc_MeasuringSystem = MS_ISO;
    CloseLocale(Loc);
  end;
  GetLengthUnit;
end;

function CloseEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  CurTrack := nil;
  if Assigned(OnTrackRedraw) then
    OnTrackRedraw();
end;

procedure MarkerChangeEvent(MarkerID: Integer);
begin
  if PlotPanel.ShowMarker and (PlotPanel.MouseModus = mmMarker) then
    ActiveTrackPt := PlotPanel.XMarkerValueIdx
  else
    ActiveTrackPt := -1;
  if Assigned(OnTrackRedraw) then
    OnTrackRedraw();
end;

procedure CreateTrackPropsWin;
var
  i: Integer;
begin
  CheckLocale;
  //
  // Locale for Plots
  Menutitle_Plot := GetLocString(MSG_PLOT_TITLE);
  Menutitle_Rescale := GetLocString(MSG_PLOT_RESCALE);
  Menutitle_Zoom := GetLocString(MSG_PLOT_ZOOM);
  Menutitle_Position := GetLocString(MSG_PLOT_POSITION);
  Menutitle_Data := GetLocString(MSG_PLOT_DATA);
  Menutitle_Marker := GetLocString(MSG_PLOT_MARKER);
  Menutitle_ASCII := GetLocString(MSG_PLOT_ASCII);
  Menutitle_PNG := GetLocString(MSG_PLOT_PNG);
  //
  HintText_XAxis1 := GetLocString(MSG_TRACKPROP_XAXIS);
  HintText_XAxis2 := GetLocString(MSG_TRACKPROP_XAXIS);
  HintText_YAxis1 := GetLocString(MSG_TRACKPROP_LEFTAXIS);
  HintText_YAxis2 := GetLocString(MSG_TRACKPROP_RIGHTAXIS);
  //
  // make x Axis
  XAxisStrings[0] := GetLocString(MSG_TRACKPROP_TIME) + ' [s]';
  XAxisStrings[1] := GetLocString(MSG_TRACKPROP_TIME) + ' [min]';
  XAxisStrings[2] := GetLocString(MSG_TRACKPROP_TIME) + ' [h]';
  XAxisStrings[3] := GetLocString(MSG_TRACKPROP_DISTANCE) + ' [' + LengthUnits[0] + ']';
  XAxisStrings[4] := GetLocString(MSG_TRACKPROP_DISTANCE) + ' [' + LengthUnits[1] + ']';
  //
  SetLength(XAxisTitles, Length(XAxisStrings) + 1);
  for i := 0 to High(XAxisTitles) do
  begin
    XAxisTitles[i] := PChar(@(XAxisStrings[i][1]));
  end;
  XAxisTitles[High(XAxisTitles)] := nil;
  // make y Axis
  YAxisStrings[0] := GetLocString(MSG_TRACKPROP_NONE);
  YAxisStrings[1] := GetLocString(MSG_TRACKPROP_HEIGHT) + ' [' + LengthUnits[0] + ']';
  YAxisStrings[2] := GetLocString(MSG_TRACKPROP_SLOPE) + ' [' + LengthUnits[0] + ']';
  YAxisStrings[3] := GetLocString(MSG_TRACKPROP_SPEED) + ' [' + LengthUnits[0] + '/s]';
  YAxisStrings[4] := GetLocString(MSG_TRACKPROP_SPEED) + ' [' + LengthUnits[1] + '/h]';
  //
  SetLength(YAxisTitles, Length(YAxisStrings) + 1);
  for i := 0 to High(YAxisTitles) do
  begin
    YAxisTitles[i] := PChar(@(YAxisStrings[i][1]));
  end;
  YAxisTitles[High(YAxisTitles)] := nil;
  //
  // make plotpanel
  PlotPanel := TPlotPanel.Create([TAG_DONE]);
  TrackPropsWin := MH_Window([
    MUIA_Window_Title,     AsTag(GetLocString(MSG_TRACKPROP_TITLE)), // 'Track Properties'
    MUIA_Window_ID,        AsTag(MAKE_ID('T','R','A','P')),
    MUIA_HelpNode,         AsTag('TrackWin'),
    WindowContents, AsTag(MH_VGroup([
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        MUIA_FrameTitle, AsTag(GetLocString(MSG_TRACKPROP_NAME)),   // 'Track Title'
        Child, AsTag(MH_String(TrackName, [
          MUIA_Weight, 180,
          MUIA_Frame, MUIV_Frame_String,
          MUIA_String_Format, MUIV_String_Format_Left,
          MUIA_String_Contents, AsTag('________________________'),
          TAG_END])),
        Child, AsTag(MH_Poppen(TrackCol, [MUIA_Weight, 20, TAG_END])),
        TAG_END])),
      Child, AsTag(PlotPanel.MUIObject),
      Child, AsTag(MH_HGroup([
        Child, AsTag(MH_Text(PChar(MUIX_R + GetLocString(MSG_TRACKPROP_XAXIS)))),
        Child, AsTag(MH_Cycle(ChooseXAxis, [
          MUIA_Cycle_Entries, AsTag(@(XAxisTitles[0])),
          TAG_DONE])),
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Text(PChar(MUIX_R + GetLocString(MSG_TRACKPROP_LEFTAXIS)))),
        Child, AsTag(MH_Image([
          MUIA_Image_Spec, AsTag('2:00000000,00000000,ffffffff'),
          TAG_DONE])),
        Child, AsTag(MH_Cycle(ChooseYLeft, [
          MUIA_Cycle_Entries, AsTag(@(YAxisTitles[0])),
          MUIA_Cycle_Active, 1,
          TAG_DONE])),
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Text(PChar(MUIX_R + GetLocString(MSG_TRACKPROP_RIGHTAXIS)))),
        Child, AsTag(MH_Image([
          MUIA_Image_Spec, AsTag('2:ffffffff,00000000,00000000'),
          TAG_DONE])),
        Child, AsTag(MH_Cycle(ChooseYRight, [
          MUIA_Cycle_Entries, AsTag(@(YAxisTitles[0])),
          MUIA_Cycle_Active, 4,
          TAG_DONE])),
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Button(DrawButton, GetLocString(MSG_TRACKPROP_DRAW))), // Draw
        Child, AsTag(MH_Button(HTMLButton, GetLocString(MSG_ROUTEPOP_EXPORT))), // Export
        TAG_END])),
      Child, AsTag(MH_HGroup([
        MUIA_Frame, MUIV_Frame_Group,
        Child, AsTag(MH_Button(SaveButton, GetLocString(MSG_GENERAL_SAVE))), // 'Save'
        Child, AsTag(MH_HSpace(0)),
        Child, AsTag(MH_Button(CloseButton, GetLocString(MSG_GENERAL_CLOSE))), // 'Close'
        TAG_DONE])),
      TAG_END])),
    TAG_END]);
  // save the changes if any
  ConnectHookFunction(MUIA_Pressed, AsTag(False), SaveButton, nil, @SaveHook, @SaveEvent);
  ConnectHookFunction(MUIA_Pressed, AsTag(False), DrawButton, nil, @DrawButtonHook, @DrawButtonEvent);
  ConnectHookFunction(MUIA_Pressed, AsTag(False), HTMLButton, nil, @HTMLButtonHook, @HTMLButtonEvent);

  ConnectHookFunction(MUIA_Pressed, AsTag(False), CloseButton, nil, @CloseHook, @CloseEvent);
  ConnectHookFunction(MUIA_Window_CloseRequest, AsTag(True), TrackPropsWin, nil, @CloseHook, @CloseEvent);

  // just close it
  DoMethod(CloseButton, [MUIM_Notify, MUIA_Pressed, AsTag(False),
      AsTag(TrackPropsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
  DoMethod(TrackPropsWin, [MUIM_Notify, MUIA_Window_CloseRequest, AsTag(True),
      AsTag(TrackPropsWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);

  PlotPanel.OnMarkerChange := @MarkerChangeEvent;
end;


initialization
  CreateTrackPropsWin;
finalization
  PlotPanel.Free;
end.
