unit PrefsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TColor = LongWord;

const
  clRed = $FF0000;
  clBlue = $0000FF;
  clLime = $00ff00;
  clGreen = $00aa00;
  clFuchsia = $ff00ff;
  clBlack = 0;
  clWhite = $ffffff;
  clGray = $cccccc;
  clYellow = $ffff00;

  SECTION_GENERAL = 'General';
  SECTION_STATISTICS = 'Statistics';
  SECTION_WINDOW = 'Window';
type

  { TPrefs }

  TPrefs = class
  private
    function GetActiveImageColor: TColor;
    function GetActiveRouteColor: TColor;
    function GetActiveTrackColor: TColor;
    function GetDownBytes: Int64;
    function GetDownFiles: Int64;
    function GetDownTime: Int64;
    function GetImageColor: TColor;
    function GetLoadFiles: Int64;
    function GetLoadPath: string;
    function GetMarkerSize: Integer;
    function GetMaxTiles: integer;
    function GetMiddleMarker: integer;
    function GetMiddleMarkerColor: TColor;
    function GetRouteColor: TColor;
    function GetSearchLang: string;
    function GetStartPosLat: Double;
    function GetStartPosLon: Double;
    function GetStartZoom: Integer;
    function GetTrackColor: TColor;
    function GetViewProg: string;
    function GetWaypointColor: TColor;
    function GetStatWinOpen: Boolean;
    procedure SetActiveImageColor(AValue: TColor);
    procedure SetActiveRouteColor(AValue: TColor);
    procedure SetActiveTrackColor(AValue: TColor);
    procedure SetDownBytes(AValue: Int64);
    procedure SetDownFiles(AValue: Int64);
    procedure SetDownTime(AValue: Int64);
    procedure SetImageColor(AValue: TColor);
    procedure SetLoadFiles(AValue: Int64);
    procedure SetLoadPath(AValue: string);
    procedure SetMarkerSize(AValue: Integer);
    procedure SetMaxTiles(AValue: integer);
    procedure SetMiddleMarker(AValue: integer);
    procedure SetMiddleMarkerColor(AValue: TColor);
    procedure SetRouteColor(AValue: TColor);
    procedure SetSearchLang(AValue: string);
    procedure SetStartPosLat(AValue: Double);
    procedure SetStartPosLon(AValue: Double);
    procedure SetStartZoom(AValue: Integer);
    procedure SetTrackColor(AValue: TColor);
    procedure SetViewProg(AValue: string);
    procedure SetWaypointColor(AValue: TColor);
    procedure SetStatWinOpen(AValue: Boolean);
  public
    IniFile: TIniFile;
    constructor Create;
    destructor Destroy; override;

    property MaxTiles: integer read GetMaxTiles write SetMaxTiles;
    property MiddleMarker: integer read GetMiddleMarker write SetMiddleMarker;
    property MiddleMarkerColor: TColor read GetMiddleMarkerColor write SetMiddleMarkerColor;
    property WaypointColor: TColor read GetWaypointColor write SetWaypointColor;
    property ActiveImageColor: TColor read GetActiveImageColor write SetActiveImageColor;
    property ImageColor: TColor read GetImageColor write SetImageColor;
    property TrackColor: TColor read GetTrackColor write SetTrackColor;
    property ActiveTrackColor: TColor read GetActiveTrackColor write SetActiveTrackColor;
    property ActiveRouteColor: TColor read GetActiveRouteColor write SetActiveRouteColor;
    property RouteColor: TColor read GetRouteColor write SetRouteColor;
    property StartPosLat: Double read GetStartPosLat write SetStartPosLat;
    property StartPosLon: Double read GetStartPosLon write SetStartPosLon;
    property StartZoom: Integer read GetStartZoom write SetStartZoom;
    property LoadPath: string read GetLoadPath write SetLoadPath;
    property MarkerSize: Integer read GetMarkerSize write SetMarkerSize;
    property SearchLang: string read GetSearchLang write SetSearchLang;
    property ViewProg: string read GetViewProg write SetViewProg;
    // Statistics
    property DownFiles: Int64 read GetDownFiles write SetDownFiles;
    property LoadedFiles: Int64 read GetLoadFiles write SetLoadFiles;
    property DownBytes: Int64 read GetDownBytes write SetDownBytes;
    property DownTime: Int64 read GetDownTime write SetDownTime;
    // Window Open
    property StatWinOpen: Boolean read GetStatWinOpen write SetStatWinOpen;
  end;

var
  Prefs: TPrefs;

implementation

{ TPrefs }

function TPrefs.GetActiveImageColor: TColor;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'ActiveImageColor', clRed);
end;

function TPrefs.GetActiveRouteColor: TColor;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'ActiveRouteColor', clLime);
end;

function TPrefs.GetActiveTrackColor: TColor;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'ActiveTrackColor', clFuchsia);
end;

function TPrefs.GetDownBytes: Int64;
begin
  Result := IniFile.ReadInt64(SECTION_STATISTICS, 'DownBytes', 0);
end;

function TPrefs.GetDownFiles: Int64;
begin
  Result := IniFile.ReadInt64(SECTION_STATISTICS, 'DownFiles', 0);
end;

function TPrefs.GetDownTime: Int64;
begin
  Result := IniFile.ReadInt64(SECTION_STATISTICS, 'DownTime', 0);
end;

function TPrefs.GetImageColor: TColor;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'ImageColor', clGreen);
end;

function TPrefs.GetLoadFiles: Int64;
begin
  Result := IniFile.ReadInt64(SECTION_STATISTICS, 'LoadFiles', 0);
end;

function TPrefs.GetLoadPath: string;
begin
  Result := IniFile.ReadString(SECTION_GENERAL, 'LoadPath', '');
end;

function TPrefs.GetMarkerSize: Integer;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'MarkerSize', 2);
end;

function TPrefs.GetMaxTiles: integer;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'MaxTiles', 50);
end;

function TPrefs.GetMiddleMarker: integer;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'MiddleMarker', 1);
end;

function TPrefs.GetMiddleMarkerColor: TColor;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'MiddleMarkerColor', clBlack);
end;

function TPrefs.GetRouteColor: TColor;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'RouteColor', clGreen);
end;

function TPrefs.GetSearchLang: string;
begin
  Result := IniFile.ReadString(SECTION_GENERAL, 'DefaultLanguage', 'en');
end;

function TPrefs.GetStartPosLat: Double;
begin
  Result := IniFile.ReadFloat(SECTION_GENERAL, 'StartLat', 52.0);
end;

function TPrefs.GetStartPosLon: Double;
begin
  Result := IniFile.ReadFloat(SECTION_GENERAL, 'StartLon', 13.0);
end;

function TPrefs.GetStartZoom: Integer;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'StartZoom', 7);
end;

function TPrefs.GetTrackColor: TColor;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'TrackColor', clRed);
end;

function TPrefs.GetViewProg: string;
begin
  Result := IniFile.ReadString(SECTION_GENERAL, 'Viewer', 'Sys:Utilities/MultiView');
end;

function TPrefs.GetWaypointColor: TColor;
begin
  Result := IniFile.ReadInteger(SECTION_GENERAL, 'WaypointColor', clBlue);
end;

function TPrefs.GetStatWinOpen: Boolean;
begin
  Result := IniFile.ReadBool(SECTION_WINDOW, 'Statistic', False);
end;

procedure TPrefs.SetActiveImageColor(AValue: TColor);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'ActiveImageColor', AValue);
end;

procedure TPrefs.SetActiveRouteColor(AValue: TColor);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'ActiveRouteColor', AValue);
end;

procedure TPrefs.SetActiveTrackColor(AValue: TColor);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'ActiveTrackColor', AValue);
end;

procedure TPrefs.SetDownBytes(AValue: Int64);
begin
  IniFile.WriteInt64(SECTION_STATISTICS, 'DownBytes', AValue);
end;

procedure TPrefs.SetDownFiles(AValue: Int64);
begin
  IniFile.WriteInt64(SECTION_STATISTICS, 'DownFiles', AValue);
end;

procedure TPrefs.SetDownTime(AValue: Int64);
begin
  IniFile.WriteInt64(SECTION_STATISTICS, 'DownTime', AValue);
end;

procedure TPrefs.SetImageColor(AValue: TColor);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'ImageColor', AValue);
end;

procedure TPrefs.SetLoadFiles(AValue: Int64);
begin
  IniFile.WriteInt64(SECTION_STATISTICS, 'LoadFiles', AValue);
end;

procedure TPrefs.SetLoadPath(AValue: string);
begin
  IniFile.WriteString(SECTION_GENERAL, 'LoadPath', AValue);
end;

procedure TPrefs.SetMarkerSize(AValue: Integer);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'MarkerSize', AValue);
end;

procedure TPrefs.SetMaxTiles(AValue: integer);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'MaxTiles', AValue);
end;

procedure TPrefs.SetMiddleMarker(AValue: integer);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'MiddleMarker', AValue);
end;

procedure TPrefs.SetMiddleMarkerColor(AValue: TColor);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'MiddleMarkerColor', AValue);
end;

procedure TPrefs.SetRouteColor(AValue: TColor);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'RouteColor', AValue);
end;

procedure TPrefs.SetSearchLang(AValue: string);
begin
  IniFile.WriteString(SECTION_GENERAL, 'DefaultLanguage', AValue);
end;

procedure TPrefs.SetStartPosLat(AValue: Double);
begin
  IniFile.WriteFloat(SECTION_GENERAL, 'StartLat', AValue);
end;

procedure TPrefs.SetStartPosLon(AValue: Double);
begin
  IniFile.WriteFloat(SECTION_GENERAL, 'StartLon', AValue);
end;

procedure TPrefs.SetStartZoom(AValue: Integer);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'StartZoom', AValue);
end;

procedure TPrefs.SetTrackColor(AValue: TColor);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'TrackColor', AValue);
end;

procedure TPrefs.SetViewProg(AValue: string);
begin
  IniFile.WriteString(SECTION_GENERAL, 'Viewer', AValue);
end;

procedure TPrefs.SetWaypointColor(AValue: TColor);
begin
  IniFile.WriteInteger(SECTION_GENERAL, 'WaypointColor', AValue);
end;

procedure TPrefs.SetStatWinOpen(AValue: Boolean);
begin
  IniFile.WriteBool(SECTION_WINDOW, 'Statistic', AValue);
end;

constructor TPrefs.Create;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.prefs'));
  IniFile.CacheUpdates := True;
end;

destructor TPrefs.Destroy;
begin
  IniFile.UpdateFile;
  IniFile.Free;
  inherited Destroy;
end;

initialization
  Prefs := TPrefs.Create;
finalization
  Prefs.Free;
end.

