unit MUIMappariumLocale;
{$mode objfpc}{$H+}
interface

{****************************************************

   This file was created automatically by 'FlexCat 2.18'
   from "locale/MUIMapparium.cd".

   Do NOT edit by hand!

****************************************************}

uses
  Exec , Locale , Utility ;

const
  MSG_ERROR_CUSTOM_CLASS = 1000 ;
  MSG_ERROR_CUSTOM_CLASS_STR = 'Could not create custom class.'#0;

  MSG_ERROR_SIDEPANEL = 1001 ;
  MSG_ERROR_SIDEPANEL_STR = 'Failed to create SidePanel.'#0;

  MSG_ERROR_APPLICATION = 1002 ;
  MSG_ERROR_APPLICATION_STR = 'Failed to create Application.'#0;

  MSG_ERROR_ONLINE = 1003 ;
  MSG_ERROR_ONLINE_STR = 'Not Online'#0;

  MSG_ERROR_NOTHINGFOUND = 1004 ;
  MSG_ERROR_NOTHINGFOUND_STR = 'Nothing found for "%s"'#0;

  MSG_ERROR_NETWORK = 1005 ;
  MSG_ERROR_NETWORK_STR = 'Network error'#0;

  MSG_ERROR_REXX_UNKNOWN = 1006 ;
  MSG_ERROR_REXX_UNKNOWN_STR = 'Unknown command'#0;

  MSG_ERROR_REXX_PARAM = 1007 ;
  MSG_ERROR_REXX_PARAM_STR = 'Wrong number of parameter: %s'#0;

  MSG_ERROR_REXX_ILLEGALPARAM = 1008 ;
  MSG_ERROR_REXX_ILLEGALPARAM_STR = 'Illegal parameter %d, should be a float value'#0;

  MSG_SEARCH_RESULTS_TITLE = 2000 ;
  MSG_SEARCH_RESULTS_TITLE_STR = 'Search Results'#0;

  MSG_FRAME_SEARCH = 2001 ;
  MSG_FRAME_SEARCH_STR = 'Search'#0;

  MSG_FRAME_WAYPOINTS = 2002 ;
  MSG_FRAME_WAYPOINTS_STR = 'Waypoints'#0;

  MSG_FRAME_TRACKS = 2003 ;
  MSG_FRAME_TRACKS_STR = 'Tracks'#0;

  MSG_SEARCH_RESULTS = 2004 ;
  MSG_SEARCH_RESULTS_STR = '%d Results for "%s"'#0;

  MSG_PREFS_TITLE = 2101 ;
  MSG_PREFS_TITLE_STR = 'Preferences'#0;

  MSG_PREFS_MIDDLETITLE = 2102 ;
  MSG_PREFS_MIDDLETITLE_STR = 'Middle position marker'#0;

  MSG_PREFS_MIDDLETYPE = 2103 ;
  MSG_PREFS_MIDDLETYPE_STR = 'Type/Size'#0;

  MSG_PREFS_LANGTITLE = 2104 ;
  MSG_PREFS_LANGTITLE_STR = 'Language'#0;

  MSG_PREFS_DEFLANG = 2105 ;
  MSG_PREFS_DEFLANG_STR = 'Default search result language'#0;

  MSG_PREFS_MEMORYTITLE = 2106 ;
  MSG_PREFS_MEMORYTITLE_STR = 'Memory'#0;

  MSG_PREFS_MAXTILES = 2107 ;
  MSG_PREFS_MAXTILES_STR = 'Max Tiles in Memory'#0;

  MSG_PREFS_HDFILES = 2108 ;
  MSG_PREFS_HDFILES_STR = 'Files on hard disk'#0;

  MSG_PREFS_BUTTONCOUNT = 2109 ;
  MSG_PREFS_BUTTONCOUNT_STR = 'Count'#0;

  MSG_PREFS_BUTTONCLEAR = 2110 ;
  MSG_PREFS_BUTTONCLEAR_STR = 'Clear'#0;

  MSG_PREFS_UPTOZOOM = 2111 ;
  MSG_PREFS_UPTOZOOM_STR = 'to Zoom'#0;

  MSG_PREFS_CACHEDDATA = 2112 ;
  MSG_PREFS_CACHEDDATA_STR = 'Cached Data:'#0;

  MSG_PREFS_MARKERTYPES = 2113 ;
  MSG_PREFS_MARKERTYPES_STR = 'None|Point|Cross|Lines'#0;

  MSG_STAT_WINTITLE = 2200 ;
  MSG_STAT_WINTITLE_STR = 'Statistics'#0;

  MSG_STAT_TITLE = 2201 ;
  MSG_STAT_TITLE_STR = 'Some Statistics:'#0;

  MSG_STAT_SESSION = 2202 ;
  MSG_STAT_SESSION_STR = 'This Session'#0;

  MSG_STAT_OVERALL = 2203 ;
  MSG_STAT_OVERALL_STR = 'Overall'#0;

  MSG_STAT_MEMTILES = 2204 ;
  MSG_STAT_MEMTILES_STR = 'Tiles in Memory'#0;

  MSG_STAT_NETTILES = 2205 ;
  MSG_STAT_NETTILES_STR = 'Tiles Loaded from Net'#0;

  MSG_STAT_HDTILES = 2206 ;
  MSG_STAT_HDTILES_STR = 'Tiles loaded from HD'#0;

  MSG_STAT_PENDINGTILES = 2207 ;
  MSG_STAT_PENDINGTILES_STR = 'Tiles to load'#0;

  MSG_STAT_DOWNLOADED = 2208 ;
  MSG_STAT_DOWNLOADED_STR = 'Bytes downloaded'#0;

  MSG_STAT_SPEED = 2209 ;
  MSG_STAT_SPEED_STR = 'Average Speed'#0;

  MSG_WAYPROP_TITLE = 2300 ;
  MSG_WAYPROP_TITLE_STR = 'Waypoint Properties'#0;

  MSG_WAYPROP_NAME = 2301 ;
  MSG_WAYPROP_NAME_STR = 'Waypoint Title'#0;

  MSG_WAYPROP_POS = 2302 ;
  MSG_WAYPROP_POS_STR = 'Position'#0;

  MSG_WAYPROP_CURRENTPOS = 2303 ;
  MSG_WAYPROP_CURRENTPOS_STR = 'Current Position'#0;

  MSG_TRACKPROP_TITLE = 2400 ;
  MSG_TRACKPROP_TITLE_STR = 'Track Properties'#0;

  MSG_TRACKPROP_NAME = 2401 ;
  MSG_TRACKPROP_NAME_STR = 'Track Title'#0;

  MSG_POPUP_WAYPOINT = 3000 ;
  MSG_POPUP_WAYPOINT_STR = 'Waypoint'#0;

  MSG_POPUP_WAYPOINT_TOGGLE = 3001 ;
  MSG_POPUP_WAYPOINT_TOGGLE_STR = 'Toggle visibility'#0;

  MSG_MENU_PROJECT = 3100 ;
  MSG_MENU_PROJECT_STR = 'Project'#0;

  MSG_MENU_MAIN_LOAD = 3101 ;
  MSG_MENU_MAIN_LOAD_STR = 'Load...'#0;

  MSG_MENU_MAIN_LOAD_KEY = 3102 ;
  MSG_MENU_MAIN_LOAD_KEY_STR = 'L'#0;

  MSG_MENU_MAIN_SAVE = 3103 ;
  MSG_MENU_MAIN_SAVE_STR = 'Save...'#0;

  MSG_MENU_MAIN_SAVE_KEY = 3104 ;
  MSG_MENU_MAIN_SAVE_KEY_STR = 'S'#0;

  MSG_MENU_MAIN_QUIT = 3105 ;
  MSG_MENU_MAIN_QUIT_STR = 'Quit'#0;

  MSG_MENU_MAIN_QUIT_KEY = 3106 ;
  MSG_MENU_MAIN_QUIT_KEY_STR = 'Q'#0;

  MSG_MENU_MAP = 3200 ;
  MSG_MENU_MAP_STR = 'Map'#0;

  MSG_MENU_MAP_FINDME = 3201 ;
  MSG_MENU_MAP_FINDME_STR = 'Find me'#0;

  MSG_MENU_MAP_ZOOMIN = 3202 ;
  MSG_MENU_MAP_ZOOMIN_STR = 'Zoom in'#0;

  MSG_MENU_MAP_ZOOMOUT = 3203 ;
  MSG_MENU_MAP_ZOOMOUT_STR = 'Zoom out'#0;

  MSG_MENU_WINDOW = 3300 ;
  MSG_MENU_WINDOW_STR = 'Window'#0;

  MSG_MENU_WINDOW_SIDEPANEL = 3301 ;
  MSG_MENU_WINDOW_SIDEPANEL_STR = 'Side Panel'#0;

  MSG_MENU_WINDOW_PREFS = 3302 ;
  MSG_MENU_WINDOW_PREFS_STR = 'Prefs'#0;

  MSG_MENU_WINDOW_STATISTICS = 3303 ;
  MSG_MENU_WINDOW_STATISTICS_STR = 'Statistics'#0;

  MSG_BUTTON_WAYPOINT_ADD = 4000 ;
  MSG_BUTTON_WAYPOINT_ADD_STR = 'Add'#0;

  MSG_BUTTON_WAYPOINT_REMOVE = 4001 ;
  MSG_BUTTON_WAYPOINT_REMOVE_STR = 'Remove'#0;

  MSG_BUTTON_WAYPOINT_EDIT = 4002 ;
  MSG_BUTTON_WAYPOINT_EDIT_STR = 'Edit'#0;

  MSG_BUTTON_TRACK_REMOVE = 4003 ;
  MSG_BUTTON_TRACK_REMOVE_STR = 'Remove'#0;

  MSG_BUTTON_TRACK_EDIT = 4004 ;
  MSG_BUTTON_TRACK_EDIT_STR = 'Edit'#0;

  MSG_GENERAL_LAT = 5000 ;
  MSG_GENERAL_LAT_STR = 'Lat:'#0;

  MSG_GENERAL_LON = 5001 ;
  MSG_GENERAL_LON_STR = 'Lon:'#0;

  MSG_GENERAL_SAVE = 5002 ;
  MSG_GENERAL_SAVE_STR = 'Save'#0;

  MSG_GENERAL_CLOSE = 5003 ;
  MSG_GENERAL_CLOSE_STR = 'Close'#0;

  MSG_GENERAL_CANCEL = 5004 ;
  MSG_GENERAL_CANCEL_STR = 'Cancel'#0;

  MSG_DEFAULT_WAYPOINT = 6000 ;
  MSG_DEFAULT_WAYPOINT_STR = 'Waypoint %d'#0;


procedure CloseCatalog;
procedure OpenCatalog(Loc: PLocale);
function GetLocString(Num: LongInt): PChar;

implementation

const
  Builtinlanguage = 'english'#0;
  Version = 0 ;
  Catalog: PCatalog = NIL ;

type

  TAppString = record
     id: LongInt;
     str: string;
  end;

  TAppStringArray = array[0..71] of TAppString;

const
  AppStrings: TAppStringArray = (
    (id: MSG_ERROR_CUSTOM_CLASS ; str: MSG_ERROR_CUSTOM_CLASS_STR ),
    (id: MSG_ERROR_SIDEPANEL ; str: MSG_ERROR_SIDEPANEL_STR ),
    (id: MSG_ERROR_APPLICATION ; str: MSG_ERROR_APPLICATION_STR ),
    (id: MSG_ERROR_ONLINE ; str: MSG_ERROR_ONLINE_STR ),
    (id: MSG_ERROR_NOTHINGFOUND ; str: MSG_ERROR_NOTHINGFOUND_STR ),
    (id: MSG_ERROR_NETWORK ; str: MSG_ERROR_NETWORK_STR ),
    (id: MSG_ERROR_REXX_UNKNOWN ; str: MSG_ERROR_REXX_UNKNOWN_STR ),
    (id: MSG_ERROR_REXX_PARAM ; str: MSG_ERROR_REXX_PARAM_STR ),
    (id: MSG_ERROR_REXX_ILLEGALPARAM ; str: MSG_ERROR_REXX_ILLEGALPARAM_STR ),
    (id: MSG_SEARCH_RESULTS_TITLE ; str: MSG_SEARCH_RESULTS_TITLE_STR ),
    (id: MSG_FRAME_SEARCH ; str: MSG_FRAME_SEARCH_STR ),
    (id: MSG_FRAME_WAYPOINTS ; str: MSG_FRAME_WAYPOINTS_STR ),
    (id: MSG_FRAME_TRACKS ; str: MSG_FRAME_TRACKS_STR ),
    (id: MSG_SEARCH_RESULTS ; str: MSG_SEARCH_RESULTS_STR ),
    (id: MSG_PREFS_TITLE ; str: MSG_PREFS_TITLE_STR ),
    (id: MSG_PREFS_MIDDLETITLE ; str: MSG_PREFS_MIDDLETITLE_STR ),
    (id: MSG_PREFS_MIDDLETYPE ; str: MSG_PREFS_MIDDLETYPE_STR ),
    (id: MSG_PREFS_LANGTITLE ; str: MSG_PREFS_LANGTITLE_STR ),
    (id: MSG_PREFS_DEFLANG ; str: MSG_PREFS_DEFLANG_STR ),
    (id: MSG_PREFS_MEMORYTITLE ; str: MSG_PREFS_MEMORYTITLE_STR ),
    (id: MSG_PREFS_MAXTILES ; str: MSG_PREFS_MAXTILES_STR ),
    (id: MSG_PREFS_HDFILES ; str: MSG_PREFS_HDFILES_STR ),
    (id: MSG_PREFS_BUTTONCOUNT ; str: MSG_PREFS_BUTTONCOUNT_STR ),
    (id: MSG_PREFS_BUTTONCLEAR ; str: MSG_PREFS_BUTTONCLEAR_STR ),
    (id: MSG_PREFS_UPTOZOOM ; str: MSG_PREFS_UPTOZOOM_STR ),
    (id: MSG_PREFS_CACHEDDATA ; str: MSG_PREFS_CACHEDDATA_STR ),
    (id: MSG_PREFS_MARKERTYPES ; str: MSG_PREFS_MARKERTYPES_STR ),
    (id: MSG_STAT_WINTITLE ; str: MSG_STAT_WINTITLE_STR ),
    (id: MSG_STAT_TITLE ; str: MSG_STAT_TITLE_STR ),
    (id: MSG_STAT_SESSION ; str: MSG_STAT_SESSION_STR ),
    (id: MSG_STAT_OVERALL ; str: MSG_STAT_OVERALL_STR ),
    (id: MSG_STAT_MEMTILES ; str: MSG_STAT_MEMTILES_STR ),
    (id: MSG_STAT_NETTILES ; str: MSG_STAT_NETTILES_STR ),
    (id: MSG_STAT_HDTILES ; str: MSG_STAT_HDTILES_STR ),
    (id: MSG_STAT_PENDINGTILES ; str: MSG_STAT_PENDINGTILES_STR ),
    (id: MSG_STAT_DOWNLOADED ; str: MSG_STAT_DOWNLOADED_STR ),
    (id: MSG_STAT_SPEED ; str: MSG_STAT_SPEED_STR ),
    (id: MSG_WAYPROP_TITLE ; str: MSG_WAYPROP_TITLE_STR ),
    (id: MSG_WAYPROP_NAME ; str: MSG_WAYPROP_NAME_STR ),
    (id: MSG_WAYPROP_POS ; str: MSG_WAYPROP_POS_STR ),
    (id: MSG_WAYPROP_CURRENTPOS ; str: MSG_WAYPROP_CURRENTPOS_STR ),
    (id: MSG_TRACKPROP_TITLE ; str: MSG_TRACKPROP_TITLE_STR ),
    (id: MSG_TRACKPROP_NAME ; str: MSG_TRACKPROP_NAME_STR ),
    (id: MSG_POPUP_WAYPOINT ; str: MSG_POPUP_WAYPOINT_STR ),
    (id: MSG_POPUP_WAYPOINT_TOGGLE ; str: MSG_POPUP_WAYPOINT_TOGGLE_STR ),
    (id: MSG_MENU_PROJECT ; str: MSG_MENU_PROJECT_STR ),
    (id: MSG_MENU_MAIN_LOAD ; str: MSG_MENU_MAIN_LOAD_STR ),
    (id: MSG_MENU_MAIN_LOAD_KEY ; str: MSG_MENU_MAIN_LOAD_KEY_STR ),
    (id: MSG_MENU_MAIN_SAVE ; str: MSG_MENU_MAIN_SAVE_STR ),
    (id: MSG_MENU_MAIN_SAVE_KEY ; str: MSG_MENU_MAIN_SAVE_KEY_STR ),
    (id: MSG_MENU_MAIN_QUIT ; str: MSG_MENU_MAIN_QUIT_STR ),
    (id: MSG_MENU_MAIN_QUIT_KEY ; str: MSG_MENU_MAIN_QUIT_KEY_STR ),
    (id: MSG_MENU_MAP ; str: MSG_MENU_MAP_STR ),
    (id: MSG_MENU_MAP_FINDME ; str: MSG_MENU_MAP_FINDME_STR ),
    (id: MSG_MENU_MAP_ZOOMIN ; str: MSG_MENU_MAP_ZOOMIN_STR ),
    (id: MSG_MENU_MAP_ZOOMOUT ; str: MSG_MENU_MAP_ZOOMOUT_STR ),
    (id: MSG_MENU_WINDOW ; str: MSG_MENU_WINDOW_STR ),
    (id: MSG_MENU_WINDOW_SIDEPANEL ; str: MSG_MENU_WINDOW_SIDEPANEL_STR ),
    (id: MSG_MENU_WINDOW_PREFS ; str: MSG_MENU_WINDOW_PREFS_STR ),
    (id: MSG_MENU_WINDOW_STATISTICS ; str: MSG_MENU_WINDOW_STATISTICS_STR ),
    (id: MSG_BUTTON_WAYPOINT_ADD ; str: MSG_BUTTON_WAYPOINT_ADD_STR ),
    (id: MSG_BUTTON_WAYPOINT_REMOVE ; str: MSG_BUTTON_WAYPOINT_REMOVE_STR ),
    (id: MSG_BUTTON_WAYPOINT_EDIT ; str: MSG_BUTTON_WAYPOINT_EDIT_STR ),
    (id: MSG_BUTTON_TRACK_REMOVE ; str: MSG_BUTTON_TRACK_REMOVE_STR ),
    (id: MSG_BUTTON_TRACK_EDIT ; str: MSG_BUTTON_TRACK_EDIT_STR ),
    (id: MSG_GENERAL_LAT ; str: MSG_GENERAL_LAT_STR ),
    (id: MSG_GENERAL_LON ; str: MSG_GENERAL_LON_STR ),
    (id: MSG_GENERAL_SAVE ; str: MSG_GENERAL_SAVE_STR ),
    (id: MSG_GENERAL_CLOSE ; str: MSG_GENERAL_CLOSE_STR ),
    (id: MSG_GENERAL_CANCEL ; str: MSG_GENERAL_CANCEL_STR ),
    (id: MSG_DEFAULT_WAYPOINT ; str: MSG_DEFAULT_WAYPOINT_STR ),
    (id: 0 ; str: '' )
    );

procedure CloseCatalog;
begin
  if Assigned(Catalog) then
  begin
    Locale.CloseCatalog(Catalog) ;
    Catalog := nil;
  end;
end;

procedure OpenCatalog(loc: PLocale);
var
   tags: array[0..7] of PtrUInt;
begin
  CloseCatalog;
  if (Catalog = nil) and (LocaleBase <> NIL) then
  begin
    tags[0] := OC_BuiltInLanguage; tags[1] := AsTag(PChar(builtinlanguage));
    tags[2] := OC_Version;         tags[3] := Version;
    tags[4] := TAG_END;
  end;
  Catalog := Locale.OpenCatalogA(loc, PChar('MUIMapparium.catalog'#0), @tags);
end;

function GetLocString(Num: LongInt): STRPTR;
var
  i: LongInt;
  Idx: Integer;
  Default: STRPTR;
begin
  Idx := 0;

  for i := 1 to High(Appstrings) do
  begin
    if AppStrings[i].id = Num then
    begin
      Idx := i;
      Break;
    end;
  end;

  if Idx > 0 then
    Default := PChar(AppStrings[i].str)
  else
    Default := nil;

  if Assigned(Catalog) then
    GetLocString := Locale.GetCatalogStr(Catalog, Num, Default)
  else
    GetLocString := Default
end;

initialization
  OpenCatalog(nil);
finalization
  CloseCatalog;
end.
