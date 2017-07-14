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
  MSG_ERROR_CUSTOM_CLASS = 10000 ;
  MSG_ERROR_CUSTOM_CLASS_STR = 'Could not create custom class.'#0;

  MSG_ERROR_SIDEPANEL = 100001 ;
  MSG_ERROR_SIDEPANEL_STR = 'Failed to create SidePanel.'#0;

  MSG_ERROR_APPLICATION = 10002 ;
  MSG_ERROR_APPLICATION_STR = 'Failed to create Application.'#0;

  MSG_ERROR_ONLINE = 10003 ;
  MSG_ERROR_ONLINE_STR = 'Not Online'#0;

  MSG_ERROR_NOTHINGFOUND = 10004 ;
  MSG_ERROR_NOTHINGFOUND_STR = 'Nothing found for "%s"'#0;

  MSG_ERROR_NETWORK = 10005 ;
  MSG_ERROR_NETWORK_STR = 'Network error'#0;

  MSG_ERROR_REXX_UNKNOWN = 10006 ;
  MSG_ERROR_REXX_UNKNOWN_STR = 'Unknown command'#0;

  MSG_ERROR_REXX_PARAM = 10007 ;
  MSG_ERROR_REXX_PARAM_STR = 'Wrong number of parameter: %s'#0;

  MSG_ERROR_REXX_ILLEGALPARAM = 10008 ;
  MSG_ERROR_REXX_ILLEGALPARAM_STR = 'Illegal parameter %d, should be a float value'#0;

  MSG_SEARCH_RESULTS_TITLE = 20000 ;
  MSG_SEARCH_RESULTS_TITLE_STR = 'Search Results'#0;

  MSG_FRAME_SEARCH = 20001 ;
  MSG_FRAME_SEARCH_STR = 'Search'#0;

  MSG_FRAME_WAYPOINTS = 20002 ;
  MSG_FRAME_WAYPOINTS_STR = 'Waypoints'#0;

  MSG_FRAME_TRACKS = 20003 ;
  MSG_FRAME_TRACKS_STR = 'Tracks'#0;

  MSG_SEARCH_RESULTS = 20004 ;
  MSG_SEARCH_RESULTS_STR = '%d Results for "%s"'#0;

  MSG_FRAME_ROUTES = 20005 ;
  MSG_FRAME_ROUTES_STR = 'Routes'#0;

  MSG_FRAME_IMAGES = 20006 ;
  MSG_FRAME_IMAGES_STR = 'Images'#0;

  MSG_PREFS_TITLE = 20101 ;
  MSG_PREFS_TITLE_STR = 'Preferences'#0;

  MSG_PREFS_MIDDLETITLE = 20102 ;
  MSG_PREFS_MIDDLETITLE_STR = 'Middle position marker'#0;

  MSG_PREFS_MIDDLETYPE = 20103 ;
  MSG_PREFS_MIDDLETYPE_STR = 'Type/Size'#0;

  MSG_PREFS_LANGTITLE = 20104 ;
  MSG_PREFS_LANGTITLE_STR = 'Language'#0;

  MSG_PREFS_DEFLANG = 20105 ;
  MSG_PREFS_DEFLANG_STR = 'Default search result language'#0;

  MSG_PREFS_MEMORYTITLE = 20106 ;
  MSG_PREFS_MEMORYTITLE_STR = 'Memory'#0;

  MSG_PREFS_MAXTILES = 20107 ;
  MSG_PREFS_MAXTILES_STR = 'Max Tiles in Memory'#0;

  MSG_PREFS_HDFILES = 20108 ;
  MSG_PREFS_HDFILES_STR = 'Files on hard disk'#0;

  MSG_PREFS_BUTTONCOUNT = 20109 ;
  MSG_PREFS_BUTTONCOUNT_STR = 'Count'#0;

  MSG_PREFS_BUTTONCLEAR = 20110 ;
  MSG_PREFS_BUTTONCLEAR_STR = 'Clear'#0;

  MSG_PREFS_UPTOZOOM = 20111 ;
  MSG_PREFS_UPTOZOOM_STR = 'to Zoom'#0;

  MSG_PREFS_CACHEDDATA = 20112 ;
  MSG_PREFS_CACHEDDATA_STR = 'Cached Data: %s in %d files.'#0;

  MSG_PREFS_DELETEDATA = 20113 ;
  MSG_PREFS_DELETEDATA_STR = '%d deleted, %d kept.'#0;

  MSG_PREFS_MARKERTYPES = 20114 ;
  MSG_PREFS_MARKERTYPES_STR = 'None|Point|Cross|Lines'#0;

  MSG_STAT_WINTITLE = 20200 ;
  MSG_STAT_WINTITLE_STR = 'Statistics'#0;

  MSG_STAT_TITLE = 20201 ;
  MSG_STAT_TITLE_STR = 'Some Statistics:'#0;

  MSG_STAT_SESSION = 20202 ;
  MSG_STAT_SESSION_STR = 'This Session'#0;

  MSG_STAT_OVERALL = 20203 ;
  MSG_STAT_OVERALL_STR = 'Overall'#0;

  MSG_STAT_MEMTILES = 20204 ;
  MSG_STAT_MEMTILES_STR = 'Tiles in Memory'#0;

  MSG_STAT_NETTILES = 20205 ;
  MSG_STAT_NETTILES_STR = 'Tiles Loaded from Net'#0;

  MSG_STAT_HDTILES = 20206 ;
  MSG_STAT_HDTILES_STR = 'Tiles loaded from HD'#0;

  MSG_STAT_PENDINGTILES = 20207 ;
  MSG_STAT_PENDINGTILES_STR = 'Tiles to load'#0;

  MSG_STAT_DOWNLOADED = 20208 ;
  MSG_STAT_DOWNLOADED_STR = 'Bytes downloaded'#0;

  MSG_STAT_SPEED = 20209 ;
  MSG_STAT_SPEED_STR = 'Average Speed'#0;

  MSG_WAYPROP_TITLE = 20300 ;
  MSG_WAYPROP_TITLE_STR = 'Waypoint Properties'#0;

  MSG_WAYPROP_NAME = 20301 ;
  MSG_WAYPROP_NAME_STR = 'Waypoint Title'#0;

  MSG_WAYPROP_POS = 20302 ;
  MSG_WAYPROP_POS_STR = 'Position'#0;

  MSG_WAYPROP_CURRENTPOS = 20303 ;
  MSG_WAYPROP_CURRENTPOS_STR = 'Current Position'#0;

  MSG_TRACKPROP_TITLE = 20400 ;
  MSG_TRACKPROP_TITLE_STR = 'Track Properties'#0;

  MSG_TRACKPROP_NAME = 20401 ;
  MSG_TRACKPROP_NAME_STR = 'Track Title'#0;

  MSG_TRACKPROP_XAXIS = 20402 ;
  MSG_TRACKPROP_XAXIS_STR = 'X Axis'#0;

  MSG_TRACKPROP_LEFTAXIS = 20403 ;
  MSG_TRACKPROP_LEFTAXIS_STR = 'Left Axis'#0;

  MSG_TRACKPROP_RIGHTAXIS = 20404 ;
  MSG_TRACKPROP_RIGHTAXIS_STR = 'Right Axis'#0;

  MSG_TRACKPROP_DRAW = 20405 ;
  MSG_TRACKPROP_DRAW_STR = 'Draw'#0;

  MSG_TRACKPROP_TIME = 20406 ;
  MSG_TRACKPROP_TIME_STR = 'Time'#0;

  MSG_TRACKPROP_DISTANCE = 20407 ;
  MSG_TRACKPROP_DISTANCE_STR = 'Distance'#0;

  MSG_TRACKPROP_NONE = 20408 ;
  MSG_TRACKPROP_NONE_STR = 'None'#0;

  MSG_TRACKPROP_HEIGHT = 20409 ;
  MSG_TRACKPROP_HEIGHT_STR = 'Height'#0;

  MSG_TRACKPROP_SLOPE = 20410 ;
  MSG_TRACKPROP_SLOPE_STR = 'Slope'#0;

  MSG_TRACKPROP_SPEED = 20411 ;
  MSG_TRACKPROP_SPEED_STR = 'Speed'#0;

  MSG_PLOT_TITLE = 20500 ;
  MSG_PLOT_TITLE_STR = 'Plot'#0;

  MSG_PLOT_RESCALE = 20501 ;
  MSG_PLOT_RESCALE_STR = 'Rescale'#0;

  MSG_PLOT_ZOOM = 20502 ;
  MSG_PLOT_ZOOM_STR = 'Zoom'#0;

  MSG_PLOT_POSITION = 20503 ;
  MSG_PLOT_POSITION_STR = 'Position Data'#0;

  MSG_PLOT_DATA = 20504 ;
  MSG_PLOT_DATA_STR = 'Curve Data'#0;

  MSG_ROUTEPROP_TITLE = 20600 ;
  MSG_ROUTEPROP_TITLE_STR = 'Route Properties'#0;

  MSG_ROUTEPROP_NAME = 20601 ;
  MSG_ROUTEPROP_NAME_STR = 'Route Title'#0;

  MSG_ROUTEPROP_NEW_NAME = 20602 ;
  MSG_ROUTEPROP_NEW_NAME_STR = 'New Route'#0;

  MSG_POPUP_WAYPOINT = 30000 ;
  MSG_POPUP_WAYPOINT_STR = 'Waypoint'#0;

  MSG_POPUP_WAYPOINT_TOGGLE = 30001 ;
  MSG_POPUP_WAYPOINT_TOGGLE_STR = 'Toggle visibility'#0;

  MSG_MENU_PROJECT = 30100 ;
  MSG_MENU_PROJECT_STR = 'Project'#0;

  MSG_MENU_MAIN_LOAD = 30101 ;
  MSG_MENU_MAIN_LOAD_STR = 'Load...'#0;

  MSG_MENU_MAIN_LOAD_KEY = 30102 ;
  MSG_MENU_MAIN_LOAD_KEY_STR = 'L'#0;

  MSG_MENU_MAIN_SAVE = 30103 ;
  MSG_MENU_MAIN_SAVE_STR = 'Save...'#0;

  MSG_MENU_MAIN_SAVE_KEY = 30104 ;
  MSG_MENU_MAIN_SAVE_KEY_STR = 'S'#0;

  MSG_MENU_MAIN_QUIT = 30105 ;
  MSG_MENU_MAIN_QUIT_STR = 'Quit'#0;

  MSG_MENU_MAIN_QUIT_KEY = 30106 ;
  MSG_MENU_MAIN_QUIT_KEY_STR = 'Q'#0;

  MSG_MENU_MAP = 30200 ;
  MSG_MENU_MAP_STR = 'Map'#0;

  MSG_MENU_MAP_FINDME = 30201 ;
  MSG_MENU_MAP_FINDME_STR = 'Find me'#0;

  MSG_MENU_MAP_ZOOMIN = 30202 ;
  MSG_MENU_MAP_ZOOMIN_STR = 'Zoom in'#0;

  MSG_MENU_MAP_ZOOMOUT = 30203 ;
  MSG_MENU_MAP_ZOOMOUT_STR = 'Zoom out'#0;

  MSG_MENU_MAP_DRAWMARKER = 30204 ;
  MSG_MENU_MAP_DRAWMARKER_STR = 'Draw Marker'#0;

  MSG_MENU_MAP_DRAWTRACKS = 30205 ;
  MSG_MENU_MAP_DRAWTRACKS_STR = 'Draw Tracks'#0;

  MSG_MENU_MAP_DRAWROUTES = 30206 ;
  MSG_MENU_MAP_DRAWROUTES_STR = 'Draw Routes'#0;

  MSG_MENU_WINDOW = 30300 ;
  MSG_MENU_WINDOW_STR = 'Window'#0;

  MSG_MENU_WINDOW_SIDEPANEL = 30301 ;
  MSG_MENU_WINDOW_SIDEPANEL_STR = 'Side Panel'#0;

  MSG_MENU_WINDOW_PREFS = 30302 ;
  MSG_MENU_WINDOW_PREFS_STR = 'Prefs'#0;

  MSG_MENU_WINDOW_STATISTICS = 30303 ;
  MSG_MENU_WINDOW_STATISTICS_STR = 'Statistics'#0;

  MSG_MENU_ABOUT = 30400 ;
  MSG_MENU_ABOUT_STR = 'About'#0;

  MSG_MENU_ABOUT_UPDATE = 30401 ;
  MSG_MENU_ABOUT_UPDATE_STR = 'Check for Updates'#0;

  MSG_MENU_ABOUT_HELP = 30402 ;
  MSG_MENU_ABOUT_HELP_STR = 'Help'#0;

  MSG_BUTTON_WAYPOINT_ADD = 40000 ;
  MSG_BUTTON_WAYPOINT_ADD_STR = 'Add'#0;

  MSG_BUTTON_WAYPOINT_REMOVE = 40001 ;
  MSG_BUTTON_WAYPOINT_REMOVE_STR = 'Remove'#0;

  MSG_BUTTON_WAYPOINT_EDIT = 40002 ;
  MSG_BUTTON_WAYPOINT_EDIT_STR = 'Edit'#0;

  MSG_BUTTON_TRACK_REMOVE = 40003 ;
  MSG_BUTTON_TRACK_REMOVE_STR = 'Remove'#0;

  MSG_BUTTON_TRACK_EDIT = 40004 ;
  MSG_BUTTON_TRACK_EDIT_STR = 'Edit'#0;

  MSG_GENERAL_LAT = 50000 ;
  MSG_GENERAL_LAT_STR = 'Lat:'#0;

  MSG_GENERAL_LON = 50001 ;
  MSG_GENERAL_LON_STR = 'Lon:'#0;

  MSG_GENERAL_SAVE = 50002 ;
  MSG_GENERAL_SAVE_STR = 'Save'#0;

  MSG_GENERAL_CLOSE = 50003 ;
  MSG_GENERAL_CLOSE_STR = 'Close'#0;

  MSG_GENERAL_CANCEL = 50004 ;
  MSG_GENERAL_CANCEL_STR = 'Cancel'#0;

  MSG_GENERAL_OK = 50005 ;
  MSG_GENERAL_OK_STR = 'OK'#0;

  MSG_DEFAULT_WAYPOINT = 60000 ;
  MSG_DEFAULT_WAYPOINT_STR = 'Waypoint %d'#0;

  MSG_UPDATE_CHECK = 70000 ;
  MSG_UPDATE_CHECK_STR = 'Update Check'#0;

  MSG_UPDATE_ERROR = 70001 ;
  MSG_UPDATE_ERROR_STR = 'Error to get update information'#0;

  MSG_UPDATE_INFO = 70002 ;
  MSG_UPDATE_INFO_STR = '\ebUpdate available\en\n  Online available: \et$OLDVER$\n  Your Version:     \et$NEWVER$\nCheck \eb$URL$\en for downloading updates.'#0;

  MSG_UPDATE_NONE = 70003 ;
  MSG_UPDATE_NONE_STR = 'No Update available.'#0;


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

  TAppStringArray = array[0..103] of TAppString;

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
    (id: MSG_FRAME_ROUTES ; str: MSG_FRAME_ROUTES_STR ),
    (id: MSG_FRAME_IMAGES ; str: MSG_FRAME_IMAGES_STR ),
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
    (id: MSG_PREFS_DELETEDATA ; str: MSG_PREFS_DELETEDATA_STR ),
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
    (id: MSG_TRACKPROP_XAXIS ; str: MSG_TRACKPROP_XAXIS_STR ),
    (id: MSG_TRACKPROP_LEFTAXIS ; str: MSG_TRACKPROP_LEFTAXIS_STR ),
    (id: MSG_TRACKPROP_RIGHTAXIS ; str: MSG_TRACKPROP_RIGHTAXIS_STR ),
    (id: MSG_TRACKPROP_DRAW ; str: MSG_TRACKPROP_DRAW_STR ),
    (id: MSG_TRACKPROP_TIME ; str: MSG_TRACKPROP_TIME_STR ),
    (id: MSG_TRACKPROP_DISTANCE ; str: MSG_TRACKPROP_DISTANCE_STR ),
    (id: MSG_TRACKPROP_NONE ; str: MSG_TRACKPROP_NONE_STR ),
    (id: MSG_TRACKPROP_HEIGHT ; str: MSG_TRACKPROP_HEIGHT_STR ),
    (id: MSG_TRACKPROP_SLOPE ; str: MSG_TRACKPROP_SLOPE_STR ),
    (id: MSG_TRACKPROP_SPEED ; str: MSG_TRACKPROP_SPEED_STR ),
    (id: MSG_PLOT_TITLE ; str: MSG_PLOT_TITLE_STR ),
    (id: MSG_PLOT_RESCALE ; str: MSG_PLOT_RESCALE_STR ),
    (id: MSG_PLOT_ZOOM ; str: MSG_PLOT_ZOOM_STR ),
    (id: MSG_PLOT_POSITION ; str: MSG_PLOT_POSITION_STR ),
    (id: MSG_PLOT_DATA ; str: MSG_PLOT_DATA_STR ),
    (id: MSG_ROUTEPROP_TITLE ; str: MSG_ROUTEPROP_TITLE_STR ),
    (id: MSG_ROUTEPROP_NAME ; str: MSG_ROUTEPROP_NAME_STR ),
    (id: MSG_ROUTEPROP_NEW_NAME ; str: MSG_ROUTEPROP_NEW_NAME_STR ),
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
    (id: MSG_MENU_MAP_DRAWMARKER ; str: MSG_MENU_MAP_DRAWMARKER_STR ),
    (id: MSG_MENU_MAP_DRAWTRACKS ; str: MSG_MENU_MAP_DRAWTRACKS_STR ),
    (id: MSG_MENU_MAP_DRAWROUTES ; str: MSG_MENU_MAP_DRAWROUTES_STR ),
    (id: MSG_MENU_WINDOW ; str: MSG_MENU_WINDOW_STR ),
    (id: MSG_MENU_WINDOW_SIDEPANEL ; str: MSG_MENU_WINDOW_SIDEPANEL_STR ),
    (id: MSG_MENU_WINDOW_PREFS ; str: MSG_MENU_WINDOW_PREFS_STR ),
    (id: MSG_MENU_WINDOW_STATISTICS ; str: MSG_MENU_WINDOW_STATISTICS_STR ),
    (id: MSG_MENU_ABOUT ; str: MSG_MENU_ABOUT_STR ),
    (id: MSG_MENU_ABOUT_UPDATE ; str: MSG_MENU_ABOUT_UPDATE_STR ),
    (id: MSG_MENU_ABOUT_HELP ; str: MSG_MENU_ABOUT_HELP_STR ),
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
    (id: MSG_GENERAL_OK ; str: MSG_GENERAL_OK_STR ),
    (id: MSG_DEFAULT_WAYPOINT ; str: MSG_DEFAULT_WAYPOINT_STR ),
    (id: MSG_UPDATE_CHECK ; str: MSG_UPDATE_CHECK_STR ),
    (id: MSG_UPDATE_ERROR ; str: MSG_UPDATE_ERROR_STR ),
    (id: MSG_UPDATE_INFO ; str: MSG_UPDATE_INFO_STR ),
    (id: MSG_UPDATE_NONE ; str: MSG_UPDATE_NONE_STR ),
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
    tags[0] := OC_BuiltInLanguage; tags[1] := 0; //AsTag(PChar(builtinlanguage));
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
