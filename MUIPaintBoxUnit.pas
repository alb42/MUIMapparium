unit MUIPaintBoxUnit;
{$mode objfpc}{$H+}
interface

uses
  {$if defined(MorphOS) or defined(Amiga68k)}
  amigalib,
  {$endif}
  Classes, Math, Sysutils,
  Exec, Utility, Intuition, inputevent, AGraphics, mui, muihelper, keymap;
type
  TMUIMouseBtn = (mmbLeft, mmbMiddle, mmbRight);
  TMUIShiftState = set of (mssShift, mssCtrl, mssLShift, mssRShift, mssLAlt, mssRAlt);

  TMUIDrawEvent = procedure(Sender: TObject; Rp: PRastPort; DrawRect: TRect) of object;
  TMUIMouseEvent = procedure(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean) of object;
  TMUIMouseWheel = procedure(Sender: TObject; ScrollUp: Boolean; var EatEvent: Boolean) of object;
  TMUIMouseMove = procedure(Sender: TObject; X,Y: Integer; var EatEvent: Boolean) of object;
  TMUIKeyDown = procedure(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean) of object;
  TMUIKeyUp = procedure(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean) of object;

  TMouseClickTime = record
    LSecs, LMicros: LongWord;
    MSecs, MMicros: LongWord;
    RSecs, RMicros: LongWord;
  end;

  TMUIPaintBox = class
  private
    // Min Max
    FMinWidth: Integer;
    FMinHeight: Integer;
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    FDefWidth: Integer;
    FDefHeight: Integer;
    FMUIObject: PObject_;
    FMouseClickTime: TMouseClickTime;
    EHNode: TMUI_EventHandlerNode;
    FMouseInObject: Boolean;
    // Events
    FOnDrawObject: TMUIDrawEvent;
    FOnMUIMouseDown: TMUIMouseEvent;
    FOnMUIMouseUp: TMUIMouseEvent;
    FOnMUIDblClick: TMUIMouseEvent;
    FOnMUIMouseWheel: TMUIMouseWheel;
    FOnMUIMouseMove: TMUIMouseMove;
    FOnMUIKeyDown: TMUIKeyDown;
    FOnMUIKeyUp: TMUIKeyUp;
    FOnMUIMouseLeave: TNotifyEvent;
  private
    function GetWidth: Integer;
    function GetHeight: Integer;
  protected
    function MUIEvent(cl: PIClass; Obj: PObject_; Msg: intuition.PMsg): PtrUInt; virtual;
    //
    function DoSetup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Setup): PtrUInt; virtual;
    function DoCleanup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Cleanup): PtrUInt; virtual;
    function DoAskMinMax(cl: PIClass; Obj: PObject_; Msg: PMUIP_AskMinMax): PtrUInt; virtual;
    function DoDraw(cl: PIClass; Obj: PObject_; Msg: PMUIP_Draw): PtrUInt; virtual;
    function DoHandleEvent(cl: PIClass; Obj: PObject_; Msg: PMUIP_HandleEvent): PtrUInt; virtual;

    procedure ResetDblClickTime;
  public
    constructor Create(const Args: array of PtrUInt); virtual;

    procedure RedrawObject;

    property MinWidth: Integer read FMinWidth write FMinWidth;
    property MinHeight: Integer read FMinHeight write FMinHeight;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;
    property MaxHeight: Integer read FMaxHeight write FMaxHeight;
    property DefWidth: Integer read FDefWidth write FDefWidth;
    property DefHeight: Integer read FDefHeight write FDefHeight;

    // Events
    property OnDrawObject: TMUIDrawEvent read FOnDrawObject write FOnDrawObject;
    property OnMUIMouseDown: TMUIMouseEvent read FOnMUIMouseDown write FOnMUIMouseDown;
    property OnMUIMouseUp: TMUIMouseEvent read FOnMUIMouseUp write FOnMUIMouseUp;
    property OnMUIDblClick: TMUIMouseEvent read FOnMUIDblClick write FOnMUIDblClick;
    property OnMUIMouseWheel: TMUIMouseWheel read FOnMUIMouseWheel write FOnMUIMouseWheel;
    property OnMUIMouseMove: TMUIMouseMove read FOnMUIMouseMove write FOnMUIMouseMove;
    property OnMUIMouseLeave: TNotifyEvent read FOnMUIMouseLeave write FOnMUIMouseLeave;
    property OnMUIKeyDown: TMUIKeyDown read FOnMUIKeyDown write FOnMUIKeyDown;
    property OnMUIKeyUp: TMUIKeyUp read FOnMUIKeyUp write FOnMUIKeyUp;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    // use this Object for the mui creation
    property MUIObject: PObject_ read FMUIObject;
  end;

var
  MUIPBType: PMUI_CustomClass = nil;

implementation

// Constructor
constructor TMUIPaintBox.Create(const Args: array of PtrUInt);
begin
  inherited Create;
  FMouseInObject := False;
  // Basic min max settings
  FMinWidth := 0;
  FMinHeight := 0;
  FMaxWidth := MUI_MAXMAX;
  FMaxHeight := MUI_MAXMAX;
  FDefWidth := 100;
  FDefHeight := 100;
  // Create the Object
  MH_NewObject(FMUIObject, MUIPBType^.mcc_Class, nil, [
    //MUIA_Weight, 200,
    MUIA_Frame, MUIV_Frame_Text,
    MUIA_Background, MUII_BACKGROUND,
    MUIA_Font, AsTag(MUIV_Font_Button),
    MUIA_FillArea, AsTag(False),
    MUIA_InnerLeft, 0,
    MUIA_InnerTop, 0,
    MUIA_InnerBottom, 0,
    MUIA_InnerRight, 0,
    MUIA_UserData, AsTag(Self),        // thats me ;)
    TAG_MORE, AsTag(@Args[0]),         // if the user supply some more tags
    TAG_DONE]);
  // Put the Object into the INST_DATA
  Pointer(INST_DATA(MUIPBType^.mcc_Class, Pointer(FMUIObject))^) := Self;
end;

// Get actual width
function TMUIPaintBox.GetWidth: Integer;
begin
  // use DefWidth if MuiObject not there (should never happen)
  Result := FDefWidth;
  if Assigned(FMUIObject) then
    Result := Obj_Width(FMUIObject);
end;

// get actual height
function TMUIPaintBox.GetHeight: Integer;
begin
  Result := FDefHeight;
  if Assigned(FMUIObject) then
    Result := Obj_Height(FMUIObject);
end;

// Call to redraw the object
procedure TMUIPaintBox.RedrawObject;
begin
  if Assigned(FMUIObject) then
    MUI_Redraw(FMUIObject, MADF_DRAWOBJECT);
end;

// OM_SETUP
function TMUIPaintBox.DoSetup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Setup): PtrUInt;
begin
  Result := DoSuperMethodA(cl, obj, msg);
  EHNode.ehn_Priority := 0;
  EHNode.ehn_Flags := 0;
  EHNode.ehn_Object := obj;
  EHNode.ehn_Class := cl;
  EHNode.ehn_Events := IDCMP_MOUSEBUTTONS or IDCMP_MOUSEMOVE or IDCMP_RAWKEY;
  {$ifdef AmigaOS4}
  EHNode.ehn_Events := EHNode.ehn_Events or IDCMP_EXTENDEDMOUSE;
  {$endif}
  DoMethod(OBJ_win(obj), [MUIM_Window_AddEventHandler, PtrUInt(@EHNode)]);
end;

// OM_CLEANUP
function TMUIPaintBox.DoCleanup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Cleanup): PtrUInt;
begin
  DoMethod(OBJ_win(obj), [MUIM_Window_RemEventHandler, PtrUInt(@EHNode)]);
  Result := DoSuperMethodA(cl,obj,msg);
end;

// MUIM_ASKMINMAX
function TMUIPaintBox.DoAskMinMax(cl: PIClass; Obj: PObject_; Msg: PMUIP_AskMinMax): PtrUInt;
begin
  // let our superclass first fill in what it thinks about sizes.
  // this will e.g. add the size of frame and inner spacing.
  Result := DoSuperMethodA(cl, obj, msg);
  // now add the values specific to our object. note that we
  // indeed need to *add* these values, not just set them!
  msg^.MinMaxInfo^.MinWidth  := msg^.MinMaxInfo^.MinWidth + FMinWidth;
  msg^.MinMaxInfo^.DefWidth  := msg^.MinMaxInfo^.DefWidth + FDefHeight;
  msg^.MinMaxInfo^.MaxWidth  := IfThen(msg^.MinMaxInfo^.MaxWidth + FMaxWidth >= MUI_MAXMAX, MUI_MAXMAX, msg^.MinMaxInfo^.MaxWidth + FMaxWidth);

  msg^.MinMaxInfo^.MinHeight := msg^.MinMaxInfo^.MinHeight + FMinHeight;
  msg^.MinMaxInfo^.DefHeight := msg^.MinMaxInfo^.DefHeight + FDefHeight;
  msg^.MinMaxInfo^.MaxHeight := IfThen(msg^.MinMaxInfo^.MaxHeight + FMaxHeight >= MUI_MAXMAX, MUI_MAXMAX, msg^.MinMaxInfo^.MaxHeight + FMaxHeight);
end;

// MUIM_DRAW
function TMUIPaintBox.DoDraw(cl: PIClass; Obj: PObject_; Msg: PMUIP_Draw): PtrUInt;
var
  Clip: Pointer;
  Ri: PMUI_RenderInfo;
  Rp: PRastPort;
  DrawRect: TRect;
begin
  // let it draw itself
  Result := DoSuperMethodA(cl,obj,msg);
  // if MADF_DRAWOBJECT isn't set, we shouldn't draw anything.
  // MUI just wanted to update the frame or something like that.
  if (Msg^.flags and MADF_DRAWOBJECT) = 0 then
    Exit;
  // get render info
  Ri := MUIRenderInfo(Obj);
  if not Assigned(Ri) then
    Exit;
  // get rastport for drawing
  Rp := Obj_Rp(Obj);
  if not Assigned(Rp) then
    Exit;

  DrawRect.Left := Obj_mLeft(Obj);
  DrawRect.Top := Obj_mTop(Obj);
  DrawRect.Width := Obj_mWidth(Obj);
  DrawRect.Height := Obj_mHeight(Obj);
  // install the clip region (do not draw over the border)
  clip := MUI_AddClipping(Ri, DrawRect.Left, DrawRect.Top, DrawRect.Width, DrawRect.Height);
  if Assigned(FOnDrawObject) then
    FOnDrawObject(Self, Rp, DrawRect);
  MUI_RemoveClipRegion(Ri, Clip);
end;

procedure TMUIPaintBox.ResetDblClickTime;
begin
  FMouseClickTime.LSecs := 0;
  FMouseClickTime.LMicros := 0;
  FMouseClickTime.MSecs := 0;
  FMouseClickTime.MMicros := 0;
  FMouseClickTime.RSecs := 0;
  FMouseClickTime.RMicros := 0;
end;

// MUIM_HANDLEEVENT
function TMUIPaintBox.DoHandleEvent(cl: PIClass; Obj: PObject_; Msg: PMUIP_HandleEvent): PtrUInt;
var
  InObject: Boolean;
  EatMe: Boolean;
  RelX, RelY: Integer;
  // Keys
  Code: Word;
  IsKeyUp: Boolean;
  Qual: Word;
  Mss: TMUIShiftState;
  Buff: array[0..10] of char;
  ie: TInputEvent;
  {$ifdef AmigaOS4}
  WheelData: PIntuiWheelData;
  {$endif}
begin
  Result := DoSuperMethodA(cl,obj,msg);
  EatMe := False;
  // is the Pointer in the Object? mouse down only accept there
  InObject :=  OBJ_IsInObject(Msg^.Imsg^.MouseX, Msg^.Imsg^.MouseY, Obj) and not Boolean(MH_Get(Obj_Win(Obj), MUIA_Window_Sleep));
  case Msg^.imsg^.IClass of
    // Mouse Buttons
    IDCMP_MOUSEBUTTONS:
    begin
      RelX := Msg^.imsg^.MouseX - obj_Left(obj);
      RelY := Msg^.imsg^.MouseY - obj_Top(obj);
      case Msg^.imsg^.Code of
        SELECTDOWN:
        begin
          if not InObject then
            Exit;
          if Assigned(FOnMUIMouseDown) then
            FOnMUIMouseDown(Self, mmbLeft, RelX, RelY, EatMe);
        end;
        SELECTUP:
        begin
          if Assigned(FOnMUIMouseUp) then
            FOnMUIMouseUp(Self, mmbLeft, RelX, RelY, EatMe);
          if DoubleClick(FMouseClickTime.LSecs, FMouseClickTime.LMicros, Msg^.imsg^.Seconds, Msg^.imsg^.Micros) then
          begin
            if Assigned(FOnMUIDblClick) and InObject then
              FOnMUIDblClick(Self, mmbLeft, RelX, RelY, EatMe);
            FMouseClickTime.LSecs := 0;
            FMouseClickTime.LMicros := 0;
          end
          else
          begin
            if InObject then
            begin
              FMouseClickTime.LSecs := Msg^.imsg^.Seconds;
              FMouseClickTime.LMicros := Msg^.imsg^.Micros;
            end;
          end;

        end;
        MENUDOWN:
        begin
          if not InObject then
            Exit;
          if Assigned(FOnMUIMouseDown) then
            FOnMUIMouseDown(Self, mmbRight, RelX, RelY, EatMe);
        end;
        MENUUP:
        begin
          if Assigned(FOnMUIMouseUp) then
            FOnMUIMouseUp(Self, mmbRight, RelX, RelY, EatMe);
          if DoubleClick(FMouseClickTime.RSecs, FMouseClickTime.RMicros, Msg^.imsg^.Seconds, Msg^.imsg^.Micros) then
          begin
            if Assigned(FOnMUIDblClick) and InObject then
              FOnMUIDblClick(Self, mmbRight, RelX, RelY, EatMe);
            FMouseClickTime.RSecs := 0;
            FMouseClickTime.RMicros := 0;
          end
          else
          begin
            if InObject then
            begin
              FMouseClickTime.RSecs := Msg^.imsg^.Seconds;
              FMouseClickTime.RMicros := Msg^.imsg^.Micros;
            end;
          end;

        end;
      end;
    end;
    {$ifdef AmigaOS4}
    IDCMP_EXTENDEDMOUSE:
    begin
      if Msg^.imsg^.Code = IMSGCODE_INTUIWHEELDATA then
      begin
        RelX := Msg^.imsg^.MouseX - obj_Left(obj);
        RelY := Msg^.imsg^.MouseY - obj_Top(obj);
        WheelData := PIntuiWheelData(Msg^.imsg^.IAddress);
        // Mouse wheel with Value 120 (from the other interfaces)
        if Assigned(FOnMUIMouseWheel) then
          FOnMUIMouseWheel(Self, WheelData^.WheelY > 0, EatMe);
      end;
    end;
    {$endif}
    // Mouse Move
    IDCMP_MOUSEMOVE:
    begin
      if FMouseInObject and not InObject then
      begin
        FMouseInObject := False;
        if Assigned(FOnMUIMouseLeave) then
          FOnMUIMouseLeave(Self);
      end;
      if not InObject then
        Exit;
      FMouseInObject := True;
      RelX := Msg^.imsg^.MouseX - obj_Left(obj);
      RelY := Msg^.imsg^.MouseY - obj_Top(obj);
      if Assigned(FOnMUIMouseMove) then
        FOnMUIMouseMove(Self, RelX, RelY, EatMe);
    end;
    // Raw Key
    IDCMP_RAWKEY:
    begin
      // Gather Values
      IsKeyUp := (Msg^.iMsg^.Code and IECODE_UP_PREFIX) <> 0;
      Code := Msg^.iMsg^.Code and not IECODE_UP_PREFIX;
      Qual := Msg^.iMsg^.Qualifier;
      // Mouse Wheel
      if (Code = $7A) or (Code = $7B) then
      begin
        if Assigned(FOnMUIMouseWheel) then
          FOnMUIMouseWheel(Self, Code = $7A, EatMe);
      end
      else
      begin
        // Check the shiftstate
        Mss := [];
        if (Qual and IEQUALIFIER_CONTROL) <> 0 then
          Mss := Mss + [mssCtrl];
        if (Qual and IEQUALIFIER_LSHIFT) <> 0 then
          Mss := Mss + [mssLShift, mssShift];
        if (Qual and IEQUALIFIER_RSHIFT) <> 0 then
          Mss := Mss + [mssRShift, mssShift];
        if (Qual and IEQUALIFIER_LALT) <> 0 then
          Mss := Mss + [mssLAlt];
        if (Qual and IEQUALIFIER_RALT) <> 0 then
          Mss := Mss + [mssRAlt];
        // Get the actual char for it
        Buff[0] := #0;
        ie.ie_Class := IECLASS_RAWKEY;
        ie.ie_SubClass := 0;
        ie.ie_Code := Code;
        ie.ie_Qualifier := Qual and (not (IEQUALIFIER_CONTROL or IEQUALIFIER_LALT));
        ie.ie_NextEvent := nil;
        MapRawKey(@ie, @Buff[0], 1, nil);
        // send message
        if IsKeyUp then
        begin
          if Assigned(FOnMUIKeyUp) then
            FOnMUIKeyUp(Self, Mss, Code, Buff[0], EatMe);
        end
        else
        begin
          if Assigned(FOnMUIKeyDown) then
            FOnMUIKeyDown(Self, Mss, Code, Buff[0], EatMe);
        end;
      end;
    end;
  end;
  if EatMe then
    Result := MUI_EventHandlerRC_Eat;
end;


function TMUIPaintBox.MUIEvent(cl: PIClass; Obj: PObject_; Msg: intuition.PMsg): PtrUInt;
begin
  case Msg^.MethodID of
    MUIM_Setup: Result := DoSetup(cl, Obj, Pointer(Msg));
    MUIM_Cleanup: Result := DoCleanup(cl, Obj, Pointer(Msg));
    //
    MUIM_AskMinMax: Result := DoAskMinMax(cl, Obj, Pointer(Msg));
    //
    MUIM_Draw: Result := DoDraw(cl, Obj, Pointer(Msg));
    MUIM_HANDLEEVENT: Result := DoHandleEvent(cl, Obj, Pointer(Msg));
    else
      Result := DoSuperMethodA(cl, obj, msg);
  end;
end;

// Dispatcher
// send everything to the object
function MPBDispatcher(cl: PIClass; Obj: PObject_; Msg: intuition.PMsg): PtrUInt;
var
  MUIPB: TMUIPaintBox;
begin
  case Msg^.MethodID of
    {OM_NEW: MPBDispatcher := DoSuperMethodA(cl, obj, msg);
    OM_DISPOSE: MPBDispatcher := DoSuperMethodA(cl, obj, msg);
    OM_GET: MPBDispatcher := DoSuperMethodA(cl, obj, msg);
    OM_SET: MPBDispatcher := DoSuperMethodA(cl, obj, msg);}
    MUIM_Setup,
    MUIM_Cleanup,
    MUIM_AskMinMax,
    MUIM_Draw,
    MUIM_HANDLEEVENT:
    begin
      MUIPB := TMUIPaintBox(INST_DATA(cl, Pointer(obj))^); // get class
      if Assigned(MUIPB) then
        MPBDispatcher := MUIPB.MUIEvent(cl, Obj, Msg)                       // call the class dispatcher
      else
        MPBDispatcher := DoSuperMethodA(cl, obj, msg);     // Still not assigned just use default
    end;
    else
      MPBDispatcher := DoSuperMethodA(cl, obj, msg);
  end;
end;

// create CustomClass
procedure MakePaintBoxClass;
begin
  MUIPBType := MH_CreateCustomClass(nil, MUIC_Area, nil, SizeOf(Pointer), @MPBDispatcher);
end;

// Destroy CustomClass
procedure FreePaintBoxClass;
begin
  if Assigned(MUIPBType) then
    MUI_DeleteCustomClass(MUIPBType);
end;

initialization
  MakePaintBoxClass;
finalization
  FreePaintBoxClass;
end.
