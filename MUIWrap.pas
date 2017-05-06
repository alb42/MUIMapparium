unit MUIWrap;
{$mode objfpc}{$H+}
interface

uses
  {$if defined(Amiga68k) or defined(MorphOS)}
    amigalib,
  {$endif}
  asl, Exec, Utility, Intuition, AGraphics, timer, mui, muihelper;

type
  TRexxMsg = record
    rm_Node: TMessage;
    rm_TaskBlock: APTR;
    rm_LibBase: APTR;
    rm_Action: LongInt;
    rm_Result1: LongInt;
    rm_Result2: PtrInt;
    rm_Args: array[0..15] of STRPTR;
    rm_MsgPort: PMsgPort;
    rm_CommAddr: STRPTR;
    rm_FileExt: STRPTR;
    rm_Stdin: BPTR;
    rm_Stdout: BPTR;
    rm_Avail: LongInt;
  end;
  PRexxMsg = ^TRexxMsg;

procedure ConnectHookFunction(MUIField: PtrUInt; TriggerValue: PtrUInt; Obj: PObject_; Data: TObject; Hook: PHook; HookFunc: THookFunc);
{$ifndef AROS}
function CreateRastPort: PRastPort;
procedure FreeRastPort(RP: PRastPort);
{$endif}
{$if defined(AROS) or defined(AmigaOS4))}
function AllocAslRequestTags(reqType: LongWord; const Param: array of PtrUInt): Pointer;
{$endif}

function GetMUITime: Int64;

implementation

// *****************************************************
// Use local GetMsCount with fixed timer.device, faster
// because it's polled very often (CheckTimer)
// But its not threadsafe!
// can be removed if a threadvar version is implemented in RTL
var
  Tr: PTimeRequest = nil;

procedure NewList (list: pList);
begin
  with list^ do
  begin
    lh_Head := PNode(@lh_Tail);
    lh_Tail := nil;
    lh_TailPred := PNode(@lh_Head)
  end;
end;

function CreateExtIO(Port: PMsgPort; Size: LongInt): PIORequest;
begin
  Result := nil;
  if Port <> nil then
  begin
    Result := ExecAllocMem(Size, MEMF_CLEAR);
    if Result <> nil then
    begin
      Result^.io_Message.mn_Node.ln_Type := 7;
      Result^.io_Message.mn_Length := Size;
      Result^.io_Message.mn_ReplyPort := Port;
    end;
  end;
end;

procedure DeleteExtIO (IoReq: PIORequest);
begin
  if IoReq <> nil then
  begin
    IoReq^.io_Message.mn_Node.ln_Type := $FF;
    IoReq^.io_Message.mn_ReplyPort := PMsgPort(-1);
    IoReq^.io_Device := PDevice(-1);
    ExecFreeMem(IoReq, IoReq^.io_Message.mn_Length);
  end
end;

function Createport(Name: PChar; Pri: LongInt): PMsgPort;
var
  sigbit: ShortInt;
begin
  Result := nil;
  SigBit := AllocSignal(-1);
  if SigBit = -1 then
   Exit;
  Result := ExecAllocMem(SizeOf(TMsgPort), MEMF_CLEAR);
  if Result = nil then
  begin
    FreeSignal(SigBit);
    Exit;
  end;
  with Result^ do
  begin
    if Assigned(Name) then
      mp_Node.ln_Name := Name
    else
      mp_Node.ln_Name := nil;
    mp_Node.ln_Pri := Pri;
    mp_Node.ln_Type := 4;
    mp_Flags := 0;
    mp_SigBit := SigBit;
    mp_SigTask := FindTask(nil);
  end;
  if Assigned(Name) then
    AddPort(Result)
  else
    NewList(Addr(Result^.mp_MsgList));
end;

procedure DeletePort(Port: PMsgPort);
begin
  if Port <> nil then
  begin
    if Port^.mp_Node.ln_Name <> nil then
      RemPort(Port);
    port^.mp_Node.ln_Type := $FF;
    port^.mp_MsgList.lh_Head := PNode(-1);
    FreeSignal(Port^.mp_SigBit);
    ExecFreeMem(Port, SizeOf(TMsgPort));
  end;
end;

function Create_Timer(TheUnit: LongInt): PTimeRequest;
var
  TimerPort: PMsgPort;
begin
  Result := nil;
  TimerPort := CreatePort(nil, 0);
  if TimerPort = nil then
    Exit;
  Result := PTimeRequest(CreateExtIO(TimerPort, SizeOf(TTimeRequest)));
  if Result = Nil then
  begin
    DeletePort(TimerPort);
    Exit;
  end;
  if OpenDevice(TIMERNAME, TheUnit, PIORequest(Result), 0) <> 0 then
  begin
    DeleteExtIO(pIORequest(Result));
    DeletePort(TimerPort);
    Result := nil;
  end;
end;

Procedure Delete_Timer(WhichTimer: PTimeRequest);
var
  WhichPort: PMsgPort;
begin
  WhichPort := WhichTimer^.tr_Node.io_Message.mn_ReplyPort;
  if assigned(WhichTimer) then
  begin
    CloseDevice(PIORequest(WhichTimer));
    DeleteExtIO(PIORequest(WhichTimer));
  end;
  if Assigned(WhichPort) then
    DeletePort(WhichPort);
end;

function get_sys_time(tv: PTimeVal): LongInt;
begin
  Result := -1;
  if not Assigned(Tr) then
    Tr := Create_Timer(UNIT_MICROHZ);
  // non zero return says error
  if tr = nil then
    Exit;
  tr^.tr_node.io_Command := TR_GETSYSTIME;
  DoIO(PIORequest(tr));
  // structure assignment
  tv^ := tr^.tr_time;
  Result := 0;
end;

function GetMUITime: Int64;
var
  TV: TTimeVal;
begin
  Get_Sys_Time(@TV);
  Result := Int64(TV.TV_Secs) * 1000 + TV.TV_Micro div 1000;
end;

procedure ConnectHookFunction(MUIField: PtrUInt; TriggerValue: PtrUInt; Obj: PObject_; Data: TObject; Hook: PHook; HookFunc: THookFunc);
begin
  MH_SetHook(Hook^, HookFunc, Data);

  DoMethod(Obj, [
    MUIM_Notify, MUIField, TriggerValue, MUIV_Notify_Self,
    2,
    MUIM_CallHook, NativeUInt(Hook),
    0]);
end;

{$ifndef AROS}
procedure FreeRastPort(Rp: PRastPort);
begin
  FreeMem(Rp);
end;

function CreateRastPort: PRastPort;
begin
  Result := AllocMem(SizeOf(TRastPort));
  InitRastPort(Result);
end;
{$endif}
{$if defined(AROS) or defined(AmigaOS4))}
function AllocAslRequestTags(reqType: LongWord; const Param: array of PtrUInt): Pointer;
begin
  Result := AllocAslRequest(reqType, Param);
end;
{$endif}

initialization
  if not Assigned(Tr) then
    Tr := create_timer(UNIT_MICROHZ);

finalization
  if Assigned(Tr) then
    Delete_timer(tr);
end.
