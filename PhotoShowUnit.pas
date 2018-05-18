unit PhotoShowUnit;
{$mode objfpc}{$H+}
interface

uses
  Types, Classes, SysUtils, fgl, Math,
  Exec, Utility, intuition, AGraphics, Layers, MUI, MUIHelper,
  MUIWrap, MUIPaintBoxUnit, MUIMappariumlocale,
  fpreadjpeg, imagesUnit, FPImage, cybergraphics,
  DataTypes;

type
  TPhotoPanel = class(TMUIPaintBox)
  private
    FUseDataType: Boolean;
    FPhotoName: string;
    // for internal
    RawImg: TFPAMemImage;
    Img: TFPAMemImage;
    // for Datatype action
    Scr: PScreen;
    DTObj: PObject_;
    RawBM: PBitMap;
    RawBMH: PBitMapHeader;
    Sw, Sh: LongWord;
    ScaledBitmap: PBitMap;
    procedure SetUseDataType(AValue: Boolean);
  public
    constructor Create(const Args: array of PtrUInt); override; // Create it
    destructor Destroy; override;

    procedure DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect); // Main Drawing event

    procedure ShowImage(APhotoName: string);

    property UseDataType: Boolean read FUseDataType write SetUseDataType;
  end;

var
  PhotoShowWin: PObject_;
  PhotoPanel: TPhotoPanel;

type
  TNextPhotoEvent = procedure (Dir: Integer);
var
  OnNextPhoto: TNextPhotoEvent;

implementation

var
  ButtonNext, ButtonBack: PObject_;
  NextHook, BackHook: PObject_;
  TitleText: string;

constructor TPhotoPanel.Create(const Args: array of PtrUInt);
begin
  inherited;
  RawImg := nil;
  Img := nil;
  Scr := nil;
  DTObj := nil;
  RawBM := nil;
  RawBMH := nil;
  ScaledBitMap := nil;
  FPhotoName := '';
  OnDrawObject := @DrawEvent;
  FUseDataType := False;
end;

destructor TPhotoPanel.Destroy;
begin
  RawImg.Free;
  Img.Free;
  if Assigned(DTObj) then
    DisposeDTObject(DTObj);
  DTObj := nil;
  RawBM := nil;
  RawBMH := nil;
  if Assigned(ScaledBitmap) then
    FreeBitmap(ScaledBitmap);
  ScaledBitMap := nil;
  if Assigned(Scr) then
    UnLockPubScreen(nil, Scr);
  Scr := nil;
  inherited;
end;

procedure TPhotoPanel.SetUseDataType(AValue: Boolean);
begin
  if AValue = FUseDataType then
    Exit;
  FUseDataType := AValue;
  // Clear Internal Stuff
  RawImg.Free;
  RawImg := Nil;
  Img.Free;
  Img := nil;
  // Clear DataType Stuff
  if Assigned(DTObj) then
    DisposeDTObject(DTObj);
  DTObj := nil;
  RawBM := nil;
  RawBMH := nil;
  if Assigned(ScaledBitmap) then
    FreeBitmap(ScaledBitmap);
  ScaledBitMap := nil;
  if Boolean(MH_Get(PhotoShowWin, MUIA_Window_Open)) then
    MH_Set(PhotoShowWin, MUIA_Window_Open, LFalse);
end;

procedure TPhotoPanel.ShowImage(APhotoName: string);
var
  JPG: TFPReaderJPEG;
  t1: LongWord;
begin
  t1 := GetTickCount;
  FPhotoName := APhotoName;
  RawImg.Free;
  RawImg := nil;
  Img.Free;
  Img := nil;
  if APhotoName = '' then
    Exit;
  if FUseDataType then
  begin
    if not Assigned(scr) then
      Scr := LockPubScreen(nil);
    if Assigned(ScaledBitmap) then
      FreeBitmap(ScaledBitmap);
    ScaledBitMap := nil;
    RawBM := nil;
    RawBMH := nil;
    if Assigned(DTObj) then
      DisposeDTObject(DTObj);
    DTObj := NewDTObject(PChar(APhotoName), [
      DTA_GroupID, GID_PICTURE,
      PDTA_DestMode,PMODE_V43,
      PDTA_Remap, AsTag(TRUE),
      PDTA_Screen, AsTag(scr),
      TAG_END, TAG_END]);
    if Assigned(DTObj) then
    begin
      DoMethod(DTObj, [DTM_PROCLAYOUT, 0 , 1]);
      GetDTAttrs(DTObj,
        [PDTA_DestBitMap, AsTag(@RawBM),
        PDTA_BitMapHeader,AsTag(@RawBMH),
        TAG_END])
    end;
    //
  end
  else
  begin
    RawImg := TFPAMemImage.Create(10, 10);
    JPG := TFPReaderJPEG.Create;
    RawImg.LoadFromFile(APhotoName, JPG);
    JPG.Free;
  end;
  TitleText := ExtractFileName(APhotoName);
  MH_Set(PhotoShowWin, MUIA_Window_Title, AsTag(PChar(TitleText)));
  //writeln('Time to load jpeg: ', GetTickCount - t1);
  MH_Set(PhotoShowWin, MUIA_Window_Open, LTrue);
  PhotoPanel.RedrawObject;
end;

procedure TPhotoPanel.DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
var
  Aspect, xFactor, yFactor: Single;
  w,h, x,y,nx,ny: Integer;
  Src, Dest, LineStart: PFPCompactImgRGBA8BitValue;
  AddRect: Classes.TRect;
  Bsa: TBitScaleArgs;
  t1: LongWord;
begin
  t1 := GetTickCount;
  if FUseDataType then
  begin
    if not Assigned(DTObj) or not Assigned(RawBM) or not Assigned(RawBMH) then
      Exit;
    Aspect := RawBMH^.bmh_Height / RawBMH^.bmh_Width;
  end
  else
  begin
    if not Assigned(RawImg) or (RawImg.Width = 0) then
      Exit;
    Aspect := RawImg.Height / RawImg.Width;
  end;
  w := DrawRect.Width;
  h := Round(Aspect * w);
  AddRect := Rect(0, h, w, DrawRect.Height);
  if h > DrawRect.Height then
  begin
    h := DrawRect.Height;
    w := Round(h / Aspect);
    AddRect := Rect(w, 0, DrawRect.Width, h);
  end;
  if FUseDataType then
  begin
    if not Assigned(ScaledBitmap) or (Sw <> w) or (Sh <> h) then
    begin
      if Assigned(ScaledBitmap) then
        FreeBitMap(ScaledBitmap);
      ScaledBitmap := AllocBitMap(w, h, RP^.Bitmap^.Depth, BMF_MINPLANES or BMF_DISPLAYABLE, RP^.Bitmap);
      with bsa do
      begin
        bsa_SrcX := 0;
        bsa_SrcY := 0;
        bsa_SrcWidth := RawBMH^.bmh_Width;
        bsa_SrcHeight := RawBMH^.bmh_Height;
        bsa_XSrcFactor := RawBMH^.bmh_Width;
        bsa_YSrcFactor := RawBMH^.bmh_Height;
        bsa_DestX := 0;
        bsa_DestY := 0;
        bsa_DestWidth := w;
        bsa_DestHeight := h;
        bsa_XDestFactor := w;
        bsa_YDestFactor := h;
        bsa_SrcBitmap := RawBM;
        bsa_DestBitmap := ScaledBitmap;
        bsa_Flags := 0;
        bsa_XDDA := 0;
        bsa_YDDA := 0;
        bsa_Reserved1 := 0;
        bsa_Reserved2 := 0;
      end;
      BitmapScale(@bsa);
      Sw := w;
      Sh := h;
    end;
    BltBitMapRastPort(ScaledBitmap, 0, 0, RP, DrawRect.Left, DrawRect.Top, w, h, $c0);
  end
  else
  begin
    if (not Assigned(Img)) or (Img.Width <> w) or (Img.Height <> h) then
    begin
      Img.Free;
      Img := TFPAMemImage.Create(w, h);
      xFactor := RawImg.Width / w;
      yFactor := RawImg.Height / h;
      Dest := Img.Data;
      for y := 0 to h - 1 do
      begin
        ny := Max(0, Min(RawImg.Height - 1, Round(y * yFactor)));
        LineStart := RawImg.Data;
        Inc(LineStart, ny * RawImg.Width);
        for x := 0 to w - 1 do
        begin
          nx := Max(0, Min(RawImg.Width - 1, Round(x * xFactor)));
          Src := LineStart;
          Inc(Src, nx);
          Dest^ := Src^;
          // next Pixel
          Inc(Dest);
        end;
      end;
    end;
    WritePixelArray(Img.Data, 0, 0, Img.Width * SizeOf(LongWord), RP, DrawRect.Left, DrawRect.Top, Img.Width, Img.Height, RECTFMT_RGBA);
  end;
  SetAPen(RP, 0);
  RectFill(RP, DrawRect.Left + AddRect.Left, DrawRect.Top + AddRect.Top, DrawRect.Left + AddRect.Left + AddRect.Width, DrawRect.Top + AddRect.Top + AddRect.Height);
  //writeln('Redraw :',  GetTickCount - t1)
end;

function NextEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  if Assigned(OnNextPhoto) then
    OnNextPhoto(+1);
end;

function BackEvent(Hook: PHook; Obj: PObject_; Msg: Pointer): NativeInt;
begin
  Result := 0;
  if Assigned(OnNextPhoto) then
    OnNextPhoto(-1);
end;


procedure CreatePhotoWin;
begin
  PhotoPanel := TPhotoPanel.Create([TAG_DONE]);
  PhotoPanel.MinWidth := 320;
  PhotoPanel.MinHeight := 200;
  PhotoPanel.DefWidth := 640;
  PhotoPanel.DefHeight := 480;

  PhotoShowWin := MH_Window([
    MUIA_Window_Title,     AsTag(PChar('')),
    MUIA_Window_ID,        AsTag(MAKE_ID('P','H','O','T')),
    MUIA_HelpNode,         AsTag('PhotoWin'),
    WindowContents, AsTag(MH_HGroup([
      Child, AsTag(MH_VGroup([
        Child, AsTag(MH_HGroup([
          Child, AsTag(MH_Image(ButtonBack, [
            MUIA_Image_Spec, MUII_ArrowLeft,
            MUIA_Background, MUII_ButtonBack,
            MUIA_Frame, ButtonFrame,
            MUIA_InputMode, MUIV_InputMode_RelVerify,
          TAG_DONE])),
          Child, AsTag(MH_Image(ButtonNext, [
            MUIA_Image_Spec, MUII_ArrowRight,
            MUIA_Background, MUII_ButtonBack,
            MUIA_Frame, ButtonFrame,
            MUIA_InputMode, MUIV_InputMode_RelVerify,
          TAG_DONE])),
          Child, AsTag(MH_HSpace(0)),
        TAG_DONE])),
      Child, AsTag(MH_VGroup([
        Child, AsTag(PhotoPanel.MUIObject),
        TAG_DONE])),
      TAG_DONE])),
    TAG_DONE])),
  TAG_DONE]);

  ConnectHookFunction(MUIA_Pressed, AsTag(False), ButtonBack, nil, @BackHook, @BackEvent);
  ConnectHookFunction(MUIA_Pressed, AsTag(False), ButtonNext, nil, @NextHook, @NextEvent);

  // Close Window
  DoMethod(PhotoShowWin, [MUIM_Notify, MUIA_Window_CloseRequest, MUI_TRUE,
    AsTag(PhotoShowWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);

end;

initialization
  CreatePhotoWin;
end.
