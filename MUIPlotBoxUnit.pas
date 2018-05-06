unit MUIPlotBoxUnit;
{$mode objfpc}{$H+}
interface

uses
  Math, Classes, SysUtils, MUIPaintBoxUnit, mui, exec, agraphics, intuition,
  utility, prefsunit, types, layers, MuiHelper,
  cybergraphics, muiwrap;

type
  TMarkerChangeEvent = procedure(MarkerID: Integer);
  TDoubleArray = array of Double;
  TAxis = class;
  TAxisScale = (atLin,atLog);
  TAxisPosition = (apBottom,apLeft,apTop,apRight);
  TAxisOptions = set of (aoMinFixed,aoMaxFixed,
                         aoShowTics,aoShowLabels,aoShowTitle,
                         aoMajorGrid,aoMinorGrid,
                         aoForceWidth,aoForceNoExp,aoForceInteger);
  TPlotBoxCurve = record
    x, y: array of Double;
    valid: array of boolean;
    xAxPos, yAxPos: TAxisPosition;
    xaxis, yaxis: TAxis;
    Color: TColor;
    //Width: Integer;
    //ScatterStyle: TScatterStyle;
    //ScatterSize: integer;
    //LinesOn:boolean;
    Name:string;
  end;

  TMouseMode = (mmZoom, mmPosition, mmData, mmMarker);

  TAxis = class
  private
    FMinValue: Double;
    FMaxValue: Double;
    FOptions: TAxisOptions;
    FPosition: TAxisPosition;
    FAxisScale: TAxisScale;
    Start, Stop: Classes.TPoint;
    Increment: Double;
    FTextPosition:TAxisPosition;
    DivideFac: Double;
    FTitle: string;
    Prefix: string;
    FAxUnit: string;
    VertTextShift:integer;
    MinorNo: Integer;
    MinSet, MaxSet: Boolean;
    FColor: TColor;
    AfterDot: Integer;
  public
    procedure DoDraw(Rp: PRastPort; DrawRect: TRect);
    procedure DrawGrid(Rp: PRastPort; ClipRect: TRect);
    function Place(Value: Double): Classes.TPoint;
    function Shift(Position: Classes.TPoint; Value: Double): Classes.TPoint;
    function PosToValue(Pos: integer): Double;
    function GetWidth(RP: PRastPort; GetForced: boolean = True): integer;
    procedure SetIncrement(MinDist: Integer);
    procedure ValidateMinMax;
    property Options: TAxisOptions read FOptions write FOptions;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property Position: TAxisPosition read FPosition;
    property TextPosition: TAxisPosition read FTextPosition write FTextPosition;
    property Title: string read FTitle write FTitle;
    property AxUnit: string read FAxUnit write FAxUnit;
    property Color: TColor read FColor write FColor;
  end;

  TPlotPanel = class(TMUIPaintBox)
  private
    //DownPos: Classes.TPoint;
    //FLastMouse: Classes.TPoint;
    //LeftDown: Boolean;
    FTitle: string;
    FAxisLeft: TAxis;
    FAxisBottom: TAxis;
    FAxisRight: TAxis;
    FAxisTop: TAxis;
    Inset: String;
    Curves: array of TPlotBoxCurve;
    FForceRedraw: Boolean;
    FLayerInfo: PLayer_Info;
    FRastPort: PRastPort;
    RPDrawRect: TRect;
    // Mousepos
    MouseDown: Boolean;
    ZoomBox: TRect;
    CrossPos: Classes.TPoint;
    CrossText: TStringList;
    PopupMenu: PObject_;
    PopupHook: THook;
    FMouseModus: TMouseMode;
    PM: array of PObject_; //
    //
    FXMarker: Double; // x Position Marker
    FXMarkerValueIdx: Integer;
    FMarkerLoc: Classes.TPoint;
    FShowMarker: Boolean; //
    FOnMarkerChange: TMarkerChangeEvent;
    OldDrawer: string;
  protected
    procedure DrawCurves(RP: PRastPort; ClipRect: TRect);
    procedure DrawMarker(RP: PRastPort; ClipRect: TRect);
    function FindNearestCurve(X, Y: Integer; var MinLoc: Classes.TPoint; var CurveIdx, PointIdx: Integer): Boolean;
  public
    constructor Create(const Args: array of PtrUInt); override;
    destructor Destroy; override;
    procedure DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
    procedure DoPlotDraw(RP: PRastPort; DrawRect: TRect; ScaleOnly: Boolean = False);
    procedure RescaleByCurves;
    //
    procedure AddCurve(var x,y:array of double; var valid:array of boolean; XAxis,YAxis:TAxisPosition; Color:TColor=$000000; Name:string=''; Index: Integer = -1);
    procedure Clear;
    procedure PlotData;
    //
    procedure MouseDownEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseUpEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseMoveEvent(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseLeaveEvent(Sender: TObject);
    procedure PopupEvent(EventID: Integer);
    procedure ExportASCII(FileName: string = '');
    procedure ExportPNG(FileName: string = '');

    {procedure MouseDblClick(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseMoveEvent(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseWheelEvent(Sender: TObject; ScrollUp: Boolean; var EatEvent: Boolean);
    //
    procedure KeyDownEvent(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    //
    procedure ZoomIn(ToPos: Boolean);
    procedure ZoomOut;
    procedure RefreshImage;
    function PixelToPos(T: Classes.TPoint): TCoord;
    function PosToPixel(C: TCoord): Classes.TPoint;

    property OnUpdateLocationLabel: TProcedure read FOnUpdateLocationLabel write FOnUpdateLocationLabel;
    property LastMouse: Classes.TPoint read FLastMouse;}
    property Title: string read FTitle write FTitle;
    property AxisTop: TAxis read FAxisTop write FAxisTop;
    property AxisLeft: TAxis read FAxisLeft write FAxisLeft;
    property AxisBottom: TAxis read FAxisBottom write FAxisBottom;
    property AxisRight: TAxis read FAxisRight write FAxisRight;
    property MouseModus: TMouseMode read FMouseModus;
    property OnMarkerChange: TMarkerChangeEvent read FOnMarkerChange write FOnMarkerChange;
    property XMarkerValueIdx: Integer read FXMarkerValueIdx;
    property ShowMarker: Boolean read FShowMarker;
  end;

function ValueToStr(Value: Double; Precision: Integer): string;

var
  Menutitle_Plot: string = 'Plot';
  Menutitle_Rescale: string = 'Rescale';
  Menutitle_Zoom: string = 'Zoom';
  Menutitle_Position: string = 'Position Data';
  Menutitle_Data: string = 'Curve Data';
  Menutitle_Marker: string = 'Set Marker';
  MenuTitle_ASCII: string = 'Export as ASCII';
  MenuTitle_PNG: string = 'Export as PNG';

  HintText_XAxis1: string = 'X Axis';
  HintText_XAxis2: string = 'X Axis';
  HintText_YAxis1: string = 'Left Axis';
  HintText_YAxis2: string = 'Right Axis';


implementation

uses
  ASL, fpwritepng, ImagesUnit;

const
  PID_Rescale  = 0;
  PID_Zoom     = 1;
  PID_Position = 2;
  PID_Data     = 3;
  PID_Marker   = 4;
  PID_ASCII    = 5;
  PID_PNG      = 6;

  PID_Last = 6;

  ln10=2.30258509299404568401799145468436;
  PreFixChar:array[-8..8] of char=
    ('y','z','a','f','p','n','u','m',
     ' ',
     'k','M','G','T','P','E','Z','Y');
  PreFixExp:array[-8..8] of string=
    ('10{^-24}','10{^-21}','10{^-18}','10{^-15}','10{^-12}','10{^-9}','10{^-6}','10{^-3}',
     '',
     '10{^3}','10{^6}','10{^9}','10{^12}','10{^15}','10{^18}','10{^21}','10{^24}');

function ValueToStr(Value: Double; Precision: Integer): string;
begin
  Result := FloatToStrF(Value, ffFixed, 16, Precision);
end;

procedure ParamNameOut(s: string; RP: PRastPort;
  Color: TColor; Rect: TRect; Rot90:boolean=false);
{var
  TempRP: PRastPort;
  TE: TTextExtent;
  nx, ny, x, y: Integer;
  Pen: LongWord;}
begin
  if Rot90 then
  begin
    {
    TextExtent(RP, PChar(s), Length(s), @TE);
    TempRP := CreateRastPort;
    TempRP^.Bitmap := AllocBitMap(TE.te_Width, TE.te_Height, rp^.Bitmap^.Depth, BMF_MINPLANES or BMF_DISPLAYABLE, rp^.Bitmap);
    TempRP^.Font := RP^.Font;
    Pen := SetColor(TempRP, 0);
    RectFill(TempRP, 0,0, TE.te_Width-1, TE.te_Height-1);
    UnSetColor(Pen);
    Pen := GetAPen(RP);
    Pen := SetColor(TempRP, Color);
    GfxMove(TempRp, 0, 0 + TE.te_Height);
    GfxText(TempRp, PChar(s), Length(s));
    UnSetColor(Pen);
    for y := 0 to TE.te_Height - 1 do
    begin
      for x := 0 to TE.te_Width - 1 do
      begin
        nx := Rect.Left + (TE.te_Height - 1 - y) - 5;
        ny := Rect.Top + x;
        Pen :=  ReadRGBPixel(TempRP, x, y);
        if Pen <> 0 then
          WriteRGBPixel(RP, nx, ny, Pen);
      end;
    end;
    FreeBitmap(TempRP^.Bitmap);
    FreeRastPort(TempRP);}
  end
  else
  begin
    GfxMove(Rp, rect.Left,rect.Top + 6);
    GfxText(Rp, PChar(s), Length(s));
  end;
end;

function PointInRect(const x,y:integer;const Rect:TRect):boolean; inline;
begin
  Result:=(x >= Rect.Left) and (x <= Rect.Right) and (y >= Rect.Top) and (y <= Rect.Bottom);
end;

procedure ClippedLine(RP: PRastPort;const x0,y0,x1,y1:integer; const Cliprect:TRect;var DoOld:boolean);
var
  dx,dy,xa0,ya0,xa1,ya1,a,b: Double;
begin
  if PointInRect(X0,Y0,ClipRect) and PointInRect(X1,Y1,ClipRect) then
  begin
    if DoOld then
      GfxMove(Rp, x0,y0);
    DoOld := false;
    Draw(Rp, x1, y1);
  end
  else
  begin
    if ((y0<=Cliprect.Top) and (y1<=Cliprect.Top)) or ((y0>=Cliprect.Bottom) and (y1>=Cliprect.Bottom)) or
      ((x0<=Cliprect.Left) and (x1<=Cliprect.Left)) or ((x0>=Cliprect.Right) and (x1>=Cliprect.Right)) then
      exit;
    xa0 := x0;
    ya0 := y0;
    xa1 := x1;
    ya1 := y1;
    dx := xa1 - xa0;
    dy := ya1 - ya0;
    if dy <> 0 then
    begin
      a := (cliprect.Top - ya0) / dy;
      b := (cliprect.Bottom - ya0) / dy;
      if (a < 0) and (b < 0) then
        Exit;
      if (a > 1) and (b > 1) then
        Exit;
      if a < 0 then
        a := 0;
      if a > 1 then
        a := 1;
      if b < 0 then
        b := 0;
      if b > 1 then
        b := 1;
      xa1 := xa0 + b * dx;
      ya1 := ya0 + b * dy;
      xa0 := xa0 + a * dx;
      ya0 := ya0 + a * dy;
      dx := xa1 - xa0;
      dy := ya1 - ya0;
    end;
    if dx <> 0 then
    begin
      a :=(cliprect.Left-xa0)/dx;
      b :=(cliprect.Right-xa0)/dx;
      if (a < 0) and (b < 0) then
        Exit;
      if (a > 1) and (b > 1) then
        Exit;
      if a < 0 then
        a := 0;
      if a > 1 then
        a := 1;
      if b < 0 then
        b := 0;
      if b > 1 then
        b := 1;
      xa1 := xa0 + b * dx;
      ya1 := ya0 + b * dy;
      xa0 := xa0 + a * dx;
      ya0 := ya0 + a * dy;
    end;
    GfxMove(Rp, Round(xa0), Round(ya0));
    Draw(Rp, Round(xa1), Round(ya1));
    DoOld:=true;
  end;
end;

// TAxis

procedure TAxis.DoDraw(Rp: PRastPort; DrawRect: TRect);
var
  wd,hg,hg2,disp: Integer;
  ds,ds2: string;
  i: Integer;
  fad: Integer;
  pt:TPoint;
  w10: Integer;
  de: Double;
  Pen: LongWord;
begin
  Pen := SetColor(RP, Color);

  GfxMove(Rp, DrawRect.Left + Start.X, DrawRect.Top + Start.Y);
  Draw(RP, DrawRect.Left + Stop.X, DrawRect.Top + Stop.Y);

  disp := 0;

  if aoShowTics in Options then
  begin
    if (FAxisScale = atLin) and (Increment <> 0) then
    begin
      for i := Round(FMinValue / Increment) - 1 to Round(FMaxValue / Increment) + 1 do
      begin
        if (i * Increment >= FMinValue) and (i * Increment <= FMaxValue) then
        begin
          pt := Place(i * Increment);
          GfxMove(Rp, Pt.x, Pt.y);
          case TextPosition of
            apTop:
              Draw(Rp, DrawRect.Left + Pt.x, DrawRect.Top + Pt.y + 5);
            apBottom:
              Draw(Rp, DrawRect.Left + Pt.x, DrawRect.Top + Pt.y - 5);
            apLeft:
              Draw(Rp, DrawRect.Left + Pt.x + 5, DrawRect.Top + Pt.y);
            apRight:
              Draw(Rp, DrawRect.Left + Pt.x - 5, DrawRect.Top + Pt.y);
          end;
        end;
      end;
    end
    else
    begin
      if (FMinValue > 0) and (FMaxValue > 0) then
      begin
        for i := Round(ln(FMinValue) / ln10) to Round(ln(FMaxValue) / ln10) do
        begin
          if (Exp(i * ln10) >= FMinValue) and (Exp(i * ln10) <= FMaxValue) then
          begin
            pt := Place(exp(i*ln10));
            GfxMove(Rp, Pt.x, Pt.y);
            case TextPosition of
              apTop:
                Draw(Rp, Pt.x, Pt.y + 5);
              apBottom:
                Draw(Rp, Pt.x, Pt.y - 5);
              apLeft:
                Draw(Rp, Pt.x + 5, Pt.y);
              apRight:
                Draw(Rp, Pt.x-5, Pt.y);
            end;
          end;
        end;
      end;
    end;
  end;
  //
  afterdot := 0;
  if aoShowLabels in Options then
  begin
    hg := Round(TH(RP, '|') * 1.2);
    disp := hg;
    hg2 := Round(TH(RP, '|') * 0.5);
    if (FAxisScale = atLin) and (Increment <> 0) then
    begin
//    if not (aoForceNoExp in Options)
//    then
      begin
        de := Increment / DivideFac;
        ds := ValueToStr(de, 5);
        if Pos(FormatSettings.DecimalSeparator, ds) = 0 then
          AfterDot := 0
        else
        begin
          for Fad := Pos(FormatSettings.decimalSeparator, ds) to Length(ds) do
            if Ds[Fad] <> '0' then
              AfterDot := Fad - Pos(FormatSettings.DecimalSeparator, ds);
        end;
      end;
      for i:=round(FMinValue / Increment) to round( FMaxValue / Increment) do
        if (i * Increment >= FMinValue) and (i * Increment <= FMaxValue) then
        begin
          pt := Place(i * Increment);
          ds := ValueToStr(i * increment / DivideFac, Afterdot);
          wd := TW(Rp, ds);
          case TextPosition of
            apBottom:
              GfxMove(Rp, pt.X-wd div 2, pt.Y + hg);
            apLeft:
              GfxMove(Rp, Pt.X-wd-hg2, pt.Y + hg2 div 2);
            apTop:
              GfxMove(Rp, pt.X-wd div 2, pt.Y - hg);
            apRight:
              GfxMove(Rp, pt.X+hg2, pt.Y + hg2 div 2);
          end;
          GfxText(Rp, PChar(ds), Length(ds))
        end;
    end
    else
      if (FMinValue > 0) and (FMaxValue > 0) then
      begin
        AfterDot := 0;
        w10 := TW(Rp, '10');
        for i := Round(ln(FMinValue) / ln10) to Round(ln(FMaxValue) / ln10) do
          if (Exp(i * ln10) >= FMinValue) and (Exp(i*ln10) <= FMaxValue) then
          begin
            pt:=Place(exp(i*ln10));
            ds:=inttostr(i);
            wd:=TW(RP,ds)+w10;
            case TextPosition of
              apBottom:
                begin
                  GfxMove(Rp, DrawRect.Left + pt.X-wd div 2, DrawRect.Top + pt.Y+hg2);
                  GfxText(Rp, '10', 2);
                  GfxMove(Rp, DrawRect.Left + pt.X-wd div 2+w10, DrawRect.Top + pt.Y+hg2-hg div 8);
                  GfxText(Rp, PChar(ds), Length(ds));
                end;
              apLeft:
                begin
                  GfxMove(Rp, DrawRect.Left + Pt.X-wd-hg2, DrawRect.Top + pt.Y-hg div 2);
                  GfxText(Rp, '10', 2);
                  GfxMove(Rp, DrawRect.Left + Pt.X-wd-hg2+w10, DrawRect.Top + pt.Y-hg div 2-hg div 8 div 8);
                  GfxText(Rp, PChar(ds), Length(ds));
                end;
              apTop:
                begin
                  GfxMove(Rp, DrawRect.Left + pt.X-wd div 2, DrawRect.Top + pt.Y-hg);
                  GfxText(Rp, '10', 2);
                  GfxMove(Rp, DrawRect.Left + pt.X-wd div 2+w10, DrawRect.Top + pt.Y-hg-hg div 8);
                  GfxText(Rp, PChar(ds), Length(ds));
                end;
              apRight:
                begin
                  GfxMove(Rp, DrawRect.Left + pt.X+hg2, DrawRect.Top + pt.Y-hg div 2);
                  GfxText(Rp, '10', 2);
                  GfxMove(Rp, DrawRect.Left + pt.X+hg2+w10, DrawRect.Top + pt.Y-hg div 2-hg div 8);
                  GfxText(Rp, PChar(ds), Length(ds));
                end;
            end;
          end;
      end;
  end;
  if aoShowTitle in Options then
  begin
    hg:=round(TH(Rp,'|')*1.2);
    hg2:=round(TH(Rp,'|')*0.2);
    ds:=Title;
  {*}      if trim(Prefix+AxUnit)<>''
    then
      ds:=ds+' ['+trim(Prefix+AxUnit)+']';
    ds2:=ds;
    while pos('{',ds2)>0 do
      delete(ds2,pos('{',ds2),1);
    while pos('}',ds2)>0 do
      delete(ds2,pos('}',ds2),1);
    while pos('_',ds2)>0 do
      delete(ds2,pos('_',ds2),1);
    while pos('^',ds2)>0 do
      delete(ds2,pos('^',ds2),1);
    wd:=TW(Rp,ds2);
    case TextPosition of
      apBottom:
        ParamNameOut(ds,Rp, Color, rect(
          DrawRect.Left + (Start.X+Stop.X-wd) div 2, DrawRect.Top + Start.Y+hg+disp,
          DrawRect.Left + (Start.X+Stop.X+wd) div 2, DrawRect.Top + Start.Y+hg+hg+disp));
      apLeft:
        ParamNameOut(ds,Rp, Color, rect(
          DrawRect.Left + Start.X-VertTextShift-hg, DrawRect.Top + (Start.Y+Stop.Y-wd) div 2,
          DrawRect.Left + Start.X-VertTextShift, DrawRect.Top + (Start.Y+Stop.Y+wd) div 2),true);
      apTop:
        ParamNameOut(ds,Rp, Color, rect(
          DrawRect.Left + (Start.X+Stop.X-wd) div 2, DrawRect.Top + Start.Y-hg-disp,
          DrawRect.Left + (Start.X+Stop.X+wd) div 2, DrawRect.Top + Start.Y-2*hg-disp));
      apRight:
        ParamNameOut(ds,Rp, Color, rect(
          DrawRect.Left + Start.X+VertTextShift+round(hg*0.3), DrawRect.Top + (Start.Y+Stop.Y-wd) div 2,
          DrawRect.Left + Start.X+VertTextShift+round(hg*1.3), DrawRect.Top + (Start.Y+Stop.Y+wd) div 2),true);
    end;
  end;
  UnSetColor(Pen);
end;

procedure TAxis.DrawGrid(Rp: PRastPort; ClipRect: TRect);
var
  i,j,n:integer;
  pt:TPoint;
  NoM:integer;
  r,g,b: LongWord;
  r1,r2,g1,g2,b1,b2: Byte;
  MajColor,MinColor: TColor;
  Pen: LongWord;
begin
  if aoMajorGrid in Options then
  begin
    // Calculate MajorGridColor/MinGridColor
    r := (Color and $ff0000) shr 16;
    g := Color and $00ff00 shr 8;
    b := Color and $0000ff;
    //
    n := 10;
    r1 := ((r + (255 * n)) div (n+1)) and $ff;
    g1 := ((g + (255 * n)) div (n+1)) and $ff;
    b1 := ((b + (255 * n)) div (n+1)) and $ff;

    n := 20;
    r2 := ((r + (255 * n)) div (n+1)) and $ff;
    g2 := ((g + (255 * n)) div (n+1)) and $ff;
    b2 := ((b + (255 * n)) div (n+1)) and $ff;

    MajColor := (r1 shl 16) or (g1 shl 8) or b1;
    MinColor := (r2 shl 16) or (g2 shl 8) or b2;
    //
    NoM := MinorNo + 1;
    if (FAxisScale = atLin) and (Increment <> 0) then
    begin
      for i:= Round(FMinValue / Increment * NoM) to Round(FMaxValue / Increment * NoM) do
      begin
        if (i * Increment / NoM > FMinValue) and (i * Increment / NoM < FMaxValue) then
        begin
          if i mod NoM = 0 then
          begin
            Pen := SetColor(RP, MajColor);
          end
          else
          begin
            if not (aoMinorGrid in Options) then
              Continue;
            Pen := SetColor(RP, MinColor);
          end;
          pt := Place(i * Increment / NoM);
          case TextPosition of
            apTop,apBottom:
              begin
                GfxMove(RP, pt.x, ClipRect.Top);
                Draw(RP, pt.x,ClipRect.Bottom);
              end;
            apLeft,apRight:
              begin
                GfxMove(RP, ClipRect.left,pt.y);
                Draw(RP, ClipRect.right,pt.y);
              end;
          end;
          UnsetColor(Pen);
        end;
      end;
    end
    else
      if (FMinValue > 0) and (FMaxValue > 0) then
      begin
        for i := Round(Ln(FMinValue) / ln10) - 1 to Round(Ln(FMaxValue) / ln10) do
        begin
          Pen := SetColor(RP, MajColor);
          pt := Place(Exp(i * ln10));
          if (Exp(i * ln10) > FMinValue) and (Exp(i * ln10) < FMaxValue) then
          begin
            case TextPosition of
              apTop,apBottom:
                begin
                  GfxMove(Rp, pt.x,ClipRect.Top);
                  Draw(RP, pt.x,ClipRect.Bottom);
                end;
              apLeft,apRight:
                begin
                  GfxMove(RP, ClipRect.left,pt.y);
                  Draw(RP, ClipRect.right,pt.y);
                end;
            end;
          end;
          UnSetColor(Pen);
          if aoMinorGrid in Options then
          begin
            Pen := SetColor(RP, MinColor);
            for j := 2 to 9 do
            begin
              pt := Place(Exp(i * ln10) * j);
              if (Exp(i * ln10) * j > FMinValue) and (Exp(i * ln10) * j < FMaxValue) then
                case TextPosition of
                  apTop,apBottom:
                    begin
                      GfxMove(RP, pt.x,ClipRect.Top);
                      Draw(RP, pt.x,ClipRect.Bottom);
                    end;
                  apLeft,apRight:
                    begin
                      GfxMove(RP, ClipRect.left,pt.y);
                      Draw(RP, ClipRect.right,pt.y);
                    end;
                end;
            end;
            UnSetColor(Pen);
          end;
        end;
      end;
  end;
end;


function TAxis.Place(value: Double): Classes.TPoint;
begin
  if FMinValue = FMaxValue then
  begin
    Result.x := 0;
    Result.Y := 0;
  end
  else
  begin
    try
      if FAxisScale = atlin then
      begin
        Result.x := Start.X + Round((Stop.X - Start.X) / (FMaxValue - FMinValue) * (Value - FMinValue));
        Result.y := Start.Y + Round((Stop.Y - Start.Y) / (FMaxValue - FMinValue) * (Value - FMinValue));
      end
      else
      begin
        if (Value > 0) and (FMinValue > 0) and (FMaxValue > 0) then
        begin
          Result.x := Start.X + Round((Stop.X - Start.X) / ln(FMaxValue / FMinValue) * ln(Value / FMinValue));
          Result.y := Start.Y + Round((Stop.Y - Start.Y) / ln(FMaxValue / FMinValue) * ln(Value / FMinValue));
        end
        else
        begin
          Result.x := Start.X - (Stop.X - Start.X);
          Result.y := Start.Y - (Stop.Y - Start.Y);
        end;
      end;
    except
      Result.x := 0;
      Result.Y := 0;
    end;
  end;
end;

function TAxis.Shift(Position: Classes.TPoint; Value: Double): Classes.TPoint;
begin
  if FMinValue = FMaxValue then
    Result := Position
  else
  begin
    try
      if FAxisScale = atlin then
      begin
        Result.x := Position.x + Round((Stop.X - Start.X) / (FMaxValue - FMinValue) * (Value - FMinValue));
        Result.y := Position.y + Round((Stop.Y - Start.Y) / (FMaxValue - FMinValue) * (Value - FMinValue));
      end
      else
      begin
        if (Value > 0) and (FMaxValue > 0) and (FMinValue > 0) then
        begin
          Result.x := Round((Stop.X - Start.X) / ln(FMaxValue / FMinValue) * ln(Value / FMinValue) + Position.x);
          Result.y := Round((Stop.Y - Start.Y) / ln(FMaxValue / FMinValue) * ln(Value / FMinValue) + Position.y);
        end
        else
        begin
          Result.x := Position.x - (Stop.X - Start.X);
          Result.y := Position.y - (Stop.Y - Start.Y);
        end;
      end;
    except
      Result:=Position
    end;
  end;
end;

function TAxis.PosToValue(Pos: integer): Double;
var
  h1, h2, h3: Double;
begin
  try
    case FPosition of
      apTop, apBottom:
        begin
          if (Stop.x = Start.X) or (FMaxValue = FMinValue) then
          begin
            Result := Start.X;
            Exit;
          end;
          if FAxisScale = atLin then
          begin
            h1 := (Pos - 1 - Start.X) / (Stop.X - Start.X) * (FMaxValue - FMinValue) + FMinValue;
            h2 := (Pos - Start.x) / (Stop.X - Start.X) * (FMaxValue - FMinValue) + FMinValue;
            h3 := (Pos + 1 - Start.x) / (Stop.X - Start.X) * (FMaxValue - FMinValue) + FMinValue;
          end
          else
          begin
            h1 := Exp((Pos - 1 - Start.x) / (Stop.X - Start.X) * Ln(FMaxValue / FMinValue)) * FMinValue;
            h2 := Exp((Pos - Start.x) / (Stop.X - Start.X) * Ln(FMaxValue / FMinValue)) * FMinValue;
            h3 := Exp((Pos + 1 - Start.x) / (Stop.X - Start.X) * Ln(FMaxValue / FMinValue)) * FMinValue;
          end;
        end
      else
        begin
          if (Stop.y = Start.y) or (FMaxValue <= FMinValue) then
          begin
            Result := Start.Y;
            Exit;
          end;
          if FAxisScale = atLin then
          begin
            h1 := (Pos - 1 - Start.y) / (Stop.y - Start.y) * (FMaxValue - FMinValue) + FMinValue;
            h2 := (Pos - Start.y) / (Stop.y-Start.y) * (FMaxValue - FMinValue) + FMinValue;
            h3 := (Pos + 1-Start.y) / (Stop.y-Start.y) * (FMaxValue - FMinValue) + FMinValue;
          end
          else
          begin
            h1 := Exp((Pos - 1 - Start.y) / (Stop.y-Start.y) * Ln(FMaxValue / FMinValue)) * FMinValue;
            h2 := Exp((Pos - Start.y) / (Stop.y-Start.y) * Ln(FMaxValue / FMinValue)) * FMinValue;
            h3 := Exp((Pos + 1 - Start.y) / (Stop.y-Start.y) * Ln(FMaxValue / FMinValue)) * FMinValue;
          end;
        end;
    end;
    h1 := Abs(h2 - h1) / 2;
    h3 := Abs(h2 - h3) / 2;
    if h3 > h1 then
      h1 := h3;
    h1 := Exp(Round(Ln(h1) / ln10 - 1) * ln10);
    Result := Round(h2 / h1) * h1;
  except
    Result:=0;
  end;
end;


function TAxis.GetWidth(RP: PRastPort; GetForced: boolean = True): integer;
var
  wd,wdh,wmax,i,hg: Integer;
  ds:string;
  de: Double;
  fad: Integer;
begin
  //writeln('--> get width ', FPosition, ' textpos', TextPosition);
  Result := 0;
  hg := round(1.1 * TH(RP, '-0.123456789'));
  if hg < 8 then
    hg := 8;
  wd := 10;
  //wd := TW(RP, '888.888');
  //if wd < 10 then
  //  wd := 10;
  if Textposition in [apLeft, apRight] then
    SetIncrement(hg)
  else
    SetIncrement(wd);
  if (aoForceWidth in Options) and GetForced then
  begin
    //writeln('  force');
    //Result := ForcedWidth;
    //Canvas.Font.Assign(Styles.TitleFont);
    //VertTextShift:=ForcedWidth-round(TH(Canvas,'|')*1);
  end
  else
  begin
    wmax := 0;
    if aoShowLabels in Options then
    begin
      case TextPosition of
        apTop, apBottom:
          begin
            wd := wd + Round(TH(RP, '|') * 1.2);
            //writeln('  top/bottom , showlabels');
          end;
        apLeft, apRight:
          begin
            //writeln('  left/right , showlabels');
            wmax := 10;
            de := Increment / DivideFac;
            ds := ValueToStr(de, 5);
            afterdot := 0;
            if Pos(FormatSettings.decimalSeparator, ds) = 0 then
              afterdot := 0
            else
            begin
              for fad := Pos(FormatSettings.decimalSeparator, ds) to Length(ds) do
                if ds[fad] <> '0' then
                  afterdot := fad - Pos(FormatSettings.DecimalSeparator, ds);
            end;
            if FAxisScale = atlin then
            begin
              if Increment > 0 then
                for i := Round(FMinValue / Increment) to Round(FMaxValue / Increment) do
                begin
                  ds := ValueToStr(i * Increment / DivideFac, afterdot);
                  wdh := TW(Rp, ds);
                  if wdh > wmax then
                    wmax := wdh;
                end;
            end
            else
              wmax := TW(RP, '10888');
            wd := wd + wmax;
            VertTextShift := wmax;
          end;
      end;
    end;
    if (aoShowTitle in Options) and (Title <> '') then
    begin
      //writeln('  ShowTitles');
      wd := wd + Round(TH(RP, '|') * 1.2);
    end;
    wd := wd;
    //writeln('  WD ', wd, ' wmax ', wmax);
    Result := wd;
  end;
  //writeln('<-- get width ', result);
end;


procedure TAxis.SetIncrement(MinDist: Integer);
var
  i, j, jopt: Integer;
  AbsMax: Double;
  d1: Integer;
  pt1,pt2: Classes.TPoint;
const
  goodInc: array[1..18] of integer =
   (10,15,20,25,30,40,50,75,100,150,200,250,300,400,500,750,1000,1500);
  goodMinorNumber: array[1..18] of integer =
   ( 4, 2, 4, 4, 2, 3, 4, 2,  4,  2,  4,  4,  2,  3,  4,  2,   4,   2);
begin
  if FMinValue = FMaxValue then
    Exit;
  //
  Increment := Abs(FMaxValue - FMinValue) / 10;
  i := 0;
  while Round(Increment * Exp(i * ln10)) < 10 do
    Inc(i);
  while (Round(Increment * Exp(i * ln10)) >= 100) or (Increment * Exp(i * ln10) >= 1000) do
    Dec(i);
  Increment := Round(Increment * exp(i * ln10));
  //
  JOpt := 1;
  for j := 2 to Length(GoodInc) do
    if (Abs(goodInc[j] - Increment) < Abs(goodInc[JOpt] - Increment)) and not ((aoForceInteger in Options) and (j in [2,4,8,10,12,16,18])) then
      JOpt := j;
  if FAxisScale = atLin then
  begin
    pt1 := Place(0);
    pt2 := Place(goodInc[jOpt] / Exp(i * ln10));
    d1 := Round(Sqrt(Sqr(pt1.X - pt2.x) + sqr(pt1.y-pt2.y)));
    while (jopt<length(goodInc)) and (d1<MinDist) do
    begin
      Inc(JOpt);
      if (aoForceInteger in Options) and (jopt in [2,4,8,10,12,16,18]) then
        Continue;
      pt2 := Place(goodInc[jOpt] / Exp(i * ln10));
      d1 := Round(Sqrt(Sqr(pt1.X - pt2.x) + Sqr(pt1.y - pt2.y)));
    end;
    Increment := goodInc[JOpt] / Exp(i * ln10);
    MinorNo := GoodMinorNumber[JOpt];
  end;
  i := 0;
  AbsMax := Abs(FMaxValue);
  if Abs(FMinValue) > AbsMax then
    AbsMax := Abs(FMinValue);
  if (Pos('²', AxUnit) > 0) and not (pos('/',AxUnit) > 0) then
  begin
    while AbsMax * Exp(i * 3 * 2 * ln10) < 0.009 do
      Inc(i);
    while AbsMax * Exp(i * 3 * 2 * ln10) > 11000 do
      Dec(i);
  end
  else
  begin
    while AbsMax * Exp(i * 3 * ln10) < 0.9 do
      Inc(i);
    while AbsMax * Exp(i * 3 * ln10) > 1100 do
      Dec(i);
  end;
  i := -i;
//  if abs(i)>8 then i:=0;
  if (FAxisScale = atLin) and not (aoForceNoExp in Options) then
  begin
    if (Trim(AxUnit) <> '') and (Abs(i) <= 8) then
      Prefix := PreFixChar[i]
    else
      if i<>0 then
        Prefix := '10{^' + IntToStr(3 * i) + '}'
      else
        Prefix := '';
    if (Pos('²', AxUnit) > 0) and not (Pos('/', AxUnit) > 0) then
      DivideFac := Exp(i * 3 * 2 * ln10)
    else
      Dividefac := Exp(i * 3 * ln10);
  end
  else
  begin
    Prefix := PreFixChar[0];
    DivideFac := 1;
  end;
end;


procedure TAxis.ValidateMinMax;
begin
  if FMaxValue = FMinValue then
  begin
    if FMaxValue <> 0 then
    begin
      FMinValue := 0;
      FMaxValue := 2 * FMaxValue;
    end
    else
    begin
      FMinValue := -0.5;
      FMaxValue := 0.5;
    end;
  end;
end;

function PopupCurEvent(Hook: PHook; Obj: PObject_; AMsg: Pointer): NativeInt;
var
  Plot: TPlotPanel;
  Idx: Integer;
begin
  Result := 0;
  Plot := TPlotPanel(Hook^.h_Data);
  if Assigned(Plot) and Assigned(Obj) then
  begin
    Idx := MH_Get(Obj, MUIA_UserData);
    Plot.PopupEvent(Idx);
  end;
end;


// #####################################################################
//   TPlotPanel.Create
constructor TPlotPanel.Create(const Args: array of PtrUInt);
var
  i: Integer;
begin
  inherited;
  OldDrawer := ExtractFilePath(ParamStr(0));
  SetLength(PM, PID_Last + 1);
  CrossPos := Point(-1,-1);
  CrossText := TStringList.Create;

  FShowMarker := False;
  FXMarker := 0;

  MinWidth := 300;
  MinHeight := 200;

  FTitle := 'Example Title';
  OnDrawObject := @DrawEvent;
  //OnMUIDblClick := @MouseDblClick;
  //OnMUIMouseDown := @MouseDownEvent;
  //OnMUIMouseUp := @MouseUpEvent;
  //OnMUIMouseMove := @MouseMoveEvent;
  //OnMUIMouseWheel := @MouseWheelEvent;

  //OnMUIKeyDown := @KeyDownEvent;
  FAxisTop := TAxis.Create;
  FAxisTop.FPosition := apTop;
  FAxisTop.TextPosition := apTop;
  FAxisTop.MinValue := 0;
  FAxisTop.MaxValue := 100;
  FAxisTop.Color := clBlack;
  FAxisTop.Options := [];

  FAxisLeft := TAxis.Create;
  FAxisLeft.FPosition := apLeft;
  FAxisLeft.TextPosition := apLeft;
  FAxisLeft.MinValue := 0;
  FAxisLeft.MaxValue := 100;
  FAxisLeft.Color := clBlack;
  FAxisLeft.Options := [aoShowTics,aoShowLabels,aoShowTitle];

  FAxisBottom := TAxis.Create;
  FAxisBottom.FPosition := apBottom;
  FAxisBottom.TextPosition := apBottom;
  FAxisBottom.MinValue := 0;
  FAxisBottom.MaxValue := 100;
  FAxisBottom.Color := clBlack;
  FAxisBottom.Options := [aoShowTics,aoShowLabels,aoShowTitle];

  FAxisRight := TAxis.Create;
  FAxisRight.FPosition := apRight;
  FAxisRight.TextPosition := apRight;
  FAxisRight.MinValue := 0;
  FAxisRight.MaxValue := 100;
  FAxisRight.Color := clBlack;
  FAxisRight.Options := [];
  //
  FForceRedraw := True;
  FRastPort := nil;
  FLayerInfo := nil;
  RPDrawRect := Rect(0,0, 0, 0);
  //
  OnMUIMouseDown := @MouseDownEvent;
  OnMUIMouseUp := @MouseUpEvent;
  OnMUIMouseMove := @MouseMoveEvent;
  OnMUIMouseLeave := @MouseLeaveEvent;
  // Popupmenu
  FMouseModus := mmZoom;
  //
  PopUpMenu := MH_Menustrip([
    Child, AsTag(MH_Menu([                      // 'Plot'
      MUIA_MENU_Title, AsTag(PChar(Menutitle_Plot)),
      Child, AsTag(MH_MenuItem(PM[0], [
        MUIA_Menuitem_Title, AsTag(PChar(Menutitle_Rescale)),   // 'Rescale'
        MUIA_UserData, PID_Rescale,
        TAG_DONE])),
      Child, AsTag(MH_MenuItem([MUIA_Menuitem_Title, AsTag(-1), TAG_DONE])),
      Child, AsTag(MH_MenuItem(PM[1], [
        MUIA_Menuitem_Title, AsTag(PChar(Menutitle_Zoom)),   // 'Zoom'
        MUIA_Menuitem_Checked, AsTag(True),
        MUIA_Menuitem_Toggle, AsTag(False),
        MUIA_Menuitem_Checkit, AsTag(True),
        MUIA_Menuitem_Exclude, AsTag((1 shl 3) or (1 shl 4) or (1 shl 5)),
        MUIA_UserData, PID_Zoom,
        TAG_DONE])),
      Child, AsTag(MH_MenuItem(PM[2], [
        MUIA_Menuitem_Title, AsTag(PChar(Menutitle_Position)),   // 'Show Position'
        MUIA_Menuitem_Toggle, AsTag(False),
        MUIA_Menuitem_Checkit, AsTag(True),
        MUIA_Menuitem_Exclude, AsTag((1 shl 2) or (1 shl 4) or (1 shl 5)),
        MUIA_UserData, PID_Position,
        TAG_DONE])),
      Child, AsTag(MH_MenuItem(PM[3], [
        MUIA_Menuitem_Title, AsTag(PChar(Menutitle_Data)),   // 'Show Curve Data'
        MUIA_Menuitem_Toggle, AsTag(False),
        MUIA_Menuitem_Checkit, AsTag(True),
        MUIA_Menuitem_Exclude, AsTag((1 shl 2) or (1 shl 3) or (1 shl 5)),
        MUIA_UserData, PID_Data,
        TAG_DONE])),
      Child, AsTag(MH_MenuItem(PM[4], [
        MUIA_Menuitem_Title, AsTag(PChar(Menutitle_Marker)),   // 'Set Marker'
        MUIA_Menuitem_Toggle, AsTag(False),
        MUIA_Menuitem_Checkit, AsTag(True),
        MUIA_Menuitem_Exclude, AsTag((1 shl 2) or (1 shl 3) or (1 shl 4)),
        MUIA_UserData, PID_Marker,
        TAG_DONE])),
      Child, AsTag(MH_MenuItem([MUIA_Menuitem_Title, AsTag(-1), TAG_DONE])),
      Child, AsTag(MH_MenuItem(PM[5], [
        MUIA_Menuitem_Title, AsTag(PChar(MenuTitle_ASCII)),   // 'Export as ASCII'
        MUIA_UserData, PID_ASCII,
        TAG_DONE])),
      Child, AsTag(MH_MenuItem(PM[6], [
        MUIA_Menuitem_Title, AsTag(PChar(MenuTitle_PNG)),   // 'Export as ASCII'
        MUIA_UserData, PID_PNG,
        TAG_DONE])),
      TAG_DONE])),
    TAG_DONE]);
  MH_Set(MUIObject, MUIA_ContextMenu, AsTag(PopUpMenu));

  // Popupmenu events
  for i := 0 to High(PM) do
    if Assigned(PM[i]) then
      ConnectHookFunction(MUIA_Menuitem_Trigger, MUIV_EveryTime, PM[i], Self, @PopupHook, @PopupCurEvent);
end;

//#######################################
// Destroy
destructor TPlotPanel.Destroy;
begin
  CrossText.Free;
  FAxisTop.Free;
  FAxisLeft.Free;
  FAxisBottom.Free;
  FAxisRight.Free;
  if Assigned(FRastPort) then
  begin
    // delete the layer
    DeleteLayer(0, FRastPort^.layer);
    DisposeLayerInfo(FLayerInfo);
    // delete the bitmap
    FreeBitmap(FRastPort^.Bitmap);
    FRastPort^.Layer := nil;
    FRastPort^.Bitmap := nil;
    // Destroy the temp rastport
    FreeRastPort(FRastPort);
  end;
  inherited;
end;

//#################################################
//  PupUpEvent
procedure TPlotPanel.PopupEvent(EventID: Integer);
var
  OldModus: TMouseMode;
begin
  OldModus := FMouseModus;
  case EventID of
    PID_Rescale:
    begin
      RescaleByCurves;
      FForceRedraw := True;
      RedrawObject;
    end;
    PID_Zoom:
    begin
      FMouseModus := mmZoom;
      CrossPos := Point(-1, -1);
      RedrawObject;
    end;
    PID_Position:
    begin
      FMouseModus := mmPosition;
      MouseDown := False;
    end;
    PID_Data:
    begin
      FMouseModus := mmData;
      MouseDown := False;
    end;
    PID_Marker:
    begin
      FMouseModus := mmMarker;
      MouseDown := False;
    end;
    PID_ASCII: ExportAscii;
    PID_PNG: ExportPNG;
  end;
  if OldModus = mmMarker then
  begin
    RedrawObject;
    CrossPos := Point(-1, -1);
    if Assigned(FOnMarkerChange) then
      FOnMarkerChange(0);
  end;
end;

procedure TPlotPanel.ExportPNG(FileName: string = '');
var
  fr: PFileRequester;
  SavePict: TFPAMemImage;
  Writer: TFPWriterPNG;
begin
  // if no Filename is given ask for it
  if Filename = '' then
  begin
    Filename := 'Plot.png';
    fr := AllocAslRequestTags(ASL_FileRequest, [
      NativeUInt(ASLFR_TitleText),      NativeUInt(PChar('Choose file to save the image')),
      NativeUInt(ASLFR_InitialPattern), NativeUInt(PChar('(#?.png)')),
      NativeUInt(ASLFR_InitialFile),    NativeUInt(PChar(Filename)),
      NativeUInt(ASLFR_InitialDrawer),  NativeUInt(PChar(OldDrawer)),
      NativeUInt(ASLFR_DoPatterns),     LTrue,
      NativeUInt(ASLFR_DoSaveMode),     LTrue,
    TAG_END]);
    if Assigned(fr) then
    begin
      if AslRequestTags(fr, [TAG_END]) then
      begin
        {$if defined(VER3_0) or defined(MorphOS) or defined(Amiga68k)}
        Filename := IncludeTrailingPathDelimiter(string(fr^.rf_dir)) + string(fr^.rf_file);
        {$else}
        Filename := IncludeTrailingPathDelimiter(string(fr^.fr_drawer)) + string(fr^.fr_file);
        {$endif}
        OldDrawer := ExtractFilePath(Filename);
      end;
      FreeAslRequest(fr);
    end;

  end;
  if FileName = '' then
    Exit;
  ChangeFileExt(Filename, '.png');
  try
    SavePict := TFPAMemImage.Create(RPDrawRect.Width, RPDrawRect.Height);
    ReadPixelArray(SavePict.Data, 0, 0, SavePict.Width * SizeOf(LongWord), FRastPort, 0, 0, SavePict.Width, SavePict.Height, RECTFMT_RGBA);
    Writer := TFPWriterPNG.Create;
    SavePict.SaveToFile(FileName, Writer);
  finally
    Writer.Free;
    SavePict.Free;
  end;
  //ClipBlit(FRastPort, 0,0, rp, DrawRect.Left, DrawRect.Top, DrawRect.Width, DrawRect.Height, $00C0);

end;

procedure TPlotPanel.ExportASCII(FileName: string = '');
var
  fr: PFileRequester;
  SL: TStringList;
  Val, Line: string;
  i, j, MaxLength: Integer;
begin
  // if no Filename is given ask for it
  if Filename = '' then
  begin
    FileName := 'Plot.txt';
    fr := AllocAslRequestTags(ASL_FileRequest, [
      NativeUInt(ASLFR_TitleText),      NativeUInt(PChar('Choose file to save data as text')),
      NativeUInt(ASLFR_InitialPattern), NativeUInt(PChar('(#?.txt)')),
      NativeUInt(ASLFR_InitialFile),    NativeUInt(PChar(Filename)),
      NativeUInt(ASLFR_InitialDrawer),  NativeUInt(PChar(OldDrawer)),
      NativeUInt(ASLFR_DoPatterns),     LTrue,
      NativeUInt(ASLFR_DoSaveMode),     LTrue,
    TAG_END]);
    if Assigned(fr) then
    begin
      if AslRequestTags(fr, [TAG_END]) then
      begin
        {$if defined(VER3_0) or defined(MorphOS) or defined(Amiga68k)}
        Filename := IncludeTrailingPathDelimiter(string(fr^.rf_dir)) + string(fr^.rf_file);
        {$else}
        Filename := IncludeTrailingPathDelimiter(string(fr^.fr_drawer)) + string(fr^.fr_file);
        {$endif}
        OldDrawer := ExtractFilePath(Filename);
      end;
      FreeAslRequest(fr);
    end;
  end;
  if FileName = '' then
    Exit;
  SL := TStringList.Create;
  try
    Maxlength := 0;
    for i := 0 to High(Curves) do
    begin
      MaxLength := Max(MaxLength, Length(Curves[i].X));
      MaxLength := Max(MaxLength, Length(Curves[i].Y));
    end;
    //
    Line := '';
    for i := 0 to High(Curves) do
    begin
      if i = 0 then
        Line := Line + 'X' + IntToStr(i+1)
      else
        Line := Line + #9 + 'X' + IntToStr(i+1);
      Line := Line + #9 + Curves[i].Name;
    end;
    SL.Add(Line);
    //
    for i := 0 to MaxLength - 1 do
    begin
      Line := '';
      for j := 0 to High(Curves) do
      begin
        if i <= High(Curves[j].X) then
          Val := FloatToStr(Curves[j].X[i])
        else
          Val := ' ';
        if Line = '' then
          Line := Line + Val
        else
          Line := Line + #9 + Val;
        if i <= High(Curves[j].Y) then
          Val := FloatToStr(Curves[j].Y[i])
        else
          Val := ' ';
        Line := Line + #9 + Val;
      end;
      SL.Add(Line);
    end;
    Sl.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

//##########################################
// DrawEvent
procedure TPlotPanel.DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
begin
  DoPlotDraw(RP, DrawRect);
end;

// ##########################################
// DoPlotDraw
procedure TPlotPanel.DoPlotDraw(RP: PRastPort; DrawRect: TRect; ScaleOnly: Boolean = False);
var
  lf,tp,rg,bt,i,fsze: Integer;
  //iw,ih: Integer;
  Pen: LongWord;
  TextRect: TRect;
  TextH, TextW: Integer;
begin
  if FForceRedraw or not assigned(FRastPort) or (RPDrawRect.Width <> DrawRect.Width) or (RPDrawRect.Height <> DrawRect.Height) then
  begin
    if Assigned(FRastPort) then
    begin
      DeleteLayer(0, FRastPort^.layer);
      DisposeLayerInfo(FLayerInfo);
      // delete the bitmap
      FreeBitmap(FRastPort^.Bitmap);
      FRastPort^.Layer := nil;
      FRastPort^.Bitmap := nil;
      // Destroy the temp rastport
      FreeRastPort(FRastPort);
    end;
    FRastPort := CreateRastPort;
    FLayerInfo := NewLayerInfo(); // Layerinfo we also need
    FRastPort^.Bitmap := AllocBitMap(DrawRect.Width, DrawRect.Height, rp^.Bitmap^.Depth, BMF_MINPLANES or BMF_DISPLAYABLE, rp^.Bitmap);
    FRastPort^.Layer := CreateUpFrontHookLayer(FLayerInfo, FRastPort^.Bitmap, 0, 0, DrawRect.Width - 1, DrawRect.Height - 1, LAYERSIMPLE, nil, nil);
    // initialize Font
    SetFont(FRastPort, rp^.Font);
    // initialize to colors
    SetAPen(FRastPort, 0);
    SetBPen(FRastPort, 0);
    // set Drawrect of the Buffer RastPort
    RPDrawRect.Top := 0;
    RPDrawRect.Left := 0;
    RPDrawRect.Width := DrawRect.Width;
    RPDrawRect.Height := DrawRect.Height;

    {if inset <> '' then
    begin
      iw := TW(RP, inset);
      ih := TH(RP, '|');
    end
    else
    begin
      iw:=0;
      ih:=0;
    end;}
    Fsze := TH(RP, '|');
   // RescaleByCurves;
    for i:=0 to 2 do
    begin
      lf := RPDrawRect.Left + AxisLeft.GetWidth(FRastPort) + Round(1 / 20 * fsze);
      rg := RPDrawRect.Right - AxisRight.GetWidth(FRastPort) - 1 - Round(1 / 20 * fsze);
      tp:=  RPDrawRect.Top + AxisTop.GetWidth(FRastPort) + Round(1 / 20 * fsze);
      bt := RPDrawRect.Bottom - AxisBottom.GetWidth(FRastPort) - 1 - Round(1 / 20 * fsze);
      with AxisTop do
      begin
        Start:=Point(lf,tp);
        Stop:=Point(rg,tp);
      end;
      with AxisBottom do
      begin
        Start:=Point(lf,bt);
        Stop:=Point(rg,bt);
      end;
      with AxisLeft do
      begin
        Start:=Point(lf,bt);
        Stop:=Point(lf,tp);
      end;
      with AxisRight do
      begin
        Start:=Point(rg,bt);
        Stop:=Point(rg,tp);
      end;
    end;
    if ScaleOnly then
      exit;
    Pen := SetColor(FRastPort, $FFFFFF);
    RectFill(FRastPort, 0, 0, RPDrawRect.Width, RPDrawRect.Height);
    UnSetColor(Pen);
    //
    //DrawFields(RP, Rect(lf, tp, rg, bt));
    //
    AxisLeft.DrawGrid(FRastPort,rect(lf,tp,rg,bt));
    AxisRight.DrawGrid(FRastPort,rect(lf,tp,rg,bt));
    AxisTop.DrawGrid(FRastPort,rect(lf,tp,rg,bt));
    AxisBottom.DrawGrid(FRastPort,rect(lf,tp,rg,bt));

    DrawCurves(FRastPort, Rect(lf,tp,rg,bt));
    DrawMarker(FRastPort, Rect(lf,tp,rg,bt));
    //
    Pen := SetColor(FRastPort, $000000);
    SetDrMd(FRastPort, JAM1);

    AxisTop.DoDraw(FRastPort, RPDrawRect);
    AxisBottom.DoDraw(FRastPort, RPDrawRect);
    AxisLeft.DoDraw(FRastPort, RPDrawRect);
    AxisRight.DoDraw(FRastPort, RPDrawRect);
    //
    if trim(inset) <> '' then
    begin
      {canvas.Brush.Color:=styles.BackgroundColor;
      canvas.Pen.Width:=1;
      canvas.Pen.Color:=styles.FrameColor;
      Canvas.Rectangle(rect(rg-3*ih-iw,tp+ih,rg-ih,tp+4*ih));
      Canvas.TextRect(rect(rg-2*ih-iw,tp+2*ih,rg-2*ih,tp+3*ih-1),
        rg-2*ih-iw,tp+2*ih,inset);}
    end;

    UnSetColor(Pen);
    FForceRedraw := False;
  end;
  ClipBlit(FRastPort, 0,0, rp, DrawRect.Left, DrawRect.Top, DrawRect.Width, DrawRect.Height, $00C0);
  // Draw Zoomrect if needed
  if MouseDown then
  begin
    Pen := SetColor(RP, $FF00FF);
    SetDrMd(RP, COMPLEMENT);
    GfxMove(RP, DrawRect.Left + ZoomBox.Left, DrawRect.Top + ZoomBox.Top);
    Draw(RP, DrawRect.Left + ZoomBox.Right, DrawRect.Top + ZoomBox.Top);
    Draw(RP, DrawRect.Left + ZoomBox.Right, DrawRect.Top + ZoomBox.Bottom);
    Draw(RP, DrawRect.Left + ZoomBox.Left, DrawRect.Top + ZoomBox.Bottom);
    Draw(RP, DrawRect.Left + ZoomBox.Left, DrawRect.Top + ZoomBox.Top);
    UnSetColor(Pen);
  end;
  if (CrossPos.X >= 0) and (CrossPos.Y >= 0) then
  begin
    Pen := SetColor(RP, $0000FF);
    SetDrMd(RP, COMPLEMENT);
    if FMouseModus <> mmMarker then
    begin
      GfxMove(RP, DrawRect.Left + CrossPos.X - 5, DrawRect.Top + CrossPos.Y);
      Draw(RP, DrawRect.Left + CrossPos.X + 5, DrawRect.Top + CrossPos.Y);
      GfxMove(RP, DrawRect.Left + CrossPos.X, DrawRect.Top + CrossPos.Y - 5);
      Draw(RP, DrawRect.Left + CrossPos.X, DrawRect.Top + CrossPos.Y + 5);
      UnSetColor(Pen);
      GfxMove(RP, DrawRect.Left + CrossPos.X, DrawRect.Top + CrossPos.Y - 20);
    end;

    TextH := Round((TH(RP, '|') * 1.2));
    TextW := 0;
    for i := 0 to CrossText.Count - 1 do
      TextW := Max(TextW, TW(RP, CrossText[i]));
    // calculate Text rectangle
    TextRect.Left := DrawRect.Left + CrossPos.X + 5;
    TextRect.Top := DrawRect.Top + CrossPos.Y + 5;
    TextRect.Width := TextW + 10;
    TextRect.Height := TextH * CrossText.Count + 5;
    if TextRect.Right > DrawRect.Right then
    begin
      TextRect.SetLocation(TextRect.Left - TextRect.Width - 10, TextRect.Top);
    end;
    if TextRect.Top - TextRect.Height > DrawRect.Top then
    begin
      TextRect.SetLocation(TextRect.Left, TextRect.Top - TextRect.Height - 10);
    end;
    // Draw background
    SetDrMd(RP, JAM1);
    Pen := SetColor(RP, $FFFFAA);
    RectFill(Rp, TextRect.Left, TextRect.Top, TextRect.Right, TextRect.Bottom);
    UnSetColor(Pen);
    Pen := SetColor(RP, $000000);
    GfxMove(RP, TextRect.Left, TextRect.Top);
    Draw(RP, TextRect.Right, TextRect.Top);
    Draw(RP, TextRect.Right, TextRect.Bottom);
    Draw(RP, TextRect.Left, TextRect.Bottom);
    Draw(RP, TextRect.Left, TextRect.Top);
    for i := 0 to CrossText.Count - 1 do
    begin
      GfxMove(RP, TextRect.Left + 5, (TextRect.Top + (i * TextH)) + (TextH div 2) + 5);
      GfxText(RP, PChar(CrossText[i]), Length(CrossText[i]));
    end;
    UnSetColor(Pen);
  end;

end;

//###############################
// DrawMarker
procedure TPlotPanel.DrawMarker(RP: PRastPort; ClipRect: TRect);
var
  loc: Classes.TPoint;
  DoMove: Boolean;
  Pen: LongWord;
begin
  if (FShowMarker) and (FMouseModus = mmMarker) then
  begin
    Pen := SetColor(Rp, clBlue);
    loc := Curves[0].XAxis.Place(FXMarker);
    DoMove := True;
    ClippedLine(Rp, Loc.X, ClipRect.Top, Loc.X, ClipRect.Bottom, ClipRect, DoMove);
    UnSetColor(Pen);
    if (FMouseModus = mmMarker) then
      CrossPos.X := Loc.X;
  end;
end;

//##################################
// DrawCurves
procedure TPlotPanel.DrawCurves(RP: PRastPort; ClipRect: TRect);
var
  i,j:integer;
  loc: Classes.TPoint;
  ox,oy{,bx,by}:integer;
  //wd,wi:integer;
  //dx,dy:integer;
  ov,DoMove:boolean;
  Pen: LongWord;
begin
  //wd:=TH(RP,'|');
  for i:=0 to Length(Curves)-1 do
  begin
    if Length(Curves[i].x) = 0 then
      Continue;
    Pen := SetColor(Rp, Curves[i].Color);
    //
    loc := Curves[i].XAxis.Place(Curves[i].x[0]);
    Loc := Curves[i].YAxis.Shift(loc, Curves[i].y[0]);
    ox := loc.x;
    oy := loc.y;
    ov := Curves[i].valid[0];
    DoMove := True;
    //if not (Curves[i].ScatterStyle=scsBar) then
    begin
      for j := 1 to Length(Curves[i].x) - 1 do
      begin
        loc := Curves[i].XAxis.Place(Curves[i].x[j]);
        Loc := Curves[i].YAxis.Shift(Loc, Curves[i].y[j]);
        if ov and DoMove then
          ClippedLine(Rp, ox, oy, ox, oy, ClipRect, DoMove);
        if Curves[i].valid[j] and ov then
          ClippedLine(Rp, ox, oy, loc.x, loc.y, ClipRect, DoMove);
        if not ov then
          DoMove := True;
        ox := loc.x;
        oy := loc.y;
        ov := Curves[i].valid[j];
      end;
    end;
    //
    UnSetColor(Pen)
  end;
end;

//###############################
// AddCurve
procedure TPlotPanel.AddCurve(var x,y:array of double; var valid:array of boolean;
      XAxis,YAxis:TAxisPosition; Color:TColor=$000000; Name:string=''; Index: Integer = -1);
var
  i,vlgt,si:integer;
begin
  if Index=-1 then
  begin
    si:=length(Curves);
    Setlength(Curves,si+1);
  end
  else
  begin
    if Index >= Length(Curves) then
      Setlength(Curves, Index + 1);
    si := Index;
  end;
  Setlength(Curves[si].x, Length(x));
  Setlength(Curves[si].y, Length(y));
  Setlength(Curves[si].valid, Length(y));
  vlgt := Length(Valid);
  for i := 0 to length(x) - 1 do
    Curves[si].x[i] := x[i];
  for i := 0 to Length(y) - 1 do
  begin
    Curves[si].y[i] := y[i];
    if i < vlgt then
      Curves[si].valid[i] := Valid[i]
    else
      Curves[si].valid[i] := True;
  end;
  case XAxis of
    apTop: Curves[si].XAxis := AxisTop;
    apBottom: Curves[si].XAxis := AxisBottom;
    apLeft: Curves[si].XAxis := AxisLeft;
    apRight: Curves[si].XAxis := AxisRight;
  end;
  case YAxis of
    apTop: Curves[si].YAxis := AxisTop;
    apBottom: Curves[si].YAxis := AxisBottom;
    apLeft: Curves[si].YAxis := AxisLeft;
    apRight: Curves[si].YAxis := AxisRight;
  end;
  Curves[si].XAxPos := XAxis;
  Curves[si].YAxPos := YAxis;
  //Curves[si].Width:=Width;
  Curves[si].Name := Name;
  //Curves[si].ScatterStyle:=ScatterStyle;
  //Curves[si].ScatterSize:=ScatterWidth;
  Curves[si].Color := Color;
  //Curves[si].LinesOn:=LinesOn;
end;

//#######################################
// Rescale By Curves
procedure TPlotPanel.RescaleByCurves;
var i,j:integer;
begin
  with AxisTop do
  begin
    MinSet:=false;
    MaxSet:=false;
  end;
  with AxisLeft do
  begin
    MinSet:=false;
    MaxSet:=false;
  end;
  with AxisRight do
  begin
    MinSet:=false;
    MaxSet:=false;
  end;
  with AxisBottom do
  begin
    MinSet:=false;
    MaxSet:=false;
  end;
  for i := 0 to Length(Curves) - 1 do
  begin
    for j := 0 to Length(Curves[i].x) - 1 do
    begin
      with Curves[i].XAxis do
      begin
        if not (aoMinFixed in Options) then
        begin
          if ((not MinSet) or (Curves[i].x[j] < FMinValue)) and ((FAxisScale = atLin) or (Curves[i].x[j] > 1e-10)) then
          begin
            FMinValue := Curves[i].x[j];
            MinSet := True;
          end;
          end;
        if not (aoMaxFixed in Options) then
        begin
          if not MaxSet then
            FMaxValue := Curves[i].x[j]
          else
            if Curves[i].x[j] > FMaxValue then
              FMaxValue := Curves[i].x[j];
        end;
        MaxSet := True;
      end;
    end;
    for j := 0 to Length(Curves[i].y) - 1 do
    begin
      with Curves[i].YAxis do
      begin
        if not (aoMinFixed in Options) then
        begin
          if ((not MinSet) or (Curves[i].y[j] < FMinValue)) and ((FAxisScale = atLin) or (Curves[i].y[j] > 1e-10)) then
          begin
            FMinValue := Curves[i].y[j];
            MinSet := True;
          end;
        end;
        if not (aoMaxFixed in Options) then
        begin
          if not MaxSet then
            FMaxValue := Curves[i].y[j]
          else
            if Curves[i].y[j] > FMaxValue then
              FMaxValue := Curves[i].y[j];
        end;
        MaxSet:=true;
      end;
    end;
  end;
  AxisTop.ValidateMinMax;
  AxisBottom.ValidateMinMax;
  AxisLeft.ValidateMinMax;
  AxisRight.ValidateMinMax;
end;

// ##############################
// Mouse Down
procedure TPlotPanel.MouseDownEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
var
  FoundCurve: Integer;
begin
  if MouseBtn = mmbLeft then
  begin
    if FMouseModus = mmZoom then
    begin
      MouseDown := True;
      ZoomBox.Left := X;
      ZoomBox.Top := Y;
      ZoomBox.Width := 1;
      ZoomBox.Height := 1;
      RedrawObject;
      EatEvent := True;
    end;
    if FMouseModus = mmMarker then
    begin
      if FindNearestCurve(x, y, FMarkerLoc, FoundCurve, FXMarkerValueIdx) then
      begin
        FXMarker := Curves[FoundCurve].x[FXMarkerValueIdx];
        FShowMarker := True;
        FForceRedraw := True;
        RedrawObject;
        if Assigned(FOnMarkerChange) then
          FOnMarkerChange(0);
      end;
    end;
  end;
end;

//#####################################
// Mouse up Event
procedure TPlotPanel.MouseUpEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
var
  a, b: Double;
begin
  if MouseBtn = mmbLeft then
  begin
    if MouseDown then
    begin
      MouseDown := False;
      if (ZoomBox.Width > 2) and (ZoomBox.Height > 2) then
      begin
        ZoomBox.Right := X;
        ZoomBox.Bottom := Y;
        a := AxisLeft.PosToValue(ZoomBox.Bottom);
        b := AxisLeft.PosToValue(ZoomBox.Top);
        AxisLeft.MinValue := a;
        AxisLeft.MaxValue := b;
        //
        a := AxisBottom.PosToValue(ZoomBox.Left);
        b := AxisBottom.PosToValue(ZoomBox.Right);
        AxisBottom.MinValue := a;
        AxisBottom.MaxValue := b;
        //
        a := AxisTop.PosToValue(ZoomBox.Left);
        b := AxisTop.PosToValue(ZoomBox.Right);
        AxisTop.MinValue := a;
        AxisTop.MaxValue := b;
        //
        a := AxisRight.PosToValue(ZoomBox.Bottom);
        b := AxisRight.PosToValue(ZoomBox.Top);
        AxisRight.MinValue := a;
        AxisRight.MaxValue := b;
        //
        FForceRedraw := True;
        RedrawObject;
      end
      else
      begin
        if (ZoomBox.Width < -2) or (ZoomBox.Height < -2) then
        begin
          RescaleByCurves;
          //
          FForceRedraw := True;
        end;
      end;
      RedrawObject;
      EatEvent := True;
    end;
  end;
end;

// #######################
// Find Nearest
function TPlotPanel.FindNearestCurve(X, Y: Integer; var MinLoc: Classes.TPoint; var CurveIdx, PointIdx: Integer): Boolean;
var
  i, j: Integer;
  MinDist, Dist: Double;
  Loc: Classes.TPoint;
begin
  Result := False;
  MinDist := 1e100;
  CurveIdx := -1;
  for i := 0 to High(Curves) do
  begin
    for j:=0 to High(Curves[i].X) do
    begin
      if (Curves[i].x[j] >= Curves[i].xaxis.MinValue) and (Curves[i].x[j] <= Curves[i].xaxis.MaxValue) and
        (Curves[i].y[j] >= Curves[i].yaxis.MinValue) and (Curves[i].y[j] <= Curves[i].yaxis.MaxValue) then
      begin
        loc := Curves[i].XAxis.Place(Curves[i].x[j]);
        loc := Curves[i].YAxis.Shift(loc,Curves[i].y[j]);
        Dist := Sqr(loc.X - x) + sqr(loc.y - y);
        if Dist < MinDist then
        begin
          MinDist := Dist;
          CurveIdx := i;
          PointIdx := j;
          MinLoc := Loc;
        end;
      end;
    end;
  end;
  Result := CurveIdx >= 0;
end;

//#############################
// Mouse Move
procedure TPlotPanel.MouseMoveEvent(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
var
  Dist: Double;
  FoundCurve, FoundPos: Integer;
  MinLoc: Classes.TPoint;
begin
  if MouseDown then
  begin
    ZoomBox.Right := X;
    ZoomBox.Bottom := Y;
    RedrawObject;
    EatEvent := True;
  end
  else
  begin
    if (FMouseModus = mmData) or (FMouseModus = mmMarker) then
    begin
      if FindNearestCurve(x, y, MinLoc, FoundCurve, FoundPos) then
      begin
        CrossText.Clear;
        CrossText.Add(Curves[FoundCurve].Name);
        CrossText.Add('X: ' + ValueToStr(Curves[FoundCurve].x[FoundPos], Curves[FoundCurve].XAxis.AfterDot + 1) + ' ' + Curves[FoundCurve].XAxis.AxUnit);
        CrossText.Add('Y: ' + ValueToStr(Curves[FoundCurve].y[FoundPos], Curves[FoundCurve].YAxis.AfterDot + 1) + ' ' + Curves[FoundCurve].YAxis.AxUnit);
        CrossPos := MinLoc;
      end
      else
      begin
        CrossPos := Point(-1, -1);
      end;
      RedrawObject;
    end;
    if FMouseModus = mmPosition then
    begin
      CrossText.Clear;
      if aoShowLabels in AxisBottom.Options then
      begin
        Dist := AxisBottom.PosToValue(x);
        CrossText.Add(HintText_XAxis2 + ': ' + ValueToStr(Dist, AxisBottom.AfterDot + 1) + ' ' + AxisBottom.AxUnit);
      end;
      if aoShowLabels in AxisTop.Options then
      begin
        Dist := AxisTop.PosToValue(x);
        CrossText.Add(HintText_XAxis1 + ': ' + ValueToStr(Dist, AxisTop.AfterDot + 1) + ' ' + AxisTop.AxUnit);
      end;
      if aoShowLabels in AxisLeft.Options then
      begin
        Dist := AxisLeft.PosToValue(x);
        CrossText.Add(HintText_YAxis1 + ': ' + ValueToStr(Dist, AxisLeft.AfterDot + 1) + ' ' + AxisLeft.AxUnit);
      end;
      if aoShowLabels in AxisRight.Options then
      begin
        Dist := AxisRight.PosToValue(x);
        CrossText.Add(HintText_YAxis2 + ': ' + ValueToStr(Dist, AxisRight.AfterDot + 1) + ' ' + AxisRight.AxUnit);
      end;
      CrossPos := Point(x, y);
      RedrawObject;
    end;
    EatEvent := True;
  end;
end;

//#########################
// Mouse Leave
procedure TPlotPanel.MouseLeaveEvent(Sender: TObject);
var
  i: Integer;
begin
  if FMouseModus = mmMarker then
  begin
    CrossText.Clear;
    CrossText.Add('Marker 1');
    CrossText.Add('X: ' + ValueToStr(FXMarker, Curves[0].XAxis.AfterDot + 1) + ' ' + Curves[0].XAxis.AxUnit);
    for i := 0 to High(Curves) do
    begin
      CrossText.Add('Y: ' + ValueToStr(Curves[i].y[FXMarkerValueIdx], Curves[i].YAxis.AfterDot + 1) + ' ' + Curves[i].YAxis.AxUnit);
    end;
    CrossPos := Point(FMarkerLoc.X, 0);
  end
  else
    CrossPos := Point(-1, -1);
  RedrawObject;
end;

procedure TPlotPanel.Clear;
begin
  SetLength(Curves, 0);
  FForceRedraw := True;
  FShowMarker := True;
end;

procedure TPlotPanel.PlotData;
begin
  RescaleByCurves;
  FForceRedraw := True;
  RedrawObject;
end;


end.
