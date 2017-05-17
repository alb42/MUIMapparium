unit MUIPlotBoxUnit;
{$mode objfpc}{$H+}
interface

uses
  Math, Classes, SysUtils, MUIPaintBoxUnit, mui, agraphics, intuition,
  utility, prefsunit, types, layers,
  cybergraphics, muiwrap;

const
  ln10=2.30258509299404568401799145468436;
  PreFixChar:array[-8..8] of char=
    ('y','z','a','f','p','n','u','m',
     ' ',
     'k','M','G','T','P','E','Z','Y');
  PreFixExp:array[-8..8] of string=
    ('10{^-24}','10{^-21}','10{^-18}','10{^-15}','10{^-12}','10{^-9}','10{^-6}','10{^-3}',
     '',
     '10{^3}','10{^6}','10{^9}','10{^12}','10{^15}','10{^18}','10{^21}','10{^24}');

type
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

  TAxis = class
  private
    FMinValue: Double;
    FMaxValue: Double;
    FOptions: TAxisOptions;
    FPosition: TAxisPosition;
    FAxisScale: TAxisScale;
    Start, Stop: TPoint;
    Increment: Double;
    FTextPosition:TAxisPosition;
    DivideFac: Double;
    FTitle: string;
    Prefix: string;
    FAxUnit: string;
    VertTextShift:integer;
    MinorNo: Integer;
  public
    procedure DoDraw(Rp: PRastPort; DrawRect: TRect);
    function Place(Value: Double): TPoint;
    function Shift(Position: TPoint; Value: Double): TPoint;
    function GetWidth(RP: PRastPort; GetForced: boolean = True): integer;
    procedure SetIncrement(MinDist: Integer);
    property Options: TAxisOptions read FOptions write FOptions;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property Position: TAxisPosition read FPosition;
    property TextPosition: TAxisPosition read FTextPosition write FTextPosition;
    property Title: string read FTitle write FTitle;
    property AxUnit: string read FAxUnit write FAxUnit;
  end;

  TPlotPanel = class(TMUIPaintBox)
  private
    //DownPos: TPoint;
    //FLastMouse: TPoint;
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
    // middle Marker
  protected
    procedure DrawCurves(RP: PRastPort; ClipRect: TRect);
  public
    constructor Create(const Args: array of PtrUInt); override;
    destructor Destroy; override;
    procedure DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
    procedure DoPlotDraw(RP: PRastPort; DrawRect: TRect; ScaleOnly: Boolean = False);
    //
    procedure AddCurve(var x,y:array of double; var valid:array of boolean; XAxis,YAxis:TAxisPosition; Color:TColor=$000000; Name:string=''; Index: Integer = -1);
    procedure Clear;
    //
    {procedure MouseDownEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseUpEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseDblClick(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseMoveEvent(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseWheelEvent(Sender: TObject; ScrollUp: Boolean; var EatEvent: Boolean);
    //
    procedure KeyDownEvent(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    //
    procedure ZoomIn(ToPos: Boolean);
    procedure ZoomOut;
    procedure RefreshImage;
    function PixelToPos(T: TPoint): TCoord;
    function PosToPixel(C: TCoord): TPoint;

    property OnUpdateLocationLabel: TProcedure read FOnUpdateLocationLabel write FOnUpdateLocationLabel;
    property LastMouse: TPoint read FLastMouse;}
    property Title: string read FTitle write FTitle;
    property AxisTop: TAxis read FAxisTop write FAxisTop;
    property AxisLeft: TAxis read FAxisLeft write FAxisLeft;
    property AxisBottom: TAxis read FAxisBottom write FAxisBottom;
    property AxisRight: TAxis read FAxisRight write FAxisRight;
  end;

function TH(RP: PRastPort; Text: string): Integer;
function TW(RP: PRastPort; Text: string): Integer;
function RoundToStr(Value: Double; Precision: Integer): string;

implementation


function TH(RP: PRastPort; Text: string): Integer;
var
  TE: TTextExtent;
begin
  TextExtent(RP, PChar(text), Length(Text), @TE);
  Result := TE.te_Height;
end;

function TW(RP: PRastPort; Text: string): Integer;
begin
  Result := TextLength(RP, PChar(text), Length(Text));
end;

function RoundToStr(Value: Double; Precision: Integer): string;
var
  j: Integer;
  sgn: string;
  prcval: Double;
  ds: string;
  di:integer;
  om: Double;
  didit: Boolean;
begin
  if Precision < 0 then
    Precision := 0;
  DidIt := False;
  sgn := '';
  if Value < 0 then
    sgn := '-';
  PrcVal := Abs(Value);
  //
  Om := 1;
  while Om <= PrcVal do
    Om := Om * 10;
  ds := '';
  j := 0;
  repeat
    if (om < 1) and (om > 0.05) and (precision > 0) then
      ds := ds + FormatSettings.DecimalSeparator;
    if om < 1 then
      j := j + 1;
    di := Trunc(PrcVal / Om);
    if (di > 0) or (om < 10) or didit then
    begin
      ds := ds + IntToStr(di);
      Didit := True;
    end;
    prcval:=prcval-om*di;
    om := om/10;
  until (om < 1) and (j >= precision + 1);
  if StrToInt(ds[length(ds)]) >= 5 then
  begin
    j := Length(ds) - 1;
    if ds[j] = FormatSettings.DecimalSeparator then
      dec(j);
    while (j > 0) and (ds[j] = '9') do
    begin
      ds[j] := '0';
      dec(j);
      if (j > 0) and (ds[j]=FormatSettings.DecimalSeparator) then
        Dec(j);
    end;
    if j > 0 then
      ds[j] := char(Ord(ds[j]) + 1)
    else
      ds := '1' + ds;
  end;
  SetLength(ds, Length(ds) - 1);
  Result := sgn + ds;
end;

procedure ParamNameOut(s: string; RP: PRastPort;
  Rect: TRect;Rot90:boolean=false);
//var ds,ds2:string;
//    xout,sase,tp:integer;
//    sf:TFont;
begin
  GfxMove(Rp, rect.Left,rect.Top);
  GfxText(Rp, PChar(s), Length(s));
(*
  sf:=TFont.Create;
  sf.Assign(canvas.Font);
  ds:=s;
      xout:=0;
      while pos('{',ds)>0 do
        begin
          ds2:=ds;
          delete(ds2,pos('{',ds2),length(ds2));
          if Rot90
          then
            RotOut(Canvas,rect.Left,rect.Bottom-xout,90,ds2)
          else
            Canvas.TextOut(rect.Left+xout,rect.Top,ds2);
          xout:=xout+
            Canvas.TextWidth(ds2);
          delete(ds,1,pos('{',ds));
          ds2:='';
          while (length(ds)>0) and (ds[1]<>'}') do
            begin
              ds2:=ds2+ds[1];
              delete(ds,1,1);
            end;
          if length(ds)>0
          then
            delete(ds,1,1);
          sase:=Canvas.Font.Size;
          TP:=0;
          if length(ds2)>0
          then
            case ds2[1] of
              'G':
                begin
                  Canvas.Font.Name:='Symbol';
                  delete(ds2,1,1);
                end;
              '_':
                begin
                  Canvas.Font.Name:=sf.name;
                  TP:=Canvas.TextHeight('|');
                  Canvas.Font.Size:=
                    round(sase*0.6);
                  TP:=TP-Canvas.TextHeight('|');
                  delete(ds2,1,1);
                end;
              '^':
                begin
                  Canvas.Font.Name:=sf.name;
                  Canvas.Font.Size:=
                    round(sase*0.6);
                  TP:=0;
                  delete(ds2,1,1);
                end;
            end;
          if Rot90
          then
            RotOut(Canvas,rect.Left+TP,rect.Bottom-xout,90,ds2)
          else
            Canvas.TextOut(rect.Left+xout,
              rect.Top+TP,ds2);
          xout:=xout+
            Canvas.TextWidth(ds2);
          Canvas.Font.Name:=sf.name;
          Canvas.Font.Size:=sase;
        end;
      if Rot90
      then
        RotOut(Canvas,rect.left,rect.Bottom-xout,90,ds)
      else
        Canvas.TextOut(rect.Left+xout,rect.Top,ds);
  sf.Free;*)
end;

procedure ClipLine(RP: PRastPort;const x0,y0,x1,y1:integer;
  const Cliprect:TRect;var DoOld:boolean);
  function InRect(const x,y:integer;const Rect:TRect):boolean;
  begin
    result:=(x>=rect.Left) and
     (x<=rect.Right) and
     (y>=rect.Top) and
     (y<=rect.Bottom);

  end;
var
  dx,dy,xa0,ya0,xa1,ya1,a,b: Double;
begin
  if InRect(X0,Y0,ClipRect) and InRect(X1,Y1,ClipRect) then
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
  w10, afterdot: Integer;
  de: Double;
begin
  GfxMove(Rp, DrawRect.Left + Start.X, DrawRect.Top + Start.Y);
  Draw(RP, DrawRect.Left + Stop.X, DrawRect.Top + Stop.Y);
  //writeln('DRaw Axis ', Start.X, ' ', Start.Y, ' to ', Stop.X,' ', Stop.Y);
  //writeln('DrawRect  ', DrawRect.Left, ' ', DrawRect.Top, ' to ', DrawRect.Right,' ', DrawRect.Bottom);

  disp := 0;

  if aoShowTics in Options then
  begin
    if (FAxisScale = atLin) and (Increment <> 0) then
    begin
      for i := Round(FMinValue / Increment) - 1 to Round(FMinValue / Increment) + 1 do
      begin
        if (i * Increment >= FMinValue) and (i * Increment <= FMinValue) then
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
              Draw(Rp, DrawRect.Left + Pt.x-5, DrawRect.Top + Pt.y);
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
            pt:=Place(exp(i*ln10));
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
  if aoShowLabels in Options then
  begin
    hg := Round(TH(RP, '|') * 1.2);
    disp := hg;
    hg2 := Round(TH(RP, '|') * 0.2);
    afterdot := 0;
    if (FAxisScale = atLin) and (Increment <> 0) then
    begin
//    if not (aoForceNoExp in Options)
//    then
      begin
        de := Increment / DivideFac;
        ds := RoundToStr(de, 5);
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
          ds := RoundToStr(i * increment / DivideFac, Afterdot);
          wd := TW(Rp, ds);
          case TextPosition of
            apBottom:
              GfxMove(Rp, pt.X-wd div 2, pt.Y+hg2 * 4);
            apLeft:
              GfxMove(Rp, Pt.X-wd-hg2, pt.Y + hg div 2);
            apTop:
              GfxMove(Rp, pt.X-wd div 2, pt.Y-hg2);
            apRight:
              GfxMove(Rp, pt.X+hg2, pt.Y + hg div 2);
          end;
          GfxText(Rp, PChar(ds), Length(ds))
        end;
    end
    else
      if (FMinValue > 0) and (FMaxValue > 0) then
      begin
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
        ParamNameOut(ds,Rp,rect(
          DrawRect.Left + (Start.X+Stop.X-wd) div 2, DrawRect.Top + Start.Y+hg2+disp,
          DrawRect.Left + (Start.X+Stop.X+wd) div 2, DrawRect.Top + Start.Y+hg+hg2+disp));
  //         Canvas.TextOut((Start.X+Stop.X-wd) div 2,Start.Y+hg2+disp,ds);
      apLeft:
        ParamNameOut(ds,Rp,rect(
          DrawRect.Left + Start.X-VertTextShift-hg, DrawRect.Top + (Start.Y+Stop.Y-wd) div 2,
          DrawRect.Left + Start.X-VertTextShift, DrawRect.Top + (Start.Y+Stop.Y+wd) div 2),true);
  //          RotOut(Canvas,Start.X-VertTextShift-hg,(Start.Y+Stop.Y+wd) div 2,90,ds);
      apTop:
        ParamNameOut(ds,Rp,rect(
          DrawRect.Left + (Start.X+Stop.X-wd) div 2, DrawRect.Top + Start.Y-hg-disp,
          DrawRect.Left + (Start.X+Stop.X+wd) div 2, DrawRect.Top + Start.Y-2*hg-disp));
  //          Canvas.TextOut((Start.X+Stop.X-wd) div 2,Start.Y-hg-disp,ds);
      apRight:
        ParamNameOut(ds,Rp,rect(
          DrawRect.Left + Start.X+VertTextShift+round(hg*0.3), DrawRect.Top + (Start.Y+Stop.Y-wd) div 2,
          DrawRect.Left + Start.X+VertTextShift+round(hg*1.3), DrawRect.Top + (Start.Y+Stop.Y+wd) div 2),true);
  //          RotOut(Canvas,Start.X+VertTextShift,(Start.Y+Stop.Y+wd) div 2,90,ds);
    end;
  end;
end;

function TAxis.Place(value: Double): TPoint;
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

function TAxis.Shift(Position: TPoint; Value: Double): TPoint;
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


function TAxis.GetWidth(RP: PRastPort; GetForced: boolean = True): integer;
var
  wd,wdh,wmax,i,hg: Integer;
  ds:string;
  de: Double;
  fad, afterdot: Integer;
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
            ds := RoundToStr(de, 5);
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
                  ds := RoundToStr(i * Increment / DivideFac, afterdot);
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
  pt1,pt2: TPoint;
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



// #####################################################################
//   TPlotPanel.Create
constructor TPlotPanel.Create(const Args: array of PtrUInt);
var
  x,y: array of Double;
  Valid: array of Boolean;
  i: Integer;
begin
  inherited;

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
  FAxisTop.Options := []; //[aoShowTics,aoShowLabels,aoShowTitle];

  FAxisLeft := TAxis.Create;
  FAxisLeft.FPosition := apLeft;
  FAxisLeft.TextPosition := apLeft;
  FAxisLeft.MinValue := 0;
  FAxisLeft.MaxValue := 100;
  FAxisLeft.Options := [aoShowTics,aoShowLabels,aoShowTitle];

  FAxisBottom := TAxis.Create;
  FAxisBottom.FPosition := apBottom;
  FAxisBottom.TextPosition := apBottom;
  FAxisBottom.MinValue := 0;
  FAxisBottom.MaxValue := 100;
  FAxisBottom.Options := [aoShowTics,aoShowLabels,aoShowTitle];

  FAxisRight := TAxis.Create;
  FAxisRight.FPosition := apRight;
  FAxisRight.TextPosition := apRight;
  FAxisRight.MinValue := 0;
  FAxisRight.MaxValue := 100;
  FAxisRight.Options := [];//[aoShowTics,aoShowLabels,aoShowTitle];

  SetLength(x, 100);
  SetLength(y, 100);
  SetLength(Valid, 100);
  for i := 0 to High(x) do
  begin
    x[i] := i;
    y[i] := Random(100);
    Valid[i] := True;
  end;
  AddCurve(x,y,Valid, apBottom, apLeft, $0000FF, 'test1');
  //
  FForceRedraw := True;
  FRastPort := nil;
  FLayerInfo := nil;
  RPDrawRect := Rect(0,0, 0, 0);
end;

destructor TPlotPanel.Destroy;
begin
  FAxisTop.Free;
  FAxisLeft.Free;
  FAxisBottom.Free;
  FAxisRight.Free;
  if Assigned(FRastPort) then
  begin
    // delete the layer
    DeleteLayer(0, FRastPort^.layer);
    // delete the bitmap
    FreeBitmap(FRastPort^.Bitmap);
    FRastPort^.Layer := nil;
    FRastPort^.Bitmap := nil;
    // Destroy the temp rastport
    FreeRastPort(FRastPort);
  end;
  DisposeLayerInfo(FLayerInfo);
  inherited;
end;

procedure TPlotPanel.DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
{var
  Pen: LongWord;
  TE: TTextExtent;
  TitlePos: TPoint;}
begin
  DoPlotDraw(RP, DrawRect);
  {Pen := SetColor(Rp, $FFFFFF);
  RectFill(Rp, DrawRect.Left, DrawRect.Top, DrawRect.Left + DrawRect.Width, DrawRect.Top + DrawRect.Height);
  UnSetColor(Pen);
  Pen := SetColor(Rp, $000000);
  SetDrMd(Rp, JAM1);
  TextExtent(RP, PChar(FTitle), Length(FTitle), @TE);
  TitlePos.X := (DrawRect.Left + (DrawRect.Width div 2)) - TE.te_Width div 2;
  TitlePos.Y := DrawRect.Top + TE.te_Height;
  GfxMove(Rp, TitlePos.X, TitlePos.Y);
  GfxText(Rp, PChar(FTitle), Length(FTitle));
  UnSetColor(Pen);}
end;



procedure TPlotPanel.DoPlotDraw(RP: PRastPort; DrawRect: TRect; ScaleOnly: Boolean = False);
var
  lf,tp,rg,bt,i,fsze: Integer;
  //iw,ih: Integer;
  Pen: LongWord;
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
    //AxisLeft.DrawGrid(RP,rect(lf,tp,rg,bt));
    //AxisRight.DrawGrid(RP,rect(lf,tp,rg,bt));
    //AxisTop.DrawGrid(RP,rect(lf,tp,rg,bt));
    //AxisBottom.DrawGrid(RP,rect(lf,tp,rg,bt));

    DrawCurves(FRastPort, Rect(lf,tp,rg,bt));
    //DrawMarkers(Canvas,rect(lf,tp,rg,bt));
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
end;


procedure TPlotPanel.DrawCurves(RP: PRastPort; ClipRect: TRect);
var
  i,j:integer;
  loc:TPoint;
  ox,oy{,bx,by}:integer;
  //wd,wi:integer;
  //dx,dy:integer;
  ov,DoMove:boolean;
  Pen: LongWord;
begin
  //wd:=TH(RP,'|');
  for i:=0 to length(Curves)-1 do
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
          ClipLine(Rp, ox, oy, ox, oy, ClipRect, DoMove);
        if Curves[i].valid[j] and ov then
          ClipLine(Rp, ox, oy, loc.x, loc.y, ClipRect, DoMove);
        if not ov then
          DoMove := True;
        ox := loc.x;
        oy := loc.y;
        ov := Curves[i].valid[j];
      end;
    end;
    //
    (*
    for j:=0 to length(Curves[i].x) - 1 do
    begin
      if curves[i].valid[j] then
      begin
        loc:=Curves[i].xaxis.Place(Curves[i].x[j]);
        Loc:=Curves[i].yaxis.Shift(loc,Curves[i].y[j]);
        if (loc.x<cliprect.Left) or (loc.x>cliprect.Right) or
        (loc.y<cliprect.top) or (loc.y>cliprect.Bottom)
        then
          continue;
        wi:=round(wd*Curves[i].ScatterSize/10);
        case Curves[i].ScatterStyle of
          scsCross:
           begin
             Canvas.MoveTo(Loc.X-wi,Loc.y-wi);
             Canvas.LineTo(Loc.X+wi+1,Loc.y+wi+1);
             Canvas.MoveTo(Loc.X+wi,Loc.y-wi);
             Canvas.LineTo(Loc.X-wi-1,Loc.y+wi+1);
           end;
          scsPlus:
           begin
             Canvas.MoveTo(Loc.X,Loc.y-wi);
             Canvas.LineTo(Loc.X,Loc.y+wi+1);
             Canvas.MoveTo(Loc.X+wi,Loc.y);
             Canvas.LineTo(Loc.X-wi-1,Loc.y);
           end;
          scsCircle:
           begin
             Canvas.Brush.Color:=Styles.BackgroundColor;
             Canvas.Ellipse(Loc.X-wi,Loc.y-wi,
                            Loc.X+wi,Loc.y+wi);
           end;
          scsCircleSolid:
           begin
             Canvas.Brush.Color:=Curves[i].Color;
             Canvas.Ellipse(Loc.X-wi,Loc.y-wi,
                            Loc.X+wi,Loc.y+wi);
           end;
          scsSquare:
           begin
             Canvas.Brush.Color:=Styles.BackgroundColor;
             Canvas.Rectangle(Loc.X-wi,Loc.y-wi,
                              Loc.X+wi,Loc.y+wi);
           end;
          scsSquareSolid:
           begin
             Canvas.Brush.Color:=Curves[i].Color;
             Canvas.Rectangle(Loc.X-wi,Loc.y-wi,
                              Loc.X+wi,Loc.y+wi);
           end;
        end;
      end;

    end;*)
    UnSetColor(Pen)
  end;
end;

procedure TPlotPanel.AddCurve(var x,y:array of double; var valid:array of boolean;
      XAxis,YAxis:TAxisPosition; Color:TColor=$000000; Name:string=''; Index: Integer = -1);
var
  i,vlgt,si:integer;
begin
  if Index=-1
  then
    begin
      si:=length(Curves);
      Setlength(Curves,si+1);
    end
  else
    begin
      if index>=length(Curves)
      then
        Setlength(Curves,index+1);
      si:=index;
    end;
  setlength(Curves[si].x,length(x));
  setlength(Curves[si].y,length(y));
  setlength(Curves[si].valid,length(y));
  vlgt:=length(valid);
  for i:=0 to length(x)-1 do
    Curves[si].x[i]:=x[i];
  for i:=0 to length(y)-1 do
    begin
      Curves[si].y[i]:=y[i];
      if i<vlgt
      then
        Curves[si].valid[i]:=valid[i]
      else
        Curves[si].valid[i]:=true;
    end;
  case XAxis of
    apTop:Curves[si].xaxis:=AxisTop;
    apBottom:Curves[si].xaxis:=AxisBottom;
    apLeft:Curves[si].xaxis:=AxisLeft;
    apRight:Curves[si].xaxis:=AxisRight;
  end;
  case YAxis of
    apTop:Curves[si].yaxis:=AxisTop;
    apBottom:Curves[si].yaxis:=AxisBottom;
    apLeft:Curves[si].yaxis:=AxisLeft;
    apRight:Curves[si].yaxis:=AxisRight;
  end;
  Curves[si].xAxPos:=XAxis;
  Curves[si].yAxPos:=YAxis;
  //Curves[si].Width:=Width;
  Curves[si].Name:=Name;
  //Curves[si].ScatterStyle:=ScatterStyle;
  //Curves[si].ScatterSize:=ScatterWidth;
  Curves[si].Color:=color;
  //Curves[si].LinesOn:=LinesOn;
end;

procedure TPlotPanel.Clear;
begin
  SetLength(Curves, 0);
  FForceRedraw := True;
end;

end.
