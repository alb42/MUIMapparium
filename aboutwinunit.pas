unit aboutwinunit;
{$mode objfpc}{$H+}
interface
uses
  {$if defined(Amiga68k) or defined(MorphOS)}
    amigalib,
  {$endif}
  Types, Classes, SysUtils, fgl, Math,
  Exec, Utility, intuition, AGraphics, Layers, MUI, MUIHelper,
  MUIWrap, MUIPaintBoxUnit, versionunit;

const
  DefText =
    'MUIMapparium'#10 +
    'by ALB42'#10 +
    '============'#10 +

    'OpenStreetMap viewer'#10 +
    'for all'#10 +
    'Amiga Systems';

  StarterText =
    'Click here and'#10 +
    'press Enter'#10#10 +
    'use Cursor + Space';

  MINSHIPDISTANCE = 20;
  PlayWidth = 200;

type
  TPlayModus = (pmStopped, pmRunning, pmEnd);
  TEnemyModus = (emIdle, emHit);

  TSomething = class
    Pos: TPoint;
  end;
  TEnemy = class(TSomething)
    Modus: TEnemyModus;
    Letter: Char;
    Height: Integer;
    Width: Integer;
    Aim: TPoint;
    Pts: Integer;
    IsNumber: Boolean;
    Health: Integer;
  end;
  TBullet = class(TSomething)
  end;
  TShip = class(TSomething)
  end;

  TEnemyList = specialize TFPGObjectList<TEnemy>;
  TBulletList = specialize TFPGObjectList<TBullet>;

  TPlayPanel = class(TMUIPaintBox)
  private
    PlayModus: TPlayModus;
    EOffset: TPoint;
    MoveDir: Integer; // - 1 or + 1
    TextLine: Integer;
    EnemyList: TEnemyList;
    BulletList: TBulletList;
    Ship: TShip;
    AboutText: TStringList;
    LastCall: Int64;
    Points: Integer;
    MaxTW: Integer;
    TitleText: TStringList;
    procedure DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
    procedure KeyDownEvent(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    procedure NextLines(RP: PRastPort; DrawRect: TRect);
    procedure CalcMovement;
    procedure CheckHits;
    procedure FireBullet;
    procedure DrawEnemies(Rp: PRastPort; DrawRect: TRect);
    procedure DrawShip(Rp: PRastPort; DrawRect: TRect);
    procedure DrawBullets(Rp: PRastPort; DrawRect: TRect);
    procedure DrawTitle(Rp: PRastPort; DrawRect: TRect);
  public
    constructor Create(const Args: array of PtrUInt); override;
    destructor Destroy; override;
    procedure DoRedrawing;
  end;

procedure OpenAboutWindow;

var
  AboutWin: PObject_;
  PlayPanel: TPlayPanel;

implementation

// Constructor
constructor TPlayPanel.Create(const Args: array of PtrUInt);
begin
  inherited;
  PlayModus := pmStopped;
  //
  TextLine := 0;
  AboutText := TStringList.Create;
  AboutText.Text := DefText;
  //
  EnemyList := TEnemyList.Create;
  BulletList := TBulletList.Create;
  Ship := TShip.Create;
  Ship.Pos.X := 100;
  Ship.Pos.Y := 280;

  //
  MinWidth := PlayWidth;
  MaxWidth := PlayWidth;
  MinHeight := 300;
  MaxHeight := 300;

  OnDrawObject := @DrawEvent;
  OnMUIKeyDown := @KeyDownEvent;
  LastCall := 0;
  MoveDir := 1;
  Points := 0;
  TitleText := TStringList.Create;
  TitleText.Text := StarterText;
end;

// destructor
destructor TPlayPanel.Destroy;
begin
  AboutText.Free;
  EnemyList.Free;
  BulletList.Free;
  Ship.Free;
  TitleText.Free;
  inherited;
end;


procedure TPlayPanel.DoRedrawing;
begin
  if LongBool(MH_Get(MUIObject, MUIA_ShowMe)) and (PlayModus = pmRunning) then
    RedrawObject;
end;

// Key down Event
procedure TPlayPanel.KeyDownEvent(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
begin
  if PlayModus = pmRunning then
  begin
    case Code of
      CURSORLEFT: begin
        Dec(Ship.Pos.X, 5);
        EatEvent := True;
      end;
      CURSORRIGHT: begin
        Inc(Ship.Pos.X, 5);
        EatEvent := True;
      end;
    end;
    if Ship.Pos.X > (PlayWidth - MINSHIPDISTANCE) then
      Ship.Pos.X := (PlayWidth - MINSHIPDISTANCE);
    if Ship.Pos.X < MINSHIPDISTANCE then
      Ship.Pos.X := MINSHIPDISTANCE;
    if Key = ' ' then
    begin
      FireBullet();
    end;
  end
  else
  begin
    if Key = #13 then
    begin
      BulletList.Clear;
      EnemyList.Clear;
      Points := 0;
      TextLine := 0;
      PlayModus := pmRunning;
    end;
  end;
end;

// Fire a Bullet
procedure TPlayPanel.FireBullet;
var
  Bullet: TBullet;
begin
  Bullet := TBullet.Create;
  Bullet.Pos := Ship.Pos;
  BulletList.Add(Bullet);
end;

// Calc movements
procedure TPlayPanel.CalcMovement;
var
  CurCall: Int64;
  Delta: Int64;
  i: Integer;
  Bullet: TBullet;
  E: TEnemy;
  Dist, Dist1: TPoint;
  Aspect: Single;
begin
  CurCall := GetMUITime;
  if LastCall = 0 then
  begin
    LastCall := CurCall;
    Exit;
  end;
  Delta := (CurCall - LastCall) div 5; // 1 Pixel per 5 ms
  for i := EnemyList.Count - 1 downto 0 do
  begin
    E := EnemyList[i];
    if E.Modus <> emIdle then
    begin
      Dist.X := E.Aim.X - E.Pos.X;
      Dist.Y := E.Aim.Y - E.Pos.Y;
      Dist1.Y := 0;
      Dist1.X := 0;
      if (Dist.Y <> 0) and (Dist.X <> 0) then
      begin
        Aspect := Dist.X / Dist.Y;
        Dist1.Y := Round(Delta / (Aspect + 1));
        Dist1.X := Delta - Dist1.Y;
        if Abs(Dist1.x) > Abs(Dist.x) then
          Dist1.x := Dist.x;
        if Abs(Dist1.y) > Abs(Dist.y) then
          Dist1.y := Dist.y;
      end
      else
      begin
        if Dist.X = 0 then
          Dist1.Y := - Sign(Dist.Y) * Delta;
        if Dist.Y = 0 then
          Dist1.X := - Sign(Dist.X) * Delta;
      end;
      E.Pos.X := E.Pos.X - Dist1.X;
      E.Pos.Y := E.Pos.Y - Abs(Dist1.Y + 1);

      if (E.Modus = emHit) and (((Abs(E.Pos.X - E.Aim.X) < 5) and (Abs(E.Pos.Y - E.Aim.Y) < 5))  or (E.Pos.Y < 0)) then
      begin
        Inc(Points, E.Pts);
        EnemyList.Delete(i);
      end;
    end;
  end;
  //
  Delta := (CurCall - LastCall) div 20; // 1 Pixel per 20 ms
  if Delta > 0 then
  begin
    LastCall := CurCall;
    EOffset.X := EOffset.X + (MoveDir * Delta);
    if (EOffset.X < 0) or (EOffset.X + MaxTW > PlayWidth) then
    begin
      if MoveDir > 0 then
        EOffset.X := PlayWidth - MaxTW;
      if MoveDir < 0 then
        EOffset.X := 5;
      MoveDir := - MoveDir;
      EOffset.Y := EOffset.Y + 8;
    end;
    for i := BulletList.Count - 1 downto 0 do
    begin
      Bullet := BulletList[i];
      Bullet.Pos.Y := Bullet.Pos.Y - Delta;
      if Bullet.Pos.Y < -8 then
        BulletList.Delete(i);
    end;
  end;
  CheckHits;
end;

// Check if any key got hit ;)
procedure TPlayPanel.CheckHits;
var
  E: TEnemy;
  B: TBullet;
  Hit: Boolean;
  i, j: Integer;
  ER, BR, R: Types.TRect;
  str: string;
begin
  for i := EnemyList.Count - 1 downto 0 do
  begin
    Hit := False;
    E := EnemyList[i];
    if E.Modus = emHit then
      Continue;
    ER := Rect(E.Pos.X + EOffset.X, E.Pos.Y + EOffset.Y, E.Pos.X + E.Width + EOffset.X, E.Pos.Y + E.Height + EOffset.Y);
    for j := BulletList.Count - 1 downto 0  do
    begin
      B := BulletList[j];
      BR := Rect(B.Pos.X + 1, B.Pos.Y + 1, B.Pos.X + 2, B.Pos.Y + 3);
      if IntersectRect(r, ER, BR) then
      begin
        Hit := True;
        BulletList.Delete(j);
        Break;
      end;
    end;
    if Hit then
    begin
      Dec(E.Health);
      if E.Health <= 0 then
      begin
        E.Modus := emHit;
        E.Aim.X := 50;
        E.Aim.Y :=  0;
        E.Pos.X := E.Pos.X + EOffset.X;
        E.Pos.Y := E.Pos.Y + EOffset.Y;
        E.Pts := E.Pts * (300 - E.Pos.X);
      end
      else
      begin
        if E.IsNumber then
          str := IntToStr(E.Health) + '0'
        else
          str := LowerCase(E.Letter) + '_';
        E.Letter := str[1];
      end;
    end;
  end;
end;

// Next text lines
procedure TPlayPanel.NextLines(RP: PRastPort; DrawRect: TRect);
var
  L: string;
  Line: array[0..2] of string;
  TE: array[0..2] of TTextExtent;
  TC: TTextExtent;
  He, Middle: Integer;
  OX, OY: Integer;
  i, j: Integer;
  Enemy: TEnemy;
begin
  EOffset.X := 0;
  EOffset.Y := 50;
  EnemyList.Clear;
  BulletList.Clear;
  if TextLine >= AboutText.Count then
  begin
    PlayModus := pmStopped;
    TitleText.Clear;
    TitleText.Add('Thanks');
    TitleText.Add('You finished');
    TitleText.Add('the Credits game');
    TitleText.Add('');
    TitleText.Add('Points: ' + IntToStr(Points));
    Exit;
  end;
  Line[0] := '';
  Line[1] := '';
  Line[2] := '';
  i := 0;
  MaxTW := 0;
  while TextLine < AboutText.Count do
  begin
    Line[i] := Trim(AboutText[TextLine]);
    L := AboutText[TextLine] + #0;
    TextExtent(RP, PChar(L), Length(L), @(TE[i]));
    He := TE[i].te_Height;
    MaxTW := Max(MaxTW, TE[i].te_Width);
    Inc(TextLine);
    Inc(i);
    if i > 2 then
      Break;
  end;
  //
  Middle := MaxTW div 2;
  OY := 0;
  for i := 0 to 2 do
  begin
    OX := Middle - (TE[i].te_Width div 2);
    L := Line[i];
    if L <> '' then
    begin
      for j := 1 to Length(L) do
      begin
        TextExtent(RP, PChar(@L[j]), 1, @(TC));
        if not (L[j] in  [' ', #10, #13]) then
        begin
          Enemy := TEnemy.Create;
          EnemyList.Add(Enemy);
          Enemy.Letter := L[j];
          Enemy.Modus := emIdle;
          Enemy.Width := TC.te_Width;
          Enemy.Height := TC.te_Height;
          Enemy.Pos.X := OX;
          Enemy.Pos.Y := OY;
          Enemy.isNumber := Enemy.Letter in ['0'..'9'];
          if Enemy.IsNumber then
          begin
            Enemy.Pts := StrToIntDef(Enemy.Letter, 1);
            Enemy.Health := Enemy.Pts;
          end
          else
          begin
            Enemy.Pts := 1;
            Enemy.Health := 1;
          end;
          if Enemy.Letter in ['A'..'Z'] then
          begin
            Enemy.Health := 2;
            Enemy.Pts := 2;
          end;
        end;
        Inc(OX, TC.te_Width);
      end;
    end;
    OY := OY + 2 * He;
  end;
end;

// Draw Enemies
procedure TPlayPanel.DrawEnemies(Rp: PRastPort; DrawRect: TRect);
var
  E: TEnemy;
  P: TPoint;
  i: Integer;
begin
  if (EnemyList.Count = 0) or (EOffset.Y > 250) then
    NextLines(RP, DrawRect);
  CalcMovement;
  for i := 0 to EnemyList.Count - 1 do
  begin
    E := EnemyList[i];
    P := E.Pos;
    if E.Modus = emIdle then
    begin
      P.X := P.X + EOffset.X;
      P.Y := P.Y + EOffset.Y;
    end;
    // more position ;)
    GfxMove(RP, P.X, P.Y);
    GfxText(RP, @E.Letter, 1);
  end;
end;

// Draw my Ship
procedure TPlayPanel.DrawShip(Rp: PRastPort; DrawRect: TRect);
begin
  GfxMove(RP, Ship.Pos.X, Ship.Pos.Y);
  Draw(RP, Ship.Pos.X - 5, Ship.Pos.Y + 17);
  Draw(RP, Ship.Pos.X + 5, Ship.Pos.Y + 17);
  Draw(RP, Ship.Pos.X, Ship.Pos.Y);
end;

// Draw fired Bullets
procedure TPlayPanel.DrawBullets(Rp: PRastPort; DrawRect: TRect);
var
  Bullet: TBullet;
  i: Integer;
begin
  for i := 0 to BulletList.Count - 1 do
  begin
    Bullet := BulletList[i];
    DrawEllipse(RP, Bullet.Pos.X, Bullet.Pos.Y, 2, 6);
  end;
end;

procedure  TPlayPanel.DrawTitle(Rp: PRastPort; DrawRect: TRect);
var
  s: string;
  TE: TTextExtent;
  x, y, i: Integer;
begin
  SetAPen(Rp, 0);
  // fill with background color
  RectFill(Rp, 0, 0, DrawRect.Right, DrawRect.Bottom);
  //
  SetAPen(Rp, 1);
  SetDrMd(Rp, JAM1);
  //
  y := 100;
  //
  for i := 0 to TitleText.Count - 1 do
  begin
    S := TitleText[i];
    TextExtent(RP, PChar(s), Length(s), @TE);
    X := (DrawRect.Width div 2) - (TE.te_Width div 2);
    GfxMove(RP, X, Y);
    GFXText(RP, PChar(S), Length(S));
    Inc(Y, TE.te_Height + 2);
  end;
end;

// Draw event
procedure TPlayPanel.DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
var
  LocalRP: PRastPort;
  li: pLayer_Info;
  s: string;
begin
    // Make a temporary Rastport to draw to
  LocalRP := CreateRastPort;
  li := NewLayerInfo(); // Layerinfo we also need
  // Bitmap and layer for the temp rastport
  LocalRP^.Bitmap := AllocBitMap(DrawRect.Width, DrawRect.Height, rp^.Bitmap^.Depth, BMF_MINPLANES or BMF_DISPLAYABLE, rp^.Bitmap);
  LocalRP^.Layer := CreateUpFrontLayer(li, LocalRP^.Bitmap, 0, 0, DrawRect.Width - 1, DrawRect.Height - 1, LAYERSIMPLE, nil);
  SetFont(LocalRP, rp^.Font);
  // initialize to background color
  SetAPen(LocalRP, 1);
  SetBPen(LocalRP, 0);

  case PlayModus of
    pmStopped: begin
      DrawTitle(LocalRP, DrawRect);
    end;
    pmRunning: begin
      // fill with background color
      RectFill(LocalRP, 0, 0, DrawRect.Right, DrawRect.Bottom);
      // the actual drawing
      SetAPen(LocalRP, 2);
      SetDrMd(LocalRP, JAM1);
      //
      DrawEnemies(LocalRP, DrawRect);
      //
      DrawShip(LocalRP, DrawRect);
      //
      DrawBullets(LocalRP, DrawRect);
      // Draw points
      GFXMove(LocalRP, 2, 10);
      S := Format('%.10d', [Points]);
      GFXText(LocalRP, PChar(S), Length(S));
    end;
    pmEnd: begin
      // fill with background color
      RectFill(LocalRP, 0, 0, DrawRect.Right, DrawRect.Bottom);
    end;
  end;
  //
  ClipBlit(LocalRP, 0,0, rp, DrawRect.Left, DrawRect.Top, DrawRect.Width, DrawRect.Height, $00C0);
  // delete the layer
  DeleteLayer(0, LocalRP^.layer);
  DisposeLayerInfo(li);
  // delete the bitmap
  FreeBitmap(LocalRP^.Bitmap);
  LocalRP^.Layer := nil;
  LocalRP^.Bitmap := nil;
  // Destroy the temp rastport
  FreeRastPort(LocalRP);
end;

procedure OpenAboutWindow;
begin
  PlayPanel.PlayModus := pmStopped;
  PlayPanel.TitleText.Text := StarterText;
  PlayPanel.Points := 0;
  MH_Set(AboutWin, MUIA_Window_Open, AsTag(True));
end;


// Create About window
procedure CreateAboutWin;
begin
  PlayPanel := TPlayPanel.Create([
    //MUIA_Font, AsTag(MUIV_Font_Fixed),
    TAG_DONE]);
  //
  AboutWin := MH_Window([
    MUIA_Window_Title,     AsTag('About MUIMapparium'),
    MUIA_Window_ID,        AsTag(MAKE_ID('A','B','O','U')),
    MUIA_HelpNode,         AsTag('AboutWin'),
    WindowContents, AsTag(MH_HGroup([
      Child, AsTag(MH_VGroup([
        Child, AsTag(MH_VGroup([
          MUIA_Frame, MUIV_Frame_Group,
          Child, AsTag(MH_Text(MUIX_B + MUIX_C + 'Mapparium' + MUIX_N, [MUIA_Font, AsTag(MUIV_Font_Big), TAG_END])),
          Child, AsTag(MH_Text(MUIX_C+'MUI Version' + MUIX_N, [MUIA_Font, AsTag(MUIV_Font_Tiny), TAG_END])),
          Child, AsTag(MH_Text(PChar('  Version: ' + IntToStr(VersionMajor) + '.' + IntToStr(VersionMinor)))),
          Child, AsTag(MH_Text(PChar('  Date: ' + {$INCLUDE %DATE%}))),
          Child, AsTag(MH_Text(PChar('  Target: ' + {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%} + '       '))),
          TAG_DONE])),
        TAG_DONE])),
      Child, AsTag(MH_VGroup([
        Child, AsTag(PlayPanel.MUIObject),
        TAG_DONE])),
      TAG_DONE])),
    TAG_DONE]);

  // Close Window
    DoMethod(AboutWin, [MUIM_Notify, MUIA_Window_CloseRequest, MUI_TRUE,
      AsTag(AboutWin), 3, MUIM_SET, MUIA_Window_Open, AsTag(False)]);
end;

initialization
  CreateAboutWin;
finalization
  PlayPanel.Free;
end.
