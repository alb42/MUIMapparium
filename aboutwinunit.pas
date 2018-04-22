unit aboutwinunit;
{$mode objfpc}{$H+}
interface
uses
  Types, Classes, SysUtils, fgl, Math,
  Exec, Utility, intuition, AGraphics, Layers, MUI, MUIHelper,
  MUIWrap, MUIPaintBoxUnit, versionunit, MUIMappariumlocale;

const
  DefText =
    'MUIMapparium'#10 +
    'by ALB42'#10 +
    '============'#10 +

    'OpenStreetMap viewer'#10 +
    'for all'#10 +
    'Amiga Systems'#10 +

    'written with the'#10 +
    'magnificant'#10 +
    'FreePascal'#10 +

    'thanks to Chain|Q'#10 +
    'migario and all'#10 +
    'users and testers.'#10 +

    '0123456789876543210'#10 +
    'End of transmission.'#10 +
    '0123456789876543210'#10;

  StarterText =
    'Click here and'#10 +
    'press Enter'#10#10 +
    'use Cursor + Space';

  MINSHIPDISTANCE = 20;
  PlayWidth = 200;
  PlayHeight = 300;

type
  TPlayModus = (pmStopped, pmRunning);
  TEnemyModus = (emIdle, emHit);

  TSomething = class
    Pos: Classes.TPoint;
  end;
  TEnemy = class(TSomething)
    Modus: TEnemyModus;
    Letter: Char;
    Height: Integer;
    Width: Integer;
    Aim: Classes.TPoint;
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
    PlayModus: TPlayModus; // game runs or not
    EOffset: Classes.TPoint; // Offset of the Enemies (all move together)
    MoveDir: Integer; // move directioy of the enemies - 1 or + 1 (for EOffset)
    TextLine: Integer; // Next line to load in About Text to create Enemies
    AboutText: TStringList; // The AboutText loaded from DefText
    MaxTW: Integer; // Maximal Line Width in Pixels (to calculate the Start position)
    //
    EnemyList: TEnemyList;   // List of enemies create from the About Text
    BulletList: TBulletList; // List of Bullets shoot by User
    Ship: TShip; // Ship properties
    //
    LastCall: Int64; // TimeTag for the last Movement update to calculate the current movement
    Points: Integer; // Number of Points earned by User

    TitleText: TStringList; // Text when the Game is not running
    //

    procedure KeyDownEvent(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    procedure NextLines(RP: PRastPort; DrawRect: TRect);
    procedure CalcMovement;
    procedure CheckHits;
    procedure FireBullet;
    // Draw Events
    procedure DrawEvent(Sender: TObject; Rp: PRastPort; DrawRect: TRect); // Main Drawing event
    procedure DrawEnemies(Rp: PRastPort; DrawRect: TRect); // Calc movements and draw Enemies to the RastPort
    procedure DrawShip(Rp: PRastPort; DrawRect: TRect);    // Draw the user ship
    procedure DrawBullets(Rp: PRastPort; DrawRect: TRect); // Draw the bullets fired by user and
    procedure DrawTitle(Rp: PRastPort; DrawRect: TRect);   // Draw non-Game Text
  public
    constructor Create(const Args: array of PtrUInt); override; // Create it
    destructor Destroy; override; // Destroy it
    procedure DoRedrawing; // update the Game if running, called by main application
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
  PlayModus := pmStopped; // initially the game does not run
  // Text without Game
  TitleText := TStringList.Create;
  TitleText.Text := StarterText;
  // init the game Text
  TextLine := 0;
  AboutText := TStringList.Create;
  AboutText.Text := DefText;
  // Enemies, Bullets and the Ship
  EnemyList := TEnemyList.Create;
  BulletList := TBulletList.Create;
  Ship := TShip.Create;
  Ship.Pos.X := PlayWidth div 2;
  Ship.Pos.Y := 280;
  // Init PlayArea size
  MinWidth := PlayWidth;
  MaxWidth := PlayWidth;
  MinHeight := PlayHeight;
  MaxHeight := PlayHeight;
  // MUI Events
  OnDrawObject := @DrawEvent;
  OnMUIKeyDown := @KeyDownEvent;
  // just some inits
  LastCall := 0; // last time movement was done
  MoveDir := 1;  // by default move to right first
  Points := 0;   // 0 Points as start
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
  // only redraw regularly if the Window is visible and Game is running
  if LongBool(MH_Get(MUIObject, MUIA_ShowMe)) and (PlayModus = pmRunning) then
    RedrawObject;
end;

// Key down Event
procedure TPlayPanel.KeyDownEvent(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
begin
  if PlayModus = pmRunning then
  begin
    // we are in the Game
    case Code of
      // Move Shipt to left
      CURSORLEFT: begin
        Dec(Ship.Pos.X, 5);
        EatEvent := True;
      end;
      // Move Ship to right
      CURSORRIGHT: begin
        Inc(Ship.Pos.X, 5);
        EatEvent := True;
      end;
    end;
    // Check if Ship reached Border
    if Ship.Pos.X > (PlayWidth - MINSHIPDISTANCE) then
      Ship.Pos.X := (PlayWidth - MINSHIPDISTANCE);
    if Ship.Pos.X < MINSHIPDISTANCE then
      Ship.Pos.X := MINSHIPDISTANCE;
    // Fire a bullet with Space
    if Key = ' ' then
    begin
      FireBullet();
    end;
  end
  else
  begin
    // Not in Game
    if Key = #13 then // Enter Key
    begin
      // init all the stuff again
      BulletList.Clear;
      EnemyList.Clear;
      Points := 0;
      TextLine := 0;
      LastCall := 0;
      // start the game
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
  Dist, Dist1: Classes.TPoint;
  Aspect: Single;
begin
  // get this calls time
  CurCall := GetMUITime;
  // no last call so we just leave
  if LastCall = 0 then
  begin
    LastCall := CurCall;
    Exit;
  end;
  // Calculate individual Enemy Movement when they fly towards Points
  // faster movement
  Delta := (CurCall - LastCall) div 5; // 1 Pixel per 5 ms
  for i := EnemyList.Count - 1 downto 0 do
  begin
    E := EnemyList[i];
    // Idle means it was not hit and as a aim to fly to
    if E.Modus <> emIdle then
    begin
      // Calculate the Distance to aim
      Dist.X := E.Aim.X - E.Pos.X;
      Dist.Y := E.Aim.Y - E.Pos.Y;
      Dist1.Y := 0;
      Dist1.X := 0;
      // maybe it's there already
      if (Dist.Y <> 0) and (Dist.X <> 0) then
      begin
        Aspect := Dist.X / Dist.Y; // Aspect of movement = direction of movement
        Dist1.Y := Round(Delta / (Aspect + 1)); // calculate the actual length of movement for Y
        Dist1.X := Delta - Dist1.Y;             // and X
      end
      else
      begin
        // if one distance is 0 it's very easy, just go the length
        if Dist.X = 0 then
          Dist1.Y := - Sign(Dist.Y) * Delta;
        if Dist.Y = 0 then
          Dist1.X := - Sign(Dist.X) * Delta;
      end;
      if Abs(Dist1.x) > Abs(Dist.x) then      // limit movement that it not overshoot it
        Dist1.x := Dist.x;
      if Abs(Dist1.y) > Abs(Dist.y) then
        Dist1.y := Dist.y;
      // Calculate real movement
      E.Pos.X := E.Pos.X - Dist1.X;
      E.Pos.Y := E.Pos.Y - Abs(Dist1.Y + 1); // always fly up
      //
      // already close enough or even out of sight, then remove it and add it's points
      if (E.Modus = emHit) and (((Abs(E.Pos.X - E.Aim.X) <= 5) and (Abs(E.Pos.Y - E.Aim.Y) <= 5))  or (E.Pos.Y <= 5)) then
      begin
        Inc(Points, E.Pts);
        EnemyList.Delete(i);
      end;
    end;
  end;
  // calcuate the joint movements of the complete text.
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
      Bullet.Pos.Y := Bullet.Pos.Y - 2 * Delta;
      if Bullet.Pos.Y < 8 then
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
      BR := Rect(B.Pos.X, B.Pos.Y, B.Pos.X + 2, B.Pos.Y + 3);
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
  P: Classes.TPoint;
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
    DrawEllipse(RP, Bullet.Pos.X, Bullet.Pos.Y, 2, 4);
  end;
end;

procedure  TPlayPanel.DrawTitle(Rp: PRastPort; DrawRect: TRect);
var
  s: string;
  TE: TTextExtent;
  x, y, i: Integer;
begin
  SetAPen(Rp, 2);
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
    MUIA_Window_Title,     AsTag(PChar(GetLocString(MSG_MENU_ABOUT) + ' MUIMapparium')),
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
          {$ifdef AMIGA68k}
            {$ifdef FPUSOFT}
            Child, AsTag(MH_Text(PChar('  Target: ' + {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%} + ' Soft  '))),
            {$else}
            Child, AsTag(MH_Text(PChar('  Target: ' + {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%} + ' 68881 '))),
            {$endif}
          {$else}
          Child, AsTag(MH_Text(PChar('  Target: ' + {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%} + '       '))),
          {$endif}
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
