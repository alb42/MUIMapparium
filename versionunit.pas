unit VersionUnit;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, StrUtils;
const
  VERSIONSTRING = '$VER: MUIMapparium 0.8 (16.05.2018)';
var
  WindowTitleTemplate: string;
  VersionMajor: Integer;
  VersionMinor: Integer;

implementation

procedure CreateVersion;
var
  str: string;
  VerN: Single;
begin
  Str := VERSIONSTRING;
  Delete(Str, 1, 6); // remove $VER
  Delete(Str, Pos('(', Str), Length(Str));
  Str := Trim(Str);
  WindowTitleTemplate := Str;
  //
  Delete(Str, 1, Pos(' ', Str));
  VerN := StrToFloatDef(Str, 0.0);
  VersionMajor := StrToIntDef(Copy(Str, 1, Pos('.', Str)), 0);
  VersionMinor := StrToIntDef(Copy(Str, Pos('.', Str) + 1, Length(Str)), 0);
end;


initialization
  //writeln('enter version');
  CreateVersion;
  //writeln('leave version');

end.
