unit VersionUnit;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, StrUtils;
const
  VERSIONSTRING = '$VER: MUIMapparium 0.4 (26.05.2017)';
var
  WindowTitleTemplate: string;
implementation

procedure CreateVersion;
var
  str: string;
begin
  Str := VERSIONSTRING;
  Delete(Str, 1, 6); // remove $VER
  Delete(Str, Pos('(', Str), Length(Str));
  WindowTitleTemplate := Str;
end;


initialization
  CreateVersion;

end.
