program makeversion;

uses
  Sysutils, VersionUnit;

procedure StartMe;
var
  str: String;
begin
  Str := Trim(WindowTitleTemplate);
  Delete(Str, 1, Pos(' ', Str));
  write(trim(Str));
end;

begin
  StartMe;
end.
