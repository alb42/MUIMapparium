unit updateunit;
{$mode objfpc}{$H+}
interface

const
  VERSIONURL = 'http://home.alb42.de/files/mapparium.version';

procedure CheckForUpdate;

implementation

uses
  Classes, SysUtils, Versionunit, networkingunit, mui, muiwrap;


procedure CheckForUpdate;
var
  ss: TStringStream;
  OnlineVersion: string;
  Msg: string;
  VerN: single;
  Major, Minor: Integer;
begin
  Msg := 'Error to get update information.';
  ss := TStringStream.Create;
  try
    if GetFile(VERSIONURL, ss) then
    begin
      ss.Position := 0;
      OnlineVersion := ss.DataString;
      VerN := StrToFloatDef(OnlineVersion, 0.0);
      Major := Trunc(VerN);
      Minor := Round(10 * Frac(VerN));
      //
      if (Major > VersionMajor) or ((Major = VersionMajor) and (Minor > VersionMinor)) then
      begin
        Msg := #27'bUpdate available'#10#27'n' +
               '  Online available: '#27't' + IntToStr(Major) + '.' + IntToStr(Minor) + #10 +
               '  Your Version:     '#27't' + IntToStr(VersionMajor) + '.' + IntToStr(VersionMinor) + #10 +
               'Check http://blog.alb42.de/muimapparium for updates.';
      end
      else
      begin
        Msg := 'No Update available, already current Version ' + OnlineVersion;
      end;
    end;
  except
    On E: Exception do
    begin
      Msg := 'Error to check for upload: ' + E.Message;
    end;
  end;
  ss.Free;
  ShowMessage('Update Check', 'OK', Msg);
end;


end.
