unit updateunit;
{$mode objfpc}{$H+}
interface

const
  VERSIONURL = 'http://home.alb42.de/files/mapparium.version';
  UPDATEURL = 'http://blog.alb42.de/muimapparium';

procedure CheckForUpdate;

implementation

uses
  Classes, SysUtils, Versionunit, networkingunit, mui, muiwrap, MUIMappariumlocale;


procedure CheckForUpdate;
var
  ss: TStringStream;
  OnlineVersion: string;
  Msg: string;
  VerN: single;
  Major, Minor: Integer;
begin
  Msg := GetLocString(MSG_UPDATE_ERROR); //'Error to get update information.';
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
        Msg := GetLocString(MSG_UPDATE_INFO);
        Msg := StringReplace(Msg, '$OLDVER$', IntToStr(Major) + '.' + IntToStr(Minor), [rfReplaceAll]);
        Msg := StringReplace(Msg, '$NEWVER$', IntToStr(VersionMajor) + '.' + IntToStr(VersionMinor), [rfReplaceAll]);
        Msg := StringReplace(Msg, '$URL$', UPDATEURL, [rfReplaceAll]);
      end
      else
      begin
        Msg := GetLocString(MSG_UPDATE_NONE); //'No Update available, already current Version ' + OnlineVersion;
      end;
    end;
  except
    On E: Exception do
    begin
      Msg := GetLocString(MSG_UPDATE_ERROR) + ': ' + E.Message;
    end;
  end;
  ss.Free;
  ShowMessage(GetLocString(MSG_UPDATE_CHECK), GetLocString(MSG_GENERAL_OK), Msg);
end;


end.
