unit networkingunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient,
  {$ifdef HASAMIGA}
  sockets,
  {$endif}
  Dos, versionunit;

function GetFile(hp: TFPHTTPClient; address: string; AStream: TStream): Boolean;
function GetFile(address: string; AStream: TStream): Boolean; overload;
function GetFile(address, filename: string): Boolean; overload;
function GetCurlFile(address: string; AStream: TStream): Boolean;
function GetYourFile(address: string; AStream: TStream): Boolean;

var
  IsOnline: Boolean = True;
  OfflineMsg: Boolean = False;
  OfflineErrText: string = '';
  LBytes: Cardinal = 0;
  LTimes: Cardinal = 0;

implementation




function GetFile(hp: TFPHTTPClient; address: string; AStream: TStream): Boolean;
var
  t1: Int64;
begin
  Result := False;
  hp.AddHeader('User-Agent', WindowTitleTemplate);
  t1 := GetMsCount;
  hp.Get(address, AStream);
  t1 := GetMsCount - t1;
  InterLockedExchangeAdd(LBytes, AStream.Size);
  InterLockedExchangeAdd(LTimes, t1);
  Result := True;
end;

function GetFile(address: string; AStream: TStream): Boolean;
var
  hp: TFPHTTPClient;
  t1: Int64;
begin
  Result := False;
  if not IsOnline then
    Exit;
  hp := TFPHTTPClient.Create(nil);
  try
    hp.AddHeader('User-Agent', WindowTitleTemplate);
    t1 := GetMsCount;
    hp.Get(address, AStream);
    t1 := GetMsCount - t1;
    InterLockedExchangeAdd(LBytes, AStream.Size);
    InterLockedExchangeAdd(LTimes, t1);
    Result := True;
  finally
    hp.Free;
  end;
end;

function GetYourFile(address: string; AStream: TStream): Boolean;
var
  hp: TFPHTTPClient;
  t1: Int64;
begin
  Result := False;
  if not IsOnline then
    Exit;
  hp := TFPHTTPClient.Create(nil);
  try
    hp.AddHeader('User-Agent', WindowTitleTemplate);
    hp.AddHeader('X-Yours-client', 'MUIMapparium');
    t1 := GetMsCount;
    hp.Get(address, AStream);
    t1 := GetMsCount - t1;
    InterLockedExchangeAdd(LBytes, AStream.Size);
    InterLockedExchangeAdd(LTimes, t1);
    Result := True;
  finally
    hp.Free;
  end;
end;

function GetCurlFile(address: string; AStream: TStream): Boolean;
var
  hp: TFPHTTPClient;
  t1: Int64;
begin
  Result := False;
  if not IsOnline then
    Exit;
  hp := TFPHTTPClient.Create(nil);
  try
    hp.AddHeader('User-Agent', 'curl/7.47.0');
    t1 := GetMsCount;
    hp.Get(address, AStream);
    t1 := GetMsCount - t1;
    InterLockedExchangeAdd(LBytes, AStream.Size);
    InterLockedExchangeAdd(LTimes, t1);
    Result := True;
  finally
    hp.Free;
  end;
end;

function GetFile(address, filename: string): Boolean;
var
  hp: TFPHTTPClient;
  t1: Int64;
begin
  Result := False;
  if not IsOnline then
    Exit;
  hp := TFPHTTPClient.Create(nil);
  try
    hp.AddHeader('User-Agent', WindowTitleTemplate);
    t1 := GetMsCount;
    hp.Get(address, filename);
    t1 := GetMsCount - t1;
    InterLockedExchangeAdd(LBytes, 10);
    InterLockedExchangeAdd(LTimes, t1);
    Result := True;
  finally
    hp.Free;
  end;
end;



initialization
  {$ifdef HASAMIGA}
  IsOnline := Assigned(SocketBase);
  if not IsOnline then
    writeln('Warning no internet connection found, Offline mode.');
  {$endif}
finalization
end.

