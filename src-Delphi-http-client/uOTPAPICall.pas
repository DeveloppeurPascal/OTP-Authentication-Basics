unit uOTPAPICall;

interface

uses
  System.SysUtils,
  System.JSON;

Const
  CAPIURL = 'http://localhost/otpauth/src/';

Type
  /// <summary>
  /// Sent when the server respond with an error
  /// </summary>
  EOTPAPIError = class(exception)
  end;

  /// <summary>
  /// Sent when the server crashed :
  /// - http status <> 200
  /// - response is not a json object
  /// </summary>
  EOTPServerScriptError = class(exception)
  end;

  TOTPProc = reference to procedure;
  TOTPProc<T> = reference to procedure(Const P: T);
  TOTPErrorProc = reference to procedure(Const E: exception);
  TOTPEvent = procedure of object;
  TOTPEvent<T> = procedure(Const P: T) of object;
  TOTPErrorEvent = procedure(Const E: exception) of object;

procedure OTP_Send_Code(email: string);
procedure OTP_Send_Code_Async(email: string; CallbackProc: TOTPProc;
  ErrorCallbackProc: TOTPErrorProc = nil); overload;
procedure OTP_Send_Code_Async(email: string; CallbackEvent: TOTPEvent;
  ErrorCallbackEvent: TOTPErrorEvent = nil); overload;

function OTP_Check_Code(email, code: string): string;
procedure OTP_Check_Code_Async(email, code: string;
  CallbackProc: TOTPProc<string>;
  ErrorCallbackProc: TOTPErrorProc = nil); overload;
procedure OTP_Check_Code_Async(email, code: string;
  CallbackEvent: TOTPEvent<string>;
  ErrorCallbackEvent: TOTPErrorEvent = nil); overload;

procedure OTP_Logout(SessID: string);
procedure OTP_Logout_Async(SessID: string; CallbackProc: TOTPProc;
  ErrorCallbackProc: TOTPErrorProc = nil); overload;
procedure OTP_Logout_Async(SessID: string; CallbackEvent: TOTPEvent;
  ErrorCallbackEvent: TOTPErrorEvent = nil); overload;

implementation

uses
  System.Classes,
  System.Net.HttpClient;

procedure OTP_Send_Code(email: string);
var
  HTTP: THTTPClient;
  HTTPResponse: IHTTPResponse;
  Params: TStringList;
  JSO: TJSONObject;
  cas, errortext: string;
begin
  HTTP := THTTPClient.Create;
  try
    Params := TStringList.Create;
    try
      Params.AddPair('email', email);
{$IFDEF DEBUG}
      sleep(2000);
{$ENDIF}
      HTTPResponse := HTTP.Post(CAPIURL + 'api-otp-send.php', Params);
      if HTTPResponse.StatusCode <> 200 then
      begin
        try
          cas := HTTPResponse.ContentAsString(tencoding.utf8);
          JSO := TJSONObject.ParseJSONValue(cas) as TJSONObject;
        except
          if cas.IsEmpty then
            raise EOTPServerScriptError.Create(HTTPResponse.StatusText)
          else // dangerous in production environment (PHP errors or other scripts failures)
            raise EOTPServerScriptError.Create(cas);
        end;
        if JSO.TryGetValue<string>('errortext', errortext) then
          raise EOTPAPIError.Create(errortext)
        else
          raise exception.Create('Error with no detail.');
      end;
    finally
      Params.free;
    end;
  finally
    HTTP.free;
  end;
end;

procedure OTP_Send_Code_Async(email: string; CallbackProc: TOTPProc;
  ErrorCallbackProc: TOTPErrorProc);
begin
  tthread.CreateAnonymousThread(
    procedure
    begin
      try
        OTP_Send_Code(email);
        if Assigned(CallbackProc) then
          tthread.Synchronize(nil,
            procedure
            begin
              CallbackProc;
            end);
      except
        on E: exception do
          if Assigned(ErrorCallbackProc) then
            tthread.Synchronize(nil,
              procedure
              begin
                ErrorCallbackProc(E);
              end);
      end;
    end).Start;
end;

procedure OTP_Send_Code_Async(email: string; CallbackEvent: TOTPEvent;
ErrorCallbackEvent: TOTPErrorEvent = nil);
begin
  OTP_Send_Code_Async(email,
    procedure
    begin
      if Assigned(CallbackEvent) then
        CallbackEvent;
    end,
    procedure(Const E: exception)
    begin
      if Assigned(ErrorCallbackEvent) then
        ErrorCallbackEvent(E);
    end);
end;

function OTP_Check_Code(email, code: string): string;
var
  HTTP: THTTPClient;
  HTTPResponse: IHTTPResponse;
  Params: TStringList;
  JSO: TJSONObject;
  cas, errortext: string;
begin
  result := '';
  HTTP := THTTPClient.Create;
  try
    Params := TStringList.Create;
    try
      Params.AddPair('email', email);
      Params.AddPair('code', code);
{$IFDEF DEBUG}
      sleep(2000);
{$ENDIF}
      HTTPResponse := HTTP.Post(CAPIURL + 'api-otp-check.php', Params);
      try
        cas := HTTPResponse.ContentAsString(tencoding.utf8);
        JSO := TJSONObject.ParseJSONValue(cas) as TJSONObject;
      except
        if cas.IsEmpty then
          raise EOTPServerScriptError.Create(HTTPResponse.StatusText)
        else // dangerous in production environment (PHP errors or other scripts failures)
          raise EOTPServerScriptError.Create(cas);
      end;
      case HTTPResponse.StatusCode of
        200:
          if not JSO.TryGetValue<string>('sessid', result) then
            raise EOTPAPIError.Create('No session ID.');
      else
        if JSO.TryGetValue<string>('errortext', errortext) then
          raise EOTPAPIError.Create(errortext)
        else
          raise exception.Create('Error with no detail.');
      end;
    finally
      Params.free;
    end;
  finally
    HTTP.free;
  end;
end;

procedure OTP_Check_Code_Async(email, code: string;
CallbackProc: TOTPProc<string>; ErrorCallbackProc: TOTPErrorProc);
begin
  tthread.CreateAnonymousThread(
    procedure
    var
      SessID: string;
    begin
      try
        SessID := OTP_Check_Code(email, code);
        if Assigned(CallbackProc) then
          tthread.Synchronize(nil,
            procedure
            begin
              CallbackProc(SessID);
            end);
      except
        on E: exception do
          if Assigned(ErrorCallbackProc) then
            tthread.Synchronize(nil,
              procedure
              begin
                ErrorCallbackProc(E);
              end);
      end;
    end).Start;
end;

procedure OTP_Check_Code_Async(email, code: string;
CallbackEvent: TOTPEvent<string>; ErrorCallbackEvent: TOTPErrorEvent = nil);
begin
  OTP_Check_Code_Async(email, code,
    procedure(Const SessID: string)
    begin
      if Assigned(CallbackEvent) then
        CallbackEvent(SessID);
    end,
    procedure(Const E: exception)
    begin
      if Assigned(ErrorCallbackEvent) then
        ErrorCallbackEvent(E);
    end);
end;

procedure OTP_Logout(SessID: string);
var
  HTTP: THTTPClient;
  HTTPResponse: IHTTPResponse;
  Params: TStringList;
  JSO: TJSONObject;
  cas, errortext: string;
begin
  HTTP := THTTPClient.Create;
  try
    Params := TStringList.Create;
    try
      Params.AddPair('sessid', SessID);
{$IFDEF DEBUG}
      sleep(2000);
{$ENDIF}
      HTTPResponse := HTTP.Post(CAPIURL + 'api-logout.php', Params);
      if HTTPResponse.StatusCode <> 200 then
      begin
        try
          cas := HTTPResponse.ContentAsString(tencoding.utf8);
          JSO := TJSONObject.ParseJSONValue(cas) as TJSONObject;
        except
          if cas.IsEmpty then
            raise EOTPServerScriptError.Create(HTTPResponse.StatusText)
          else // dangerous in production environment (PHP errors or other scripts failures)
            raise EOTPServerScriptError.Create(cas);
        end;
        if JSO.TryGetValue<string>('errortext', errortext) then
          raise EOTPAPIError.Create(errortext)
        else
          raise exception.Create('Error with no detail.');
      end;
    finally
      Params.free;
    end;
  finally
    HTTP.free;
  end;
end;

procedure OTP_Logout_Async(SessID: string; CallbackProc: TOTPProc;
ErrorCallbackProc: TOTPErrorProc);
begin
  tthread.CreateAnonymousThread(
    procedure
    begin
      try
        OTP_Logout(SessID);
        if Assigned(CallbackProc) then
          tthread.Synchronize(nil,
            procedure
            begin
              CallbackProc;
            end);
      except
        on E: exception do
          if Assigned(ErrorCallbackProc) then
            tthread.Synchronize(nil,
              procedure
              begin
                ErrorCallbackProc(E);
              end);
      end;
    end).Start;
end;

procedure OTP_Logout_Async(SessID: string; CallbackEvent: TOTPEvent;
ErrorCallbackEvent: TOTPErrorEvent = nil);
begin
  OTP_Logout_Async(SessID,
    procedure
    begin
      if Assigned(CallbackEvent) then
        CallbackEvent;
    end,
    procedure(Const E: exception)
    begin
      if Assigned(ErrorCallbackEvent) then
        ErrorCallbackEvent(E);
    end);
end;

end.
