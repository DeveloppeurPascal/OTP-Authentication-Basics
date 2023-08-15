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

procedure OTP_Send_Code(email: string);

function OTP_Check_Code(email, code: string): string;

procedure OTP_Logout(SessID: string);

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
  cas: string;
begin
  HTTP := THTTPClient.Create;
  try
    Params := TStringList.Create;
    try
      Params.AddPair('email', email);
      HTTPResponse := HTTP.Post(CAPIURL + 'api-otp-send.php', Params);
      if HTTPResponse.StatusCode <> 200 then
        try
          cas := HTTPResponse.ContentAsString(tencoding.utf8);
          JSO := TJSONObject.ParseJSONValue(cas) as TJSONObject;
          if JSO.TryGetValue<string>('errortext', cas) then
            raise EOTPAPIError.Create(cas)
          else
            raise exception.Create('');
        except
          if cas.IsEmpty then
            raise EOTPServerScriptError.Create(HTTPResponse.StatusText)
          else
            raise EOTPServerScriptError.Create(cas);
        end;
    finally
      Params.free;
    end;
  finally
    HTTP.free;
  end;
end;

function OTP_Check_Code(email, code: string): string;
begin
end;

procedure OTP_Logout(SessID: string);
begin
end;

end.
