object dmAPICall: TdmAPICall
  Height = 480
  Width = 640
  object OTPProject: TRESTClient
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'utf-8, *;q=0.8'
    BaseURL = 'http://localhost/otpauth/src'
    ContentType = 'application/x-www-form-urlencoded'
    Params = <>
    SynchronizedEvents = False
    Left = 72
    Top = 64
  end
  object OTPSendRequest: TRESTRequest
    AssignedValues = [rvConnectTimeout, rvReadTimeout]
    Client = OTPProject
    Method = rmPOST
    Params = <
      item
        Name = 'email'
      end>
    Resource = 'api-otp-send.php'
    Response = OTPSendResponse
    SynchronizedEvents = False
    Left = 168
    Top = 64
  end
  object OTPSendResponse: TRESTResponse
    ContentType = 'application/json'
    Left = 296
    Top = 64
  end
  object OTPCheckRequest: TRESTRequest
    AssignedValues = [rvConnectTimeout, rvReadTimeout]
    Client = OTPProject
    Method = rmPOST
    Params = <
      item
        Name = 'email'
      end
      item
        Name = 'code'
      end>
    Resource = 'api-otp-check.php'
    Response = OTPCheckResponse
    SynchronizedEvents = False
    Left = 168
    Top = 152
  end
  object OTPCheckResponse: TRESTResponse
    ContentType = 'application/json'
    Left = 296
    Top = 152
  end
  object OTPLogoutRequest: TRESTRequest
    AssignedValues = [rvConnectTimeout, rvReadTimeout]
    Client = OTPProject
    Method = rmPOST
    Params = <
      item
        Name = 'sessid'
      end>
    Resource = 'api-logout.php'
    Response = OTPLogoutResponse
    SynchronizedEvents = False
    Left = 168
    Top = 232
  end
  object OTPLogoutResponse: TRESTResponse
    ContentType = 'application/json'
    Left = 296
    Top = 232
  end
end
