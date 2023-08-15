unit uOTPAPICall;

interface

uses
  System.SysUtils, System.Classes, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope;

type
  TdmAPICall = class(TDataModule)
    OTPProject: TRESTClient;
    OTPSendRequest: TRESTRequest;
    OTPSendResponse: TRESTResponse;
    OTPCheckRequest: TRESTRequest;
    OTPCheckResponse: TRESTResponse;
    OTPLogoutRequest: TRESTRequest;
    OTPLogoutResponse: TRESTResponse;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  dmAPICall: TdmAPICall;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
