program OTPRESTClientSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  uOTPAPICall in 'uOTPAPICall.pas' {dmAPICall: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TdmAPICall, dmAPICall);
  Application.Run;
end.
