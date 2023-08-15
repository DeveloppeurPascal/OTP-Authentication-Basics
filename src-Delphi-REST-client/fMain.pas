unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit;

type
  TForm1 = class(TForm)
    edtEmail: TEdit;
    btnSendOTP: TButton;
    edtOTPCode: TEdit;
    btnConnect: TButton;
    btnLogout: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    lblSessionID: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnSendOTPClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnLogoutClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.JSON,
  uOTPAPICall;

procedure TForm1.btnConnectClick(Sender: TObject);
var
  jso: TJSONObject;
  SessionID: string;
begin
  if edtOTPCode.Text.IsEmpty then
  begin
    edtOTPCode.SetFocus;
    raise exception.Create('Please give the code sent to your email address !');
  end
  else
  begin
    // dmapicall.OTPCheckRequest.Params.Items[0].Value := edtEmail.Text;
    dmapicall.OTPCheckRequest.Params.Items
      [dmapicall.OTPCheckRequest.Params.IndexOf('email')].Value :=
      edtEmail.Text;
    // dmapicall.OTPCheckRequest.Params.Items[1].Value := edtOTPCode.Text;
    dmapicall.OTPCheckRequest.Params.Items
      [dmapicall.OTPCheckRequest.Params.IndexOf('code')].Value := edtOTPCode.Text;
    dmapicall.OTPCheckRequest.Execute;
    if (dmapicall.OTPCheckResponse.StatusCode <> 200) then
      raise exception.Create(dmapicall.OTPCheckResponse.JSONText)
    else if (dmapicall.OTPCheckResponse.JSONValue is TJSONObject) then
    begin
      jso := dmapicall.OTPCheckResponse.JSONValue as TJSONObject;
      if jso.TryGetValue<string>('sessid', SessionID) then
      begin
        lblSessionID.Text := SessionID;
        TabControl1.Next;
      end
      else
        raise exception.Create('Unknow session ID.' + slinebreak +
          dmapicall.OTPCheckResponse.JSONText); // Don't do it in production !
    end
    else
      raise exception.Create('Wrong answer format.' + slinebreak +
        dmapicall.OTPCheckResponse.JSONText); // Don't do it in production !
  end;
end;

procedure TForm1.btnLogoutClick(Sender: TObject);
begin
    dmapicall.OTPLogoutRequest.Params.Items[0].Value := lblSessionID.Text;
    dmapicall.OTPLogoutRequest.Execute;
    if (dmapicall.OTPSendResponse.StatusCode <> 200) then
      raise exception.Create(dmapicall.OTPSendResponse.JSONText)
    else
      TabControl1.GotoVisibleTab(0);
end;

procedure TForm1.btnSendOTPClick(Sender: TObject);
begin
  if edtEmail.Text.IsEmpty then
  begin
    edtEmail.SetFocus;
    raise exception.Create('Please give your email !');
  end
  else
  begin
    dmapicall.OTPSendRequest.Params.Items[0].Value := edtEmail.Text;
    dmapicall.OTPSendRequest.Execute;
    if (dmapicall.OTPSendResponse.StatusCode <> 200) then
      raise exception.Create(dmapicall.OTPSendResponse.JSONText)
    else
      TabControl1.Next;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem1;
end;

end.
