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
    btnSendCodeAgain: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSendOTPClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnLogoutClick(Sender: TObject);
    procedure btnSendCodeAgainClick(Sender: TObject);
  private
    FEmail: string;
    FSessionID: string;
    procedure SetEmail(const Value: string);
    procedure SetSessionID(const Value: string);
  public
    property Email: string read FEmail write SetEmail;
    property SessionID: string read FSessionID write SetSessionID;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.JSON,
  uOTPAPICall;

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  if edtOTPCode.Text.IsEmpty then
  begin
    edtOTPCode.SetFocus;
    raise exception.Create('Please give the code sent to your email address !');
  end
  else
  begin
    btnConnect.Enabled := false;
    OTP_Check_Code_async(Email, edtOTPCode.Text,
      procedure(AsessionID: string)
      begin
        btnConnect.Enabled := true;
        SessionID := AsessionID;
        edtOTPCode.Text := '';
        TabControl1.Next;
      end,
      procedure(Const E: exception)
      begin
        btnConnect.Enabled := true;
        if assigned(E) then
          raise E;
      end);
  end;
end;

procedure TForm1.btnLogoutClick(Sender: TObject);
begin
  btnLogout.Enabled := false;
  OTP_Logout_Async(SessionID,
    procedure
    begin
      btnLogout.Enabled := true;
      SessionID := '';
      Email := '';
      TabControl1.GotoVisibleTab(0);
    end,
    procedure(Const E: exception)
    begin
      btnLogout.Enabled := true;
      if assigned(E) then
        raise E;
    end);
end;

procedure TForm1.btnSendCodeAgainClick(Sender: TObject);
begin
  btnSendCodeAgain.Enabled := false;
  OTP_Send_Code_Async(Email,
    procedure
    begin
      btnSendCodeAgain.Enabled := true;
      showmessage('New code sent.');
    end,
    procedure(Const E: exception)
    begin
      btnSendCodeAgain.Enabled := true;
      if assigned(E) then
        raise E;
    end);
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
    btnSendOTP.Enabled := false;
    OTP_Send_Code_Async(edtEmail.Text,
      procedure
      begin
        btnSendOTP.Enabled := true;
        Email := edtEmail.Text;
        edtEmail.Text := '';
        TabControl1.Next;
      end,
      procedure(Const E: exception)
      begin
        btnSendOTP.Enabled := true;
        if assigned(E) then
          raise E;
      end);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem1;
  FEmail := '';
  FSessionID := '';
end;

procedure TForm1.SetEmail(const Value: string);
begin
  FEmail := Value;
end;

procedure TForm1.SetSessionID(const Value: string);
begin
  FSessionID := Value;
  lblSessionID.Text := FSessionID;
end;

end.
