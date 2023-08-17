unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.Objects, FMX.Layouts;

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
    WaitAnimIndicator: TAniIndicator;
    WaitAnim: TLayout;
    WaitAnimBackground: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure btnSendOTPClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnLogoutClick(Sender: TObject);
    procedure btnSendCodeAgainClick(Sender: TObject);
  private
    FEmail: string;
    FSessionID: string;
    FWaitAnimRun: boolean;
    procedure SetEmail(const Value: string);
    procedure SetSessionID(const Value: string);
    procedure SetWaitAnimRun(const Value: boolean);
  public
    property Email: string read FEmail write SetEmail;
    property SessionID: string read FSessionID write SetSessionID;
    property WaitAnimRun: boolean read FWaitAnimRun write SetWaitAnimRun;
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
    TabControl1.Enabled := false;
    WaitAnimRun := true;
    OTP_Check_Code_async(Email, edtOTPCode.Text,
      procedure(Const AsessionID: string)
      begin
        TabControl1.Enabled := true;
        WaitAnimRun := false;
        SessionID := AsessionID;
        edtOTPCode.Text := '';
        TabControl1.Next;
      end,
      procedure(Const E: exception)
      begin
        TabControl1.Enabled := true;
        WaitAnimRun := false;
        if assigned(E) then
          // TODO : intercept E.ClassName if needed
          raise exception.Create(E.Message);
      end);
  end;
end;

procedure TForm1.btnLogoutClick(Sender: TObject);
begin
  TabControl1.Enabled := false;
  WaitAnimRun := true;
  OTP_Logout_Async(SessionID,
    procedure
    begin
      TabControl1.Enabled := true;
      WaitAnimRun := false;
      SessionID := '';
      Email := '';
      TabControl1.GotoVisibleTab(0);
    end,
    procedure(Const E: exception)
    begin
      TabControl1.Enabled := true;
      WaitAnimRun := false;
      if assigned(E) then
        // TODO : intercept E.ClassName if needed
        raise exception.Create(E.Message);
    end);
end;

procedure TForm1.btnSendCodeAgainClick(Sender: TObject);
begin
  TabControl1.Enabled := false;
  WaitAnimRun := true;
  OTP_Send_Code_Async(Email,
    procedure
    begin
      TabControl1.Enabled := true;
      WaitAnimRun := false;
      showmessage('New code sent.');
    end,
    procedure(Const E: exception)
    begin
      TabControl1.Enabled := true;
      WaitAnimRun := false;
      if assigned(E) then
        // TODO : intercept E.ClassName if needed
        raise exception.Create(E.Message);
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
    // btnSendOTP.Enabled := false;
    TabControl1.Enabled := false;
    WaitAnimRun := true;
    OTP_Send_Code_Async(edtEmail.Text,
      procedure
      begin
        // btnSendOTP.Enabled := true;
        TabControl1.Enabled := true;
        WaitAnimRun := false;
        Email := edtEmail.Text;
        edtEmail.Text := '';
        TabControl1.Next;
      end,
      procedure(Const E: exception)
      begin
        // btnSendOTP.Enabled := true;
        TabControl1.Enabled := true;
        WaitAnimRun := false;
        if assigned(E) then
          // TODO : intercept E.ClassName if needed
          raise exception.Create(E.Message);
      end);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem1;
  FEmail := '';
  FSessionID := '';
  WaitAnimRun := false;
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

procedure TForm1.SetWaitAnimRun(const Value: boolean);
begin
  FWaitAnimRun := Value;
  if FWaitAnimRun then
  begin
    WaitAnim.Visible := true;
    WaitAnim.BringToFront;
    WaitAnimIndicator.Enabled := true;
    WaitAnimIndicator.BringToFront;
  end
  else
  begin
    WaitAnimIndicator.Enabled := false;
    WaitAnim.Visible := false;
  end;
end;

end.
