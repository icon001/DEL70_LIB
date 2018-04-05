unit uFingerRegistDevice;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,FINGERAPI22, DB, ADODB;

const
  IMG_X=280;
  IMG_Y=320;


type
  TfmFingerRegistDevice = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Panel2: TPanel;
    Image2: TImage;
    Label1: TLabel;
    cmb_FingerPort: TComboBox;
    btnRefresh: TButton;
    btn_Cancel: TBitBtn;
    btn_Save: TBitBtn;
    GetFingerTimer: TTimer;
    panMessage: TPanel;
    lb_Message: TLabel;
    MessageTimer: TTimer;
    TempADOQuery: TADOQuery;
    procedure btn_CancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btn_SaveClick(Sender: TObject);
    procedure cmb_FingerPortChange(Sender: TObject);
    procedure GetFingerTimerTimer(Sender: TObject);
    procedure MessageTimerTimer(Sender: TObject);
  private
    FFingerCardNo: string;
    FFingerUserID: string;
    gFeature1  : TFeature;
    gFeature2  : TFeature;
    L_bGetFinger_1 : Boolean;
    L_bGetFinger_2 : Boolean;
    L_bDelay : Boolean;
    FSave: Boolean;
    { Private declarations }
    Function GetFinger1:Boolean;
    Function GetFinger2:Boolean;
  public
    { Public declarations }
    property FingerUserID : string read FFingerUserID write FFingerUserID;
    property FingerCardNo : string read FFingerCardNo write FFingerCardNo;
    property Save : Boolean read FSave write FSave;
  end;

var
  fmFingerRegistDevice: TfmFingerRegistDevice;

implementation

uses
  uDataModule1,
  udmAdoQuery,
  uLomosUtil, uDBFunction;
{$R *.dfm}

procedure TfmFingerRegistDevice.btn_CancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmFingerRegistDevice.FormActivate(Sender: TObject);
begin
  L_bGetFinger_1 := False;
  L_bGetFinger_2 := False;
  Save := False;
  btnRefreshClick(btnRefresh);
end;

procedure TfmFingerRegistDevice.btnRefreshClick(Sender: TObject);
var
  Feature1  : TFeature;
  RawImage1 : TRawImage;
  DeviceNum,ret       : Integer;
begin
  cmb_FingerPort.Clear;
  btnRefresh.Enabled:=false;
  //Finding USB Port Number of FingerPrint Reader Device ;
  for DeviceNum:=0 to 255 do
  begin
    ret:=GetFinger(DeviceNum,RawImage1,Feature1);
    if ret= 0 then cmb_FingerPort.Items.Add(intTostr(DeviceNum))
  end;//end for

  if cmb_FingerPort.Items.Count < 1 then
  begin
    showmessage('������ϱ⸦ ã�� �� �����ϴ�.');
    btn_Save.Enabled := False;
    Exit;
  end
  else
  begin
    cmb_FingerPort.ItemIndex:=0;
    cmb_FingerPortChange(cmb_FingerPort);
  end;
  btnRefresh.Enabled:=true;
end;

procedure TfmFingerRegistDevice.btn_SaveClick(Sender: TObject);
var
  stFingerData : string;
  i : integer;
begin
  Save := True;
  if FingerCardNo = '' then FingerCardNo := dmDBFunction.GetFdmsCardNo;

  stFingerData := '';

  for i := 0 to MAX_FEATUREVECT_LEN - 1 do
  begin
    stFingerData := stFingerData + Ascii2Hex(gFeature1[i]);
  end;

  if DataModule1.DupCheckTB_CARDFINGER(FingerUserID) then
  begin
    dmAdoQuery.UpdateTB_CARDFINGER(FingerUserID,FingerCARDNO,stFingerData,'1','Y');
  end else
  begin
    dmAdoQuery.InsertIntoTB_CARDFINGER(FingerUserID,FingerCARDNO,stFingerData,'1','Y');
  end;
  
  Close;
end;

procedure TfmFingerRegistDevice.cmb_FingerPortChange(Sender: TObject);
begin
  GetFingerTimer.Enabled := True;
end;

procedure TfmFingerRegistDevice.GetFingerTimerTimer(Sender: TObject);
var
  L_bResult : Boolean;
  ret: integer;
begin
  L_bResult := False;
  Try
    GetFingerTimer.Enabled := False;
    if Not L_bGetFinger_1 then
    begin
      GetFinger1;
    end else if Not L_bDelay then
    begin
      L_bDelay := True;
      lb_Message.Caption := '���� ��ϱ⿡�� �հ����� ���� �ּ���...';
      lb_Message.Visible := True;
      panMessage.Visible := True;
      Delay(1000);
      lb_Message.Visible := False;
      panMessage.Visible := False;
      Delay(1000);
      //lb_Message.Caption := '���� ��ϱ⿡�� �հ����� ���� �ּ���...';
      //lb_Message.Visible := True;
      //panMessage.Visible := True;
      //MessageTimer.Enabled := True;
    end else if Not L_bGetFinger_2 then
    begin
      GetFinger2;
    end else
    begin
      ret := MatchFingerOneToOne(gFeature1,gFeature2,2);
      if ret = FPAPIERR_OK then
      begin
        L_bResult := True;
        btn_Save.enabled := True;
      end else
      begin
        Image1.Picture := nil;
        Image2.Picture := nil;
        L_bGetFinger_1 := False;
        L_bGetFinger_2 := False;
        L_bDelay := False;
        showmessage('2���� ���� ��Ī�� ���� �Ͽ����ϴ�. ó������ �ٽ� �õ� �Ͽ� �ּ���.');
      end;  
    end;
  Finally
    GetFingerTimer.Enabled := Not L_bResult;
  End;
end;

function TfmFingerRegistDevice.GetFinger1: Boolean;
var
  RawImage1 : TRawImage;
  DeviceNum,ret       : Integer;
begin
  Image1.Repaint;
  if cmb_FingerPort.Items.Count< 1 then
  begin
    exit;
  end;
  lb_Message.Caption := '������ ������ϱ⿡ ��� �ּ���.';
  lb_Message.Visible := True;
  panMessage.Visible := True;
  MessageTimer.Enabled := True;
  DeviceNum:=strToint(cmb_FingerPort.Text);
  ret:=GetFinger(DeviceNum,RawImage1,gFeature1);
  if ret=FPAPIERR_OK then
  begin
    DisplayRawImage(Image1.Canvas,RawImage1,IMG_X,IMG_Y);
    Image1.Refresh;
    L_bGetFinger_1 := True;
    panMessage.Visible := False;
    MessageTimer.Enabled := False;
  end
  else
  begin
    //DisplayRawImage(Image1.Canvas,RawImage1,IMG_X,IMG_Y);
    //Image1.Refresh;
    //ShowErrorMSG(ret);
  end;
end;

function TfmFingerRegistDevice.GetFinger2: Boolean;
var
  RawImage2 : TRawImage;
  DeviceNum,i,ret       : Integer;
begin
  if cmb_FingerPort.Items.Count< 1 then
  begin
    exit;
  end;
  lb_Message.Caption := '�ٽ� �ѹ� ������ ������ϱ⿡ ��� �ּ���.';
  lb_Message.Visible := True;
  panMessage.Visible := True;
  MessageTimer.Enabled := True;
  DeviceNum:=strToint(cmb_FingerPort.Text);
  ret:=GetFinger(DeviceNum,RawImage2,gFeature2);
  if ret=FPAPIERR_OK then
  begin
    DisplayRawImage(Image2.Canvas,RawImage2,IMG_X,IMG_Y);
    Image2.Refresh;
    L_bGetFinger_2 := True;
    panMessage.Visible := False;
    MessageTimer.Enabled := False;
  end
  else begin
    //DisplayRawImage(Image2.Canvas,RawImage2,IMG_X,IMG_Y);
    //Image2.Refresh;
    //ShowErrorMSG(ret);
  end;
end;

procedure TfmFingerRegistDevice.MessageTimerTimer(Sender: TObject);
begin
  Exit;
  lb_Message.Visible := Not lb_Message.Visible;
end;


end.
