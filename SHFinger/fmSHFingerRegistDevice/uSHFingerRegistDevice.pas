unit uSHFingerRegistDevice;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, DB, ADODB;

type
  TfmSHFingerRegistDevice = class(TForm)
    Panel1: TPanel;
    lb_Message: TLabel;
    Panel2: TPanel;
    btn_GetFPData: TSpeedButton;
    btn_Save: TSpeedButton;
    btn_Cancel: TSpeedButton;
    TempADOQuery: TADOQuery;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btn_SaveClick(Sender: TObject);
    procedure btn_CancelClick(Sender: TObject);
    procedure btn_GetFPDataClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    L_stDeviceMessage : string;
    L_stFingerData : string;
    FSave: Boolean;
    FFingerCardNo: string;
    FFingerUserID: string;
    FFPDeviceID: integer;
    FFPDeviceVer: integer;
    { Private declarations }
  public
    { Public declarations }
    property FingerUserID : string read FFingerUserID write FFingerUserID;
    property FingerCardNo : string read FFingerCardNo write FFingerCardNo;
    property FPDeviceID : integer read FFPDeviceID write FFPDeviceID;
    property FPDeviceVer : integer read FFPDeviceVer write FFPDeviceVer; //0:2.0���� 1:2.4�̻�
    property Save : Boolean read FSave write FSave;
  end;

var
  fmSHFingerRegistDevice: TfmSHFingerRegistDevice;

implementation
uses
  uDataModule1,
  uSHComModule,
  udmAdoQuery,
  uLomosUtil, uDBFunction;
  
{$R *.dfm}

procedure TfmSHFingerRegistDevice.FormCreate(Sender: TObject);
begin
  L_stDeviceMessage := '$DeviceIP ���� $UserID ������ ���� ����� �������� ��ư�� Ŭ�� �Ͽ� �ּ���.';
  FPDeviceID := 1;
end;

procedure TfmSHFingerRegistDevice.FormActivate(Sender: TObject);
var
  stMessage : string;
begin
  stMessage := StringReplace(L_stDeviceMessage,'$DeviceIP',G_stFingerReaderIP,[rfReplaceAll]);
  stMessage := StringReplace(stMessage,'$UserID',FillZeroStrNum(FingerUserID,G_nFPUserIDLength),[rfReplaceAll]);
  lb_Message.Caption := stMessage;
  
end;

procedure TfmSHFingerRegistDevice.btn_SaveClick(Sender: TObject);
begin
  Save := True;
  if FingerCardNo = '' then FingerCardNo := dmDBFunction.GetFdmsCardNo;

  if DataModule1.DupCheckTB_CARDFINGER(FingerUserID) then
  begin
    dmAdoQuery.UpdateTB_CARDFINGER(FingerUserID,FingerCARDNO,L_stFingerData,'1','Y');
  end else
  begin
    dmAdoQuery.InsertIntoTB_CARDFINGER(FingerUserID,FingerCARDNO,L_stFingerData,'1','Y');
  end;
  Close;
end;

procedure TfmSHFingerRegistDevice.btn_CancelClick(Sender: TObject);
begin
  Save := False;
  Close;
end;

procedure TfmSHFingerRegistDevice.btn_GetFPDataClick(Sender: TObject);
var
  oFPNode : TFPNode;
  Tick: DWORD;
  NowTick: DWORD;
begin
  Try
    btn_GetFPData.Enabled := False;
    btn_Save.Enabled := False;
    oFPNode := TFPNode.Create(nil);
    oFPNode.ReaderType := 0; //��ϱ� Ÿ������ ��������.
    oFPNode.FPNodeNo := 1;
    oFPNode.FPNodeIP := G_stFingerReaderIP;
    oFPNode.FPNodePort := 7005;
    oFPNode.FPDeviceID := FPDeviceID;
    oFPNode.FPNodeName := '��ϱ�';
    oFPNode.FPDeviceType := FPDeviceVer;
    
    if oFPNode.Open then oFPNode.Open := False;
    oFPNode.Open := True;

    Tick := GetTickCount + DWORD(3000);    //3�� ���� ���� ���� ���ϸ� ���� �����̴�.

    While oFPNode.SocketConnected <> Connected do
    begin
      NowTick := GetTickCount;
      if Tick < NowTick then break;
      Application.ProcessMessages;
    end;

    if oFPNode.SocketConnected <> Connected then
    begin
      showmessage('�������� ���ӿ� ���� �Ͽ����ϴ�.');
      Exit;
    end;

    L_stFingerData := oFPNode.GetFPData(FingerUserID);
    if L_stFingerData = '' then
    begin
      showmessage('�ش� ������ ������ �������� ���߽��ϴ�..');
      Exit;
    end;
    btn_Save.Enabled := True;
    oFPNode.Open := False;

  Finally
    oFPNode.Destroy;
    btn_GetFPData.Enabled := True;
  End;
end;


procedure TfmSHFingerRegistDevice.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (L_stFingerData <> '') and (Not Save) then
  begin
    if Application.MessageBox(PChar('���������͸� ���� ���� �ʾҽ��ϴ�.���� �Ͻðڽ��ϱ�?'),'���',MB_OKCANCEL) = ID_CANCEL  then
    begin
      CanClose := False;
    end;
  end;

end;

end.