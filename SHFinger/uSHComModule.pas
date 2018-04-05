unit uSHComModule;

interface

uses
  SysUtils, Classes,Messages,Forms,Windows,ExtCtrls,
  u_c_byte_buffer,WinSock;

  
const wm_asynch_select= wm_User;
const k_buffer_max= 4096;
      k_tcp_ip_chunk= 1500;
      MAXSOCKCOUNT = 100;
const DisConnected = 0;
      Connecting = 1;
      Connected = 2;
                                               
type
  TCARDStateChangeEvent = procedure(Sender: TObject;  aFPNo,aUSERID : integer;aNodeName,aFPSEND:string) of object;
  TFPReaderConnected = procedure(Sender: TObject;  aFPNo: integer;aNodeIP:string;aNodePort:integer;aNodeName:string;aConnected:integer) of object;
  TCardEvent = procedure(Sender: TObject;  aFPNo : integer;aNodeName,aTxRx,aData:string) of object;

  TFPUser = class(TComponent)
  private
    FFPUSERID: integer;
    FFPSEND: string;
    FFPDATA: string;
    FFPPERMIT: string;
    FFPCARD: string;
    FFPNodeNo: integer;
    FOnCARDStateChangeEvent: TCARDStateChangeEvent;
    procedure SetFPSEND(const Value: string);
  public
  published
    property FPNodeNo : integer read FFPNodeNo write FFPNodeNo;
    property FPUSERID : integer read FFPUSERID write FFPUSERID;
    property FPCARD : string read FFPCARD write FFPCARD;
    property FPDATA : string read FFPDATA write FFPDATA;
    property FPPERMIT : string read FFPPERMIT write FFPPERMIT;
    property FPSEND : string read FFPSEND write SetFPSEND;
  published
    property OnCARDStateChangeEvent: TCARDStateChangeEvent read FOnCARDStateChangeEvent write FOnCARDStateChangeEvent;
  end;

  TFPNode = class(TComponent)
  private
    FOnCARDStateChangeEvent: TCARDStateChangeEvent;
    FFPNodeName: string;
    FWinSocket: tSocket;
    FOpen: Boolean;
    FSocketConnected: integer;
    FOnConnected: TFPReaderConnected;
    FReaderType: integer;
    FOnCardEvent: TCardEvent;
    FFPDeviceID: integer;
    FFPDeviceType: integer;
    procedure CARDStateChangeEvent(Sender: TObject;  aFPNo,aUSERID : integer;aNodeName,aFPSEND:string);
    procedure SetOpen(const Value: Boolean);
    procedure SetSocketConnected(const Value: integer);
  private
    L_bCardDownLoading : Boolean;
    L_bDestroy :Boolean;
    L_bGetFDData:Boolean;
    //********************* WinSock 변수
    l_wsa_data: twsaData;
    l_c_reception_buffer: c_byte_buffer;
    L_bSocketWriting : Boolean;
    L_nFPSendCount :integer;
    L_nResult : integer;
    L_stGetFDData : string;
    ComBuff : string;
    FHandle : THandle;
    FPUserList : TStringList;
    FFPNodePort: integer;
    FFPNodeIP: string;
    FFPNodeNo: integer;
    FPSendTimer : TTimer;
    SocketCheckTimer: TTimer;  //소켓 Open 또는 Close
    function GetHandle: THandle;
    procedure FPSendTimerTimer(Sender: TObject);
    procedure SocketCheckTimerTimer(Sender: TObject);
  protected
    procedure WndProc ( var Message : TMessage ); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HandleAllocated : Boolean;
    procedure HandleNeeded;
    procedure handle_fd_close_notification(p_socket: Integer);
    procedure handle_fd_connect_notification(p_socket: Integer);
    procedure handle_fd_read_notification(p_socket: tSocket);
    procedure handle_fd_write_notification(p_socket: Integer);
    procedure handle_wm_async_select(var Msg: TMessage); message wm_asynch_select;
    procedure CommNodeTriggerAvail(Sender: TObject;SockNo:integer; Buf:String;DataLen: Integer);
    procedure CommNodeWsError(Sender: TObject;SockNo:integer;SocketError: Integer);
    function  FP_HexSendPacket(aCmd : char;aHexAddress,aHexData:string):Boolean;
    function  FP_SendPacket(aCmd : char;aAddress:char;aData:string):Boolean;
    function  PutString(aData:string;aLen:integer):Boolean;
    function CheckSHFDDataPacket(aData:String; var bData:String):string;
    function SetEncrypt(aFPReaderID,aType:integer):Boolean;
    function SHFDataPacektProcess(aHexPacket:string):Boolean;
    function ShunghunSyncTime(aFPReaderID:integer;aSendData:string) : Boolean;
    function SyncTimeSend : Boolean;
    function UserCardSend(aFPReaderID,aUserID:integer;aUserCard,aPermit:string) : integer;
    function UserFPDataSend(aFPReaderID,aUserID:integer;aFPDATA,aPermit:string) : integer;
    function UserFPDelete(aFPReaderID,aUserID:integer): integer;
    function UserAllDelete:integer;
  public
    procedure Add_FPData(aUserID:integer;aUserCard,aUserFPData,aFPPermit:string);
    function GetFPData(aUserID:string):string;
  published
    property FPNodeNo : integer read FFPNodeNo write FFPNodeNo;
    property FPNodeIP : string read FFPNodeIP write FFPNodeIP;
    property FPNodePort :integer read FFPNodePort write FFPNodePort;
    property FPNodeName :string read FFPNodeName write FFPNodeName;
    property FPDeviceID : integer read FFPDeviceID write FFPDeviceID;
    property FPDeviceType:integer read FFPDeviceType write FFPDeviceType; //0:2.0,1:2.4이상
    property Handle : THandle read GetHandle;
    property WinSocket : tSocket read FWinSocket write FWinSocket;
    property Open : Boolean read FOpen write SetOpen;
    ProPerty SocketConnected : integer read FSocketConnected Write SetSocketConnected;
    Property ReaderType : integer read FReaderType write FReaderType;    //0.등록기 타입
  published
    property OnCARDStateChangeEvent: TCARDStateChangeEvent read FOnCARDStateChangeEvent write FOnCARDStateChangeEvent;
    property OnConnected : TFPReaderConnected read FOnConnected write FOnConnected;
    property OnCardEvent : TCardEvent read FOnCardEvent write FOnCardEvent;
  end;


  TdmSHComModule = class(TDataModule)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmSHComModule: TdmSHComModule;
  SHNodeList : TStringList;

implementation
uses
  uDataModule1,
  uLomosUtil,
  uSyFpReaderFunction;
{$R *.dfm}

{ TFPNode }

procedure TFPNode.Add_FPData(aUserID:integer;aUserCard, aUserFPData,aFPPermit: string);
var
  stUserID : string;
  nIndex : integer;
  oFPUser : TFPUser;
begin
  stUserID := FillZeroNumber(aUserID,G_nFPUserIDLength);
  nIndex := FPUserList.IndexOf(stUserID);

  if nIndex < 0 then
  begin
    oFPUser := TFPUser.Create(nil);
    oFPUser.FPNodeNo := FPNodeNo;
    oFPUser.FPUSERID := aUserID;
    oFPUser.FPDATA := aUserFPData;
    oFPUser.FPCARD := aUserCard;
    oFPUser.FPPERMIT := aFPPermit;
    oFPUser.OnCARDStateChangeEvent := CARDStateChangeEvent;
    oFPUser.FPSEND := 'S';

    FPUserList.AddObject(stUserID,oFPUser);
  end else
  begin
    TFPUser(FPUserList.Objects[nIndex]).FPDATA := aUserFPData;
    TFPUser(FPUserList.Objects[nIndex]).FPCARD := aUserCard;
    TFPUser(FPUserList.Objects[nIndex]).FPPERMIT := aFPPermit;
    TFPUser(FPUserList.Objects[nIndex]).FPSEND := 'S';
  end;

end;

procedure TFPNode.CARDStateChangeEvent(Sender: TObject; aFPNo,
  aUSERID: integer;aNodeName, aFPSEND: string);
begin
  if Assigned(FOnCARDStateChangeEvent) then
  begin
    OnCARDStateChangeEvent(Self,aFPNo,aUSERID,FPNodeName,aFPSEND);
  end;

end;

function TFPNode.CheckSHFDDataPacket(aData:String; var bData:String): string;
var
  nIndex: Integer;
  Lenstr: String;
  DefinedDataLength: Integer;
  StrBuff: String;
  etxIndex: Integer;
  stPacket : string;
  stCrcData : string;
  stHexCRC : string;
  nCRC : word;
begin
  Result:= '';
  nIndex:= Pos(Ascii2Hex(STX),aData);
  if nIndex = 0 then
  begin
    result := ''; //자릿수가 작게 들어온 경우
    bData:= '';
    Exit;
  end;

  if nIndex > 1 then
  begin
    //STX 가 처음이 아니면 STX앞데이터 삭제
    Delete(aData,1,nIndex-1);
  end;

  if Length(aData) < 16 then
  begin
    result := ''; //자릿수가 작게 들어온 경우
    bData:= aData;
    Exit;
  end;
  Lenstr := Copy(aData,3,4);
  Lenstr := copy(Lenstr,3,2) + copy(Lenstr,1,2);
  //데이터 길이 위치 데이터가 숫자가 아니면...
(*  if not isDigit(Lenstr) then
  begin
    Delete(aData,1,2);       //1'st STX 삭제
    nIndex:= Pos(Ascii2Hex(STX),aData); // 다음 STX 찾기
    if nIndex = 0 then       //STX가 없으면...
    begin
      //전체 데이터 버림
      bData:= '';
    end else if nIndex > 1 then // STX가 1'st가 아니면
    begin
      Delete(aData,1,nIndex-1);//STX 앞 데이터 삭제
      bData:= aData;
    end else
    begin
      bData:= aData;
    end;
    Exit;
  end;
*)
  //패킷에 정의된 길이
  DefinedDataLength:= Hex2Dec(Lenstr);
  //패킷에 정의된 길이보다 실제 데이터가 작으면
  if Length(aData) < (DefinedDataLength * 2) then
  begin
    //실제 데이터를 다 못 받은 경우
    bData:= aData;
    Exit;
  end;
  stPacket := copy(aData,1,DefinedDataLength * 2);
  Delete(aData, 1, DefinedDataLength * 2);
  bData:= aData;
  stCrcData := copy(stPacket,1, (DefinedDataLength * 2) - 4);
  stCrcData := Hex2Ascii(stCrcData);
  nCRC := crc16_ccitt(pchar(stCrcData),DefinedDataLength - 2);
  stHexCRC := Dec2Hex64(nCRC,4);
  stHexCRC := copy(stHexCRC,3,2) + copy(stHexCRC,1,2);
  if stHexCRC = copy(stPacket,(DefinedDataLength * 2) - 4 + 1,4) then  //CRC 체크하지 말자.
  begin
    result := stPacket; //패킷이 맞는거다.
  end;
end;

procedure TFPNode.CommNodeTriggerAvail(Sender: TObject; SockNo: integer;
  Buf: String; DataLen: Integer);
var
  nIndex : integer;
  stPacket : string;
  st2 : string;
begin
  ComBuff:= ComBuff + AsciiLen2Hex(Buf,DataLen);
  nIndex:= Pos(Ascii2Hex(STX),ComBuff);
  if nIndex = 0 then
  begin
    ComBuff := ''; //STX 가 없으면 잘못된 패킷이므로 수신데이터를 버리고 빠져 나간다.
  end;

  if nIndex > 1 then
  begin
    //STX 가 처음이 아니면 STX앞데이터 삭제
    Delete(ComBuff,1,nIndex-1);
  end;

  if Length(Combuff) < 16 then Exit;  //최소 패킷 수가 8바이트 이상이다.
  
  repeat
    stPacket:= CheckSHFDDataPacket(ComBuff,st2);
    ComBuff:= st2;
    if stPacket <> '' then SHFDataPacektProcess(stPacket);
  until stPacket = '';

end;

procedure TFPNode.CommNodeWsError(Sender: TObject; SockNo,
  SocketError: Integer);
begin
  SocketError := 0;
  SocketConnected := -1;
  Open := False;
end;

constructor TFPNode.Create(AOwner: TComponent);
begin
  inherited;
  L_bDestroy := False;
  FHandle := 0;
  FPDeviceID := 1; 
  L_nFPSendCount := 0;
  ReaderType := 1; //리더 타입으로 사용하자.
  l_c_reception_buffer:= c_byte_buffer.create_byte_buffer('reception_buffer', k_buffer_max);
  FPUserList := TStringList.Create;
  SocketCheckTimer:= TTimer.Create(nil);
  SocketCheckTimer.Interval := 2000;
  SocketCheckTimer.OnTimer := SocketCheckTimerTimer;
  SocketCheckTimer.Enabled := True;

  FPSendTimer := TTimer.Create(nil);
  FPSendTimer.Interval := 1000;
  FPSendTimer.OnTimer := FPSendTimerTimer;
  FPSendTimer.Enabled := True;

  
end;

destructor TFPNode.Destroy;
var
  i : integer;
begin
  if Open then Open := False;
  SocketCheckTimer.Enabled := False;
  FPSendTimer.Enabled := False;
  L_bDestroy := True;
  Delay(1000); //정리하는 시간을 좀 갖자.
  if FPUserList.Count > 0 then
  begin
    for i := FPUserList.Count - 1 downto 0 do
      TFPUser(FPUserList.Objects[i]).Free;
  end;
  FPUserList.Clear;
  inherited;
end;

procedure TFPNode.FPSendTimerTimer(Sender: TObject);
begin
  if L_bDestroy then Exit;
  if ReaderType = 0 then       //등록기 타입으로 동작시에는 이 루틴은 타지 말자.
  begin
    FPSendTimer.Enabled := False;
    Exit;
  end;
  Try
    FPSendTimer.Enabled := False;
    L_bCardDownLoading := True;
    if G_bApplicationTerminate then Exit;
    if SocketConnected <> Connected then Exit; //접속이 안되어 있으면 전송 하지 말자.
    if FPUserList.Count < 1 then Exit;  //전송할 데이터가 없으면 빠져 나가자.

    if L_nFPSendCount > FPUserList.Count - 1 then L_nFPSendCount := 0;
    if (TFPUser(FPUserList.Objects[L_nFPSendCount]).FPPERMIT = '1') and (TFPUser(FPUserList.Objects[L_nFPSendCount]).FPSEND = 'S') then
    begin
      //여기에서 카드 정보 전송 하고 응답을 기다린다.
      if UserCardSend(FPDeviceID,TFPUser(FPUserList.Objects[L_nFPSendCount]).FPUSERID,TFPUser(FPUserList.Objects[L_nFPSendCount]).FPCARD,TFPUser(FPUserList.Objects[L_nFPSendCount]).FPPERMIT) = 1 then // 전송 성공 후 응답 받은 경우
      begin
        TFPUser(FPUserList.Objects[L_nFPSendCount]).FPSEND := 'F';
      end else
      begin
        Open := False;
        Exit;
      end;
    end;
    if (TFPUser(FPUserList.Objects[L_nFPSendCount]).FPPERMIT = '1') and (TFPUser(FPUserList.Objects[L_nFPSendCount]).FPSEND = 'F') then
    begin
      //여기에서 지문 정보 전송 하고 응답을 기다린다.
      if UserFPDataSend(FPDeviceID,TFPUser(FPUserList.Objects[L_nFPSendCount]).FPUSERID,TFPUser(FPUserList.Objects[L_nFPSendCount]).FPDATA,TFPUser(FPUserList.Objects[L_nFPSendCount]).FPPERMIT) = 1 then
      begin
        TFPUser(FPUserList.Objects[L_nFPSendCount]).FPSEND := 'Y';
      end else
      begin
        Open := False;
        Exit;
      end;
    end;
    if TFPUser(FPUserList.Objects[L_nFPSendCount]).FPPERMIT = '0' then
    begin
      //여기서 지문 삭제 정보 전송하고 응답을 기다린다.
      if UserFPDelete(FPDeviceID,TFPUser(FPUserList.Objects[L_nFPSendCount]).FPUSERID) = 1 then
      begin
        TFPUser(FPUserList.Objects[L_nFPSendCount]).FPSEND := 'Y';
      end else
      begin
        Open := False;
        Exit;
      end;

    end;
    if TFPUser(FPUserList.Objects[L_nFPSendCount]).FPSEND = 'Y' then  //전송 성공 했으면 삭제 하자.
    begin
      TFPUser(FPUserList.Objects[L_nFPSendCount]).Free;
      FPUserList.Delete(L_nFPSendCount);
    end else L_nFPSendCount := L_nFPSendCount + 1;
  Finally
    L_bCardDownLoading := False;
    FPSendTimer.Enabled := Not G_bApplicationTerminate;
  End;
end;

function TFPNode.FP_HexSendPacket(aCmd: char;
  aHexAddress,aHexData: string): Boolean;
var
  stPacket : string;
  nLength : integer;
  stHexLen : string;
  nCRC : word;
  stHexCRC : string;
begin
  stPacket := aHexAddress + Ascii2Hex(aCmd) + aHexData;
  nLength := Length(stPacket) + (6 * 2);
  nLength := nLength div 2;
  stHexLen := Dec2Hex64(nLength,4);
  stHexLen := copy(stHexLen,3,2) + copy(stHexLen,1,2); //앞 뒤를 바꾼다.
  stPacket := STX + Hex2Ascii(stHexLen) + Hex2Ascii(stPacket) + ETX;
  nCRC := crc16_ccitt(pchar(stPacket),nLength - 2);
  stHexCRC := Dec2Hex64(nCRC,4);
  stHexCRC := copy(stHexCRC,3,2) + copy(stHexCRC,1,2); //앞 뒤를 바꾼다.
  stPacket := stPacket + Hex2Ascii(stHexCRC);

  PutString(stPacket,nLength);
  if Assigned(FOnCARDEvent) then
  begin
    OnCARDEvent(Self,FPNodeNo,FPNodeName,'TX',Ascii2Hex(stPacket));
  end;
end;

function TFPNode.FP_SendPacket(aCmd, aAddress: char;
  aData: string): Boolean;
var
  stPacket : string;
  nLength : integer;
  stHexLen : string;
  nCRC : word;
  stHexCRC : string;
begin
  stPacket := aAddress + aCmd + aData;
  nLength := Length(stPacket) + 6;
  stHexLen := Dec2Hex64(nLength,4);
  stHexLen := copy(stHexLen,3,2) + copy(stHexLen,1,2); //앞 뒤를 바꾼다.
  stPacket := STX + Hex2Ascii(stHexLen) + stPacket + ETX;
  nCRC := crc16_ccitt(pchar(stPacket),nLength - 2);
  stHexCRC := Dec2Hex64(nCRC,4);
  stHexCRC := copy(stHexCRC,3,2) + copy(stHexCRC,1,2); //앞 뒤를 바꾼다.
  stPacket := stPacket + Hex2Ascii(stHexCRC);

  PutString(stPacket,nLength);
end;

function TFPNode.GetFPData(aUserID: string): string;
var
  stUserID : string;
  Tick: DWORD;
  NowTick: DWORD;
  stAddr : string;
begin
  if FPDeviceType = 1 then //2.4 버젼은 SetEncrpt 설정해 주자.
  begin
//    SetEncrypt(FPDeviceID,2);  //설정 되어 있는데로 놔두자. 1.암호화 해제,2.암호화
  end;
  if FPDeviceID < 9 then stAddr := Dec2Hex(FPDeviceID - 1,2)
  else
  begin
    stAddr := Dec2Hex(FPDeviceID - 1,4);
    stAddr := copy(stAddr,3,2) + copy(stAddr,1,2);
  end;

  stUserID := FillZeroStrNum(aUserID,G_nFPUserIDLength);
  stUserID := FillZeroStrNum(stUserID,10,False);
  stUserID := Ascii2Hex(stUserID);
  L_stGetFDData := '';
  Try
    L_nResult := 0;
    L_bGetFDData := True;
    FP_HexSendPacket(cmdFPGetFP,stAddr,stUserID);
    Tick := GetTickCount + DWORD(2000);
    while (L_nResult = 0) do
    begin
      NowTick := GetTickCount;
      if Tick < NowTick then break;
      Application.ProcessMessages;
    end;
  Finally
    L_bGetFDData := False;
    result := L_stGetFDData;
  End;
end;

function TFPNode.GetHandle: THandle;
begin
  HandleNeeded;
  Result := FHandle;
end;

function TFPNode.HandleAllocated: Boolean;
begin
  Result := ( FHandle <> 0 );
end;

procedure TFPNode.HandleNeeded;
begin
  if not HandleAllocated
   then FHandle := AllocateHWND ( WndProc ); 
end;

procedure TFPNode.handle_fd_close_notification(p_socket: Integer);
var
  l_status: Integer;
  l_linger: TLinger;
  l_absolute_linger: array[0..3] of char absolute l_linger;
begin
    if WSAIsBlocking
          then
            begin
              WSACancelBlockingCall;
            end;
    Open := False;
end;

procedure TFPNode.handle_fd_connect_notification(p_socket: Integer);
begin
  SocketConnected:= Connected;
end;

procedure TFPNode.handle_fd_read_notification(p_socket: tSocket);
var
  l_remaining: Integer;
  l_pt_start_reception: Pointer;
  l_packet_bytes: Integer;
  l_eol_position: Integer;
  stTemp : String;
begin
  if l_c_reception_buffer = nil then Exit;

  with l_c_reception_buffer do
  begin
    l_remaining:= m_buffer_size- m_write_index;

    // -- if not at least a tcp-ip chunk, increase the room
    if l_remaining < k_tcp_ip_chunk then
    begin
      // -- reallocate
      double_the_capacity;
      l_remaining:= m_buffer_size- m_write_index;
    end;

    // -- add the received data to the current buffer
    l_pt_start_reception:= @ m_oa_byte_buffer[m_write_index];

    // -- get the data from the client socket
    //LogSave(ExeFolder + '\..\log\log'+ ConnectIP +'.log','RecvStart ');
    l_packet_bytes:= Recv(WinSocket, l_pt_start_reception^, l_remaining, 0);
    if l_packet_bytes < 0 then
    begin
      LogSave(ExeFolder + '\..\log\log'+ FormatDateTIme('yyyymmdd',Now)+'.log','Error connect(Recv) '+ FPNodeIP);
      CommNodeWsError(Self,p_socket,WSAGetLastError);
    end else
    begin
      m_write_index:= m_write_index+ l_packet_bytes;
      stTemp := ByteCopy(l_pt_start_reception,l_packet_bytes);
      //LogSave(ExeFolder + '\..\log\log'+ ConnectIP +'.log',stTemp);
      CommNodeTriggerAvail(Self,p_socket,stTemp,l_packet_bytes);
    end;
  end; // with g_c_reception_buffer

end;

procedure TFPNode.handle_fd_write_notification(p_socket: Integer);
begin
  L_bSocketWriting := False; //전송 완료 소켓 버퍼 Write 공간 생김
end;

procedure TFPNode.handle_wm_async_select(var Msg: TMessage);
var
  l_param: Integer;
  l_error, l_notification: Integer;
  l_socket_handle: Integer;
begin
  if L_bDestroy then Exit;
    l_param:= Msg.lParam;
    l_socket_handle:= Msg.wParam;

    // -- extract the error and the notification code from l_param
    l_error:= wsaGetSelectError(l_param);
    l_notification:= wsaGetSelectEvent(l_param);

    if l_error <= wsaBaseErr then
    begin
        case l_notification of
          FD_CONNECT: handle_fd_connect_notification(l_socket_handle);
          FD_ACCEPT: {display_bug_stop('no_client_accept')} ;
          FD_WRITE: handle_fd_write_notification(l_socket_handle);
          FD_READ: handle_fd_read_notification(l_socket_handle);
          FD_CLOSE:
          begin
            LogSave(ExeFolder + '\..\log\log'+ FormatDateTIme('yyyymmdd',Now)+'.log','Error connect(fd_close_EVENT) -' + FPNodeIP);
            handle_fd_close_notification(l_socket_handle);
          end;
        end // case
    end else
    begin
      if l_notification= FD_CLOSE then
      begin
        LogSave(ExeFolder + '\..\log\log'+ FormatDateTIme('yyyymmdd',Now)+'.log','Error connect(fd_close_ERR) -' + FPNodeIP);
         handle_fd_close_notification(l_socket_handle);
      end
      else
      begin
         LogSave(ExeFolder + '\..\log\log'+ FormatDateTIme('yyyymmdd',Now)+'.log','Error connect(SELECT) -'+ inttostr(l_notification) + '-' + FPNodeIP);
         handle_fd_close_notification(l_socket_handle);
      end;
    end;
end;

function TFPNode.PutString(aData: string;aLen:integer): Boolean;
var
  l_result: Integer;
  buf: array of Byte;
  i : integer;
begin
  Try
    result := False;

    if WinSocket = INVALID_SOCKET then Exit;
    if Not Open then Exit;

    While L_bSocketWriting do
    begin
      if Not Open then Exit;
      Application.ProcessMessages;
      sleep(1);
    end;//전송 중에는 보내지 말자.  => 전송 완료 메시지 이벤트가 발생 안되어 무용지물


    SetLength(buf, aLen);
    for i := 1 to aLen do
    begin
      buf[i-1] := ord(aData[i]);
    end;

    Try
      l_result:= Send(WinSocket,buf[0], aLen, 0);

      if l_result < 0 then
      begin
        if l_result = wsaEWouldBlock  then
        begin
          L_bSocketWriting := True;  //Socket에 Full 나면 Write
        end else
        begin
          LogSave(ExeFolder + '\..\log\log'+ FormatDateTIme('yyyymmdd',Now)+'.log','Error connect(Send) -'+ inttostr(l_result) + '-' + FPNodeIP);
          CommNodeWsError(Self,WinSocket,WSAGetLastError);
        end;
      end;
    Except
      Exit;
    End;
    result := True;
  Finally
//    FTCSDeviceSender.Leave;
  End;
end;

function TFPNode.SetEncrypt(aFPReaderID, aType: integer): Boolean;
var
  stData : string;
  stAddr : string;
  Tick: DWORD;
  NowTick: DWORD;
begin
  result := False;
  if aFPReaderID < 9 then stAddr := Dec2Hex(aFPReaderID - 1,2)
  else
  begin
    stAddr := Dec2Hex(aFPReaderID - 1,4);
    stAddr := copy(stAddr,3,2) + copy(stAddr,1,2);
  end;
  stData := FillZeroNumber(aType,3);
  stData := Ascii2Hex(stData);
  Try
    L_nResult := 0;
    FP_HexSendPacket(cmdFPSetEncrypt,stAddr,stData);
    Tick := GetTickCount + DWORD(3000);
    while (L_nResult = 0) do
    begin
      NowTick := GetTickCount;
      if Tick < NowTick then break;
      Application.ProcessMessages;
    end;
  Finally
    if L_nResult = 1 then result := True;
  End;

end;

procedure TFPNode.SetOpen(const Value: Boolean);
var
  l_result : Integer;
  l_error: Integer;
  l_socket_address_in: tSockAddrIn;
  l_ip_z: array[0..255] of char;
  rset: TFDSet;
  t: TTimeVal;
  rslt: integer;
  stConnectIP : string;
begin
  if FOpen = Value then Exit;
  FOpen := Value;
  stConnectIP := FPNodeIP;
  if Value then
  begin
    SocketConnected:= Connecting;   //Connecting
    l_result := wsaStartup(MAKEWORD(1, 1), l_wsa_data);
    if l_result <> 0 then
    begin
      Open := False;
      Exit;  //소켓생성 실패 시에 Open False
    end;
    WinSocket:= Socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
    if WinSocket = INVALID_SOCKET then
    begin
      Open := False;
      Exit;  //소켓생성 실패 시에 Open False
    end;
    l_result:= wsaAsyncSelect(WinSocket, Handle,
        wm_asynch_select,
        FD_CONNECT+ FD_READ+ FD_WRITE+ FD_CLOSE);

    FillChar(l_socket_address_in, sizeof(l_socket_address_in), 0);
    with l_socket_address_in do
    begin
      sin_family:= pf_Inet;
      // -- the requested service
      sin_port:= hToNs(FPNodePort);
      // -- the server IP address
      if Not IsIPTypeCheck(FPNodeIP) then
      begin
        stConnectIP := GetIpFromDomain(FPNodeIP);
      end;
      StrPCopy(l_ip_z, stConnectIP);
      sin_addr.s_Addr:= inet_addr(PAnsichar(AnsiString(l_ip_z)));
    end; // with m_socket_address_in
    l_result:= Connect(WinSocket, l_socket_address_in,
        sizeof(l_socket_address_in));
    if l_result<> 0 then
    begin
      l_error:= WSAGetLastError;
      if l_error <> wsaEWouldBlock then
      begin
        //LogSave(ExeFolder + '\..\log\Connectlog'+ FormatDateTIme('yyyymmdd',Now)+'.log',ConnectIP + 'OpenError' + inttostr(WinSocket));
        Open := False;
        Exit;  //소켓생성 실패 시에 Open False
      end else
      begin
      end;
      //LogSave(ExeFolder + '\..\log\Connectlog'+ FormatDateTIme('yyyymmdd',Now)+'.log',ConnectIP + 'Open' + inttostr(WinSocket));
    end;
  end else
  begin
    SocketConnected:= DisConnected;
    if WinSocket <> INVALID_SOCKET then
    begin
      //LogSave(ExeFolder + '\..\log\Connectlog'+ FormatDateTIme('yyyymmdd',Now)+'.log',ConnectIP + 'Close' + inttostr(WinSocket));
      shutdown(WinSocket,SD_BOTH);
      l_result:= CloseSocket(WinSocket);
      if l_result = 0 then
      begin
        WinSocket:= INVALID_SOCKET;
      end else
      begin
        WinSocket:= INVALID_SOCKET;
      end;
      if WSAISBlocking then WSACancelBlockingCall;  //--추가 20141215 충남대에서 에러 때문에 혹시나 해서...ㅠ.ㅠ
      WSACleanup;
    end;
  end;
end;

procedure TFPNode.SetSocketConnected(const Value: integer);
begin
  if FSocketConnected = Value then Exit;
  FSocketConnected := Value;
  if Assigned(FOnConnected) then
  begin
    OnConnected(Self,FPNodeNo,FPNodeIP,FPNodePort,FPNodeName,Value);
  end;
end;

function TFPNode.SHFDataPacektProcess(aHexPacket: string): Boolean;
var
  stResult : string;
begin
  if Assigned(FOnCARDEvent) then
  begin
    OnCARDEvent(Self,FPNodeNo,FPNodeName,'RX',aHexPacket);
  end;

  stResult := copy(aHexPacket,4*2 + 1,2);
  //if stResult = '00' then L_nResult:= 1
  //else L_nResult := -1;  //에러 발생한거다
  L_nResult:= 1;   //에러 응답시 체크 하지 말자 2016-07-26

  if L_bGetFDData then //지문 조회 중인 거다... 여기에서 지문 데이터 수신 하자.
  begin
    L_bGetFDData := False;
    if FPDeviceID < 9 then Delete(aHexPacket,1,4*2 + 2)
    else Delete(aHexPacket,1,4*2 + 2 + 2);
    L_stGetFDData := copy(aHexPacket,1,Length(aHexPacket) - 6);
  end;
end;

function TFPNode.ShunghunSyncTime(aFPReaderID: integer;
  aSendData: string): Boolean;
var
  stAddr : string;
  stSendData : string;
  Tick: DWORD;
  NowTick: DWORD;
begin
(*  Dec(L_nConnectedTime);
  if Not Connected then
  begin
    if L_nConnectedTime > 0 then Exit; //소켓 Close 후에 3초 간은 들어 오지 말자.
    if SocketOpen then
    begin
      SocketOpen := False;
      Exit;
    end;
    L_nConnectedTime := 6;
    SocketOpen := True;
    Exit; //접속 안되어 있으면 전송 하러 가지 말자.
  end;

  if L_bCardDownLoading then Exit;
  if L_bModuleDestory then Exit;
  Try
    L_bCardDownLoading := True;
    if aFPReaderID < 9 then stAddr := Dec2Hex(aFPReaderID - 1,2)
    else
    begin
      stAddr := Dec2Hex(aFPReaderID - 1,4);
      stAddr := copy(stAddr,3,2) + copy(stAddr,1,2);
    end;
    stSendData := Ascii2Hex(aSendData);
    L_nResult := 0;

    L_cFPCmd := cmdFPTimeSet;
    FP_HexSendPacket(cmdFPTimeSet,stAddr,stSendData);
    Tick := GetTickCount + DWORD(2000);
    while (L_nResult = 0) do
    begin
      NowTick := GetTickCount;
      if Tick < NowTick then break;
      Application.ProcessMessages;
    end;
    if L_nResult = 1 then SyncTime := now; //현재시간으로 시간 동기화를 했다.
    SocketOpen := False;
  Finally
    L_bCardDownLoading := False;
  End;
*)
end;

procedure TFPNode.SocketCheckTimerTimer(Sender: TObject);
begin
  if L_bDestroy then Exit;
  if G_bApplicationTerminate then Exit;
  if ReaderType = 0 then       //등록기 타입으로 동작시에는 이 루틴은 타지 말자.
  begin
    SocketCheckTimer.Enabled := False;
    Exit;
  end;
  //SocketCheckTimer.Interval := 10000; //10초 주기별로
  if FPUserList.Count > 0 then
  begin
    //전송할 데이터가 있으면...
    if SocketConnected = Connected then Exit; //접속 성공 상태이면 빠져 나간다.
    if Open then Open := False
    else Open := True;
  end else
  begin
    //없으면 소켓 Close
    Open := False;
  end;
end;

function TFPNode.SyncTimeSend: Boolean;
var
  stSendData:string;
begin
(*  if SyncTime > Now - 1 then Exit; //시간 전송 한지 하루가 안 지났으면 전송하지 말자
  if L_bCardDownLoading then Exit; //현재 카드 데이터 전송중이면 시간 전송하지 말자.

  stSendData := inttostr(DayofWeek(now)) + copy(formatdateTime('yyyymmddhhnnss',now),3,12);
  ShunghunSyncTime(FPReaderID,stSendData);
*)
end;

function TFPNode.UserAllDelete: integer;
var
  stUserID : string;
  Tick: DWORD;
  NowTick: DWORD;
  stSendData : string;
  stAddr : string;
begin
  if FPDeviceID < 9 then stAddr := Dec2Hex(FPDeviceID - 1,2)
  else
  begin
    stAddr := Dec2Hex(FPDeviceID - 1,4);
    stAddr := copy(stAddr,3,2) + copy(stAddr,1,2);
  end;

  Try
    L_nResult := 0;
    FP_HexSendPacket(cmdFPAllDelete,stAddr,'');
    Tick := GetTickCount + DWORD(2000);
    while (L_nResult = 0) do
    begin
      NowTick := GetTickCount;
      if Tick < NowTick then break;
      Application.ProcessMessages;
    end;
  Finally
    result := L_nResult;
  End;
end;

function TFPNode.UserCardSend(aFPReaderID,aUserID: integer;
  aUserCard,aPermit: string): integer;
var
  stUserID : string;
  stCardNo : string;
  stPassword : string;
  stMode : string;
  stTimeTable : string;
  stAdmin : string;
  stGrade : string;
  stDate : string;
  Tick: DWORD;
  NowTick: DWORD;
  stSendData : string;
  stAddr : string;
begin
  if FPDeviceType = 1 then //2.4 버젼은 SetEncrpt 설정해 주자.
  begin
//    SetEncrypt(aFPReaderID,2);  //설정되어 있는데로 놔두자. 1.암호화 해제,2.암호화
  end;
  if aFPReaderID < 9 then stAddr := Dec2Hex(aFPReaderID - 1,2)
  else
  begin
    stAddr := Dec2Hex(aFPReaderID - 1,4);
    stAddr := copy(stAddr,3,2) + copy(stAddr,1,2);
  end;
  stUserID := FillZeroNumber(aUserID,G_nFPUserIDLength);
  stUserID := FillZeroStrNum(stUserID,10,False);
  stUserID := Ascii2Hex(stUserID);
  stPassword := FillCharString('','F',10 * 2);
  stMode := '33333330';
  stMode := Ascii2Hex(stMode);
  stTimeTable := FillCharString('','F',168 * 2);
  stAdmin := '30';
  stGrade := '01';
  stDate := FillCharString('','F',20 * 2);

  if CARDLENGTHTYPE = 0 then
  begin
    if IsNumericCardNo then stCardNo:= Dec2Hex64(strtoint64(aUserCard),8)
    else stCardNo:= aUserCard;
  end else if CARDLENGTHTYPE = 1 then    //16byte
  begin
    if copy(aUserCard,15,2) = '**' then stCardNo:= copy(aUserCard,1,8)
    else
    begin
      stCardNo := copy(aUserCard,1,10) + copy(aUserCard,15,2);
      stCardNo := Ascii2Hex(stCardNo);
    end;
  end else if CARDLENGTHTYPE = 2 then    //KT사옥(현재는 지원하지 않는다.)
  begin
  end;
  if FPDeviceType = 1 then stCardNo := FillCharString(stCardNo,'F',72)
  else stCardNo := FillCharString(stCardNo,'F',24); //24자리 카드 번호를 만든다.


  stSendData := stUserID + stCardNo + stPassword + stMode + stTimeTable + stAdmin + stGrade + stDate;
  
  Try
    L_nResult := 0;
    FP_HexSendPacket(cmdFPCard,stAddr,stSendData);
    Tick := GetTickCount + DWORD(2000);
    while (L_nResult = 0) do
    begin
      NowTick := GetTickCount;
      if Tick < NowTick then break;
      Application.ProcessMessages;
    end;
  Finally
    result := L_nResult;
  End;
end;

function TFPNode.UserFPDataSend(aFPReaderID,aUserID: integer;
  aFPDATA,aPermit: string): integer;
var
  stUserID : string;
  Tick: DWORD;
  NowTick: DWORD;
  stSendData : string;
  stAddr : string;
begin
  if aFPReaderID < 9 then stAddr := Dec2Hex(aFPReaderID - 1,2)
  else
  begin
    stAddr := Dec2Hex(aFPReaderID - 1,4);
    stAddr := copy(stAddr,3,2) + copy(stAddr,1,2);
  end;
  stUserID := FillZeroNumber(aUserID,G_nFPUserIDLength);
  stUserID := FillZeroStrNum(stUserID,10,False);
  stUserID := Ascii2Hex(stUserID);

  stSendData := stUserID + aFPDATA;

  Try
    L_nResult := 0;
    FP_HexSendPacket(cmdFPData,stAddr,stSendData);
    Tick := GetTickCount + DWORD(2000);
    while (L_nResult = 0) do
    begin
      NowTick := GetTickCount;
      if Tick < NowTick then break;
      Application.ProcessMessages;
    end;
  Finally
    result := L_nResult;
  End;
end;

function TFPNode.UserFPDelete(aFPReaderID, aUserID: integer): integer;
var
  stUserID : string;
  Tick: DWORD;
  NowTick: DWORD;
  stSendData : string;
  stAddr : string;
begin
  if aFPReaderID < 9 then stAddr := Dec2Hex(aFPReaderID - 1,2)
  else
  begin
    stAddr := Dec2Hex(aFPReaderID - 1,4);
    stAddr := copy(stAddr,3,2) + copy(stAddr,1,2);
  end;
  stUserID := FillZeroNumber(aUserID,G_nFPUserIDLength);
  stUserID := FillZeroStrNum(stUserID,10,False);
  stUserID := Ascii2Hex(stUserID);

  stSendData := stUserID;

  Try
    L_nResult := 0;
    FP_HexSendPacket(cmdFPDelete,stAddr,stSendData);
    Tick := GetTickCount + DWORD(2000);
    while (L_nResult = 0) do
    begin
      NowTick := GetTickCount;
      if Tick < NowTick then break;
      Application.ProcessMessages;
    end;
  Finally
    result := L_nResult;
  End;
end;

procedure TFPNode.WndProc(var Message: TMessage);
begin
  if L_bDestroy then Exit;
  Try
    Dispatch ( Message );
  Except
    Exit;
  End;
end;

{ TFPUser }

procedure TFPUser.SetFPSEND(const Value: string);
begin
  if FFPSEND = Value then Exit;
  FFPSEND := Value;
  //여기에서 이벤트 발생 시켜서 데이터베이스
  if Assigned(FOnCARDStateChangeEvent) then
  begin
    OnCARDStateChangeEvent(Self,FPNodeNo,FPUSERID,'',Value);
  end;
end;

end.
