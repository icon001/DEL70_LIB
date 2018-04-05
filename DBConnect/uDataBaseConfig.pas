unit uDataBaseConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, StdCtrls,iniFiles,DB, ComCtrls,ADOdb,ActiveX,
  FolderDialog;

type
  TDataBaseConfig = class(TComponent)
  private
    FDBConnected: Boolean;
    FCancel: Boolean;
    class function FindSelf:TComponent;
    procedure SetDBConnected(const Value: Boolean);
    procedure SetCancel(const Value: Boolean);
    { Private declarations }
  public
    procedure GuestTableVersionCheck ;
    function GetGuestTableVersion:integer;
    function GuestTable001VersionMake:Boolean;
    function GuestTable002VersionMake:Boolean;
    function GuestTable003VersionMake:Boolean;
  public
    procedure FingerTableVersionCheck;
    function FingerTable001VersionMake : Boolean;
    function FingerTable002VersionMake : Boolean;
    function FingerTable003VersionMake : Boolean;
    function GetFingerTableVersion : integer;
    Function CreateTB_COMPANY_PrimaryKey:Boolean;
    Function CreateTB_LOCATION_PrimaryKey:Boolean;
    function DropTB_COMPANY_PrimaryKey:Boolean;
    function DropTB_LOCATION_PrimaryKey:Boolean;
  public
    { Public declarations }
    Procedure ShowDataBaseConfig;
    Function DataBaseConnect(aMessage:Boolean=True):Boolean;
    procedure TableVersionCheck;
    function Table1VersionMake: Boolean;
    function Table2VersionMake: Boolean;
    function Table3VersionMake: Boolean;
    function Table4VersionMake: Boolean;
    function Table5VersionMake: Boolean;
    function Table6VersionMake: Boolean;
    function Table7VersionMake: Boolean;
    function Table8VersionMake: Boolean;
    function Table9VersionMake: Boolean;
    function Table10VersionMake: Boolean;
    function Table11VersionMake: Boolean;
    function Table12VersionMake: Boolean;
    function Table13VersionMake: Boolean;
    function Table14VersionMake: Boolean;
    function Table15VersionMake: Boolean;
    function Table16VersionMake: Boolean;
    function Table17VersionMake: Boolean;
    function Table18VersionMake: Boolean;
    function Table19VersionMake: Boolean;
    function Table20VersionMake: Boolean;
    function Table21VersionMake: Boolean;
    function Table22VersionMake: Boolean;
    function Table23VersionMake: Boolean;
    function Table24VersionMake: Boolean;
    function Table25VersionMake: Boolean;
    function Table26VersionMake: Boolean;
    function Table27VersionMake: Boolean;
    function Table28VersionMake: Boolean;
    function Table29VersionMake: Boolean;
    function Table30VersionMake: Boolean;
    function Table31VersionMake: Boolean;
    function Table32VersionMake: Boolean;
    function Table33VersionMake: Boolean;
    function Table34VersionMake: Boolean;
    function Table35VersionMake: Boolean;
    function Table36VersionMake: Boolean;
    function Table37VersionMake: Boolean;
    function Table38VersionMake: Boolean;
    function Table39VersionMake: Boolean;
    function Table40VersionMake: Boolean;
    function Table41VersionMake: Boolean;
    function Table42VersionMake: Boolean;
    function Table43VersionMake: Boolean;
    function Table44VersionMake: Boolean;
    function Table45VersionMake: Boolean;
    function Table46VersionMake: Boolean;
    function Table47VersionMake: Boolean;
    function Table48VersionMake: Boolean;
    function Table49VersionMake: Boolean;
    function Table50VersionMake: Boolean;
    function Table51VersionMake: Boolean;
    function Table52VersionMake: Boolean;
    function Table53VersionMake: Boolean;
    function Table54VersionMake: Boolean;
    function Table55VersionMake: Boolean;
    function Table56VersionMake: Boolean;
    function Table57VersionMake: Boolean;
    function Table58VersionMake: Boolean;
    function Table59VersionMake: Boolean;
    function Table60VersionMake: Boolean;
    function Table61VersionMake: Boolean;
    function Table62VersionMake: Boolean;
    function Table63VersionMake: Boolean;
    function Table64VersionMake: Boolean;
    function Table65VersionMake: Boolean;
    function Table66VersionMake: Boolean;
    function Table67VersionMake: Boolean;
    function Table68VersionMake: Boolean;
    function Table69VersionMake: Boolean;
    function Table70VersionMake: Boolean;
    function Table71VersionMake: Boolean;
    function Table72VersionMake: Boolean;
    function Table73VersionMake: Boolean;
    function Table74VersionMake: Boolean;
    function Table75VersionMake: Boolean;
    function Table76VersionMake: Boolean;
    function Table77VersionMake: Boolean;
    function Table78VersionMake: Boolean;
    function Table79VersionMake: Boolean;
    function Table80VersionMake: Boolean;
    function Table81VersionMake: Boolean;
    function Table82VersionMake: Boolean;
    function Table83VersionMake: Boolean;
    function Table84VersionMake: Boolean;
    function Table85VersionMake: Boolean;  //KTT관제 연동
    function Table86VersionMake: Boolean;
    function Table87VersionMake: Boolean;
    function Table88VersionMake: Boolean;
    function Table89VersionMake: Boolean;
    function Table90VersionMake: Boolean;  //KTT관제 연동
    function Table91VersionMake: Boolean;  //KTT관제 연동
    function Table92VersionMake: Boolean;  //KTT관제 연동
    function Table93VersionMake: Boolean;
    function Table94VersionMake: Boolean;
    function Table95VersionMake: Boolean;
    function Table96VersionMake: Boolean;
    function Table97VersionMake: Boolean;
    function Table98VersionMake: Boolean;
    function Table99VersionMake: Boolean;
    function Table100VersionMake: Boolean;
    function Table101VersionMake: Boolean;
    function Table102VersionMake: Boolean;
    function Table103VersionMake: Boolean;
    function Table104VersionMake: Boolean;
    function Table105VersionMake: Boolean;
    function Table106VersionMake: Boolean;
    function Table107VersionMake: Boolean;
    function Table108VersionMake: Boolean;
    function Table109VersionMake: Boolean;
    function Table110VersionMake: Boolean;
    function Table111VersionMake: Boolean;
    function Table112VersionMake: Boolean;
    function Table113VersionMake: Boolean;
    function Table114VersionMake: Boolean;
    function Table115VersionMake: Boolean;
    function Table116VersionMake: Boolean;
    function Table117VersionMake: Boolean;
    function Table118VersionMake: Boolean;
    function Table119VersionMake: Boolean;
    function Table120VersionMake: Boolean;
    function Table121VersionMake: Boolean;
    function Table122VersionMake: Boolean;
    function Table123VersionMake: Boolean;
    function Table124VersionMake: Boolean;
    function Table125VersionMake: Boolean;
    function Table126VersionMake: Boolean;
    function Table127VersionMake: Boolean;
    function Table128VersionMake: Boolean;
    function Table129VersionMake: Boolean;
    function Table130VersionMake: Boolean;
    function Table131VersionMake: Boolean;
    function Table132VersionMake: Boolean;
    function Table133VersionMake: Boolean;
    function Table134VersionMake: Boolean;
    function Table135VersionMake: Boolean;
    function Table136VersionMake: Boolean;
    function Table137VersionMake: Boolean;
    function Table138VersionMake: Boolean;
    function Table139VersionMake: Boolean;
    function Table140VersionMake: Boolean;
    function Table141VersionMake: Boolean;
    function Table142VersionMake: Boolean;
    function Table143VersionMake: Boolean;
    function Table144VersionMake: Boolean;
    function Table146VersionMake: Boolean;
    function Table147VersionMake: Boolean;
    function Table148VersionMake: Boolean;
    function Table149VersionMake: Boolean;
    function Table150VersionMake: Boolean;
    function Table151VersionMake: Boolean;
    function Table152VersionMake: Boolean;
    function Table153VersionMake: Boolean;
    function Table154VersionMake: Boolean;
    function Table155VersionMake: Boolean;
    function Table156VersionMake: Boolean;
    function Table157VersionMake: Boolean;
    function Table158VersionMake: Boolean;
    function Table159VersionMake: Boolean;
    function Table160VersionMake: Boolean;
    function Table161VersionMake: Boolean;
    function Table162VersionMake: Boolean;
    function Table163VersionMake: Boolean;
    function Table164VersionMake: Boolean;
    function Table165VersionMake: Boolean;
    function Table166VersionMake: Boolean;
    function Table167VersionMake: Boolean;
    function Table168VersionMake: Boolean;
    function Table169VersionMake: Boolean;
    function Table170VersionMake: Boolean;
    function Table171VersionMake: Boolean;
    function Table172VersionMake: Boolean;
    function Table173VersionMake: Boolean;
    function Table174VersionMake: Boolean;
    function Table175VersionMake: Boolean;
    function Table176VersionMake: Boolean;
    function Table177VersionMake: Boolean;
    function Table178VersionMake: Boolean;
    function Table179VersionMake: Boolean;
    function Table180VersionMake: Boolean;
    function Table181VersionMake: Boolean;
    function Table182VersionMake: Boolean;
    function Table183VersionMake: Boolean;
    function Table184VersionMake: Boolean;
    function Table185VersionMake: Boolean;
    function Table186VersionMake: Boolean;
    function Table187VersionMake: Boolean;
    function Table188VersionMake: Boolean;
    function Table189VersionMake: Boolean;
    function Table190VersionMake: Boolean;
    function Table191VersionMake: Boolean;
    function Table192VersionMake: Boolean;
    function Table193VersionMake: Boolean;
    function Table194VersionMake: Boolean;
    function Table195VersionMake: Boolean;
    function Table196VersionMake: Boolean;
    function Table197VersionMake: Boolean;
    function Table198VersionMake: Boolean;
    function Table199VersionMake: Boolean;
    function Table200VersionMake: Boolean;
    function Table201VersionMake: Boolean;
    function Table202VersionMake: Boolean;
    function Table203VersionMake: Boolean;
    function Table204VersionMake: Boolean;
    function Table205VersionMake: Boolean;
    function PositionNumberValueSetting:Boolean;
    Function UpdateTB_CARDPoitionNum(aGroupCode,aCardNo:string;aPositionNumber:integer):Boolean;
    function GetVersion:integer;
    function CheckPersonRelay : string;
    function CheckSamyuk : Boolean;
    function CheckFdms : Boolean;
    class Function GetObject:TDataBaseConfig;   //자기자신을 찾는것  class 는 폼생성전에도 사용가능
  Published
    { Published declarations }
    Property Cancel:Boolean read FCancel write SetCancel;
    Property DBConnected : Boolean read FDBConnected write SetDBConnected;
  end;

  TfmDataBaseConfig = class(TForm)
    sbSave: TSpeedButton;
    sbCancel: TSpeedButton;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    rg_DBType1: TRadioGroup;
    pn_Mssql1: TPanel;
    Label1: TLabel;
    edServerIP1: TEdit;
    Label2: TLabel;
    edServerPort1: TEdit;
    Label3: TLabel;
    edUserid1: TEdit;
    Label4: TLabel;
    edPasswd1: TEdit;
    Label5: TLabel;
    edDataBaseName1: TEdit;
    pn_Mssql2: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    edServerIP2: TEdit;
    edServerPort2: TEdit;
    edUserid2: TEdit;
    edPasswd2: TEdit;
    edDataBaseName2: TEdit;
    rg_DBType2: TRadioGroup;
    pn_Mssql3: TPanel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    edServerIP3: TEdit;
    edServerPort3: TEdit;
    edUserid3: TEdit;
    edPasswd3: TEdit;
    edDataBaseName3: TEdit;
    rg_DBType3: TRadioGroup;
    OpenDialog1: TOpenDialog;
    chk_Copy: TCheckBox;
    pn_MDB1: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    ed_mdbPass1: TEdit;
    ed_MdbName1: TEdit;
    btn_File1: TButton;
    pn_MDB2: TPanel;
    Label18: TLabel;
    Label19: TLabel;
    ed_mdbPass2: TEdit;
    ed_MdbName2: TEdit;
    btn_File2: TButton;
    pn_MDB3: TPanel;
    Label20: TLabel;
    Label21: TLabel;
    ed_mdbPass3: TEdit;
    ed_MdbName3: TEdit;
    btn_File3: TButton;
    FolderDialog1: TFolderDialog;
    procedure sbCancelClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
    procedure edServerIP1KeyPress(Sender: TObject; var Key: Char);
    procedure edServerPort1KeyPress(Sender: TObject; var Key: Char);
    procedure edUserid1KeyPress(Sender: TObject; var Key: Char);
    procedure edPasswd1KeyPress(Sender: TObject; var Key: Char);
    procedure FormActivate(Sender: TObject);
    procedure edServerIP1Enter(Sender: TObject);
    procedure edServerPort1Enter(Sender: TObject);
    procedure ed(Sender: TObject);
    procedure edPasswd1Enter(Sender: TObject);
    procedure edDataBaseName1KeyPress(Sender: TObject; var Key: Char);
    procedure edDataBaseName1Enter(Sender: TObject);
    procedure btn_File1Click(Sender: TObject);
    procedure rg_DBType1Click(Sender: TObject);
    procedure rg_DBType2Click(Sender: TObject);
    procedure rg_DBType3Click(Sender: TObject);
    procedure chk_CopyClick(Sender: TObject);
    procedure edDataBaseName1Exit(Sender: TObject);
    procedure edPasswd1Exit(Sender: TObject);
    procedure edUserid1Exit(Sender: TObject);
    procedure edServerPort1Exit(Sender: TObject);
    procedure edServerIP1Exit(Sender: TObject);
  private
    { Private declarations }
    function SaveLOMOSMSSQL(aType:string):Boolean;
    function SaveLOMOSMDB:Boolean;
    function SaveEVENTMSSQL:Boolean;
    function SaveEVENTMDB:Boolean;
    function SaveLOGMSSQL:Boolean;
    function SaveLOGMDB:Boolean;
  public
    { Public declarations }
  end;

var
  fmDataBaseConfig: TfmDataBaseConfig;

implementation

uses
  uDataModule1,
  DIMime,
  uCommonSql,
  uMDBSql,
  uPostGreSql,
  uMSSQL,
  uFireBird,
  udmAdoQuery,
  uLomosUtil, uDBCreate, uDBFunction;

{$R *.dfm}

procedure TfmDataBaseConfig.sbCancelClick(Sender: TObject);
begin
  TDataBaseConfig.GetObject.Cancel := True;
  Close;
end;

{ TDataBaseConfig }

function TDataBaseConfig.CheckFdms: Boolean;
var
  stSql : string;
begin
  result := False;
  stSql := 'select * from TB_CONFIG ';
  stSql := stSql + ' where CO_CONFIGGROUP = ''RELAY'' ';
  stSql := stSql + ' AND CO_CONFIGCODE = ''fdmsUSE'' ';
  with DataModule1.ADOTmpQuery do
  begin
    Close;
    Sql.Clear;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;
    if recordCount < 1 then Exit;
    Try
      if UpperCase(FindField('CO_CONFIGVALUE').AsString) = 'TRUE' then result := True;
    Except
      Exit;
    End;
  end;
end;

function TDataBaseConfig.CheckPersonRelay: string;
var
  stSql : string;
begin
  result := '0';
  stSql := 'select * from TB_CONFIG ';
  stSql := stSql + ' where CO_CONFIGGROUP = ''RELAY'' ';
  stSql := stSql + ' AND CO_CONFIGCODE = ''PER_RELAY'' ';
  with DataModule1.ADOTmpQuery do
  begin
    Close;
    Sql.Clear;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;
    if recordCount < 1 then Exit;
    Try
      result := FindField('CO_CONFIGVALUE').AsString;
    Except
      Exit;
    End;
  end;
end;

function TDataBaseConfig.CheckSamyuk: Boolean;
var
  stSql : string;
begin
  result := False;
  if CheckPersonRelay = '1' then result := True;  //삼육대연동이면 True
end;

function TDataBaseConfig.CreateTB_COMPANY_PrimaryKey: Boolean;
var
  stSql : string;
begin
  stSql := 'ALTER TABLE TB_COMPANY ADD CONSTRAINT TB_COMPANY_PKIndex1 PRIMARY KEY ( GROUP_CODE,CO_COMPANYCODE,CO_JIJUMCODE,CO_DEPARTCODE ) ';
  result := DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.CreateTB_LOCATION_PrimaryKey: Boolean;
var
  stSql : string;
begin
  stSql := 'ALTER TABLE TB_LOCATION ADD CONSTRAINT TB_LOCATION_PKIndex1 PRIMARY KEY ( GROUP_CODE,LO_DONGCODE,LO_FLOORCODE,LO_AREACODE ) ';
  result := DataModule1.ProcessExecSQL(stSql);
end;

Function TDataBaseConfig.DataBaseConnect(aMessage:Boolean=True):Boolean;
var
  conStr : wideString;
  stHost : String;
  stPort : String;
  stuserID : String;
  stuserPW : String;
  stpsqlOdbcSetup : String;
  ini_fun : TiniFile;
  stExeFolder : String;
  stDBPath : string;
begin
    if DBConnected then Exit;
    result := False;
    CanCel := False;

    stExeFolder  := ExtractFileDir(Application.ExeName);
  Try
    ini_fun := TiniFile.Create(stExeFolder + '\Zmos.INI');

    stHost  := ini_fun.ReadString('DBConfig','Host','127.0.0.1');
    stPort := ini_fun.ReadString('DBConfig','Port','1433');
    stuserID := ini_fun.ReadString('DBConfig','UserID','sa');
    stuserPW := MimeDecodeString(ini_fun.ReadString('DBConfig','UserPW',''));  //saPasswd
    DBName := lowerCase(ini_fun.ReadString('DBConfig','DBNAME',''));
    stDBPath := ini_fun.ReadString('DBConfig','DBPATH',stExeFolder + '/../DB');
    GROUPCODE := ini_fun.ReadString('COMPANY','GROUPCODE','1234567890');
    DBType := UpperCase(ini_fun.ReadString('DBConfig','TYPE','MSSQL'));
    G_nSmallSetDBRelay := ini_fun.ReadInteger('SMALLSET','RELAY',0);
    DBServer := stHost;
    DBUser := stuserID;
    DBUserPw := stuserPW;
    DBPort := stPort;
    stpsqlOdbcSetup := UpperCase(ini_fun.ReadString('DBConfig','psqlodbcsetup','FALSE'));
    conStr := '';
    if UpperCase(DBType) = 'MSSQL' then
    begin
      conStr := constr + 'Provider=SQLOLEDB.1;';
      conStr := constr + 'Password=' + stuserPW + ';';
      conStr := constr + 'Persist Security Info=True;';
      conStr := constr + 'User ID=' + stuserID + ';';
      conStr := constr + 'Initial Catalog=' + DBName + ';';
      conStr := constr + 'Data Source=' + stHost  + ',' + stPort;
    end else if UpperCase(DBType) = 'PG' then
    begin
//      conStr := 'Provider=PostgreSQL.1;';
      //conStr := 'Provider=PSQLODBC35W;';
      if stpsqlOdbcSetup = 'TRUE' then
      begin
        conStr := 'Provider=MSDASQL.1;Persist Security Info=False;';
        conStr := conStr + 'Extended Properties="DSN=PostgreSQL35W;DATABASE=' + DBName + ';';
        conStr := conStr + 'SERVER=' + stHost + ';PORT=5432;UID='+ stuserID + ';PWD=' + stuserPW + ';SSLmode=disable;';
        conStr := conStr + 'ReadOnly=0;Protocol=7.4;FakeOidIndex=0;ShowOidColumn=0;';
        conStr := conStr + 'RowVersioning=0;ShowSystemTables=0;ConnSettings=;Fetch=100;';
        conStr := conStr + 'Socket=4096;UnknownSizes=0;MaxVarcharSize=255;MaxLongVarcharSize=8190;';
        conStr := conStr + 'Debug=0;CommLog=0;Optimizer=1;Ksqo=1;UseDeclareFetch=0;TextAsLongVarchar=1;';
        conStr := conStr + 'UnknownsAsLongVarchar=0;BoolsAsChar=1;Parse=0;CancelAsFreeStmt=0;';
        conStr := conStr + 'ExtraSysTablePrefixes=dd_;;LFConversion=1;UpdatableCursors=1;';
        conStr := conStr + 'DisallowPremature=0;TrueIsMinus1=0;BI=0;ByteaAsLongVarBinary=0;UseServerSidePrepare=0;LowerCaseIdentifier=0;XaOpt=1"';
      end else
      begin
        conStr := 'Provider=PostgreSQL OLE DB Provider;';
        //conStr := 'Provider=PostgreSQL.1;';
        conStr := constr + 'Data Source=' + stHost + ';'   ;
        conStr := constr + 'location=' + DBName + ';';
        conStr := constr + 'User Id='+ stuserID + ';';
        conStr := constr + 'password=' + stuserPW;
      end;

{      conStr := 'Provider=PGNP.1;';
      conStr := constr + 'password=' + stuserPW + ';';
      conStr := constr + 'Persist Security Info=True;';
      conStr := constr + 'User Id='+ stuserID + ';';
      conStr := constr + 'Data Source=' + stHost + ';'   ;
      conStr := constr + 'Extended Properties="SSL=allow;"';  }
    end else if UpperCase(DBType) = 'MDB' then
    begin
      //if DBName = '' then
      DBName := stDBPath + '\ZMOS.mdb';
      conStr := 'Provider=Microsoft.Jet.OLEDB.4.0;';
      conStr := conStr + 'Data Source=' + DBName + ';';
      conStr := conStr + 'Persist Security Info=True;';
      conStr := conStr + 'Jet OLEDB:Database ';
      //if stuserPW <> '' then  conStr := conStr + ' Password=' + stuserPW;
    end else if UpperCase(DBType) = 'FB' then
    begin
      conStr := 'Provider=MSDASQL;';
      conStr := conStr + 'DRIVER=Firebird/InterBase(r) driver;';
      conStr := constr + 'UID=' + stuserID + ';';
      conStr := constr + 'PWD=' + stuserPW + ';';
      conStr := constr + 'Auto Commit=true;';
      conStr := constr + 'DBNAME=' + stHost + ':' + DBName;
//      conStr := 'Provider=FB.FirebirdProvider;User=SYSDBA;Password=masterkey;DBNAME=127.0.0.1:E:\DB\FDB\ZMOS.FDB;DIALECT=3;';
//      conStr := 'Provider=LCPI.IBProvider;Data Source=127.0.0.1:E:\DB\FDB\ZMOS.FDB;User ID=SYSDBA;Password=masterkey;Auto Commit=true;DIALECT=3;';
      conStr := 'Provider=ZStyle IBOLE Provider;Data Source=127.0.0.1:zmos;UID=sysdba;Password=masterkey;DIALECT=3;';


    end else
    begin
      showmessage('DB Type 이 정확하지 않습니다.');
      Exit;
    end;

    with DataModule1.ADOConnection do
    begin
      Connected := False;
      Try
        ConnectionString := conStr;
        LoginPrompt:= false ;
        Connected := True;
      Except
        on E : EDatabaseError do
          begin
            // ERROR MESSAGE-BOX DISPLAY
            if aMessage then
              ShowMessage(E.Message );
            Exit;
          end;
        else
          begin
            if aMessage then
              ShowMessage('데이터베이스 접속 에러'+ conStr );
            Exit;
          end;
      End;
      CursorLocation := clUseServer;
      //CursorLocation := clUseClient;
    end;

    stHost  := ini_fun.ReadString('EVENTDBConfig','Host','127.0.0.1');
    stPort := ini_fun.ReadString('EVENTDBConfig','Port','1433');
    stuserID := ini_fun.ReadString('EVENTDBConfig','UserID','sa');
    stuserPW := MimeDecodeString(ini_fun.ReadString('EVENTDBConfig','UserPW',''));  //saPasswd
    DBName := lowerCase(ini_fun.ReadString('EVENTDBConfig','DBNAME',''));
    conStr := '';
    if UpperCase(DBType) = 'MSSQL' then
    begin
      conStr := constr + 'Provider=SQLOLEDB.1;';
      conStr := constr + 'Password=' + stuserPW + ';';
      conStr := constr + 'Persist Security Info=True;';
      conStr := constr + 'User ID=' + stuserID + ';';
      conStr := constr + 'Initial Catalog=' + DBName + ';';
      conStr := constr + 'Data Source=' + stHost  + ',' + stPort;
    end else if UpperCase(DBType) = 'PG' then
    begin
      if stpsqlOdbcSetup = 'TRUE' then
      begin
        conStr := 'Provider=MSDASQL.1;Persist Security Info=False;';
        conStr := conStr + 'Extended Properties="DSN=PostgreSQL35W;DATABASE=' + DBName + ';';
        conStr := conStr + 'SERVER=' + stHost + ';PORT=5432;UID='+ stuserID + ';PWD=' + stuserPW + ';SSLmode=disable;';
        conStr := conStr + 'ReadOnly=0;Protocol=7.4;FakeOidIndex=0;ShowOidColumn=0;';
        conStr := conStr + 'RowVersioning=0;ShowSystemTables=0;ConnSettings=;Fetch=100;';
        conStr := conStr + 'Socket=4096;UnknownSizes=0;MaxVarcharSize=255;MaxLongVarcharSize=8190;';
        conStr := conStr + 'Debug=0;CommLog=0;Optimizer=1;Ksqo=1;UseDeclareFetch=0;TextAsLongVarchar=1;';
        conStr := conStr + 'UnknownsAsLongVarchar=0;BoolsAsChar=1;Parse=0;CancelAsFreeStmt=0;';
        conStr := conStr + 'ExtraSysTablePrefixes=dd_;;LFConversion=1;UpdatableCursors=1;';
        conStr := conStr + 'DisallowPremature=0;TrueIsMinus1=0;BI=0;ByteaAsLongVarBinary=0;UseServerSidePrepare=0;LowerCaseIdentifier=0;XaOpt=1"';
      end else
      begin
        conStr := 'Provider=PostgreSQL OLE DB Provider;';
        //conStr := 'Provider=PostgreSQL.1;';
        conStr := constr + 'Data Source=' + stHost + ';'   ;
        conStr := constr + 'location=' + DBName + ';';
        conStr := constr + 'User Id='+ stuserID + ';';
        conStr := constr + 'password=' + stuserPW;
      end;
    end else if UpperCase(DBType) = 'MDB' then
    begin
      //if DBName = '' then
      //DBName := stExeFolder + '\..\DB\ZEVENT.mdb';
      DBName := stDBPath + '\ZEVENT.mdb';
      conStr := 'Provider=Microsoft.Jet.OLEDB.4.0;';
      conStr := conStr + 'Data Source=' + DBName + ';';
      conStr := conStr + 'Persist Security Info=True;';
      conStr := conStr + 'Jet OLEDB:Database ';
      //if stuserPW <> '' then  conStr := conStr + ' Password=' + stuserPW;
    end else if UpperCase(DBType) = 'FB' then
    begin
      conStr := 'Provider=MSDASQL;';
      conStr := conStr + 'DRIVER=Firebird/InterBase(r) driver;';
      conStr := constr + 'UID=' + stuserID + ';';
      conStr := constr + 'PWD=' + stuserPW + ';';
      conStr := constr + 'Auto Commit=true;';
      conStr := constr + 'DBNAME=' + stHost + ':' + DBName;
    end else
    begin
      showmessage('DB Type 이 정확하지 않습니다.');
      Exit;
    end;

    with DataModule1.ADOConnectionEVENT do
    begin
      Connected := False;
      ConnectionString := conStr;
      Try
        LoginPrompt:= false ;
        Connected := True;
      Except
        on E : EDatabaseError do
          begin
            // ERROR MESSAGE-BOX DISPLAY
            ShowMessage(E.Message );
            Exit;
          end;
        else
          begin
            ShowMessage('데이터베이스 접속 에러(E)' );
            Exit;
          end;
      End;
      CursorLocation := clUseServer;
      //CursorLocation := clUseClient;
    end;

    stHost  := ini_fun.ReadString('LOGDBConfig','Host','127.0.0.1');
    stPort := ini_fun.ReadString('LOGDBConfig','Port','1433');
    stuserID := ini_fun.ReadString('LOGDBConfig','UserID','sa');
    stuserPW := MimeDecodeString(ini_fun.ReadString('LOGDBConfig','UserPW',''));  //saPasswd
    DBName := lowerCase(ini_fun.ReadString('LOGDBConfig','DBNAME',''));
    //DBType := UpperCase(ini_fun.ReadString('LOGDBConfig','TYPE','MSSQL'));
    conStr := '';
    if UpperCase(DBType) = 'MSSQL' then
    begin
      conStr := constr + 'Provider=SQLOLEDB.1;';
      conStr := constr + 'Password=' + stuserPW + ';';
      conStr := constr + 'Persist Security Info=True;';
      conStr := constr + 'User ID=' + stuserID + ';';
      conStr := constr + 'Initial Catalog=' + DBName + ';';
      conStr := constr + 'Data Source=' + stHost  + ',' + stPort;
    end else if UpperCase(DBType) = 'PG' then
    begin
      if stpsqlOdbcSetup = 'TRUE' then
      begin
        conStr := 'Provider=MSDASQL.1;Persist Security Info=False;';
        conStr := conStr + 'Extended Properties="DSN=PostgreSQL35W;DATABASE=' + DBName + ';';
        conStr := conStr + 'SERVER=' + stHost + ';PORT=5432;UID='+ stuserID + ';PWD=' + stuserPW + ';SSLmode=disable;';
        conStr := conStr + 'ReadOnly=0;Protocol=7.4;FakeOidIndex=0;ShowOidColumn=0;';
        conStr := conStr + 'RowVersioning=0;ShowSystemTables=0;ConnSettings=;Fetch=100;';
        conStr := conStr + 'Socket=4096;UnknownSizes=0;MaxVarcharSize=255;MaxLongVarcharSize=8190;';
        conStr := conStr + 'Debug=0;CommLog=0;Optimizer=1;Ksqo=1;UseDeclareFetch=0;TextAsLongVarchar=1;';
        conStr := conStr + 'UnknownsAsLongVarchar=0;BoolsAsChar=1;Parse=0;CancelAsFreeStmt=0;';
        conStr := conStr + 'ExtraSysTablePrefixes=dd_;;LFConversion=1;UpdatableCursors=1;';
        conStr := conStr + 'DisallowPremature=0;TrueIsMinus1=0;BI=0;ByteaAsLongVarBinary=0;UseServerSidePrepare=0;LowerCaseIdentifier=0;XaOpt=1"';
      end else
      begin
        conStr := 'Provider=PostgreSQL OLE DB Provider;';
        //conStr := 'Provider=PostgreSQL.1;';
        conStr := constr + 'Data Source=' + stHost + ';'   ;
        conStr := constr + 'location=' + DBName + ';';
        conStr := constr + 'User Id='+ stuserID + ';';
        conStr := constr + 'password=' + stuserPW;
      end;
    end else if UpperCase(DBType) = 'MDB' then
    begin
      //if DBName = '' then
      //DBName := stExeFolder + '\..\DB\ZLOG.mdb';
      DBName := stDBPath + '\ZLOG.mdb';
      conStr := 'Provider=Microsoft.Jet.OLEDB.4.0;';
      conStr := conStr + 'Data Source=' + DBName + ';';
      conStr := conStr + 'Persist Security Info=True;';
      conStr := conStr + 'Jet OLEDB:Database ';
      //if stuserPW <> '' then  conStr := conStr + ' Password=' + stuserPW;
    end else if UpperCase(DBType) = 'FB' then
    begin
      conStr := 'Provider=MSDASQL;';
      conStr := conStr + 'DRIVER=Firebird/InterBase(r) driver;';
      conStr := constr + 'UID=' + stuserID + ';';
      conStr := constr + 'PWD=' + stuserPW + ';';
      conStr := constr + 'Auto Commit=true;';
      conStr := constr + 'DBNAME=' + stHost + ':' + DBName;
    end else
    begin
      showmessage('DB Type 이 정확하지 않습니다.');
      Exit;
    end;

    with DataModule1.ADOConnectionLOG do
    begin
      Connected := False;
      ConnectionString := conStr;
      Try
        LoginPrompt:= false ;
        Connected := True;
      Except
        on E : EDatabaseError do
          begin
            // ERROR MESSAGE-BOX DISPLAY
            ShowMessage(E.Message );
            Exit;
          end;
        else
          begin
            ShowMessage('데이터베이스 접속 에러(L)' );
            Exit;
          end;
      End;
      CursorLocation := clUseServer;
      //CursorLocation := clUseClient;
    end;
    FingerTableVersionCheck;
    TableVersionCheck;
    GuestTableVersionCheck;

    DBConnected := True;
    result := True;

  Finally
    ini_fun.free;
  End;
end;

function TDataBaseConfig.DropTB_COMPANY_PrimaryKey: Boolean;
var
  stSql : string;
begin
  stSql := 'sp_helpindex TB_COMPANY ';

  with DataModule1.ADOTmpQuery do
  begin
    Close;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;

    if recordCount < 1 then Exit;
    stSql := 'ALTER TABLE TB_COMPANY DROP ' + FieldByName('index_name').AsString;
    DataModule1.ProcessExecSQL(stSql);
  end;
end;

function TDataBaseConfig.DropTB_LOCATION_PrimaryKey: Boolean;
var
  stSql : string;
begin
  stSql := 'sp_helpindex TB_LOCATION ';

  with DataModule1.ADOTmpQuery do
  begin
    Close;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;

    if recordCount < 1 then Exit;
    stSql := 'ALTER TABLE TB_LOCATION DROP ' + FieldByName('index_name').AsString;
    DataModule1.ProcessExecSQL(stSql);
  end;
end;

class function TDataBaseConfig.FindSelf: TComponent;
var
  Loop:Integer;
begin
  Result:=Nil;
  for Loop:=0 to Application.ComponentCount-1 do begin
      if Application.Components[Loop] is TDataBaseConfig then begin
          Result:= Application.Components[Loop];
          Break;
      end;
  end;
end;

function TDataBaseConfig.FingerTable001VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('FINGER','TABLE_VER','1');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.FingerTable002VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_CARDFINGER
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_CARDFINGER
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_CARDFINGER
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_CARDFINGER
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_FINGERDEVICE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_FINGERDEVICE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_FINGERDEVICE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FINGERDEVICE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_FINGERDEVICECARD
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_FINGERDEVICECARD
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_FINGERDEVICECARD
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FINGERDEVICECARD
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('FINGER','TABLE_VER','2');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.FingerTable003VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_FINGERDEVICE_CHANGE_ADD
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_FINGERDEVICE_CHANGE_ADD
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_FINGERDEVICE_CHANGE_ADD
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_FINGERDEVICE_CHANGE_ADD
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('FINGER','TABLE_VER','3');
  DataModule1.ProcessExecSQL(stSql);
end;

procedure TDataBaseConfig.FingerTableVersionCheck;
var
  nTableVersion : integer;
begin
  nTableVersion := GetFingerTableVersion;

  if nTableVersion < 1 then FingerTable001VersionMake;
  if nTableVersion < 2 then FingerTable002VersionMake;
  if nTableVersion < 3 then FingerTable003VersionMake;

end;

function TDataBaseConfig.GetFingerTableVersion: integer;
var
  stSql : string;
begin
  result := 0;
  stSql := 'select * from TB_CONFIG ';
  stSql := stSql + ' where CO_CONFIGGROUP = ''FINGER'' ';
  stSql := stSql + ' AND CO_CONFIGCODE = ''TABLE_VER'' ';
  with DataModule1.ADOTmpQuery do
  begin
    Close;
    Sql.Clear;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;
    if recordCount < 1 then Exit;
    Try
      result := strtoint(FindField('CO_CONFIGVALUE').AsString);
    Except
      Exit;
    End;
  end;
end;

function TDataBaseConfig.GetGuestTableVersion: integer;
var
  stSql : string;
begin
  result := 0;
  stSql := 'select * from TB_CONFIG ';
  stSql := stSql + ' where CO_CONFIGGROUP = ''GUEST'' ';
  stSql := stSql + ' AND CO_CONFIGCODE = ''TABLE_VER'' ';
  with DataModule1.ADOTmpQuery do
  begin
    Close;
    Sql.Clear;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;
    if recordCount < 1 then Exit;
    Try
      result := strtoint(FindField('CO_CONFIGVALUE').AsString);
    Except
      Exit;
    End;
  end;
end;

class function TDataBaseConfig.GetObject: TDataBaseConfig;
begin
   If FindSelf = Nil then TDataBaseConfig.Create(Application);
   Result := TDataBaseConfig(FindSelf);
end;

function TDataBaseConfig.GetVersion: integer;
var
  stSql : string;
begin
  result := 0;
  stSql := 'select * from TB_CONFIG ';
  stSql := stSql + ' where CO_CONFIGGROUP = ''COMMON'' ';
  stSql := stSql + ' AND CO_CONFIGCODE = ''TABLE_VER'' ';
  with DataModule1.ADOTmpQuery do
  begin
    Close;
    Sql.Clear;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;
    if recordCount < 1 then Exit;
    Try
      result := strtoint(FindField('CO_CONFIGVALUE').AsString);
    Except
      Exit;
    End;
  end;
end;

function TDataBaseConfig.GuestTable001VersionMake: Boolean;
var
  stSql : string;
begin
  dmDBCreate.CreateTB_GUEST;
  dmDBCreate.CreateTB_GUEST_Idx01;
  dmDBCreate.CreateTB_GUESTDEVICECARDNO;
  dmDBCreate.AlterTB_CARD_GUEST_Add;
  dmDBCreate.CreateTB_GUESTEVENT;

  stSql := CommonSql.InsertIntoTB_CONFIG('GUEST','TABLE_VER','1');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.GuestTable002VersionMake: Boolean;
var
  stSql : string;
  stCompanyCode : string;
begin
  stCompanyCode := dmDBFunction.GetNextCompanyCode;
  dmDBFunction.InsertIntoTB_COMPANY_CompanyName(stCompanyCode,'방문자');

  stSql := CommonSql.InsertIntoTB_CONFIG('GUEST','COCODE',stCompanyCode);
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('GUEST','TABLE_VER','2');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.GuestTable003VersionMake: Boolean;
var
  stSql : string;
begin
  dmDBCreate.AlterTB_GUEST_NOTAUTCONTENT_Add;

  stSql := CommonSql.UpdateTB_CONFIG('GUEST','TABLE_VER','3');
  DataModule1.ProcessExecSQL(stSql);
end;

procedure TDataBaseConfig.GuestTableVersionCheck;
var
  nTableVersion : integer;
begin
  nTableVersion := GetGuestTableVersion;

  if nTableVersion < 1 then GuestTable001VersionMake;
  if nTableVersion < 2 then GuestTable002VersionMake;
  if nTableVersion < 3 then GuestTable003VersionMake;
end;

function TDataBaseConfig.PositionNumberValueSetting: Boolean;
var
  stSql : string;
  nPositionNumber : integer;
begin
  stSql := 'Select * from TB_CARD ';
  nPositionNumber := 1;

  with DataModule1.ADOTmpQuery do
  begin
    Close;
    Sql.Clear;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;
    if recordCount < 1 then Exit;
    While Not Eof do
    begin
      UpdateTB_CARDPoitionNum(FindField('GROUP_CODE').AsString,FindField('CA_CARDNO').AsString,nPositionNumber);
      inc(nPositionNumber);
      Next;
    end;
  end;


end;

procedure TDataBaseConfig.SetCancel(const Value: Boolean);
begin
  FCancel := Value;
end;

procedure TDataBaseConfig.SetDBConnected(const Value: Boolean);
begin
  FDBConnected := Value;
end;

procedure TDataBaseConfig.ShowDataBaseConfig;
begin
  FDBConnected := False;

  fmDataBaseConfig:=TfmDataBaseConfig.Create(Nil);
  Try
    fmDataBaseConfig.ShowModal;
  Finally
    fmDataBaseConfig.Free;
  End;
end;

procedure TfmDataBaseConfig.sbSaveClick(Sender: TObject);
var
  bResult : Boolean;
begin
  if rg_DBType1.ItemIndex = 0 then
  begin
    chk_CopyClick(self);
    bResult := SaveLOMOSMSSQL('MSSQL');
  end else
  if rg_DBType1.ItemIndex = 1 then
  begin
    chk_CopyClick(self);
    bResult := SaveLOMOSMSSQL('PG');
  end else
  if rg_DBType1.ItemIndex = 3 then
  begin
    chk_CopyClick(self);
    bResult := SaveLOMOSMSSQL('FB');
  end else
  begin
    bResult := SaveLOMOSMDB;
  end;
  if Not bResult then Exit;

  if rg_DBType1.ItemIndex = 0 then
  begin
    bResult := SaveEVENTMSSQL;
  end else
  if rg_DBType1.ItemIndex = 1 then
  begin
    bResult := SaveEVENTMSSQL;
  end else
  if rg_DBType1.ItemIndex = 3 then
  begin
    bResult := SaveEVENTMSSQL;
  end else
  begin
    bResult := SaveEVENTMDB;
  end;
  if Not bResult then Exit;

  if rg_DBType1.ItemIndex = 0 then
  begin
    bResult := SaveLOGMSSQL;
  end else
  if rg_DBType1.ItemIndex = 1 then
  begin
    bResult := SaveLOGMSSQL;
  end else
  if rg_DBType1.ItemIndex = 3 then
  begin
    bResult := SaveLOGMSSQL;
  end else
  begin
    bResult := SaveLOGMDB;
  end;
  if Not bResult then Exit;


  TDataBaseConfig.GetObject.DataBaseConnect;
  Close;
end;

procedure TfmDataBaseConfig.edServerIP1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    edServerPort1.SetFocus;
  end  else
  begin
    if chk_Copy.Checked then
    begin
      edServerIP2.Text := edServerIP1.Text;
      edServerIP3.Text := edServerIP1.Text;
    end;
  end;
end;

procedure TfmDataBaseConfig.edServerPort1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    edUserID1.SetFocus;
  end else
  begin
    if chk_Copy.Checked then
    begin
      edServerPort2.Text := edServerPort1.Text;
      edServerPort3.Text := edServerPort1.Text;
    end;
  end;

end;

procedure TfmDataBaseConfig.edUserid1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    edPasswd1.SetFocus;
  end else
  begin
    if chk_Copy.Checked then
    begin
      edUserid2.Text := edUserid1.Text;
      edUserid3.Text := edUserid1.Text;
    end;
  end;

end;

procedure TfmDataBaseConfig.edPasswd1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    edDataBaseName1.SetFocus;
  end else
  begin
    if chk_Copy.Checked then
    begin
      edPasswd2.Text := edPasswd1.Text;
      edPasswd3.Text := edPasswd1.Text;
    end;
  end;

end;

procedure TfmDataBaseConfig.FormActivate(Sender: TObject);
var
  ini_fun : TiniFile;
  stExeFolder : string;
  stDBType : string;
begin
  StatusBar1.Panels[0].Text := 'DB 환경설정이 잘못되었습니다. 재설정하시기 바랍니다.';

  stExeFolder  := ExtractFileDir(Application.ExeName);
  ini_fun := TiniFile.Create(stExeFolder + '\Zmos.INI');

  stDBType := ini_fun.ReadString('DBConfig','Type','MSSQL');
  if UpperCase(stDBType) = 'MSSQL' then
  begin
    rg_DBType1.ItemIndex := 0;
    edServerIP1.Text := ini_fun.ReadString('DBConfig','Host','127.0.0.1');
    edServerPort1.Text := ini_fun.ReadString('DBConfig','Port','1433');
    edUserID1.Text := ini_fun.ReadString('DBConfig','UserID','sa');
    edPasswd1.Text := MimeDEcodeString(ini_fun.ReadString('DBConfig','UserPW','sapasswd'));
    edDataBaseName1.Text := ini_fun.ReadString('DBConfig','DBNAME','ZMOS');

    edServerIP2.Text := ini_fun.ReadString('EVENTDBConfig','Host','127.0.0.1');
    edServerPort2.Text := ini_fun.ReadString('EVENTDBConfig','Port','1433');
    edUserID2.Text := ini_fun.ReadString('EVENTDBConfig','UserID','sa');
    edPasswd2.Text := MimeDEcodeString(ini_fun.ReadString('EVENTDBConfig','UserPW','sapasswd'));
    edDataBaseName2.Text := ini_fun.ReadString('EVENTDBConfig','DBNAME','ZMOS');

    edServerIP3.Text := ini_fun.ReadString('LOGDBConfig','Host','127.0.0.1');
    edServerPort3.Text := ini_fun.ReadString('LOGDBConfig','Port','1433');
    edUserID3.Text := ini_fun.ReadString('LOGDBConfig','UserID','sa');
    edPasswd3.Text := MimeDEcodeString(ini_fun.ReadString('LOGDBConfig','UserPW','sapasswd'));
    edDataBaseName3.Text := ini_fun.ReadString('LOGDBConfig','DBNAME','ZMOS');
  end else
  if UpperCase(stDBType) = 'PG' then
  begin
    rg_DBType1.ItemIndex := 1;
    edServerIP1.Text := ini_fun.ReadString('DBConfig','Host','127.0.0.1');
    edServerPort1.Text := ini_fun.ReadString('DBConfig','Port','5432');
    edUserID1.Text := ini_fun.ReadString('DBConfig','UserID','postgres');
    edPasswd1.Text := MimeDEcodeString(ini_fun.ReadString('DBConfig','UserPW','1'));
    edDataBaseName1.Text := ini_fun.ReadString('DBConfig','DBNAME','ZMOS');

    edServerIP2.Text := ini_fun.ReadString('EVENTDBConfig','Host','127.0.0.1');
    edServerPort2.Text := ini_fun.ReadString('EVENTDBConfig','Port','5432');
    edUserID2.Text := ini_fun.ReadString('EVENTDBConfig','UserID','postgres');
    edPasswd2.Text := MimeDEcodeString(ini_fun.ReadString('EVENTDBConfig','UserPW','1'));
    edDataBaseName2.Text := ini_fun.ReadString('EVENTDBConfig','DBNAME','ZMOS');

    edServerIP3.Text := ini_fun.ReadString('LOGDBConfig','Host','127.0.0.1');
    edServerPort3.Text := ini_fun.ReadString('LOGDBConfig','Port','5432');
    edUserID3.Text := ini_fun.ReadString('LOGDBConfig','UserID','postgres');
    edPasswd3.Text := MimeDEcodeString(ini_fun.ReadString('LOGDBConfig','UserPW','1'));
    edDataBaseName3.Text := ini_fun.ReadString('LOGDBConfig','DBNAME','ZMOS');
  end else
  if UpperCase(stDBType) = 'FB' then
  begin
    rg_DBType1.ItemIndex := 3;
    edServerIP1.Text := ini_fun.ReadString('DBConfig','Host','127.0.0.1');
    edServerPort1.Text := ini_fun.ReadString('DBConfig','Port','1433');
    edUserID1.Text := ini_fun.ReadString('DBConfig','UserID','SYSDBA');
    edPasswd1.Text := MimeDEcodeString(ini_fun.ReadString('DBConfig','UserPW','masterkey'));
    edDataBaseName1.Text := ini_fun.ReadString('DBConfig','DBNAME','ZMOS');

    edServerIP2.Text := ini_fun.ReadString('EVENTDBConfig','Host','127.0.0.1');
    edServerPort2.Text := ini_fun.ReadString('EVENTDBConfig','Port','1433');
    edUserID2.Text := ini_fun.ReadString('EVENTDBConfig','UserID','SYSDBA');
    edPasswd2.Text := MimeDEcodeString(ini_fun.ReadString('EVENTDBConfig','UserPW','masterkey'));
    edDataBaseName2.Text := ini_fun.ReadString('EVENTDBConfig','DBNAME','ZMOS');

    edServerIP3.Text := ini_fun.ReadString('LOGDBConfig','Host','127.0.0.1');
    edServerPort3.Text := ini_fun.ReadString('LOGDBConfig','Port','1433');
    edUserID3.Text := ini_fun.ReadString('LOGDBConfig','UserID','SYSDBA');
    edPasswd3.Text := MimeDEcodeString(ini_fun.ReadString('LOGDBConfig','UserPW','masterkey'));
    edDataBaseName3.Text := ini_fun.ReadString('LOGDBConfig','DBNAME','ZMOS');
  end else
  begin
    rg_DBType1.ItemIndex := 2;
    ed_mdbPass1.Text := ini_fun.ReadString('DBConfig','UserPW','');
    ed_MdbName1.Text := ini_fun.ReadString('DBConfig','DBPATH','');

    ed_mdbPass2.Text := ini_fun.ReadString('EVENTDBConfig','UserPW','');
    ed_MdbName2.Text := ini_fun.ReadString('EVENTDBConfig','DBPATH','');

    ed_mdbPass3.Text := ini_fun.ReadString('LOGDBConfig','UserPW','');
    ed_MdbName3.Text := ini_fun.ReadString('LOGDBConfig','DBPATH','');
  end;
  ini_fun.free;

  PageControl1.ActivePageIndex := 0;

end;

procedure TfmDataBaseConfig.edServerIP1Enter(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'DB 서버의 IP를 입력하여 주세요.';
end;

procedure TfmDataBaseConfig.edServerPort1Enter(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'DB 서버의 Port를 입력하여 주세요. 기본포트는 1433 입니다.';
end;

procedure TfmDataBaseConfig.ed(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'DB 서버의 계정을 입력하여 주세요.';
end;

procedure TfmDataBaseConfig.edPasswd1Enter(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'DB 서버 계정의 비밀번호를 입력하여 주세요.';
end;

procedure TfmDataBaseConfig.edDataBaseName1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    sbSaveClick(Self);
  end else
  begin
    if chk_Copy.Checked then
    begin
      edDataBaseName2.Text := edDataBaseName1.Text;
      edDataBaseName3.Text := edDataBaseName1.Text;
    end;
  end;

end;

procedure TfmDataBaseConfig.edDataBaseName1Enter(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := '데이터베이스명을 입력하여 주세요.';

end;

procedure TfmDataBaseConfig.btn_File1Click(Sender: TObject);
begin
  FolderDialog1.Title := 'DB 폴더 찾기';
  //OpenDialog1.DefaultExt := 'MDB';
  //OpenDialog1.Filter := 'Access files (*.mdb)|*.MDB';
  if FolderDialog1.Execute then
  begin
    if Sender = btn_File1 then ed_MdbName1.Text := FolderDialog1.Directory;
    if Sender = btn_File2 then ed_MdbName2.Text := FolderDialog1.Directory;
    if Sender = btn_File3 then ed_MdbName3.Text := FolderDialog1.Directory;
  end;
end;

procedure TfmDataBaseConfig.rg_DBType1Click(Sender: TObject);
begin
  if rg_DBType1.ItemIndex = 0 then
  begin
    pn_Mssql1.Visible := True;
    pn_MDB1.Visible := False;
    pn_Mssql1.Align := alClient;
    pn_Mssql2.Visible := True;
    pn_MDB2.Visible := False;
    pn_Mssql2.Align := alClient;
    pn_Mssql3.Visible := True;
    pn_MDB3.Visible := False;
    pn_Mssql3.Align := alClient;
    Label2.Visible := True;
    edServerPort1.Visible := True;
  end else
  if rg_DBType1.ItemIndex = 1 then
  begin
    pn_Mssql1.Visible := True;
    pn_MDB1.Visible := False;
    pn_Mssql1.Align := alClient;
    pn_Mssql2.Visible := True;
    pn_MDB2.Visible := False;
    pn_Mssql2.Align := alClient;
    pn_Mssql3.Visible := True;
    pn_MDB3.Visible := False;
    pn_Mssql3.Align := alClient;
    Label2.Visible := False;
    edServerPort1.Visible := False;
  end else
  begin
    pn_Mssql1.Visible := False;
    pn_MDB1.Visible := True;
    pn_MDB1.Align := alClient;
    pn_Mssql2.Visible := False;
    pn_MDB2.Visible := True;
    pn_MDB2.Align := alClient;
    pn_Mssql3.Visible := False;
    pn_MDB3.Visible := True;
    pn_MDB3.Align := alClient;
  end;
end;

function TfmDataBaseConfig.SaveLOMOSMSSQL(aType:string): Boolean;
var
  ini_fun : TiniFile;
  stExeFolder : String;
begin
  result := False;

  if edServerIP1.Text = '' then
  begin
    showmessage('서버IP가 비어 있습니다.');
    PageControl1.ActivePageIndex := 0;
    edServerIP1.SetFocus;
    Exit;
  End;

  if edServerPort1.Text = '' then
  begin
    showmessage('서버Port가 비어 있습니다.');
    PageControl1.ActivePageIndex := 0;
    edServerPort1.SetFocus;
    Exit;
  end;

  if edUserID1.Text = '' then
  begin
    showmessage('서버계정이 비어 있습니다.');
    PageControl1.ActivePageIndex := 0;
    edUserID1.SetFocus;
    Exit;
  end;

  if edPasswd1.Text = '' then
  begin
    showmessage('서버계정의 비밀번호가 비어 있습니다.');
    PageControl1.ActivePageIndex := 0;
    edPasswd1.SetFocus;
    Exit;
  end;

  stExeFolder  := ExtractFileDir(Application.ExeName);
  ini_fun := TiniFile.Create(stExeFolder + '\Zmos.INI');

  ini_fun.WriteString('DBConfig','TYPE',aType);
  ini_fun.WriteString('DBConfig','Host',Trim(edServerIP1.Text));
  ini_fun.WriteString('DBConfig','Port',Trim(edServerPort1.Text));
  ini_fun.WriteString('DBConfig','UserID',Trim(edUserID1.Text));
  ini_fun.WriteString('DBConfig','UserPW',MimeEncodeString(Trim(edPasswd1.Text)));
  ini_fun.WriteString('DBConfig','DBNAME',Trim(edDataBaseName1.Text));
  ini_fun.free;

  result := True;
end;

function TfmDataBaseConfig.SaveLOMOSMDB: Boolean;
var
  ini_fun : TiniFile;
  stExeFolder : String;
begin
  result := False;

  if ed_MdbName1.Text = '' then
  begin
    showmessage('DB 파일 위치를 확인하세요..');
    PageControl1.ActivePageIndex := 0;
    ed_MdbName1.SetFocus;
    Exit;
  End;

  stExeFolder  := ExtractFileDir(Application.ExeName);
  ini_fun := TiniFile.Create(stExeFolder + '\Zmos.INI');

  ini_fun.WriteString('DBConfig','TYPE','MDB');
  ini_fun.WriteString('DBConfig','UserPW',MimeEncodeString(Trim(ed_mdbPass1.Text)));
  ini_fun.WriteString('DBConfig','DBPATH',Trim(ed_MdbName1.Text));
  ini_fun.free;

  result := True;
end;

procedure TfmDataBaseConfig.rg_DBType2Click(Sender: TObject);
begin
  if rg_DBType2.ItemIndex = 0 then
  begin
    pn_Mssql2.Visible := True;
    pn_MDB2.Visible := False;
    pn_Mssql2.Align := alClient;
  end else
  begin
    pn_Mssql2.Visible := False;
    pn_MDB2.Visible := True;
    pn_MDB2.Align := alClient;
  end;
end;

procedure TfmDataBaseConfig.rg_DBType3Click(Sender: TObject);
begin
  if rg_DBType3.ItemIndex = 0 then
  begin
    pn_Mssql3.Visible := True;
    pn_MDB3.Visible := False;
    pn_Mssql3.Align := alClient;
  end else
  begin
    pn_Mssql3.Visible := False;
    pn_MDB3.Visible := True;
    pn_MDB3.Align := alClient;
  end;
end;

function TfmDataBaseConfig.SaveEVENTMDB: Boolean;
var
  ini_fun : TiniFile;
  stExeFolder : String;
begin
{  result := False;

  if ed_MdbName2.Text = '' then
  begin
    showmessage('DB 파일 위치를 확인하세요..');
    PageControl1.ActivePageIndex := 1;
    ed_MdbName2.SetFocus;
    Exit;
  End;

  stExeFolder  := ExtractFileDir(Application.ExeName);
  ini_fun := TiniFile.Create(stExeFolder + '\Zmos.INI');

  ini_fun.WriteString('EVENTDBConfig','TYPE','MDB');
  ini_fun.WriteString('EVENTDBConfig','UserPW',MimeEncodeString(Trim(ed_mdbPass2.Text)));
  ini_fun.WriteString('EVENTDBConfig','DBNAME',Trim(ed_MdbName2.Text));
  ini_fun.free;

  result := True;  }
end;

function TfmDataBaseConfig.SaveEVENTMSSQL: Boolean;
var
  ini_fun : TiniFile;
  stExeFolder : String;
begin
  result := False;

  if edServerIP2.Text = '' then
  begin
    showmessage('서버IP가 비어 있습니다.');
    PageControl1.ActivePageIndex := 1;
    edServerIP2.SetFocus;
    Exit;
  End;

  if edServerPort2.Text = '' then
  begin
    showmessage('서버Port가 비어 있습니다.');
    PageControl1.ActivePageIndex := 1;
    edServerPort2.SetFocus;
    Exit;
  end;

  if edUserID2.Text = '' then
  begin
    showmessage('서버계정이 비어 있습니다.');
    PageControl1.ActivePageIndex := 1;
    edUserID2.SetFocus;
    Exit;
  end;

  if edPasswd2.Text = '' then
  begin
    showmessage('서버계정의 비밀번호가 비어 있습니다.');
    PageControl1.ActivePageIndex := 1;
    edPasswd2.SetFocus;
    Exit;
  end;

  stExeFolder  := ExtractFileDir(Application.ExeName);
  ini_fun := TiniFile.Create(stExeFolder + '\Zmos.INI');

//  ini_fun.WriteString('DBConfig','TYPE','MSSQL');
  ini_fun.WriteString('EVENTDBConfig','Host',Trim(edServerIP2.Text));
  ini_fun.WriteString('EVENTDBConfig','Port',Trim(edServerPort2.Text));
  ini_fun.WriteString('EVENTDBConfig','UserID',Trim(edUserID2.Text));
  ini_fun.WriteString('EVENTDBConfig','UserPW',MimeEncodeString(Trim(edPasswd2.Text)));
  ini_fun.WriteString('EVENTDBConfig','DBNAME',Trim(edDataBaseName2.Text));
  ini_fun.free;

  result := True;
end;

function TfmDataBaseConfig.SaveLOGMDB: Boolean;
var
  ini_fun : TiniFile;
  stExeFolder : String;
begin
{  result := False;

  if ed_MdbName3.Text = '' then
  begin
    showmessage('DB 파일 위치를 확인하세요..');
    PageControl1.ActivePageIndex := 2;
    ed_MdbName3.SetFocus;
    Exit;
  End;

  stExeFolder  := ExtractFileDir(Application.ExeName);
  ini_fun := TiniFile.Create(stExeFolder + '\Zmos.INI');

  ini_fun.WriteString('LOGDBConfig','TYPE','MDB');
  ini_fun.WriteString('LOGDBConfig','UserPW',MimeEncodeString(Trim(ed_mdbPass3.Text)));
  ini_fun.WriteString('LOGDBConfig','DBNAME',Trim(ed_MdbName3.Text));
  ini_fun.free;

  result := True; }
end;

function TfmDataBaseConfig.SaveLOGMSSQL: Boolean;
var
  ini_fun : TiniFile;
  stExeFolder : String;
begin
  result := False;

  if edServerIP3.Text = '' then
  begin
    showmessage('서버IP가 비어 있습니다.');
    PageControl1.ActivePageIndex := 2;
    edServerIP3.SetFocus;
    Exit;
  End;

  if edServerPort3.Text = '' then
  begin
    showmessage('서버Port가 비어 있습니다.');
    PageControl1.ActivePageIndex := 2;
    edServerPort3.SetFocus;
    Exit;
  end;

  if edUserID2.Text = '' then
  begin
    showmessage('서버계정이 비어 있습니다.');
    PageControl1.ActivePageIndex := 2;
    edUserID3.SetFocus;
    Exit;
  end;

  if edPasswd3.Text = '' then
  begin
    showmessage('서버계정의 비밀번호가 비어 있습니다.');
    PageControl1.ActivePageIndex := 2;
    edPasswd3.SetFocus;
    Exit;
  end;

  stExeFolder  := ExtractFileDir(Application.ExeName);
  ini_fun := TiniFile.Create(stExeFolder + '\Zmos.INI');

//  ini_fun.WriteString('DBConfig','TYPE','MSSQL');
  ini_fun.WriteString('LOGDBConfig','Host',Trim(edServerIP3.Text));
  ini_fun.WriteString('LOGDBConfig','Port',Trim(edServerPort3.Text));
  ini_fun.WriteString('LOGDBConfig','UserID',Trim(edUserID3.Text));
  ini_fun.WriteString('LOGDBConfig','UserPW',MimeEncodeString(Trim(edPasswd3.Text)));
  ini_fun.WriteString('LOGDBConfig','DBNAME',Trim(edDataBaseName3.Text));
  ini_fun.free;

  result := True;
end;

procedure TfmDataBaseConfig.chk_CopyClick(Sender: TObject);
begin
  if chk_Copy.Checked then
  begin
    edServerIP2.Text := edServerIP1.text;
    edServerIP3.Text := edServerIP1.Text;
    edServerPort2.Text := edServerPort1.Text;
    edServerPort3.Text := edServerPort1.Text;
    edUserid2.Text := edUserid1.Text;
    edUserid3.Text := edUserid1.Text;
    edPasswd2.Text := edPasswd1.Text;
    edPasswd3.Text := edPasswd1.Text;
    edDataBaseName2.Text := edDataBaseName1.Text;
    edDataBaseName3.Text := edDataBaseName1.Text;
  end;
end;

function TDataBaseConfig.Table100VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_ATWORKTYPE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_ATWORKTYPE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_ATWORKTYPE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_ATWORKTYPE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','100');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table101VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ZONEDEVICENameChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ZONEDEVICENameChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ZONEDEVICENameChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ZONEDEVICENameChange
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','101');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table102VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','AUTOFCARD','N');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','102');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table103VersionMake: Boolean;
var
  stSql : string;
  stTemp : string;
  stAW_YESTERDAYTIME : string; //어제 날짜 기준 시간
  stAW_SATURDAYTYPE : string; //토요일근무타입 0:공휴일,1:반휴일,2:평일
  stAW_DEVICETYPE : string; //0:업데이트,1:카드리더,2:버튼방식
  stAW_FIXATTYPE : string; //0:정상,1:전직원 정상 출퇴근
  stAW_NOTBACKUPTYPE : string; //0:미복귀시조퇴처리,1:미복귀시정상퇴근
  stAW_ATSTARTBUTTON : string; //출근조작버튼
  stAW_ATOFFBUTTON : string; //퇴근조작버튼
  stAW_INOUTDEVICETYPE : string; //외출-0:사용안함,1:리더,2:버튼
  stAW_WORKOUTBUTTON : string; //외출조작버튼
  stAW_WORKINBUTTON : string; //복귀조작버튼
begin
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','103');
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'select * from TB_CONFIG ';
  stSql := stSql + ' where CO_CONFIGGROUP = ''ATTEND'' ';

  with DataModule1.ADOQuery do
  begin
    Close;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;

    While Not Eof do
    begin
      if FindField('CO_CONFIGCODE').AsString = 'ATTYPE1' then stAW_FIXATTYPE := FindField('CO_CONFIGVALUE').AsString
      else if FindField('CO_CONFIGCODE').AsString = 'DEVICETYPE' then stAW_DEVICETYPE := FindField('CO_CONFIGVALUE').AsString
      else if FindField('CO_CONFIGCODE').AsString = 'INOUTTYPE' then stAW_INOUTDEVICETYPE := FindField('CO_CONFIGVALUE').AsString
      else if FindField('CO_CONFIGCODE').AsString = 'OFFWKNO' then stAW_ATOFFBUTTON := FindField('CO_CONFIGVALUE').AsString
      else if FindField('CO_CONFIGCODE').AsString = 'OUTTIME' then stAW_NOTBACKUPTYPE := FindField('CO_CONFIGVALUE').AsString
      else if FindField('CO_CONFIGCODE').AsString = 'SATURDAY' then stAW_SATURDAYTYPE := FindField('CO_CONFIGVALUE').AsString
      else if FindField('CO_CONFIGCODE').AsString = 'STARTWKNO' then stAW_ATSTARTBUTTON := FindField('CO_CONFIGVALUE').AsString
      else if FindField('CO_CONFIGCODE').AsString = 'WKINNO' then stAW_WORKINBUTTON := FindField('CO_CONFIGVALUE').AsString
      else if FindField('CO_CONFIGCODE').AsString = 'WKOUTNO' then stAW_WORKOUTBUTTON := FindField('CO_CONFIGVALUE').AsString
      else if FindField('CO_CONFIGCODE').AsString = 'YTIME' then stAW_YESTERDAYTIME := FindField('CO_CONFIGVALUE').AsString ;

      Next;
    end;
    stTemp := 'Insert Into TB_ATWORKTYPE(';
    stTemp := stTemp + 'AW_CODE,';
    stTemp := stTemp + 'AW_NAME,';
    stTemp := stTemp + 'AW_YESTERDAYTIME,';
    stTemp := stTemp + 'AW_SATURDAYTYPE,';
    stTemp := stTemp + 'AW_DEVICETYPE,';
    stTemp := stTemp + 'AW_FIXATTYPE,';
    stTemp := stTemp + 'AW_NOTBACKUPTYPE,';
    stTemp := stTemp + 'AW_ATSTARTBUTTON,';
    stTemp := stTemp + 'AW_ATOFFBUTTON,';
    stTemp := stTemp + 'AW_INOUTDEVICETYPE,';
    stTemp := stTemp + 'AW_WORKOUTBUTTON,';
    stTemp := stTemp + 'AW_WORKINBUTTON) ';
    stTemp := stTemp + ' Values( ';
    stTemp := stTemp + '''001'',';
    stTemp := stTemp + '''상시근무자'',';
    stTemp := stTemp + '''' + stAW_YESTERDAYTIME + ''',';
    stTemp := stTemp + '''' + stAW_SATURDAYTYPE + ''',';
    stTemp := stTemp + '''' + stAW_DEVICETYPE + ''',';
    stTemp := stTemp + '''' + stAW_FIXATTYPE + ''',';
    stTemp := stTemp + '''' + stAW_NOTBACKUPTYPE + ''',';
    stTemp := stTemp + '''' + stAW_ATSTARTBUTTON + ''',';
    stTemp := stTemp + '''' + stAW_ATOFFBUTTON + ''',';
    stTemp := stTemp + '''' + stAW_INOUTDEVICETYPE + ''',';
    stTemp := stTemp + '''' + stAW_WORKOUTBUTTON + ''',';
    stTemp := stTemp + '''' + stAW_WORKINBUTTON + ''') ';

    DataModule1.ProcessExecSQL(stTemp);
  end;
end;

function TDataBaseConfig.Table104VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATCODEAWCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATCODEAWCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATCODEAWCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATCODEAWCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Update TB_ATCODE set AW_CODE = ''001'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','104');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table105VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEEAWCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEEAWCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEEAWCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEEAWCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Update TB_EMPLOYEE set AW_CODE = ''001'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','105');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table106VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMSTATUSCODEGubun_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMSTATUSCODEGubun_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMSTATUSCODEGubun_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMSTATUSCODEGubun_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMSTATUSCODEColor_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMSTATUSCODEColor_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMSTATUSCODEColor_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMSTATUSCODEColor_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Update TB_ALARMSTATUSCODE set AL_COLOR = 255 ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Update TB_ALARMSTATUSCODE set AL_GUBUN = ''001'' ';
  stSql := stSql + ' Where AL_ALARMSTATUSCODE = ''00'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''AF'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''AN'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''BF'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''CC'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''CO'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''CR'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''DF'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''DL'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''DR'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''E1'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''EM'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''FI'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''G1'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''NF'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''NR'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''PF'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''Q1'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''S1'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''S2'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''S3'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''S4'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''S5'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''S6'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''S7'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''S8'' ';
  stSql := stSql + ' OR AL_ALARMSTATUSCODE = ''WR'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Update TB_ALARMSTATUSCODE set AL_GUBUN = ''002'' ';
  stSql := stSql + ' Where AL_GUBUN IS NULL ';
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','106');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table107VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_CURRENTDAEMON(CU_STATECODE,CU_STATEVALUE) ';
  stSql := stSql + ' Values(''FDMSCARD'',''30'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','107');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table108VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_FIREGUBUN
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_FIREGUBUN
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_FIREGUBUN
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FIREGUBUN
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_FIREGUBUN(FG_CODE,FG_NAME) ';
  stSql := stSql + ' Values(''001'',''전체'')';
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_FIREGROUP
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_FIREGROUP
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_FIREGROUP
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FIREGROUP
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert into TB_FIREGROUP select ''1234567890'',AC_NODENO,''001'' from TB_ACCESSDEVICE where AC_ECUID = ''00'' ';
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','108');
  DataModule1.ProcessExecSQL(stSql);

end;


function TDataBaseConfig.Table109VersionMake: Boolean;
var
  stSql : string;
begin

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATCODEATOUTRANGE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATCODEATOUTRANGE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATCODEATOUTRANGE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATCODEATOUTRANGE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATCODEATSOUTRANGE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATCODEATSOUTRANGE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATCODEATSOUTRANGE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATCODEATSOUTRANGE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATCODEATHOUTRANGE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATCODEATHOUTRANGE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATCODEATHOUTRANGE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATCODEATHOUTRANGE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','109');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table10VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_DEVICECARDNOACKINDEX
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_DEVICECARDNOACKINDEX
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_DEVICECARDNOACKINDEX
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_DEVICECARDNOACKINDEX
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISCARDNO
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISCARDNO
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISCARDNO
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISCARDNO
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISCARDTYPE
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISCARDTYPE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISCARDTYPE
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISCARDTYPE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISSENDSTATUS2
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISSENDSTATUS2
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISSENDSTATUS2
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISSENDSTATUS2
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','10');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table110VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_ATLISTEVENT
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_ATLISTEVENT
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_ATLISTEVENT
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_ATLISTEVENT
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','110');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table111VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_CLIENTSOCK
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_CLIENTSOCK
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_CLIENTSOCK
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_CLIENTSOCK
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','CSOCKTYPE','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','111');
  DataModule1.ProcessExecSQL(stSql);

end;


function TDataBaseConfig.Table112VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICE_JAVARATYPEAdd
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ACCESSDEVICE_JAVARATYPEAdd
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICE_JAVARATYPEAdd
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICE_JAVARATYPEAdd
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Update TB_ACCESSDEVICE set AC_JAVARATYPE = ''0'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','112');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table113VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := ' Update TB_NOTCARDALARMCODE set AL_STATUSCODE2 = ''A2'' ';
  stSql := stsql + ' Where AN_CODE = ''006'' ';
  stSql := stSql + ' AND AN_MODE = ''a'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Update TB_NOTCARDALARMCODE set AL_STATUSCODE2 = ''C2'' ';
  stSql := stsql + ' Where AN_CODE = ''006'' ';
  stSql := stSql + ' AND AN_MODE = ''d'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','113');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table114VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_JAVARASCHEDULE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_JAVARASCHEDULE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_JAVARASCHEDULE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_JAVARASCHEDULE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','114');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table115VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICE_CARDTYPEChange
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICE_CARDTYPEChange
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICE_CARDTYPEChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICE_CARDTYPEChange
  else Exit;
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','115');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table116VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEE_COPHONEChange
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEE_COPHONEChange
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEE_COPHONEChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEE_COPHONEChange
  else Exit;
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','116');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table117VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','LONGTIMECD','');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','117');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table118VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_INOUTREADERGROUP
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_INOUTREADERGROUP
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_INOUTREADERGROUP
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_INOUTREADERGROUP
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_READER_INOUTGROUPCODEAdd
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_READER_INOUTGROUPCODEAdd
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_READER_INOUTGROUPCODEAdd
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_READER_INOUTGROUPCODEAdd
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','118');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table119VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_INOUTGROUPLIST
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_INOUTGROUPLIST
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_INOUTGROUPLIST
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_INOUTGROUPLIST
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','119');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table11VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_WORKLOG
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_WORKLOG
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_WORKLOG
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_WORKLOG
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','11');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table120VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_INOUTGROUPLISTNodeNo_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_INOUTGROUPLISTNodeNo_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_INOUTGROUPLISTNodeNo_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_INOUTGROUPLISTNodeNo_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_INOUTGROUPLISTEcuID_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_INOUTGROUPLISTEcuID_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_INOUTGROUPLISTEcuID_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_INOUTGROUPLISTEcuID_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_INOUTGROUPLISTReaderNo_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_INOUTGROUPLISTReaderNo_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_INOUTGROUPLISTReaderNo_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_INOUTGROUPLISTReaderNo_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_INOUTGROUPLISTTime_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_INOUTGROUPLISTTime_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_INOUTGROUPLISTTime_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_INOUTGROUPLISTTime_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','120');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table121VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','ASCIITOHEX','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','121');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table122VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_DEVICECARDGROUPCODE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_DEVICECARDGROUPCODE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_DEVICECARDGROUPCODE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_DEVICECARDGROUPCODE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEE_DGCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEE_DGCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEE_DGCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEE_DGCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEE_DGAPPLY_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEE_DGAPPLY_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEE_DGAPPLY_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEE_DGAPPLY_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','122');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table123VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_PERMITCODE(';
  stSql := stSql + 'PE_PERMITCODE,';
  stSql := stSql + 'PE_PERMITNAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''G'',';
  stSql := stSql + '''근태'')';
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','123');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table124VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_ACCESSINPUTTYPE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_ACCESSINPUTTYPE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_ACCESSINPUTTYPE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_ACCESSINPUTTYPE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_ACCESSINPUTTYPE(';
  stSql := stSql + 'AC_INPUTTYPE,';
  stSql := stSql + 'AC_INPUTTYPENAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''R'',';
  stSql := stSql + '''원격제어'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_ACCESSINPUTTYPE(';
  stSql := stSql + 'AC_INPUTTYPE,';
  stSql := stSql + 'AC_INPUTTYPENAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''P'',';
  stSql := stSql + '''전화'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_ACCESSINPUTTYPE(';
  stSql := stSql + 'AC_INPUTTYPE,';
  stSql := stSql + 'AC_INPUTTYPENAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''B'',';
  stSql := stSql + '''퇴실버튼'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_ACCESSINPUTTYPE(';
  stSql := stSql + 'AC_INPUTTYPE,';
  stSql := stSql + 'AC_INPUTTYPENAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''S'',';
  stSql := stSql + '''스케줄'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_ACCESSINPUTTYPE(';
  stSql := stSql + 'AC_INPUTTYPE,';
  stSql := stSql + 'AC_INPUTTYPENAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''F'',';
  stSql := stSql + '''화재발생'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_ACCESSINPUTTYPE(';
  stSql := stSql + 'AC_INPUTTYPE,';
  stSql := stSql + 'AC_INPUTTYPENAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''f'',';
  stSql := stSql + '''원격화재'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_ACCESSINPUTTYPE(';
  stSql := stSql + 'AC_INPUTTYPE,';
  stSql := stSql + 'AC_INPUTTYPENAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''C'',';
  stSql := stSql + '''카드'')';
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_DOORPOSICODE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_DOORPOSICODE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_DOORPOSICODE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_DOORPOSICODE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_DOORPOSICODE(';
  stSql := stSql + 'AC_DOORPOSI,';
  stSql := stSql + 'AC_DOORPOSINAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''0'',';
  stSql := stSql + '''내부'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_DOORPOSICODE(';
  stSql := stSql + 'AC_DOORPOSI,';
  stSql := stSql + 'AC_DOORPOSINAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''1'',';
  stSql := stSql + '''외부'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','124');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table125VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMEVENT_COMPANYCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMEVENT_COMPANYCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMEVENT_COMPANYCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMEVENT_COMPANYCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMEVENT_EMCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMEVENT_EMCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMEVENT_EMCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMEVENT_EMCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','125');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table126VersionMake: Boolean;
var
  stSql : string;
begin
  Exit;
  //현재 TB_DEVICETYPE 은 미사용 ==> TB_ALARMDEVICETYPE 테이블을 사용중
  stSql := 'Insert Into TB_DEVICETYPE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' DE_DEVICETYPE,';
  stSql := stSql + ' DE_TYPENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''MN'', ';
  stSql := stSql + '''메인컨트롤러'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_DEVICETYPE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' DE_DEVICETYPE,';
  stSql := stSql + ' DE_TYPENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''EX'', ';
  stSql := stSql + '''로컬컨트롤러'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_DEVICETYPE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' DE_DEVICETYPE,';
  stSql := stSql + ' DE_TYPENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''JP'', ';
  stSql := stSql + '''조작표시기'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_DEVICETYPE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' DE_DEVICETYPE,';
  stSql := stSql + ' DE_TYPENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''PT'', ';
  stSql := stSql + '''포인터'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_DEVICETYPE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' DE_DEVICETYPE,';
  stSql := stSql + ' DE_TYPENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''PR'', ';
  stSql := stSql + '''프린터'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_DEVICETYPE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' DE_DEVICETYPE,';
  stSql := stSql + ' DE_TYPENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''PC'', ';
  stSql := stSql + '''PC'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_DEVICETYPE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' DE_DEVICETYPE,';
  stSql := stSql + ' DE_TYPENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''AD'', ';
  stSql := stSql + '''알람표시기'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_DEVICETYPE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' DE_DEVICETYPE,';
  stSql := stSql + ' DE_TYPENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''WR'', ';
  stSql := stSql + '''무선중계기'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_DEVICETYPE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' DE_DEVICETYPE,';
  stSql := stSql + ' DE_TYPENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''CD'', ';
  stSql := stSql + '''카드리더'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','126');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table127VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_ALARMMODECODE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AL_ALARMMODECODE,';
  stSql := stSql + ' AL_ALARMMODENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''a'', ';
  stSql := stSql + '''경계모드'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_ALARMMODECODE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AL_ALARMMODECODE,';
  stSql := stSql + ' AL_ALARMMODENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''A'', ';
  stSql := stSql + '''경계모드'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_ALARMMODECODE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AL_ALARMMODECODE,';
  stSql := stSql + ' AL_ALARMMODENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''d'', ';
  stSql := stSql + '''해제모드'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_ALARMMODECODE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AL_ALARMMODECODE,';
  stSql := stSql + ' AL_ALARMMODENAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''D'', ';
  stSql := stSql + '''해제모드'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','127');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table128VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_LOCATION_TOTWIDTH_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_LOCATION_TOTWIDTH_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_LOCATION_TOTWIDTH_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_LOCATION_TOTWIDTH_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_LOCATION_TOTHEIGHT_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_LOCATION_TOTHEIGHT_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_LOCATION_TOTHEIGHT_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_LOCATION_TOTHEIGHT_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_LOCATION_CURX_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_LOCATION_CURX_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_LOCATION_CURX_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_LOCATION_CURX_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_LOCATION_CURY_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_LOCATION_CURY_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_LOCATION_CURY_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_LOCATION_CURY_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','125');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table129VersionMake: Boolean;
var
  stSql : string;
begin
  //tb_employee 이름 사이즈 변경
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEEEmNameChange('100')
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_EMPLOYEEEmNameChange('100')
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEEEmNameChange('100')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEEEmNameChange('100')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','129');
  DataModule1.ProcessExecSQL(stSql);  
end;

function TDataBaseConfig.Table12VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := ' update TB_CONFIG set CO_CONFIGVALUE = ''DOOR'' ';
  stSql := stSql + ' Where CO_CONFIGGROUP = ''MOSTYPE'' ';
  stSql := stSql + ' AND CO_CONFIGCODE = ''FOODDEVICE'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','12');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table130VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('CARD','READERTYPE','0','0.일반,1.스마트원');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','130');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table131VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISEMNAME_Change(100)
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISEMNAME_Change(100)
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISEMNAME_Change(100)
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISEMNAME_Change(100)
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','131');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table132VersionMake: Boolean;
var
  stSql : string;
  TempAdoQuery : TADOQuery;
begin
  stSql := 'update tb_alarmdevice set al_zonenum = ''00'' '; //AL_ZONENUM 에 데이터 값을 입력 한다
  DataModule1.ProcessExecSQL(stSql);
  stSql := 'update TB_ALARMEVENT set AL_ZONECODE = ''00'' where AL_ZONECODE is null '; //AL_ZONECODE 에 데이터 값을 입력 한다
  DataModule1.ProcessExecSQL(stSql);
  if DBTYPE = 'MDB' then
  begin
    stSql := 'alter table tb_alarmdevice alter column al_zonenum  text(2) NOT NULL ';
    DataModule1.ProcessExecSQL(stSql);
  end else if DBTYPE = 'PG' then
  begin
    stSql := 'ALTER TABLE tb_alarmdevice ALTER COLUMN al_zonenum SET DEFAULT ''00'' ';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'ALTER TABLE tb_alarmdevice ALTER COLUMN al_zonenum SET NOT NULL ';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'alter table tb_alarmdevice drop constraint tb_alarmdevice_pkey ';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'alter table tb_alarmdevice add primary key (GROUP_CODE,AC_NODENO,AC_ECUID,AL_ZONENUM)';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'ALTER TABLE TB_ALARMEVENT ALTER COLUMN AL_ZONECODE SET DEFAULT ''00'' ';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'ALTER TABLE TB_ALARMEVENT ALTER COLUMN AL_ZONECODE SET NOT NULL ';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'alter table TB_ALARMEVENT drop constraint tb_alarmevent_pkey ';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'alter table TB_ALARMEVENT add primary key (group_code, al_date, al_time, ac_nodeno, ac_ecuid, al_msgno,AL_ZONECODE)';
    DataModule1.ProcessExecSQL(stSql);
  end else if DBTYPE = 'MSSQL' then
  begin
    stSql := 'alter table tb_alarmdevice alter column al_zonenum varchar(2) NOT NULL ';
    DataModule1.ProcessExecSQL(stSql);
    stSql := ' ALTER TABLE TB_ALARMDEVICE ADD DEFAULT ''00'' FOR AL_ZONENUM ';
    DataModule1.ProcessExecSQL(stSql);

    stSql := 'alter table TB_ALARMEVENT alter column AL_ZONECODE varchar(2) NOT NULL ';
    DataModule1.ProcessExecSQL(stSql);
    stSql := ' ALTER TABLE TB_ALARMEVENT ADD DEFAULT ''00'' FOR AL_ZONECODE ';
    DataModule1.ProcessExecSQL(stSql);

    Try
      CoInitialize(nil);
      TempAdoQuery := TADOQuery.Create(nil);
      TempAdoQuery.Connection := DataModule1.ADOConnection;
      TempAdoQuery.DisableControls;
      with TempAdoQuery do
      begin
        Close;
        //sql.Clear;
        stSql := 'select name from sysobjects where xtype= ''PK'' and parent_obj=(select id from sysobjects where name = ''tb_alarmdevice'')';
        sql.Text := stSql;
        Try
          Open;
        Except
          //에러처리
          Exit;
        End;
        if recordcount > 0 then
        begin
          stSql := 'ALTER TABLE tb_alarmdevice DROP ' + FindField('name').AsString ;
          DataModule1.ProcessExecSQL(stSql);
        end;
        Close;
        //sql.Clear;
        stSql := 'select name from sysobjects where xtype= ''PK'' and parent_obj=(select id from sysobjects where name = ''tb_alarmevent'')';
        sql.Text := stSql;
        Try
          Open;
        Except
          //에러처리
          Exit;
        End;
        if recordcount > 0 then
        begin
          stSql := 'ALTER TABLE tb_alarmevent DROP ' + FindField('name').AsString ;
          DataModule1.ProcessExecSQL(stSql);
        end;
      End;
    Finally
      TempAdoQuery.EnableControls;
      TempAdoQuery.Free;
      CoUninitialize;
    End;
    stSql := 'alter table tb_alarmdevice add constraint PK_alarmdevice PRIMARY KEY (GROUP_CODE,AC_NODENO,AC_ECUID,AL_ZONENUM)';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'alter table tb_alarmevent add constraint PK_alarmevent PRIMARY KEY (group_code, al_date, al_time, ac_nodeno, ac_ecuid, al_msgno,AL_ZONECODE)';
    DataModule1.ProcessExecSQL(stSql);
  end else if DBTYPE = 'FB' then
  begin
    stSql := 'alter table tb_alarmdevice alter column  al_zonenum varchar(2) DEFAULT ''00''  NOT NULL ';
    DataModule1.ProcessExecSQL(stSql);
  end else Exit;
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','132');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table133VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISEMNAME_Change(100)
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISEMNAME_Change(100)
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISEMNAME_Change(100)
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISEMNAME_Change(100)
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','133');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table134VersionMake: Boolean;
var
  stSql : string;
begin

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_TIMECODE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_TIMECODE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_TIMECODE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_TIMECODE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_TIMECODEDEVICE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_TIMECODEDEVICE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_TIMECODEDEVICE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_TIMECODEDEVICE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOORTIMECODEUSE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOORTIMECODEUSE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DOORTIMECODEUSE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOORTIMECODEUSE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEETIMECODEUSE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEETIMECODEUSE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEETIMECODEUSE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEETIMECODEUSE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEETIMEGROUP_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEETIMEGROUP_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEETIMEGROUP_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEETIMEGROUP_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEETIME1_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEETIME1_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEETIME1_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEETIME1_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEETIME2_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEETIME2_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEETIME2_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEETIME2_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEETIME3_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEETIME3_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEETIME3_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEETIME3_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEETIME4_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEETIME4_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEETIME4_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEETIME4_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEEWEEKCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEEWEEKCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEEWEEKCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEEWEEKCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_TIMECODE_Value('0','00000000','00000000','00000000','00000000','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_TIMECODE_Value('1','00000000','00000000','00000000','00000000','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','134');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table135VersionMake: Boolean;
var
  stSql : string;
begin

{  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICETimeCodeSend_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICETimeCodeSend_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICETimeCodeSend_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICETimeCodeSend_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
}
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICETIMECODEASYNC_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICETIMECODEASYNC_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICETIMECODEASYNC_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICETIMECODEASYNC_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','135');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table136VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','TIMEUSE','FALSE','타임코드 사용유무');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','136');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table137VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FDMSDEPART','FALSE','부서코드연동유무');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','137');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table138VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNODoor3_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNODoor3_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNODoor3_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNODoor3_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNODoor4_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNODoor4_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNODoor4_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNODoor4_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNODoor5_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNODoor5_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNODoor5_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNODoor5_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNODoor6_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNODoor6_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNODoor6_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNODoor6_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNODoor7_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNODoor7_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNODoor7_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNODoor7_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNODoor8_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNODoor8_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNODoor8_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNODoor8_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOAlarm0_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOAlarm0_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOAlarm0_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOAlarm0_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOAlarm1_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOAlarm1_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOAlarm1_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOAlarm1_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOAlarm2_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOAlarm2_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOAlarm2_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOAlarm2_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOAlarm3_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOAlarm3_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOAlarm3_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOAlarm3_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOAlarm4_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOAlarm4_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOAlarm4_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOAlarm4_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOAlarm5_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOAlarm5_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOAlarm5_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOAlarm5_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOAlarm6_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOAlarm6_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOAlarm6_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOAlarm6_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOAlarm7_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOAlarm7_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOAlarm7_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOAlarm7_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOAlarm8_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOAlarm8_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOAlarm8_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOAlarm8_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','138');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table139VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATEVENT_JIJUMCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATEVENT_JIJUMCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATEVENT_JIJUMCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATEVENT_JIJUMCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATEVENT_DEPARTCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATEVENT_DEPARTCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATEVENT_DEPARTCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATEVENT_DEPARTCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATEVENT_EMNAME_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATEVENT_EMNAME_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATEVENT_EMNAME_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATEVENT_EMNAME_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_FOODEVENT_JIJUMCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_FOODEVENT_JIJUMCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_FOODEVENT_JIJUMCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_FOODEVENT_JIJUMCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_FOODEVENT_DEPARTCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_FOODEVENT_DEPARTCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_FOODEVENT_DEPARTCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_FOODEVENT_DEPARTCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_FOODEVENT_EMNAME_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_FOODEVENT_EMNAME_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_FOODEVENT_EMNAME_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_FOODEVENT_EMNAME_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','139');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table13VersionMake: Boolean;
var
  stSql : string;
begin

  stSql := 'ALTER TABLE TB_CARD ADD POSITIONNUM INTEGER ';
  if DBType <> 'FB' then stSql := stSql + ' NULL ';
  DataModule1.ProcessExecSQL(stSql);

  PositionNumberValueSetting;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','13');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table140VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.UpdateTB_ATEVENT_EmInfo
  else if DBTYPE = 'PG' then stSql := PostGreSql.UpdateTB_ATEVENT_EmInfo
  else if DBTYPE = 'MSSQL' then stSql := MSSql.UpdateTB_ATEVENT_EmInfo
  else if DBTYPE = 'FB' then stSql := FireBird.UpdateTB_ATEVENT_EmInfo
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.UpdateTB_FOODEVENT_EmInfo
  else if DBTYPE = 'PG' then stSql := PostGreSql.UpdateTB_FOODEVENT_EmInfo
  else if DBTYPE = 'MSSQL' then stSql := MSSql.UpdateTB_FOODEVENT_EmInfo
  else if DBTYPE = 'FB' then stSql := FireBird.UpdateTB_FOODEVENT_EmInfo
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','140');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table141VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','COMPANYCD','1','0:회사코드미사용,1:회사코드사용');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','141');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table142VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATDAYSUMMARY_JIJUMCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATDAYSUMMARY_JIJUMCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATDAYSUMMARY_JIJUMCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATDAYSUMMARY_JIJUMCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATDAYSUMMARY_DEPARTCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATDAYSUMMARY_DEPARTCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATDAYSUMMARY_DEPARTCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATDAYSUMMARY_DEPARTCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATDAYSUMMARY_EMNAME_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATDAYSUMMARY_EMNAME_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATDAYSUMMARY_EMNAME_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATDAYSUMMARY_EMNAME_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATMONTHSUMMARY_JIJUMCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATMONTHSUMMARY_JIJUMCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATMONTHSUMMARY_JIJUMCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATMONTHSUMMARY_JIJUMCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATMONTHSUMMARY_DEPARTCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATMONTHSUMMARY_DEPARTCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATMONTHSUMMARY_DEPARTCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATMONTHSUMMARY_DEPARTCODE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATMONTHSUMMARY_EMNAME_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATMONTHSUMMARY_EMNAME_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATMONTHSUMMARY_EMNAME_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATMONTHSUMMARY_EMNAME_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','142');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table143VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_TIMECODEDEVICE_EACHCHANGE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_TIMECODEDEVICE_EACHCHANGE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_TIMECODEDEVICE_EACHCHANGE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_TIMECODEDEVICE_EACHCHANGE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','143');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table144VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MSSQL' then
  begin
    stSql := 'CREATE VIEW Vw_SMS_EVENT AS ';
    stSql := stSql + 'select b.AL_ZONENAME as v_DEVICE_NAME, ' ;
    stSql := stSql + ' c.AL_ZONENAME as v_ZONE_NAME, ' ;
    stSql := stSql + ' a.AL_ALARMSTATUSCODE as v_EVENT_CODE, ' ;
    stSql := stSql + ' a.AL_ZONESTATE as v_ZONE_STATE, ' ;
    stSql := stSql + ' d.AL_ALARMNAME as v_EVENT_NAME, ' ;
    stSql := stSql + ' a.AL_DATE + a.AL_TIME  as v_EVENT_TIME, ' ;
    stSql := stSql + ' a.AL_INPUTTIME  as v_INSERT_TIME,';
    stSql := stSql + ' a.AL_MSGNO as v_MSG_ID ' ;
    stSql := stSql + ' FROM TB_ALARMEVENT a  ' ;
    stSql := stSql + ' Left Join TB_ALARMDEVICE b  ' ;
    stSql := stSql + ' ON(a.GROUP_CODE = b.GROUP_CODE ';
    stSql := stSql + ' AND a.AC_NODENO = b.AC_NODENO ';
    stSql := stSql + ' AND a.AC_ECUID = b.AC_ECUID) ';
    stSql := stSql + ' Left Join TB_ZONEDEVICE c  ' ;
    stSql := stSql + ' ON(a.GROUP_CODE = c.GROUP_CODE ';
    stSql := stSql + ' AND a.AC_NODENO = c.AC_NODENO ';
    stSql := stSql + ' AND a.AC_ECUID = c.AC_ECUID ';
    stSql := stSql + ' AND a.AL_ZONENO = ''0'' + c.AL_ZONENUM )';
    stSql := stSql + ' Left Join TB_ALARMSTATUSCODE d  ' ;
    stSql := stSql + ' ON(a.GROUP_CODE = d.GROUP_CODE ';
    stSql := stSql + ' AND a.AL_ALARMSTATUSCODE = d.AL_ALARMSTATUSCODE )';

    DataModule1.ProcessExecSQL(stSql);

    stSql := 'CREATE LOGIN [sms] WITH PASSWORD=N''smspass'', DEFAULT_DATABASE=[zmos], CHECK_EXPIRATION=OFF, CHECK_POLICY=OFF';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'CREATE USER [sms] FOR LOGIN [sms]';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'ALTER LOGIN [sms] WITH PASSWORD=N''smspass''';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'EXEC sp_addrolemember ''db_datareader'', ''sms''';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'EXEC sp_addrolemember ''db_accessadmin'', ''sms''';
    DataModule1.ProcessExecSQL(stSql);
    stSql := 'GRANT SELECT ON [dbo].[Vw_SMS_EVENT] TO [sms] ' ;
    DataModule1.ProcessExecSQL(stSql);
  end;
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','144');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table146VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FACECOP','0','FACECOP연동유무');
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_CARDTYPE(CT_CODE,CT_NAME) ';
  stSql := stSql + ' VALUES(''3'',''FACECOP'')';
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_FACECOP
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_FACECOP
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_FACECOP
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FACECOP
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','146');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table147VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATLISTEVENTCARDNOChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ATLISTEVENTCARDNOChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATLISTEVENTCARDNOChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATLISTEVENTCARDNOChange
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','147');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table148VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICETIMETYPE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ACCESSDEVICETIMETYPE_Add
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICETIMETYPE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICETIMETYPE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','148');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table149VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','REALREPORT','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','149');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table14VersionMake: Boolean;
var
  stSql : string;
begin
  {
  stSql := 'Insert Into TB_PROGRAMID( ';
  stSql := stSql + 'GROUP_CODE,';
  stSql := stSql + 'PR_PROGRAMID,';
  stSql := stSql + 'PR_GROUPCODE,';
  stSql := stSql + 'PR_PROGRAMNAME,';
  stSql := stSql + 'PR_VISIBLE,';
  stSql := stSql + 'PR_VIEWSEQ,';
  stSql := stSql + 'CO_CONFIGCODE,';
  stSql := stSql + 'PR_ENABLE';
  stSql := stSql + ') ';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''' + GROUPCODE + ''',';
  stSql := stSql + '''ATC04r'',';
  stSql := stSql + '''ATREPORT'',';
  stSql := stSql + '''실시간근태보고서'',';
  stSql := stSql + '''Y'',';
  stSql := stSql + '23,';
  stSql := stSql + '''ATTEND'',';
  stSql := stSql + '''Y'')';


  DataModule1.ProcessExecSQL(stSql); }
  stSql := commonSql.InsertIntoTB_PROGRAMID('ATC04r','ATREPORT','실시간근태보고서','Y','23','ATTEND','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','14');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table150VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_FORMNAME('107','핸드폰','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('108','주소1','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('109','주소2','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','150');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table151VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FDMSDELETE','FALSE');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','151');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table152VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('FOOD','YSDAYTIME','0400');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','152');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table153VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','EVENTDB','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','153');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table154VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ZONEDEVICE_View_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ZONEDEVICE_View_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ZONEDEVICE_View_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ZONEDEVICE_View_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Update TB_ZONEDEVICE set AL_VIEW =''Y'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','154');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table155VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','EVENTSTART','0000');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','EVENTEND','2459');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','155');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table156VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_CARD_MEMLOADAdd
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_CARD_MEMLOADAdd
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_CARD_MEMLOADAdd
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_CARD_MEMLOADAdd
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','156');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table157VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('MOSTYPE','PWLENUSE','FALSE');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('MOSTYPE','PWLENGTH','8');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('MOSTYPE','PWCHAR','FALSE');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('MOSTYPE','PWCHANGE','30');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','157');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table158VersionMake: Boolean;
var
  stSql : string;
  i  : integer;
  stArmAreaUse : string;
  stArmAreaName : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_ARMAREA
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_ARMAREA
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_ARMAREA
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_ARMAREA
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'select * from tb_alarmdevice ';
  with DataModule1.ADOTmpQuery do
  begin
    Close;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;
    if recordCount < 1 then Exit;
    While Not Eof do
    begin
      for i := 0 to 8 do
      begin
        if i = 0 then
        begin
          stArmAreaUse := 'Y';
          stArmAreaName := FindField('AL_ZONENAME').AsString;
        end else
        begin
          stArmAreaUse := 'N';
          stArmAreaName := FindField('AL_ZONENAME').AsString + '_' + inttostr(i) + '구역';
        end;

        stSql := CommonSql.InsertIntoTB_ARMAREA(inttostr(FindField('AC_NODENO').AsInteger),
                             FindField('AC_ECUID').AsString,
                             inttostr(i),
                             stArmAreaName,
                             stArmAreaUse,
                             '',
                             inttostr(FindField('AL_VIEWSEQ').AsInteger),
                             FindField('AL_LOCATEUSE').AsString,
                             inttostr(FindField('AL_TOTWIDTH').AsInteger),
                             inttostr(FindField('AL_TOTHEIGHT').AsInteger),
                             inttostr(FindField('AL_CURX').AsInteger),
                             inttostr(FindField('AL_CURY').AsInteger),
                             FindField('LO_DONGCODE').AsString,
                             FindField('LO_FLOORCODE').AsString,
                             FindField('LO_AREACODE').AsString);

        DataModule1.ProcessExecSQL(stSql);
      end;
      Next;
    end;
  end;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','158');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table159VersionMake: Boolean;
var
  stSql : string;
  i  : integer;
  stArmAreaUse : string;
  stArmAreaName : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_ADMINALARMAREA
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_ADMINALARMAREA
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_ADMINALARMAREA
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_ADMINALARMAREA
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'select * from TB_ADMINALARMDEVICE ';
  with DataModule1.ADOTmpQuery do
  begin
    Close;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;
//    if recordCount < 1 then Exit;
    While Not Eof do
    begin
      for i := 0 to 8 do
      begin
        stSql := CommonSql.InsertIntoTB_ADMINALARMAREA(FindField('AD_USERID').AsString,FindField('AC_NODENO').asstring,
                             FindField('AC_ECUID').AsString,
                             FillZeroNumber(i,2),
                             FindField('AD_GUBUN').AsString,
                             '',
                             '');

        DataModule1.ProcessExecSQL(stSql);
      end;
      Next;
    end;
  end;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','159');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table15VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_READERInOutCount
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_READERInOutCount
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_READERInOutCount
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_READERInOutCount
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_INOUTCOUNT
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_INOUTCOUNT
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_INOUTCOUNT
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_INOUTCOUNT
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_INOUTGROUP
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_INOUTGROUP
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_INOUTGROUP
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_INOUTGROUP
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('MOSTYPE','INOUTCOUNT','N');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','15');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table160VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ARMAREATelNo_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ARMAREATelNo_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ARMAREATelNo_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ARMAREATelNo_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ARMAREAmemo_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ARMAREAmemo_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ARMAREAmemo_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ARMAREAmemo_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'ALTER TABLE TB_ARMAREA ADD AG_ARMCODE varchar(3) DEFAULT ''000'' NULL ';
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.UpdateTB_ARMAREA_FromAlarmDeviceMemo
  else if DBTYPE = 'PG' then stSql := PostGreSql.UpdateTB_ARMAREA_FromAlarmDeviceMemo
  else if DBTYPE = 'MSSQL' then stSql := MSSql.UpdateTB_ARMAREA_FromAlarmDeviceMemo
  else if DBTYPE = 'FB' then stSql := FireBird.UpdateTB_ARMAREA_FromAlarmDeviceMemo
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','160');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table161VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEDoor3_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEDoor3_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEDoor3_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEDoor3_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEDoor4_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEDoor4_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEDoor4_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEDoor4_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEDoor5_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEDoor5_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEDoor5_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEDoor5_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEDoor6_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEDoor6_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEDoor6_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEDoor6_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEDoor7_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEDoor7_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEDoor7_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEDoor7_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEDoor8_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEDoor8_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEDoor8_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEDoor8_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEAlarm0_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEAlarm0_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEAlarm0_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEAlarm0_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEAlarm1_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEAlarm1_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEAlarm1_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEAlarm1_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEAlarm2_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEAlarm2_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEAlarm2_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEAlarm2_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEAlarm3_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEAlarm3_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEAlarm3_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEAlarm3_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEAlarm4_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEAlarm4_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEAlarm4_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEAlarm4_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEAlarm5_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEAlarm5_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEAlarm5_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEAlarm5_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEAlarm6_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEAlarm6_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEAlarm6_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEAlarm6_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEAlarm7_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEAlarm7_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEAlarm7_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEAlarm7_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_PROMISEAlarm8_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_PROMISEAlarm8_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_PROMISEAlarm8_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_PROMISEAlarm8_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);



  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPDoor3_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPDoor3_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPDoor3_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPDoor3_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPDoor4_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPDoor4_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPDoor4_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPDoor4_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPDoor5_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPDoor5_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPDoor5_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPDoor5_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPDoor6_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPDoor6_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPDoor6_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPDoor6_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPDoor7_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPDoor7_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPDoor7_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPDoor7_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPDoor8_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPDoor8_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPDoor8_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPDoor8_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPAlarm0_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPAlarm0_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPAlarm0_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPAlarm0_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPAlarm1_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPAlarm1_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPAlarm1_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPAlarm1_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPAlarm2_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPAlarm2_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPAlarm2_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPAlarm2_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPAlarm3_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPAlarm3_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPAlarm3_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPAlarm3_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPAlarm4_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPAlarm4_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPAlarm4_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPAlarm4_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPAlarm5_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPAlarm5_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPAlarm5_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPAlarm5_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPAlarm6_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPAlarm6_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPAlarm6_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPAlarm6_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPAlarm7_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPAlarm7_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPAlarm7_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPAlarm7_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNOGROUPAlarm8_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNOGROUPAlarm8_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNOGROUPAlarm8_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNOGROUPAlarm8_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);



  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISDoor3_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISDoor3_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISDoor3_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISDoor3_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISDoor4_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISDoor4_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISDoor4_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISDoor4_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISDoor5_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISDoor5_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISDoor5_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISDoor5_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISDoor6_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISDoor6_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISDoor6_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISDoor6_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISDoor7_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISDoor7_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISDoor7_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISDoor7_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISDoor8_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISDoor8_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISDoor8_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISDoor8_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISAlarm0_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISAlarm0_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISAlarm0_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISAlarm0_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISAlarm1_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISAlarm1_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISAlarm1_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISAlarm1_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISAlarm2_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISAlarm2_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISAlarm2_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISAlarm2_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISAlarm3_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISAlarm3_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISAlarm3_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISAlarm3_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISAlarm4_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISAlarm4_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISAlarm4_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISAlarm4_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISAlarm5_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISAlarm5_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISAlarm5_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISAlarm5_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISAlarm6_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISAlarm6_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISAlarm6_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISAlarm6_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISAlarm7_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISAlarm7_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISAlarm7_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISAlarm7_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_HISAlarm8_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_HISAlarm8_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_HISAlarm8_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_HISAlarm8_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','161');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table162VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICE_CARDBYTE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICE_CARDBYTE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICE_CARDBYTE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICE_CARDBYTE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','162');
  DataModule1.ProcessExecSQL(stSql);

end;


function TDataBaseConfig.Table163VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := 'ALTER TABLE TB_ACCESSDEVICE ADD AC_SOCKTYPE integer DEFAULT 1 NOT NULL';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Update TB_ACCESSDEVICE Set AC_SOCKTYPE = 1 ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','163');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table164VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := 'ALTER TABLE TB_DEVICECARDNO ADD DE_MEMLOAD char(1) ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','164');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table165VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := 'CREATE INDEX TB_ALARMEVENT_idx1 ON TB_ALARMEVENT (AL_CHECKOK, AL_DATE) ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','165');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table166VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','CARDMEMORY','0'); //카드 메모리 로딩 유무
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','GLANTIUSE','0');  //글로벌 안티패스
  DataModule1.ProcessExecSQL(stSql);

  //안티패스 그룹 코드 생성
  stSql := 'Create Table TB_ANTIGROUPCODE (';
  stSql := stSql + ' GROUP_CODE varchar(10) DEFAULT ''1234567890'' NOT NULL,';
  stSql := stSql + ' AG_CODE integer NOT NULL,';
  stSql := stSql + ' AG_NAME varchar(50),';
  stSql := stSql + ' AG_ANTINO integer ,';          //출입문에 설정되는 AntipassNo
  stSql := stSql + ' PRIMARY KEY (GROUP_CODE,AG_CODE) ';
  stSql := stSql + ' ) ';
  DataModule1.ProcessExecSQL(stSql);

  //출입문 안티패스그룹코드 추가
  stSql := 'ALTER TABLE TB_DOOR ADD AG_CODE INTEGER DEFAULT 0 ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'ALTER TABLE TB_DOOR ADD AG_ANTINO INTEGER DEFAULT 0 ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','166');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table167VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_PERMITCODE(';
  stSql := stSql + 'PE_PERMITCODE,';
  stSql := stSql + 'PE_PERMITNAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''H'',';
  stSql := stSql + '''폐쇄모드출입불가'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_PERMITCODE(';
  stSql := stSql + 'PE_PERMITCODE,';
  stSql := stSql + 'PE_PERMITNAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''I'',';
  stSql := stSql + '''방범구역다름'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_PERMITCODE(';
  stSql := stSql + 'PE_PERMITCODE,';
  stSql := stSql + 'PE_PERMITNAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''J'',';
  stSql := stSql + '''안티패스출입불가'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','167');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table168VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','ARMAREAUSE','0','구역경계사용유무');  //글로벌 안티패스
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','168');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table169VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FACECOPIP','127.0.0.1','FACECOP IP');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','169');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table16VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','DEVICESVER','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','DTOSPORT1','3000');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','DTOSPORT2','3001');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','DTOSPORT3','3002');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','16');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table170VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FALASTTIME','','최종가져온시간');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','170');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table171VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FA_RETIME','1','연동시간');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','171');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table172VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','GLINITUSE','0','글로벌초기화사용유무');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','GLINITTIME','10','글로벌초기화시간');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','172');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table173VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','GLINITSTM','0000','글로벌초기화시작시간');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','GLINITETM','2400','글로벌초기화종료시간');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','173');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table174VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEE_MASTER_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEE_MASTER_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_EMPLOYEE_MASTER_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEE_MASTER_Add
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','174');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table175VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_PERMITCODE(';
  stSql := stSql + 'PE_PERMITCODE,';
  stSql := stSql + 'PE_PERMITNAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''K'',';
  stSql := stSql + '''DS열림출입불가'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Insert Into TB_PERMITCODE(';
  stSql := stSql + 'PE_PERMITCODE,';
  stSql := stSql + 'PE_PERMITNAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''L'',';
  stSql := stSql + '''마스터모드출입불가'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','175');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table176VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_SONGHOFDCONFIG
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_SONGHOFDCONFIG
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_SONGHOFDCONFIG
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_SONGHOFDCONFIG
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_SEMESTER
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_SEMESTER
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_SEMESTER
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_SEMESTER
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_FOODCodeCount
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_FOODCodeCount
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_FOODCodeCount
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FOODCodeCount
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_FOODDayCount
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_FOODDayCount
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_FOODDayCount
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FOODDayCount
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_FOODWeekCount
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_FOODWeekCount
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_FOODWeekCount
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FOODWeekCount
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_FOODSemesterCount
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_FOODSemesterCount
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_FOODSemesterCount
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FOODSemesterCount
  else exit;
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.InsertIntoTB_SONGHOFDCONFIG('DAY','Y','2');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_SONGHOFDCONFIG('WEEK','Y','10');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_SONGHOFDCONFIG('SEMESTER','Y','150');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_SEMESTER('1','Y','0301','0614');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_SEMESTER('2','Y','0817','1129');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_SEMESTER('3','N','0101','0101');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_SEMESTER('4','N','1231','1231');
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','176');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table177VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_PERMITCODE(';
  stSql := stSql + 'PE_PERMITCODE,';
  stSql := stSql + 'PE_PERMITNAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''Z'',';
  stSql := stSql + '''횟수초과'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','FOODPORT','3432','식수포트');
  DataModule1.ProcessExecSQL(stSql);  

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','177');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table178VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_WORKGUBUN
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_WORKGUBUN
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_WORKGUBUN
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_WORKGUBUN
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEEWORKCODE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEEWORKCODE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_EMPLOYEEWORKCODE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEEWORKCODE_Add
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_WORKGUBUN_Value('1','1','재직');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_WORKGUBUN_Value('2','2','휴직');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_WORKGUBUN_Value('3','3','퇴직');
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' update TB_EMPLOYEE set WG_CODE = 1 where WG_CODE is Null ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','178');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table179VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_EMPLOYEECHANGE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_EMPLOYEECHANGE
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_EMPLOYEECHANGE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FTPLIST
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','179');
  DataModule1.ProcessExecSQL(stSql); 
end;

function TDataBaseConfig.Table17VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_FTPLIST
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_FTPLIST
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_FTPLIST
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FTPLIST
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','FTPPORT','3100');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','17');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table180VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','MODBUSPORT','502');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','180');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table181VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMEVENTKTTSENDSTATUS_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ALARMEVENTKTTSENDSTATUS_Add
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMEVENTKTTSENDSTATUS_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMEVENTKTTSENDSTATUS_Add
  else Exit;
  DataModule1.ProcessEventExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','KTTSENDSER','0');  //이벤트 송신 전용서버 1.
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','181');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table182VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MSSQL' then DropTB_COMPANY_PrimaryKey;
  if DBTYPE = 'PG' then
    stSql := 'alter table TB_COMPANY alter column  CO_DEPARTCODE TYPE varchar(50) NOT NULL'
  else if DBTYPE = 'MSSQL' then
    stSql := 'alter table TB_COMPANY alter column  CO_DEPARTCODE varchar(50) NOT NULL'
  else if DBTYPE = 'FB' then
    stSql := 'alter table TB_COMPANY ALTER CO_DEPARTCODE TYPE varchar(50) NOT NULL'
  else if DBTYPE = 'MDB' then
    stSql := 'alter table TB_COMPANY alter column  CO_DEPARTCODE text(50) NOT NULL';
  DataModule1.ProcessExecSQL(stSql);
  if DBTYPE = 'MSSQL' then CreateTB_COMPANY_PrimaryKey;

  if DBTYPE = 'MSSQL' then DropTB_LOCATION_PrimaryKey;

  if DBTYPE = 'PG' then
    stSql := 'alter table TB_LOCATION alter column  LO_AREACODE TYPE varchar(50) NOT NULL'
  else if DBTYPE = 'MSSQL' then
    stSql := 'alter table TB_LOCATION alter column  LO_AREACODE varchar(50) NOT NULL'
  else if DBTYPE = 'FB' then
    stSql := 'alter table TB_LOCATION ALTER LO_AREACODE TYPE varchar(50) NOT NULL'
  else if DBTYPE = 'MDB' then
    stSql := 'alter table TB_LOCATION alter column  LO_AREACODE text(50) NOT NULL';
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MSSQL' then CreateTB_LOCATION_PrimaryKey;

  if DBTYPE = 'PG' then
    stSql := 'ALTER TABLE TB_EMPLOYEE ALTER COLUMN CO_DEPARTCODE TYPE varchar(50) '
  else if DBTYPE = 'MSSQL' then
    stSql := 'alter table TB_EMPLOYEE alter column  CO_DEPARTCODE varchar(50) '
  else if DBTYPE = 'FB' then
    stSql := 'alter table TB_EMPLOYEE ALTER CO_DEPARTCODE TYPE varchar(50) '
  else if DBTYPE = 'MDB' then
    stSql := 'alter table TB_EMPLOYEE alter column  CO_DEPARTCODE text(50) ';
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'PG' then
    stSql := 'ALTER TABLE TB_ACCESSDEVICE ALTER COLUMN LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MSSQL' then
    stSql := 'alter table TB_ACCESSDEVICE alter column  LO_AREACODE varchar(50) '
  else if DBTYPE = 'FB' then
    stSql := 'alter table TB_ACCESSDEVICE ALTER LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MDB' then
    stSql := 'alter table TB_ACCESSDEVICE alter column  LO_AREACODE text(50) ';
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'PG' then
    stSql := 'ALTER TABLE TB_ALARMDEVICE ALTER COLUMN LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MSSQL' then
    stSql := 'alter table TB_ALARMDEVICE alter column  LO_AREACODE varchar(50) '
  else if DBTYPE = 'FB' then
    stSql := 'alter table TB_ALARMDEVICE ALTER LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MDB' then
    stSql := 'alter table TB_ALARMDEVICE alter column  LO_AREACODE text(50) ';
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'PG' then
    stSql := 'ALTER TABLE TB_ARMAREA ALTER COLUMN LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MSSQL' then
    stSql := 'alter table TB_ARMAREA alter column  LO_AREACODE varchar(50) '
  else if DBTYPE = 'FB' then
    stSql := 'alter table TB_ARMAREA ALTER LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MDB' then
    stSql := 'alter table TB_ARMAREA alter column  LO_AREACODE text(50) ';
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'PG' then
    stSql := 'ALTER TABLE TB_DOOR ALTER COLUMN LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MSSQL' then
    stSql := 'alter table TB_DOOR alter column  LO_AREACODE varchar(50) '
  else if DBTYPE = 'FB' then
    stSql := 'alter table TB_DOOR ALTER LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MDB' then
    stSql := 'alter table TB_DOOR alter column  LO_AREACODE text(50) ';
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'PG' then
    stSql := 'ALTER TABLE TB_FOOD ALTER COLUMN LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MSSQL' then
    stSql := 'alter table TB_FOOD alter column  LO_AREACODE varchar(50) '
  else if DBTYPE = 'FB' then
    stSql := 'alter table TB_FOOD ALTER LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MDB' then
    stSql := 'alter table TB_FOOD alter column  LO_AREACODE text(50) ';
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'PG' then
    stSql := 'ALTER TABLE TB_ZONEDEVICE ALTER COLUMN LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MSSQL' then
    stSql := 'alter table TB_ZONEDEVICE alter column  LO_AREACODE varchar(50) '
  else if DBTYPE = 'FB' then
    stSql := 'alter table TB_ZONEDEVICE ALTER LO_AREACODE TYPE varchar(50) '
  else if DBTYPE = 'MDB' then
    stSql := 'alter table TB_ZONEDEVICE alter column  LO_AREACODE text(50) ';
  DataModule1.ProcessExecSQL(stSql);


  stSql := 'ALTER TABLE TB_ACCESSDEVICE ADD AC_ARMAREAUSE char(1) DEFAULT ''N'' NULL ';
  DataModule1.ProcessExecSQL(stSql);
  stSql := ' Update TB_ACCESSDEVICE set AC_ARMAREAUSE = ''N'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'ALTER TABLE TB_HOLIDAY ADD HO_ACUSE char(1) DEFAULT ''1'' NULL ';
  DataModule1.ProcessExecSQL(stSql);
  stSql := 'ALTER TABLE TB_HOLIDAY ADD HO_ATUSE char(1) DEFAULT ''1'' NULL ';
  DataModule1.ProcessExecSQL(stSql);
  stSql := ' Update TB_HOLIDAY set HO_ACUSE = ''1'',HO_ATUSE = ''1'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'ALTER TABLE TB_COMPANY ADD CO_VIEWSEQ integer DEFAULT 0 NULL ';
  DataModule1.ProcessExecSQL(stSql);
  stSql := 'ALTER TABLE TB_LOCATION ADD LO_VIEWSEQ integer DEFAULT 0 NULL ';
  DataModule1.ProcessExecSQL(stSql);
  stSql := 'ALTER TABLE TB_POSI ADD PO_VIEWSEQ integer DEFAULT 0 NULL ';
  DataModule1.ProcessExecSQL(stSql);

  if dmAdoQuery.CheckTB_COMPANY_Blank <> 1 then
  begin
    dmAdoQuery.InsertIntoTB_COMPANY_CompanyName('001','기본회사');
  end;


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','182');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table183VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := 'Create Table TB_CARDGROUPCODE(';
  stSql := stSql + ' GROUP_CODE varchar(10) DEFAULT ''1234567890'' NOT NULL,';
  stSql := stSql + ' CA_GROUP varchar(50) NOT NULL,';
  stSql := stSql + ' CA_GROUPNAME nvarchar(100),';
  stSql := stSql + ' CA_DEEPSEQ integer,';
  stSql := stSql + ' CA_CODEUSE char(1),';
  stSql := stSql + ' CA_VIEWSEQ integer,';
  stSql := stSql + ' PRIMARY KEY (GROUP_CODE,CA_GROUP) ';
  stSql := stSql + ' ) ';
  if DBTYPE = 'MDB' then
  begin
    stSql := StringReplace(stSql,'image','OLEObject',[rfReplaceAll]);
    stSql := StringReplace(stSql,'int IDENTITY','COUNTER',[rfReplaceAll]);
    stSql := StringReplace(stSql,'varchar','text',[rfReplaceAll]);
    stSql := StringReplace(stSql,'nvarchar','text',[rfReplaceAll]);
    stSql := StringReplace(stSql,'char','text',[rfReplaceAll]);
  end else if DBTYPE = 'PG' then
  begin
    stSql := StringReplace(stSql,'image','oid',[rfReplaceAll]);
    stSql := StringReplace(stSql,'int IDENTITY','serial',[rfReplaceAll]);
  end else if DBTYPE = 'FB' then
  begin
    stSql := StringReplace(stSql,'image','BLOB',[rfReplaceAll]);
    stSql := StringReplace(stSql,'varbinary(MAX)','BLOB',[rfReplaceAll]);
    stSql := StringReplace(stSql,'int IDENTITY','integer',[rfReplaceAll]);
  end;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_CARDGROUPCODE(';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' CA_GROUP,';
  stSql := stSql + ' CA_GROUPNAME, ';
  stSql := stSql + ' CA_DEEPSEQ, ';
  stSql := stSql + ' CA_CODEUSE, ';
  stSql := stSql + ' CA_VIEWSEQ) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GroupCode + ''', ';
  stSql := stSql + '''0'', ';
  stSql := stSql + '''Group'', ';
  stSql := stSql + '0, ';
  stSql := stSql + '''1'', ';
  stSql := stSql + '1) ';

  DataModule1.ProcessExecSQL(stSql);


  stSql := 'CREATE TABLE TB_DEVICECARDNOEMPLOYEE( ';
	stSql := stSql + ' GROUP_CODE varchar(10) DEFAULT ''1234567890'' NOT NULL,';
	stSql := stSql + ' CO_COMPANYCODE varchar(3) NOT NULL,';
	stSql := stSql + ' EM_CODE varchar(50) NOT NULL,';
	stSql := stSql + ' AC_NODENO integer NOT NULL, ';
	stSql := stSql + ' AC_ECUID varchar(2) NOT NULL,';
	stSql := stSql + ' DE_DOOR1 char(1) NULL,';
	stSql := stSql + ' DE_DOOR2 char(1) NULL,';
	stSql := stSql + ' DE_DOOR3 char(1) NULL,';
	stSql := stSql + ' DE_DOOR4 char(1) NULL,';
	stSql := stSql + ' DE_DOOR5 char(1) NULL,';
	stSql := stSql + ' DE_DOOR6 char(1) NULL,';
	stSql := stSql + ' DE_DOOR7 char(1) NULL,';
	stSql := stSql + ' DE_DOOR8 char(1) NULL,';
	stSql := stSql + ' DE_USEACCESS char(1) NULL,';
	stSql := stSql + ' DE_USEALARM char(1) NULL,';
	stSql := stSql + ' DE_TIMECODE char(1) NULL,';
	stSql := stSql + ' DE_PERMIT char(1) NULL,';
	stSql := stSql + ' DE_RCVACK char(1) NULL,';
	stSql := stSql + ' DE_UPDATETIME varchar(14) NULL,';
	stSql := stSql + ' DE_UPDATEOPERATOR varchar(10) NULL,';
	stSql := stSql + ' AC_MCUID varchar(7) NULL,';
	stSql := stSql + ' DE_ALARM0 varchar(1) NULL,';
	stSql := stSql + ' DE_ALARM1 varchar(1) NULL,';
	stSql := stSql + ' DE_ALARM2 varchar(1) NULL,';
	stSql := stSql + ' DE_ALARM3 varchar(1) NULL,';
	stSql := stSql + ' DE_ALARM4 varchar(1) NULL,';
	stSql := stSql + ' DE_ALARM5 varchar(1) NULL,';
	stSql := stSql + ' DE_ALARM6 varchar(1) NULL,';
	stSql := stSql + ' DE_ALARM7 varchar(1) NULL,';
	stSql := stSql + ' DE_ALARM8 varchar(1) NULL, ';
  stSql := stSql + ' PRIMARY KEY (GROUP_CODE,CO_COMPANYCODE,EM_CODE,AC_NODENO,AC_ECUID) ';
  stSql := stSql + ' ) ';
  if DBTYPE = 'MDB' then
  begin
    stSql := StringReplace(stSql,'image','OLEObject',[rfReplaceAll]);
    stSql := StringReplace(stSql,'int IDENTITY','COUNTER',[rfReplaceAll]);
    stSql := StringReplace(stSql,'varchar','text',[rfReplaceAll]);
    stSql := StringReplace(stSql,'nvarchar','text',[rfReplaceAll]);
    stSql := StringReplace(stSql,'char','text',[rfReplaceAll]);
  end else if DBTYPE = 'PG' then
  begin
    stSql := StringReplace(stSql,'image','oid',[rfReplaceAll]);
    stSql := StringReplace(stSql,'int IDENTITY','serial',[rfReplaceAll]);
  end else if DBTYPE = 'FB' then
  begin
    stSql := StringReplace(stSql,'image','BLOB',[rfReplaceAll]);
    stSql := StringReplace(stSql,'varbinary(MAX)','BLOB',[rfReplaceAll]);
    stSql := StringReplace(stSql,'int IDENTITY','integer',[rfReplaceAll]);
  end;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEE_CAGROUP_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEE_CAGROUP_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEE_CAGROUP_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEE_CAGROUP_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','183');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table184VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATWORKTYPE_ATSTARTBUTTON_Change
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATWORKTYPE_ATSTARTBUTTON_Change
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATWORKTYPE_ATSTARTBUTTON_Change
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATWORKTYPE_ATSTARTBUTTON_Change
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATWORKTYPE_ATOFFBUTTON_Change
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATWORKTYPE_ATOFFBUTTON_Change
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATWORKTYPE_ATOFFBUTTON_Change
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATWORKTYPE_ATOFFBUTTON_Change
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATWORKTYPE_WORKOUTBUTTON_Change
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATWORKTYPE_WORKOUTBUTTON_Change
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATWORKTYPE_WORKOUTBUTTON_Change
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATWORKTYPE_WORKOUTBUTTON_Change
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATWORKTYPE_WORKINBUTTON_Change
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATWORKTYPE_WORKINBUTTON_Change
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATWORKTYPE_WORKINBUTTON_Change
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATWORKTYPE_WORKINBUTTON_Change
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','184');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table185VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICE_MEMLOAD_ADD
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICE_MEMLOAD_ADD
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICE_MEMLOAD_ADD
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICE_MEMLOAD_ADD
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOOR_MEMLOAD_ADD
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOOR_MEMLOAD_ADD
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DOOR_MEMLOAD_ADD
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOOR_MEMLOAD_ADD
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ARMAREA_MEMLOAD_ADD
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ARMAREA_MEMLOAD_ADD
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ARMAREA_MEMLOAD_ADD
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ARMAREA_MEMLOAD_ADD
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEE_EXPIREUSE_ADD
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEE_EXPIREUSE_ADD
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEE_EXPIREUSE_ADD
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEE_EXPIREUSE_ADD
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_EMPLOYEEEXPIRECHANGE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_EMPLOYEEEXPIRECHANGE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_EMPLOYEEEXPIRECHANGE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_EMPLOYEEEXPIRECHANGE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'PG' then PostGreSql.CreateTB_EMPLOYEEEXPIRE_Trigger
  else if DBTYPE = 'MSSQL' then MSSql.CreateTB_EMPLOYEEEXPIRE_Trigger;
  


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','185');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table186VersionMake: Boolean;
var
  stSql : string;
begin
  dmDBCreate.AlterTB_EMPLOYEE_GRADETYPE_Add ;
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','186');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table187VersionMake: Boolean;
var
  stSql : string;
begin
  dmDBCreate.AlterTB_ARMAREA_DISARMCHECKUSE_Add ;
  dmDBCreate.AlterTB_ARMAREA_DISARMCHECKTIME1FROM_Add ;
  dmDBCreate.AlterTB_ARMAREA_DISARMCHECKTIME1TO_Add ;
  stSql := 'Update TB_ARMAREA set AR_DISARMCHECKTIME1FROM = ''0000'',AR_DISARMCHECKTIME1TO = ''0000'' ';
  DataModule1.ProcessExecSQL(stSql);
  
  stSql := ' Insert Into TB_ALARMSTATUSCODE(';
  stSql := stSql + ' AL_ALARMSTATUSCODE,';
  stSql := stSql + ' AL_ALARMNAME,';
  stSql := stSql + ' AL_ALARMVIEW,';
  stSql := stSql + ' AL_ALARMGRADE,';
  stSql := stSql + ' AL_ALARMSOUND ) ';
  stSql := stSql + ' VALUES( ';
  stSql := stSql + '''NA'',';
  stSql := stSql + '''미경계알람신호'',';
  stSql := stSql + '1,';
  stSql := stSql + '3,';
  stSql := stSql + '1) ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','187');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table188VersionMake: Boolean;
var
  stSql : string;
begin
  dmDBCreate.AlterTB_HOLIDAY_HOTYPE_Add ;
  dmAdoQuery.UpdateTB_HOLIDAY_Field_IntValue('','HO_TYPE','3');

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','188');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table189VersionMake: Boolean;
var
  stSql : string;
  TempAdoQuery : TADOQuery;
  i : integer;
begin
  stSql := 'select * from TB_DOOR where DO_SCHUSE = ''1'' ';
  Try
    CoInitialize(nil);
    TempAdoQuery := TADOQuery.Create(nil);
    TempAdoQuery.Connection := DataModule1.ADOConnection;
    TempAdoQuery.DisableControls;
    with TempAdoQuery do
    begin
      Close;
      sql.Text := stSql;
      Try
        Open;
      Except
        //에러처리
        Exit;
      End;
      While Not Eof do
      begin
        for i :=  0 to 5 do
        begin
          stSql := ' Insert Into TB_DEVICESCHEDULE ';
          stSql := stSql + '( GROUP_CODE,AC_NODENO,AC_ECUID,DO_DOORNO,DE_DAYCODE,';
          stSql := stSql + ' DE_CELL1TIME,DE_CELL1MODE,DE_CELL2TIME,DE_CELL2MODE,';
          stSql := stSql + ' DE_CELL3TIME,DE_CELL3MODE,DE_CELL4TIME,DE_CELL4MODE,';
          stSql := stSql + ' DE_CELL5TIME,DE_CELL5MODE,DE_UPDATETIME,DE_UPDATEOPERATOR,';
          stSql := stSql + ' DE_RCVACK ) ';
          stSql := stSql + ' Values(''' + GROUPCODE + ''',';
          stSql := stSql + FindField('AC_NODENO').AsString + ',';
          stSql := stSql + '''' + FindField('AC_ECUID').AsString + ''',';
          stSql := stSql + '''' + FindField('DO_DOORNO').AsString + ''',';
          stSql := stSql + '''' + inttostr(i) + ''',';
          stSql := stSql + '''0500'',';
          stSql := stSql + '''1'',';
          stSql := stSql + '''1200'',';
          stSql := stSql + '''1'',';
          stSql := stSql + '''1300'',';
          stSql := stSql + '''1'',';
          stSql := stSql + '''1800'',';
          stSql := stSql + '''1'',';
          stSql := stSql + '''0000'',';
          stSql := stSql + '''1'',';
          stSql := stSql + '''' + FormatDateTime('yyyymmddHHMMSS',Now) + ''',';
          stSql := stSql + '''SYSTEM'',';
          stSql := stSql + '''N'')';
          DataModule1.ProcessExecSQL(stSql);
        end;
        Next;
      end;
    End;
  Finally
    TempAdoQuery.EnableControls;
    TempAdoQuery.Free;
    CoUninitialize;
  End;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','189');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table18VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMSTATUSCODEALARMSOUND
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMSTATUSCODEALARMSOUND
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMSTATUSCODEALARMSOUND
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMSTATUSCODEALARMSOUND
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','18');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table190VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEE_CardChange_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEE_CardChange_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEE_CardChange_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEE_CardChange_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','PGUPDATE',FormatDateTime('yyyymmdd',now));
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('KT','OLDEMPDEL','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','CARDDLTUSE','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','CARDDELETE','90');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','190');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table191VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('EMPLOYEE','ENCCOTEL','0','회사전화번호암호화유무');
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEE_COTELENCRYPT_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEE_COTELENCRYPT_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEE_COTELENCRYPT_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEE_COTELENCRYPT_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','191');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table192VersionMake: Boolean;
var
  stSql : string;
begin
  dmDBCreate.CreateTB_TIMECODEBUILDING ;
  stSql := CommonSql.InsertIntoTB_PROGRAMID('MOA05i', 'MOBASECODE', '타임코드관리', 'Y', '4', 'TimeCode', 'Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','192');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table193VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_CURRENTDAEMON(CU_STATECODE,CU_STATEVALUE) ';
  stSql := stSql + ' Values(''FOODTIME'',''N'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_PROGRAMID('FOA03i', 'FOBASECODE','식수환경','Y','4','FOOD','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','193');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table194VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','PERMITSYNC','1');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','194');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table195VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','AUTOLOGOUT','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','AUTOLOTIME','5');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','LOGINLIMIT','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','LOGINCOUNT','5');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','195');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table196VersionMake: Boolean;
var
  stSql : string;
begin
  dmDBCreate.CreateTB_LOGINCOUNT;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','196');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table197VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','LINECHECK','FALSE');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','197');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table198VersionMake: Boolean;
var
  stSql : string;
begin
  dmDBCreate.AlterTB_DOOR_SchIgnore_Add;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','198');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table199VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','EMPSERVER','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','EMPSPORT','5300');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','DOORCTIME','3');
  DataModule1.ProcessExecSQL(stSql);

  dmDBCreate.CreateTB_RELAYCONTROLDOOR;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','199');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table19VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','SPECIALCD','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','19');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table1VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_CONFIG(';
  stSql := stSql + 'GROUP_CODE,';
  stSql := stSql + 'CO_CONFIGGROUP,';
  stSql := stSql + 'CO_CONFIGCODE,';
  stSql := stSql + 'CO_CONFIGVALUE)';
  stSql := stSql + ' Values(';
  stSql := stSql + '''1234567890'',';
  stSql := stSql + '''COMMON'',';
  stSql := stSql + '''TABLE_VER'',';
  stSql := stSql + '''1'')';
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table200VersionMake: Boolean;
var
  stSql : string;
begin
  dmDBCreate.CreateTB_EMMAPPING;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','200');
  DataModule1.ProcessExecSQL(stSql);
end;


function TDataBaseConfig.Table201VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','IOTIMEUSE','0','외출시간 근무시간 계산');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','IOTIME','10','외출시간 근무시간 계산');
  DataModule1.ProcessExecSQL(stSql);

  dmDBCreate.AlterTB_ATEVENT_LASTINTIME_Add;
  dmDBCreate.AlterTB_ATEVENT_LASTOUTTIME_Add;
  dmDBCreate.AlterTB_ATEVENT_NIGHTWORKTIME_Add;
  dmDBCreate.AlterTB_ATEVENT_WORKTIME_Add;

  stSql := 'Update TB_ATEVENT set NIGHTWORKTIME = 0 ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Update TB_ATEVENT set WORKTIME = 0 ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','201');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table202VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','SCHUSEPACK','0','스케줄 적용 유무 패킷');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','202');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table203VersionMake: Boolean;
var
  stSql : string;
begin
  dmDBCreate.AlterTB_ATEVENT_WORKSTARTTIME_Add;
  dmDBCreate.AlterTB_ATEVENT_WORKENDTIME_Add;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','203');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table204VersionMake: Boolean;
var
  stSql : string;
begin
  dmDBCreate.CreateTB_DEVICECARDNO_Idx01;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','204');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table205VersionMake: Boolean;
var
  stSql : string;
  nOrd : integer;
begin
(*
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','205');
  DataModule1.ProcessExecSQL(stSql);
*)

end;

function TDataBaseConfig.Table20VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_KTCARDISSUE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_KTCARDISSUE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_KTCARDISSUE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_KTCARDISSUE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISINSERTTIME
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISINSERTTIME
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISINSERTTIME
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISINSERTTIME
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','20');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table21VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then Exit
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateView_CARD
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateView_CARD
  else if DBTYPE = 'FB' then stSql := FireBird.CreateView_CARD
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','21');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table22VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := commonSql.InsertIntoTB_PROGRAMID('MOB06i','MOMANAGE','KT카드발급','Y','11','MONITORING','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','22');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table23VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_RELAYGUBUN
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_RELAYGUBUN
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_RELAYGUBUN
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_RELAYGUBUN
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEERelayGubun
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEERelayGubun
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEERelayGubun
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEERelayGubun
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Update TB_EMPLOYEE set RG_CODE = ''001'' ';
  stSql := stSql + ' where RG_CODE IS NULL ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','23');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table24VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_FORMNAME('040','직원구분','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('041','직원코드','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('042','직원코드명칭','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','24');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table25VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_RELAYGUBUN('001','정직원');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_RELAYGUBUN('002','외부인');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','25');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table26VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_COMPANYUPDATECHECK
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_COMPANYUPDATECHECK
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_COMPANYUPDATECHECK
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_COMPANYUPDATECHECK
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_POSIUPDATECHECK
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_POSIUPDATECHECK
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_POSIUPDATECHECK
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_POSIUPDATECHECK
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','26');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table27VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ADMINBuildingGrade
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ADMINBuildingGrade
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ADMINBuildingGrade
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ADMINBuildingGrade
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ADMINDongCode
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ADMINDongCode
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ADMINDongCode
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ADMINDongCode
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ADMINFloorCode
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ADMINFloorCode
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ADMINFloorCode
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ADMINFloorCode
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ADMINAreaCode
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ADMINAreaCode
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ADMINAreaCode
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ADMINAreaCode
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEDaemonGubun
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEDaemonGubun
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEDaemonGubun
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEDaemonGubun
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.updateTB_ACCESSDEVICEDaemonGubunAll('1');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_FORMNAME('901','빌딩코드','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('902','빌딩명칭','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('903','빌딩권한','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('911','층코드','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('912','층명칭','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('913','층권한','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('921','구역코드','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('922','구역명칭','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('923','구역권한','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('933','출입문권한','Y');
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','27');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table28VersionMake: Boolean;
var
  stSql : string;
begin

  stSql := CommonSql.InsertIntoTB_PROGRAMGROUP('MODEVICE','관제-기기관리','Y','MONITORING');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_PROGRAMID('MOF00','MODEVICE','기기관리','D','50','MONITORING','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_PROGRAMID('MOF01i','MODEVICE','위치정보관리','Y','51','MONITORING','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_PROGRAMID('MOF02i','MODEVICE','컨트롤러관리','Y','52','MONITORING','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_PROGRAMID('MOF03i','MODEVICE','출입문관리','Y','53','MONITORING','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_PROGRAMID('MOF04i','MODEVICE','방범구역관리','Y','54','MONITORING','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','28');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table29VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','FTPUSE','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','MULTI','0');
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_DAEMONMULTI
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_DAEMONMULTI
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_DAEMONMULTI
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_DAEMONMULTI
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','29');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table2VersionMake: Boolean;
var
  stSql : string;
begin

  stSql := 'select * from TB_MONTHAT ';
  with DataModule1.ADOQuery do
  begin
    Close;
    Sql.Clear;
    Sql.Text := stSql;
    Try
      Open;
    Except
      Exit;
    End;
    if FieldCount < 42 then
    begin
      stSql := ' ALTER TABLE TB_MONTHAT ADD MO_NIGHTTIME int ';
      if DBTYPE <> 'FB' then stSql := stSql + ' NULL ';
      DataModule1.ProcessExecSQL(stSql);
      stSql := ' ALTER TABLE TB_MONTHAT ADD MO_HOLIDAYTIME int ';
      if DBTYPE <> 'FB' then stSql := stSql + ' NULL ';
      DataModule1.ProcessExecSQL(stSql);
    end;
  end;
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','2');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table30VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_CARDDoorGrade
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_CARDDoorGrade
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_CARDDoorGrade
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_CARDDoorGrade
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Update TB_CARD set CA_DOORGRADE = ''Y'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','30');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table31VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_PROGRAMID('MOF05i','MODEVICE','출입문스케줄관리','Y','55','MONITORING','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','31');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table32VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_DOORGUBUN
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_DOORGUBUN
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_DOORGUBUN
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_DOORGUBUN
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOORGUBUN
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOORGUBUN
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DOORGUBUN
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOORGUBUN
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertTB_DOORGUBUN('001','구분1');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_DOORGUBUN('002','구분2');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','32');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table33VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_PROGRAMID('MOE04r','MOREPORT','출입통계보고서','Y','44','MONITORING','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','33');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table34VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','KHUADDDAY','','경희대입력연동날짜');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','KHUDELDAY','','경희대삭제연동날짜');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','34');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table35VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','MAXPROCESS','10');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','35');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table36VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FDMSNO','4');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','36');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table37VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISEMNAME
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISEMNAME
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISEMNAME
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISEMNAME
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISHANDPHONE
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISHANDPHONE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISHANDPHONE
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISHANDPHONE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISCOMPANYNAME
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISCOMPANYNAME
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISCOMPANYNAME
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISCOMPANYNAME
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISJIJUMNAME
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISJIJUMNAME
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISJIJUMNAME
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISJIJUMNAME
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISDEPARTNAME
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISDEPARTNAME
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISDEPARTNAME
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISDEPARTNAME
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISPOSINAME
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISPOSINAME
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISPOSINAME
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISPOSINAME
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','37');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table38VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','CARDADDDAY','','경희대카드연동날짜');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','CARDDELDAY','','경희대카드삭제연동날짜');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','38');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table39VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_PERMITCODE(';
  stSql := stSql + 'PE_PERMITCODE,';
  stSql := stSql + 'PE_PERMITNAME)';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''F'',';
  stSql := stSql + '''유효기간만료'')';
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','39');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table3VersionMake: Boolean;
var
  stSql : string;
  stPath : string;
begin
  stPath := 'C:\Program Files\ACServer\ACCESS.mdb';
  if DBTYPE = 'PG' then stPath := 'C:\\Program Files\\ACServer\\ACCESS.mdb';
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','fdmsUSE','FALSE');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','fdmsPath',stPath);
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','fdmsPW','fdmsamho');
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' ALTER TABLE TB_EMPLOYEE ADD FDMS_ID int ';
  if DBTYPE <> 'FB' then stSql := stSql + ' NULL ';
  DataModule1.ProcessExecSQL(stSql);
  if DBTYPE = 'MDB' then  stSql := ' ALTER TABLE TB_EMPLOYEE ADD FDMS_RELAY text(1) '
  else stSql := ' ALTER TABLE TB_EMPLOYEE ADD FDMS_RELAY varchar(1) ';
  if DBTYPE <> 'FB' then stSql := stSql + ' NULL ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' ALTER TABLE TB_DOOR ADD FDMS_ID int ';
  if DBTYPE <> 'FB' then stSql := stSql + ' NULL ';
  DataModule1.ProcessExecSQL(stSql);
  if DBTYPE = 'MDB' then  stSql := ' ALTER TABLE TB_DOOR ADD FDMS_RELAY text(1) '
  else stSql := ' ALTER TABLE TB_DOOR ADD FDMS_RELAY varchar(1) ';
  if DBTYPE <> 'FB' then stSql := stSql + ' NULL ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','3');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table40VersionMake: Boolean;
var
  stSql : string;
  stCardLen : string;
begin
  stCardLen := '24'; //성현지문카드길이

  if CheckPersonRelay = '3' then stCardLen := '32'; //경희대 성현지문카드길이 = 32자리

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FDMSCARDLN',stCardLen,'성현지문카드길이');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','40');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table41VersionMake: Boolean;
var
  stSql : string;
begin

  stSql := CommonSql.InsertIntoTB_CONFIG('CARD','CARDFIXED','TRUE','카드길이고정유무');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('CARD','FIXEDLEN','16','카드길이');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('CARD','FILLPOSI','1','채움문자위치 0:앞,1:뒤');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('CARD','FILLCHAR','*','채움문자');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','41');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table42VersionMake: Boolean;
var
  stSql : string;
  stCode1Name,stCode2Name : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_CARDTYPE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_CARDTYPE
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_CARDTYPE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_CARDTYPE
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  stCode1Name := '일반카드';
  stCode2Name := '기타카드';
  if CheckSamyuk then  //삼육대인경우
  begin
    stCode1Name := '일반카드';
    stCode2Name := '모바일카드';
  end else if CheckFdms then  //지문연동인 경우
  begin
    stCode1Name := '지문연동카드';
    stCode2Name := '일반카드';
  end;
  
  stSql := 'Insert Into TB_CARDTYPE(CT_CODE,CT_NAME) ';
  stSql := stSql + ' Values( ''1'',''' + stCode1Name + ''') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_CARDTYPE(CT_CODE,CT_NAME) ';
  stSql := stSql + ' Values( ''2'',''' + stCode2Name + ''') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','42');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table43VersionMake: Boolean;
var
  stSql : string;
begin

  stSql := 'Insert Into TB_CURRENTDAEMON(CU_STATECODE,CU_STATEVALUE) ';
  stSql := stSql + ' Values( ''FIREORIGIN'',''FALSE'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','43');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table44VersionMake: Boolean;
var
  stSql : string;
begin

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_HOLIDAYState
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_HOLIDAYState
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_HOLIDAYState
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_HOLIDAYState
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','44');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table45VersionMake: Boolean;
var
  stSql : string;
begin

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEHoSend
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEHoSend
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEHoSend
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEHoSend
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','45');
  DataModule1.ProcessExecSQL(stSql);

end;


function TDataBaseConfig.Table46VersionMake: Boolean;
var
  stSql : string;
begin

  stSql := 'Insert Into TB_CURRENTDAEMON(CU_STATECODE,CU_STATEVALUE) ';
  stSql := stSql + ' Values( ''SEND_HOYY'','''') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','46');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table47VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_ALARMSTATUSCODE(';
  stSql := stSql + ' AL_ALARMSTATUSCODE,';
  stSql := stSql + ' AL_ALARMNAME,';
  stSql := stSql + ' AL_ALARMVIEW,';
  stSql := stSql + ' AL_ALARMGRADE,';
  stSql := stSql + ' AL_ALARMSOUND ) ';
  stSql := stSql + ' VALUES( ';
  stSql := stSql + '''FI'',';
  stSql := stSql + '''FI 화재알람'',';
  stSql := stSql + '1,';
  stSql := stSql + '3,';
  stSql := stSql + '1) ';
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','47');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table48VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('R1','RESET(경계모드)','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('R2','RESET(해제모드)','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('A1','경계모드변경(카드)','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('A2','경계모드변경(관제)','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('A3','경계모드변경(연동)','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('A4','경계모드변경(서버)','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('C1','해제모드변경(카드)','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('C2','해제모드변경(관제)','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('C3','해제모드변경(연동)','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('C4','해제모드변경(서버)','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('R3','복구버튼','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('ZS','알람발생(쇼트)','1','3','1');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('ZP','알람발생(단선)','1','3','1');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('ZN','알람복구','1','0','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','48');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table49VersionMake: Boolean;
var
  stSql : string;
begin
  //뷰삭제
  stSql := 'DROP VIEW VI_CARDINFO ';
  DataModule1.ProcessExecSQL(stSql);
  //tb_employee 이름 사이즈 변경
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEEEmNameChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_EMPLOYEEEmNameChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEEEmNameChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEEEmNameChange
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  {
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMSTATUSCODEPrimaryDelete
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ALARMSTATUSCODEPrimaryDelete
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMSTATUSCODEPrimaryDelete
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMSTATUSCODEPrimaryDelete
  else Exit;
  DataModule1.ProcessExecSQL(stSql);   }
  //알람상태코드 사이즈 변경
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMSTATUSCODECodeChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ALARMSTATUSCODECodeChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMSTATUSCODECodeChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMSTATUSCODECodeChange
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  //알람 이벤트 상태코드 사이즈 변경
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMEVENTCodeChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ALARMEVENTCodeChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMEVENTCodeChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMEVENTCodeChange
  else Exit;
  DataModule1.ProcessEventExecSQL(stSql);
  //알람 이벤트 oper 사이즈 변경
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMEVENTOperChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ALARMEVENTOperChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMEVENTOperChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMEVENTOperChange
  else Exit;
  DataModule1.ProcessEventExecSQL(stSql);
  //알람 이벤트 재정의 상태코드 생성
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMEVENTSTATECODE2Add
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ALARMEVENTSTATECODE2Add
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMEVENTSTATECODE2Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMEVENTSTATECODE2Add
  else Exit;
  DataModule1.ProcessEventExecSQL(stSql);
  //알람상태 그룹 테이블 생성
  if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_ALARMCODEGROUP
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_ALARMCODEGROUP
  else if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_ALARMCODEGROUP
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_ALARMCODEGROUP
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  //알람 모드변경 제외 그룹 테이블 생성
  if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_ALARMMODENOTCARD
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_ALARMMODENOTCARD
  else if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_ALARMMODENOTCARD
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_ALARMMODENOTCARD
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  //알람 모드변경 제외 데이터 인서트
  stSql := CommonSql.InsertIntoTB_ALARMMODENOTCARD('001','***RESET**');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMMODENOTCARD('002','.KTLinkUs.');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMMODENOTCARD('003','ForceRelay');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMMODENOTCARD('004','0000000000');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMMODENOTCARD('005','RecoveryOK');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('ET','기타','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  
  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','49');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table4VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_FormName
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_FormName
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_FormName
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_FormName
  else exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_FORMNAME('001','회사코드','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('002','회사명','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('011','지점코드','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('012','지점명','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('021','부서코드','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('022','부서명','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('031','직위코드','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('032','직위명','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('101','사번','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('102','이름','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('103','사내전화번호','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('104','입사일','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('105','퇴사일','Y');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_FORMNAME('106','사원','Y');
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','4');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table50VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','DAEMON_VER','0','데몬버젼');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','50');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table51VersionMake: Boolean;
var
  stSql : string;
begin
  //카드외의 상태코드 테이블
  if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_NOTCARDALARMCODE
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_NOTCARDALARMCODE
  else if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_NOTCARDALARMCODE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_NOTCARDALARMCODE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('RESET','R1');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('RESET','R2');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('RESET','WR');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('RESET','CR');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('모드변경','A1');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('모드변경','A2');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('모드변경','A3');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('모드변경','A4');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('모드변경','C1');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('모드변경','C2');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('모드변경','C3');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('모드변경','C4');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Alarm','ZS');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Alarm','ZP');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Alarm','ZN');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Alarm','E1');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Alarm','F1');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Alarm','FI');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Alarm','G1');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Alarm','Q1');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Network','NF');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Network','NR');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('AC전원','AF');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('AC전원','AN');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Data','BF');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('기기관련','CC');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('기기관련','CO');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('밧데리','DF');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('밧데리','DL');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('밧데리','DR');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('기타','ET');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('001','a','R1');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('001','d','R2');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('002','a','A2');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('002','d','C2');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('003','a','A3');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('003','d','C3');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('004','a','A4');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('004','d','C4');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('005','a','R3');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('005','d','R3');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','51');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table52VersionMake: Boolean;
var
  stSql : string;
begin
  //카드발급테이블에 여기서 발급했는지 여부
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_KTCARDISSUEWriteAdd
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_KTCARDISSUEWriteAdd
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_KTCARDISSUEWriteAdd
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_KTCARDISSUEWriteAdd
  else Exit;
  DataModule1.ProcessEventExecSQL(stSql);

  //카드발급테이블에 발급일자
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_KTCARDISSUEWriteDateAdd
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_KTCARDISSUEWriteDateAdd
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_KTCARDISSUEWriteDateAdd
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_KTCARDISSUEWriteDateAdd
  else Exit;
  DataModule1.ProcessEventExecSQL(stSql);


  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSEVENTCOMPANYCODE
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSEVENTCOMPANYCODE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSEVENTCOMPANYCODE
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSEVENTCOMPANYCODE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSEVENTEMCODE
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSEVENTEMCODE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSEVENTEMCODE
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSEVENTEMCODE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSEVENTJIJUMCODE
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSEVENTJIJUMCODE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSEVENTJIJUMCODE
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSEVENTJIJUMCODE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSEVENTDEPARTCODE
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSEVENTDEPARTCODE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSEVENTDEPARTCODE
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSEVENTDEPARTCODE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSEVENTEMNAME
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSEVENTEMNAME
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSEVENTEMNAME
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSEVENTEMNAME
  else Exit;
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','52');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table53VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','EMPLOYEE','0','0:사원별,1:카드별');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','53');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table54VersionMake: Boolean;
var
  stSql : string;
begin

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_MAPLOCATION
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_MAPLOCATION
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_MAPLOCATION
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_MAPLOCATION
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_MAPPOSITION
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_MAPPOSITION
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_MAPPOSITION
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_MAPPOSITION
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_MAPZONE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_MAPZONE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_MAPZONE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_MAPZONE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','54');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table55VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEUPDATE
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEUPDATE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEUPDATE
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEUPDATE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOORUPDATE
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOORUPDATE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DOORUPDATE
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOORUPDATE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMDEVICEUPDATE
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMDEVICEUPDATE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMDEVICEUPDATE
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMDEVICEUPDATE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ZONEDEVICEUPDATE
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ZONEDEVICEUPDATE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ZONEDEVICEUPDATE
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ZONEDEVICEUPDATE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','55');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table56VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_FTPLISTRetryCount
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_FTPLISTRetryCount
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_FTPLISTRetryCount
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_FTPLISTRetryCount
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','56');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table57VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOOROPENMONI_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOOROPENMONI_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DOOROPENMONI_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOOROPENMONI_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOORSENDDOOR_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOORSENDDOOR_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DOORSENDDOOR_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOORSENDDOOR_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOORALARMLONG_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOORALARMLONG_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DOORALARMLONG_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOORALARMLONG_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOORDSOPEN_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOORDSOPEN_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DOORDSOPEN_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOORDSOPEN_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOORREMOTEDOOR_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOORREMOTEDOOR_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DOORREMOTEDOOR_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOORREMOTEDOOR_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','57');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table58VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','ATTYPE1','0','1:전사원정상출퇴근고정'); //전사원 교대타입에 따른 출퇴근처리
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','58');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table59VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOORControlTime_Change
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOORControlTime_Change
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DOORControlTime_Change
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOORControlTime_Change
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','59');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table5VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoAT_INCODE('101','정상출근','1','N');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoAT_INCODE('102','외근후출근','1','N');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoAT_INCODE('103','지각','2','N');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoAT_INCODE('104','조기출근','4','N');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoAT_INCODE('105','결근','3','N');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoAT_OUTCODE('101','정상퇴근','1','N');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoAT_OUTCODE('102','외근후퇴근','1','N');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoAT_OUTCODE('103','조퇴','2','N');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoAT_OUTCODE('104','연장근무','3','N');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoAT_OUTCODE('105','야간근무','4','N');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_PROGRAMID('MOE02r','MOREPORT','경보이력보고서','Y','42','MONITORING','Y');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','5');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table60VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := 'update TB_FTPLIST set FL_RETRYCOUNT = 0 ';

  DataModule1.ProcessExecSQL(stSql);

  stSql := 'update TB_ACCESSDEVICE set SEND_ACK = ''Y'' ';

  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','60');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table61VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','cardServer','0','카드서버연동유무');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','PER_RELAY','0','사원정보연동유무');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','PER_SRELAY','0','보내는사원정보연동유무');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','SENT_RELAY','0','전문연동유무');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','CARDNOTYPE','1','카드번호Type(고정/가변)');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','CARDNUM','1','카드숫자인식Type');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','CAPOSINUM','0','카드위치사용유무');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','DUPCARD','0','1인당카드중복여부');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('FIRE','FIRERECUSE','1','화재복구사용유무');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('FIRE','FIREOPEN','0','화재감지시전체개방');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('FIRE','FIRESTATUS','FI','화재감지시상태값');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('COMMON','ACEVENT','1','0.전체이력,1.카드이벤트만');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','CONTRLPORT','10001','제어포트');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','STATEPORT','10011','상태포트');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','EVENTPORT','10021','이벤트포트');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','MULTISOCK','0','멀티소켓');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','61');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table62VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMEVENTAlarmSound_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMEVENTAlarmSound_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMEVENTAlarmSound_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMEVENTAlarmSound_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMEVENTAlarmGRADE_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMEVENTAlarmGRADE_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMEVENTAlarmGRADE_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMEVENTAlarmGRADE_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMEVENTCheckUser_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMEVENTCheckUser_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMEVENTCheckUser_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMEVENTCheckUser_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','62');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table63VersionMake: Boolean;
var
  stSql : string;
begin

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','USEARMLEN','TRUE');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','ALARMLEN','16');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','63');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table64VersionMake: Boolean;
var
  stSql : string;
begin
  //tb_ACCESSEVENT 이름 사이즈 변경
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSEVENTEMCODEChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ACCESSEVENTEMCODEChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSEVENTEMCODEChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSEVENTEMCODEChange
  else Exit;
  DataModule1.ProcessEventExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','64');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table65VersionMake: Boolean;
var
  stSql : string;
begin
  G_nDaemonServerVersion := DataModule1.GetDaemonVersion;
  if (G_nDaemonServerVersion > 0) and (G_nDaemonServerVersion < 4) then
  begin
    stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','SCHASYNC','0');
  end else
  begin
    stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','SCHASYNC','1');
  end;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','SCHDEVICE','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','65');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table66VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','COMMLOG','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','CARDLOG','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','66');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table67VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEDeviceCode_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEDeviceCode_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEDeviceCode_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEDeviceCode_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICERegSend_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICERegSend_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICERegSend_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICERegSend_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMDEVICERegSend_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMDEVICERegSend_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMDEVICERegSend_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMDEVICERegSend_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOORRegSend_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOORRegSend_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DOORRegSend_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOORRegSend_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_READERDoorPosi_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_READERDoorPosi_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_READERDoorPosi_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_READERDoorPosi_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_READERBuildPosi_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_READERBuildPosi_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_READERBuildPosi_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_READERBuildPosi_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_READERRegSend_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_READERRegSend_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_READERRegSend_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_READERRegSend_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ZONEDEVICEDelayUse_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ZONEDEVICEDelayUse_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ZONEDEVICEDelayUse_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ZONEDEVICEDelayUse_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ZONEDEVICEPortRecovery_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ZONEDEVICEPortRecovery_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ZONEDEVICEPortRecovery_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ZONEDEVICEPortRecovery_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ZONEDEVICERegSend_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ZONEDEVICERegSend_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ZONEDEVICERegSend_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ZONEDEVICERegSend_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);


  stSql := 'Insert Into TB_CURRENTDAEMON(CU_STATECODE,CU_STATEVALUE) ';
  stSql := stSql + ' Values( ''REGPW'',''' + MimeEncodeString('1234') + ''') ';
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','67');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table68VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMEVENTCardno_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMEVENTCardno_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMEVENTCardno_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMEVENTCardno_Add
  else Exit;
  DataModule1.ProcessEventExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMDEVICETelNo_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMDEVICETelNo_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMDEVICETelNo_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMDEVICETelNo_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMDEVICEmemo_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMDEVICEmemo_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMDEVICEmemo_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMDEVICEmemo_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','68');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table69VersionMake: Boolean;
var
  stSql : string;
begin

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICELinkusSystemID_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICELinkusSystemID_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICELinkusSystemID_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICELinkusSystemID_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICELinkusTelNo_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICELinkusTelNo_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICELinkusTelNo_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICELinkusTelNo_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEArmControlRing_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEArmControlRing_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEArmControlRing_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEArmControlRing_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEDisArmControlRing_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEDisArmControlRing_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEDisArmControlRing_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEDisArmControlRing_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEDeviceType_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEDeviceType_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEDeviceType_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEDeviceType_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEPowerType_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEPowerType_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEPowerType_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEPowerType_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEOutDelay_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEOutDelay_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEOutDelay_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEOutDelay_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEInDelay_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEInDelay_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEInDelay_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEInDelay_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEJaejung_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEJaejung_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEJaejung_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEJaejung_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEDoor1Type_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEDoor1Type_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEDoor1Type_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEDoor1Type_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEDoor2Type_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEDoor2Type_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEDoor2Type_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEDoor2Type_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEReaderType_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEReaderType_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEReaderType_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEReaderType_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','69');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table6VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','LOGINTYPE','TRUE');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('MOSTYPE','ALARMVIEW','REAL');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','fdmsType','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','6');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table70VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := 'update TB_DOOR set DO_UPDATE = ''Y'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'update TB_ALARMDEVICE set AL_UPDATE = ''Y'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'update TB_ACCESSDEVICE set AC_PTTYPE = ''1'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','70');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table71VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_CURRENTDAEMON(CU_STATECODE,CU_STATEVALUE) ';
  stSql := stSql + ' Values( ''DEVICESORT'',''00000000'') ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','71');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table72VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_READERReaderVer_Add
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_READERReaderVer_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_READERReaderVer_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_READERReaderVer_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','72');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table73VersionMake: Boolean;
VAR
  stSql : string;
begin

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEReaderType_Delete
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEReaderType_Delete
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSDEVICEReaderType_Delete
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEReaderType_Delete
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMDEVICEOUTDELAY_Delete
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMDEVICEOUTDELAY_Delete
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMDEVICEOUTDELAY_Delete
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMDEVICEOUTDELAY_Delete
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMDEVICEINDELAY_Delete
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMDEVICEINDELAY_Delete
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMDEVICEINDELAY_Delete
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMDEVICEINDELAY_Delete
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMDEVICEALARMID_Delete
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMDEVICEALARMID_Delete
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMDEVICEALARMID_Delete
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMDEVICEALARMID_Delete
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMDEVICEMUXTEL_Delete
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMDEVICEMUXTEL_Delete
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMDEVICEMUXTEL_Delete
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMDEVICEMUXTEL_Delete
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMDEVICERINGCOUNT_Delete
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMDEVICERINGCOUNT_Delete
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMDEVICERINGCOUNT_Delete
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMDEVICERINGCOUNT_Delete
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','73');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table74VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','fdmsEMCODE','FALSE'); //RUC 센터 때문에 추가 했는데... RUC 특화로 사용 안함
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','74');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table75VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_DEVICESETTINGINFO
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_DEVICESETTINGINFO
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_DEVICESETTINGINFO
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_DEVICESETTINGINFO
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','75');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table76VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_PERRELAYCONFIG(';
  stSql := stSql + 'PC_CONFIGCODE,';
  stSql := stSql + 'PC_CONFIGVALUE,';
  stSql := stSql + 'PC_CONFIGDETAIL) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''GETDATETIME'',';
  stSql := stSql + '''00000000000000'',';
  stSql := stSql + '''최종가져온시간'')';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','76');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table77VersionMake: Boolean;
var
  stSql : string;
begin
  Exit; //Event 테이블 변경 시 문제 소지 발생 해서 꼭 필요한 경우 유틸로 만들어서 작성

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSEVENTCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSEVENTCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSEVENTCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSEVENTCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSEVENTJijumCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSEVENTJijumCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSEVENTJijumCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSEVENTJijumCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSEVENTDepartCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSEVENTDepartCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ACCESSEVENTDepartCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSEVENTDepartCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ADMINCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ADMINCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ADMINCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ADMINCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ADMINJijumCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ADMINJijumCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ADMINJijumCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ADMINJijumCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ADMINDepartCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ADMINDepartCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ADMINDepartCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ADMINDepartCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ADMINCOMPANYCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ADMINCOMPANYCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ADMINCOMPANYCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ADMINCOMPANYCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ADMINCOMPANYJijumCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ADMINCOMPANYJijumCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ADMINCOMPANYJijumCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ADMINCOMPANYJijumCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ADMINCOMPANYDepartCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ADMINCOMPANYDepartCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ADMINCOMPANYDepartCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ADMINCOMPANYDepartCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATDAYSUMMARYCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATDAYSUMMARYCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATDAYSUMMARYCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATDAYSUMMARYCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATEMPEXTRACompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATEMPEXTRACompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATEMPEXTRACompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATEMPEXTRACompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATEVENTCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATEVENTCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATEVENTCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATEVENTCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATMONTHEXTRACompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATMONTHEXTRACompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATMONTHEXTRACompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATMONTHEXTRACompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATMONTHSUMMARYCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATMONTHSUMMARYCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATMONTHSUMMARYCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATMONTHSUMMARYCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ATVACATIONCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ATVACATIONCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ATVACATIONCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ATVACATIONCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_BASEPAYCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_BASEPAYCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_BASEPAYCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_BASEPAYCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_CARDCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_CARDCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_CARDCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_CARDCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_COMPANYCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_COMPANYCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_COMPANYCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_COMPANYCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_COMPANYJijumCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_COMPANYJijumCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_COMPANYJijumCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_COMPANYJijumCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_COMPANYDepartCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_COMPANYDepartCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_COMPANYDepartCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_COMPANYDepartCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPHISCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPHISCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPHISCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPHISCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEECompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEECompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEECompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEECompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEEJijumCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEEJijumCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEEJijumCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEEJijumCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEEDepartCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEEDepartCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEEDepartCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEEDepartCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_EMPLOYEEPosiCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_EMPLOYEEPosiCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_EMPLOYEEPosiCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_EMPLOYEEPosiCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_FOODEVENTCompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_FOODEVENTCompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_FOODEVENTCompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_FOODEVENTCompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_FOODGRADECompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_FOODGRADECompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_FOODGRADECompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_FOODGRADECompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_POSICompanyCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_POSICompanyCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_POSICompanyCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_POSICompanyCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_POSIPosiCodeChange('10')
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_POSIPosiCodeChange('10')
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_POSIPosiCodeChange('10')
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_POSIPosiCodeChange('10')
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','77');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table78VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','PCSERVERTM','1','PCServerDelayTime');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','78');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table79VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','C_INTERVAL','5','ClientSendInterval(ms)');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','79');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table7VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_ALARMSHOW
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_ALARMSHOW
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_ALARMSHOW
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_ALARMSHOW
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','7');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table80VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','INOUTTYPE','0','외출-0:사용안함,1:리더,2:버튼');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','STARTWKNO','1','출근리더(버튼)번호');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','OFFWKNO','2','퇴근리더(버튼)번호');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','WKOUTNO','3','외출리더(버튼)번호');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','WKINNO','4','복귀리더(버튼)번호');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','80');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table81VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := 'Delete From TB_ALARMSTATUSCODE ';
  stSql := stSql + ' Where AL_ALARMSTATUSCODE = ''F1'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','81');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table82VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('ATTEND','ATPRSERVER','FALSE','근태전용서버');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','82');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table83VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FDMSDBTYPE','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FDMSDBIP','127.0.0.1');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FDMSDBPORT','1433');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FDMSUSER','sa');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FDMSDBNAME','sh_fpsystem');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','83');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table84VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_ALARMGUBUNCODE
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_ALARMGUBUNCODE
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_ALARMGUBUNCODE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_ALARMGUBUNCODE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertTB_ALARMGUBUNCODE('001','구분1');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_ALARMGUBUNCODE('002','구분2');
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMDEVICEARMGUBUN
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMDEVICEARMGUBUN
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_ALARMDEVICEARMGUBUN
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMDEVICEARMGUBUN
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','84');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table85VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','KTTSUSES','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','KTTSIP','121.170.197.175');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','KTTSPORT','7001');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','KTTCPORT','4101');
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','85');
  DataModule1.ProcessExecSQL(stSql);

end;


function TDataBaseConfig.Table86VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_ALARMMODENOTCARD('006','CtrlCenter');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('006','a','R3');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_NOTCARDALARMCODE('006','d','R3');
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' Update TB_ALARMEVENT set al_operator = ''CtrlCenter'' ';
  if DBTYPE = 'PG' then stSql := stsql + ' Where substr(al_operator,1,10) = ''.KTLinkUs.'' ';
  if DBTYPE = 'MSSQL' then stSql := stsql + ' Where substring(al_operator,1,10) = ''.KTLinkUs.'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','86');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table87VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','CLIENTSEQ','N');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('DAEMON','CLIENTTIME','5');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','87');
  DataModule1.ProcessExecSQL(stSql);

end;


function TDataBaseConfig.Table88VersionMake: Boolean;
var
  stSql : string;
begin

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','FDMSNOAUTO','Y','지문번호자동생성유무');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','88');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table89VersionMake: Boolean;
var
  stSql : string;
begin

  //TB_CONFIG
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_CONFIG_ValueChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_CONFIG_ValueChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_CONFIG_ValueChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_CONFIG_ValueChange
  else Exit;
  DataModule1.ProcessEventExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','89');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table8VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_EMPHIS
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_EMPHIS
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_EMPHIS
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_EMPHIS
  else Exit;
  DataModule1.ProcessExecSQL(stSql);
  if DBTYPE = 'FB' then
  begin
    stSql := 'CREATE SEQUENCE GEN_TB_EMPHIS_ID ';
    DataModule1.ProcessExecSQL(stSql);

    stSql := 'CREATE OR ALTER TRIGGER TB_EMPHIS_BI FOR TB_EMPHIS ';
    stSql := stSql + ' ACTIVE BEFORE INSERT POSITION 0 ';
    stSql := stSql + ' AS ';
    stSql := stSql + ' BEGIN ';
    stSql := stSql + '   IF (NEW.SEQ IS NULL) THEN ';
    stSql := stSql + '     NEW.SEQ = GEN_ID(GEN_TB_EMPHIS_ID,1); ';
    stSql := stSql + ' END ';
    DataModule1.ProcessExecSQL(stSql);
  end;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','8');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table90VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_PERMITCODE( ';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' PE_PERMITCODE,' ;
  stSql := stSql + ' PE_PERMITNAME) ';
  stSql := stSql + ' Values( ' ;
  stSql := stSql + '''' + GROUPCODE + ''',' ;
  stSql := stSql + '''3'',';
  stSql := stSql + '''방범경계승인'')'  ;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Insert Into TB_PERMITCODE( ';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' PE_PERMITCODE,';
  stSql := stSql + ' PE_PERMITNAME) '  ;
  stSql := stSql + ' Values( ';
  stSql := stSql + '''' + GROUPCODE + ''',' ;
  stSql := stSql + '''4'',';
  stSql := stSql + '''방범해제승인'')';
  DataModule1.ProcessExecSQL(stSql);


  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','90');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table91VersionMake: Boolean;
var
  stSql : string;
begin
//  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','ALARMCOUNT','50');
//  DataModule1.ProcessExecSQL(stSql);

  //기기 테이블에 MUXID 추가
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEMuxID_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ACCESSDEVICEMuxID_Add
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEMuxID_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEMuxID_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Update TB_ACCESSDEVICE set AC_MUXID = ''00'' ';
  DataModule1.ProcessExecSQL(stSql);

  //기기 테이블에 데코더아이디 추가
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICEDecoderID_Add
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ACCESSDEVICEDecoderID_Add
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICEDecoderID_Add
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICEDecoderID_Add
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Update TB_ACCESSDEVICE set AC_DECODERID = ''KTT00'' ';
  DataModule1.ProcessExecSQL(stSql);

  //알람 이벤트신호 Count
  stSql := 'Insert Into TB_CURRENTDAEMON(CU_STATECODE,CU_STATEVALUE) ';
  stSql := stSql + ' Values( ''KTALARMSEQ'',''0'') ';
  DataModule1.ProcessExecSQL(stSql);

  //PAM Cycle Time - Pre Alive Manager  회선체크
  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','KTTPCYCLE','30');
  DataModule1.ProcessExecSQL(stSql);

  //관제이벤트 매핑 코드 테이블 생성
  if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_KTTMAPPINGCODE
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.CreateTB_KTTMAPPINGCODE
  else if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_KTTMAPPINGCODE
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_KTTMAPPINGCODE
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  //가입자확장기 단선
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','21','11','E701');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','21','11','E701');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','21','11','E701');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','NF','11','E701');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','NF','11','E701');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','NF','11','E701');
  DataModule1.ProcessExecSQL(stSql);
  //가입자확장기 복구
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','22','11','R701');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','22','11','R701');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','22','11','R701');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','NR','11','R701');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','NR','11','R701');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','NR','11','R701');
  DataModule1.ProcessExecSQL(stSql);
  //가입자확장기 열림
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','23','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','23','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','23','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','CO','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','CO','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','CO','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','23','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','23','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','23','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','CO','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','CO','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','CO','11','E137');
  DataModule1.ProcessExecSQL(stSql);
  //가입자확장기 닫힘
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','24','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','24','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','24','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','CC','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','CC','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','CC','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','24','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','24','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','24','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','CC','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','CC','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','CC','11','R137');
  DataModule1.ProcessExecSQL(stSql);
  //카드리더 단선
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','A','31','11','E138');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','D','31','11','E138');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','N','31','11','E138');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','A','NF','11','E138');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','D','NF','11','E138');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','N','NF','11','E138');
  DataModule1.ProcessExecSQL(stSql);
  //카드리더 복구
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','A','32','11','R138');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','D','32','11','R138');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','N','32','11','R138');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','A','NR','11','R138');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','D','NR','11','R138');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','N','NR','11','R138');
  DataModule1.ProcessExecSQL(stSql);
  //카드리더 열림
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','A','33','11','E136');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','D','33','11','E136');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','N','33','11','E136');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','A','CO','11','E136');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','D','CO','11','E136');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','N','CO','11','E136');
  DataModule1.ProcessExecSQL(stSql);
  //카드리더 닫힘
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','A','34','11','R136');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','D','34','11','R136');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','N','34','11','R136');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','A','CC','11','R136');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','D','CC','11','R136');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('CD','N','CC','11','R136');
  DataModule1.ProcessExecSQL(stSql);
  //AC 단선
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','AF','11','E301');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','AF','11','E301');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','AF','11','E301');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','AF','11','E301');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','AF','11','E301');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','AF','11','E301');
  DataModule1.ProcessExecSQL(stSql);
  //AC 복구
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','AN','11','R301');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','AN','11','R301');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','AN','11','R301');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','AN','11','R301');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','AN','11','R301');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','AN','11','R301');
  DataModule1.ProcessExecSQL(stSql);
  //카드경계모드변경
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','A1','11','E400');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','A1','11','E400');
  DataModule1.ProcessExecSQL(stSql);
  //카드해제모드변경
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','C1','11','R400');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','C1','11','R400');
  DataModule1.ProcessExecSQL(stSql);
  //강제경계모드변경
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','A3','11','E300');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','A3','11','E300');
  DataModule1.ProcessExecSQL(stSql);
  //강제해제모드변경
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','C3','11','R300');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','C3','11','R300');
  DataModule1.ProcessExecSQL(stSql);
  //PC경계모드변경
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','A4','11','E620');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','A4','11','E620');
  DataModule1.ProcessExecSQL(stSql);
  //PC해제모드변경
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','C4','11','R620');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','C4','11','R620');
  DataModule1.ProcessExecSQL(stSql);
  //관제경계모드변경
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','A2','11','E610');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','A2','11','E610');
  DataModule1.ProcessExecSQL(stSql);
  //관제해제모드변경
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','C2','11','R610');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','C2','11','R610');
  DataModule1.ProcessExecSQL(stSql);
  //전원리셋
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','R1','01','E305');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','R2','01','E305');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','R2','01','E305');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','R1','01','E305');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','R2','01','E305');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','R2','01','E305');
  DataModule1.ProcessExecSQL(stSql);
  //국선체크
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','PC','11','E600');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','PC','11','E600');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','PC','11','E600');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','PC','11','E600');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','PC','11','E600');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','PC','11','E600');
  DataModule1.ProcessExecSQL(stSql);
  //프로그램댐퍼
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','EM','11','E306');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','EM','11','E306');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','EM','11','E306');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','EM','11','E306');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','EM','11','E306');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','EM','11','E306');
  DataModule1.ProcessExecSQL(stSql);
  //TF단말확인
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','PF','11','E139');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','PF','11','E139');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','PF','11','E139');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','PF','11','E139');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','PF','11','E139');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','PF','11','E139');
  DataModule1.ProcessExecSQL(stSql);
  //DC 단선
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','DL','11','E302');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','DL','11','E302');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','DL','11','E302');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','DL','11','E302');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','DL','11','E302');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','DL','11','E302');
  DataModule1.ProcessExecSQL(stSql);
  //DC 복구
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','DR','11','R302');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','DR','11','R302');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','N','DR','11','R302');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','DR','11','R302');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','DR','11','R302');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','N','DR','11','R302');
  DataModule1.ProcessExecSQL(stSql);
  //침입
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','ZP','11','E130');  //오픈
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','ZS','11','E130');  //쇼트
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','ZP','11','E130');  //오픈
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','ZS','11','E130');  //쇼트
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','FI','11','E130');  //화재
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','E1','11','E130');  //비상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','G1','11','E130');  //가스
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','Q1','11','E130');  //설비
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','FI','11','E130');  //화재
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','E1','11','E130');  //비상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','G1','11','E130');  //가스
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','Q1','11','E130');  //설비
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','ZP','11','E130');  //오픈
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','ZS','11','E130');  //쇼트
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','ZP','11','E130');  //오픈
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','ZS','11','E130');  //쇼트
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','FI','11','E130');  //화재
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','E1','11','E130');  //비상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','G1','11','E130');  //가스
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','Q1','11','E130');  //설비
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','FI','11','E130');  //화재
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','E1','11','E130');  //비상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','G1','11','E130');  //가스
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','Q1','11','E130');  //설비
  DataModule1.ProcessExecSQL(stSql);
  //침입복구
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','ZN','11','R130');  //정상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','ZN','11','R130');  //정상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','F2','11','R130');  //화재
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','E2','11','R130');  //비상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','G2','11','R130');  //가스
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','A','Q2','11','R130');  //설비
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','F2','11','R130');  //화재
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','E2','11','R130');  //비상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','G2','11','R130');  //가스
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('MN','D','Q2','11','R130');  //설비
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','ZN','11','R130');  //정상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','ZN','11','R130');  //정상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','F2','11','R130');  //화재
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','E2','11','R130');  //비상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','G2','11','R130');  //가스
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','A','Q2','11','R130');  //설비
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','F2','11','R130');  //화재
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','E2','11','R130');  //비상
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','G2','11','R130');  //가스
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertTB_KTTMAPPINGCODE('EX','D','Q2','11','R130');  //설비
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('21','가입자확장기단선','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('22','가입자확장기복구','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('31','카드리더단선','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('32','카드리더복구','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('41','감지기확장기단선','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('42','감지기확장기복구','0','0','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('23','기계열림','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('24','기계닫힘','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('33','카드리더열림','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('34','카드리더닫힘','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('43','감지기확장기열림','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('44','감지기확장기닫힘','0','0','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := 'Update TB_ALARMSTATUSCODE set AL_ALARMNAME = ''화재알람'' where AL_ALARMSTATUSCODE = ''FI'' ';
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('F2','화재존복구','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('E2','비상존복구','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('G2','가스존복구','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('Q2','설비존복구','0','0','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('EM','프로그램댐퍼','0','0','0');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMSTATUSCODE('PF','TF단말확인','0','0','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Network','21');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Network','22');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Network','31');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Network','32');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Network','41');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('Network','42');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('기기관련','23');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('기기관련','24');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('기기관련','33');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('기기관련','34');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('기기관련','43');
  DataModule1.ProcessExecSQL(stSql);
  stSql := CommonSql.InsertIntoTB_ALARMCODEGROUP('기기관련','44');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','91');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table92VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','DDNSUSES','0');
  DataModule1.ProcessExecSQL(stSql);
  //DDNS Server IP
  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','DDNS_SIP','121.170.197.180');
  DataModule1.ProcessExecSQL(stSql);
  //DDNS Server PORT
  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','DDNS_SPORT','9300');
  DataModule1.ProcessExecSQL(stSql);
  //DDNS Query Server IP
  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','DDNS_QIP','121.170.197.180');
  DataModule1.ProcessExecSQL(stSql);
  //DDNS Query Server PORT
  stSql := CommonSql.InsertIntoTB_CONFIG('KTT','DDNS_QPORT','9301');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','92');
  DataModule1.ProcessExecSQL(stSql);
end;

function TDataBaseConfig.Table93VersionMake: Boolean;
var
  stSql : string;
begin
  stSql := CommonSql.InsertIntoTB_CONFIG('FIRE','RELAYUSE','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('FIRE','RELAYNO','6');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('FIRE','RELAYTIME','2');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','93');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table94VersionMake: Boolean;
var
  stSql : string;
begin
  //TB_ALARMDEVIC 이름 사이즈 변경
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ALARMDEVICENameChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ALARMDEVICENameChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ALARMDEVICENameChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ALARMDEVICENameChange
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  //TB_ACCESSDEVIC 이름 사이즈 변경
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_ACCESSDEVICENameChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_ACCESSDEVICENameChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_ACCESSDEVICENameChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_ACCESSDEVICENameChange
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  //TB_DOOR 이름 사이즈 변경
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DOORNameChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_DOORNameChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DOORNameChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DOORNameChange
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  //TB_FOOD 이름 사이즈 변경
  if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_FOODNameChange
  else if DBTYPE = 'MSSQL' then stSql := MSSQL.AlterTB_FOODNameChange
  else if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_FOODNameChange
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_FOODNameChange
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','94');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table95VersionMake: Boolean;
var
  stSql : string;
begin

  stSql := CommonSql.InsertIntoTB_CONFIG('FOOD','DUPEVENT','0');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','95');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table96VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_RELAYUNIVERCITY
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_RELAYUNIVERCITY
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_RELAYUNIVERCITY
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_RELAYUNIVERCITY
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','96');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table97VersionMake: Boolean;
var
  stSql : string;
begin    {
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_DEVICECARDNO_downseq
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_DEVICECARDNO_downseq
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_DEVICECARDNO_downseq
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_DEVICECARDNO_downseq
  else Exit;
  DataModule1.ProcessExecSQL(stSql);  }

  dmAdoQuery.NotGradePermitDelete;

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','97');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table98VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.AlterTB_KTTMAPPINGCODE_SendUse
  else if DBTYPE = 'PG' then stSql := PostGreSql.AlterTB_KTTMAPPINGCODE_SendUse
  else if DBTYPE = 'MSSQL' then stSql := MSSql.AlterTB_KTTMAPPINGCODE_SendUse
  else if DBTYPE = 'FB' then stSql := FireBird.AlterTB_KTTMAPPINGCODE_SendUse
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := ' update TB_KTTMAPPINGCODE set senduse = ''Y'' ';
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','98');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table99VersionMake: Boolean;
var
  stSql : string;
begin

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','DELCARDUSE','N');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','DCARDSTART','11');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.InsertIntoTB_CONFIG('RELAY','DELCARDLEN','3');
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','99');
  DataModule1.ProcessExecSQL(stSql);

end;

function TDataBaseConfig.Table9VersionMake: Boolean;
var
  stSql : string;
begin
  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_SERVERCARDRELAY
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_SERVERCARDRELAY
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_SERVERCARDRELAY
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_SERVERCARDRELAY
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_SERVERCARDRELAYHIS
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_SERVERCARDRELAYHIS
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_SERVERCARDRELAYHIS
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_SERVERCARDRELAYHIS
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  if DBTYPE = 'MDB' then stSql := MDBSql.CreateTB_PERRELAYCONFIG
  else if DBTYPE = 'PG' then stSql := PostGreSql.CreateTB_PERRELAYCONFIG
  else if DBTYPE = 'MSSQL' then stSql := MSSql.CreateTB_PERRELAYCONFIG
  else if DBTYPE = 'FB' then stSql := FireBird.CreateTB_PERRELAYCONFIG
  else Exit;
  DataModule1.ProcessExecSQL(stSql);

  stSql := CommonSql.UpdateTB_CONFIG('COMMON','TABLE_VER','9');
  DataModule1.ProcessExecSQL(stSql);
end;

procedure TDataBaseConfig.TableVersionCheck;
var
  nTableVersion : integer;
begin
  nTableVersion := GetVersion;

  if nTableVersion < 1 then Table1VersionMake;
  if nTableVersion < 2 then Table2VersionMake;
  if nTableVersion < 3 then Table3VersionMake;
  if nTableVersion < 4 then Table4VersionMake;
  if nTableVersion < 5 then Table5VersionMake;
  if nTableVersion < 6 then Table6VersionMake;
  if nTableVersion < 7 then Table7VersionMake;
  if nTableVersion < 8 then Table8VersionMake;
  if nTableVersion < 9 then Table9VersionMake;
  if nTableVersion < 10 then Table10VersionMake;
  if nTableVersion < 11 then Table11VersionMake;
  if nTableVersion < 12 then Table12VersionMake;
  if nTableVersion < 13 then Table13VersionMake;
  if nTableVersion < 14 then Table14VersionMake;
  if nTableVersion < 15 then Table15VersionMake;
  if nTableVersion < 16 then Table16VersionMake;
  if nTableVersion < 17 then Table17VersionMake;
  if nTableVersion < 18 then Table18VersionMake;
  if nTableVersion < 19 then Table19VersionMake;
  if nTableVersion < 20 then Table20VersionMake;
  if nTableVersion < 21 then Table21VersionMake;
  if nTableVersion < 22 then Table22VersionMake;
  if nTableVersion < 23 then Table23VersionMake;
  if nTableVersion < 24 then Table24VersionMake;
  if nTableVersion < 25 then Table25VersionMake;
  if nTableVersion < 26 then Table26VersionMake;
  if nTableVersion < 27 then Table27VersionMake;
  if nTableVersion < 28 then Table28VersionMake;
  if nTableVersion < 29 then Table29VersionMake;
  if nTableVersion < 30 then Table30VersionMake;
  if nTableVersion < 31 then Table31VersionMake;
  if nTableVersion < 32 then Table32VersionMake;
  if nTableVersion < 33 then Table33VersionMake;
  if nTableVersion < 34 then Table34VersionMake;
  if nTableVersion < 35 then Table35VersionMake;
  if nTableVersion < 36 then Table36VersionMake;
  if nTableVersion < 37 then Table37VersionMake;
  if nTableVersion < 38 then Table38VersionMake;
  if nTableVersion < 39 then Table39VersionMake;
  if nTableVersion < 40 then Table40VersionMake;
  if nTableVersion < 41 then Table41VersionMake;
  if nTableVersion < 42 then Table42VersionMake;
  if nTableVersion < 43 then Table43VersionMake;
  if nTableVersion < 44 then Table44VersionMake;
  if nTableVersion < 45 then Table45VersionMake;
  if nTableVersion < 46 then Table46VersionMake;
  if nTableVersion < 47 then Table47VersionMake;
  if nTableVersion < 48 then Table48VersionMake;
  if nTableVersion < 49 then Table49VersionMake;
  if nTableVersion < 50 then Table50VersionMake;
  if nTableVersion < 51 then Table51VersionMake;
  if nTableVersion < 52 then Table52VersionMake;
  if nTableVersion < 53 then Table53VersionMake;
  if nTableVersion < 54 then Table54VersionMake;
  if nTableVersion < 55 then Table55VersionMake;
  if nTableVersion < 56 then Table56VersionMake;
  if nTableVersion < 57 then Table57VersionMake;
  if nTableVersion < 58 then Table58VersionMake;
  if nTableVersion < 59 then Table59VersionMake;
  if nTableVersion < 60 then Table60VersionMake;
  if nTableVersion < 61 then Table61VersionMake;
  if nTableVersion < 62 then Table62VersionMake;
  if nTableVersion < 63 then Table63VersionMake;
//  if nTableVersion < 64 then Table64VersionMake; ==>EM_CODE 는 20자리만 가져가도 문제 없을듯
  if nTableVersion < 65 then Table65VersionMake;
  if nTableVersion < 66 then Table66VersionMake;
  if nTableVersion < 67 then Table67VersionMake;
  if nTableVersion < 68 then Table68VersionMake;
  if nTableVersion < 69 then Table69VersionMake;
  if nTableVersion < 70 then Table70VersionMake;
  if nTableVersion < 71 then Table71VersionMake;
  if nTableVersion < 72 then Table72VersionMake;
  if nTableVersion < 73 then Table73VersionMake;
  if nTableVersion < 74 then Table74VersionMake;
  if nTableVersion < 75 then Table75VersionMake;
  if nTableVersion < 76 then Table76VersionMake;
  //if nTableVersion < 77 then Table77VersionMake;
  if nTableVersion < 78 then Table78VersionMake;
  if nTableVersion < 79 then Table79VersionMake;
  if nTableVersion < 80 then Table80VersionMake;
  if nTableVersion < 81 then Table81VersionMake;
  if nTableVersion < 82 then Table82VersionMake;
  if nTableVersion < 83 then Table83VersionMake;
  if nTableVersion < 84 then Table84VersionMake;
  if nTableVersion < 85 then Table85VersionMake;
  if nTableVersion < 86 then Table86VersionMake;
  if nTableVersion < 87 then Table87VersionMake;
  if nTableVersion < 88 then Table88VersionMake;
  if nTableVersion < 89 then Table89VersionMake;
  if nTableVersion < 90 then Table90VersionMake;
  if nTableVersion < 91 then Table91VersionMake;
  if nTableVersion < 92 then Table92VersionMake;
  if nTableVersion < 93 then Table93VersionMake;
  if nTableVersion < 94 then Table94VersionMake;
  if nTableVersion < 95 then Table95VersionMake;
  if nTableVersion < 96 then Table96VersionMake;
  if nTableVersion < 97 then Table97VersionMake;
  if nTableVersion < 98 then Table98VersionMake;
  if nTableVersion < 99 then Table99VersionMake;
  if nTableVersion < 100 then Table100VersionMake;
  if nTableVersion < 101 then Table101VersionMake;
  if nTableVersion < 102 then Table102VersionMake;
  if nTableVersion < 103 then Table103VersionMake;
  if nTableVersion < 104 then Table104VersionMake;
  if nTableVersion < 105 then Table105VersionMake;
  if nTableVersion < 106 then Table106VersionMake;
  if nTableVersion < 107 then Table107VersionMake;
  if nTableVersion < 108 then Table108VersionMake;
  if nTableVersion < 109 then Table109VersionMake;
  if nTableVersion < 110 then Table110VersionMake;
  if nTableVersion < 111 then Table111VersionMake;
  if nTableVersion < 112 then Table112VersionMake;
  if nTableVersion < 113 then Table113VersionMake;
  if nTableVersion < 114 then Table114VersionMake;
  if nTableVersion < 115 then Table115VersionMake;
  if nTableVersion < 116 then Table116VersionMake;
  if nTableVersion < 117 then Table117VersionMake;
  if nTableVersion < 118 then Table118VersionMake;
  if nTableVersion < 119 then Table119VersionMake;
  if nTableVersion < 120 then Table120VersionMake;
  if nTableVersion < 121 then Table121VersionMake;
  if nTableVersion < 122 then Table122VersionMake;
  if nTableVersion < 123 then Table123VersionMake;
  if nTableVersion < 124 then Table124VersionMake;
  if nTableVersion < 125 then Table125VersionMake;
  if nTableVersion < 126 then Table126VersionMake;
  if nTableVersion < 127 then Table127VersionMake;
  if nTableVersion < 128 then Table128VersionMake;
  if nTableVersion < 129 then Table129VersionMake;
  if nTableVersion < 130 then Table130VersionMake;
  if nTableVersion < 131 then Table131VersionMake;
  if nTableVersion < 132 then Table132VersionMake;
  if nTableVersion < 133 then Table133VersionMake;
  if nTableVersion < 134 then Table134VersionMake;
  if nTableVersion < 135 then Table135VersionMake;
  if nTableVersion < 136 then Table136VersionMake;
  if nTableVersion < 137 then Table137VersionMake;
  if nTableVersion < 138 then Table138VersionMake;
  if nTableVersion < 139 then Table139VersionMake;
  if nTableVersion < 140 then Table140VersionMake;
  if nTableVersion < 141 then Table141VersionMake;
  if nTableVersion < 142 then Table142VersionMake;
  if nTableVersion < 143 then Table143VersionMake;
  if nTableVersion < 144 then Table144VersionMake;
  if nTableVersion < 146 then Table146VersionMake;
  if nTableVersion < 147 then Table147VersionMake;
  if nTableVersion < 148 then Table148VersionMake;
  if nTableVersion < 149 then Table149VersionMake;
  if nTableVersion < 150 then Table150VersionMake;
  if nTableVersion < 151 then Table151VersionMake;
  if nTableVersion < 152 then Table152VersionMake;
  if nTableVersion < 153 then Table153VersionMake;
  if nTableVersion < 154 then Table154VersionMake;
  if nTableVersion < 155 then Table155VersionMake;
  if nTableVersion < 156 then Table156VersionMake;
  if nTableVersion < 157 then Table157VersionMake;
  if nTableVersion < 158 then Table158VersionMake;
  if nTableVersion < 159 then Table159VersionMake;
  if nTableVersion < 160 then Table160VersionMake;
  if nTableVersion < 161 then Table161VersionMake;
  if nTableVersion < 162 then Table162VersionMake;
  if nTableVersion < 163 then Table163VersionMake;
  if nTableVersion < 164 then Table164VersionMake;
  if nTableVersion < 165 then Table165VersionMake;
  if nTableVersion < 166 then Table166VersionMake;
  if nTableVersion < 167 then Table167VersionMake;
  if nTableVersion < 168 then Table168VersionMake;
  if nTableVersion < 169 then Table169VersionMake;
  if nTableVersion < 170 then Table170VersionMake;
  if nTableVersion < 171 then Table171VersionMake;
  if nTableVersion < 172 then Table172VersionMake;
  if nTableVersion < 173 then Table173VersionMake;
  if nTableVersion < 174 then Table174VersionMake;
  if nTableVersion < 175 then Table175VersionMake;
  if nTableVersion < 176 then Table176VersionMake;
  if nTableVersion < 177 then Table177VersionMake;
  if nTableVersion < 178 then Table178VersionMake;
  if nTableVersion < 179 then Table179VersionMake;
  if nTableVersion < 180 then Table180VersionMake;
  if nTableVersion < 181 then Table181VersionMake;
  if nTableVersion < 182 then Table182VersionMake;
  if nTableVersion < 183 then Table183VersionMake;
  if nTableVersion < 184 then Table184VersionMake;
  if nTableVersion < 185 then Table185VersionMake;
  if nTableVersion < 186 then Table186VersionMake;
  if nTableVersion < 187 then Table187VersionMake;
  if nTableVersion < 188 then Table188VersionMake;
  if nTableVersion < 189 then Table189VersionMake;
  if nTableVersion < 190 then Table190VersionMake;
  if nTableVersion < 191 then Table191VersionMake;
  if nTableVersion < 192 then Table192VersionMake;
  if nTableVersion < 193 then Table193VersionMake;
  if nTableVersion < 194 then Table194VersionMake;
  if nTableVersion < 195 then Table195VersionMake;
  if nTableVersion < 196 then Table196VersionMake;
  if nTableVersion < 197 then Table197VersionMake;
  if nTableVersion < 198 then Table198VersionMake;
  if nTableVersion < 199 then Table199VersionMake;
  if nTableVersion < 200 then Table200VersionMake;
  if nTableVersion < 201 then Table201VersionMake;
  if nTableVersion < 202 then Table202VersionMake;
  if nTableVersion < 203 then Table203VersionMake;
  if nTableVersion < 204 then Table204VersionMake;
  if nTableVersion < 205 then Table205VersionMake;

end;

procedure TfmDataBaseConfig.edDataBaseName1Exit(Sender: TObject);
begin
  if chk_Copy.Checked then
  begin
    edDataBaseName2.Text := edDataBaseName1.Text;
    edDataBaseName3.Text := edDataBaseName1.Text;
  end;

end;

procedure TfmDataBaseConfig.edPasswd1Exit(Sender: TObject);
begin
    if chk_Copy.Checked then
    begin
      edPasswd2.Text := edPasswd1.Text;
      edPasswd3.Text := edPasswd1.Text;
    end;

end;

procedure TfmDataBaseConfig.edUserid1Exit(Sender: TObject);
begin
    if chk_Copy.Checked then
    begin
      edUserid2.Text := edUserid1.Text;
      edUserid3.Text := edUserid1.Text;
    end;

end;

procedure TfmDataBaseConfig.edServerPort1Exit(Sender: TObject);
begin
    if chk_Copy.Checked then
    begin
      edServerPort2.Text := edServerPort1.Text;
      edServerPort3.Text := edServerPort1.Text;
    end;

end;

procedure TfmDataBaseConfig.edServerIP1Exit(Sender: TObject);
begin
    if chk_Copy.Checked then
    begin
      edServerIP2.Text := edServerIP1.Text;
      edServerIP3.Text := edServerIP1.Text;
    end;

end;

function TDataBaseConfig.UpdateTB_CARDPoitionNum(aGroupCode,
  aCardNo: string; aPositionNumber: integer): Boolean;
var
  stSql :string;
begin
  stSql := 'Update TB_CARD ';
  stSql := stSql + ' Set POSITIONNUM = ' + inttostr(aPositionNumber) ;
  stSql := stSql + ' Where GROUP_CODE = ''' + aGroupCode + ''' ';
  stSql := stSql + ' AND CA_CARDNO = ''' + aCardNo + ''' ';

  result := DataModule1.ProcessExecSQL(stSql);

end;

end.
