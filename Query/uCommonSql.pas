unit uCommonSql;

interface

uses
  SysUtils, Classes,uDataModule1;

type
  TCommonSql = class(TDataModule)
  private
    { Private declarations }
  public
    { Public declarations }

    //조회
    Function GetNodeTypeSelect(aNetType:string):string;
    Function GetECUAllSelect(aNetType:string):string;
    Function GetVisibleGradeSelect:string;
    Function GetDaemonPasswordSelect(aPasswd:string):string;

    Function GetTB_ACCESSDEVICEAllSelect:string;
    Function GetTB_ACCESSEVENTToDateSelect(aDate:string):string;
    Function GetTB_ALARMDEVICEAllSelect:string;
    Function GetTB_ATCARDLodingTypeSelect(aLoadType:string):string;
    Function GetTB_ATCODEAllSelect:string;
    Function GetTB_ATCODEHolidayFromToTimeSelect(aTime:string):string;
    Function GetTB_ATCODESaturdayFromToTimeSelect(aTime:string):string;
    Function GetTB_ATCODEWeekFromToTimeSelect(aTime:string):string;
    Function GetTB_ATEVENTToDateSelect(aDate:string):string;
    Function GetTB_ATPASSLodingTypeSelect(aLoadType:string):string;
    Function GetTB_CardAllSelect:string;
    Function GetTB_CARDequalCardNoSelect(aCardNo,aGubun,aCardType:string):string;
    Function GetTB_COMPANYAllSelect:string;
    Function GetTB_CONFIGGroupSelect(aGroup:string):string;
    Function GetTB_CURRENTDAEMONStateSelect(aState:string):string;
    Function GetTB_DEVICECARDNOAllSelect:string;
    Function GetTB_DOORAllSelect:string;
    Function GetTB_EMPLOYEEAllSelect:string;
    Function GetTB_EMPLOYEEequalEmPassSelect(aPassNo:string):string;
    Function GetTB_FOODAllSelect:string;
    Function GetTB_HOLIDAYequalDate(aDate:string):string;
    Function GetTB_POSIAllSelect:string;
    Function GetMaxCompanyCode:string;
    Function GetMaxJijumCode(aCompanyCode:string):string;
    Function GetMaxDepartCode(aCompanyCode,aJijumCode:string):string;
    Function GetMaxPosiCode(aCompanyCode:string):string;
    Function GetMaxATCode:string;
    Function GetMaxGradeCode:string;
    Function GetMaxATInCode : string;
    Function GetMaxATOutCode : string;
    Function GetMaxATVaCode : string;
    Function GetMaxAlarmCheckCode:string;

    //입력

    Function InsertIntoTB_ACCEVENT(aDate,aTime,aNodeNo,aEcuID,aDoorNo,aCardNo,aReaderNo,aButton,aDoorPosi,
             aInputType,aDoorMode,aPermitMode,aPermitCode,aInsertDate,aCompanyCode,
             aEmCode,aJijumCode,aDepartCode,aEmName:string):string;
    function InsertIntoTB_ADMINALARMAREA(aUserID,aNodeNo,aEcuID,aArmAreaNo,aGubun,aUpdateTime,aOperater:string):string;
    Function InsertIntoTB_ANTIGROUPCODE_All(aCode,aName,aAntiNo: string):string;
    Function InsertIntoTB_ALARMCODEGROUP(aAG_CODE,aAL_ALARMSTATUSCODE:string):string;
    Function InsertIntoTB_ALARMMODENOTCARD(aAN_CODE,aAN_NOTDATA:string):string;
    Function InsertTB_ALARMGUBUNCODE(aCode,aName:string):string;
    Function InsertIntoTB_ALARMSTATUSCODE(aAL_ALARMSTATUSCODE,aAL_ALARMNAME,aAL_ALARMVIEW,aAL_ALARMGRADE,aAL_ALARMSOUND:string):string;
    function InsertIntoTB_ARMAREA(aNodeNo,aEcuID,aArmAreaNo,aArmAreaName,aArmAreaUse,
                             aLastMode,aVIEWSEQ,aLOCATEUSE,aTOTWIDTH,aTOTHEIGHT,
                             aCURX,aCURY,aDONGCODE,aFLOORCODE,aAREACODE:string):string;
    Function InsertIntoAT_INCODE(aAtInCode,aAtInName,aAtInType,aAtSystem:string):string;
    Function InsertIntoAT_OUTCODE(aAtOutCode,aAtOutName,aAtOutType,aAtSystem:string):string;
    Function InsertIntoTB_COMPANY(aCompanyCode,aJijumCode,aDepartCode,aName,aGubun:string):string;
    Function InsertIntoTB_CONFIG(aCONFIGGROUP,aCONFIGCODE,aCONFIGVALUE:string;aDetail:string=''):string;
    Function InsertTB_DOORGUBUN(aCode,aName:string):string;
    Function InsertIntoTB_EMPLOYEECHANGE(aCompanyCode,aEmCode,aCardNo,aFormName,aWorkType:string):string;
    Function InsertIntoTB_EMPHIS(aCompanyCode,aEmCode,afdmsID,aMode,aCardNo,aCardType,aEmName,aHandPhone,aCompanyName,aJijumName,aDepartName,aPosiName:string):string;
    Function InsertIntoTB_FOODCODECount(aDate,aFoodCode,aCompanyCode,aEmCode,aCount:string):string;
    Function InsertIntoTB_FoodDayCount(aDate,aCompanyCode,aEmCode,aCount:string):string;
    Function InsertIntoTB_FoodSemesterCount(aYear,aSemesterCode,aCompanyCode,aEmCode,aCount:string):string;
    Function InsertIntoTB_FoodWeekCount(aYear,aWeekDay,aCompanyCode,aEmCode,aCount:string):string;
    Function InsertIntoTB_FORMNAME(aFmCode,aFmName,aFmUse:string):string;
    Function InsertTB_KTTMAPPINGCODE(aDeviceType,aAlarmMode,aStatusCode,aFuncode,aEventCode:string):string;
    Function InsertIntoTB_NOTCARDALARMCODE(aAN_CODE,aAN_MODE,aAL_STATUSCODE2:string):string;
    Function InsertIntoTB_Posi(aCompanyCode,aPosiCode,aPosiName:string):string;
    Function InsertIntoTB_PROGRAMID(aProgramID,aGroupCode,aProgramName,aVisible,aViewSeq,aConfigCode,aEnable:string):string;
    Function InsertIntoTB_PROGRAMGROUP(aGroupCode,aGroupName,aVisible,aGubun:string):string;
    Function InsertIntoTB_RELAYGUBUN(aCode,aName:string):string;
    Function InsertIntoTB_SEMESTER(aCode,aUse,aStartDate,aEndDate:string):string;
    Function InsertIntoTB_SONGHOFDCONFIG(aCode,aUse,aLimitCount:string):string;
    Function InsertIntoTB_TIMECODE_Value(aGroup,aTime1,aTime2,aTime3,aTime4,aChange:string):string;
    Function InsertIntoTB_TIMECODEDEVICE_Value(aNodeNo,aEcuID,aGroup,aTime1,aTime2,aTime3,aTime4,aSend,aChange:string):string;
    Function InsertIntoTB_WORKGUBUN_Value(aWorkCode,aWorkType,aWorkName:string):string;

    //수정
    Function updateTB_ACCESSDEVICEDaemonGubunAll(aDaemonGubun:string):string;
    Function UpdateTB_ACCESSDEVICEConnected(aNodeNo, aECUID,aConnected: string):string;
    Function UpdateTB_ANTIGROUPCODE_Field_StringValue(aCode,aFieldName,aData:string):string;
    Function UpdateTB_ATCARDAllLoadType(aLoadType:string):string;
    Function UpdateTB_ATCARDLoadTypeFromTarget(aTarget,aLoadType:string):string;
    Function UpdateTB_ATPASSAllLoadType(aLoadType:string):string;
    Function UpdateTB_ATPASSLoadTypeFromTarget(aTarget,aLoadType:string):string;
    Function UpdateTB_CONFIG(aCONFIGGROUP,aCONFIGCODE,aCONFIGVALUE:string):string;
    Function UpdateTB_CURRENTDAEMONKey(aKey,aValue:string):string;
    Function UpdateTB_DEVICECARDNO_RCVACK(OrgState,NewState:String):String;
    Function UpdateTB_DOOR_Field_IntValue(aNodeNo,aEcuID,aDoorNo,aFieldName,aData:string):string;
    Function UpdateTB_DOORfdmsRelay(aOldRelay,aNewRelay:string):string;
    Function UpdateTB_DoorfdmsSendAck(aNODENO,aECUID,aDOORNO,aFdmsRelay:string):String;
    Function UpdateTB_EMPLOYEEfdmsSendAck(aCompanyCode,aEmCode,aFdmsRelay:string):String;
    Function UpdateTB_EMPHISSendAck1(aSeq,aStatus:string):String;
    Function UpdateTB_EMPHISSendAck2(aSeq,aStatus:string):String;
    Function UpdateTB_EMPLOYEEfdmsRelay(aOldRelay,aNewRelay:string):string;
    Function UpdateTB_EMPHISState1(aOldStatus,aNewStatus:string):string;
    Function UpdateTB_EMPHISState2(aOldStatus,aNewStatus:string):string;
    Function UpdateTB_FOODCODECount(aDate,aFoodCode,aCompanyCode,aEmCode,aCount:string):string;
    Function UpdateTB_FOODCODECount_Dec(aDate,aFoodCode,aCompanyCode,aEmCode:string):string;
    Function UpdateTB_FOODCODECount_Inc(aDate,aFoodCode,aCompanyCode,aEmCode:string):string;
    Function UpdateTB_FoodDayCount(aDate,aCompanyCode,aEmCode,aCount:string):string;
    Function UpdateTB_FoodDayCount_Dec(aDate,aCompanyCode,aEmCode:string):string;
    Function UpdateTB_FoodDayCount_Inc(aDate,aCompanyCode,aEmCode:string):string;
    Function UpdateTB_FoodSemesterCount(aYear,aSemesterCode,aCompanyCode,aEmCode,aCount:string):string;
    Function UpdateTB_FoodSemesterCount_Dec(aYear,aSemesterCode,aCompanyCode,aEmCode:string):string;
    Function UpdateTB_FoodSemesterCount_Inc(aYear,aSemesterCode,aCompanyCode,aEmCode:string):string;
    Function UpdateTB_FoodWeekCount(aYear,aWeekDay,aCompanyCode,aEmCode,aCount:string):string;
    Function UpdateTB_FoodWeekCount_Dec(aYear,aWeekDay,aCompanyCode,aEmCode:string):string;
    Function UpdateTB_FoodWeekCount_Inc(aYear,aWeekDay,aCompanyCode,aEmCode:string):string;
    Function UpdateTB_PROGRAMIDVisible(aCode, aValue: string):string;
    Function UpdateTB_SEMESTER_Field_StringValue(aCode, aFieldName,aData:string):string;
    Function UpdateTB_SONGHOFDCONFIG_Field_IntValue(aCode, aFieldName,aData:string):string;
    Function UpdateTB_SONGHOFDCONFIG_Field_StringValue(aCode, aFieldName,aData:string):string;
    Function UpdateTB_TIMECODE_Value(aGroup,aTime1,aTime2,aTime3,aTime4,aChange:string):string;
    Function UpdateTB_TIMECODEDEVICE_Value(aNodeNo,aEcuID,aGroup,aTime1,aTime2,aTime3,aTime4,aSend,aChange:string):string;

    //삭제
    Function DeleteTB_ACCESSEVENTToDate(aDate:string):string;
    Function DeleteTB_ANTIGROUPCODE_Value(aCode:string):string;
    Function DeleteTB_ATEVENTToDate(aDate:string):string;
    Function DeleteTB_LOWDATAToDate(aDate:string):string;
    Function DeleteTB_FIREGROUP_Value(aGroupCode:string):string;

  end;

var
  CommonSql: TCommonSql;

implementation
uses
  uLomosUtil;
  
{$R *.dfm}

{ TCommonSql }

function TCommonSql.DeleteTB_ACCESSEVENTToDate(aDate: string): string;
var
  stSql : string;
begin
  stSql := ' Delete From TB_ACCESSEVENT ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AC_DATE <= ''' + aDate + ''' ';
  result := stSql;
end;

function TCommonSql.DeleteTB_ATEVENTToDate(aDate: string): string;
var
  stSql : string;
begin
  stSql := ' Delete From TB_ATEVENT ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AT_DATE <= ''' + aDate + ''' ';
  result := stSql;
end;

function TCommonSql.DeleteTB_LOWDATAToDate(aDate: string): string;
var
  stSql : string;
begin
  stSql := 'Delete From TB_LOWDATA ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + '''';
  stSql := stSql + ' AND LO_DATE <= ''' + aDate + '''';
  result := stSql;
end;

function TCommonSql.GetDaemonPasswordSelect(aPasswd: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_CURRENTDAEMON ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  stSql := stSql + ' and CU_STATECODE = ''Password''';
  stSql := stSql + ' and CU_STATEVALUE = ''' + aPasswd + '''';
  result := stSql;
end;

function TCommonSql.GetECUAllSelect(aNetType: string): string;
var
  stSql : string;
begin
  stSql := ' select * from TB_ACCESSDEVICE ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  stSql := stSql + ' AND AC_NETTYPE = ''' + aNetType + ''' ';
  stSql := stSql + ' order by AC_NODENO ';
  result := stSql;
end;

function TCommonSql.GetMaxAlarmCheckCode: string;
var
  stSql:string;
begin
  stSql := ' select Max(AL_ALARMCHECKCODE) as AL_ALARMCHECKCODE from TB_ALARMCHECKCODE ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  result := stSql;

end;

function TCommonSql.GetMaxATCode: string;
var
  stSql:string;
begin
  stSql := ' select Max(AT_ATCODE) as AT_ATCODE from TB_ATCODE ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  result := stSql;
end;

function TCommonSql.GetMaxATInCode: string;
var
  stSql:string;
begin
  stSql := ' select Max(AT_INCODE) as AT_INCODE from TB_ATINCODE ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  result := stSql;

end;

function TCommonSql.GetMaxATOutCode: string;
var
  stSql:string;
begin
  stSql := ' select Max(AT_OUTCODE) as AT_OUTCODE from TB_ATOUTCODE ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  result := stSql;

end;

function TCommonSql.GetMaxATVaCode: string;
var
  stSql:string;
begin
  stSql := ' select Max(AT_VACODE) as AT_VACODE from TB_ATVACODE ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  result := stSql;

end;

function TCommonSql.GetMaxCompanyCode: string;
var
  stSql:string;
begin
  stSql := ' select Max(CO_COMPANYCODE) as CO_COMPANYCODE from TB_COMPANY ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  stSql := stSql + ' AND CO_GUBUN = ''1'' ';
  result := stSql;
end;

function TCommonSql.GetMaxDepartCode(aCompanyCode,
  aJijumCode: string): string;
var
  stSql:string;
begin
  stSql := ' select Max(CO_DEPARTCODE) as CO_DEPARTCODE from TB_COMPANY ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND CO_JIJUMCODE = ''' + aJijumCode + ''' ';
  stSql := stSql + ' AND CO_GUBUN = ''3'' ';
  result := stSql;
end;

function TCommonSql.GetMaxGradeCode: string;
var
  stSql:string;
begin
  stSql := ' select Max(GR_GRADECODE) as GR_GRADECODE from TB_GRADE ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  result := stSql;
end;

function TCommonSql.GetMaxJijumCode(aCompanyCode: string): string;
var
  stSql:string;
begin
  stSql := ' select Max(CO_JIJUMCODE) as CO_JIJUMCODE from TB_COMPANY ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND CO_GUBUN = ''2'' ';
  result := stSql;
end;

function TCommonSql.GetMaxPosiCode(aCompanyCode: string): string;
var
  stSql:string;
begin
  stSql := ' select Max(PO_POSICODE) as PO_POSICODE from TB_POSI ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  result := stSql;
end;

function TCommonSql.GetNodeTypeSelect(aNetType: string): string;
var
  stSql:string;
begin
  stSql := ' select * from TB_ACCESSDEVICE ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + '''';
  stSql := stSql + ' AND AC_GUBUN = ''0'' ';
  stSql := stSql + ' AND AC_NETTYPE = ''' + aNetType + ''' ';
  result := stSql;
end;


function TCommonSql.GetTB_ACCESSDEVICEAllSelect: string;
var
  stSql : string;
begin
  stSql := 'select * from TB_ACCESSDEVICE ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_ACCESSEVENTToDateSelect(aDate: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_ACCESSEVENT ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AC_DATE <= ''' + aDate + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_ALARMDEVICEAllSelect: string;
var
  stSql : string;
begin
  stSql := 'select * from TB_ALARMDEVICE ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_ATCARDLodingTypeSelect(
  aLoadType: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_ATCARD ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AT_LOAD = ''' + aLoadType + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_ATCODEAllSelect: string;
var
  stSql : string;
begin
  stSql := 'select * from TB_ATCODE ';
  stSql := stSql + 'where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_ATCODEHolidayFromToTimeSelect(
  aTime: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_ATCODE ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AT_HINFROMTIME <= ''' + aTime + ''' ';
  stSql := stSql + ' AND AT_HINTOTIME >= ''' + aTime + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_ATCODESaturdayFromToTimeSelect(
  aTime: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_ATCODE ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AT_SINFROMTIME <= ''' + aTime + ''' ';
  stSql := stSql + ' AND AT_SINTOTIME >= ''' + aTime + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_ATCODEWeekFromToTimeSelect(
  aTime: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_ATCODE ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AT_INFROMTIME <= ''' + aTime + ''' ';
  stSql := stSql + ' AND AT_INTOTIME >= ''' + aTime + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_ATEVENTToDateSelect(aDate: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_ATEVENT ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AT_DATE <= ''' + aDate + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_ATPASSLodingTypeSelect(
  aLoadType: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_ATPASS ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AT_LOAD = ''' + aLoadType + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_CardAllSelect: string;
var
  stSql : string;
begin
  stSql := 'select * from TB_CARD ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_CARDequalCardNoSelect(aCardNo, aGubun,
  aCardType: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_CARD ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND CA_CARDNO = ''' + aCardNo + ''' ';
  stSql := stSql + ' AND CA_GUBUN = '''+ aGubun +''' ';  //HEX TYPE
  stSql := stSql + ' AND CA_CARDTYPE = '''+ aCardType + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_COMPANYAllSelect: string;
var
  stSql:string;
begin
  stSql := 'select * from TB_COMPANY ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_CONFIGGroupSelect(aGroup: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_CONFIG ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND CO_CONFIGGROUP = '''+ aGroup + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_CURRENTDAEMONStateSelect(aState: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_CURRENTDAEMON ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND CU_STATECODE = '''+ aState + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_DEVICECARDNOAllSelect: string;
var
  stSql : string;
begin
  stSql := 'select * from TB_DEVICECARDNO ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_DOORAllSelect: string;
var
  stSql : string;
begin
  stSql := 'select * from TB_DOOR ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_EMPLOYEEAllSelect: string;
var
  stSql : string;
begin
  stSql := 'select * from TB_EMPLOYEE ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result:= stSql;
end;

function TCommonSql.GetTB_EMPLOYEEequalEmPassSelect(
  aPassNo: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_EMPLOYEE ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND EM_PASS = ''' + aPassNo + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_FOODAllSelect: string;
var
  stSql : string;
begin
  stSql := 'select * from TB_FOOD ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_HOLIDAYequalDate(aDate: string): string;
var
  stSql : string;
begin
  stSql := 'select * from TB_HOLIDAY ';
  stSql := stSql + ' where HO_DAY = ''' + aDate + ''' ';
  result := stSql;
end;

function TCommonSql.GetTB_POSIAllSelect: string;
var
  stSql : string;
begin
  stSql := 'select * from TB_POSI ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result := stSql;
end;

function TCommonSql.GetVisibleGradeSelect: string;
var
  stSql : string;
begin
  stSql := ' select a.CO_CONFIGCODE,b.CO_CONFIGVALUE ';
  stSql := stSql + ' FROM TB_PROGRAMID a  ';
  stSql := stSql + ' INNER JOIN TB_CONFIG b ';
  stSql := stSql + ' ON (a.CO_CONFIGCODE = b.CO_CONFIGCODE';
  stSql := stSql + ' AND a.GROUP_CODE = b.GROUP_CODE ';
  stSql := stSql + ' AND b.CO_CONFIGGROUP = ''MOSTYPE'' )';
  stSql := stSql + ' where a.GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' group by a.CO_CONFIGCODE,b.CO_CONFIGVALUE ';
  result := stSql;
end;

function TCommonSql.InsertIntoAT_INCODE(aAtInCode, aAtInName, aAtInType,
  aAtSystem: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_ATINCODE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AT_INCODE,';
  stSql := stSql + ' AT_INNAME,';
  stSql := stSql + ' AT_INTYPE,';
  stSql := stSql + ' AT_SYSTEM) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aAtInCode + ''', ';
  stSql := stSql + '''' + aAtInName + ''', ';
  stSql := stSql + '''' + aAtInType + ''', ';
  stSql := stSql + '''' + aAtSystem + ''') ';

  result :=  stSql;
end;

function TCommonSql.InsertIntoAT_OUTCODE(aAtOutCode, aAtOutName,
  aAtOutType, aAtSystem: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_ATOUTCODE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AT_OUTCODE,';
  stSql := stSql + ' AT_OUTNAME,';
  stSql := stSql + ' AT_OUTTYPE,';
  stSql := stSql + ' AT_SYSTEM) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aAtOutCode + ''', ';
  stSql := stSql + '''' + aAtOutName + ''', ';
  stSql := stSql + '''' + aAtOutType + ''', ';
  stSql := stSql + '''' + aAtSystem + ''') ';

  result :=  stSql;
end;

function TCommonSql.InsertIntoTB_CONFIG(aCONFIGGROUP, aCONFIGCODE,
  aCONFIGVALUE: string;aDetail:string=''): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_CONFIG (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' CO_CONFIGGROUP,';
  stSql := stSql + ' CO_CONFIGCODE,';
  stSql := stSql + ' CO_CONFIGVALUE,';
  stSql := stSql + ' CO_CONFIGDETAIL) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aCONFIGGROUP + ''', ';
  stSql := stSql + '''' + aCONFIGCODE + ''', ';
  stSql := stSql + '''' + aCONFIGVALUE + ''',';
  stSql := stSql + '''' + aDetail + ''') ';

  result :=  stSql;

end;

function TCommonSql.InsertIntoTB_EMPHIS(aCompanyCode, aEmCode,afdmsID,
  aMode,aCardNo,aCardType,aEmName,aHandPhone,aCompanyName,aJijumName,aDepartName,aPosiName: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_EMPHIS(';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' CO_COMPANYCODE,';
  stSql := stSql + ' EM_CODE,';
  if afdmsID <> '' then
    stSql := stSql + ' FDMS_ID,';
  stSql := stSql + ' MODE, ';
  stSql := stSql + ' SEND_STATUS, ';
  stSql := stSql + ' CA_CARDNO, ';
  stSql := stSql + ' CA_CARDTYPE, ';
  stSql := stSql + ' SEND_STATUS2,';
  stSql := stSql + ' EH_INSERTTIME, ';
  stSql := stSql + ' EM_NAME, ';
  stSql := stSql + ' EM_HANDPHONE, ';
  stSql := stSql + ' COMPANY_NAME, ';
  stSql := stSql + ' JIJUM_NAME, ';
  stSql := stSql + ' DEPART_NAME, ';
  stSql := stSql + ' PO_NAME) ';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''' + GROUPCODE + ''',';
  stSql := stSql + '''' + aCompanyCode + ''',';
  stSql := stSql + '''' + aEmCode + ''',';
  if afdmsID <> '' then
    stSql := stSql + afdmsID + ',';
  stSql := stSql + '''' + aMode + ''',';
  stSql := stSql + '''N'',';
  stSql := stSql + '''' + aCardNo + ''',';
  stSql := stSql + '''' + aCardType + ''',';
  stSql := stSql + '''N'',';
  stSql := stSql + '''' + FormatDateTime('yyyymmddhhnnss',Now) + ''',';
  stSql := stSql + '''' + aEmName + ''',';
  stSql := stSql + '''' + aHandPhone + ''',';
  stSql := stSql + '''' + aCompanyName + ''',';
  stSql := stSql + '''' + aJijumName + ''',';
  stSql := stSql + '''' + aDepartName + ''',';
  stSql := stSql + '''' + aPosiName + ''') ';

  result := stSql;
end;

function TCommonSql.InsertIntoTB_FORMNAME(aFmCode, aFmName,
  aFmUse: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_FORMNAME(';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' FM_CODE,';
  stSql := stSql + ' FM_NAME,';
  stSql := stSql + ' FM_USE) ';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''' + GROUPCODE + ''',';
  stSql := stSql + '''' + aFmCode + ''',';
  stSql := stSql + '''' + aFmName + ''',';
  stSql := stSql + '''' + aFmUse + ''')';

  result := stSql;
end;

function TCommonSql.InsertIntoTB_Posi(aCompanyCode, aPosiCode,
  aPosiName: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_POSI(';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' CO_COMPANYCODE,';
  stSql := stSql + ' PO_POSICODE,';
  stSql := stSql + ' PO_NAME) ';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''' + GROUPCODE + ''',';
  stSql := stSql + '''' + aCompanyCode + ''',';
  stSql := stSql + '''' + aPosiCode + ''',';
  stSql := stSql + '''' + aPosiName + ''')';

  result := stSql;
end;

function TCommonSql.InsertIntoTB_PROGRAMID(aProgramID, aGroupCode,
  aProgramName, aVisible, aViewSeq, aConfigCode, aEnable: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_PROGRAMID (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' PR_PROGRAMID,';
  stSql := stSql + ' PR_GROUPCODE,';
  stSql := stSql + ' PR_PROGRAMNAME,';
  stSql := stSql + ' PR_VISIBLE,';
  stSql := stSql + ' PR_VIEWSEQ,';
  stSql := stSql + ' CO_CONFIGCODE,';
  stSql := stSql + ' PR_ENABLE) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aProgramID + ''', ';
  stSql := stSql + '''' + aGroupCode + ''', ';
  stSql := stSql + '''' + aProgramName + ''', ';
  stSql := stSql + '''' + aVisible + ''', ';
  stSql := stSql + aViewSeq + ', ';
  stSql := stSql + '''' + aConfigCode + ''', ';
  stSql := stSql + '''' + aEnable + ''') ';

  result :=  stSql;
end;

function TCommonSql.UpdateTB_ACCESSDEVICEConnected(aNodeNo, aECUID,
  aConnected: string): string;
var
  stSql : string;
begin
  stSql := ' Update TB_ACCESSDEVICE set ';
  stSql := stSql + ' AC_CONNECTED = ''' + aConnected + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AC_NODENO = ' + inttostr(strtoint(aNodeNo));
  stSql := stSql + ' AND AC_ECUID = ''' + aECUID + ''' ';
  result := stSql;
end;

function TCommonSql.UpdateTB_ATCARDAllLoadType(aLoadType: string): string;
var
  stSql : string;
begin
  stSql := 'update TB_ATCARD set AT_LOAD =''' + aLoadType + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result := stSql;
end;

function TCommonSql.UpdateTB_ATCARDLoadTypeFromTarget(aTarget,
  aLoadType: string): string;
var
  stSql : string;
begin
  stSql := 'update TB_ATCARD set AT_LOAD =''' + aLoadType + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AT_LOAD = ''' +  aTarget + ''' ' ;
  result := stSql;

end;

function TCommonSql.UpdateTB_ATPASSAllLoadType(aLoadType: string): string;
var
  stSql : string;
begin
  stSql := 'update TB_ATPASS set AT_LOAD =''' + aLoadType + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  result := stSql;

end;

function TCommonSql.UpdateTB_ATPASSLoadTypeFromTarget(aTarget,
  aLoadType: string): string;
var
  stSql : string;
begin
  stSql := 'update TB_ATPASS set AT_LOAD =''' + aLoadType + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AT_LOAD = ''' +  aTarget + ''' ';
  result := stSql;
end;

function TCommonSql.UpdateTB_CONFIG(aCONFIGGROUP, aCONFIGCODE,
  aCONFIGVALUE: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_CONFIG ';
  stSql := stSql + ' Set CO_CONFIGVALUE = ''' + aCONFIGVALUE + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND CO_CONFIGGROUP = ''' + aCONFIGGROUP + ''' ';
  stSql := stSql + ' AND CO_CONFIGCODE = ''' + aCONFIGCODE + ''' ';

  result :=  stSql;
end;

function TCommonSql.UpdateTB_CURRENTDAEMONKey(aKey,
  aValue: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_CURRENTDAEMON Set CU_STATEVALUE = ''' + aValue + ''' ';
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND CU_STATECODE = ''' + aKey + ''' ';
  result := stSql;
end;

function TCommonSql.UpdateTB_DEVICECARDNO_RCVACK(OrgState,
  NewState: String): String;
var
  stSql : string;
begin
  stSql := 'Update TB_DEVICECARDNO Set DE_RCVACK = ''' + NewState + ''' ';     //송신중으로 변경
  stSql := stSql + ' Where DE_RCVACK = ''' + OrgState + ''' ';
  stSql := stSql + ' AND GROUP_CODE = ''' + GROUPCODE + ''' ' ;

  Result := stSql;

end;

function TCommonSql.UpdateTB_DOORfdmsRelay(aOldRelay,
  aNewRelay: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_DOOR set FDMS_RELAY = ''' + aNewRelay + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FDMS_RELAY = ''' + aOldRelay + ''' ';
  result := stSql;

end;

function TCommonSql.UpdateTB_DoorfdmsSendAck(aNODENO, aECUID, aDOORNO,
  aFdmsRelay: string): String;
var
  stSql : string;
begin
  stSql := 'Update TB_DOOR set FDMS_RELAY = ''' + aFdmsRelay + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AC_NODENO = ' + inttostr(strtoint(aNODENO)) ;
  stSql := stSql + ' AND AC_ECUID  = ''' + aECUID + ''' ';
  stSql := stSql + ' AND DO_DOORNO  = ''' + aDOORNO + ''' ';
  result := stSql;
end;

function TCommonSql.UpdateTB_EMPHISState1(aOldStatus, aNewStatus: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_EMPHIS set SEND_STATUS = ''' + aNewStatus + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND SEND_STATUS = ''' + aOldStatus + ''' ';
  result := stSql;

end;

function TCommonSql.UpdateTB_EMPHISSendAck1(aSeq,
  aStatus: string): String;
var
  stSql : string;
begin
  stSql := 'Update TB_EMPHIS set SEND_STATUS = ''' + aStatus + ''' ';
  stSql := stSql + ' Where SEQ = ' + aSeq ;
  result := stSql;

end;

function TCommonSql.UpdateTB_EMPLOYEEfdmsRelay(aOldRelay,
  aNewRelay: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_EMPLOYEE set FDMS_RELAY = ''' + aNewRelay + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FDMS_RELAY = ''' + aOldRelay + ''' ';
  result := stSql;

end;

function TCommonSql.UpdateTB_EMPLOYEEfdmsSendAck(aCompanyCode, aEmCode,
  aFdmsRelay: string): String;
var
  stSql : string;
begin
  stSql := 'Update TB_EMPLOYEE set FDMS_RELAY = ''' + aFdmsRelay + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE  = ''' + aEmCode + ''' ';
  result := stSql;

end;

function TCommonSql.UpdateTB_PROGRAMIDVisible(aCode,
  aValue: string): string;
var
  stSql : string;
begin
  stSql := 'update TB_PROGRAMID  Set PR_VISIBLE = ''' + aValue + ''' ' ;
  stSql := stSql + ' where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND PR_VISIBLE <> ''D'' ';
  stSql := stSql + ' AND CO_CONFIGCODE = ''' + aCode + ''' ';
  result := stSql;
end;

function TCommonSql.UpdateTB_EMPHISSendAck2(aSeq, aStatus: string): String;
var
  stSql : string;
begin
  stSql := 'Update TB_EMPHIS set SEND_STATUS2 = ''' + aStatus + ''' ';
  stSql := stSql + ' Where SEQ = ' + aSeq ;
  result := stSql;
end;

function TCommonSql.UpdateTB_EMPHISState2(aOldStatus,
  aNewStatus: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_EMPHIS set SEND_STATUS2 = ''' + aNewStatus + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND SEND_STATUS2 = ''' + aOldStatus + ''' ';
  result := stSql;
end;

function TCommonSql.InsertIntoTB_RELAYGUBUN(aCode, aName: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_RELAYGUBUN (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' RG_CODE,';
  stSql := stSql + ' RG_NAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aCode + ''', ';
  stSql := stSql + '''' + aName + ''') ';

  result :=  stSql;
end;

function TCommonSql.updateTB_ACCESSDEVICEDaemonGubunAll(
  aDaemonGubun:string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_ACCESSDEVICE set AC_DAEMONGUBUN = ' + aDaemonGubun + ' ';
  result := stSql;

end;

function TCommonSql.InsertIntoTB_PROGRAMGROUP(aGroupCode, aGroupName,
  aVisible, aGubun: string): string;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_PROGRAMGROUP( ';
  stSql := stSql + ' GROUP_CODE, ';
  stSql := stSql + ' PR_GROUPCODE, ';
  stSql := stSql + ' PR_GROUPCODENAME, ';
  stSql := stSql + ' PR_VISIBLE, ';
  stSql := stSql + ' GUBUN ) ';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''' + GROUPCODE + ''',';
  stSql := stSql + '''' + aGroupCode + ''',';
  stSql := stSql + '''' + aGroupName + ''',';
  stSql := stSql + '''' + aVisible + ''',';
  stSql := stSql + '''' + aGubun + ''' )';

  result := stSql;
end;

function TCommonSql.InsertTB_DOORGUBUN(aCode, aName: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_DOORGUBUN (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' DG_CODE,';
  stSql := stSql + ' DG_NAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aCode + ''', ';
  stSql := stSql + '''' + aName + ''') ';

  result :=  stSql;
end;

function TCommonSql.InsertIntoTB_ALARMSTATUSCODE(aAL_ALARMSTATUSCODE,
  aAL_ALARMNAME, aAL_ALARMVIEW, aAL_ALARMGRADE,
  aAL_ALARMSOUND: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_ALARMSTATUSCODE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AL_ALARMSTATUSCODE,';
  stSql := stSql + ' AL_ALARMNAME,';
  stSql := stSql + ' AL_ALARMVIEW,';
  stSql := stSql + ' AL_ALARMGRADE,';
  stSql := stSql + ' AL_ALARMSOUND) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aAL_ALARMSTATUSCODE + ''', ';
  stSql := stSql + '''' + aAL_ALARMNAME + ''', ';
  stSql := stSql + '' + aAL_ALARMVIEW + ', ';
  stSql := stSql + '' + aAL_ALARMGRADE + ', ';
  stSql := stSql + '' + aAL_ALARMSOUND + ') ';

  result :=  stSql;

end;

function TCommonSql.InsertIntoTB_ALARMMODENOTCARD(aAN_CODE,
  aAN_NOTDATA: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_ALARMMODENOTCARD (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AN_CODE,';
  stSql := stSql + ' AN_NOTDATA) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aAN_CODE + ''', ';
  stSql := stSql + '''' + aAN_NOTDATA + ''') ';

  result :=  stSql;
end;

function TCommonSql.InsertIntoTB_ALARMCODEGROUP(aAG_CODE,
  aAL_ALARMSTATUSCODE: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_ALARMCODEGROUP (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AG_CODE,';
  stSql := stSql + ' AL_ALARMSTATUSCODE) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aAG_CODE + ''', ';
  stSql := stSql + '''' + aAL_ALARMSTATUSCODE + ''') ';

  result :=  stSql;

end;

function TCommonSql.InsertIntoTB_NOTCARDALARMCODE(aAN_CODE, aAN_MODE,
  aAL_STATUSCODE2: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_NOTCARDALARMCODE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AN_CODE,';
  stSql := stSql + ' AN_MODE,';
  stSql := stSql + ' AL_STATUSCODE2) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aAN_CODE + ''', ';
  stSql := stSql + '''' + aAN_MODE + ''', ';
  stSql := stSql + '''' + aAL_STATUSCODE2 + ''') ';

  result :=  stSql;

end;

function TCommonSql.InsertIntoTB_ACCEVENT(aDate, aTime, aNodeNo, aEcuID,
  aDoorNo, aCardNo, aReaderNo, aButton, aDoorPosi, aInputType, aDoorMode,
  aPermitMode, aPermitCode, aInsertDate, aCompanyCode, aEmCode, aJijumCode,
  aDepartCode, aEmName: string): string;
var
  stSql : string;
begin
    stSql :=  'Insert into TB_ACCESSEVENT';
    stSql := stSql + '(GROUP_CODE,AC_DATE,AC_TIME,AC_NODENO,AC_ECUID,';
    stSql := stSql + 'DO_DOORNO,CA_CARDNO,AC_READERNO,AC_BUTTONNO,AC_DOORPOSI,AC_INPUTTYPE,';
    stSql := stSql + 'AC_DOORMODE,AC_PERMITMODE,PE_PERMITCODE,AC_INSERTDATE,CO_COMPANYCODE,';
    stSql := stSql + 'EM_CODE,CO_JIJUMCODE,CO_DEPARTCODE,EM_NAME)';
    stSql := stSql + ' Values(';
    stSql := stSql + '''' + GROUPCODE + ''',';
    stSql := stSql + '''' + aDate + ''',';
    stSql := stSql + '''' + aTime + ''',';
    stSql := stSql + aNodeNo + ',';
    stSql := stSql + '''' + aEcuID + ''',';
    stSql := stSql + '''' + aDoorNo + ''',';
    stSql := stSql + '''' + aCardNo + ''',';
    stSql := stSql + '''' + aReaderNo + ''',';
    stSql := stSql + '''' + aButton + ''',';
    stSql := stSql + '''' + aDoorPosi + ''',';
    stSql := stSql + '''' + aInputType + ''',';
    stSql := stSql + '''' + aDoorMode + ''',';    //운영/개방
    stSql := stSql + '''' + aPermitMode + ''',';  //Posi/Nega
    stSql := stSql + '''' + aPermitCode + ''',';
    stSql := stSql + '''' + aInsertDate + ''',';
    stSql := stSql + '''' + aCompanyCode + ''',';
    stSql := stSql + '''' + aEmCode + ''',';
    stSql := stSql + '''' + aJijumCode + ''',';
    stSql := stSql + '''' + aDepartCode + ''',';
    stSql := stSql + '''' + aEmName + ''')';

    result := stSql;
end;


function TCommonSql.InsertTB_ALARMGUBUNCODE(aCode, aName: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_ALARMGUBUNCODE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AG_ARMCODE,';
  stSql := stSql + ' AG_ARMNAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aCode + ''', ';
  stSql := stSql + '''' + aName + ''') ';

  result :=  stSql;

end;

function TCommonSql.InsertTB_KTTMAPPINGCODE(aDeviceType, aAlarmMode,
  aStatusCode, aFuncode, aEventCode: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_KTTMAPPINGCODE (';
  stSql := stSql + ' AL_ALARMDEVICETYPECODE,';
  stSql := stSql + ' AL_ALARMMODECODE,';
  stSql := stSql + ' AL_ALARMSTATUSCODE2,';
  stSql := stSql + ' KTTFUNCODE,';
  stSql := stSql + ' KTTEVENTCODE) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + aDeviceType + ''', ';
  stSql := stSql + '''' + aAlarmMode + ''', ';
  stSql := stSql + '''' + aStatusCode + ''', ';
  stSql := stSql + '''' + aFuncode + ''', ';
  stSql := stSql + '''' + aEventCode + ''') ';

  result :=  stSql;

end;

function TCommonSql.InsertIntoTB_TIMECODE_Value(aGroup, aTime1, aTime2,
  aTime3, aTime4,aChange: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_TIMECODE (';
  stSql := stSql + ' TC_GROUP,';
  stSql := stSql + ' TC_TIME1,';
  stSql := stSql + ' TC_TIME2,';
  stSql := stSql + ' TC_TIME3,';
  stSql := stSql + ' TC_TIME4, ';
  stSql := stSql + ' TC_CHANGE) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + aGroup + ''', ';
  stSql := stSql + '''' + aTime1 + ''', ';
  stSql := stSql + '''' + aTime2 + ''', ';
  stSql := stSql + '''' + aTime3 + ''', ';
  stSql := stSql + '''' + aTime4 + ''', ';
  stSql := stSql + '''' + aChange + ''') ';

  result :=  stSql;
end;

function TCommonSql.UpdateTB_TIMECODE_Value(aGroup, aTime1, aTime2, aTime3,
  aTime4,aChange: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_TIMECODE Set ';
  stSql := stSql + ' TC_TIME1 = ''' + aTime1 + ''',';
  stSql := stSql + ' TC_TIME2 = ''' + aTime2 + ''',';
  stSql := stSql + ' TC_TIME3 = ''' + aTime3 + ''',';
  stSql := stSql + ' TC_TIME4 = ''' + aTime4 + ''', ';
  stSql := stSql + ' TC_CHANGE = ''' + aChange + ''' ';
  stSql := stSql + ' Where TC_GROUP = ''' + aGroup + '''';

  result :=  stSql;
end;

function TCommonSql.UpdateTB_TIMECODEDEVICE_Value(aNodeNo, aEcuID, aGroup,
  aTime1, aTime2, aTime3, aTime4, aSend,aChange: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_TIMECODEDEVICE Set ';
  stSql := stSql + ' TC_TIME1 = ''' + aTime1 + ''',';
  stSql := stSql + ' TC_TIME2 = ''' + aTime2 + ''',';
  stSql := stSql + ' TC_TIME3 = ''' + aTime3 + ''',';
  stSql := stSql + ' TC_TIME4 = ''' + aTime4 + ''', ';
  stSql := stSql + ' TC_SEND = ''' + aSend + ''', ';
  stSql := stSql + ' TC_EACHCHANGE = ''' + aChange + ''' ';
  stSql := stSql + ' Where TC_GROUP = ''' + aGroup + '''';
  stSql := stSql + ' AND AC_NODENO = ' + aNodeNo + '';
  stSql := stSql + ' AND AC_ECUID = ''' + aEcuID + '''';

  result :=  stSql;
end;

function TCommonSql.InsertIntoTB_TIMECODEDEVICE_Value(aNodeNo, aEcuID,
  aGroup, aTime1, aTime2, aTime3, aTime4,aSend, aChange: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_TIMECODEDEVICE (';
  stSql := stSql + ' AC_NODENO,';
  stSql := stSql + ' AC_ECUID,';
  stSql := stSql + ' TC_GROUP,';
  stSql := stSql + ' TC_TIME1,';
  stSql := stSql + ' TC_TIME2,';
  stSql := stSql + ' TC_TIME3,';
  stSql := stSql + ' TC_TIME4, ';
  stSql := stSql + ' TC_SEND, ';
  stSql := stSql + ' TC_EACHCHANGE) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '' + aNodeNo + ', ';
  stSql := stSql + '''' + aEcuID + ''', ';
  stSql := stSql + '''' + aGroup + ''', ';
  stSql := stSql + '''' + aTime1 + ''', ';
  stSql := stSql + '''' + aTime2 + ''', ';
  stSql := stSql + '''' + aTime3 + ''', ';
  stSql := stSql + '''' + aTime4 + ''', ';
  stSql := stSql + '''' + aSend + ''', ';
  stSql := stSql + '''' + aChange + ''') ';

  result :=  stSql;
end;

function TCommonSql.InsertIntoTB_COMPANY(aCompanyCode, aJijumCode,
  aDepartCode, aName, aGubun: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_COMPANY (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' CO_COMPANYCODE,';
  stSql := stSql + ' CO_JIJUMCODE,';
  stSql := stSql + ' CO_DEPARTCODE,';
  stSql := stSql + ' CO_NAME,';
  stSql := stSql + ' CO_GUBUN) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aCompanyCode + ''', ';
  stSql := stSql + '''' + aJijumCode + ''', ';
  stSql := stSql + '''' + aDepartCode + ''', ';
  stSql := stSql + '''' + aName + ''', ';
  stSql := stSql + '''' + aGubun + ''') ';

  result :=  stSql;
end;

function TCommonSql.InsertIntoTB_ARMAREA(aNodeNo, aEcuID, aArmAreaNo,
  aArmAreaName, aArmAreaUse, aLastMode, aVIEWSEQ, aLOCATEUSE, aTOTWIDTH,
  aTOTHEIGHT, aCURX, aCURY, aDONGCODE, aFLOORCODE,
  aAREACODE: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_ARMAREA( ';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AC_NODENO,';
  stSql := stSql + ' AC_ECUID,';
  stSql := stSql + ' AR_AREANO,';
  stSql := stSql + ' AR_NAME,';
  stSql := stSql + ' AR_USE,';
  stSql := stSql + ' AR_LASTMODE,';
  stSql := stSql + ' AR_VIEWSEQ,';
  stSql := stSql + ' AR_LOCATEUSE,';
  stSql := stSql + ' AR_TOTWIDTH,';
  stSql := stSql + ' AR_TOTHEIGHT,';
  stSql := stSql + ' AR_CURX,';
  stSql := stSql + ' AR_CURY,';
  stSql := stSql + ' LO_DONGCODE,';
  stSql := stSql + ' LO_FLOORCODE,';
  stSql := stSql + ' LO_AREACODE) ';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''' + GROUPCODE  + ''',';
  stSql := stSql + '' + aNodeNo + ',';
  stSql := stSql + '''' + aEcuID + ''',';
  stSql := stSql + '''' + FillZeroStrNum(aArmAreaNo,2) + ''',';
  stSql := stSql + '''' + aArmAreaName + ''',';
  stSql := stSql + '''' + aArmAreaUse + ''',';
  stSql := stSql + '''' + aLastMode + ''',';
  stSql := stSql + '' + aVIEWSEQ + ',';
  stSql := stSql + '''' + aLOCATEUSE + ''',';
  stSql := stSql + '' + aTOTWIDTH + ',';
  stSql := stSql + '' + aTOTHEIGHT + ',';
  stSql := stSql + '' + aCURX + ',';
  stSql := stSql + '' + aCURY + ',';
  stSql := stSql + '''' + aDONGCODE + ''',';
  stSql := stSql + '''' + aFLOORCODE + ''',';
  stSql := stSql + '''' + aAREACODE + ''')';

  result :=  stSql;
end;

function TCommonSql.InsertIntoTB_ADMINALARMAREA(aUserID, aNodeNo, aEcuID,
  aArmAreaNo, aGubun, aUpdateTime, aOperater: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_ADMINALARMAREA( ';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AD_USERID,';
  stSql := stSql + ' AC_NODENO,';
  stSql := stSql + ' AC_ECUID,';
  stSql := stSql + ' AR_AREANO,';
  stSql := stSql + ' AD_GUBUN,';
  stSql := stSql + ' AD_UPDATETIME,';
  stSql := stSql + ' AD_UPDATEOPERATOR) ';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''' + GROUPCODE  + ''',';
  stSql := stSql + '''' + aUserID + ''',';
  stSql := stSql + '' + aNodeNo + ',';
  stSql := stSql + '''' + aEcuID + ''',';
  stSql := stSql + '''' + FillZeroStrNum(aArmAreaNo,2) + ''',';
  stSql := stSql + '''' + aGubun + ''',';
  stSql := stSql + '''' + aUpdateTime + ''',';
  stSql := stSql + '''' + aOperater + ''')';

  result :=  stSql;
end;

function TCommonSql.InsertIntoTB_ANTIGROUPCODE_All(aCode, aName,
  aAntiNo: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_ANTIGROUPCODE (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' AG_CODE,';
  stSql := stSql + ' AG_NAME,';
  stSql := stSql + ' AG_ANTINO) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '' + aCode + ', ';
  stSql := stSql + 'N''' + aName + ''', ';
  stSql := stSql + '' + aAntiNo + ') ';

  result := stSql;
end;

function TCommonSql.UpdateTB_ANTIGROUPCODE_Field_StringValue(aCode,
  aFieldName, aData: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_ANTIGROUPCODE set ' + aFieldName + ' = ''' + aData + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AG_CODE = ' + aCode + ' ';

  result := stSql;
end;

function TCommonSql.DeleteTB_FIREGROUP_Value(aGroupCode: string): string;
var
  stSql : string;
begin
  stSql := ' Delete From TB_FIREGROUP ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FG_GROUPCODE = ''' + aGroupCode + ''' ';

  result := stSql;
end;

function TCommonSql.DeleteTB_ANTIGROUPCODE_Value(aCode: string): string;
var
  stSql : string;
begin
  stSql := ' Delete From TB_ANTIGROUPCODE ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AG_CODE = ' + aCode + ' ';

  result := stSql;

end;

function TCommonSql.UpdateTB_DOOR_Field_IntValue(aNodeNo, aEcuID,
  aDoorNo, aFieldName, aData: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_DOOR set ' + aFieldName + ' = ' + aData + ' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND AC_NODENO = ' + aNodeNo + ' ';
  stSql := stSql + ' AND AC_ECUID = ''' + aEcuID + ''' ';
  stSql := stSql + ' AND DO_DOORNO = ''' + aDoorNo + ''' ';

  result := stSql;
end;

function TCommonSql.InsertIntoTB_SONGHOFDCONFIG(aCode, aUse,
  aLimitCount: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_SONGHOFDCONFIG (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' SF_CODE,';
  stSql := stSql + ' SF_USE,';
  stSql := stSql + ' SF_COUNT) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aCode + ''', ';
  stSql := stSql + '''' + aUse + ''', ';
  stSql := stSql + '' + aLimitCount + ') ';

  result :=  stSql;
end;

function TCommonSql.InsertIntoTB_SEMESTER(aCode, aUse, aStartDate,
  aEndDate: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_SEMESTER (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' SE_CODE,';
  stSql := stSql + ' SE_USE,';
  stSql := stSql + ' SE_STARTDATE,';
  stSql := stSql + ' SE_ENDDATE) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '''' + aCode + ''', ';
  stSql := stSql + '''' + aUse + ''', ';
  stSql := stSql + '''' + aStartDate + ''', ';
  stSql := stSql + '''' + aEndDate + ''') ';

  result :=  stSql;
end;

function TCommonSql.UpdateTB_SEMESTER_Field_StringValue(aCode, aFieldName,aData: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_SEMESTER set ' + aFieldName + ' = ''' + aData + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND SE_CODE = ' + aCode + ' ';

  result := stSql;
end;

function TCommonSql.UpdateTB_SONGHOFDCONFIG_Field_IntValue(aCode,
  aFieldName, aData: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_SONGHOFDCONFIG set ' + aFieldName + ' = ' + aData + ' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND SF_CODE = ''' + aCode + ''' ';

  result := stSql;
end;

function TCommonSql.UpdateTB_SONGHOFDCONFIG_Field_StringValue(aCode,
  aFieldName, aData: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_SONGHOFDCONFIG set ' + aFieldName + ' = ''' + aData + ''' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND SF_CODE = ''' + aCode + ''' ';

  result := stSql;
end;

function TCommonSql.UpdateTB_FOODCODECount(aDate, aFoodCode, aCompanyCode,
  aEmCode, aCount: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FOODCODECount set FD_COUNT = ' + aCount + ' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FC_DATE = ''' + aDate + ''' ';
  stSql := stSql + ' AND FO_FOODCODE = ''' + aFoodCode + ''' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';

  result := stSql;
end;

function TCommonSql.InsertIntoTB_FOODCODECount(aDate, aFoodCode,
  aCompanyCode, aEmCode, aCount: string): string;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_FOODCODECount( ';
  stSql := stSql + 'GROUP_CODE,';
  stSql := stSql + 'FC_DATE,';
  stSql := stSql + 'FO_FOODCODE,';
  stSql := stSql + 'CO_COMPANYCODE,';
  stSql := stSql + 'EM_CODE,';
  stSql := stSql + 'FD_COUNT) ';
  stSql := stSql + ' Values(';
  stSql := stSql + '''' + GROUPCODE + ''',';
  stSql := stSql + '''' + aDate + ''',';
  stSql := stSql + '''' + aFoodCode + ''',';
  stSql := stSql + '''' + aCompanyCode + ''',';
  stSql := stSql + '''' + aEmCode + ''',';
  stSql := stSql + '' + aCount + ')';

  result := stSql;
end;

function TCommonSql.UpdateTB_FoodDayCount(aDate, aCompanyCode, aEmCode,
  aCount: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FoodDayCount set FD_COUNT = ' + aCount + ' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FD_DATE = ''' + aDate + ''' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';

  result := stSql;
end;

function TCommonSql.InsertIntoTB_FoodDayCount(aDate, aCompanyCode, aEmCode,
  aCount: string): string;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_FoodDayCount( ';
  stSql := stSql + 'GROUP_CODE,';
  stSql := stSql + 'FD_DATE,';
  stSql := stSql + 'CO_COMPANYCODE,';
  stSql := stSql + 'EM_CODE,';
  stSql := stSql + 'FD_COUNT) ';
  stSql := stSql + ' Values(';
  stSql := stSql + '''' + GROUPCODE + ''',';
  stSql := stSql + '''' + aDate + ''',';
  stSql := stSql + '''' + aCompanyCode + ''',';
  stSql := stSql + '''' + aEmCode + ''',';
  stSql := stSql + '' + aCount + ')';

  result := stSql;
end;

function TCommonSql.UpdateTB_FoodWeekCount(aYear, aWeekDay, aCompanyCode,
  aEmCode, aCount: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FoodWeekCount set FW_COUNT = ' + aCount + ' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FW_YEAR = ''' + aYear + ''' ';
  stSql := stSql + ' AND FW_WEEKDAY = ' + aWeekDay + ' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';

  result := stSql;
end;

function TCommonSql.InsertIntoTB_FoodWeekCount(aYear, aWeekDay,
  aCompanyCode, aEmCode, aCount: string): string;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_FoodWeekCount( ';
  stSql := stSql + 'GROUP_CODE,';
  stSql := stSql + 'FW_YEAR,';
  stSql := stSql + 'FW_WEEKDAY,';
  stSql := stSql + 'CO_COMPANYCODE,';
  stSql := stSql + 'EM_CODE,';
  stSql := stSql + 'FW_COUNT) ';
  stSql := stSql + ' Values(';
  stSql := stSql + '''' + GROUPCODE + ''',';
  stSql := stSql + '''' + aYear + ''',';
  stSql := stSql + '' + aWeekDay + ',';
  stSql := stSql + '''' + aCompanyCode + ''',';
  stSql := stSql + '''' + aEmCode + ''',';
  stSql := stSql + '' + aCount + ')';

  result := stSql;
end;

function TCommonSql.UpdateTB_FoodSemesterCount(aYear, aSemesterCode,
  aCompanyCode, aEmCode, aCount: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FoodSemesterCount set FS_COUNT = ' + aCount + ' ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FS_YEAR = ''' + aYear + ''' ';
  stSql := stSql + ' AND SE_CODE = ' + aSemesterCode + ' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';

  result := stSql;
end;

function TCommonSql.InsertIntoTB_FoodSemesterCount(aYear, aSemesterCode,
  aCompanyCode, aEmCode, aCount: string): string;
var
  stSql : string;
begin
  stSql := ' Insert Into TB_FoodSemesterCount( ';
  stSql := stSql + 'GROUP_CODE,';
  stSql := stSql + 'FS_YEAR,';
  stSql := stSql + 'SE_CODE,';
  stSql := stSql + 'CO_COMPANYCODE,';
  stSql := stSql + 'EM_CODE,';
  stSql := stSql + 'FS_COUNT) ';
  stSql := stSql + ' Values(';
  stSql := stSql + '''' + GROUPCODE + ''',';
  stSql := stSql + '''' + aYear + ''',';
  stSql := stSql + '' + aSemesterCode + ',';
  stSql := stSql + '''' + aCompanyCode + ''',';
  stSql := stSql + '''' + aEmCode + ''',';
  stSql := stSql + '' + aCount + ')';

  result := stSql;
end;

function TCommonSql.UpdateTB_FOODCODECount_Inc(aDate, aFoodCode,
  aCompanyCode, aEmCode: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FOODCODECount set FD_COUNT = FD_COUNT + 1 ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FC_DATE = ''' + aDate + ''' ';
  stSql := stSql + ' AND FO_FOODCODE = ''' + aFoodCode + ''' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';

  result := stSql;
end;

function TCommonSql.UpdateTB_FoodDayCount_Inc(aDate, aCompanyCode,
  aEmCode: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FoodDayCount set FD_COUNT = FD_COUNT + 1';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FD_DATE = ''' + aDate + ''' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';

  result := stSql;
end;

function TCommonSql.UpdateTB_FoodWeekCount_Inc(aYear, aWeekDay,
  aCompanyCode, aEmCode: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FoodWeekCount set FW_COUNT = FW_COUNT + 1 ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FW_YEAR = ''' + aYear + ''' ';
  stSql := stSql + ' AND FW_WEEKDAY = ' + aWeekDay + ' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';

  result := stSql;
end;

function TCommonSql.UpdateTB_FoodSemesterCount_Inc(aYear, aSemesterCode,
  aCompanyCode, aEmCode: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FoodSemesterCount set FS_COUNT = FS_COUNT + 1 ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FS_YEAR = ''' + aYear + ''' ';
  stSql := stSql + ' AND SE_CODE = ' + aSemesterCode + ' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';

  result := stSql;
end;

function TCommonSql.InsertIntoTB_WORKGUBUN_Value(aWorkCode, aWorkType,
  aWorkName: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_WORKGUBUN (';
  stSql := stSql + ' GROUP_CODE,';
  stSql := stSql + ' WG_CODE,';
  stSql := stSql + ' WG_TYPE,';
  stSql := stSql + ' WG_NAME) ';
  stSql := stSql + ' VALUES(';
  stSql := stSql + '''' + GROUPCODE + ''', ';
  stSql := stSql + '' + aWorkCode + ', ';
  stSql := stSql + '''' + aWorkType + ''', ';
  stSql := stSql + '''' + aWorkName + ''') ';

  result :=  stSql;
end;

function TCommonSql.UpdateTB_FOODCODECount_Dec(aDate, aFoodCode,
  aCompanyCode, aEmCode: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FOODCODECount set FD_COUNT = FD_COUNT - 1 ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FC_DATE = ''' + aDate + ''' ';
  stSql := stSql + ' AND FO_FOODCODE = ''' + aFoodCode + ''' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';
  stSql := stSql + ' AND FD_COUNT > 0 ';

  result := stSql;
end;

function TCommonSql.UpdateTB_FoodDayCount_Dec(aDate, aCompanyCode,
  aEmCode: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FoodDayCount set FD_COUNT = FD_COUNT - 1';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FD_DATE = ''' + aDate + ''' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';
  stSql := stSql + ' AND FD_COUNT > 0 ';

  result := stSql;
end;

function TCommonSql.UpdateTB_FoodSemesterCount_Dec(aYear, aSemesterCode,
  aCompanyCode, aEmCode: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FoodSemesterCount set FS_COUNT = FS_COUNT - 1 ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FS_YEAR = ''' + aYear + ''' ';
  stSql := stSql + ' AND SE_CODE = ' + aSemesterCode + ' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';
  stSql := stSql + ' AND FS_COUNT > 0 ';

  result := stSql;
end;

function TCommonSql.UpdateTB_FoodWeekCount_Dec(aYear, aWeekDay,
  aCompanyCode, aEmCode: string): string;
var
  stSql : string;
begin
  stSql := 'Update TB_FoodWeekCount set FW_COUNT = FW_COUNT - 1 ';
  stSql := stSql + ' Where GROUP_CODE = ''' + GROUPCODE + ''' ';
  stSql := stSql + ' AND FW_YEAR = ''' + aYear + ''' ';
  stSql := stSql + ' AND FW_WEEKDAY = ' + aWeekDay + ' ';
  stSql := stSql + ' AND CO_COMPANYCODE = ''' + aCompanyCode + ''' ';
  stSql := stSql + ' AND EM_CODE = ''' + aEmCode + ''' ';
  stSql := stSql + ' AND FW_COUNT > 0 ';

  result := stSql;
end;

function TCommonSql.InsertIntoTB_EMPLOYEECHANGE(aCompanyCode, aEmCode,
  aCardNo, aFormName,aWorkType: string): string;
var
  stSql : string;
begin
  stSql := 'Insert Into TB_EMPLOYEECHANGE(';
  stSql := stSql + ' CO_COMPANYCODE,';
  stSql := stSql + ' EM_CODE,';
  stSql := stSql + ' CA_CARDNO, ';
  stSql := stSql + ' EC_CLIENTIP, ';
  stSql := stSql + ' EC_OPERATOR, ';
  stSql := stSql + ' EC_INSERTTIME, ';
  stSql := stSql + ' EC_FORMNAME, ';
  stSql := stSql + ' EC_WORKTYPE) ';
  stSql := stSql + ' Values( ';
  stSql := stSql + '''' + aCompanyCode + ''',';
  stSql := stSql + '''' + aEmCode + ''',';
  stSql := stSql + '''' + aCardNo + ''',';
  stSql := stSql + '''' + Get_Local_IPAddr + ''',';
  stSql := stSql + '''' + Master_ID + ''',';
  stSql := stSql + '''' + FormatDateTime('yyyymmddhhnnss',Now) + ''',';
  stSql := stSql + '''' + aFormName + ''', ';
  stSql := stSql + '''' + aWorkType + ''') ';

  result := stSql;
end;

end.
