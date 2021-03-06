object fmFingerRegistDevice: TfmFingerRegistDevice
  Left = 656
  Top = 143
  Width = 416
  Height = 360
  Caption = #51648#47928#46321#47197
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 12
    Width = 97
    Height = 13
    AutoSize = False
    Caption = #51648#47928#46321#47197#44592#54252#53944
  end
  object Panel1: TPanel
    Left = 16
    Top = 48
    Width = 177
    Height = 185
    BevelInner = bvLowered
    BevelOuter = bvLowered
    TabOrder = 0
    object Image1: TImage
      Left = 8
      Top = 8
      Width = 161
      Height = 169
    end
  end
  object Panel2: TPanel
    Left = 208
    Top = 48
    Width = 177
    Height = 185
    BevelInner = bvLowered
    BevelOuter = bvLowered
    TabOrder = 1
    object Image2: TImage
      Left = 8
      Top = 9
      Width = 161
      Height = 169
    end
  end
  object cmb_FingerPort: TComboBox
    Left = 112
    Top = 8
    Width = 73
    Height = 21
    Style = csDropDownList
    ImeName = #54620#44397#50612'('#54620#44544') (MS-IME98)'
    ItemHeight = 13
    TabOrder = 2
    OnChange = cmb_FingerPortChange
  end
  object btnRefresh: TButton
    Left = 192
    Top = 6
    Width = 137
    Height = 25
    Caption = 'Refresh Port Num'
    TabOrder = 3
    OnClick = btnRefreshClick
  end
  object btn_Save: TBitBtn
    Left = 32
    Top = 264
    Width = 145
    Height = 41
    Caption = #51648#47928#46321#47197
    Enabled = False
    TabOrder = 4
    OnClick = btn_SaveClick
    Kind = bkOK
  end
  object btn_Cancel: TBitBtn
    Left = 216
    Top = 264
    Width = 145
    Height = 41
    Caption = #52712#49548
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = btn_CancelClick
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333303
      333333333333337FF3333333333333903333333333333377FF33333333333399
      03333FFFFFFFFF777FF3000000999999903377777777777777FF0FFFF0999999
      99037F3337777777777F0FFFF099999999907F3FF777777777770F00F0999999
      99037F773777777777730FFFF099999990337F3FF777777777330F00FFFFF099
      03337F773333377773330FFFFFFFF09033337F3FF3FFF77733330F00F0000003
      33337F773777777333330FFFF0FF033333337F3FF7F3733333330F08F0F03333
      33337F7737F7333333330FFFF003333333337FFFF77333333333000000333333
      3333777777333333333333333333333333333333333333333333}
    NumGlyphs = 2
  end
  object panMessage: TPanel
    Left = 16
    Top = 104
    Width = 369
    Height = 57
    TabOrder = 6
    Visible = False
    object lb_Message: TLabel
      Left = 8
      Top = 16
      Width = 353
      Height = 25
      Alignment = taCenter
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object GetFingerTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = GetFingerTimerTimer
    Left = 184
    Top = 256
  end
  object MessageTimer: TTimer
    Enabled = False
    OnTimer = MessageTimerTimer
    Left = 256
    Top = 240
  end
  object TempADOQuery: TADOQuery
    Connection = DataModule1.ADOConnection
    Parameters = <>
    Left = 480
    Top = 112
  end
end
