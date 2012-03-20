object Form1: TForm1
  Left = 507
  Top = 309
  Caption = 'STUN test'
  ClientHeight = 130
  ClientWidth = 238
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnTest: TButton
    Left = 168
    Top = 96
    Width = 57
    Height = 25
    Caption = 'Test'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object edtServer: TLabeledEdit
    Left = 56
    Top = 8
    Width = 89
    Height = 21
    EditLabel.Width = 34
    EditLabel.Height = 13
    EditLabel.Caption = 'Server:'
    LabelPosition = lpLeft
    TabOrder = 1
    Text = 'rooyee.im'
  end
  object edtPort: TLabeledEdit
    Left = 176
    Top = 8
    Width = 49
    Height = 21
    EditLabel.Width = 19
    EditLabel.Height = 13
    EditLabel.Caption = 'Port'
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '3478'
  end
  object edtNatType: TLabeledEdit
    Left = 55
    Top = 40
    Width = 170
    Height = 21
    EditLabel.Width = 44
    EditLabel.Height = 13
    EditLabel.Caption = 'NetType:'
    LabelPosition = lpLeft
    TabOrder = 3
  end
  object edtPublicIP: TLabeledEdit
    Left = 55
    Top = 64
    Width = 170
    Height = 21
    EditLabel.Width = 42
    EditLabel.Height = 13
    EditLabel.Caption = 'PublicIP:'
    LabelPosition = lpLeft
    TabOrder = 4
  end
end
