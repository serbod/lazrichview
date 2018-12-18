object frmRVSEdit: TfrmRVSEdit
  Left = 104
  Top = 31
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'RichView Styles Editor'
  ClientHeight = 370
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 7
    Top = 326
    Width = 386
    Height = 6
    Shape = bsBottomLine
  end
  object Label1: TLabel
    Left = 8
    Top = 7
    Width = 55
    Height = 13
    Caption = 'Text Styles:'
  end
  object btnOk: TButton
    Left = 321
    Top = 339
    Width = 75
    Height = 25
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnDel: TButton
    Left = 319
    Top = 69
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 2
    OnClick = btnDelClick
  end
  object btnEdit: TButton
    Left = 319
    Top = 41
    Width = 75
    Height = 25
    Caption = 'Edit...'
    TabOrder = 1
    OnClick = btnEditClick
  end
  object btnAdd: TButton
    Left = 319
    Top = 13
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 0
    OnClick = btnAddClick
  end
  object tv: TTreeView
    Left = 8
    Top = 23
    Width = 306
    Height = 218
    HideSelection = False
    Indent = 19
    ReadOnly = True
    ShowRoot = False
    TabOrder = 4
    OnChange = tvChange
  end
  object panPreview: TPanel
    Left = 6
    Top = 247
    Width = 390
    Height = 78
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Style Preview'
    TabOrder = 5
  end
end
