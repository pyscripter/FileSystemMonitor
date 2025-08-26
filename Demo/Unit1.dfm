object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'FileSystemMonitor Demo'
  ClientHeight = 441
  ClientWidth = 713
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object lblnfo: TLabel
    Left = 0
    Top = 0
    Width = 713
    Height = 34
    Align = alTop
    AutoSize = False
    Caption = 
      'This demo monitors the user TEMP directory.  Make changes to fil' +
      'es in this directory and observe the received notifications belo' +
      'w:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lvEventList: TListView
    Left = 0
    Top = 56
    Width = 713
    Height = 385
    Align = alBottom
    Columns = <
      item
        Caption = 'Event'
        Width = 100
      end
      item
        Caption = 'Path'
        Width = 500
      end>
    GridLines = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
