unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, FileSystemMonitor,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    lvEventList: TListView;
    lblnfo: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FFileSystemMonitor: IFileSystemMonitor;
  public
    { Public declarations }
    procedure HandleChange(Sender: TObject; const Path: string; ChangeType:
        TFileChangeType);
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils,
  System.TypInfo;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create the IFileSystemMonitor
  FFileSystemMonitor := CreateFileSystemMonitor;
  // Monitor a directory
  FFileSystemMonitor.AddDirectory(TPath.GetTempPath, False, HandleChange);
  // Also monitor a specific file  Change the path to a file of your choice
  FFileSystemMonitor.AddFile(
    TPath.Combine(TPath.GetTempPath, 'TestFolder\Test.py'),
    HandleChange);
end;

procedure TForm1.HandleChange(Sender: TObject; const Path: string;
  ChangeType: TFileChangeType);
begin
  with lvEventList.Items.Add do
  begin
    Caption := GetEnumName(TypeInfo(TFileChangeType), Integer(ChangeType));
    SubItems.Add(Path);
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
end.
