# FileSystemMonitor

A Delphi library for getting notifications about file system changes.

## Introduction
Whilst there are many Delphi components for detecting changes to file system folders they suffer from serious limitations:
- typically they only allow you to monitor a single folder
- they do not support the monitoring of specific files
- they rely on the FindFirstChangeNotification API which gives no information about what has changed, requiring an inefficient search.

This library was created to address these limitations and provide additional features.

## Features
- Easy to use, but also suitable for heavy duty monitoring
- Single unit with no external dependencies
- Allows monitoring folders and/or specific files
- Uses the ReadDirectoryChangesW API which provides information about what exactly was changed
- A single instance of the component can handle the monitoring of many folders and/or files 
- Uses an I/O completion port for efficient handling of large numbers of requests
- A single thread handles all requests.
- A different notification handler can be specified for each request
- You can have multiple handlers for each folder or file
- When you monitor folders you can specify whether you want to also monitor subfolders

## Installation
You do not need to install the library. Just download or clone this repo and add the source subdirectory to the Library path.

## Usage

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create IFileSystemMonitor
  FFileSystemMonitor := CreateFileSystemMonitor;
  // Monitor a directory
  FFileSystemMonitor.AddDirectory(TPath.GetTempPath, False, HandleChange);
  // Also monitor a specific file
  FFileSystemMonitor.AddFile('pathtoyourfile',  HandleChange);
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
```

To stop monitoring a specific file or folder you use the following methods:
```pascal
    function RemoveDirectory(const Directory: string; OnChange: TMonitorChangeHandler): Boolean;
    function RemoveFile(const FilePath: string; OnChange: TMonitorChangeHandler): Boolean;
```