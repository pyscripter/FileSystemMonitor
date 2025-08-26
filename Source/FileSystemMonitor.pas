{-----------------------------------------------------------------------------
 Unit Name: FileSystemMonitor
 Author:    PyScripter
 Purpose:   Component for monitoring changes in files and/or directories
 License:   MIT
-----------------------------------------------------------------------------}

unit FileSystemMonitor;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  System.IOUtils;

type
  TFileChangeType = (fcAdded, fcRemoved, fcModified, fcRenamedOld, fcRenamedNew);

  TMonitorChangeHandler = procedure(Sender: TObject; const Path: string;
    ChangeType: TFileChangeType) of object;

  TNotifyFlag = (
    nfFileName, nfDirName, nfAttributes, nfSize, nfLastWrite, nfLastAccess,
    nfCreation, nfSecurity
  );
  TNotifyFlags = set of TNotifyFlag;

  const
    DefaultNotifyFlags = [nfFileName, nfDirName, nfSize, nfLastWrite, nfCreation];

type
  TFileSystemMonitor = class;

  TFileNotifyInformation = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array[0..MAX_PATH - 1] of WideChar;
  end;
  PFileNotifyInformation = ^TFileNotifyInformation;

  TDirectoryHandlerInfo = record
    Handler: TMonitorChangeHandler;
    WatchSubtree: Boolean;
    class function Create(AHandler: TMonitorChangeHandler;
      AWatchSubtree: Boolean): TDirectoryHandlerInfo; static;
  end;

  PMonitorInfo = ^TMonitorInfo;
  TMonitorInfo = record
    Overlapped: TOverlapped;
    Directory: string;
    DirectoryHandle: THandle;
    Buffer: TBytes;
    NotifyFilter: DWORD;
    DirectoryHandlers: TList<TDirectoryHandlerInfo>;
    FileHandlers: TObjectDictionary<string, TList<TMonitorChangeHandler>>;
    function WatchSubtree: Boolean;
    class function NewMonitorInfo: PMonitorInfo; static;
    class procedure FreeMonitorInfo(MonitorInfo: PMonitorInfo); static;
  end;

  TFileSystemMonitor = class(TComponent)
  private
    FCompletionPort: THandle;
    FMonitorList: TList<PMonitorInfo>;
    FWorkerThread: TThread;
    FSync: TCriticalSection;
    procedure HandleChange(MonitorInfo: PMonitorInfo; NumBytes: DWORD);
    procedure WorkerThreadMethod;
    function NormalizePath(const Path: string): string;
    function AddDirectoryMonitor(const Directory: string; WatchSubtree: Boolean;
        NotifyFlags: TNotifyFlags; const FilePath: string; OnChange:
        TMonitorChangeHandler): Boolean;
    function NotifyFlagsToDWORD(const Flags: TNotifyFlags): DWORD;
    function StartDirectoryMonitoring(MonitorInfo: PMonitorInfo): Boolean;
    procedure Start;
    procedure Stop;
  public
    class var BufferSize: Integer; // default 65536
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddDirectory(const Directory: string; WatchSubtree: Boolean;
      OnChange: TMonitorChangeHandler; NotifyFlags: TNotifyFlags = DefaultNotifyFlags): Boolean;
    function AddFile(const FilePath: string; OnChange: TMonitorChangeHandler;
      NotifyFlags: TNotifyFlags = DefaultNotifyFlags): Boolean;
    function RemoveDirectory(const Directory: string; OnChange: TMonitorChangeHandler): Boolean;
    function RemoveFile(const FilePath: string; OnChange: TMonitorChangeHandler): Boolean;
    function IsMonitoring: Boolean;
  end;

implementation

{ TDirectoryHandlerInfo }

class function TDirectoryHandlerInfo.Create(AHandler: TMonitorChangeHandler;
  AWatchSubtree: Boolean): TDirectoryHandlerInfo;
begin
  Result.Handler := AHandler;
  Result.WatchSubtree := AWatchSubtree;
end;

function TFileSystemMonitor.NotifyFlagsToDWORD(const Flags: TNotifyFlags): DWORD;
begin
  Result := 0;
  if nfFileName in Flags then Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
  if nfDirName in Flags then Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
  if nfAttributes in Flags then Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if nfSize in Flags then Result := Result or FILE_NOTIFY_CHANGE_SIZE;
  if nfLastWrite in Flags then Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if nfLastAccess in Flags then Result := Result or FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if nfCreation in Flags then Result := Result or FILE_NOTIFY_CHANGE_CREATION;
  if nfSecurity in Flags then Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
end;

{ TFileSystemMonitor }

constructor TFileSystemMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BufferSize := 65536;
  FMonitorList := TList<PMonitorInfo>.Create;
  FSync := TCriticalSection.Create;

  // Create empty Completion port
  FCompletionPort := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  if FCompletionPort = 0 then
    raise EOSError.Create('Failed to create I/O completion port.');
  Start;
end;

destructor TFileSystemMonitor.Destroy;
begin
  Stop;
  FreeAndNil(FMonitorList);
  FreeAndNil(FSync);
  inherited;
end;

function TFileSystemMonitor.NormalizePath(const Path: string): string;
var
  LongPath: array[0..MAX_PATH - 1] of Char;
begin
  Result := TPath.GetFullPath(Path);

  // Remove trailing path delimiter
  Result := ExcludeTrailingPathDelimiter(Result);

  // Convert to long path (correct casing, expand 8.3 names)
  if GetLongPathName(PChar(Result), LongPath, MAX_PATH) > 0 then
    Result := LongPath;

  // Uppercase drive letter if present
  if (Length(Result) >= 2) and (Result[2] = DriveDelim) then
    Result[1] := UpCase(Result[1]);
end;

function TFileSystemMonitor.AddDirectory(const Directory: string; WatchSubtree:
    Boolean; OnChange: TMonitorChangeHandler; NotifyFlags: TNotifyFlags =
    DefaultNotifyFlags): Boolean;
begin
  Result := AddDirectoryMonitor(Directory, WatchSubtree, NotifyFlags, '', OnChange);
end;

function TFileSystemMonitor.AddFile(const FilePath: string; OnChange:
  TMonitorChangeHandler; NotifyFlags: TNotifyFlags = DefaultNotifyFlags):Boolean;
begin
  Result := AddDirectoryMonitor(TPath.GetDirectoryName(FilePath), False,
    NotifyFlags, FilePath, OnChange);
end;

function TFileSystemMonitor.AddDirectoryMonitor(const Directory: string;
  WatchSubtree: Boolean; NotifyFlags: TNotifyFlags; const FilePath: string;
  OnChange: TMonitorChangeHandler): Boolean;
var
  MonitorInfo: PMonitorInfo;
  DirectoryHandle: THandle;
  NormalizedDir, NormalizedFile: string;
  HandlerList: TList<TMonitorChangeHandler>;
begin
  FSync.Enter;
  try
    Result := False;
    if not TDirectory.Exists(Directory) then Exit;
    if not Assigned(OnChange) then Exit;

    NormalizedDir := NormalizePath(Directory);

    for MonitorInfo in FMonitorList do
    begin
      if SameText(MonitorInfo.Directory, NormalizedDir) then
      begin
         // The folder is already monitored
        if FilePath = '' then
        begin
          MonitorInfo.DirectoryHandlers.Add(TDirectoryHandlerInfo.Create(OnChange, WatchSubtree));
          if WatchSubtree and not MonitorInfo.WatchSubtree then
          begin
            CancelIoEx(MonitorInfo.DirectoryHandle, @MonitorInfo.Overlapped);
            StartDirectoryMonitoring(MonitorInfo);
          end;
        end
        else
        begin
          NormalizedFile := NormalizePath(FilePath);
          if not MonitorInfo.FileHandlers.TryGetValue(NormalizedFile, HandlerList) then
          begin
            HandlerList := TList<TMonitorChangeHandler>.Create;
            MonitorInfo.FileHandlers.Add(NormalizedFile, HandlerList);
          end;
          HandlerList.Add(OnChange);
        end;
        Exit(True);
      end;
    end;

    // Create new folder monitor
    DirectoryHandle := CreateFile(
      PChar(NormalizedDir),
      FILE_LIST_DIRECTORY,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
      nil,
      OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,
      0
    );

    if DirectoryHandle = INVALID_HANDLE_VALUE then
      Exit;

    MonitorInfo := TMonitorInfo.NewMonitorInfo;
    try
      MonitorInfo.Directory := NormalizedDir;
      MonitorInfo.DirectoryHandle := DirectoryHandle;
      MonitorInfo.NotifyFilter := NotifyFlagsToDWORD(NotifyFlags);

      if FilePath = '' then
        MonitorInfo.DirectoryHandlers.Add(TDirectoryHandlerInfo.Create(OnChange, WatchSubtree))
      else
      begin
        NormalizedFile := NormalizePath(FilePath);
        HandlerList := TList<TMonitorChangeHandler>.Create;
        HandlerList.Add(OnChange);
        MonitorInfo.FileHandlers.Add(NormalizedFile, HandlerList);
      end;

      if (CreateIoCompletionPort(DirectoryHandle, FCompletionPort,
        ULONG_PTR(MonitorInfo), 0) <> FCompletionPort) or
        not StartDirectoryMonitoring(MonitorInfo)
      then
        raise EOSError.Create('Failed to start directory monitoring.');

      FMonitorList.Add(MonitorInfo);
      Result := True;
    except
      TMonitorInfo.FreeMonitorInfo(MonitorInfo);
    end;

  finally
    FSync.Leave;
  end;
end;

function TFileSystemMonitor.StartDirectoryMonitoring(MonitorInfo: PMonitorInfo): Boolean;
begin
  Result := ReadDirectoryChangesW(
    MonitorInfo.DirectoryHandle,
    @MonitorInfo.Buffer[0],
    Length(MonitorInfo.Buffer),
    MonitorInfo.WatchSubtree,
    MonitorInfo.NotifyFilter,
    nil,
    POverlapped(MonitorInfo),
    nil
  );
end;

procedure TFileSystemMonitor.HandleChange(MonitorInfo: PMonitorInfo; NumBytes: DWORD);

  procedure LaunchHandlers(const FullPath: string; ChangeType: TFileChangeType);
  // The anonymous mehtod is cantained in a function, so that we capture values
  // and not variables which may change by the time the method is executed
  begin
    TThread.Queue(FWorkerThread,
      procedure
      var
        IsSubdirectoryChange: Boolean;
        HandlerInfo: TDirectoryHandlerInfo;
        Handler: TMonitorChangeHandler;
        FileHandlerList: TList<TMonitorChangeHandler>;
      begin
        IsSubdirectoryChange := not SameText(MonitorInfo.Directory,
          TPath.GetDirectoryName(FullPath));

        for HandlerInfo in MonitorInfo.DirectoryHandlers do
        begin
          if HandlerInfo.WatchSubtree or not IsSubdirectoryChange then
            HandlerInfo.Handler(Self, FullPath, ChangeType);
        end;

        if MonitorInfo.FileHandlers.TryGetValue(FullPath, FileHandlerList) then
        begin
          for Handler in FileHandlerList do
            Handler(Self, FullPath, ChangeType);
        end;
      end);
  end;

var
  NotifyInfo: PFileNotifyInformation;
  Offset: Longint;
  FileName, FullPath: string;
  ChangeType: TFileChangeType;
begin
  if NumBytes = 0 then
    Exit;

  NotifyInfo := PFileNotifyInformation(@MonitorInfo.Buffer[0]);
  repeat
    Offset := NotifyInfo^.NextEntryOffset;
    SetString(FileName, NotifyInfo^.FileName, NotifyInfo^.FileNameLength div SizeOf(Char));
    FullPath := TPath.Combine(MonitorInfo.Directory, FileName);

    FSync.Enter;
    try
      case NotifyInfo^.Action of
        FILE_ACTION_ADDED: ChangeType := fcAdded;
        FILE_ACTION_REMOVED: ChangeType := fcRemoved;
        FILE_ACTION_MODIFIED: ChangeType := fcModified;
        FILE_ACTION_RENAMED_OLD_NAME: ChangeType := fcRenamedOld;
        FILE_ACTION_RENAMED_NEW_NAME: ChangeType := fcRenamedNew;
      else
        ChangeType := fcModified;  // to keep the compiler happy
      end;

      LaunchHandlers(FullPath, ChangeType);

    finally
      FSync.Leave;
    end;

    if Offset > 0 then
      PByte(NotifyInfo) := PByte(NotifyInfo) + Offset
    else
      Break;
  until False;

  StartDirectoryMonitoring(MonitorInfo);
end;

procedure TFileSystemMonitor.WorkerThreadMethod;
var
  NumBytes: DWORD;
  CompletionKey: ULONG_PTR;
  Overlapped: POverlapped;
  MonitorInfo: PMonitorInfo;
  Success: Boolean;
  MonitorEntry: PMonitorInfo;
  DirectoriesToDelete: TArray<PMonitorInfo>;
begin
  while not TThread.CheckTerminated do
  begin
    NumBytes := 0;
    CompletionKey := 0;
    Overlapped := nil;

    Success := GetQueuedCompletionStatus(
      FCompletionPort,
      NumBytes,
      CompletionKey,
      Overlapped,
      10000 // 10-second timeout
    );

    if Success then
    begin
      if (NumBytes = 0) and (CompletionKey = 0) and (Overlapped = nil) then
        // Empty Completion status posted signalling to exit the thread
        Break;

      // Handle the Change
      FSync.Enter;
      try
        HandleChange(PMonitorInfo(Overlapped), NumBytes);
      finally
        FSync.Leave;
      end;
    end
    else
    begin
      if GetLastError = WAIT_TIMEOUT then
      begin
        // If a monitored directory gets deleted/renamed we get no notification.
        // So periodically we need to check whether the monitored directories
        // still exist.
        DirectoriesToDelete := [];
        FSync.Enter;
        try
          for MonitorEntry in FMonitorList do
          begin
            if not TDirectory.Exists(MonitorEntry.Directory) then
              DirectoriesToDelete := DirectoriesToDelete + [MonitorEntry];
          end;

          for MonitorInfo in DirectoriesToDelete do
          begin
            FMonitorList.Remove(MonitorInfo);
            TMonitorInfo.FreeMonitorInfo(MonitorInfo);
          end;
        finally
          FSync.Leave;
        end;
        Continue;
      end;
    end;
  end;
end;

procedure TFileSystemMonitor.Start;
begin
  FSync.Enter;
  try
    if (FWorkerThread <> nil) then
      Exit;

    FWorkerThread := TThread.CreateAnonymousThread(WorkerThreadMethod);
    FWorkerThread.FreeOnTerminate := False;
    FWorkerThread.NameThreadForDebugging('File System Monitor');
    FWorkerThread.Start;
  finally
    FSync.Leave;
  end;
end;

procedure TFileSystemMonitor.Stop;
var
  MonitorInfo: PMonitorInfo;
begin
  FSync.Enter;
  try
    if Assigned(FWorkerThread) then
    begin
      PostQueuedCompletionStatus(FCompletionPort, 0, 0, nil);
    end;

    for MonitorInfo in FMonitorList do
      TMonitorInfo.FreeMonitorInfo(MonitorInfo);

    FreeAndNil(FMonitorList);

    if FCompletionPort <> 0 then
    begin
      CloseHandle(FCompletionPort);
      FCompletionPort := 0;
    end;
  finally
    FSync.Leave;
  end;

  FreeAndNil(FWorkerThread);
end;

function TFileSystemMonitor.RemoveDirectory(const Directory: string; OnChange:
    TMonitorChangeHandler): Boolean;
var
  MonitorInfo: PMonitorInfo;
  NormalizedDir: string;
  I: Integer;
  OldWatchSubtree: Boolean;
begin
  FSync.Enter;
  try
    Result := False;
    NormalizedDir := NormalizePath(Directory);
    for MonitorInfo in FMonitorList.ToArray do
    begin
      if SameText(MonitorInfo.Directory, NormalizedDir) then
      begin
        for I := MonitorInfo.DirectoryHandlers.Count - 1 downto 0 do
        begin
          if TMethod(MonitorInfo.DirectoryHandlers[I].Handler) = TMethod(OnChange) then
          begin
            Result := True;
            OldWatchSubtree := MonitorInfo.WatchSubtree;
            MonitorInfo.DirectoryHandlers.Delete(I);

            if (MonitorInfo.DirectoryHandlers.Count = 0) and
              (MonitorInfo.FileHandlers.Count = 0) then
            begin
              TMonitorInfo.FreeMonitorInfo(MonitorInfo);
              FMonitorList.Remove(MonitorInfo);
            end
            else if OldWatchSubtree <> MonitorInfo.WatchSubtree then
            begin
              CancelIoEx(MonitorInfo.DirectoryHandle, @MonitorInfo.Overlapped);
              StartDirectoryMonitoring(MonitorInfo);
            end;

            Break;
          end;
        end;
      end;
    end;
  finally
    FSync.Leave;
  end;
end;

function TFileSystemMonitor.RemoveFile(const FilePath: string; OnChange:
    TMonitorChangeHandler): Boolean;
var
  MonitorInfo: PMonitorInfo;
  NormalizedFile, Directory: string;
  HandlerList: TList<TMonitorChangeHandler>;
  HandlerIndex: Integer;
begin
  FSync.Enter;
  try
    Result := False;
    NormalizedFile := NormalizePath(FilePath);
    Directory := TPath.GetDirectoryName(NormalizedFile);

    for MonitorInfo in FMonitorList.ToArray do
    begin
      if SameText(MonitorInfo.Directory, Directory) then
      begin
        if MonitorInfo.FileHandlers.TryGetValue(NormalizedFile, HandlerList) then
        begin
          HandlerIndex := HandlerList.IndexOf(OnChange);
          if HandlerIndex >= 0 then
          begin
            Result := True;
            HandlerList.Delete(HandlerIndex);
            if HandlerList.Count = 0 then
              MonitorInfo.FileHandlers.Remove(NormalizedFile);

            if Result and (MonitorInfo.DirectoryHandlers.Count = 0) and
              (MonitorInfo.FileHandlers.Count = 0) then
            begin
              TMonitorInfo.FreeMonitorInfo(MonitorInfo);
              FMonitorList.Remove(MonitorInfo);
            end;

            Break;
          end;
        end;
      end;
    end;
  finally
    FSync.Leave;
  end;
end;

function TFileSystemMonitor.IsMonitoring: Boolean;
begin
  FSync.Enter;
  try
    Result := (FWorkerThread <> nil) and not FWorkerThread.Finished;
  finally
    FSync.Leave;
  end;
end;

{ TMonitorInfo }

class function TMonitorInfo.NewMonitorInfo: PMonitorInfo;
begin
  New(Result);
  ZeroMemory(@Result.Overlapped, SizeOf(TOverlapped));
  Result.DirectoryHandlers := TList<TDirectoryHandlerInfo>.Create;
  Result.FileHandlers := TObjectDictionary<string,
        TList<TMonitorChangeHandler>>.Create([doOwnsValues]);
  SetLength(Result.Buffer, TFileSystemMonitor.BufferSize);
end;

function TMonitorInfo.WatchSubtree: Boolean;
var
  HandlerInfo: TDirectoryHandlerInfo;
begin
  Result := False;
  for HandlerInfo in DirectoryHandlers do
    if HandlerInfo.WatchSubtree then
      Exit(True);
end;

class procedure TMonitorInfo.FreeMonitorInfo(MonitorInfo: PMonitorInfo);
begin
  if Assigned(MonitorInfo) then
  begin
    if MonitorInfo.DirectoryHandle <> INVALID_HANDLE_VALUE then
    begin
      CancelIoEx(MonitorInfo.DirectoryHandle, @MonitorInfo.Overlapped);
      CloseHandle(MonitorInfo.DirectoryHandle);
    end;
    MonitorInfo.DirectoryHandlers.Free;
    MonitorInfo.FileHandlers.Free;
    Dispose(MonitorInfo);
  end;
end;

end.
