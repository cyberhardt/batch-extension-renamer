unit main;

{$mode delphi}{$H+}
(*
  Batch file extension renamer
    main.pas - main unit
    (c)2011-2015 Matthew Hipkin <http://www.matthewhipkin.co.uk>
*)
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, StrUtils, LCLIntf, Buttons, Menus, ExtCtrls, XiPanel, XiButton,
  fphttpclient{$IFDEF MSWINDOWS}, Windows{$ENDIF};

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    checkRecurse: TCheckBox;
    checkReplace: TCheckBox;
    labelUpdate: TLabel;
    listFiles: TListBox;
    menuFile: TMenuItem;
    menuDupRemover: TMenuItem;
    menuExit: TMenuItem;
    menuHelp: TMenuItem;
    menuAbout: TMenuItem;
    btnSave: TMenuItem;
    menuClearAll: TMenuItem;
    menuTools: TMenuItem;
    menuMain: TMainMenu;
    btnAbout: TSpeedButton;
    btnDirBased: TRadioButton;
    OpenDialog1: TOpenDialog;
    btnFileBased: TRadioButton;
    SaveDialog1: TSaveDialog;
    btnAddFile: TSpeedButton;
    btnRemoveFile: TSpeedButton;
    btnClearList: TSpeedButton;
    textTo: TComboBox;
    textFrom: TComboBox;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    listOutput: TListBox;
    textOutputDir: TDirectoryEdit;
    Label2: TLabel;
    textInputDir: TDirectoryEdit;
    Label1: TLabel;
    updatesTimer: TTimer;
    procedure btnAboutClick(Sender: TObject);
    procedure btnAddFileClick(Sender: TObject);
    procedure btnClearListClick(Sender: TObject);
    procedure btnDirBasedClick(Sender: TObject);
    procedure btnFileBasedClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnRemoveFileClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSwapClick(Sender: TObject);
    procedure checkReplaceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure labelUpdateClick(Sender: TObject);
    procedure menuClearAllClick(Sender: TObject);
    procedure menuDupRemoverClick(Sender: TObject);
    procedure menuExitClick(Sender: TObject);
    procedure updatesTimerTimer(Sender: TObject);
  private
    { private declarations }
    updatePanel: TXiPanel;
    statusPanel: TXiPanel;
    btnGo: TXiButton;
    btnSwap: TXiButton;
    btnCancel: TXiButton;
    stop: Boolean;
    fcount: integer;
    procedure GradientFillRect(Canvas: TCanvas; Rect: TRect;
                    StartColor, EndColor: TColor; Direction: TFillDirection);
    procedure btnCancelClick(Sender: TObject);
  public
    { public declarations }
    appdir: String;
    procedure showStatusPanel;
    procedure hideStatusPanel;
    procedure createFileList(mask: String; path: String; recurse: Boolean; var list: TStrings);
    procedure copyDirectoryStructure(inDir: String; outDir: String);
  end;

var
  frmMain: TfrmMain;

const
  APPVER = '1.0';
  CURRVER = 20150414;

implementation

{$R *.lfm}

uses about, duplicate, confdiag;

{$IFDEF MSWINDOWS}
function getWinVer: String;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  Result := 'Windows NT '+IntToStr(VerInfo.dwMajorVersion) + '.' + IntToStr(VerInfo.dwMinorVersion)
end;
{$ENDIF}

function httpGet(URL: String): String;
var
  HTTP: TFPHttpClient;
  OS: String;
begin
  {$ifdef Windows}
  OS := getWinVer;
  {$endif}
  {$ifdef Linux}
  OS := 'Linux';
  {$endif}
  {$ifdef FreeBSD}
  OS := 'FreeBSD';
  {$endif}
  {$ifdef Darwin}
  OS := 'OSX';
  {$endif}
  Result := '';
  HTTP := TFPHttpClient.Create(nil);
  HTTP.RequestHeaders.Add('User-Agent: Mozilla/5.0 (compatible; '+OS+'; batch-file-extension-renamer '+APPVER+' ('+IntToStr(CURRVER)+'))');
  Result := HTTP.Get(URL);
  HTTP.Free;
end;

{ TfrmMain }

procedure TfrmMain.createFileList(mask: String; path: String; recurse: Boolean; var list: TStrings);
var
  s: TSearchRec;
  searchStr: string;
  fp: string;
begin
  fp := IncludeTrailingPathDelimiter(path);
  searchStr := fp + '*';
  if FindFirst(searchStr, faAnyFile, s) = 0 then
  begin
    repeat
      Application.ProcessMessages;
      if stop = true then exit;
      if (s.Attr and faDirectory) <> faDirectory then
      begin
        if (Lowercase(ExtractFileExt(s.Name)) = Lowercase(ExtractFileExt(mask))) or (mask = '*.*') then
        begin
          list.Add(fp + s.Name);
          inc(fcount);
          statusPanel.Caption := IntToStr(fcount) + ' files found';
        end;
      end
      else
      begin
        if (s.Name <> '.') and (s.Name <> '..') and (recurse = true) then
          createFileList(mask,fp + s.Name, true, list);
      end;
    until FindNext(s) <> 0;
  end;
end;

procedure TfrmMain.copyDirectoryStructure(inDir: String; outDir: String);
var
  s: TSearchRec;
  nInDir, nOutDir: String;
begin
  if FindFirst(IncludeTrailingPathDelimiter(inDir) + '*',faDirectory, s) = 0 then
  begin
    repeat
      if (s.Name <> '.') and (s.Name <> '..') and ((s.Attr and faDirectory) = faDirectory) then
      begin
        nInDir := IncludeTrailingPathDelimiter(inDir) + s.Name;
        nOutDir := IncludeTrailingPathDelimiter(outDir) + s.Name;
        // Create new subdirectory in outDir
        if not DirectoryExists(nOutDir) then mkdir(nOutDir);
        // Recurse into subdirectory in inDir
        copyDirectoryStructure(nInDir,nOutDir);
      end;
    until FindNext(s) <> 0;
  end;
end;

procedure TfrmMain.showStatusPanel;
begin
  textInputDir.Enabled := false;
  textOutputDir.Enabled := false;
  listOutput.Enabled := false;
  checkReplace.Enabled := false;
  textFrom.Enabled := false;
  textTo.Enabled := false;
  checkRecurse.Enabled := false;
  btnAbout.Enabled := false;
  btnGo.Enabled := false;
  btnSwap.Enabled := false;
  menuFile.Enabled := false;
  menuTools.Enabled := false;
  menuHelp.Enabled := false;
  statusPanel.Visible := true;
  statusPanel.Top := (frmMain.ClientHeight div 2) - (statusPanel.ClientHeight div 2);
  statusPanel.Left := (frmMain.ClientWidth div 2) - (statusPanel.ClientWidth div 2);
end;

procedure TfrmMain.hideStatusPanel;
begin
  textInputDir.Enabled := true;
  if checkReplace.Checked = false then textOutputDir.Enabled := true;
  listOutput.Enabled := true;
  checkReplace.Enabled := true;
  textFrom.Enabled := true;
  textTo.Enabled := true;
  checkRecurse.Enabled := true;
  btnAbout.Enabled := true;
  btnGo.Enabled := true;
  btnSwap.Enabled := true;
  menuFile.Enabled := true;
  menuTools.Enabled := true;
  menuHelp.Enabled := true;
  statusPanel.Visible := false;
end;

procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  stop := true;
  hideStatusPanel;
end;

procedure TfrmMain.btnGoClick(Sender: TObject);
var
  newFilename: String;
  c: integer;
  add: Boolean;
  files: TStrings;
  x: integer;
begin
  fcount := 0;
  files := TStringList.Create;
  stop := false;
  // Add to lists if it doesn't already exist
  add := true;
  for c := 0 to textTo.Items.Count -1 do
  begin
    if textTo.Items[c] = textTo.Text then add := false;
  end;
  if add then textTo.Items.Add(textTo.Text);
  add := true;
  for c := 0 to textFrom.Items.Count -1 do
  begin
    if textFrom.Items[c] = textFrom.Text then add := false;
  end;
  if add then textFrom.Items.Add(textFrom.Text);
  // Firstly exit if anything isn't set that should be
  if (textInputDir.Directory = '') and (btnDirBased.Checked = true) then
  begin
    showmessage('No input directory specified!');
    exit;
  end;
  if (textOutputDir.Directory = '') and (checkReplace.Checked = false) then
  begin
    showmessage('No output directory specified!');
    exit;
  end;
  if textFrom.Text = '' then
  begin
    showmessage('No input files specified!');
    exit;
  end;
  if textTo.Text = '' then
  begin
    showmessage('No output files specified!');
    exit;
  end;
  if (textFrom.Text = '*.*') and (checkReplace.Checked = true) then
  begin
    { Display a confirmation dialog to make the user fully aware of what
      they are about to do }
    frmConfirm.Label1.Caption := 'Are you sure you wish to rename ALL files'#13#10'in '+textInputDir.Directory+'?';
    if frmConfirm.ShowModal = mrCancel then exit;
  end;
  // Firstly build file list if we are in directory mode
  if btnDirBased.Checked then
  begin
    showStatusPanel;
    listOutput.Items.Add('Building file list...');
    //createFileList('*.*',textInputDir.Directory,checkRecurse.Checked,files);
    createFileList(textFrom.Text,textInputDir.Directory,checkRecurse.Checked,files);
    hideStatusPanel;
    listOutput.Items.Add(IntToStr(files.Count) + ' files found.');
  end
  // Or just take the list from the file list
  else files.AddStrings(listFiles.Items);

  (*
    Conditions:
     - is file based?
       - is replacing original?
     - is directory based?
       - is recursive?
       - is replacing original?
  *)

  { If we are doing a recursive directory based rename that doesn't replace
    existing files we'll need to recreate the directory structure in the new location }
  if (btnDirBased.Checked) and (checkRecurse.Checked) and (checkReplace.Checked = false) then
    copyDirectoryStructure(textInputDir.Directory,textOutputDir.Directory);

  // Loop through files list and rename them
  for x := 0 to files.Count -1 do
  begin
    // Build new filename
    newFileName := '';
    if ExtractFileExt(files[x]) <> '' then
      newFileName := AnsiReplaceStr(files[x],ExtractFileExt(files[x]),ExtractFileExt(textTo.Text))
    else
      newFileName := files[x] + ExtractFileExt(textTo.Text);
    // Perform rename/copy
    if checkReplace.Checked then RenameFileUTF8(files[x],newFileName)
    else
    begin
      { Need to modify newFilename if we are doing a recursive directory based
        rename }
      if (btnDirBased.Checked = true) and (checkRecurse.Checked = true) then
      begin
        newFileName := AnsiReplaceStr(newFileName,textInputDir.Text,textOutputDir.Text);
        FileUtil.CopyFile(files[x],newFileName,true);
      end
      else
        FileUtil.CopyFile(files[x],IncludeTrailingPathDelimiter(textInputDir.Directory) + ExtractFilename(newFileName),true);
    end;
    // Add a line to the listbox
    listOutput.Items.Add(files[x] + ' >> ' + newFilename);
  end;
  files.Free;
end;

procedure TfrmMain.btnRemoveFileClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to listFiles.Count -1 do
  begin
    if listFiles.Selected[i] then listFiles.Items.Delete(i);
  end;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    listOutput.Items.SaveToFile(SaveDialog1.FileName);
end;

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.btnAddFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    listFiles.Items.AddStrings(OpenDialog1.Files);
end;

procedure TfrmMain.btnClearListClick(Sender: TObject);
begin
  listFiles.Clear;
end;

procedure TfrmMain.btnDirBasedClick(Sender: TObject);
begin
  if btnDirBased.Checked then
  begin
    textInputDir.Enabled := true;
    checkReplace.Enabled := true;
    checkRecurse.Enabled := true;
    textFrom.Enabled := true;
    textOutputDir.Enabled := not checkReplace.Checked;
    listFiles.Enabled := false;
    btnAddFile.Enabled := false;
    btnRemoveFile.Enabled := false;
    btnClearList.Enabled := false;
    btnSwap.Enabled := true;
  end;
end;

procedure TfrmMain.btnFileBasedClick(Sender: TObject);
begin
  if btnFileBased.Checked then
  begin
    listFiles.Enabled := true;
    btnAddFile.Enabled := true;
    btnRemoveFile.Enabled := true;
    btnClearList.Enabled := true;
    textInputDir.Enabled := false;
    checkRecurse.Enabled := false;
    textFrom.Enabled := false;
    btnSwap.Enabled := false;
  end;
end;

procedure TfrmMain.btnSwapClick(Sender: TObject);
var
  t: String;
begin
  t := textTo.Text;
  textTo.Text := textFrom.Text;
  textFrom.Text := t;
end;

procedure TfrmMain.checkReplaceClick(Sender: TObject);
begin
  textOutputDir.Enabled := not checkReplace.Checked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  appdir :=  GetUserDir + '.batchrename' + PathDelim;
  if not DirectoryExists(appdir) then mkdir(appdir);
  // Load dropdown contents
  if FileExists(appdir + 'to.lst') then
    textTo.Items.LoadFromFile(appdir + 'to.lst');
  if FileExists(appdir + 'from.lst') then
    textFrom.Items.LoadFromFile(appdir + 'from.lst');
  Label3.Caption := APPVER;
  // Positioning fixes for different font/widget sizes
  Label5.Top := textFrom.Top + (textFrom.Height - Label5.Height) - 1;
  Label6.Top := textFrom.Top + (textFrom.Height - Label6.Height) - 1;
  btnSwap := TXiButton.Create(Self);
  btnSwap.Parent := frmMain;
  btnSwap.Left := Button1.Left;
  btnSwap.Top := Button1.Top;
  btnSwap.Width := Button1.Width;
  btnSwap.Height := Button1.Height;
  btnSwap.Caption := 'Swap';
  btnSwap.ColorScheme := csNeoSky;
  btnSwap.OnClick := btnSwapClick;
  Button1.Visible := false;
  btnGo := TXiButton.Create(Self);
  btnGo.Parent := frmMain;
  btnGo.Left := Button2.Left;
  btnGo.Top := Button2.Top;
  btnGo.Width := Button2.Width;
  btnGo.Height := Button2.Height;
  btnGo.Caption := 'Go';
  btnGo.ColorScheme := csNeoGrass;
  btnGo.OnClick := btnGoClick;
  Button2.Visible := false;
  updatePanel := TXiPanel.Create(self);
  updatePanel.Parent := frmMain;
  updatePanel.Top := 345;
  updatePanel.Height := 29;
  updatePanel.Width := frmMain.Width div 2;
  updatePanel.Left := (frmMain.ClientWidth div 2) - (updatePanel.Width div 2);
  updatePanel.ColorScheme := XiPanel.csRose;
  labelUpdate.Parent := updatePanel;
  labelUpdate.Align := alClient;
  labelUpdate.Alignment := taCenter;
  labelUpdate.Layout := tlCenter;
  labelUpdate.Caption := 'A new version is available';
  updatePanel.Visible := false;
  statusPanel := TXiPanel.Create(self);
  statusPanel.Parent := frmMain;
  statusPanel.Top := 100;
  statusPanel.Left := 100;
  statusPanel.Width := 353;
  statusPanel.Height := 189;
  statusPanel.ColorScheme := XiPanel.csGrass;
  statusPanel.Caption := 'Please wait';
  statusPanel.Visible := false;
  btnCancel := TXiButton.Create(self);
  btnCancel.Parent := statusPanel;
  btnCancel.Height := 25;
  btnCancel.Width := 75;
  btnCancel.Left := (statusPanel.Width div 2) - (btnCancel.Width div 2);
  btnCancel.Top := (statusPanel.ClientHeight - btnCancel.Height) - 2;
  btnCancel.Caption := 'Cancel';
  btnCancel.ColorScheme := csNeoRose;
  btnCancel.OnClick := btnCancelClick;
  updatesTimer.Enabled := true;
  btnDirbasedClick(Sender);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Stops a crash if an XiButton was the last used control
  textInputDir.SetFocus;
  // Save dropbox contents
  textTo.Items.SaveToFile(appdir + 'to.lst');
  textFrom.Items.SaveToFile(appdir + 'from.lst');
  // Clean up
  btnGo.Free;
  btnSwap.Free;
  updatePanel.Free;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i: integer;
begin
  for i := Low(FileNames) to High(FileNames) do
  begin
    listFiles.Items.Add(FileNames[i]);
  end;
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  GradientFillRect(frmMain.Canvas,
                   Classes.Rect(0, 0, frmMain.ClientWidth, frmMain.ClientHeight),
                   clWhite, $00FFE0C1, fdVertical);
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  if updatePanel.Visible then
  begin
    updatePanel.Left := (frmMain.ClientWidth div 2) - (updatePanel.Width div 2);
    updatePanel.Top := frmMain.ClientHeight - updatePanel.Height;
  end;
  if statusPanel.Visible then
  begin
    statusPanel.Top := (frmMain.ClientHeight div 2) - (statusPanel.ClientHeight div 2);
    statusPanel.Left := (frmMain.ClientWidth div 2) - (statusPanel.ClientWidth div 2);
  end;
end;

procedure TfrmMain.labelUpdateClick(Sender: TObject);
begin
  OpenURL('http://www.matthewhipkin.co.uk');
end;

procedure TfrmMain.menuClearAllClick(Sender: TObject);
begin
  textInputDir.Text := '';
  textOutputDir.Text := '';
  listFiles.Clear;
  listOutput.Clear;
end;

procedure TfrmMain.menuDupRemoverClick(Sender: TObject);
begin
  frmDup.ShowModal;
end;

procedure TfrmMain.menuExitClick(Sender: TObject);
begin
  // Goodbye!
  Application.Terminate;
end;

procedure TfrmMain.updatesTimerTimer(Sender: TObject);
var
  response: String;
  newVer: Boolean;
begin
  updatesTimer.Enabled := false;
  // Check for a new version comparing the CURRVER variable to the value returned
  newVer := false;
  try
    response := httpGet('http://www.matthewhipkin.co.uk/batchrename.txt');
    response := trim(response);
    if CURRVER < StrToInt(response) then newVer := true;
  except
    newVer := false;
  end;
  if newVer then
  begin
    updatePanel.Visible := true;
    updatePanel.Top := frmMain.ClientHeight - updatePanel.Height;
    updatePanel.BringToFront;
  end;
end;

{ Taken from XiPanel.pas
  due to an incompatibility with Lazarus/FreePascal its better to paint the
  gradient directly onto the form's canvas }

procedure TfrmMain.GradientFillRect(Canvas: TCanvas; Rect: TRect;
                StartColor, EndColor: TColor; Direction: TFillDirection);
var
  Steps: Integer;
  StartR, StartG, StartB, EndR, EndG, EndB: Byte;
  CrrR, CrrG, CrrB: Double;
  IncR, IncG, incB: Double;
  i: integer;
begin
  case Direction of
    fdVertical:   Steps:= Rect.Bottom - Rect.Top;
    fdHorizontal: Steps:= Rect.Right - Rect.Left;
    fdDiagonal:   Steps:= Rect.Bottom - Rect.Top + Rect.Right - Rect.Left;
  end;

  StartR:= GetRValue(StartColor);  EndR:= GetRValue(EndColor);
  StartG:= GetGValue(StartColor);  EndG:= GetGValue(EndColor);
  StartB:= GetBValue(StartColor);  EndB:= GetBValue(EndColor);

  IncR:= (EndR - StartR) / steps;
  IncG:= (EndG - StartG) / steps;
  IncB:= (EndB - StartB) / steps;

  CrrR:= StartR;
  CrrG:= StartG;
  CrrB:= StartB;

  for i:= 0 to Steps do begin
    Canvas.Pen.Color:= RGB(Round(CrrR), Round(CrrG), Round(CrrB));
    case Direction of
      fdVertical:   begin
                      Canvas.MoveTo(Rect.Left, i);
                      Canvas.LineTo(Rect.Right + Rect.Left, i);
                    end;
      fdHorizontal: begin
                      Canvas.MoveTo(i, Rect.Top);
                      Canvas.LineTo(i, Rect.Top + Rect.Bottom);
                    end;
      fdDiagonal:   begin
                      Canvas.MoveTo(i, Rect.Top);
                      Canvas.LineTo(Rect.Left, i);
                    end;
    end;
    CrrR:= CrrR + IncR;
    CrrG:= CrrG + IncG;
    CrrB:= CrrB + IncB;
  end;
end;

end.

