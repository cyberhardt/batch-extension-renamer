unit duplicate;

{$mode objfpc}{$H+}
(*
  Batch file extension renamer
    duplicate.pas - duplicate fixer unit
    (c)2011-2015 Matthew Hipkin <http://www.matthewhipkin.co.uk>
*)
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, XiPanel, XiButton, StrUtils;

type

  { TfrmDup }

  TfrmDup = class(TForm)
    textDir: TDirectoryEdit;
    Label1: TLabel;
    listFiles: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    bgPanel: TXiPanel;
    btnGo: TXiButton;
    btnDone: TXiButton;
    procedure btnGoClick(Sender: TObject);
  public
    { public declarations }
  end;

type
  TArray = array of string;

var
  frmDup: TfrmDup;

implementation

{$R *.lfm}

{ TfrmDup }

(* Taken from
   http://www.jasonwhite.co.uk/delphi-explode-function-like-php-explode/ *)
function explode(cDelimiter,  sValue : string; iCount : integer) : TArray;
var
  s : string; i,p : integer;
begin
  s := sValue; i := 0;
  while length(s) > 0 do
  begin
    inc(i);
    SetLength(result, i);
    p := pos(cDelimiter,s);
    if ( p > 0 ) and ( ( i < iCount ) OR ( iCount = 0) ) then
    begin
      result[i - 1] := copy(s,0,p-1);
      s := copy(s,p + length(cDelimiter),length(s));
    end else
    begin
      result[i - 1] := s;
      s :=  '';
    end;
  end;
end;

(* Taken from
   http://stackoverflow.com/questions/5265317/delphi-count-number-of-times-a-string-occurs-in-another-string *)
function Occurrences(const Substring, Text: string): integer;
var
  offset: integer;
begin
  result := 0;
  offset := PosEx(Substring, Text, 1);
  while offset <> 0 do
  begin
    inc(result);
    offset := PosEx(Substring, Text, offset + length(Substring));
  end;
end;

procedure TfrmDup.FormCreate(Sender: TObject);
begin
  bgPanel := TXiPanel.Create(Self);
  bgPanel.Parent := frmDup;
  bgPanel.Align := alClient;
  bgPanel.BorderStyle := bsNone;
  bgPanel.BevelInner:=bvNone;
  bgPanel.BevelOuter := bvNone;
  bgPanel.ColorScheme := XiPanel.csSky;
  Label1.Parent := bgPanel;
  textDir.Parent := bgPanel;
  listFiles.Parent := bgPanel;
  btnGo := TXiButton.Create(Self);
  btnGo.Parent := bgPanel;
  btnGo.Left := 80;
  btnGo.Top := 239;
  btnGo.Width := 75;
  btnGo.Height := 25;
  btnGo.Caption := 'Go';
  btnGo.ColorScheme := csNeoGrass;
  btnGo.OnClick := @btnGoClick;
  btnDone := TXiButton.Create(Self);
  btnDone.Parent := bgPanel;
  btnDone.Left := 161;
  btnDone.Top := 239;
  btnDone.Width := 75;
  btnDone.Height := 25;
  btnDone.Caption := 'Done';
  btnDone.ColorScheme := csNeoSky;
  btnDone.ModalResult := mrOK;
end;

procedure TfrmDup.btnGoClick(Sender: TObject);
var
  s: TSearchRec;
  c: Integer;
  felements: TArray;
  newName: String;
  i: Integer;
  x: Integer;
begin
  if textDir.Directory = '' then
  begin
    showmessage('No directory specified!');
    exit;
  end;
  if not DirectoryExists(textDir.Directory) then
  begin
    showmessage('Directory does not exist!');
    exit;
  end;
  // Scan directory, looking at every file, remove any duplicate extensions
  if FindFirst(textDir.Directory + PathDelim + '*', faAnyFile, s) = 0 then
  begin
    c := 0;
    repeat
      // As usual ignore directories
      if (s.Attr and not faDirectory) <> 0 then
      begin
        // Count the dots!
        felements := explode('.',ExtractFileName(s.Name),0);
        // Check the last two elements in the array
        i := High(felements);
        // Probably very long-winded way of doing this
        if felements[i] = felements[i-1] then
        begin
          // Oh oh, we have two matching extensions!
          newName := textDir.Directory + PathDelim;
          for x := 0 to i-1 do
          begin
            newName := newName + felements[x] + '.';
          end;
          newName := TrimSet(newName,['.']);
          //showmessage(s.Name + #13#10 + newName);
          RenameFileUTF8(textDir.Directory + PathDelim + s.Name,newName);
          inc(c);
          listFiles.Items.Add(s.Name + ' >> ' + newName);
        end;
      end;
    until FindNext(s) <> 0;
    listFiles.Items.Add(IntToStr(c) + ' files renamed.');
  end;
end;

end.
