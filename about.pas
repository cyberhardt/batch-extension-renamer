unit about;

{$mode objfpc}{$H+}
(*
  Batch file extension renamer
    about.pas - about dialog unit
    (c)2011-2015 Matthew Hipkin <http://www.matthewhipkin.co.uk>
*)
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  XiPanel, XiButton, LCLIntF, Buttons;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnTwitter: TSpeedButton;
    procedure btnTwitterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    { private declarations }
    btnOK: TXiButton;
    bgPanel: TXiPanel;
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  bgPanel := TXiPanel.Create(self);
  bgPanel.Parent := frmAbout;
  bgPanel.Align := alClient;
  bgPanel.BorderStyle := bsNone;
  bgPanel.BevelInner:=bvNone;
  bgPanel.BevelOuter := bvNone;
  bgPanel.ColorScheme := XiPanel.csSky;
  Label1.Parent := bgPanel;
  Label2.Parent := bgPanel;
  Label3.Parent := bgPanel;
  Label4.Parent := bgPanel;
  btnTwitter.Parent := bgPanel;
  btnOK := TXiButton.Create(Self);
  btnOK.Parent := bgPanel;
  btnOK.ColorScheme := csNeoGrass;
  btnOK.ModalResult := mrOK;
  btnOK.Top := 110;
  btnOK.Height := 25;
  btnOK.Width := 75;
  btnOK.Left := (frmAbout.ClientWidth div 2) - (btnOK.Width div 2);
  btnOK.Caption := 'OK';
end;

procedure TfrmAbout.btnTwitterClick(Sender: TObject);
begin
  OpenURL('http://twitter.com/hippy2094');
end;

procedure TfrmAbout.Label3Click(Sender: TObject);
begin
  OpenURL('http://www.matthewhipkin.co.uk');
end;

end.
