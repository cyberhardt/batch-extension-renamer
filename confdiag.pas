unit confdiag;

{$mode objfpc}{$H+}
(*
  Batch file extension renamer
    confdiag.pas - confirmation dialog unit
    (c)2011-2015 Matthew Hipkin <http://www.matthewhipkin.co.uk>
*)
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, XiPanel, XiButton;

type

  { TfrmConfirm }

  TfrmConfirm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    bgPanel: TXiPanel;
    btnOK: TXiButton;
    btnCancel: TXiButton;
  public
    { public declarations }
  end;

var
  frmConfirm: TfrmConfirm;

implementation

{$R *.lfm}

{ TfrmConfirm }

procedure TfrmConfirm.FormCreate(Sender: TObject);
begin
  bgPanel := TXiPanel.Create(Self);
  bgPanel.Parent := frmConfirm;
  bgPanel.Align := alClient;
  bgPanel.BorderStyle := bsNone;
  bgPanel.BevelInner:=bvNone;
  bgPanel.BevelOuter := bvNone;
  bgPanel.ColorScheme := XiPanel.csSky;
  btnOK := TXiButton.Create(Self);
  btnOK.Parent := bgPanel;
  btnOK.Width := 75;
  btnOK.Height := 25;
  btnOK.ColorScheme := csNeoGrass;
  btnOK.Caption := 'OK';
  btnOK.Top := (frmConfirm.ClientHeight - btnOK.Height) - 10;
  btnOK.Left := (frmConfirm.ClientWidth - (btnOK.Width * 2)) - 20;
  btnOK.ModalResult := mrOK;
  btnCancel := TXiButton.Create(Self);
  btnCancel.Parent := bgPanel;
  btnCancel.Width := 75;
  btnCancel.Height := 25;
  btnCancel.ColorScheme := csNeoRose;
  btnCancel.Caption := 'Cancel';
  btnCancel.Top := (frmConfirm.ClientHeight - btnCancel.Height) - 10;
  btnCancel.Left := (frmConfirm.ClientWidth - btnCancel.Width) - 10;
  btnCancel.ModalResult := mrCancel;
  Label1.Parent := bgPanel;
  Image1.Parent := bgPanel;
end;

procedure TfrmConfirm.FormDestroy(Sender: TObject);
begin
  btnOK.Free;
  btnCancel.Free;
  bgPanel.Free;
end;

end.
