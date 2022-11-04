(*
* The unit represents a form for a purchase data.
* Application: Purchase Register
* Creator: Laszlo Grimm
* From: 04-11-2022
*)
unit UnitItem;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
  FMX.ListBox, FMX.DateTimeCtrls;

type
  TFormItem = class(TForm)
    GBPaymentMethod: TGroupBox;
    RBCard: TRadioButton;
    RBCash: TRadioButton;
    RBInternet: TRadioButton;
    MDescription: TMemo;
    EDay: TEdit;
    GBDescription: TGroupBox;
    GBDate: TGroupBox;
    EYear: TEdit;
    GBValue: TGroupBox;
    EValue: TEdit;
    BRegister: TButton;
    BBack: TButton;
    LMessage: TLabel;
    BModify: TButton;
    BDelete: TButton;
    CBMonth: TComboBox;
    procedure BBackClick(Sender: TObject);
    procedure BBackTap(Sender: TObject; const Point: TPointF);
    procedure BRegisterClick(Sender: TObject);
    procedure EYearExit(Sender: TObject);
    procedure EDayExit(Sender: TObject);
    procedure EValueExit(Sender: TObject);
    procedure BRegisterTap(Sender: TObject; const Point: TPointF);
    procedure BModifyClick(Sender: TObject);
    procedure BModifyTap(Sender: TObject; const Point: TPointF);
    procedure BDeleteClick(Sender: TObject);
    procedure BDeleteTap(Sender: TObject; const Point: TPointF);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormItem: TFormItem;

implementation

{$R *.fmx}

uses
    System.DateUtils,
    UnitPurchase, UnitPurchaseRegister, UnitItemHelpers;

procedure TFormItem.BBackClick(Sender: TObject);
begin
ButtonBackEvents(FormItem);
end;

procedure TFormItem.BBackTap(Sender: TObject; const Point: TPointF);
begin
ButtonBackEvents(FormItem);
end;

procedure TFormItem.BDeleteClick(Sender: TObject);
begin
ButtonDeleteEvents(FormItem);
end;

procedure TFormItem.BDeleteTap(Sender: TObject; const Point: TPointF);
begin
ButtonDeleteEvents(FormItem);
end;

procedure TFormItem.BModifyClick(Sender: TObject);
begin
ButtonModifyEvents(FormItem);
end;

procedure TFormItem.BModifyTap(Sender: TObject; const Point: TPointF);
begin
ButtonModifyEvents(FormItem);
end;

procedure TFormItem.BRegisterClick(Sender: TObject);
begin
ButtonRegisterEvents(FormItem);
end;

procedure TFormItem.BRegisterTap(Sender: TObject; const Point: TPointF);
begin
ButtonRegisterEvents(FormItem);
end;

procedure TFormItem.EDayExit(Sender: TObject);
begin
FormItem.EDay.Text := CorrectIntText(FormItem.EDay.Text, 2);
end;

procedure TFormItem.EValueExit(Sender: TObject);
begin
FormItem.EValue.Text := CorrectFloatText(FormItem.EValue.Text, 2);
end;

procedure TFormItem.EYearExit(Sender: TObject);
begin
FormItem.EYear.Text := CorrectIntText(FormItem.EYear.Text, 4);
end;

procedure TFormItem.FormResize(Sender: TObject);
begin
if resizePossible then
    ResizeItemControls(FormItem);
end;

procedure TFormItem.FormShow(Sender: TObject);
begin
MDescription.GoToTextBegin;
EDay.SetFocus;
end;

end.
