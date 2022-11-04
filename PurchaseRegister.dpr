program PurchaseRegister;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMain in 'UnitMain.pas' {FormMain},
  UnitItem in 'UnitItem.pas' {FormItem},
  UnitPurchase in 'UnitPurchase.pas',
  UnitPurchaseRegister in 'UnitPurchaseRegister.pas',
  UnitPurchaseList in 'UnitPurchaseList.pas',
  UnitFileHandler in 'UnitFileHandler.pas',
  UnitStat in 'UnitStat.pas',
  UnitStatList in 'UnitStatList.pas',
  UnitMainHelpers in 'UnitMainHelpers.pas',
  UnitItemHelpers in 'UnitItemHelpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormItem, FormItem);
  Application.Run;
end.
