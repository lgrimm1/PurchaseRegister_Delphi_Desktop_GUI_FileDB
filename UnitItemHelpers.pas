(*
* The unit contains helper methods of UnitItem.
* Application: Purchase Register
* Creator: Laszlo Grimm
* From: 04-11-2022
*)
unit UnitItemHelpers;

interface

uses
    UnitItem, UnitPurchase;

type
    TItemFunctionType = (NEW, MODIFY_DELETE);

//Handles BBack click and tap events.
procedure ButtonBackEvents(var formItem: TFormItem);
//Handles BRegister click and tap events.
procedure ButtonRegisterEvents(var formItem: TFormItem);
//Handles BModify click and tap events.
procedure ButtonModifyEvents(var formItem: TFormItem);
//Handles BDelete click and tap events.
procedure ButtonDeleteEvents(var formItem: TFormItem);
//Formats the original integer text with trailing zeros.
function CorrectIntText(originalText: string; neededWidth: integer): string;
//Formats the original float text by decimal precision.
function CorrectFloatText(originalText: string; neededDecimals: integer): string;
//Defines purchase type based upon check status of purchase type radio buttons.
function RadiosToPurchaseType(cardChecked, cashChecked, internetchecked: boolean): TPurchaseType;
//Prepares FormItem controls based upon its needed function.
procedure PrepareFormItemControls(var formItem: TFormItem; listIndex: integer; itemFunction: TItemFunctionType);
//Checks the data for errors and returns an error message or an empty string.
function CheckParameters(yearText: string; month: integer; dayText: string; cardIsChecked, cashIsChecked, internetIsChecked: boolean; valueText: string): string;
//Prepares the purchase item based upon form controls.
function PrepareItem(var formItem: TFormItem): boolean;
//Sets the positions and sizes of TFormItem controls.
procedure ResizeItemControls(var formItem: TFormItem);


implementation

uses
    System.SysUtils, System.DateUtils,
    UnitPurchaseRegister;

(*
* Handles BBack click and tap events.
*)
procedure ButtonBackEvents(var formItem: TFormItem);
begin
FormItem.ModalResult := 1;
end;

(*
* Handles BRegister click and tap events.
*)
procedure ButtonRegisterEvents(var formItem: TFormItem);
begin
if PrepareItem(formItem) then
    FormItem.ModalResult := 2;
end;

(*
* Handles BModify click and tap events.
*)
procedure ButtonModifyEvents(var formItem: TFormItem);
begin
if PrepareItem(formItem) then
    FormItem.ModalResult := 3;
end;

(*
* Handles BDelete click and tap events.
*)
procedure ButtonDeleteEvents(var formItem: TFormItem);
begin
FormItem.ModalResult := 4;
end;

(*
* Formats the original integer text with trailing zeros.
* In case the given text is empty or the length of it greater than the given width or the text is not valid as an integer number, returns an empty string.
*)
function CorrectIntText(originalText: string; neededWidth: integer): string;
var
    number: integer;
begin
if not originalText.isEmpty and (neededWidth >= originalText.Length) and TryStrToInt(originalText, number) then
    Result := Format('%.' + IntToStr(neededWidth) + 'd', [number])
else
    Result := '';
end;

(*
* Formats the original float text by decimal precision.
* Uses global TFormatSettings for the float number conversion.
* In case the given text is empty or not valid as a float number along local settings, returns an empty string.
*)
function CorrectFloatText(originalText: string; neededDecimals: integer): string;
var
    number: double;
begin
if not originalText.isEmpty and TryStrToFloat(originalText, number, formatSettingsEng) then
    Result := Format('%.' + IntToStr(neededDecimals) + 'f', [number])
else
    Result := '';
end;

(*
* Defines purchase type based upon check status of purchase type radio buttons.
* In case none of the given values is true, returns CARD as default.
*)
function RadiosToPurchaseType(cardChecked, cashChecked, internetchecked: boolean): TPurchaseType;
begin
if cardChecked then
    Result := CARD
else
    if cashChecked then
        Result := CASH
    else
        if internetChecked then
            Result := INTERNET
        else
            Result := CARD;
end;

(*
* Prepares FormItem controls based upon its needed function.
*)
procedure PrepareFormItemControls(var formItem: TFormItem; listIndex: integer; itemFunction: TItemFunctionType);
begin
with formItem do
    case itemFunction of
        NEW:
            begin
            pListIndex := -1;
            EDay.Text := '';
            CBMonth.ItemIndex := 0;
            EYear.Text := '';
            RBCard.IsChecked := true;
            EValue.Text := '';
            MDescription.Text := '';
            BRegister.Visible := true;
            BModify.Visible := false;
            BDelete.Visible := false;
            SetLabel(LMessage, '', INFO);
            end;
        MODIFY_DELETE:
            begin
            pListIndex := listIndex;
            purchase := pList.Get(pListIndex);
            EDay.Text := Format('%.2d', [DayOf(purchase.GetPurchaseDate)]);
            CBMonth.ItemIndex := MonthOf(purchase.GetPurchaseDate) - 1;
            EYear.Text := Format('%.4d', [YearOf(purchase.GetPurchaseDate)]);;
            RBCard.IsChecked := purchase.GetPurchaseType = CARD;
            RBCash.IsChecked := purchase.GetPurchaseType = CASH;
            RBInternet.IsChecked := purchase.GetPurchaseType = INTERNET;
            EValue.Text := Format('%.2f', [purchase.GetPurchaseValue]);
            MDescription.Text := purchase.GetPurchaseDescription;
            BRegister.Visible := false;
            BModify.Visible := true;
            BDelete.Visible := true;
            SetLabel(formItem.LMessage, '', INFO);
            end;
        end;
end;

(*
* Checks the data for errors and returns an error message or an empty string.
*)
function CheckParameters(yearText: string; month: integer; dayText: string; cardIsChecked, cashIsChecked, internetIsChecked: boolean; valueText: string): string;
var
    value: double;
    returnMessage: string;
    ok: boolean;
begin
returnMessage := '';
ok := not yearText.IsEmpty and (month > 0) and not dayText.IsEmpty and
    isValidDate(StrToInt(yearText), month, StrToInt(dayText));
if not ok then
    returnMessage := 'Wrong or no number for YEAR or DAY, or no MONTH was selected.';

if ok then
    if not (cardIsChecked or cashIsChecked or internetIsChecked) then
        begin
        returnMessage := 'No selected PAYMENT METHOD';
        ok := false;
        end;

if ok then
    if valueText.IsEmpty or not TryStrToFloat(valueText, value, formatSettingsEng) then
        returnMessage := 'Not defined VALUE OF PURCHASE.';

Result := returnMessage;
end;

(*
* Prepares the purchase item based upon form controls.
*)
function PrepareItem(var formItem: TFormItem): boolean;
var
    purchaseType: TPurchaseType;
    messageText: string;
begin
with FormItem do
    begin
    messageText := CheckParameters(formItem.EYear.Text, formItem.CBMonth.ItemIndex + 1, formItem.EDay.Text, formItem.RBCard.IsChecked, formItem.RBCash.IsChecked, formItem.RBInternet.IsChecked, formItem.EValue.Text);
    if messageText.IsEmpty then
        begin
        SetLabel(formItem.LMessage, '', INFO);
        purchaseType := RadiosToPurchaseType(formItem.RBCard.IsChecked, formItem.RBCash.IsChecked, formItem.RBInternet.IsChecked);
        purchase := TPurchase.CreateDateComponents(StrToInt(formItem.EYear.Text), formItem.CBMonth.ItemIndex + 1, StrToInt(formItem.EDay.Text), purchaseType, StrToFloat(formItem.EValue.Text), formItem.MDescription.Text);
        Result := true;
        end
    else
        begin
        SetLabel(formItem.LMessage, messageText, ERROR);
        Result := false;
        end;
    end;
end;

(*
* Sets the positions and sizes of TFormItem controls.
*)
procedure ResizeItemControls(var formItem: TFormItem);
const
    designedGBDescriptionHeight = 90;
    designedButtonRowY = 390;
var
    differenceInGBDescriptionHeight: integer;
    buttonRegisterWidth, buttonOtherWidth: integer;
begin
with formItem do
    begin
    buttonOtherWidth := Trunc((MDescription.Width - 20) / 3);
    buttonRegisterWidth := 2 * buttonOtherWidth + 10;

    BModify.Width := buttonOtherWidth;
    BDelete.Width := buttonOtherWidth;
    BBack.Width := buttonOtherWidth;
    BRegister.Width := buttonRegisterWidth;

    BDelete.Position.X := BModify.Position.X + buttonOtherWidth + 10;
    BBack.Position.X := BDelete.Position.X + buttonOtherWidth + 10;

    differenceInGBDescriptionHeight := Trunc(GBDescription.Height - designedGBDescriptionHeight);

    BModify.Position.Y := designedButtonRowY + differenceInGBDescriptionHeight;
    BDelete.Position.Y := BModify.Position.Y;
    BBack.Position.Y := BModify.Position.Y;
    BRegister.Position.Y := BModify.Position.Y;

    LMessage.Position.Y := GBDescription.Position.Y + GBDescription.Height + 5;
    end;
end;
end.
