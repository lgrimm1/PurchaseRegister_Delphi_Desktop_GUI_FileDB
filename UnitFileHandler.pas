(*
* The unit contains helper methods for file handling processes.
* Application: Purchase Register
* Creator: Laszlo Grimm
* From: 03-11-2022
*)
unit UnitFileHandler;

interface

{
uses
    FMX.StdCtrls, FMX.Forms, FMX.ListView, FMX.SearchBox,
    System.Classes;
}

uses
    UnitPurchaseList;

//Reads data from storage to TPurchaseList.
function ReadData(var oldPList: TPurchaseList): boolean;
//Writes data to storage from TPurchaseList.
function WriteData(var oldPList: TPurchaseList): boolean;
//Creates an empty file on storage.
function CreateData: boolean;
//Checks existence of file on storage.
function IsExistsData: boolean;

implementation

uses
    System.SysUtils, System.DateUtils, System.Classes, System.Math,
    UnitPurchase, UnitPurchaseRegister;

(*
* Returns the TPurchase fields in a text, locale-independently.
* Helper method of WriteData().
* Data will be concatenated with | separator character.
* For date, components, for float number, whole and fraction parts will be processed independently.
*)
function PurchaseToString(purchase: TPurchase): string;
begin
with purchase do
Result :=
    IntToStr(YearOf(GetPurchaseDate)) + '|' +
    IntToStr(MonthOf(GetPurchaseDate)) + '|' +
    IntToStr(DayOf(GetPurchaseDate)) + '|' +
    GetPurchaseTypeString + '|' +
    IntToStr(Trunc(GetPurchaseValue)) + '|' +
    IntToStr(Trunc(Frac(GetPurchaseValue) * 100)) + '|' +
    MemoTextToText(GetPurchaseDescription, '|');
end;

(*
* Converts text to TPurchaseType.
* Helper method of StringToPurchase().
* In case the given text is not proper, returns false.
*)
function TextToPurchaseType(purchaseText: string; var purchaseType: TPurchaseType): boolean;
begin
Result := true;
if purchaseText = 'CARD' then
    purchaseType := CARD
else
    if purchaseText = 'CASH' then
        purchaseType := CASH
    else
        if purchaseText = 'INET' then
            purchaseType := INTERNET
        else
            Result := false;
end;

(*
* Converts the text to a TPurchase, locale-independently.
* Helper method of ReadData().
* In the text, data is separated with | separator character.
* In the text for date, components, for float number, whole and decimal parts should be separated.
* In case the given text is not proper, returns nil.
*)
function StringToPurchase(purchaseAsText: String; var newPurchase: TPurchase): boolean;
var
    ok: boolean;
    year, month, day: integer;
    pType: TPurchaseType;
    value: double;
    valueFractionPart: integer;
    description: TStringList;
    p: integer;
begin
ok := true;
description := TStringList.Create;

p := Pos('|', purchaseAsText);
ok := (p > 0) and TryStrToInt(Copy(purchaseAsText, 1, p - 1), year);

if ok then
    begin
    Delete(purchaseAsText, 1, p);
    p := Pos('|', purchaseAsText);
    ok := (p > 0) and TryStrToInt(Copy(purchaseAsText, 1, p - 1), month);
    end;

if ok then
    begin
    Delete(purchaseAsText, 1, p);
    p := Pos('|', purchaseAsText);
    ok := (p > 0) and TryStrToInt(Copy(purchaseAsText, 1, p - 1), day);
    end;

if ok then
    ok := IsValidDate(year, month, day);

if ok then
    begin
    Delete(purchaseAsText, 1, p);
    p := Pos('|', purchaseAsText);
    ok := (p > 0) and TextToPurchaseType(Copy(purchaseAsText, 1, p - 1), pType);
    end;

if ok then
    begin
    Delete(purchaseAsText, 1, p);
    p := Pos('|', purchaseAsText);
    ok := (p > 0) and TryStrToFloat(Copy(purchaseAsText, 1, p - 1), value);
    end;

if ok then
    begin
    Delete(purchaseAsText, 1, p);
    p := Pos('|', purchaseAsText);
    ok := (p > 0) and TryStrToInt(Copy(purchaseAsText, 1, p - 1), valueFractionPart);
    if ok then
        value := value + valueFractionPart / Power(10, p - 1);
    end;

if ok then
    begin
    Delete(purchaseAsText, 1, p);
    p := Pos(' | ', purchaseAsText);
    while p > 0 do
        begin
        description.Add(Copy(purchaseAsText, 1, p - 1));
        Delete(purchaseAsText, 1, p);
        p := Pos(' | ', purchaseAsText);
        end;
    if not purchaseAsText.IsEmpty then
        description.Add(purchaseAsText);
    end;

if ok then
    begin
    newPurchase := TPurchase.CreateDateComponents(year, month, day, ptype, value, description.Text);
    end;
description.Free;
Result := ok;
end;

(*
* Reads data from storage to TPurchaseList.
*)
function ReadData(var oldPList: TPurchaseList): boolean;
var
    data: TStringList;
    listIndex: integer;
    newPList: TPurchaseList;
    newPurchase: TPurchase;
begin
data := TStringList.Create;
try
    data.LoadFromFile(dataFile);
    Result := true;
    except
    Result := false;
    end;
if Result then
    begin
    newPList := TPurchaseList.Create;
    listIndex := 0;
    while (listIndex < data.Count) and Result do
        begin
        Result := StringToPurchase(data[listIndex], newPurchase);
        if Result then
            Result := newPList.Add(newPurchase);
        listIndex := listIndex + 1;
        end;
    if Result then
        begin
        newPList.Sort;
        oldPList.Free;
        oldPList := newPList;
        end
    else
        newPlist.Free;
    end;
data.Free;
end;

(*
* Writes data to storage from TPurchaseList.
*)
function WriteData(var oldPList: TPurchaseList): boolean;
var
    data: TStringList;
    listIndex: integer;
begin
data := TStringList.Create;
for listIndex := 0 to oldPList.Count - 1 do
    data.Add(PurchaseToString(oldPList.Get(listIndex)));
try
    data.SaveToFile(dataFile);
    Result := true;
    except
    Result := false;
    end;
data.Free;
end;

(*
* Creates an empty file on storage.
*)
function CreateData: boolean;
var
    data: TStringList;
begin
data := TStringList.Create;
try
    data.SaveToFile(dataFile);
    Result := true;
    except
    Result := false;
    end;
data.Free;
end;

(*
* Checks existence of file on storage.
*)
function IsExistsData: boolean;
begin
Result := FileExists(dataFile);
end;

end.
