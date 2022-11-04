(*
* The unit contains helper methods.
* Application: Purchase Register
* Creator: Laszlo Grimm
* From: 03-11-2022
*)
unit UnitPurchaseRegister;

interface

uses
    System.SysUtils, FMX.StdCtrls, FMX.Forms, FMX.ListView, FMX.SearchBox,
    System.Classes,
    UnitMain, UnitItem, UnitPurchase, UnitPurchaseList, UnitStat, UnitStatList;

type
    TMessageType = (INFO, ERROR);
    TStatType = (GRAND, YEAR, MONTH);

var
    formatSettingsEng: TFormatSettings;
    pList: TPurchaseList;
    purchase: TPurchase;
    pListIndex: integer;
    dataFile: string;
    notSaved: boolean;
    statMode: boolean;
    sList: TStatList;
    resizePossible: boolean = false;

//Initialization of environment, loading data.
procedure initIt(var formMain: TFormMain; var searchBox: TSearchBox);
//Final moments of environment.
procedure finalIt(var messageLabel: TLabel);
//Sets text and font color of a TLabel.
procedure SetLabel(var messageLabel: TLabel; messageText: string; messageType: TMessageType);
//Converts TMemo.text to a single string.
function MemoTextToText(linesInText, lineSeparator: string): string;
//Projects TPurchaseList to TListView.
function PurchaseListToListView(var listView: TListView): boolean;
//Adds element to TListView.
function AddToListView(var listView: TListView; newPurchase: TPurchase): boolean;
//Modifies element in TListView at index.
function ModifyInListView(var listView: TListView; itemIndex: integer; newPurchase: TPurchase): boolean;
//Deletes element from TListView.
function DeleteFromListView(var listView: TListView; itemIndex: integer): boolean;
//Generates absolute total statistics and fill the given TListView.
procedure GenerateStatGrand(var listView: TListView);
//Generates yearly statistics and fill the given TListView.
procedure GenerateStatYears(var listView: TListView);
//Generates monthly statistics and fill the given TListView.
procedure GenerateStatMonths(var listView: TListView);

implementation

uses
    System.Generics.Collections, System.DateUtils, System.UIConsts,
    FMX.ListView.Types,
    UnitFileHandler;

const
    dataFileName = 'PRData.dat';

var
    exePath: string;

(*
* Initialization of environment, loading data.
*)
procedure initIt(var formMain: TFormMain; var searchBox: TSearchBox);
var
    controlIndex: integer;
begin
with formMain do
    begin
    exePath := ExtractFilePath(ParamStr(0));
    dataFile := exePath + dataFileName;
    formatSettingsEng := TFormatSettings.Create;
    formatSettingsEng.DateSeparator := '-';
    formatSettingsEng.ShortDateFormat := 'yyyy-mm-dd';
    pList := TPurchaseList.Create;
    SetLabel(LMessage, 'Loading data...', INFO);
    if (isExistsData or (not IsExistsData and CreateData)) and ReadData(pList) then
        begin
        PurchaseListToListView(LVItems);
        controlIndex := 0;
        while (controlIndex < LVItems.ControlsCount) and not (LVItems.Controls[controlIndex] is TSearchBox) do
            controlIndex := controlIndex + 1;
        searchBox := TSearchBox(LVItems.Controls[controlIndex]);
        notSaved := false;
        statMode := false;
        sList := TStatList.Create;
        SetLabel(LMessage, 'Welcome!', INFO);
        end
    else
        SetLabel(LMessage, 'Access to data on storage is NOT possible.', ERROR);
    end;
end;

(*
* Final moments of environment.
*)
procedure finalIt(var messageLabel: TLabel);
begin
SetLabel(messageLabel, 'Good bye!', INFO);
pList.Free;
sList.Free;
end;

(*
* Sets text and font color of a TLabel.
* In case of wrong parameters e.g. a nil is passed as argument, no changes will apply.
*)
procedure SetLabel(var messageLabel: TLabel; messageText: string; messageType: TMessageType);
begin
if messageLabel <> nil then
    begin
    messageLabel.Text := messageText;
    case messageType of
        INFO: messageLabel.TextSettings.FontColor := claBlue;
        ERROR: messageLabel.TextSettings.FontColor := claRed;
        end;
    end;
end;

(*
* Converts TMemo.text to a single string.
* Uses | as line separator.
* In case the original text is nil or empty, returns empty string.
*)
function MemoTextToText(linesInText, lineSeparator: string): string;
var
    memoIndex: integer;
    memo: TStringList;
begin
if (linesInText.IsEmpty) then
    Result := ''
else
    begin
    memo := TStringList.Create;
    memo.Text := linesInText;
    Result := memo[0];
    for memoIndex := 1 to memo.Count - 1 do
        Result := Result + lineSeparator + memo[memoIndex];
    memo.Free;
    end;
end;

(*
* Fills TListViewItem with purchase data.
* Helper method of PurchaseListToListView, AddToListView(), ModifyInListView().
*)
procedure FillItem(var listItem: TListViewItem; purchase: TPurchase);
begin
listItem.Text := DateToStr(purchase.GetPurchaseDate, formatSettingsEng) + '  ' + purchase.GetPurchaseTypeString + '  ' + Format('%.2f', [purchase.GetPurchaseValue]);
listItem.Detail := MemoTextToText(purchase.GetPurchaseDescription, ' | ');
end;

(*
* Projects TPurchaseList to TListView.
* In case the argument is nil, returns false.
*)
function PurchaseListToListView(var listView: TListView): boolean;
var
    itemIndex: integer;
    listItem: TListViewItem;
    purchase: TPurchase;
begin
if listView = nil then
    Result := false
else
    begin
    listView.BeginUpdate;
    listView.Items.Clear;
    itemIndex := 0;
    while itemIndex < pList.Count do
        begin
        purchase := pList.Get(itemIndex);
        listItem := listView.Items.Add;
        FillItem(listItem, purchase);
        itemIndex := itemIndex + 1;
        end;
    (*
    if pList.Count > 0 then
        for itemIndex := 0 to pList.Count do
            begin
            purchase := pList.Get(itemIndex);
            listItem := listView.Items.Add;
            FillItem(listItem, purchase);
            end;
    *)
    listView.EndUpdate;
    Result := true;
    end;
end;

(*
* Adds element to TListView.
* In case an argument is nil, returns false.
*)
function AddToListView(var listView: TListView; newPurchase: TPurchase): boolean;
var
    listItem: TListViewItem;
begin
if (listView = nil) or (newPurchase = nil) then
    Result := false
else
    begin
    listView.BeginUpdate;
    listItem := listView.Items.Add;
    FillItem(listItem, newPurchase);
    listView.EndUpdate;
    Result := true;
    end;
end;

(*
* Modifies element in TListView at index.
* In case the given TListView or TPurchase are nil or the index is wrong, will skip.
*)
function ModifyInListView(var listView: TListView; itemIndex: integer; newPurchase: TPurchase): boolean;
var
    listItem: TListViewItem;
begin
if (listView = nil) or (itemIndex < 0) or (itemIndex >= listView.ItemCount) or (newPurchase = nil) then
    Result := false
else
    begin
    listView.BeginUpdate;
    listView.Items.Delete(itemIndex);
    listItem := listView.Items.Insert(itemIndex);
    FillItem(listItem, newPurchase);
    listView.EndUpdate;
    Result := true;
    end;
end;

(*
* Deletes element from TListView.
* In case the given TListView is nil or the index is wrong, will skip.
*)
function DeleteFromListView(var listView: TListView; itemIndex: integer): boolean;
begin
if (listView = nil) or (itemIndex < 0) or (itemIndex >= listView.ItemCount) then
    Result := false
else
    begin
    listView.BeginUpdate;
    listView.Items.Delete(itemIndex);
    listView.EndUpdate;
    Result := true;
    end;
end;

(*
* Fills TListView with stat data.
* Helper method of GenerateStatTotal(), GenerateStatYears(), GenerateStatMonths().
* In case the argument is nil, skips.
*)
procedure FillStat(var listView: TListView; statType: TStatType);
var
    itemIndex: integer;
    listItem: TListViewItem;
    stat: TStat;
begin
if (listView <> nil) then
    begin
    listView.BeginUpdate;
    listView.Items.Clear;
    if sList.Count > 0 then
        begin
        for itemIndex := 0 to sList.Count - 1 do
            begin
            stat := sList.Get(itemIndex);
            listItem := listView.Items.Add;
            case statType of
                GRAND: listItem.Text := 'GRAND';
                YEAR: listItem.Text := Format('%.4d', [YearOf(stat.GetDate)]);
                MONTH: listItem.Text := Format('%.4d', [YearOf(stat.GetDate)]) + '-' + Format('%.2d', [MonthOf(stat.GetDate)]);
                end;
            listItem.Detail := 'Total: ' + Format('%.2f', [stat.GetTotal]) + '  Avg: ' + Format('%.2f', [stat.GetAverage]) + '  Count: ' + IntToStr(stat.GetCount);
            end;
        end;
    listView.EndUpdate;
    end;
end;

(*
* Generates absolute total statistics and fill the given TListView.
* In case the argument is nil, results an empty TListView.
*)
procedure GenerateStatGrand(var listView: TListView);
var
    itemIndex: integer;
    actualPurchase: TPurchase;
begin
sList.Clear;
if pList.Count > 0 then
    for itemIndex := 0 to pList.Count - 1 do
        begin
        actualPurchase := pList.Get(itemIndex);
        sList.Put(TStat.Create(1, actualPurchase.GetPurchaseValue));
        end;
FillStat(listView, GRAND);
end;

(*
* Generates yearly statistics and fill the given TListView.
* In case the argument is nil, results an empty TListView.
*)
procedure GenerateStatYears(var listView: TListView);
var
    itemIndex: integer;
    actualPurchase: TPurchase;
    actualDate: TDate;
begin
sList.Clear;
if pList.Count > 0 then
    for itemIndex := 0 to pList.Count - 1 do
        begin
        actualPurchase := pList.Get(itemIndex);
        actualDate := actualPurchase.GetPurchaseDate;
        sList.Put(TStat.Create(EncodeDate(YearOf(actualDate), 1, 1), actualPurchase.GetPurchaseValue));
        end;
FillStat(listView, YEAR);
end;

(*
* Generates monthly statistics and fill the given TListView.
* In case the argument is nil, results an empty TListView.
*)
procedure GenerateStatMonths(var listView: TListView);
var
    itemIndex: integer;
    actualPurchase: TPurchase;
    actualDate: TDate;
begin
sList.Clear;
if pList.Count > 0 then
    for itemIndex := 0 to pList.Count - 1 do
        begin
        actualPurchase := pList.Get(itemIndex);
        actualDate := actualPurchase.GetPurchaseDate;
        sList.Put(TStat.Create(EncodeDate(YearOf(actualDate), MonthOf(actualDate), 1), actualPurchase.GetPurchaseValue));
        end;
FillStat(listView, MONTH);
end;

end.
