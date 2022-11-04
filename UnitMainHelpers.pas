(*
* The unit contains helper methods of UnitMain.
* Application: Purchase Register
* Creator: Laszlo Grimm
* From: 03-11-2022
*)
unit UnitMainHelpers;

interface

uses
    FMX.SearchBox,
    UnitMain, UnitItem;

//Sets enabled status of controls.
procedure SetControls(var formMain: TFormMain);
//Handles BCheckAll click and tap events.
procedure ButtonCheckAllEvents(var formMain: TFormMain);
//Handles BCheckNone click and tap events.
procedure ButtonCheckNoneEvents(var formMain: TFormMain);
//Handles BCheckInvert click and tap events.
procedure ButtonCheckInvertEvents(var formMain: TFormMain);
//Handles BDeleteChecked click and tap events.
//Handles BDeleteChecked click and tap events.
procedure ButtonDeleteCheckedEvents(var formMain: TFormMain; var searchBox: TSearchBox);
//Handles LVItems item selection events.
procedure LVItemSelectionEvents(var formMain: TFormMain);
//Handles BNew click and tap events.
procedure ButtonNewEvents(var formMain: TFormMain; var formItem: TFormItem; searchBox: TSearchBox);
//Handles BView click and tap, LVItems double click events.
procedure ButtonViewEvents(var formMain: TFormMain; formItem: TFormItem; searchBox: TSearchBox);
//Handles BStats click and tap events.
procedure ButtonStatsEvents(var formMain: TFormMain; searchBox: TSearchBox);
//Handles BReload click and tap events.
procedure ButtonReloadEvents(var formMain: TFormMain; searchBox: TSearchBox);
//Handles BSave click and tap events.
procedure ButtonSaveEvents(var formMain: TFormMain; searchBox: TSearchBox);
//Sets the positions and sizes of TFormMain controls.
procedure ResizeMainControls(var formMain: TFormMain);

implementation

uses
    System.SysUtils,
    UnitPurchaseRegister, UnitFileHandler, UnitItemHelpers;

(*
* Sets enabled status of controls.
*)
procedure SetControls(var formMain: TFormMain);
var
    checked: integer;
begin
with formMain do
    begin
    if statmode then
        begin
        BCheckAll.Enabled := true;
        BCheckNone.Enabled := true;
        BCheckInvert.Enabled := true;
        end
    else
        begin
        checked := LVItems.Items.CheckedCount(true);
        BCheckAll.Enabled := LVItems.ItemCount > checked;
        BCheckNone.Enabled := checked > 0;
        BCheckInvert.Enabled := LVItems.ItemCount > 0;
        BDeleteChecked.Enabled := BCheckNone.Enabled;
        BView.Enabled := checked = 1;
        BStats.Enabled := LVItems.ItemCount > 0;
        BSave.Enabled := notSaved;
        end;
    end;
end;

(*
* Handles BCheckAll click and tap events.
*)
procedure ButtonCheckAllEvents(var formMain: TFormMain);
begin
with formMain do
    begin
    if statMode then
        begin
        SetLabel(LMessage, 'Calculating statistics...', INFO);
        GenerateStatGrand(LVItems);
        SetLabel(LMessage, 'Grand data have been calculated.', INFO);
        end
    else
        begin
        LVItems.Items.CheckAll(true);
        SetControls(formMain);
        SetLabel(LMessage, IntToStr(LVItems.Items.CheckedCount(true)) + ' purchase(s) have been selected.', INFO);
        end;
    end;
end;

(*
* Handles BCheckNone click and tap events.
*)
procedure ButtonCheckNoneEvents(var formMain: TFormMain);
begin
with formMain do
    begin
    if statMode then
        begin
        SetLabel(LMessage, 'Calculating statistics...', INFO);
        GenerateStatYears(LVItems);
        SetLabel(LMessage, 'Data broken down to years have been calculated.', INFO);
        end
    else
        begin
        LVItems.Items.CheckAll(false);
        SetControls(formMain);
        SetLabel(LMessage, IntToStr(LVItems.Items.CheckedCount(true)) + ' purchase(s) have been selected.', INFO);
        end;
    end;
end;

(*
* Handles BCheckInvert click and tap events.
*)
procedure ButtonCheckInvertEvents(var formMain: TFormMain);
var
    itemIndex: integer;
begin
with formMain do
    begin
    if statMode then
        begin
        SetLabel(LMessage, 'Calculating statistics...', INFO);
        GenerateStatMonths(LVItems);
        SetLabel(LMessage, 'Data broken down to months have been calculated.', INFO);
        end
    else
        begin
        for itemIndex := 0 to LVItems.ItemCount - 1 do
            LVItems.Items[itemIndex].Checked := not LVItems.Items[itemIndex].Checked;
        SetControls(formMain);
        SetLabel(LMessage, IntToStr(LVItems.Items.CheckedCount(true)) + ' purchase(s) have been selected.', INFO);
        end;
    end;
end;

(*
* Handles BDeleteChecked click and tap events.
*)
procedure ButtonDeleteCheckedEvents(var formMain: TFormMain; var searchBox: TSearchBox);
var
    deletedCount, notDeletedCount: integer;
    itemIndex: integer;
begin
with formMain do
    begin
    searchBox.Text := '';
    deletedCount := 0;
    notDeletedCount := 0;
    itemIndex := pList.Count - 1;
    while itemIndex > -1 do
        begin
        if (LVItems.Items[itemIndex].Checked) then
            if pList.Delete(itemIndex) then
                deletedCount := deletedCount + 1
            else
                notDeletedCount := notDeletedCount + 1;
        itemIndex := itemIndex - 1;
        end;
    if notDeletedCount = 0 then
        SetLabel(LMessage, IntToStr(deletedCount) + ' purchase(s) have been deleted.', INFO)
    else
        SetLabel(LMessage, IntToStr(deletedCount) + ' purchase(s) have been, ' + IntToStr(notDeletedCount) + ' have NOT been deleted.', ERROR);
    PurchaseListToListView(LVItems);
    notSaved := true;
    end;
SetControls(formMain);
end;

(*
* Handles LVItems item selection events.
*)
procedure LVItemSelectionEvents(var formMain: TFormMain);
begin
with formMain do
    begin
    SetLabel(LMessage, IntToStr(LVItems.Items.CheckedCount(true)) + ' purchase(s) have been selected.', INFO);
    BView.Enabled := LVItems.ItemIndex > -1;
    end;
SetControls(formMain);
end;

(*
* Handles BNew click and tap events.
*)
procedure ButtonNewEvents(var formMain: TFormMain; var formItem: TFormItem; searchBox: TSearchBox);
begin
with formMain do
    begin
    searchBox.Text := '';
    PrepareFormItemControls(FormItem, -1, NEW);
    if formItem.ShowModal = 2 then //register
        begin
        if pList.Add(purchase) then
            begin
            AddToListView(LVItems, purchase);
            LVItems.ScrollTo(LVItems.ItemCount - 1);
            notSaved := true;
            SetControls(formMain);
            SetLabel(LMessage, 'The new purchase has been recorded.', INFO);
            end
        else
            SetLabel(LMessage, 'NO purchase has been recorded.', ERROR);
        end
    else //back
        SetLabel(LMessage, '', INFO);
    end;
end;

(*
* Handles BView click and tap, LVItems double click events.
*)
procedure ButtonViewEvents(var formMain: TFormMain; formItem: TFormItem; searchBox: TSearchBox);
var
    itemIndex: integer;
begin
with formMain do
    begin
    if LVItems.ItemIndex > -1 then
        begin
        searchBox.Text := '';
        PrepareFormItemControls(FormItem, LVItems.ItemIndex, MODIFY_DELETE);
        case formItem.ShowModal of
            3: //modify
                begin
                itemIndex := LVItems.ItemIndex;
                if pList.Modify(itemIndex, purchase) then
                    begin
                    ModifyInListView(LVItems, itemIndex, purchase);
                    notSaved := true;
                    SetControls(formMain);
                    SetLabel(LMessage, 'The purchase has been modified.', INFO);
                    end
                else
                    SetLabel(LMessage, 'NO purchase has been modified.', ERROR)
                end;
            4: //delete
                begin
                if pList.Delete(LVItems.ItemIndex) then
                    begin
                    DeleteFromListView(LVItems, LVItems.ItemIndex);
                    notSaved := true;
                    SetControls(formMain);
                    SetLabel(LMessage, 'The purchase has been deleted.', INFO);
                    end
                else
                    SetLabel(LMessage, 'NO purchase has been deleted.', ERROR)
                end;
            else //back
                SetLabel(LMessage, '', INFO);
            end;
        end;
    end;
end;

(*
* Handles BStats click and tap events.
*)
procedure ButtonStatsEvents(var formMain: TFormMain; searchBox: TSearchBox);
begin
with formMain do
    begin
    searchBox.Text := '';
    SetLabel(LMessage, '', INFO);
    statMode := not statMode;
    if statMode then
        begin
        BCheckAll.Text := 'Grand';
        BCheckNone.Text := 'Years';
        BCheckInvert.Text := 'Months';
        BStats.Text := 'Back';
        BDeleteChecked.Visible := false;
        BNew.Visible := false;
        BView.Visible := false;
        BReload.Visible := false;
        BSave.Visible := false;
        LVItems.Items.Clear;
        LVItems.EditMode := false;
        LVItems.SearchVisible := false;
        end
    else
        begin
        BCheckAll.Text := 'All';
        BCheckNone.Text := 'None';
        BCheckInvert.Text := 'Invert';
        BStats.Text := 'Statistics';
        BDeleteChecked.Visible := true;
        BNew.Visible := true;
        BView.Visible := true;
        BReload.Visible := true;
        BSave.Visible := true;
        LVItems.EditMode := true;
        LVItems.SearchVisible := true;
        PurchaseListToListView(LVItems);
        end;
    end;
SetControls(formMain);
end;

(*
* Handles BReload click and tap events.
*)
procedure ButtonReloadEvents(var formMain: TFormMain; searchBox: TSearchBox);
begin
with formMain do
    begin
    searchBox.Text := '';
    SetLabel(LMessage, 'Loading data...', INFO);
    if (isExistsData or (not IsExistsData and CreateData)) and ReadData(pList) then
        begin
        PurchaseListToListView(LVItems);
        notSaved := false;
        SetLabel(LMessage, 'Purchase data has been (re)loaded.', INFO);
        end
    else
        SetLabel(LMessage, 'Access to data on storage is NOT possible.', ERROR);
    end;
SetControls(formMain);
end;

(*
* Handles BSave click and tap events.
*)
procedure ButtonSaveEvents(var formMain: TFormMain; searchBox: TSearchBox);
begin
with formMain do
    begin
    searchBox.Text := '';
    if WriteData(pList) then
        begin
        notSaved := false;
        SetLabel(LMessage, 'The purchase data have been saved.', INFO);
        end
    else
        SetLabel(LMessage, 'The purchase data have NOT been saved.', ERROR);
    end;
SetControls(formMain);
end;

(*
* Sets the positions and sizes of TFormMain controls.
*)
procedure ResizeMainControls(var formMain: TFormMain);
const
    designedLVItemsHeight = 220;
    designedFirstButtonRowY = 290;
    designedSecondButtonRowY = 330;
    designedThirdButtonRowY = 370;
var
    differenceInLVItemsHeight: integer;
    buttonWidth: integer;
begin
with formMain do
    begin
    buttonWidth := Trunc((formMain.ClientWidth - (4 - 1) * 5 - 20) / 4);

    BCheckAll.Width := buttonWidth;
    BCheckNone.Width := buttonWidth;
    BCheckInvert.Width := buttonWidth;
    BDeleteChecked.Width := buttonWidth;
    BReload.Width := buttonWidth;
    BNew.Width := buttonWidth;
    BView.Width := buttonWidth;
    BStats.Width := buttonWidth;
    BSave.Width := buttonWidth;

    BCheckNone.Position.X := 10 + buttonWidth + 5;
    BView.Position.X := BCheckNone.Position.X;
    BCheckInvert.Position.X := BCheckNone.Position.X + buttonWidth + 5;
    BStats.Position.X := BCheckInvert.Position.X;
    BDeleteChecked.Position.X := BCheckInvert.Position.X + buttonWidth + 5;
    BReload.Position.X := BDeleteChecked.Position.X;
    BSave.Position.X := BDeleteChecked.Position.X;
    LMessage.Width := BReload.Position.X - 10 - 5;

    differenceInLVItemsHeight := Trunc(LVItems.Height - designedLVItemsHeight);

    BCheckAll.Position.Y := designedFirstButtonRowY + differenceInLVItemsHeight;
    BCheckNone.Position.Y := BCheckAll.Position.Y;
    BCheckInvert.Position.Y := BCheckAll.Position.Y;
    BDeleteChecked.Position.Y := BCheckAll.Position.Y;

    LMessage.Position.Y := designedSecondButtonRowY + differenceInLVItemsHeight;
    BReload.Position.Y := LMessage.Position.Y;

    BNew.Position.Y := designedThirdButtonRowY + differenceInLVItemsHeight;
    BView.Position.Y := BNew.Position.Y;
    BStats.Position.Y := BNew.Position.Y;
    BSave.Position.Y := BNew.Position.Y;
    end;
end;

end.
