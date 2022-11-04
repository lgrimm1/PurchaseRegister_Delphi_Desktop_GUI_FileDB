(*
* The unit contains the TPurchaseList class which serves as type of a purchase list which has TPurchase elements.
* Application: Purchase Register
* Creator: Laszlo Grimm
* From: 03-11-2022
*)
unit UnitPurchaseList;

interface

uses
    System.Generics.Collections, System.Classes, System.Generics.Defaults,
    UnitPurchase;

type
    TPurchaseComparer = class(TComparer<TPurchase>)
        public
            function Compare(const purchase1, purchase2: TPurchase): integer; override;
    end;
    TPurchaseList = class(TObject)
        private
            purchaseList: TObjectList<TPurchase>;
            purchaseComparer: TPurchaseComparer;
        public
            //Constructs a new TPurchaseList instance.
            constructor Create;
            //Returns an element at the given index.
            function Get(itemIndex: integer): TPurchase;
            //Adds a TPurchase to the TPurchase list.
            function Add(var newPurchase: TPurchase): boolean;
            //Modifies a TPurchase element at the given index.
            function Modify(itemIndex: integer; var newPurchase: TPurchase): boolean;
            //Deletes a TPurchase element at the given index.
            function Delete(itemIndex: integer): boolean;
            //Returns the size of the TPurchaseList.
            function Count: integer;
            //Clears the TPurchaseList.
            procedure Clear;
            //Sorts the TPurchaseList.
            procedure Sort;
    end;

implementation

uses
    System.SysUtils, System.DateUtils,
    UnitPurchaseRegister;

(*
* Custom compare method of TPurchase-s.
*)
function TPurchaseComparer.Compare(const purchase1, purchase2: TPurchase): integer;
begin
Result := CompareDate(purchase1.GetPurchaseDate, purchase2.GetPurchaseDate);
end;

(*
* Constructs a new TPurchaseList instance.
* Overrides constructor of TObject.
*)
constructor TPurchaseList.Create;
begin
purchaseComparer := TPurchaseComparer.Create;
purchaseList := TObjectList<TPurchase>.Create;
end;

(*
* Returns a TPurchase element at the given index.
* In case the index is not valid, returns nil.
*)
function TPurchaseList.Get(itemIndex: integer): TPurchase;
begin
if (itemIndex > -1) and (itemIndex < purchaseList.Count) then
    Result := purchaseList.Items[itemIndex]
else
    Result := nil
end;

(*
* Adds a TPurchase element.
*)
function TPurchaseList.Add(var newPurchase: TPurchase): boolean;
begin
Result := (newPurchase <> nil) and (purchaseList.Add(newPurchase) > -1);
end;

(*
* Modifies a TPurchase element at the given index.
*)
function TPurchaseList.Modify(itemIndex: integer; var newPurchase: TPurchase): boolean;
begin
if (itemIndex > -1) and (itemIndex < purchaseList.count) and (newPurchase <> nil) then
    begin
    purchaseList.Delete(itemIndex);
    purchaseList.Insert(itemIndex, newPurchase);
    Result := true;
    end
else
    Result := false;
end;

(*
* Deletes a TPurchase element at the given index.
*)
function TPurchaseList.Delete(itemIndex: integer): boolean;
begin
Result := (itemIndex > -1) and (itemIndex < purchaseList.count);
if Result then
    purchaseList.Delete(itemIndex);
end;

(*
* Returns the size of the TPurchaseList.
*)
function TPurchaseList.Count: integer;
begin
Result := purchaseList.Count;
end;

(*
* Clears the TPurchaseList.
*)
procedure TPurchaseList.Clear;
begin
purchaseList.Clear;
end;

(*
* Sorts the TPurchaseList.
*)
procedure TPurchaseList.Sort;
begin
purchaseList.Sort(purchaseComparer);
end;

end.
