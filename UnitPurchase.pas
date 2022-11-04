(*
* The unit contains the TPurchase class which serves as type of a purchase.
* Application: Purchase Register
* Creator: Laszlo Grimm
* From: 02-11-2022
*)
unit UnitPurchase;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants;

type
    TPurchaseType = (CARD, CASH, INTERNET);
    TPurchase = class(TObject)
        private
            purchaseDate: TDate;
            purchaseType: TPurchaseType;
            purchaseValue: double;
            purchaseDescription: string;
        public
            //Constructs an instance with the given TDate.
            constructor Create(dateOfPurchase: TDate; typeOfPurchase: TPurchaseType; valueOfPurchase: double; descriptionOfPurchase: string);
            //Constructs an instance with given date components.
            constructor CreateDateComponents(yearOfPurchase, monthOfPurchase, dayOfPurchase: word; typeOfPurchase: TPurchaseType; valueOfPurchase: double; descriptionOfPurchase: string);
            //Returns the date as TDate.
            function GetPurchaseDate: TDate;
            //Returns the type.
            function GetPurchaseType: TPurchaseType;
            //Returns the purchase value.
            function GetPurchaseValue: double;
            //Returns the description.
            function GetPurchaseDescription: string;
            //Returns the purchase type as a string.
            function GetPurchaseTypeString: string;
    end;

implementation

uses
    System.DateUtils;

(*
* Constructs an instance with the given TDate.
* Overrides constructor of TObject.
*)
constructor TPurchase.Create(dateOfPurchase: TDate; typeOfPurchase: TPurchaseType; valueOfPurchase: double; descriptionOfPurchase: string);
begin
purchaseDate := dateOfPurchase;
purchaseType := typeOfPurchase;
purchaseValue := valueOfPurchase;
purchaseDescription := descriptionOfPurchase;
end;

(*
* Constructs an instance with given date components.
* In case unable to convert the date components to a valid date, uses system date.
*)
constructor TPurchase.CreateDateComponents(yearOfPurchase, monthOfPurchase, dayOfPurchase: word; typeOfPurchase: TPurchaseType; valueOfPurchase: double; descriptionOfPurchase: string);
begin
if IsValidDate(yearOfPurchase, monthOfPurchase, dayOfPurchase) then
    purchaseDate := EncodeDate(yearOfPurchase, monthOfPurchase, dayOfPurchase)
else
    purchaseDate := Date;
purchaseType := typeOfPurchase;
purchaseValue := valueOfPurchase;
purchaseDescription := descriptionOfPurchase;
end;

(*
* Returns the date as TDate.
*)
function TPurchase.GetPurchaseDate: TDate;
begin
Result := purchaseDate;
end;

(*
* Returns the type.
*)
function TPurchase.GetPurchaseType: TPurchaseType;
begin
Result := purchaseType;
end;

(*
* Returns the purchase value.
*)
function TPurchase.GetPurchaseValue: double;
begin
Result := purchaseValue;
end;

(*
* Returns the description.
*)
function TPurchase.GetPurchaseDescription: string;
begin
Result := purchaseDescription;
end;

(*
* Returns the purchase type as a string.
*)
function TPurchase.GetPurchaseTypeString: string;
begin
case purchaseType of
    CARD: Result := 'CARD';
    CASH: Result := 'CASH';
    INTERNET: Result := 'INET';
    end;
end;

end.

