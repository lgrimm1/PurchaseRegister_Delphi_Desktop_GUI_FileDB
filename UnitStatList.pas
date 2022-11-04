(*
* The unit contains the TStatList class which serves as type of a stat list which has TStat elements.
* Application: Purchase Register
* Creator: Laszlo Grimm
* From: 03-11-2022
*)
unit UnitStatList;

interface

uses
    System.Generics.Collections, System.Generics.Defaults,
    UnitStat;

type
    TStatComparer = class(TComparer<TStat>)
        public
            function Compare(const stat1, stat2: TStat): integer; override;
    end;
    TStatList = class(TObject)
        private
            statList: TObjectList<TStat>;
            statComparer: TStatComparer;
        public
            constructor Create();
            //Creates a new element or modifies an existing one, identified by date.
            function Put(const newStat: TStat): boolean;
            //Returns a TStat by index.
            function Get(itemIndex: integer): TStat;
            //Returns the number of elements.
            function Count: integer;
            //Clears the TStatList.
            procedure Clear;
    end;

implementation

uses
    System.DateUtils;

(*
* Custom compare method of TStat-s.
*)
function TStatComparer.Compare(const stat1: TStat; const stat2: TStat): integer;
begin
Result := CompareDate(stat1.GetDate, stat2.GetDate);
end;

(*
* Compares two dates, helper method for Sort().
*)
(*
function CompareStats(const stat1, stat2: TStat): integer;
begin
Result := CompareDate(stat1.GetDate, stat2.GetDate);
end;
*)

(*
* Binary search for index of the given date in TObjectList<TStat>.
* Helper method of put().
* In case of no such element, returns -1.
*)
function IndexOf(date: TDate; var list: TObjectList<TStat>): integer;
var
    lowIndex, highIndex, actualIndex: integer;
    notFound: boolean;
begin
if list = nil then
    Result := -1
else
    if list.Count = 1 then
        if list[0].GetDate = date then
            Result := 0
        else
            Result := -1
    else //list.Count > 1
        begin
        lowIndex := 0;
        highIndex := list.Count - 1;
        notFound := true;
        actualIndex := 0;
        while notFound and (lowIndex <= highIndex) do
            begin
            actualIndex := Trunc((lowIndex + highIndex) / 2);
            if list[actualIndex].GetDate = date then
                notFound := false
            else
                if list[actualIndex].GetDate < date then
                    lowindex := actualIndex + 1
                else //list[actualIndex].GetDate > date
                    highIndex := actualIndex - 1;
            end;
        if notFound then
            Result := -1
        else
            Result := actualIndex;
        end;
end;

(*
* Constructs a new TStatList.
* Overrides constructor of TObject.
*)
constructor TStatList.Create;
begin
statComparer := TStatComparer.Create;
statList := TObjectList<TStat>.Create;
end;

(*
* Creates a new element or modifies an existing one, identified by date.
* In case of nil parameter, returns false.
*)
function TStatList.Put(const newStat: TStat): boolean;
var
    itemIndex: integer;
begin
if newStat = nil then
    Result := false
else
    begin
    itemIndex := IndexOf(newStat.GetDate, statList);
    if itemIndex = -1 then
        begin
        statList.Add(newStat);
        statList.Sort(statComparer);
        end
    else
        begin
        statList[itemIndex].AddToTotal(newStat.GetTotal);
        statList[itemIndex].RaiseCount;
        end;
    Result := true;
    end;
end;

(*
* Returns a TStat by index.
* In case of wrong index, returns nil.
*)
function TStatList.Get(itemIndex: Integer): TStat;
begin
if (itemIndex > -1) and (itemIndex < statList.Count) then
    Result := statList[itemIndex]
else
    Result := nil;
end;

(*
* Returns the number of elements.
*)
function TStatList.Count: integer;
begin
Result := statList.Count;
end;

(*
* Clears the TStatList.
*)
procedure TStatList.Clear;
begin
statList.Clear;
end;

end.
