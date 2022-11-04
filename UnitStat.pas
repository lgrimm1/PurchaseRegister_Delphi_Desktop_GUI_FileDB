(*
* The unit contains the TStat class which serves as type of a statistics element.
* Application: Purchase Register
* Creator: Laszlo Grimm
* From: 03-11-2022
*)
unit UnitStat;

interface

type
    TStat = class(TOBject)
        private
            date: TDate;
            total: double;
            count: integer;
        public
            constructor Create(newDate: TDate; newTotal: double);
            procedure AddToTotal(value: double);
            procedure RaiseCount;
            function GetDate: TDate;
            function GetTotal: double;
            function GetCount: integer;
            function GetAverage: double;
    end;

implementation

(*
* Constructs a new TStatElement.
*)
constructor TStat.Create(newDate: TDate; newTotal: double);
begin
date := newDate;
total := newTotal;
count := 1;
end;

(*
* Increases total.
*)
procedure TStat.AddToTotal(value: double);
begin
total := total + value;
end;

(*
* Increases count by 1.
*)
procedure TStat.RaiseCount;
begin
count := count + 1;
end;

(*
* Returns date.
*)
function TStat.GetDate: TDate;
begin
Result := date;
end;

(*
* Returns total.
*)
function TStat.GetTotal: double;
begin
Result := total;
end;

(*
* Returns count.
*)
function TStat.GetCount: integer;
begin
Result := count;
end;

(*
* Calculates average.
*)
function TStat.GetAverage: double;
begin
Result := total / count;
end;
end.
