(*
* The unit represents the main form of Purchase Register, also represents the purchase list and a hub for all operations within the application.
* Application: Purchase Register
* Creator: Laszlo Grimm
* From: 03-11-2022
*)
unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation,
  FMX.ListView.Types, FMX.ListView;

type
  TFormMain = class(TForm)
    LTitle: TLabel;
    LMessage: TLabel;
    BNew: TButton;
    BView: TButton;
    BStats: TButton;
    LVItems: TListView;
    BCheckAll: TButton;
    BCheckNone: TButton;
    BCheckInvert: TButton;
    BDeleteChecked: TButton;
    BSave: TButton;
    BReload: TButton;
    procedure BNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BNewTap(Sender: TObject; const Point: TPointF);
    procedure BViewClick(Sender: TObject);
    procedure BViewTap(Sender: TObject; const Point: TPointF);
    procedure LVItemsClick(Sender: TObject);
    procedure LVItemsDblClick(Sender: TObject);
    procedure LVItemsTap(Sender: TObject; const Point: TPointF);
    procedure LVItemsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure BStatsClick(Sender: TObject);
    procedure BStatsTap(Sender: TObject; const Point: TPointF);
    procedure BCheckAllClick(Sender: TObject);
    procedure BCheckAllTap(Sender: TObject; const Point: TPointF);
    procedure BCheckNoneClick(Sender: TObject);
    procedure BCheckNoneTap(Sender: TObject; const Point: TPointF);
    procedure BCheckInvertClick(Sender: TObject);
    procedure BCheckInvertTap(Sender: TObject; const Point: TPointF);
    procedure BDeleteCheckedClick(Sender: TObject);
    procedure BDeleteCheckedTap(Sender: TObject; const Point: TPointF);
    procedure BSaveClick(Sender: TObject);
    procedure BSaveTap(Sender: TObject; const Point: TPointF);
    procedure BReloadClick(Sender: TObject);
    procedure BReloadTap(Sender: TObject; const Point: TPointF);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
    FMX.SearchBox,
    UnitItem, UnitPurchaseRegister, UnitMainHelpers;

var
    searchBox: TSearchBox = nil;

procedure TFormMain.BCheckAllClick(Sender: TObject);
begin
ButtonCheckAllEvents(FormMain);
end;

procedure TFormMain.BCheckAllTap(Sender: TObject; const Point: TPointF);
begin
ButtonCheckAllEvents(FormMain);
end;

procedure TFormMain.BCheckInvertClick(Sender: TObject);
begin
ButtonCheckInvertEvents(FormMain);
end;

procedure TFormMain.BCheckInvertTap(Sender: TObject; const Point: TPointF);
begin
ButtonCheckInvertEvents(FormMain);
end;

procedure TFormMain.BCheckNoneClick(Sender: TObject);
begin
ButtonCheckNoneEvents(FormMain);
end;

procedure TFormMain.BCheckNoneTap(Sender: TObject; const Point: TPointF);
begin
ButtonCheckNoneEvents(FormMain);
end;

procedure TFormMain.BDeleteCheckedClick(Sender: TObject);
begin
ButtonDeleteCheckedEvents(FormMain, searchBox);
end;

procedure TFormMain.BDeleteCheckedTap(Sender: TObject; const Point: TPointF);
begin
ButtonDeleteCheckedEvents(FormMain, searchBox);
end;

procedure TFormMain.BNewClick(Sender: TObject);
begin
ButtonNewEvents(FormMain, FormItem, searchBox);
end;

procedure TFormMain.BNewTap(Sender: TObject; const Point: TPointF);
begin
ButtonNewEvents(FormMain, FormItem, searchBox);
end;

procedure TFormMain.BReloadClick(Sender: TObject);
begin
ButtonReloadEvents(FormMain, searchBox);
end;

procedure TFormMain.BReloadTap(Sender: TObject; const Point: TPointF);
begin
ButtonReloadEvents(FormMain, searchBox);
end;

procedure TFormMain.BSaveClick(Sender: TObject);
begin
ButtonSaveEvents(FormMain, searchBox);
end;

procedure TFormMain.BSaveTap(Sender: TObject; const Point: TPointF);
begin
ButtonSaveEvents(FormMain, searchBox);
end;

procedure TFormMain.BStatsClick(Sender: TObject);
begin
ButtonStatsEvents(FormMain, searchBox);
end;

procedure TFormMain.BStatsTap(Sender: TObject; const Point: TPointF);
begin
ButtonStatsEvents(FormMain, searchBox);
end;

procedure TFormMain.BViewClick(Sender: TObject);
begin
ButtonViewEvents(FormMain, FormItem, searchBox);
end;

procedure TFormMain.BViewTap(Sender: TObject; const Point: TPointF);
begin
ButtonViewEvents(FormMain, FormItem, searchBox);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
initIt(FormMain, searchBox);
SetControls(FormMain);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
finalIt(FormMain.LMessage);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
if resizePossible then
    ResizeMainControls(FormMain);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
if not resizePossible then
    resizePossible := true;
end;

procedure TFormMain.LVItemsClick(Sender: TObject);
begin
LVItemSelectionEvents(FormMain);
end;

procedure TFormMain.LVItemsDblClick(Sender: TObject);
begin
ButtonViewEvents(FormMain, FormItem, searchBox);
end;

procedure TFormMain.LVItemsItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
LVItemSelectionEvents(FormMain);
end;

procedure TFormMain.LVItemsTap(Sender: TObject; const Point: TPointF);
begin
LVItemSelectionEvents(FormMain);
end;

end.
