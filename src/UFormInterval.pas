unit UFormInterval;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  UDisjointInterval;

type
  TFormInterval = class(TForm)
    PanelInterval: TPanel;
    SplitterMain: TSplitter;
    PaintBoxResult: TPaintBox;
    ListBoxResult: TListBox;
    LabelIntervalPart1: TLabel;
    EditIntervalStart: TEdit;
    LabelIntervalPart2: TLabel;
    EditIntervalClose: TEdit;
    LabelIntervalPart3: TLabel;
    FlowPanelTop: TFlowPanel;
    PanelButtons: TPanel;
    ButtonAdd: TBitBtn;
    ButtonRemove: TBitBtn;
    ShapeColor: TShape;
    ButtonColor: TBitBtn;
    PanelLeft: TPanel;
    LabelCount: TLabel;
    ColorDialog: TColorDialog;
    procedure EditIntervalChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonColorClick(Sender: TObject);
    procedure ListBoxResultDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure ShapeColorMouseActivate(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y, HitTest: Integer; var MouseActivate: TMouseActivate);
  protected
    DisjointIntervals: TDisjointIntervals<TColor>;
    function UpdateForm: boolean;
    procedure OnMerge(const Intervals: TIntervalList<TColor>; var NewData: TColor);
    procedure OnSplit(const Interval1, Interval2: TIntervalConflictResolution<TColor>);

    procedure InitDemoData;
  end;

var
  FormInterval: TFormInterval;

implementation

{$R *.dfm}

uses
  UITypes;

const
  COLOR_INVALID = $A0A0FF;

type
  TBoxedColor = class
  private
    FColor: TColor;
  public
    constructor Create(AColor: TColor);
    property Color: TColor read FColor write FColor;
  end;

constructor TBoxedColor.Create(AColor: TColor);
begin
  inherited Create;
  FColor := AColor;
end;

procedure TFormInterval.ButtonAddClick(Sender: TObject);
var
  InvervalStart, IntervalClose: integer;
begin
  try
    if not TryStrToInt(EditIntervalStart.Text, InvervalStart) then
      Exit;

    if not TryStrToInt(EditIntervalClose.Text, IntervalClose) then
      Exit;

    DisjointIntervals.Add(TInterval<TColor>.Create(InvervalStart, IntervalClose, ShapeColor.Brush.Color));
  finally
    UpdateForm;
  end;
end;

procedure TFormInterval.ButtonColorClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    ShapeColor.Brush.Color := ColorDialog.Color;
end;

procedure TFormInterval.ButtonRemoveClick(Sender: TObject);
var
  InvervalStart, IntervalClose: integer;
begin
  try
    if not TryStrToInt(EditIntervalStart.Text, InvervalStart) then
      Exit;

    if not TryStrToInt(EditIntervalClose.Text, IntervalClose) then
      Exit;

    DisjointIntervals.Remove(TInterval<TColor>.Create(InvervalStart, IntervalClose));
  finally
    UpdateForm;
  end;
end;

procedure TFormInterval.EditIntervalChange(Sender: TObject);
begin
  UpdateForm;
end;

procedure TFormInterval.FormCreate(Sender: TObject);
begin
  DisjointIntervals := TDisjointIntervals<TColor>.Create;
  DisjointIntervals.OnMerge := OnMerge;
  DisjointIntervals.OnSplit := OnSplit;

  InitDemoData;

  UpdateForm;
end;

procedure TFormInterval.FormShow(Sender: TObject);
begin
  UpdateForm;
end;

procedure TFormInterval.InitDemoData;
begin
  DisjointIntervals.Clear;
  DisjointIntervals.Add(TInterval<TColor>.Create( 1,  4, clYellow));
  DisjointIntervals.Add(TInterval<TColor>.Create( 7, 11, clRed));
  DisjointIntervals.Add(TInterval<TColor>.Create(16, 22, clBlue));
  DisjointIntervals.Add(TInterval<TColor>.Create(25, 30, clGreen));

  EditIntervalStart.Text := '3';
  EditIntervalClose.Text := '13';
  ShapeColor.Brush.Color := clPurple;
end;

procedure TFormInterval.ListBoxResultDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  ColorRect: TRect;
  TextRect: TRect;
  ItemColor: TColor;
  myListBox: TListBox;
  oldBrushColor: TColor;
begin
  myListBox := TListBox(Control);
  ItemColor := (myListBox.Items.Objects[Index] as TBoxedColor).Color;

  myListBox.Canvas.FillRect(Rect);

  oldBrushColor := myListBox.Canvas.Brush.Color;
  try
    ColorRect := Rect;
    ColorRect.Right := ColorRect.Left + 16;
    InflateRect(ColorRect, -2, -2);
    myListBox.Canvas.Brush.Color := ItemColor;
    myListBox.Canvas.FillRect(ColorRect);
  finally
    myListBox.Canvas.Brush.Color := oldBrushColor;
  end;

  TextRect := Rect;
  TextRect.Left := ColorRect.Right + 4;
  myListBox.Canvas.TextOut(TextRect.Left, TextRect.Top, myListBox.Items[Index]);
end;

procedure TFormInterval.OnMerge(const Intervals: TIntervalList<TColor>; var NewData: TColor);

  type
    TSelector = reference to function(c: TColor): Byte;

  function MixChannel(Selector: TSelector): Byte;
  var
    weight: Int64;
    weight_sum: Double;
  begin
    weight := 0;
    for var i in Intervals do
    begin
      weight := weight + i.Close - i.Start;
    end;

    weight_sum := 0;
    for var i in Intervals do
    begin
      weight_sum := weight_sum + (i.Close - i.Start) / weight * Selector(i.Data);
    end;

    Result := Trunc(weight_sum)
  end;

begin
  NewData :=
    MixChannel( function(c: TColor): Byte
                begin
                  Result := c and $ff;
                end)
    or ( MixChannel( function(c: TColor): Byte
                begin
                  Result := (c shr 8) and $ff;
                end) shl 8)
    or ( MixChannel( function(c: TColor): Byte
                begin
                  Result := (c shr 16) and $ff;
                end) shl 16);
end;

procedure TFormInterval.OnSplit(const Interval1, Interval2: TIntervalConflictResolution<TColor>);

  function MoveColorChannel(Source, Target: Byte; Factor: Double): Byte;
  begin
    Result := Trunc(Source * (1 - Factor) + Target * Factor);
  end;

begin
  Interval1.NewData :=
      MoveColorChannel(Interval1.NewData and $ff, 255, 0.3)
      or (MoveColorChannel((Interval1.NewData shr 8) and $ff, 255, 0.3) shl 8)
      or (MoveColorChannel((Interval1.NewData shr 16) and $ff, 255, 0.3) shl 16);

  Interval2.NewData :=
      MoveColorChannel(Interval2.NewData and $ff, 0, 0.3)
      or (MoveColorChannel((Interval2.NewData shr 8) and $ff, 0, 0.3) shl 8)
      or (MoveColorChannel((Interval2.NewData shr 16) and $ff, 0, 0.3) shl 16);
end;

procedure TFormInterval.ShapeColorMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer; var MouseActivate: TMouseActivate);
begin
  ButtonColor.Click;
end;

function TFormInterval.UpdateForm: boolean;

  function Validate(edit: TEdit): boolean;
  var
    dummy: integer;
  begin
    Result := TryStrToInt(edit.Text, dummy);
    if Result then
    begin
      edit.Color := clWindow;
      Exit;
    end;
    edit.Color := COLOR_INVALID;
  end;

begin
  Result := Validate(EditIntervalStart);
  Result := Validate(EditIntervalClose) and Result;
  ButtonAdd.Enabled := Result;
  ButtonRemove.Enabled := Result;

  ListBoxResult.Items.BeginUpdate;
  try
    ListBoxResult.Items.Clear;
    for var item in DisjointIntervals do
    begin
      ListBoxResult.Items.AddObject(Format('%4d, %4d',[item.Start, item.Close]), TBoxedColor.Create(item.Data));
    end;
  finally
    ListBoxResult.Items.EndUpdate;
  end;

  LabelCount.Caption := Format('Count: %d', [DisjointIntervals.Count]);

  PaintBoxResult.Invalidate;
end;

end.
