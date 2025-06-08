unit UFormInterval;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  UDisjointInterval, Vcl.ComCtrls;

type
  TFormInterval = class(TForm)
    PanelInterval: TPanel;
    SplitterMain: TSplitter;
    PaintBoxResult: TPaintBox;
    ListBoxResult: TListBox;
    lblIntervalPart1: TLabel;
    EditIntervalStart: TEdit;
    lblIntervalPart2: TLabel;
    EditIntervalClose: TEdit;
    lblIntervalPart3: TLabel;
    FlowPanelTop: TFlowPanel;
    PanelButtons: TPanel;
    btnAdd: TBitBtn;
    btnRemove: TBitBtn;
    ShapeColor: TShape;
    btnColor: TBitBtn;
    PanelLeft: TPanel;
    lblCount: TLabel;
    ColorDialog: TColorDialog;
    TimerListSelectionWatcher: TTimer;
    PanelClient: TPanel;
    lblSelection: TLabel;
    tbIntervalStart: TTrackBar;
    tbIntervalClose: TTrackBar;
    sbRandomColor: TSpeedButton;
    procedure EditIntervalChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnColorClick(Sender: TObject);
    procedure ListBoxResultDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure btnRemoveClick(Sender: TObject);
    procedure ShapeColorMouseActivate(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y, HitTest: Integer; var MouseActivate: TMouseActivate);
    procedure PaintBoxResultPaint(Sender: TObject);
    procedure TimerListSelectionWatcherTimer(Sender: TObject);
    procedure PaintBoxResultMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxResultMouseLeave(Sender: TObject);
    procedure tbIntervalStartChange(Sender: TObject);
    procedure tbIntervalCloseChange(Sender: TObject);
    procedure sbRandomColorClick(Sender: TObject);
  protected
    DisjointIntervals: TDisjointIntervals<TColor>;
    LastSelectedIndex: Integer;
    function UpdateForm: boolean;
    function CalculateWindowDimension(out WindowMin, WindowMax, WindowWidth,
                                          NewInvervalStart, NewIntervalClose: Integer;
                                      out NewIntervalGood, HasAddedInterval: boolean): boolean;
    procedure UpdatePaintBoxSelection(X: Integer);
    procedure OnMerge(const Intervals: TIntervalList<TColor>; var NewData: TColor);
    procedure OnSplit(const Interval1, Interval2: TIntervalConflictResolution<TColor>);

    procedure InitDemoData;
  end;

var
  FormInterval: TFormInterval;

implementation

{$R *.dfm}

uses
  UITypes, Math;

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

procedure TFormInterval.btnAddClick(Sender: TObject);
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

procedure TFormInterval.btnColorClick(Sender: TObject);
begin
  if ColorDialog.Execute then
  begin
    ShapeColor.Brush.Color := ColorDialog.Color;
    PaintBoxResult.Invalidate;
    Activate;
  end;
end;

procedure TFormInterval.btnRemoveClick(Sender: TObject);
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

function TFormInterval.CalculateWindowDimension(out WindowMin, WindowMax,
  WindowWidth, NewInvervalStart, NewIntervalClose: Integer;
  out NewIntervalGood, HasAddedInterval: boolean): boolean;
begin
  WindowMin := 0;
  WindowMax := 0;
  WindowWidth := 0;
  Result := False;

  HasAddedInterval := False;
  NewIntervalGood :=
      TryStrToInt(EditIntervalStart.Text, NewInvervalStart)
      and TryStrToInt(EditIntervalClose.Text, NewIntervalClose)
      and (NewIntervalClose > NewInvervalStart);

  if DisjointIntervals.Count = 0 then
  begin
    if NewIntervalGood then
    begin
      WindowMin := NewInvervalStart;
      WindowMax := NewIntervalClose;
      WindowWidth := WindowMax - WindowMin;
      Result := True;
    end;
    Exit;
  end;

  WindowMin := DisjointIntervals.Min.Start;
  WindowMax := DisjointIntervals.Max.Close;
  HasAddedInterval := True;

  if NewIntervalGood then
  begin
    WindowMin := Min(WindowMin, NewInvervalStart);
    WindowMax := Max(WindowMax, NewIntervalClose);
  end;

  WindowWidth := WindowMax - WindowMin;
  Result := True;
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

  LastSelectedIndex := -1;
  TimerListSelectionWatcher.Enabled := True;

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

procedure TFormInterval.PaintBoxResultMouseLeave(Sender: TObject);
begin
  lblSelection.Caption := '';
end;

procedure TFormInterval.PaintBoxResultMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  UpdatePaintBoxSelection(X);
end;

procedure TFormInterval.PaintBoxResultPaint(Sender: TObject);

  procedure DrawHBar(MyCanvas: TCanvas; X1,X2, YCenter, Height: Integer; Color: TColor);
  var
    Rect: TRect;
  begin
    MyCanvas.Pen.Width := 1;
    MyCanvas.Pen.Color := clDkGray;
    MyCanvas.Brush.Color := Color;
    Rect:=TRect.Create(x1, YCenter - (Height - 1) div 2, x2, YCenter + Height div 2);
    MyCanvas.Rectangle(Rect);
  end;

const
  AXIS_PEN_WIDTH = 3;
  ITEM_PEN_WIDTH = 5;
  ITEM_PEN_HIGHTLIGHT_WIDTH = 9;
  AXIS_DISTANCE = 8;
  BORDER_WIDTH = 1;
var
  MyCanvas: TCanvas;
  Rect: TRect;

  AddedIntervalYPos, NewIntervalYPos: Integer;
  WindowMin, WindowMax, WindowWidth: Integer;
  NewInvervalStart, NewIntervalClose: integer;
  NewIntervalGood, HasAddedInterval: boolean;

  idx: integer;
  BarHeight: integer;
begin
  MyCanvas := (Sender as TPaintBox).Canvas;
  Rect := (Sender as TPaintBox).ClientRect;
  MyCanvas.Pen.Width := BORDER_WIDTH;
  MyCanvas.Pen.Color := clBlack;
  MyCanvas.Brush.Color := clWhite;
  MyCanvas.Rectangle(Rect);

  MyCanvas.Pen.Width := AXIS_PEN_WIDTH;
  MyCanvas.Pen.Color := clBlack;
  MyCanvas.MoveTo(Rect.Left, Rect.CenterPoint.Y);
  MyCanvas.LineTo(Rect.Right, Rect.CenterPoint.Y);

  InflateRect(Rect, -3, -3);

  if Rect.Width <= 0 then
    Exit;

  CalculateWindowDimension(WindowMin, WindowMax, WindowWidth, NewInvervalStart,
                           NewIntervalClose, NewIntervalGood, HasAddedInterval);

  if HasAddedInterval then
  begin
    // draw added items
    AddedIntervalYPos := Rect.CenterPoint.Y - AXIS_DISTANCE;

    idx := 0;
    for var item in DisjointIntervals do
    begin
      if idx = ListBoxResult.ItemIndex then
        BarHeight := ITEM_PEN_HIGHTLIGHT_WIDTH
      else
        BarHeight := ITEM_PEN_WIDTH;

      DrawHBar(MyCanvas,
               Rect.Left + Trunc((item.Start - WindowMin) / WindowWidth * Rect.Width),
               Rect.Left + Trunc((item.Close - WindowMin) / WindowWidth * Rect.Width),
               AddedIntervalYPos,
               BarHeight,
               item.Data);

      inc(idx);
    end;
  end;

  // draw new item
  NewIntervalYPos := Rect.CenterPoint.Y + AXIS_DISTANCE;

  if NewIntervalGood then
  begin
    DrawHBar(MyCanvas,
             Rect.Left + Trunc((NewInvervalStart - WindowMin) / WindowWidth * Rect.Width),
             Rect.Left + Trunc((NewIntervalClose - WindowMin) / WindowWidth * Rect.Width),
             NewIntervalYPos,
             ITEM_PEN_WIDTH,
             ShapeColor.Brush.Color);
  end;
end;

procedure TFormInterval.ShapeColorMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer; var MouseActivate: TMouseActivate);
begin
  btnColor.Click;
end;

procedure TFormInterval.sbRandomColorClick(Sender: TObject);
begin
  ShapeColor.Brush.Color := Random($ffffff);
  PaintBoxResult.Invalidate;
end;

procedure TFormInterval.tbIntervalCloseChange(Sender: TObject);
begin
  EditIntervalClose.Text := IntToStr(tbIntervalClose.Position);
end;

procedure TFormInterval.tbIntervalStartChange(Sender: TObject);
begin
  EditIntervalStart.Text := IntToStr(tbIntervalStart.Position);
end;

procedure TFormInterval.TimerListSelectionWatcherTimer(Sender: TObject);
begin
  if LastSelectedIndex <> ListBoxResult.ItemIndex then
  begin
    LastSelectedIndex := ListBoxResult.ItemIndex;
    PaintBoxResult.Invalidate;
  end;
end;

function TFormInterval.UpdateForm: boolean;

  function Validate(edit: TEdit; tb: TTrackBar; out val:integer): boolean;
  begin
    Result := TryStrToInt(edit.Text, val);
    if Result then
    begin
      edit.Color := clWindow;
      if (val >= tb.Min)
         and (val <= tb.Max)
      then begin
        tb.Position := val;
        tb.Enabled := True;
      end else
        tb.Enabled := False;

      Exit;
    end;
    edit.Color := COLOR_INVALID;
    tb.Enabled := True;
  end;

var
  NewIntervalStart, NewIntervalClose: integer;
begin
  Result := Validate(EditIntervalStart, tbIntervalStart, NewIntervalStart);
  Result := Validate(EditIntervalClose, tbIntervalClose, NewIntervalClose) and Result;
  Result := Result and (NewIntervalStart < NewIntervalClose);
  btnAdd.Enabled := Result;
  btnRemove.Enabled := Result;

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

  lblCount.Caption := Format('Count: %d', [DisjointIntervals.Count]);

  PaintBoxResult.Invalidate;
end;

procedure TFormInterval.UpdatePaintBoxSelection(X: Integer);
const
  MSG_INCLUDED: array[Boolean] of string = ('Not included', 'Included');
var
  WindowMin, WindowMax, WindowWidth: Integer;
  NewInvervalStart, NewIntervalClose: integer;
  NewIntervalGood, HasAddedInterval: boolean;

  CurrentPosition: Integer;
  PointIncluded: Boolean;
begin
  CalculateWindowDimension(WindowMin, WindowMax, WindowWidth, NewInvervalStart,
                           NewIntervalClose, NewIntervalGood, HasAddedInterval);

  CurrentPosition := Trunc(X / PaintBoxResult.ClientWidth * WindowWidth) + WindowMin;

  PointIncluded := DisjointIntervals.IsPointIn(CurrentPosition);
  lblSelection.Caption := Format('X: %d - %s',[CurrentPosition, MSG_INCLUDED[PointIncluded]]);
end;

end.
