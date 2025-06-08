unit UDisjointInterval;

interface

uses
  SysUtils, Generics.Collections, UAvlTree;

type
  TInterval<T> = record
    Start, Close: Integer;
    Data: T;
    constructor Create(AStart, AClose: Integer); overload;
    constructor Create(AStart, AClose: Integer; AData: T); overload;
    function TouchOrIntersect(other: TInterval<T>): boolean;
  end;

  TIntervalIterator<T> = class(TInterfacedObject, IEnumerator<TInterval<T> >, IEnumerator)
  protected
    FReseted: boolean;
    FCurrent: TAvlTreeItem<Integer, TInterval<T> >;
    FItemOrig: TAvlTreeItem<Integer, TInterval<T> >;
  public
    { IEnumerator<T> }
    function GetCurrentGeneric: TInterval<T>;
    function IEnumerator<TInterval<T>>.GetCurrent = GetCurrentGeneric;

    { IEnumerator }
    function GetCurrent: TObject;
    function IEnumerator.GetCurrent = GetCurrent;

    property Current: TInterval<T> read GetCurrentGeneric;

    constructor Create(Item: TAvlTreeItem<Integer, TInterval<T> >);
    function MovePrev: Boolean;
    function MoveNext: Boolean;
    procedure Reset;
  end;

  TIntervalConflictResolution<T> = class
  private
    FNewData: T;
    FInterval: TInterval<T>;
    procedure SetNewData(const Value: T);
  public
    property Interval: TInterval<T> read FInterval;
    property NewData: T read FNewData write SetNewData;
    constructor Create(AInterval: TInterval<T>);
  end;

  TIntervalList<T> = array of TInterval<T>;

  TOnMerge<T> = procedure(const Intervals: TIntervalList<T>; var NewData: T) of object;
  TOnSplit<T> = procedure(const Interval1, Interval2: TIntervalConflictResolution<T>) of object;

  EIntervalNotFound = class(Exception)
    constructor Create;
  end;

  TDisjointIntervals<T> = class
  private
    FOnMerge: TOnMerge<T>;
    FOnSplit: TOnSplit<T>;
    procedure SetOnMerge(const Value: TOnMerge<T>);
    procedure SetOnSplit(const Value: TOnSplit<T>);
  protected type
    TItems = TAvlTree<Integer, TInterval<T> >;
    TItem = TAvlTreeItem<Integer, TInterval<T> >;
  protected
    Items: TItems;
    procedure DoOnMerge(const Intervals: TIntervalList<T>; var NewData: T); virtual;
    procedure DoOnSplit(const Interval1, Interval2: TIntervalConflictResolution<T>); virtual;
    procedure GetTouchOrIntersectItems(const Interval: TInterval<T>; Target: TList<TItem>);
  public
    property OnMerge: TOnMerge<T> read FOnMerge write SetOnMerge;
    property OnSplit: TOnSplit<T> read FOnSplit write SetOnSplit;

    procedure Add(Interval: TInterval<T>);
    procedure Remove(Interval: TInterval<T>);
    function GetIntervalAt(Pos: Integer): TInterval<T>;
    function IsPointIn(Pos: Integer): boolean;

    function Min: TInterval<T>;
    function Max: TInterval<T>;

    function Count: integer;
    procedure Clear;
    function GetEnumerator: TIntervalIterator<T>;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Generics.Defaults, Math;

{ TDisjointIntervals<T> }

procedure TDisjointIntervals<T>.Add(Interval: TInterval<T>);
var
  i: TItem;
  TouchOrIntersectItems: TList<TItem>;
  MergeList: TList<TInterval<T> >;
  NewData: T;
begin
  if Interval.Start >= Interval.Close then
    Exit;

  if Items.Count = 0 then
  begin
    TItem.Create(Items, Interval.Start).Data := Interval;
    Exit;
  end;

  TouchOrIntersectItems := TList<TItem>.Create;
  try
    GetTouchOrIntersectItems(Interval, TouchOrIntersectItems);

    if TouchOrIntersectItems.Count > 0 then
    begin
      Interval.Start := Math.Min(Interval.Start, TouchOrIntersectItems[0].Data.Start);
      Interval.Close := Math.Max(Interval.Close,
                                 TouchOrIntersectItems[TouchOrIntersectItems.Count - 1].Data.Close);
    end;

    for i in TouchOrIntersectItems do
      Items.DeleteItem(i);

    if TouchOrIntersectItems.Count > 0 then
    begin
      MergeList:=TList<TInterval<T> >.Create;
      try
        for i in TouchOrIntersectItems do
          MergeList.Add(i.Data);
        MergeList.Add(Interval);
        NewData := Interval.Data;
        DoOnMerge(MergeList.ToArray, NewData);
        Interval.Data := NewData;
      finally
        MergeList.Free;
      end;
    end;

    TItem.Create(Items, Interval.Start).Data := Interval;
  finally
    TouchOrIntersectItems.Free;
  end;
end;

procedure TDisjointIntervals<T>.Clear;
begin
  Items.Clear;
end;

function TDisjointIntervals<T>.Count: integer;
begin
  Result := Items.Count;
end;

constructor TDisjointIntervals<T>.Create;
begin
  Items := TItems.Create(TComparer<Integer>.Default)
end;

destructor TDisjointIntervals<T>.Destroy;
begin
  Items.Free;
  inherited;
end;

procedure TDisjointIntervals<T>.DoOnMerge(const Intervals: TIntervalList<T>; var NewData: T);
begin
  if Assigned(FOnMerge) then
    FOnMerge(Intervals, NewData)
end;

procedure TDisjointIntervals<T>.DoOnSplit(const Interval1,
  Interval2: TIntervalConflictResolution<T>);
begin
  if Assigned(FOnSplit) then
    FOnSplit(Interval1, Interval2)
end;

function TDisjointIntervals<T>.GetEnumerator: TIntervalIterator<T>;
begin
  Result := TIntervalIterator<T>.Create(Items.Min);
end;

function TDisjointIntervals<T>.GetIntervalAt(Pos: Integer): TInterval<T>;
var
  i: TItem;
begin
  i := Items.SearchItemOrPrev(Pos);
  if not Assigned(i) then
    raise EIntervalNotFound.Create;
  if i.Data.Close > Pos then
    Exit(i.Data);
  raise EIntervalNotFound.Create;
end;

procedure TDisjointIntervals<T>.GetTouchOrIntersectItems(const Interval: TInterval<T>;
  Target: TList<TItem>);
var
  i: TItem;
begin
  Target.Clear;
  i := Items.SearchItemOrPrev(Interval.Start);
  if not Assigned(i) then
    i := Items.Min;

  while Assigned(i) and (i.Data.Start <= Interval.Close) do
  begin
    if Interval.TouchOrIntersect(i.Data) then
      Target.Add(i);
    i := i.Next;
  end;
end;

function TDisjointIntervals<T>.IsPointIn(Pos: Integer): boolean;
var
  i: TItem;
begin
  i := Items.SearchItemOrPrev(Pos);
  if not Assigned(i) then
    Exit(false);
  Result := i.Data.Close > Pos;
end;

function TDisjointIntervals<T>.Max: TInterval<T>;
var
  R: TItem;
begin
  R := Items.Max;

  if not Assigned(R) then
    raise EIntervalNotFound.Create;

  Result := R.Data;
end;

function TDisjointIntervals<T>.Min: TInterval<T>;
var
  R: TItem;
begin
  R := Items.Min;

  if not Assigned(R) then
    raise EIntervalNotFound.Create;

  Result := R.Data;
end;

procedure TDisjointIntervals<T>.Remove(Interval: TInterval<T>);
var
  i: TItem;
  TouchOrIntersectItems: TList<TItem>;
  Interval1, Interval2: TInterval<T>;
  Interval1CR, Interval2CR: TIntervalConflictResolution<T>;
begin
  if Interval.Start >= Interval.Close then
    Exit;

  if Items.Count = 0 then
    Exit;

  TouchOrIntersectItems := TList<TItem>.Create;
  try
    GetTouchOrIntersectItems(Interval, TouchOrIntersectItems);

    if TouchOrIntersectItems.Count = 0 then
      Exit;

    if (TouchOrIntersectItems.Count = 1)
       and (Interval.Start > TouchOrIntersectItems[0].Data.Start)
       and (Interval.Close < TouchOrIntersectItems[0].Data.Close)
    then begin
      Items.DeleteItem(TouchOrIntersectItems[0]);
      Interval1 := TInterval<T>.Create(
                       TouchOrIntersectItems[0].Data.Start,
                       Interval.Start,
                       TouchOrIntersectItems[0].Data.Data);
      Interval2 := TInterval<T>.Create(
                       Interval.Close,
                       TouchOrIntersectItems[0].Data.Close,
                       TouchOrIntersectItems[0].Data.Data);

      Interval1CR := TIntervalConflictResolution<T>.Create(Interval1);
      Interval2CR := TIntervalConflictResolution<T>.Create(Interval2);
      DoOnSplit(Interval1CR, Interval2CR);

      Interval1.Data := Interval1CR.NewData;
      Interval2.Data := Interval2CR.NewData;

      Items.DeleteItem(TouchOrIntersectItems[0]);

      TItem.Create(Items, Interval1.Start).Data := Interval1;
      TItem.Create(Items, Interval2.Start).Data := Interval2;
      Exit;
    end;

    Interval1 := TouchOrIntersectItems[0].Data;
    Interval1.Close := Interval.Start;

    Interval2 := TouchOrIntersectItems[TouchOrIntersectItems.Count - 1].Data;
    Interval2.Start := Interval.Close;

    for i in TouchOrIntersectItems do
      Items.DeleteItem(i);

    Add(Interval1);
    Add(Interval2);
  finally
    TouchOrIntersectItems.Free;
  end;
end;

procedure TDisjointIntervals<T>.SetOnMerge(const Value: TOnMerge<T>);
begin
  FOnMerge := Value;
end;

procedure TDisjointIntervals<T>.SetOnSplit(const Value: TOnSplit<T>);
begin
  FOnSplit := Value;
end;

{ TDisjointInterval<T> }

constructor TInterval<T>.Create(AStart, AClose: Integer; AData: T);
begin
  Start := AStart;
  Close := AClose;
  Data := AData;
end;

function TInterval<T>.TouchOrIntersect(other: TInterval<T>): boolean;
begin
  Result :=
    ( (Start >= other.Start) and (Start <= other.Close) )
    or ( (other.Start >= Start) and (other.Start <= Close) )
end;

constructor TInterval<T>.Create(AStart, AClose: Integer);
begin
  Start := AStart;
  Close := AClose;
end;

{ TIntervalConflictResolution<T> }

constructor TIntervalConflictResolution<T>.Create(AInterval: TInterval<T>);
begin
  FInterval := AInterval;
  FNewData := FInterval.Data;
end;

procedure TIntervalConflictResolution<T>.SetNewData(const Value: T);
begin
  FNewData := Value;
end;

{ TIntervalIterator<T> }

constructor TIntervalIterator<T>.Create(
  Item: TAvlTreeItem<Integer, TInterval<T>>);
begin
  FItemOrig := Item;
  Reset;
end;

function TIntervalIterator<T>.GetCurrent: TObject;
begin
  Result := nil;
end;

function TIntervalIterator<T>.GetCurrentGeneric: TInterval<T>;
begin
  Result := FCurrent.Data;
end;

function TIntervalIterator<T>.MoveNext: Boolean;
begin
  if FReseted then
  begin
    FCurrent := FItemOrig;
    FReseted := False;
  end else
    FCurrent := FCurrent.Next;
  Result := Assigned(FCurrent)
end;

function TIntervalIterator<T>.MovePrev: Boolean;
begin
  if FReseted then
  begin
    FCurrent := FItemOrig;
    FReseted := False;
  end else
    FCurrent := FCurrent.Prev;
  Result := Assigned(FCurrent)
end;

procedure TIntervalIterator<T>.Reset;
begin
  FReseted := True;
end;

{ EIntervalNotFound }

constructor EIntervalNotFound.Create;
begin
  inherited Create('Interval not found');
end;

end.
