unit UAvlTree;

interface

uses
  SysUtils, System.Generics.Defaults;


type
  TFactoryMethod<T> = reference to function: T;
  TDestoryMethod<T> = reference to procedure(item: T);
  TAvlTree<TKey, TItemData> = class;
  EKeyConflict = class(Exception);

  TAvlTreeItem<TKey, TData> = class
  private
    FKey: TKey;
    FHeight: integer;
    FData: TData;
    FLinkedIn: boolean;
  protected
    FParent, FLeft, FRight: TAvlTreeItem<TKey, TData>;
    FTree: TAvlTree<TKey, TData>;
    procedure SetData(const Value: TData);
    function SetHeight: boolean;
    function SafeGetHeight: integer;
  public
    property Height: integer read FHeight;
    function IsLeaf: boolean;
    function Next: TAvlTreeItem<TKey, TData>;
    function Prev: TAvlTreeItem<TKey, TData>;
    property Key: TKey read FKey;
    property Data: TData read FData write SetData;
    constructor Create(ATree: TAvlTree<TKey, TData>; AKey: TKey);
    destructor Destroy; override;
  end;

  TAvlTree<TKey, TItemData> = class
  private
    FOwnsObjects: boolean;
    FCount: integer;
    FComparer: IComparer<TKey>;
    FCreateItemData: TFactoryMethod<TItemData>;
    FDestoryItemData: TDestoryMethod<TItemData>;
    procedure SetOwnsObjects(const Value: boolean);
    procedure AdjustL(Item: TAvlTreeItem<TKey, TItemData>);
    procedure AdjustR(Item: TAvlTreeItem<TKey, TItemData>);
    class procedure InternalSwapItem(var item1, item2: TAvlTreeItem<TKey, TItemData>); static;
  protected
    FRoot: TAvlTreeItem<TKey, TItemData>;
    function InternalSearchItem(Key: TKey; out Parent: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>;
    function InternalMin(Root: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>;
    function InternalMax(Root: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>;
    function InternalNext(Item: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>;
    function InternalPrev(Item: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>;
    procedure RotateR(Parent: TAvlTreeItem<TKey, TItemData>);
    procedure RotateL(Parent: TAvlTreeItem<TKey, TItemData>);
    procedure LinkOut(Item: TAvlTreeItem<TKey, TItemData>);
    procedure LinkIn(Item: TAvlTreeItem<TKey, TItemData>;parent:TAvlTreeItem<TKey, TItemData>);
    procedure ChangeMyParentLinkTo(Item, ChangeTo: TAvlTreeItem<TKey, TItemData>);
    procedure SwapItem(Item1,Item2: TAvlTreeItem<TKey, TItemData>);
    procedure InternalAfterLinkOut(Item: TAvlTreeItem<TKey, TItemData>);
    procedure SetHeightUpper(item: TAvlTreeItem<TKey, TItemData>);
    procedure Adjust(Item: TAvlTreeItem<TKey, TItemData>);
  public
    property OwnsObjects: boolean read FOwnsObjects write SetOwnsObjects;

    function Min: TAvlTreeItem<TKey, TItemData>;
    function Max: TAvlTreeItem<TKey, TItemData>;
    property Count: integer read FCount;
    function Height: integer;
    procedure Clear;
    function IsIn(Key: TKey): boolean;
    function SearchItem(Key: TKey): TAvlTreeItem<TKey, TItemData>;
    procedure DeleteItem(Item: TAvlTreeItem<TKey, TItemData>);
    function EnsureItem(Key: TKey): TAvlTreeItem<TKey, TItemData>;
    function AddItem(Key: TKey): TAvlTreeItem<TKey, TItemData>; overload;
    function AddItem(Item: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>; overload;
    constructor Create(AComparer: IComparer<TKey>;
                       ACreateItemData: TFactoryMethod<TItemData> = nil;
                       ADestoryItemData: TDestoryMethod<TItemData> = nil);
    destructor Destroy; override;
  end;

implementation

uses
  Math;

{ TAvlTree<TKey, TItemData> }

function TAvlTree<TKey, TItemData>.AddItem(
  Key: TKey): TAvlTreeItem<TKey, TItemData>;
begin
  Result := TAvlTreeItem<TKey, TItemData>.Create(Self, Key);
  if Assigned(FCreateItemData) then
    Result.Data := FCreateItemData;
end;

function TAvlTree<TKey, TItemData>.AddItem(
  Item: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>;
var
  parent: TAvlTreeItem<TKey, TItemData>;
begin
  if Assigned(item) then
  begin
    if Assigned(item.FTree) then
      item.FTree.LinkOut(Item);

    Item.FTree:=Self;

    if Assigned(InternalSearchItem(Item.Key,parent)) then
      raise EKeyConflict.Create('Key conflict.');

    LinkIn(Item, parent);
  end;
  Result:=Item;
end;

procedure TAvlTree<TKey, TItemData>.Adjust(Item: TAvlTreeItem<TKey, TItemData>);
begin
  while Assigned(Item) do
  begin
    if Item.FLeft.SafeGetHeight - Item.FRight.SafeGetHeight < -1 then
      AdjustL(Item)
    else if Item.FLeft.SafeGetHeight - Item.FRight.SafeGetHeight > 1 then
      AdjustR(Item);
    Item := Item.FParent;
  end;
end;

procedure TAvlTree<TKey, TItemData>.AdjustL(
  Item: TAvlTreeItem<TKey, TItemData>);
begin
  if Item.FRight.FLeft.SafeGetHeight > Item.FRight.FRight.SafeGetHeight then
  begin
    RotateR(Item.FRight);
  end;
  RotateL(Item);
end;

procedure TAvlTree<TKey, TItemData>.AdjustR(
  Item: TAvlTreeItem<TKey, TItemData>);
begin
  if Item.FLeft.FLeft.SafeGetHeight < Item.FLeft.FRight.SafeGetHeight then
  begin
    RotateL(Item.FLeft);
  end;
  RotateR(Item);
end;

procedure TAvlTree<TKey, TItemData>.ChangeMyParentLinkTo(Item,
  ChangeTo: TAvlTreeItem<TKey, TItemData>);
begin
  if Assigned(Item) then
  begin
    if Assigned(item.FParent) then
    begin
      if item.FParent.FLeft = Item then
        item.FParent.FLeft := changeto
      else
        item.FParent.FRight := changeto;
    end else
      FRoot := changeto;
  end;
end;

procedure TAvlTree<TKey, TItemData>.Clear;
begin
  while Assigned(FRoot) do
    DeleteItem(FRoot);
end;

constructor TAvlTree<TKey, TItemData>.Create(AComparer: IComparer<TKey>;
    ACreateItemData: TFactoryMethod<TItemData>; ADestoryItemData: TDestoryMethod<TItemData>);
begin
  FComparer := AComparer;
  FCreateItemData := ACreateItemData;
  FDestoryItemData := ADestoryItemData;
  FOwnsObjects := false;
  FRoot := nil;
  FCount := 0;
end;

procedure TAvlTree<TKey, TItemData>.DeleteItem(
  Item: TAvlTreeItem<TKey, TItemData>);
begin
  if Assigned(Item) and (Item.FTree = self) then
  begin
    LinkOut(Item);

    if OwnsObjects then
      Item.Free;
  end;
end;

destructor TAvlTree<TKey, TItemData>.Destroy;
begin
  Clear;
  inherited;
end;

function TAvlTree<TKey, TItemData>.EnsureItem(
  Key: TKey): TAvlTreeItem<TKey, TItemData>;
var
  t: TAvlTreeItem<TKey, TItemData>;
begin
  t := SearchItem(Key);
  if not Assigned(t) then
    Result := AddItem(Key)
  else
    Result := t;
end;

function TAvlTree<TKey, TItemData>.Height: integer;
begin
  Result:=FRoot.SafeGetHeight;
end;

procedure TAvlTree<TKey, TItemData>.InternalAfterLinkOut(
  Item: TAvlTreeItem<TKey, TItemData>);
var
  parent: TAvlTreeItem<TKey, TItemData>;
begin
  parent := item.FParent;
  Item.FTree := nil;
  Item.FParent := nil;
  Item.FLeft := nil;
  Item.FRight := nil;
  SetHeightUpper(parent);
  Adjust(parent);
  Dec(FCount);
end;

function TAvlTree<TKey, TItemData>.InternalMax(
  Root: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>;
begin
  if not Assigned(Root) then
    Exit(nil);

  Result := Root;
  while assigned(Result.FRight) do
    Result := Result.FRight;
end;

function TAvlTree<TKey, TItemData>.InternalMin(
  Root: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>;
begin
  if not Assigned(Root) then
    Exit(nil);

  Result := Root;
  while assigned(Result.FLeft) do
    Result := Result.FLeft;
end;

function TAvlTree<TKey, TItemData>.InternalNext(
  Item: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>;
var
  curr: TAvlTreeItem<TKey, TItemData>;
begin
  if not Assigned(Item) then
    Exit(nil);

  if Assigned(Item.FRight) then
    Exit(InternalMin(Item.FRight));

  curr:=Item;
  while assigned(curr.FParent) and (curr.FParent.FRight = curr) do
  begin
    curr:=curr.FParent;
  end;
  Result:=curr.FParent;
end;

function TAvlTree<TKey, TItemData>.InternalPrev(
  Item: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>;
var
  curr: TAvlTreeItem<TKey, TItemData>;
begin
  if not Assigned(Item) then
    Exit(nil);

  if Assigned(Item.FLeft) then
    Exit(InternalMax(Item.FLeft));

  curr:=Item;
  while assigned(curr.FParent) and (curr.FParent.FLeft = curr) do
  begin
    curr:=curr.FParent;
  end;
  Result:=curr.FParent;
end;

function TAvlTree<TKey, TItemData>.InternalSearchItem(Key: TKey;
  out Parent: TAvlTreeItem<TKey, TItemData>): TAvlTreeItem<TKey, TItemData>;
var
  CurrItem: TAvlTreeItem<TKey, TItemData>;
begin
  Parent := nil;
  CurrItem := FRoot;
  while Assigned(CurrItem) and (FComparer.Compare(CurrItem.Key, Key) <> 0) do
  begin
    Parent := CurrItem;
    if FComparer.Compare(Key, CurrItem.Key) < 0 then
      CurrItem := CurrItem.FLeft
    else
      CurrItem := CurrItem.FRight;
  end;
  Result := CurrItem;
end;

class procedure TAvlTree<TKey, TItemData>.InternalSwapItem(var item1,
  item2: TAvlTreeItem<TKey, TItemData>);
var
  temp: TAvlTreeItem<TKey, TItemData>;
begin
  temp := item1;
  item1 := item2;
  item2 := temp;
end;

function TAvlTree<TKey, TItemData>.IsIn(Key: TKey): boolean;
begin
  Result:=Assigned(SearchItem(Key));
end;

procedure TAvlTree<TKey, TItemData>.LinkIn(Item,
  parent: TAvlTreeItem<TKey, TItemData>);
begin
  if not Assigned(parent) then
  begin
    FRoot := Item;
  end else begin
    if FComparer.Compare(Item.Key, parent.Key) < 0 then
      parent.FLeft := Item
    else
      parent.FRight := Item;
  end;
  Item.FParent := parent;

  SetHeightUpper(Item);
  Adjust(Item.FParent);
  Inc(FCount);
  Item.FLinkedIn := True;
end;

procedure TAvlTree<TKey, TItemData>.LinkOut(
  Item: TAvlTreeItem<TKey, TItemData>);
var
  parent: TAvlTreeItem<TKey, TItemData>;
begin
  if not assigned(Item) then
    Exit;
  if not Item.FLinkedIn then
    Exit;

  parent := Item.FParent;
  if Item.IsLeaf then
  begin
    ChangeMyParentLinkTo(Item, nil);
    InternalAfterLinkOut(Item);
  end else if not assigned(Item.FRight) then
  begin
    ChangeMyParentLinkTo(Item, Item.FLeft);
    Item.FLeft.FParent := parent;
    InternalAfterLinkOut(Item);
  end else if not assigned(Item.FLeft) then
  begin
    ChangeMyParentLinkTo(Item, Item.FRight);
    Item.FRight.FParent := parent;
    InternalAfterLinkOut(Item);
  end else // both children
  begin
    SwapItem(Item.Next, Item);
    LinkOut(Item);
  end;
end;

function TAvlTree<TKey, TItemData>.Max: TAvlTreeItem<TKey, TItemData>;
begin
  Result := InternalMax(FRoot);
end;

function TAvlTree<TKey, TItemData>.Min: TAvlTreeItem<TKey, TItemData>;
begin
  Result := InternalMin(FRoot);
end;

procedure TAvlTree<TKey, TItemData>.RotateL(
  Parent: TAvlTreeItem<TKey, TItemData>);
var
  child: TAvlTreeItem<TKey, TItemData>;
  grandparent: TAvlTreeItem<TKey, TItemData>;
  b: TAvlTreeItem<TKey, TItemData>;
begin
  child := parent.FRight;
  if assigned(child) then
  begin
    grandparent := parent.FParent;
    b := child.FLeft;

    child.FLeft := parent;
    child.FParent := grandparent;
    parent.FParent := child;
    parent.FRight := b;
    if Assigned(b) then
      b.FParent := parent;

    if Assigned(grandparent) then
    begin
      if grandparent.FLeft = parent then
        grandparent.FLeft := child
      else
        grandparent.FRight := child
    end else
      FRoot := child;
    SetHeightUpper(parent);
  end;
end;

procedure TAvlTree<TKey, TItemData>.RotateR(
  Parent: TAvlTreeItem<TKey, TItemData>);
var
  child: TAvlTreeItem<TKey, TItemData>;
  grandparent: TAvlTreeItem<TKey, TItemData>;
  b: TAvlTreeItem<TKey, TItemData>;
begin
  child := parent.FLeft;
  if Assigned(child) then
  begin
    grandparent := parent.FParent;
    b := child.FRight;

    child.FRight := parent;
    child.FParent := grandparent;
    parent.FParent := child;
    parent.FLeft := b;
    if Assigned(b) then
      b.FParent := parent;

    if Assigned(grandparent) then
    begin
      if grandparent.FLeft = parent then
        grandparent.FLeft := child
      else
        grandparent.FRight := child
    end else
      FRoot := child;
    SetHeightUpper(parent);
  end;
end;

function TAvlTree<TKey, TItemData>.SearchItem(
  Key: TKey): TAvlTreeItem<TKey, TItemData>;
var
  parent: TAvlTreeItem<TKey, TItemData>;
begin
  Result:=InternalSearchItem(Key, Parent);
end;

procedure TAvlTree<TKey, TItemData>.SetHeightUpper(
  item: TAvlTreeItem<TKey, TItemData>);
begin
  while Assigned(Item) do
  begin
    item.SetHeight;
    Item:=Item.FParent;
  end;
end;

procedure TAvlTree<TKey, TItemData>.SetOwnsObjects(const Value: boolean);
begin
  FOwnsObjects := Value;
end;

procedure TAvlTree<TKey, TItemData>.SwapItem(Item1,
  Item2: TAvlTreeItem<TKey, TItemData>);
begin
  ChangeMyParentLinkTo(item1, item2);
  ChangeMyParentLinkTo(item2, item1);

  InternalSwapItem(item1.FParent, item2.FParent);
  InternalSwapItem(item1.FLeft, item2.FLeft);
  InternalSwapItem(item1.FRight, item2.FRight);

  if assigned(item2.FLeft) then
    item2.FLeft.FParent := item2;
  if assigned(item2.FRight) then
    item2.FRight.FParent := item2;
  if assigned(item1.FLeft) then
    item1.FLeft.FParent := item1;
  if assigned(item1.FRight) then
    item1.FRight.FParent := item1;
end;

{ TAvlTreeItem<TKey, TData> }

constructor TAvlTreeItem<TKey, TData>.Create(ATree: TAvlTree<TKey, TData>;
  AKey: TKey);
begin
  inherited Create;
  FTree := nil;
  FKey := AKey;
  FParent := nil;
  FLeft := nil;
  FRight := nil;
  FHeight := 1;
  if assigned(ATree) then
    ATree.AddItem(Self);
end;

destructor TAvlTreeItem<TKey, TData>.Destroy;
var
  FDestoryItemData: TDestoryMethod<TData>;
begin
  FDestoryItemData := nil;

  if Assigned(FTree) then
    FTree.LinkOut(Self);
    FDestoryItemData := FTree.FDestoryItemData;

  if Assigned(FDestoryItemData) then
    FDestoryItemData(Data);

  inherited;
end;

function TAvlTreeItem<TKey, TData>.SafeGetHeight: integer;
begin
  if Assigned(Self) then
    Exit(Height);
  Result := 0;
end;

function TAvlTreeItem<TKey, TData>.IsLeaf: boolean;
begin
  Result:=(not Assigned(FLeft)) and (not Assigned(FRight));
end;

function TAvlTreeItem<TKey, TData>.Next: TAvlTreeItem<TKey, TData>;
begin
  Result:=FTree.InternalNext(self);
end;

function TAvlTreeItem<TKey, TData>.Prev: TAvlTreeItem<TKey, TData>;
begin
  Result:=FTree.InternalPrev(self);
end;

procedure TAvlTreeItem<TKey, TData>.SetData(const Value: TData);
begin
  FData := Value;
end;

function TAvlTreeItem<TKey, TData>.SetHeight: boolean;
var
  oldHeight: integer;
begin
  oldHeight := FHeight;
  FHeight := 1 + Max(FRight.SafeGetHeight, FLeft.SafeGetHeight);
  Result := FHeight<>oldHeight;
end;

end.
