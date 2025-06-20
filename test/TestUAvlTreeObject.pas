unit TestUAvlTreeObject;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework, System.Generics.Defaults, SysUtils, UAvlTree;

const
  LOG_MESSAGES = False;

type
  // Test methods for class TAvlTree
  TTestObject = class
  public
    TestData: string;
    constructor Create;
    destructor Destroy; override;
  end;

  TestTAvlTree = class(TTestCase)
  strict private
    FAvlTree: TAvlTree<integer, TTestObject>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestObjectBehaviour;
  end;

implementation

uses
  Classes;

type
  TTestObjectTracker = class
  strict private
    class var FInstance: TTestObjectTracker;
    constructor Create;
  public
    List: TStrings;
    TestObjectCount: Integer;
    class function Instance: TTestObjectTracker;
    procedure Log(msg: string);
    destructor Destroy; override;
  end;

var
  TestObjectMessages: TStrings;

procedure TestTAvlTree.SetUp;
begin
  FAvlTree := TAvlTree<integer, TTestObject>.Create(
      TComparer<Integer>.Default,
      function(): TTestObject
      begin
        Result := TTestObject.Create;
      end,
      procedure (o: TTestObject)
      begin
        o.Free;
      end);
  FAvlTree.OwnsObjects := True;
end;

procedure TestTAvlTree.TearDown;
begin
  FAvlTree.Free;
  FAvlTree := nil;
end;

procedure TestTAvlTree.TestObjectBehaviour;

  function TestDataStr: string;
  var
    i: TAvlTreeItem<integer, TTestObject>;
  begin
    Result := '';
    i := FAvlTree.Min;
    while Assigned(i) do
    begin
      Result := Result + i.Data.TestData;
      i := i.Next;
    end;
  end;

var
  i: TAvlTreeItem<integer, TTestObject>;
  log_msg: string;
begin
  try
    FAvlTree.AddItem(58);
    FAvlTree.AddItem(51);
    FAvlTree.AddItem(1);
    FAvlTree.AddItem(12);
    FAvlTree.AddItem(15);

    CheckEquals(TTestObjectTracker.Instance.TestObjectCount, 5);

    FAvlTree.DeleteItem(FAvlTree.SearchItem(1));
    CheckEquals(TTestObjectTracker.Instance.TestObjectCount, 4);

    i := TAvlTreeItem<integer, TTestObject>.Create(FAvlTree, 2);
    i.Data := TTestObject.Create;
    i.Data.TestData := 'x';

    CheckEquals(TestDataStr, 'x');
    FAvlTree.SearchItem(12).Data.TestData := 'y';
    CheckEquals(TestDataStr, 'xy');

    FAvlTree.DeleteItem(FAvlTree.SearchItem(12));
    CheckEquals(TTestObjectTracker.Instance.TestObjectCount, 4);

    FAvlTree.Clear;
    CheckEquals(TTestObjectTracker.Instance.TestObjectCount, 0);
  finally
    while TTestObjectTracker.Instance.List.Count > 0 do
    begin
      log_msg := TTestObjectTracker.Instance.List[0];
      TTestObjectTracker.Instance.List.Delete(0);
      if LOG_MESSAGES then
        Status(log_msg)
    end;
  end;
end;

{ TTestObject }

constructor TTestObject.Create;
begin
  TTestObjectTracker.Instance.Log('Object created. ' + IntToStr(Integer(self)));
  inc(TTestObjectTracker.Instance.TestObjectCount);
end;

destructor TTestObject.Destroy;
begin
  TTestObjectTracker.Instance.Log('Object destroyed. ' + IntToStr(Integer(self)));
  dec(TTestObjectTracker.Instance.TestObjectCount);
  inherited;
end;

{ TTestObjectTracker }

constructor TTestObjectTracker.Create;
begin
  List := TStringList.Create;
end;

destructor TTestObjectTracker.Destroy;
begin
  FInstance := nil;
  List.Free;
  inherited;
end;

procedure TTestObjectTracker.Log(msg: string);
begin
  List.Add(msg);
end;

class function TTestObjectTracker.Instance: TTestObjectTracker;
begin
  if FInstance = nil then
    FInstance := TTestObjectTracker.Create;
  Result := FInstance;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTAvlTree.Suite);
  TestObjectMessages := TStringList.Create;

finalization
  FreeAndNil(TestObjectMessages);

end.

