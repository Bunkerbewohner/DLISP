unit Collections;

interface

uses
  System.SysUtils,
  Math,
  System.Generics.Collections,
  System.Generics.Defaults,
  Data,
  Interfaces,
  Rtti;

type

  TList = class(TData, ICountable)
    protected
      Items : TList<DataRef>;

      function GetSize() : Integer;
      function GetItem(n : Integer) : DataRef;

    public

      Executable : Boolean;

      property Size : Integer read GetSize;
      property DataItems[n : Integer] : DataRef read GetItem; default;

      procedure Add(item : DataRef);

      function ValueEquals(b : TData) : Boolean; override;

      constructor Create(); overload;
      constructor Create(Items : array of DataRef); overload;
      destructor Destroy(); override;

      function Copy() : TData; override;

      function GetEnumerator : TEnumerator<DataRef>;

      function ToString : string; override;
      function ToTValue() : TValue; override;

      function Count : Integer;
  end;

  TLispListEnumerator = class(TEnumerator<DataRef>)
    private
      FList : TList;
      FCurrent : Ref<TData>;
      FIndex : Integer;

    public
      constructor Create(list : TList);

      function DoGetCurrent : Ref<TData>; override;
      function DoMoveNext : Boolean; override;
  end;

  ListRef = Ref<TList>;

  DataComparer = class(TInterfacedObject, IEqualityComparer<DataRef>)
    function Equals(const Left, Right : DataRef) : Boolean; reintroduce;
    function GetHashCode(const Value : DataRef) : Integer; reintroduce;
  end;

  TDictionary = class(TData)
    protected
      FContent : TDictionary<DataRef, DataRef>;

    public
      constructor Create(); overload;
      constructor Create(list : TList); overload;
      destructor Destroy(); override;

      function Contains(key : DataRef) : Boolean;
      procedure Add(key : DataRef; Value : DataRef);
      function Get(key : DataRef) : DataRef;

      function ToString : string; override;
      function Copy() : TData; override;
  end;

function CreateListRef(Data : TList) : DataRef;

implementation

function CreateListRef(Data : TList) : DataRef;
begin
  Result := TRef<TData>.Create(Data);
end;

{ TLispListEnumerator }

constructor TLispListEnumerator.Create(list : TList);
begin
  Self.FList := list;
  Self.FCurrent := nil;
  Self.FIndex := 0;
end;

function TLispListEnumerator.DoGetCurrent : Ref<TData>;
begin
  Result := FList[FIndex];
end;

function TLispListEnumerator.DoMoveNext : Boolean;
begin
  Result := FIndex < FList.Size;
end;

{ TParseList }

procedure TList.Add(item : Ref<TData>);
begin
  Items.Add(item);
end;

function TList.Copy : TData;
var
  item : DataRef;
  list : TList;
begin
  list := TList.Create();
  for item in Items do
  begin
    list.Add(CreateRef(item().Copy()));
  end;
  Result := list;
end;

function TList.Count : Integer;
begin
  Result := Items.Count;
end;

constructor TList.Create(Items : array of DataRef);
var
  I : Integer;
begin
  inherited Create();
  Self.Items := TList<DataRef>.Create();

  for I := 0 to Length(Items) - 1 do
      Add(Items[I]);
end;

constructor TList.Create;
begin
  inherited;
  Items := TList<DataRef>.Create();
  Executable := true;
end;

destructor TList.Destroy;
begin
  Items.Free;
  inherited;
end;

function TList.GetEnumerator : TEnumerator<Ref<TData>>;
begin
  Result := TLispListEnumerator.Create(Self);
end;

function TList.GetItem(n : Integer) : Ref<TData>;
begin
  Result := Ref<TData>(Items[n]);
end;

function TList.GetSize : Integer;
begin
  Result := Items.Count;
end;

function TList.ToString : string;
var
  I : Integer;
  DataRef : Ref<TData>;
  Data : TData;
begin
  Result := '(';
  for I := 0 to Items.Count - 1 do
  begin
    if I > 0 then Result := Result + ' ';
    DataRef := Ref<TData>(Items[I]);
    Data := DataRef();
    Result := Result + Data.ToString;
  end;
  Result := Result + ')';
end;

function TList.ToTValue : TValue;
var
  list : TList<TValue>;
  I : Integer;
begin
  list := TList<TValue>.Create();
  for I := 0 to Self.Size - 1 do
      list[I] := Items[I]().ToTValue();

  Result := TValue.From(list);
end;

function TList.ValueEquals(b : TData) : Boolean;
var
  other : TList;
  I : Integer;
begin
  other := b as TList;
  Result := true;

  for I := 0 to Min(Size - 1, other.Size - 1) do
  begin
    if not Items[I]().ValueEquals(other[I]()) then
        Exit(False);
  end;
end;

{ TDictionary }

procedure TDictionary.Add(key : DataRef; Value : DataRef);
begin
  FContent.AddOrSetValue(key, Value);
end;

constructor TDictionary.Create;
begin
  FContent := TDictionary<DataRef, DataRef>.Create(DataComparer.Create());
end;

constructor TDictionary.Create(list : TList);
var
  I : Integer;
begin
  Create();
  Assert(list.Size = 2);
  I := 0;

  while I < list.Size do
  begin
    FContent.Add(list[I], list[I + 1]);
    I := I + 2;
  end;

end;

destructor TDictionary.Destroy;
begin
  FContent.Free;
  inherited;
end;

function TDictionary.Contains(key : DataRef) : Boolean;
begin
  Result := FContent.ContainsKey(key);
end;

function TDictionary.Copy: TData;
var
  dict : TDictionary;
  pair: TPair<DataRef,DataRef>;
begin
  dict := TDictionary.Create();
  for pair in FContent do
  begin
    dict.Add(pair.Key, pair.Value);
  end;

  Result := dict;
end;

function TDictionary.Get(key : DataRef) : DataRef;
begin
  Result := FContent[key];
end;

function TDictionary.ToString : string;
var
  I : Integer;
  pair : TPair<DataRef, DataRef>;
begin
  Result := '{ ';
  for pair in FContent do
  begin
    Result := Result + pair.key().ToString + ' ' + pair.Value().ToString + ', ';
  end;

  Result := Result + '}';
end;

{ DataComparer }

function DataComparer.Equals(const Left, Right : DataRef) : Boolean;
begin
  Result := Left().ValueEquals(Right());
end;

function DataComparer.GetHashCode(const Value : DataRef) : Integer;
begin
  Result := Value().GetHashCode;
end;

end.
