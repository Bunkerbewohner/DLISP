unit Data;

interface

uses
  System.StrUtils,
  Classes,
  SysUtils,
  Memory,
  System.Generics.Collections,
  System.RegularExpressions,
  Math;

var
  FormatSettings : TFormatSettings;

type

  TData = class abstract(TInterfacedObject)
    private
      class var FInstances : TList<TData>;

    public
      Value : string;

      ConsumedCharacters : Integer;
      constructor Create();
      destructor Destroy(); override;

      function ValueEquals(b : TData) : Boolean; virtual;

      function Copy() : TData; virtual; abstract;

      procedure Release();
      function ToQualifiedString() : string;
  end;

  TNothing = class(TData)
    public
      constructor Create();

      function Copy() : TData; override;

      function ToString : string; override;
  end;

  TAtom = class(TData)
    public
      constructor Create(v : string);
      function Copy() : TData; override;

      function ToString : string; override;
  end;

  TList = class;
  DataRef = Ref<TData>;

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

  TSymbol = class(TAtom)
    public

      function Copy() : TData; override;
      class function Match(v : string) : Boolean;

  end;

  TString = class(TAtom)
    public
      function Copy() : TData; override;
      function ToString : string; override;
  end;

  TBoolean = class(TAtom)
    public
      BoolValue : Boolean;

      constructor Create(v : string); overload;
      constructor Create(v : Boolean); overload;
      function Copy() : TData; override;

      class function Match(v : string) : Boolean;

      function ToString() : string; override;
  end;

  TNumber = class(TAtom)
    public
      function ToInteger() : Integer; virtual; abstract;
      function ToSingle() : Single; virtual; abstract;

      function Plus(b : TNumber) : TNumber; virtual; abstract;
      function Minus(b : TNumber) : TNumber; virtual; abstract;
      function Multiply(b : TNumber) : TNumber; virtual; abstract;
      function Divide(b : TNumber) : TNumber; virtual; abstract;

      function Compare(b : TNumber) : TNumber; virtual; abstract;
  end;

  TInteger = class(TNumber)
    public
      IntValue : Integer;

      constructor Create(v : string); overload;
      constructor Create(v : Integer); overload;

      function ToInteger() : Integer; override;
      function ToSingle() : Single; override;

      function Copy() : TData; override;

      function Plus(b : TNumber) : TNumber; override;
      function Minus(b : TNumber) : TNumber; override;
      function Multiply(b : TNumber) : TNumber; override;
      function Divide(b : TNumber) : TNumber; override;

      function Compare(b : TNumber) : TNumber; override;

      class function Match(v : string) : Boolean;
  end;

  TFloat = class(TNumber)
    public
      FloatValue : Single;

      constructor Create(v : string); overload;
      constructor Create(v : Single); overload;

      function ToInteger() : Integer; override;
      function ToSingle() : Single; override;

      function Copy() : TData; override;

      function Plus(b : TNumber) : TNumber; override;
      function Minus(b : TNumber) : TNumber; override;
      function Multiply(b : TNumber) : TNumber; override;
      function Divide(b : TNumber) : TNumber; override;

      function Compare(b : TNumber) : TNumber; override;

      class function Match(v : string) : Boolean;
  end;

  TAbstractObject = class abstract(TAtom)

  end;

  /// <summary>
  /// An abstract object implementation wrapping a native Delphi class instance.
  /// </summary>
  TDelphiObject = class(TAbstractObject)
    protected
      FObject : TObject;
      FOwned : Boolean;

    public
      /// <summary>Creates a Lisp object that owns the contained delphi object
      /// </summary>
      constructor CreateOwned(obj : TObject);
      constructor Create(obj : TObject);
      destructor Destroy(); override;

      property Content : TObject read FObject;

  end;

  /// <summary>
  /// An abstract object implementation for DLISP.
  /// </summary>
  TLispObject = class(TAbstractObject)

  end;

  TList = class(TData)
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
  end;

  ListRef = Ref<TList>;

  /// <summary>
  /// Hierarchical program execution context. Contains a symbol reference table
  /// and an optional reference to a parent context. Reads will search the tree
  /// upwards. Writes always only apply the current context.
  /// </summary>
  TContext = class
    protected
      FParent : TContext;
      FSymbols : TDictionary<string, DataRef>;

      function GetSymbol(name : string) : Ref<TData>; virtual;
      procedure SetSymbol(name : string; Data : Ref<TData>); virtual;

    public
      ///<summary>Creates a new context optionally chained to given parent context.</summary>
      constructor Create(parent : TContext);
      destructor Destroy(); override;

      ///<summary>Symbols defined in this context. Can be used for reading
      ///symbols in this and parent contexts, and writing into this context.
      ///</summary>
      property Symbols[name : string] : Ref<TData>
        read GetSymbol write SetSymbol; default;

      ///<summary>Return true if the symbol is defined in this context or
      ///in one of its ancestors.</summary>
      function IsDefined(name : string) : Boolean; virtual;

      ///<summary>Looks up a TDelphiObject wrapper and returns the contained
      ///native delphi object.</summary>
      function GetDelphiObject<T : class>(name : string) : T;

      ///<summary>Imports all symbols from another context into this context.
      ///All symbol names will be prefixed with <prefix>.</summary>
      procedure Import(context : TContext; prefix : string); virtual;

      ///<summary>Removes a symbol reference from this context.</summary>
      procedure Remove(name : string); virtual;
  end;

  /// <summary>
  /// Dual context that uses an external primary and secondary context for
  /// looking up symbols. This context is read-only.
  /// </summary>
  TDualLookupContext = class(TContext)
    protected
      FSecondary : TContext;
      FPrimary : TContext;

      procedure SetSymbol(name : string; Data : Ref<TData>); override;

    public
      constructor Create(primary : TContext; secondary : TContext);

      function GetSymbol(name : string) : Ref<TData>; override;
      function IsDefined(name : string) : Boolean; override;

      procedure Import(context : TContext; prefix : string); override;
      procedure Remove(name : string); override;
  end;

  ///<summary>An abstract function</summary>
  TFunction = class abstract(TAtom)
    protected
      FContext : TContext;

    public

  end;

  TUserFunction = class(TFunction)
    protected
      FCode : Ref<TList>;
      FArgs : Ref<TList>;

    public
      constructor Create(code : Ref<TList>; parentContext : TContext);
      destructor Destroy(); override;

      property context : TContext read FContext;
      property Args : Ref<TList> read FArgs;
      property code : Ref<TList> read FCode;

      function Copy() : TData; override;

      function ToString : string; override;
  end;

function CreateListRef(Data : TList) : DataRef;
function CreateRef(Data : TData) : DataRef;

implementation

function CreateRef(Data : TData) : DataRef;
begin
  Result := TRef<TData>.Create(Data);
end;

function CreateListRef(Data : TList) : DataRef;
begin
  Result := TRef<TData>.Create(Data);
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

function TList.ValueEquals(b : TData) : Boolean;
var
  other : TList;
  I : Integer;
begin
  other := b as TList;
  Result := True;

  for I := 0 to Min(Size - 1, other.Size - 1) do
  begin
    if not Items[I]().ValueEquals(other[I]()) then
        Exit(False);
  end;
end;

{ TParseString }

function TAtom.Copy : TData;
begin
  Result := TAtom.Create(Value);
end;

constructor TAtom.Create(v : string);
begin
  inherited Create();
  Value := v;
end;

function TAtom.ToString : string;
begin
  Result := Value;
end;

{ TContext }

constructor TContext.Create(parent : TContext);
begin
  FParent := parent;
  FSymbols := TDictionary<string, DataRef>.Create();
end;

destructor TContext.Destroy;
begin
  inherited;
  FSymbols.Free;
end;

function TContext.GetDelphiObject<T>(name : string) : T;
begin
  Result := (GetSymbol(name)() as TDelphiObject).Content as T;
end;

function TContext.GetSymbol(name : string) : Ref<TData>;
begin
  if FSymbols.ContainsKey(name) then
  begin
    Result := FSymbols[name];
  end
  else if FParent <> nil then
  begin
    Result := FParent.GetSymbol(name);
  end
  else
  begin
    raise Exception.Create('Symbol "' + name + '" unknown');
  end;
end;

procedure TContext.Import(context: TContext; prefix: string);
var
  symbol : string;
begin
  for symbol in context.FSymbols.Keys do
  begin
    SetSymbol(prefix + symbol, context[symbol]);
  end;
end;

function TContext.IsDefined(name : string) : Boolean;
begin
  Result := FSymbols.ContainsKey(name);
  if (not Result) and (FParent <> nil) then
      Result := FParent.IsDefined(name);
end;

procedure TContext.Remove(name : string);
begin
  FSymbols.Remove(name);
end;

procedure TContext.SetSymbol(name : string; Data : Ref<TData>);
begin
  if FSymbols.ContainsKey(name) then
  begin
    FSymbols[name] := DataRef(Data);
  end
  else
  begin
    FSymbols.Add(name, DataRef(Data));
  end;
end;

{ TInteger }

constructor TInteger.Create(v : string);
begin
  inherited;
  IntValue := StrToInt(v);
end;

function TInteger.Compare(b : TNumber) : TNumber;
var
  I : Integer;
begin
  if b is TInteger then
      I := Sign(IntValue - (b as TInteger).IntValue)
  else if b is TFloat then
      I := Sign(IntValue - (b as TFloat).FloatValue)
  else
      raise Exception.Create('unsupported operand');

  Result := TInteger.Create(I);
end;

function TInteger.Copy : TData;
begin
  Result := TInteger.Create(IntValue);
end;

constructor TInteger.Create(v : Integer);
begin
  IntValue := v;
  Value := IntToStr(v);
end;

function TInteger.Divide(b : TNumber) : TNumber;
begin
  if b is TFloat then
      Result := TFloat.Create(IntValue / (b as TFloat).FloatValue)
  else if b is TInteger then
      Result := TInteger.Create(IntValue div (b as TInteger).IntValue)
  else
      raise Exception.Create('Unsupported operand');
end;

class function TInteger.Match(v : string) : Boolean;
begin
  Result := TRegEx.IsMatch(v, '^[-]?\d+$');
end;

function TInteger.Minus(b : TNumber) : TNumber;
begin
  if b is TFloat then
      Result := TFloat.Create(IntValue - (b as TFloat).FloatValue)
  else if b is TInteger then
      Result := TInteger.Create(IntValue - (b as TInteger).IntValue)
  else
      raise Exception.Create('Unsupported operand');
end;

function TInteger.Multiply(b : TNumber) : TNumber;
begin
  if b is TFloat then
      Result := TFloat.Create(IntValue * (b as TFloat).FloatValue)
  else if b is TInteger then
      Result := TInteger.Create(IntValue * (b as TInteger).IntValue)
  else
      raise Exception.Create('Unsupported operand');
end;

function TInteger.Plus(b : TNumber) : TNumber;
begin
  if b is TFloat then
      Result := TFloat.Create(IntValue + (b as TFloat).FloatValue)
  else if b is TInteger then
      Result := TInteger.Create(IntValue + (b as TInteger).IntValue)
  else
      raise Exception.Create('Unsupported operand');
end;

function TInteger.ToInteger : Integer;
begin
  Result := IntValue;
end;

function TInteger.ToSingle : Single;
begin
  Result := IntValue;
end;

{ TSingle }

constructor TFloat.Create(v : string);
begin
  inherited;
  FloatValue := StrToFloat(v, FormatSettings);
end;

function TFloat.Compare(b : TNumber) : TNumber;
var
  I : Integer;
begin
  if b is TInteger then
      I := Sign(FloatValue - (b as TInteger).IntValue)
  else if b is TFloat then
      I := Sign(FloatValue - (b as TFloat).FloatValue)
  else
      raise Exception.Create('unsupported operand');

  Result := TInteger.Create(I);
end;

function TFloat.Copy : TData;
begin
  Result := TFloat.Create(FloatValue);
end;

constructor TFloat.Create(v : Single);
begin
  FloatValue := v;
  Value := FloatToStr(v, FormatSettings);
end;

function TFloat.Divide(b : TNumber) : TNumber;
begin
  if b is TInteger then
      Result := TFloat.Create(FloatValue / ((b as TInteger).IntValue))
  else if b is TFloat then
      Result := TFloat.Create(FloatValue / (b as TFloat).FloatValue)
  else
      raise Exception.Create('Unsupported operand');
end;

class function TFloat.Match(v : string) : Boolean;
begin
  Result := TRegEx.IsMatch(v, '^[-]?\d+\.\d+');
end;

function TFloat.Minus(b : TNumber) : TNumber;
begin
  if b is TInteger then
      Result := TFloat.Create(FloatValue - (b as TInteger).IntValue)
  else if b is TFloat then
      Result := TFloat.Create(FloatValue - (b as TFloat).FloatValue)
  else
      raise Exception.Create('Unsupported operand');
end;

function TFloat.Multiply(b : TNumber) : TNumber;
begin
  if b is TInteger then
      Result := TFloat.Create(FloatValue * (b as TInteger).IntValue)
  else if b is TFloat then
      Result := TFloat.Create(FloatValue * (b as TFloat).FloatValue)
  else
      raise Exception.Create('Unsupported operand');
end;

function TFloat.Plus(b : TNumber) : TNumber;
begin
  if b is TInteger then
      Result := TFloat.Create(FloatValue + (b as TInteger).IntValue)
  else if b is TFloat then
      Result := TFloat.Create(FloatValue + (b as TFloat).FloatValue)
  else
      raise Exception.Create('Unsupported operand');
end;

function TFloat.ToInteger : Integer;
begin
  Result := Round(FloatValue);
end;

function TFloat.ToSingle : Single;
begin
  Result := FloatValue;
end;

{ TSymbol }

function TSymbol.Copy : TData;
begin
  Result := TSymbol.Create(Value);
end;

class function TSymbol.Match(v : string) : Boolean;
begin
  Result := TRegEx.IsMatch(v, '^[\D][\S]*$');
end;

{ TBoolean }

function TBoolean.Copy : TData;
begin
  Result := TBoolean.Create(Value);
end;

constructor TBoolean.Create(v : string);
begin
  inherited;
  BoolValue := StrToBool(v);
end;

constructor TBoolean.Create(v : Boolean);
begin
  BoolValue := v;
  Value := BoolToStr(v);
end;

class function TBoolean.Match(v : string) : Boolean;
var
  lower : string;
begin
  lower := v.ToLower;
  Result := lower.Equals('true') or lower.Equals('false');
end;

function TBoolean.ToString : string;
begin
  if BoolValue then Result := 'True'
  else Result := 'False'
end;

{ TData }

constructor TData.Create;
begin
  Value := '()';
end;

destructor TData.Destroy;
begin
  inherited;
end;

procedure TData.Release;
begin
  Self._Release;
end;

function TData.ToQualifiedString : string;
begin
  Result := Self.ClassName + '[' + Self.ToString + ']';
end;

function TData.ValueEquals(b : TData) : Boolean;
begin
  Result := Self.Value.Equals(b.Value);
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

{ TNothing }

function TNothing.Copy : TData;
begin
  Result := TNothing.Create();
end;

constructor TNothing.Create;
begin
  Value := '';
end;

function TNothing.ToString : string;
begin
  Result := 'TNothing';
end;

{ TFunction }

function TUserFunction.Copy : TData;
var
  fn : TUserFunction;
begin
  fn := TUserFunction.Create(FCode, FContext.FParent);
  Result := fn;
end;

constructor TUserFunction.Create(code : Ref<TList>; parentContext : TContext);
var
  Args, list : TList;
  I : Integer;
begin
  FCode := code;
  FContext := TContext.Create(parentContext);

  // (fn [p1 p2 & ps] (...) (...))
  Assert((code[0]() is TSymbol));
  Assert(code[1]() is TList);
  Assert(code.Size > 2);

  // First argument is a list of symbols for arguments
  // FArgs := TRef<TList>.Create(code[1]() as TList);
  list := code[1]() as TList;
  Args := TList.Create();
  for I := 1 to list.Size - 1 do
      Args.Add(list[I]);

  FArgs := TRef<TList>.Create(Args);
end;

destructor TUserFunction.Destroy;
begin
  FContext.Free;
  inherited;
end;

function TUserFunction.ToString : string;
begin
  Result := FCode.ToString;
end;

{ TString }

function TString.Copy : TData;
begin
  Result := TString.Create(Value);
end;

function TString.ToString : string;
begin
  Result := '"' + Value + '"';
end;

{ TObject }

constructor TDelphiObject.Create(obj : TObject);
begin
  FObject := obj;
  FOwned := False;
end;

constructor TDelphiObject.CreateOwned(obj : TObject);
begin
  FObject := obj;
  FOwned := True;
end;

destructor TDelphiObject.Destroy;
begin
  if FOwned then
      FObject.Free;
  inherited;
end;

{ TScopedContext }

constructor TDualLookupContext.Create(primary, secondary : TContext);
begin
  inherited Create(Nil);
  FSecondary := secondary;
  FPrimary := primary;
end;

function TDualLookupContext.GetSymbol(name : string) : Ref<TData>;
begin
  if FPrimary.IsDefined(name) then
      Result := FPrimary[name]
  else
      Result := FSecondary[name];
end;

procedure TDualLookupContext.Import(context: TContext; prefix: string);
begin
  raise Exception.Create('read-only');
end;

function TDualLookupContext.IsDefined(name : string) : Boolean;
begin
  Result := FPrimary.IsDefined(name) or FSecondary.IsDefined(name);
end;

procedure TDualLookupContext.Remove(name: string);
begin
  raise Exception.Create('read-only');
end;

procedure TDualLookupContext.SetSymbol(name: string; Data: Ref<TData>);
begin
  raise Exception.Create('read-only');
end;

initialization

FormatSettings := TFormatSettings.Create('en_US');

end.
