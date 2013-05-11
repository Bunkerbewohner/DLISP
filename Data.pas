unit Data;

interface

uses
  System.StrUtils,
  Classes,
  SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.RegularExpressions,
  Math,
  Interfaces,
  Rtti;

var
  FormatSettings : TFormatSettings;

type

  // <summary>Smart Pointer</summary>
  Ref<T> = reference to function : T;

  TSymbol = class;

  /// <summary>
  /// Implementation of a smart pointer (interface Ref<T>).
  /// </summary>
  TRef<T : class, constructor> = class(TInterfacedObject, Ref<T>)
    private
      FValue : T;

    public
      /// <summary>Creates a new reference pointing to a new instance of T.</summary>
      constructor Create(); overload;
      /// <summary>Creates a new reference pointing to val.</summary>
      constructor Create(val : T); overload;
      destructor Destroy(); override;

      function Invoke : T;

      function AsSymbol : TSymbol;

  end;

  ISomething = interface(IInterface)
    function ToQualifiedString() : string;
  end;

  TData = class abstract(TInterfacedObject, ISomething)
    public
      Value : string;

      ConsumedCharacters : Integer;
      constructor Create();
      destructor Destroy(); override;

      function ValueEquals(b : TData) : Boolean; virtual;

      function Copy() : TData; virtual; abstract;

      procedure Release();
      function ToQualifiedString() : string;

      function ToTValue() : TValue; virtual; abstract;
  end;

  TNothing = class(TData)
    private
      class var FInstance : TNothing;
      class var FReference : Ref<TData>;

      class function GetInstance() : TNothing; static;
      class function GetReference() : Ref<TData>; static;

    public
      constructor Create();

      function Copy() : TData; override;

      function ToString : string; override;

      class property Instance : Ref<TData> read GetReference;
  end;

  TAtom = class(TData)
    public
      constructor Create(v : string);
      function Copy() : TData; override;

      function ToString : string; override;
  end;

  DataRef = Ref<TData>;

  TValueType = class(TAtom)
    public
      function GetHashCode : Integer; override;
  end;

  TSymbol = class(TValueType)
    public

      function Copy() : TData; override;
      class function Match(v : string) : Boolean;

      function ToTValue() : TValue; override;
  end;

  TString = class(TValueType)
    public
      function Copy() : TData; override;
      function ToString : string; override;
      function ToTValue() : TValue; override;
  end;

  TBoolean = class(TValueType)
    public
      BoolValue : Boolean;

      constructor Create(v : string); overload;
      constructor Create(v : Boolean); overload;
      function Copy() : TData; override;

      class function Match(v : string) : Boolean;

      function ToString() : string; override;
      function ToTValue() : TValue; override;
  end;

  TNumber = class(TValueType)
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
      function ToTValue() : TValue; override;
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
      function ToTValue() : TValue; override;
  end;

  TAbstractObject = class abstract(TAtom)

  end;

  /// <summary>
  /// An abstract object implementation wrapping a native Delphi class instance.
  /// </summary>
  TDelphiObject = class(TAbstractObject)
    protected
      FValue : TValue;
      FObject : TObject;
      FOwned : Boolean;
      FContext : TRttiContext;
      FType : TRttiInstanceType;

      procedure Init();

    public
      /// <summary>Creates a Lisp object that owns the contained delphi object
      /// </summary>
      constructor CreateOwned(obj : TObject); overload;

      /// <summary>Creates a new Lisp object wrapping a new instance of the type
      /// denoted by the qualified name. Ownership is taken for that instance.</summary>
      constructor CreateOwned(qualifiedName : string; const args : array of TValue); overload;

      /// <summary>Creates a lisp object wrapping obj but not taking ownership</summary>
      constructor Create(obj : TObject);

      destructor Destroy(); override;

      property Content : TObject read FObject;

      function ToTValue() : TValue; override;

      function ToString() : string; override;

  end;

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
      /// <summary>Creates a new context optionally chained to given parent context.</summary>
      constructor Create(parent : TContext);
      destructor Destroy(); override;

      /// <summary>Symbols defined in this context. Can be used for reading
      /// symbols in this and parent contexts, and writing into this context.
      /// </summary>
      property Symbols[name : string] : Ref<TData>
        read GetSymbol write SetSymbol; default;

      property parent : TContext read FParent;

      /// <summary>Return true if the symbol is defined in this context or
      /// in one of its ancestors.</summary>
      function IsDefined(name : string) : Boolean; virtual;

      /// <summary>Looks up a TDelphiObject wrapper and returns the contained
      /// native delphi object.</summary>
      function GetDelphiObject<T : class>(name : string) : T;

      /// <summary>Imports all symbols from another context into this context.
      /// All symbol names will be prefixed with <prefix>.</summary>
      procedure Import(context : TContext; prefix : string); virtual;

      /// <summary>Removes a symbol reference from this context.</summary>
      procedure Remove(name : string); virtual;

      function ToString : string; override;
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

  /// <summary>An abstract function</summary>
  TFunction = class abstract(TAtom)
    protected
      FContext : TContext;

    public

  end;

function CreateRef(Data : TData) : DataRef;

implementation

function CreateRef(Data : TData) : DataRef;
begin
  Result := TRef<TData>.Create(Data);
end;

{ TRef<T> }

constructor TRef<T>.Create;
begin
  inherited;
  FValue := T.Create();
end;

function TRef<T>.AsSymbol: TSymbol;
begin
  Result := FValue as TSymbol;
end;

constructor TRef<T>.Create(val : T);
begin
  inherited Create();
  if val <> nil then
      FValue := val
  else
      FValue := T.Create();
end;

destructor TRef<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TRef<T>.Invoke : T;
begin
  Result := FValue;
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

procedure TContext.Import(context : TContext; prefix : string);
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

function TContext.ToString: string;
var
  pair: TPair<string,DataRef>;
begin
  Result := 'TContext { ';
  for pair in FSymbols do
  begin
    Result := Result + pair.Key + ' => ' + pair.Value().ToString + ', ';
  end;
  Result := Result + ' }';
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

function TInteger.ToTValue: TValue;
begin
  Result := TValue.From(Self.IntValue);
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

function TFloat.ToTValue: TValue;
begin
  Result := TValue.From(Self.FloatValue);
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

function TSymbol.ToTValue : TValue;
begin
  Result := TValue.From(Self.Value);
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

function TBoolean.ToTValue: TValue;
begin
  Result := TValue.From(Self.BoolValue);
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

{ TNothing }

function TNothing.Copy : TData;
begin
  Result := TNothing.Create();
end;

constructor TNothing.Create;
begin
  Value := '';
end;

class function TNothing.GetInstance: TNothing;
begin
  if FInstance = nil then
    FInstance := TNothing.Create();

  Result := FInstance;
end;

class function TNothing.GetReference: Ref<TData>;
begin
  if FReference = Nil then
    FReference := CreateRef(GetInstance());

  Result := FReference;
end;

function TNothing.ToString : string;
begin
  Result := 'TNothing';
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

function TString.ToTValue: TValue;
begin
  Result := TValue.From(Value);
end;

{ TObject }

constructor TDelphiObject.Create(obj : TObject);
begin
  FObject := obj;
  FOwned := False;
  FValue := TValue.From(FObject);
  Init;
  FType := FContext.GetType(obj.ClassInfo) as TRttiInstanceType;
end;

constructor TDelphiObject.CreateOwned(qualifiedName : string; const args : array of TValue);
begin
  Init;
  FType := FContext.FindType(qualifiedName) as TRttiInstanceType;

  FValue := FType.GetMethod('Create').Invoke(FType.MetaclassType, args);
  if FValue.IsObject then
      FObject := FValue.AsObject
  else
      FObject := nil
end;

constructor TDelphiObject.CreateOwned(obj : TObject);
begin
  FObject := obj;
  FValue := TValue.From(FObject);
  FOwned := True;
  Init;
  FType := FContext.GetType(obj.ClassInfo) as TRttiInstanceType;
end;

destructor TDelphiObject.Destroy;
begin
  if (FObject <> nil) and FOwned then
      FObject.Free;
  inherited;
end;

procedure TDelphiObject.Init;
begin
  FContext := TRttiContext.Create();
end;

function TDelphiObject.ToString: string;
begin
  if FObject <> Nil then
    Result := FType.QualifiedName + '(' + FObject.ToString + ')'
  else
    Result := FType.QualifiedName + '(' + FValue.ToString + ')';
end;

function TDelphiObject.ToTValue: TValue;
begin
  Result := FValue;
end;

{ TScopedContext }

constructor TDualLookupContext.Create(primary, secondary : TContext);
begin
  inherited Create(nil);
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

procedure TDualLookupContext.Import(context : TContext; prefix : string);
begin
  raise Exception.Create('read-only');
end;

function TDualLookupContext.IsDefined(name : string) : Boolean;
begin
  Result := FPrimary.IsDefined(name) or FSecondary.IsDefined(name);
end;

procedure TDualLookupContext.Remove(name : string);
begin
  raise Exception.Create('read-only');
end;

procedure TDualLookupContext.SetSymbol(name : string; Data : Ref<TData>);
begin
  raise Exception.Create('read-only');
end;

{ TValueType }

function TValueType.GetHashCode: Integer;
begin
  Result := Self.Value.GetHashCode;
end;

initialization

TNothing.FInstance := TNothing.Create();
TNothing.FReference := CreateRef(TNothing.FInstance);

FormatSettings := TFormatSettings.Create('en_US');

finalization

end.
