unit NativeFunctions;

interface

uses
  Classes,
  Data,
  Memory,
  Common,
  System.Generics.Collections,
  SysUtils;

var
  NativeFunctionList : TList<TFunction>;

type

  ProcNativeFunction = reference to function(runtime : TRuntime;
    context : TContext; args : Ref<TList>) : DataRef;

  TNativeFunction = class(TFunction)
    protected
      FName : string;
      FFunction : ProcNativeFunction;

    public
      constructor Create(name : string; func : ProcNativeFunction);
      destructor Destroy(); override;

      property Fn : ProcNativeFunction read FFunction write FFunction;
      property name : string read FName;

      /// <summary>Applies this function</summary>
      /// <param name="args">UNEVALUATED arguments</param>
      function Apply(runtime : TRuntime; context : TContext; args : Ref<TList>) : DataRef; virtual;
  end;

  TEvaluatingNativeFunction = class(TNativeFunction)
    public
      function Apply(runtime : TRuntime; context : TContext; args : Ref<TList>) : DataRef; override;
  end;

implementation

{ TNativeFunction }

function TNativeFunction.Apply(runtime : TRuntime; context : TContext; args : Ref<TList>) : DataRef;
begin
  Result := FFunction(runtime, context, args);
end;

constructor TNativeFunction.Create(name : string; func : ProcNativeFunction);
begin
  inherited Create();
  FName := name;
  FFunction := func;
  NativeFunctionList.Add(self);
end;

destructor TNativeFunction.Destroy;
begin
  inherited;
end;

{ TEvaluatingNativeFunction }

function TEvaluatingNativeFunction.Apply(runtime : TRuntime; context : TContext;
  args : Ref<TList>) : DataRef;
var
  evald : TList;
  a, rf : DataRef;
  i : Integer;
begin
  evald := TList.Create();
  evald.Add(args[0]);

  for i := 1 to args.Size - 1 do
      evald.Add(runtime.Eval(args[i], context));

  Result := inherited Apply(runtime, context, TRef<TList>.Create(evald));
end;

{ basic native functions }

function __quote(runtime : TRuntime; context : TContext; args : Ref<TList>) : DataRef;
begin
  Result := args[1];
end;

function __def(runtime : TRuntime; context : TContext; args : Ref<TList>) : DataRef;
var
  symbol : TSymbol;
begin
  if args.Size < 3 then
      raise Exception.Create('Too few arguments');

  if args[1]() is TSymbol then
      symbol := args[1]() as TSymbol
  else
      symbol := runtime.Eval(args[1], context)() as TSymbol;

  Result := runtime.Eval(args[2], context);
  context[symbol.Value] := Result;
end;

function __let(runtime : TRuntime; context : TContext; args : Ref<TList>) : DataRef;
var
  bindings : TList;
  subcontext : TContext;
  i: Integer;
  symbol : TSymbol;
  exp : DataRef;
  temp : TData;
begin
  bindings := args[1]() as TList;
  subcontext := TContext.Create(context);

  for i := 0 to (bindings.Size div 2) - 1 do
  begin
    symbol := bindings[1 + 2 * i]() as TSymbol;
    exp := runtime.Eval(bindings[1 + 2 * i + 1], subcontext);
    subcontext[symbol.Value] := exp;
  end;

  for i := 2 to args.Size - 1 do
  begin
    Result := runtime.Eval(args[i], subcontext);
  end;

  subcontext.Free;
end;

function _atom_(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  bool : Boolean;
begin
  bool := args[1]() is TAtom;
  Result := CreateRef(TBoolean.Create(bool));
end;

function __defn(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  symbol : TSymbol;
  funcArgs : TList;
  d : DataRef;
  i : Integer;
begin
  // symbol under which to define the function
  symbol := args[1]() as TSymbol;

  // TFunction.Create exptects basically the same arguments except the
  // symbol in the front; so just copy the rest into a new list
  funcArgs := TList.Create();
  for i := 1 to args.Size - 1 do
      funcArgs.Add(args[i]);

  // Create and save the function
  Result := CreateRef(TUserFunction.Create(TRef<TList>.Create(funcArgs), context));
  context[symbol.Value] := Result;
end;

function _eq(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  a, b : TData;
begin
  a := args[1]();
  b := args[2]();

  Result := CreateRef(TBoolean.Create(a.ValueEquals(b)));
end;

function _first(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  list : TList;
  evald : DataRef;
begin
  evald := args[1];
  list := evald() as TList;

  if list.Size = 0 then
      Result := CreateRef(TNothing.Create())
  else
      Result := CreateRef(list[0]().Copy);
end;

function __fn(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  func : TUserFunction;
begin
  func := TUserFunction.Create(args, context);
  Result := CreateRef(func);
end;

function __if(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  cond : Ref<TData>;
  bool : TBoolean;
  Data : TData;
begin
  cond := runtime.Eval(args[1], context);
  Data := cond();
  bool := Data as TBoolean;

  if bool.BoolValue then
      Result := runtime.Eval(args[2], context)
  else if args.Size > 3 then
      Result := runtime.Eval(args[3], context)
  else
      Result := CreateRef(TNothing.Create()); // if no else is supplied
end;

function _last(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  list : TList;
begin
  list := args[1]() as TList;
  Result := CreateRef(list[list.Size - 1]().Copy);
end;

function _length(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  list : TList;
begin
  list := args[1]() as TList;
  Result := CreateRef(TInteger.Create(list.Size));
end;

function _list(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  list : TList;
  a, b : DataRef;
  i : Integer;
begin
  list := TList.Create();

  for i := 1 to args.Size - 1 do
  begin
    list.Add(args[i]);
  end;

  Result := CreateRef(list);
end;

function _neq(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  bool : TBoolean;
begin
  bool := _eq(runtime, context, args)() as TBoolean;
  Result := CreateRef(TBoolean.Create(not bool.BoolValue));
end;

function _not(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  bool : TBoolean;
begin
  bool := args[1]() as TBoolean;
  Result := CreateRef(TBoolean.Create(not bool.BoolValue));
end;

function _nth(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  list : TList;
  index : TInteger;
begin
  list := args[1]() as TList;
  index := args[2]() as TInteger;

  Result := list[index.IntValue];
end;

function _print(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  i : Integer;
begin
  for i := 1 to args.Size - 1 do
  begin
    Writeln(args[i]().Value);
  end;

  Result := CreateRef(TNothing.Create());
end;

function _rest(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  list : TList;
  res : TList;
  i : Integer;
begin
  list := args[1]() as TList;
  res := TList.Create();
  for i := 1 to list.Size - 1 do
      res.Add(CreateRef(list[i]().Copy));

  Result := CreateRef(res);
end;

function _str(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  res : string;
  i : Integer;
  Ref : DataRef;
begin
  res := '';

  for i := 1 to args.Size - 1 do
  begin
    if args[i]() is TString then
        res := res + args[i]().Value
    else
    begin
      res := res + args[i]().ToString;
    end;
  end;

  Result := CreateRef(TString.Create(res));
end;

function _type(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  name : string;
  val : Ref<TData>;
begin
  val := args[1];
  Result := CreateRef(TString.Create(val().ClassName));
end;

function __apply(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  params, newargs, temp : TList;
  i : Integer;
begin
  temp := args();

  newargs := TList.Create();
  newargs.Add(runtime.Eval(args[1], context)); // the function to be applied

  // arguments to the function
  if args.Size > 2 then
  begin
    params := runtime.Eval(args[2], context)() as TList;
    for i := 0 to params.Size - 1 do
        newargs.Add(params[i]);
  end;

  Result := runtime.Eval(CreateRef(newargs), context);
end;

function _map(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  maplist, col, newargs : TList;
  mapping : DataRef;
  i : Integer;
  Fn : DataRef;
  params : Ref<TList>;
  Apply, params2 : DataRef;
begin
  Apply := CreateRef(TSymbol.Create('apply'));
  maplist := TList.Create();
  Fn := runtime.Eval(args[1], context);
  col := args[2]() as TList;

  for i := 0 to col.Size - 1 do
  begin
    newargs := TList.Create();
    newargs.Add(Fn);
    newargs.Add(col[i]);
    maplist.Add(runtime.Eval(CreateRef(newargs), context));
  end;

  Result := CreateRef(maplist);
end;

function _plus(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  number, temp : TNumber;
  i: Integer;
begin
  Assert(args.Size >= 3);
  Assert(args[1]() is TNumber);

  number := args[1]() as TNumber;
  for i := 2 to args.Size - 1 do
  begin
    Assert(args[i]() is TNumber);
    temp := number;
    number := number.Plus(args[i]() as TNumber);
    if i > 2 then FreeAndNil(temp);
  end;

  Result := CreateRef(number);
end;

function _minus(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  number, temp : TNumber;
  i: Integer;
begin
  Assert(args.Size >= 3);
  Assert(args[1]() is TNumber);

  number := args[1]() as TNumber;
  for i := 2 to args.Size - 1 do
  begin
    Assert(args[i]() is TNumber);
    temp := number;
    number := number.Minus(args[i]() as TNumber);
    if i > 2 then FreeAndNil(temp);
  end;

  Result := CreateRef(number);
end;

function _multiply(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  number, temp : TNumber;
  i: Integer;
begin
  Assert(args.Size >= 3);
  Assert(args[1]() is TNumber);

  number := args[1]() as TNumber;
  for i := 2 to args.Size - 1 do
  begin
    Assert(args[i]() is TNumber);
    temp := number;
    number := number.Multiply(args[i]() as TNumber);
    if i > 2 then FreeAndNil(temp);
  end;

  Result := CreateRef(number);
end;

function _divide(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  number, temp : TNumber;
  i: Integer;
begin
  Assert(args.Size >= 3);
  Assert(args[1]() is TNumber);

  number := args[1]() as TNumber;
  for i := 2 to args.Size - 1 do
  begin
    Assert(args[i]() is TNumber);
    temp := number;
    number := number.Divide(args[i]() as TNumber);
    if i > 2 then FreeAndNil(temp);
  end;

  Result := CreateRef(number);
end;

function __do(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  ops : TList;
  i : Integer;
begin
  for i := 1 to args.Size - 1 do
  begin
    Result := runtime.Eval(args[i], context);
  end;
end;

function _inc(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  num : TNumber;
  add : TInteger;
begin
  num := args[1]() as TNumber;
  add := TInteger.Create(1);
  num := num.Plus(add);
  result := CreateRef(num);
  add.Free;
end;

function _dec(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  num : TNumber;
  sub : TInteger;
begin
  num := args[1]() as TNumber;
  sub := TInteger.Create(1);
  num := num.Minus(sub);
  result := CreateRef(num);
  sub.Free;
end;

function _filter(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  fn : TFunction;
  fnRef : DataRef;
  col, filtered, l : TList;
  item, params: DataRef;
  b : Boolean;
  bRef : DataRef;
  i: Integer;
begin
  fnRef := args[1];
  fn := fnRef() as TFunction;
  col := args[2]() as TList;
  filtered := TList.Create();
  for i := 0 to col.Size - 1 do
  begin
    params := CreateRef(TList.Create([fnRef, col[i]]));
    l := params() as TList;
    bRef := runtime.Eval(params, context);
    if (bRef is TBoolean) and (bref as TBoolean).BoolValue then
    begin
      filtered.Add(col[i]);
    end;
  end;

  Result := CreateRef(filtered);
end;

function _nil_(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
begin
  Result := CreateRef(TBoolean.Create(args[1]() is TNothing));
end;

function _less(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  a, b, num : TNumber;
  i : Integer;
begin
  if (args[1]() is TNumber) and (args[2]() is TNumber) then
  begin
    a := args[1]() as TNumber;
    b := args[2]() as TNumber;
    num := a.Compare(b);
    i := (num as TInteger).IntValue;
    num.Free;
    if i < 0 then Result := CreateRef(TBoolean.Create(True))
    else Result := CreateRef(TBoolean.Create(False));
  end
  else
    raise Exception.Create('unsupported operand');
end;

function _lessorequal(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  a, b, num : TNumber;
  i : Integer;
begin
  if (args[1]() is TNumber) and (args[2]() is TNumber) then
  begin
    a := args[1]() as TNumber;
    b := args[2]() as TNumber;
    num := a.Compare(b);
    i := (num as TInteger).IntValue;
    num.Free;
    if i <= 0 then Result := CreateRef(TBoolean.Create(True))
    else Result := CreateRef(TBoolean.Create(False));
  end
  else
    raise Exception.Create('unsupported operand');
end;

function _greater(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  i : Integer;
  a, b, num : TNumber;
begin
  if (args[1]() is TNumber) and (args[2]() is TNumber) then
  begin
    a := args[1]() as TNumber;
    b := args[2]() as TNumber;
    num := a.Compare(b);
    i := (num as TInteger).IntValue;
    num.Free;
    if i > 0 then Result := CreateRef(TBoolean.Create(True))
    else Result := CreateRef(TBoolean.Create(False));
  end
  else
    raise Exception.Create('unsupported operand');
end;

function _greaterorequal(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  i : Integer;
  a, b, num : TNumber;
begin
  if (args[1]() is TNumber) and (args[2]() is TNumber) then
  begin
    a := args[1]() as TNumber;
    b := args[2]() as TNumber;
    num := a.Compare(b);
    i := (num as TInteger).IntValue;
    num.Free;
    if i >= 0 then Result := CreateRef(TBoolean.Create(True))
    else Result := CreateRef(TBoolean.Create(False));
  end
  else
    raise Exception.Create('unsupported operand');
end;

function __and(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  b : Boolean;
  temp : DataRef;
  expr : TData;
  i: Integer;
begin
  b := True;
  for i := 1 to args.Size - 1 do
  begin
    temp := runtime.Eval(args[i], context);
    expr := temp();
    if (not (expr is TBoolean)) or (not (expr as TBoolean).BoolValue) then
    begin
      b := False;
      break;
    end;
  end;

  Result := CreateRef(TBoolean.Create(b));
end;

function __or(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  b : Boolean;
  temp : DataRef;
  expr : TData;
  i: Integer;
begin
  b := False;
  for i := 1 to args.Size - 1 do
  begin
    temp := runtime.Eval(args[i], context);
    expr := temp();
    if (expr is TBoolean) and (expr as TBoolean).BoolValue then
    begin
      b := True;
      break;
    end;
  end;

  Result := CreateRef(TBoolean.Create(b));
end;

function __comment(runtime : TRuntime; context : TContext; args : Listref) : DataRef;
begin
  Result := CreateRef(TNothing.Create());
end;

function _read(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  arg1 : DataRef;
  text : Data.TString;
begin
  Assert(args.Size = 2);
  text := args[1]() as Data.TString;
  Result := runtime.Read(text.Value);
end;

function _eval(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
begin
  Result := runtime.Eval(args[1], context);
end;

initialization

NativeFunctionList := TList<TFunction>.Create();

TNativeFunction.Create('quote', __quote);
TNativeFunction.Create('def', __def);
TNativeFunction.Create('defn', __defn);
TNativeFunction.Create('apply', __apply);
TNativeFunction.Create('fn', __fn);
TNativeFunction.Create('if', __if);
TNativeFunction.Create('do', __do);
TNativeFunction.Create('let', __let);
TNativeFunction.Create('and', __and);
TNativeFunction.Create('or', __or);
TNativeFunction.Create('comment', __comment);

TEvaluatingNativeFunction.Create('read', _read);
TEvaluatingNativeFunction.Create('eval', _eval);
TEvaluatingNativeFunction.Create('print', _print);
TEvaluatingNativeFunction.Create('type', _type);
TEvaluatingNativeFunction.Create('str', _str);
TEvaluatingNativeFunction.Create('atom?', _atom_);
TEvaluatingNativeFunction.Create('=', _eq);
TEvaluatingNativeFunction.Create('first', _first);
TEvaluatingNativeFunction.Create('last', _last);
TEvaluatingNativeFunction.Create('length', _length);
TEvaluatingNativeFunction.Create('list', _list);
TEvaluatingNativeFunction.Create('not=', _neq);
TEvaluatingNativeFunction.Create('not', _not);
TEvaluatingNativeFunction.Create('nth', _nth);
TEvaluatingNativeFunction.Create('rest', _rest);
TEvaluatingNativeFunction.Create('map', _map);
TEvaluatingNativeFunction.Create('filter', _filter);
TEvaluatingNativeFunction.Create('nil?', _nil_);

TEvaluatingNativeFunction.Create('+', _plus);
TEvaluatingNativeFunction.Create('-', _minus);
TEvaluatingNativeFunction.Create('/', _divide);
TEvaluatingNativeFunction.Create('*', _multiply);
TEvaluatingNativeFunction.Create('inc', _inc);
TEvaluatingNativeFunction.Create('dec', _dec);
TEvaluatingNativeFunction.Create('<', _less);
TEvaluatingNativeFunction.Create('<=', _lessorequal);
TEvaluatingNativeFunction.Create('>', _greater);
TEvaluatingNativeFunction.Create('>=', _greaterorequal);


finalization

NativeFunctionList.Free;

end.
