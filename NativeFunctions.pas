unit NativeFunctions;

interface

uses
  Classes,
  Data,
  Common,
  System.Generics.Collections,
  SysUtils,
  System.IOUtils,
  Modules,
  RegularExpressions,
  Collections,
  UserData,
  Interfaces,
  Rtti;

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

function _get(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;

implementation

{ TNativeFunction }

function TNativeFunction.Apply(runtime : TRuntime; context : TContext; args : Ref<TList>) : DataRef;
begin
  Result := FFunction(runtime, context, args);
end;

constructor TNativeFunction.Create(name : string; func : ProcNativeFunction);
begin
  inherited Create(name);
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
  i : Integer;
  symbol : TSymbol;
  exp : DataRef;
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
  i : Integer;
begin
  // symbol under which to define the function
  symbol := args[1]() as TSymbol;

  // TFunction.Create exptects basically the same arguments except the
  // symbol in the front; so just copy the rest into a new list
  funcArgs := TList.Create();
  for i := 1 to args.Size - 1 do
  begin
    funcArgs.Add(args[i]);
  end;

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
  Data : TData;
  list : TList;
  c : ICountable;
  count : Integer;
begin
  Data := args[1]();

  if Data is TList then count := (Data as TList).Size
  else if Supports(Data, ICountable, c) then count := c.count;

  Result := CreateRef(TInteger.Create(count));
end;

function _list(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  list : TList;
  i : Integer;
begin
  list := TList.Create();
  list.Executable := False;

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
  val : Ref<TData>;
begin
  val := args[1];
  Result := CreateRef(TString.Create(val().ClassName));
end;

function __apply(runtime : TRuntime; context : TContext; args : Ref<TList>) : Ref<TData>;
var
  params, newargs : TList;
  i : Integer;
begin
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
  i : Integer;
  Fn : DataRef;
  Apply : DataRef;
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
  i : Integer;
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
  i : Integer;
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
  i : Integer;
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
  i : Integer;
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
  i : Integer;
begin
  if args.Size = 0 then
  begin
    Result := CreateRef(TNothing.Create);
    Exit;
  end;

  for i := 1 to args.Size - 1 do
  begin
    Result := runtime.Eval(args[i], context);
  end;
end;

function _inc(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  num : TNumber;
  Add : TInteger;
begin
  num := args[1]() as TNumber;
  Add := TInteger.Create(1);
  num := num.Plus(Add);
  Result := CreateRef(num);
  Add.Free;
end;

function _dec(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  num : TNumber;
  sub : TInteger;
begin
  num := args[1]() as TNumber;
  sub := TInteger.Create(1);
  num := num.Minus(sub);
  Result := CreateRef(num);
  sub.Free;
end;

function _filter(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  fnRef : DataRef;
  col, filtered : TList;
  params : DataRef;
  bRef : DataRef;
  i : Integer;
begin
  fnRef := args[1];
  col := args[2]() as TList;
  filtered := TList.Create();
  for i := 0 to col.Size - 1 do
  begin
    params := CreateRef(TList.Create([fnRef, col[i]]));
    bRef := runtime.Eval(params, context);
    if (bRef is TBoolean) and (bRef as TBoolean).BoolValue then
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
  i : Integer;
begin
  b := True;
  for i := 1 to args.Size - 1 do
  begin
    temp := runtime.Eval(args[i], context);
    expr := temp();
    if (not(expr is TBoolean)) or (not(expr as TBoolean).BoolValue) then
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
  i : Integer;
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

function __comment(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
begin
  Result := CreateRef(TNothing.Create());
end;

function _read(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
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

// (foreach <symbol> [in] <list> <expr>)
function __foreach(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  c : TContext;
  s : TSymbol;
  expr : DataRef;
  list : TList;
  offset, i : Integer;
begin
  s := args[1]() as TSymbol;
  offset := 0;
  if (args[2]() is TSymbol) and ((args[2]() as TSymbol).Value = 'in') then
      offset := 1;

  list := runtime.Eval(args[2 + offset], context)() as TList;
  expr := args[3 + offset];
  c := TContext.Create(context);
  c['#i'] := CreateRef(TInteger.Create(0));

  for i := 0 to list.Size - 1 do
  begin
    c[s.Value] := list[i];
    (c['#i']() as TInteger).Value := IntToStr(i);
    runtime.Eval(expr, c);
  end;

  Result := CreateRef(TNothing.Create);
  c.Free;
end;

/// <summary>
/// Loads a LISP file by evaluating all contained expressions in the current
/// context.
/// </summary>
function _load(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  filename, code : string;
begin
  filename := (args[1]() as TString).Value;
  if not TFile.Exists(filename) then
      raise Exception.Create('File does not exist: "' + filename + '"');

  try
    code := TFile.ReadAllText(filename);
  except
    Exit(CreateRef(TNothing.Create));
  end;

  // Wrap everything into a do statement and evaluate all expressions
  code := '(do ' + code + ')';
  runtime.Eval(code, context);

  Result := CreateRef(TNothing.Create);
end;

// (use <path>)              - import into current context without prefix
// (use <path> :ns)          - import with prefix "<ModuleName>/"
// (use <path> :as <prefix>) - import with prefix "<prefix>/"
function _use(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  path : string;
  module : TModule;
  moduleManager : TModuleManager;
  prefix : string;
begin
  if args[1]() is TSymbol then
  begin
    // TODO: use search path additionally to CWD
    path := (args[1]() as TSymbol).Value + '.cl';
  end
  else
      path := (args[1]() as TString).Value;

  moduleManager := context.GetDelphiObject<TModuleManager>('*module-manager*');
  module := moduleManager.Load(path);

  // import the symbols prefixed into the current context
  if (args.Size = 4) and (args[2]().Value = ':as') then
      prefix := args[3]().Value + '/'
  else if (args.Size = 3) and (args[2]().Value = ':ns') then
      prefix := module.name + '/'
  else
      prefix := '';

  // Always import symbols with <ModuleName>/ prefix
  if prefix <> (module.name + '/') then
      context.Import(module.context, module.name + '/');

  // Import with user defined prefix (default: empty)
  context.Import(module.context, prefix);

  Result := context['nil'];
end;

function __module_get(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  c : TDualLookupContext;
  module : TModule;
  moduleName : string;
  moduleManager : TModuleManager;
begin
  moduleName := (runtime.Eval(args[1], context)() as TSymbol).Value;
  moduleManager := context.GetDelphiObject<TModuleManager>('*module-manager*');
  module := moduleManager.Modules[moduleName];
  c := TDualLookupContext.Create(context, module.context);
  Result := runtime.Eval(args[2], c);
  c.Free;
end;

function _in(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  list : TList;
  i : Integer;
begin
  list := args[1]() as TList;
  for i := 0 to list.Size - 1 do
  begin
    if list[i]().ValueEquals(args[2]()) then
        Exit(CreateRef(TBoolean.Create(True)));
  end;

  Result := CreateRef(TBoolean.Create(False));
end;

/// (anonymous-function expr)
/// creates an anonymous function that can make use of
/// implicit arguments in the form of "_\d?".
function __anonymous_function(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  i : Integer;
  implicitArgs : TList;
  code, expr : TList;
  text, symbol : string;
  matches : TMatchCollection;
  match : TMatch;
  symbols : TList<string>;
begin
  implicitArgs := TList.Create();
  symbols := TList<string>.Create();
  expr := TList.Create();
  for i := 1 to args.Size - 1 do
      expr.Add(args[i]);

  // scan for placeholders to find the required arguments
  text := expr.ToString;
  matches := TRegEx.matches(text, '%\d?');
  for match in matches do
  begin
    symbol := match.Value;
    if not symbols.Contains(symbol) then symbols.Add(symbol);
  end;
  symbols.Sort;

  // list arguments to the anonymous functions
  implicitArgs.Add(CreateRef(TSymbol.Create('list')));
  for symbol in symbols do
  begin
    implicitArgs.Add(CreateRef(TSymbol.Create(symbol)));
  end;
  implicitArgs.Executable := False;

  // generate the code (fn [p0 p1 ... pN] expr)
  code := TList.Create();
  code.Add(CreateRef(TSymbol.Create('fn'))); // (fn
  code.Add(CreateRef(implicitArgs));         // (fn <args>
  code.Add(CreateRef(expr));                 // (fn <args> <expr>)

  // evaluate the code
  Result := runtime.Eval(CreateRef(code), context);
  symbols.Free;
end;

function _create(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  qualifiedName : string;
  constructorArgs : TList;
  valueArray : array of TValue;
  i : Integer;
  obj : TDelphiObject;
begin
  Assert((args[1]() is TSymbol) or (args[1]() is TString)); // class name
  Assert(args[2]() is TList); // constructor arguments

  qualifiedName := args[1]().Value;
  constructorArgs := (args[2]() as TList);

  SetLength(valueArray, constructorArgs.Size);
  for i := 0 to constructorArgs.Size - 1 do
      valueArray[i] := constructorArgs[i]().ToTValue();

  obj := TDelphiObject.CreateOwned(qualifiedName, valueArray);
  Result := CreateRef(obj);
end;

function _dict(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  dict : TDictionary;
  i : Integer;
begin
  dict := TDictionary.Create();
  i := 1;
  while i < args().Size - 1 do
  begin
    dict.Add(args[i], args[i + 1]);
    i := i + 2;
  end;

  Result := CreateRef(dict);
end;

function _get(runtime : TRuntime; context : TContext; args : ListRef) : DataRef;
var
  dict : TDictionary;
begin
  if args[1]() is TDictionary then
  begin
    dict := args[1]() as TDictionary;
    if dict.Contains(args[2]) then Result := dict.Get(args[2])
    else if args().Size = 4 then Result := args[3]// default value
    else Result := TNothing.Instance;
  end
  else
      raise Exception.Create('Unsupported operand');
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
TNativeFunction.Create('foreach', __foreach);
TNativeFunction.Create('module-get', __module_get);
TNativeFunction.Create('anonymous-function', __anonymous_function);

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
TEvaluatingNativeFunction.Create('dict', _dict);
TEvaluatingNativeFunction.Create('not=', _neq);
TEvaluatingNativeFunction.Create('not', _not);
TEvaluatingNativeFunction.Create('nth', _nth);
TEvaluatingNativeFunction.Create('rest', _rest);
TEvaluatingNativeFunction.Create('map', _map);
TEvaluatingNativeFunction.Create('filter', _filter);
TEvaluatingNativeFunction.Create('nil?', _nil_);
TEvaluatingNativeFunction.Create('load', _load);
TEvaluatingNativeFunction.Create('use', _use);
TEvaluatingNativeFunction.Create('in', _in);
TEvaluatingNativeFunction.Create('get', _get);

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

TEvaluatingNativeFunction.Create('create', _create);

finalization

NativeFunctionList.Free;

end.
