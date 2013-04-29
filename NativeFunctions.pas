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
      res := res + Ref.ToString;
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

initialization

NativeFunctionList := TList<TFunction>.Create();

TNativeFunction.Create('quote', __quote);
TNativeFunction.Create('def', __def);
TNativeFunction.Create('defn', __defn);
TNativeFunction.Create('fn', __fn);
TNativeFunction.Create('if', __if);

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
TEvaluatingNativeFunction.Create('nth', _nth);
TEvaluatingNativeFunction.Create('rest', _rest);

finalization

NativeFunctionList.Free;

end.
