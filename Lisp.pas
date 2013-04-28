unit Lisp;

interface

uses
  System.StrUtils,
  Classes,
  SysUtils,
  Memory,
  System.Generics.Collections,
  System.RegularExpressions, Data;

var
  LispFormatSettings : TFormatSettings;
  ShowMemoryLog : Boolean;

type

  ProcBuiltIn = reference to function(context : TContext; args : Ref<TList>) : Ref<TData>;

  TLisp = class

    protected
      FGlobal : TContext;
      FBuiltIns : TDictionary<string, ProcBuiltIn>;

      function Eval(code : DataRef) : DataRef; overload;
      function Eval(code : DataRef; context : TContext) : DataRef; overload;

      /// <summary>Evaluates all expressions in the list and returns a new
      /// list with the results.</summary>
      function EvaluatedArgs(list : Ref<TList>; context : TContext; offset : Integer) : Ref<TList>;

    public
      constructor Create();
      destructor Destroy(); override;

      function Read(input : string) : Ref<TData>;
      function Eval(input : string) : DataRef; overload;
      function Eval(input : string; context : TContext) : DataRef; overload;

      // ========================================================================
      // built-in functions
      // ========================================================================

      /// <summary>Defines a *new* variable in the current context</summary>
      function _def(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Creates a new function</summary>
      function _fn(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Shortcut for (def <symbol> (fn ...))</summary>
      function _defn(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Returns the data type of the first argument</summary>
      function _type(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Prints a variable number of arguments to stdout</summary>
      function _print(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Returns the first argument unevaluated</summary>
      function _quote(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Creates a list of given arguments</summary>
      function _list(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Creates one string from all arguments</summary>
      function _str(context : TContext; args : Ref<TList>) : Ref<TData>;

      // math
      function _plus(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Gets or sets DLISP configuration settings</summary>
      function __cfg(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>if branch (if <condition> <expr-true> <expr-false>)</summary>
      function _if(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Returns first element of a list</summary>
      function _first(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Returns a list without its first element</summary>
      function _rest(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Returns the number of items in a list</summary>
      function _length(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Returns the last list item</summary>
      function _last(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summaryReturns the nth element of a list</summary>
      function _nth(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Checks if argument is an atomic data type</summary>
      function _atom_(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Compares two atomic values</summary>
      function _eq(context : TContext; args : Ref<TList>): Ref<TData>;
      function _neq(context : TContext; args : Ref<TList>): Ref<TData>;
      function _not(context : TContext; args : Ref<TList>): Ref<TData>;

  end;

function CreateRef(data : TData) : DataRef;

implementation

function CreateRef(data : TData) : DataRef;
begin
  Result := TRef<TData>.Create(data);
end;

{ TLisp }

constructor TLisp.Create;
begin
  FGlobal := TContext.Create(Nil);

  FBuiltIns := TDictionary<string, ProcBuiltIn>.Create();
  FBuiltIns.Add('def', _def);
  FBuiltIns.Add('defn', _defn);
  FBuiltIns.Add('type', _type);
  FBuiltIns.Add('print', _print);
  FBuiltIns.Add('list', _list);
  FBuiltIns.Add('quote', _quote);
  FBuiltIns.Add('fn', _fn);
  FBuiltIns.Add('str', _str);
  FBuiltIns.Add('__cfg', __cfg);
  FBuiltIns.Add('if', _if);
  FBuiltIns.Add('first', _first);
  FBuiltIns.Add('rest', _rest);
  FBuiltIns.Add('last', _last);
  FBuiltIns.Add('length', _length);
  FBuiltIns.Add('nth', _nth);
  FBuiltIns.Add('atom?', _atom_);
  FBuiltIns.Add('=', _eq);
  FBuiltIns.Add('not=', _neq);
  FBuiltIns.Add('not', _not);

  FBuiltIns.Add('+', _plus);
end;

destructor TLisp.Destroy;
begin
  FGlobal.Free;
  FBuiltIns.Free;
  inherited;
end;

function TLisp.Eval(code: DataRef) : DataRef;
begin
  Result := Eval(code, FGlobal);
end;

function TLisp.Eval(code : DataRef; context : TContext) : DataRef;
var
  list, evaluated : Ref<TList>;
  opName, symbol : TSymbol;
  expr : Ref<TData>;
  fn : TFunction;
  i: Integer;
  data : TData;
  fnScope : TContext;
  
begin
  if code() is TSymbol then
  begin
    symbol := code() as TSymbol;
    if context.IsDefined(symbol.Value) then
    begin
      Result := context[symbol.Value];
    end
    else
    begin
      Result := code;
    end;
  end
  else if code() is TList then
  begin
    list := Ref<TList>(code);

    if (list[0]() is TSymbol) and (FBuiltIns.ContainsKey((list[0]() as TSymbol).Value)) then
    begin
      opName := (list[0]() as TSymbol);
      // don't evaluate the parameters; the built-in functions must decide
      // which arguments they want quoted or evaluated
      Result := FBuiltIns[opName.Value](context, list);
    end
    else
    begin
      // this looks like a function application; evaluate all arguments
      evaluated := TRef<TList>.Create(TList.Create());
      for i := 0 to list.Size - 1 do
      begin
        evaluated.Add(Eval(list[i], context));
      end;

      fn := evaluated[0]() as TFunction;
      fnScope := TContext.Create(fn.Context);

      // args[0] = function name
      // args[1] = function arguments
      // args[2:] = function body expressions
      
      // register arguments in context
      for i := 1 to evaluated.Size - 1 do
      begin
        symbol := fn.Args()[i - 1]() as TSymbol;
        data := evaluated[i]();
        fnScope[symbol.Value] := evaluated[i];
      end;

      // execute the function code
      for i := 2 to fn.Code().Size - 1 do
      begin
        Result := Eval(fn.Code()[i], fnScope);
      end;      

      // Clear the arguments from the context again
      for i := 1 to evaluated.Size - 1 do
      begin
        symbol := fn.Args()[i - 1]() as TSymbol;
        fnScope.Remove(symbol.Value);
      end;

      fnScope.Free;
    end;
  end
  else
  begin
    Result := code;
  end;
end;

function TLisp.Eval(input : string; context : TContext) : DataRef;
begin
  Result := Eval(read(input), context);
end;

function TLisp.EvaluatedArgs(list: Ref<TList>; context : TContext; offset : Integer): Ref<TList>;
var
  args : TList;
  i: Integer;
begin
  args := TList.Create();
  for i := offset to list.Size - 1 do
  begin
    args.Add(Eval(list[i], context));
  end;

  Result := TRef<TList>.Create(args);
end;

function TLisp.Eval(input : string) : DataRef;
begin
  Result := Eval(read(input), FGlobal);
end;

function TLisp.Read(input : string) : Ref<TData>;
var
  i, j : Integer;
  inString : Boolean;
  list : Ref<TList>;
  item : Ref<TData>;
  text : string;
  charsTrimmed : Integer;

  function IsWhitespace(c : Char) : Boolean;
  begin
    Result := (c = ' ') or (c = chr(9)) or
      (c = chr(10)) or (c = chr(13));
  end;

  function IsParenthesis(c : Char) : Boolean;
  begin
    Result := (c = ')') or (c = ']') or (c = '}') or
      (c = '(') or (c = '[') or (c = '{');
  end;

begin
  text := input.Trim;
  charsTrimmed := input.Length - text.Length;
  input := text;
  if Length(input) = 0 then Exit(nil);

  // check for atom
  if (input[1] <> '(') and (input[1] <> '[') and (input[1] <> '{') then
  begin
    // search for end of atom
    if input[1] <> '"' then
    begin
      // something not a string (i.e. a number or symbol)
      i := 2;
      while (i <= Length(input)) and (not IsWhitespace(input[i])) and (not IsParenthesis(input[i])) do
      begin
        i := i + 1;
      end;

      text := input.Substring(0, i - 1);

      if TInteger.Match(text) then
          Result := CreateRef(TInteger.Create(text))
      else if TFloat.Match(text) then
          Result := CreateRef(TFloat.Create(text))
      else if TBoolean.Match(text) then
          Result := CreateRef(TBoolean.Create(text))
      else if TSymbol.Match(text) then
          Result := CreateRef(TSymbol.Create(text))
      else
          Result := CreateRef(TAtom.Create(text));

      text := Result().ClassName;
      Result().ConsumedCharacters := i - 1;
    end
    else
    begin
      // parsing a string
      i := 2;

      while (i <= Length(input)) do
      begin
        if (input[i] <> '"') then i := i + 1
        else if (input[i - 1] = '\') then i := i + 1
        else break;
      end;

      // including closing quote (i - 1 + 1 = i)
      text := input.Substring(1, i - 2);
      Result := CreateRef(TString.Create(text));
      Result().ConsumedCharacters := i;
    end;

    Result().ConsumedCharacters := Result().ConsumedCharacters + charsTrimmed;
    Exit(Result);
  end;

  // assume it's a list
  i := 2;

  list := TRef<TList>.Create(TList.Create());

  while i <= Length(input) do
  begin
    if (input[i] = ')') or (input[i] = ']') or (input[i] = '}') then
    begin
      i := i + 1;
      break;
    end;

    item := read(input.Substring(i - 1));
    i := i + item.ConsumedCharacters;
    list.Add(item);
  end;

  list.ConsumedCharacters := i - 1 + charsTrimmed;
  Result := Ref<TData>(list);
end;

function TLisp._atom_(context: TContext; args: Ref<TList>): Ref<TData>;
var
  bool : Boolean;
begin
  bool := args[1]() is TAtom;
  Result := CreateRef(TBoolean.Create(bool));
end;

function TLisp._def(context : TContext; args : Ref<TList>) : Ref<TData>;
var
  symbol : TSymbol;
begin
  if args.Size < 3 then
      raise Exception.Create('def: not enough arguments');

  if args[1]() is TSymbol then
      symbol := args[1]() as TSymbol
  else
      symbol := Eval(args[1], context)() as TSymbol;

  Result := Eval(args[2], context);
  context[symbol.Value] := Result;
end;

function TLisp._defn(context: TContext; args: Ref<TList>): Ref<TData>;
var
  symbol : TSymbol;
  funcArgs : TList;
  d : DataRef;
  i: Integer;
begin
  // symbol under which to define the function
  symbol := args[1]() as TSymbol;

  // TFunction.Create exptects basically the same arguments except the
  // symbol in the front; so just copy the rest into a new list
  funcArgs := TList.Create();
  for i := 1 to args.Size-1 do
    funcArgs.Add(args[i]);

  // Create and save the function
  Result := CreateRef(TFunction.Create(TRef<TList>.Create(funcArgs), context));
  context[symbol.Value] := Result;
end;

function TLisp._eq(context: TContext; args: Ref<TList>): Ref<TData>;
var
  a, b : TData;
begin
  a := Eval(args[1], context)();
  b := Eval(args[2], context)();

  Result := CreateRef(TBoolean.Create(a.ValueEquals(b)));
end;

function TLisp._first(context: TContext; args: Ref<TList>): Ref<TData>;
var
  list : TList;
  evald : DataRef;
begin
  evald := Eval(args[1], context);
  list := evald() as TList;

  if list.Size = 0 then
    Result := CreateRef(TNothing.Create())
  else
    Result := CreateRef(list[0]().Copy);
end;

function TLisp._fn(context : TContext; args : Ref<TList>) : Ref<TData>;
var
  func : TFunction;
begin
  func := TFunction.Create(args, context);
  Result := CreateRef(func);
end;

function TLisp._if(context: TContext; args: Ref<TList>): Ref<TData>;
var
  cond : Ref<TData>;
  bool : TBoolean;
  data : TData;
begin
  cond := Eval(args[1], context);
  data := cond();
  bool := data as TBoolean;

  if bool.BoolValue then
    Result := Eval(args[2], context)
  else if args.Size > 3 then
    Result := Eval(args[3], context)
  else
    Result := CreateRef(TNothing.Create()); // if no else is supplied
end;

function TLisp._last(context: TContext; args: Ref<TList>): Ref<TData>;
var
  list : TList;
begin
  list := Eval(args[1], context)() as TList;
  Result := CreateRef(list[list.Size - 1]().Copy);
end;

function TLisp._length(context: TContext; args: Ref<TList>): Ref<TData>;
var
  list : TList;
begin
  list := Eval(args[1], context)() as TList;
  Result := CreateRef(TInteger.Create(list.Size));
end;

function TLisp._list(context : TContext; args : Ref<TList>) : Ref<TData>;
var
  list : TList;
  a, b : DataRef;
  i : Integer;
begin
  list := TList.Create();

  for i := 1 to args.Size - 1 do
  begin
    list.Add(Eval(args[i], context));
  end;

  Result := CreateRef(list);
end;

function TLisp._neq(context: TContext; args: Ref<TList>): Ref<TData>;
var
  bool : TBoolean;
begin
  bool := _eq(context, args)() as TBoolean;
  Result := CreateRef(TBoolean.Create(not bool.BoolValue));
end;

function TLisp._not(context: TContext; args: Ref<TList>): Ref<TData>;
var
  bool : TBoolean;
begin
  bool := Eval(args[1], context)() as TBoolean;
  Result := CreateRef(TBoolean.Create(not bool.BoolValue));
end;

function TLisp._nth(context: TContext; args: Ref<TList>): Ref<TData>;
var
  list : TList;
  index : TInteger;
begin
  list := Eval(args[1], context)() as TList;
  index := Eval(args[2], context)() as TInteger;

  Result := list[index.IntValue];
end;

function TLisp._plus(context : TContext; args : Ref<TList>) : Ref<TData>;
var
  resultType : string;
  i, sumInt : Integer;
  sumFloat : Single;
  evaluatedArgs : TList;
  arg, item : TData;
  resInt : TInteger;
  resFloat : TFloat;
begin
  (*
   // evaluate arguments
   evaluatedArgs := TList.Create();
   for i := 1 to args.Size - 1 do
   begin
   item := Eval(args[i]);
   evaluatedArgs.Items.Add(item);
   item._AddRef;
   end;

   // Sum up the result; type depending on first item
   if evaluatedArgs[0] is TFloat then
   begin
   resFloat := TFloat.Create(0);
   for arg in evaluatedArgs.Items do
   begin
   resFloat.Plus(arg as TNumber);
   end;

   Result := resFloat;
   Result._AddRef;
   end
   else if evaluatedArgs[0] is TInteger then
   begin
   resInt := TInteger.Create(0);
   for arg in evaluatedArgs.Items do
   begin
   resInt.Plus(arg as TNumber);
   end;

   Result := resInt;
   Result._AddRef;
   end
   else
   begin
   raise Exception.Create('+: unknown number type');
   end;

   evaluatedArgs.Free;
  *)
end;

function TLisp._print(context : TContext; args : Ref<TList>) : Ref<TData>;
var
  i : Integer;
  res, a : Ref<TData>;
begin
  for i := 1 to args.Size - 1 do
  begin
    res := Eval(args[i], context);
    Writeln(res.Value);
  end;

  Result := CreateRef(TNothing.Create());
end;

function TLisp._quote(context : TContext; args : Ref<TList>) : Ref<TData>;
begin
  Result := args[1];
end;

function TLisp._rest(context: TContext; args: Ref<TList>): Ref<TData>;
var
  list : TList;
  res : TList;
  i: Integer;
begin
  list := Eval(args[1], context)() as TList;
  res := TList.Create();
  for i := 1 to list.Size - 1 do
    res.Add(CreateRef(list[i]().Copy));

  Result := CreateRef(res);
end;

function TLisp._str(context: TContext; args: Ref<TList>): Ref<TData>;
var
  res : string;
  i: Integer;
  ref : DataRef;
begin
  res := '';

  for i := 1 to args.Size - 1 do
  begin
    if args[i]() is TString then
      res := res + args[i]().Value
    else
    begin
      ref := Eval(args[i], context);
      if ref() is TString then res := res + ref().Value
      else res := res + ref.ToString;
    end;
  end;

  Result := CreateRef(TString.Create(res));
end;

function TLisp._type(context : TContext; args : Ref<TList>) : Ref<TData>;
var
  name : string;
  val : Ref<TData>;
begin
  val := Eval(args[1], context);
  Result := CreateRef(TString.Create(val().ClassName));
end;

function TLisp.__cfg(context : TContext; args : Ref<TList>) : Ref<TData>;
var
  setting : TSymbol;
  bool : TBoolean;
begin
  setting := args[1]() as TSymbol;
  if setting.Value = 'ShowMemoryLog' then
  begin
    if args.Size = 3 then
    begin
      bool := args[2]() as TBoolean;
      ShowMemoryLog := bool.BoolValue;
      Result := args[2];
    end
    else
    begin
      if ShowMemoryLog then
          Result := CreateRef(TBoolean.Create('True'))
      else
          Result := CreateRef(TBoolean.Create('False'));
    end;
  end
  else
  begin
    Result := nil;
  end;
end;

initialization

finalization

end.
