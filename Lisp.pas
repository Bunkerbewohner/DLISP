unit Lisp;

interface

uses
  System.StrUtils,
  NativeFunctions,
  Classes,
  SysUtils,
  Memory,
  System.Generics.Collections,
  System.RegularExpressions,
  Data,
  Common,
  Modules;

type

  /// <summary>
  /// DLisp interpreter. Provides everything needed for executing and loading
  /// DLisp code.
  /// </summary>
  TLisp = class(TRuntime)
    protected
      FGlobal : TContext;
      FModuleManager : TModuleManager;

      function GetGlobalContext : TContext; override;

    public
      ///<summary>Creates new DLisp interpreter</summary>
      constructor Create();
      destructor Destroy(); override;

      ///<summary>Parses a string and returns equivalent DLisp code.</summary>
      ///<exception cref="Exception">When something goes wrong</exception>
      function Read(input : string) : Ref<TData>; override;

      ///<summary>Evaluates DLisp code in the given context.</summary>
      function Eval(code : DataRef; context : TContext) : DataRef; override;

      ///<summary>Parses and evaluates the input string in given context.</summary>
      function Eval(input : string; context : TContext) : DataRef; overload; override;

      ///<summary>Parses and evaluates the input string in the global context.</summary>
      function Eval(input : string) : DataRef; overload;

      ///<summary>Register a native function in global context.</summary>
      procedure RegisterFunction(fn : TFunction);

  end;

implementation

{ TLisp }

constructor TLisp.Create;
var
  fn : TFunction;
begin
  FGlobal := TContext.Create(nil);
  FModuleManager := TModuleManager.Create(Self);

  for fn in NativeFunctionList do
  begin
    Self.RegisterFunction(fn);
  end;

  FGlobal['nil'] := CreateRef(TNothing.Create());
  FGlobal['*module-manager*'] := CreateRef(TDelphiObject.Create(FModuleManager));
end;

destructor TLisp.Destroy;
begin
  FGlobal.Free;
  FModuleManager.Free;
  inherited;
end;

function TLisp.Eval(code : DataRef; context : TContext) : DataRef;
var
  list, evaluated : Ref<TList>;
  opName, symbol : TSymbol;
  expr : Ref<TData>;
  fn : TUserFunction;
  native : TNativeFunction;
  i : Integer;
  Data : TData;
  fnScope : TContext;
  className, dbg : string;

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
    if list[0]() is TSymbol then symbol := list[0]() as TSymbol
    else symbol := nil;

{$IFDEF DEBUG}
    className := list[0]().className;
    dbg := list.ToString;
{$ENDIF}
    if (symbol <> nil) and (FGlobal.IsDefined(symbol.Value)) and
      (FGlobal[symbol.Value]() is TNativeFunction) then
    begin
      native := FGlobal[symbol.Value]() as TNativeFunction;
      Result := native.Apply(Self, context, list);
    end
    else if (list[0]() is TNativeFunction) then
    begin
      Result := (list[0]() as TNativeFunction).Apply(Self, context, list);
    end
    else if list().Executable then
    begin
      // this looks like a function application; evaluate all arguments
      evaluated := TRef<TList>.Create(TList.Create());
      for i := 0 to list.Size - 1 do
      begin
        evaluated.Add(Eval(list[i], context));
      end;

      if not(evaluated[0]() is TUserFunction) then
      begin
        raise Exception.Create('Unkown function "' + evaluated[0]().Value + '"');
      end;

      fn := evaluated[0]() as TUserFunction;
      fnScope := TContext.Create(fn.context);

      // args[0] = function name
      // args[1] = function arguments
      // args[2:] = function body expressions

      // register arguments in context
      for i := 1 to evaluated().Size - 1 do
      begin
        symbol := fn.Args()[i - 1]() as TSymbol;
        Data := evaluated[i]();
        fnScope[symbol.Value] := evaluated[i];
      end;

      // execute the function code
      for i := 2 to fn.code().Size - 1 do
      begin
        Result := Eval(fn.code()[i], fnScope);
      end;

      // Clear the arguments from the context again
      for i := 1 to evaluated.Size - 1 do
      begin
        symbol := fn.Args()[i - 1]() as TSymbol;
        fnScope.Remove(symbol.Value);
      end;

      fnScope.Free;
    end
    else
        Result := code;
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

function TLisp.GetGlobalContext : TContext;
begin
  Result := FGlobal;
end;

function TLisp.Read(input : string) : Ref<TData>;
var
  i, j : Integer;
  inString : Boolean;
  list : Ref<TList>;
  item : Ref<TData>;
  text : string;
  charsTrimmed : Integer;
  anonymousFunction : Boolean;

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

  // remove all comments
  text := TRegEx.Replace(text, ';.*$', '');

  charsTrimmed := input.Length - text.Length;
  input := text;
  if Length(text) = 0 then Exit(CreateRef(TNothing.Create));

  // check for #( prefix which denotes an anonymous function def.
  if (input[1] = '#') and (input[2] = '(') then
  begin
    charsTrimmed := charsTrimmed + 1;
    input := input.Substring(1);
    anonymousFunction := True;
  end
  else anonymousFunction := False;

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

      text := Result().className;
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
  if anonymousFunction then
      list.Add(CreateRef(TSymbol.Create('anonymous-function')))
  else if input[1] = '[' then
      list.Add(CreateRef(TSymbol.Create('list')));

  while i <= Length(input) do
  begin
    if (input[i] = ')') or (input[i] = ']') or (input[i] = '}') then
    begin
      i := i + 1;
      break;
    end;

    item := read(input.Substring(i - 1));
    i := i + item.ConsumedCharacters;
    if not(item is TNothing) then
        list.Add(item);
  end;

  list.ConsumedCharacters := i - 1 + charsTrimmed;
  Result := Ref<TData>(list);
end;

procedure TLisp.RegisterFunction(fn : TFunction);
var
  native : TNativeFunction;
begin
  if fn is TNativeFunction then
  begin
    native := fn as TNativeFunction;
    FGlobal[native.name] := CreateRef(native);
  end;
end;

function TLisp.Eval(input : string) : DataRef;
begin
  Result := Eval(input, FGlobal);
end;

initialization

finalization

end.
