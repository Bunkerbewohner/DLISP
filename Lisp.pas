unit Lisp;

interface

uses
  System.StrUtils,
  Classes,
  SysUtils,
  Memory,
  System.Generics.Collections,
  System.RegularExpressions;

var
  LispFormatSettings : TFormatSettings;
  ShowMemoryLog : Boolean;

type

  TData = class abstract(TInterfacedObject)
    private
      class var FInstances : TList<TData>;

    public
      Value : string;

      ConsumedCharacters : Integer;
      constructor Create();
      destructor Destroy(); override;

      procedure Release();
      function ToQualifiedString() : string;
  end;

  TNothing = class(TData)
    public
      constructor Create();

      function ToString : string; override;
  end;

  TAtom = class(TData)
    public
      constructor Create(v : string);

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
      constructor Create(list : Lisp.TList);

      function DoGetCurrent : Ref<TData>; override;
      function DoMoveNext : Boolean; override;
  end;

  TSymbol = class(TAtom)
    public

      class function Match(v : string) : Boolean;

  end;

  TString = class(TAtom)

  end;

  TBoolean = class(TAtom)
    public
      BoolValue : Boolean;

      constructor Create(v : string);

      class function Match(v : string) : Boolean;
  end;

  TNumber = class(TAtom)
    public
      function ToInteger() : Integer; virtual; abstract;
      function ToSingle() : Single; virtual; abstract;

      procedure Plus(b : TNumber); virtual; abstract;
  end;

  TInteger = class(TNumber)
    public
      IntValue : Integer;

      constructor Create(v : string); overload;
      constructor Create(v : Integer); overload;

      function ToInteger() : Integer; override;
      function ToSingle() : Single; override;

      procedure Plus(b : TNumber); override;

      class function Match(v : string) : Boolean;
  end;

  TFloat = class(TNumber)
    public
      FloatValue : Single;

      constructor Create(v : string); overload;
      constructor Create(v : Single); overload;

      function ToInteger() : Integer; override;
      function ToSingle() : Single; override;

      procedure Plus(b : TNumber); override;

      class function Match(v : string) : Boolean;
  end;

  TList = class(TData)
    protected
      Items : TList<DataRef>;

      function GetSize() : Integer;
      function GetItem(n : Integer) : DataRef;

    public

      property Size : Integer read GetSize;
      property DataItems[n : Integer] : DataRef read GetItem; default;

      procedure Add(item : DataRef);

      constructor Create();
      destructor Destroy(); override;

      function GetEnumerator : TEnumerator<DataRef>;

      function ToString : string; override;
  end;

  TContext = class
    protected
      FSymbols : TDictionary<string, DataRef>;

      function GetSymbol(name : string) : Ref<TData>;
      procedure SetSymbol(name : string; data : Ref<TData>);

    public
      constructor Create();
      destructor Destroy(); override;

      property Symbols[name : string] : Ref<TData>
        read GetSymbol write SetSymbol; default;
  end;

  ProcBuiltIn = reference to function(context : TContext; args : Ref<TList>) : Ref<TData>;

  TLisp = class

    protected
      FGlobal : TContext;
      FBuiltIns : TDictionary<string, ProcBuiltIn>;

      function Eval(code : Ref<TData>) : Ref<TData>; overload;

    public
      constructor Create();
      destructor Destroy(); override;

      function Read(input : string) : Ref<TData>;
      function Eval(input : string) : Ref<TData>; overload;

      // ========================================================================
      // built-in functions
      // ========================================================================

      /// <summary>Defines a *new* variable in the current context</summary>
      function _def(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Returns the data type of the first argument</summary>
      function _type(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Prints a variable number of arguments to stdout</summary>
      function _print(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Returns the first argument unevaluated</summary>
      function _quote(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Creates a list of given arguments</summary>
      function _list(context : TContext; args : Ref<TList>) : Ref<TData>;

      // math
      function _plus(context : TContext; args : Ref<TList>) : Ref<TData>;

      /// <summary>Gets or sets DISP configuration settings</summary>
      function __cfg(context : TContext; args : Ref<TList>) : Ref<TData>;

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
  FGlobal := TContext.Create();

  FBuiltIns := TDictionary<string, ProcBuiltIn>.Create();
  FBuiltIns.Add('def', _def);
  FBuiltIns.Add('type', _type);
  FBuiltIns.Add('print', _print);
  FBuiltIns.Add('list', _list);
  FBuiltIns.Add('quote', _quote);
  FBuiltIns.Add('__cfg', __cfg);

  FBuiltIns.Add('+', _plus);
end;

destructor TLisp.Destroy;
begin
  FGlobal.Free;
  FBuiltIns.Free;
  inherited;
end;

function TLisp.Eval(code : Ref<TData>) : Ref<TData>;
var
  list, evaluated : Ref<TList>;
  opName, symbol : TSymbol;
  expr : Ref<TData>;
begin
  if code() is TSymbol then
  begin
    symbol := code() as TSymbol;
    if FGlobal.FSymbols.ContainsKey(symbol.Value) then
    begin
      Result := FGlobal[symbol.Value];
    end
    else
    begin
      Result := code;
    end;
  end
  else if code() is TList then
  begin
    list := Ref<TList>(code);
    if not(list[0]() is TSymbol) then
        raise Exception.Create(list[0]().ToString() + ' is not a function');

    opName := list[0]() as TSymbol;
    if FBuiltIns.ContainsKey(opName.Value) then
    begin
      // don't evaluate the parameters; the built-in functions must decide
      // which arguments they want quoted or evaluated
      Result := FBuiltIns[opName.Value](FGlobal, list);
    end
    else
    begin
      raise Exception.Create('TODO');

      // this looks like a function application; evaluate all arguments
      evaluated := TRef<TList>.Create(TList.Create());
      for expr in list do
      begin
        evaluated.Add(Eval(expr));
      end;

      Result := FGlobal[opName.Value];
    end;
  end
  else
  begin
    Result := code;
  end;
end;

function TLisp.Eval(input : string) : Ref<TData>;
begin
  Result := Eval(read(input));
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
      text := input.Substring(0, i);
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

function TLisp._def(context : TContext; args : Ref<TList>) : Ref<TData>;
var
  symbol : TSymbol;
begin
  if args.Size < 3 then
      raise Exception.Create('def: not enough arguments');

  if args[1]() is TSymbol then
      symbol := args[1]() as TSymbol
  else
      symbol := Eval(args[1])() as TSymbol;

  Result := Eval(args[2]);
  context[symbol.Value] := Result;
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
    list.Add(Eval(args[i]));
  end;

  Result := CreateRef(list);
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
    res := Eval(args[i]);
    Writeln(res.ToString);
  end;

  Result := CreateRef(TNothing.Create());
end;

function TLisp._quote(context : TContext; args : Ref<TList>) : Ref<TData>;
begin
  Result := args[1];
end;

function TLisp._type(context : TContext; args : Ref<TList>) : Ref<TData>;
var
  name : string;
  val : Ref<TData>;
begin
  val := Eval(args[1]);
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

{ TParseList }

procedure TList.Add(item : Ref<TData>);
begin
  Items.Add(item);
end;

constructor TList.Create;
begin
  inherited;
  Items := TList<DataRef>.Create();
end;

destructor TList.Destroy;
begin
  Items.Free;
  inherited;
end;

function TList.GetEnumerator : TEnumerator<Ref<TData>>;
begin
  Result := TLispListEnumerator.Create(self);
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
  i : Integer;
  DataRef : Ref<TData>;
  data : TData;
begin
  Result := '(';
  for i := 0 to Items.Count - 1 do
  begin
    if i > 0 then Result := Result + ' ';
    DataRef := Ref<TData>(Items[i]);
    data := DataRef();
    Result := Result + data.ToString;
  end;
  Result := Result + ')';
end;

{ TParseString }

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

constructor TContext.Create;
begin
  FSymbols := TDictionary<string, DataRef>.Create();
end;

destructor TContext.Destroy;
begin
  inherited;
  FSymbols.Free;
end;

function TContext.GetSymbol(name : string) : Ref<TData>;
begin
  if FSymbols.ContainsKey(name) then
  begin
    Result := FSymbols[name];
  end
  else
  begin
    raise Exception.Create('Symbol "' + name + '" unknown');
  end;
end;

procedure TContext.SetSymbol(name : string; data : Ref<TData>);
begin
  if FSymbols.ContainsKey(name) then
  begin
    FSymbols[name] := DataRef(data);
  end
  else
  begin
    FSymbols.Add(name, DataRef(data));
  end;
end;

{ TInteger }

constructor TInteger.Create(v : string);
begin
  inherited;
  IntValue := StrToInt(v);
end;

constructor TInteger.Create(v : Integer);
begin
  IntValue := v;
  Value := IntToStr(v);
end;

class function TInteger.Match(v : string) : Boolean;
begin
  Result := TRegEx.IsMatch(v, '^\d+$');
end;

procedure TInteger.Plus(b : TNumber);
begin
  IntValue := IntValue + b.ToInteger;
  Value := IntToStr(IntValue);
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
  FloatValue := StrToFloat(v, LispFormatSettings);
end;

constructor TFloat.Create(v : Single);
begin
  FloatValue := v;
  Value := FloatToStr(v);
end;

class function TFloat.Match(v : string) : Boolean;
begin
  Result := TRegEx.IsMatch(v, '^\d+\.\d+');
end;

procedure TFloat.Plus(b : TNumber);
begin
  FloatValue := FloatValue + b.ToSingle;
  Value := FloatToStr(FloatValue);
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

class function TSymbol.Match(v : string) : Boolean;
begin
  Result := TRegEx.IsMatch(v, '^[\D][\S]*$');
end;

{ TBoolean }

constructor TBoolean.Create(v : string);
begin
  inherited;
  BoolValue := StrToBool(v);
end;

class function TBoolean.Match(v : string) : Boolean;
var
  lower : string;
begin
  lower := v.ToLower;
  Result := lower.Equals('true') or lower.Equals('false');
end;

{ TData }

constructor TData.Create;
begin
  // FInstances.Add(self);
  Value := '()';
end;

destructor TData.Destroy;
begin
  FInstances.Remove(self);
  if ShowMemoryLog then
      Writeln('Destroy ' + self.ToQualifiedString);
  inherited;
end;

procedure TData.Release;
begin
  self._Release;
end;

function TData.ToQualifiedString : string;
begin
  Result := self.ClassName + '[' + self.ToString + ']';
end;

procedure CheckMemoryLeaks();
var
  x : TData;
begin
  if TData.FInstances.Count > 0 then
  begin
    Writeln('Warning: Memory leaks found!');
    Writeln;
    for x in TData.FInstances do
    begin
      Writeln(x.ToQualifiedString + ' (' + IntToStr(x.RefCount) + ' references)');
    end;
    Readln;
  end;
end;

{ TLispListEnumerator }

constructor TLispListEnumerator.Create(list : Lisp.TList);
begin
  self.FList := list;
  self.FCurrent := nil;
  self.FIndex := 0;
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

constructor TNothing.Create;
begin
  Value := '';
end;

function TNothing.ToString : string;
begin
  Result := 'TNothing';
end;

initialization

TData.FInstances := TList<TData>.Create();
LispFormatSettings := TFormatSettings.Create('en_US');
ShowMemoryLog := False;

finalization

CheckMemoryLeaks;
TData.FInstances.Free;

end.
