unit Lisp;

interface

uses
  System.StrUtils,
  Classes,
  SysUtils,
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

  TAtom = class(TData)
    public
      constructor Create(v : string);

      function ToString : string; override;
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
      function GetSize() : Integer;

      function GetItem(n : Integer) : TData;

    public
      Items : TObjectList<TData>;

      property Size : Integer read GetSize;
      property DataItems[n : Integer] : TData read GetItem; default;

      constructor Create();
      destructor Destroy(); override;

      function ToString : string; override;
  end;

  TContext = class
    protected
      FSymbols : TObjectDictionary<string, TData>;

      function GetSymbol(name : string) : TData;
      procedure SetSymbol(name : string; data : TData);

    public
      constructor Create();
      destructor Destroy(); override;

      property Symbols[name : string] : TData
        read GetSymbol write SetSymbol; default;
  end;

  ProcBuiltIn = reference to function(context : TContext; args : TList) : TData;

  TLisp = class

    protected
      FGlobal : TContext;
      FBuiltIns : TDictionary<string, ProcBuiltIn>;

      function Eval(code : TData) : TData; overload;

    public
      constructor Create();
      destructor Destroy(); override;

      function Read(input : string) : TData;
      function Eval(input : string) : TData; overload;

      // built-in functions
      function _def(context : TContext; args : TList) : TData;
      function _set(context : TContext; args : TList) : TData;
      function _type(context : TContext; args : TList) : TData;
      function _print(context : TContext; args : TList) : TData;
      function __cfg(context : TContext; args : TList) : TData;

      function _list(context : TContext; args : TList) : TData;

      // math
      function _plus(context : TContext; args : TList) : TData;

  end;

implementation

{ TLisp }

constructor TLisp.Create;
begin
  FGlobal := TContext.Create();

  FBuiltIns := TDictionary<string, ProcBuiltIn>.Create();
  FBuiltIns.Add('def', _def);
  FBuiltIns.Add('set!', _set);
  FBuiltIns.Add('type', _type);
  FBuiltIns.Add('print', _print);
  FBuiltIns.Add('list', _list);
  FBuiltIns.Add('__cfg', __cfg);

  FBuiltIns.Add('+', _plus);
end;

destructor TLisp.Destroy;
begin
  FGlobal.Free;
  FBuiltIns.Free;
  inherited;
end;

function TLisp.Eval(code : TData) : TData;
var
  list, evaluated : TList;
  opName, symbol : TSymbol;
  expr : TData;
begin
  if code is TSymbol then
  begin
    symbol := code as TSymbol;
    if FGlobal.FSymbols.ContainsKey(symbol.Value) then
    begin
      Result := FGlobal[symbol.Value];
      Result._AddRef;
      code.Release;
    end
    else
    begin
      Result := symbol;
    end;
  end
  else if code is TList then
  begin
    list := code as TList;
    if not(list.Items[0] is TSymbol) then
        raise Exception.Create(list.Items[0].ToString() + ' is not a function');

    opName := list.Items[0] as TSymbol;
    if FBuiltIns.ContainsKey(opName.Value) then
    begin
      // don't evaluate the parameters; the built-in functions must decide
      // which arguments they want quoted or evaluated
      Result := FBuiltIns[opName.Value](FGlobal, list);
      list.Release;
    end
    else
    begin
      raise Exception.Create('TODO');
      // this looks like a function application; evaluate all arguments
      evaluated := TList.Create();
      for expr in list.Items do evaluated.Items.Add(Eval(expr));

      if not(evaluated[0] is TSymbol) then
          raise Exception.Create(evaluated[0].ToString() + ' is not a function');

      opName := (evaluated.Items[0] as TSymbol);
      Result := FGlobal[opName.Value];

      Result._AddRef; // inc ref for occurence in list
      Result._AddRef; // inc ref for occurence in evaluated copy

      list.Free;
      evaluated.Free;
    end;
  end
  else
  begin
    Result := code;
  end;
end;

function TLisp.Eval(input : string) : TData;
begin
  Result := Eval(read(input));
end;

function TLisp.Read(input : string) : TData;
var
  i, j : Integer;
  inString : Boolean;
  list : TList;
  item : TData;
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

      if TInteger.Match(text) then Result := TInteger.Create(text)
      else if TFloat.Match(text) then Result := TFloat.Create(text)
      else if TBoolean.Match(text) then Result := TBoolean.Create(text)
      else if TSymbol.Match(text) then Result := TSymbol.Create(text)
      else Result := TAtom.Create(text);

      text := Result.ClassName;
      Result.ConsumedCharacters := i - 1;
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
      Result := TString.Create(text);
      Result.ConsumedCharacters := i;
    end;

    Result.ConsumedCharacters := Result.ConsumedCharacters + charsTrimmed;
    Result._AddRef;
    Exit(Result);
  end;

  // assume it's a list
  i := 2;

  list := TList.Create();

  while i <= Length(input) do
  begin
    if (input[i] = ')') or (input[i] = ']') or (input[i] = '}') then
    begin
      i := i + 1;
      break;
    end;

    item := read(input.Substring(i - 1));
    i := i + item.ConsumedCharacters;
    list.Items.Add(item);
  end;

  list.ConsumedCharacters := i - 1 + charsTrimmed;
  Result := list;
  Result._AddRef;
end;

function TLisp._def(context : TContext; args : TList) : TData;
var
  symbol : TSymbol;
  Value : TData;
begin
  if args.Size < 3 then
      raise Exception.Create('def: not enough arguments');
  if not(args[1] is TSymbol) then
      raise Exception.Create('def: ' + args[1].ToString + ' is not a valid symbol');

  symbol := args[1] as TSymbol;
  Value := args[2] as TData;

  Result := Eval(Value);
  context[symbol.Value] := Result;
  Result._AddRef;
end;

function TLisp._list(context : TContext; args : TList) : TData;
var
  i : Integer;
  list : TList;
begin
  list := TList.Create();
  for i := 1 to args.Size - 1 do
  begin
    list.Items.Add(Eval(args[i]));
    list[i - 1]._AddRef;
  end;

  list._AddRef;
  Result := list;
end;

function TLisp._plus(context : TContext; args : TList) : TData;
var
  resultType : string;
  i, sumInt : Integer;
  sumFloat : Single;
  evaluatedArgs : TList;
  arg, item : TData;
  resInt : TInteger;
  resFloat : TFloat;
begin
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
end;

function TLisp._print(context : TContext; args : TList) : TData;
var
  i : Integer;
  res : TData;
begin
  for i := 1 to args.Size - 1 do
  begin
    res := Eval(args[i]);
    Writeln(res.ToString);
    res.Release;
  end;

  Result := nil;
end;

function TLisp._set(context : TContext; args : TList) : TData;
var
  symbol : TSymbol;
  Value : TData;
begin
  if args.Size < 3 then
      raise Exception.Create('set!: not enough arguments');
  if not(args[1] is TSymbol) then
      raise Exception.Create('set!: ' + args[1].ToString + ' is not a valid symbol');

  symbol := args[1] as TSymbol;
  Value := args[2] as TData;

  if not(context.FSymbols.ContainsKey(symbol.Value)) then
      raise Exception.Create('set!: variable ' + symbol.Value + ' does not exist');

  Result := Eval(Value);
  context[symbol.Value] := Result;
  Result._AddRef;
end;

function TLisp._type(context : TContext; args : TList) : TData;
var
  name : string;
  val : TData;
begin
  args[1]._AddRef;
  val := Eval(args[1]);
  Result := TString.Create(val.ClassName);
  val.Release;
  Result._AddRef;
end;

function TLisp.__cfg(context : TContext; args : TList) : TData;
var
  setting : TSymbol;
  Value : TData;
  bool : TBoolean;
begin
  setting := args[1] as TSymbol;
  if setting.Value = 'ShowMemoryLog' then
  begin
    if args.Size = 3 then
    begin
      bool := args[2] as TBoolean;
      ShowMemoryLog := bool.BoolValue;
      Result := bool;
      Result._AddRef;
    end
    else
    begin
      if ShowMemoryLog then Result := TBoolean.Create('True')
      else Result := TBoolean.Create('False');
      Result._AddRef;
    end;
  end
  else
  begin
    Result := nil;
  end;
end;

{ TParseList }

constructor TList.Create;
begin
  inherited;
  Items := TObjectList<TData>.Create(False);
end;

destructor TList.Destroy;
var
  i : Integer;
  temp : TData;
  text : string;
begin
  for temp in Items do
  begin
    temp.Release;
  end;
  Items.Free;
  inherited;
end;

function TList.GetItem(n : Integer) : TData;
begin
  Result := Items[n];
end;

function TList.GetSize : Integer;
begin
  Result := Items.Count;
end;

function TList.ToString : string;
var
  i : Integer;
begin
  Result := '(';
  for i := 0 to Items.Count - 1 do
  begin
    if i > 0 then Result := Result + ' ';
    Result := Result + Items[i].ToString;
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
  FSymbols := TObjectDictionary<string, TData>.Create([]);
end;

destructor TContext.Destroy;
var
  vs : TArray<TData>;
  v : TData;
begin
  inherited;
  vs := FSymbols.Values.ToArray;
  FSymbols.Free;
  for v in vs do
  begin
    v.Release;
  end;
end;

function TContext.GetSymbol(name : string) : TData;
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

procedure TContext.SetSymbol(name : string; data : TData);
begin
  if FSymbols.ContainsKey(name) then
  begin
    FSymbols[name].Release;
    FSymbols[name] := data;
    data._AddRef;
  end
  else
  begin
    FSymbols.Add(name, data);
    data._AddRef;
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

procedure TInteger.Plus(b: TNumber);
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

procedure TFloat.Plus(b: TNumber);
begin
  FloatValue := FloatValue + b.ToSingle;
  Value := FloatToStr(FloatValue);
end;

function TFloat.ToInteger: Integer;
begin
  Result := Round(FloatValue);
end;

function TFloat.ToSingle: Single;
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
  FInstances.Add(self);
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

initialization

TData.FInstances := TList<TData>.Create();
LispFormatSettings := TFormatSettings.Create('en_US');
ShowMemoryLog := False;

finalization

CheckMemoryLeaks;
TData.FInstances.Free;

end.
