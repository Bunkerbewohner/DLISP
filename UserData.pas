unit UserData;

interface

uses
  Data,
  Collections;

type
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


  /// <summary>
  /// An abstract object implementation for DLISP.
  /// </summary>
  TLispObject = class(TAbstractObject)

  end;

implementation

{ TFunction }

function TUserFunction.Copy : TData;
var
  fn : TUserFunction;
begin
  fn := TUserFunction.Create(FCode, FContext.Parent);
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
  Assert(code().Size > 2);

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

end.
