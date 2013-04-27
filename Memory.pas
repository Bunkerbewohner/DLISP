unit Memory;

interface

type
  // <summary>Smart Pointer</summary>
  Ref<T> = reference to function: T;

  /// <summary>
  /// Implementation of a smart pointer (interface Ref<T>).
  /// </summary>
  TRef<T : class, constructor> = class (TInterfacedObject, Ref<T>)
    private
      FValue : T;

    public
      ///<summary>Creates a new reference pointing to a new instance of T.</summary>
      constructor Create(); overload;
      ///<summary>Creates a new reference pointing to val.</summary>
      constructor Create(val : T); overload;
      destructor Destroy(); override;

      function Invoke : T;

      function Convert<T2 : class, constructor> : TRef<T2>;
  end;

  TRefHelper = class
    public
      class function Convert<T : class, constructor; V : class, constructor>(a : Ref<T>) : Ref<V>;
  end;

implementation

{ TRef<T> }

constructor TRef<T>.Create;
begin
  inherited;
  FValue := T.Create();
end;

function TRef<T>.Convert<T2>: TRef<T2>;
begin
  Result := TRef<T2>.Create(FValue as T2);
end;

constructor TRef<T>.Create(val: T);
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

function TRef<T>.Invoke: T;
begin
  Result := FValue;
end;

{ TRefHelper }

class function TRefHelper.Convert<T, V>(a: Ref<T>): Ref<V>;
begin
  Result := TRef<V>.Create(a() as V);
end;

end.
