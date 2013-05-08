unit Memory;

interface

type
  // <summary>Smart Pointer</summary>
  Ref<T> = reference to function : T;

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
  end;

implementation

{ TRef<T> }

constructor TRef<T>.Create;
begin
  inherited;
  FValue := T.Create();
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

end.
