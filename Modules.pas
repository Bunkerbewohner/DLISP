unit Modules;

interface

uses
  Memory, Data, Common, Lisp, System.IOUtils,
  System.Generics.Collections;

type

  /// <summary>
  /// A module is a set of functions and types in a common namespace.
  /// </summary>
  TModule = class
    protected
      FSourceFile : string;
      FContext : TContext;
      FRuntime : TRuntime;

    public
      constructor Create(moduleFile : string);
      destructor Destroy(); override;

      function Load(runtime : TRuntime) : Boolean;
  end;

  TModuleManager = class
    protected
      FModules : TObjectList<TModule>;

    public
      constructor Create();
      destructor Destroy(); override;

  end;

var
  ModuleManager : TModuleManager;

implementation


{ TModule }

constructor TModule.Create(moduleFile: string);
begin
  FContext := TContext.Create(Nil);
  FSourceFile := moduleFile;
end;

destructor TModule.Destroy;
begin
  FContext.Free;
  FRuntime := nil;
  inherited;
end;

function TModule.Load(runtime: TRuntime): Boolean;
var
  text : string;
  res : DataRef;
begin
  try
    text := TFile.ReadAllText(FSourceFile);
    runtime.Eval(text, FContext);
  except
    Exit(False);
  end;

  // Just

  Result := True;
end;

{ TModuleManager }

constructor TModuleManager.Create;
begin
  FModules := TObjectList<TModule>.Create();
end;

destructor TModuleManager.Destroy;
begin
  FModules.Free;
  inherited;
end;

initialization

ModuleManager := TModuleManager.Create;

finalization

ModuleManager.Free;

end.
