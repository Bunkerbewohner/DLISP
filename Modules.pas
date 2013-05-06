unit Modules;

interface

uses
  Memory, Data, Common, System.IOUtils,
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
      FName : string;

    public
      constructor Create(moduleFile : string);
      destructor Destroy(); override;

      property Context : TContext read FContext;
      property Name : string read FName;

      function Load(runtime : TRuntime) : Boolean;
  end;

  TModuleManager = class
    protected
      FModules : TObjectDictionary<string,TModule>;
      FRuntime : TRuntime;

    public
      constructor Create(runtime : TRuntime);
      destructor Destroy(); override;

      property Modules : TObjectDictionary<string,TModule> read FModules;

      function Load(filename : string) : TModule;

  end;

implementation


{ TModule }

constructor TModule.Create(moduleFile: string);
begin
  FContext := TContext.Create(Nil);
  FSourceFile := moduleFile;

  FName := TPath.GetFileNameWithoutExtension(moduleFile);
end;

destructor TModule.Destroy;
begin
  FContext.Free;
  FRuntime := nil;
  inherited;
end;

function TModule.Load(runtime: TRuntime): Boolean;
begin
  runtime.Eval('(load "' + FSourceFile + '")', FContext);

  Result := True;
end;

{ TModuleManager }

constructor TModuleManager.Create(runtime : TRuntime);
begin
  FModules := TObjectDictionary<string, TModule>.Create([doOwnsValues]);
  FRuntime := runtime;
end;

destructor TModuleManager.Destroy;
begin
  FModules.Free;
  FRuntime := Nil;
  inherited;
end;

function TModuleManager.Load(filename: string) : TModule;
var
  module : TModule;
begin
  // TODO: Extract proper module name from filename

  // Check if the module already has been loaded
  if FModules.ContainsKey(filename) then
    Exit(FModules[filename]);

  // Otherwise load the module
  module := TModule.Create(filename);
  module.Load(FRuntime);
  FModules.Add(filename, module);

  Result := module;
end;

initialization

finalization

end.
