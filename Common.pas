unit Common;

interface

uses
  Data;

type

  TRuntime = class abstract

    protected
      function GetGlobalContext : TContext; virtual; abstract;

    public
      function Read(input : string) : DataRef; virtual; abstract;
      function Eval(input : string; context : TContext) : DataRef; overload; virtual; abstract;
      function Eval(code : DataRef; context : TContext) : DataRef; overload; virtual; abstract;

      property GlobalContext : TContext read GetGlobalContext;

      procedure RegisterFunction(fn : TFunction); virtual; abstract;

  end;

implementation

{ TRuntime }

end.
