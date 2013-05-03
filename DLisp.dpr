program DLisp;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  Lisp in 'Lisp.pas',
  Memory in 'Memory.pas',
  Data in 'Data.pas',
  NativeFunctions in 'NativeFunctions.pas',
  Common in 'Common.pas',
  Modules in 'Modules.pas';

var
  input : string;
  Lisp : TLisp;
  res : Ref<TData>;
begin
  try
    begin
      ReportMemoryLeaksOnShutdown := DebugHook <> 0;

      Lisp := TLisp.Create();
      WriteLn('DLISP 0.1');

      repeat
        ReadLn(input);
        if input <> '' then
        begin
          try
            res := Lisp.Eval(input, Lisp.GlobalContext);
            if not (res() is TNothing) then
            begin
              WriteLn(res.ToString);
            end;
          except
            on e : Exception do
            begin
              WriteLn('Error: ' + e.Message);
            end;
          end;
        end;
      until input = '';

      Lisp.Free;
    end;
  except
    on E : Exception do
        WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
