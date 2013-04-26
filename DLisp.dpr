program DLisp;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Lisp in 'Lisp.pas';

var
  input : string;
  Lisp : TLisp;
  res : TData;

begin
  try
    begin
      ReportMemoryLeaksOnShutdown := DebugHook <> 0;

      Lisp := TLisp.Create();
      WriteLn('DLISP 0.1');
      WriteLn;

      repeat
        ReadLn(input);
        if input <> '' then
        begin
          res := Lisp.Eval(input);
          if res <> Nil then
          begin
            WriteLn(res.ToString);
            res.Release;
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
