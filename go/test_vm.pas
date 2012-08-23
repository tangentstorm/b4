unit test_vm;
interface procedure run;
implementation uses vm;

var core : vm.core;

procedure run;
begin
  core.init;
end;

end.