unit tptester;
interface uses crt;
{
  This is a simple procedural test harness for turbo pascal.
  The idea is you put all your tests in a testselector procedure,
  which takes a test number as an argument.

  The number simply counts up until it reaches 65535 or you call
  tptester.stop;

}
type
  setuphandler = procedure;
  testselector = procedure( testnum : word );

{ use test('name') inside your testselector to name a group of assertions }
procedure Test(name:string);

{ use Assert(1+1=2,'one plus one should equal two') to test a condition }
procedure Assert(condition:boolean; message:string);

{ call Stop when the counter reaches the end of your test suite }
procedure Stop;

{ call OnSetup if you want to install a testhandler }
procedure OnSetup(handler:setuphandler);

{ to disable the setup handler, call OnSetup(NoSetup).
  (this is the default) }
procedure NoSetup;

{ call RunTests() to run your tests }
procedure RunTests(select:testselector);

{ call ResetErrors if you want to clear the error results }
procedure ResetErrors;


implementation

procedure nosetup;
  begin
    { default setup handler. intentionally blank. }
  end;

type
  ErrorMessage = record
    num : word;
    grp : string;
    msg : string;
  end;

const
  maxError = 16;
  maxTest  = 1024;
var
  errors: array[1..maxError] of ErrorMessage;
  errorcount : byte;
  setup : setuphandler;
  selector : testselector;
  testcounter : word;
  groupname : string;
  localchecks, totalchecks : word;
  done, toomany : boolean;

procedure error(message:string);
  begin
    inc(errorcount);
    if errorcount > 16 then toomany := true
    else with errors[errorcount] do
      begin
        num := testcounter;
        grp := groupname;
        msg := message;
      end;
  end;

procedure assert(condition:boolean; message:string);
  begin
    inc(localchecks); inc(totalchecks);
    if condition then
      begin textcolor(green); write('.') end
    else
       begin
         textcolor(red); write('x');
         error(message);
       end;
  end;

procedure stop;
  begin
    done := true
  end;


procedure OnSetup(handler:setuphandler);
  begin
    setup := handler
  end;

procedure test(name:string);
  begin
    groupname := name
  end;

procedure runtests(select:testselector);
  var i : byte;
  begin
    done := false; toomany:=false; testcounter := 0; totalchecks := 0;
    while (testcounter < maxtest) and not (done or toomany) do
      begin
        localchecks := 0;
        setup; select(testcounter);
        inc(testcounter);
        if (localchecks = 0) and not done then
         begin textcolor(8); write('-') end;
      end;
    writeln;
    textcolor(yellow);     write(totalchecks);
    textcolor(lightgray);  write(' assertions issued. ');
    if errorcount < totalchecks then textcolor(green);
    write(totalchecks-errorcount);
    textcolor(lightgray);  write(' passed, ');
    if errorcount > 0 then textcolor(red);
    write(errorcount);
    textcolor(lightgray);  write(' errors.');
    writeln;
    for i := 1 to errorcount do with errors[errorcount] do
      begin
        textcolor(darkgray); write('#', num:3, ': ', grp, ' ');
        textcolor(red); writeln(msg);
      end;
    if errorcount = maxerror then begin
      textcolor(lightred); writeln('gave up after ', maxerror, ' errors.');
    end;
    normvideo;
  end;

procedure ResetErrors;
  var i : byte;
  begin
    errorcount := 0; totalchecks:=0;
    fillchar(errors, sizeof(errors), 0);
  end;

begin
  ResetErrors;
  OnSetup(NoSetup);
end.