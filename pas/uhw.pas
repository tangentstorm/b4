// b4 virtual hardware devices
{$mode objfpc }
unit uhw;

interface

type
  TB4Device = class
    constructor create;
    procedure invoke(op : char); virtual;
  end;

implementation
uses ub4;

constructor TB4Device.create;
begin
end;

procedure TB4Device.invoke(op:char);
begin
  if op='e' then write(op);
end;

begin
end.
