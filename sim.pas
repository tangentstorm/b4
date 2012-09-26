{
   simulator
   author  : michal j wallace
   license : MIT
}
{$i xpc.inc}
unit sim;
interface uses xpc;

  type
    runnable = interface
      property active : boolean;
      procedure step;
    end;     
    drawable = interface
      property visible : boolean;
      procedure show;
    end;     
    hardware = object
      constructor init;
      function send( msg : int32 ) : int32; virtual;
    end;		


implementation

  constructor hardware.init;
  begin
  end; { hardware.init }
  
  function hardware.send( msg: int32 ): int32;
  begin
    result := 0;
  end; { hardware.send }


end.
