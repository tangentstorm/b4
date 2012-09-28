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


implementation

end.
