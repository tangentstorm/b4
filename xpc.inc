{ xpc.inc : include to ease cross-compiler portability }
{$IFDEF FPC}
  {$mode objfpc }
  {$rangechecks on }
  {$objectchecks on }
  {$overflowchecks on }
  {$assertions on }
  {$goto off }
  {$modeswitch nestedprocvars}
{$ENDIF}
