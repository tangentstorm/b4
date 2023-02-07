{ database assembler }
{$mode objfpc}{$H+}
program mr_dba;
uses sqlite3, sqlite3ds, di;

type
  TCharGen  = function (var ch: char): char;
  TTokenTag = ( ttEOF, ttWS, ttComment, ttStr, ttInt,
	        ttTbl, ttFlags, ttType  );
  TToken = record
    tag : TTokenTag;
    rep : byte;
    sid : UInt32;
  end;

var
  stringdb : array of string;
  ch : char = #0;

function ReadToken(next : TCharGen): TToken;
  begin
    case next(ch) of
      #0..#32: result.tag := ;
      '"' :    result.tag := ttStr;
      '@' :    result.tag := ;
    end
  end;

begin
  
end.
