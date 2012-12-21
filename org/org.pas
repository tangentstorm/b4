{$i xpc.inc}
unit org;
interface uses xpc, di, li, ll, stacks, stri;

  type
    node_type = (
      nt_document,
      nt_headline,
      nt_blank,
      nt_markup,
      nt_comment,
      nt_block,
      nt_meta,
      nt_macro,
      nt_result,
      nt_verbatim,
      nt_table,
      nt_tbl_row,
      nt_tbl_div,
      nt_source,
      nt_property,
      nt_drawer
    );
    block_type	  = (
      bl_center, bl_docbook, bl_example,
      bl_html, bl_latex, bl_lstlisting, { ?? }
      bl_quote, bl_result, bl_src, bl_verbatim, bl_verse );
    tok_meta	  = (
      tm_bind,
      tm_caption,
      tm_category,
      tm_columns,
      tm_description,
      tm_docbook,
      tm_drawers,
      tm_email,
      tm_export_exclude_tags,
      tm_export_select_tags,
      tm_filetags,
      tm_header,
      tm_headers,
      tm_html,
      tm_include,
      tm_infojs_opt,       tm_keywords,
      tm_label,          tm_language,      tm_latex,
      tm_latex_class,
      tm_latex_class_options,              tm_latex_header,
      tm_link,
      tm_link_home,      tm_link_up,       tm_macro,
      tm_mathjax,
      tm_name,           tm_options,       tm_orgtbl,
      tm_priorities,
      tm_property,       tm_results,       tm_seq_todo,
      tm_setupfile,
      tm_startup,        tm_tags,          tm_tblfm,
      tm_tblname,
      tm_title,          tm_todo,          tm_typ_todo,
      tm_xslt );

    generic element<tag_t,val_t> = class ( li.node )
    private
      type
        this = specialize element< tag_t, val_t >;
        elements = specialize list<this>;
    public
      tag       : tag_t;
      value     : val_t;
      children  : elements;
      constructor create( t : tag_t );
      constructor create( t : tag_t; v : val_t );
      procedure append( t : tag_t );
      procedure append( t : tag_t; v : val_t );
    end;
    generic document<tag_t> = class(specialize element<tag_t, string>)
    end;
    org_node = class ( specialize document< node_type >)
      depth	 : cardinal;
      headln	 : string;
      content	 : specialize list<org_node>;
      tags	 : stringdict;
      keyword	 : string;
      priority	 : cardinal;
      scheduled,
      deadline	 : string; {todo: datetime;}
      properties : stringdict;
    end;
    org_stack= specialize stacks.stack<org_node>;
    orgvisitor= class
    end;
    emitter  = class( orgvisitor )
      nodes : nodelist;
      procedure comment( line : string );
      procedure verbatim( line : string );
      procedure markup( line : string );
    end;
    table_node= class( org_node )
      constructor create;
      procedure row( line : string );
      procedure divider( line : string );
    end;
    divider_node= class( org_node )
    end;
    block_node= class( org_node )
      endline : string;
      constructor create( line : string );
      function consume_end( line : string ) : boolean;
    end;
    drawer_node= class( org_node )
      endline : string;
      pairs   : stringdict;
      constructor create( line : string );
      function consume_end( line : string ) : boolean;
    end;

    function parse( var tx : text; out root : org_node ) : boolean;

implementation

  constructor element.create( t : tag_t );
  begin
    self.tag := t;
    self.children := elements.create;
  end;

  constructor element.create( t : tag_t; v : val_t );
  begin
    self.create( t );
    self.value := v
  end;

  procedure element.append( t : tag_t );
  begin
    self.children.append( element.create( t ));
  end;

  procedure element.append( t : tag_t; v : val_t );
  begin
    self.children.append( element.create( t, v ));
  end;

  procedure emitter.comment( line : string );
  begin
    self.nodes.append( org_node.create( nt_comment, line ));
  end;
  procedure emitter.verbatim( line : string );
  begin
    self.nodes.append( org_node.create( nt_verbatim, line ));
  end;

  procedure emitter.markup( line : string );
  begin
    self.nodes.append( org_node.create( nt_markup, line ));
  end; { emitter.markup }

  constructor table_node.create;
  begin inherited create( nt_table );
  end;

  procedure table_node.row( line : string );
  begin
    self.append( nt_tbl_row, line );
  end;

  procedure table_node.divider( line : string );
  begin
    self.append( nt_tbl_div, line )
  end;

  constructor block_node.create( line : string );
  begin inherited create( nt_block, line );
  end;

  constructor drawer_node.create( line : string );
  begin inherited create( nt_drawer, line );
  end;

  function block_node.consume_end( line : string ) : boolean;
  begin
    result := startswith( dnstr( line ), '#+end_' );
    if result then self.endline := line
    else self.append( nt_source, line );
  end;

  function drawer_node.consume_end( line : string ) : boolean;
  begin
    // pretty sure it's case sensitive:
    result := startswith( line, ':end:' );
    if result then self.endline := line
    else self.append( nt_property, line )
  end;

  function parse( var tx : text; out root : org_node ) : boolean;
    const
      k_max_depth = 8;
    var
      line  : string;
      depth : cardinal = 0;
      stack : org_stack;
      focus : org_node;

    procedure emit( nt : node_type );
    begin
      focus.append( nt_verbatim, line );
    end;

    procedure headline;
    var
      i : byte = 1;
      j : byte;
    begin
      result := false;
      if line[ i ] = '*' then begin
          result := true;
          while line[ i ] = '*' do inc( i );
          for j := i downto depth do stack.pop;
          depth := i;
        end
    end; { headline }

    function parse_block : block_node;
    begin
      result := block_node.create( line );
      repeat readln( tx, line )
      until eof( tx ) or result.consume_end( line );
    end;

    function parse_drawer : drawer_node;
    begin
      result := drawer_node.create( line );
      repeat readln( tx, line ) until result.consume_end( line )
    end;

    function parse_table : table_node;
    begin
      result := table_node.create;
      repeat
	if line[ 1 ] = '|' then result.row( line )
	else result.divider( line );
	readln( tx, line )
      until not (line[ 1 ] in [ '|','+' ]);
    end;

  begin { parse }
    stack.init( k_max_depth );
    root := org_node.create( nt_document );
    focus := root;
    while not eof( tx ) do begin
      readln( tx, line );
      // log.debug([ '>>' + line ]);
      if length( line ) = 0 then emit( nt_blank )
      else case line[ 1 ] of
	'*' : emit( nt_headline );
	'#' : if startswith( dnstr( line ), '#+begin' ) then parse_block
	      else if ( length( line ) > 1 ) and ( line[ 2 ] = '+' )
              then emit( nt_meta )
	      else emit( nt_comment );
        ':' : if line[ 2 ] = ' ' then emit( nt_verbatim )
              else parse_drawer;
	'|' : parse_table;
      else emit( nt_markup );
      end
    end;
  end; { parse }

initialization
end.
