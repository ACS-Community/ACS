%token TIDENT
%token TLBRACE TRBRACE
%token TLPAREN TRPAREN
%token TLBRACK TRBRACK
%token TOTHER
%token TAUTO
%token TCHAR
%token TCLASS
%token TCOLON
%token TCOMMA
%token TSEMI
%token TDBLCOLON
%token TENUM
%token TEXTERN
%token TEQUAL
%token TFLOAT 
%token TFRIEND
%token TINT
%token TLONG
%token TPUBLIC
%token TREG
%token TSHORT
%token TSTAR
%token TSTATIC
%token TSTRUCT
%token TTYPEDEF
%token TUNION
%token TUNSIGNED
%token TVIRTUAL
%token TVOID
%token TDEFINE
%token TNUM
%%
something : other
	| function
	| class
	| instance
	| something function
	| something instance
	| something class
	| something other;

function : ident TLPAREN {record_func();}
	| ident ident TLPAREN {record_func();} 
	| TSTAR ident TLPAREN {record_func();} ;
	| ident TSTAR ident TLPAREN {record_func();} ;

class : TCLASS ident
	 TCOLON ident
	 TLBRACE {record_class();}
	| TCLASS ident
	 TCOLON TPUBLIC ident 
	 TLBRACE {record_class();}
	| TCLASS ident 
	 TLBRACE {record_class();};

inst_head : ident inst_var inst_list
	|  ident inst_var TLBRACK {unput(yytext[0]);eat_til_either();} inst_list
	|  ident inst_var TEQUAL {eat_til_either();} inst_list;

inst_var : ident 
	| TSTAR inst_var;


inst_list : TCOMMA inst_var inst_list
	| TCOMMA inst_var TLBRACK {unput(yytext[0]);eat_til_either();} inst_list
	| TCOMMA inst_var TEQUAL {eat_til_either ();} inst_list
	| TCOMMA function /* this is an error really but it helps */
	|  ;

inst_list : error;

instance : inst_head {record_inst();};


storage : TAUTO
	| TEXTERN
	| TREG
	| TSTATIC;

ident : TIDENT ;

other :   inst_list {clean_stack();}
	| inst_var {clean_stack();}
	| ident TSTAR {clean_stack();}
	| TCOMMA ident TSTAR {clean_stack();}
	| TCOMMA inst_var TCOMMA {clean_stack();}
	| storage
	| TSEMI
	| TEQUAL
	| TLBRACE
	| TRBRACE
	| TLPAREN 
	| TRPAREN
	| TLBRACK
	| TRBRACK
	| TOTHER
	| TCHAR
	| TCLASS
	| TCOLON
	| TCOMMA
	| TDBLCOLON
	| TENUM
	| TFLOAT 
	| TFRIEND
	| TINT
	| TLONG
	| TNUM
	| TPUBLIC
	| TSHORT
	| TSTAR
	| TSTRUCT
	| TTYPEDEF
	| TUNION
	| TUNSIGNED
	| TVIRTUAL
	| TVOID
	| TDEFINE;

%%
#include "lexer.c"
char	lastcaller[YYLMAX] = "";

clean_stack ()
{
  save_ident * i;
  while (sp > 0)
    {
      i = pop();
      free (i->name);
      free (i);
    }
}

print_stack ()
{
  save_ident * i;
  while (sp > 0)
    {
      i = pop();
      printf ("print stack: %s\n", i->name);
    }
}

record_func()
{

  char caller[YYLMAX];
  save_ident *fname;

  fname = pop();
  if ( fname == NULL ) {
    printf("%d = level, %s = lastcaller \n", level, lastcaller);
    pfnote_c("foo", 'f', 0, 0, lastcaller);
    return;
  }

  caller[0]= '\0'; 
  if (level)
    {
      strcpy (caller , lastcaller);
    }
  pfnote_c(fname->name, 'f', fname->line, fname->charpos, caller);
  if (!level)
    {
      /* if (strcmp (lastcaller , "") error== 0) */
      strcpy (lastcaller , fname->name);
    }
  free(fname->name);
  free (fname);
  /* clean up stack because there must have been a type */
  if (sp > 0)
    {
      fname = pop();
      free (fname->name);
      free (fname);
    }
}

record_class()
{
  save_ident *cname;
  save_ident *sname;

  if (sp >= 2)
    { 
      sname = pop();
      cname = pop();
      pfnote_c(cname->name, 'c', cname->line, cname->charpos,
	       sname->name);
      free (cname->name);
      free(cname);
      free (sname->name);
      free(sname);
    }
  else
    {
      cname = pop();
      pfnote_c(cname->name, 'c', cname->line, cname->charpos,
	       "");
      free (cname->name);
      free (cname);
    }

}

record_inst()
{
  int n;
  save_ident *cname;
  save_ident *iname;

  if (sp < 2) return;		/* need at least a type and a instance */
  n = sp;
  cname = Stack[0];
  for (n = sp; n > 1; n--)
    {
      iname = pop ();
      pfnote_c (iname->name, 'i', iname->line, iname->charpos, 
		cname->name); 
      free (iname->name);
      free (iname);
    }
  iname = pop();
  free (iname->name);
  free (iname);			/* get rid of the class name at the bottom */

}

yyerror(s)
char *s;
{
  /* Called only when a syntax error is found by yyparse. */

  /* printf ("%s at line %d\n", s, yylineno); */
  clean_stack();
}

eat_til_match (brack, end_brack)
char brack, end_brack;
{
  int count = 1;
  char c;

  while (count > 0)
    {
      c = input ();
      if (c == end_brack)
        count--;
      else if (c == brack)
        count++;
    }
}

eat_til_either ()
{
  int  ret;
  int  stop;
  int  scope;
  save_ident * i;

  stop = 0;
  scope = 0;

  while (stop == 0)
    {
      ret = yylex();
      switch (ret)
        {
        case TCOMMA:
        case TSEMI:
	  stop = 1;
	  break;
        case TLBRACK:
        case TLBRACE:
	  scope++;
	  break;
        case TRBRACK:
        case TRBRACE:
	  scope--;
	  break;
	case TIDENT:
	  i = pop();
	  free (i->name);
	  free(i);
	  break;
        case YYEOF:		/* End-of-file. Stop at all costs */
	  stop++;
	  scope = 0;
	  break;
	  default:
	  break;
        }
      if (stop && (scope > 0)) stop = 0;
    }
  if (ret != YYEOF) unput(yytext[0]);
}
