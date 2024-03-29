/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> ORDER
%token <Support.Error.info> FULL_ORDER
%token <Support.Error.info> NORMAL_ORDER
%token <Support.Error.info> APPLICATIVE_ORDER
%token <Support.Error.info> CBN_ORDER
%token <Support.Error.info> CBV_ORDER
%token <Support.Error.info> TRACE
%token <Support.Error.info> TYPING
%token <Support.Error.info> ON
%token <Support.Error.info> OFF
%token <Support.Error.info> STEP

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Syntax.context -> (Syntax.command list * Syntax.context) > toplevel

%start toplevel1
%type < Syntax.context -> (Syntax.command option * Syntax.context) > toplevel1
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { fun ctx -> [],ctx }
  | Command SEMI toplevel
      { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          cmd::cmds,ctx }

toplevel1 :
  | EOF
      { fun ctx -> None,ctx }
  | Command SEMI
    { fun ctx -> let cmd,ctx = $1 ctx in Some cmd, ctx}

/* A top-level command */
Command :
  | Term 
      { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)),ctx }
  | LCID Binder
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }
  | ORDER OfOrder
      { fun ctx -> ((Order($1,$2 ctx)),ctx) }
  | TRACE Switch
      { fun ctx -> ((Trace($1,$2 ctx)),ctx) }
  | TYPING Switch
      { fun ctx -> ((Typing($1,$2 ctx)),ctx) }
  | STEP OfStep
      { fun ctx -> ((Step($1, $2 ctx)),ctx) }

OfStep :
  | STAR { fun ctx -> -1 }
  | INTV { fun ctx -> $1.v }

/* Order of Reduction */
OfOrder :
  | FULL_ORDER        { fun ctx -> FullOrder }
  | NORMAL_ORDER      { fun ctx -> NormalOrder }
  | APPLICATIVE_ORDER { fun ctx -> ApplicativeOrder }
  | CBN_ORDER      { fun ctx -> CbnOrder }
  | CBV_ORDER { fun ctx -> CbvOrder }

/* Switch: On or Off (Boolean) */
Switch :
  | ON  { fun ctx -> true }
  | OFF { fun ctx -> false }

/* Right-hand sides of top-level bindings */
Binder :
    SLASH
      { fun ctx -> NameBind }
  | EQ Term
      { fun ctx -> TmAbbBind($2 ctx) }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx1) }
  | LAMBDA USCORE DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1, "_", $4 ctx1) }

AppTerm :
    PathTerm
      { $1 }
  | AppTerm PathTerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1,e1,e2) }

PathTerm :
  | ATerm
      { $1 }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID 
      { fun ctx ->
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }

Fields :
    /* empty */
      { fun ctx i -> [] }
  | NEFields
      { $1 }

NEFields :
    Field
      { fun ctx i -> [$1 ctx i] }
  | Field COMMA NEFields
      { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

Field :
    LCID EQ Term
      { fun ctx i -> ($1.v, $3 ctx) }
  | Term
      { fun ctx i -> (string_of_int i, $1 ctx) }


/*   */
