/*------------------------------------------------------------------------
  File:        ProgressCode.p
  Author:      RJM 
  Description: Custom progress logic run from compile utility

  Parameters:  (1) ObjectType ({@GLOBALOBJECT} or {&COMPILEOBJECT}
               (2) ipProgressCodeRunMode (START or END)

               The global shared variable is a handle to the compile
               utility in case this progress code needs access to the
               internal procedures of that program, such as the log
               writing capabilities.               

               Any custom progress logic program (such as this one) 
               must incorporate the 2 input parameters below.
------------------------------------------------------------------------*/
def input param ipObjectType            as char no-undo.
def input param ipProgressCodeRunMode   as char no-undo.    

&scoped-define GLOBALOBJECT             GLOBALOBJECT
&scoped-define COMPILEOBJECT            COMPILEOBJECT

def new global shared var hCompileProcedure as handle no-undo.

if ipProgressCodeRunMode = "START" then do:
end.

if ipProgressCodeRunMode = "END" then do:
end.
