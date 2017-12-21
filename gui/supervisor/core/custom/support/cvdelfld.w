&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT        PARAMETER p_contextid  AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p_code      AS CHARACTER NO-UNDO.


define variable i_contex as character no-undo.
define var procid as integer no-undo.
DEFINE VAR CRESULT AS CHARACTER.
define var k as integer.
define var lista_campos as character.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retorna_campo Procedure 
FUNCTION retorna_campo RETURNS CHARACTER
  ( input c as character , input j as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retorna_tabla Procedure 
FUNCTION retorna_tabla RETURNS CHARACTER
  ( input c as character , input j as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN adeuib/_uibinfo.p (p_contextID, ?, "PROCEDURE", OUTPUT i_contex).
/* Is this being run in a TEMPLATE.  If so, then don't bother doing 
   anything. */
assign procid = integer(i_contex).
RUN adeuib/_uibinfo.p (procid, ?, "TEMPLATE":U, OUTPUT cResult).
IF cResult = "yes":U THEN return.

message "Desea borrar todos los campos ?" view-as alert-box button yes-no 
    update resp as logical.

if resp then
do:      
    run adeuib/_uibinfo.p (procid,?,"FRAMES",output cresult).
    RUN adeuib/_uibinfo.p (?, cresult,input "context", output i_contex).
    /*run adeuib/_uibinfo.p (?,input cresult,
                       input "FIELDS",output lista_campos).

    do k = 1 to num-entries(lista_campos) :               
        run adeuib/_uibinfo.p(input ? , input entry(k,lista_campos) ,
                              input "context", output i_contex).
        run adeuib/_uib_del.p(input integer(i_contex)).                       
    end.
    */
    run adeuib/_uibinfo.p (?,input cresult,
                       input "contains *",output lista_campos).

    message lista_campos view-as alert-box.
    do k = 1 to num-entries(lista_campos) :               
        run adeuib/_uibinfo.p(input ? , input entry(k,lista_campos) ,
                              input "context", output i_contex).
        run adeuib/_uib_del.p(input integer(i_contex)).                       
    end.
    
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retorna_campo Procedure 
FUNCTION retorna_campo RETURNS CHARACTER
  ( input c as character , input j as integer) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
define var r as character no-undo.
define var i as integer no-undo.
define var cuenta as integer initial 0 no-undo.

do i = 1 to length(c):
    if cuenta >= j then
        r = r + substring(c,i,1).
    if substring(c,i,1) = "." then
        cuenta = cuenta + 1.
         
end.

    
  RETURN r.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retorna_tabla Procedure 
FUNCTION retorna_tabla RETURNS CHARACTER
  ( input c as character , input j as integer) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
define var r as character no-undo.
define var i as integer no-undo.
define var cuenta as integer initial 0 no-undo.

do i = 1 to length(c):
    if substring(c,i,1) = "." then
        cuenta = cuenta + 1.
    if cuenta < j then
        r = r + substring(c,i,1).     
end.

    
  RETURN r.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


