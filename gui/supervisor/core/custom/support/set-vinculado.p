&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS xftrprocedure 
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
define var procid as integer no-undo.
define var cresult as character no-undo.
define var srecid as integer no-undo.
define var cnt as integer no-undo.
define var x_code as character no-undo.
define var flag as logical no-undo.
define var i as integer no-undo.

define var V-TABLA as character.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE xftrprocedure



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: xftrprocedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW xftrprocedure ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK xftrprocedure 


/* ***************************  Main Block  *************************** */
RUN adeuib/_uibinfo.p (p_contextID, ?, "PROCEDURE", OUTPUT cresult).
/* Is this being run in a TEMPLATE.  If so, then don't bother doing 
   anything. */
assign procid = integer(cresult).
RUN adeuib/_uibinfo.p (procid, ?, "TEMPLATE":U, OUTPUT cResult).
IF cResult = "yes":U THEN return.
srecid = ?.
run adeuib/_accsect.p ("GET", procid,"MAIN-CODE-BLOCK",
                       input-output srecid ,
                       input-output p_code).
cnt = num-entries(p_code,chr(10)).    
x_code = "".
flag = true.
do i = 1 to cnt :
    if entry(i,p_code,chr(10)) matches "*EMPIEZA-VINCULADO*" then
        flag = false.
    if entry(i,p_code,chr(10)) matches "*TERMINA-VINCULADO*" then
    do:
        flag = true.
        next.
    end.
    if flag then
        x_code = x_code + entry(i,p_code,chr(10)) + chr(10).   
    
end.
run custom/support/w-vinculado.w ( output V-TABLA).
p_code = "/**********EMPIEZA-VINCULADO*********/" + chr(10) +
         "  DEFINE BUFFER " + V-TABLA + " FOR " + V-TABLA + ". " + chr(10) +
         "/**********TERMINA-VINCULADO*********/".
p_code = x_code + chr(10) + p_code .
srecid = ?.
run adeuib/_accsect.p ("SET", procid,"MAIN-CODE-BLOCK",
                       input-output srecid ,
                       input-output p_code).

p_code = "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


