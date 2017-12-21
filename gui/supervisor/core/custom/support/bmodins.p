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
DEFINE INPUT        PARAMETER p_contextid  AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p_code      AS CHARACTER NO-UNDO.


/* ***************************  Definitions  ************************** */

define var modo as character no-undo initial "1".
define var i_contex as character no-undo.
define var cresult as character no-undo.
define var srecid as integer no-undo.
define var procid as integer no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
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
run custom/support/bmodins1.w (output modo).
    
srecid = ?.
run adeuib/_accsect.p ("GET", procid,"MAIN-CODE-BLOCK",
                       input-output srecid ,
                       input-output p_code).

if modo = "2" then /* inserta bajo la línea existente */
    p_code= p_code + chr(10) + 
            "run set-attribute-list('modo-insert=final').".
                       
run adeuib/_accsect.p ("SET", procid,"MAIN-CODE-BLOCK",
                       input-output srecid ,
                       input-output p_code).

p_code = "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


