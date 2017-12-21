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
DEFINE INPUT        PARAMETER p_contextID  AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER lista_campos as character no-undo.
DEFINE INPUT-OUTPUT PARAMETER p_code       AS CHARACTER NO-UNDO.

/**************************** Local Definitions **************************/
DEFINE VARIABLE cResult    AS CHAR    NO-UNDO.
DEFINE VARIABLE proc-ID    AS INTEGER NO-UNDO.
define var lista_f as character no-undo.
define var srecid as integer no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retorna-campo Procedure 
FUNCTION retorna-campo RETURNS CHARACTER
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
/* What is the ID of this procedure? */
RUN adeuib/_uibinfo.p (p_contextID, ?, "PROCEDURE", OUTPUT cResult).
proc-ID = INTEGER (cResult).

/* Is this being run in a TEMPLATE.  If so, then don't bother doing 
   anything. */
RUN adeuib/_uibinfo.p (proc-ID, ?, "TEMPLATE":U, OUTPUT cResult).
IF cResult NE "yes":U THEN DO:
  RUN adeuib/_uibinfo.p 
      (proc-id, ?, "EXTERNAL-TABLES",
       OUTPUT cResult).
  cresult = retorna-campo( input cresult , 1).
  find _file where _file._file-name = cresult no-lock no-error.
  find _index where recid(_index) = _file._prime-index no-lock no-error.
  for each _index-field of _index , first _field of _index-field no-lock:
    IF LOOKUP(lista_campos,_field._field-name) <> 0 then
        lista_f = lista_f + " " + cresult + "." + _field._field-name.   
  end.  
  if lista_f <> "" then
  do:
     srecid = ?.
    RUN adeuib/_accsect.p 
      ('GET':U, proc-ID,
       INPUT 'DEFINITIONS',
       input-output srecid,
       INPUT-OUTPUT cresult).
 
     cresult = cresult + chr(10) +
    "&SCOPED-DEFINE ADM-CREATE-FIELDS " + lista_f. 
    srecid=?.
    RUN adeuib/_accsect.p 
      ('SET':U, proc-ID, 
        INPUT 'DEFINITIONS',
        input-output srecid,
        INPUT-OUTPUT cresult).
   end. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retorna-campo Procedure 
FUNCTION retorna-campo RETURNS CHARACTER
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


