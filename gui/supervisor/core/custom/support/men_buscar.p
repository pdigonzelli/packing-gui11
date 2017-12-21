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

define input parameter i as integer.
define input parameter lista_campos as character.
define input parameter cresult as character.
define input parameter encuentra as character.
define input-output parameter f_code as character.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retorna-tabla Procedure 
FUNCTION retorna-tabla RETURNS CHARACTER
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
   message 'entra buscar' view-as alert-box.

    if entry(1,cresult) = retorna-tabla(entry(i,lista_campos),2) then
        do:  
       find DICTDB._file where DICTDB._file._file-name = retorna-tabla(retorna-campo(entry(i,lista_campos),1),1) no-error.
       find DICTDB._field where DICTDB._field._field-name = retorna-campo(entry(i,lista_campos),2) and 
        	              DICTDB._field._file-recid = recid(DICTDB._file) no-error.
         if not available DICTDB._field then
         message retorna-campo(entry(i,lista_campos),2) view-as alert-box.

         f_code = f_code + 
                 "  WHEN '" + retorna-campo(entry(i,lista_campos),1) + "' then  " + chr(10).
         f_code = f_code + 
                 "       " + encuentra + " " + retorna-campo(entry(i,lista_campos),2).
         CASE DICTDB._field._data-type :
            WHEN "character" THEN
                f_code = f_code + " begins valor ".
            WHEN "integer" THEN
                f_code = f_code + " = integer(valor) ".
            WHEN "date" THEN
                f_code = f_code + " = date(valor) ".
            WHEN "decimal" THEN
                f_code = f_code + " = decimal(valor) ".     
         
         end.        
         f_code = f_code + " no-lock no-error ." + chr(10).
        end.

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
define var r as character no-undo initial "".
define var i as integer no-undo.
define var cuenta as integer initial 0 no-undo.

do i = 1 to length(c):
    if cuenta >= j then
        r = r + substring(c,i,1).
    if substring(c,i,1) = "." then
        cuenta = cuenta + 1.
         
end.
return r.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retorna-tabla Procedure 
FUNCTION retorna-tabla RETURNS CHARACTER
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


