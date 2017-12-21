&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */
    define var mensaje as character no-undo.
    DEFINE VAR VALIDACION-FRAME AS LOGICAL INITIAL FALSE.
    DEFINE VAR VALIDO AS LOGICAL NO-UNDO.
    RUN GET-ATTRIBUTE IN THIS-PROCEDURE ('tipo-validacion') NO-ERROR.
    IF RETURN-VALUE = 'FRAME' OR RETURN-VALUE = 'BROWSE' THEN
        VALIDACION-FRAME = TRUE.
    &IF DEFINED(adm-browser) = 0 &THEN
        run descriptivos.
    &ENDIF    
     IF NOT VALIDACION-FRAME THEN DO:
         run validacion-field(self:handle , output valido ).
         if not valido then
         do:
             apply "entry" to self.
             return no-apply.
         end.    
     END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


