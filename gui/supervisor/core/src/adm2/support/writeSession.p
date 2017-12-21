&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER hTT AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEFINE VAR qhtt AS HANDLE NO-UNDO.
DEFINE VAR bhtt AS HANDLE NO-UNDO.
DEFINE VAR hf   AS HANDLE NO-UNDO.
DEFINE VAR bhSesiones AS HANDLE NO-UNDO.

bhTT = hTT:DEFAULT-BUFFER-HANDLE.
CREATE QUERY qhTT.
qhTT:SET-BUFFERS(bhTT).
qhTT:QUERY-PREPARE('for each ttSesiones no-lock').
qhTT:QUERY-OPEN().

REPEAT :
    qhTT:GET-NEXT().
    IF qhTT:QUERY-OFF-END THEN
    DO:
        LEAVE.
    END.
END.
qhTT:QUERY-CLOSE().

hf = bhtt:BUFFER-FIELD('idSesion').
bhSesiones = BUFFER sesiones:HANDLE.
FIND sesiones WHERE sesiones.idSesion = hf:BUFFER-VALUE NO-ERROR.
IF AVAILABLE sesiones THEN
DO:
   bhsesiones:BUFFER-COPY(bhTT).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


