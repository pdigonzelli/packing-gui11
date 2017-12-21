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

DEFINE OUTPUT PARAMETER  hTT AS HANDLE  NO-UNDO.
DEFINE SHARED VAR giSession  AS INTEGER NO-UNDO.

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

DEFINE VAR hSession AS HANDLE NO-UNDO.
DEFINE VAR bhTT     AS HANDLE NO-UNDO.

 
hSession = BUFFER Sesiones:HANDLE.

CREATE TEMP-TABLE hTT.                                 
hTT:ADD-FIELDS-FROM(hSession).
hTT:TEMP-TABLE-PREPARE('ttSesiones').

bhTT     = hTT:DEFAULT-BUFFER-HANDLE.

gisession = INTEGER(SESSION:HANDLE).
MESSAGE giSession VIEW-AS ALERT-BOX.

FIND sesiones WHERE sesiones.idSesion = giSession NO-LOCK NO-ERROR.
IF AVAILABLE sesiones THEN
DO:
    bhtt:BUFFER-CREATE().
    bhTT:BUFFER-COPY(hSession).
END.
ELSE
DO:
   CREATE sesiones.
   ASSIGN sesiones.idSesion = gisession
          sesiones.fechaInicio = TODAY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


