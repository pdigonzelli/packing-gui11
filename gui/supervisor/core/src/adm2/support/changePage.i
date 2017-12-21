&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */
  DEFINE VAR lDataModified    AS LOGICAL      NO-UNDO.
  DEFINE VAR cRowObjectState  AS CHARACTER    NO-UNDO.
  DEFINE VAR lResp            AS LOGICAL      NO-UNDO.
  DEFINE VAR lModified        AS LOGICAL      NO-UNDO INITIAL FALSE.
  DEFINE VAR xSDO             AS CHARACTER    NO-UNDO.
  DEFINE VAR hSDO             AS HANDLE       NO-UNDO.
  DEFINE VAR xModifiedSDO     AS CHARACTER    NO-UNDO INITIAL ''.
  DEFINE VAR i                AS INTEGER      NO-UNDO.  
  DEFINE VAR iActualPage      AS INTEGER      NO-UNDO.
  DEFINE VAR iPage            AS INTEGER      NO-UNDO.
  DEFINE VAR plCancel         AS LOGICAL NO-UNDO.


  {get CurrentPage iActualPage}.

  
  PUBLISH 'confirmExit':U  (INPUT-OUTPUT plCancel).
  
  /*  
  xSDO = DYNAMIC-FUNCTION('getSDO').
  DO i = 1 TO NUM-ENTRIES(xSDO):
      hSDO = WIDGET-HANDLE(ENTRY(i,xSDO)).
      cRowObjectState = DYNAMIC-FUNCTION('getRowObjectState':U IN hSDO) 
                        NO-ERROR.
      iPage = DYNAMIC-FUNCTION ('getObjectPage' IN hSDO).
      IF cRowObjectState = "RowUpdated":U AND iPage = iOldPAge THEN
      DO:
        lMOdified = TRUE.
        IF xModifiedSDO = '' THEN
            xModifiedSDO = ENTRY(i,xSDO).
        ELSE
            xModifiedSDO = xModifiedSDO + ',' + ENTRY(i,xSDO).
      END.
  END.
  
  IF lModified THEN
  DO:
        MESSAGE 'Los Datos han sido modificados.Acepta los cambios ?' 
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lResp.
        IF lResp THEN
        DO:
            DO i = 1 TO NUM-ENTRIES(xModifiedSDO):
                hSDO = WIDGET-HANDLE(ENTRY(i,xModifiedSDO)).
                RUN CommitTransaction IN hSDO.        
            END.
        END.
  END.
*/
  iOldPage = iActualPage.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


