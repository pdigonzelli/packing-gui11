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

  DEFINE VAR xDataSource AS CHARACTER NO-UNDO.
  DEFINE VAR hContainer     AS HANDLE       NO-UNDO.
  DEFINE VAR xQuery         AS CHARACTER    NO-UNDO.
  DEFINE VAR xSort          AS CHARACTER    NO-UNDO.
  DEFINE VAR hDataSource    AS HANDLE       NO-UNDO.
  DEFINE VAR cquery         AS CHARACTER    NO-UNDO.
  DEFINE VAR xWhere         AS CHARACTER    NO-UNDO.
  DEFINE VAR xASDivision    AS CHARACTER    NO-UNDO.
  DEFINE VAR hAppServer     AS HANDLE       NO-UNDO.

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


  {get ContainerSource hContainer}.
  {get DataSource hDataSource}.
  
  cQuery = DYNAMIC-FUNCTION('queryName' IN hcontainer).
  
  hAppServer = SESSION:LAST-SERVER.
  IF hAppServer = ? THEN
  DO:
      hAppServer = THIS-PROCEDURE.
  END.
  IF hAppserver <> THIS-PROCEDURE THEN
  DO:
        RUN adm2/support/getQuery.p ON hAppServer (TARGET-PROCEDURE:FILE-NAME,cQuery,OUTPUT xSort ,OUTPUT xWhere).
  END.
  ELSE
  DO:
        RUN adm2/support/getQuery.p  (TARGET-PROCEDURE:FILE-NAME,cQuery,OUTPUT xSort, OUTPUT xWhere).
  END.
  
  IF xSort <> '' THEN
  DO:
      {set QuerySort xSort}.
  END.
  IF xWhere <> '' THEN
  DO:
      {set QueryWhere xWhere}.
  END.
  IF xSort <> '' AND xWhere <> '' THEN
  DO:
      DYNAMIC-FUNCTION ('openQuery' IN THIS-PROCEDURE).
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


