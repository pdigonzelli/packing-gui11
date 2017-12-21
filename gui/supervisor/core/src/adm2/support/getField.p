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
DEFINE INPUT    PARAMETER xTable     AS CHARACTER NO-UNDO.
DEFINE INPUT    PARAMETER xQuery     AS CHARACTER NO-UNDO.
DEFINE INPUT    PARAMETER xColumns   AS CHARACTER NO-UNDO.
DEFINE OUTPUT   PARAMETER xResult    AS CHARACTER NO-UNDO.

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
    DEFINE VAR hQuery   AS HANDLE       NO-UNDO.
    DEFINE VAR hBuffer  AS HANDLE       NO-UNDO.
    DEFINE VAR hField   AS HANDLE       NO-UNDO.
    DEFINE VAR i        AS INTEGER      NO-UNDO.
    DEFINE VAR xValue   AS CHARACTER    NO-UNDO.


    CREATE BUFFER hBuffer FOR TABLE xTable.

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuffer) NO-ERROR.
    hQuery:QUERY-PREPARE(xQuery) NO-ERROR.
    
    hQuery:QUERY-OPEN() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN 'ERROR DE REGISTRO'.
    
    hQuery:GET-NEXT() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN 'ERROR DE REGISTRO'.
    

    hField = hBuffer:BUFFER-FIELD( entry(1,xColumns) ) NO-ERROR.
    xValue = hField:BUFFER-VALUE NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN 'Registro inexistente'.
    
    DO i = 1 TO NUM-ENTRIES(xColumns):
            hField = hBuffer:BUFFER-FIELD( entry(i,xColumns) ) NO-ERROR.
            xValue = hField:BUFFER-VALUE NO-ERROR.
            IF xValue = ? THEN xValue = ''.
            IF xResult = ''  THEN
                xResult = xValue.
            ELSE
                xResult = xResult + ',' + xvalue.
    END.
    RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


