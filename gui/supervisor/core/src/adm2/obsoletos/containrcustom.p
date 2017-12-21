&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : containrcustom.p
    Purpose     : Super procedure to extend containr class.

    Syntax      : containrcustom.p

    Modified    : 06/03/1999
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ADMSuper containrcustom.p

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getChilds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getChilds Procedure 
FUNCTION getChilds RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSdo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSdo Procedure 
FUNCTION getSdo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{src/adm2/cntnprop.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-afterFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterFilter Procedure 
PROCEDURE afterFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER pcFieldNames  AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER pcFieldValues AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER pcOperators   AS CHARACTER NO-UNDO.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-postOpenQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postOpenQuery Procedure 
PROCEDURE postOpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getChilds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getChilds Procedure 
FUNCTION getChilds RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR xTarget        AS CHARACTER      NO-UNDO.
  DEFINE VAR h              AS HANDLE         NO-UNDO.
  DEFINE VAR i              AS INTEGER        NO-UNDO.
  DEFINE VAR xType          AS CHARACTER      NO-UNDO.
  DEFINE VAR xHandles       AS CHARACTER      NO-UNDO INITIAL ''.
  DEFINE VAR lChild         AS LOGICAL        NO-UNDO.

  {get ContainerTarget xTarget}.
  DO i = 1 TO NUM-ENTRIES(xTarget):
        h = WIDGET-HANDLE(ENTRY(i,xTarget)).
        xType = DYNAMIC-FUNCTION('getObjectType' IN h) NO-ERROR.
        IF xType = 'SmartDataObject' THEN
        DO:
            lChild = DYNAMIC-FUNCTION('isChild' IN h ).
            IF lChild THEN
                xHandles = xHandles + (IF xHandles <> '' THEN ',' ELSE '') + STRING(h).
        END.
  END.

  RETURN xHandles.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSdo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSdo Procedure 
FUNCTION getSdo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR xTarget  AS CHARACTER NO-UNDO.
  DEFINE VAR h        AS HANDLE NO-UNDO.
  DEFINE VAR i        AS INTEGER NO-UNDO.
  DEFINE VAR xType    AS CHARACTER NO-UNDO.
  DEFINE VAR xHandles AS CHARACTER NO-UNDO.

  {get ContainerTarget xTarget}.
  DO i = 1 TO NUM-ENTRIES(xTarget):
        h = WIDGET-HANDLE(ENTRY(i,xTarget)).
        xType = DYNAMIC-FUNCTION('getObjectType' IN h) NO-ERROR.
        IF xType = 'SmartDataObject' THEN
            xHandles = xHandles + (IF xHandles <> '' THEN ',' ELSE '') + STRING(h).
  END.
  RETURN xHandles.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

