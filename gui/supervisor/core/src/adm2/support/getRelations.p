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
DEFINE INPUT    PARAMETER xObjectName           AS CHARACTER NO-UNDO.
DEFINE INPUT    PARAMETER xContainerName        AS CHARACTER NO-UNDO.
DEFINE OUTPUT   PARAMETER xFieldInformation     AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-baseName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD baseName Procedure 
FUNCTION baseName RETURNS CHARACTER
  ( INPUT cFileName AS CHARACTER )  FORWARD.

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
xObjectName = baseName(xObjectName).
xContainerName = baseName(xContainerName).

FOR EACH CustomTable WHERE ObjectName    = xObjectName NO-LOCK:
    xFieldInformation   = xFieldInformation + "," + FieldName + CHR(3) + RelatedFieldName + CHR(3) + RelatedTable +
                          CHR(3) + SelectionProgram + CHR(3) + RelatedQuery + CHR(3) +  RelatedTextQuery + CHR(3) + 
                          CompatibleFieldName.
END.
xFieldInformation = LEFT-TRIM(xFieldInformation,',').
RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-baseName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION baseName Procedure 
FUNCTION baseName RETURNS CHARACTER
  ( INPUT cFileName AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR cResult AS CHARACTER NO-UNDO.
  DEFINE VAR iPosition AS INTEGER NO-UNDO.
  DEFINE VAR iPosition1 AS INTEGER NO-UNDO.

  iPosition  = R-INDEX(cFileName,'/').
  iPosition1 = R-INDEX(cFileName,'\').
  iPosition  = IF iPosition > iPosition1 THEN iPosition ELSE iPosition1.

  cResult = SUBSTRING(cFileName, iPosition + 1).
  RETURN cResult.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

