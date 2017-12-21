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
DEFINE INPUT PARAMETER hAppserver   AS HANDLE       NO-UNDO.
DEFINE INPUT PARAMETER xProgram     AS CHARACTER    NO-UNDO.
DEFINE INPUT PARAMETER xObject      AS CHARACTER    NO-UNDO.
DEFINE INPUT PARAMETER hField       AS HANDLE       NO-UNDO.
DEFINE OUTPUT PARAMETER xField      AS CHARACTER    NO-UNDO.

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

DEFINE VAR xTable       AS CHARACTER NO-UNDO.
DEFINE VAR xQuery       AS CHARACTER NO-UNDO.
DEFINE VAR xcolumn      AS CHARACTER NO-UNDO.
DEFINE VAR xResult      AS CHARACTER NO-UNDO.
DEFINE VAR cFieldName   AS CHARACTER NO-UNDO.

xObject = baseName(xObject).
xProgram = baseName(xProgram).


xTable = 'CustomTable'.
xQuery = ' for each CustomTable where ObjectName  = ' + '"' +  xObject   + '"' + ' and FieldName = ' + '"' + hField:NAME + '"' + ' no-lock.' .
xColumn = 'SelectionBrowse,SelectionQuery,CompatibleFieldName,RelatedTextQuery'.

IF hAppserver <> ?  THEN
    RUN adm2/support/getField.p  ON hAppServer ( xTable, xQuery , xColumn , OUTPUT xResult ).
ELSE
    RUN adm2/support/getField.p  ( xTable, xQuery , xColumn , OUTPUT xResult ).


IF ENTRY(3,xResult) <> "" THEN
    cFieldName = ENTRY(3,xResult).
ELSE
    cFieldName = hField:NAME.

    
IF xResult <> ? AND xResult <> '' THEN
    RUN adm2/support/gconsultas.w ( ENTRY(1,xResult), ENTRY(2,xResult) , cFieldName, ENTRY(4,xResult) , OUTPUT xResult) NO-ERROR.


xField = xResult.

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

