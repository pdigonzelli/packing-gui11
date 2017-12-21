&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*--------------------------------------------------------------------------
    File        : dataviscustom.p
    Purpose     : Super procedure to extend datavis class.

    Syntax      : dataviscustom.p

    Modified    : 06/03/1999
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ADMSuper dataviscustom.p
{adm2/support/customColors.i}
DEFINE VARIABLE cFieldWithProblem           AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-applyColorToFieldWithProblem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD applyColorToFieldWithProblem Procedure 
FUNCTION applyColorToFieldWithProblem RETURNS CHARACTER
(  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clearColorField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD clearColorField Procedure 
FUNCTION clearColorField RETURNS CHARACTER
  ( INPUT hField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clearFieldWithProblem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD clearFieldWithProblem Procedure 
FUNCTION clearFieldWithProblem RETURNS CHARACTER
  ( INPUT cFieldWithProblem AS CHARACTER)  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{src/adm2/dvisprop.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-deleteRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord Procedure 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE 'Desea borrar el registro?' VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lResp AS LOGICAL.
IF lResp  THEN
   RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportToExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportToExcel Procedure 
PROCEDURE exportToExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piNumRecords AS INTEGER NO-UNDO.

DEFINE VAR hDataSource AS HANDLE NO-UNDO.
DEFINE VAR cFieldList  AS CHARACTER NO-UNDO.

{get DataSource hDataSource}.
{get displayedFields cFieldList }.

RUN transferToExcel IN hDataSource (cFieldList, TRUE , TRUE ,piNumRecords).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getField Procedure 
PROCEDURE getField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cField AS CHARACTER NO-UNDO.

DEFINE VAR cFieldhandles    AS CHARACTER NO-UNDO.
DEFINE VAR i                AS INTEGER   NO-UNDO.
DEFINE VAR iCount           AS INTEGER   NO-UNDO.
DEFINE VAR hWidget          AS HANDLE    NO-UNDO.

{get FieldHandles cFieldHandles}.

iCount = NUM-ENTRIES(cFieldhandles).
DO i = 1 TO iCount:
    hWidget = WIDGET-HANDLE(ENTRY(i,cFieldhandles)).
    IF hWidget:NAME = cField THEN
    DO:
        RETURN hWidget:SCREEN-VALUE.
    END.
END.
RETURN.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setField Procedure 
PROCEDURE setField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cField AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cValue AS CHARACTER NO-UNDO.

DEFINE VAR cFieldhandles    AS CHARACTER NO-UNDO.
DEFINE VAR i                AS INTEGER   NO-UNDO.
DEFINE VAR iCount           AS INTEGER   NO-UNDO.
DEFINE VAR hWidget          AS HANDLE    NO-UNDO.

{get FieldHandles cFieldHandles}.

iCount = NUM-ENTRIES(cFieldhandles).
DO i = 1 TO iCount:
    hWidget = WIDGET-HANDLE(ENTRY(i,cFieldhandles)).
    IF hWidget:NAME = cField THEN
    DO:
       hWidget:SCREEN-VALUE = cValue.
       hWidget:MODIFIED = TRUE.
       {set DataModified TRUE}.
    END.
END.
RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord Procedure 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR cret AS CHARACTER NO-UNDO.
DEFINE VAR cresult AS CHARACTER NO-UNDO.

RUN SUPER.
cret = RETURN-VALUE.

IF cret <> ''  THEN
    cfieldWithProblem = applyColorToFieldWithProblem( ).
ELSE
    DO:
        IF cFieldWithProblem <> '' THEN
            cFieldWithProblem = clearFieldWithProblem(cFieldWithProblem).
    END.
RETURN cret.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-applyColorToFieldWithProblem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION applyColorToFieldWithProblem Procedure 
FUNCTION applyColorToFieldWithProblem RETURNS CHARACTER
(  ):
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
  DEFINE VARIABLE hWidget         AS HANDLE     NO-UNDO.

    
    hWidget = FOCUS:HANDLE.
    IF VALID-HANDLE (hwidget) THEN
        hWidget:BGCOLOR = {&ERROR-COLOR}.
    RETURN STRING(hWidget).


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clearColorField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION clearColorField Procedure 
FUNCTION clearColorField RETURNS CHARACTER
  ( INPUT hField AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF hField:BGCOLOR = {&ERROR-COLOR} THEN
      hField:BGCOLOR = ?.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clearFieldWithProblem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION clearFieldWithProblem Procedure 
FUNCTION clearFieldWithProblem RETURNS CHARACTER
  ( INPUT cFieldWithProblem AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hWidget         AS HANDLE NO-UNDO.

    hWidget = WIDGET-HANDLE(cFieldWithProblem) NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN
    DO:
        hWidget:BGCOLOR = ?.
    END.
    RETURN ''.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

