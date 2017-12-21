&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12
&ANALYZE-RESUME
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : viewercustom.p
    Purpose     : Super procedure to extend viewer class.

    Syntax      : viewercustom.p

    Modified    : 06/03/1999
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ADMSuper viewercustom.p

    DEFINE VAR hDataSource          AS HANDLE       NO-UNDO.
    DEFINE VAR cASDivision          AS CHARACTER    NO-UNDO.
    DEFINE VAR hAppServer           AS HANDLE       NO-UNDO.

DEFINE VAR xRelatedTexts            AS CHARACTER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getColumn Procedure 
FUNCTION getColumn RETURNS CHARACTER
  ( INPUT cFieldInformation AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompatibleField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCompatibleField Procedure 
FUNCTION getCompatibleField RETURNS CHARACTER
  ( INPUT cFieldInformation AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTable Procedure 
FUNCTION getTable RETURNS CHARACTER
  ( INPUT cFieldInformation AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWhere Procedure 
FUNCTION getWhere RETURNS CHARACTER
  ( INPUT  cFieldInformation AS CHARACTER)  FORWARD.

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
   Other Settings: CODE-ONLY COMPILE APPSERVER
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

{src/adm2/viewprop.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-createText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createText Procedure 
PROCEDURE createText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hField     AS HANDLE    NO-UNDO.
DEFINE OUTPUT PARAMETER hText     AS HANDLE    NO-UNDO. 
        
DEFINE VAR hFrame   AS HANDLE    NO-UNDO.
DEFINE VAR iwidth   AS INTEGER   NO-UNDO.
DEFINE VAR hParent  AS HANDLE    NO-UNDO.
    
    hFrame  = hField:FRAME.
    hParent = hField:PARENT.

    iWidth = INTEGER( hField:COLUMN + hField:WIDTH + 2 ).
    IF hFrame:WIDTH - iWidth < 1 THEN
            hFrame:WIDTH = hParent:WIDTH - 2.

    IF hFrame:WIDTH - iWidth > 1  THEN
        CREATE TEXT hText
           ASSIGN 
           FRAME  = hField:FRAME
           NAME   = 'F' + hField:NAME
           HEIGHT = hField:HEIGHT
           WIDTH  = hFrame:WIDTH - iWidth
           ROW    = hField:ROW
           COLUMN = hField:COLUMN + hField:WIDTH + 1
           FORMAT = 'X(' + STRING(INTEGER(hFrame:WIDTH) - iWidth - 1) + ')'
           VISIBLE = TRUE.
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createText-1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createText-1 Procedure 
PROCEDURE createText-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hField     AS HANDLE    NO-UNDO.
DEFINE OUTPUT PARAMETER hText     AS HANDLE    NO-UNDO. 
        
DEFINE VAR hFrame   AS HANDLE    NO-UNDO.
DEFINE VAR iwidth   AS INTEGER   NO-UNDO.
DEFINE VAR hParent  AS HANDLE    NO-UNDO.
    

    hFrame  = hField:FRAME.
    hParent = hField:PARENT.

    iWidth = INTEGER( hField:COLUMN + hField:WIDTH + 2 ).
    IF hFrame:WIDTH - iWidth < 1 THEN
            hFrame:WIDTH = hParent:WIDTH - 2.

    IF hFrame:WIDTH - iWidth > 1  THEN
        CREATE TEXT hText
           ASSIGN 
           FRAME  = hField:FRAME
           NAME   = 'F' + hField:NAME
           HEIGHT = hField:HEIGHT
           WIDTH  = hFrame:WIDTH - iWidth
           ROW    = hField:ROW
           COLUMN = hField:COLUMN + hField:WIDTH + 1
           FORMAT = 'X(' + STRING(INTEGER(hFrame:WIDTH) - iWidth - 1) + ')'
           VISIBLE = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-displayDescriptor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayDescriptor Procedure 
PROCEDURE displayDescriptor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER hfield AS HANDLE NO-UNDO.

  DEFINE VAR xFieldInformation      AS CHARACTER    NO-UNDO.
  DEFINE VAR xFieldHandles          AS CHARACTER    NO-UNDO.
  DEFINE VAR i                      AS INTEGER      NO-UNDO.
  DEFINE VAR iCount                 AS INTEGER      NO-UNDO.
  DEFINE VAR htext                  AS HANDLE       NO-UNDO.
  DEFINE VAR cFieldList             AS CHARACTER    NO-UNDO.
  

  
  IF NOT VALID-HANDLE(hField) THEN
  DO:
      RETURN.
  END.
 {get FieldInformation xFieldInformation TARGET-PROCEDURE}.
 {get RelatedTexts xRelatedTexts TARGET-PROCEDURE}. 
 
  RUN getFieldList(xFieldInformation , OUTPUT cFieldList).
  iCount = NUM-ENTRIES(cFieldList).
  DO i = 1 TO iCount:
        IF hField:NAME = ENTRY(i,cFieldList) THEN
        DO:
            hText = WIDGET-HANDLE (ENTRY(i,xRelatedTexts)).
            IF VALID-HANDLE(hText) THEN
            DO:
              RUN selectDescriptor (INPUT hField , INPUT hText , ENTRY(i,xFieldInformation)) NO-ERROR.
            END.
        END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-displayDescriptor-2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayDescriptor-2 Procedure 
PROCEDURE displayDescriptor-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER hfield AS HANDLE NO-UNDO.

  DEFINE VAR xFieldInformation      AS CHARACTER    NO-UNDO.
  DEFINE VAR xFieldHandles          AS CHARACTER    NO-UNDO.
  DEFINE VAR i                      AS INTEGER      NO-UNDO.
  DEFINE VAR iCount                 AS INTEGER      NO-UNDO.
  DEFINE VAR htext                  AS HANDLE       NO-UNDO.
  DEFINE VAR cFieldList             AS CHARACTER    NO-UNDO.
  
  
  IF NOT VALID-HANDLE(hField) THEN
  DO:
      RETURN.
  END.
 {get FieldInformation xFieldInformation}.

  RUN getFieldList(xFieldInformation , OUTPUT cFieldList).
  iCount = NUM-ENTRIES(cFieldList).
  DO i = 1 TO iCount:
        IF hField:NAME = ENTRY(i,cFieldList) THEN
        DO:
            hText = WIDGET-HANDLE (ENTRY(i,xRelatedTexts)).
            IF VALID-HANDLE(hText) THEN
            DO:
              RUN selectDescriptor (INPUT hField , INPUT hText , ENTRY(i,xFieldInformation)) NO-ERROR.
            END.
        END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-displayDescriptors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayDescriptors Procedure 
PROCEDURE displayDescriptors :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER  hProcedure    AS HANDLE    NO-UNDO.


  DEFINE VAR xFieldInformation          AS CHARACTER    NO-UNDO.
  DEFINE VAR hField                     AS HANDLE       NO-UNDO.
  DEFINE VAR i                          AS INTEGER      NO-UNDO.
  DEFINE VAR iCount                     AS INTEGER      NO-UNDO.
  DEFINE VAR iCount1                    AS INTEGER      NO-UNDO.
  DEFINE VAR j                          AS INTEGER      NO-UNDO.
  DEFINE VAR hText                      AS HANDLE       NO-UNDO.
  DEFINE VAR FieldName                  AS CHARACTER    NO-UNDO.
  DEFINE VAR xResult                    AS CHARACTER    NO-UNDO.
  DEFINE VAR xFieldHandles              AS CHARACTER    NO-UNDO.
  DEFINE VAR cFieldList                 AS CHARACTER    NO-UNDO.
  
  IF NOT VALID-HANDLE(hProcedure) THEN
  DO:
      RETURN.
  END.
  
  {get FieldHandles    xFieldhandles hProcedure}.
  {get RelatedTexts    xRelatedTexts hProcedure}.
  IF xFieldHandles = ''  THEN
      RETURN.                                          
  
 {get FieldInformation xFieldInformation  hProcedure}.
 IF xFieldInformation = '' THEN
 DO:
     RUN setRelatedFields (hProcedure).
     {get FieldInformation xFieldInformation  hProcedure}.
 END.
  RUN getFieldList ( xFieldInformation , OUTPUT cFieldList).
  iCount  = NUM-ENTRIES(xFieldHandles).
  iCount1 = NUM-ENTRIES(cFieldList).
  DO j = 1 TO iCount1:
    DO i = 1 TO iCount:
        hField = WIDGET-HANDLE(ENTRY(i,xFieldHandles)).
        IF valid-handle(hField) AND hField:NAME = ENTRY(j,cFieldList)THEN
        DO:
            hText = WIDGET-HANDLE (ENTRY(j,xRelatedTexts)).
            IF VALID-HANDLE(hText) THEN
                RUN selectDescriptor (INPUT hField , INPUT hText , ENTRY(j,xFieldInformation) ) NO-ERROR.
        END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-displayFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields Procedure 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.
 
DEFINE VAR cFields                  AS CHARACTER NO-UNDO.
DEFINE VAR i                        AS INTEGER   NO-UNDO.
DEFINE VAR iFields                  AS INTEGER   NO-UNDO.


RUN SUPER(pcColValues).

{get DataSource hDataSource}.
RUN displayDescriptors ( TARGET-PROCEDURE ) NO-ERROR.
RUN fieldsWithProblems IN hDataSource (OUTPUT cFields) NO-ERROR.
IF cFields <> '' THEN
    RUN displayFieldsWithProblem  (cFields , TARGET-PROCEDURE ) NO-ERROR.

RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-displayFieldsWithProblem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFieldsWithProblem Procedure 
PROCEDURE displayFieldsWithProblem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cFields AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER hProcedure AS HANDLE NO-UNDO.

DEFINE VAR j              AS INTEGER   NO-UNDO.
DEFINE VAR jCount         AS INTEGER   NO-UNDO.
DEFINE VAR i              AS INTEGER   NO-UNDO.
DEFINE VAR iCount         AS INTEGER   NO-UNDO.
DEFINE VAR cFieldhandles  AS CHARACTER NO-UNDO.
DEFINE VAR hWidget        AS HANDLE    NO-UNDO.

IF NOT VALID-HANDLE(hProcedure) THEN
DO:
    RETURN.
END.

{get fieldhandles cFieldHandles hProcedure}.

iCount = NUM-ENTRIES(cFieldHandles).

jCount = NUM-ENTRIES(cFields).
DO j = 1 TO jCount:
    DO i = 1 TO iCount:
       hWidget = WIDGET-HANDLE(ENTRY(i,cFieldHandles)).
       IF hWidget:NAME = ENTRY(1,ENTRY(j,cFields),CHR(3)) THEN
       DO:
           hWidget:BGCOLOR = integer(ENTRY(2,ENTRY(j,cFields),CHR(3) ) ).
           LEAVE.
       END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-enableFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields Procedure 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cEnableFields AS CHARACTER NO-UNDO.  /* List of handles. */
  DEFINE VARIABLE iField        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE hField        AS HANDLE    NO-UNDO.
  DEFINE VARIABLE lEnabled      AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cState        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cNewRecord    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cUpdateTarget AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hGASource     AS HANDLE    NO-UNDO.

  RUN SUPER.

  
  {get FieldsEnabled lEnabled}.
  {get GroupAssignSource hGASource}.
   
  /* Check the record state in the GA source to avoid timing problems
     when this method is called from queryPosition.
     The NewRecord value is not even propagated to GroupAssign-Target(s). */
  IF VALID-HANDLE(hGASource) THEN
  DO:
    {get RecordState cState hGASource}.
    {get NewRecord cNewRecord hGASource}.
    {get UpdateTarget cUpdateTarget hGaSource}.
  END.
  ELSE
  DO:
    {get RecordState cState}.
    {get NewRecord cNewRecord}.
    {get UpdateTarget cUpdateTarget}.
  END.
  
  IF  (cUpdateTarget NE "":U) 
  AND cState = "RecordAvailable":U AND cNewRecord NE "Add":U THEN     
  DO:  
    {get EnabledHandles cEnableFields}.
    DO iField = 1 TO NUM-ENTRIES(cEnableFields):
       hField = WIDGET-HANDLE(ENTRY(iField, cEnableFields)).
        /* "Frame Field" May be a SmartObject procedure handle */
       IF hField:TYPE = "procedure" THEN NEXT.

       IF NOT hField:HIDDEN AND hField:PRIVATE-DATA = 'adm-create' THEN /* Skip fields hidden for multi-layout */
       DO:
         hField:SENSITIVE = NO.
         IF hField:TYPE = "EDITOR":U THEN /* Ed's must be sensitive, not R-O*/
           hField:READ-ONLY = YES.     
       END.
    END.
  END.
  
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFieldList Procedure 
PROCEDURE getFieldList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cFieldInformation AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cFieldList       AS CHARACTER NO-UNDO.

DEFINE VAR i AS INTEGER         NO-UNDO.
DEFINE VAR iCount AS INTEGER    NO-UNDO.

iCount = NUM-ENTRIES(cFieldInformation).

DO i = 1 TO iCount:
    cFieldList = cFieldList + ',' + ENTRY(1,ENTRY(i,cFieldInformation),CHR(3)).
END.
cFieldList = LEFT-TRIM (cFieldList,',').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initializeObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Procedure 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
IF xRelatedTexts = '' THEN  
    RUN setRelatedFields (TARGET-PROCEDURE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-selectDescriptor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectDescriptor Procedure 
PROCEDURE selectDescriptor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER hField               AS HANDLE           NO-UNDO.
    DEFINE INPUT PARAMETER hText                AS HANDLE           NO-UNDO.
    DEFINE INPUT PARAMETER xFieldInformation    AS CHARACTER        NO-UNDO.

    DEFINE VAR FieldName            AS CHARACTER    NO-UNDO.
    DEFINE VAR xResult              AS CHARACTER    NO-UNDO.
    DEFINE VAR i                    AS INTEGER      NO-UNDO.
    DEFINE VAR xConversion          AS CHARACTER    NO-UNDO.
    DEFINE VAR xCompatibleFieldName AS CHARACTER    NO-UNDO.
    DEFINE VAR xWhere               AS CHARACTER    NO-UNDO.
    DEFINE VAR xQuery               AS CHARACTER    NO-UNDO.
    DEFINE VAR xTable               AS CHARACTER    NO-UNDO.
    DEFINE VAR xColumn              AS CHARACTER    NO-UNDO.

   IF NOT VALID-HANDLE(hField) THEN RETURN.

   

   IF xFieldInformation <> '' THEN
    DO:
        xCompatibleFieldName    = getCompatibleField(xFieldInformation).
        xWhere                  = getWhere(xFieldInformation).
        xTable                  = getTable(xFieldInformation).
        xColumn                 = getColumn(xFieldInformation).
    END.
    
    IF xCompatibleFieldName <> '' THEN
    DO:
        FieldName = xCompatibleFieldName.
    END.
    ELSE
        FieldName = hField:NAME.

    IF VALID-HANDLE(hText) THEN
    DO:
        CASE hField:DATA-TYPE:
            WHEN 'character' THEN
                xConversion = "'" + hField:SCREEN-VALUE + "'".
            WHEN 'date'    THEN
                xConversion = "date('" + hField:SCREEN-VALUE + "')".
            WHEN 'decimal' THEN
                xConversion = "decimal('" + hField:SCREEN-VALUE + "')".
            WHEN 'rowid'   THEN
                xConversion = "TO-ROWID('" + hField:SCREEN-VALUE + "')".
            WHEN 'integer' THEN
                xConversion = "integer('" + hField:SCREEN-VALUE + "')".
        END CASE.
        IF xWhere <> '' THEN
        DO:
            xQuery = 'for each ' + xTable + ' where ' + xWhere + ' and ' + FieldName + ' = ' + xConversion  + ' no-lock.'.
        END.
        ELSE
        DO:
            xQuery = 'for each ' + xTable + ' where ' + FieldName + ' = ' + xConversion  + ' no-lock.'.
        END.
        IF hAppserver <> THIS-PROCEDURE THEN
            RUN adm2/support/getField.p  ON hAppServer ( xTable, xQuery , xColumn , OUTPUT xResult ).
        ELSE
            RUN adm2/support/getField.p  ( xTable, xQuery , xColumn , OUTPUT xResult ).
        htext:WIDTH = LENGTH(xResult) + 5.
        hText:SCREEN-VALUE = xResult.
    END.

    END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-selectField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectField Procedure 
PROCEDURE selectField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hField   AS HANDLE       NO-UNDO.

DEFINE VAR FieldName            AS CHARACTER    NO-UNDO.
DEFINE VAR xResult              AS CHARACTER    NO-UNDO.
DEFINE VAR xFieldInformation    AS CHARACTER    NO-UNDO.
DEFINE VAR i                    AS INTEGER      NO-UNDO.
DEFINE VAR hText                AS HANDLE       NO-UNDO.
DEFINE VAR xQuery               AS CHARACTER    NO-UNDO.
DEFINE VAR xConversion          AS CHARACTER    NO-UNDO.
DEFINE VAR hContainer           AS HANDLE       NO-UNDO.
DEFINE VAR cResult              AS CHARACTER    NO-UNDO.
DEFINE VAR xFields              AS CHARACTER    NO-UNDO.

IF NOT VALID-HANDLE(hField) THEN
DO:
    RETURN.
END.

FieldName = hField:NAME.

{get FieldInformation   xFieldInformation}.
{get RelatedTexts       xRelatedTexts}.

RUN getFieldList (xFieldInformation , OUTPUT xFields).
i = LOOKUP(FieldName,xFields ).
IF i > 0 THEN
DO:
    hAppServer = SESSION:LAST-SERVER.
    IF NOT VALID-HANDLE(hAppServer) THEN
        hAppServer = THIS-PROCEDURE.
    RUN adm2/support/seleccion.p (  IF hAppserver = THIS-PROCEDURE THEN ? ELSE hAppserver , PROGRAM-NAME(3) , 
                                    SOURCE-PROCEDURE:FILE-NAME  , hField , OUTPUT xResult) NO-ERROR.
    IF hField:SENSITIVE AND xResult <> '' AND xResult <> ? THEN
        RUN setField IN TARGET-PROCEDURE (hField:NAME , xResult).
    hText = WIDGET-HANDLE (ENTRY(i,xRelatedTexts)).
    IF VALID-HANDLE(hText) THEN
        RUN selectDescriptor (INPUT hField , INPUT hText , entry(i,xFieldInformation)) NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRelatedFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRelatedFields Procedure 
PROCEDURE setRelatedFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER hProcedure AS HANDLE NO-UNDO.
  
  DEFINE VAR xFieldInformation      AS CHARACTER    NO-UNDO.
  DEFINE VAR hField                 AS HANDLE       NO-UNDO.
  DEFINE VAR xFieldHandles          AS CHARACTER    NO-UNDO.
  DEFINE VAR i                      AS INTEGER      NO-UNDO.
  DEFINE VAR iCount                 AS INTEGER      NO-UNDO.
  DEFINE VAR iCount1                AS INTEGER      NO-UNDO.
  DEFINE VAR j                      AS INTEGER      NO-UNDO.
  DEFINE VAR hText                  AS HANDLE       NO-UNDO.
  DEFINE VAR xQuery                 AS CHARACTER    NO-UNDO.
  DEFINE VAR hContainer             AS HANDLE       NO-UNDO.
  DEFINE VAR xFieldInformation1     AS CHARACTER    NO-UNDO.
  DEFINE VAR xb                     AS CHARACTER    NO-UNDO.

  IF NOT VALID-HANDLE(hProcedure) THEN
  DO:
      RETURN.
  END.


  {get FieldInformation  xFieldInformation  hProcedure}.
  {get FieldHandles      xFieldHandles      hProcedure}.
  {get DataSource        hDataSource        hProcedure}.
  {get RelatedTexts      XRelatedTexts      hProcedure}.
  

  hAppServer = SESSION:LAST-SERVER.
  IF NOT VALID-HANDLE(hAppServer) THEN
    hAppServer = THIS-PROCEDURE.
  
 IF xFieldInformation = '' THEN
 DO:
      {get ContainerSource hContainer hProcedure}.
      IF hAppserver <> THIS-PROCEDURE THEN
        RUN adm2/support/getRelations.p ON hAppServer (hProcedure:FILE-NAME,hContainer:FILE-NAME, OUTPUT xFieldInformation) NO-ERROR.
      ELSE
        RUN adm2/support/getRelations.p (hProcedure:FILE-NAME,hContainer:FILE-NAME, OUTPUT  xFieldInformation1) NO-ERROR.
      IF xFieldInformation = ''  THEN
        xFieldInformation = xFieldInformation1.
      ELSE
        xFieldInformation = xFieldInformation + ',' + xFieldInformation1.
      {set FieldInformation            xFieldInformation  hProcedure}.
 END.
  

  iCount  = NUM-ENTRIES(xFieldHandles).
  iCount1 = NUM-ENTRIES(xFieldInformation).

  DO j = 1 TO iCount1:
      IF xRelatedTexts <> '' THEN
          xRelatedTexts = xRelatedTexts + ',' + ''.
      ELSE
          xRelatedTexts = ''.
    DO i = 1 TO iCount:
        hField = WIDGET-HANDLE(ENTRY(i,xFieldHandles)).
        IF hField:TYPE = "procedure" THEN NEXT.
        IF hField:NAME = ENTRY (1,ENTRY(j,xFieldInformation),CHR(3)) THEN
        DO:
            IF /* hField:SENSITIVE  THEN
               IF */ SESSION:WINDOW-SYSTEM <> 'TTY' THEN
                hField:LOAD-MOUSE-POINTER('glove').
               ELSE
                hField:PFCOLOR = 14.
            xb = ENTRY(j,xRelatedTexts) NO-ERROR.
            IF valid-handle(WIDGET-HANDLE(xb))   THEN NEXT.
            RUN createText(hField ,OUTPUT htext).
            IF VALID-HANDLE (hText) THEN
            DO:
                ENTRY(j,xRelatedTexts) = STRING(hText).
            END.
        END.
    END.
  END.
  {set RelatedTexts xRelatedTexts hProcedure}.
  RETURN.
  
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRelatedFields-1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRelatedFields-1 Procedure 
PROCEDURE setRelatedFields-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER hProcedure AS HANDLE NO-UNDO.
  
  DEFINE VAR xFieldInformation      AS CHARACTER    NO-UNDO.
  DEFINE VAR hField                 AS HANDLE       NO-UNDO.
  DEFINE VAR xFieldHandles          AS CHARACTER    NO-UNDO.
  DEFINE VAR i                      AS INTEGER      NO-UNDO.
  DEFINE VAR iCount                 AS INTEGER      NO-UNDO.
  DEFINE VAR iCount1                AS INTEGER      NO-UNDO.
  DEFINE VAR j                      AS INTEGER      NO-UNDO.
  DEFINE VAR hText                  AS HANDLE       NO-UNDO.
  DEFINE VAR xQuery                 AS CHARACTER    NO-UNDO.
  DEFINE VAR hContainer             AS HANDLE       NO-UNDO.


  IF NOT VALID-HANDLE(hProcedure) THEN
  DO:
      RETURN.
  END.


  {get FieldInformation  xFieldInformation hProcedure}.
  {get FieldHandles      xFieldHandles  hProcedure}.
  {get DataSource        hDataSource    hProcedure}.
  
  hAppServer = SESSION:LAST-SERVER.
  IF NOT VALID-HANDLE(hAppServer) THEN
    hAppServer = THIS-PROCEDURE.
  
  IF xFieldInformation = '' THEN
  DO:
      {get ContainerSource hContainer hProcedure}.
      IF hAppserver <> THIS-PROCEDURE THEN
        RUN adm2/support/getRelations.p ON hAppServer (hProcedure:FILE-NAME,hContainer:FILE-NAME, OUTPUT xFieldInformation) NO-ERROR.
      ELSE
        RUN adm2/support/getRelations.p (hProcedure:FILE-NAME,hContainer:FILE-NAME, OUTPUT  xFieldInformation) NO-ERROR.
      
      {set FieldInformation            xFieldInformation  hProcedure}.
      
  END.
  
  iCount  = NUM-ENTRIES(xFieldHandles).
  iCount1 = NUM-ENTRIES(xFieldInformation).
  DO j = 1 TO iCount1:
      IF xRelatedTexts <> '' THEN
          xRelatedTexts = xRelatedTexts + ',' + ''.
      ELSE
          xRelatedTexts = ''.
    DO i = 1 TO iCount:
        hField = WIDGET-HANDLE(ENTRY(i,xFieldHandles)).
        IF hField:NAME = ENTRY (1,ENTRY(j,xFieldInformation),CHR(3)) THEN
        DO:
            IF /* hField:SENSITIVE  THEN
               IF */ SESSION:WINDOW-SYSTEM <> 'TTY' THEN
                hField:LOAD-MOUSE-POINTER('glove').
               ELSE
                hField:PFCOLOR = 14.
            RUN createText-1(hField ,OUTPUT htext).
            IF VALID-HANDLE (hText) THEN
            DO:
                ENTRY(j,xRelatedTexts) = STRING(hText).
            END.
        END.
    END.
  END.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getColumn Procedure 
FUNCTION getColumn RETURNS CHARACTER
  ( INPUT cFieldInformation AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    

  IF NUM-ENTRIES (cFieldInformation , CHR(3) ) >= 2 THEN
    RETURN ENTRY( 2 , cFieldInformation , CHR(3) ).   /* Function return value. */
  ELSE
    RETURN ''.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompatibleField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCompatibleField Procedure 
FUNCTION getCompatibleField RETURNS CHARACTER
  ( INPUT cFieldInformation AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    


  RETURN ENTRY( 7 , cFieldInformation , CHR(3) ).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTable Procedure 
FUNCTION getTable RETURNS CHARACTER
  ( INPUT cFieldInformation AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    


  RETURN ENTRY( 3 , cFieldInformation , CHR(3) ).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWhere) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWhere Procedure 
FUNCTION getWhere RETURNS CHARACTER
  ( INPUT  cFieldInformation AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    


  RETURN ENTRY( 6 , cFieldInformation , CHR(3) ).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

