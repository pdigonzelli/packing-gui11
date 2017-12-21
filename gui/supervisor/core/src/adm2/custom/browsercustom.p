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
    File        : browsercustom.p
    Purpose     : Super procedure to extend browser class.

    Syntax      : browsercustom.p

    Modified    : 06/03/1999
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ADMSuper browsercustom.p

DEFINE VAR hDataSource          AS HANDLE       NO-UNDO.
DEFINE VAR cASDivision          AS CHARACTER    NO-UNDO.
DEFINE VAR hAppServer           AS HANDLE       NO-UNDO.

DEFINE VARIABLE iRowColor AS INTEGER INITIAL 0   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getFilasSeleccionadas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFilasSeleccionadas Procedure 
FUNCTION getFilasSeleccionadas RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSelectedRowIds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectedRowIds Procedure 
FUNCTION getSelectedRowIds RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSelectedRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectedRows Procedure 
FUNCTION getSelectedRows RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTitle Procedure 
FUNCTION setTitle RETURNS CHARACTER
  ( INPUT ptitle AS CHARACTER)  FORWARD.

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

{src/adm2/brsprop.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CustomCreateSearchField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustomCreateSearchField Procedure 
PROCEDURE CustomCreateSearchField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cSearchField AS CHARACTER NO-UNDO.
DEFINE VAR hFrame                   AS HANDLE NO-UNDO.
DEFINE VAR cSortField               AS CHARACTER NO-UNDO.
DEFINE VAR hBrowse                  AS HANDLE NO-UNDO.
DEFINE VAR cSearchlabel             AS CHARACTER NO-UNDO.
DEFINE VAR hSearchLabel             AS HANDLE NO-UNDO.
DEFINE VAR hdataSource              AS HANDLE NO-UNDO.
DEFINE VAR cSearchFields            AS CHARACTER NO-UNDO.
DEFINE VAR hSearchField             AS HANDLE NO-UNDO.
DEFINE VAR cSearchField1            AS CHARACTER NO-UNDO.
DEFINE VAR flagDestroy              AS LOGICAL NO-UNDO.
DEFINE VAR lQueryOpen               AS LOGICAL NO-UNDO.
DEFINE VAR cRowIdent                AS CHARACTER NO-UNDO.
DEFINE VAR lHideOnInit              AS LOGICAL NO-UNDO.
  
  {get searchHandle hSearchField}.
  {get DataSource hDataSource}.   /* Proc. handle of our SDO. */
  {get BrowseHandle hBrowse}.     /* Handle of the browse widget. */

  IF valid-handle(hSearchField) AND hSearchField:NAME = cSearchField THEN
  DO:
      hSearchLabel = hSearchField:SIDE-LABEL-HANDLE.
      DELETE WIDGET hSearchLabel.
      DELETE WIDGET hSearchField.
      hBrowse:ROW = 1.
      hBrowse:HEIGHT = hBrowse:HEIGHT + 1.
      RETURN.    
  END.

  IF VALID-HANDLE(hSearchField) THEN 
  DO:
      DELETE WIDGET hSearchField.
      hBrowse:ROW = 1.
      hBrowse:HEIGHT = hBrowse:HEIGHT + 1.
  END.
  


    IF cSearchField NE "":U AND cSearchField NE ? THEN
    DO:
      {get ContainerHandle hFrame}.    /* Frame handle to put the widgets in. */
      /* NOTE: This resorts the query by the SearchField. */
      cSortField = {fnarg columnDbColumn cSearchField hDataSource}.
      /* Currently we only support dbfields as search/sort field */
      IF cSortField <> "":U THEN
      DO:
        
        ASSIGN hBrowse:HEIGHT = hBrowse:HEIGHT - 1  /* Shorten the browse */
               hBrowse:ROW = 2                     /* and place at row 2 */
               cSearchLabel = {fnarg columnLabel cSearchfield hDataSource}
                               + ": ":U.
        CREATE TEXT hSearchLabel            /* Label for the field */
          ASSIGN
            SCREEN-VALUE = cSearchLabel
            FORMAT = "X(":U + STRING(LENGTH(cSearchLabel)) + ")":U
            ROW     = 1
            WIDTH   = FONT-TABLE:GET-TEXT-WIDTH(cSearchLabel)
            HEIGHT  = 1
            COL     = 2
            FRAME   = hFrame
            SCREEN-VALUE = cSearchLabel
            HIDDEN       = FALSE.

        CREATE FILL-IN hSearchField
          ASSIGN
            DATA-TYPE = dynamic-function('columnDataType':U IN hDataSource,
                                         cSearchField)
            FORMAT    = dynamic-function('columnFormat':U IN hDataSource,
                                          cSearchField)
            ROW       = 1
            COL       = hSearchLabel:COL + hSearchLabel:WIDTH
            FRAME     = hFrame
            VISIBLE   = yes
            SENSITIVE = yes
            SIDE-LABEL-HANDLE = hSearchLabel
            NAME = cSearchField
          TRIGGERS:
            ON ANY-PRINTABLE
              PERSISTENT RUN searchTrigger IN TARGET-PROCEDURE .
          END TRIGGERS.
/*          {set QuerySort cSortField hDataSource}.
          {fn openQuery hDataSource}. */
          {set SearchField cSearchField}.
          {set SearchHandle hSearchField}.
          
          IF {fn getQuerySort hDataSource} <> cSortField THEN
          DO:
            {set QuerySort cSortField hDataSource}. 
            {get QueryOpen lQueryOpen hDataSource}.
            IF lQueryOpen THEN 
            DO:
              {get RowIdent cRowIdent hDataSource}.
              IF cRowIdent > "":U THEN
              DO:
                DYNAMIC-FUNCTION('closeQuery':U IN hDataSource).
                DYNAMIC-FUNCTION('fetchRowIdent':U IN hDataSource, cRowIdent, '':U) NO-ERROR.
              END.
              ELSE
                /* Reopen query and reposition to current rowid */
                DYNAMIC-FUNCTION('openQuery':U IN hDataSource).     
            END.
          END.
          {get HideOnInit lHideOnInit}.
          IF lHideOnInit THEN 
            RUN hideObject IN TARGET-PROCEDURE.

      END.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CustomValidateField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustomValidateField Procedure 
PROCEDURE CustomValidateField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER hfield     AS HANDLE       NO-UNDO.

  DEFINE VAR hSource                AS HANDLE       NO-UNDO.
  DEFINE VAR cDisplayedFields       AS CHARACTER    NO-UNDO.
  DEFINE VAR cEnabledFields       AS CHARACTER    NO-UNDO.
  DEFINE VAR hContainer             AS HANDLE       NO-UNDO.
  DEFINE VAR lBrowseField           AS LOGICAL      NO-UNDO.
  DEFINE VAR lResult                AS LOGICAL      NO-UNDO.
  DEFINE VAR mRes                   AS character   NO-UNDO.

  mRes = ''.
  
  {get EnabledFields cEnabledFields}.

  IF LOOKUP(hfield:NAME,cEnabledFields) = 0 THEN RETURN.


  {get DisplayedFields cDisplayedFields}.
  {get ContainerHandle hContainer}.  

  DYNAMIC-FUNCTION('clearColorField' IN TARGET-PROCEDURE , hField) NO-ERROR.

   /* Ignore the event if it wasn't a viewer field. 
      Note that we assume that an event from a widget in a different
      frame is for a SmartDataField and also is a viewerfield  */
  IF LOOKUP(hfield:TABLE + '.':U + hfield:NAME,cDisplayedFields) NE 0 OR 
      LOOKUP(hfield:NAME,cDisplayedFields) NE 0 THEN
     lBrowseField = yes.
   
   IF lBrowseField THEN 
   DO:
     {get FieldsEnabled lResult}.  /* Only if the object's enable for input.*/
     IF lResult THEN DO:
         {get DataSource hSource}.
         RUN VALUE(hfield:NAME + 'Validate') IN hSource (hfield:INPUT-VALUE) NO-ERROR.
         IF RETURN-VALUE <> '' THEN
         DO:
            mres = RETURN-VALUE.
            DYNAMIC-FUNCTION('applyColorToFieldWithProblem' IN TARGET-PROCEDURE).
            RUN addMessage IN TARGET-PROCEDURE 
                    (IF RETURN-VALUE NE "" THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1), 
                     hfield:NAME, 
                      ?).
         END.
         RETURN mRes.
     END.   /* END DO IF Fields are Enabled */
   END.     /* END DO IF it's a RowObject field */




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
RUN setRelatedFields (TARGET-PROCEDURE).
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
DEFINE VAR xFields              AS CHARACTER    NO-UNDO.


FieldName = hField:NAME.

{get FieldInformation   xFieldInformation}.
RUN getFieldList (xFieldInformation , OUTPUT xFields).

i = LOOKUP(FieldName,xFields ).
IF i > 0 THEN
DO:
    IF NOT hField:READ-ONLY THEN
    DO:
        hAppServer = SESSION:LAST-SERVER.
        IF NOT VALID-HANDLE(hAppServer) THEN
            hAppServer = THIS-PROCEDURE.
        RUN adm2/support/seleccion.p (IF hAppserver = THIS-PROCEDURE THEN ? ELSE hAppserver , PROGRAM-NAME(3) , SOURCE-PROCEDURE:FILE-NAME  , hField , OUTPUT xResult) NO-ERROR.
        IF NOT hField:READ-ONLY AND xResult <> '' AND xResult <> ? THEN
            RUN setField IN TARGET-PROCEDURE (hField:NAME , xResult).
        IF xResult <> '' AND xResult <> ? THEN
            hField:SCREEN-VALUE =  xResult.
    END.
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
  
  DEFINE VAR xRelatedFields   AS CHARACTER NO-UNDO.
  DEFINE VAR hField           AS HANDLE       NO-UNDO.
  DEFINE VAR xFieldHandles    AS CHARACTER    NO-UNDO.
  DEFINE VAR i                AS INTEGER      NO-UNDO.
  DEFINE VAR iCount           AS INTEGER      NO-UNDO.
  DEFINE VAR iCount1          AS INTEGER      NO-UNDO.
  DEFINE VAR j                AS INTEGER      NO-UNDO.
  DEFINE VAR hText            AS HANDLE       NO-UNDO.
  DEFINE VAR hContainer       AS HANDLE       NO-UNDO.



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
       
 {get ContainerSource hContainer hProcedure}.

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
    DO i = 1 TO iCount:
        hField = WIDGET-HANDLE(ENTRY(i,xFieldHandles)).
        IF hField:NAME = ENTRY (1,ENTRY(j,xFieldInformation),CHR(3)) THEN
        DO:
            IF NOT hField:READ-ONLY  THEN
               IF SESSION:WINDOW-SYSTEM <> 'TTY' THEN
                hField:LOAD-MOUSE-POINTER('glove').
               ELSE
                hField:PFCOLOR = 14.
        END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getFilasSeleccionadas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFilasSeleccionadas Procedure 
FUNCTION getFilasSeleccionadas RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR i              AS INTEGER      NO-UNDO.
  DEFINE VAR cRows          AS CHARACTER    NO-UNDO.
  DEFINE VAR hDataSource    AS HANDLE       NO-UNDO.
  DEFINE VAR hDataQuery     AS HANDLE       NO-UNDO.
  DEFINE VAR hRowObject     AS HANDLE       NO-UNDO.
  DEFINE VAR hBrowse        AS HANDLE       NO-UNDO.
  DEFINE VAR hField         AS HANDLE       NO-UNDO.

  {get BrowseHandle hBrowse}.
  {get DataSource hDataSource}.
  
  
  DO i = 1 TO hBrowse:NUM-SELECTED-ROWS:
    hBrowse:FETCH-SELECTED-ROW(i).
    cRows = cRows + DYNAMIC-FUNCTION('columnValue' IN hDataSource, 'rowNum') + CHR(10).
  END.

  cRows = SUBSTRING(cRows, 1 , LENGTH(cRows) - 1).
  
  /*
  hDataQuery = hBrowse:QUERY.
  hRowObject = hDataQuery:GET-BUFFER-HANDLE(1).
  
  DO i = 1 TO hBrowse:NUM-SELECTED-ROWS:
    hBrowse:FETCH-SELECTED-ROW(i).
    hField = hRowObject:BUFFER-FIELD('RowNum').
    cRows  = cRows +  "," + hField:BUFFER-VALUE.
  END.
  IF cRows <> "" THEN
    cRows = SUBSTRING(cRows,2).

  RETURN cRows.   
  */


  RETURN cRows.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSelectedRowIds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectedRowIds Procedure 
FUNCTION getSelectedRowIds RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRow AS INTEGER NO-UNDO.
  DEFINE VARIABLE hSdo AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hBro AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  {get BrowseHandle hBro}.
  {get dataSource hSdo}.
  
  DO iRow = 1 TO hBro:NUM-SELECTED-ROWS:
    hBro:FETCH-SELECTED-ROW(iRow).
    cRet = cRet + "," + STRING(DYNAMIC-FUNCTION('getRowId' IN hSdo)).    
  END.

  IF cRet <> "" THEN
    cRet = SUBSTRING(cRet, 2).

  RETURN cRet.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSelectedRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectedRows Procedure 
FUNCTION getSelectedRows RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR i              AS INTEGER      NO-UNDO.
  DEFINE VAR cRows          AS CHARACTER    NO-UNDO.
  DEFINE VAR hDataSource    AS HANDLE       NO-UNDO.
  DEFINE VAR hDataQuery     AS HANDLE       NO-UNDO.
  DEFINE VAR hRowObject     AS HANDLE       NO-UNDO.
  DEFINE VAR hBrowse        AS HANDLE       NO-UNDO.
  DEFINE VAR hField         AS HANDLE       NO-UNDO.

  {get BrowseHandle hBrowse}.
  {get dataSource hDataSource}.
  
  hDataQuery = hBrowse:QUERY.
  hRowObject = hDataQuery:GET-BUFFER-HANDLE(1).
  
  DO i = 1 TO hBrowse:NUM-SELECTED-ROWS:
    hBrowse:FETCH-SELECTED-ROW(i).
    hField = hRowObject:BUFFER-FIELD('RowNum').
    cRows  = cRows +  "," + hField:BUFFER-VALUE.
  END.
  IF cRows <> "" THEN
    cRows = SUBSTRING(cRows,2).

  RETURN cRows.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTitle Procedure 
FUNCTION setTitle RETURNS CHARACTER
  ( INPUT ptitle AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR hBrowse        AS HANDLE       NO-UNDO.
  {get BrowseHandle hBrowse}
  hbrowse:TITLE = ptitle. 
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

