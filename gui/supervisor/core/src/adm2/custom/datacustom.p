&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*--------------------------------------------------------------------------
    File        : datacustom.p
    Purpose     : Super procedure to extend data class.

    Syntax      : datacustom.p

    Modified    : 06/03/1999
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ADMSuper datacustom.p

&SCOPED-DEFINE cSeparator CHR(3)

DEFINE TEMP-TABLE ttTable NO-UNDO
FIELD iRow                AS INTEGER    /* Row 0 = labels, 1 = fieldnames, 2 = datatypes, 3 = field widths >9 = data */
FIELD iCol                AS INTEGER    /* Column number */
FIELD cCell               AS CHARACTER  /* Character representation of field value */
INDEX mainidx IS PRIMARY UNIQUE iRow iCol.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getItemsHandles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getItemsHandles Procedure 
FUNCTION getItemsHandles RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowid Procedure 
FUNCTION getRowid RETURNS ROWID
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRowidByIndex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowidByIndex Procedure 
FUNCTION getRowidByIndex RETURNS ROWID
  ( INPUT indice AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isChild) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isChild Procedure 
FUNCTION isChild RETURNS LOGICAL
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
   Other Settings: CODE-ONLY COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 51.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{src/adm2/dataprop.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-assignDBRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignDBRow Procedure 
PROCEDURE assignDBRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER phRowObjUpd AS HANDLE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  
  RUN preAssignDbRow IN TARGET-PROCEDURE( INPUT phRowObjUpd ) NO-ERROR.
  IF RETURN-VALUE <> ''  THEN RETURN RETURN-VALUE.
  
  RUN especialPreSection (INPUT phRowObjUpd, 'x' , TARGET-PROCEDURE).
  IF RETURN-VALUE <> ''  THEN RETURN RETURN-VALUE.
  
  RUN SUPER( INPUT phRowObjUpd).
  IF RETURN-VALUE <> ''  THEN RETURN RETURN-VALUE.
  
  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN postAssignDbRow IN TARGET-PROCEDURE( INPUT phRowObjUpd) NO-ERROR.
  IF RETURN-VALUE <> ''  THEN RETURN RETURN-VALUE.
  
  RUN especialPosSection(INPUT phRowObjUpd , 'x' , TARGET-PROCEDURE).
  IF RETURN-VALUE <> ''  THEN RETURN RETURN-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-especialPosSection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE especialPosSection Procedure 
PROCEDURE especialPosSection :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER phRowObjUpd  AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER xCase        AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER hProcedure   AS HANDLE    NO-UNDO.


DEFINE VAR hObjUpd          AS HANDLE NO-UNDO.
DEFINE VAR hBUffer          AS HANDLE NO-UNDO.
DEFINE VAR xBufferHandles   AS CHARACTER NO-UNDO.
DEFINE VAR hRowMod          AS HANDLE NO-UNDO.
DEFINE VAR fFieldRowObj     AS HANDLE NO-UNDO.

IF NOT VALID-HANDLE(hProcedure) THEN
DO:
    RETURN.
END.
{get RowObjUpd hObjUpd hProcedure}.
{get BufferHandles xBufferHandles hProcedure}.
hBuffer = WIDGET-HANDLE(entry(1,xBufferHandles)).
RUN especialPosUpdate IN hProcedure (hBuffer , xCase) NO-ERROR.
RETURN RETURN-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-especialPreSection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE especialPreSection Procedure 
PROCEDURE especialPreSection :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER phRowObjUpd  AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER xCase        AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER hProcedure   AS HANDLE    NO-UNDO.

DEFINE VAR hObjUpd          AS HANDLE NO-UNDO.
DEFINE VAR hBUffer          AS HANDLE NO-UNDO.
DEFINE VAR xBufferHandles   AS CHARACTER NO-UNDO.
DEFINE VAR hRowMod          AS HANDLE NO-UNDO.
DEFINE VAR fFieldRowObj     AS HANDLE NO-UNDO.

IF NOT VALID-HANDLE(hProcedure) THEN
DO:
    RETURN.
END.
{get RowObjUpd hObjUpd hProcedure}.
{get BufferHandles xBufferHandles hProcedure}.
hBuffer = WIDGET-HANDLE(entry(1,xBufferHandles)).
hRowMod = hObjUpd:BUFFER-FIELD('RowMod').

IF hRowMod:BUFFER-VALUE = 'A' OR hRowMod:BUFFER-VALUE = 'C' THEN
    RUN especialPreCreate IN hProcedure (xCase) NO-ERROR.
ELSE
    RUN especialPreUpdate IN hProcedure (hBUffer , xCase) NO-ERROR.

RETURN RETURN-VALUE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-tableOut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tableOut Procedure 
PROCEDURE tableOut :
/*------------------------------------------------------------------------------
  Purpose: Parche al original que era de dinamycs    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
  Purpose:     Output requested fields of SDO in standard temp-table
  Parameters:  input field list
               input include object fields yes/no
               input maximum records to process
               output temp-table of data from sdo
               output number of records extracted
  
  Notes:       Temp table is defined in afttsdoout.i
               Fields passed in are checked with a can-do so support * for all
               or !field to exclude a field, e.g.
               "!RowNum,!RowIdent,!RowMod, *" would use all non SDO specific
               fields.
               For fields passed in - do NOT use a table prefix
               The temp table contains a record per record/field combination
               
               Currently has a hard coded limit of 5000 records to fetch - max!
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pcFieldList                AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER plIncludeObj               AS LOGICAL    NO-UNDO.
DEFINE INPUT PARAMETER piMaxRecords               AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttTable.
DEFINE OUTPUT PARAMETER iExtractedRecs            AS INTEGER    NO-UNDO.

DEFINE VARIABLE iRowNum                           AS INTEGER    NO-UNDO.
DEFINE VARIABLE iColNum                           AS INTEGER    NO-UNDO.
DEFINE VARIABLE iPosn                             AS INTEGER    NO-UNDO.
DEFINE VARIABLE iField                            AS INTEGER    NO-UNDO.
DEFINE VARIABLE lAvailable                        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE hQuery                            AS HANDLE     NO-UNDO.
DEFINE VARIABLE hBuffer                           AS HANDLE     NO-UNDO.
DEFINE VARIABLE hBufferField                      AS HANDLE     NO-UNDO.
DEFINE VARIABLE lInitialized                      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iRowsToBatch                      AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNumRecords                       AS INTEGER    NO-UNDO.
DEFINE VARIABLE cQueryPosition                    AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cRowIdent                         AS CHARACTER  NO-UNDO.

/* Variables for Security Check of Fields */
DEFINE VARIABLE hContainerHandle  AS HANDLE     NO-UNDO.
DEFINE VARIABLE cRunAttribute     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cContainerName    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSecuredFields    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cHiddenFields     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iLoop             AS INTEGER    NO-UNDO.
DEFINE VARIABLE cFieldName        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cNewFieldList     AS CHARACTER  NO-UNDO.

/* Start of Security check */

ASSIGN hContainerHandle = TARGET-PROCEDURE.


IF VALID-HANDLE(hContainerHandle) THEN DO:
  {get LogicalObjectName cContainerName hContainerHandle} NO-ERROR.
  cRunAttribute = DYNAMIC-FUNCTION('getRunAttribute' IN hContainerHandle) NO-ERROR.  
END.
ELSE
  cContainerName = "":U.
   
IF cContainerName = "":U AND
  VALID-HANDLE(hContainerHandle) THEN
   cContainerName = hContainerHandle:FILE-NAME.
   
IF cContainerName <> "":U THEN
  ASSIGN cContainerName = REPLACE(cContainerName,"~\":U,"/":U)
         cContainerName = SUBSTRING(cContainerName,R-INDEX(cContainerName,"/":U) + 1,LENGTH(cContainerName)).
ELSE
 cContainerName = "":U.

  
ASSIGN cHiddenFields = "":U
       cNewFieldList = "":U.


/* Now Remove the hidden fields from pcFieldList */
IF INDEX(pcFieldList,"*":U) > 0 THEN DO:
  /* Now add ! to every field to exclude them */
  DO iLoop = 1 TO NUM-ENTRIES(cHiddenFields):
    ENTRY(iLoop,cHiddenFields) = "!":U + ENTRY(iLoop,cHiddenFields).
  END.
  pcFieldList = TRIM(pcFieldList,"*":U) + cHiddenFields + ",*":U.
END.
ELSE DO:
  DO iLoop = 1 TO NUM-ENTRIES(pcFieldList):
    cFieldName = ENTRY(iLoop,pcFieldList).
    IF LOOKUP(cFieldName,cHiddenFields) > 0 THEN
      NEXT.
    cNewFieldList = IF cNewFieldList = "":U
                       THEN cFieldName
                       ELSE cNewFieldList + ",":U + cFieldName.
  END.
  ASSIGN pcFieldList = cNewFieldList.
END.

ASSIGN
  piMaxRecords = (IF piMaxRecords > 0 THEN piMaxRecords ELSE 99999999)
  iNumRecords = 0
.

 /* Store current position in query */
 cRowident = DYNAMIC-FUNCTION('getRowIdent':U IN TARGET-PROCEDURE) NO-ERROR.

 /* Pretend we are not initialized to disable datavailable messages, etc whilst
    we do the business here
 */
 {get objectinitialized lInitialized}.
 {set objectinitialized NO}.

 /* Ensure temp-table is empty to start */
 EMPTY TEMP-TABLE ttTable.

 ASSIGN
   iRowNum = 0
   iColNum = 0
   .

 /* set rows to batch very high as we will read all the data */
 {get rowsToBatch iRowsToBatch}.
 {set rowsToBatch piMaxRecords}.

 /* start at the beginning */
 RUN fetchFirst IN TARGET-PROCEDURE.

 /* check if any records */
 lAvailable = DYNAMIC-FUNCTION('getQueryPosition':U IN TARGET-PROCEDURE) <> "NoRecordAvailable":U.

 IF lAvailable THEN
 DO:
   ASSIGN
     iNumRecords = 1
     hQuery = DYNAMIC-FUNCTION('getDataHandle':U IN TARGET-PROCEDURE)
     hBuffer = hQuery:GET-BUFFER-HANDLE(1)
     .

   /* loop through sdo fields and create TT records for field labels, names,  datatypes and widths */
   field-loop:
   DO iField = 1 TO hBuffer:NUM-FIELDS:
     hBufferField = hBuffer:BUFFER-FIELD(iField).

     IF plIncludeObj = NO AND hBufferField:NAME MATCHES("*_obj":U) THEN NEXT field-loop.

     IF CAN-DO(pcFieldList, hBufferField:NAME) THEN
     DO:
       ASSIGN 
         iColNum = iColNum + 1
         iPosn = LOOKUP(hBufferField:NAME, pcFieldList)
         .      
       /* Store labels in row 0 */
       CREATE ttTable.
       ASSIGN 
         ttTable.iRow = 0
         ttTable.iCol = (IF iPosn = 0 THEN iColNum ELSE iPosn) /* use sdo col posittion if fields passed in as */
         ttTable.cCell = TRIM(hBufferField:COLUMN-LABEL)
         .
       /* Store names in row 1 */
       CREATE ttTable.
       ASSIGN 
         ttTable.iRow = 1
         ttTable.iCol = (IF iPosn = 0 THEN iColNum ELSE iPosn) /* use sdo col posittion if fields passed in as */
         ttTable.cCell = TRIM(hBufferField:NAME)
         .
       /* Store datatypes in row 2 */
       CREATE ttTable.
       ASSIGN 
         ttTable.iRow = 2
         ttTable.iCol = (IF iPosn = 0 THEN iColNum ELSE iPosn) /* use sdo col posittion if fields passed in as */
         ttTable.cCell = TRIM(hBufferField:DATA-TYPE)
         .

       /* Store widths in row 3 */
       CREATE ttTable.
       ASSIGN 
         ttTable.iRow = 3
         ttTable.iCol = (IF iPosn = 0 THEN iColNum ELSE iPosn) /* use sdo col posittion if fields passed in as */
         ttTable.cCell = TRIM(STRING(hBufferField:WIDTH-CHARS))
         .
     END. /* This field is requested */
   END. /* Loop through the buffer fields */

   ASSIGN
     iRowNum = 9.  /* >9 = data */

   /* now loop through all available records */
   REPEAT WHILE lAvailable:

     IF NOT hBuffer:AVAILABLE THEN LEAVE.

     ASSIGN 
       iRowNum = iRowNum + 1
       iColNum = 0.
     field-loop2:
     DO iField = 1 to hBuffer:NUM-FIELDS:
       ASSIGN hBufferField = hBuffer:BUFFER-FIELD(iField).

       IF plIncludeObj = NO AND hBufferField:NAME MATCHES("*_obj":U) THEN NEXT field-loop2.

       IF CAN-DO(pcFieldList, hBufferField:NAME) THEN 
       DO:
         CREATE ttTable.
         ASSIGN
           iColNum = iColNum + 1
           iPosn = LOOKUP(hBufferField:NAME, pcFieldList)
           ttTable.iRow = iRowNum
           ttTable.iCol = (IF iPosn = 0 THEN iColNum ELSE iPosn)
           ttTable.cCell = TRIM(hBufferField:STRING-VALUE)
         .
       END. /* This field is requested */
     END. /* Loop through the buffer fields */


     cQueryPosition = DYNAMIC-FUNCTION('getQueryPosition':U IN TARGET-PROCEDURE).
     lAvailable = NOT CAN-DO("LastRecord,OnlyRecord":U, cQueryPosition).

     ASSIGN
       iNumRecords = iNumRecords + 1.

     IF lAvailable AND iNumRecords <= piMaxRecords THEN
       RUN fetchNext IN TARGET-PROCEDURE.
     ELSE
       ASSIGN lAvailable = NO.

   END. /* Loop through all available records */

 END.  /* lAvailable = true */

 /* reposition back to previously selected record */
 IF cRowIdent <> ? AND cRowIdent <> "":U THEN
   DYNAMIC-FUNCTION('fetchRowIdent' IN TARGET-PROCEDURE, cRowIdent, '':U) NO-ERROR.

 /* tidy up */
 ASSIGN
   hQuery = ?
   hBuffer = ?
   iExtractedRecs = iNumRecords - 1
   .

 {set objectinitialized lInitialized}.

 /* reset rows to batch back */
 {set rowsToBatch iRowsToBatch}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferToExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferToExcel Procedure 
PROCEDURE transferToExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcFieldList      AS CHARACTER            NO-UNDO.
DEFINE INPUT PARAMETER plIncludeObj     AS LOGICAL              NO-UNDO.
DEFINE INPUT PARAMETER plUseExisting    AS LOGICAL              NO-UNDO.
DEFINE INPUT PARAMETER piMaxRecords     AS INTEGER              NO-UNDO.

DEFINE VARIABLE chExcel                 AS COM-HANDLE           NO-UNDO.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE           NO-UNDO.
DEFINE VARIABLE chWorkSheet             AS COM-HANDLE           NO-UNDO.
DEFINE VARIABLE chRange                 AS COM-HANDLE           NO-UNDO.
DEFINE VARIABLE chColumns               AS COM-HANDLE           NO-UNDO.
DEFINE VARIABLE cRow                    AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cRange1                 AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cRange2                 AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cRange3                 AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cFullRange              AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHeading                AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cNumericCellString      AS CHARACTER            NO-UNDO.
DEFINE VARIABLE iSplit                  AS INTEGER              NO-UNDO.
DEFINE VARIABLE hBrowserColumn          AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE iNumberOfColumns        AS INTEGER              NO-UNDO.
DEFINE VARIABLE iNumberOfRows           AS INTEGER              NO-UNDO.
DEFINE VARIABLE iLoop                   AS INTEGER              NO-UNDO.
DEFINE VARIABLE lDone                   AS LOGICAL              NO-UNDO INITIAL NO.
DEFINE VARIABLE lExcelInstalled         AS LOGICAL              NO-UNDO.
DEFINE VARIABLE cAbort                  AS CHARACTER            NO-UNDO.      
DEFINE VARIABLE cErrorMessage           AS CHARACTER            NO-UNDO.      
DEFINE VARIABLE cDataType               AS CHARACTER            NO-UNDO.      
DEFINE VARIABLE cDataTypeList           AS CHARACTER            NO-UNDO.      
DEFINE VARIABLE cFieldName              AS CHARACTER            NO-UNDO.      
DEFINE VARIABLE cFieldNameList          AS CHARACTER            NO-UNDO.      
DEFINE VARIABLE dWidth                  AS DECIMAL              NO-UNDO.      
DEFINE VARIABLE cFieldWidthList         AS CHARACTER            NO-UNDO.      
DEFINE VARIABLE iRecordCnt              AS INTEGER              NO-UNDO.

IF plUseExisting = ? THEN ASSIGN plUseExisting = YES.

SESSION:SET-WAIT-STATE("GENERAL":U).

IF pcFieldList = "":U THEN
  ASSIGN pcFieldList = "!RowNum,!RowIdent,!RowMod,*":U.
ELSE
  ASSIGN pcFieldList = pcFieldList + ",!RowNum,!RowIdent,!RowMod":U.

/* get data to export to excel */

RUN tableOut IN TARGET-PROCEDURE (INPUT pcFieldList, INPUT plIncludeObj, INPUT piMaxRecords, OUTPUT TABLE ttTable, OUTPUT iRecordCnt).
IF NOT CAN-FIND(FIRST ttTable) THEN
DO:
  SESSION:SET-WAIT-STATE("":U).
  ASSIGN cErrorMessage = "No data to export to Excel".
  RUN showMessages IN gshSessionManager (INPUT cErrorMessage,
                                         INPUT "INF":U,
                                         INPUT "OK":U,
                                         INPUT "OK":U,
                                         INPUT "OK":U,
                                         INPUT "Excel Tranfer Error",
                                         INPUT YES,
                                         INPUT ?,
                                         OUTPUT cAbort).
  RETURN.
END.

IF NOT lExcelInstalled THEN DO:
    /* Only allow export to MS Excel if at least Office97 exists */
    LOAD "Excel.Application":U BASE-KEY "HKEY_CLASSES_ROOT":U NO-ERROR. /* Open Registry key */
    ASSIGN lExcelInstalled = (ERROR-STATUS:ERROR = NO).
    UNLOAD "Excel.Application":U NO-ERROR.
END.    /* check if Excel is installed. */

IF NOT lExcelInstalled THEN
DO:
  SESSION:SET-WAIT-STATE("":U).
  ASSIGN cErrorMessage = "Excel not installed or running version prior to Office 97".
  RUN showMessages IN gshSessionManager (INPUT cErrorMessage,
                                         INPUT "INF":U,
                                         INPUT "OK":U,
                                         INPUT "OK":U,
                                         INPUT "OK":U,
                                         INPUT "Excel Tranfer Erro",
                                         INPUT YES,
                                         INPUT ?,
                                         OUTPUT cAbort).
  RETURN.
END.

IF lExcelInstalled THEN
DO:
  IF plUseExisting THEN
    CREATE "Excel.Application" chExcel CONNECT NO-ERROR.
  IF NOT VALID-HANDLE(chExcel) THEN
    CREATE "Excel.Application" chExcel.
  
  chExcel:VISIBLE  = FALSE.                   /* Launch Excel so it is visible to the user    */
  /* 
  chExcel:DecimalSeparator = ".". 
  
  chExcel:ThousandsSeparator = ",".
  chExcel:UseSystemSeparators = False. */

  chWorkbook       = chExcel:Workbooks:ADD(). /* Create a new Workbook                        */
  chWorkSheet      = chExcel:Sheets:ITEM(1).  /* Get the active Worksheet                     */
  chWorkSheet:NAME = "Browser".               /* Set the worksheet name                       */

  ASSIGN 
    iNumberOfColumns = 0
    iNumberOfRows    = 0
    cDataTypeList = "":U
    cFieldNameList = "":U
    cFieldWidthList = "":U
    cRow = "A"
    .

  /* build list of field names */
  FOR EACH ttTable WHERE ttTable.irow = 1:
    ASSIGN
      cFieldNameList = cFieldNameList +
                       (IF cFieldNameList <> "":U THEN ",":U ELSE "":U) +
                       ttTable.cCell 
      .     
  END.

  /* build list of field datatypes */
  FOR EACH ttTable WHERE ttTable.irow = 2:
    ASSIGN
      cDataTypeList = cDataTypeList +
                       (IF cDataTypeList <> "":U THEN ",":U ELSE "":U) +
                       ttTable.cCell 
      .     
  END.

  /* build list of field widths, chr(3) delimited to cope with European formats */
  FOR EACH ttTable WHERE ttTable.irow = 3:
    ASSIGN
      cFieldWidthList = cFieldWidthList +
                       (IF cFieldWidthList <> "":U THEN CHR(3) ELSE "":U) +
                       ttTable.cCell 
      .     
  END.



  /* loop through column headings */
  heading-loop:
  FOR EACH ttTable 
     WHERE ttTable.irow = 0
     BREAK BY ttTable.iCol:

    ASSIGN 
      iNumberOfColumns = iNumberOfColumns + 1
      cRange1  = STRING((CHR(ASC(cRow) + (iNumberOfColumns - 1)) + STRING(1)))
      cHeading = ttTable.cCell
      iSplit   = INDEX(cHeading,"!":U)
      cDataType = ENTRY(iNumberOfColumns, cDataTypeList)
      dWidth    = DECIMAL(ENTRY(iNumberOfColumns,cFieldWidthList,CHR(3)))
      .

    IF iNumberOfColumns > 26 THEN LEAVE heading-loop. /* max 26 columns */
    
    IF iSplit = 0 THEN
        ASSIGN
            chWorkSheet:Range(cRange1):Value     = cHeading
            chWorkSheet:Range(cRange1):Font:Bold = TRUE.
    ELSE
        ASSIGN
            cRange2                              = STRING((CHR(ASC(cRow) + (iNumberOfColumns - 1)) + STRING(2)))
            chWorkSheet:Range(cRange1):Value     = SUBSTRING(cHeading, 1, iSplit - 1)
            chWorkSheet:Range(cRange1):Font:Bold = TRUE
            chWorkSheet:Range(cRange2):Value     = SUBSTRING(cHeading, iSplit + 1)
            chWorkSheet:Range(cRange2):Font:Bold = TRUE.

    ASSIGN
        cRange3                                   = SUBSTRING(cRange1, 1, 1)
        chWorkSheet:Columns(cRange3):ColumnWidth  = IF cDataType                       = "DECIMAL":U THEN 20
                                                    ELSE IF cDataType                  = "INTEGER":U THEN 6
                                                    ELSE IF dWidth > 100 THEN 100 ELSE dWidth
        chWorkSheet:Columns(cRange3):NumberFormat = IF cDataType                       = "DECIMAL":U THEN "###,###,##0.00":U
                                                    ELSE IF cDataType                  = "INTEGER":U THEN "#####0":U
                                                    ELSE "@":U
        .
  END. /* heading-loop */

  IF iNumberOfColumns > 26 THEN ASSIGN iNumberOfColumns = 26. /* max 26 columns */

  ERROR-STATUS:ERROR = NO.

  ASSIGN
      cRow  = "A":U
      lDone = NO NO-ERROR.
  
  /* Load the data
     ============= */

  data-loop:
  FOR EACH ttTable
     WHERE ttTable.iRow > 9:

    IF ttTable.iCol > iNumberOfColumns THEN NEXT data-loop.
    IF ttTable.iCol = 1 THEN ASSIGN iNumberOfRows = ttTable.iRow.

    ASSIGN 
      cDataType = ENTRY(ttTable.iCol, cDataTypeList)
      dWidth    = DECIMAL(ENTRY(ttTable.iCol,cFieldWidthList,CHR(3)))
      .

    /* We must make sure that decimals and integers have a leading sign. */
    IF  cDataType = "DECIMAL":U OR  cDataType = "INTEGER":U THEN
    DO:
      /* Remove any percentage symbols, and plus signs ... */
      ASSIGN
          cNumericCellString = REPLACE(ttTable.cCell, "%":U, "":U)
          cNumericCellString = REPLACE(cNumericCellString, "+":U, "":U).
      
      /* ... the convert all DR/CR symbols and parentheses to '-' where neccessary ... */
      IF cNumericCellString BEGINS "(":U THEN
          ASSIGN
              cNumericCellString = "-":U + SUBSTRING(cNumericCellString,2)
              cNumericCellString = SUBSTRING(cNumericCellString, 1, LENGTH(cNumericCellString) - 1).

      ASSIGN
          cNumericCellString = REPLACE(cNumericCellString, "DR":U, "-":U)
          cNumericCellString = REPLACE(cNumericCellString, "CR":U, "-":U)
          cNumericCellString = REPLACE(cNumericCellString, "DB":U, "-":U).
      
      /* ... then make sure that the negative sign is leading (by now we should have a regular decimal/integer) */
      IF cDataType = "DECIMAL":U THEN
          IF  NOT cNumericCellString BEGINS "-":U
          AND DECIMAL(cNumericCellString)   < 0 THEN
              ASSIGN
              cNumericCellString = "-":U + STRING(ABSOLUTE(DECIMAL(cNumericCellString))) NO-ERROR.
      ELSE IF cDataType = "INTEGER":U THEN
          IF  NOT cNumericCellString BEGINS "-":U
          AND INTEGER(cNumericCellString)   < 0 THEN
              ASSIGN
                  cNumericCellString = "-":U + STRING(ABSOLUTE(INTEGER(cNumericCellString))) NO-ERROR.
      
      /* (just in case there's something wrong) */
      IF  ERROR-STATUS:ERROR THEN
          ASSIGN cNumericCellString = ttTable.cCell.
    END.    /* decimal/integer */
    
    ASSIGN
      cRange1                          = STRING((CHR(ASC(cRow) + (ttTable.iCol - 1)) + STRING(ttTable.iRow - 8)))
      chWorkSheet:Range(cRange1):Value = IF  cDataType = "INTEGER":U OR cDataType = "DECIMAL":U THEN
                                             cNumericCellString
                                         ELSE IF cDataType = "CHARACTER":U
                                         AND LENGTH( ttTable.cCell ) > 319 THEN
                                             SUBSTRING( ttTable.cCell, 1, 319 )
                                         ELSE ttTable.cCell
      .

    IF ttTable.iCol = iNumberOfColumns THEN
    ASSIGN
      cRow    = "A":U
      cRange1 = cRow + ":":U + cRange3
      .
  END. /* data-loop */

  IF CAN-FIND(FIRST ttTable
              WHERE ttTable.iRow > 9) THEN DO:
    ASSIGN iNumberofRows = iNumberOfRows - 8.  /* real number of data rows, plus heading line */
  
    ASSIGN
      cFullRange = "A1:" + STRING((CHR(ASC(cRow) + (iNumberofColumns - 1)) + STRING(iNumberofRows)))
      chRange = chWorkSheet:Range(cFullRange)
      chColumns = chRange:COLUMNS.
      .
    chColumns:autofit.
  END.

  SESSION:SET-WAIT-STATE("":U).
  
  ASSIGN
      chExcel:Visible = True.
  
  IF VALID-HANDLE(chRange) THEN
    RELEASE OBJECT chRange.
  IF VALID-HANDLE(chColumns) THEN
    RELEASE OBJECT chColumns.
  IF VALID-HANDLE(chWorkSheet) THEN
    RELEASE OBJECT chWorkSheet.
  IF VALID-HANDLE(chWorkbook) THEN
    RELEASE OBJECT chWorkbook.
  IF VALID-HANDLE(chExcel) THEN
    RELEASE OBJECT chExcel.

  ASSIGN
    chRange = ?
    chColumns = ?
    chWorkSheet = ?
    chWorkbook = ?
    chExcel = ?
    .    

END.    /* valid handle - browse */

RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getItemsHandles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getItemsHandles Procedure 
FUNCTION getItemsHandles RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR i              AS INTEGER      NO-UNDO.
  DEFINE VAR xDataTargets   AS CHARACTER    NO-UNDO.
  DEFINE VAR hObject        AS HANDLE       NO-UNDO.
  DEFINE VAR xObjectType    AS CHARACTER    NO-UNDO.
  DEFINE VAR xItemsHandles  AS CHARACTER    NO-UNDO.

  {get DataTarget xDataTargets}.
  DO i = 1 TO NUM-ENTRIES(xDataTargets) :
          hObject = WIDGET-HANDLE(ENTRY(i,xDataTargets)).
         {get ObjectType xObjectType hObject}.
         IF xObjectType = 'SmartDataObject' THEN
         DO:
           IF xItemsHandles = '' THEN
               xItemsHandles = ENTRY(i,xDataTargets).
           ELSE
               xItemsHandles = xItemsHandles + ',' + ENTRY(i,xDataTargets).
         END.
  END.
  {set ItemsHandles xItemshandles}.
  RETURN xItemsHandles.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRowid Procedure 
FUNCTION getRowid RETURNS ROWID
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR xRowid AS CHARACTER NO-UNDO.
  {get RowIdent xRowid}.
  RETURN TO-ROWID(entry(1,xRowid)).

  END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRowidByIndex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRowidByIndex Procedure 
FUNCTION getRowidByIndex RETURNS ROWID
  ( INPUT indice AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR xRowid AS CHARACTER NO-UNDO.
  {get RowIdent xRowid}.
  IF NUM-ENTRIES (xRowid) >= indice THEN
    RETURN TO-ROWID(entry(indice,xRowid)).
  ELSE
    RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isChild) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isChild Procedure 
FUNCTION isChild RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR xDataSources AS CHARACTER NO-UNDO INITIAL ''.

  {get DataSource xDataSources}.
  IF xDataSources <> '' THEN
      RETURN TRUE.

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

