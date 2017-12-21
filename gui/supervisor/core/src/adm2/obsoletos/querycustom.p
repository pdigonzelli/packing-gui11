&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : querycustom.p
    Purpose     : Super procedure to extend query class.

    Syntax      : querycustom.p

    Modified    : 06/03/1999
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ADMSuper querycustom.p

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-assignQuerySelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignQuerySelection Procedure 
FUNCTION assignQuerySelection RETURNS LOGICAL
  (pcColumns   AS CHARACTER,   
   pcValues    AS CHARACTER,    
   pcOperators AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-assignQuerySelectionMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignQuerySelectionMaster Procedure 
FUNCTION assignQuerySelectionMaster RETURNS LOGICAL
  (pcColumns   AS CHARACTER,   
   pcValues    AS CHARACTER,    
   pcOperators AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-openQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD openQuery Procedure 
FUNCTION openQuery RETURNS LOGICAL
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

{src/adm2/qryprop.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-postAssignDbRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postAssignDbRow Procedure 
PROCEDURE postAssignDbRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER phRowObjUpd AS HANDLE NO-UNDO.

DEFINE VAR hObjUpd          AS HANDLE NO-UNDO.
DEFINE VAR hBUffer          AS HANDLE NO-UNDO.
DEFINE VAR xBufferHandles   AS CHARACTER NO-UNDO.
DEFINE VAR hRowMod          AS HANDLE NO-UNDO.
DEFINE VAR fFieldRowObj     AS HANDLE NO-UNDO.

{get RowObjUpd hObjUpd}.
{get BufferHandles xBufferHandles}.
hBuffer = WIDGET-HANDLE(entry(1,xBufferHandles)).
RUN postUpdate IN TARGET-PROCEDURE ( INPUT hBuffer).
RETURN RETURN-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-preAssignDbRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preAssignDbRow Procedure 
PROCEDURE preAssignDbRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER phRowObjUpd AS HANDLE NO-UNDO.

DEFINE VAR hObjUpd          AS HANDLE       NO-UNDO.
DEFINE VAR hBUffer          AS HANDLE       NO-UNDO.
DEFINE VAR xBufferHandles   AS CHARACTER    NO-UNDO.
DEFINE VAR hRowMod          AS HANDLE       NO-UNDO.
DEFINE VAR fFieldRowObj     AS HANDLE       NO-UNDO.
DEFINE VAR xItemsHandles    AS CHARACTER    NO-UNDO.
DEFINE VAR i                AS INTEGER      NO-UNDO.
DEFINE VAR hItem            AS HANDLE       NO-UNDO.
DEFINE VAR lDataModified    AS LOGICAL      NO-UNDO.

{get RowObjUpd hObjUpd}.
{get BufferHandles xBufferHandles}.

hBuffer = WIDGET-HANDLE(entry(1,xBufferHandles)).
hRowMod = hObjUpd:BUFFER-FIELD('RowMod').

IF  hRowMod:BUFFER-VALUE = 'A' OR hRowMod:BUFFER-VALUE = 'C' THEN
    RUN preCreate IN TARGET-PROCEDURE ( INPUT phRowObjUpd).
ELSE
    RUN preUpdate IN TARGET-PROCEDURE ( INPUT hBuffer).
RETURN RETURN-VALUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-assignQuerySelection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignQuerySelection Procedure 
FUNCTION assignQuerySelection RETURNS LOGICAL
  (pcColumns   AS CHARACTER,   
   pcValues    AS CHARACTER,    
   pcOperators AS CHARACTER):
/*------------------------------------------------------------------------------
  Purpose:  invocar el procedimiento custom 'afterFilter' del container para 
            capturar el evento buscar del smart filter.
    Notes:  
------------------------------------------------------------------------------*/
   SUPER (pcColumns, pcValues, pcOperators).
  

/*by facundo 25/03/2004*/    
  DEFINE VARIABLE hFilterSource    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hFilterContainer AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vcColumns        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcValues         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcOperators      AS CHARACTER  NO-UNDO.
  
  {get FilterSource hFilterSource}.
   
  IF VALID-HANDLE(hFilterSource) THEN DO:
    ASSIGN vcColumns   = pcColumns
           vcValues    = pcValues
           vcOperators = pcOperators.
    {get ContainerSource hFilterContainer hFilterSource}.
    RUN afterFilter IN hFilterContainer (INPUT-OUTPUT pcColumns, 
                                         INPUT-OUTPUT pcValues, 
                                         INPUT-OUTPUT pcOperators).
    /*si los parametros cambiaron ejecuto de nuevo el super con los nuevos valores de los parametros.*/
    IF vcColumns <> pcColumns OR vcValues <> pcValues OR vcOperators <> pcOperators THEN DO:
      SUPER (pcColumns, pcValues, pcOperators).
    END.

  END.
  /*end by facundo*/


  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-assignQuerySelectionMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignQuerySelectionMaster Procedure 
FUNCTION assignQuerySelectionMaster RETURNS LOGICAL
  (pcColumns   AS CHARACTER,   
   pcValues    AS CHARACTER,    
   pcOperators AS CHARACTER):
/*------------------------------------------------------------------------------   
   Purpose: Assigns selection criteria to the query and distributes the 
            column/value pairs to the corresponding buffer's where-clause. 
            Each buffer's expression will always be embedded in parenthesis.
   Parameters: 
     pcColumns   - Column names (Comma separated) 
                   
                   Fieldname of a table in the query in the form of 
                   TBL.FLDNM or DB.TBL.FLDNM (only if qualified with db is specified),
                   (RowObject.FLDNM should be used for SDO's)  
                   If the fieldname isn't qualified it checks the tables in 
                   the TABLES property and assumes the first with a match.
                   
     pcValues    - corresponding Values (CHR(1) separated)
     pcOperators - Operator - one for all columns
                              - blank - defaults to (EQ)  
                              - Use slash to define alternative string operator
                                EQ/BEGINS etc..
                            - comma separated for each column/value       
   Notes:   This procedure is designed to run on the client and to be 
            called several times to build up the the query's where clause 
            (storing intermediate results in the QueryString property) before 
            it is finally used in a Query-Prepare method. 
            openQuery takes care of the preparation of the QueryString property.
            The QueryColumns property is used to ensure that each column and 
            operator only will be added once to the QueryString. The property is 
            also used to store the offset and length of the corresponding values.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cQueryString   AS CHARACTER NO-UNDO.
    
  DEFINE VARIABLE cBufferList    AS CHAR      NO-UNDO.
  DEFINE VARIABLE cBuffer        AS CHARACTER NO-UNDO.
  
  /* We need the columns name and the parts */  
  DEFINE VARIABLE cColumn        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColumnName    AS CHARACTER NO-UNDO.
    
  DEFINE VARIABLE iBuffer        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iColumn        AS INTEGER   NO-UNDO.
  
  DEFINE VARIABLE cUsedNums      AS CHAR      NO-UNDO.
  
  /* Used to builds the column/value string expression */
  DEFINE VARIABLE cBufWhere      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDataType      AS CHAR      NO-UNDO.
  DEFINE VARIABLE cQuote         AS CHAR      NO-UNDO.    
  DEFINE VARIABLE cValue         AS CHAR      NO-UNDO.  
  DEFINE VARIABLE cOperator      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cStringOp      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAndOr         AS CHAR      NO-UNDO.
       
  /* Used to store and maintain offset and length */    
  DEFINE VARIABLE iValLength     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iValPos        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iExpPos        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iPos           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iDiff          AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cQueryColumns  AS CHAR      NO-UNDO.
  DEFINE VARIABLE cQueryBufCols  AS CHAR      NO-UNDO.
  DEFINE VARIABLE cQueryColOp    AS CHAR      NO-UNDO.
  DEFINE VARIABLE cChangedValues AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cChangedList   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iOldEntries    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iLowestChanged AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cBufferDiffs   AS CHAR      NO-UNDO.
  DEFINE VARIABLE iBufPos        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iColPos        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iWhereBufPos   AS INTEGER   NO-UNDO.
          
  {get Tables cBufferList}.    

  /******************************************************************/
  /*by facundo 25/03/2004*/    
  DEFINE VARIABLE hFilterSource    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hFilterContainer AS HANDLE     NO-UNDO.
  
  {get FilterSource hFilterSource}.
   
  IF VALID-HANDLE(hFilterSource) THEN DO:
    {get ContainerSource hFilterContainer hFilterSource}.
    RUN afterFilter IN hFilterContainer (INPUT-OUTPUT pcColumns, 
                                         INPUT-OUTPUT pcValues, 
                                         INPUT-OUTPUT pcOperators).
  END.
  /*end by facundo*/
  /********************************************************************/
   
  /* The QueryString contains data if the query is being currently worked on 
     by this method or addQuerywhere over many calls. */
  {get QueryString cQueryString}.      
  /* If no QueryString find the current query */ 
  IF cQueryString = "":U OR cQueryString = ? THEN
  DO:
    {get QueryWhere cQueryString}.    
     /* If no current Query find the defined base query */ 
     IF cQueryString = "":U OR cQueryString = ? THEN
       {get OpenQuery cQueryString}.       
  END. /* cQueryString = "":U */
  IF cQueryString = "":U OR cQueryString = ? THEN
    RETURN FALSE.

  {get QueryColumns cQueryColumns}.
  /* cQueryColumns has the form of:
        BufName1:columns_of_buf1:BufName2:columns_of_buf2...
      
        Each columns_of_buf has the form of:
        ColumnName.Operator,ValuePosition,ValueLength
        
        The Operator is one of: ">=", "<=","<", ">", "=", "BEGINS", etc.
        The ValuePosition refers to the character position of the value
        in an expression: ColumnName Opr Value
        (The quote is considered part of the value)
        The length of the value is the number of characters in the string
        that represents the value (including quotes) */
  
  ASSIGN 
    cAndOr       = "AND":U /* We only support and */
    cBufferDiffs = LEFT-TRIM(FILL(",0",NUM-ENTRIES(cBufferList)),","). 
  
  DO iBuffer = 1 TO NUM-ENTRIES(cBufferList):  
    ASSIGN
      cBufWhere      = "":U
      cBuffer        = ENTRY(iBuffer,cBufferList)
      iBufPos        = LOOKUP(cBuffer,cQueryColumns,":":U)
      cQueryBufCols  = IF iBufPos > 0 
                       THEN ENTRY(iBufPos + 1,cQueryColumns,":":U) 
                       ELSE "":U
      iOldEntries    = NUM-ENTRIES(cQueryBufCols) / 3    
      cChangedValues = FILL(CHR(1),iOldEntries - 1)
      cChangedList   = "":U
      iLowestChanged = 0.
      
    ColumnLoop:    
    DO iColumn = 1 TO NUM-ENTRIES(pcColumns):
             
      IF CAN-DO(cUsedNums,STRING(iColumn)) THEN 
        NEXT ColumnLoop.      
        
      cColumn     = ENTRY(iColumn,pcColumns).
      
      /* Convert rowObject reference to db reference */
      IF cColumn BEGINS "RowObject" + "." THEN        
        cColumn =  DYNAMIC-FUNCTION("columnDBColumn":U IN TARGET-PROCEDURE,
                                    ENTRY(2,cColumn,".":U)) NO-ERROR.                                      
      
      /* Unqualified fields will use the first buffer that has a match
         because the columnDataType below searches all buffers in the query */           
      ELSE IF INDEX(cColumn,".":U) = 0 THEN       
        cColumn = cBuffer + ".":U + cColumn.
      
      /* Wrong buffer? */
      IF NOT (cColumn BEGINS cBuffer + ".":U) THEN 
      DO: 
        /* If the column db qualification does not match the query's we do 
           an additionl check to see if it is the correct table after all */                                
        IF NUM-ENTRIES(cColumn,".":U) - 1 <> NUM-ENTRIES(cBuffer,".":U) THEN
        DO:
          IF {fnarg columnTable cColumn} <> cBuffer THEN 
            NEXT ColumnLoop.  
        END.
        ELSE
          NEXT ColumnLoop.
      END.

      ASSIGN
        /* Get the operator for this valuelist. 
           Be forgiving and make sure we handle '',? and '/begins' as default */                                                  
        cOperator   = IF pcOperators = "":U 
                      OR pcOperators BEGINS "/":U 
                      OR pcOperators = ?                       
                      THEN "=":U 
                      ELSE IF NUM-ENTRIES(pcOperators) = 1 
                           THEN ENTRY(1,pcOperators,"/":U)                                                 
                           ELSE ENTRY(iColumn,pcOperators)
                                           
        /* Look for optional string operator if only one entry in operator */          
        cStringOp   = IF NUM-ENTRIES(pcOperators) = 1 
                      AND NUM-ENTRIES(pcOperators,"/":U) = 2  
                      THEN ENTRY(2,pcOperators,"/":U)                                                 
                      ELSE cOperator                    
        cColumnName = ENTRY(NUM-ENTRIES(cColumn,".":U),cColumn,".":U)              
        cDataType   = {fnarg columnDataType cColumn}.

      IF cDataType <> ? THEN
      DO:
        ASSIGN          
          cValue     = ENTRY(iColumn,pcValues,CHR(1))                         
          cValue     = IF CAN-DO("INTEGER,DECIMAL":U,cDataType) AND cValue = "":U 
                       THEN "0":U 
                       ELSE IF cDataType = "DATE":U and cValue = "":U
                       THEN "?":U 
                       ELSE IF cValue = ? /*This could happen if only one value*/
                       THEN "?":U 
                       ELSE cValue
          
          cValue     = (IF cValue <> "":U 
                        THEN REPLACE(cValue,"'","~~~'")
                        ELSE " ":U)  
         
          /* We are quoting ALL values to ensures that decimals behave 
             in both american and european format. 
             This works also with ? as the dynamic query handles quoted unknown 
             values as unknown for all data-types except character of course */ 
          cQuote     = (IF cDataType = "CHARACTER":U AND cValue = "?" 
                        THEN "":U 
                        ELSE "'":U)   

          cUsedNums  = cUsedNums
                      + (IF cUsedNums = "":U THEN "":U ELSE ",":U)
                      + STRING(iColumn) 
          
          /* The Column and operator are unique entries so we must mak sure that  
             that blank or different styles doesn't get misinterpreted  */
          cQueryColOp = cOperator
          cQueryColOp = cStringOp WHEN cDataType = "CHARACTER"            
          cQueryColOp = TRIM(     IF cQueryColOp = "GE":U THEN ">=":U
                             ELSE IF cQueryColOp = "LE":U THEN "<=":U
                             ELSE IF cQueryColOp = "LT":U THEN "<":U
                             ELSE IF cQueryColOp = "GT":U THEN ">":U
                             ELSE IF cQueryColOp = "EQ":U THEN "=":U
                             ELSE    cQueryColOp)
                        
          /* Have the column and operator been added to the querystring
             (by this function) */  
          iPos        = LOOKUP(cColumnName + ".":U + cQueryColOp,cQueryBufCols)
          
        /* From 9.1B the quotes are included in the value to avoid problems
           when replacing unquoted ? to a quoted value */ 
          cValue      = cQuote + cValue + cQuote.
      
        /* If the column + operator was found in the list
           we build a list of the new values to use when we insert the data
           into the QueryString further down.
           We also build a list of the changed numbers, to check if any change 
           has occured. (The list of new values cannot be checked because any 
           data may be new data and we don't know the old value) */         
        IF iPos > 0 THEN
        DO:        
          ASSIGN
            ENTRY(INT((iPos - 1) / 3 + 1),cChangedValues,CHR(1)) = cValue    
            iLowestChanged = MIN(iPos,IF iLowestChanged = 0 
                                      THEN iPos 
                                      ELSE iLowestChanged)
            cChangedList  = cChangedList 
                          + (IF cChangedList = "":U THEN "":U ELSE ",":U)
                          + STRING(INT((iPos - 1) / 3 + 1)).     
        END. /* IF ipos > 0 */   
        ELSE DO: /* This is a new column + operator so we build the new 
                    expression and add the column and offset info to the list 
                    that will be stored as a part of QueryColumns */   
          ASSIGN          
            cBufWhere  = cBufWhere 
                      + (If cBufWhere = "":U 
                         THEN "":U 
                         ELSE " ":U + cAndOr + " ":U)
                      + cColumn 
                      + " ":U
                      + (IF cDataType = "CHARACTER":U  
                         THEN cStringOp
                         ELSE cOperator)
                      + " ":U
                      + cValue
                                             
           /* Calculate the temporary offset of this columns value. 
              We (Who are we?) will justify it after the expression has been 
              added to the whereclause, because even if we know the buffer's 
              position, the expression may or may not need and/where */
           iValPos   = LENGTH(cBufWhere) - LENGTH(cValue)         
                    
           /* Store the ColumName and operator with period as delimiter and 
              add the position and length as separate entries*/
           cQueryBufCols = cQueryBufCols 
                       + (IF cQueryBufCols <> "":U THEN ",":U ELSE "":U)
                       + cColumnName 
                       + ".":U 
                       + cQueryColOp 
                       + ",":U
                       + STRING(iValPos)  
                       + ",":U
                       + STRING(LENGTH(cValue))
           
           /* Ensure that the list used to log changes have correct number of
              entries (Probably only necessary if the SAME column and operator
                       appears a second time in the same call, which is unlikely)
                       */                
           cChangedValues = cChangedValues + CHR(1).                
        END. /* else do =(ipos = 0) */
      END. /* if cDatatype <> ? */          
    END. /* do iColumn = 1 to num-entries(pColumns) */  
    
    /* Get the buffers position in the where clause (always the
       first entry in a dynamic query because there's no 'of <external>')*/ 
    ASSIGN
      iWhereBufPos = INDEX(cQueryString + " "," ":U + cBuffer + " ":U)
      iPos         = INDEX(cQueryString,      " ":U + cBuffer + ",":U)
      iWhereBufPos = (IF iWhereBufPos > 0 AND iPos > 0
                      THEN MIN(iPos,iWhereBufPos) 
                      ELSE MAX(iPos,iWhereBufPos))
                      + 1
      iDiff        = 0.                          

    /* We have a new expression */                               
    IF cBufWhere <> "":U THEN
    DO: 
      
      ASSIGN 
        cQueryString = DYNAMIC-FUNCTION('newWhereClause':U IN TARGET-PROCEDURE,
                                         cBuffer,
                                         cBufWhere,
                                         cQueryString,
                                         'AND':U) 
        /* get the offset of the new expression */
        iExpPos      = INDEX(cQuerystring,cBufwhere,iWhereBufPos).
      
      /* Store the offset from the buffer's offset */  
      DO iColumn =((iOldEntries + 1) * 3) - 2 TO NUM-ENTRIES(cQueryBufCols) BY 3:
        ENTRY(iColumn + 1,cQueryBufCols) = 
                            STRING(INT(ENTRY(iColumn + 1,cQueryBufCols)) 
                                   + (iExpPos - iWhereBufPos)
                                   ).                 
      END. /* do icolumn = 1 to num-entries */        
    END. /* if cbufwhere <> '' do */  
    
    IF iLowestChanged > 0 THEN 
    DO iColumn = iLowestChanged TO NUM-ENTRIES(cQueryBufCols) BY 3:       
      ASSIGN
        iValPos    = INT(ENTRY(iColumn + 1,cQueryBufCols))
        iValLength = INT(ENTRY(iColumn + 2,cQueryBufCols))
        iValPos    = iValPos + iDiff.                    
                     
      IF CAN-DO(cChangedList,STRING(INT((iColumn - 1) / 3 + 1))) THEN       
      DO:
        ASSIGN
          cValue     = ENTRY(INT((iColumn - 1) / 3 + 1),cChangedValues,CHR(1)) 
          SUBSTR(cQueryString,iValPos + iWhereBufPos,iValLength) = cValue
          idiff      = iDiff + (LENGTH(cValue) - iValLength)
          iValLength = LENGTH(cValue).   
      END. /* can-do(changelist,string(..) */          
      ASSIGN      
        ENTRY(iColumn + 1,cQueryBufCols) = STRING(iValPos)
        ENTRY(iColumn + 2,cQueryBufCols) = STRING(iVallength).      
    END. /* else if ilowestchanged do icolumn = ilowestChanged to num-entries */  
    
    /* If the buffer has no entry in QueryColumns we append the new entry 
       The order in Querycolumns is NOT dependent of the order in the query */              
    IF cQueryBufCols <> "":U THEN
    DO:    
      IF iBufPos = 0 THEN   
         cQueryColumns = cQueryColumns 
                         + (IF cQueryColumns = "":U THEN "":U ELSE ":":U)
                         + cBuffer + ":" + cQueryBufCols.
      
      ELSE /* There is already a entry for this buffer */
        ENTRY(iBufPos + 1,cQueryColumns,":":U) = cQueryBufCols.        
    END. /* cQueryBufCols <> '' */

  END. /* do iBuffer = 1 to hQuery:num-buffers */
  
  {set QueryColumns cQueryColumns}.
  {set QueryString cQueryString}.



  RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-openQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION openQuery Procedure 
FUNCTION openQuery RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  SUPER().

  /*by facundo 25/03/2004*/    
  DEFINE VARIABLE hFilterSource    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hFilterContainer AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vcColumns        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcValues         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcOperators      AS CHARACTER  NO-UNDO.
  
  {get FilterSource hFilterSource}.
   
  IF VALID-HANDLE(hFilterSource) THEN DO:
    {get ContainerSource hFilterContainer hFilterSource}.
    RUN postOpenQuery IN hFilterContainer .

  END.
  /*end by facundo*/

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

