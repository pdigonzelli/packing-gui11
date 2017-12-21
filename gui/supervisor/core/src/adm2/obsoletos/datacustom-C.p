&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12
&ANALYZE-RESUME
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowid Procedure  _DB-REQUIRED
FUNCTION getRowid RETURNS ROWID
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRowidByIndex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowidByIndex Procedure  _DB-REQUIRED
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
  
  RUN preAssignDbRow IN TARGET-PROCEDURE( INPUT phRowObjUpd ).
  IF RETURN-VALUE <> ''  THEN RETURN RETURN-VALUE.
  
  RUN especialPreSection (INPUT phRowObjUpd, 'x' , TARGET-PROCEDURE).
  IF RETURN-VALUE <> ''  THEN RETURN RETURN-VALUE.
  
  RUN SUPER( INPUT phRowObjUpd).
  IF RETURN-VALUE <> ''  THEN RETURN RETURN-VALUE.
  
  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN postAssignDbRow IN TARGET-PROCEDURE( INPUT phRowObjUpd).
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
RUN especialPosUpdate IN hProcedure (hBuffer , xCase).
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
    RUN especialPreCreate IN hProcedure (xCase).
ELSE
    RUN especialPreUpdate IN hProcedure (hBUffer , xCase).

RETURN RETURN-VALUE.


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
  RETURN TO-ROWID(entry(indice,xRowid)).

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

