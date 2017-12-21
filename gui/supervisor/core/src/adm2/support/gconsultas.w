&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER xsBrowse      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER xsSDO         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER xFieldList    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER cQuery        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER xFieldResult  AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel Btn_Help RECT-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getChilds gDialog 
FUNCTION getChilds RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSDO gDialog 
FUNCTION getSDO RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD queryName gDialog 
FUNCTION queryName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bconsultas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dconsultas AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 48.4 BY 1.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     Btn_OK AT ROW 20.33 COL 42
     Btn_Cancel AT ROW 20.33 COL 57.6
     Btn_Help AT ROW 20.33 COL 73.8
     RECT-2 AT ROW 20.05 COL 41
     SPACE(40.19) SKIP(0.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Consulta"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
                                                                        */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Consulta */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help gDialog
ON CHOOSE OF Btn_Help IN FRAME gDialog /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
MESSAGE "Help for File: {&FILE-NAME}":U VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
DO:
    DEFINE VAR xResult  AS CHARACTER NO-UNDO.
    DEFINE VAR i        AS INTEGER   NO-UNDO.

    xResult = DYNAMIC-FUNCTION('colValues' IN widget-handle(getSdo()) , xFieldList) NO-ERROR.
    DO i = 1 TO NUM-ENTRIES(xFieldList):
        IF xFieldResult = '' THEN
            xFieldResult = ENTRY( i + 1 ,xResult,CHR(1)).
        ELSE
            xFieldResult = xFieldResult + ',' + ENTRY( i + 1 ,xResult,CHR(1)).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'm:/src/adm2/support/dconsultas.wDB-AWARE':U ,
           &ELSE
             INPUT xsSDo ,
           &ENDIF
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedconsultas':U ,
             OUTPUT h_dconsultas ).
       RUN repositionObject IN h_dconsultas ( 19.81 , 95.00 ) NO-ERROR.
       /* Size in AB:  ( 2.00 , 11.60 ) */

       RUN constructObject (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'm:/src/adm2/support/bconsultas.w':U ,
           &ELSE
             INPUT xsBrowse ,
           &ENDIF
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bconsultas ).
       RUN repositionObject IN h_bconsultas ( 1.57 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_bconsultas ( 18.00 , 121.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bconsultas. */
       RUN addLink ( h_dconsultas , 'Data':U , h_bconsultas ).
       RUN addLink ( h_bconsultas , 'Update':U , h_dconsultas ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bconsultas ,
             Btn_OK:HANDLE , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE constructObject gDialog 
PROCEDURE constructObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT  PARAMETER pcProcName AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER phParent   AS HANDLE NO-UNDO.
  DEFINE INPUT  PARAMETER pcPropList AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER phObject   AS HANDLE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(SESSION:LAST-SERVER) AND pcProcName = xsSdo THEN
  DO:
      pcProcName = pcProcName + CHR(3) + 'DB-AWARE'.
  END.
  RUN SUPER( INPUT pcProcName, INPUT phParent, INPUT pcPropList, OUTPUT phObject).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableObject gDialog 
PROCEDURE enableObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR xQuery AS CHARACTER.
  DEFINE VAR cTables AS CHARACTER.
  
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */


  IF cQuery <> "" THEN
  DO:
     {get Tables cTables h_dconsultas}.

     CASE NUM-ENTRIES(cTables):
         WHEN 1 THEN
           xQuery = 'for each ' + entry(1,cTables) +  ' where ' + cQuery.
         WHEN 2 THEN
           xQuery = 'for each ' + entry(1,cTables) +  ', each ' +
                     ENTRY(2,cTables) + ' of ' + entry(1,cTables) + ' where ' + cQuery.
         WHEN 3 THEN
           xQuery = 'for each ' + entry(1,cTables) +  ', each ' +
                     ENTRY(2,cTables) + ' of ' + entry(1,cTables) +   ', each ' +
                     ENTRY(3,cTables) + ' of ' + entry(1,cTables) + 
                     ' where ' + cQuery.
     END CASE.

     {set queryWhere xQuery h_dconsultas}.
      DYNAMIC-FUNCTION ('openQuery':U IN h_dconsultas).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE Btn_OK Btn_Cancel Btn_Help RECT-2 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getQuery gDialog 
PROCEDURE getQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER queryText AS CHARACTER NO-UNDO.
    queryText = queryText1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSort gDialog 
PROCEDURE getSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER querySort AS CHARACTER NO-UNDO.
    querySort = querySort1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF valid-handle(SESSION:LAST-SERVER) THEN
  DO:
      xsSdo = xsSdo + CHR(3) + 'DB-AWARE'.
  END.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSort gDialog 
PROCEDURE setSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER xSort AS CHARACTER NO-UNDO.

querySort1 = xSort.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getChilds gDialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSDO gDialog 
FUNCTION getSDO RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION queryName gDialog 
FUNCTION queryName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN cQuery.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

