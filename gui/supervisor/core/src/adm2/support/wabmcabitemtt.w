&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 Character ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER vSDOCabecera     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vBCabecera       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vVCabecera       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vSDOitem         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vBitem           AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getChilds wWin 
FUNCTION getChilds RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSDO wWin 
FUNCTION getSDO RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitem AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditem AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pcommit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-3 
     LABEL "&Items" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
     &ELSE SIZE 15 BY 1 &ENDIF.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 65
          &ELSE AT ROW 18 COL 65 &ENDIF
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 19.06
         WIDTH              = 80
         MAX-HEIGHT         = 19.06
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 19.06
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 wWin
ON CHOOSE OF BUTTON-3 IN FRAME fMain /* Items */
DO:
  DEFINE VAR iPAge AS INTEGER NO-UNDO.
  {get currentPage iPage}.
  IF iPAge = 0 THEN
    RUN selectPAge(1).
  ELSE
    RUN selectPage(0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
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
             INPUT  'b.w':U ,
           &ELSE
             INPUT vBcabecera ,
           &ENDIF
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b ).
       RUN repositionObject IN h_b ( 1.00 , 6.00 ) NO-ERROR.
       RUN resizeObject IN h_b ( 7.00 , 66.00 ) NO-ERROR.

       RUN constructObject (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'd.wDB-AWARE':U ,
           &ELSE
             INPUT vSDOCabecera ,
           &ENDIF
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServerno':U ,
             OUTPUT h_d ).
       RUN repositionObject IN h_d ( 1.00 , 70.00 ) NO-ERROR.
       /* Size in AB:  ( 2.00 , 11.00 ) */

       RUN constructObject (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'v.w':U ,
           &ELSE
             INPUT vvCabecera ,
           &ENDIF
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_v ).
       RUN repositionObject IN h_v ( 10.00 , 14.00 ) NO-ERROR.
       /* Size in AB:  ( 5.00 , 49.00 ) */

       /* Initialize other pages that this page requires. */
       RUN initPages ('7') NO-ERROR.

       /* Links to SmartDataBrowser h_b. */
       RUN addLink ( h_d , 'Data':U , h_b ).

       /* Links to SmartDataViewer h_v. */
       RUN addLink ( h_d , 'Data':U , h_v ).
       RUN addLink ( h_v , 'Update':U , h_d ).
       RUN addLink ( h_pupdsav , 'TableIO':U , h_v ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'ditem.wDB-AWARE':U ,
           &ELSE
             INPUT vSDOitem ,
           &ENDIF
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsOrder.CustNum,CustNumRowsToBatch200CheckCurrentChangedyesRebuildOnReposyesServerOperatingModeNONEDestroyStatelessnoDisconnectAppServerno':U ,
             OUTPUT h_ditem ).
       RUN repositionObject IN h_ditem ( 3.00 , 70.00 ) NO-ERROR.
       /* Size in AB:  ( 2.00 , 11.00 ) */

       RUN constructObject (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'bitem.w':U ,
           &ELSE
             INPUT vBitem ,
           &ENDIF
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitem ).
       RUN repositionObject IN h_bitem ( 9.00 , 6.00 ) NO-ERROR.
       RUN resizeObject IN h_bitem ( 7.00 , 66.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('8') NO-ERROR.

       /* Links to SmartDataObject h_ditem. */
       RUN addLink ( h_d , 'Data':U , h_ditem ).
       RUN addLink ( h_pcommit , 'Commit':U , h_ditem ).

       /* Links to SmartDataBrowser h_bitem. */
       RUN addLink ( h_ditem , 'Data':U , h_bitem ).
       RUN addLink ( h_bitem , 'Update':U , h_ditem ).
       RUN addLink ( h_pupdsav-2 , 'TableIO':U , h_bitem ).

    END. /* Page 1 */

    WHEN 7 THEN DO:
       RUN constructObject (
             INPUT  'adm2/pupdsav.r':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionOne-RecordEdgePixels2PanelTypeSaveHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav ).
       RUN repositionObject IN h_pupdsav ( 13.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav ( 1.00 , 78.00 ) NO-ERROR.

    END. /* Page 7 */

    WHEN 8 THEN DO:
       RUN constructObject (
             INPUT  'adm2/pcommit.r':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2PanelTypeCommitHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pcommit ).
       RUN repositionObject IN h_pcommit ( 12.00 , 42.00 ) NO-ERROR.
       RUN resizeObject IN h_pcommit ( 1.00 , 8.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/pupdsav.r':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionMultiple-RecordsEdgePixels2PanelTypeSaveHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav-2 ).
       RUN repositionObject IN h_pupdsav-2 ( 13.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav-2 ( 1.00 , 78.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
    END. /* Page 8 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage wWin 
PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  {adm2/support/changePage.i}

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  ENABLE BUTTON-3 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getQuery wWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSort wWin 
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getChilds wWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSDO wWin 
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

