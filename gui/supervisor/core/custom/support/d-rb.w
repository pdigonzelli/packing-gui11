&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&GLOBAL-DEFINE WIN95-BTN YES
/* Parameters Definitions ---                                           */

define input parameter p_hSmo as handle no-undo.
/* Local Variable Definitions ---                                       */

define var attr-list as character no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS f-Rbdbs f-Rblib BUTTON-5 f-Rbrep f-Rbio ~
f-Rbcop f-Rbinc f-rbotr 
&Scoped-Define DISPLAYED-OBJECTS f-Rbdbs f-Rblib f-Rbrep f-Rbio f-Rbcop ~
f-Rbinc f-rbotr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-5 
     LABEL "&Files" 
     SIZE 15 BY 1.

DEFINE VARIABLE f-Rbrep AS CHARACTER FORMAT "X(256)":U 
     LABEL "Reporte" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","" 
     SIZE 61 BY 1.

DEFINE VARIABLE f-Rbcop AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cantidad de Copias" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE f-Rbdbs AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dbs" 
     VIEW-AS FILL-IN 
     SIZE 97.6 BY 1 NO-UNDO.

DEFINE VARIABLE f-Rblib AS CHARACTER FORMAT "X(256)":U 
     LABEL "Libreria" 
     VIEW-AS FILL-IN 
     SIZE 58.4 BY 1 NO-UNDO.

DEFINE VARIABLE f-rbotr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Otros Parametros" 
     VIEW-AS FILL-IN 
     SIZE 76 BY 1 NO-UNDO.

DEFINE VARIABLE f-Rbinc AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Use el filtro Grabado", 1,
"Reemplaze el filtro grabado", 2,
"Incluya todos los registros", 3,
"Pregunte en run-time", 4
     SIZE 34 BY 3.14.

DEFINE VARIABLE f-Rbio AS LOGICAL INITIAL no 
     LABEL "Pantalla/Impresora" 
     VIEW-AS TOGGLE-BOX
     SIZE 27.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     f-Rbdbs AT ROW 1.86 COL 7.6 COLON-ALIGNED
     f-Rblib AT ROW 3 COL 7.8 COLON-ALIGNED
     BUTTON-5 AT ROW 3 COL 68.8
     f-Rbrep AT ROW 4.14 COL 7.8 COLON-ALIGNED
     f-Rbio AT ROW 5.38 COL 10.2
     f-Rbcop AT ROW 5.38 COL 59.2 COLON-ALIGNED
     f-Rbinc AT ROW 6.81 COL 9.8 NO-LABEL
     f-rbotr AT ROW 10.24 COL 16.4 COLON-ALIGNED
     SPACE(15.59) SKIP(1.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Propiedades del Reporte".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON GO OF FRAME D-Dialog /* Propiedades del Reporte */
DO:
  define var cInclude as character no-undo.
  cInclude   = if      (f-Rbinc:screen-value = "1") then "S" 
               else if (f-Rbinc:screen-value = "2") then "O" 
               else if (f-Rbinc:screen-value = "3") then "E" 
               else "?".

  attr-list = 'Rbdbs='  + f-Rbdbs:screen-value + 
              ',Rblib=' + f-Rblib:screen-value +
              ',RbRep=' + f-RbRep:screen-value +
              ',Rbinc=' + cInclude +
              ',Rbcop=' + f-Rbcop:screen-value +
              ',Rbotr=' + f-Rbotr:screen-value +
              ',Rbio='  + if f-Rbio:screen-value = 'yes' then 'D' else '' .
  

  RUN set-attribute-list IN p_hSMO (INPUT attr-list).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Propiedades del Reporte */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 D-Dialog
ON CHOOSE OF BUTTON-5 IN FRAME D-Dialog /* Files */
DO:
  def var name    as char    no-undo.
  def var bPicked as logical no-undo.

  system-dialog get-file
      name
      filters           "*.prl" "*.prl"  /* Filter                */
      default-extension "*.prl"        /* default-extensions     */
      title             "Report Library"
      must-exist
      update bPicked.

  if bPicked then
    f-Rblib:screen-value = name. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-Rbio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-Rbio D-Dialog
ON VALUE-CHANGED OF f-Rbio IN FRAME D-Dialog /* Pantalla/Impresora */
DO:
  if ( self:screen-value = "D" ) then
        ( f-Rbcop:sensitive = true ).
  else 
        ( f-Rbcop:sensitive = false).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-Rbrep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-Rbrep D-Dialog
ON ENTRY OF f-Rbrep IN FRAME D-Dialog /* Reporte */
DO:
  run fill-report-list.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Define Context ID's for HELP files */
{ src/adm/support/admhlp.i }    

/* Attach the standard OK/Cancel/Help button bar. */
{ adecomm/okbar.i  &TOOL = "UIB"
                   &CONTEXT = {&SmartBrowser_Attributes_Dlg_Box} }

/* ***************************  Main Block  *************************** */

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  /* Get the values of the attributes in the SmartObject that can be 
     changed in this dialog-box. */
  RUN get-SmO-attributes.
  /* Enable the interface. */         
  RUN enable_UI.
  /* Set the cursor */
  RUN adecomm/_setcurs.p ("":U).
 
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.  
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog _DEFAULT-ENABLE
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
  DISPLAY f-Rbdbs f-Rblib f-Rbrep f-Rbio f-Rbcop f-Rbinc f-rbotr 
      WITH FRAME D-Dialog.
  ENABLE f-Rbdbs f-Rblib BUTTON-5 f-Rbrep f-Rbio f-Rbcop f-Rbinc f-rbotr 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fill-report-list D-Dialog 
PROCEDURE fill-report-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  def var cName  as char    no-undo.
  def var iCount as integer no-undo.
  
  run aderb/_getname.p (f-Rblib:screen-value in frame {&FRAME-NAME},
                        output cName, 
                        output iCount).
  
  assign
    f-Rbrep:sensitive    = yes
    f-Rbrep:list-items   = cName
    f-Rbrep:screen-value = ENTRY (1,cName). 
 END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-SmO-attributes D-Dialog 
PROCEDURE get-SmO-attributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ldummy AS LOGICAL   NO-UNDO.


  DO /* WITH FRAME {&FRAME-NAME} */:   
    /* Get the attributes used in this Instance Attribute dialog-box. */
    RUN get-attribute IN p_hSMO ("Rbdbs":U).
    f-rbdbs = RETURN-VALUE.
    RUN get-attribute IN p_hSMO ("Rblib":U).
    f-rblib = RETURN-VALUE.
    RUN get-attribute IN p_hSMO ("Rbrep":U).
    f-rbrep = RETURN-VALUE.
    RUN get-attribute IN p_hSMO ("Rbio":U).
    f-rbio = IF RETURN-VALUE = 'D' then true else false.
    RUN get-attribute IN p_hSMO ("Rbinc":U).
    CASE RETURN-VALUE :
        WHEN 'E' THEN
            f-Rbinc = 3.
        WHEN '?' THEN
            f-Rbinc = 4.
        WHEN 'O' THEN
            f-Rbinc = 2.
        WHEN 'S' THEN
            f-Rbinc = 1.
    END.
    RUN get-attribute IN p_hSMO ("Rbrep":U).
    f-Rbrep = RETURN-VALUE.

    RUN get-attribute IN p_hSMO ("Rbcop":U).
    f-Rbcop = integer(RETURN-VALUE).

    RUN get-attribute IN p_hSMO ("Rbotr":U).
    f-Rbotr = RETURN-VALUE.

  END. /* DO WITH FRAME... */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


