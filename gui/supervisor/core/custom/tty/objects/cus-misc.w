&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 Character
/* Procedure Description
"This SmartPanel sends navigation messages 
to its NAVIGATION-TARGET. Its buttons have 
icons and are arranged horizontally."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS P-Win 
/*------------------------------------------------------------------------

  File: - p-navico.w

  Description: 

        This is the icon version of the navigation SmartPanel.
        It sends navigation events to its NAVIGATION-TARGET(s),
        such as a SmartQuery.

        By default, the icons are arranged so that the records
        the SmartQuery sends (to wherever) are done in a left
        to right fashion; i.e. pressing the leftmost arrow sends
        record 1 and pressing the rightmost arrow sends the last
        record.

        The attribute Right-To-Left reverses the order in which
        the records are sent; i.e. the leftmost button sends the
        last record and the rightmost button sends the first
        record. This attribute is only available on the icon
        version of the navigation SmartPanel, and is provided to
        satisfy Internationalization requirements.


  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE CSmart-Panel-Misc

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Panel-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-Cons Btn-Print Btn-Exit 

/* Custom List Definitions                                              */
/* Box-Rectangle,List-2,List-3,List-4,List-5,List-6                     */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Cons 
     IMAGE-UP FILE "custom/imagen/grandes/preview":U
     LABEL "&Consultas":L 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
     &ELSE SIZE 12 BY 1 &ENDIF TOOLTIP "Consulta Relacionada".

DEFINE BUTTON Btn-Exit 
     IMAGE-UP FILE "custom/imagen/grandes/cueexit":U
     LABEL "&Salir":L 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
     &ELSE SIZE 12 BY 1 &ENDIF TOOLTIP "Salir".

DEFINE BUTTON Btn-Print 
     IMAGE-UP FILE "custom/imagen/grandes/print":U
     LABEL "&Imprimir":L 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
     &ELSE SIZE 12 BY 1 &ENDIF TOOLTIP "Imprimir".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Panel-Frame
     Btn-Cons
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 2
          &ELSE AT ROW 1 COL 2 &ENDIF
     Btn-Print
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 14
          &ELSE AT ROW 1 COL 14 &ENDIF
     Btn-Exit
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 26
          &ELSE AT ROW 1 COL 26 &ENDIF
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: CSmart-Panel-Misc
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW P-Win ASSIGN
         HEIGHT             = 1.5
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW P-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME Panel-Frame
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME Panel-Frame:SCROLLABLE       = FALSE
       FRAME Panel-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Panel-Frame
/* Query rebuild information for FRAME Panel-Frame
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME Panel-Frame */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB P-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/panel.i}
{custom/method/hcontenedor.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-Cons
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cons P-Win
ON CHOOSE OF Btn-Cons IN FRAME Panel-Frame /* Consultas */
DO:
  define var h as handle no-undo.
  define var cresult as character no-undo.
  run get-link-handle in adm-broker-hdl ( input this-procedure , input 'CONTAINER-SOURCE' , output cresult).  
  h = widget-handle(cresult).
  if valid-handle(h) then
    run consulta in h.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Exit P-Win
ON CHOOSE OF Btn-Exit IN FRAME Panel-Frame /* Salir */
DO:
  define var hcontainer as handle.
  run get-container(output hcontainer).
  run dispatch in hcontainer ('exit').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Print P-Win
ON CHOOSE OF Btn-Print IN FRAME Panel-Frame /* Imprimir */
DO:
  define var h as handle no-undo.
  define var cresult as character no-undo.
  run get-link-handle in adm-broker-hdl ( input this-procedure , input 'CONTAINER-SOURCE' , output cresult).  
  h = widget-handle(cresult).
  if valid-handle(h) then
    run impresion in h.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK P-Win 


/* ***************************  Main Block  *************************** */

  RUN set-attribute-list ("SmartPanelType=Misc-icon,
                           Edge-Pixels=2":U).

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI P-Win _DEFAULT-DISABLE
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
  HIDE FRAME Panel-Frame.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-container P-Win 
PROCEDURE get-container :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter h_container as handle.

define var ccontainer as character no-undo.
define var h as handle no-undo.

run get-link-handle in adm-broker-hdl (this-procedure , 'CONTAINER-SOURCE' , output ccontainer).
h = widget-handle(ccontainer).
if valid-handle (h) then
    h_container = h .
else
    h_container = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable P-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose: The SmartPanel's buttons sensitivities are re-set to whatever
           state they were in when they were disabled. This state is de-
           termined from the variable adm-panel-state.
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch ('enable':U).      /* Get all objects enabled to start. */
  RUN set-buttons (adm-panel-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-buttons P-Win 
PROCEDURE set-buttons :
/*------------------------------------------------------------------------------
  Purpose: Sets sensitivity of panel buttons based on record states.    
  Parameters:  character flag with possible values 'first', 'last',
               'enable-all', or 'disable-all'.
  Notes:  This is invoked from the query object (through new-state) or
          from the included state cases in pnstates.i. The panel itself
          does not make decisions to set its buttons sensitive or 
          insensitive because this would duplicate actions taken when
          state messages are received through the query (this puts the
          panel in the right state even if the first/last/next/prev 
          is initiated by some other object).
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER first-last AS CHARACTER NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed P-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  /*    {src/adm/template/pnstates.i} */
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


