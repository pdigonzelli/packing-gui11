&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME f-dlg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS f-dlg 
/*------------------------------------------------------------------------

  File: keyedit.w

  Description: A editor for Accepted and Supplied keys

  Input Parameters:
      p_context - Context of the XFTR code section
      
  Input-Output parameters:
      p_code    - The code to update
      
  Output Parameters:
      <none>

  Author: Wm.T.Wood 

  Created: December 22, 1995
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&GLOBAL-DEFINE WIN95-BTN YES

&IF "{&UIB_is_Running}" eq "" &THEN
  DEFINE INPUT        PARAMETER p_context AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER p_code    AS CHAR NO-UNDO.
&ELSE
  DEFINE VARIABLE p_context AS INTEGER NO-UNDO.
  DEFINE VARIABLE p_code    AS CHAR NO-UNDO.
  /* Testing code */
  p_code =
"/* STRUCTURED-DATA
<FOREIGN-KEYS>
Cust-Num|y|y|Sports.Customer.Cust-Num" + CHR(10) +
"Sales-Rep||y|Sports.Customer.Sales-Rep
</FOREIGN-KEYS> */".

&ENDIF

/* Local Variable Definitions ---                                       */
DEFINE VAR ch              AS CHAR NO-UNDO.
DEFINE VAR cnt             AS INTEGER NO-UNDO.
DEFINE VAR foreign-keys    AS CHAR NO-UNDO.
DEFINE VAR i               AS INTEGER NO-UNDO.
DEFINE VAR none-accepted   AS LOGICAL NO-UNDO.
DEFINE VAR key-object      AS CHAR NO-UNDO.  
DEFINE VAR key-object-type AS CHAR NO-UNDO.  
DEFINE VAR key-table       AS CHAR NO-UNDO. 
DEFINE VAR ldummy          AS LOGICAL NO-UNDO.
DEFINE VAR open-recid      AS RECID   NO-UNDO.
DEFINE VAR open-on-row     AS INTEGER NO-UNDO.
DEFINE VAR proc-ID         AS INTEGER NO-UNDO.
DEFINE VAR resultado       AS CHARACTER NO-UNDO.
DEFINE VAR srecid          AS INTEGER NO-UNDO.
DEFINE VAR cresult         AS CHARACTER NO-UNDO.
DEFINE VAR procid          AS INTEGER NO-UNDO.
/* Temp-Table for browsing keys ---                                     */
DEFINE TEMP-TABLE tt NO-UNDO
  FIELD num AS INTEGER
  FIELD db-field AS CHAR
  FIELD related AS LOGICAL FORMAT "yes/no" INITIAL no
  FIELD program AS char
  FIELD related-db-field AS CHAR 
  INDEX num IS PRIMARY num
  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-dlg
&Scoped-define BROWSE-NAME brws-keys

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt

/* Definitions for BROWSE brws-keys                                     */
&Scoped-define FIELDS-IN-QUERY-brws-keys tt.db-field tt.related tt.program tt.related-db-field   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brws-keys all   
&Scoped-define FIELD-PAIRS-IN-QUERY-brws-keys~
 ~{&FP1}all ~{&FP2}all ~{&FP3}
&Scoped-define SELF-NAME brws-keys
&Scoped-define OPEN-QUERY-brws-keys OPEN QUERY brws-keys PRESELECT EACH tt.
&Scoped-define TABLES-IN-QUERY-brws-keys tt
&Scoped-define FIRST-TABLE-IN-QUERY-brws-keys tt


/* Definitions for DIALOG-BOX f-dlg                                     */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brws-keys b_Insert BUTTON-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 AUTO-GO 
     LABEL "&Salir" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b_Insert 
     LABEL "&Insert Key" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b_Remove 
     LABEL "&Remove Key" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brws-keys FOR 
      tt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brws-keys
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brws-keys f-dlg _FREEFORM
  QUERY brws-keys NO-LOCK DISPLAY
      tt.db-field  FORMAT "X(256)" WIDTH 30 LABEL "Campo"
      tt.related  WIDTH 4 LABEL "Rel."
      tt.program  format "x(100)" WIDTH 20 LABEL "Programa"
      tt.related-db-field FORMAT "X(128)" WIDTH 27 LABEL "Campo asociado"   
ENABLE all
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 59 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-dlg
     brws-keys AT ROW 1.29 COL 2
     b_Insert AT ROW 1.29 COL 62
     b_Remove AT ROW 2.62 COL 62
     BUTTON-1 AT ROW 4.1 COL 62
     SPACE(0.00) SKIP(4.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Foreign Keys"
         DEFAULT-BUTTON BUTTON-1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX f-dlg
                                                                        */
/* BROWSE-TAB brws-keys 1 f-dlg */
ASSIGN 
       FRAME f-dlg:SCROLLABLE       = FALSE.

ASSIGN 
       brws-keys:NUM-LOCKED-COLUMNS IN FRAME f-dlg = 1
       brws-keys:MAX-DATA-GUESS IN FRAME f-dlg     = 12.

/* SETTINGS FOR BUTTON b_Remove IN FRAME f-dlg
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brws-keys
/* Query rebuild information for BROWSE brws-keys
     _START_FREEFORM
OPEN QUERY brws-keys PRESELECT EACH tt.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE brws-keys */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB f-dlg 
/* ************************* Included-Libraries *********************** */

{src/adm/support/keyprocs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME f-dlg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-dlg f-dlg
ON WINDOW-CLOSE OF FRAME f-dlg /* Foreign Keys */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brws-keys
&Scoped-define SELF-NAME brws-keys
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brws-keys f-dlg
ON DEFAULT-ACTION OF brws-keys IN FRAME f-dlg
DO:
  /* When DOUBLE-CLICKING on a line edit the associated DB-FIELD.
     ******
     NOTE: Dbl-clicks in enabled columns won't fire this trigger. So the only
     column where this works is normally the DB-FIELD column.
     ****** */
  IF AVAILABLE tt THEN DO:
    RUN edit-related-db-field.
    DISPLAY tt.related-db-field WITH BROWSE brws-keys.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brws-keys f-dlg
ON VALUE-CHANGED OF brws-keys IN FRAME f-dlg
DO:
  RUN set-screen-elements.
  ASSIGN open-recid  = IF AVAILABLE tt THEN RECID(tt) ELSE ?
         open-on-row = SELF:FOCUSED-ROW.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 f-dlg
ON CHOOSE OF BUTTON-1 IN FRAME f-dlg /* Salir */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_Insert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_Insert f-dlg
ON CHOOSE OF b_Insert IN FRAME f-dlg /* Insert Key */
DO:
  DEFINE VAR l_ok AS LOGICAL NO-UNDO.
  DEFINE VAR new-fld AS CHAR NO-UNDO.
  DEFINE VAR old-num AS INTEGER NO-UNDO.

  IF key-table = "" THEN DO:
    MESSAGE "Foreign Keys cannot be choosen until at least"
            "one table has been defined for this query."
         VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END. 
  /* Create a new tt record and edit the db-field. */
  IF NOT available tt THEN FIND LAST tt NO-ERROR.
  IF available tt THEN old-num = tt.num.
  CREATE tt.
  RUN edit-db-field.
  IF tt.db-field eq "":U THEN DELETE tt.
  ELSE DO:
    tt.num = old-num + 1.
    RUN reorder-browse.
    ASSIGN open-recid = RECID(tt)
           open-on-row = open-on-row + 1
           . 
    RUN reopen-query. 
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_Remove f-dlg
ON CHOOSE OF b_Remove IN FRAME f-dlg /* Remove Key */
DO:
  DEFINE VAR i AS INTEGER NO-UNDO.
   
  DO i = 1 TO brws-keys:NUM-SELECTED-ROWS:
    ldummy = brws-keys:FETCH-SELECTED-ROW(i). /* Inserted, and unassigned, rows */ 
    IF AVAILABLE (tt) THEN DELETE tt.         /* ...won't have a record.        */
  END.
  ldummy = brws-keys:DELETE-SELECTED-ROWS().
  RUN set-screen-elements.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK f-dlg 


/* ***************************  Main Block  *************************** */
RUN adeuib/_uibinfo.p (p_context, ?, "PROCEDURE", OUTPUT cresult).
/* Is this being run in a TEMPLATE.  If so, then don't bother doing 
   anything. */
procid=integer(cresult).
RUN adeuib/_uibinfo.p (procid, ?, "TEMPLATE":U, OUTPUT cResult).
IF cResult = "yes":U THEN return.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

RUN adeuib/_uibinfo.p (procid, ?, "EXTERNAL-TABLES":U, OUTPUT cResult).
key-table=cresult.
resultado = p_code.
/*srecid=?.
RUN adeuib/_accsect.p
            ( 'GET':U, procid, 'XFTR:relaciones':U,
            INPUT-OUTPUT srecid,
            INPUT-OUTPUT resultado).
*/
cnt = NUM-ENTRIES(resultado, CHR(10)).
DO i = 1 TO cnt:
  ch = ENTRY(i, resultado, CHR(10)).
  if ch = "" then leave.
  CREATE tt.
  ASSIGN tt.num      = i * 2                /* Leave room for an INSERT */
         tt.db-field = ENTRY(1, ch)
         tt.related = IF ENTRY(2, ch) = "si" then true else false 
         tt.program = ENTRY(3, ch) 
         tt.related-db-field = ENTRY(4, ch).
     
END.

/* Don't allow commas or | in the name. */
ON ",":U, "|":U OF tt.db-field IN BROWSE brws-keys RETURN NO-APPLY.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  RUN reopen-query.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.

  run borra-descriptivos.
  for each tt where tt.db-field <> "" and tt.related-db-field <> "" no-lock:
    run crea-descriptivo. 
  end.
  
  for each tt where tt.db-field <> "" and tt.program <> "" no-lock:
    run crea-trigger-relacion. 
  end. 
  
  /* Recreate the list foreign-keys from the temp-table. */
  ASSIGN foreign-keys = "".
  FOR EACH tt:
    /* Ignore blank lines. */
    IF tt.db-field ne "" and tt.related-db-field ne "" THEN DO:
      /* Strip commas and | from the name. */
      foreign-keys = (IF foreign-keys eq "" THEN "" ELSE foreign-keys + CHR(10)) +
                      SUBSTITUTE ("&1,&2,&3,&4":U,
                                      tt.db-field,
                                      IF tt.related THEN "si":U ELSE "no",
                                      tt.program,
                                      tt.related-db-field).
    END.
  END.
END.
p_code = foreign-keys.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE borra-descriptivos f-dlg 
PROCEDURE borra-descriptivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI f-dlg _DEFAULT-DISABLE
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
  HIDE FRAME f-dlg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE edit-db-field f-dlg 
PROCEDURE edit-db-field :
/*------------------------------------------------------------------------------
  Purpose:     Edit the DB Field of the current temp-table record.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR db_name       AS CHAR          NO-UNDO.
  DEFINE VAR fld_name      AS CHAR          NO-UNDO.
  DEFINE VAR l_ok          AS LOGICAL       NO-UNDO.
  DEFINE VAR tbl_name      AS CHAR          NO-UNDO.
  DEFINE VAR use_Prefix    AS INTEGER       NO-UNDO INITIAL ?.
  
  /* Error condition. */
  IF NOT AVAILABLE (tt) THEN RETURN.
  ELSE DO:
    IF NUM-ENTRIES (tt.db-field, ".":U) eq 3 THEN 
      ASSIGN db_name  = ENTRY(1, tt.db-field, ".":U)
             tbl_name = ENTRY(2, tt.db-field, ".":U)
             fld_name = ENTRY(3, tt.db-field, ".":U) .
    ELSE IF NUM-ENTRIES (key-table) ne 2 THEN
      ASSIGN db_name  = ENTRY(1, key-table, ".":U)
             tbl_name = ENTRY(2, key-table, ".":U) .
    use_Prefix = ?. /* Don't give user choice of changing prefix. */
    RUN adecomm/_fldsel.p (FALSE,       /* Multiple select = no */
                           ?,           /* All data-types       */
                           ?,           /* No temp-table info --- yet! */
                           INPUT-OUTPUT use_Prefix,
                           INPUT-OUTPUT db_name, 
                           INPUT-OUTPUT tbl_name,
                           INPUT-OUTPUT fld_name,
                           OUTPUT l_ok).
    IF l_OK THEN DO:
      /* Assign the field, and the Key-Name, if it is blank. */
      tt.db-field = db_name + ".":U + tbl_name + ".":U + fld_name.
      IF tt.db-field eq "" THEN tt.db-field = fld_name.
    END.
  END. /* ... AVAILABLE tt ... */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE edit-related-db-field f-dlg 
PROCEDURE edit-related-db-field :
/*------------------------------------------------------------------------------
  Purpose:     Edit the DB Field of the current temp-table record.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR db_name       AS CHAR          NO-UNDO.
  DEFINE VAR fld_name      AS CHAR          NO-UNDO.
  DEFINE VAR l_ok          AS LOGICAL       NO-UNDO.
  DEFINE VAR tbl_name      AS CHAR          NO-UNDO.
  DEFINE VAR use_Prefix    AS INTEGER       NO-UNDO INITIAL ?.
  
  /* Error condition. */
  IF NOT AVAILABLE (tt) THEN RETURN.
  ELSE DO:
    IF NUM-ENTRIES (tt.related-db-field, ".":U) eq 3 THEN 
      ASSIGN db_name  = ENTRY(1, tt.related-db-field, ".":U)
             tbl_name = ENTRY(2, tt.related-db-field, ".":U)
             fld_name = ENTRY(3, tt.related-db-field, ".":U) .
    ELSE IF NUM-ENTRIES (key-table) ne 2 THEN
      ASSIGN db_name  = ENTRY(1, key-table, ".":U)
             tbl_name = ENTRY(2, key-table, ".":U) .
    use_Prefix = ?. /* Don't give user choice of changing prefix. */
    RUN adecomm/_fldsel.p (FALSE,       /* Multiple select = no */
                           ?,           /* All data-types       */
                           ?,           /* No temp-table info --- yet! */
                           INPUT-OUTPUT use_Prefix,
                           INPUT-OUTPUT db_name, 
                           INPUT-OUTPUT tbl_name,
                           INPUT-OUTPUT fld_name,
                           OUTPUT l_ok).
    IF l_OK THEN DO:
      /* Assign the field, and the related-db-field, if it is blank. */
      tt.related-db-field = db_name + ".":U + tbl_name + ".":U + fld_name.
      IF tt.related-db-field eq "" THEN tt.related-db-field = fld_name.
    END.
  END. /* ... AVAILABLE tt ... */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI f-dlg _DEFAULT-ENABLE
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
  ENABLE brws-keys b_Insert BUTTON-1 
      WITH FRAME f-dlg.
  {&OPEN-BROWSERS-IN-QUERY-f-dlg}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query f-dlg 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     Reopen the browse query, and set the interface correctly.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i    AS INTEGER NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    {&OPEN-QUERY-brws-keys}
    IF open-on-row > 0 THEN 
      ldummy = brws-keys:SET-REPOSITIONED-ROW (open-on-row, "CONDITIONAL":U).
    REPOSITION brws-keys TO RECID open-recid NO-ERROR.
    ldummy = brws-keys:SELECT-ROW(brws-keys:FOCUSED-ROW) NO-ERROR.
    RUN set-screen-elements.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reorder-browse f-dlg 
PROCEDURE reorder-browse :
/*------------------------------------------------------------------------------
  Purpose:     Numbers the items in the browse by 2
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INTEGER NO-UNDO.
  DEF BUFFER xtt FOR tt.
  
  REPEAT PRESELECT EACH xtt BY xtt.num:
    FIND NEXT xtt.
    ASSIGN i = i + 2
           xtt.num = i.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-screen-elements f-dlg 
PROCEDURE set-screen-elements :
/*------------------------------------------------------------------------------
  Purpose:     Set the sensitivity of buttons etc. in the interface.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      b_remove:SENSITIVE    = brws-keys:NUM-SELECTED-ROWS > 0.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


