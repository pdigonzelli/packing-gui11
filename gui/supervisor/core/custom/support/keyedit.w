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
define var lista_c as character no-undo.
define var lista_d as character no-undo.
define var lista_p as character no-undo.
DEFINE VAR FLAG-CANCELA AS LOGICAL NO-UNDO INITIAL FALSE.

/* Temp-Table for browsing keys ---                                     */
DEFINE TEMP-TABLE tt NO-UNDO
  FIELD num AS INTEGER
  FIELD db-field AS CHAR
  FIELD program AS char
  FIELD related-db-field AS CHAR
  FIELD related-db-field-1 AS CHAR
  FIELD db-where AS CHAR 
  INDEX num IS PRIMARY num.

define temp-table tt1 no-undo
    field nombre1 as character 
    field cuenta as integer format "99"
    index nom is primary unique nombre1.

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
&Scoped-define FIELDS-IN-QUERY-brws-keys tt.db-field tt.program tt.related-db-field tt.related-db-field-1 tt.db-where   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brws-keys all   
&Scoped-define FIELD-PAIRS-IN-QUERY-brws-keys~
 ~{&FP1}all ~{&FP2}all ~{&FP3}
&Scoped-define SELF-NAME brws-keys
&Scoped-define OPEN-QUERY-brws-keys OPEN QUERY brws-keys PRESELECT EACH tt.
&Scoped-define TABLES-IN-QUERY-brws-keys tt
&Scoped-define FIRST-TABLE-IN-QUERY-brws-keys tt


/* Definitions for DIALOG-BOX f-dlg                                     */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brws-keys b_Insert BUTTON-5 BUTTON-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retorna_campo f-dlg 
FUNCTION retorna_campo RETURNS CHARACTER
  ( input c as character , input j as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retorna_tabla f-dlg 
FUNCTION retorna_tabla RETURNS CHARACTER
  ( input c as character , input j as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 AUTO-GO 
     LABEL "&Salir" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-5 AUTO-GO 
     LABEL "&Cancelar" 
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
      tt.program  format "x(200)" WIDTH 30 LABEL "Programa"
      tt.related-db-field FORMAT "X(128)" WIDTH 30 LABEL "Campo asociado"
      tt.related-db-field-1 FORMAT "x(40)" WIDTH 30 LABEL "Clave equivalente" 
      tt.db-where FORMAT "x(80)" WIDTH 30 LABEL "Claususla Where" 
ENABLE all
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 107 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-dlg
     brws-keys AT ROW 1 COL 1
     b_Insert AT ROW 1.24 COL 111
     b_Remove AT ROW 2.57 COL 111
     BUTTON-5 AT ROW 3.95 COL 110.8
     BUTTON-1 AT ROW 5.29 COL 110.8
     SPACE(0.39) SKIP(3.37)
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


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 f-dlg
ON CHOOSE OF BUTTON-5 IN FRAME f-dlg /* Cancelar */
DO:
 FLAG-CANCELA = TRUE.
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
assign cnt = NUM-ENTRIES(resultado, CHR(10)). 
DO i = 1 TO cnt:
  ch = ENTRY(i, resultado, CHR(10)).
  
/**** el siguiente parrafo es por compatibilidad con versiones anteriores ****/  
  if not ch matches "*(*" then
     ch = replace(ch, ",",";").
/******************************************************************************/     
  if ch = "" or ch = "*/" then leave.
  if substr(ch,1,2) = "/*" then next.
  CREATE tt.
  ASSIGN tt.num      = i * 2                /* Leave room for an INSERT */
         tt.db-field = ENTRY(1, ch , ";")
         tt.program = ENTRY(2, ch , ";") 
         tt.related-db-field = ENTRY(3, ch, ";").
         if num-entries(ch,";") >= 4 then
             tt.related-db-field-1 = ENTRY(4,ch,";").
         if num-entries(ch,";") = 5 then
             tt.db-where = ENTRY(5,ch,";").
END.

/* Don't allow commas or | in the name. */
ON ",":U, "|":U OF tt.db-field IN BROWSE brws-keys RETURN NO-APPLY.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  RUN reopen-query.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.

  IF NOT FLAG-CANCELA THEN
  DO:
      run borra-descriptivos( input procid).

      run crea-eventos ( input procid).

      /* Recreate the list foreign-keys from the temp-table. */
      ASSIGN foreign-keys = "/* campos relacionados con tablas externas ".
      FOR EACH tt:
        IF tt.db-field ne "" and tt.related-db-field ne "" THEN DO:
      /* Strip commas and | from the name. */
          foreign-keys =  foreign-keys + CHR(10) +
                          SUBSTITUTE ("&1;&2;&3;&4;&5":U,
                                          tt.db-field,
                                          tt.program,
                                          tt.related-db-field,
                                          tt.related-db-field-1,
                                          tt.db-where).
        END.
      END.
  END.        
END.

IF NOT FLAG-CANCELA THEN 
DO:
    foreign-keys = foreign-keys + chr(10) + "*/".
    p_code = foreign-keys.
END.    
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
define input parameter procid as integer no-undo.

define var p_code as character no-undo.
define var i_contex as character no-undo.
define var srecid as integer no-undo.
define var cresult as character no-undo.

p_code = "" + chr(10) + "END PROCEDURE.".
srecid = ?.
run adeuib/_accsect.p ("SET" , input procid , "PROCEDURE:descriptivos", 
                               input-output srecid, input-output p_code).
srecid = ?.
run adeuib/_accsect.p ("SET" , input procid, "PROCEDURE:habilitar_relacion", 
                                   input-output srecid, input-output p_code).   

run adeuib/_uibinfo.p(input procid,input ?,input "FRAMES", output cresult).
run adeuib/_uibinfo.p(input procid, input cresult ,input "context", output i_contex).

RUN adeuib/_uibinfo.p (?,cresult, "CONTAINS FILL-IN" , OUTPUT i_contex).
do i=1 to num-entries(i_contex):
    RUN adeuib/_uibinfo.p (integer(entry(i,i_contex)),?,"NAME" , OUTPUT cresult).  
    if substring(cresult,1,3) = "fi-" then
        RUN adeuib/_uib_del.p(integer(entry(i,i_contex))).        
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crea-descriptivos f-dlg 
PROCEDURE crea-descriptivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter procid as character no-undo.
define input parameter lista_relacion as character no-undo.

define var p_code as character no-undo.
define var i_contex as character no-undo.
define var srecid as integer no-undo.
define var c as character no-undo.
define var i as integer no-undo.
define var h as handle no-undo.
define var f as handle no-undo.
define var fila as decimal no-undo.
define var columna as decimal no-undo.
define var ancho as decimal no-undo.
define var nombre as character no-undo.
define var nombre_frame as character no-undo.
define var cnt as integer no-undo.
define var campo as character no-undo.
define var base as character no-undo.
define var t-code as character no-undo initial "".
define var clave as character no-undo.
define var cuenta-nombres as integer no-undo.
define var c_tipo as character no-undo.
define var cWhere as character no-undo.

for each tt1 :
    delete tt1.
end.
cuenta-nombres = 0.
cnt = num-entries(lista_relacion,chr(10)) - 1.
t-code="DO:" + CHR(10) + "~{custom/support/validacion.i}" + chr(10) + 
           "     run descriptivos." + chr(10) + "END.".
do i = 1 to cnt:
    base=entry(i,lista_relacion,chr(10)). 
    c=entry(1,base).
    campo=entry(2,base).
    if trim(entry(3,base)) <> "" then
        clave = entry(3,base) .
    else
        clave = retorna_campo(c,2).
    cWhere = entry(4,base).    
    if c <> "" then
    do:
        run adeuib/_uibinfo.p(input ?, input c,input "handle", output i_contex).    
        h=widget-handle(i_contex).
        fila = decimal(h:row).
        columna = decimal(h:col) + decimal(h:width) + 1.00.
        run adeuib/_uibinfo.p(input procid,input ?, input "FRAMES", output nombre_frame).
        run adeuib/_uibinfo.p(input procid, input nombre_frame ,input "handle", output f).
        ancho= if ( decimal(f:width) - columna - 1.00 ) > 40 then 40 else
                  ( decimal(f:width) - columna - 1.00 ).
        nombre=substring("fi-" + retorna_tabla(campo,1) + "-" + retorna_campo(campo,1),1,31) .
        find first tt1 where nombre1 = nombre  no-error.
        if not available tt1 then
        do:
            create tt1.
            assign tt1.nombre1 = nombre
                   tt1.cuenta  = 1.
        end.
        else
        do:
            nombre = substring(nombre,1,29) + "-" + string(tt1.cuenta).
            tt1.cuenta = tt1.cuenta + 1.
        end.    
        run adeuib/_uibinfo.p(input ?, input nombre_frame,input "context", output i_contex).
        run adeuib/_uib_crt.p(input integer(i_contex),"FILL-IN",
                               "special: " + chr(10) + 
                               "name " + nombre + chr(10) + 
                               "no-label yes" + chr(10) + 
                               "enable false" + chr(10) +
                               "fgcolor 1" ,
                               input fila , 
                               input columna,
                               input 0.90, 
                               input ancho,
                               output srecid).

        run adeuib/_uibinfo.p(input srecid, input ?,input "handle", output i_contex).
        h = widget-handle(i_contex).
        h:sensitive = false.
        h:height = 0.90.
        h:row = fila.
        run custom/support/c_tipo.p ( input retorna_tabla(retorna_campo(c,1),1) , retorna_campo(c,2) , output c_tipo ).

        CASE c_tipo :
            WHEN "character" THEN
            p_code = p_code + chr(10) + 
             "find first " + retorna_tabla(retorna_tabla(campo,2),1) + " where "  + 
             retorna_tabla(retorna_tabla(campo,2),1) + "."  +
             clave + " = " +      
             retorna_campo(c,1) +  ":screen-value in frame " + nombre_frame. 
            WHEN "integer" THEN
             p_code = p_code + chr(10) + 
             "find first " + retorna_tabla(retorna_tabla(campo,2),1) + " where "  + 
             retorna_tabla(retorna_tabla(campo,2),1) + "."  +
             clave + " = " +      
             "integer(" + retorna_campo(c,1) +  ":screen-value in frame " + nombre_frame + ")" .
            WHEN "date" THEN
            p_code = p_code + chr(10) + 
             "find first " + retorna_tabla(retorna_tabla(campo,2),1) + " where "  + 
             retorna_tabla(retorna_tabla(campo,2),1) + "."  +
             clave + " = " +      
             "date(" + retorna_campo(c,1) +  ":screen-value in frame " + nombre_frame + ")" .
            WHEN "decimal" THEN
             p_code = p_code + chr(10) + 
             "find first " + retorna_tabla(retorna_tabla(campo,2),1) + " where "  + 
             retorna_tabla(retorna_tabla(campo,2),1) + "."  +
             clave + " = " +      
             "decimal(" + retorna_campo(c,1) +  ":screen-value in frame " + nombre_frame + ")" .
        END CASE.        
        p_code = p_code + " " + cWhere + " no-lock no-error .".

        p_code = p_code + chr(10) + 
            "if available " + retorna_tabla(retorna_tabla(campo,2),1) + " then " + chr(10) + 
            nombre + ":screen-value in frame " + nombre_frame + " = string(" + campo + ")." + chr(10).
        p_code = p_code + "else" + chr(10) +
        nombre + ":screen-value in frame " + nombre_frame + " = ''" + "." + chr(10).
        run adeuib/_uibinfo.p(input ?, input c,input "context", output i_contex).
        srecid = integer(i_contex).
        i_contex = ?.
        run adeuib/_accsect.p ("SET" , input srecid, "TRIGGER:U1", 
                                   input-output i_contex, input-output t-code).
        i_contex = ?.
        run adeuib/_accsect.p ("SET" , input srecid, "TRIGGER:LEAVE", 
                                   input-output i_contex, input-output t-code).
        i_contex = ?.
        run adeuib/_accsect.p ("SET" , input srecid, "TRIGGER:GO", 
                                   input-output i_contex, input-output t-code).   
     end.
end.

if p_code <> "" then
do:
    p_code = p_code + chr(10) + "END PROCEDURE.".
    srecid = ?.
    run adeuib/_accsect.p ("SET" , input procid, "PROCEDURE:descriptivos", 
                                   input-output srecid, input-output p_code).
end.
else
do:
    p_code = "" + chr(10) + "END PROCEDURE.".
    srecid = ?.
    run adeuib/_accsect.p ("SET" , input procid, "PROCEDURE:descriptivos", 
                                   input-output srecid, input-output p_code).
end.                                   


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crea-eventos f-dlg 
PROCEDURE crea-eventos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter procid as integer no-undo.

define var lista_relacion as character no-undo.
define var srecid as integer no-undo.
define var p_code as character no-undo.


/**** PRIMER PASO : CUANDO HAY PROGRAMA DE SELECCION CAMBIAR LOS PUNTEROS ****/
p_code = "END PROCEDURE.".

srecid = ?.
run adeuib/_accsect.p ("SET" , input procid, "PROCEDURE:habilitar_relacion", 
                       input-output srecid, input-output p_code).   
for each tt :
    if tt.program <> "" then
        if lista_relacion = "" then
            lista_relacion = retorna_campo(tt.db-field,2).
        else lista_relacion = lista_relacion + "," + retorna_campo(tt.db-field,2).
end.

run crea-puntero ( input lista_relacion).


/***** FIN DEL PRIMER PASO *****/

/****** SEGUNDO PASO : CREAR LOS FILL-INS DE LOS CAMPOS ASOCIADOS Y LOS TRIGGERS
 DE REFRESCO DE ESTOS FILL-INS ********/

lista_relacion = "".
for each tt :
    if retorna_campo(tt.related-db-field,1) <> "" then
        lista_relacion = lista_relacion + tt.db-field + "," + tt.related-db-field + "," + tt.related-db-field-1 + "," + tt.db-where + chr(10).
end.
define var nombre-tabla as character no-undo.
find first tt.
nombre-tabla = retorna_tabla(tt.db-field,1).
create alias "DICTDB" for database value(nombre-tabla).
run crea-descriptivos ( input procid , input lista_relacion).

/***** FIN DEL SEGUNDO PASO *******/

/****** TERCER PASO : CREACION DE TRIGGERS DE RELACION  ********/

lista_relacion = "".
for each tt :
    if tt.related-db-field <> "" and tt.program <> "" and tt.db-field <> "" then
        lista_relacion = lista_relacion + tt.db-field + ";" + tt.program + 
                         ";" + tt.related-db-field +
                         ";" + tt.related-db-field-1 + chr(10).
end.

run crea-trigger-relacion ( input lista_relacion).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crea-puntero f-dlg 
PROCEDURE crea-puntero :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter lista_relacion as character no-undo.

define var nombre_frame as character no-undo.
define var p_code as character no-undo.
define var s_recid as integer no-undo.

p_code = "define var field-group as handle." + chr(10) + 
        "define var cur-control as handle.".
p_code = p_code + chr(10) + 
             "define var lista_relacion as character no-undo initial " + 
             """" + lista_relacion + """" + ".".
p_code = p_code + chr(10) + "field-group = frame ~{&FRAME-NAME}:first-child." 
             + chr(10) + 
             "cur-control = field-group:first-tab-item." + chr(10) + 
             "do while valid-handle(cur-control): " + chr(10) . 
p_code = p_code  + chr(10) +
             "    if cur-control:visible and cur-control:type = " + 
             """" + "fill-in" + """".
p_code = p_code  + chr(10) +  
             "    and lookup(cur-control:name,lista_relacion) <> 0 then " + chr(10) + 
             "        cur-control:load-mouse-pointer(""glove"")." + chr(10) + 
             "    cur-control = cur-control:next-tab-item." + chr(10) + 
             "end." + chr(10) + 
             "END PROCEDURE.".
srecid = ?.
run adeuib/_accsect.p ("SET" , input ?, "PROCEDURE:habilitar_relacion", 
                                   input-output srecid, input-output p_code).   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crea-trigger-relacion f-dlg 
PROCEDURE crea-trigger-relacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter lista_relacion as character no-undo.

define var p_code as character no-undo.
define var i_contex as character no-undo.
define var srecid as integer no-undo.
define var c as character no-undo.
define var d as character no-undo.
define var cnt as integer no-undo.
define var base as character no-undo.
define var clave as character no-undo.

define var i as integer no-undo.
define var flag-parametros as logical no-undo initial no.
cnt = num-entries(lista_relacion,chr(10)) - 1.

do i = 1 to cnt:
    p_code = "".
    base=entry(i,lista_relacion,chr(10)). 
    c=entry(1,base,";").
    if entry(2,base,";") matches "*(*" and 
     not entry(2,base,";") matches "*output r*" then
        do:
        message "Error en la definición de parámetros en algún programa de consulta .
Debe tener siempre el parámetro output r" view-as alert-box error .
    leave.
   end.
    if entry(2,base,";") matches "*(*" then
        flag-parametros = true.
    else 
        flag-parametros = false.
    if trim(entry(4,base,";")) <> "" then
        clave = entry(4,base,";").
    else
        clave = retorna_campo(c,2).    
    run adeuib/_uibinfo.p(input ?, input c,input "context", output i_contex).
    p_code = "do: " + chr(10) +
        "define var r as rowid no-undo." + chr(10) +   
        "run " + entry(2,base,";") + 
        (if flag-parametros then "." else "(output r).") + chr(10) + 
        "find " + retorna_tabla(entry(3,base,";"),1) + " where " + "rowid(" + retorna_tabla(entry(3,base,";"),1) + ") = r no-lock no-error." + chr(10) +
        "if available " + retorna_tabla(entry(3,base,";"),1) + " then " + chr(10) +
        c + ":screen-value = " + "string(" + retorna_tabla(entry(3,base,";"),1) + "." + clave + ")." + chr(10). 
        if retorna_campo(entry(3,base,";"),1) <> "" then
            p_code = p_code + "apply 'U1' to self." + chr(10).
        p_code = p_code + "end.".
 
    srecid = ?.
    run adeuib/_accsect.p ("SET" , input integer(i_contex), "TRIGGER:MOUSE-SELECT-DBLCLICK", 
                                   input-output srecid, input-output p_code).
end.

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
  ENABLE brws-keys b_Insert BUTTON-5 BUTTON-1 
      WITH FRAME f-dlg.
  {&OPEN-BROWSERS-IN-QUERY-f-dlg}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilitar_relacion f-dlg 
PROCEDURE habilitar_relacion :
define var field-group as handle.
define var cur-control as handle.
define var lista_relacion as character no-undo initial "id_empresa,id_ejercicio".
field-group = frame {&FRAME-NAME}:first-child.
cur-control = field-group:first-tab-item.
do while valid-handle(cur-control): 

    if cur-control:visible and cur-control:type = "fill-in"
    and lookup(cur-control:name,lista_relacion) <> 0 then 
        cur-control:load-mouse-pointer("glove").
    cur-control = cur-control:next-tab-item.
end.
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


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retorna_campo f-dlg 
FUNCTION retorna_campo RETURNS CHARACTER
  ( input c as character , input j as integer) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
define var r as character no-undo.
define var i as integer no-undo.
define var cuenta as integer initial 0 no-undo.

do i = 1 to length(c):
    if cuenta >= j then
        r = r + substring(c,i,1).
    if substring(c,i,1) = "." then
        cuenta = cuenta + 1.
         
end.

    
  RETURN r.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retorna_tabla f-dlg 
FUNCTION retorna_tabla RETURNS CHARACTER
  ( input c as character , input j as integer) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
define var r as character no-undo.
define var i as integer no-undo.
define var cuenta as integer initial 0 no-undo.

do i = 1 to length(c):
    if substring(c,i,1) = "." then
        cuenta = cuenta + 1.
    if cuenta < j then
        r = r + substring(c,i,1).     
end.

    
  RETURN r.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


