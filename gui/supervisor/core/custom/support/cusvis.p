&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT        PARAMETER p_contextid  AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p_code      AS CHARACTER NO-UNDO.
define var cresult as character no-undo.
define var i_contex as character no-undo.
define var procid as integer no-undo.
define var lista_frame as character no-undo.
define var h as handle no-undo.
define var lista_campos as character no-undo.
define var srecid as integer no-undo.
define var i as integer no-undo.
define var nombre_viewer as character no-undo.
define var e as character view-as editor inner-chars 120 inner-lines 50.
form e with width 150.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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
         HEIGHT             = 2
         WIDTH              = 39.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

define var field-group as handle.
define var cur-control as handle.

RUN adeuib/_uibinfo.p (p_contextID, ?, "PROCEDURE", OUTPUT i_contex).
/* Is this being run in a TEMPLATE.  If so, then don't bother doing 
   anything. */
assign procid = integer(i_contex).
RUN adeuib/_uibinfo.p (procid, ?, "TEMPLATE":U, OUTPUT cResult).
IF cResult = "yes":U THEN return.

/**** BUSCO LA MAIN DEL VIEWER ****/

run adeuib/_uibinfo.p (procid,?,"contains * return name",output cresult).
do i=1 to num-entries(cresult):
    nombre_viewer = "" .
    if substring(entry(i,cresult),1,2) = "h_" and
       substring(entry(i,cresult),3,1) = "v" then
            nombre_viewer = entry(i,cresult).
       if nombre_viewer <> "" then
       do:     
        RUN adeuib/_uibinfo.p (?, nombre_viewer, "context":U, OUTPUT i_contex).
        RUN adeuib/_uibinfo.p (integer(i_contex), "F-MAIN", "handle":U, OUTPUT i_contex).
        h= widget-handle(i_contex).
        field-group = h:first-child.
        cur-control = field-group:first-tab-item.
        do while valid-handle(cur-control):
            if cur-control:type = "FILL-IN" then 
            if lista_campos = "" then
                lista_campos= cur-control:name.
            else    
                lista_campos = lista_campos + "," + cur-control:name.    
            cur-control = cur-control:next-tab-item.
        end.
       end. 
end.

run custom/support/cusvis1.w (input-output lista_campos).

if lista_campos <> "" then
do:
    srecid=?.
    p_code = "define var lista as character no-undo." + chr(10) +
    "define var i as integer no-undo." + chr(10) + 
    "define var h as handle no-undo." + chr(10) + 
    "define var lista_campos as character no-undo initial " + """" + lista_campos + """" + "." +  chr(10) +  
    "    run get-link-handle in adm-broker-hdl (input this-procedure,""CONTAINER-TARGET""," + chr(10) + 
    "                                       output lista)." + chr(10) + 
    "    do i=1 to num-entries(lista):" + chr(10) + 
    "        h=widget-handle(entry(i,lista))."+ chr(10) + 
    "        run get-attribute in h ('TYPE')." + chr(10) + 
    "        if return-value = ""smartviewer"" then " + chr(10) + 
    "            run deshabilita_campos in h (input lista_campos)." + chr(10) + 
    "    end.".  
    p_code = p_code + chr(10) + "END PROCEDURE.".
    run adeuib/_accsect.p("SET",procid,"PROCEDURE:deshabilita_viewer",input-output srecid,
                          input-output p_code).
                        
end.

p_code = "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


