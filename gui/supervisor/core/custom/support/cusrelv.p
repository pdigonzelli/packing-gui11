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


define variable i_contex as character no-undo.
define variable j_contex as integer no-undo.
define var procid as integer no-undo.
DEFINE VAR CRESULT AS CHARACTER.
define var k as integer.
define var fila as decimal no-undo.
define var columna as decimal no-undo.
define variable srecid as integer no-undo initial ? .
define var lista_campos as character.
define var h as handle no-undo.
define var lista_relacion as character no-undo.
define var lista_programas as character no-undo.
define var lista_descriptivo as character.
define var lista_c as character no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retorna_campo Procedure 
FUNCTION retorna_campo RETURNS CHARACTER
  ( input c as character , input j as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retorna_tabla Procedure 
FUNCTION retorna_tabla RETURNS CHARACTER
  ( input c as character , input j as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN adeuib/_uibinfo.p (p_contextID, ?, "PROCEDURE", OUTPUT i_contex).
/* Is this being run in a TEMPLATE.  If so, then don't bother doing 
   anything. */
assign procid = integer(i_contex).
RUN adeuib/_uibinfo.p (procid, ?, "TEMPLATE":U, OUTPUT cResult).
IF cResult = "yes":U THEN return.

 
srecid=?.
RUN adeuib/_accsect.p 
      ('GET':U, procid, 'XFTR:relaciones':U,
       INPUT-OUTPUT srecid,
       INPUT-OUTPUT p_code).

run adeuib/_uibinfo.p (procid,?,"FRAMES",output cresult).
RUN adeuib/_uibinfo.p (?, cresult,input "context", output i_contex).
run adeuib/_uibinfo.p (?,input cresult,
                       input "FIELDS",output lista_campos).

do k = 1 to num-entries(lista_campos) :               
   if lista_c = "" then
    lista_c=entry(k,lista_campos).
   else
    lista_c=lista_c + "," + entry(k,lista_campos).
end.
run borra-descriptivos.
if lista_c <> "" then
do: 
    run custom/support/cuslisr.w 
        (input lista_c, output lista_relacion, output lista_programas
         , output lista_descriptivo).
    if lista_relacion <> "" then
    do:
        run crea_trigger_relacion ( input lista_relacion , 
                                    input lista_programas ,
                                    input lista_descriptivo , 
                                    input procid).
        run crea_habilita_relacion ( input lista_relacion , input procid).
        run crea_descriptivo (input lista_relacion , 
                              input lista_descriptivo , input procid).
    end.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE borra-descriptivos Procedure 
PROCEDURE borra-descriptivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var p_code as character no-undo.
define var i_contex as character no-undo.
define var srecid as integer no-undo.

p_code = "" + chr(10) + "END PROCEDURE.".
srecid = ?.
run adeuib/_accsect.p ("SET" , input ? , "PROCEDURE:descriptivos", 
                               input-output srecid, input-output p_code).
srecid = ?.
run adeuib/_accsect.p ("SET" , input ?, "PROCEDURE:habilitar_relacion", 
                                   input-output srecid, input-output p_code).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crea_descriptivo Procedure 
PROCEDURE crea_descriptivo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter lista_c as character.
define input parameter lista_d as character.
define input parameter procid as integer.

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

do i = 1 to num-entries(lista_c):
    c=retorna_campo(entry(i,lista_c),1).
    if entry(i,lista_d) <> "" then
    do:
        run adeuib/_uibinfo.p(input ?, input c,input "handle", output i_contex).    
        h=widget-handle(i_contex).
        fila = decimal(h:row).
        columna = decimal(h:col) + decimal(h:width) + 2.00.
        run adeuib/_uibinfo.p(input procid,input ?, input "FRAMES", output nombre_frame).
        run adeuib/_uibinfo.p(input ?, input nombre_frame ,input "handle", output f).
        ancho= decimal(f:width) - columna - 1.00.
        nombre="fi-" + retorna_campo(entry(i,lista_d),1). 
        run adeuib/_uibinfo.p(input ?, input nombre_frame,input "context", output i_contex).
        run adeuib/_uib_crt.p( input integer(i_contex),"FILL-IN",
                               "special: " + chr(10) + 
                               "name " + nombre + chr(10) + 
                               "no-label yes" ,
                               input fila , 
                               input columna,
                               input 1.00, 
                               input ancho,
                               output srecid).
        run adeuib/_uibinfo.p(input ?, nombre,input "handle", output f).
        f:sensitive = false.                       
        p_code = p_code + 
            "find " + retorna_tabla(entry(i,lista_d),1) + " of "  + retorna_tabla(c,1) + " no-lock no-error ."
            +  chr(10) +   
            "if available " + retorna_tabla(entry(i,lista_d),1) + " then " + chr(10) + 
            nombre + ":screen-value in frame " + nombre_frame + " = string(" + entry(i,lista_d) + ")." + chr(10).
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

do i = 1 to num-entries(lista_c):
    c=retorna_campo(entry(i,lista_c),1).
    if entry(i,lista_d) <> "" then
    do:
        run adeuib/_uibinfo.p(input ?, input c,input "context", output i_contex).
        run adeuib/_uibinfo.p(input procid,input ?, input "FRAMES", output nombre_frame).
        nombre="fi-" + retorna_campo(entry(i,lista_d),1). 
        p_code = "DO:" + CHR(10) +
            "find " + retorna_tabla(entry(i,lista_d),1) + " WHERE "  + 
            retorna_tabla(entry(i,lista_d),1) + "." + retorna_campo(c,1) +
            " = input " + c + " no-lock no-error ."
            +  chr(10) +   
            "if available " + retorna_tabla(entry(i,lista_d),1) + " then " + chr(10) + 
            nombre + ":screen-value in frame " + nombre_frame + " = " + entry(i,lista_d) + "." + chr(10).
        p_code = p_code + chr(10) + "END.".
        
        srecid = ?.
        run adeuib/_accsect.p ("SET" , input integer(i_contex), "TRIGGER:ANY-KEY", 
                                   input-output srecid, input-output p_code).
        srecid = ?.
        run adeuib/_accsect.p ("SET" , input integer(i_contex), "TRIGGER:LEAVE", 
                                   input-output srecid, input-output p_code).
        srecid = ?.
        run adeuib/_accsect.p ("SET" , input integer(i_contex), "TRIGGER:GO", 
                                   input-output srecid, input-output p_code).                                                      
     end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crea_habilita_relacion Procedure 
PROCEDURE crea_habilita_relacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter lista_c as character.
define input parameter procid as integer.

define var p_code as character no-undo.
define var srecid as integer no-undo.
define var nombre_frame as character no-undo.
define var lista_relacion as character no-undo initial "".
define var i as integer no-undo.

if lista_c = "" then
    p_code = "END PROCEDURE". 
else
do:
    do i = 1 to num-entries(lista_c):
        if lista_relacion = "" then
            lista_relacion = retorna_campo(entry(i,lista_c),2).
        else
            lista_relacion = lista_relacion + "," + retorna_campo(entry(i,lista_c),2).    
    end.    
    run adeuib/_uibinfo.p (input procid , ? , input "FRAMES" , output nombre_frame).
    p_code = "define var field-group as handle." + chr(10) + 
        "define var cur-control as handle.".
    p_code = p_code + chr(10) + 
             "define var lista_relacion as character no-undo initial " + 
             """" + lista_relacion + """" + ".".
    p_code = p_code + chr(10) + "field-group = frame " + nombre_frame + ":first-child." 
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
end.                   
srecid = ?.
run adeuib/_accsect.p ("SET" , input procid, "PROCEDURE:habilitar_relacion", 
                                   input-output srecid, input-output p_code).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crea_trigger_relacion Procedure 
PROCEDURE crea_trigger_relacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter lista_c as character no-undo.
define input parameter lista_p as character no-undo.
define input parameter lista_d as character no-undo.
define input parameter procid as integer no-undo.

define var p_code as character no-undo.
define var i_contex as character no-undo.
define var srecid as integer no-undo.
define var c as character no-undo.
define var d as character no-undo.

define var i as integer no-undo.

do i = 1 to num-entries(lista_p):
    p_code = "".
    c=retorna_campo(entry(i,lista_c),1).
    d=entry(i,lista_d).
    run adeuib/_uibinfo.p(input ?, input c,input "context", output i_contex).
    p_code = "do: " + chr(10) +
        "define var r as rowid no-undo." + chr(10) +   
        "run " + entry(i,lista_p) + 
        "(output r)." + chr(10) + 
        "find " + retorna_tabla(d,1) + " where " + "rowid(" + retorna_tabla(d,1) + ") = r no-lock no-error." + chr(10) +
        "if available " + retorna_tabla(d,1) + " then " + chr(10) +
        c + ":screen-value = " + "string(" + retorna_tabla(d,1) + "." + retorna_campo(c,1) + ")." + chr(10) + 
        "apply 'any-key' to self." + chr(10) +
        "end.".
    srecid = ?.
    run adeuib/_accsect.p ("SET" , input integer(i_contex), "TRIGGER:MOUSE-SELECT-DBLCLICK", 
                                   input-output srecid, input-output p_code).

end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retorna_campo Procedure 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retorna_tabla Procedure 
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


