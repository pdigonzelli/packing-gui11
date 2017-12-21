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
define var c as character.
DEFINE VAR CRESULT AS CHARACTER.
define var k as integer.
define var fila as decimal no-undo.
define var columna as decimal no-undo.
define variable srecid as integer no-undo initial ? .
define var lista_tablas as character.
define var lista_campos as character.
define var frase-frame as character.
define var texto as character no-undo.
define var lista_campos_completa as character.
define var alto_frame as integer no-undo.
define var h as handle no-undo.
define var f as handle no-undo.
define var flag as logical no-undo.
define var resultado as character no-undo.
define var respuesta as logical no-undo.
define var i as integer no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD devuelve-linea Procedure 
FUNCTION devuelve-linea RETURNS CHARACTER
  ( input lista as character , input linea as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retorna-campo Procedure 
FUNCTION retorna-campo RETURNS CHARACTER
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
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
RUN adeuib/_uibinfo.p (p_contextID, ?, "PROCEDURE", OUTPUT i_contex).
/* Is this being run in a TEMPLATE.  If so, then don't bother doing 
   anything. */
assign procid = integer(i_contex).
RUN adeuib/_uibinfo.p (procid, ?, "TEMPLATE":U, OUTPUT cResult).
IF cResult = "yes":U THEN return.

run adeuib/_uibinfo.p (?, input ?, input "FRAMES", output cresult).
run adeuib/_uibinfo.p (?, input cresult, input "context",output  i_contex).                       
run adeuib/_uibinfo.p (integer(i_contex),?,
                       input "FIELDS",output lista_campos).
fila = 0.
if lista_campos <> "" then
do i = 1 to num-entries ( lista_campos ) :
    run adeuib/_uibinfo.p (input ? , entry(i,lista_campos),
                   "handle",output h).
    if valid-handle(h) and h:row >= fila then
        fila = h:row.
    if valid-handle(h) and h:col >= columna then
            columna = h:col.
end.
fila=fila + 1.0.                       

run adeuib/_uib_crt.p (INPUT integer(i_contex),
    INPUT "DB-FIELDS",INPUT ?,
    INPUT fila,
    INPUT ?,
    INPUT ?,
    INPUT ?,OUTPUT j_contex).

run adeuib/_uibinfo.p (integer(i_contex),?,
                       input "FIELDS",output lista_campos).

alto_frame= num-entries(lista_campos).
run adeuib/_uibinfo.p (input procid,input cresult,
                       "handle",output f).              
if f:height-chars < alto_frame then
do:
        message "La frame no puede contener tantos campos.¿Desea Borrarlos ?"
        update respuesta view-as alert-box question button yes-no .
        if respuesta then
        do k = 1 to num-entries(lista_campos) :               
            run adeuib/_uibinfo.p (input ? , entry(k,lista_campos),
                   "context",output i_contex).
            run adeuib/_uib_del.p (input integer(i_contex)).
        end.
end.
/*
else
if lista_campos <> "" then
do:
    flag = false.
    columna = 0.
    do k = 1 to num-entries(lista_campos) :               
        run adeuib/_uibinfo.p (input ? , entry(k,lista_campos),
                   "handle",output h).
        if columna = 0 then columna = h:col.           
        if h:col > columna then
        do:
            flag = true.
            columna = h:col.
        end.    
    end.
    fila = 1.00.
    if flag then
    do k = 1 to num-entries(lista_campos) :               
        run adeuib/_uibinfo.p (input ?, entry(k,lista_campos),
                   "handle",output h).
        if (h:width + h:col) > f:width then
            h:col = 1.
        else           
            h:col = columna.
        h:row = fila.
        fila = fila + 1.
    end.
end.    
*/
do k = 1 to num-entries(lista_campos):               
       if lista_campos="" then
        cresult = retorna-campo(entry(k,lista_campos),2).
       else
        cresult = cresult + "," + retorna-campo(entry(k,lista_campos),2).  
end.
run custom/support/cuswfld.p ( input p_contextid, input-output p_code). 
p_code = "".
run custom/support/cusprim.p ( input p_contextid,
                               input cresult,
                               input-output p_code).

srecid=?.
p_code = "".
RUN adeuib/_accsect.p 
      ('GET':U, procid, 'XFTR:Relaciones':U,
       INPUT-OUTPUT srecid,
       INPUT-OUTPUT p_code).
if p_code = "" then
do:       
srecid=?.
p_code = "".
RUN adeuib/_accsect.p 
      ('GET':U, procid, 'XFTR:Foreign Keys':U,
       INPUT-OUTPUT srecid,
       INPUT-OUTPUT p_code).
if p_code <> "" then
do:
    k=6.
    cresult = devuelve-linea(p_code,k).
    if cresult matches "*FOREIGN*"  then
            cresult = "".   
    do while cresult <> "" :
        if lookup ( entry(4,cresult,"|") , lista_campos) <> 0 then
        resultado = resultado + entry(4,cresult,"|") + "," + "" + "," + "" + chr(10). 
        k = k + 1.
        cresult = devuelve-linea(p_code,k).
        if cresult matches "*FOREIGN*"  then
            cresult = "".
    end.
    if resultado <> "" then
        srecid = ?.
        RUN adeuib/_accsect.p 
            ('SET':U, procid, 'XFTR:relaciones':U,
            INPUT-OUTPUT srecid,
            INPUT-OUTPUT resultado).
end.
end.

p_code = "DO:" + CHR(10) + "~{custom/support/validacion.i}" + chr(10) + 
         "END.".
run adeuib/_uibinfo.p (procid,?,"FRAMES",output cresult).
RUN adeuib/_uibinfo.p (?, cresult,input "context", output i_contex).
run adeuib/_uibinfo.p (?,input cresult,
                       input "contains fill-in",output lista_campos).
if lista_campos <> "" then
do k = 1 to num-entries(lista_campos) :               
    Run adeuib/_uibinfo.p(input integer(entry(k,lista_campos)),
                          input ?,  
                          input "context", output i_contex).
    srecid = ?.
    run adeuib/_accsect.p ("SET" , input i_contex, "TRIGGER:LEAVE", 
                           input-output srecid, input-output p_code).                              
end.
p_code = "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos Procedure 
PROCEDURE descriptivos :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilitar_relacion Procedure 
PROCEDURE habilitar_relacion :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION devuelve-linea Procedure 
FUNCTION devuelve-linea RETURNS CHARACTER
  ( input lista as character , input linea as integer) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
define var i as integer no-undo.
define var cresult as character no-undo initial "".
define var conta as integer no-undo initial 1.

do i = 1 to length(lista):
    if substring(lista,i,1) = chr(10) then
        conta = conta + 1.
    if conta > linea then leave.    
    if conta = linea then    
        cresult = cresult + substring(lista,i,1).
end.    
RETURN cresult.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retorna-campo Procedure 
FUNCTION retorna-campo RETURNS CHARACTER
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


