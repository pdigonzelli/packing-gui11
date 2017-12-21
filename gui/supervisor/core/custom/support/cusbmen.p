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

define var lista_campos as character no-undo.
define var lista_titulos as character no-undo.

define var i_contex as character no-undo.
define var procid as integer no-undo.
define var cresult as character no-undo.
define var i as integer no-undo.
define var srecid as integer  no-undo.
define var h as handle no-undo.
define var coneccion as character no-undo.
define var f_code as character no-undo.
define var x_code as character no-undo.
define var flag as logical no-undo.
define var cnt as integer no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retorna-campo Procedure 
FUNCTION retorna-campo RETURNS CHARACTER
  ( input c as character , input j as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retorna-tabla Procedure 
FUNCTION retorna-tabla RETURNS CHARACTER
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
         HEIGHT             = 1.91
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
srecid = ?.
run adeuib/_accsect.p ("GET", procid,"MAIN-CODE-BLOCK",
                       input-output srecid ,
                       input-output p_code).
                       
cnt = num-entries(p_code,chr(10)).    
x_code = "".
flag = true.
do i = 1 to cnt :
    if entry(i,p_code,chr(10)) matches "*EMPIEZA MENU*" then
        flag = false.
    if entry(i,p_code,chr(10)) matches "*TERMINA MENU*" then
    do:
        flag = true.
        next.
    end.
    if flag then
        x_code = x_code + entry(i,p_code,chr(10)) + chr(10).   
    
end.
run adeuib/_uibinfo.p (?,?,
                       input "FIELDS",
                       output lista_campos).

cresult = "".                       
do i = 1 to num-entries(lista_campos) :
  if not entry(i,lista_campos) matches "*[*" then
    if i = num-entries(lista_campos) then 
        cresult = cresult + entry(i,lista_campos).
    else
        cresult = cresult + entry(i,lista_campos) + ",". 
end.

if length(cresult) > 0 Then
 do:
 if substring(cresult,length(cresult),1) = "," then
     cresult = substring(cresult,1,(length(cresult) - 1)).
 end.     

lista_campos = cresult.

/******Carga lista con labels de los campos *********/
do i = 1 to num-entries(lista_campos) :
    run custom/support/create_alias.p ( input retorna-tabla(entry(i,lista_campos),1)).
    run custom/support/men_lista.p ( i , lista_campos , input-output lista_titulos).
end.

RUN adeuib/_uibinfo.p (?, ?, "TABLES", OUTPUT cResult).
if lista_campos <> "" then
do:
    &ANALYZE-SUSPEND
    p_code = "/******** EMPIEZA MENU ************/".
    p_code = p_code + chr(10) + "define sub-menu Ordena " + chr(10).
    do i = 1 to num-entries(lista_campos) :
        p_code = p_code + 
                 "  menu-item " + 
                 retorna-campo(retorna-tabla(entry(i,lista_campos),2),1) + "-" +
                 retorna-campo(entry(i,lista_campos),2) + chr(10) +
                 "label " + "'" + retorna-campo(entry(i,lista_campos),2) + "( " +
                 retorna-campo(retorna-tabla(entry(i,lista_campos),2),1) + " ) " + "'" + chr(10) +
                  "          triggers: " + chr(10) +
                  "              on choose " + chr(10) +
                  "                       do:" + Chr(10).
        p_code = p_code + 
                 "                  run set-attribute-list('orden=" + retorna-campo(entry(i,lista_campos),1) + "')." + chr(10).

        p_code = p_code + 
                 "                  run set-attribute-list('titulo=" + entry(i,lista_titulos) + "')." + chr(10).


        if entry(1,cresult) = retorna-tabla(entry(i,lista_campos),2) then
        p_code = p_code + 
                 "                  run set-attribute-list('busca= yes')." + chr(10).

        else    
        p_code = p_code + 
                 "                  run set-attribute-list('busca= no')." + chr(10).

        p_code = p_code +
                 "                  &SCOPE SORTBY-PHRASE  by " + retorna-campo(entry(i,lista_campos),1) 
                                    + CHR(10).

        p_code = p_code +                             
                 "                  ~{&OPEN-QUERY-~{&BROWSE-NAME}} " + chr(10) +
                 "              end." + chr(10).  

        p_code = p_code +
                 "          end." + chr(10).
                 
    end.
     p_code = p_code + 
        "&SCOPE SORTBY-PHRASE by " + retorna-campo(entry(1,lista_campos),1) + chr(10) +
        "~{&OPEN-QUERY-~{&BROWSE-NAME}}" + chr(10) + 
        "define menu m-accesorios sub-menu Ordena" + chr(10) +
        "   menu-item Busca " + chr(10) +
        "        triggers:  " + chr(10) + 
        "          on choose apply 'F3' TO ~{&BROWSE-NAME} in frame ~{&FRAME-NAME}." + chr(10) + 
        "        end." + chr(10) + 
        "menu m-accesorios:popup-only = true." + chr(10) +
        "~{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle." + chr(10) +
        "run set-attribute-list('orden=" + retorna-campo(entry(1,lista_campos),1) + "')." + chr(10) + 
        "run set-attribute-list('titulo=" + entry(1,lista_titulos) + "').". 
        
    &ANALYZE-RESUME
    p_code = p_code + chr(10) +
             "/********** TERMINA MENU ***********/".
    p_code = x_code + chr(10) + p_code .
    srecid = ?.
    run adeuib/_accsect.p ("SET", procid,"MAIN-CODE-BLOCK",
                       input-output srecid ,
                       input-output p_code).
     
end.    

RUN adeuib/_uibinfo.p (?, ?, "TABLES", OUTPUT cResult).
define var encuentra as character no-undo initial "find first ~{&FIRST-TABLE-IN-QUERY-~{&BROWSE-NAME~}~} where ".
if lista_campos <> "" then
do:
    f_code ="".
    f_code = f_code + chr(10) + "define input parameter p as character no-undo." + chr(10) +
                                "define input parameter valor as character no-undo.".
    f_code = f_code + chr(10) + "case p:" + chr(10).
    do i = 1 to num-entries(lista_campos) :
        run custom/support/create_alias.p ( input retorna-tabla(entry(i,lista_campos),1)).
        run custom/support/men_buscar.p ( i , lista_campos , cresult , encuentra , input-output f_code).
    end.        
    f_code = f_code + "end." + chr(10) + "END PROCEDURE.".
    srecid = ?.
    run adeuib/_accsect.p ("SET", procid,"PROCEDURE:busca-registro",
                       input-output srecid ,
                       input-output f_code).
end.    

p_code = "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retorna-campo Procedure 
FUNCTION retorna-campo RETURNS CHARACTER
  ( input c as character , input j as integer) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
define var r as character no-undo initial "".
define var i as integer no-undo.
define var cuenta as integer initial 0 no-undo.

do i = 1 to length(c):
    if cuenta >= j then
        r = r + substring(c,i,1).
    if substring(c,i,1) = "." then
        cuenta = cuenta + 1.
         
end.
return r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retorna-tabla Procedure 
FUNCTION retorna-tabla RETURNS CHARACTER
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


