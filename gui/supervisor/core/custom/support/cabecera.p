&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS xftrprocedure 
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
define var procid as integer no-undo.
define var cresult as character no-undo.
define var cabecera as character no-undo.
define var detalle as character no-undo.
define var items as character no-undo.
define var srecid as integer no-undo.
define var tabla-cabecera as character no-undo.
define var tabla-detalle as character no-undo.
define var tabla-items as character no-undo.
define var cnt as integer no-undo.
define var x_code as character no-undo.
define var flag as logical no-undo.
define var i as integer no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE xftrprocedure



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: xftrprocedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW xftrprocedure ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK xftrprocedure 


/* ***************************  Main Block  *************************** */
RUN adeuib/_uibinfo.p (p_contextID, ?, "PROCEDURE", OUTPUT cresult).
/* Is this being run in a TEMPLATE.  If so, then don't bother doing 
   anything. */
assign procid = integer(cresult).
RUN adeuib/_uibinfo.p (procid, ?, "TEMPLATE":U, OUTPUT cResult).
IF cResult = "yes":U THEN return.


p_code = replace(p_code,"/*","").
p_code = replace(p_code,"*/","").

cabecera = entry(1,p_code).
detalle  = entry(2,p_code).
items    = entry(3,p_code).
tabla-cabecera = cabecera.
tabla-detalle = detalle.
tabla-items = items.
run custom/support/wcabecera.w ( input-output cabecera , 
                                 input-output detalle ,
                                 input-output items ).

if cabecera <> "" and detalle <> "" and items <> "" then
do:
    srecid = ?.
    run adeuib/_accsect.p ("GET", procid,"DEFINITIONS",
                       input-output srecid ,
                       input-output p_code).
    cnt = num-entries(p_code,chr(10)).    
    x_code = "".
    flag = true.
    do i = 1 to cnt :
        if entry(i,p_code,chr(10)) matches "*EMPIEZA-TEMP-TABLES*" then
            flag = false.
        if entry(i,p_code,chr(10)) matches "*TERMINA-TEMP-TABLES*" then
        do:
            flag = true.
            next.
        end.
        if flag then
            x_code = x_code + entry(i,p_code,chr(10)) + chr(10).   
    
    end.
    &ANALYZE-SUSPEND
    p_code = "/**********EMPIEZA-TEMP-TABLES*********/" + chr(10) +
         "&SCOPED-DEFINE TABLA-CABECERA " + cabecera + chr(10) +
         "&SCOPED-DEFINE TABLA-DETALLE " + detalle + chr(10) +
         "&SCOPED-DEFINE TABLA-ITEMS " + items + chr(10) +
         "~{custom/support/temp-tables.i ~&detalle=" + '~{&TABLA-DETALLE}' + chr(10) +
         "                               ~&items=" + '~{&TABLA-ITEMS}' + "~}" + chr(10) +
         "/**********TERMINA-TEMP-TABLES*********/".
    &ANALYZE-RESUME         
    p_code = x_code + chr(10) + p_code .
    srecid = ?.
    run adeuib/_accsect.p ("SET", procid,"DEFINITIONS",
                       input-output srecid ,
                       input-output p_code).

/*    p_code = "~{custom/support/gendetalle.i &cabecera=" + cabecera + chr(10) +
             "                &detalle=" + detalle + "}".
    p_code = p_code + chr(10) + "END PROCEDURE.".
    srecid=?.
    RUN adeuib/_accsect.p 
      ('SET':U, procid, 'Procedure:genero-detalle-interno':U,
       INPUT-OUTPUT srecid,
       INPUT-OUTPUT p_code).

    p_code = "~{custom/support/genitems.i &detalle=" + detalle + chr(10) +
             "                &items=" + items + "}".
    p_code = p_code + chr(10) + "END PROCEDURE.".
    srecid=?.
    RUN adeuib/_accsect.p 
      ('SET':U, procid, 'Procedure:genero-items-interno':U,
       INPUT-OUTPUT srecid,
       INPUT-OUTPUT p_code).

    p_code = "~{custom/support/deldetalle.i &cabecera=" + cabecera + chr(10) +
             "             &detalle=" + detalle + chr(10) +
             "             &items=" + items + chr(10) +
             "             &bloque-borrado=}".
    p_code = p_code + chr(10) + "END PROCEDURE.".
    srecid=?.
    RUN adeuib/_accsect.p 
      ('SET':U, procid, 'Procedure:proceso-cabecera-detalle-interno':U,
       INPUT-OUTPUT srecid,
       INPUT-OUTPUT p_code).

    p_code = "~{custom/support/delitems.i &detalle=" + detalle + chr(10) +
             "             &items=" + items + chr(10) +
             "             &accion=si" + chr(10) +
             "             &bloque-borrado=}".
    p_code = p_code + chr(10) + "END PROCEDURE.".
    srecid=?.
    RUN adeuib/_accsect.p 
      ('SET':U, procid, 'Procedure:proceso-detalle-items-interno':U,
       INPUT-OUTPUT srecid,
       INPUT-OUTPUT p_code).
*/       
end.       
p_code = "/*" + (if cabecera <> "" then cabecera else "cabecera") + "," + 
         (if detalle <> "" then detalle else "detalle") + "," + 
         (if items <> "" then items else "items") + "*/".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


