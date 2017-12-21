&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */
define var lista as character .
define var xx as handle.
define var i as integer no-undo.

run select-page(1).
run get-link-handle in adm-broker-hdl ( input this-procedure , 'PAGE1-TARGET' ,output lista).
do i = 1 to num-entries(lista) :
    xx = widget-handle(entry(i,lista)).
    run get-attribute in xx ('tipo-detalle').
    if return-value = 'cabecera'  then
    do:
                hcabecera = xx.
                leave .
    end.    
end.        
run select-page(2).
run get-link-handle in adm-broker-hdl ( input this-procedure , 'PAGE2-TARGET' ,output lista).
do i = 1 to num-entries(lista) :
    xx = widget-handle(entry(i,lista)).
    run get-attribute in xx ('tipo-detalle').
    if return-value = 'detalle'  then
    do:
                hdetalle = xx.
                leave .
    end.    
end.        

run select-page(3) no-error.
run get-link-handle in adm-broker-hdl ( input this-procedure , 'PAGE3-TARGET' ,output lista).
do i = 1 to num-entries(lista) :
    xx = widget-handle(entry(i,lista)).
    run get-attribute in xx ('tipo-detalle').
    if return-value = 'detalle'  then
    do:
                hdetalle = xx.
                leave .
    end.    
    if return-value = 'items'  then
    do:
                hitemsdet = xx.
                leave .
    end.    
end.        

run select-page(1).
run get-link-handle in adm-broker-hdl ( input this-procedure , 'PAGE-SOURCE' ,output lista).
do i = 1 to num-entries(lista) :
    xx = widget-handle(entry(i,lista)).
    run get-attribute in xx ('type').
    if return-value = "smartfolder" then
    do:
        hfolder = xx.
        leave.
    end.
end.
run set-attribute-list('tipo-window=cabecera-detalle').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


