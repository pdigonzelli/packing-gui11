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
define var respuesta as logical.
define var r as rowid no-undo.
define buffer aux-detalle for {&detalle}.

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
respuesta = true.
if "{&accion}" = "si" then
do:
    message "Cambiaron algunos datos . Desea aceptar el cambio ? " view-as alert-box 
    question buttons yes-no  update respuesta .
end.
if not respuesta then
do:
        find first tt-items no-error.
        find aux-detalle of tt-items no-lock.
        if "{&bloque-borrado}" <> "" then
        do:
            {&bloque-borrado}.
        end.
        else
        do:
            if available aux-detalle then
                for each {&items} of aux-detalle:
                    delete {&items}.
                end.
        end.    
        for each tt-items : 
                create {&items}.
                buffer-copy  tt-items to {&items}.
        end.  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


