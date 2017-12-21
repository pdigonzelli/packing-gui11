&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-validacion_browse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validacion_browse Method-Library 
FUNCTION validacion_browse RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 6.81
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-asigna-campo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-campo Method-Library 
PROCEDURE asigna-campo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter campo as character.
define input parameter valor as character.

define var cur-control as handle no-undo.

cur-control = browse {&BROWSE-NAME}:first-column.

do while valid-handle(cur-control) :
    if cur-control:name = campo then
    do:
        assign cur-control:screen-value = valor.
        leave.
    end.
    cur-control = cur-control:next-column.    
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-devuelve-campo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-campo Method-Library 
PROCEDURE devuelve-campo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter campo as character.
define output parameter valor as character.

define var cur-control as handle no-undo.

cur-control = browse {&BROWSE-NAME}:first-column.

do while valid-handle(cur-control) :
    if cur-control:name = campo then
    do:
        assign valor = cur-control:screen-value .
        leave.
    end.
    cur-control = cur-control:next-column.    
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-habilita-campo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilita-campo Method-Library 
PROCEDURE habilita-campo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter campo as character.

define var cur-control as handle no-undo.

cur-control = browse {&BROWSE-NAME}:first-column.

do while valid-handle(cur-control) :
    if cur-control:name = campo then
    do:
        assign cur-control:sensitive = true.
        leave.
    end.
    cur-control = cur-control:next-column.    
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-inhibe-campo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inhibe-campo Method-Library 
PROCEDURE inhibe-campo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter campo as character.

define var cur-control as handle no-undo.

cur-control = browse {&BROWSE-NAME}:first-column.

do while valid-handle(cur-control) :
    if cur-control:name = campo then
    do:
        assign cur-control:sensitive = false.
        leave.
    end.
    cur-control = cur-control:next-column.    
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-add-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record Method-Library 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
define var h as handle no-undo.
define var ch as character no-undo.
define var r as rowid no-undo.
    run get-link-handle in adm-broker-hdl (input THIS-PROCEDURE, input "CONTAINER-SOURCE", output ch).
    h=widget-handle(ch).
  

  /* Code placed here will execute PRIOR to standard behavior. */
  if valid-handle(h) then
    run pre-create in h (input this-procedure). 
    run dispatch in this-procedure (input 'pre-create').
  
  run get-attribute ('modo-insert').
  if return-value = "final" then
    run dispatch IN THIS-PROCEDURE ( INPUT 'get-last').
  /* Dispatch standard ADM method. 
                              */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(h) then
    run post-create in h (input this-procedure).   
    run dispatch in this-procedure(input 'post-create').  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-cancel-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record Method-Library 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
define var h as handle no-undo.
define var ch as character no-undo.
define var r as rowid no-undo.
run get-link-handle in adm-broker-hdl (input THIS-PROCEDURE, input "CONTAINER-SOURCE", output ch).
h=widget-handle(ch).
  

  /* Code placed here will execute PRIOR to standard behavior. */
  if valid-handle(h) then
    run resetea-registro in h (input this-procedure).
  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-copy-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record Method-Library 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
define var h as handle no-undo.
define var ch as character no-undo.
define var r as rowid no-undo.
run get-link-handle in adm-broker-hdl (input THIS-PROCEDURE, input "CONTAINER-SOURCE", output ch).
h=widget-handle(ch).
  

  /* Code placed here will execute PRIOR to standard behavior. */
  if valid-handle(h) then
    run pre-copy in h (input this-procedure). 
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(h) then
    run post-copy in h (input this-procedure).   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-delete-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record Method-Library 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
define var h as handle no-undo.
define var ch as character no-undo.
define var r as rowid no-undo.

    run get-link-handle in adm-broker-hdl (input THIS-PROCEDURE, input "CONTAINER-SOURCE", output ch).
    h=widget-handle(ch).    
    run get-rowid(output r).

  /* Code placed here will execute PRIOR to standard behavior. */
  if valid-handle(h) then
    run pre-delete in h (input r , input this-procedure). 
    run dispatch in this-procedure (input 'pre-delete').    
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(h) then
    run post-delete in h (input this-procedure).  
    run dispatch in this-procedure (input 'post-delete').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-update-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record Method-Library 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
define var h as handle no-undo.
define var ch as character no-undo.
define var r as rowid no-undo.

   if not validacion_browse() then return. 
  /* Code placed here will execute PRIOR to standard behavior. */
    run get-link-handle in adm-broker-hdl (input THIS-PROCEDURE, input "CONTAINER-SOURCE", output ch).
    h=widget-handle(ch).
    run get-rowid(output r).


  /* Code placed here will execute PRIOR to standard behavior. */
  if valid-handle(h) then
    run pre-update in h (input r,input this-procedure).   
    run dispatch in this-procedure (input 'pre-update').
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(h) then
    run post-update in h (input r,input this-procedure).   
    run dispatch in this-procedure(input 'post-update').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create Method-Library 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-delete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete Method-Library 
PROCEDURE post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-update) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update Method-Library 
PROCEDURE post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pre-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create Method-Library 
PROCEDURE pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pre-delete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete Method-Library 
PROCEDURE pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pre-update) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update Method-Library 
PROCEDURE pre-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-validacion_browse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validacion_browse Method-Library 
FUNCTION validacion_browse RETURNS LOGICAL
  ( /* parameter-definitions */ ) :


define var cur-column as handle.
define var mensaje as character no-undo.
define var l as logical no-undo.

cur-column = browse {&BROWSE-NAME}:first-column.
do while valid-handle(cur-column): 
    apply "entry" to cur-column.
    if  cur-column:type = "fill-in" then 
    do:
        l = dynamic-function("valida" , cur-column:name , cur-column:screen-value , output mensaje) no-error.
        if l <> ? and not l  then
        do:
            message mensaje view-as alert-box. 
            apply "entry" to cur-column.
            return false.
        end.   
        cur-column = cur-column:next-column.
    end.
end.
cur-column = browse {&BROWSE-NAME}:first-column.
apply "entry" to cur-column.

RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

