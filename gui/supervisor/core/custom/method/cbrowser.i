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

&IF DEFINED(EXCLUDE-new-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD new-record Method-Library 
FUNCTION new-record RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validacion_browse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validacion_browse Method-Library 
FUNCTION validacion_browse RETURNS LOGICAL () FORWARD.

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
         HEIGHT             = 6.76
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{custom/method/hcontenedor.i}

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


cur-control = {&BROWSE-NAME}:first-column in frame {&FRAME-NAME}.

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

&IF DEFINED(EXCLUDE-devuelve-tabla) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-tabla Method-Library 
PROCEDURE devuelve-tabla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter tabla as character.
&IF DEFINED(FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}) &THEN
    tabla = "{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}".
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-container) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-container Method-Library 
PROCEDURE get-container :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter h_container as handle.

define var ccontainer as character no-undo.
define var h as handle no-undo.

run get-link-handle in adm-broker-hdl (this-procedure , 'CONTAINER-SOURCE' , output ccontainer).
h = widget-handle(ccontainer).
if valid-handle (h) then
    h_container = h .
else
    h_container = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-rowid1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid1 Method-Library 
PROCEDURE get-rowid1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid.
&IF DEFINED(FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}) &THEN
find current {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} no-error.
if available {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} then
    r = rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
else
    r = ?.
&ENDIF    
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
define var r as rowid no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */
  run get-attribute ('modo-insert').
  if return-value = "final" then
    run dispatch IN THIS-PROCEDURE ( INPUT 'get-last').
  /* Dispatch standard ADM method. */
  RUN pre-add in this-procedure no-error.
  IF RETURN-VALUE <> '' AND RETURN-VALUE <> ? THEN RETURN 'ADM-ERROR'.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-assign-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record Method-Library 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    define var h as handle no-undo.
    define var ch as character no-undo.
    define var r as rowid no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  if not validaCion_browse() then return 'ADM-ERROR'. 

    run get-container(output h).
    run get-rowid1(output r).

  /* Code placed here will execute PRIOR to standard behavior. */

  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  if RETURN-VALUE = "ADM-ERROR":U THEN UNDO , RETURN "ADM-ERROR":U.    
  
    DO:
        run dispatch in this-procedure (input 'pre-update').
        if RETURN-VALUE = "ADM-ERROR":U THEN UNDO , RETURN "ADM-ERROR":U.    
        if valid-handle(h) then
        run pre-update in h (input r,input this-procedure) no-error.   
        if RETURN-VALUE = "ADM-ERROR":U THEN UNDO ,RETURN "ADM-ERROR":U.    
    END.


  /* Code placed here will execute AFTER standard behavior.    */
    DO:
        run dispatch in this-procedure(input 'post-update').
        if RETURN-VALUE = "ADM-ERROR":U THEN UNDO , RETURN "ADM-ERROR":U.    

        if valid-handle(h) then
            run post-update in h (input r,input this-procedure) no-error.   
        if RETURN-VALUE = "ADM-ERROR":U THEN UNDO , RETURN "ADM-ERROR":U.    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-assign-statement) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement Method-Library 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method. */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .
  &IF DEFINED(FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}) &THEN
 /* if search("s_audito.i") <> ? then
      {s_audito.i "{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}"}  */
  &ENDIF
  /* Code placed here will execute AFTER standard behavior.    */

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

&IF DEFINED(EXCLUDE-local-create-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record Method-Library 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
define var h as handle no-undo.
define var ch as character no-undo.
define var r as rowid no-undo.


  /* Code placed here will execute PRIOR to standard behavior. */
  run get-container (output h).
  if valid-handle(h) then
    run pre-create in h (input this-procedure) no-error. 
    run dispatch in this-procedure (input 'pre-create').

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  if valid-handle(h) then
    run post-create in h (input this-procedure) no-error.   
    run dispatch in this-procedure(input 'post-create').  

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
define var respuesta as logical initial true.
    run get-link-handle in adm-broker-hdl (input THIS-PROCEDURE, input "CONTAINER-SOURCE", output ch).
    h=widget-handle(ch).    
    run get-rowid1(output r).

  /* Code placed here will execute PRIOR to standard behavior. */
  message " Está seguro que borrará el registro ?" view-as alert-box question buttons yes-no 
   title "" update respuesta.
  if respuesta Then
  do ON ERROR UNDO , LEAVE:
      if valid-handle(h) then
            run pre-delete in h (input r , input this-procedure) no-error. 
      if return-value = "error" then UNDO , LEAVE.  
        run dispatch in this-procedure (input 'pre-delete').    

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    if valid-handle(h) then
        run post-delete in h (input this-procedure) no-error.  
        run dispatch in this-procedure (input 'post-delete').
  end.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-end-update) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-end-update Method-Library 
PROCEDURE local-end-update :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'end-update':U ) .

  RUN dispatch IN THIS-PROCEDURE ('POST-END-UPDATE') NO-ERROR.
  /* Code placed here will execute AFTER standard behavior.    */
  run get-attribute ( 'alta-automatica' ) . 
  if return-value = 'yes' then
  do:
    run dispatch ('add-record').
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-open-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query Method-Library 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  &IF DEFINED(CON-PARAMETROS) &THEN 
  
  define var lista-parametros as character no-undo.
  if valid-handle(hcontenedor) then
  do:    
        run get-parametros in hcontenedor ( output lista-parametros).
        run asigna-parametros(input lista-parametros).
  end.
  &ENDIF
  /* Dispatch standard ADM method.                           */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-reset-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-reset-record Method-Library 
PROCEDURE local-reset-record :
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'reset-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-row-changed) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed Method-Library 
PROCEDURE local-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/support/crow-available.i}
  /* Dispatch standard ADM method.    
                           */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
   define var clnk as character no-undo.
   define var r as rowid no-undo.
   
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".

  /* Code placed here will execute AFTER standard behavior.    */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-repone-query) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repone-query Method-Library 
PROCEDURE repone-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid.

reposition {&BROWSE-NAME} TO rowid r.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validacion-field) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validacion-field Method-Library 
PROCEDURE validacion-field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT    PARAMETER CUR-CONTROL AS HANDLE NO-UNDO.
DEFINE OUTPUT   PARAMETER VALIDO AS LOGICAL     NO-UNDO.

define var flag-control as logical no-undo.
define var color-normal as integer no-undo.
define var mensaje as character no-undo.


        if lookup (cur-control:type , 'fill-in,editor' ) = 0 or cur-control:modified then
        do:
            valido = true .
            return.
        end.
        apply "entry" to cur-control. 
        COLOR-NORMAL = BROWSE {&BROWSE-NAME}:BGCOLOR.

        flag-control = false.
        run get-attribute ('bg-' + cur-control:name).
        if return-value <> ? then
            color-normal = integer(return-value).
        else
            run set-attribute-list ('bg-' + cur-control:name + '=' + if cur-control:bgcolor = ? then "8" else string(cur-control:bgcolor) ).

        if not cur-control:validate() then
        DO:
            cur-control:bgcolor = 12.
            flag-control = true.
        END.        
        if  not valida(cur-control:name,cur-control:screen-value , output mensaje) then
        do:
            if mensaje <> '' and mensaje <> ? then 
                message mensaje view-as alert-box error.
            cur-control:bgcolor = 12.
            flag-control = true.
        end.   
        if not flag-control then
            cur-control:bgcolor = color-normal.
            
        valido = not flag-control.   


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-new-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION new-record Method-Library 
FUNCTION new-record RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RUN get-attribute ("ADM-NEW-RECORD").
  if return-value = 'yes' then
    return true.
    
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validacion_browse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validacion_browse Method-Library 
FUNCTION validacion_browse RETURNS LOGICAL ():
define var cur-column as handle     no-undo.
define var cur-column1 as handle    no-undo.
define var valido as logical        no-undo.
define var valido-frame as logical  no-undo.

IF VALID-HANDLE ( browse {&BROWSE-NAME}:CURRENT-COLUMN ) then
    cur-column1 = browse {&BROWSE-NAME}:CURRENT-COLUMN.
else    
    cur-column1 = browse {&BROWSE-NAME}:first-column.
    
assign cur-column = browse {&BROWSE-NAME}:first-column.

assign  valido = true
        valido-frame = true.
        
do while valid-handle(cur-column): 
    apply "entry" to cur-column.
    if  cur-column:type = "fill-in" then 
        run validacion-field ( cur-column , output valido).
    if not valido then
        valido-frame = false.
    cur-column = cur-column:next-column.
end.

if not valido-frame then
do:
    apply 'entry' to cur-column1.
    return false.
end.

RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

