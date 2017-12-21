&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*-------------------------------------------------------------------------
    File        : viewer.i  
    Purpose     : Basic SmartViewer methods for the ADM
  
    Syntax      : {src/adm/method/viewer.i}

    Description :
  
    Author(s)   :
    Created     :
    Notes       :
    HISTORY: 
-------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define var modo-alta as logical no-undo initial false.
define var flag-valida as logical no-undo initial false.
DEFINE VAR HVINCULADO AS HANDLE NO-UNDO.

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

&IF DEFINED(EXCLUDE-validacion_frame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validacion_frame Method-Library 
FUNCTION validacion_frame RETURNS LOGICAL () FORWARD.

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
         HEIGHT             = 6.86
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{custom/method/hcontenedor.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-adm-create-objects) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects Method-Library _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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

define var field-group as handle no-undo.
define var cur-control as handle no-undo.
field-group = frame {&FRAME-NAME}:first-child.
cur-control = field-group:first-tab-item.

do while valid-handle(cur-control) :
    if cur-control:name = campo then
    do:
        assign cur-control:screen-value = valor.
        leave.
    end.
    cur-control = cur-control:next-tab-item.    
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-asigna-campo-extent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-campo-extent Method-Library 
PROCEDURE asigna-campo-extent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter campo as character.
define input parameter indice as integer.
define input parameter valor as character.

define var field-group as handle no-undo.
define var cur-control as handle no-undo.
field-group = frame {&FRAME-NAME}:first-child.
cur-control = field-group:first-tab-item.

do while valid-handle(cur-control) :
    if cur-control:name = campo and cur-control:index = indice then
    do:
        assign cur-control:screen-value = valor.
        leave.
    end.
    cur-control = cur-control:next-tab-item.    
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

define var field-group as handle no-undo.
define var cur-control as handle no-undo.

field-group = frame {&FRAME-NAME}:first-child.
cur-control = field-group:first-tab-item.

do while valid-handle(cur-control) :
    if cur-control:name = campo then
    do:
        assign valor = cur-control:screen-value.
        leave.
    end.
    cur-control = cur-control:next-tab-item.    
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-devuelve-rowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-rowid Method-Library 
PROCEDURE devuelve-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid.
&IF DEFINED(FIRST-EXTERNAL-TABLE) &THEN
r = rowid({&FIRST-EXTERNAL-TABLE}).
&ELSE
r = ?.
&ENDIF
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
&IF DEFINED(FIRST-EXTERNAL-TABLE) &THEN
    tabla = "{&FIRST-EXTERNAL-TABLE}".
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
define output parameter r as rowid no-undo.
&IF DEFINED(FIRST-EXTERNAL-TABLE) &THEN
find current {&FIRST-EXTERNAL-TABLE} no-lock no-error.
if available {&FIRST-EXTERNAL-TABLE} then
    r = rowid({&FIRST-EXTERNAL-TABLE}).
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

define var field-group as handle no-undo.
define var cur-control as handle no-undo.

field-group = frame {&FRAME-NAME}:first-child.
cur-control = field-group:first-tab-item.

do while valid-handle(cur-control) :
    if cur-control:name = campo then
    do:
        assign cur-control:sensitive = true.
        leave.
    end.
    cur-control = cur-control:next-tab-item.    
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
define var field-group as handle no-undo.
define var cur-control as handle no-undo.

field-group = frame {&FRAME-NAME}:first-child.
cur-control = field-group:first-tab-item.

do while valid-handle(cur-control) :
    if cur-control:name = campo then
    do:
        assign cur-control:sensitive = false.
        leave.
    end.
    cur-control = cur-control:next-tab-item.    
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
define var hcontainer as handle no-undo.
define var r as rowid no-undo.
    run get-container (output hcontainer).
    run dispatch in this-procedure ('pre-add') NO-ERROR.
    if valid-handle(hcontainer) then
        run dispatch in hcontainer ('pre-add') no-error.
    IF RETURN-VALUE <> '' AND RETURN-VALUE <> ? THEN RETURN 'ADM-ERROR'.
  /* Code placed here will execute PRIOR to standard behavior. */
  
  
  
  modo-alta = true.
  /* Dispatch standard ADM method. */
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
    run dispatch in this-procedure ('post-add').
    if valid-handle(hcontainer) then
        run dispatch in hcontainer ('post-add',this-procedure) no-error.

  modo-alta=false.
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

    run get-container(output h).
    run get-rowid1 in this-procedure (output r).  

  /* Code placed here will execute PRIOR to standard behavior. */
  DO:
      run dispatch in this-procedure (input 'pre-update').
      if RETURN-VALUE = "ADM-ERROR":U THEN UNDO ,RETURN "ADM-ERROR":U. 
      if valid-handle(h) then
        run pre-update in h (input r,input this-procedure).   
      if RETURN-VALUE = "ADM-ERROR":U THEN UNDO ,RETURN "ADM-ERROR":U. 
  END.      
 
  /* Dispatch standard ADM method.                           */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  if RETURN-VALUE = "ADM-ERROR" THEN UNDO , RETURN "ADM-ERROR".
  if not validacion_frame() then  UNDO , RETURN "ADM-ERROR":U.

 /* Code placed here will execute AFTER standard behavior.    */
 
  DO: 
      run dispatch ('post-update').
      if RETURN-VALUE = "ADM-ERROR":U THEN UNDO ,RETURN "ADM-ERROR":U. 
      if valid-handle(h) then
        run post-update in h (input r,input this-procedure). 
      if RETURN-VALUE = "ADM-ERROR":U THEN UNDO ,RETURN "ADM-ERROR":U. 
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
  define var l as logical.
  /* Code placed here will execute PRIOR to standard behavior. */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .
  /*
  &IF DEFINED(FIRST-EXTERNAL-TABLE) &THEN 
  if search("s_audito.i") <> ? then
      {s_audito.i "{&FIRST-EXTERNAL-TABLE}"}
  &ENDIF
  */
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
  run get-container(output h).
  
  DO:
    run dispatch in this-procedure(input 'pre-create'). 
    if RETURN-VALUE = "ADM-ERROR":U THEN UNDO , RETURN "ADM-ERROR":U.    
    if valid-handle(h) then
        run pre-create in h (input this-procedure). 
    if RETURN-VALUE = "ADM-ERROR":U THEN UNDO , RETURN "ADM-ERROR":U.    
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
  if RETURN-VALUE = "ADM-ERROR":U THEN UNDO , RETURN "ADM-ERROR":U.    

  /* Code placed here will execute AFTER standard behavior.    */
  DO:
      run dispatch in this-procedure(input 'post-create'). 
      if RETURN-VALUE = "ADM-ERROR":U THEN UNDO , RETURN "ADM-ERROR":U.    
      if valid-handle(h) then
        run post-create in h (input this-procedure).   
      if RETURN-VALUE = "ADM-ERROR":U THEN UNDO , RETURN "ADM-ERROR":U.    
  END.   
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
    run get-container(output h).
    run get-rowid1(output r).

  /* Code placed here will execute PRIOR to standard behavior. */
   message " Está seguro que borrará el registro ?" view-as alert-box question buttons yes-no 
   title "" update respuesta.
   if respuesta Then
   do ON ERROR UNDO , LEAVE :
      if valid-handle(h) then
        run pre-delete in h (input r , input this-procedure). 
        if return-value = "error" OR RETURN-VALUE = 'ADM-ERROR' then UNDO , LEAVE.  
        if return-value = 'next' then return.
        run dispatch in this-procedure (input 'pre-delete'). 
        if return-value = "error" OR RETURN-VALUE = 'ADM-ERROR' then UNDO , LEAVE.  
        if return-value = 'next' then return.
           
   /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    if valid-handle(h) then
        run post-delete in h (input this-procedure). 
        if return-value = "error" OR RETURN-VALUE = 'ADM-ERROR' then UNDO , LEAVE.  
        run dispatch in this-procedure (input 'post-delete').
        if return-value = "error" OR RETURN-VALUE = 'ADM-ERROR' then UNDO , LEAVE.  
  end.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-enable-fields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields Method-Library 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT ADM-NEW-RECORD THEN
  DO:
      DISABLE {&ADM-CREATE-FIELDS} WITH FRAME {&FRAME-NAME}.
      RUN ENABLE-FIELDS-VIEWER IN THIS-PROCEDURE NO-ERROR.
  END. 
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
  define var r as rowid no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */
  /* Dispatch standard ADM method.                             */

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'end-update':U ) .

  run get-rowid1 in this-procedure (output r).  
 
  run dispatch in this-procedure (input 'post-end-update') no-error.
  if RETURN-VALUE = "ADM-ERROR":U THEN UNDO ,RETURN "ADM-ERROR":U. 
 FIND CURRENT {&adm-first-enabled-table} NO-LOCK NO-ERROR.
 &IF "{&adm-second-enabled-table}":U NE "":U &THEN
      FIND CURRENT {&adm-second-enabled-table} NO-LOCK NO-ERROR.
 &ENDIF
 &IF "{&adm-third-enabled-table}":U NE "":U &THEN
      FIND CURRENT {&adm-third-enabled-table} NO-LOCK NO-ERROR.
 &ENDIF
  if valid-handle(hcontenedor) then
        run post-end-update in hcontenedor (input r ,input this-procedure ) no-error.   
  if RETURN-VALUE = "ADM-ERROR":U THEN UNDO ,RETURN "ADM-ERROR":U. 
 
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

&IF DEFINED(EXCLUDE-local-update-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record Method-Library 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  define var clnk as character no-undo.
 
  /* Dispatch standard ADM method.                           */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ).
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".


  /* Code placed here will execute AFTER standard behavior.    */
  run get-attribute ( 'alta-automatica' ) . 
  if return-value = 'yes' then
  do:
    run get-link-handle in adm-broker-hdl ( input THIS-PROCEDURE , 'TABLEIO-SOURCE' , output clnk ).
    if valid-handle(widget-handle(clnk)) then
        run agregar in widget-handle(clnk).
    
  end.

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
define input parameter cur-control as handle.
define output parameter valido as logical no-undo.

define var flag-control as logical no-undo.
define var color-normal as integer no-undo.
define var mensaje as character no-undo.

    apply "entry" to cur-control. 
    flag-control = false.
    
    if cur-control:bgcolor = ? then
    do:
        if cur-control:sensitive then
            color-normal = 15.
        else     
            color-normal = 8.
    end.        
        
    if lookup (cur-control:type , 'fill-in,editor' ) = 0 or not cur-control:modified then 
    do:
        valido = true.
        return.
    end.
    
    run get-attribute ('bg-' + cur-control:name).
    if return-value <> ? then
        color-normal = integer(return-value).
    else
        run set-attribute-list ('bg-' + cur-control:name + '=' + if cur-control:bgcolor = ? then string(color-normal) else string(cur-control:bgcolor) ).

    if not cur-control:validate() then
    DO:
        cur-control:bgcolor = 12.
        flag-control = true.
    END.        
    if  not valida(cur-control:name,cur-control:screen-value , output mensaje) then
    do:
        if mensaje <> '' and mensaje <> ?  then 
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

&IF DEFINED(EXCLUDE-validacion_frame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validacion_frame Method-Library 
FUNCTION validacion_frame RETURNS LOGICAL ():
define var field-group as handle.
define var cur-control as handle.
define var hfirst  as handle no-undo.
define var flag-control as logical no-undo.
define var color-normal as integer no-undo.
define var valido as logical no-undo.
define var valida-frame as logical no-undo.

flag-valida = true.
field-group = frame {&FRAME-NAME}:first-child.
cur-control = field-group:first-tab-item.
hfirst = cur-control.

valida-frame = true.
do while valid-handle(cur-control): 
    if lookup (cur-control:type , "fill-in,editor") <> 0 then 
    do:
        run validacion-field ( cur-control , output valido ) .        
        if not valido then 
            valida-frame = false.
    end.        
    cur-control = cur-control:next-tab-item.
end.

if not valida-frame then
do:
    if valid-handle(hfirst) then
    do:
        apply 'entry' to hfirst.
        return false.
    end.    
end.    
 

flag-valida = false.

RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

