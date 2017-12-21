&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE SHARED VAR rSession  AS ROWID        NO-UNDO.
DEFINE VARIABLE hSession    AS HANDLE       NO-UNDO.
DEFINE VAR iEmpresa         AS INTEGER      NO-UNDO.
DEFINE VAR iUnidad          AS INTEGER      NO-UNDO.
DEFINE VAR iSucursal        AS INTEGER      NO-UNDO.
DEFINE VAR hSysEmp          AS HANDLE       NO-UNDO.
DEFINE VAR hSysDate         AS HANDLE       NO-UNDO.
DEFINE VAR cEmpresa         AS CHARACTER    NO-UNDO.
DEFINE VAR cUnidad          AS CHARACTER    NO-UNDO.
DEFINE VAR cSucursal        AS CHARACTER    NO-UNDO.
DEFINE VAR cUsuario         AS CHARACTER    NO-UNDO.
DEFINE VAR cHoraInicio      AS CHARACTER    NO-UNDO.
DEFINE VAR cHoraFinal       AS CHARACTER    NO-UNDO.
DEFINE VAR hTT              AS HANDLE       NO-UNDO.
DEFINE VAR bhTT             AS HANDLE       NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getEmpresa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEmpresa Procedure 
FUNCTION getEmpresa RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNumericFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNumericFormat Procedure 
FUNCTION getNumericFormat RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSucursal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursal Procedure 
FUNCTION getSucursal RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnidad Procedure 
FUNCTION getUnidad RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEmpresa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEmpresa Procedure 
FUNCTION setEmpresa RETURNS LOGICAL
  ( INPUT iEmp AS INTEGER  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNumericFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNumericFormat Procedure 
FUNCTION setNumericFormat RETURNS LOGICAL
  ( INPUT cNumericFormat AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSucursal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSucursal Procedure 
FUNCTION setSucursal RETURNS LOGICAL
  ( INPUT iSuc AS INTEGER  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUnidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUnidad Procedure 
FUNCTION setUnidad RETURNS LOGICAL
  ( INPUT iUni AS INTEGER  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

hSession = SESSION:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-displaySession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displaySession Procedure 
PROCEDURE displaySession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hSysEmp:SCREEN-VALUE = cEmpresa + '-' + cUnidad + '-' + cSucursal.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-readSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE readSession Procedure 
PROCEDURE readSession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR hServer AS HANDLE NO-UNDO.
DEFINE VAR qhTT AS HANDLE NO-UNDO.

hServer = SESSION:LAST-SERVER.

IF VALID-HANDLE(hServer) THEN
    RUN adm2/support/createSession.p ON hServer TRANSACTION DISTINCT (OUTPUT hTT).
ELSE
    RUN adm2/support/createSession.p (OUTPUT hTT).

bhTT = hTT:DEFAULT-BUFFER-HANDLE.
CREATE QUERY qhTT.
qhTT:SET-BUFFERS(bhTT).
qhTT:QUERY-PREPARE('for each ttSesiones no-lock').
qhTT:QUERY-OPEN().

REPEAT :
    qhTT:GET-NEXT().
    IF qhTT:QUERY-OFF-END THEN
    DO:
        LEAVE.
    END.
END.
qhTT:QUERY-CLOSE().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-writeSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE writeSession Procedure 
PROCEDURE writeSession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR hServer AS HANDLE NO-UNDO.

hServer = SESSION:LAST-SERVER.

IF VALID-HANDLE(hServer) THEN
    RUN adm2/support/writeSession.p ON hServer TRANSACTION DISTINCT (INPUT hTT).
ELSE
    RUN adm2/support/writeSession.p (INPUT hTT).
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getEmpresa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEmpresa Procedure 
FUNCTION getEmpresa RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR hf AS HANDLE NO-UNDO.
 
  RUN readSession.
  hf = bhTT:BUFFER-FIELD('idEmpresa').
  iEmpresa = hf:BUFFER-VALUE.
  RETURN iEmpresa.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNumericFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNumericFormat Procedure 
FUNCTION getNumericFormat RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN SESSION:NUMERIC-FORMAT.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSucursal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursal Procedure 
FUNCTION getSucursal RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR hf AS HANDLE NO-UNDO.

  RUN readSession.
  hf = bhTT:BUFFER-FIELD('idSucursal').
  iSucursal = hf:BUFFER-VALUE.

  RETURN iSucursal.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnidad Procedure 
FUNCTION getUnidad RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR hf AS HANDLE NO-UNDO.

  RUN readSession.
  hf = bhTT:BUFFER-FIELD('idUnidad').
  iUNidad = hf:BUFFER-VALUE.

  RETURN iUnidad.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEmpresa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEmpresa Procedure 
FUNCTION setEmpresa RETURNS LOGICAL
  ( INPUT iEmp AS INTEGER  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR hf             AS HANDLE NO-UNDO.

  RUN readSession.
  hf = bhTT:BUFFER-FIELD('idEmpresa').
  iEmpresa = iEmp.
  hf:BUFFER-VALUE = iEmpresa.
  RUN writeSession.
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNumericFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNumericFormat Procedure 
FUNCTION setNumericFormat RETURNS LOGICAL
  ( INPUT cNumericFormat AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  SESSION:NUMERIC-FORMAT = cNumericFormat.
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSucursal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSucursal Procedure 
FUNCTION setSucursal RETURNS LOGICAL
  ( INPUT iSuc AS INTEGER  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR hf             AS HANDLE NO-UNDO.
  
  iSucursal = iSuc.

  RUN readSession.
  hf = bhTT:BUFFER-FIELD('idSucursal').
  hf:BUFFER-VALUE = iSucursal.
  RUN writeSession.
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUnidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUnidad Procedure 
FUNCTION setUnidad RETURNS LOGICAL
  ( INPUT iUni AS INTEGER  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR hf AS HANDLE NO-UNDO.

iUnidad = iUni.
  
  
RUN readSession.
hf = bhTT:BUFFER-FIELD('idUnidad').
hf:BUFFER-VALUE = iUnidad.
RUN writeSession.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

