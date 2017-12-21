&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors: Matt Verrinder, Stuart Butler, Chris Skeldon         *
*                                                                    *
*********************************************************************/
/*--------------------------------------------------------------------------
    File        : escript.p
    Purpose     : Super procedure for escript class.

    Syntax      : RUN start-super-proc("web/escript.p":U).

    Modified    : 24/03/2003
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOP ADMSuper escript.p

  /* Custom exclude file */

  {src/web/custom/escriptexclcustom.i}

{src/web/escrtags.i}

&GLOBAL-DEFINE MAX_CHAR_SIZE 31990
&GLOBAL-DEFINE BUFFER_SIZE 32768

&GLOBAL-DEFINE DELIMITER CHR(1)
&GLOBAL-DEFINE PARAM_DELIMITER CHR(2)

&GLOBAL-DEFINE TRIM_CHARS "~~r~~n~~t":U

CREATE WIDGET-POOL.

DEFINE VARIABLE glBufferEnabled     AS LOGICAL INITIAL TRUE   NO-UNDO.
DEFINE VARIABLE glPreprocess        AS LOGICAL INITIAL FALSE  NO-UNDO.

/* Buffer for generated page */
DEFINE VARIABLE ptrBuffer           AS MEMPTR                         NO-UNDO.
DEFINE VARIABLE giContentLength     AS INTEGER                        NO-UNDO.
DEFINE VARIABLE giBufferSize        AS INTEGER INITIAL {&BUFFER_SIZE} NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-appendBytes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendBytes Procedure 
FUNCTION appendBytes RETURNS LOGICAL
  ( INPUT prData AS RAW )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-appendMemptr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendMemptr Procedure 
FUNCTION appendMemptr RETURNS LOGICAL
  ( INPUT pmData AS MEMPTR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-appendString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendString Procedure 
FUNCTION appendString RETURNS LOGICAL
  ( INPUT pcScript AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-disablePageBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD disablePageBuffer Procedure 
FUNCTION disablePageBuffer RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-disablePageCache) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD disablePageCache Procedure 
FUNCTION disablePageCache RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-errorPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD errorPage Procedure 
FUNCTION errorPage RETURNS LOGICAL
  ( INPUT pcError AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-evaluateIf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD evaluateIf Procedure 
FUNCTION evaluateIf RETURNS LOGICAL PRIVATE
  ( INPUT-OUTPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-evaluateRepeat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD evaluateRepeat Procedure 
FUNCTION evaluateRepeat RETURNS LOGICAL PRIVATE
  ( INPUT-OUTPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-evaluateScript) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD evaluateScript Procedure 
FUNCTION evaluateScript RETURNS LOGICAL
  ( INPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-evaluateWhile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD evaluateWhile Procedure 
FUNCTION evaluateWhile RETURNS LOGICAL PRIVATE
  ( INPUT-OUTPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-expandScript) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD expandScript Procedure 
FUNCTION expandScript RETURNS CHARACTER
  ( INPUT pcScript AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fetchMarkup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fetchMarkup Procedure 
FUNCTION fetchMarkup RETURNS CHARACTER
  ( INPUT pcMarkupFile AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fieldTags) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fieldTags Procedure 
FUNCTION fieldTags RETURNS LOGICAL PRIVATE
  ( INPUT-OUTPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-flushBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD flushBuffer Procedure 
FUNCTION flushBuffer RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-flushStream) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD flushStream Procedure 
FUNCTION flushStream RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContentType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getContentType Procedure 
FUNCTION getContentType RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMessages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMessages Procedure 
FUNCTION getMessages RETURNS CHARACTER
  ( INPUT pcGroup AS CHARACTER,
    INPUT plDelete AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPageBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPageBuffer Procedure 
FUNCTION getPageBuffer RETURNS MEMPTR
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hasMessages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hasMessages Procedure 
FUNCTION hasMessages RETURNS LOGICAL
  ( INPUT pcGroup AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-includeMarkup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD includeMarkup Procedure 
FUNCTION includeMarkup RETURNS LOGICAL
  ( INPUT-OUTPUT pcScript AS CHARACTER,
    output pcError AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initPage Procedure 
FUNCTION initPage RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isDefined) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isDefined Procedure 
FUNCTION isDefined RETURNS LOGICAL
  ( INPUT pcArgValue AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isGet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isGet Procedure 
FUNCTION isGet RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isPost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isPost Procedure 
FUNCTION isPost RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isSelected) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isSelected Procedure 
FUNCTION isSelected RETURNS CHARACTER
  ( INPUT pcField AS CHARACTER,
    INPUT pcValue AS CHARACTER,
    INPUT pcSelected AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-outputHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD outputHeader Procedure 
FUNCTION outputHeader RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-outputPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD outputPage Procedure 
FUNCTION outputPage RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-preprocessMarkup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD preprocessMarkup Procedure 
FUNCTION preprocessMarkup RETURNS CHARACTER PRIVATE
  ( INPUT pcScript AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processMarkup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD processMarkup Procedure 
FUNCTION processMarkup RETURNS LOGICAL
  ( INPUT pcMarkupName AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-queueMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD queueMessage Procedure 
FUNCTION queueMessage RETURNS LOGICAL
  ( INPUT pcGroup AS CHARACTER,
    INPUT pcMessage AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-readFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD readFile Procedure 
FUNCTION readFile RETURNS CHARACTER PRIVATE
  ( INPUT pcFilename AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-replaceArguments) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD replaceArguments Procedure 
FUNCTION replaceArguments RETURNS CHARACTER
  ( INPUT pcScript AS CHARACTER,
    INPUT pcArguments AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-resizeBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD resizeBuffer Procedure 
FUNCTION resizeBuffer RETURNS LOGICAL PRIVATE
  ( INPUT-OUTPUT ptrBuffer AS MEMPTR,
    INPUT piLength AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-scriptTags) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD scriptTags Procedure 
FUNCTION scriptTags RETURNS LOGICAL PRIVATE
  ( INPUT pcTagStart AS CHARACTER,
    INPUT pcTagEnd AS CHARACTER,
    INPUT plEmbedValue AS LOGICAL,
    INPUT-OUTPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCookies) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCookies Procedure 
FUNCTION setCookies RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPreprocess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPreprocess Procedure 
FUNCTION setPreprocess RETURNS LOGICAL
  ( INPUT plPreprocess AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD showField Procedure 
FUNCTION showField RETURNS CHARACTER
  ( INPUT pcScript AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-streamFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD streamFile Procedure 
FUNCTION streamFile RETURNS LOGICAL
  ( INPUT pcFilename AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-writeFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD writeFile Procedure 
FUNCTION writeFile RETURNS LOGICAL
  ( INPUT pcFilename AS CHARACTER )  FORWARD.

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
         HEIGHT             = 14.29
         WIDTH              = 77.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{src/web/method/cgidefs.i}
{src/web/escrprop.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-appendBytes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendBytes Procedure 
FUNCTION appendBytes RETURNS LOGICAL
  ( INPUT prData AS RAW ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iLength     AS INTEGER                        NO-UNDO.

  ASSIGN iLength = LENGTH(prData, "RAW":U) WHEN prData <> ?.

  IF iLength > 0 THEN
  DO:
    IF glBufferEnabled THEN
    DO:
      /* Resize buffer? */
      IF giContentLength + iLength > giBufferSize THEN
        resizeBuffer(INPUT-OUTPUT ptrBuffer, INPUT iLength).

      ASSIGN
        PUT-BYTES(ptrBuffer, giContentLength + 1) = prData
        giContentLength                           = giContentLength + iLength.
    END.
    ELSE
    DO:
      PUT STREAM WEBSTREAM CONTROL prData.
    END.
  END.

  LENGTH(prData) = 0.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-appendMemptr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendMemptr Procedure 
FUNCTION appendMemptr RETURNS LOGICAL
  ( INPUT pmData AS MEMPTR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iLength     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iOffset     AS INTEGER INITIAL 1              NO-UNDO.
  DEFINE VARIABLE iBlockSize  AS INTEGER INITIAL {&BUFFER_SIZE} NO-UNDO.
  DEFINE VARIABLE rawData     AS RAW                            NO-UNDO.

  ASSIGN iLength = GET-SIZE(pmData).

  IF iLength > 0 THEN
  DO:
    DO WHILE iOffset <= iLength:
      IF iOffset + iBlockSize > iLength THEN
        ASSIGN iBlockSize = iLength - iOffset + 1.

      MESSAGE iOffset iLength iBlockSize
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

      ASSIGN
        LENGTH(rawData) = iBlockSize
        rawData         = GET-BYTES(pmData, iOffset, iBlockSize).

      DYNAMIC-FUNCTION("appendBytes":U IN TARGET-PROCEDURE, INPUT rawData).

      ASSIGN
        LENGTH(rawData) = 0
        iOffset         = iOffset + iBlockSize.
    END.
  END.

  SET-SIZE(pmData) = 0.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-appendString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendString Procedure 
FUNCTION appendString RETURNS LOGICAL
  ( INPUT pcScript AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iLength AS INTEGER  NO-UNDO.

  ASSIGN iLength = LENGTH(pcScript, "RAW":U) WHEN pcScript <> ?.

  IF iLength > 0 THEN
  DO:
    IF glBufferEnabled THEN
    DO:
      /* Resize buffer? */
      IF giContentLength + iLength > giBufferSize THEN
        resizeBuffer(INPUT-OUTPUT ptrBuffer, INPUT iLength).

      ASSIGN
        PUT-STRING(ptrBuffer, giContentLength + 1, iLength) = pcScript
        giContentLength                                     = giContentLength + iLength.
    END.
    ELSE
    DO:
      {&OUT} pcScript.
    END.
  END.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-disablePageBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION disablePageBuffer Procedure 
FUNCTION disablePageBuffer RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-disablePageCache) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION disablePageCache Procedure 
FUNCTION disablePageCache RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-errorPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION errorPage Procedure 
FUNCTION errorPage RETURNS LOGICAL
  ( INPUT pcError AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  output-content-type ("text/html":U).

  {&OUT}
    '<html>' SKIP
    '  <head>' SKIP
    '    <title>eScript Error</title>' SKIP
    '  </head>' SKIP
    '  <body>' SKIP
    '    <pre>' html-encode(pcError) '</pre>' SKIP
    '  </body>' SKIP
    '</html>' SKIP.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-evaluateIf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION evaluateIf Procedure 
FUNCTION evaluateIf RETURNS LOGICAL PRIVATE
  ( INPUT-OUTPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cFunction       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFunctionName   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFuncParams     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cParam          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cParameters     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTag            AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIfTag          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cElseTag        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEndTag         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTrueFragment   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFalseFragment  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iOffset         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iElseOffset     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEndOffset      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNumParams      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i               AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hFunction       AS HANDLE     NO-UNDO.
  DEFINE VARIABLE lSuccess        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE hasNot          AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE hasElse         AS LOGICAL    NO-UNDO.

  ASSIGN iOffset = INDEX(pcScript, {&xcScriptTagEnd}).

  IF iOffset = 0 THEN
  DO:
    /* End of eScript tag not found */
    ASSIGN pcError = SUBSTITUTE("Invalid IF tag, cannot find &1":U, {&xcScriptTagEnd}).
    RETURN FALSE.
  END.

  ASSIGN
    iOffset   = iOffset + LENGTH({&xcScriptTagEnd})
    cIfTag    = SUBSTRING(pcScript, 1, iOffset - 1)
    pcScript  = SUBSTRING(pcScript, iOffset)
    cTag      = SUBSTRING(cIfTag,
                          LENGTH({&xcScriptTagStart}) + 1,
                          LENGTH(cIfTag) - LENGTH({&xcScriptTagStart}) - LENGTH({&xcScriptTagEnd}))
    iOffset   = INDEX(cTag, {&xcFunctionDelimiter}).

  IF iOffset = 0 THEN
  DO:
    ASSIGN pcError = SUBSTITUTE("Missing function: &1":U, cIfTag).
    RETURN FALSE.
  END.

  ASSIGN
    cFunction     = SUBSTRING(cTag, iOffset + 1)
    cFunctionName = ENTRY(1, cFunction, "(":U)
    cParameters   = SUBSTRING(cFunction, LENGTH(cFunctionName) + 1)
    cFunctionName = TRIM ( cFunctionName, " ":U ).

  IF cFunctionName = "":U THEN
  DO:
    ASSIGN pcError = SUBSTITUTE("Invalid tag: &1":U, cIfTag).
    RETURN FALSE.
  END.

  IF cFunctionName BEGINS "!":U THEN
    ASSIGN
      hasNot        = TRUE
      cFunctionName = TRIM ( SUBSTRING ( cFunctionName, 2 ), " ":U ).
  ELSE
  IF NUM-ENTRIES(cFunctionName, " ":U) > 1 AND ENTRY(1, cFunctionName, " ":U) = "NOT":U THEN
    ASSIGN
      hasNot        = TRUE
      cFunctionName = TRIM ( SUBSTRING ( cFunctionName, 5 ), " ":U ).

  ASSIGN
    cElseTag    = {&xcIfElse} + {&xcFunctionDelimiter} + cFunctionName + {&xcScriptTagEnd}
    cEndTag     = {&xcIfEnd} + {&xcFunctionDelimiter} + cFunctionName + {&xcScriptTagEnd}
    iElseOffset = INDEX(pcScript, cElseTag)
    iEndOffset  = INDEX(pcScript, cEndTag).

  IF iEndOffset = 0 THEN
  DO:
    ASSIGN pcError = SUBSTITUTE("Missing end tag: &1":U, cEndTag).
    RETURN FALSE.
  END.

  IF iElseOffset > 0 AND iElseOffset < iEndOffset THEN
    ASSIGN
      hasElse       = TRUE
      cTrueFragment = RIGHT-TRIM(SUBSTRING(pcScript, 1, iElseOffset - 1), {&TRIM_CHARS})
      pcScript      = SUBSTRING(pcScript, iElseOffset + LENGTH(cElseTag))
      iEndOffset    = INDEX(pcScript, cEndTag).

  IF hasNot THEN
  DO:
    IF hasElse THEN
      ASSIGN
        cFalseFragment = cTrueFragment
        cTrueFragment  = RIGHT-TRIM(SUBSTRING(pcScript, 1, iEndOffset - 1), {&TRIM_CHARS}).
    ELSE
      ASSIGN
        cTrueFragment  = "":U
        cFalseFragment = RIGHT-TRIM(SUBSTRING(pcScript, 1, iEndOffset - 1), {&TRIM_CHARS}).
  END.
  ELSE
  DO:
    IF hasElse THEN
      ASSIGN cFalseFragment = RIGHT-TRIM(SUBSTRING(pcScript, 1, iEndOffset - 1), {&TRIM_CHARS}).
    ELSE
      ASSIGN cTrueFragment = RIGHT-TRIM(SUBSTRING(pcScript, 1, iEndOffset - 1), {&TRIM_CHARS}).
  END.

  ASSIGN
    pcScript    = SUBSTRING(pcScript, iEndOffset + LENGTH(cEndTag))
    cParameters = TRIM( cParameters )
    cParameters = TRIM( cParameters, "().":U )
    cParameters = TRIM( cParameters )
    iNumParams  = NUM-ENTRIES( cParameters ).

  /* Use dynamic call */
  CREATE CALL hFunction.
  ASSIGN
    hFunction:CALL-TYPE      = FUNCTION-CALL-TYPE
    hFunction:CALL-NAME      = cFunctionName
    hFunction:IN-HANDLE      = TARGET-PROCEDURE:HANDLE
    hFunction:NUM-PARAMETERS = iNumParams.

  REPEAT i = 1 TO iNumParams:
      ASSIGN
        cParam      = ENTRY( i, cParameters )
        cParam      = TRIM( cParam )
        cParam      = TRIM( cParam, "~"" )
        cParam      = TRIM( cParam, "'" )
        cParam      = TRIM( cParam, "~"" )
        cFuncParams = SUBSTITUTE( "&1&2&3":U,
                                  cFuncParams,
                                  IF LENGTH( cFuncParams ) > 0 THEN "," ELSE "",
                                  cParam ).

      hFunction:SET-PARAMETER(i, "CHARACTER":U, "INPUT":U, cParam).
  END.

  hFunction:INVOKE() NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
      MESSAGE
        SUBSTITUTE("Function: &1(&2) NumParams: &3 TARGET-PROCEDURE: &4~nERROR: &5":U,
                   cFunction,
                   cFuncParams,
                   iNumParams,
                   TARGET-PROCEDURE:FILE-NAME,
                   ERROR-STATUS:GET-MESSAGE( 1 )).

  ASSIGN lSuccess = hFunction:RETURN-VALUE.

  DELETE OBJECT hFunction NO-ERROR.

  IF lSuccess AND cTrueFragment > "":U THEN
    IF NOT DYNAMIC-FUNCTION("evaluateScript":U IN TARGET-PROCEDURE, INPUT cTrueFragment, OUTPUT pcError) THEN
      RETURN FALSE.

  IF NOT lSuccess AND cFalseFragment > "":U THEN
    IF NOT DYNAMIC-FUNCTION("evaluateScript":U IN TARGET-PROCEDURE, INPUT cFalseFragment, OUTPUT pcError) THEN
      RETURN FALSE.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-evaluateRepeat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION evaluateRepeat Procedure 
FUNCTION evaluateRepeat RETURNS LOGICAL PRIVATE
  ( INPUT-OUTPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cFunction     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFunctionName AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFuncParams   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cParam        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cParameters   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTag          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cStartTag     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEndTag       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFragment     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iOffset       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNumParams    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hFunction     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE lSuccess      AS LOGICAL    NO-UNDO.

  ASSIGN iOffset = INDEX(pcScript, {&xcScriptTagEnd}).

  IF iOffset = 0 THEN
  DO:
    /* End of eScript tag not found */
    ASSIGN pcError = SUBSTITUTE("Invalid REPEAT tag, cannot find &1":U, {&xcScriptTagEnd}).
    RETURN FALSE.
  END.

  ASSIGN
    iOffset   = iOffset + LENGTH({&xcScriptTagEnd})
    cStartTag = SUBSTRING(pcScript, 1, iOffset - 1)
    pcScript  = SUBSTRING(pcScript, iOffset)
    cTag      = SUBSTRING(cStartTag,
                          LENGTH({&xcScriptTagStart}) + 1,
                          LENGTH(cStartTag) - LENGTH({&xcScriptTagStart}) - LENGTH({&xcScriptTagEnd}))
    iOffset   = INDEX(cTag, {&xcFunctionDelimiter}).

  IF iOffset = 0 THEN
  DO:
    ASSIGN pcError = SUBSTITUTE("Missing function: &1":U, cStartTag).
    RETURN FALSE.
  END.

  ASSIGN
    cFunction     = SUBSTRING(cTag, iOffset + 1)
    cFunctionName = ENTRY(1, cFunction, "(":U)
    cParameters   = SUBSTRING(cFunction, LENGTH(cFunctionName) + 1).

  IF cFunctionName = "":U THEN
  DO:
    ASSIGN pcError = SUBSTITUTE("Invalid tag: &1":U, cStartTag).
    RETURN FALSE.
  END.

  ASSIGN
    cEndTag = {&xcRepeatEnd} + {&xcFunctionDelimiter} + cFunctionName + {&xcScriptTagEnd}
    iOffset = INDEX(pcScript, cEndTag).

  IF iOffset = 0 THEN
  DO:
    ASSIGN pcError = SUBSTITUTE("Missing end tag: &1":U, cEndTag).
    RETURN FALSE.
  END.

  ASSIGN
    cFragment   = RIGHT-TRIM ( SUBSTRING(pcScript, 1, iOffset - 1), {&TRIM_CHARS} )
    pcScript    = SUBSTRING(pcScript, iOffset + LENGTH(cEndTag))

    cParameters = TRIM( cParameters )
    cParameters = TRIM( cParameters, "().":U )
    cParameters = TRIM( cParameters )
    iNumParams  = NUM-ENTRIES( cParameters ).

  /* Use dynamic call */
  CREATE CALL hFunction.
  ASSIGN
    hFunction:CALL-TYPE      = FUNCTION-CALL-TYPE
    hFunction:CALL-NAME      = cFunctionName
    hFunction:IN-HANDLE      = TARGET-PROCEDURE:HANDLE
    hFunction:NUM-PARAMETERS = iNumParams + 2.

  REPEAT i = 1 TO iNumParams:
      ASSIGN
        cParam      = ENTRY( i, cParameters )
        cParam      = TRIM( cParam )
        cParam      = TRIM( cParam, "~"":U )
        cParam      = TRIM( cParam, "'":U )
        cParam      = TRIM( cParam, "~"":U )

        cFuncParams = SUBSTITUTE ( "&1&2&3":U,
                                   cFuncParams,
                                   IF LENGTH( cFuncParams ) > 0 THEN "," ELSE "",
                                   cParam ).

      hFunction:SET-PARAMETER(i, "CHARACTER":U, "INPUT":U, cParam).
  END.

  /* add common parameters */
  hFunction:SET-PARAMETER(iNumParams + 1, "CHARACTER":U, "INPUT":U, cFragment).
  hFunction:SET-PARAMETER(iNumParams + 2, "CHARACTER":U, "OUTPUT":U, pcError).

  hFunction:INVOKE() NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
    MESSAGE
      SUBSTITUTE("FUNCTION: &1 ( &2 ) NumParams: &3 TARGET-PROCEDURE: &4 ERROR: &5":U,
                 cFunction,
                 cFuncParams,
                 iNumParams,
                 TARGET-PROCEDURE:FILE-NAME,
                 ERROR-STATUS:GET-MESSAGE(1)).

  ASSIGN lSuccess = hFunction:RETURN-VALUE.

  DELETE OBJECT hFunction NO-ERROR.

  IF lSuccess = ? THEN
    RETURN TRUE.

  RETURN lSuccess.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-evaluateScript) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION evaluateScript Procedure 
FUNCTION evaluateScript RETURNS LOGICAL
  ( INPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER ) :
/*------------------------------------------------------------------------------
pcScript: The html page or section of html to be evaluated.  Script tags are
  found, and specified functions run.  Output is put into ttLines to overcome
  restrictions on the CHAR data type (max string is 32K).
returns: Returns "" if everything evaluated okay.  Else it holds an error
  message.

------------------------------------------------------------------------------*/

  DEFINE VARIABLE iStart        AS INTEGER   INITIAL 1    NO-UNDO.
  DEFINE VARIABLE iEnd          AS INTEGER                NO-UNDO.
  DEFINE VARIABLE lContinue     AS LOGICAL   INITIAL TRUE NO-UNDO.

  ASSIGN iStart = INDEX ( pcScript, {&xcScriptTagStart} ).

  evalBlock:
  DO WHILE ( iStart > 0 and lContinue ) :
    /* Output HTML upto the start of the eScript tag */
    DYNAMIC-FUNCTION("appendString":U IN TARGET-PROCEDURE, INPUT SUBSTRING(pcScript, 1, iStart - 1)).

    /* and then trim it off */
    ASSIGN pcScript = SUBSTRING( pcScript, iStart ).

    IF pcScript BEGINS {&xcValueTagStart} THEN
      lContinue = DYNAMIC-FUNCTION("scriptTags":U IN TARGET-PROCEDURE,
                                   INPUT {&xcValueTagStart},
                                   INPUT {&xcValueTagEnd},
                                   INPUT TRUE,
                                   INPUT-OUTPUT pcScript,
                                   OUTPUT pcError ).
    ELSE IF pcScript BEGINS {&xcIfStart} THEN
      lContinue = DYNAMIC-FUNCTION("evaluateIf":U IN TARGET-PROCEDURE,
                                   INPUT-OUTPUT pcScript,
                                   OUTPUT pcError).
    ELSE IF pcScript BEGINS {&xcRepeatStart} THEN
      lContinue = DYNAMIC-FUNCTION("evaluateRepeat":U IN TARGET-PROCEDURE,
                                   INPUT-OUTPUT pcScript,
                                   OUTPUT pcError).
    ELSE IF pcScript BEGINS {&xcWhileStart} THEN
      lContinue = DYNAMIC-FUNCTION("evaluateWhile":U IN TARGET-PROCEDURE,
                                   INPUT-OUTPUT pcScript,
                                   OUTPUT pcError).
    ELSE IF pcScript BEGINS {&xcDiscardStart} THEN
      pcScript = SUBSTRING(pcScript,
                           INDEX(pcScript, {&xcDiscardEnd}) + LENGTH({&xcDiscardEnd})).
    ELSE IF pcScript BEGINS {&xcFieldStart} THEN
      lContinue = DYNAMIC-FUNC('fieldTags':U IN TARGET-PROCEDURE,
                               INPUT-OUTPUT pcScript,
                               OUTPUT pcError ).
    ELSE IF pcScript BEGINS {&xcIncludeStart} THEN
      lContinue = DYNAMIC-FUNCTION("includeMarkup":U IN TARGET-PROCEDURE,
                                   INPUT-OUTPUT pcScript,
                                   OUTPUT pcError ).
    ELSE IF pcScript BEGINS {&xcScriptCommentStart} THEN
      pcScript = SUBSTRING(pcScript,
                           INDEX( pcScript, {&xcScriptCommentEnd} ) + LENGTH ( {&xcScriptCommentEnd} ) ).
    ELSE
      lContinue = DYNAMIC-FUNCTION("scriptTags":U IN TARGET-PROCEDURE,
                                   INPUT {&xcScriptTagStart},
                                   INPUT {&xcScriptTagEnd},
                                   INPUT FALSE,
                                   INPUT-OUTPUT pcScript,
                                   OUTPUT pcError ).

    IF NOT lContinue THEN
      LEAVE evalBlock.

    ASSIGN iStart = INDEX(pcScript, {&xcScriptTagStart} ).
  END.

  IF LENGTH( pcScript ) > 0 THEN
    DYNAMIC-FUNCTION("appendString":U IN TARGET-PROCEDURE, INPUT pcScript).

  RETURN lContinue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-evaluateWhile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION evaluateWhile Procedure 
FUNCTION evaluateWhile RETURNS LOGICAL PRIVATE
  ( INPUT-OUTPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cFunction       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFunctionName   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFuncParams     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cParam          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cParameters     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTag            AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cWhileTag       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEndTag         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFragment       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iOffset         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEndOffset      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNumParams      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i               AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hFunction       AS HANDLE     NO-UNDO.
  DEFINE VARIABLE lSuccess        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lLoop           AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lLoopWhile      AS LOGICAL   INITIAL TRUE NO-UNDO.

  ASSIGN iOffset = INDEX(pcScript, {&xcScriptTagEnd}).

  IF iOffset = 0 THEN
  DO:
    /* End of eScript tag not found */
    ASSIGN pcError = SUBSTITUTE("Invalid WHILE tag, cannot find &1":U, {&xcScriptTagEnd}).
    RETURN FALSE.
  END.

  ASSIGN
    iOffset   = iOffset + LENGTH({&xcScriptTagEnd})
    cWhileTag = SUBSTRING(pcScript, 1, iOffset - 1)
    pcScript  = SUBSTRING(pcScript, iOffset)
    cTag      = SUBSTRING(cWhileTag,
                          LENGTH({&xcScriptTagStart}) + 1,
                          LENGTH(cWhileTag) - LENGTH({&xcScriptTagStart}) - LENGTH({&xcScriptTagEnd}))
    iOffset   = INDEX(cTag, {&xcFunctionDelimiter}).

  IF iOffset = 0 THEN
  DO:
    ASSIGN pcError = SUBSTITUTE("Missing function: &1":U, cWhileTag).
    RETURN FALSE.
  END.

  ASSIGN
    cFunction     = SUBSTRING(cTag, iOffset + 1)
    cFunctionName = ENTRY(1, cFunction, "(":U)
    cParameters   = SUBSTRING(cFunction, LENGTH(cFunctionName) + 1)
    cParameters   = TRIM( cParameters )
    cParameters   = TRIM( cParameters, "().":U )
    cParameters   = TRIM( cParameters )
    iNumParams    = NUM-ENTRIES( cParameters ).

  IF cFunctionName = "":U THEN
  DO:
    ASSIGN pcError = SUBSTITUTE("Invalid tag: &1":U, cWhileTag).
    RETURN FALSE.
  END.

  IF cFunctionName BEGINS "!":U THEN
    ASSIGN
      lLoopWhile    = FALSE
      cFunctionName = SUBSTRING(cFunctionName, 2).
  ELSE
  IF NUM-ENTRIES(cFunctionName, " ":U) > 1 AND ENTRY(1, cFunctionName, " ":U) = "NOT":U THEN
    ASSIGN
      lLoopWhile    = FALSE
      cFunctionName = ENTRY(2, cFunctionName, " ":U).

  ASSIGN
    cEndTag     = {&xcWhileEnd} + {&xcFunctionDelimiter} + cFunctionName + {&xcScriptTagEnd}
    iEndOffset  = INDEX(pcScript, cEndTag).

  IF iEndOffset = 0 THEN
  DO:
    ASSIGN pcError = SUBSTITUTE("Missing end tag: &1":U, cEndTag).
    RETURN FALSE.
  END.

  ASSIGN
    cFragment = RIGHT-TRIM(SUBSTRING(pcScript, 1, iEndOffset - 1), {&TRIM_CHARS})
    pcScript  = SUBSTRING(pcScript, iEndOffset + LENGTH(cEndTag)).

  /* Use dynamic call */
  CREATE CALL hFunction.
  ASSIGN
    hFunction:CALL-TYPE      = FUNCTION-CALL-TYPE
    hFunction:CALL-NAME      = cFunctionName
    hFunction:IN-HANDLE      = TARGET-PROCEDURE:HANDLE
    hFunction:NUM-PARAMETERS = iNumParams.

  REPEAT i = 1 TO iNumParams:
      ASSIGN
        cParam      = ENTRY( i, cParameters )
        cParam      = TRIM( cParam )
        cParam      = TRIM( cParam, "~"" )
        cParam      = TRIM( cParam, "'" )
        cParam      = TRIM( cParam, "~"" )
        cFuncParams = SUBSTITUTE( "&1&2&3":U,
                                  cFuncParams,
                                  IF LENGTH( cFuncParams ) > 0 THEN "," ELSE "",
                                  cParam ).

      hFunction:SET-PARAMETER(i, "CHARACTER":U, "INPUT":U, cParam).
  END.

  ASSIGN
    lLoop    = lLoopWhile
    lSuccess = TRUE.

  IF cFragment > "":U THEN
  DO WHILE (lLoop = lLoopWhile AND lSuccess):
    hFunction:INVOKE() NO-ERROR.

    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
    DO:
      MESSAGE
        SUBSTITUTE("FUNCTION: &1 ( &2 ) NumParams: &3 TARGET-PROCEDURE: &4 ERROR: &5":U,
                   cFunction,
                   cFuncParams,
                   iNumParams,
                   TARGET-PROCEDURE:FILE-NAME,
                   ERROR-STATUS:GET-MESSAGE( 1 )).
      ASSIGN lLoop = NOT lLoopWhile.
    END.
    ELSE
    DO:
      ASSIGN lLoop = hFunction:RETURN-VALUE.

      IF lLoop = lLoopWhile THEN
        IF NOT DYNAMIC-FUNCTION("evaluateScript":U IN TARGET-PROCEDURE, INPUT cFragment, OUTPUT pcError) THEN
          ASSIGN lSuccess = FALSE.
    END.
  END.

  DELETE OBJECT hFunction NO-ERROR.

  RETURN lSuccess.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-expandScript) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION expandScript Procedure 
FUNCTION expandScript RETURNS CHARACTER
  ( INPUT pcScript AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Evaluate the value tags <%=function()%> and return as character
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cFullScript AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFunction   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFuncParams AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cParameters AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cParamValue AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValue      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hFunction   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iStart      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEnd        AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLength     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNumParams  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iParam      AS INTEGER    NO-UNDO.

  ASSIGN iStart = INDEX( pcScript, {&xcValueTagStart}, iStart + 1 ).

  DO WHILE iStart > 0:
    ASSIGN
      cValue = "":U
      iEnd   = INDEX( pcScript, {&xcValueTagEnd}, iStart ).

    IF iEnd > 0 THEN
    DO:
      ASSIGN
        iLength     = iEnd - iStart + LENGTH( {&xcValueTagEnd} )
        cFullScript = SUBSTRING( pcScript, iStart, iLength )
        cFunction   = REPLACE( cFullScript, {&xcValueTagStart}, "":U )
        cFunction   = REPLACE( cFunction, {&xcValueTagEnd}, "":U )
        cParameters = ENTRY( 2, cFunction, "(":U )
        cFunction   = ENTRY( 1, cFunction, "(":U )
        cFunction   = TRIM( cFunction )
        cParameters = TRIM( cParameters )
        cParameters = TRIM( cParameters, ").":U )
        cParameters = TRIM( cParameters )
        iNumParams  = NUM-ENTRIES( cParameters ).

      /* Use dynamic call */
      CREATE CALL hFunction.
      ASSIGN
        hFunction:CALL-TYPE      = FUNCTION-CALL-TYPE
        hFunction:CALL-NAME      = cFunction
        hFunction:IN-HANDLE      = TARGET-PROCEDURE:HANDLE
        hFunction:NUM-PARAMETERS = iNumParams.

      REPEAT iParam = 1 TO iNumParams:
          ASSIGN
            cParamValue = ENTRY( iParam, cParameters )
            cParamValue = TRIM( cParamValue )
            cParamValue = TRIM( cParamValue, "~"" )
            cParamValue = TRIM( cParamValue, "'" )
            cParamValue = TRIM( cParamValue, "~"" )
            cFuncParams = SUBSTITUTE( "&1&2&3":U,
                                      cFuncParams,
                                      IF LENGTH( cFuncParams ) > 0 THEN "," ELSE "",
                                      cParamValue ).

          hFunction:SET-PARAMETER(iParam, "CHARACTER":U, "INPUT":U, cParamValue).
      END.

      hFunction:INVOKE() NO-ERROR.

      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
        MESSAGE
          SUBSTITUTE("FUNCTION: &1 ( &2 ) NumParams: &3 TARGET-PROCEDURE: &4 ERROR: &5":U,
                     cFunction,
                     cFuncParams,
                     iNumParams,
                     TARGET-PROCEDURE:FILE-NAME,
                     ERROR-STATUS:GET-MESSAGE( 1 )).

      ASSIGN cValue = hFunction:RETURN-VALUE.

      IF cValue = ? THEN
        ASSIGN cValue = "":U.

      DELETE OBJECT hFunction NO-ERROR.

      ASSIGN
        pcScript = REPLACE( pcScript, cFullScript, cValue )
        iStart   = INDEX(pcScript, {&xcValueTagStart}, iStart).
    END.
    ELSE
    DO:
      ASSIGN
        iStart   = INDEX(pcScript, {&xcValueTagStart}, iStart + 1).
    END.

  END.

  RETURN pcScript.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fetchMarkup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fetchMarkup Procedure 
FUNCTION fetchMarkup RETURNS CHARACTER
  ( INPUT pcMarkupFile AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cScript AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cReturn AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iStart  AS INTEGER    NO-UNDO.

  ASSIGN cScript = readFile(pcMarkupFile).

  IF glPreprocess THEN
    RETURN preprocessMarkup(cScript).

  RETURN cScript.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fieldTags) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fieldTags Procedure 
FUNCTION fieldTags RETURNS LOGICAL PRIVATE
  ( INPUT-OUTPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose: lSuccess = evaluateValue( INPUT-OUTPUT pcScript, OUTPUT cError ).
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cCallTag      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTag          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEndTag       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFunctionName AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cParameters   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iOffset       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNumParams    AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iEnd          AS INTEGER NO-UNDO.
  DEFINE VARIABLE iEndField     AS INTEGER NO-UNDO.
  DEFINE VARIABLE cFullScript   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cParams       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cParam        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFuncParams   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFunction     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFieldScript  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cName         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cValue        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iLen          AS INTEGER NO-UNDO.
  DEFINE VARIABLE iIdx          AS INTEGER NO-UNDO.
  DEFINE VARIABLE cFieldName    AS CHARACTER NO-UNDO INITIAL "":U.
  DEFINE VARIABLE lSuccess      AS LOGICAL NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE hFunction     AS HANDLE NO-UNDO.

    /*
    ASSIGN
      pcScript = SUBSTRING( pcScript, LENGTH( {&xcFieldStart} ) + 1 )
      pcScript = TRIM( pcScript ).

    IF pcScript BEGINS "-":U THEN
    DO:
        ASSIGN
          cFieldName  = ENTRY( 1, pcScript, " ":U )
          pcScript    = SUBSTRING( pcScript, LENGTH( cFieldName ) + 1 )
          pcScript    = TRIM( pcScript ).
    END.


    IF NOT pcScript BEGINS {&xcFieldFunction} THEN
    DO:
        pcError = {&xcFieldStart} + cFieldName + " without " + {&xcFieldFunction}.
        RETURN FALSE.
    END.

    ASSIGN pcScript = SUBSTRING( pcScript, LENGTH( {&xcFieldFunction} ) + 1 ).

    iEnd = INDEX( pcScript, {&xcFieldStartEnd} ).
    IF iEnd < 2 THEN
    DO:
        pcError = {&xcFieldStart} + " without " + {&xcFieldStartEnd}.
        RETURN FALSE.
    END.
    */

    ASSIGN iOffset = INDEX(pcScript, {&xcScriptTagEnd}).

    IF iOffset = 0 THEN
    DO:
      /* End of eScript tag not found */
      ASSIGN pcError = SUBSTITUTE("Invalid CALL tag, cannot FIND &1":U, {&xcScriptTagEnd}).
      RETURN FALSE.
    END.

    ASSIGN
      iOffset   = iOffset + LENGTH({&xcScriptTagEnd})
      cCallTag  = SUBSTRING(pcScript, 1, iOffset - 1)
      pcScript  = SUBSTRING(pcScript, iOffset)
      cTag      = SUBSTRING(cCallTag,
                            LENGTH({&xcScriptTagStart}) + 1,
                            LENGTH(cCallTag) - LENGTH({&xcScriptTagStart}) - LENGTH({&xcScriptTagEnd}))
      iOffset   = INDEX(cTag, {&xcFunctionDelimiter}).

    IF iOffset = 0 THEN
    DO:
      ASSIGN pcError = SUBSTITUTE("Missing FUNCTION: &1":U, cCallTag).
      RETURN FALSE.
    END.

    ASSIGN
      cFunction     = SUBSTRING(cTag, iOffset + 1)
      cFunctionName = ENTRY(1, cFunction, "(":U)
      cParameters   = SUBSTRING(cFunction, LENGTH(cFunctionName) + 1).

    if cFunctionName = "":U THEN
    DO:
      ASSIGN pcError = SUBSTITUTE("Invalid tag: &1":U, cCallTag).
      RETURN FALSE.
    END.

    ASSIGN
      cEndTag  = {&xcFieldEnd} + {&xcFunctionDelimiter} + cFunctionName + {&xcScriptTagEnd}
      iOffset  = INDEX(pcScript, cEndTag).

    IF iOffset = 0 THEN
    DO:
      ASSIGN pcError = SUBSTITUTE("Missing END tag: &1":U, cEndTag).
      RETURN FALSE.
    END.

    ASSIGN
      cFieldScript  = SUBSTRING(pcScript, 1, iOffset - 1)
      pcScript      = SUBSTRING(pcScript, iOffset + LENGTH(cEndTag))

      cParameters   = TRIM( cParameters )
      cParameters   = TRIM( cParameters, "().":U )
      cParameters   = TRIM( cParameters )
      iNumParams    = NUM-ENTRIES( cParameters ).

    /* Use dynamic call */
    CREATE CALL hFunction.
    ASSIGN
      hFunction:CALL-TYPE = FUNCTION-CALL-TYPE
      hFunction:CALL-NAME = cFunctionName
      hFunction:IN-HANDLE = TARGET-PROCEDURE:HANDLE
    /*
    ASSIGN
      cFullScript = SUBSTRING( pcScript, 1, iEnd + LENGTH( {&xcFieldStartEnd} ) - 1 )
      pcScript    = SUBSTRING( pcScript, LENGTH( cFullScript ) + 1 )
      cFunction   = REPLACE( cFullScript, {&xcFieldStartEnd}, "":U )
      cFunction   = TRIM( cFunction, "~"" )
      cFunction   = TRIM( cFunction, "'" )
      cFunction   = TRIM( cFunction, "~"" ) /* in case it was '"' */
      cFunction   = TRIM( cFunction )
      cParams     = ENTRY( 2, cFunction, "(":U )
      cFunction   = ENTRY( 1, cFunction, "(":U )
      cFunction   = TRIM( cFunction )
      cParams     = TRIM( cParams )
      cParams     = TRIM( cParams, ").":U )
      cParams     = TRIM( cParams )
      iLen        = NUM-ENTRIES( cParams ).

    iEndField = INDEX( pcScript, {&xcFieldEndStart} + cFieldName + {&xcFieldEndEnd}).
    IF iEndField < 2 THEN DO:
        pcError = {&xcFieldStart} + cFieldName + " without " + {&xcFieldEndStart} + cFieldName + {&xcFieldEndEnd}.
        RETURN FALSE.
    END.

    ASSIGN
      cFieldScript  = SUBSTRING( pcScript, 1, iEndField + LENGTH( {&xcFieldEndStart} + cFieldName + {&xcFieldEndEnd} ) - 1 )
      pcScript      = SUBSTRING( pcScript, LENGTH( cFieldScript ) + 1 )
      cFieldScript  = REPLACE( cFieldScript, {&xcFieldEndStart} + cFieldName + {&xcFieldEndEnd}, "":U ).

    /* Use dynamic call */
    CREATE CALL hFunction.
    ASSIGN
      hFunction:CALL-TYPE      = FUNCTION-CALL-TYPE
      hFunction:CALL-NAME      = cFunction
      hFunction:IN-HANDLE      = TARGET-PROCEDURE:HANDLE
    */
      hFunction:NUM-PARAMETERS = iLen + 1.

    REPEAT iIdx = 1 TO iLen:
        ASSIGN
          cParam      = ENTRY( iIdx, cParams )
          cParam      = TRIM( cParam )
          cParam      = TRIM( cParam, "~"" )
          cParam      = TRIM( cParam, "'" )
          cParam      = TRIM( cParam, "~"" )
          cFuncParams = SUBSTITUTE ( "&1&2&3":U,
                                     cFuncParams,
                                     (IF LENGTH( cFuncParams ) > 0 THEN ",":U ELSE "":U),
                                     cParam ).

        hFunction:SET-PARAMETER(iIdx, "CHARACTER":U, "INPUT":U, cParam).
    END.

    /* add common parameters */
    hFunction:SET-PARAMETER(iLen + 1, "CHARACTER":U, "INPUT":U, cFieldScript).

    hFunction:INVOKE() NO-ERROR.

    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
        MESSAGE
          "Function =" cFunction "(" cFuncParams ")"
          "NumParams =" iLen
          "Target-Procedure =" TARGET-PROCEDURE:FILE-NAME
          "~nError =" ERROR-STATUS:GET-MESSAGE( 1 ).

    ASSIGN cValue = hFunction:RETURN-VALUE.

    DELETE OBJECT hFunction NO-ERROR.

    IF cValue = ? THEN
      RETURN FALSE.

    IF NOT DYNAMIC-FUNCTION("evaluateScript":U IN TARGET-PROCEDURE, INPUT cValue, OUTPUT pcError) THEN
      RETURN FALSE.

    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-flushBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION flushBuffer Procedure 
FUNCTION flushBuffer RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-flushStream) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION flushStream Procedure 
FUNCTION flushStream RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  PUT {&WEBSTREAM} CONTROL NULL.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContentType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getContentType Procedure 
FUNCTION getContentType RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN "text/html":U.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMessages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMessages Procedure 
FUNCTION getMessages RETURNS CHARACTER
  ( INPUT pcGroup AS CHARACTER,
    INPUT plDelete AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  IF pcGroup = "?":U OR pcGroup = "*":U THEN
    ASSIGN pcGroup = ?.

  RETURN get-messages(INPUT pcGroup, INPUT plDelete).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPageBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPageBuffer Procedure 
FUNCTION getPageBuffer RETURNS MEMPTR
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iOffset     AS INTEGER INITIAL 1              NO-UNDO.
  DEFINE VARIABLE iBlockSize  AS INTEGER INITIAL {&BUFFER_SIZE} NO-UNDO.
  DEFINE VARIABLE ptrTemp     AS MEMPTR                         NO-UNDO.
  DEFINE VARIABLE rawData     AS RAW                            NO-UNDO.

  SET-SIZE(ptrTemp) = 0.

  IF glBufferEnabled AND giContentLength > 0 THEN
  DO:
    SET-SIZE(ptrTemp) = giContentLength.

    /* Copy existing buffer contents into return buffer */
    DO WHILE iOffset <= giContentLength:
      IF iOffset + iBlockSize > giContentLength THEN
        ASSIGN iBlockSize = giContentLength - iOffset + 1.

      ASSIGN
        LENGTH(rawData)             = iBlockSize                                /* allocate   */
        rawData                     = GET-BYTES(ptrBuffer, iOffset, iBlockSize) /* copy       */
        PUT-BYTES(ptrTemp, iOffset) = rawData                                   /* paste      */
        LENGTH(rawData)             = 0                                         /* deallocate */
        iOffset                     = iOffset + iBlockSize.
    END.
  END.

  RETURN ptrTemp.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hasMessages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hasMessages Procedure 
FUNCTION hasMessages RETURNS LOGICAL
  ( INPUT pcGroup AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  IF pcGroup = "?":U OR pcGroup = "*":U THEN
    ASSIGN pcGroup = ?.

  RETURN available-messages(INPUT pcGroup).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-includeMarkup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION includeMarkup Procedure 
FUNCTION includeMarkup RETURNS LOGICAL
  ( INPUT-OUTPUT pcScript AS CHARACTER,
    output pcError AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cFullScript     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIncludeFile    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIncludeScript  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iEnd            AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSaveTextId     AS INTEGER    NO-UNDO.

  ASSIGN
    pcScript = SUBSTRING( pcScript, LENGTH( {&xcIncludeStart} + {&xcFunctionDelimiter}) + 1 )
    iEnd     = INDEX( pcScript, {&xcIncludeEnd} ).

  IF iEnd = 0 THEN
  DO:
    pcError = {&xcIncludeStart} + " without " + {&xcIncludeEnd}.
    RETURN FALSE.
  END.

  ASSIGN
    cFullScript    = SUBSTRING ( pcScript, 1, iEnd - 1 )
    pcScript       = SUBSTRING ( pcScript, LENGTH( cFullScript + {&xcIncludeEnd} ) + 1 )
    /*
    cFullScript    = TRIM ( REPLACE ( cFullScript, {&xcFunctionDelimiter}, " ":U ) )
    */
    cFullScript    = TRIM ( cFullScript, " ":U )
    cIncludeFile   = ENTRY ( 1, cFullScript, " ":U)
    cIncludeFile   = TRIM( cIncludeFile, "~"" )
    cIncludeFile   = TRIM( cIncludeFile, "'" )
    cIncludeFile   = TRIM( cIncludeFile, "~"" ) /* in case it was '"' */
    cIncludeFile   = TRIM( cIncludeFile )

    cIncludeScript = DYNAMIC-FUNCTION("fetchMarkup":U IN TARGET-PROCEDURE, cIncludeFile).

  IF cFullScript <> cIncludeFile THEN
    cIncludeScript = DYNAMIC-FUNCTION ( "replaceArguments":U IN TARGET-PROCEDURE,
                                        INPUT cIncludeScript,
                                        INPUT cFullScript ).

  IF NOT DYNAMIC-FUNCTION("evaluateScript":U IN TARGET-PROCEDURE, INPUT cIncludeScript, OUTPUT pcError) THEN
    RETURN FALSE.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initPage Procedure 
FUNCTION initPage RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  ASSIGN
    giContentLength     = 0
    SET-SIZE(ptrBuffer) = 0
    SET-SIZE(ptrBuffer) = giBufferSize
    glBufferEnabled     = NOT DYNAMIC-FUNCTION("disablePageBuffer":U IN TARGET-PROCEDURE)
    NO-ERROR.

  IF NOT glBufferEnabled THEN
    DYNAMIC-FUNCTION("outputHeader":U IN TARGET-PROCEDURE).

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isDefined) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isDefined Procedure 
FUNCTION isDefined RETURNS LOGICAL
  ( INPUT pcArgValue AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN (pcArgValue <> ? AND pcArgValue > "":U).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isGet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isGet Procedure 
FUNCTION isGet RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN REQUEST_METHOD = "get":U.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isPost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isPost Procedure 
FUNCTION isPost RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN REQUEST_METHOD = "post":U.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isSelected) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isSelected Procedure 
FUNCTION isSelected RETURNS CHARACTER
  ( INPUT pcField AS CHARACTER,
    INPUT pcValue AS CHARACTER,
    INPUT pcSelected AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  IF CAN-DO(get-field(?), pcField) AND get-field(pcField) = pcValue THEN
    RETURN pcSelected.

  RETURN "":U.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-outputHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION outputHeader Procedure 
FUNCTION outputHeader RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cContentType AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMimeCharset AS CHARACTER  NO-UNDO.

  IF DYNAMIC-FUNCTION("disablePageCache":U IN TARGET-PROCEDURE) THEN
  DO:
    output-http-header("Expires":U, "Mon, 01 Nov 1971 00:00:00 GMT":U).
    output-http-header("Pragma":U, "no-cache":U).
    output-http-header("Cache-Control":U, "no-cache":U).
  END.

  IF glBufferEnabled AND giContentLength > 0 THEN
    output-http-header("Content-Length":U, STRING(giContentLength)).

  DYNAMIC-FUNCTION("setCookies":U IN TARGET-PROCEDURE).

  ASSIGN cContentType = DYNAMIC-FUNCTION("getContentType":U IN TARGET-PROCEDURE).

  &IF KEYWORD-ALL("HTML-CHARSET":U) <> ? &THEN
  /* Sascha Hoffman - Add MIME codepage if available */
  IF cContentType BEGINS "text/":U AND INDEX(cContentType, "charset":U) = 0 AND WEB-CONTEXT:HTML-CHARSET <> "":U THEN
  DO:
    RUN adecomm/convcp.p (WEB-CONTEXT:HTML-CHARSET, "toMime":U, OUTPUT cMimeCharset) NO-ERROR.
    IF cMimeCharset > "":U THEN
      ASSIGN cContentType = SUBSTITUTE("&1; charset=&2":U, cContentType, cMimeCharset).
  END.
  &ENDIF

  output-http-header("Content-Type":U, cContentType).
  output-http-header("":U, "":U).

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-outputPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION outputPage Procedure 
FUNCTION outputPage RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iOffset     AS INTEGER INITIAL 1              NO-UNDO.
  DEFINE VARIABLE iBlockSize  AS INTEGER INITIAL {&BUFFER_SIZE} NO-UNDO.
  DEFINE VARIABLE rawData     AS RAW                            NO-UNDO.

  DYNAMIC-FUNCTION("outputHeader":U IN TARGET-PROCEDURE).

  DO WHILE iOffset <= giContentLength:
    IF iOffset + iBlockSize > giContentLength THEN
      ASSIGN iBlockSize = giContentLength - iOffset + 1.

    ASSIGN
      LENGTH(rawData) = iBlockSize
      rawData         = GET-BYTES(ptrBuffer, iOffset, iBlockSize).

    PUT {&WEBSTREAM} CONTROL rawData.

    ASSIGN
      iOffset = iOffset + iBlockSize
      LENGTH(rawData) = 0.
  END.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-preprocessMarkup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION preprocessMarkup Procedure 
FUNCTION preprocessMarkup RETURNS CHARACTER PRIVATE
  ( INPUT pcScript AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cReturn AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iStart  AS INTEGER    NO-UNDO.

  /* Pre-process to remove DISCARD blocks */
  iStart = INDEX(pcScript, {&xcDiscardStart}).

  DO WHILE iStart > 0:
    IF iStart > 0 THEN
      ASSIGN
        cReturn   = cReturn + SUBSTRING(pcScript, 1, iStart - 1)
        pcScript  = SUBSTRING(pcScript, iStart + LENGTH({&xcDiscardStart}))
        pcScript  = SUBSTRING(pcScript, INDEX( pcScript, {&xcDiscardEnd} ) + LENGTH ( {&xcDiscardEnd} ) ).
    iStart = INDEX(pcScript, {&xcDiscardStart}).
  END.

  RETURN (cReturn + pcScript).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processMarkup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION processMarkup Procedure 
FUNCTION processMarkup RETURNS LOGICAL
  ( INPUT pcMarkupName AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cMarkup   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cError    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lPageOk   AS LOGICAL    NO-UNDO.

  DYNAMIC-FUNCTION("initPage":U IN TARGET-PROCEDURE).

  ASSIGN
    cMarkup = DYNAMIC-FUNCTION("fetchMarkup":U IN TARGET-PROCEDURE, INPUT pcMarkupName)
    lPageOk = DYNAMIC-FUNCTION("evaluateScript":U IN TARGET-PROCEDURE, INPUT cMarkup, OUTPUT cError).

  IF glBufferEnabled THEN
  DO:
    IF lPageOk THEN
      DYNAMIC-FUNCTION("outputPage":U IN TARGET-PROCEDURE).
    ELSE
      DYNAMIC-FUNCTION("errorPage":U IN TARGET-PROCEDURE, INPUT cError).
  END.

  RETURN lPageOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-queueMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION queueMessage Procedure 
FUNCTION queueMessage RETURNS LOGICAL
  ( INPUT pcGroup AS CHARACTER,
    INPUT pcMessage AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  queue-message(pcGroup, pcMessage).

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-readFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION readFile Procedure 
FUNCTION readFile RETURNS CHARACTER PRIVATE
  ( INPUT pcFilename AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cScript   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ptrHTML AS MEMPTR NO-UNDO.

  FILE-INFO:FILE-NAME = pcFilename.

  IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
    IF DEBUGGING-ENABLED THEN
      MESSAGE SUBSTITUTE("Reading file &1":U, FILE-INFO:FULL-PATHNAME).

    SET-SIZE(ptrHTML) = FILE-INFO:FILE-SIZE.

    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) BINARY NO-CONVERT.
    IMPORT ptrHTML.
    INPUT CLOSE.

    /* 32k limit on Progress character type means we have to truncate HTML */
    IF GET-SIZE(ptrHTML) > {&MAX_CHAR_SIZE} THEN
      cScript = GET-STRING(ptrHTML, 1, {&MAX_CHAR_SIZE}).
    ELSE
      cScript = GET-STRING(ptrHTML, 1).

    SET-SIZE(ptrHTML) = 0.
  END.

  RETURN (cScript).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-replaceArguments) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION replaceArguments Procedure 
FUNCTION replaceArguments RETURNS CHARACTER
  ( INPUT pcScript AS CHARACTER,
    INPUT pcArguments AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cArg      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValue    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iOffset   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEnd      AS INTEGER    NO-UNDO.

  DO WHILE LENGTH(pcArguments) > 0:
    ASSIGN iOffset = INDEX(pcArguments, "$":U).
    IF iOffset = 0 THEN
      LEAVE.
  
    ASSIGN iEnd = INDEX(pcArguments, "$":U, iOffset + 1) - 1.
    IF iEnd < 0 THEN
      ASSIGN iEnd = LENGTH(pcArguments) + 1.
  
    ASSIGN
      cArg        = SUBSTRING(pcArguments, iOffset, iEnd - iOffset)
      pcArguments = SUBSTRING(pcArguments, iEnd)
      iEnd        = INDEX(cArg, "=":U)
      cValue      = SUBSTRING(cArg, iEnd + 1)
      cValue      = TRIM(cValue, "~"":U)
      cValue      = TRIM(cValue, "'":U)
      cArg        = SUBSTRING(cArg, 1, iEnd - 1)
      pcScript    = REPLACE ( pcScript, cArg, cValue ).
  END.

  /*
  DEFINE VARIABLE iArgCount AS INTEGER    NO-UNDO.

  DO iArgCount = 1 TO NUM-ENTRIES(pcArguments, " ":U):
    ASSIGN cArg = ENTRY ( iArgCount, pcArguments, " ":U ).

    IF cArg BEGINS "$":U THEN
    DO:
      ASSIGN
        cValue = ENTRY ( 2, cArg, "=":U )
        cArg   = ENTRY ( 1, cArg, "=":U )
        NO-ERROR.

      IF cArg > "":U THEN
      DO:
        IF cValue = ? THEN
          ASSIGN cValue = "":U.

        ASSIGN pcScript = REPLACE ( pcScript, cArg, cValue ).
      END.
    END.
  END.
  */
  RETURN pcScript.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-resizeBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION resizeBuffer Procedure 
FUNCTION resizeBuffer RETURNS LOGICAL PRIVATE
  ( INPUT-OUTPUT ptrBuffer AS MEMPTR,
    INPUT piLength AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iOffset     AS INTEGER INITIAL 1              NO-UNDO.
  DEFINE VARIABLE iBlockSize  AS INTEGER INITIAL {&BUFFER_SIZE} NO-UNDO.
  DEFINE VARIABLE ptrTemp     AS MEMPTR                         NO-UNDO.
  DEFINE VARIABLE rawData     AS RAW                            NO-UNDO.

  /* Define new temporary */
  ASSIGN
    giBufferSize      = giBufferSize + MAX(piLength, {&BUFFER_SIZE})
    SET-SIZE(ptrTemp) = giBufferSize.

  /* Copy existing buffer contents into new buffer */
  DO WHILE iOffset <= giContentLength:
    IF iOffset + iBlockSize > giContentLength THEN
      ASSIGN iBlockSize = giContentLength - iOffset + 1.

    ASSIGN
      LENGTH(rawData)             = iBlockSize
      rawData                     = GET-BYTES(ptrBuffer, iOffset, iBlockSize)
      PUT-BYTES(ptrTemp, iOffset) = rawData
      LENGTH(rawData)             = 0
      iOffset                     = iOffset + iBlockSize.
  END.

  /* Reset buffer pointer */
  ASSIGN
    SET-SIZE(ptrBuffer)  = 0
    ptrBuffer            = ptrTemp
    SET-SIZE(ptrTemp)    = 0.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-scriptTags) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION scriptTags Procedure 
FUNCTION scriptTags RETURNS LOGICAL PRIVATE
  ( INPUT pcTagStart AS CHARACTER,
    INPUT pcTagEnd AS CHARACTER,
    INPUT plEmbedValue AS LOGICAL,
    INPUT-OUTPUT pcScript AS CHARACTER,
    OUTPUT pcError AS CHARACTER ) :
/*------------------------------------------------------------------------------

------------------------------------------------------------------------------*/

    DEFINE VARIABLE cFullScript AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParams     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParam      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFuncParams AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFunction   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hFunction   AS HANDLE NO-UNDO.
    DEFINE VARIABLE iEnd        AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLen        AS INTEGER NO-UNDO.
    DEFINE VARIABLE iIdx        AS INTEGER NO-UNDO.

    ASSIGN
      pcScript = SUBSTRING( pcScript, LENGTH( pcTagStart ) + 1 )
      iEnd   = INDEX( pcScript, pcTagEnd ).

    IF iEnd < 1 THEN DO:
        pcError = SUBSTITUTE( "ERROR: &1 with no matching &2", pcTagStart, pcTagEnd ).
        RETURN FALSE.
    END.

    ASSIGN
      cFullScript = SUBSTRING( pcScript, 1, iEnd + LENGTH( pcTagEnd ) - 1 )
      pcScript    = SUBSTRING( pcScript, LENGTH( cFullScript ) + 1 )
      cFunction   = REPLACE( cFullScript, pcTagEnd, "":U )
      cParams     = ENTRY( 2, cFunction, "(":U )
      cFunction   = ENTRY( 1, cFunction, "(":U )
      cFunction   = TRIM( cFunction )
      cParams     = TRIM( cParams )
      cParams     = TRIM( cParams, ").":U )
      cParams     = TRIM( cParams )
      iLen        = NUM-ENTRIES( cParams ).

    /* Use dynamic call */
    CREATE CALL hFunction.
    ASSIGN
      hFunction:CALL-TYPE      = FUNCTION-CALL-TYPE
      hFunction:CALL-NAME      = cFunction
      hFunction:IN-HANDLE      = TARGET-PROCEDURE:HANDLE
      hFunction:NUM-PARAMETERS = iLen.

    REPEAT iIdx = 1 TO iLen:
        ASSIGN
          cParam      = ENTRY( iIdx, cParams )
          cParam      = TRIM( cParam )
          cParam      = TRIM( cParam, "~"" )
          cParam      = TRIM( cParam, "'" )
          cParam      = TRIM( cParam, "~"" )

          cFuncParams = SUBSTITUTE ( "&1&2&3":U,
                                     cFuncParams,
                                     IF LENGTH( cFuncParams ) > 0 THEN "," ELSE "",
                                     cParam ).

        hFunction:SET-PARAMETER(iIdx, "CHARACTER":U, "INPUT":U, cParam).
    END.

    hFunction:INVOKE() NO-ERROR.

    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
        MESSAGE
          "FUNCTION =" cFunction "(" cFuncParams ")"
          "NumParams =" iLen
          "TARGET-PROCEDURE =" TARGET-PROCEDURE:FILE-NAME
          "~nError =" ERROR-STATUS:GET-MESSAGE( 1 ).

    ASSIGN cValue = hFunction:RETURN-VALUE.

    DELETE OBJECT hFunction NO-ERROR.

    IF cValue <> ? AND plEmbedValue AND LENGTH ( cValue ) > 0 THEN
      appendString(cValue).

    RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCookies) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCookies Procedure 
FUNCTION setCookies RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPreprocess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPreprocess Procedure 
FUNCTION setPreprocess RETURNS LOGICAL
  ( INPUT plPreprocess AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  IF plPreprocess <> ? THEN
    ASSIGN glPreprocess = plPreprocess.

  RETURN glPreprocess.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION showField Procedure 
FUNCTION showField RETURNS CHARACTER
  ( INPUT pcScript AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN pcScript.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-streamFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION streamFile Procedure 
FUNCTION streamFile RETURNS LOGICAL
  ( INPUT pcFilename AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cContentType  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFileExtn     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFilename    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPos      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE rawData       AS RAW        NO-UNDO.

  ASSIGN
    iPos                        = NUM-ENTRIES(pcFilename, ".")
    cFileExtn                   = ENTRY(iPos, pcFilename, ".")
    FILE-INFORMATION:FILE-NAME  = pcFilename.

  IF FILE-INFORMATION:FULL-PATHNAME <> ? THEN
  DO:
    ASSIGN
      cFilename = REPLACE(pcFilename, "~\":U, "~/":U)
      iPos      = NUM-ENTRIES(cFilename, "~/":U)
      cFilename = ENTRY(iPos, cFilename, "~/":U).

    /* Calculate the Content Type from the Extension */
    CASE cFileExtn:
      WHEN "ZIP":U THEN
        cContentType = "application/x-zip-compressed":U.
      WHEN "PDF":U THEN
        cContentType = "application/pdf":U.
      WHEN "DOC":U OR
      WHEN "RTF":U THEN
        cContentType = "application/msword":U.
      WHEN "XLS":U THEN
        cContentType = "application/vnd.ms-excel":U.
      WHEN "EXE":U THEN
        cContentType = "application/exe":U.
      WHEN "GIF":U THEN
        cContentType = "image/gif":U.
      WHEN "TXT":U OR
      WHEN "LOG":U THEN
        cContentType = "text/plain":U.
      WHEN "JPEG":U OR
      WHEN "JPG":U THEN
        cContentType = "image/jpeg":U.
      WHEN "PNG":U THEN
        cContentType = "image/png":U.
      OTHERWISE
        cContentType = "application/octet-stream":U.  /* last resort */
    END.

    /* Output it */
    {&OUT}
      "Content-Type: ":U cContentType SKIP
      "Content-Length: ":U FILE-INFORMATION:FILE-SIZE
      "Content-Disposition: ":U SUBSTITUTE("attachment; filename=~"&1~"":U, cFilename)
      SKIP(1).

    INPUT FROM VALUE(FILE-INFORMATION:FULL-PATHNAME) BINARY NO-MAP NO-CONVERT.

    REPEAT:
      ASSIGN LENGTH(rawData) = 8192.
      IMPORT UNFORMATTED rawData.
      PUT {&WEBSTREAM} CONTROL rawData.
      ASSIGN LENGTH(rawData) = 0.  /* Deallocate memory */
    END.

    INPUT CLOSE.

    RETURN TRUE.
  END.

  RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-writeFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION writeFile Procedure 
FUNCTION writeFile RETURNS LOGICAL
  ( INPUT pcFilename AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iOffset    AS INTEGER INITIAL 1              NO-UNDO.
  DEFINE VARIABLE iBlockSize AS INTEGER INITIAL {&BUFFER_SIZE} NO-UNDO.
  DEFINE VARIABLE rawData    AS RAW                            NO-UNDO.

  OUTPUT TO VALUE(pcFilename).

  DO WHILE iOffset <= giContentLength:
    IF iOffset + iBlockSize > giContentLength THEN
      ASSIGN iBlockSize = giContentLength - iOffset + 1.

    ASSIGN
      LENGTH(rawData) = iBlockSize
      rawData         = GET-BYTES(ptrBuffer, iOffset, iBlockSize).

    PUT CONTROL rawData.

    ASSIGN
      iOffset         = iOffset + iBlockSize
      LENGTH(rawData) = 0.
  END.

  OUTPUT CLOSE.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

