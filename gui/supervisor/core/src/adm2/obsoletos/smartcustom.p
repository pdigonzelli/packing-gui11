&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : smartcustom.p
    Purpose     : Super procedure to extend smart class.

    Syntax      : smartcustom.p

    Modified    : 06/03/1999
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ADMSuper smartcustom.p

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-messageNumber) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD messageNumber Procedure  _DB-REQUIRED
FUNCTION messageNumber RETURNS CHARACTER
  ( piMessage AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD showMessage Procedure 
FUNCTION showMessage RETURNS LOGICAL
  ( pcMessage AS CHARACTER )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{src/adm2/smrtprop.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-messageNumber) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION messageNumber Procedure 
FUNCTION messageNumber RETURNS CHARACTER
  ( piMessage AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  Returns a message text given a message number. Allows these
            these messages to be translated and kept track of in one place
            (src/adm2/admmsgs.i)
   Params:  INPUT piMessage AS INTEGER
  Returns:  CHARACTER: message text
------------------------------------------------------------------------------*/

  {src/adm2/customadmmsgs.i}    /* Defines the array cADMMessages */
  
  RETURN cADMMessages[piMessage].


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION showMessage Procedure 
FUNCTION showMessage RETURNS LOGICAL
  ( pcMessage AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:     Displays (using a simple MESSAGE statement by default)
               either a literal message string, or a message number which
               is returned by the messageNumber function.
  Parameters: INPUT pcMessage AS CHARACTER -- 
              - Either a literal message string 
              - Or a message number in character form. 
               
               A message number can be followed by a comma delimited list
               with maximum 10 entries:
               
               The LAST entry (2 - 10) is:               
               1) The word "Question" or "YesNo", in which case the message is 
                  displayed with YES-NO buttons and the answer is returned.
               
               2) The word "YesNoCancel", in which case the message is displayed
                  with YES-NO-CANCEL buttons and the answer is returned.
                  
               Optional entries from 2 to 10: 
                  Each entry will be placed into the numeric message
                  in place of the the string of form &n, where n is an integer 
                  between 1 and 9, inclusive (Entry 2 will replace &1 etc)         
                  
    Returns:   LOGICAL: true/false if the Question option is used,
                        true/false/unknown if the YesNoCancel option is used 
                        else true.
  Notes:       This function can be overridden to use a mechanism other than
               the MESSAGE statement to display messages, and still use the
               messageNumber function to map message numbers to translatable text.
               Note that this is different from addMessage, fetchMessages, etc.,
               which log messages in a temp-table for later retrieval.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iMessage      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMessageType  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMsg          AS CHARACTER EXTENT 9 NO-UNDO.
  DEFINE VARIABLE iNumParam     AS INT       NO-UNDO.
  DEFINE VARIABLE lAnswer       AS LOGICAL   NO-UNDO.

 
  iMessage = INTEGER(ENTRY(1,pcMessage)) NO-ERROR.  /* was a number passed? */
  IF ERROR-STATUS:ERROR THEN 
    MESSAGE pcMessage VIEW-AS ALERT-BOX INFORMATION. /* No -- use the literal text */
  ELSE DO:   /* A numeric message */
    ASSIGN
      cMessage     = messageNumber(iMessage)
      iNumParam    = NUM-ENTRIES(pcMessage)
      cMessageType = ENTRY(iNumParam,pcMessage)
      cMsg[1]      = IF iNumParam > 1 THEN ENTRY(2,pcMessage) ELSE "":U
      cMsg[2]      = IF iNumParam > 2 THEN ENTRY(3,pcMessage) ELSE "":U
      cMsg[3]      = IF iNumParam > 3 THEN ENTRY(4,pcMessage) ELSE "":U
      cMsg[4]      = IF iNumParam > 4 THEN ENTRY(5,pcMessage) ELSE "":U
      cMsg[5]      = IF iNumParam > 5 THEN ENTRY(6,pcMessage) ELSE "":U
      cMsg[6]      = IF iNumParam > 6 THEN ENTRY(7,pcMessage) ELSE "":U
      cMsg[7]      = IF iNumParam > 7 THEN ENTRY(8,pcMessage) ELSE "":U
      cMsg[8]      = IF iNumParam > 8 THEN ENTRY(9,pcMessage) ELSE "":U
      cMsg[9]      = IF iNumParam > 9 THEN ENTRY(10,pcMessage) ELSE "":U      
      cMessage     = SUBSTITUTE(cMessage,
                                cMsg[1],cMsg[2],cMsg[3],cMsg[4],cMsg[5],
                                cMsg[6],cMsg[7],cMsg[8],cMsg[9]).
      
    /* Yes -- get the msg */
    CASE cMessageType:
      WHEN 'Question':U OR WHEN 'YesNo':U THEN
      DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
        UPDATE lAnswer.
        RETURN lAnswer.
      END.
      WHEN 'YesNoCancel':U THEN
      DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL 
        UPDATE lAnswer.
        RETURN lAnswer.
      END.
      OTHERWISE 
        MESSAGE cMessage VIEW-AS ALERT-BOX INFORMATION.      
    END CASE.
  END.  /* END ELSE IF numeric message */
  
  RETURN TRUE.       /* Return value not meaningful in this case. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

