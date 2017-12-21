&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */
  IF SESSION:WINDOW-SYSTEM = 'TTY' THEN
        STATUS INPUT 'F1:AGREGA , F2:BORRA , F3:RELACIONA , F5:GRABA , F10:CANCELA , F11:RESET'.
  
  ON 'insert':U , 'f1':U OF FRAME {&FRAME-NAME} ANYWHERE 
  DO:
      RUN addRecord IN h_v NO-ERROR.
      RETURN.
  END.
  
  ON 'f5':U OF FRAME {&FRAME-NAME} ANYWHERE 
  DO:
      DEFINE VAR lModified AS LOGICAL NO-UNDO.
      lModified = DYNAMIC-FUNCTION('getDataModified' IN h_v).
      IF lModified THEN
        RUN updateRecord IN h_v NO-ERROR.
      RETURN.
  END.
  
  ON 'f2':U OF FRAME {&FRAME-NAME} ANYWHERE 
  DO:
      RUN deleteRecord IN h_v NO-ERROR.
      RETURN.
  END.
  
  ON 'f10':U OF FRAME {&FRAME-NAME} ANYWHERE 
  DO:
      RUN cancelRecord IN h_v NO-ERROR.
      RETURN.
  END.
  
  ON 'f11':U OF FRAME {&FRAME-NAME} ANYWHERE 
  DO:
      RUN resetRecord IN h_v NO-ERROR.
      RETURN.
  END.
  
  ON 'ALT-CURSOR-RIGHT':U OF FRAME {&FRAME-NAME} ANYWHERE 
  DO:
      RUN fetchLast IN h_d NO-ERROR.
      RETURN.
  END.
  ON 'ALT-CURSOR-LEFT':U OF FRAME {&FRAME-NAME} ANYWHERE 
  DO:
      RUN fetchFirst IN h_d NO-ERROR.
      RETURN.
  END.

  ON 'F3':U OF FRAME {&FRAME-NAME} ANYWHERE
  DO:
      APPLY 'MOUSE-SELECT-DBLCLICK' TO SELF.
      RETURN.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


