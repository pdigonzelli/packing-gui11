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

frame f-botones:frame   = frame {&FRAME-NAME}:handle.
/*-- CARGO IMAGENES A LOS BOTONES --*/
b_help:load-image-up("../im-util.ico").
b_help:load-image-insensitive("../im-util.ico") .

b_print:load-image-up("../im-print.ico").
b_print:load-image-insensitive("../im-print.ico").

b_mail:load-image-up("../im-mail1.ico").
b_mail:load-image-insensitive("../im-mail1.ico").

b_calc:load-image-up("../im-calc.ico").
b_calc:load-image-insensitive("../im-calc.ico").

b_passwd:load-image-up("../im-passw1.ico").
b_passwd:load-image-insensitive("../im-passw1.ico").


enable all with frame f-botones.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


