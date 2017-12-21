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
         HEIGHT             = 1.99
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/************************************/
/* EVENTO: F2 ACCEDER A LAS AYUDAS  */
/************************************/
on "CHOOSE" of b_help in frame f-botones
 do:
         run applhelpg.p persistent (input current-window).
 end.
 
/****************************************/
/* EVENTO: CLICK EN BOTON DE IMPRESION  */
/****************************************/
on "CHOOSE" of b_print in frame f-botones
 do:
    run h_print.p.
 end.
 
/****************************************/
/* EVENTO: CLICK EN BOTON DE MAIL       */
/****************************************/
on "CHOOSE" of b_mail in frame f-botones
 do:
    run h_xmail.p.
 end.

/*****************************************/
/* EVENTO: CLICK EN BOTON DE CALCULADORA */
/*****************************************/
on "CHOOSE" of b_calc in frame f-botones
 do:
    run h_calcg.p (input 10, input 10, output c-calculo).
 end.

/**************************************/
/* EVENTO: CLICK EN BOTON DE PASSWORD */
/**************************************/
on "CHOOSE" of b_passwd in frame f-botones
 do:
    run h_passwd.p.
 end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


