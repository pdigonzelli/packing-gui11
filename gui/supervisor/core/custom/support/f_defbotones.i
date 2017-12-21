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

define var c-calculo as character no-undo.
define button b_help      size 6 by 1.4 tooltip "Caja de herramientas".  
define button b_print     size 6 by 1.4 tooltip "Spool de Impresi¢n".  
define button b_mail      size 6 by 1.4 tooltip "Correo interno".
define button b_calc      size 6 by 1.4 tooltip "Calculadora". 
define button b_passwd    size 6 by 1.4 tooltip "Cambio de Claves de Acceso".

define frame f-botones
       b_help   at row 1 col 2 
       b_print  at row 1 col 9
       b_mail   at row 1 col 16
       b_calc   at row 1 col 23
       b_passwd at row 1 col 30 
       with no-box overlay at row {&row} col {&column} three-d.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


