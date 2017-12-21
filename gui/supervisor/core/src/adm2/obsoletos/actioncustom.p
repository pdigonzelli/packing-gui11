&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : actioncustom.p
    Purpose     : Super procedure to extend action class.

    Syntax      : adm2/custom/actioncustom.p

    Modified    : 05/25/1999
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ADMSuper actioncustom.p

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
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

{src/adm2/actiprop.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-externalAddRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE externalAddRecord Procedure 
PROCEDURE externalAddRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUBLISH('AddRecord').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initAction Procedure 
PROCEDURE initAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR xcColumns AS CHAR INIT "Name,Caption,Image,Type,OnChoose,AccessType,Parent,EnableRule".
  
  /*banda 1*/
  DYNAMIC-FUNCTION('defineAction' IN TARGET-PROCEDURE,"Banda1",
                                                      "Name,Caption",
                                                      "Banda1" + CHR(1) +
                                                      "Banda1") NO-ERROR.  

  /*banda 2*/
  DYNAMIC-FUNCTION('defineAction' IN TARGET-PROCEDURE,"Banda2",
                                                      "Name,Caption",
                                                      "Banda2" + CHR(1) +
                                                      "Banda2") NO-ERROR.
  
  /*banda 3*/
  DYNAMIC-FUNCTION('defineAction' IN TARGET-PROCEDURE,"Banda3",
                                                      "Name,Caption",
                                                      "Banda3" + CHR(1) +
                                                      "Banda3") NO-ERROR.
  
  
  /*boton de Impresion*/
  DYNAMIC-FUNCTION('defineAction' IN TARGET-PROCEDURE, "printAction",
                                                       xcColumns,
                                                       "printAction"      + CHR(1) +    /*Name*/
                                                       "Imprimir"         + CHR(1) +    /*Caption*/
                                                       "print.bmp"        + CHR(1) +    /*Image*/
                                                       "PUBLISH"          + CHR(1) +    /*Type*/
                                                       "tlbPrint"         + CHR(1) +    /*OnChoose*/
                                                       "Imprimir"         + CHR(1) +    /*AccessType*/
                                                       "Banda1"           + CHR(1) +    /*Parent*/
                                                       "") NO-ERROR.                    /*EnableRule*/
  
  /*boton de salida*/
  DYNAMIC-FUNCTION('defineAction' IN TARGET-PROCEDURE, "exitAction",
                                                       xcColumns,
                                                       "exitAction"       + CHR(1) +    /*Name*/
                                                       "Salir"            + CHR(1) +    /*Caption*/
                                                       "exit.bmp"         + CHR(1) +    /*Image*/
                                                       "PUBLISH"          + CHR(1) +    /*Type*/
                                                       "tlbExit"          + CHR(1) +    /*OnChoose*/
                                                       "Salir"            + CHR(1) +    /*AccessType*/
                                                       "Banda1"           + CHR(1) +    /*Parent*/
                                                       "") NO-ERROR.                    /*EnableRule*/
  
  /*boton de Exportacion a Excel*/
  DYNAMIC-FUNCTION('defineAction' IN TARGET-PROCEDURE, "excelAction",
                                                       xcColumns,
                                                       "excelAction"      + CHR(1) +    /*Name*/
                                                       "Exportar a Excel" + CHR(1) +    /*Caption*/
                                                       "excel.bmp"        + CHR(1) +    /*Image*/
                                                       "PUBLISH"          + CHR(1) +    /*Type*/
                                                       "tlbExcel"         + CHR(1) +    /*OnChoose*/
                                                       "Exportar a Excel" + CHR(1) +    /*AccessType*/
                                                       "Banda2"           + CHR(1) +    /*Parent*/
                                                       "") NO-ERROR.                    /*EnableRule*/

  /*boton custom para lo que se necesite*/
  DYNAMIC-FUNCTION('defineAction' IN TARGET-PROCEDURE, "customAction",
                                                       xcColumns,
                                                       "customAction"     + CHR(1) +    /*Name*/
                                                       ""                 + CHR(1) +    /*Caption*/
                                                       "ingreso.gif"      + CHR(1) +    /*Image*/
                                                       "PUBLISH"          + CHR(1) +    /*Type*/
                                                       "tlbCustom"        + CHR(1) +    /*OnChoose*/
                                                       ""                 + CHR(1) +    /*AccessType*/
                                                       "Banda3"           + CHR(1) +    /*Parent*/
                                                       "") NO-ERROR.                    /*EnableRule*/

  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

