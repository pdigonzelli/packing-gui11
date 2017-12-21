&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*--------------------------------------------------------------------------
    File        : toolbarcustom.p
    Purpose     : Super procedure to extend toolbar class.

    Syntax      : adm2/custom/toolbarcustom.p

    Modified    : 05/31/1999
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ADMSuper toolbarcustom.p

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



&IF DEFINED(EXCLUDE-initializeMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initializeMenu Procedure 
FUNCTION initializeMenu RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initializeToolbar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initializeToolbar Procedure 
FUNCTION initializeToolbar RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

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

{src/adm2/toolprop.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-initAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initAction Procedure 
PROCEDURE initAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER. 

  /*MESSAGE "tooblarcustom.p->initAction" VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
  
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

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-initializeMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initializeMenu Procedure 
FUNCTION initializeMenu RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/



  DYNAMIC-FUNCTION('insertMenu' IN TARGET-PROCEDURE, "", "File,Navigation,Banda1,Banda2,Banda3", no, ?) NO-ERROR.
  
  DYNAMIC-FUNCTION('insertMenu' IN TARGET-PROCEDURE, "File",
     "Add,Update,Copy,Delete,RULE,":U
  +  "Save,Reset,Cancel,RULE,Transaction,":U
  +  "RULE,Function,RULE,Exit":U,
      yes, /* expand children */
      ?) NO-ERROR.

 /* build the menubar */
 /*DYNAMIC-FUNCTION('buildMenu' IN TARGET-PROCEDURE, INPUT "").*/

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initializeToolbar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initializeToolbar Procedure 
FUNCTION initializeToolbar RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*MESSAGE "toolbarcustom.p->initializeToolbar" VIEW-AS ALERT-BOX INFO BUTTONS OK.*/



  DYNAMIC-FUNCTION('createToolBar' IN TARGET-PROCEDURE, "Tableio,RULE,Transaction,RULE,Navigation,RULE,Function,RULE,Banda1,RULE,Banda2,RULE,Banda3") NO-ERROR.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

