&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include
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
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*--------------------------------------------------------------------------
    File        : escrprop.i
    Purpose     : Defines basic properties.
    Syntax      : {src/web/escrprop.i}

    Description :

    Modified    : 24/03/2003
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

  /* Custom instance definition file */

  {src/web/custom/escriptdefscustom.i}

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
         HEIGHT             = 8
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include


/* ***************************  Main Block  *************************** */

  /* Include the file which defines prototypes for all of the super
     procedure's entry points.
     And skip including the prototypes if we are *any* super procedure. */
&IF "{&ADMSuper}":U EQ "":U &THEN
  {src/web/escrprto.i}
&ENDIF

  /* Put your xp{&Property} preprocessor definitions here.
     Use the following format, e.g.,
     &GLOBAL-DEFINE xpMyProperty
     These preprocessors tell at compile time which properties can
     be retrieved directly from the temp-table */

/* Now include the next-level-up property include file. This builds up the
   property temp-table definition, to which we will add our field definitions. */
{src/adm2/smrtprop.i}


&IF "{&ADMSuper}":U = "":U &THEN
  /* Put your property field definitions here.
     Use the following syntax, e.g.,
     ghADMProps:ADD-NEW-FIELD('MyProperty':U, 'CHAR':U, 0,'X(20)':U, 'Hi':U). */

&ENDIF

  {src/web/custom/escrpropcustom.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


