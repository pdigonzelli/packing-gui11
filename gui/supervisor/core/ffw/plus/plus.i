/*-----------------------------------------------------------------------
 File: plus.i
 Purpose: Include for the pluspack object initializations.
 Description:
 Author(s) :PerSDigre/PSC
 Created: April 1998
 Notes:    
 Modification History:    
 $Header: /cvsroot/freeframework/ffw1.2/ffw/plus/plus.i,v 1.1 2002/08/21 16:14:25 freeframework Exp $
 $Log: plus.i,v $
 Revision 1.1  2002/08/21 16:14:25  freeframework
 Initial import

 Revision 1.1.1.1  2001/03/23 14:50:43  slichtenberg
 initial load 1.03


This file contains sample code which may assist you in creating applications.
You may use the code as you see fit. If you modify the code or include it in another software program,
you will refrain from identifying Progress Software as the supplier of the code, or using any
Progress Software trademarks in connection with your use of the code.
THE CODE IS NOT SUPPORTED BY PROGRESS SOFTWARE AND IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
INCLUDING, WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR NONINFRINGEMENT.
-----------------------------------------------------------------------*/
{plus/session.i}
{src/web/method/cgidefs.i}


/****** objects **************************************/

def new global shared var hProutil as handle no-undo.
def new global shared var hHTML    as handle no-undo.
def new global shared var hPlus    as handle no-undo.
def new global shared var hCode    as handle no-undo.
def new global shared var hConfig  as handle no-undo.


function fDoWithFiles returns char(input cAction as char,input cFiles as char,input cTask as char) in hCode.

/******************************************************/
/********* The HTML object ****************************/
/******************************************************/



&GLOBAL-DEFINE cColorHeader " bgcolor=#C5AF96 "
&GLOBAL-DEFINE cColorRow1   " bgcolor=#FFFFE6 "
&GLOBAL-DEFINE cBrowseTableDef " border=1 cellpadding=0 cellspacing=0 width=90% "

DEF NEW GLOBAL SHARED VAR cLink     AS CHAR NO-UNDO.

FUNCTION fLink       RETURNS CHAR(INPUT cMode AS CHAR
                                 ,INPUT cValue AS CHAR
                                 ,INPUT cText AS CHAR)    IN hHTML.
FUNCTION fRow        RETURNS CHAR(INPUT cData   AS CHAR)  IN hHTML.
FUNCTION fHRow       RETURNS CHAR(INPUT cLabels AS CHAR)  IN hHTML.
FUNCTION fBeginTable RETURNS CHAR(INPUT cLabels AS CHAR)  IN hHTML.
FUNCTION fTable      RETURNS CHAR(INPUT cLabels AS CHAR
                                 ,INPUT cData AS CHAR)    IN hHTML.
FUNCTION fProcess RETURNS CHARACTER(INPUT cCmd AS CHARACTER
                                   ,INPUT cAvoid AS CHAR) IN hHTML.



/******************************************************/
/********* The PLUS object (pluspack specific *********/
/******************************************************/


/********* Object for Updating status screen **********/
DEFINE NEW GLOBAL SHARED VARIABLE cBrk         AS CHARACTER NO-UNDO.  /** am server config   **/
DEFINE NEW GLOBAL SHARED VARIABLE cDB          AS CHARACTER NO-UNDO.  /** am server config   **/
DEFINE NEW GLOBAL SHARED VARIABLE cDir         AS CHARACTER NO-UNDO.  /** am app config      **/
DEFINE NEW GLOBAL SHARED VARIABLE cLib         AS CHARACTER NO-UNDO.  /** am app config      **/
DEFINE NEW GLOBAL SHARED VARIABLE cTsk         AS CHARACTER NO-UNDO.  /** am app config      **/

DEFINE NEW GLOBAL SHARED VARIABLE cStatic      AS CHARACTER NO-UNDO.  /** location of images **/
DEFINE NEW GLOBAL SHARED VARIABLE cFormTarget  AS CHARACTER NO-UNDO.  /** HTML Header param  **/
DEFINE NEW GLOBAL SHARED VARIABLE cFormHelp    AS CHARACTER NO-UNDO.  /** HTML Header param  **/
DEFINE NEW GLOBAL SHARED VARIABLE cFormBack    AS CHARACTER NO-UNDO.  /** HTML Header param  **/
DEFINE NEW GLOBAL SHARED VARIABLE cFormRefresh AS CHARACTER NO-UNDO.  /** HTML Header param  **/
DEFINE NEW GLOBAL SHARED VARIABLE cFormTitle   AS CHARACTER NO-UNDO.  /** HTML Header param  **/

DEFINE NEW GLOBAL SHARED VARIABLE cAction      AS CHARACTER NO-UNDO.  /** HTML Command       **/
DEFINE NEW GLOBAL SHARED VARIABLE cName        AS CHARACTER NO-UNDO.  /** HTML param         **/
DEFINE NEW GLOBAL SHARED VARIABLE cFiles       AS CHARACTER NO-UNDO.  /** Files selected     **/
DEFINE NEW GLOBAL SHARED VARIABLE cPath        AS CHARACTER NO-UNDO.  /** CodePath selected  **/
DEFINE NEW GLOBAL SHARED VARIABLE cTask        AS CHARACTER NO-UNDO.  /** Task  selected     **/

DEFINE NEW GLOBAL SHARED VARIABLE cTaskFiles   AS CHARACTER NO-UNDO.  /** list of full paths to non progress directories ***/
DEFINE NEW GLOBAL SHARED VARIABLE cProPath     AS CHARACTER NO-UNDO.  /** list of full paths to non progress directories ***/
DEFINE NEW GLOBAL SHARED VARIABLE cDirCD       AS CHARACTER NO-UNDO.  /** Current directory **/
DEFINE NEW GLOBAL SHARED VARIABLE cDLCBIN      AS CHARACTER NO-UNDO.  /** DLC install directory **/
DEFINE NEW GLOBAL SHARED VARIABLE lLocal       AS Log       NO-UNDO.  /** indicator if local RCS user ***/
DEFINE NEW GLOBAL SHARED VARIABLE cRCS         AS CHARACTER NO-UNDO.  /** Path RCS-Root **/

FUNCTION fTrim     RETURNS CHAR(INPUT cIn AS CHARACTER) IN hPlus.
FUNCTION fIniLoad  RETURNS CHAR() IN hPlus.
FUNCTION fSetFiles RETURNS CHAR() IN hPlus.
FUNCTION fIniSave  RETURNS CHAR() IN hPlus.


&GLOB reportwindow IF fTestExecute() THEN RETURN.
/***************************************/
/*** Rules 1. Dir uses only forward /   ***/
/*** Rules 2. Dir always ends with  /    ***/

function fTaskFiles  returns char (input cTask as char) in hPlus.
function fFilesInDir returns char(input cDir as char) in hPlus.
FUNCTION fExe        returns char (input cCmd as char) IN hPlus.
FUNCTION fHeader     RETURNS CHAR () IN hPlus.
FUNCTION fFooter     RETURNS CHAR () IN hPlus.


/**** Webspeed integration ******/
/* Developing add below line to allow changes
*/
if valid-handle(hHTML) then DELETE PROCEDURE hHTML.
if not valid-handle(hHTML)
then do:
  run plus/oHTML.p persistent set hHTML.
end.
run callback in hHTML (this-procedure,'outputHTML').

/* Developing... add below line to allow changes effective immediately */
if valid-handle(hPLUS) then DELETE PROCEDURE hPlus.
if not valid-handle(hPLUS)
then do:
  run plus/oPlus.p persistent set hPlus.
end.
run callback in hPlus (this-procedure,'outputHTML').

if get-value('output-header') = '' then fIniLoad().


plusLog("Ready to run program.").

IF THIS-PROCEDURE:GET-SIGNATURE("output-header")  BEGINS "PROCEDURE" THEN RUN output-header.
IF THIS-PROCEDURE:GET-SIGNATURE("output-headers") BEGINS "PROCEDURE" THEN RUN output-headers.

if get-value('output-header') = '' then do:
  set-user-field('output-header','yes').
  OUTPUT-HTTP-HEADER("Pragma","No-Cache").
  OUTPUT-HTTP-HEADER("Cache-Control","No-Cache").
  /* Removed by SES */
  /*
  OUTPUT-HTTP-HEADER("Expires","0").
  */
  output-content-type("text/html").
end.


procedure destroy:
end.


       