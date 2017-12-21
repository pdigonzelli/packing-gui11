/*-----------------------------------------------------------------------
File: oHTML.p
Purpose: Generic HTML formatting object.
Description:
Author(s) :PerSDigre/PSC
Created: April 1998
Notes:    
Modification History:    
$Header: /cvsroot/freeframework/ffw1.2/ffw/plus/oHTML.p,v 1.1 2002/08/21 16:14:25 freeframework Exp $
$Log: oHTML.p,v $
Revision 1.1  2002/08/21 16:14:25  freeframework
Initial import

Revision 1.1.1.1  2001/03/23 14:50:39  slichtenberg
initial load 1.03


This file contains sample code which may assist you in creating applications.
You may use the code as you see fit. If you modify the code or include it in another software program,
you will refrain from identifying Progress Software as the supplier of the code, or using any
Progress Software trademarks in connection with your use of the code.
THE CODE IS NOT SUPPORTED BY PROGRESS SOFTWARE AND IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
INCLUDING, WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR NONINFRINGEMENT.
-----------------------------------------------------------------------*/
{src/web/method/cgidefs.i}
{plus/session.i}
plusLog("Init HTML-object").

&GLOBAL-DEFINE cBrowseTableDef " border=0 cellpadding=1 cellspacing=2 width=100% "


/***** Common for all objects  *******/
DEFINE VAR refer-hdl AS HANDLE NO-UNDO.
DEFINE STREAM sIni.


DEFINE VARIABLE hParent   AS HANDLE NO-UNDO.
DEFINE VARIABLE cCallback AS CHAR   NO-UNDO.

PROCEDURE callback:
  DEFINE INPUT PARAMETER h1 AS handle NO-UNDO.
  DEFINE INPUT PARAMETER c1 AS char   NO-UNDO.
  assign hParent   = h1
         cCallBack = c1.
END PROCEDURE.

PROCEDURE pHandle:
  def input param pHdl as handle no-undo.
  assign refer-hdl = pHdl.
END PROCEDURE.



/****** HTML routines *********/

FUNCTION fLink RETURNS CHAR (INPUT cAction AS CHAR,INPUT cValue AS CHAR,INPUT cText AS CHAR):
  IF NOT cValue MATCHES "*(*" THEN cValue = "'" + cValue + "'".
  RETURN "<a HREF=""JavaScript:document.form.Do.value='" + cAction
       + "';document.form.Name.value=" + cValue
       + ";document.form.submit()"">" + cText + "</a>" + chr(10).
END FUNCTION.

DEF VAR v-fl AS LOGICAL NO-UNDO.

FUNCTION fRow RETURNS CHAR(INPUT cData AS CHAR):
  ASSIGN v-fl = not v-fl.
  RETURN '<TR class="datarow' + trim(STRING(v-fl,"odd/even")) + '"><TD class="datacell">' + REPLACE(cData,"|","</td><td>") + "</TD></TR>" + CHR(10).
END FUNCTION.

FUNCTION fHRow RETURNS CHAR(INPUT cLabels AS CHAR):

  IF cLabels > ""
  THEN RETURN '<TR class="headerrow"><td class="columnheader">' + REPLACE(cLabels,"|",'</td><td class="columnheader">') + "</TH></TR>" + CHR(10).
  ELSE RETURN "".
END FUNCTION.

FUNCTION fBeginTable RETURNS CHAR(INPUT cLabels AS CHAR):
  RETURN "<table " + {&cBrowseTableDef} + ">" + CHR(10) + fHRow(cLabels).
END FUNCTION.

/*** This does not create an HTML table unless there is data ***/
FUNCTION fTable RETURNS CHAR(INPUT cLabels AS CHAR,INPUT cData AS CHAR):
  DEF VAR cReturn AS CHAR NO-UNDO.
  IF cData > "" THEN DO:
    ASSIGN cReturn = fBeginTable(cLabels) + cData + "</table>" + CHR(10).
  END.
  RETURN cReturn.
END FUNCTION.


FUNCTION fProcess RETURNS CHARACTER(INPUT cCmd AS CHARACTER,INPUT cAvoid AS CHAR):
  DEF VAR cIn AS CHAR no-undo.
  {&out} "<pre>" chr(0) skip.
  IF cAvoid = "" THEN cAvoid = "&&&gh~&&&".
  if opsys <> "win32" then cCmd = replace(cCmd,'"','').
  plusLog("CMD:" + cCmd).
  INPUT STREAM sIni THROUGH VALUE(cCmd).
  REPEAT:
    ASSIGN cIn = "".
    IMPORT STREAM sIni UNFORMATTED cIn NO-ERROR.
    IF Error-status:error THEN DO:
      {&out} "Error in command.</pre>" chr(0) skip.
      RETURN "".
    END.
    IF cIn MATCHES cAvoid THEN NEXT.
      {&out} HTML-ENCODE(cIn) chr(0) skip.
  END.
  {&out} "</pre>" skip.
  INPUT STREAM sIni CLOSE.
END FUNCTION.


