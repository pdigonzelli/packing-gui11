/*-----------------------------------------------------------------------
File: oProutil.p
Purpose: Commandline based tools for database and brokers.
Description:
Author(s) :Per S Digre/PSC
Created: April 1998
Notes:    
Modification History:    
$Header: /cvsroot/freeframework/ffw1.2/ffw/plus/oProutil.p,v 1.1 2002/08/21 16:14:25 freeframework Exp $
$Log: oProutil.p,v $
Revision 1.1  2002/08/21 16:14:25  freeframework
Initial import

Revision 1.1.1.1  2001/03/23 14:50:41  slichtenberg
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
plusLog("Init ProUtil-object").
 
/**** generic object definition section  ********/
DEFINE VARIABLE hParent   AS HANDLE NO-UNDO.
DEFINE VARIABLE cCallback AS CHAR   NO-UNDO.

PROCEDURE callback:
  DEFINE INPUT PARAMETER h1 AS handle NO-UNDO.
  DEFINE INPUT PARAMETER c1 AS char   NO-UNDO.
  assign hParent   = h1
         cCallBack = c1.
END PROCEDURE.



/***************************************/

DEF stream s1.

DEFINE NEW GLOBAL SHARED VARIABLE cLib     AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cName    AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cFiles   AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cDLCBIN     AS CHARACTER NO-UNDO. /** DLC install directory **/


def new global shared var hPlus    as handle no-undo.
FUNCTION fExe        returns char (input cCmd as char) IN hPlus.
function fNTKill  RETURNS CHAR(input iPID as int) in hPlus.


def new global shared var hHTML as handle no-undo.
FUNCTION fRow        RETURNS CHAR(INPUT cData   AS CHAR) IN hHTML.
FUNCTION fHRow       RETURNS CHAR(INPUT cLabels AS CHAR) IN hHTML.
FUNCTION fBeginTable RETURNS CHAR(INPUT cLabels AS CHAR) IN hHTML.
FUNCTION fTable      RETURNS CHAR(INPUT cLabels AS CHAR
                                 ,INPUT cData AS CHAR) IN hHTML.
FUNCTION fLink       RETURNS CHAR(INPUT cMode AS CHAR
                                 ,INPUT cValue AS CHAR
                                 ,INPUT cText AS CHAR)    IN hHTML.
FUNCTION fProcess    RETURNS CHARACTER(INPUT cCmd AS CHARACTER
                                   ,INPUT cAvoid AS CHAR) IN hHTML.



function fCmd returns char (input cCmd as char, input cPrm as char, input cFormat as char):
  {&out} "OS-CMD: " cCmd " " cPrm "." SKIP.
  if opsys = "win32" 
  then fProcess('"' + cDLCBIN + cCmd + '" ' + cPrm,cFormat).
  else fProcess(cCmd + ' ' + cPrm,cFormat).
end function.

FUNCTION fLibraryUpdate RETURNS CHAR():
  DEFINE VARIABLE i1      AS INTEGER NO-UNDO.
  ASSIGN cFiles = REPLACE(REPLACE(REPLACE(REPLACE(cFiles + ",",".w,",".r "),".html,",".r "),".htm,",".r "),".p,",".r ").
  {&out} "<h2>Building library</h2>".
  fCmd('prolib', cLib + " -create","* already exists.*").
  DO i1 = 1 TO NUM-ENTRIES(cFiles," "):
    fCmd('prolib', cLib + " -add " + ENTRY(i1,cFiles," "),"*replace *").
    fCmd('prolib', cLib + " -replace " + ENTRY(i1,cFiles," "),"").
  END.
end function.

FUNCTION fLibraryView RETURNS CHAR():
  DEFINE VARIABLE c1      AS CHARACTER NO-UNDO.
  /***** Displaying library statistics *******/
  INPUT    STREAM s1 THROUGH prolib VALUE(cLib) -list.
  REPEAT:
    IMPORT STREAM s1 UNFORMATTED c1.
    if c1 matches "* Offset *" and c1 matches "* Modified *" then leave.
    {&out} "<p>" HTML-ENCODE(c1) "</p>".
  END.
  {&out} "<table " {&cBrowseTableDef} ">" SKIP.
  ASSIGN c1 = REPLACE(REPLACE(c1,"    "," "),"    "," ").
  ASSIGN c1 = REPLACE(REPLACE(c1,"  "," "),"  "," ").
  {&out} "<tr " {&cColorHeader} "><th>" REPLACE(c1," ","</th><th>") "</th></tr>".

  IMPORT STREAM s1 UNFORMATTED c1.
  REPEAT:
    ASSIGN c1 = REPLACE(REPLACE(c1,"    "," "),"    "," ").
    ASSIGN c1 = REPLACE(REPLACE(c1,"  "," "),"  "," ").
    IF NOT c1 BEGINS "Total of" THEN
      {&out} "<tr " {&cColorRow1} "><td>" REPLACE(c1," ","</td><td>") "</td></tr>".
    IMPORT STREAM s1 UNFORMATTED c1.
  END.
  {&out} "</table><p>" HTML-ENCODE(c1) "</p>".
  INPUT    STREAM s1 CLOSE.
end function.


FUNCTION fBrkStart RETURNS CHAR(input cUtil as char,input cName as char,input cName2 as char):
  fCmd(cUtil,' -start -name ' + cName2,"").
end function.

FUNCTION fBrkStop RETURNS CHAR(input cUtil as char,input cName as char,input cName2 as char):
  fCmd(cUtil,' -stop -name ' + cName2,"").
end function.

FUNCTION fBrkAdd RETURNS CHAR(input cUtil as char,input cName as char,input cName2 as char):
  fCmd(cUtil,' -addagents 1 -name ' + cName2,"").
end function.
 
FUNCTION fBrkKill RETURNS CHAR(input cName2 as char,input cPID as char):
  IF OPSYS = "win32"
  THEN {&out} "<p>" HTML-ENCODE(fNTKill(INT(cPID))) "</p>".
  ELSE fCmd('kill',' -15 ' + cPID,"").
end function.
 

FUNCTION fBrkView RETURNS CHAR(input cUtil as char,input cName as char,input cName2 as char):
  DEFINE VARIABLE c1      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE eIn     AS CHARACTER NO-UNDO EXTENT 10.
  {&out} "Using " cUtil " to view " cName "." SKIP.
  INPUT STREAM s1 THROUGH VALUE(cUtil + " -query -name " + cName2).

  REPEAT:
    ASSIGN eIn = "".
   IMPORT STREAM s1 eIn.
    IF  (eIn[1] = "PID" OR eIn[2] = "PID" AND eIn[1] <> "BROKER" )
     OR eIn[1] = "Application"
        THEN LEAVE.
  END.

  if cUtil = "nsman"
  THEN {&out} fBeginTable("Application Service|" + eIn[3] + "|" + eIn[4] + "|" + eIn[5] + "|" +
       eIn[6] + "|" + eIn[7] + "|" + eIn[8]).
  else {&out} fBeginTable(
       eIn[1] + "|" + eIn[2] + "|" + eIn[3] + "|" + eIn[4] + "|" + eIn[5] + "|" +
       eIn[6] + "|" + eIn[7] + "|" + eIn[8] +
      "|" + eIn[9] + "|" + fLink("AddBrk",cName,"Add")).
  REPEAT:
    IF cUtil = "nsman" THEN DO:
      IMPORT STREAM s1 c1.
      IMPORT STREAM s1 c1.
    END.
    IMPORT STREAM s1 eIn.
    IF eIn[1] BEGINS "==" THEN NEXT.
    IF eIn[1] > ""
    THEN if cUtil = "nsman"
      then {&out} fRow(c1 + "|" +
        eIn[1] + "|" + eIn[2] + "|" + eIn[3] + "|" + eIn[4] + "|" + eIn[5] + "|" +
        eIn[6]).
        else {&out} fRow(
        eIn[1] + "|" + eIn[2] + "|" + eIn[3] + "|" + eIn[4] + "|" + eIn[5] + "|" +
        eIn[6] + "|" + eIn[7] + "|" + eIn[8] + "|" + eIn[9] + "|" +
    fLink("KillBrk" + eIn[1],cName,"Kill")).
    ASSIGN eIn = "".
  END.
  {&out} "</table>" SKIP.
  INPUT    STREAM s1 CLOSE.
end function.


