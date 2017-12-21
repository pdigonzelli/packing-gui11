/*-----------------------------------------------------------------------
File: oConfig.p
Purpose: Configuration tools.
Description:
Author(s) :PerSDigre/PSC
Created: April 1998
Notes:    
Modification History:    
$Header: /cvsroot/freeframework/ffw1.2/ffw/plus/oConfig.p,v 1.1 2002/08/21 16:14:24 freeframework Exp $
$Log: oConfig.p,v $
Revision 1.1  2002/08/21 16:14:24  freeframework
Initial import

Revision 1.1.1.1  2001/03/23 14:50:37  slichtenberg
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
plusLog("Init CONFIG-object").

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


def new global shared var hPlus    as handle no-undo.
FUNCTION fIniSave RETURNS CHAR() IN hPlus.
FUNCTION fExe        returns char (input cCmd as char) IN hPlus.

def new global shared var hHTML as handle no-undo.
FUNCTION fRow        RETURNS CHAR(INPUT cData   AS CHAR) IN hHTML.
FUNCTION fHRow       RETURNS CHAR(INPUT cLabels AS CHAR) IN hHTML.
FUNCTION fBeginTable RETURNS CHAR(INPUT cLabels AS CHAR) IN hHTML.
FUNCTION fTable      RETURNS CHAR(INPUT cLabels AS CHAR
                                 ,INPUT cData AS CHAR) IN hHTML.
FUNCTION fLink       RETURNS CHAR(INPUT cMode AS CHAR
                                 ,INPUT cValue AS CHAR
                                 ,INPUT cText AS CHAR)    IN hHTML.


/********* Object for Updating status screen **********/
DEFINE NEW GLOBAL SHARED VARIABLE cDB      AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cDir     AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cLib     AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cBrk     AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cTsk     AS CHARACTER NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE cRCS     AS CHARACTER NO-UNDO. /*** Path RCS-Root **/
DEFINE NEW GLOBAL SHARED VARIABLE cDirCD   AS CHARACTER NO-UNDO. /** Current directory **/
DEFINE NEW GLOBAL SHARED VARIABLE cProPath AS CHARACTER NO-UNDO. /** list of full paths to non progress directories ***/
DEFINE NEW GLOBAL SHARED VARIABLE cName    AS CHARACTER NO-UNDO.  /** HTML param         **/



FUNCTION fRCSConfig RETURNS CHAR():
    ASSIGN
      cRCS = cDirCD.
    os-create-dir value('RCS') no-error.
    output stream s1 to 'RCS/rcs.ini'.
    put stream s1 unformatted cDirCD "," web-context:config-name skip.
    output stream s1 close.
    output stream s1 to 'RCS/rcs.log'.
    put stream s1 unformatted "Started RCS repository with " web-context:config-name "." skip.
    put stream s1 unformatted "Date:" string(today,'99/99/9999') ' Time:' string(time,'hh:mm:ss') skip.
    output stream s1 close.
end function.


FUNCTION fSaveAppCfg RETURNS CHAR():
  DEFINE VARIABLE i1   AS INTEGER NO-UNDO.
  DEFINE VARIABLE c1   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c2   AS CHARACTER NO-UNDO.
  ASSIGN
    cDir = ''
    c1   = get-field(?).
  DO i1 = 1 TO NUM-ENTRIES(c1):
    ASSIGN c2 = ENTRY(i1,c1).
    IF c2 BEGINS "DIR"  THEN cDir  = cDir  + "," + SUBSTRING(c2,4).
  END.
  fIniSave().
end function.

FUNCTION fSaveSysCfg RETURNS CHAR():
  DEFINE VARIABLE i1   AS INTEGER NO-UNDO.
  DEFINE VARIABLE c1   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c2   AS CHARACTER NO-UNDO.
    assign
      cBrk = ''
      cDB  = ''
      c1   = get-field(?).
    do i1 = 1 to num-entries(c1):
      assign c2 = entry(i1,c1).
      if c2 begins "DB"  then cDB  = cDB  + "," + substring(c2,3).
      if c2 begins "BRK" then cBrk = cBrk + "," + substring(c2,4).
    end.
    fIniSave().
end function.


FUNCTION fSetConfig RETURNS CHAR(input cDo as char,input cName as char):
  CASE cDo:
     WHEN "AddTsk" THEN do:
       cTsk = cName + "," + cTsk.
       if cRCS > "" THEN DO:
         output stream s1 to value(cRCS + "RCS/rcs.log") APPEND.
         put stream s1 unformatted "Opened " web-context:config-name "," cName skip.
         put stream s1 unformatted "Date: " string(today,"99/99/9999") ", Time:" string(time,"HH:MM:SS") skip.
         output stream s1 close.
       end.
     END.
     WHEN "RemTsk" THEN do:
       IF LOOKUP(cName,cTsk)  > 0 THEN ENTRY(LOOKUP(cName,cTsk),cTsk) = "".
       if cRCS > "" THEN DO:
         output stream s1 to value(cRCS + "RCS/rcs.log") APPEND.
         put stream s1 unformatted "Closed " web-context:config-name "," cName skip.
         put stream s1 unformatted "Date: " string(today,"99/99/9999") ", Time:" string(time,"HH:MM:SS") skip.
         output stream s1 close.
       end.
     end.
     WHEN "AddLib" THEN clib = cName.
     WHEN "RemLib" THEN cLib = "".
     WHEN "AddDir" THEN cDir = cDir + "," + cName + if (substring(cName,length(cName)) = "/") then "" else "/".
     WHEN "RemDir" THEN IF LOOKUP(cName,cDir)  > 0 THEN ENTRY(LOOKUP(cName,cDir),cDir) = "".
     WHEN "AddWS"  THEN cBrk = cBrk + ",WS:" + cName.
     WHEN "AddNS"  THEN cBrk = cBrk + ",NS:" + cName.
     WHEN "AddAS"  THEN cBrk = cBrk + ",AS:" + cName.
     WHEN "AddOD"  THEN cBrk = cBrk + ",OD:" + cName.
     WHEN "AddOR"  THEN cBrk = cBrk + ",OR:" + cName.
     WHEN "AddDB"  THEN cDB  = cDB  + ","    + cName.
  end case.
  fIniSave().
end function.


FUNCTION fRecurse RETURNS CHARACTER(INPUT cPath AS CHARACTER, INPUT cRelPath as char, INPUT iRec AS INTEGER):
  DEFINE VARIABLE i1   AS INTEGER NO-UNDO.
  DEFINE VARIABLE c1   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c2   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c3   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c4   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER NO-UNDO INITIAL "".

  ASSIGN cRet = (if cPath = cDirCD and cRelPath > "" then "" else cPath) + cRelPath.
  IF iRec < 1 THEN RETURN cRet.
  INPUT STREAM s1 FROM OS-DIR(cPath + cRelPath).
  REPEAT:
    ASSIGN
      c1 = ""
      c2 = ""
      c3 = "".
    IMPORT STREAM s1 c1 c2 c3.
    IF c3 = "D" AND NOT c1 BEGINS "." THEN c4 = c4 + "," + cRelPath + c1 + '/'.
  END.
  DO i1 = 2 TO NUM-ENTRIES(c4):
    ASSIGN
      c1 = ENTRY(i1,c4).
      cRet = cRet + "," + fRecurse(cPath,c1,iRec - 1).
  END.
  RETURN cRet.
END FUNCTION.

procedure showAppCfg:
  DEFINE VARIABLE cListed AS CHARACTER NO-UNDO.
  def var cRCStemp as char no-undo.
  def var i1 as int no-undo.
  DEFINE VARIABLE c1   AS CHARACTER NO-UNDO.

  DO i1 = 1 TO NUM-ENTRIES(cProPath):
    ASSIGN
      c1      = ENTRY(i1,cProPath)
      cListed = cListed + "," + fRecurse(c1,'',int(cName)).
  END.

  {&out} "propath" cProPath.
  {&out} '<br>Recurse
    <INPUT TYPE=button NAME=recurse1  VALUE="1 level"  onClick="document.form.Do.value=~'appCfg~';document.form.Name.value=~'1~';document.form.submit();">
  / <INPUT TYPE=button NAME=recurse2  VALUE="2 levels" onClick="document.form.Do.value=~'appCfg~';document.form.Name.value=~'2~';document.form.submit();">
  / <INPUT TYPE=button NAME=recurse3  VALUE="3 levels" onClick="document.form.Do.value=~'appCfg~';document.form.Name.value=~'3~';document.form.submit();">' SKIP.

  if cRCS ="" then
  {&out} ' &nbsp; &nbsp; <small><INPUT TYPE=button NAME=addRCS  VALUE="Use RCS with this broker" onClick="document.form.Do.value=~'RCSconfig~';document.form.submit();">.</a></small>' skip.

  cRCStemp = if cRCS > "" then cRCS else "?,&.%".

  {&out} fBeginTable("Recursing PROPATH|" + if cRCS > "" THEN "RCS-Root:" + cRCS else "&nbsp;").
  DO i1 = 2 TO NUM-ENTRIES(cListed):
    ASSIGN c1 = ENTRY(i1,cListed).
    IF LOOKUP(c1,cListed) < i1 THEN NEXT.
    {&out} fRow("<INPUT TYPE=checkbox NAME='dir" + c1 + (IF CAN-DO(cDir,c1) THEN "' checked" ELSE "'") + ">" + c1 + "|&nbsp;"
     + (if c1 = cRCStemp then " RCS_root " else "")
     + (if c1 = cDirCD then " Current_Dir " else "")).
  END.
  {&out} fHRow("Unlisted codepaths &nbsp; &nbsp; &nbsp;"
      + fLink("AddDir","prompt('CodePath:')","Add unlisted|&nbsp;")).
  DO i1 = 1 TO NUM-ENTRIES(cDir):
    ASSIGN c1 = ENTRY(i1,cDir).
    IF CAN-DO(cListed,c1) THEN NEXT.
    {&out} fRow("<INPUT TYPE=checkbox NAME=dir" + c1 + (IF CAN-DO(cDir,c1) THEN " checked" ELSE " ") + ">" + c1 + "|&nbsp;").
  END.

  {&out} '</TABLE><CENTER><INPUT TYPE=button NAME=saveexit  VALUE="  OK  " onClick="document.form.Do.value=~'saveAppCfg~';document.form.submit();"></CENTER>' skip.
end procedure.





procedure showSysCfg:
  DEFINE VARIABLE cRegBrk AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cRegDB  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDBCurr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBrk9   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDB9    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cGR     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPf     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i1      AS INTEGER NO-UNDO.
  DEFINE VARIABLE c1      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c2      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c3      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPath   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cListed AS CHARACTER NO-UNDO.


/********** Check PROCONTROL ******************************************/
  IF OPSYS = "win32"  AND cGR > "" THEN DO:
    ASSIGN c2 = ""
           c1  = "".
    INPUT STREAM s1 THROUGH VALUE(fExe("gr.exe HLM Software~\PSC~\ProService")).

    REPEAT:
      IMPORT STREAM s1 UNFORMATTED c1.
      ASSIGN c2 = c2 + "," + SUBSTRING(c1,R-INDEX(c1,"~\") + 1).
    END.
    INPUT STREAM s1 CLOSE.
    DO i1 = 2 TO NUM-ENTRIES(c2):
      INPUT STREAM s1 THROUGH VALUE(fExe("gr.exe HLM Software~\PSC~\ProService~\" + ENTRY(i1,c2) + "~\Databases")).

      REPEAT:
        ASSIGN c1 = "".
        IMPORT STREAM s1 UNFORMATTED c1.
        IF c1 MATCHES "*Database*"
        THEN c3 = c3 + "," + c1.
      END.
      INPUT STREAM s1 CLOSE.
    END.
    DO i1 = 2 TO NUM-ENTRIES(c3):
      INPUT STREAM s1 THROUGH VALUE(fExe("gr.exe HLM " + ENTRY(i1,c3))).
        REPEAT:
          ASSIGN c1 = "".
          IMPORT STREAM s1 UNFORMATTED c1.
          IF c1 MATCHES "*Workdi*" THEN DO:
          IMPORT STREAM s1 UNFORMATTED c1.
          ASSIGN cPath = ENTRY(2,c1,'"').
        END.
        IF c1 MATCHES "*DBNam*" THEN DO:
          ASSIGN c1 = "".
          IMPORT STREAM s1 UNFORMATTED c1.
          ASSIGN cPath = cPath + "~\" + ENTRY(2,c1,'"').
        END.
        IF c1 MATCHES "*StartupPara*" THEN DO:
          ASSIGN c1 = "".
          IMPORT STREAM s1 UNFORMATTED c1.
          ASSIGN cPf = ENTRY(2,c1,'"').
        END.
      END.
      INPUT  STREAM s1 CLOSE.
      OUTPUT STREAM s1 TO VALUE(cPath + ".pf").
      PUT    STREAM s1 UNFORMATTED cPF SKIP.
      INPUT  STREAM s1 CLOSE.
      ASSIGN cPath = cPath + ".db".
      IF NOT CAN-DO(cRegDB,cPath)
      THEN   cRegDB = cRegDB + "," + cPath.
    END.
  END.


  DO:

/********** Check ubroker.properties ******************************************/
    IF SEARCH("properties/ubroker.properties") > "" THEN DO:
      INPUT STREAM s1 FROM VALUE(SEARCH("properties/ubroker.properties")).
      REPEAT:
        ASSIGN c1 = "".
        IMPORT STREAM s1 UNFORMATTED c1.
        IF TRIM(c1) BEGINS "#" THEN NEXT.
        IF c1 BEGINS "[UBroker.WS." OR
           c1 BEGINS "[UBroker.AS." OR
           c1 BEGINS "[UBroker.OR." OR
           c1 BEGINS "[UBroker.OD."
        THEN ASSIGN cBrk9 = cBrk9 + "," + REPLACE(REPLACE(SUBSTRING(c1,10),".",":"),"]","").
        IF c1 BEGINS "[nameserver."
       THEN ASSIGN cBrk9 = cBrk9 + ",NS:" + REPLACE(SUBSTRING(c1,13),"]","").
      END.
      INPUT STREAM s1 CLOSE.
    END.
/********** Check conmgr.properties ******************************************/
    IF SEARCH("properties/conmgr.properties") > "" THEN DO:
      INPUT STREAM s1 FROM VALUE(SEARCH("properties/conmgr.properties")).
      REPEAT:
        ASSIGN c1 = "".
        IMPORT STREAM s1 UNFORMATTED c1.
        IF TRIM(c1) BEGINS "#" THEN NEXT.
        IF TRIM(c1) BEGINS "databasename="
        THEN ASSIGN
          cDBCurr = SUBSTRING(TRIM(c1),14)
          cDB9 = cDB9 + "," + cDBCurr
           .
      END.
      INPUT STREAM s1 CLOSE.
    END.
  END.


  ASSIGN
    cRegDB  = replace(cRegDB,'~\','/')
    cDB9    = replace(cDB9  ,'~\','/')
    cListed = cRegBrk + cRegDB + cBrk9 + cDB9.

  {&out} fBeginTable("Registry Search - Config Utility").
  DO i1 = 2 TO NUM-ENTRIES(cRegBrk):
    {&out} fRow("<INPUT TYPE=checkbox NAME=brk" + ENTRY(i1,cRegBrk)
      + (IF CAN-DO(cBrk,ENTRY(i1,cRegBrk)) THEN " checked" ELSE " ") + ">"
      + ENTRY(i1,cRegBrk)).
  END.
  {&out} fHRow("Registry Search - ProControl").
  DO i1 = 2 TO NUM-ENTRIES(cRegDB):
    {&out} fRow("<INPUT TYPE=checkbox NAME=db" + ENTRY(i1,cRegDB)
      + (IF CAN-DO(cDB,ENTRY(i1,cRegDB)) THEN " checked" ELSE " ") + ">"
      + ENTRY(i1,cRegDB)).
  END.
  {&out} fHRow("Progress Explorer - ubroker.properties").
  DO i1 = 2 TO NUM-ENTRIES(cBrk9):
    {&out} fRow("<INPUT TYPE=checkbox NAME=brk" + ENTRY(i1,cBrk9)
      + (IF CAN-DO(cBrk,ENTRY(i1,cBrk9)) THEN " checked" ELSE " ") + ">"
      + ENTRY(i1,cBrk9)).
  END.
  {&out} fHRow("Progress Explorer - conmgr.properties").
  DO i1 = 2 TO NUM-ENTRIES(cDB9):
    {&out} fRow("<INPUT TYPE=checkbox NAME=db" + ENTRY(i1,cDB9)
      + (IF CAN-DO(cDB,ENTRY(i1,cDB9)) THEN " checked" ELSE " ") + ">DB:"
      + ENTRY(i1,cDB9)).
  END.
  {&out} fHRow("Unlisted servers &nbsp; &nbsp; &nbsp; (Add "
      + fLink("AddWS","prompt('Webspeed Broker:')","WS")
      + (IF proversion BEGINS "9" THEN
          fLink("AddNS","prompt('Nameserver:')","NS")
        + fLink("AddAS","prompt('App Server:')","AS")
        + fLink("AddOD","prompt('ODBC Dataserver:')","OD")
        + fLink("AddOR","prompt('Oracle Dataserver:')","OR") ELSE "")
        + fLink("AddDB","prompt('Path of the database (including *.db):')","DB")
        + ")|").
  DO i1 = 1 TO NUM-ENTRIES(cBrk):
    if can-do(cListed,ENTRY(i1,cBrk)) then next.
    {&out} fRow("<INPUT TYPE=checkbox NAME=brk" + ENTRY(i1,cBrk) + " checked>" + ENTRY(i1,cBrk)).
  END.
  DO i1 = 1 TO NUM-ENTRIES(cDB):
    if can-do(cListed,ENTRY(i1,cDB)) then next.
    {&out} fRow("<INPUT TYPE=checkbox NAME=db" + ENTRY(i1,cDB) + " checked>DB:" + ENTRY(i1,cDB)).
  END.
  {&out} '</TABLE><CENTER><INPUT TYPE=button NAME=saveexit  VALUE="  OK  " onClick="document.form.Do.value=~'saveSysCfg~';document.form.submit();">
</CENTER>' skip.
end procedure.


