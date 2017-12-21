/*-----------------------------------------------------------------------
File: oPlus.p
Purpose: Generic code library for use in all Plus programs.
Description:
Author(s) :PerSDigre/PSC
Created: April 1998
Notes:    
Modification History:    
$Header: /cvsroot/freeframework/ffw1.2/ffw/plus/oPlus.p,v 1.1 2002/08/21 16:14:25 freeframework Exp $
$Log: oPlus.p,v $
Revision 1.1  2002/08/21 16:14:25  freeframework
Initial import

Revision 1.1.1.1  2001/03/23 14:50:40  slichtenberg
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
plusLog("Init PLUS-object").
{ ffw/lib/ffw_global.i }                    /*PATH SETTINGS AND OTHER GENERAL THINGS*/
{ ffw/lib/ffpplib.i }


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

DEFINE NEW GLOBAL SHARED VARIABLE cTaskFiles   AS CHARACTER NO-UNDO. /** list of full paths to non progress directories ***/
DEFINE NEW GLOBAL SHARED VARIABLE cProPath     AS CHARACTER NO-UNDO. /** list of full paths to non progress directories ***/
DEFINE NEW GLOBAL SHARED VARIABLE cDirCD       AS CHARACTER NO-UNDO. /** Current directory **/
DEFINE NEW GLOBAL SHARED VARIABLE cPlusDir     AS CHARACTER NO-UNDO. /** PLUS directory    **/
DEFINE NEW GLOBAL SHARED VARIABLE cPlusHTML    AS CHARACTER NO-UNDO. /** PLUS HTML directory    **/
DEFINE NEW GLOBAL SHARED VARIABLE cDLCBIN      AS CHARACTER NO-UNDO. /** DLC install directory **/
DEFINE NEW GLOBAL SHARED VARIABLE lLocal       AS Log       NO-UNDO. /** indicator if local RCS user ***/
DEFINE NEW GLOBAL SHARED VARIABLE cRCS         AS CHARACTER NO-UNDO. /** Path RCS-Root **/

DEFINE STREAM s1.




/****************************************************************/
/***    File & directory utilitites      ************************/
/****************************************************************/


def var cDirRelPP as char no-undo.

FUNCTION fCheckDir RETURNS CHAR(input cDir as char):
  def var i1 as int no-undo.
  def var c1 as char no-undo.
  def var c2 as char no-undo.
  def var cDirPP    as char no-undo.

  assign
    cDir      = replace(cDir,"~\","/")
    cDir      = cDir + if (substring(cDir,length(cDir)) = "/") then "" else "/"
    cDirPP    = ?
    cDirRelPP = ?.
  do i1 = 1 to num-entries(cProPath):
    c1 = entry(i1,cPropath).
    if not cDir begins c1 then next.
    if c1 < cDirPP then next.
    assign
      cDirPP = c1
      cDirRelPP = replace(cDir,c1,"")
      .
  end.
end function.

FUNCTION fProPath RETURNS CHAR():
  def var i1 as int  no-undo.
  def var c1 as char no-undo.
  def var c2 as char no-undo.
  def var c3 as char no-undo.
  def var c4 as char no-undo.
  assign
    c1 = replace(propath,".,",get-config("workdir") + ",")
    c2 = substring(search('_dict.r'),1,max(index(search('_dict.r'),'tty') - 2,-1))
    c2 = replace(c2,"~\","/")
    c2 = c2 + if (substring(c2,length(c2)) = "/") then "" else "/"
         .
  do i1 = 1 to num-entries(c1):
    assign
      c4 = replace(entry(i1,c1),"~\","/")
      c4 = c4 + if (substring(c4,length(c4)) = "/") then "" else "/".
    if entry(1,c4,"/") = "." then c4 = cDirCD + substring(c4,2).
    if not c4 begins c2
      then if not can-do(c3,c4)
        then c3 = c3 + "," + c4.
  end.
  return substring(c3,2).
end function.


FUNCTION fTrim RETURNS CHARACTER(INPUT c1 AS CHARACTER):
  ASSIGN c1 = REPLACE(c1,",,",",")
         c1 = REPLACE(c1,"~\","/")
         .
         /*
  IF c1 BEGINS  ","  THEN ASSIGN c1 = SUBSTRING(c1,2).
  IF c1 MATCHES "*," THEN ASSIGN c1 = SUBSTRING(c1,1,LENGTH(c1) - 1).
  */
  /* ||| ses*/
  ASSIGN c1 = trim(c1,",").
  RETURN c1.
END FUNCTION.

function fTaskFiles returns char (input cTask as char):
  DEFINE VARIABLE cTaskStatus AS CHARACTER NO-UNDO. /** list of full paths to non progress directories ***/
  def var c1 as char no-undo.
  assign
    cTaskFiles = ''
    cTaskStatus = ''.
  input stream s1 from value(cTask + ".task").
  repeat:
    assign c1 = ''.
    import stream s1 unformatted c1.
    if can-do(cTaskFiles,entry(1,c1,";"))
    then assign
      entry(lookup(entry(1,c1,";"),cTaskFiles),cTaskStatus) = entry(lookup(entry(1,c1,";"),cTaskFiles),cTaskStatus) + "|" + entry(2,c1 + ";",";").
    else assign
      cTaskFiles  = cTaskFiles  + "," + entry(1,c1,";").
      cTaskStatus = cTaskStatus + "," + entry(2,c1 + ";",";").
    if entry(2,c1 + ";",";") begins "del"
    then cTaskFiles = replace(cTaskFiles,"," + entry(1,c1,";"),",").
  end.
  if cTaskFiles > '' then assign
    cTaskFiles  = substring(cTaskFiles,2)
    cTaskStatus = substring(cTaskStatus,2).
  input stream s1 close.
  return cTaskFiles.
end function.

function fFilesInDir returns char(input cDir as char):
  def var cFiles as char no-undo.
  def var c1    as char no-undo.
  fCheckDir(cDir).
  if cDirRelPP = ? then cDirRelPP = cDir.
  INPUT STREAM s1 FROM OS-DIR(cDir).
  REPEAT:
    assign c1 = ''.
    IMPORT STREAM s1 c1.
    IF not can-do("html,htm,w,i,p",entry(num-entries(c1,"."),c1,".")) then next.
    if can-do(cFiles,c1) then next.
    cFiles = cFiles + (if cFiles  > "" then "," else "") + cDirRelPP + c1.
  END.
  INPUT STREAM s1 CLOSE.
  return cFiles.
end function.




/****************************************************************/
/***    Configuration files read & save  ************************/
/****************************************************************/

DEFINE VARIABLE cPlusAppStamp AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPlusApp      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPlusIniStamp AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPlusIni      AS CHARACTER NO-UNDO.

FUNCTION fIniLoad RETURNS CHAR ():
  DEFINE VARIABLE i1       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE c1      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c2     AS CHARACTER NO-UNDO.
  /*** This section is needed here in case RCS is just enabled ***/
  assign cRCS = search('RCS/rcs.ini').
  if cRCS > ''
  then assign
       cRCS = replace(replace(cRCS,'~\','/'),'./',cDirCD)
       cRCS = replace(cRCS,'RCS/rcs.ini','').
  else cRCS = ''.
  assign lLocal = (cDirCD <> cRCS).


  /*** Section for reading common variables used in all programs ***/
  ASSIGN
    cAction  = get-value("Do")
    cName    = get-value('Name').
    cFiles = "".


  /***** Reading the server configuration ******/
  ASSIGN c1 = "".
  IF SEARCH(cPlusIni) > "" THEN DO:
    ASSIGN file-info:file-name = cPlusIni.
    if cPlusIniStamp <> string(file-info:file-mod-date) + '-' + string(file-info:file-mod-time,'hh:mm:ss') then do:
      INPUT STREAM s1 FROM value(cPlusIni).
      REPEAT:
        ASSIGN c2 = "".
        IMPORT STREAM s1 UNFORMATTED c2.
        ASSIGN c1  = c1 + TRIM(c2) + "|"
               c2 = "".
      END.
      INPUT STREAM s1 CLOSE.
      assign
        cPlusIniStamp = string(file-info:file-mod-date) + '-' + string(file-info:file-mod-time,'hh:mm:ss')
        c1   = c1 + "|||||||"
        cDB  = ENTRY(1,c1,"|")
        cBrk = ENTRY(2,c1,"|").
    END.
  END.


  /***** Reading the application configuration ******/
  ASSIGN c1 = "".
  IF SEARCH(cPlusApp) > "" THEN DO:
    ASSIGN file-info:file-name = cPlusApp.
    if cPlusAppStamp <> string(file-info:file-mod-date) + '-' + string(file-info:file-mod-time,'hh:mm:ss') then do:
      INPUT STREAM s1 FROM value(cPlusApp).
      REPEAT:
        ASSIGN c2 = "".
        IMPORT STREAM s1 UNFORMATTED c2.
        ASSIGN c1  = c1 + TRIM(c2) + "|"
               c2 = "".
      END.
      INPUT STREAM s1 CLOSE.
      assign
        cPlusAppStamp = string(file-info:file-mod-date) + '-' + string(file-info:file-mod-time,'hh:mm:ss')
        c1   = c1 + "|||||||"
        cDir = ENTRY(1,c1,"|")
        cLib = ENTRY(2,c1,"|")
        cTsk = ENTRY(3,c1,"|").
    END.
  END.
  DO i1 = 1 TO NUM-ENTRIES(cBrk):    /*** Converting from old versions ***/
    IF NOT ENTRY(i1,cBrk) MATCHES "*:*"
    THEN ASSIGN ENTRY(i1,cBrk) = "WS:" + ENTRY(i1,cBrk).
  END.
END function.



FUNCTION fSetFiles RETURNS CHAR ():
  DEFINE VARIABLE i1    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE c1    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c2    AS CHARACTER NO-UNDO.
  ASSIGN
    cFiles = "".
  DO i1 = 1 TO NUM-ENTRIES(cDir):    /**** Find files in all selected directories *****/
    IF GET-VALUE("dir" + ENTRY(i1,cDir)) > ""
    THEN ASSIGN c1 = c1 + "," + ENTRY(i1,cDir).
  END.
  DO i1 = 2 TO NUM-ENTRIES(c1):
    INPUT STREAM s1 FROM OS-DIR(ENTRY(i1,c1)).
    REPEAT:
      ASSIGN c2 = "X".
      IMPORT STREAM s1 c2.
      IF c2 = "X" THEN NEXT.
      IF can-do('html,htm,w,p',entry(num-entries(c2,'.'),c2,'.'))
      THEN cFiles = cFiles + "," + ENTRY(i1,c1) + c2.
    END.
    INPUT STREAM s1 CLOSE.
  END.
  IF cFiles BEGINS "," THEN cFiles = SUBSTRING(cFiles,2).
END function.



/***** Assign directories either by clicked or by checked ******/

FUNCTION fIniSave RETURNS CHAR ():
    ASSIGN cDB  = fTrim(cDB)
           cDir = fTrim(cDir)
           cLib = fTrim(cLib)
           cBrk = fTrim(cBrk)
           cTsk = fTrim(cTsk).
    IF cDb  BEGINS "," THEN ASSIGN cDB = SUBSTRING(cDB,2).
    IF cBrk BEGINS "," THEN ASSIGN cbrk = SUBSTRING(cBrk,2).
    IF cTsk BEGINS "," THEN ASSIGN cTsk = SUBSTRING(cTsk,2).
    OUTPUT STREAM s1 TO VALUE(cPlusIni).
    PUT STREAM s1 UNFORMATTED
       cDB  " " SKIP
       cBrk " " SKIP
       .
    OUTPUT STREAM s1 CLOSE.
    OUTPUT STREAM s1 TO VALUE(cPlusApp).
    PUT STREAM s1 UNFORMATTED
       cDir " " SKIP
       cLib " " SKIP
       cTsk " " SKIP.
    OUTPUT STREAM s1 CLOSE.
END.





/****************************************************************/
/***    Command line executions          ************************/
/****************************************************************/


/*** Initialization values *******/
DEFINE VARIABLE cExeCmd     AS CHARACTER NO-UNDO. /** The command file info                   **/

assign
  cDirCD    = replace(get-config("workdir"),"~\","/")
  cDirCD    = cDirCD + if (substring(cDirCD,length(cDirCD)) = "/") then "" else "/"
  cProPath  = fProPath()
  cPlusDir  = replace(search('plus/oPlus.p'),'~\','/')
  cPlusDir  = replace(search(replace(cPlusDir,'./',cDirCD)),'oPlus.p','')
  cPlusHTML = "file:///" + replace(cPlusdir,'~\','/') + "doc/"
  cPlusApp  = cPlusDir + WEB-CONTEXT:CONFIG-NAME + '.ini'
  cPlusIni  = cPlusDir + 'machine.ini'
  cStatic   = get-config('wsRoot')
  cStatic   = if cStatic > '' then cStatic else '/webspeed'.

plusLog("WorkDir:" + cDirCD).
plusLog("PlusDir:" + cPlusDir).
plusLog("PlusApp:" + cPlusApp).
plusLog("PlusIni:" + cPlusIni).


if opsys='win32' then do:     /**** PC-DOS *****/
  assign
    cExeCmd = substring(cplusDir,1,2) + chr(10) + chr(13)
    cExeCmd = cExeCmd + 'cd ' + substring(cplusDir,3) + chr(10)
    cExeCmd = cExeCmd + 'set LOGNAME=' + WEB-CONTEXT:config-name + chr(10)
    cExeCmd = cExeCmd + 'set PATH=%PATH%;' + cPlusDir + chr(10)
    cDLCBIN   = replace(search("progress.ini"),"progress.ini","").
end.
else do:                      /**** Unix *****/
  assign
    cExeCmd = 'cd ' + cplusDir + chr(10)
    cExeCmd = cExeCmd + 'setenv LOGNAME=' + WEB-CONTEXT:config-name + chr(10)
    cExeCmd = cExeCmd + 'setenv path=~{$path~},' + cPlusDir + chr(10)
    cDLCBIN = replace(search("_progres"),"_progres","").
end.

def stream s1.

FUNCTION fExe returns char (input cCmd as char):
  output stream s1 to value(cPlusDir + WEB-CONTEXT:config-name + ".bat").
  put stream s1 unformatted cExeCmd + cCmd skip.
  output stream s1 close.
  return '"' + SEARCH(cPlusDir + WEB-CONTEXT:config-name + ".bat") + '"'.
end function.


/***** Kill command on NT ******************************/
function fNTKill RETURNS CHAR(input iPID as int):
  DEFINE VARIABLE iResult AS INTEGER NO-UNDO.
  DEFINE VARIABLE iHandle AS INTEGER NO-UNDO.
  RUN OpenProcess(1, 0, iPID, OUTPUT iHandle).
  IF iHandle <> 0 THEN DO:
    RUN TerminateProcess(iHandle, 0, OUTPUT iResult).
    IF iResult = 1
    THEN return "Process Terminated".
    ELSE return "Termination Failed".
  END.
  ELSE return "Unable to Obtain Process Handle".
end function.

PROCEDURE OpenProcess EXTERNAL "KERNEL32.DLL":
  DEFINE INPUT  PARAMETER intAccess        AS LONG.
  DEFINE INPUT  PARAMETER intInherit       AS LONG.
  DEFINE INPUT  PARAMETER intProcessId     AS LONG.
  DEFINE RETURN PARAMETER intProcessHandle AS LONG.
END PROCEDURE.

PROCEDURE TerminateProcess EXTERNAL "KERNEL32.DLL":
  DEFINE INPUT  PARAMETER intProcessID AS LONG.
  DEFINE INPUT  PARAMETER intExitCode  AS LONG.
  DEFINE RETURN PARAMETER intResult    AS LONG.
END PROCEDURE.



/****************************************************************/
/***    generic look & feel              ************************/
/****************************************************************/

FUNCTION fHeader RETURNS CHAR ():
set-user-field("Do",cAction).
{&OUT} '  <HTML><HEAD><TITLE>'  cFormTitle  '</TITLE>~n'
	   '  <link rel="STYLESHEET" type="TEXT/CSS" href="' ffstyles 'freestyle.css">'
       '  <STYLE>BR.page ~{ page-break-after: always ~}</STYLE>~n'
       '  </HEAD><BODY>~n'.
{&OUT} '  <FORM NAME=form METHOD=post ACTION="'  cFormTarget  '.p">~n'
       '  <TABLE BORDER=0 WIDTH="100%"><TR>~n'
       '  <TD ALIGN="LEFT">~n'
       '  <FONT SIZE="+2" COLOR="#660066"><B>' cFormTitle  '</B></FONT></TD>~n'.
{&OUT} '  <TD ALIGN= "RIGHT">'
       '  <INPUT TYPE=button NAME=Back    VALUE=Back    onClick="' cFormBack    '">'
       '  <INPUT TYPE=button NAME=Refresh VALUE=Refresh onClick="' cFormRefresh '">~n'.
if valid-handle(hSession) then {&OUT} '<small><a href=viewlog.p>View Log</a></small>' skip.
{&OUT} '  <A HREF="doc.p?doc='  cFormHelp  '" ~n'
       '   TARGET="helpWindow"~n'
       '   ONCLICK="window.open('''',''helpWindow'',''width=630,height=400,menubar=1,toolbar=1,location=1,scrollbars=1,resizable=1,status=1'')~;">~n'
       '  <IMG SRC="'  cStatic  '/images/help.gif" BORDER=0 ALT="Help"></A></TD>~n'
       '  </TR></TABLE>~n'.
{&OUT} '  <CENTER><IMG SRC="'  cStatic  '/images/wsrule.gif" HEIGHT=3 WIDTH="100%"></CENTER>~n'
       '  <INPUT TYPE=hidden NAME=Name  VALUE='''  cName  '''>~n'
       '  <INPUT TYPE=hidden NAME=Do    VALUE=' cAction '>~n'
        SKIP.
END FUNCTION.

FUNCTION fFooter RETURNS CHAR ():
  {&OUT} '</form></body></HTML>   ~n'.
END FUNCTION.

