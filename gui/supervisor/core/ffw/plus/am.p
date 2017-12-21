 /*-----------------------------------------------------------------------
 File: am.p
 Purpose: Application manager, the front-end.
 Description:
 Author(s) :Per S Digre/PSC
 Created: April 1998
 Notes:    
 Modification History:    
$Header: /cvsroot/freeframework/ffw1.2/ffw/plus/am.p,v 1.1 2002/08/21 16:14:24 freeframework Exp $
$Log: am.p,v $
Revision 1.1  2002/08/21 16:14:24  freeframework
Initial import

Revision 1.1.1.1  2001/03/23 14:50:21  slichtenberg
initial load 1.03


This file contains sample code which may assist you in creating applications.
You may use the code as you see fit. If you modify the code or include it in another software program,
you will refrain from identifying Progress Software as the supplier of the code, or using any
Progress Software trademarks in connection with your use of the code.
THE CODE IS NOT SUPPORTED BY PROGRESS SOFTWARE AND IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
INCLUDING, WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR NONINFRINGEMENT.

-----------------------------------------------------------------------*/
{plus/plus.i}

 ASSIGN
  cFormTitle   = "Application Manager"
  cFormTarget  = "am"
  cFormBack    = "window.open('am.p','_self')"
  cFormHelp    = "am"
  cFormRefresh = "document.form.Name.value='';document.form.submit()"
  .

PROCEDURE outputHTML:
  DEF INPUT PARAM c1 AS CHAR NO-UNDO.
  {&out} '<pre>' c1 '</pre>' SKIP.
END PROCEDURE.

/**** Other Object interactions  ************/
FUNCTION fRCSConfig      RETURNS CHAR() IN hConfig.
FUNCTION fSaveAppCfg     RETURNS CHAR() IN hConfig.
FUNCTION fSaveSysCfg     RETURNS CHAR() IN hConfig.
FUNCTION fSetConfig      RETURNS CHAR(INPUT cDo AS CHAR,INPUT cName AS CHAR) IN hConfig.

FUNCTION fLibraryUpdate  RETURNS CHAR() IN hProutil.
FUNCTION fLibraryView    RETURNS CHAR() IN hProutil.
FUNCTION fBrkStart       RETURNS CHAR(INPUT cUtil AS CHAR,INPUT cName AS CHAR,INPUT cName1 AS CHAR) IN hProutil.
FUNCTION fBrkStop        RETURNS CHAR(INPUT cUtil AS CHAR,INPUT cName AS CHAR,INPUT cName1 AS CHAR) IN hProutil.
FUNCTION fBrkKill        RETURNS CHAR(INPUT cName AS CHAR,INPUT cPID AS CHAR) IN hProutil.
FUNCTION fBrkView        RETURNS CHAR(INPUT cUtil AS CHAR,INPUT cName AS CHAR,INPUT cName1 AS CHAR) IN hProutil.
FUNCTION fBrkAdd         RETURNS CHAR(INPUT cUtil AS CHAR,INPUT cName AS CHAR,INPUT cName1 AS CHAR) IN hProutil.

FUNCTION fCompileResults RETURNS CHAR() IN hCode.
FUNCTION fCompileStart   RETURNS CHAR() IN hCode.
FUNCTION fCompile RETURNS CHAR (INPUT cFileFull AS CHAR,INPUT cAction AS CHAR) IN hCode.

/*******          Main code       **********/

DEF VAR cUtil  AS CHAR NO-UNDO.
DEF VAR cName2 AS CHAR NO-UNDO.
DEFINE VARIABLE i1      AS INTEGER NO-UNDO.

DEFINE VARIABLE cPath       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTask       AS CHARACTER NO-UNDO.


PROCEDURE output-headers:
  plusLog("DB:" + cDB).
  plusLog("Dir:" + cDir).
  plusLog("Lib:" + cLib).
  plusLog("Tsk:" + cTsk).

  IF VALID-HANDLE(hProutil) THEN DELETE PROCEDURE hProutil.
  plusLog("Running oProutil").
  RUN plus/oProutil.p PERSISTENT SET hProutil.
  plusLog("Callback oProutil").
  RUN callback IN hProutil (THIS-PROCEDURE,'outputHTML').

  /* Developing add below line to allow changes  */

  IF VALID-HANDLE(hCode) THEN DELETE PROCEDURE hCode.
  plusLog("Running oCode").
  RUN plus/oCode.p PERSISTENT SET hCode.
  plusLog("Callback oCode").
  RUN callback IN hCode (THIS-PROCEDURE,'outputHTML').

  IF VALID-HANDLE(hConfig) THEN DELETE PROCEDURE hConfig.
  plusLog("Running oConfig").
  RUN plus/oConfig.p PERSISTENT SET hConfig.
  plusLog("Callback oConfig").
  RUN callback IN hConfig (THIS-PROCEDURE,'outputHTML').

  IF CAN-DO("undefined,null",cName) AND cAction BEGINS "add" THEN cAction = "".


  IF CAN-DO('AddTsk,RemTsk,AddLib,RemLib,AddDir,RemDir,AddWS,AddNS,AddAS,AddOD,AddOR,AddDB',cAction)
  THEN fSetConfig(cAction,cName).
  IF CAN-DO('saveAppCfg',cAction) THEN fSaveAppCfg().
  IF CAN-DO('saveSysCfg',cAction) THEN fSaveSysCfg().
  IF CAN-DO('RCSconfig',cAction)  THEN fRCSConfig().



  plusLog("getting files..").

  IF CAN-DO('AllCompile,AllDocument,xcode,xcomp',cAction)  /*** operations on checked files *****/
  THEN fSetFiles().
  ELSE DO:
    ASSIGN
      cPath    = GET-VALUE("CD")
      cFiles = ""
      cTask  = ENTRY(1,cTsk).

    IF cPath = ""       THEN cPath = cName.
    IF cPath BEGINS "*" THEN cTask  = SUBSTRING(cPath,2).
    IF cTask > ""      THEN DO:
      IF SEARCH(cTask + ".task") > ""
      THEN fTaskFiles(cTask).
    END.
    IF cPath BEGINS "*"
    THEN cFiles = cTaskfiles.
    ELSE IF cPath > "" THEN cFiles = fFilesInDir(cPath).

    IF CAN-DO('DirUtil',cAction) THEN DO:  /***** Move task to active *****/
        IF LOOKUP(SUBSTRING(cName,2),cTsk)  > 1 THEN DO:
          ASSIGN
            ENTRY(LOOKUP(SUBSTRING(cName,2),cTsk),cTsk) = ""
            cTsk = SUBSTRING(cName,2) + "," + cTsk.
          fIniSave().
        END.
    END.
  END.
END PROCEDURE.
 {&cStopTest}


IF CAN-DO('AppCfg,AddDir,RemDir,RCSconfig',cAction) THEN DO:             /**** System config *****/
  ASSIGN cFormTitle  = "Application Config"
         cFormHelp   = "appcfg".
  fHeader().
  RUN showAppcfg IN hConfig.
  fFooter().
  RETURN.
END.

IF CAN-DO('SysCfg,AddWS,AddNS,AddAS,AddOD,AddOR,AddDB',cAction) THEN DO: /**** Application config ****/
  ASSIGN cFormTitle  = "System Config"
         cFormHelp   = "syscfg".
  fHeader().
  RUN showSysCfg IN hConfig.
  fFooter().
  RETURN.
END.

IF CAN-DO('remtsk',cAction) THEN DO:  /**** Complete the task *****/
  IF cRCS > "" THEN DO:
    {&out} "<br>Completing task files for <b>" cTask "</b>:" cFiles SKIP.
    fDoWithFiles("End",cFiles,cTask).
  END.
END.



/*********************************************************/
/******** Code Utilities *********************************/
/*********************************************************/

IF CAN-DO('DirUtil,Del,Add,End,Diff,RCS,CompileFile,Beautify,Listing,PreCompile,XREF,SXR,Debug',cAction) THEN DO:
  ASSIGN cFormTitle  = IF cPath BEGINS "*" THEN "Task Utilities" ELSE "Code Utilities"
         cFormHelp   = "amfile".
  fHeader().
  {&out} '  <INPUT TYPE=hidden NAME=cd VALUE="' cPath '" >'.

  IF CAN-DO('beautify',cAction) THEN RUN showBeautify IN hCode.
  fDoWithFiles(cAction,cName,cTask).
  IF CAN-DO('Del,Add',cAction) THEN DO:  /*** Update files after delete/add ****/
    fTaskFiles(cTask).
    IF cPath BEGINS "*" THEN cFiles = cTaskfiles.
  END.
  IF NOT (cName > '' AND CAN-DO('diff,beautify,Listing,PreCompile,XREF,SXR,Debug',cAction))
  THEN RUN showCode IN hCode.
  fFooter().
  RETURN.
END.

fHeader().

RUN appMan.

IF cAction = "UpdLib" AND cLib > "" THEN fLibraryUpdate().
IF cAction MATCHES "*brk*" THEN ASSIGN
  cUtil = ENTRY(LOOKUP(ENTRY(1,cName,":"),"WS,NS,AS,OD,OR") + 1,",wtbman,nsman,asbman,odbman,oraman")
  cName2 = ENTRY(2,cName,":").

CASE cAction:
  WHEN "ListLib" OR WHEN "UpdLib" THEN IF cLib > "" THEN fLibraryView().
  WHEN "AllCompile"                                 THEN fCompileStart().
  WHEN "DirCompile"                                 THEN fCompileStart().
  WHEN "Compile"                                    THEN fCompileResults().
  WHEN "StartBrk"                                   THEN fBrkStart(cUtil,cName,cName2).
  WHEN "StopBrk"                                    THEN fBrkStop(cUtil,cName,cName2).
  WHEN "AddBrk"                                     THEN fBrkAdd(cUtil,cName,cName2).
  WHEN "AllDocument" THEN DO:
    {&out} "</center><h2>Documenting</h2>" + cFiles.
    DO i1 = 1 TO NUM-ENTRIES(cFiles,","):
      RUN plus/amdoc.p(ENTRY(i1,cFiles,",")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN {&out} "ERROR:" ERROR-STATUS:GET-MESSAGE(1).
    END.
    {&out} "<center>".
  END.
  WHEN "XComp" THEN DO:
    {&out} "</center><h2>XCode compiling</h2>" + cFiles.
    DO i1 = 1 TO NUM-ENTRIES(cFiles,","):
      fCompile(ENTRY(i1,cFiles,","),"xcomp").
    END.
    {&out} "<center>".
  END.
END CASE.
IF cAction BEGINS "KillBrk" THEN fBrkKill(cName,SUBSTRING(cAction,8)).
IF CAN-DO("StartBrk,ViewBrk,AddBrk",cAction) OR cAction BEGINS "KillBrk" THEN fBrkView(cUtil,cName,cName2).

fFooter().




PROCEDURE appMan:
  DEFINE VARIABLE i1      AS INTEGER NO-UNDO.
  DEFINE VARIABLE c1      AS CHARACTER NO-UNDO.
  {&out} fBeginTable("Servers &nbsp; &nbsp; &nbsp; "
        + fLink("syscfg", "","Config")
        + "|&nbsp;").
  DO i1 = 1 TO NUM-ENTRIES(cBrk):
    ASSIGN c1 = ENTRY(i1,cBrk).
     {&out} fRow(c1 + "|"
        + fLink("ViewBrk",c1,"Status")
        + fLink("StartBrk",c1,"Start")
        + fLink("StopBrk",c1,"Stop")
        + (IF c1 BEGINS "ws:" THEN "<A HREF=" + SCRIPT_NAME + "/WService=" + SUBSTRING(c1,4) + "/workshop TARGET=_top>Go to..</a>" ELSE "")
        ).
   END.
   DO i1 = 1 TO NUM-ENTRIES(cDB):
     ASSIGN c1 = ENTRY(i1,cDB).
     {&out} fRow("DB:" + c1 + "|"
        + " <A HREF=""amdb.p?db=" + STRING(i1) + '">Tools</a>'
        + " <A HREF=""amdb.p?return=am&Do=serve&db=" + STRING(i1) + '">Start</a>'
        + " <A HREF=""amdb.p?return=am&Do=shut&db=" + STRING(i1) + '">Stop</a>'  ).
   END.
   {&out} fHRow("CodePath &nbsp; &nbsp;"
        + fLink("appcfg", "","Config")  + "|"
        + fLink("AllCompile","","Compile")
        + fLink("xcomp","prompt('Compile decryption key:','R&D')","XComp")
        + fLink("AllDocument","","Document") ).
   DO i1 = 1 TO NUM-ENTRIES(cDir):
     ASSIGN c1 = ENTRY(i1,cDir).
     {&out} fRow("<INPUT TYPE=checkbox NAME=dir" + c1
        + (IF GET-VALUE("dir" + c1) = "" AND REQUEST_METHOD = "Post" THEN " " ELSE " checked") + ">"
        + fLink("DirUtil",c1,ENTRY(i1,cDir)) + "|"
        + fLink("DirCompile",c1,"Compile")
        + " <A HREF=""../webtools/dirlist?filter=*.w%3B*.p%3B*.i%3B*.htm%3B*.html&directory=" + URL-ENCODE(ENTRY(i1,cDir),"default") + '">FileTools</a> ').
   END.
   {&out} fHRow("Tasks &nbsp; (" + ENTRY(1,cTsk) + ") &nbsp; &nbsp;"
        +       fLink("AddTsk","prompt('Name of new task:')","Add")
        + "|&nbsp;").
   DO i1 = 1 TO NUM-ENTRIES(cTsk):
     ASSIGN c1 = ENTRY(i1,cTsk).
     {&out} fRow(fLink("DirUtil","*" + c1,c1) + "|"
        + fLink("DirCompile","*" + c1,"Compile")).
   END.
   {&out} fHRow(IF cLib = ""
     THEN fLink("AddLib","prompt('Full path of library (including *.pl):')","Configure R-Code Library") + "|&nbsp;"
     ELSE ("RcodeLib:" + fLink("ListLib","",cLib)
        + "|" + fLink("RemLib","","Del")
        +       fLink("UpdLib","","Update"))).
   {&out} "</TABLE>" SKIP.

END PROCEDURE.







