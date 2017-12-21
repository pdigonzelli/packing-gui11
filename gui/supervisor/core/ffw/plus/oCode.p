/*-----------------------------------------------------------------------
File: oProutil.p
Purpose: RCS, Compile & Server tools.
Description:
Author(s) :Per S Digre/PSC
Created: April 1998
Notes:    
Modification History:    
$Header: /cvsroot/freeframework/ffw1.2/ffw/plus/oCode.p,v 1.1 2002/08/21 16:14:24 freeframework Exp $
$Log: oCode.p,v $
Revision 1.1  2002/08/21 16:14:24  freeframework
Initial import

Revision 1.1.1.1  2001/03/23 14:50:35  slichtenberg
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
plusLog("Init Code-object").

/**** generic object definition section  ********/
DEFINE VARIABLE hParent   AS HANDLE NO-UNDO.
DEFINE VARIABLE cCallback AS CHAR   NO-UNDO.

PROCEDURE callback:
  DEFINE INPUT PARAMETER h1 AS handle NO-UNDO.
  DEFINE INPUT PARAMETER c1 AS char   NO-UNDO.
  assign hParent   = h1
         cCallBack = c1.
END PROCEDURE.


/**** Other Object interactions  ************/
DEFINE NEW GLOBAL SHARED VARIABLE cAction     AS CHARACTER NO-UNDO.  /** HTML Command       **/
DEFINE NEW GLOBAL SHARED VARIABLE cName       AS CHARACTER NO-UNDO.  /** HTML param         **/
DEFINE NEW GLOBAL SHARED VARIABLE cPath       AS CHARACTER NO-UNDO.  /** CodePath selected  **/
DEFINE NEW GLOBAL SHARED VARIABLE cTask       AS CHARACTER NO-UNDO.  /** Task  selected     **/
DEFINE NEW GLOBAL SHARED VARIABLE cFiles      AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cDirCD      AS CHARACTER NO-UNDO. /** Current directory **/
DEFINE NEW GLOBAL SHARED VARIABLE cTaskFiles  AS CHARACTER NO-UNDO. /** list of full paths to non progress directories ***/
DEFINE NEW GLOBAL SHARED VARIABLE lLocal      AS Log       NO-UNDO. /** indicator if local RCS user ***/
DEFINE NEW GLOBAL SHARED VARIABLE cRCS        AS CHARACTER NO-UNDO. /** Path RCS-Root **/

def new global shared var hHTML    as handle no-undo.
def new global shared var hPlus    as handle no-undo.
FUNCTION fRow        RETURNS CHAR(INPUT cData   AS CHAR) IN hHTML.
FUNCTION fHRow       RETURNS CHAR(INPUT cLabels AS CHAR) IN hHTML.
FUNCTION fBeginTable RETURNS CHAR(INPUT cLabels AS CHAR) IN hHTML.
FUNCTION fTable      RETURNS CHAR(INPUT cLabels AS CHAR
                                 ,INPUT cData AS CHAR) IN hHTML.
FUNCTION fLink       RETURNS CHAR(INPUT cMode AS CHAR
                                 ,INPUT cValue AS CHAR
                                 ,INPUT cText AS CHAR)    IN hHTML.
FUNCTION fExe        returns char (input cCmd as char) IN hPlus.




/******* Object Main code       **********/
DEF stream s1.
DEF stream s2.


/******** Compile several files ******/
FUNCTION fCompileResults RETURNS CHAR():
  def var c1 as char no-undo.
  def var c2 as char no-undo.
  DEFINE VARIABLE i1   AS INTEGER NO-UNDO.
  OUTPUT STREAM WebStream CLOSE.
  OUTPUT STREAM WebStream TO "compile.lst".
  RUN webtools/fileact.r no-error.
  OUTPUT STREAM WebStream CLOSE.
  OUTPUT STREAM WebStream TO "WEB".
  {&out} fBeginTable("Filename|Compile Status").
  INPUT STREAM s1 FROM "compile.lst".
  REPEAT:
    assign c1 = ''.
    IMPORT STREAM s1 UNFORMATTED c1.
    i1 = INDEX(c1,"<i>").
    IF c1 MATCHES "*Generating SpeedScript*" THEN c2 = "".
    IF c1 MATCHES '*<FONT COLOR="#990000"><B>Compiling*' THEN c2 = SUBSTRING(SUBSTRING(c1,i1 + 3),1,INDEX(SUBSTRING(c1,i1 + 3),"</i>") - 1) + "|".
    IF c1 MATCHES "*<p>*" THEN c2 = c2 + c1.
    IF c1 MATCHES "*<UL>No Errors*" THEN c2 = c2 + "OK!".
    IF c1 MATCHES "*</UL>*" THEN DO:
      IF c2 > "" THEN {&out} fRow(c2).
      assign c2 = "".
    end.
  END.
  {&out} "</table>" SKIP.
  INPUT STREAM s1 CLOSE.
end function.

FUNCTION fCompileStart RETURNS CHAR():
  {&OUT} '<INPUT TYPE="HIDDEN" NAME="FileAction" VALUE="Compile">~n'.
  {&OUT} '<INPUT TYPE="HIDDEN" NAME="Filename" VALUE="'  HTML-ENCODE(cFiles)  '">~n'.
  {&OUT} '<H2>Please wait!</H2><P>Compilation listing will be shown after all programs have finished compiling!</p>~n'.
  {&OUT} '<script language="JavaScript">~n'.
  {&OUT} "  document.form.Do.value='Compile';~n".
  {&OUT} '  document.form.submit()~n'.
  {&OUT} '</SCRIPT>~n'.
end function.



/***** Compile single files *******/

FUNCTION fCompile returns char (input cFileFull as char,input cAction as char):
  DEFINE VARIABLE i1         AS INT       NO-UNDO.
  DEFINE VARIABLE cSpeedFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOptions   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFile      AS CHARACTER NO-UNDO.
  ASSIGN
    cFile     = cFileFull
    cSpeedFile = ""
    cOptions = "". /* uncompiled webspeed object to create */
  {&out} "<br>Compiling " cFileFull SKIP.
  IF cFileFull MATCHES "*.html" OR
     cFileFull MATCHES "*.htm"  THEN DO:
    ASSIGN cOptions = ENTRY(1,cFileFull,".") + ".w".
    RUN webutil/e4gl-gen.r
         (INPUT cFileFull,
          INPUT-OUTPUT cSpeedFile,
          INPUT-OUTPUT cOptions) NO-ERROR.
    IF ERROR-STATUS:ERROR
    THEN {&out} "<br>ERROR = " ERROR-STATUS:GET-MESSAGE(1) SKIP.
    ASSIGN cFileFull = cOptions.
    {&out} "<br>Scripted " cFileFull SKIP.
  END.

  case cAction:
    when "compileFile" then COMPILE VALUE(cFileFull) SAVE NO-ERROR.
    when "precompile" then COMPILE VALUE(cFileFull) SAVE preprocess  VALUE(cFile + ".pre") NO-ERROR.
    when "listing"    then COMPILE VALUE(cFileFull) SAVE listing     VALUE(cFile + ".lst") NO-ERROR.
    when "xref"       then COMPILE VALUE(cFileFull) SAVE xref        VALUE(cFile + ".xrf") NO-ERROR.
    when "sxr"        then COMPILE VALUE(cFileFull) SAVE string-xref VALUE(cFile + ".sxr") NO-ERROR.
    when "debug"      then COMPILE VALUE(cFileFull) SAVE debug-list  VALUE(cFile + ".dbg") NO-ERROR.
    when "xcomp"      then COMPILE VALUE(cFileFull) SAVE XCODE cName NO-ERROR.

  end case.

  DO i1 = 1 TO ERROR-STATUS:NUM-MESSAGES:
    {&out} "<br>Message " i1 ": " ERROR-STATUS:GET-MESSAGE(i1) SKIP.
  END.
  {&out} "<br>" cAction "  " cFileFull " : " STRING(COMPILER:ERROR,"ERROR/OK") SKIP.
  IF cOptions > "" THEN OS-DELETE VALUE(cFileFull).
end function.



/***** TASK & RCS management *********/

FUNCTION fSaveStatus returns char (
    input cStat as char,
    input cTask as char):
  if cStat > "" then do:
    output stream s1 to value(cTask + ".task") append.
    put stream s1 unformatted
       cStat skip.
    output stream s1 close.
  end.
  RETURN "".
end function.


/***** Executing Command-Line *******/
FUNCTION fRCS returns log (
    input cTxt as char,
    input cCmd as char,
    input cStat as char,
    input cTask as char):
  def var c1  as char no-undo.
  def var lRet as log  no-undo init TRUE.
  def var cVer as char no-undo.
    {&out} "<font color=black><pre>".

  INPUT STREAM s1 THROUGH VALUE(fExe(cCmd)).
  REPEAT:
    IMPORT STREAM s1 UNFORMATTED c1 NO-ERROR.
    if c1 matches "*rm -f *"
    then lRet = FALSE.
    {&out} HTML-ENCODE(c1) skip.
    END.
  INPUT STREAM s1 CLOSE.
  {&out} "</pre></font>" skip.

  if cStat > ""
  then fSaveStatus(cStat + ":" + cVer ,cTask).

end function.


FUNCTION fLine returns char (input i1 as int, input i2 as int, input cTxt as char):
  RETURN
        "<font color="
      + (if i1 = 0 then "red" else if i2 = 0 then "green" else "black")
      + ">" + string(i1,">>>>") + string(i2,">>>>") + " " + HTML-ENCODE(cTxt) + "</font>" + CHR(10).
end function.


function fDiff returns char (input cOld as char, input cDif as char):
  DEFINE VARIABLE iNew        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iOld        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i1          AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i2          AS INTEGER   NO-UNDO.
  DEFINE VARIABLE c1          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c2          AS CHARACTER NO-UNDO.

  def var iFFW   as int  no-undo.
  def var iNum   as int  no-undo.
  def var cCmd   as char no-undo.

  input stream s1 from value(cDif).
  input stream s2 from value(cOld).
  {&out} "<pre>" skip.
/*  {&out} "Old:" cOld "  Diff:" cDif skip. */

  repeat:
    assign
      c1    = "".
    import stream s1 unformatted c1.
    if c1 = "" then leave.
    assign
      cCmd = substring(c1,1,1)
      iFFW = int(entry(1,substring(c1,2)," "))
      iNum = int(entry(2,substring(c1,2)," ")).

/*    {&out} "FFW" string(iFFW) skip.      */
    i1 = iOld + 1.
    i2 = iFFW - 1.
    do iOld = i1 to i2:           /**** Fast forward in old file ***/
      import stream s2 unformatted c2.
      assign iNew = iNew + 1.
      {&out} fLine(iNew,iOld,c2).
    end.
    assign iOld = iOld - 1.
    case cCmd:
     when "a" then do:                  /**** iNum steps new changes ***/
/*      {&out} "Add iNum" string(iNum) skip.   */
      i1 = iNew + 1.
      i2 = iNew + iNum.
      do iNew = i1 to i2:
        import stream s1 unformatted c2.
        {&out} fLine(iNew,0,c2).
      end.
      assign iNew = iNew - 1.
     end.
     when "d" then do:                  /**** iNum steps deleted old ***/
/*      {&out} "Del iNum" string(iNum) skip.     */
      i1 = iOld + 1.
      i2 = iOld + iNum.
      do iOld = i1 to i2:
        import stream s2 unformatted c2.
        {&out} fLine(0,iOld,c2).
      end.
      assign iOld = iOld - 1.
     end.
    end.
  end.

  input stream s1 close.
  repeat:
    import stream s2 unformatted c2.
    assign iNew = iNew + 1
           iOld = iOld + 1.
    {&out} fLine(iNew,iOld,c2).                        /**** End of file, Both ***/
  end.
  input stream s2 close.
  {&out} "</pre>" skip.
end.


function fDoWithFiles returns char(input cAction as char,input cFiles as char,input cTask as char):
  def var         i1        as int  no-undo.
  def var        c1         as char no-undo.
  DEFINE VARIABLE cFileRepo AS CHARACTER NO-UNDO.  /*** Location of RCS common dir ***/
  DEFINE VARIABLE cFileLocl AS CHARACTER NO-UNDO.  /*** Location of Local File     ***/
  DEFINE VARIABLE cFile     AS CHARACTER NO-UNDO.  /*** Input Filename             ***/
  DEFINE VARIABLE cFileFull AS CHARACTER NO-UNDO.  /*** Full Input filename        ***/
  DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.  /*** Just Filename              ***/
  DEFINE VARIABLE lRCS      AS Logical   NO-UNDO.  /*** Indicator for file in RCS  ***/

  do i1 = 1 to num-entries(cFiles):
    assign cFile = ENTRY(i1,cFiles).
    if cFile = "" then next.
    assign
        cFileFull = replace(replace(search(cFile),"~\","/"),"./",cDirCD)  /** Full filename **/
        cFileName = entry(num-entries(cFileFull,"/"),cFileFull,"/").      /** Filename **/
    if cRCS > "" then do:
      assign
        cFileRepo = search(replace(cFile,cFileName,"") + "RCS/" + cFileName + ",v") /*** RCS *,v file ***/
        lRCS      = (cFileRepo <> ?).
      if not lRCS
      then assign
        cFileRepo = replace(cFileFull,cDirCD,cRCS)
        cFileRepo = replace(cFileRepo,cFileName,"") + "RCS/" + cFileName + ",v"
        .
      assign                                            /** Real repository dir **/
        cFileRepo = replace(replace(cFileRepo,"~\","/"),"./",cDirCD)
        cFileRepo = substring(cFileRepo,1, index(cFileRepo,"RCS/") - 1).

      if lLocal then assign
        cFileLocl = replace(cFileRepo,cRCS,cDirCD).

    end.
    else assign
        cFileRepo = ""
        cFileLocl = ""
        lRCS      = false.

    CASE cAction:
      WHEN "Beautify" THEN RUN beautify(cFileFull).
      WHEN "CompileFile"  THEN fCompile(cFileFull,cAction).
      WHEN "Listing" OR
      WHEN "XREF"    OR
      WHEN "SXR"     OR
      WHEN "Debug"   OR
      WHEN "Precompile" THEN DO:
        fCompile(cFileFull,cAction).
        run pShowFile(cFileFull + "." + entry(1 + lookup(cAction,"listing,xref,precompile,sxr,debug"),"tmp,lst,xrf,pre,sxr,dbg")).
      END.
      WHEN "RCS" THEN DO:    /**** RCS History *****/
        fRCS("RCS History","rlog " + cFileRepo + cFileName,"",cTask).
      end.
      WHEN "Del" THEN DO:    /**** Remove from task *****/
        if cRCS > ""
        then do:
          fRCS("Undo","co -l -f " + cFileRepo + cFileName,cFile + ";del ",cTask).
          os-delete value(cFileRepo + "rcs/" + cFileName + ".stamp") no-error.
          if lLocal then os-delete value(cFileLocl + cFileName) no-error.
        end.
        else fSaveStatus(cFileFull + ";del ",cTask).
      end.
      WHEN "End" THEN DO:  /**** Final Checkin for the task *****/
        fRCS("Finish task",'ci -j -u -t-Final -m' + replace(cName,' ','') + ' ' + cFileRepo + cFileName,cFile + ";ci ",cTask).
        os-delete value(cFileRepo + "RCS/" + cFileName + ".stamp") no-error.
      END.
      WHEN "Add" THEN DO:  /***** Initial check-in *****/
        if cRCS > "" THEN DO:
          IF lRCS
          THEN DO:        /**** Regular checkout ****/
            fRCS("Checking out","co -l -f " + cFileRepo + cFileName,cFile + ";co ",cTask).
            if lLocal then os-copy value(cFileRepo + cFileName) value(cFileLocl).
          END.
          ELSE DO:        /**** Initial checkout ****/
            os-create-dir value(cFileRepo + 'RCS') no-error.
            os-delete value(cFileRepo + "RCS/" + cFileName) no-error.
            if lLocal then os-copy value(cFileLocl + cFileName) value(cFileRepo).
            fRCS("Creating in task","ci -i -l -t-New " + cFileRepo + cFileName,cFile + ";new ",cTask).
          end.
          os-copy value(cFileRepo + cFileName) value(cFileRepo + "RCS/").
          output stream s1 to value(cFileRepo + "RCS/" + cFileName + ".stamp").
          put stream s1 unformatted web-context:config-name "," cTask skip.
          output stream s1 close.
        END.
        ELSE
          fSaveStatus(cFileFull + ";add ",cTask).
      END.
      WHEN "diff" then do:
        OS-DELETE value(cFileRepo + cFileName + ".dif").
        fRCS("Diff","rcsdiff -n " + cFileRepo + cFileName + " > " + cFileRepo + cFileName + ".dif","",cTask).
        fDiff(cFileRepo + "rcs/" + cFileName,cFileRepo + cFileName + ".dif").
      end.
    END CASE.
  END.
END FUNCTION.

procedure pShowFile:
  def var        c1        as char no-undo.
  def input param cName as char no-undo.
  {&out} "<hr><pre>" SKIP.
  INPUT STREAM s1 FROM VALUE(cName).
  REPEAT:
    assign c1 = "".
    IMPORT STREAM s1 UNFORMATTED c1.
    {&out} HTML-ENCODE(c1) SKIP.
  END.
  INPUT STREAM s1 CLOSE.
end procedure.




/****** Beautify interface ******************/

def new global shared var hAnalyze as handle no-undo.

procedure beautify:
  DEFINE INPUT PARAMETER pFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFile     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c1        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBeautify AS CHARACTER NO-UNDO.

  {&out} '<BR CLASS=page><H2>Beautifying: ' pFile '</h2><HR><BR><PRE>' skip.

  /***** Initialize *****************/

  ASSIGN
     pFile = SEARCH(pFile)
     cFile = pFile + ".beau".

  IF GET-VALUE("savechg") = "o" THEN DO:
    OS-COPY VALUE(pFile) VALUE(cFile).
    ASSIGN
      c1    = cFile
      cFile = pFile
      pFile = c1.
  END.

  ASSIGN
    cBeautify = (if GET-VALUE("typeout") > ""  then ',pe' else '')
              + (if GET-VALUE("keyword") = "u" then ',pu' else '')
              + (if GET-VALUE("keyword") = "l" then ',pl' else '')
              + (if GET-VALUE("keyhtml") = "u" then ',hu' else '')
              + (if GET-VALUE("keyhtml") = "l" then ',hl' else '').

  RUN outputLine(replace('HTML|SCRP|PROG|COMM|QUOT|CR','|',chr(2)),
                 replace(' HTML | Script-Language | Progress-4GL | Progress-Comments | Progress-Text-Strings |CR','|',chr(2))).

  /***** Initialize analyze object *******/
  if not valid-handle(hAnalyze)
  then run plus/oAnalyze.p persistent set hAnalyze.
  run callback in hAnalyze (this-procedure,',outputLine').
  run beautify in hAnalyze(cBeautify,cFile).
  run process  in hAnalyze(pFile).
  DELETE PROCEDURE hAnalyze.


  IF GET-VALUE("savechg") = "o" THEN DO:
/*    {&out} "<br>"  "rename " + cFile + " " + ENTRY(NUM-ENTRIES(pFile,"~\"),cFile,"~\") SKIP.    */
    IF OPSYS = "win32"
    THEN DO:
      DOS SILENT del    VALUE(pFile).
      DOS SILENT VALUE("rename " + cFile + " " + ENTRY(NUM-ENTRIES(cFile,"~\"),cFile,"~\")).   /**** Deal with uppercasing of filenames ****/
    END.
    ELSE UNIX SILENT rm  VALUE(pFile).
  END.

  {&out} '</PRE>' skip.
END PROCEDURE.

PROCEDURE outputLine:     /**** For use with callback for the analyze object **/
  DEFINE INPUT PARAMETER cLineCmd  AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER cLineData AS CHAR NO-UNDO.

  DEF VAR i1 as INT NO-UNDO.
  DEF VAR i2 as INT NO-UNDO.
  DEF VAR cElemCmd as char NO-UNDO.
  DEF VAR cElemData as char NO-UNDO.
  assign i2 = NUM-ENTRIES(cLineCmd,chr(2)).
  if NUM-ENTRIES(cLineData,chr(2)) <> i2 then return.

  DO i1 = 1 TO i2:
    ASSIGN cElemCmd  = ENTRY(i1,cLineCmd, chr(2))
           cElemData = ENTRY(i1,cLineData,chr(2)).
    CASE cElemCmd:
      WHEN "CR"   THEN {&out} skip.
      WHEN "PB"   THEN {&out} .
      WHEN "HTML" THEN {&out} "<FONT color=blue>"   HTML-ENCODE(cElemData) "</FONT>".
      WHEN "SCRP" THEN {&out} "<FONT color=brown>"  HTML-ENCODE(cElemData) "</FONT>".
      WHEN "COMM" THEN {&out} "<FONT color=green>"  HTML-ENCODE(cElemData) "</FONT>".
      WHEN "QUOT" THEN {&out} "<FONT color=red>"    HTML-ENCODE(cElemData) "</FONT>".
      WHEN "PROG" THEN {&out} "<FONT color=black>"  HTML-ENCODE(cElemData) "</FONT>".
      WHEN "SCRB" THEN {&out} "<FONT color=black><u>"  HTML-ENCODE(cElemData) "</u></FONT>".
      OTHERWISE        {&out}                       HTML-ENCODE(cElemData).
    END CASE.
  END.
END PROCEDURE.


function fLocked returns char (input cFile as char):
  def var c1 as char no-undo.
  input  stream s1 from value(cFile).
  import stream s1 unformatted c1.
  input  stream s1 close.
  return "Locked by " + entry(1,c1) + " in task " + entry(2,c1 + ",").
end function.

PROCEDURE showCode:
  DEFINE VARIABLE cFileRepo AS CHARACTER NO-UNDO.  /*** Location of RCS common dir ***/
  DEFINE VARIABLE cFileLocl AS CHARACTER NO-UNDO.  /*** Location of Local File     ***/
  DEFINE VARIABLE cFile     AS CHARACTER NO-UNDO.  /*** Input Filename             ***/
  DEFINE VARIABLE cFileFull AS CHARACTER NO-UNDO.  /*** Full Input filename        ***/
  DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.  /*** Just Filename              ***/
  DEFINE VARIABLE lRCS      AS Logical   NO-UNDO.  /*** Indicator for file in RCS  ***/
  def var         i1        as int  no-undo.

  {&OUT}
       '<br>Perform~n'
       '<SELECT NAME=perform SIZE=1 onChange="document.form.Do.value = document.form.perform.options[document.form.perform.selectedIndex].value;document.form.submit()">~n'
       ' <OPTION VALUE=Compile '    if cAction = "Compile"    then "selected" else "" '>Compile</option>~n'
       ' <OPTION VALUE=Beautify '   if cAction = "Beautify"   then "selected" else "" '>Beautify</option>~n'
       ' <OPTION VALUE=Listing '    if cAction = "Listing"    then "selected" else "" '>Compile Listing</option>~n'
       ' <OPTION VALUE=PreCompile ' if cAction = "PreCompile" then "selected" else "" '>PreProcess</option>~n'
       ' <OPTION VALUE=XREF '       if cAction = "XREF"       then "selected" else "" '>XREF-Compile</option>~n'
       ' <OPTION VALUE=SXR '        if cAction = "SXR"        then "selected" else "" '>String X-Ref</option>~n'
       ' <OPTION VALUE=Debug '      if cAction = "Debug"      then "selected" else "" '>Debug-list</option>~n'
       '<table  border=0 cellpadding=1 cellspacing=2 width=100% >~n'.

  {&out} fHRow("Files in " + cPath
       + "|Task: "
       + (if cPath begins "*"
          then "<A HREF=""JavaScript:fEnd();"">Finish</a>"
          ELSE "<A HREF=""am.htm?cd=*" + cTask + '">' + cTask + "</a>")
          ).

  DO i1 = 1 TO NUM-ENTRIES(cFiles):
    assign cFile = ENTRY(i1,cFiles).
    if cFile = "" then next.
    assign
        cFileFull = replace(replace(search(cFile),"~\","/"),"./",cDirCD)  /** Full filename **/
        cFileName = entry(num-entries(cFileFull,"/"),cFileFull,"/").      /** Filename **/
    if cRCS > "" then do:
      assign
        cFileRepo = search(replace(cFile,cFileName,"") + "RCS/" + cFileName + ",v") /*** RCS *,v file ***/
        lRCS      = (cFileRepo <> ?).
      if not lRCS
      then assign
        cFileRepo = replace(cFileFull,cDirCD,cRCS)
        cFileRepo = replace(cFileRepo,cFileName,"") + "RCS/" + cFileName + ",v"
        .
      assign                                            /** Real repository dir **/
        cFileRepo = replace(replace(cFileRepo,"~\","/"),"./",cDirCD)
        cFileRepo = substring(cFileRepo,1, index(cFileRepo,"RCS/") - 1).

      if lLocal then assign
        cFileLocl = replace(cFileRepo,cRCS,cDirCD).

    end.
    else assign
        cFileRepo = ""
        cFileLocl = ""
        lRCS      = false.

    {&out} fRow("<A HREF=""JavaScript:fExec('"
       + cFile + "');"">" + cFile + "</a> &nbsp; &nbsp; " + "|"
       + (IF lRCS
          THEN   fLink("RCS" ,cFile,"RCS")
          ELSE   "&nbsp;")                             /*** RCS history only if in RCS *****/

       + (IF cPath begins "*"
          THEN fLink("Del" ,cFile,"Undo")        /**** in TASK mode ****/
             + (if lRCS then fLink("Diff" ,cFile,"Diff") else "")
          ELSE
           (IF CAN-DO(cTaskFiles,cFile) or cTask = ""
            THEN "&nbsp;"                              /**** in File mode & in Task ****/
            ELSE
             (IF search(cFileRepo + "RCS/" + cFileName + ".stamp") > ""
              THEN fLocked(cFileRepo + "RCS/" + cFileName + ".stamp")  /*** Locked file ***/
              ELSE fLink("Add" ,cFile,"Add")     /**** in File mode & in Task ****/
              ))   /**** in File mode & not in Task ****/
          ) ).
  END.
  {&out} "</TABLE>" skip.
  {&OUT}
       '<SCRIPT language=javascript>~n'
       "document.form.Name.value=''~;~n"
       "if (document.form.Do.value=='DUtil') document.form.Do.value='Compile'~;~n"
       'function fExec(cFile)~{~n'
       '  document.form.Name.value=cFile~;~n'
       '  document.form.submit()~;~n'
       '~}~n'
       'function fEnd()~{~n'
       '  document.form.Do.value=''remtsk''~;~n'
       '  document.form.Name.value=prompt(''Description of changes'')~;~n'
       '  document.form.submit()~;~n'
       '~}~n'
       '</SCRIPT>~n'.
END PROCEDURE.


/***** Display beautify options *******/

FUNCTION fRadio returns char (input cName as char, input cValue as char):
  RETURN "<INPUT TYPE=radio  NAME=" + cName + " VALUE='"
       + cValue + "'"
       + (IF GET-VALUE(cName) = cValue THEN " CHECKED>" ELSE ">").
end function.

procedure showBeautify:
  {&out} fBeginTable("Beautify options:|"
   + fRadio("savechg","")  + "No "
   + fRadio("savechg","o") + "Replace "
   + fRadio("savechg","b") + "copy to *b.*|").
  {&out} fRow("Progress Keywords:|"
   + fRadio("keyword","")  + "No change "
   + fRadio("keyword","u") + "Uppercase "
   + fRadio("keyword","l") + "Lowercase|"
   + "<INPUT TYPE=checkbox NAME=typeout VALUE='ON' " + (IF GET-VALUE("typeout") > "" THEN "CHECKED" ELSE "" ) + ">Typeout").
  {&out} fRow("HTML Keywords:|"
   + fRadio("keyhtml","")  + "No change "
   + fRadio("keyhtml","u") + "Uppercase "
   + fRadio("keyhtml","l") + "Lowercase|").
  {&out} "</table>" SKIP.
end procedure.
