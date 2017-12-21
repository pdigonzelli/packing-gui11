/*-----------------------------------------------------------------------
File: dbutil.html
Purpose: Application compiler and library utility.
Description:
Author(s) :PerSDigre/PSC
Created: April 1998
Notes:    
Modification History:    
$Header: /cvsroot/freeframework/ffw1.2/ffw/plus/amdb.p,v 1.1 2002/08/21 16:14:24 freeframework Exp $
$Log: amdb.p,v $
Revision 1.1  2002/08/21 16:14:24  freeframework
Initial import

Revision 1.1.1.1  2001/03/23 14:50:23  slichtenberg
initial load 1.03

Revision 1.1  2000/03/21 06:01:11  wsbroker1
Initial revision

This file contains sample code which may assist you in creating applications.
You may use the code as you see fit. If you modify the code or include it in another software program,
you will refrain from identifying Progress Software as the supplier of the code, or using any
Progress Software trademarks in connection with your use of the code.
THE CODE IS NOT SUPPORTED BY PROGRESS SOFTWARE AND IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
INCLUDING, WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR NONINFRINGEMENT.
-----------------------------------------------------------------------*/

DEFINE STREAM s1.
DEFINE STREAM s2.

DEFINE VARIABLE cDBname        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDBini         AS CHARACTER NO-UNDO.

DEFINE VARIABLE cPFvalue       AS CHARACTER NO-UNDO. /** content of *.pf file **/
DEFINE VARIABLE cPF            AS CHARACTER NO-UNDO. /** Location of *.pf file **/

DEFINE VARIABLE lServed        AS LOG NO-UNDO.
DEFINE VARIABLE lConnected     AS LOG NO-UNDO.  /*** Auto connections ***/
DEFINE VARIABLE lPrevConnected AS LOG NO-UNDO.  /*** Auto connections ***/


/************ Generic code ***********************/
{plus/plus.i}
ASSIGN
  cFormTitle   = "Database Utilities"
  cFormTarget  = "amdb"
  cFormBack    = "window.open('am.p','_self')"
  cFormHelp    = "amdb"
  cFormRefresh = "document.form.submit()"
  .

/***** Table formatting ****************************/
DEFINE VARIABLE iTableLevel AS INTEGER NO-UNDO.
DEFINE VARIABLE iLevel     AS INTEGER NO-UNDO.
DEFINE VARIABLE iLine      AS INTEGER NO-UNDO.

FUNCTION fDisp RETURNS CHARACTER(INPUT c1 AS CHARACTER,INPUT lHeader AS LOG,INPUT cFormat AS CHARACTER):
  DEFINE VARIABLE i1         AS INTEGER  NO-UNDO.
  IF iTableLevel > iLevel AND iTableLevel > 0 THEN {&out} "</td></tr>" SKIP.
  IF iTablelevel > iLevel                     THEN {&out} "</table><pre>" SKIP.
  IF iTableLevel < iLevel AND iLevel > 0      THEN {&out} "<tr" {&cColorRow1} "><td>".
  IF iTablelevel < iLevel                     THEN {&out} "</pre><table>".
  IF iLevel > 0 THEN DO:
    {&out} IF lHeader THEN ("<tr" + {&cColorHeader} + ">") ELSE ("<tr" + {&cColorRow1} + ">").
    DO i1 = 1 TO NUM-ENTRIES(cFormat):
      IF lHeader
      THEN {&out} "<th><nobr>" TRIM(SUBSTRING(c1,1,INTEGER(ENTRY(i1,cFormat)))) "</nobr></th>".
      ELSE {&out} "<td><nobr>" TRIM(SUBSTRING(c1,1,INTEGER(ENTRY(i1,cFormat)))) "</nobr></td>".
      ASSIGN SUBSTRING(c1,1,INTEGER(ENTRY(i1,cFormat))) = "".
    END.
    {&out} "</tr>" SKIP.
  END.
  ELSE {&out} c1 SKIP.
  ASSIGN iTableLevel = iLevel.
END FUNCTION.

PROCEDURE formatOutput:
  DEFINE INPUT PARAMETER c1 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i1        AS INTEGER  NO-UNDO.
  CASE cAction:
    WHEN "ixanalys" THEN DO:
      IF c1 MATCHES "*-----*" THEN RETURN.
      IF c1 MATCHES "*=====*" THEN RETURN.
      IF TRIM(c1) = "" THEN RETURN.
      fDisp(c1,NOT c1 MATCHES " *","24,8,8,7,8,8,9,7,100").
      IF c1 MATCHES "* INDEX BLOCK SUMMARY *" THEN iLevel = iLevel + 1.
      IF c1 MATCHES "*Totals:*" THEN iLevel = iLevel - 1.
    END.
    WHEN "tabanalys" THEN DO:
      IF TRIM(c1) = "" THEN RETURN.
      fDisp(c1,c1 MATCHES " *" OR c1 BEGINS "Table ","20,8,10,6,6,7,9,8,7,100").
      IF c1 MATCHES "* RECORD BLOCK SUMMARY *" THEN iLevel = iLevel + 1.
      IF c1 MATCHES "*Totals:*" THEN iLevel = iLevel - 1.
    END.
    OTHERWISE DO:
      IF TRIM(c1) = "" THEN RETURN.
      fDisp(c1,FALSE,"").
    END.
  END CASE.
END PROCEDURE.

PROCEDURE formatEditor:
  DEFINE INPUT PARAMETER c1 AS CHARACTER NO-UNDO.
  {&out} c1 SKIP.
END PROCEDURE.



/******* Other stuff *******************************/
PROCEDURE trimv9:
  DEFINE VARIABLE i1            AS INTEGER  NO-UNDO.
  DEFINE VARIABLE c1            AS CHAR     NO-UNDO.
  DEFINE VARIABLE c2            AS CHAR     NO-UNDO.
  DEFINE VARIABLE cFile         AS CHAR     NO-UNDO.
  DEFINE INPUT PARAM cDir       AS CHAR     NO-UNDO.

  ASSIGN cDir = SUBSTRING(cDir,1,R-INDEX(cDir,"/") - 1).
  {&out} "Trimming Directory: " cDir "<br>" SKIP.
  INPUT STREAM s1 FROM OS-DIR(cDir).
  REPEAT:
    IMPORT STREAM s1 cFile.
    IF cFile MATCHES "*.df" THEN DO:
      {&out} "Processing Definitions: " cFile "<br>" SKIP.
      INPUT  STREAM s1 FROM VALUE(cDir + "/" + cFile).
      OUTPUT STREAM s2 TO   VALUE(cDir + "/temp.df").
      REPEAT:
        ASSIGN c1 = "".
        IMPORT STREAM s1 UNFORMATTED c1.
        IF c1 MATCHES "* area *"
        OR c1 MATCHES "* SQL-WIDTH *"
        THEN NEXT.
        PUT STREAM s2 UNFORMATTED c1 SKIP.
      END.
      INPUT  STREAM s1 CLOSE.
      OUTPUT STREAM s2 CLOSE.
      OS-DELETE VALUE(cDir + "/" + cFile).
      OS-RENAME VALUE(cDir + "/temp.df") VALUE(cDir + "/" + cFile).
    END.
    IF cFile MATCHES "*.d" THEN DO:
      {&out} "Processing Definitions: " cFile "<br>" SKIP.
      INPUT  STREAM s1 FROM VALUE(cDir + "/" + cFile).
      OUTPUT STREAM s2 TO   VALUE(cDir + "/temp.d").
      REPEAT:
        ASSIGN c1 = "".
        IMPORT STREAM s1 UNFORMATTED c1.
        IF c1 MATCHES "*numformat=44,46*"
        THEN c1 = "numformat=.".
        PUT STREAM s2 UNFORMATTED c1 SKIP.
      END.
      INPUT  STREAM s1 CLOSE.
      OUTPUT STREAM s2 CLOSE.
      OS-DELETE VALUE(cDir + "/" + cFile).
      OS-RENAME VALUE(cDir + "/temp.d") VALUE(cDir + "/" + cFile).
    END.
  END.
  INPUT STREAM s1 CLOSE.
END PROCEDURE.




PROCEDURE amDBinit:
  DEFINE VARIABLE i1         AS INTEGER  NO-UNDO.
  DEFINE VARIABLE c1         AS CHARACTER NO-UNDO.

  ASSIGN
    cDBini  = ENTRY(INT(GET-VALUE("db")),cDB)   /** Defaulting to Unix file syntax (not in URL) **/
    cPF     = REPLACE(cDBini,".db","") + ".pf".

  OUTPUT STREAM s1 TO VALUE(cPF) APPEND.        /**** Create a *.pf file if not there ***/
  OUTPUT STREAM s1 CLOSE.

  IF GET-VALUE("pf") > "" OR cAction = "save"
  THEN DO:
    cPFvalue = GET-VALUE("pf").
    IF cAction = "save" THEN DO:                /**** Save the *.pf file ***/
      OUTPUT STREAM s1 TO VALUE(cPF).
      PUT STREAM s1 UNFORMATTED cPFvalue SKIP.
      OUTPUT STREAM s1 CLOSE.
    END.
  END.
  ELSE DO:
    INPUT STREAM s1 FROM VALUE(cPF).            /**** Read the *.pf file ***/
    REPEAT:
      ASSIGN c1 = ''.
      IMPORT STREAM s1 UNFORMATTED c1.
      ASSIGN cPFvalue = cPFvalue + c1 + " ".
    END.
    INPUT STREAM s1 CLOSE.
    ASSIGN cPFvalue = TRIM(REPLACE(REPLACE(cPFvalue,"    "," "),"  "," ")).
    IF LOOKUP("-ld",cPFvalue," ") > 0
    THEN cDBName = ENTRY(LOOKUP("-ld",cPFvalue," ") + 1,cPFvalue," ").
    ELSE cDBName = ENTRY(1,ENTRY(NUM-ENTRIES(cDBini,"/"),cDBini,"/"),".").
  END.
END PROCEDURE.


RUN amDBinit.
ASSIGN lPrevConnected   = CONNECTED(cDBName).

ASSIGN lConnected = CONNECTED(cDBName)
       lServed    = SEARCH(REPLACE(cDBini,".db","") + ".lk") > "".


IF get-value("frame") > "" OR get-value("do") = "" THEN DO:
  fHeader().
  {&out}  "<br>action:" get-value("do").
  RUN amDBview.
  RUN pFrame("amdb","db,do,format,return").
  RUN amDBframes.
  fFooter().
END.
ELSE
  RUN pFrame("amdb","db,do,format,return").




PROCEDURE DBcmd:
  DEFINE INPUT PARAM cTempfile AS CHAR NO-UNDO.
  DEFINE VARIABLE cCmd       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDBini     AS CHARACTER NO-UNDO.
  ASSIGN
    cDBini  = ENTRY(INT(GET-VALUE("db")),cDB).   /** Finding the database **/
  CASE get-value("do"):
    WHEN "serve"      THEN cCmd = "proserve " + cDBini + " -pf " + cPF.
    WHEN "shut"       THEN cCmd = "proshut " + cDBini + " -by -pf " + cPF.
    WHEN "sharedmem"  THEN cCmd = "proutil " + cDBini + " -C DBIPCS".
    WHEN "truncate"   THEN cCmd = "proutil " + cDBini + " -C truncate BI".
    WHEN "iostats"    THEN cCmd = "proutil " + cDBini + " -C iostats".
    WHEN "holder"     THEN cCmd = "proutil " + cDBini + " -C holder".
    WHEN "busy"       THEN cCmd = "proutil " + cDBini + " -C busy".
    WHEN "dellog"     THEN cCmd = "prolog  " + cDBini.
    WHEN "idxbuild"   THEN cCmd = "proutil " + cDBini + " -C idxbuild all".
    WHEN "ixanalys"   THEN cCmd = "proutil " + cDBini + " -C ixanalys".
    WHEN "tabanalys"  THEN cCmd = "proutil " + cDBini + " -C tabanalys".
    WHEN "idxcheck"   THEN cCmd = "proutil " + cDBini + " -C idxcheck all".
    WHEN "backup"     THEN cCmd = "probkup online " + cDBini + " " + cDBini + ".bkup -com -verbose".
    WHEN "incremental" THEN cCmd = "probkup online " + cDBini + " incremental " + cDBini + ".ibkup -com -verbose".
    WHEN "structstat" THEN cCmd = "prostrct statistics " + cDBini.
    WHEN "disconnect" THEN DISCONNECT VALUE(cDBname) NO-ERROR.
    WHEN "connect"    THEN CONNECT VALUE(cDBini) -pf VALUE(cPF) NO-ERROR.
    WHEN "trimV9"     THEN RUN trimV9(cDBini).
  END CASE.

  DEFINE VARIABLE cIn       AS CHARACTER NO-UNDO.
  OUTPUT STREAM s2 TO VALUE(cTempfile).
  PUT STREAM s2 UNFORMATTED "<b>" cCmd "</b>" SKIP.
  OUTPUT STREAM s2 CLOSE.

  IF cCmd > "" THEN DO:
    INPUT STREAM s1 THROUGH VALUE(cCmd).
    REPEAT:
      IMPORT STREAM s1 UNFORMATTED cIn.
      OUTPUT STREAM s2 TO VALUE(cTempfile) APPEND.
      PUT STREAM s2 UNFORMATTED cIn SKIP.
      OUTPUT STREAM s2 CLOSE.
    END.
    INPUT STREAM s1 CLOSE.
  END.

  OUTPUT STREAM s2 TO VALUE(cTempfile) APPEND.
  PUT STREAM s2 UNFORMATTED SKIP(1) "ReportIsFinished" SKIP(1).
  OUTPUT STREAM s2 CLOSE.
  RETURN cCmd.

END PROCEDURE.



PROCEDURE amDBview:
  /*  <br>`cDBini`  <br>`cDBname`  <br>`cPF` */
  {&OUT} '  <nobr>' cPF '<input TYPE=text NAME=pf SIZE=40 VALUE='''  cPFvalue '''>~n'
         '   <INPUT TYPE=hidden NAME=db VALUE="'  GET-VALUE("db")  '">~n'
         '   <INPUT TYPE=button NAME=save VALUE=Save onClick="document.form.Do.value=''Save''~;document.form.submit()">~n'
         '  </nobr>~n'
         '  <br> ' IF lServed THEN cDBName + ' currently ' + (IF CONNECTED(cDBname) THEN '' ELSE 'not ') + 'connected.' ELSE ''  .
  {&out} fBeginTable("Type|Options").
  {&out} fRow("Manage|"
       + (IF lServed
    THEN fLink("Shut","","Shutdown")
       + fLink("DisConnect","","DisConnect")
       + fLink("Connect","","Connect")
    ELSE fLink("Serve","","Serve")) ).
  {&out} fRow("Maintain|"
       + (IF lServed
    THEN fLink("Backup","","Backup")
       + fLink("Incremental","","Incremental")
    ELSE fLink("idxbuild","","Index Rebuild")
       + fLink("Truncate","","Truncate BI")
       + fLink("dellog","","Truncate Log"))).
  IF lServed
  THEN {&out}
         fRow("Online info|"
       + (IF opsys<>'win32' THEN fLink("sharedmem","","Shared Mem") ELSE '')
       + fLink("ixanalys","","Index Blocks")
       + fLink("tabanalys","","Record Blocks")).
  ELSE {&out}
         fRow("Offline info|"
       + fLink("idxcheck","","Index Check")
       + fLink("chanalys","","Chain Blocks")
       + fLink("structstat","","Storage Util")
       + fLink("iostats","","IO Stats")).
  {&out}
         fRow("Other|"
       + fLink("trimV9","","Trim V9 Dumpdata to V8-loadable.")).
  {&out} "</TABLE>".

  ASSIGN iLevel = 0.
  IF GET-VALUE("temp") > "" THEN DO:
    CASE GET-VALUE("Execute"):
      WHEN "STATUS" THEN {&out} "<b>Executing: " + STRING(TIME - INT(get-value('time')),"HH:MM:SS") + "</b>".
      WHEN "FINAL"  THEN {&out} "<b>Finished:</b>".
      OTHERWISE          {&out} "<b>Start:</b>".
    END CASE.
    IF CAN-DO("ixanalys,tabanalys",cAction)         /*** Show status window ***/
    THEN set-user-field("format","formatOutput").     /*** use formatOutput as formatting procedure ****/
    ELSE set-user-field("format","formatEditor").    /*** use a TEXTAREA box for output ****/
  END.
END PROCEDURE.



/******* Automatic connections *************************/

PROCEDURE amDBframes:
  /**** Start database ****/
  IF (GET-VALUE("frame") = "end" AND cAction = "serve") AND GET-VALUE("return") > "" THEN DO:
    IF lServed THEN DO:
      {&OUT} '<H2> Start connecting </H2>~n'
             '<SCRIPT LANGUAGE="Javascript">~n'
             '  window.open("amdb.p?return='  GET-VALUE("return")  '&db='  get-value('db')  '&Do=connect","_self")~;~n'
             '</SCRIPT>~n'.
    END.
    ELSE {&out} "<H2> Cannot start dataserver. </H2>" SKIP.
  END.

  /**** Continue connecting agents ****/
  IF (cAction = "connect" AND (NOT lPrevConnected)) AND GET-VALUE("return") > "" THEN DO:
    IF lConnected THEN DO:
      {&OUT} '<H2> Continue connecting </H2>~n'
             '<SCRIPT LANGUAGE="Javascript">~n'
             '  window.open("amdb.p?return='  GET-VALUE("return")  '&DB='  cDBini  '&Do=connect","_self")~;~n'
             '</SCRIPT>~n'.
    END.
    ELSE {&out} "<H2> Cannot connect to dataserver. </H2>" SKIP.
  END.

  /**** Finished connecting *****/
  IF ((cAction = "connect" AND lPrevConnected) OR (cAction = "shut" AND GET-VALUE("frame") = "end")) AND GET-VALUE("return") > "" THEN DO:
    {&OUT} '<SCRIPT LANGUAGE="Javascript">~n'
           '  window.open("'  GET-VALUE("return")  '","_self")~;~n'
           '</SCRIPT>~n'.
  END.
END PROCEDURE.


PROCEDURE pFrame:
  DEF INPUT PARAM cProg  AS CHAR NO-UNDO.
  DEF INPUT PARAM cLink  AS CHAR NO-UNDO.
  DEF VAR         cFrame AS CHAR NO-UNDO.
  DEF VAR         cIn    AS CHAR NO-UNDO.

  DEFINE VARIABLE lFinished AS LOG  NO-UNDO.

  IF get-value('time') = '' THEN set-user-field("time",STRING(TIME)).
  IF get-value('temp') = '' THEN set-user-field("temp",STRING(RANDOM(100000,999999)) + ".tmp").

  ASSIGN
    cLink  = cProg + ".p?" + url-field-list("temp,time," + cLink,"&") + "&frame="
    cFrame = get-value("frame").

  CASE cFrame:
    WHEN "exec" THEN DO:
      RUN DBcmd(get-value("TEMP")).
      {&OUT}
        '<HTML><HEAD><TITLE>Processing</TITLE></HEAD><BODY>~n'
        '<SCRIPT LANGUAGE="JavaScript"> ~n'
        "window.open('" + cLink + "end','_parent') ~n"
        '</SCRIPT>~n'
        '</BODY></HTML>~n'.
    END.

    WHEN "stat" OR WHEN "end" THEN DO:
      {&out} "<br>Time elapsed:" + STRING(TIME - INT(get-value('time')),"HH:MM:SS") "<br>" SKIP.
      {&out} "<pre>".
      IF SEARCH(get-value("TEMP")) > "" THEN DO:
        INPUT STREAM s2 FROM VALUE(get-value("TEMP")).
        REPEAT:
          IMPORT STREAM s2 UNFORMATTED cIn.
          IF cIn BEGINS "ReportIsFinished"
          THEN lFinished = TRUE.
          ELSE RUN VALUE(get-value("format")) (cIn).
        END.
        INPUT STREAM s2 CLOSE.
      END.
      ELSE lFinished = TRUE.
      {&out} "</pre>".

      IF cFrame = "end"
      THEN OS-DELETE VALUE(get-value("TEMP")).
      ELSE DO:

        {&OUT} '<SCRIPT LANGUAGE="JavaScript">    ~n'.
        IF lFinished THEN DO:
          {&OUT}  "window.open('" + cLink + "end','_parent') ~n".
        END.
        ELSE DO:
          {&OUT}
            'var timerID = null  ~n'
             'timerID = setTimeout("RefreshConfirm()",3000)  ~n'
             'function RefreshConfirm() ~{  ~n'
             '  clearTimeout(timerID)  ~n'
             "   window.open('" cLink  + "stat~',~'stat~') ~n"
             '~}  ~n'.
        END.
        {&OUT} '</SCRIPT>~n'
               '</BODY></HTML>~n'.
      END.
    END.

    OTHERWISE DO:
      {&OUT}
       '<HTML><HEAD><TITLE>Start processing</TITLE></HEAD>~n'
       ' <FRAMESET ROWS="*,0">~n'
       '  <FRAME NAME="stat" SRC="'  cLink 'stat"~n'
       '         FRAMEBORDER=yes MARGINHEIGHT=15 MARGINWIDTH=10>~n'
       '  <FRAME NAME="exec" SRC="'  cLink 'exec" SCROLLING=no~n'
       '         FRAMEBORDER=yes MARGINHEIGHT=0 MARGINWIDTH=0>~n'
       ' </FRAMESET>~n'
       '<NOFRAME></NOFRAME></HTML>~n'.
    END.
  END CASE.

END PROCEDURE.

