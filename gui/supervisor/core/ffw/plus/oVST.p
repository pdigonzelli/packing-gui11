/*-----------------------------------------------------------------------
File: oVST.p
Purpose: Virtual System Tables Utilitites.
Description:
Author(s) :Per S Digre/PSC
Created: April 1998
Notes:    
Modification History:    
$Header: /cvsroot/freeframework/ffw1.2/ffw/plus/oVST.p,v 1.1 2002/08/21 16:14:25 freeframework Exp $
$Log: oVST.p,v $
Revision 1.1  2002/08/21 16:14:25  freeframework
Initial import

Revision 1.1.1.1  2001/03/23 14:50:42  slichtenberg
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
plusLog("Init VST-object").
plusLog("DB:" + PDBNAME("DICTDB")).

&GLOBAL-DEFINE cColorHeader " bgcolor=#C5AF96 "
&GLOBAL-DEFINE cColorRow1   " bgcolor=#FFFFE6 "
&GLOBAL-DEFINE cBrowseTableDef " border=1 cellpadding=0 cellspacing=0 width=90% "

def new global shared var hHTML as handle no-undo.
FUNCTION fRow        RETURNS CHAR(INPUT cData   AS CHAR) IN hHTML.
FUNCTION fHRow       RETURNS CHAR(INPUT cLabels AS CHAR) IN hHTML.
FUNCTION fBeginTable RETURNS CHAR(INPUT cLabels AS CHAR) IN hHTML.
FUNCTION fTable      RETURNS CHAR(INPUT cLabels AS CHAR
                                 ,INPUT cData AS CHAR) IN hHTML.


/**** generic object definition section  ********/
DEFINE VARIABLE hParent   AS HANDLE NO-UNDO.
DEFINE VARIABLE cCallback AS CHAR   NO-UNDO.

PROCEDURE callback:
  DEFINE INPUT PARAMETER h1 AS handle NO-UNDO.
  DEFINE INPUT PARAMETER c1 AS char   NO-UNDO.
  assign hParent   = h1
         cCallBack = c1.
END PROCEDURE.

function fOut returns char (input c1 as char):
  if c1 = ? then c1 = "".
  return c1.
end function.

def new global shared var hDatabase as handle no-undo.
/**** Data Browse control *****/

DEFINE NEW GLOBAL SHARED VARIABLE cAction   AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cDBid       AS CHARACTER NO-UNDO.
DEFINE new GLOBAl SHARED VARIABLE cType     AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cTable    AS CHARACTER NO-UNDO.
DEFINE STREAM sIni.


function fRecid2table returns char (input iRecid as int):
  DEFINE VARIABLE q1   AS WIDGET-HANDLE.
  DEFINE VARIABLE b1   AS HANDLE.
  DEFINE VARIABLE cRet AS CHAR NO-UNDO init ''.

  CREATE QUERY q1.
  for each dictdb._file no-lock where _file-number > 0 and _file-number < 32000:
    CREATE BUFFER b1 FOR TABLE dictdb._file._file-name.
    q1:SET-BUFFERS(b1).
    q1:QUERY-PREPARE('FOR EACH ' + dictdb._file._file-name + ' where recid(' + dictdb._file._file-name + ') = ' + string(iReciD)).
    q1:QUERY-OPEN().
    q1:GET-NEXT.
    if not q1:QUERY-OFF-END then do:
      DELETE OBJECT b1.
      assign cRet = dictdb._file._file-name .
      leave.
    end.
    DELETE OBJECT b1.
  end.
  DELETE OBJECT q1.
  return cRet.
end function.

FUNCTION fDisp RETURNS CHARACTER (INPUT cLabel AS CHARACTER,INPUT cValue AS CHARACTER):
  RETURN "<TD align=right><b>" + cLabel + ":</b></td><td align=right>" + HTML-ENCODE(cValue) + "</td>".
END FUNCTION.

procedure dbVST:
  DEFINE VARIABLE cIn     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOut    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cRecID  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCmd    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i1      AS INTEGER  NO-UNDO.
  DEFINE VARIABLE cPF     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cNewPF  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cSearch AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iTableLevel AS INTEGER NO-UNDO.
  DEFINE VARIABLE iLevel  AS INTEGER NO-UNDO.
  DEFINE VARIABLE iLine   AS INTEGER NO-UNDO.
  DEFINE VARIABLE lConnected AS LOG NO-UNDO.
  DEFINE VARIABLE lServed AS LOG NO-UNDO.

  ASSIGN cRecid = GET-VALUE("recid").

  CASE cAction:
    WHEN "vstInfo" OR WHEN "" THEN DO:
      FIND FIRST DICTDB._actbuffer NO-ERROR.

{&OUT} '    <TABLE>~n'.
{&OUT} '    <TR>' fDisp("Buffer Reads",STRING(DICTDB._actbuffer._buffer-logicrds)) .
{&OUT} fDisp("OS Reads",STRING(DICTDB._actbuffer._buffer-osrds)) .
{&OUT} fDisp("Hit Rate",STRING(100 * DICTDB._actbuffer._buffer-logicrds  / (DICTDB._actbuffer._buffer-logicrds + DICTDB._actbuffer._buffer-osrds),"99.99")) '</tr>~n'.
{&OUT} '    <TR>' fDisp("Buffer Writes",STRING(DICTDB._actbuffer._buffer-logicwrts)) .
{&OUT} fDisp("OS Writes",STRING(DICTDB._actbuffer._buffer-oswrts)) .
{&OUT} fDisp("Hit Rate",STRING(100 * DICTDB._actbuffer._buffer-logicwrts / (DICTDB._actbuffer._buffer-logicwrts + DICTDB._actbuffer._buffer-oswrts),"99.99")) '</tr>~n'.
{&OUT} '    </TABLE>~n'.


      {&out} fBeginTable('FileName|IO-type|Size|Blocksize|LogicalSize|Extend') SKIP.
      FOR EACH DICTDB._filelist:
        {&out} fRow(_filelist-name
            + '|' + _filelist-openmode
            + '|' + string(_filelist-Size)
            + '|' + string(_filelist-blksize)
            + '|' + string(_filelist-logicalsz)
            + '|' + string(_filelist-extend)) skip.
      END.
      {&out} "</table>" SKIP.

      {&out} fBeginTable('ServerID|Num|Type|PID|Port|Protocol|Logins|Curr|Max|Rec sent|KBytes sent|Rec recvd|Kbytes recvd') SKIP.
      FOR EACH DICTDB._server, EACH DICTDB._actserver OF DICTDB._server:
        {&out} fRow(string(_server._server-id)
            + '|' + string(_server-num)
            + '|' + _server-type
            + '|' + string(_server-pid)
            + '|' + string(_server-portnum)
            + '|' + _server-protocol
            + '|' + string(_server-logins)
            + '|' + string(_server-currusers)
            + '|' + string(_server-maxusers)
            + '|' + string(_server-msgsent)
            + '|' + string(INT(_server-bytesent / 1000))
            + '|' + string(_server-msgrec)
            + '|' + string(int(DICTDB._actserver._server-byterec  / 1000))) skip.
      END.
      {&out} "</table>" SKIP.
    END.

    WHEN "vstUsers" THEN DO:
      {&out} fBeginTable('UserNr|Type|Name|Host|PID|Wait|--|Trans|Sem|Server|Time') SKIP.
      FOR EACH DICTDB._connect WHERE DICTDB._connect._connect-usr <> ?:
        {&out} fRow(fOut(STRING(_connect-usr))
            + '|' + fOut(_connect-type)
            + '|' + fOut(_connect-name)
            + '|' + fOut(_connect-device)
            + '|' + fOut(string(_connect-PID))
            + '|' + fOut(string(_connect-wait1))
            + '|' + fOut(string(_connect-wait))
            + '|' + fOut(string(_connect-transid))
            + '|' + fOut(string(_connect-semnum))
            + '|' + fOut(string(_connect-server))
            + '|' + fOut(string(_connect-time)) )    SKIP.
      END.
      {&out} "</table>" SKIP.
    END.


    WHEN "vstLocks" THEN DO:
      {&out} fBeginTable('Device|PID|Name|User|Recid|File|Type') SKIP.

  def var iRecid as int no-undo.
  def var iUsr as int no-undo.


      FOR EACH DICTDB._Lock NO-LOCK:
        assign iRecid = DICTDB._Lock._Lock-recid
               iUsr   = DICTDB._Lock._Lock-Usr
               .
        IF iRecid = ? or iRecid = 0 THEN LEAVE.

        FIND FIRST DICTDB._Connect NO-LOCK WHERE
               DICTDB._Connect._Connect-Id = iUsr NO-ERROR.


        IF AVAIL DICTDB._Connect
        THEN {&out} "<tr" {&cColorRow1}
                   "><td>"         DICTDB._Connect._Connect-Device
               "</td><td>"         DICTDB._Connect._Connect-Pid.
        ELSE {&out} "<tr" {&cColorRow1}
                   "><td>N/A"
               "</td><td>N/A".

        FIND FIRST DICTDB._File NO-LOCK WHERE
               DICTDB._File._File-number = DICTDB._Lock._Lock-Table NO-ERROR.
        IF AVAIL DICTDB._File
        THEN ASSIGN cFile = DICTDB._File._File-Name.
        ELSE ASSIGN cFile = "N/A".


        {&out}
               "</td><td>"         DICTDB._Lock._Lock-Name
               "</td><td>"         DICTDB._Lock._Lock-Usr
               "</td><td>"         DICTDB._Lock._Lock-recid
               "</td><td><a href=db?Do=edit&tb=" cTable "&db=" cDBid
                         "&recid=" DICTDB._Lock._Lock-recid   ">"
                                   cFile   "</a>"
               "</td><td>"         DICTDB._Lock._Lock-type + " - " + DICTDB._Lock._Lock-Flags
               "</td></tr>" SKIP.
      END.
      {&out} "</table>" SKIP.
    END.
    WHEN "vstRecid" THEN DO:
      assign cFile = ''.
      if cRecid > "" then cFile = fRecid2Table(int(cRecid)).
      IF cFile = ""
      then {&out} " <b>File: Not Found !</b>" skip.
      else {&out} " <b>File: <a href=db?Do=edit&db=" cDBid '&tb=' cTable "&recid=" cRecid ">" cFile "</a>"
        "</b>" SKIP.
    END.
  END CASE.
END procedure.

