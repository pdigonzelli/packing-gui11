/*-----------------------------------------------------------------------
     File: 	db.html
     Description:main menu for file-maintenance gives a list of
                 database-tables to browse and edit.
     Created:	10/10-2000
     Author:	Per S Digre/ PSC
     Modification History:

This file contains sample code which may assist you in creating applications.
You may use the code as you see fit. If you modify the code or include it in another software program,
you will refrain from identifying Progress Software as the supplier of the code, or using any
Progress Software trademarks in connection with your use of the code.
THE CODE IS NOT SUPPORTED BY PROGRESS SOFTWARE AND IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
INCLUDING, WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR NONINFRINGEMENT.
-----------------------------------------------------------------------*/
{plus/plus.i}

DEFINE NEW GLOBAL SHARED VARIABLE cDictdb      AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hDatabase    AS HANDLE    NO-UNDO.   /**** Object handle for database func ****/
DEFINE NEW GLOBAL SHARED VARIABLE hVST         AS HANDLE    NO-UNDO.   /**** Object handle for database func ****/
DEFINE NEW GLOBAL SHARED VARIABLE cDBid        AS CHARACTER NO-UNDO.   /**** Generic DB control *****/
DEFINE NEW GLOBAL SHARED VARIABLE cTable       AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cFile        AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cRowID       AS CHARACTER NO-UNDO.   /**** Edit Record control *****/
DEFINE NEW GLOBAL SHARED VARIABLE iCount       AS INTEGER   NO-UNDO.   /**** Data Browse control *****/
DEFINE NEW GLOBAL SHARED VARIABLE iCurrent     AS INTEGER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cIndex       AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cCountString AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cWhere       AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cFirst       AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cLast        AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cType        AS CHARACTER NO-UNDO.   /**** Virtual System Tables ****/
DEFINE NEW GLOBAL SHARED VARIABLE lVST         AS LOG       NO-UNDO.



ASSIGN
    cType    = IF GET-VALUE('type') > "" THEN GET-VALUE('type') ELSE "std"
    cTable   = GET-VALUE("tb")
    cDBid    = GET-VALUE("db")
    cRowID   = get-value("rowid")
    iCount   = INT(get-value("count"))
    iCurrent = INT(get-value("current"))
    cIndex   = get-value("index")
    cWhere   = get-value("where")
    cFirst   = get-value("first")
    cLast    = get-value("last")
    cFile    = cDBid + '.' + cTable
     .

ASSIGN
  cFormTitle   = 'Database List'
  cFormTarget  = "db"
  cFormBack    = "window.open('db.p','_self')"
  cFormHelp    = "db"
  cFormRefresh = "document.form.submit()"
  .

IF cDBid > "" THEN DO:
  plusLog("DB:" + cDBid).
  CREATE ALIAS DICTDB FOR DATABASE VALUE(cDBid).
  IF VALID-HANDLE(hDatabase) /* and cDBid <> cDictDb */
  THEN DELETE PROCEDURE hDatabase.
  IF NOT VALID-HANDLE(hDatabase)
  THEN RUN plus/oDatabase.p PERSISTENT SET hDatabase.

  IF (cRowid > '' AND CAN-DO('delete,save',cAction)) OR cAction = "add" THEN DO:
    RUN processRecord IN hDatabase.
    IF cAction = 'save' AND NOT available-messages(?)
    THEN cRowID = ''.
    ELSE cAction = 'edit'.
  END.

  IF cRowid > '' THEN DO:                                       /**** record detail ****/
    ASSIGN
      cFormTitle  = 'Record Detail'
      cFormHelp   = "dbdt".
    fHeader().


{&OUT} '<P>Record: <b>'  html-encode(cDBid + '.' + cTable) + '</b>  (RowID=' + cRowid + ')'  '</p>~n'
       '<TABLE><TD><TABLE><TR><TD><TABLE>~n'.


    IF available-messages(?)    /*** display all error-messages ***/
    THEN {&out} "<br><P><strong><font color=red>" output-messages("all",?,"Error-Messages") "</strong></font></P>" SKIP.
    RUN displayRecord IN hDatabase.


{&OUT} ' </TABLE></TD><TD VALIGN=top>~n'
       ' <INPUT TYPE=button NAME=Save   VALUE='' Save '' onClick="document.form.Do.value=''save''~;document.form.submit()~;"><BR>~n'
       ' <INPUT TYPE=button NAME=Delete VALUE=''Delete'' onClick="if(confirm(''Are you sure you want to delete this record''))~{document.form.Do.value=''delete''~;document.form.submit()~;~}~;"><BR><BR>~n'
       ' </p></td></tr></table><td></table>~n'
       hidden-field-list("i1,w2,w3,w4,index,count,RowID,db,tb,current,first,last")
       .
    fFooter().
  END.

  IF cTable > '' AND cRowID = '' THEN DO:                      /**** record browse ****/
    ASSIGN
      cFormTitle  = 'Record Browse'
      cFormHelp   = "dbbr".
    fHeader().


{&OUT} '<SCRIPT LANGUAGE=JavaScript>~n'
       'function fEdit(rowid)~{~n'
       '  document.form.RowID.value = rowid~;~n'
       '  document.form.Do.value = ''Edit''~;~n'
       '  document.form.submit()~;~n'
       '~}~n'
       '</SCRIPT>~n'.


      RUN dbBrowse IN hDatabase.

set-user-field("RowID","").

{&OUT} ' </TD></TABLE>~n'
       '<input type=button name=first   value="&lt~;&lt~;" onClick="document.form.Do.value=''first''~;document.form.submit()~;">~n'
       '<input type=button name=prev    value="&lt~;"      onClick="document.form.Do.value=''prev''~; document.form.submit()~;">~n'
       '<input type=button name=next    value="&gt~;"      onClick="document.form.Do.value=''next''~; document.form.submit()~;">~n'
       '<input type=button name=last    value="&gt~;&gt~;" onClick="document.form.Do.value=''last''~; document.form.submit()~;"> &nbsp~;~n'
       IF cCountString > '' THEN cCountstring ELSE (
       '<input type=button name=count   value="Count"      onClick="document.form.Do.value=''count''~;document.form.submit()~;"> &nbsp~;~n')

       '<input type=button name=add     value="Add"        onClick="document.form.Do.value=''add'' ~; document.form.submit()~;"><br>~n'
       '<input type=hidden name=db      value='''  cDBid   '''>~n'
       '<input type=hidden name=tb      value='''  cTable  '''>~n'
       '<input type=hidden name=RowID   value=''            ''>~n'
       '<input type=hidden name=count   value='    iCount    '>~n'
       '<input type=hidden name=current value='    iCurrent  '>~n'
       '<input type=hidden name=first   value='''  cFirst  '''>~n'
       '<input type=hidden name=last    value='''  cLast   '''>~n'.

      fFooter().
  END.
  IF cTable = '' AND cRowID = '' THEN CASE cAction:


    WHEN "rep" OR WHEN "repall" THEN DO:
      ASSIGN
        cFormTitle  = 'Dictionary Report'
        cFormHelp   = "dbrep".
      fHeader().
      RUN dbReport IN hDatabase.
      {&out} "</TABLE>".
      fFooter().
    END.


    WHEN "vstinfo" OR WHEN "vstusers" OR WHEN 'vstlocks' OR WHEN 'vstrecid'  THEN DO:
      ASSIGN
        cFormTitle  = 'Promon Utility'
        cFormHelp   = "dbvst".
      fHeader().
{&OUT} '<P>Database: '  cDBid  ' . .~n'
       '<INPUT TYPE=button NAME=binfo   VALUE="Info"        ONCLICK="document.form.Do.value=''vstinfo''~;document.form.submit()">~n'
       '<INPUT TYPE=button NAME=busers  VALUE="Connections" ONCLICK="document.form.Do.value=''vstusers''~;document.form.submit()">~n'
       '<INPUT TYPE=button NAME=blocks  VALUE="Locks"       ONCLICK="document.form.Do.value=''vstlocks''~;document.form.submit()">~n'
       ' &nbsp~; &nbsp~; <b>RecID:</b><input type=input name=recid value='''   get-value('recid')  ''' size=10>~n'
       '<INPUT TYPE=button NAME=brecid  VALUE="Find"        ONCLICK="document.form.Do.value=''vstrecid''~;document.form.submit()"></P>~n'
       '<INPUT TYPE=hidden NAME=db      VALUE="'  cDBid   '">~n'
       '<INPUT TYPE=hidden NAME=tb      VALUE="'  cTable  '">~n'.
  IF VALID-HANDLE(hVST) THEN DELETE PROCEDURE hVST.
  RUN plus/oVST.p PERSISTENT SET hVST.
      RUN dbVST IN hVST.
      fFooter().
    END.
    OTHERWISE DO:
      fHeader().
      RUN tableList IN hDatabase.
      {&out} '<INPUT TYPE=hidden NAME=db      VALUE="'  cDBid   '">~n'.
      {&out} '<INPUT TYPE=button NAME=saveexit  VALUE="Dictionary" onClick="document.form.Do.value=~'rep~';document.form.submit();">'.
      fFooter().
    end.
  END CASE.
  DELETE PROCEDURE hDatabase.
  RETURN.
END.


DO:  /**** List of connected databases *****/
  DEFINE VARIABLE i1 AS INTEGER NO-UNDO.
  fHeader().
  {&out} fBeginTable("Database|Srv|Type|Ver|Rstr|Location|View|Options").
  DO i1 = 1 TO NUM-DBS:
     cDBid = LDBNAME(i1).
     {&out} fRow(cDBid
         + "|" + SDBNAME(i1)
         + "|" + DBType(i1)
         + "|" + DBVersion(i1)
         + "|" + DBRestrictions(i1)
         + "|" + PDBNAME(i1)
         + '|<a href="db.p?db=' + cDBid + '&type=std">Tables</a>
             <a href="db.p?db=' + cDBid + '&type=sys">System</a>'
         + '|<a href="db.p?db=' + cDBid + '&do=repall&type=std">Dictionary</a>
             <a href="db.p?db=' + cDBid + '&do=vstinfo">Status</a>'   ).
  END.
  {&out} "</table>" SKIP.

  {&OUT} '<input type=hidden name=db    value="' cDBid    '">~n'
         '<input type=hidden name=tb    value="' cTable '">~n'.
  IF NUM-DBS = 0 THEN {&out} "<P>No Databases Connected</p>" skip.
  fFooter().
end.
