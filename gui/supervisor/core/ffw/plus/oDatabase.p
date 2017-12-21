/*-----------------------------------------------------------------------
File: oDatabase.p
Purpose: Database browse tools library.
Description:
Author(s) :PerSDigre/PSC
Created: April 1998
Notes:    
Modification History:    
$Header: /cvsroot/freeframework/ffw1.2/ffw/plus/oDatabase.p,v 1.1 2002/08/21 16:14:25 freeframework Exp $
$Log: oDatabase.p,v $
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
plusLog("Init Database-object.").

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

def new global shared var hDatabase as handle no-undo.
/**** Data Browse control *****/

DEFINE NEW GLOBAL SHARED VARIABLE cDictdb   AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cAction   AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cIndex    AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cWhere    AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cCountString AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cFirst    AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cLast     AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cDBid     AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cTable    AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cRowID    AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cFile     AS CHARACTER NO-UNDO.
DEFINE new GLOBAl SHARED VARIABLE cType     AS CHARACTER NO-UNDO.
DEFINE new GLOBAl SHARED VARIABLE lVST      AS LOG       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE iCurrent    AS integer   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE iCount      AS integer   NO-UNDO.
DEFINE STREAM sIni.

DEFINE var lWordIdx as logical no-undo init false.

assign cDictDb = LDBNAME("DICTDB").
plusLog("DB:" + cDictDb).


procedure tableList.
  def var c1 as char no-undo.
  FIND FIRST DICTDB._file WHERE DICTDB._file._file-name = "_connect" NO-ERROR.
  ASSIGN lVST = AVAIL DICTDB._file.

  define query qList for DICTDB._file FIELDS (DICTDB._file._frozen DICTDB._file._Desc DICTDB._file._file-name).
  CASE cType:
    WHEN "std" THEN OPEN QUERY qList PRESELECT EACH DICTDB._file no-lock WHERE not _frozen.
    WHEN "sys" THEN OPEN QUERY qList PRESELECT EACH DICTDB._file no-lock WHERE _frozen.
  END CASE.

  {&out} fBeginTable("Table|Description").
  get first qList.
  do while available DICTDB._file:
    assign c1 = DICTDB._file._file-name.
    {&out} fRow('<INPUT TYPE=checkbox NAME="F' + c1
        + (IF GET-VALUE("F" + c1) = "" THEN '" ' ELSE '" checked') + ">"
        + '<a href="db.p?db=' + cDBid + '&tb=' + DICTDB._file._file-name + '">' + DICTDB._file._file-name + '</a>'
      + '|' + if DICTDB._file._Desc = ? then "?" else DICTDB._file._Desc
      ).
    get next qList.
  end.
  {&out}
    fRow('<INPUT TYPE=checkbox NAME=seq' + (IF GET-VALUE("seq") = "" THEN ' ' ELSE ' checked') + '> # Sequences|')
    fRow('<INPUT TYPE=checkbox NAME=tri' + (IF GET-VALUE("tri") = "" THEN ' ' ELSE ' checked') + '> # Triggers|')
        + "</table>".
  RETURN "OK".
END PROCEDURE.


function fOut returns char (input c1 as char):
  if c1 = ? then c1 = "".
  return c1.
end function.

procedure dbReport.
  def var c1 as char no-undo.

  define query qList for DICTDB._file /* FIELDS (DICTDB._file._frozen DICTDB._file._Desc DICTDB._file._file-name) */.
  CASE cType:
    WHEN "std" THEN OPEN QUERY qList PRESELECT EACH DICTDB._file no-lock WHERE not _frozen  BY _file-name.
    WHEN "sys" THEN OPEN QUERY qList PRESELECT EACH DICTDB._file no-lock WHERE     _frozen  BY _file-name.
  END CASE.
  get first qList.
  do while available DICTDB._file:
    IF cAction = "rep" and GET-VALUE("F" + DICTDB._file._file-name) = '' then do:
      get next qList.
      next.
    end.
    {&out} '<H2>' cDBid '.' DICTDB._file._file-name '</h2><P>' DICTDB._file._desc '</p>' skip.
    {&out} fBeginTable('Field Name|Type|Format|Label|Col Label').
    FOR EACH DICTDB._field NO-LOCK OF DICTDB._file BY DICTDB._field._field-name:
      {&out} fRow(_field._field-name + '|' + _field._data-type + '|' + _field._format + '|' + fOut(_field._Label) + '|' + fOut(_field._col-label)).
    END.
    {&out} "</TABLE>" SKIP.

    {&out} fBeginTable('Index Name|Seq|Index Field|Flags|Desc').
    FOR EACH DICTDB._index NO-LOCK OF DICTDB._file BY _index-name:
      {&out} '<TR ' {&cColorRow1} '><TD>' _index._index-name '</td><TD COLSPAN=2><TABLE>' skip.
      FOR EACH DICTDB._index-field NO-LOCK OF _index, FIRST DICTDB._field WHERE RECID(_field) = _index-field._field-recid BY _index-seq:
        {&out} '<TR ' {&cColorRow1} '><TD> ' _index-seq '</TD><TD>' _field._field-name '</td></TR>' skip.
      END.
      {&out} '</TABLE></TD><TD>' skip.
      {&out} (IF _unique       THEN " Unique"  ELSE "")
             (IF _file._prime-index = RECID(_index)  THEN " Primary"  ELSE "").
      {&out} (IF _active       THEN " Active"  ELSE "")
             (IF _wordidx <> ? THEN " WordIdx" ELSE "").
      {&out} '</td><TD>&nbsp;' fOut(_index._desc) '</td></TR>' skip.
    END.
    {&out} '</TABLE><BR CLASS="break">' SKIP.
    get next qList.
  END.
  CLOSE QUERY qList.

  IF cAction = "repall" or GET-VALUE("seq") > '' then do:
    {&out} '<H2>' cDBid '.SEQUENCES:</h2>' skip.
    {&out} fBeginTable('Sequence Name|Number|Min|Max').
    FOR EACH DICTDB._Sequence NO-LOCK BY _Seq-Name:
      {&out} fRow(_Seq-Name + '|' + string(_Seq-Num) + '|' + string(_Seq-Min) + '|' + if _Seq-Max <> ? then string(_Seq-Max) else "?").
    END.
    {&out} '</TABLE>' SKIP.
  end.

  IF cAction = "repall" or GET-VALUE("tri") > '' then do:
    {&out} '<H2>' cDBid '.TRIGGERS:</h2>' skip.
    {&out} fBeginTable('Trigger|Event|Procedure').
    FOR EACH DICTDB._File-Trig NO-LOCK, EACH DICTDB._File OF DICTDB._File-Trig BY _File-Name:
      {&out} fRow(_File-Name + '|' + _File-Trig._Event + '|' + _File-Trig._Proc-Name).
    END.
    FOR EACH DICTDB._Field-Trig NO-LOCK, EACH DICTDB._Field OF DICTDB._Field-Trig, EACH DICTDB._File OF DICTDB._Field BY _File-Name BY _Field-Name :
      {&out} fRow(_File-Name + '.' + _Field-Name + '|' + _Field-Trig._Event + '|' + _Field-Trig._Proc-Name).
    END.
  end.
end procedure.



function fFieldAdd returns char(input hf as handle):
  DEFINE VARIABLE q1       AS WIDGET-HANDLE.
  DEFINE VARIABLE hb       AS HANDLE.
  DEFINE VARIABLE h1       AS HANDLE.

  /** If field is mandatory we'll have to assign a value **/
  IF hf:buffer-value = ? AND hf:mandatory THEN DO:
    IF hf:data-type = "logical"   THEN hf:buffer-value = false.
    IF hf:data-type = "character" THEN hf:buffer-value = 'z'.
    IF hf:data-type = "integer"   THEN hf:buffer-value = 1.
    IF hf:data-type = "decimal"   THEN hf:buffer-value = 1.0.
    IF hf:data-type = "date"      THEN hf:buffer-value = today.
  END.

  /*** If field is part of an unique index then find a unique value to it **/
  FOR EACH DICTDB._file        NO-LOCK                 WHERE DICTDB._file._file-name   = cTable
     ,EACH DICTDB._field       NO-LOCK OF DICTDB._file WHERE DICTDB._field._field-name = hf:name
     ,EACH DICTDB._index-field NO-LOCK OF DICTDB._field
     ,EACH DICTDB._index       NO-LOCK OF DICTDB._index-field WHERE DICTDB._index._unique:
    CREATE QUERY q1.
    CREATE BUFFER hb FOR TABLE string(cDBid + "." + cTable).
    q1:SET-BUFFERS(hb).
    q1:QUERY-PREPARE('FOR EACH ' + cTable + ' no-lock by ' + hf:name).
    q1:QUERY-OPEN().
    q1:GET-LAST.
    h1 = hb:BUFFER-FIELD(hf:name).
    IF hf:data-type = "logical"   THEN hf:buffer-value = not h1:buffer-value.
    IF hf:data-type = "character" THEN hf:buffer-value = h1:buffer-value + 'z'.
    IF hf:data-type = "integer"   THEN hf:buffer-value = h1:buffer-value + 1.
    IF hf:data-type = "decimal"   THEN hf:buffer-value = h1:buffer-value + 1.0.
    IF hf:data-type = "date"      THEN hf:buffer-value = h1:buffer-value + 1.
    DELETE OBJECT hb.
    DELETE OBJECT q1.
  END.
end function.


function fFieldSave returns char(input hf as handle):
  DEFINE VARIABLE cValue   AS char no-undo.
  def var i1 as int no-undo.
  def var c1 as char no-undo.
  do i1 = 0 to hf:extent:
    if i1 = 0 and hf:extent > 0 then next.
    if i1 > 0 then c1 = "/" + string(i1).
    assign cValue = get-value(cTable + "." + hf:NAME + c1).
    case hf:data-type:
      when "character" then hf:buffer-value(i1) = cValue no-error.
      when "date"      then hf:buffer-value(i1) = date(cValue) no-error.
      WHEN "integer"   then hf:buffer-value(i1) = int(cValue) no-error.
      when "decimal"   then hf:buffer-value(i1) = dec(cValue) no-error.
      when "logical" then do:
        if cValue = 'true'  THEN hf:buffer-value(i1) = true no-error.
        if cValue = 'false' THEN hf:buffer-value(i1) = false no-error.
        if cValue = '?'     THEN hf:buffer-value(i1) = ? no-error.
      end.
    end case.
    IF ERROR-STATUS:ERROR
    THEN queue-message("error","UPDATE ERROR on " + hf:NAME + c1 + ":" + ERROR-STATUS:get-message(1)).
  end.
end function.

function fFieldHTML returns char(input hf as handle):
  def var i1 as int no-undo.
  def var c1 as char no-undo.
  do i1 = 0 to hf:extent:
    if i1 = 0 and hf:extent > 0 then next.
    if i1 > 0 then c1 = "/" + string(i1).

    {&out} "<tr><td align=right>" + hf:NAME + c1 + ":</td><td align=left>".
    IF hf:data-type = "logical" THEN DO:
       {&out} "<input type=radio value=true name='" + cTable + "." + hf:name + "' ".
       IF     hf:BUFFER-VALUE(i1) THEN {&out} " checked".
       {&out} ">"  + ENTRY(1,hf:format,"/") + "<input type=radio name='" + hf:name + c1 + "' value=false".
       IF NOT hf:BUFFER-VALUE(i1) THEN {&out} " checked".
       {&out} ">"  + ENTRY(2,hf:format,"/") + " &nbsp; / <input type=radio name='" + hf:name + c1 + "' value='?'".
       IF ? = hf:BUFFER-VALUE(i1) THEN {&out} " checked".
       {&out}   ">?".
    END.
    ELSE
    IF can-do("character,decimal,integer,date",hf:data-type) THEN DO:
      IF hf:width-chars < 40
      THEN {&out} "<input name='" + cTable + "." + hf:NAME + c1 + "' type=text size=" + string(hf:width-chars) + " value='" + html-encode(right-trim(hf:STRING-VALUE(i1))) + "'>".
      ELSE {&out} "<textarea rows=3 cols=40 name='" + cTable + "." + hf:NAME + c1 + "'>" + html-encode(right-trim(hf:STRING-VALUE(i1))) + "</textarea>".
    END.
    ELSE   {&out} "<input name='" + cTable + "." + hf:NAME + c1 + "' type=text size=20 value='" + html-encode(right-trim(hf:STRING-VALUE(i1))) + "'>".
           {&out} "</td></tr>" + CHR(10).
  end.
end function.


procedure processRecord:
  DEFINE VARIABLE hb       AS HANDLE.
  DEFINE VARIABLE hf       AS HANDLE.
  DEFINE VARIABLE i1       AS int.
  DO TRANSACTION:
    CREATE BUFFER hb FOR TABLE string(cDBid + "." + cTable).
    if cAction = "add" then do:
      hb:buffer-create().
      assign cRowID = string(hb:rowid).
    end.
    hb:find-by-rowid(to-rowid(cRowid)).
    if cAction = "del" then do:
      hb:buffer-delete().
      assign cRowID = ''.
    end.
    REPEAT i1 = 1 TO hb:NUM-FIELDS:
      hf = hb:BUFFER-FIELD(i1).
      if cAction = "add"  then fFieldAdd(hf).     /****** Assign new data fields *******/
      if cAction = "save" then fFieldSave(hf).    /****** Assign the data fields *******/
    END.
  END. /** Transaction ***/
  DELETE OBJECT hb.
end procedure.

procedure displayRecord:
  DEFINE VARIABLE q1       AS WIDGET-HANDLE.
  DEFINE VARIABLE hb       AS HANDLE.
  DEFINE VARIABLE hf       AS HANDLE.
  DEFINE VARIABLE i1       AS int.
  CREATE BUFFER hb FOR TABLE string(cDBid + "." + cTable).
  IF get-value("recid") > "" THEN DO:        /* FIND rowid from recid ****/
    /**** Use dynamic query to find record for buffer  ******/
    plusLog("Convert recid:" + get-value("recid")).
    CREATE QUERY q1.
    q1:SET-BUFFERS(hb).
    q1:QUERY-PREPARE('FOR EACH ' + cTable + ' no-lock where recid(' + cTable + ') = ' + get-value("recid")).
    q1:QUERY-OPEN().
    q1:GET-NEXT.
    cRowid = string(hb:rowid).
    DELETE OBJECT q1.
  END.
  plusLog("Display Rec rowid=" + cRowid).
  hb:find-by-rowid(to-rowid(cRowid)).
  REPEAT i1 = 1 TO hb:NUM-FIELDS:
    hf = hb:BUFFER-FIELD(i1).
    fFieldHTML(hf).    /****** Display the data fields *******/
  END.
  DELETE OBJECT hb.
end procedure.

function fChoices returns char ():
  def var cFields as char no-undo.
  def var cReturn as char no-undo.
  def var i1      as int  no-undo.

   {&out} '<input type=button name=find value=Find onClick="document.form.Do.value=~'find~';document.form.submit();"> '
       cDBid '.' cTable " where <select name=index value='' onChange="
     + "~"document.form.index.value = document.form.index.options[document.form.index.selectedIndex].value;document.form.submit();~">" skip.
   {&out} "<option value=''> - </option>".

   if can-do(" Save ,Delete",get-value("do")) then return.
   else assign cWhere = ""
              lWordIdx = false.
   FOR EACH DICTDB._file NO-LOCK WHERE DICTDB._file._file-name = cTable
      ,EACH DICTDB._index       NO-LOCK OF _file:

   assign cFields  = ''.
     FOR EACH DICTDB._index-field NO-LOCK OF DICTDB._index
        ,EACH DICTDB._field NO-LOCK OF DICTDB._index-field:
       cFields = cFields + "," + DICTDB._field._field-name.
       IF DICTDB._index._index-name = cIndex then
          assign i1 = i1 + 1.
       IF DICTDB._index._index-name = cIndex and get-value('w' + string(i1)) > '' then do:
         if cWhere = ''
         then cWhere = 'where ' + DICTDB._field._field-name.
         else cWhere = cWhere + ' and ' + DICTDB._field._field-name.

         if DICTDB._index._wordidx > 0
                            then assign
            cWhere = cWhere + ' contains "' + get-value('w' + string(i1)) + '"'
            lWordIdx = true.
        else case DICTDB._field._data-type:
           when 'character' then cWhere = cWhere + ' begins "' + get-value('w' + string(i1)) + '"'.
           when 'integer'   then cWhere = cWhere + ' > ' + string(int(get-value('w' + string(i1))),'>>>>>>>>9').
           when 'decimal'   then cWhere = cWhere + ' > ' + get-value('w' + string(i1)).
           when 'date'      then cWhere = cWhere + ' = date("' + get-value('w' + string(i1)) + '")'.
         end case.
       end.
     end.

     {&out} "<option ".
     IF DICTDB._index._index-name = cIndex then do:
       {&out} " selected".
       assign cReturn = cFields.
     end.
     {&out} " value='" + DICTDB._index._index-name + "'>" + substring(cFields,2) + "</option>" skip.
   END.

   {&out} "</select> " (if lWordIdx then "contains" else "is") skip.

   do i1 = 2 to num-entries(cReturn):
     {&out} "<input type=text size=10 name=w" string(i1 - 1) " value='" + html-encode(get-value('w' + string(i1 - 1))) "'>" .
   end.
   {&out} "<BR>" skip.
   return cReturn.

end function.


procedure dbBrowse:

  DEFINE VARIABLE q1       AS WIDGET-HANDLE no-undo.
  DEFINE VARIABLE hb       AS HANDLE no-undo.
  DEFINE VARIABLE hf       AS HANDLE no-undo.
  DEFINE VARIABLE i1       AS int no-undo.
  DEFINE VARIABLE i2       AS int no-undo.
  DEFINE VARIABLE iRow     AS int no-undo.
  DEFINE VARIABLE c1       AS char no-undo.
  DEFINE VARIABLE cQuery   AS char no-undo.
  CREATE BUFFER hb FOR TABLE string(cDBid + "." + cTable).

  /**** Use dynamic query to find record for buffer  ******/

  if cAction <> 'browse' then cWhere = get-value('where').

  fChoices().

  assign
    cQuery = if cAction = "count" then 'PRESELECT' else 'FOR'
    cQuery = cQuery + ' EACH ' + cTable + ' no-lock ' + cWhere
           + (if cIndex > '' and not lWordIdx then ' use-index ' + cIndex else '')
           + ' indexed-reposition'.

  plusLog("Query:" + cQuery).

  CREATE QUERY q1.
  q1:SET-BUFFERS(hb).
  q1:QUERY-PREPARE(cQuery).
  q1:QUERY-OPEN().

  CASE cAction:
    WHEN "NEXT" THEN do:
      iCurrent = iCurrent + 10.
      q1:REPOSITION-TO-ROWID(to-rowid(cFirst)).
      q1:REPOSITION-FORWARD(20).
      q1:REPOSITION-BACKWARD(10).
      q1:GET-NEXT.
    END.
    WHEN "count" THEN DO:
      assign iCount = q1:num-results.
      q1:REPOSITION-TO-ROWID(to-rowid(cFirst)).
      q1:GET-NEXT.
    END.
    WHEN "PREV" THEN do:
      iCurrent = iCurrent - 10.
      q1:REPOSITION-TO-ROWID(to-rowid(cFirst)).
      q1:REPOSITION-BACKWARD(10).
      q1:GET-NEXT.
    END.
    WHEN "LAST" THEN do:
      iCurrent = iCount - 9.
      q1:get-last.
      q1:REPOSITION-BACKWARD(10).
      q1:GET-NEXT.
    END.
    OTHERWISE do:
      q1:get-first.
      iCurrent = 1.
    END.
  END CASE.
  if cAction = "find" then iCount = 0.

  if q1:query-off-end then do:
    q1:get-first.
    iCurrent = 1.
  end.

  if iCurrent > iCount - 10 then iCurrent = iCount - 9.
  if iCurrent < 1 then iCurrent = 1.

  assign
    cCountString = if iCount > 0
      then " Showing " + string(iCurrent) + " - " + string(iCurrent + 9) + " of " + string(iCount) + " records "
      else ""
    i2 = 0
    c1 = ''
    cFirst = string(hb:rowid).

  REPEAT i1 = 1 TO hb:NUM-FIELDS:
    hf = hb:BUFFER-FIELD(i1).
    if not can-do('character,integer,date,decimal',hf:data-type) then next.
    if hf:extent > 0 then next.
    c1 = c1 + "|"  + hf:name.
    i2 = i2 + 1.
    if i2 = 8 then leave.
  END.
  {&out} fBeginTable(substring(c1,2)) skip.


  do iRow = 1 to 10:
    if q1:query-off-end then leave.
    assign i2 = 0.
    REPEAT i1 = 1 TO hb:NUM-FIELDS:
      hf = hb:BUFFER-FIELD(i1).
      if not can-do('character,integer,date,decimal',hf:data-type) then next.
      if hf:extent > 0 then next.
      if i2 = 0
      then c1 = "<a href=~"Javascript:fEdit('" + string(hb:rowid) + "')~"> &nbsp; " + hf:string-value + " &nbsp; </a>".
      else c1 = c1 + '|' + hf:string-value.
      i2 = i2 + 1.
      if i2 = 8 then leave.
    END.
    {&out} fRow(c1) skip.
    q1:GET-NEXT.
  end.
  {&out} "</TR></TABLE>" skip.
  ASSIGN
    cLast = string(hb:rowid).

  DELETE OBJECT hb.
  DELETE OBJECT q1.

end procedure.


