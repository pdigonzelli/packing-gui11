/*------------------------------------------------------------------------

  File: showschema.p
  Version: 1.1 - 3/8/02
  Description: Takes the schema of the dictdb and displays it to the web
  Input Parameters:
      database name
  Output Parameters:
      <none>
  Author: S.E. Southwell - Bravepoint / USI
  Created: 6/01/98
  Last Modified:
            March, 2002 - SES updated to use dictdb instead of a passed-in ref.
  			March, 1999 - SES updated to show extents, indices, help, etc.
            12/2000 - SES added sequences
------------------------------------------------------------------------*/
{ src/web/method/wrap-cgi.i }
{ ffw/lib/ffw_global.i }                    /*PATH SETTINGS AND OTHER GENERAL THINGS*/
{ ffw/lib/ffpplib.i }

  DEFINE VAR showfieldsize   AS INTEGER NO-UNDO.
  DEFINE VAR maxfieldsize    AS INTEGER NO-UNDO.
  DEFINE VAR thiskeys        AS CHAR    NO-UNDO.
  DEFINE VAR findwhereclause AS CHAR    NO-UNDO.
  DEFINE VAR thislist        AS CHAR    NO-UNDO.
  DEFINE VAR thisai          AS CHAR    NO-UNDO.
  DEFINE VAR thisdi          AS CHAR    NO-UNDO.
  DEFINE VAR thiswi          AS CHAR    NO-UNDO.
  DEFINE VAR findfields      AS CHAR    NO-UNDO.
  DEFINE VAR findtitles      AS CHAR    NO-UNDO.
  DEFINE VAR findcols        AS CHAR    NO-UNDO.
  DEFINE VAR keyfields       AS CHAR    NO-UNDO.
  DEFINE VAR fullkeyfields       AS CHAR    NO-UNDO.
  DEFINE VAR codein          AS CHAR    NO-UNDO.
  DEFINE VAR codeout         AS CHAR    NO-UNDO.
  DEFINE VAR fieldname       AS CHAR    NO-UNDO.
  DEFINE VAR fieldstart      AS INTEGER NO-UNDO.
  DEFINE VAR fieldend        AS INTEGER NO-UNDO.
  DEFINE VAR i               AS INTEGER NO-UNDO.


    {&OUT}
	 '<a name="top"></a>'
     "<span class=~"sechdr~">" LDBNAME("dictdb")  " Schema listing </span><br>"
     "As of: " TODAY "<br>"
	 .
	 
    {&OUT} 
	 "<br> <br>"
     "<span class=~"sechdr2~">Legend:</span><br>"
     "<TABLE BORDER=1>"
     "<TR><TD><IMG SRC=~"" ffimages "key.gif~" width=~"30~" height=~"16~"></TD><TD>Key Fields</TD></TR>"
     "<TR><TD><IMG SRC=~"" ffimages "aindex.gif~" width=~"14~" height=~"14~"></TD><TD>Indexed field</TD></TR>"
     "<TR><TD><IMG SRC=~"" ffimages "wordindex.gif~" width=~"34~" height=~"14~"></TD><TD>Word Indexed field</TD></TR>"
     "</TABLE>"
 	 "<br> <br>"
     "<span class=~"sechdr2~">Sequences:</span><br>"
   .
     FOR EACH dictdb._sequence:
        {&OUT}
         dictdb._sequence._seq-name " &nbsp~;"
        .
     END.
   {&out}  
 	 "<br> <br>"
     "<span class=~"sechdr2~">Tables:</span><br>"
   .
     FOR EACH dictdb._file
      WHERE dictdb._file._hidden = FALSE:
        {&OUT}
         "<A HREF='#" dictdb._file._file-name "' CLASS=~"lnavlink~">" dictdb._file._file-name "</A> &nbsp;"
        .
     END.
	 {&out}
 	  "<br> <br>"
	 .
    FOR EACH dictdb._file    
     WHERE dictdb._file._hidden = FALSE:    
        {&OUT}
         "<BR><TABLE BORDER=0 CELLSPACING=1 CELLPADDING=1><TR><TD COLSPAN='3'><A NAME ='" dictdb._file._file-name "' class=~"sechdr2~">" dictdb._file._file-name "</a></TD>"
         "<TD>" dictdb._file._desc "</TD></TR>" SKIP
         "</TR>" SKIP
         "<TR><TD COLSPAN='4'><HR></TD></TR>" SKIP.    
        ASSIGN
         thiskeys = ""
         thiswi = ""
         thisai = ""
         thisdi = ""
        .    
        FIND FIRST dictdb._Index    
         WHERE RECID(dictdb._Index) = dictdb._file._Prime-Index 
         AND dictdb._Index._unique    
         NO-LOCK NO-ERROR.    
        IF NOT AVAIL dictdb._index
         THEN FIND FIRST dictdb._Index    
         WHERE dictdb._Index._file-recid = RECID(dictdb._file) 
         AND dictdb._index._unique    
         NO-LOCK NO-ERROR.    

        IF AVAIL dictdb._Index THEN DO:    
            FOR EACH dictdb._Index-field     
             WHERE dictdb._Index-field._Index-recid = recid(dictdb._Index):    
                ASSIGN thiskeys = thiskeys + "," + STRING(dictdb._Index-field._field-recid).    
            END.    
        END. /*found primary unique index*/   
        FOR EACH dictdb._index
         WHERE dictdb._index._file-recid = recid(dictdb._file),
         EACH dictdb._index-field OF dictdb._index,
         EACH dictdb._field 
         WHERE RECID(dictdb._field) = dictdb._index-field._Field-recid:
            IF dictdb._index._wordidx NE 1 THEN DO:
                ASSIGN thisai = thisai + _field._field-name + ",".
            END.
            ELSE DO:
                ASSIGN thiswi = thiswi + _field._field-name + ",".
            END.
        END.
        FOR EACH dictdb._field OF dictdb._file by dictdb._field._order: 
            {&OUT} "<TR><TD ALIGN=~"RIGHT~">".  
            IF CAN-DO(thiskeys,STRING(recid(dictdb._field))) THEN {&OUT} "<IMG SRC=~"" ffimages "key.gif~" width=~"30~" height=~"16~">".
            IF CAN-DO(thisai,dictdb._field._field-name) THEN {&OUT} "<IMG SRC=~"" ffimages "aindex.gif~" width=~"14~" height=~"14~">".
            IF CAN-DO(thiswi,dictdb._field._field-name) THEN {&OUT} "<IMG SRC=~"" ffimages "wordindex.gif~" width=~"34~" height=~"14~">".
            ELSE DO:
                {&OUT} "&nbsp~;".
            END.    
            {&OUT}
             "</TD><TD NOWRAP>"
			  dictdb._field._field-name 
			  (IF dictdb._field._extent > 1 THEN " (" + string(dictdb._field._extent) + ")" ELSE "")
			 "</TD>"
             "<TD>" dictdb._field._format "</TD>"
             "<TD>" (IF dictdb._field._desc NE '' THEN dictdb._field._desc ELSE dictdb._field._help) " - (" dictdb._field._field-rpos ") </TD></TR>" SKIP
            .    
        END. /*each _field*/    
       {&OUT} '<TR><td>&nbsp~;</td><TH COLSPAN="3">' _file._file-name ' Indices:<HR></TH></TR>'.
        DEFINE VAR v-thisindexflags AS CHAR NO-UNDO.
        FOR EACH dictdb._index
         WHERE dictdb._index._file-recid = recid(dictdb._file):
            ASSIGN v-thisindexflags = "".
            if RECID(dictdb._Index) = dictdb._file._Prime-Index THEN ASSIGN v-thisindexflags = v-thisindexflags + "P".
            IF dictdb._Index._unique THEN ASSIGN v-thisindexflags = v-thisindexflags + "U".
            IF dictdb._index._wordidx = 1 THEN ASSIGN v-thisindexflags = v-thisindexflags + "W".
            IF v-thisindexflags NE "" THEN ASSIGN v-thisindexflags = "(" + v-thisindexflags + ")" .
            {&OUT} 
             '<TR><td>'
             v-thisindexflags
             '&nbsp~;</td><TH ALIGN="LEFT">' dictdb._index._index-name '</TH><TD COLSPAN="2">'.
            FOR EACH dictdb._index-field OF dictdb._index,
             EACH dictdb._field     
             WHERE RECID(dictdb._field) = dictdb._index-field._Field-recid
             by dictdb._index-field._index-seq:
                {&OUT} dictdb._field._field-name " &nbsp~;&nbsp~;".
            END. /*EACH FIELD OF INDEX*/
            {&OUT} '</TD></TR>'.
        END. /*INDEX*/
        {&OUT} "</TABLE>" SKIP.    
    END. /*each _file*/
  
  

