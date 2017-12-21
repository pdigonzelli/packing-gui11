/*-----------------------------------------------------------------------*
  File........: filetools.i
  Version.....: 1.03 - 12/21/2000
  Description : Method library for file manipulation tools.
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  BravePoint / USI (770) 449-9696
  Copyright...: FreeFramework 2000  - http://www.freeframework.org
  Created.....: 5/10/2000
  Notes.......:
  Modified....:  
                2/6/2001 - SES added Xcode functionality
 *-----------------------------------------------------------------------*/


FUNCTION htmlCompile RETURNS CHAR (
	INPUT myFile AS CHAR,
	INPUT mywFile AS CHAR
):
  DEFINE VAR v-objtype AS CHAR NO-UNDO.
  DEFINE VAR v-tfile   AS CHAR NO-UNDO.
  DEFINE VAR v-return  AS CHAR NO-UNDO.
  DEFINE VAR v-errcnt  AS INT  NO-UNDO.

  RUN webutil/e4gl-gen.p (
  	INPUT SEARCH(myFile), 
	INPUT-OUTPUT v-objtype,
	INPUT-OUTPUT v-tfile
	) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN error-status:get-message(1).
  IF ENTRY(1,v-objtype) ne "include" THEN DO:
	  IF v-tfile > "" AND v-tfile <> myFile
	   THEN OS-RENAME VALUE(SEARCH(v-tfile)) VALUE(mywfile + ".w"). 
      &IF "(&XCODE_COMPILES}" = "YES" &THEN
      COMPILE VALUE(mywfile + ".w") XCODE getAgentSetting("xcode") SAVE NO-ERROR.
      &ELSE
      COMPILE VALUE(mywfile + ".w") SAVE NO-ERROR.
      &ENDIF
	  IF ERROR-STATUS:ERROR OR COMPILER:ERROR
	   THEN DO v-errcnt = 1 TO ERROR-STATUS:NUM-MESSAGES:
	  	v-return = (IF v-return > "" THEN v-return + "<br>" ELSE "") +
		html-encode(error-status:get-message(v-errcnt)).
	  END.
	  OS-DELETE VALUE(mywfile + ".w") NO-ERROR.
  END. /*NOT AN INCLUDE FILE - COMPILE IT*/
  ELSE ASSIGN v-return = "OK:include".
  IF v-return = "" THEN ASSIGN v-return = "OK".
  RETURN v-return.
END FUNCTION.

FUNCTION trueRandom RETURNS CHAR ():
    &IF "{&WEB_CONTEXT_EXCLUDE}" &THEN
    RETURN STRING(RANDOM(1000,9999)) + string(time,"999999").
    &ELSE
	RETURN STRING(RANDOM(1000,9999)) 
      + entry(3,web-context:exclusive-id,":") 
      + ENTRY(4,WEB-CONTEXT:EXCLUSIVE-ID,":").
    &ENDIF
END FUNCTION.

FUNCTION htmlXref RETURNS CHAR (
	INPUT myFile AS CHAR,
	INPUT mywFile AS CHAR
):
  DEFINE VAR v-objtype AS CHAR NO-UNDO.
  DEFINE VAR v-tfile   AS CHAR NO-UNDO.
  DEFINE VAR v-return  AS CHAR NO-UNDO.
  DEFINE VAR v-errcnt  AS INT  NO-UNDO.

  RUN webutil/e4gl-gen.p (
  	INPUT SEARCH(myFile), 
	INPUT-OUTPUT v-objtype,
	INPUT-OUTPUT v-tfile
	) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN error-status:get-message(1).
  IF ENTRY(1,v-objtype) ne "include" THEN DO:
	  IF v-tfile > "" AND v-tfile <> myFile
	   THEN OS-RENAME VALUE(SEARCH(v-tfile)) VALUE(mywfile + ".w"). 
      COMPILE VALUE(mywfile + ".w") SAVE = FALSE XREF VALUE(myfile + ".x") NO-ERROR.
	  IF ERROR-STATUS:ERROR /* OR COMPILER:ERROR */
	   THEN DO v-errcnt = 1 TO ERROR-STATUS:NUM-MESSAGES:
	  	v-return = (IF v-return > "" THEN v-return + "<br>" ELSE "") +
		html-encode(error-status:get-message(v-errcnt)).
	  END.
	  OS-DELETE VALUE(mywfile + ".w") NO-ERROR.
  END. /*NOT AN INCLUDE FILE - COMPILE IT*/
  ELSE ASSIGN v-return = "This is an include file - no xref generated.".
  IF v-return = "" THEN ASSIGN v-return = "OK".
  RETURN v-return.
END FUNCTION.

FUNCTION DoXref RETURNS LOGICAL(
 INPUT mydir AS CHAR,
 INPUT myfile AS CHAR,
 OUTPUT mycomment AS CHAR
):
	DEFINE VAR v-filetype 		AS CHAR NO-UNDO.
	DEFINE VAR v-thisfilespec 	AS CHAR NO-UNDO.
	DEFINE VAR v-noextfilename  AS CHAR NO-UNDO. /*filename without extension*/
	DEFINE VAR v-thisfiletype   AS CHAR NO-UNDO.
    DEFINE VAR v-errcnt		    AS INTEGER NO-UNDO.

	ASSIGN FILE-INFO:FILE-NAME = mydir + myfile.
	IF FILE-INFO:FULL-PATHNAME = ? OR INDEX(FILE-INFO:FILE-TYPE,"d") > 0
	 THEN DO:
	 	ASSIGN mycomment = "File not found!".
	 	RETURN FALSE. 
	END. /*file didn't exist or was a directory*/
	
	ELSE DO: /*file is a regular file*/
		/* ||| test that file has extension*/
		ASSIGN
		 v-filetype = SUBSTRING(myfile,R-INDEX(myfile,".") + 1)
		 v-thisfiletype = v-filetype
		 v-noextfilename = SUBSTRING(myfile,1,R-INDEX(myfile,".") - 1)
		 v-thisfilespec = FILE-INFO:FULL-PATHNAME
		.

		/*treat it differently, depending on what type of file it is*/
		CASE v-filetype:
			WHEN "htm" OR WHEN "html" OR WHEN "xml" OR WHEN "cgi" OR WHEN "php3" THEN DO:
				/*check to make sure there's no pre-existing offset file*/
				ASSIGN FILE-INFO:FILE-NAME = mydir + v-noextfilename + ".off".
				IF FILE-INFO:FULL-PATHNAME NE ? THEN DO: /*.off exists*/
					ASSIGN mycomment = 'Cannot compile SpeedScript file ' + v-thisfilespec + ' because an offset (.off) file exists with this name.'.
					RETURN FALSE.
				END. /*.off exists*/
				/*check to make sure there's no pre-existing .w file*/
				ASSIGN FILE-INFO:FILE-NAME = mydir + v-noextfilename + ".w".
				IF FILE-INFO:FULL-PATHNAME NE ? THEN DO: /*.w exists*/
					ASSIGN mycomment = 'Cannot compile SpeedScript file ' + v-thisfilespec + ' because a webobject (.w) file exists with this name.'.
					RETURN FALSE.
				END. /*.w exists*/
				ASSIGN mycomment = htmlXref(v-thisfilespec,SUBSTRING(v-thisfilespec,1,R-INDEX(v-thisfilespec,"{&SLASH}")) + v-noextfilename).
                IF NOT mycomment BEGINS "OK" THEN RETURN FALSE.
				ELSE RETURN TRUE.
			END. /*html*/
			WHEN "w" OR WHEN "p" THEN DO:
				COMPILE VALUE(v-thisfilespec) SAVE = FALSE XREF VALUE(v-thisfilespec + ".x") NO-ERROR.
                IF ERROR-STATUS:ERROR /* OR COMPILER:ERROR */
				 THEN DO:
				 	DO v-errcnt = 1 TO ERROR-STATUS:NUM-MESSAGES:
						mycomment = (IF mycomment > "" THEN mycomment + "<br>" ELSE "") +
						html-encode(error-status:get-message(v-errcnt)).
					END. /*v-errcnt*/
					RETURN FALSE.
				END. /*ERRORS WITH COMPILE*/
				ELSE RETURN TRUE.
			END. /*.w or .p*/
			OTHERWISE DO: /*can't handle this filetype*/
				ASSIGN mycomment = 'Cannot create Xref for files of type "' + v-filetype + '".'.
				RETURN FALSE.
			END. /*can't handle this filetype*/
		END CASE. /*v-filetype*/
		ASSIGN mycomment = v-filetype.
		RETURN TRUE.
	END.
END FUNCTION. /*doXref*/

FUNCTION makeTempW RETURNS LOGICAL (
	INPUT myFile AS CHAR,
    INPUT-OUTPUT myTFile AS CHAR
):
  DEFINE VAR v-objtype AS CHAR NO-UNDO.

  RUN webutil/e4gl-gen.p (
  	INPUT SEARCH(myFile), 
	INPUT-OUTPUT v-objtype,
	INPUT-OUTPUT MYtfile
	) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN FALSE.
  RETURN TRUE.
END FUNCTION.

FUNCTION whichDLC RETURNS CHAR ():
    DEFINE VAR utilpath AS CHAR NO-UNDO.
    DEFINE VAR myDLC AS CHAR NO-UNDO.
    ASSIGN myDLC = OS-GETENV("FFWDLC").
    IF myDLC = ? THEN ASSIGN myDLC = OS-GETENV("DLC").
    IF myDLC = ? AND OPSYS NE "UNIX":U
     THEN GET-KEY-VALUE SECTION "Startup" KEY "DLC" VALUE myDlc.  
    IF myDLC = ? THEN DO:
        ASSIGN utilpath = search("prolang/convmap").
        IF utilpath NE ? THEN ASSIGN myDLC = 
         SUBSTRING(utilpath,1,INDEX(utilpath,"/prolang") - 1).
    END.
    RETURN myDLC.
END FUNCTION.



