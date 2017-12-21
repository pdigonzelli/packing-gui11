/*-----------------------------------------------------------------------*
The contents of this file are subject to the POSSENET Public License Version 
1.0 (the "License"); you may not use  this file except in compliance with the License. 
You may obtain a copy of the License at http://www.possenet.org/license.html.

Software distributed under the License is distributed on an "AS IS" basis, 
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for 
the specific language governing rights and limitations under the License. 

The Original Code is Progress IDE, released December 1, 2000. 

The Initial Developer of the Original Code is Progress Software Corporation. 
Portions created by are Copyright (c) 2000, 2001 FreeFrameWork. All Rights Reserved. 

Contributor(s): 
    Steve Southwell - BravePoint, Inc.
    Mario Paranhos - BravePoint, Inc.
    
Major Modifications:
    Removed all support for state-aware objects
    Included robust.i for better robustness and security

  File........: ffweb-disp.p
  Version.....: 1.03 2/6/2001
  Description : Replacement for web-disp.p, which is normally distributed with 
  				WebSpeed
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell - BravePoint / USI (770) 449-9696
  Copyright...: PSC and FreeFramework 2000  - http://www.freeframework.org
  Created.....: 5/10/2000
  Notes.......: This is a stripped-down and streamlined version of web-disp.p
  
  PRV wants a more elegant way to start databases.
 *-----------------------------------------------------------------------*/
{ ffw/lib/ffw_global.i }

DEFINE NEW GLOBAL SHARED VAR v-resettable    AS LOGICAL NO-UNDO. /*whether to allow resets*/
DEFINE NEW GLOBAL SHARED VAR v-checkinterval AS INTEGER NO-UNDO. /*how often to recycle the wait-for*/
DEFINE NEW GLOBAL SHARED VAR v-batchprocname AS CHAR    NO-UNDO. /*procedure to run (if any) during recycle*/
DEFINE VAR totalRequests AS INTEGER NO-UNDO.
DEFINE VAR origPropath AS CHAR NO-UNDO.

DEFINE VAR v-wftimedout AS LOGICAL INIT TRUE NO-UNDO. /*whether the wait-for timed out*/
ASSIGN
 v-logtypes = "*"
 origPropath = PROPATH
.

{ ffw/lib/lognote.i }

/* To avoid a conflict in 3.1d with FFW's methodologies */
&GLOBAL-DEFINE SESSION-INCLUDED NO

{ src/web/method/cgidefs.i NEW }      /* CGI variables */
{ src/web/method/cgiarray.i NEW }     /* CGI variables and User Fields*/
&if "{&MAPPEDOBJECTS}" = "YES" &then
{ src/web/method/tagmap.i NEW }       /* For html-mapped objects */
&endif

&if "{&WEBSERVER_SIG_CHECK}" = "YES" &then
{ ffw/lib/webserversigcheck.i }       /* For webserver signature checking */
&endif

DEFINE VARIABLE ix                  AS INTEGER NO-UNDO.
DEFINE VARIABLE pause-period        AS INTEGER NO-UNDO.

&IF "{&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
DEFINE NEW GLOBAL SHARED VAR MSNGRPID          AS CHAR NO-UNDO.
DEFINE VAR textin            AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR web-current-env   AS CHAR NO-UNDO.

ASSIGN MSNGRPID = SESSION:PARAMETER.

/*AFTER EACH HIT*/
PROCEDURE CleanUp:
    FOR EACH tt-cgi: 
        DELETE tt-cgi.
    END.
    ASSIGN web-current-env = "".
END PROCEDURE. /*CleanUp*/

/*ON EACH HIT*/
PROCEDURE HandleHit:    
    DEFINE VAR v-field AS CHAR NO-UNDO.
    DEFINE VAR v-value AS CHAR NO-UNDO.
    
    /*WEB CONTEXT*/
    ASSIGN web-current-env = "".
    REPEAT:
        IMPORT UNFORMATTED textin.
        IF textin = "." THEN LEAVE.
        ASSIGN web-current-env = web-current-env + textin + "~n".
    END.
    ASSIGN web-current-env = TRIM(web-current-env,"~n").
    
    FOR EACH tt-webfield:
        DELETE tt-webfield.
    END.
    ASSIGN
     wc-form-list = ""
     wc-query-list = ""
    .
    REPEAT:
        IMPORT UNFORMATTED textin.
        IF textin = "." THEN LEAVE.
        ASSIGN
         v-field = ENTRY(1,textin,chr(255))
         v-value = url-decode(ENTRY(2,textin,chr(255)))
        .
        FIND tt-webfield
         WHERE tt-webfield.webfieldname = v-field
         NO-ERROR.
        IF NOT AVAIL tt-webfield THEN CREATE tt-webfield.
        ASSIGN 
         tt-webfield.webfieldname = v-field
         tt-webfield.webfieldvalue = v-value
         wc-form-list = wc-form-list + v-field + ","
        .  
    END.
    assign wc-form-list = trim(wc-form-list,",").
END PROCEDURE.


&ELSE
/*Web Requests are handled by this Pseudo-event*/
ON "WEB-NOTIFY":U ANYWHERE DO:
  DEFINE VARIABLE c-value AS CHARACTER NO-UNDO.
  OUTPUT {&WEBSTREAM} TO "WEB":U.
  ASSIGN v-wftimedout = FALSE. /*to signal the proactive checking not to run when webobject is done.*/
  ASSIGN totalrequests = totalrequests + 1.
  
  /* Parse the request from the web server. */
  RUN init-request IN web-utilities-hdl.
  &if "{&WEBSERVER_SIG_CHECK}" = "YES" &then
    IF WebServerSigCheck() = FALSE THEN RETURN.
  &endif
  /* Check for specific requests: 
       PING               - respond that web-disp.p is running
       DEBUG              - call up debugging administration form
       RESET              - reset the web-utilities. 
  */
  IF AppProgram EQ "PING":U THEN DO:
    output-http-header("Content-Type":U, "text/html":U).
    output-http-header("","").
    {&OUT}
      '<HTML>':U SKIP
      '<HEAD><TITLE>':U "WebSpeed Agent successfully accessed"
        '</TITLE></HEAD>':U SKIP
      '<BODY BGCOLOR="#FFFFCC" TEXT="#000000">':U SKIP
      '<H1>':U "WebSpeed Agent successfully accessed" '</H1>':U SKIP
    {&END}
    IF debugging-enabled THEN DO:
      ASSIGN FILE-INFO:FILE-NAME = ".":U.  /* current directory */
      {&OUT}
        '<DL>':U SKIP
        '  <DT><B>':U "Default Directory:" '</B>':U SKIP
        '    <DD><FONT SIZE="-1">':U FILE-INFO:FULL-PATHNAME 
        '</FONT><BR><BR>':U SKIP
        '  <DT><B>':U "Web Object Path (PROPATH):" '</B>':U SKIP
        '    <DD><FONT SIZE="-1">':U REPLACE(PROPATH, ",":U, "<BR>~n":U) 
        '</FONT><BR><BR>':U SKIP
        '  <DT><B>':U "Connected Databases:" '</B>':U SKIP.

      DO ix = 1 TO NUM-DBS:
        {&OUT}
          '    <DD><FONT SIZE="-1">':U LDBNAME(ix) ' (':U DBTYPE(ix)
          ')</FONT>':U SKIP.
      END.        
      {&OUT} '</DL>':U SKIP.
    END.
    {&OUT} '</BODY></HTML>':U SKIP.
  END.

  /* If debugging is enabled (probably Environment = Development mode) */
  ELSE IF AppProgram EQ "DEBUG":U AND debugging-enabled THEN 
    RUN web/support/printval.p ("admin":U).

  /* Reset all web utilities including ffweb-util.p and reload the .ini file */
  ELSE IF AppProgram EQ "RESET":U AND debugging-enabled AND v-resettable THEN DO:
    LogNote("Note","Received reset request.").
    ASSIGN PROPATH = origPropath.
    RUN reset-utilities.
    output-http-header("Content-Type":U, "text/plain":U).
    output-http-header("","").
    {&OUT}
      "Reset " (IF VALID-HANDLE(web-utilities-hdl)
        THEN "succeeded" ELSE "failed") " for this Agent." SKIP
    {&END}
  END.


  /* Try to run whatever was specified in the URL */
  ELSE DO:
      RUN run-web-object IN web-utilities-hdl (AppProgram).
  
     /* If any debugging options are set except "top" ... */
     IF debugging-enabled AND debug-options <> "" AND
        LOOKUP("top":U,debug-options) = 0 THEN
     RUN web/support/printval.p (debug-options).
  END.

  /* Output any pending messages queued up by queue-message() */
  IF available-messages(?) THEN
    output-messages("all", ?, "Messages:").

  OUTPUT {&WEBSTREAM} CLOSE.

END. /* ON "{&WEB-NOTIFY}"... */
&ENDIF

/* Initialize the utilities. */
RUN reset-utilities.  

/* Initialize any session-specific information */
RUN init-session IN web-utilities-hdl.

/* If there is a user-defined startup procedure, then run it here */
RUN user-startup IN web-utilities-hdl.

&IF "{&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
OUTPUT STREAM WEBSTREAM TO VALUE("/tmp/wa_" + trim(MSNGRPID)) APPEND UNBUFFERED.
DEFINE VAR mytime AS INTEGER NO-UNDO.
ASSIGN mytime = ETIME.
WAIT-BLOCK: REPEAT: 
    IF totalrequests = 0 /*initial startup*/
     OR (etime > mytime + (MAX(15,v-checkinterval) * 1000)) THEN DO:
  		RUN Check-Default-Databases IN web-utilities-hdl NO-ERROR.
        IF ERROR-STATUS:ERROR THEN LogNote("Error","Unable to check default databases in ffweb-disp.p").
		/*If there is a batch program that needs to be run, then run it now. */
		IF v-batchprocname ne "" THEN DO:  /*either this was the first time to run, or the wait-for timed out*/
			RUN VALUE (v-batchprocname) NO-ERROR.
			IF ERROR-STATUS:ERROR THEN DO:
	  		LogNote("Error","Your batch procedure, " + v-batchprocname + " had the following error: " + ERROR-STATUS:GET-MESSAGE(1)).
			END.
		END. /*there was a batch program to run*/
        ASSIGN mytime = etime.
    END. /*reached timeout*/
    REPEAT:
        IMPORT UNFORMATTED textin.
        IF textin = "--Begin Request--" THEN DO:
            ASSIGN totalRequests = totalRequests + 1.
            RUN HandleHit IN THIS-PROCEDURE. /*get the cgi-environment into a variable*/
            /* Parse the request from the web server. */
            RUN init-request IN web-utilities-hdl.
            CASE AppProgram:
                WHEN "PING":U THEN DO:
                    output-http-header("Content-Type":U, "text/html":U).
                    output-http-header("","").
                    {&OUT}
                      '<HTML>':U SKIP
                      '<HEAD><TITLE>':U "Agent successfully accessed"
                        '</TITLE></HEAD>':U SKIP
                      '<BODY BGCOLOR="#FFFFCC" TEXT="#000000">':U SKIP
                      '<H1>':U "Agent successfully accessed" '</H1>':U SKIP
                    {&END}
                    IF debugging-enabled THEN DO:
                      ASSIGN FILE-INFO:FILE-NAME = ".":U.  /* current directory */
                      {&OUT}
                        '<DL>':U SKIP
                        '  <DT><B>':U "Default Directory:" '</B>':U SKIP
                        '    <DD><FONT SIZE="-1">':U FILE-INFO:FULL-PATHNAME 
                        '</FONT><BR><BR>':U SKIP
                        '  <DT><B>':U "Web Object Path (PROPATH):" '</B>':U SKIP
                        '    <DD><FONT SIZE="-1">':U REPLACE(PROPATH, ",":U, "<BR>~n":U) 
                        '</FONT><BR><BR>':U SKIP
                        '  <DT><B>':U "Connected Databases:" '</B>':U SKIP.
                
                      DO ix = 1 TO NUM-DBS:
                        {&OUT}
                          '    <DD><FONT SIZE="-1">':U LDBNAME(ix) ' (':U DBTYPE(ix)
                          ')</FONT>':U SKIP.
                      END.        
                      {&OUT} '</DL>':U SKIP.
                    END.
                    {&OUT} '</BODY></HTML>':U SKIP.
                  END. /*PING*/
                
                  /* If debugging is enabled (probably Environment = Development mode) */
                  WHEN "DEBUG":U THEN IF debugging-enabled THEN RUN web/support/printval.p ("admin":U).
                
                  /* Reset all web utilities including ffweb-util.p and reload the .ini file */
                  WHEN "RESET":U THEN IF debugging-enabled AND v-resettable THEN DO:
                    RUN reset-utilities.
                    output-http-header("Content-Type":U, "text/plain":U).
                    output-http-header("","").
                    {&OUT}
                      "Reset " (IF VALID-HANDLE(web-utilities-hdl)
                        THEN "succeeded" ELSE "failed") " for this Agent." SKIP
                    {&END}
                  END.
                  OTHERWISE RUN run-web-object IN web-utilities-hdl (AppProgram).
            END CASE. /*APPPROGRAM*/
            PUT STREAM WEBSTREAM UNFORMATTED "~n<-- DeEpSpEw DoNe PrOcEsSiNg -->" SKIP.
            RUN CleanUp IN THIS-PROCEDURE.
            NEXT WAIT-BLOCK.
        END.
        IF textin = "--Quit--" THEN QUIT.
    END. /*INNER REPEAT*/
END. /* WAIT-BLOCK: REPEAT... */
OUTPUT CLOSE.
&ELSE

WAIT-FOR-BLOCK: REPEAT ON ERROR UNDO WAIT-FOR-BLOCK, LEAVE WAIT-FOR-BLOCK 
 ON QUIT  UNDO WAIT-FOR-BLOCK, LEAVE WAIT-FOR-BLOCK
 ON STOP  UNDO WAIT-FOR-BLOCK, NEXT WAIT-FOR-BLOCK: 
	IF v-wftimedout = TRUE THEN DO:
  		RUN Check-Default-Databases IN web-utilities-hdl NO-ERROR.
        IF ERROR-STATUS:ERROR THEN LogNote("Error","Unable to check default databases in ffweb-disp.p").

		/*If there is a batch program that needs to be run, then run it now. */
		IF v-batchprocname ne "" THEN DO:  /*either this was the first time to run, or the wait-for timed out*/
			RUN VALUE (v-batchprocname) NO-ERROR.
			IF ERROR-STATUS:ERROR THEN DO:
	  		LogNote("Error","Your batch procedure, " + v-batchprocname + " had the following error: " + ERROR-STATUS:GET-MESSAGE(1) ).
			END.
		END. /*there was a batch program to run*/
	END. /*either this was the first time to run, or the wait-for timed out*/
ASSIGN v-wftimedout = TRUE. /*re-initialize it*/

WAIT-FOR "WEB-NOTIFY":U OF DEFAULT-WINDOW PAUSE MAX(15,v-checkinterval). 
END. /* WAIT-FOR-BLOCK: REPEAT... */
&ENDIF
  
/* -------------------------------------------------------------------
   Procedure: reset-utilities
   Purpose:   Restarts the web-utilities-hdl, and initializes is.
 --------------------------------------------------------------------*/   
PROCEDURE reset-utilities :      
  /* Clean up the existing utilities procedure. */
  IF VALID-HANDLE(web-utilities-hdl) THEN DO: 
    RUN dispatch IN web-utilities-hdl ('destroy') NO-ERROR.
    /* Make sure it worked. */
    IF VALID-HANDLE(web-utilities-hdl) 
    THEN DELETE PROCEDURE web-utilities-hdl.
  END.
  /* Create the utilities handle as a persistent procedure. */
  RUN ffw/robust/ffweb-util.p PERSISTENT SET web-utilities-hdl NO-ERROR.
  IF ERROR-STATUS:ERROR THEN LogNote("Error","The FreeFrameWork web utilities procedure, ffweb-util.p, could not be loaded.").

  /* If this didn't work, there are big problems. */
  IF NOT VALID-HANDLE(web-utilities-hdl) THEN DO:
  	LogNote("Error","The FreeFrameWork web utilities procedure, ffweb-util.p, could not be loaded.").
  	QUIT.
  END. /*BIG PROBLEMS*/
  &IF "{&mappedobjects}" = "yes" &THEN
  /* Initialize the tagmap file. */
  RUN dispatch IN web-utilities-hdl ('reset-tagmap-utilities':U).
  &ENDIF.
END PROCEDURE.

