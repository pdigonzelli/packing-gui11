/*-----------------------------------------------------------------------*
  File........: webserversigcheck.i
  Version.....: 1.03 - 2/2/2001
  Description : Checks the "signature" of the webserver through which the
                request was funneled to make sure that it is a valid host.
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell - USI / BravePoint, Inc. (770) 449-9696
  Copyright...: FreeFramework 2001  - http://www.freeframework.org
  Created.....: 1/2/2001
  Notes.......: This code is written to allow a WebSpeed broker to run
                on a public host without exposing the broker to hits from
                untrusted webservers on other hosts.  For instance, by 
                default a WebSpeed broker will respond to any request from
                any host that can reach its IP address and port.  Thus someone
                could setup any arbitrary webserver with a freely downloadable
                cgiip messenger and fake requests, thus getting around any 
                webserver-based authentication.  Further, since CGIIP relies
                on environment variables to populate important CGI variables 
                such as REMOTE_ADDR, it could in theory be run from the malicious
                user's commandline after setting a few environment variables.
                
                THIS FILE REQUIRES FREEFRAMEWORK 1.03 OR GREATER.
                
 *-----------------------------------------------------------------------*/
&IF DEFINED(webserversigcheck_i) = 0 &THEN
    &GLOBAL-DEFINE webserversigcheck_i YES
    &GLOBAL-DEFINE defaultsigvars "SERVER_SOFTWARE,OS,SERVER_NAME,LOCAL_ADDR"
    { ffw/lib/ffw_global.i }
    { ffw/lib/agentsetting.i }
    { ffw/lib/lognote.i }
    
DEFINE NEW GLOBAL SHARED VARIABLE WebServerSigList AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE WebServerSigVars AS CHAR NO-UNDO.

PROCEDURE InitServerSigVars:
    ASSIGN
     WebServerSigList = GetAgentSetting("WebServerSigList")
     WebServerSigVars = GetAgentSetting("WebServerSigVars")
    .
    IF WebServerSigList = "" THEN ASSIGN WebServerSigList = "*". /*default to allow every host*/
    IF WebServerSigVars = "" THEN ASSIGN WebServerSigVars = {&defaultsigvars}.
END PROCEDURE.

FUNCTION GetThisSig RETURNS CHAR():
    DEFINE VAR v-envcount AS INTEGER NO-UNDO.
    DEFINE VAR v-thissig  AS CHAR    NO-UNDO.

    /*Initialize variables on the first run of this function. 
      This is done here to avoid increasing the size of the action
      segment*/
    IF WebServerSigList = "" THEN RUN InitServerSigVars IN THIS-PROCEDURE.
    
    DO v-envcount = 1 to NUM-ENTRIES(WebServerSigVars):
        ASSIGN v-thissig = v-thissig + "|" + GET-CGI(ENTRY(v-envcount,WebServerSigVars)).
    END. /*v-envcount*/
    ASSIGN v-thissig = TRIM(v-thissig,"| ").
    RETURN v-thissig.
END FUNCTION.

FUNCTION WebServerSigCheck RETURNS LOGICAL():
    
    DEFINE VAR v-thissig  AS CHAR    NO-UNDO.
    
    /*Initialize variables on the first run of this function. 
      This is done here to avoid increasing the size of the action
      segment*/
    IF WebServerSigList = "" THEN RUN InitServerSigVars IN THIS-PROCEDURE.

    /*Compute the webserver's signature for this hit*/
    ASSIGN v-thissig = GetThisSig().

    /*Now compare the approved WebServer signature with the incoming signature
      for this hit. */
    IF CAN-DO(WebServerSigList,v-thissig) THEN RETURN TRUE.
    ELSE DO:
        /*sound the alarm!*/
        LogNote("Warning","This WebSpeed Broker is being utilized by an unapproved web server with a server signature of: " + v-thissig).
        RETURN FALSE.
    END.
END FUNCTION.


&ENDIF

