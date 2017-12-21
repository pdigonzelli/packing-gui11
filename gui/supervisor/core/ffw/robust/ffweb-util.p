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
    Many functions formerly in web-disp have been eliminated or moved to robust.i

Contributors:
    Steve Southwell - BravePoint, Inc.
    Mario Paranhos - BravePoint, Inc.
 *-----------------------------------------------------------------------*/

/*-----------------------------------------------------------------------*
  File........: ffweb-util.p
  Version.....: 1.04 - 12/13/2001
  Description : FreeFrameWork version of web-util.p which was provided with WebSpeed.
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell - BravePoint / USI (770) 449-9696
  Copyright...: PSC and FreeFramework 2000  - http://www.freeframework.org
  Created.....: 6/4/2000
  Notes.......:
 *-----------------------------------------------------------------------*/
&GLOBAL-DEFINE WEB-UTIL_P TRUE
{ ffw/lib/ffw_global.i }
&IF "{&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
DEFINE NEW GLOBAL SHARED VAR web-current-env   AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR MSNGRPID          AS CHAR NO-UNDO.
&ENDIF

{ src/web/method/cgidefs.i {&NEW} }      /* Basic CGI variables */
{ src/web/method/cgiarray.i {&NEW} }     /* Extended CGI array variables */
{ ffw/lib/robust.i }


&if "{&MAPPEDOBJECTS}" = "YES" &then
/*html mapping stuff */
{ src/web/method/tagmap.i {&NEW} }       /* HTML/PSC type mapping */
&SCOPED-DEFINE tagMapFileName "tagmap.dat":U
DEFINE STREAM tagMapStream.
&endif

/*old include files need these*/
&Scoped-define WEB-NOTIFY WEB-NOTIFY
&Scoped-define WEB-CURRENT-ENVIRONMENT WEB-CONTEXT:CURRENT-ENVIRONMENT
&Scoped-define WEB-FORM-INPUT WEB-CONTEXT:FORM-INPUT
&Scoped-define WEB-EXCLUSIVE-ID WEB-CONTEXT:EXCLUSIVE-ID
&Global-define WSEU-NAME "WSEU":U
/* Variables for configuration options.  Initialized upon Agent startup. */
DEFINE VARIABLE cfg-environment     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cfg-eval-mode       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cfg-appurl          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cfg-debugging       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cfg-cookiepath      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cfg-cookiedomain    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFunction           AS CHARACTER NO-UNDO.

{src/web/method/proto.i}
{src/web/method/admweb.i}
{src/web/method/cgiutils.i}
{src/web/method/cookies.i}
{src/web/method/message.i}



IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  logNote("Error","{&FILE-NAME} should only be RUN PERSISTENT.").
  RETURN.
END.


ASSIGN web-utilities-hdl = THIS-PROCEDURE.


FUNCTION check-agent-mode RETURNS LOGICAL 
  (INPUT p_mode AS CHARACTER):
  RETURN CAN-DO(cfg-environment, p_mode).
END FUNCTION.


PROCEDURE init-form :
/*---------------------------------------------------------------------------
Procedure:   init-form
Description: Initializes variables from the web form input and QUERY_STRING
             part of the URL.
Input:       Web form input
Output:      Sets global variables defined in src/web/method/cgidefs.i
----------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.
  DEFINE VARIABLE m AS INTEGER NO-UNDO.
  DEFINE VARIABLE a AS INTEGER NO-UNDO.
  DEFINE VARIABLE i-field AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i-old-field AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i-value AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i-pair AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i-www-form-urlencoded AS CHARACTER NO-UNDO.

  ASSIGN
    i-www-form-urlencoded = ""
    FieldVar              = ""
    FieldList             = ""
    j                     = 0.

  RETURN.
END PROCEDURE.  /* init-form */


PROCEDURE init-request :
/*---------------------------------------------------------------------------
Procedure:   init-request
Description: Initializes WebSpeed environment for each web request
Input:       Environment variables
Output:      Sets global variables defined in src/web/method/cgidefs.i
---------------------------------------------------------------------------*/
  DEFINE VARIABLE i-key           AS CHARACTER FORMAT "x(20)" NO-UNDO.
  DEFINE VARIABLE i-value         AS CHARACTER FORMAT "x(40)" NO-UNDO.
  DEFINE VARIABLE i-debug-cookie  AS CHARACTER FORMAT "x(20)" NO-UNDO.
  DEFINE VARIABLE i               AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-http-host     AS CHARACTER FORMAT "x(40)" NO-UNDO.
  DEFINE VARIABLE v-host          AS CHARACTER FORMAT "x(40)" NO-UNDO.
  DEFINE VARIABLE v-port          AS CHARACTER FORMAT "x(5)"  NO-UNDO.

  ASSIGN
    /* Check for a "WSDebug" Cookie */
    i-debug-cookie = get-cookie ("WSDebug":U)
    /* Initialize debug options based on Cookie.  If there is no Cookie,
       then debug-options will be blank.  This is fine because it needs
       to be initialized for each request anyway. */
    debug-options = i-debug-cookie.

  RUN init-variables.    /* initialize CGI and misc. variables */
  RUN init-form.         /* initialize form input and parse QUERY_STRING */

  /* Initialize User Fields */
  ASSIGN
    UserFieldVar  = ""
    UserFieldList = "".

  /* Get debugging options if specified in QUERY_STRING or form input */
  ASSIGN i-value = get-field("debug":U).

  /* Check for "debug" with no options in QUERY_STRING for backwards
     compatibility. */
  IF i-value = "" AND QUERY_STRING MATCHES "*debug*":U THEN
    ASSIGN i-value = "all":U.

  /* Don't allow debugging if disabled via configuration options. */
  IF debugging-enabled = FALSE THEN
    ASSIGN i-value = "".
    
  /* Turn debugging off? */
  IF CAN-DO(i-value,"off":U) THEN DO:  /* debug=off? */
    ASSIGN debug-options = "".
    IF i-debug-cookie <> "" THEN
      delete-cookie("WSDebug":U,?,?).  /* delete cookie if there is one */
  END.

  /* Else, if debug=option1,option2 etc. was specified, turn debugging on? */
  ELSE IF i-value <> "" THEN DO:
    IF CAN-DO(i-value,"on":U) THEN     /* debug=on? */
      ASSIGN debug-options = "all":U.
    /* Else, assign specified values */
    ELSE
      ASSIGN debug-options = i-value.
    /* Set the debug Cookie if different than debug options or not set */
    IF debug-options <> i-debug-cookie THEN
      set-cookie ("WSDebug":U, debug-options, ?, ?, ?, ?, ?).
  END.

  /*
  ** Set global variables HostURL, AppURL and SelfURL so self-referencing
  ** URL's cat be generated by applications.
  */

  /* If the Host: header (HTTP_HOST) was sent by the browser, using it will
     provide for fewer problems with self-referencing URL's than
     SERVER_NAME and SERVER_PORT. */
  ASSIGN v-http-host = get-cgi("HTTP_HOST":U).
  IF v-http-host = "" THEN
    /* No Host: header was sent by the browser. */
    ASSIGN v-host = SERVER_NAME
           v-port = SERVER_PORT.
  ELSE IF NUM-ENTRIES(v-http-host, ":":U) = 2 THEN
    /* Host: hostname:port combination was sent by the browser */
    ASSIGN v-host = ENTRY(1, v-http-host, ":":U)
           v-port = ENTRY(2, v-http-host, ":":U).
  ELSE
    /* Else Host: hostname with no port number was sent by the browser */
    ASSIGN v-host = v-http-host
           v-port = SERVER_PORT.
  /* Set the scheme, host and port of the URL to ourself.  Omit
     port if 80 or 443 if https is on. */
  IF HTTPS = "ON" THEN
    ASSIGN HostURL = (IF v-host = "" THEN ""
                      ELSE "https://":U + v-host +
                           (IF v-port = "443":U THEN "" ELSE ":":U + v-port)).
  ELSE
    ASSIGN HostURL = (IF v-host = "" THEN ""
                      ELSE "http://":U + v-host +
                           (IF v-port = "80":U THEN "" ELSE ":":U + v-port)).

  ASSIGN
    /* Server-relative URL to ourself (this program) except for optional
       QUERY_STRING. */
    SelfURL = SCRIPT_NAME + PATH_INFO.

  /* Check for alternate URL format used by the Messengers */
  IF PATH_INFO BEGINS "/WService=":U THEN
    ASSIGN
      /* Web object filename is everything after the second "/" in PATH_INFO */
      AppProgram = (IF NUM-ENTRIES(PATH_INFO, "/":U) >= 3
                    THEN SUBSTRING(PATH_INFO, INDEX(PATH_INFO, "/":U, 2) + 1)
                    ELSE "")
      /* Server relative URL of this Web objects's application */
      AppURL     = SCRIPT_NAME + "/":U + ENTRY(2, PATH_INFO, "/":U).

  ELSE
    ASSIGN
      /* Web object filename is everything after the second "/" in PATH_INFO */
      AppProgram = SUBSTRING(PATH_INFO, 2)
      /* Server relative URL of this Web objects's application */
      AppURL     = SCRIPT_NAME.

  /* If the ApplicationURL option was set in the Windows Registry or
     webspeed.cnf, then use that to set AppURL instead of SCRIPT_NAME and
     PATH_INFO.  Make sure it's prefixed with a "/" since we don't handle
     an entire URL. */
  IF cfg-appurl BEGINS "/":U THEN
    ASSIGN
      AppURL = cfg-appurl
      SelfURL = AppURL + "/":U + AppProgram.

  /* The Alibaba 2.0 NT server upper cases SCRIPT_NAME and PATH_INFO.  This
     is a bug.  To work around this, lower case AppURL, etc.  Otherwise
     Cookies (which are case sensitive) will fail to match preventing
     locking from working . */
  IF SERVER_SOFTWARE BEGINS "Alibaba/2":U THEN
    ASSIGN
      HostURL = LC(HostURL)
      AppURL = LC(AppURL)
      SelfURL = LC(SelfURL)
      AppProgram = LC(AppProgram).
  IF SERVER_SOFTWARE BEGINS "Netscape-":U OR SERVER_SOFTWARE BEGINS "FrontPage-PWS":U THEN
    ASSIGN http-newline = "~n":U.
  ELSE
    ASSIGN http-newline = "~r~n":U.

  /* Set Cookie defaults from either configuration defaults or AppURL */
  ASSIGN
    CookiePath = (IF cfg-cookiepath <> "" THEN cfg-cookiepath ELSE AppURL)
    CookieDomain = cfg-cookiedomain.
    
END PROCEDURE.  /* init-request */


PROCEDURE init-session :
/*---------------------------------------------------------------------------
Procedure:   init-session
Description: Initializes PROGRESS session variables from the environment. 
Input:       <none>
Output:      Sets global variables defined in src/web/method/cgidefs.i
Notes:
             These values should be the default values on a WEB-based client.
             (But it never hurts to make sure.)
             
----------------------------------------------------------------------------*/
  /* Never pause for user input. */
  PAUSE 0 BEFORE-HIDE.
  ASSIGN SESSION:SYSTEM-ALERT-BOXES = no
         SESSION:APPL-ALERT-BOXES   = no
         .
&IF "{&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
    ASSIGN
     cfg-environment = &if "{&development_mode}" = "true" &then "Development" &else "Production" &endif
     cfg-eval-mode = false
     cfg-debugging = "false"
     cfg-appurl = GetAgentSetting("appURL")
     cfg-cookiepath = GetAgentSetting("cookiePath")
     cfg-cookiedomain = GetAgentSetting("cookieDomain")
     RootURL          = GetAgentSetting("wsRoot":U)
    . 
&ELSE
 /* Get configuration settings from ubroker.properties */
  ASSIGN
    cfg-environment  = IF PROVERSION BEGINS "3" OR PROVERSION BEGINS "9" THEN WEB-CONTEXT:GET-CONFIG-VALUE("srvrAppMode":U) ELSE WEB-CONTEXT:GET-CONFIG-VALUE("ENVIRONMENT":U)
    cfg-eval-mode    = check-agent-mode("Evaluation") /* TRUE if eval mode */
    cfg-debugging    = IF PROVERSION BEGINS "3" OR PROVERSION BEGINS "9" THEN WEB-CONTEXT:GET-CONFIG-VALUE("srvrDebug":U) ELSE WEB-CONTEXT:GET-CONFIG-VALUE("Debugging":U)
    cfg-appurl       = WEB-CONTEXT:GET-CONFIG-VALUE("applicationURL":U)
    cfg-cookiepath   = WEB-CONTEXT:GET-CONFIG-VALUE("defaultCookiePath":U)
    cfg-cookiedomain = WEB-CONTEXT:GET-CONFIG-VALUE("defaultCookieDomain":U)
    RootURL          = WEB-CONTEXT:GET-CONFIG-VALUE("wsRoot":U)

    .
&ENDIF
  /* If in Production mode and debugging is not enabled or debugging is
     disabled, then set flag to disable debugging. */
  IF (check-agent-mode("Production":U) AND
    NOT CAN-DO(cfg-debugging, "Enabled":U)) OR
    CAN-DO(cfg-debugging, "Disabled":U) OR
	CAN-DO(cfg-debugging, "Off":U) THEN
    ASSIGN debugging-enabled = FALSE.

END PROCEDURE.


PROCEDURE init-variables :
/*---------------------------------------------------------------------------
Procedure:   init-variables
Description: Initializes PROGRESS variables from the environment
Input:       Environment variables
Output:      Sets global variables defined in src/web/method/cgidefs.i
----------------------------------------------------------------------------*/
  DEFINE VARIABLE i-field AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i-pair  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i-value AS CHARACTER NO-UNDO.
  DEFINE VARIABLE asc-del AS CHARACTER NO-UNDO
    INITIAL "~377":U.   /* delimiter character in octal = CHR(255) */
  DEFINE VARIABLE hex-del AS CHARACTER NO-UNDO
    INITIAL "%FF":U.    /* delimiter character in encoded hex */
  DEFINE VARIABLE ix      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE eql     AS INTEGER   NO-UNDO.

&IF "{&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
DEFINE VAR cgicount          AS INTEGER NO-UNDO.
DEFINE VAR cgipair           AS CHAR NO-UNDO.
DEFINE VAR pairname          AS CHAR NO-UNDO.
DEFINE VAR pairvalue         AS CHAR NO-UNDO.
 
ASSIGN output-content-type = "".

FOR EACH tt-cgi:
    DELETE tt-cgi.
END.

DO cgicount = 1 to NUM-ENTRIES(web-current-env,"~377") - 1:
    ASSIGN cgiPair = ENTRY(cgicount,web-current-env,"~377").
    ASSIGN
     pairName = ENTRY(1,cgiPair,"=")
     pairValue = (IF NUM-ENTRIES(cgiPair,"=") > 1 THEN SUBSTRING(cgiPair,INDEX(cgiPair,"=") + 1,-1) ELSE "")
    .
    CREATE tt-cgi.
    ASSIGN
     tt-cgi.cgiName = pairName
     tt-cgi.cgiValue = pairValue
    .
END.
&ELSE
  /* Global variables to initialize with each request */
  ASSIGN
    CgiVar              = ""  
    CgiList             = ""  
    output-content-type = ""
    SelDelim            = ",":U.

  /* Read in the CGI environment variable pairs which are delimited by 
     ASCII 255 characters.  Any literal ASCII 255 values have been encoded
     as hexidecimal %FF in the same manner as URL encoding. */
  DO ix = 1 TO NUM-ENTRIES({&WEB-CURRENT-ENVIRONMENT},asc-del):
    ASSIGN
      i-pair     = ENTRY(ix,{&WEB-CURRENT-ENVIRONMENT},asc-del)
      eql        = INDEX(i-pair,"=":U)
      i-field    = SUBSTRING(i-pair,1,eql - 1,"RAW":U)
      CgiVar[ix] = REPLACE(SUBSTRING(i-pair,eql + 1,-1,"RAW":U),hex-del,asc-del)
      CgiList    = CgiList + (IF CgiList = "" THEN "" ELSE ",":U ) + i-field.
  END.

&ENDIF
  /* Import CGI 1.1 variables into global variables */
  ASSIGN
    GATEWAY_INTERFACE       = get-cgi("GATEWAY_INTERFACE":U)
    SERVER_SOFTWARE         = get-cgi("SERVER_SOFTWARE":U)
    SERVER_PROTOCOL         = get-cgi("SERVER_PROTOCOL":U)
    SERVER_NAME             = get-cgi("SERVER_NAME":U)
    SERVER_PORT             = get-cgi("SERVER_PORT":U)
    REQUEST_METHOD          = get-cgi("REQUEST_METHOD":U)
    SCRIPT_NAME             = get-cgi("SCRIPT_NAME":U)
    PATH_INFO               = get-cgi("PATH_INFO":U)
    PATH_TRANSLATED         = get-cgi("PATH_TRANSLATED":U)
    QUERY_STRING            = get-cgi("QUERY_STRING":U)
    REMOTE_ADDR             = get-cgi("REMOTE_ADDR":U)
    REMOTE_HOST             = get-cgi("REMOTE_HOST":U)
    REMOTE_IDENT            = get-cgi("REMOTE_IDENT":U)
    REMOTE_USER             = get-cgi("REMOTE_USER":U)
    AUTH_TYPE               = get-cgi("AUTH_TYPE":U)
    REMOTE_IDENT            = get-cgi("REMOTE_IDENT":U)
    CONTENT_TYPE            = get-cgi("CONTENT_TYPE":U)
    CONTENT_LENGTH          = INTEGER(get-cgi("CONTENT_LENGTH":U)) NO-ERROR.

  /* Import some HTTP variables into global variables */
  ASSIGN
    HTTP_ACCEPT             = get-cgi("HTTP_ACCEPT":U)
    HTTP_COOKIE             = get-cgi("HTTP_COOKIE":U)
    HTTP_REFERER            = get-cgi("HTTP_REFERER":U)
    HTTP_USER_AGENT         = get-cgi("HTTP_USER_AGENT":U)
    HTTPS                   = get-cgi("HTTPS":U).

  /* Test for Microsoft's IIS which doesn't use HTTPS ON/OFF*/
  IF SERVER_SOFTWARE BEGINS "Microsoft-IIS/":U AND
    get-cgi("SERVER_PORT_SECURE":U) = "1":U THEN
    ASSIGN HTTPS = "ON":U.

  /* Other environment variables */
  ASSIGN
    utc-offset              = WEB-CONTEXT:UTC-OFFSET.

  /* If SERVER_PORT is null, then set it 80 or 443 if HTTPS is ON */
  IF SERVER_PORT = "" THEN
    ASSIGN SERVER_PORT = (IF HTTPS = "ON":U THEN "443":U ELSE "80":U).

END PROCEDURE.  /* init-variables */


PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Destroy Web object, if any, before destroying this-procedure. 
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  RUN adm-destroy NO-ERROR.
  ASSIGN web-utilities-hdl = ?.
END PROCEDURE.



/*******************************************************************************
 *                     ONLY MAPPED WEBOBJECT JUNK BELOW                        *
 ******************************************************************************/

&if "{&MAPPEDOBJECTS}" = "YES" &then
PROCEDURE adm-delete-tagmap-utilities :
/*------------------------------------------------------------------------------
  Purpose:     Delete any tagmap utility procedures as well as the tagmap
               records.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Remove any existing tagmap records and persistent utilities. */
  FOR EACH tagmap:
    /* Delete the persistent process. */
    IF VALID-HANDLE(tagmap.util-Proc-Hdl) THEN
      DELETE PROCEDURE tagmap.util-Proc-Hdl.
    /* Now the record can be deleted. */
    DELETE tagmap.
  END.

END PROCEDURE.


PROCEDURE adm-reset-tagmap-utilities :
/*------------------------------------------------------------------------------
  Purpose:     Load the tagmap.dat file and create entries in the tagmap temp-
               table. Run the procedures associated with each one of these
               files.              
  Parameters:  <none>
  Notes:       Any existing tagmap records are first deleted.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE next-line   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE tagmapfile  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE rSearchFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i-count     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE num-ent     AS INTEGER   NO-UNDO.
  
  DEFINE BUFFER xtagmap FOR tagmap.
  
  /* Remove any existing tagmap records and persistent utilities. */
  RUN dispatch IN THIS-PROCEDURE ('delete-tagmap-utilities':U) NO-ERROR.
  
  /* Make sure the tagmap.dat file exists in the PROPATH */
  ASSIGN tagmapfile = SEARCH({&tagMapFileName})
         i-count    = 0.
  IF tagmapfile = ? THEN DO:
    logNote("Error","The file '" + {&tagMapFileName} + "' was not in your PROPATH").
    RETURN ERROR.
  END.
  INPUT STREAM tagMapStream FROM VALUE(tagmapfile) NO-ECHO.

  REPEAT ON ENDKEY UNDO, LEAVE:
    /* Clear variable before read to handle blank lines. */
    ASSIGN next-line = "". 
    IMPORT STREAM tagMapStream UNFORMATTED next-line.
    
    IF LENGTH(next-line,"CHARACTER":U) > 4 AND
      SUBSTRING(next-line,1,1,"CHARACTER":U) <> "#":U THEN DO:
      CREATE tagmap.
      ASSIGN
        i-count               = i-count + 1
        num-ent               = NUM-ENTRIES(next-line)
        tagmap.i-order        = i-count
        tagmap.htm-Tag        = ENTRY(1,next-line)
        tagmap.htm-Type       = (IF num-ent >= 3 THEN 
                                   ENTRY(3,next-line) ELSE "":U)
        tagmap.psc-Type       = (IF num-ent >= 4 
                                   THEN ENTRY(4,next-line) ELSE "":U)
        tagmap.util-Proc-Name = (IF num-ent >= 5 
                                   THEN ENTRY(5,next-line) ELSE "":U)
        .
      
      /* We allow for empty utility procedures. */
      IF tagmap.util-Proc-Name ne "":U THEN DO:
        /* If there another tagmap that is already running this procedure? */
        FIND FIRST xtagmap WHERE xtagmap.util-Proc-Name eq tagmap.util-Proc-Name
                             AND RECID(xtagmap) ne RECID(tagmap) NO-ERROR.
        IF AVAILABLE (xtagmap) AND VALID-HANDLE(xtagmap.util-Proc-Hdl) THEN
          tagmap.util-Proc-Hdl = xtagmap.util-Proc-Hdl.
        ELSE DO:
          /* Check that the file exists. */
          RUN adecomm/_rsearch.p (INPUT tagmap.util-Proc-Name, OUTPUT rSearchFile).
          IF rSearchFile ne ? THEN DO:
            RUN VALUE(rSearchFile) PERSISTENT SET tagmap.util-Proc-Hdl NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
              tagmap.util-Proc-Hdl = ?.
              &IF "{&OPSYS}" = "MSDOS" &THEN
                MESSAGE tagmap.util-Proc-Name "could not run." VIEW-AS ALERT-BOX.
              &ELSE 
                RUN HtmlError (SUBSTITUTE ("Unable to run Tagmap Utility file '&1'", 
                                           tagmap.util-Proc-Name )).
              &ENDIF
            END. /* IF...ERROR... */
          END. /* IF rSearchFile ne ?... */
          ELSE DO:
            &IF "{&OPSYS}" = "MSDOS" &THEN
              MESSAGE tagmap.util-Proc-Name "not found." VIEW-AS ALERT-BOX.
            &ELSE 
              RUN HtmlError (SUBSTITUTE ("Unable to find Tagmap Utility file '&1'", 
                                          tagmap.util-Proc-Name )).
            &ENDIF
          END. /* IF rSearchFile ...eg ?... */
        END. /* IF <not> AVAILABLE (xtagmap)... */
      END. /* IF...util-Proc-Name ne ""... */
     END. /* IF LENGTH... */
  END. /* REPEAT... */
  
  /* Close the tagmap stream. */
  INPUT STREAM tagMapStream CLOSE.
  
END PROCEDURE.
&endif



