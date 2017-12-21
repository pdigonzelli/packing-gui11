/*-----------------------------------------------------------------------*
  File........: ffwebstate.i
  Version.....: Not yet assigned (8/3/2000)
  Description : Method library for state-keeping mechanisms
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  United Systems, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000 - http://www.freeframework.org
  Created.....: 6/20/1998
  Notes.......: This file is heavily commented.  The best way to see what's 
  				going on here is to print this out and study the comments.
				
				If you make changes to this program that others could benefit
				from, please share your code with me: ses@usiatl.com

/*  Mario says it needs some sort of trigger to fire custom code upon session expiration*/		

SES:  Also needs more comments and documentation.  Consider making a variable for stateinfo.  
  Probably should move DomainName() to seperate program.		
----------------------------------------------------------------------*/

FUNCTION lockSession RETURNS CHARACTER () FORWARD.
FUNCTION unlockSession RETURNS CHARACTER () FORWARD.

&IF DEFINED(FFWEB_I) = 0 &THEN
{lib/ffweb.i}
&ENDIF
{lib/surffuncs.i}

&GLOBAL-DEFINE FFWEBSTATE_I YES
&GLOBAL-DEFINE STATEDEBUG FALSE

&GLOBAL-DEFINE stateInfo WebSession.WebSessionID + "|" + string(thisWebHit.WebHitID)
&GLOBAL-DEFINE urlstateInfo "stateInfo=" + {&stateInfo}
&GLOBAL-DEFINE hiddenStateInfo '<INPUT TYPE="HIDDEN" NAME="stateInfo" ID="stateInfo" VALUE="' + {&stateInfo} + '">'


DEFINE BUFFER LastWebHit for WebHit.
DEFINE BUFFER ThisWebHit for WebHit.

DEFINE VAR securemode    AS LOGICAL NO-UNDO.
DEFINE VAR stateinfo     AS CHAR    NO-UNDO.
DEFINE VAR cookieinfo	 AS CHAR    NO-UNDO.
DEFINE VAR thisrowid     AS ROWID   NO-UNDO.
DEFINE VAR queuedlognote AS CHAR    NO-UNDO.

FUNCTION HandleIPChange  RETURNS CHARACTER () FORWARD.
FUNCTION StartNewSession RETURNS CHARACTER () FORWARD.
FUNCTION DomainName      RETURNS CHARACTER (INPUT myurl AS CHAR) FORWARD.
FUNCTION lockSession     RETURNS CHARACTER () FORWARD.
FUNCTION unLockSession   RETURNS CHARACTER () FORWARD.
FUNCTION KillOldSessions RETURNS LOGICAL   () FORWARD.
FUNCTION logNote         RETURNS LOGICAL   (INPUT notetype AS CHAR, INPUT mynote AS CHAR) FORWARD.

FUNCTION DomainName RETURNS CHARACTER (
INPUT myurl AS CHAR
) :
/*------------------------------------------------------------------------------
  Returns:     The name of the domain from a URL passed in.
  Parameters:  myURL = the web address to parse.
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR thisname AS CHAR NO-UNDO.
ASSIGN    
 myurl = REPLACE(myurl,"http://","")
 myurl = REPLACE(myurl,"https://","")
 thisNAME = ENTRY(1,myurl,"/")    
.

  /* Return a value for the function. */
  RETURN thisname.
END FUNCTION.

FUNCTION HandleIPChange RETURNS CHARACTER () :
/*------------------------------------------------------------------------------
  Returns:     
  Parameters:  <none>
  Notes:       Main reason for this function is to determine whether a mid-session 
                IP address change is reason for alarm, or just some glitch caused by
                an overactive proxy out there.  An IP address change where the referrer 
                is suddenly blank or pointing to another domain is a big indicator that
                something fishy is going on, and we need to start a new session.

              HTTP_Referer CGI environment variable could probably be faked, but it's beyond 
               the capability of all but the most sophisticated individuals.  Those would be the
               same individuals with the ability to IP spoof.  We have no cure for that.
------------------------------------------------------------------------------*/

    DEFINE VAR reflist AS CHAR NO-UNDO.
    ASSIGN reflist = GetSetting("*","All","General","ValidSessionReferrers").
	assign queuedlognote = queuedlognote + "IP changed mid-session from " + WebSession.RemoteAddr + " to " + Remote_addr + " with referer of: " + HTTP_referer.
    IF NOT CAN-DO(reflist,DomainName(HTTP_REFERER)) THEN StartNewSession().
    ELSE DO:
		ASSIGN WebSession.RemoteAddr = WebSession.remoteAddr + "," + REMOTE_ADDR.
        SET-USER-FIELD("brandNewSession","Yes").
	END.
  /* Return a value for the function. */
  RETURN "".
END FUNCTION.


FUNCTION KillOldSessions RETURNS LOGICAL () :
/*------------------------------------------------------------------------------
  Returns:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER OldWebSession for WebSession.
    DEFINE BUFFER OldWebHit for WebHit.
    DEFINE VAR stoptime      AS INTEGER.
    DEFINE VAR timeoutperiod AS INTEGER NO-UNDO.
    DEFINE VAR archivedays   AS INTEGER NO-UNDO.
    DEFINE VAR rightnow      AS INTEGER NO-UNDO.
	DEFINE VAR inactivecutoff AS INTEGER NO-UNDO.
	DEFINE VAR archivecutoffdate AS DATE NO-UNDO.
	
    ASSIGN
     stoptime      = ETIME + 1000
     timeoutperiod = INTEGER(GetSetting("*","All","General","SessionTimeOutPeriod"))
     archivedays   = MAX(INTEGER(GetSetting("*","All","General","SessionArchiveDays")),5)
     rightnow      = TIME
	 archivecutoffdate = TODAY + 2 - archivedays
	 inactivecutoff = rightnow - timeoutperiod
    .
    IF timeoutperiod = ? OR timeoutperiod < 300 THEN ASSIGN timeoutperiod = 300. /*DEFAULT TO 5 MINUTES*/

     FOR EACH OldWebSession
      WHERE OldWebSession.LastDate < archivecutoffdate + 1
      OR (OldWebSession.LastDate < archivecutoffdate
         AND OldWebSession.LastTime < rightnow)  
         /*DELETE A FEW EACH HOUR TO KEEP FIRST HIT OF THE DAY FROM GETTING SOCKED*/
     EXCLUSIVE-LOCK TRANSACTION: 
        FOR EACH OldWebHit OF OldWebSession
         EXCLUSIVE-LOCK:
            DELETE OldWebHit.
        END. 
        DELETE OldWebSession.
        IF ETIME > stoptime THEN LEAVE. /*Only loop for about a second, MAX, so we don't clobber the first sucker that comes along...*/
    END. /*each oldWebSession TRANSACTION*/

 RETURN ?.
END FUNCTION.

FUNCTION lockSession RETURNS CHARACTER () :
FIND CURRENT WebSession EXCLUSIVE-LOCK.
FIND CURRENT ThisWebHit EXCLUSIVE-LOCK.
  /* Return a value for the function. */
  RETURN "".
END FUNCTION.


FUNCTION logNote RETURNS LOGICAL (
 INPUT notetype AS CHAR, 
 INPUT mynote AS CHAR
) :
IF AVAIL thisWebHit THEN DO:
    lockSession().
    ASSIGN thisWebHit.LogNote = thisWebHit.LogNote + "~n [" + notetype + "] " + mynote.
    unLockSession().
    RETURN TRUE.
END.
ELSE RETURN FALSE.
END FUNCTION.


FUNCTION StartNewSession RETURNS CHARACTER () :
/*------------------------------------------------------------------------------
  Returns:     
  Parameters:  <none>
  Notes:       Date and time are included in the random string just to keep Progress 
               from generating the same random numbers each time the agents are started.
------------------------------------------------------------------------------*/
DEFINE VAR surferid AS CHAR NO-UNDO.
DEFINE VAR newsessionid AS CHAR NO-UNDO.
ASSIGN newsessionid = ENCODE(STRING(TIME) + STRING(TODAY) + STRING(RANDOM(1000,9999)) + STRING(ETIME)) + STRING(RANDOM(1000,9999)).
    DO TRANSACTION:
        IF NOT CAN-FIND(WebSession WHERE WebSession.WebSessionID = newsessionid)
         THEN CREATE WebSession.
        ELSE DO: /*WebSession randomly chosen already existed*/
            StartNewSession().  /*Call recursively until we find one not in use*/
            ErrorLog("Picked a random WebSessionID that already existed: " + newsessionid).
            ASSIGN queuedlognote = queuedlognote + "~n Picked random WebSessionID that already existed - " + newsessionid.
            RETURN "".
            /*This has the potential to blow the stack if something goes wrong, 
             but it should almost NEVER run.  In fact, if it runs, 
             it would almost seem to indicate an error in other code, 
             but the logged error message might help us find where.*/
        END.  /*WebSession randomly chosen already existed*/
		
		/*NOW ASSIGN THIS SESSION A SURFER*/
		ASSIGN surferid = REPLACE(GET-COOKIE("ValuedGuest"),"Yes|","").
		/*IF NOT IN COOKIE, TRY QUERY_STRING*/
		IF surferid = "" THEN ASSIGN surferID = GET-FIELD("GuestID").
		IF surferid ne "" THEN FIND webSurfer
		 WHERE webSurfer.WebSurferID = surferID
		 EXCLUSIVE-LOCK NO-ERROR.
		IF NOT AVAIL webSurfer THEN DO:
			ASSIGN surferid = STRING(NEXT-VALUE(webSurferSeq),"999999") + ENCODE(STRING(TIME) + STRING(TODAY) + STRING(RANDOM(1000,9999)) + STRING(ETIME)).
			IF CAN-FIND(WebSurfer WHERE WebSurfer.WebSurferID = surferid) THEN DO:
				startNewSession(). /*handle cases where it chose the same as one that existed - RARE or never*/
				RETURN "".
			END.
			CREATE webSurfer.
			ASSIGN
			 webSurfer.WebSurferID = surferID
			 { lib/create.i &tablename="webSurfer" }
			.
		END.
		
        ASSIGN
         WebSession.WebSessionID = newsessionid
         WebSession.WebUser = ?
         WebSession.RemoteAddr = Remote_Addr
         WebSession.WebHitCount = 1
         WebSession.StartDate = TODAY
         WebSession.LastDate = WebSession.StartDate
         WebSession.StartTime = TIME
         WebSession.LastTime = WebSession.StartTime
         WebSession.SessionReferrer = HTTP_REFERER
         WebSession.SessionUserAgent = HTTP_USER_AGENT
		 WebSession.WebSurferID = WebSurfer.WebSurferID
		 WebSurfer.LastWebSessionID = WebSurfer.CurrentWebSessionID
		 WebSurfer.CurrentWebSessionID = WebSession.WebSessionID
		 WebSurfer.chg-dt = TODAY
		 WebSurfer.chg-userid = "Login"
		 WebSurfer.chg-time = TIME
        .
		setSurferAttribute("PreviousVisitDateTime",getSurferAttribute("LastVisitDateTime"),?).
		setSurferAttribute("LastVisitDateTime",STRING(TODAY,"99/99/9999") + " - " + STRING(time,"hh:mm:ss am"),?).
        CREATE LastWebHit.
        ASSIGN
         LastWebHit.WebSessionID = WebSession.WebSessionID
         LastWebHit.WebHitID = 0
         LastWebHit.QueryString = ""
         LastWebHit.RequestMethod = "GET"
         LastWebHit.WebState = "Starting"
         LastWebHit.HitDate = TODAY
         LastWebHit.HitTime = TIME
         LastWebHit.HitReferrer = HTTP_REFERER
         LastWebHit.HitCookies = HTTP_COOKIE
        .
        SET-USER-FIELD("brandNewSession","Yes").
		FIND CURRENT webSurfer NO-LOCK.
    END. /*TRANSACTION*/
    ASSIGN stateinfo = WebSession.WebSessionID + "|" + STRING(LastWebHit.WebHitID).
    SET-USER-FIELD("stateInfo",stateinfo).
  /* Return a value for the function. */
  RETURN "".
END FUNCTION.


FUNCTION unLockSession RETURNS CHARACTER () :
	ASSIGN thisrowid = ROWID(WebSession).
	RELEASE WebSession NO-ERROR.
	FIND WebSession WHERE ROWID(WebSession) = thisrowid NO-LOCK.
	ASSIGN thisrowid = ROWID(ThisWebHit).
	RELEASE ThisWebHit NO-ERROR.
	FIND ThisWebHit WHERE ROWID(ThisWebHit) = thisrowid NO-LOCK.
	
	/*------------------------------------------------------------------
	 *   Now we have
	 *     WebSession - No Lock
	 *     LastWebHit - No Lock
	 *     ThisWebHit - No Lock
	 *--------------------------------------------------------------------*/

	RETURN "".
END FUNCTION.


PROCEDURE ffw-Output-Header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 &if "{&noheader}" ne "yes" &then
  IF GET-USER-FIELD("HeaderSent") ne "Yes" THEN DO:
    IF GET-USER-FIELD("brandNewSession") = "Yes" THEN DO:  
        IF securemode THEN DO:
	    set-cookie ("safeSessionID":U, webSession.webSessionID, today + 2, ?, ?, ?, "secure":U).  
	END.
        ELSE set-cookie ("safeSessionID":U, webSession.webSessionID, today + 2, ?, ?, ?, ?).  
	SET-COOKIE ("valuedGuest":U, "Yes|" + webSurfer.WebSurferID, today + 365, ?, ?, ?, ?).  
		
    END. /*it was a brand new session: set the cookie*/  
	IF REQUEST_METHOD ne "POST" THEN DO:
	    output-http-header("Cache-Control","No-Cache").
        /* These would be BAD, very BAD!!!*/
	    /* output-http-header("Pragma","No-Cache").*/
        /* output-http-header("Expires","0"). */
	END.
    output-content-type ("text/html":U).
    SET-USER-FIELD("HeaderSent","Yes").
  END.
&else 
	assign e4gl-options = "no-content-type".
&endif
END PROCEDURE.


PROCEDURE Get-Last-State :
/*------------------------------------------------------------------------------
  Purpose:   Using the unique identifier stored in form data, query string or cookie, 
                pull the state information out of database and resume the user's session  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR ce AS INTEGER INIT 1 NO-UNDO.
DEFINE VAR te AS INTEGER NO-UNDO.
DEFINE VAR timeoutperiod AS INTEGER NO-UNDO.
DEFINE VAR rightnow      AS INTEGER NO-UNDO.
DEFINE VAR inactivecutoff AS INTEGER NO-UNDO.
	
ASSIGN
 stateinfo = GET-VALUE("stateInfo")
 cookieinfo = GET-COOKIE("SafeSessionID")
 timeoutperiod = INTEGER(GetSetting("*","All","General","SessionTimeOutPeriod"))
 rightnow      = TIME
 inactivecutoff = rightnow - timeoutperiod
.
KillOldSessions().
IF {&STATEDEBUG} THEN queue-message("steve","StateInfo is: " + stateinfo). 
IF {&STATEDEBUG} THEN queue-message("steve","RemoteAddr is: " + Remote_Addr). 
IF {&STATEDEBUG} THEN queue-message("steve","HTTPREFERER is: " + HTTP_REFERER). 
DO TRANSACTION:
IF stateinfo = "" THEN DO:  /*CHECK THE COOKIE*/
    ASSIGN
     stateinfo = cookieInfo
     ce = NUM-ENTRIES(stateinfo)
    .
/*****HAVE TO HANDLE IF MULTIPLE COOKIES OF SAME NAME GET RECEIVED**/
    IF ce > 1 THEN SLOOP: DO te = 1 TO ce:
        IF CAN-FIND(WebSession WHERE WebSession.WebSessionID = ENTRY(te,stateinfo) AND WebSession.SessionActive = TRUE)
         THEN DO: /*PICK THIS ONE - IT'S VALID*/
            ASSIGN stateinfo = ENTRY(te,stateinfo).
            LEAVE SLOOP.
        END. /*PICKED THIS ONE*/
    END. 
/*******END - HANDLING MULTIPLE COOKIES OF SAME NAME****************/
    IF stateinfo NE "" AND GET-VALUE("StateReset") NE "YES"
     THEN DO: /*THERE WAS A COOKIE*/
		FIND WebSession
		 WHERE WebSession.WebSessionID = stateinfo
		 EXCLUSIVE-LOCK NO-ERROR.
		IF AVAIL WebSession AND WebSession.SessionActive THEN DO:
		    IF ((WebSession.LastDate = TODAY
			 AND WebSession.LastTime >= inactivecutoff)
			 OR
			 (TIME < timeoutperiod
			  AND WebSession.LastDate = TODAY - 1
			  AND LastTime >= 86400 - (timeoutperiod - TIME)))
		     THEN DO: /*NOT TIMED OUT*/
	            IF can-do(WebSession.RemoteAddr,REMOTE_ADDR) THEN DO:
	                IF GET-USER-FIELD("WebHitCountIncremented") ne "Yes" THEN DO:            
	                    ASSIGN            
	                     WebSession.WebHitCount = WebSession.WebHitCount + 1 /*Increment once on each actual hit*/            
	            	     WebSession.LastTime = TIME            
	            	     WebSession.LastDate = TODAY            
	                    .            
	                    SET-USER-FIELD("WebHitCountIncremented","Yes").            
	                END. /*Had to increment the webhitcount*/        
	            END. /*IP Address matched*/
	            ELSE DO: /*IP address did not match*/
	                HandleIPChange().
	            END. /*IP Address did not match*/
	            FIND LAST LastWebHit                
	             OF WebSession                
	             NO-LOCK NO-ERROR.          
	            IF NOT AVAIL lastwebhit THEN DO:    
	                IF {&STATEDEBUG} THEN QUEUE-MESSAGE("Steve","Had to start a new session because we couldn't find your last hit").    
	                StartNewSession().       
	            END. /*lastwebhit not found*/   
	    	END. /*WebSession NOT TIMED OUT*/
	    	ELSE DO:  /*WebSession TIMED OUT*/
		        IF {&STATEDEBUG} THEN QUEUE-MESSAGE ("Steve","Your session timed out for your protection because of inactivity."). 
		        assign webSession.sessionActive = false.
				StartNewSession(). 
			END. /*webSession TIMED OUT*/
	    END. /*WebSession available using cookie*/
		ELSE StartNewSession().
    END. /*THERE WAS A COOKIE*/
	ELSE StartNewSession().  
END. /*Checked the cookie for state info*/
ELSE DO:
    FIND WebSession    
     WHERE WebSession.WebSessionID = ENTRY(1,stateinfo,"|") 
     EXCLUSIVE-LOCK NO-ERROR.    
    IF NOT AVAIL WebSession OR WebSession.SessionActive = false THEN DO:
		assign queuedlognote = queuedlognote + "Couldn't find session with query string stateInfo.".
		if locked WebSession then assign queuedlognote = queuedlognote + "Couldn't find session because it was locked.".
        StartNewSession().    
        IF {&STATEDEBUG} THEN QUEUE-MESSAGE("STEVE","COULDN'T FIND SESSION.").
    END. /*NOT AVAIL WEBSESSION*/
    ELSE DO:
		IF NOT ((WebSession.LastDate = TODAY
		 AND WebSession.LastTime >= inactivecutoff)
		 OR
		 (TIME < timeoutperiod
		  AND WebSession.LastDate = TODAY - 1
		  AND LastTime >= 86400 - (timeoutperiod - TIME)))
	     THEN DO:
            QUEUE-MESSAGE("Steve","Your session timed out for your protection because of inactivity.").
            StartNewSession().
        END.
        IF not can-do(WebSession.RemoteAddr,REMOTE_ADDR) THEN HandleIPChange().
    END. 
    IF GET-USER-FIELD("WebHitCountIncremented") ne "Yes" THEN DO:
        ASSIGN
         WebSession.WebHitCount = WebSession.WebHitCount + 1 /*Increment once on each actual hit*/
	 	 WebSession.LastTime = TIME
	 	 WebSession.LastDate = TODAY
        .
        SET-USER-FIELD("WebHitCountIncremented","Yes").
    END. /*Had to increment the webhitcount*/   
    FIND LastWebHit    
     OF WebSession    
     WHERE LastWebHit.WebHitID = INTEGER(ENTRY(2,stateinfo,"|"))    
     NO-LOCK NO-ERROR.    
       
    IF NOT AVAIL LastWebHit THEN RETURN.
END. /*There was stateinfo from the client*/

/*---------At this point, we should have both WebSession and LastWebHit------*/
FIND ThisWebHit
 WHERE ThisWebHit.WebSessionID = WebSession.WebSessionID
 AND   ThisWebHit.WebHitID = WebSession.WebHitCount
 EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAIL ThisWebHit THEN DO:
    CREATE ThisWebHit.
    ASSIGN
     ThisWebHit.WebSessionID = WebSession.WebSessionID
     ThisWebHit.WebHitID = WebSession.WebHitCount
     ThisWebHit.PrevWebHitID = LastWebHit.WebHitID /*To make a chain*/
     ThisWebHit.CurrRowID = LastWebHit.CurrRowID
     ThisWebHit.HitDate = TODAY
     ThisWebHit.HitTime = TIME
     ThisWebHit.HitReferrer = HTTP_REFERER
     ThisWebHit.HitCookies = HTTP_COOKIE
     ThisWebHit.RequestMethod = REQUEST_METHOD
    .
ASSIGN
 ThisWebHit.QueryString = (IF ThisWebHit.RequestMethod = "GET" THEN QUERY_STRING ELSE WEB-CONTEXT:FORM-INPUT)
.
END.
DO:
    ASSIGN
     ThisWebHit.ObjectRun = TRIM(ThisWebHit.ObjectRun + " " + "{&WEB-FILE}")
     ThisWebHit.LogNote = ThisWebHit.LogNote + (IF queuedlognote NE "" THEN "~n [Advisory] " + queuedlognote ELSE "")
    .
	IF {&STATEDEBUG} THEN queue-message("steve","cookieInfo is: " + cookieinfo).
	IF (WebSession.CookiesEnabled and WebSession.WebHitCount > 2
	 AND cookieInfo = "")
	 OR (cookieInfo ne "" and cookieInfo ne WebSession.WebSessionID)
	 THEN SET-USER-FIELD("brandnewsession","yes"). /*refresh the cookie*/
    IF WebSession.WebHitCount > 1
     AND cookieInfo = "" THEN ASSIGN WebSession.CookiesEnabled = NO.
	ELSE ASSIGN WebSession.CookiesEnabled = YES.
END. 

/*------------------------------------------------------------------
 *   Now we have
 *     WebSession - Exclusive Locked
 *     LastWebHit - No Lock
 *     ThisWebHit - Initialized with Exclusive Lock
 *--------------------------------------------------------------------*/
END. /*TRANSACTION*/

unLockSession().

FIND webSurfer
 WHERE webSurfer.WebSurferId = webSession.WebSurferID
 NO-LOCK NO-ERROR.

/*ADD LOGIC HERE TO CREATE IT IF NOT FOUND*/

/*------------------------------------------------------------------
 *   Now we have
 *     WebSession - No Lock
 *     LastWebHit - No Lock
 *     ThisWebHit - No Lock
 *     WebSurfer  - No Lock
 *--------------------------------------------------------------------*/
END PROCEDURE.


/*That's all folks!*/
 

