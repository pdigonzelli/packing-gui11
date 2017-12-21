/*-----------------------------------------------------------------------*
  File........: surffuncs.i
  Version.....: Not yet assigned (8/3/2000)
  Description : Method library for session and surfer data storage and retrieval
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  United Systems, Inc. (770) 449-9696
  Copyright...: FreeFrameWork 2000 - http://www.freeframework.org
  Created.....: 01/25/2000
  Notes.......: This file is heavily commented.  The best way to see what's 
  				going on here is to print this out and study the comments.
				
				THERE IS ALSO HTML-BASED DOCUMENTATION FOR THESE FUNCTIONS
				
				If you make changes to this program that others could benefit
				from, please share your code with me: ses@usiatl.com

		OTHER FUTURE FUNCTIONS:
 - popSurferAttribute (Remove from the end of the list)
 - decrementSurferAttribute() - substract 1 from the value
 - deleteSessionAttribute()
 - endSession()
 - mergeSurfers(permanent,temporary)
 	* permanent takes precedence
	* temporary one deleted
	* cookie reset
 - lockoutSurfer() - used to keep the surfer from logging in for a give period of time.
 - privatize()
 
----------------------------------------------------------------------*/
FUNCTION getSessionAttribute RETURNS CHAR (
 INPUT myName AS CHAR
) FORWARD.

FUNCTION pushSessionAttribute RETURNS LOGICAL (
 INPUT myName AS CHAR,
 INPUT myValue AS CHAR
) FORWARD.

FUNCTION getSurferAttribute RETURNS CHAR (
 INPUT myName AS CHAR
) FORWARD.

FUNCTION getSessionAttribute RETURNS CHAR (
 INPUT myName AS CHAR
):
	DEFINE BUFFER b-myatt FOR webSessionAttribute.
	
	FIND b-myatt OF webSession
	 WHERE b-myatt.AttributeName = myName
	 NO-LOCK NO-ERROR.
	IF AVAIL b-myatt THEN RETURN b-myatt.AttributeValue.
	ELSE RETURN "".
END FUNCTION. /*getSurferAttribute*/


FUNCTION getSurferAttribute RETURNS CHAR (
 INPUT myName AS CHAR
):
	DEFINE BUFFER b-myatt FOR webSurferAttribute.
	IF NOT AVAIL webSurfer THEN RETURN ?.
	
	FIND b-myatt OF webSurfer
	 WHERE b-myatt.AttributeName = myName
	 NO-LOCK NO-ERROR.
	IF AVAIL b-myatt THEN DO:
		IF b-myatt.expireDate < today
		 AND b-myatt.expireDate ne ?
		 THEN DO TRANSACTION:
		 	FIND CURRENT b-myatt EXCLUSIVE-LOCK.
			DELETE b-myatt.
			RETURN ?.
		END. /*TRANSACTION - Attribute expired, so deleted.*/
		ELSE DO: /*not expired*/
			IF b-myatt.isPrivate
			 AND NOT webSession.SurferVerified
			 AND NOT CAN-DO(getSessionAttribute("PrivateSurferAttributesOK"),myName)
			 THEN RETURN "". /*BLANK IF NOT LOGGED IN AND SET IN A PREVIOUS SESSION*/
			 ELSE RETURN b-myatt.AttributeValue.
		END. /*not expired*/
	END. /*found it*/
	ELSE RETURN "".
END FUNCTION. /*getSurferAttribute*/


FUNCTION setPrivateSurferAttribute RETURNS LOGICAL (
 INPUT myname AS CHAR,
 INPUT myvalue AS CHAR,
 INPUT myexpires AS DATE
):
	DEFINE BUFFER b-myatt FOR webSurferAttribute.
	IF NOT AVAIL webSurfer THEN RETURN ?.
	
		FIND b-myatt OF webSurfer
		 WHERE b-myatt.AttributeName = myname
		 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
		 
		IF NOT AVAIL b-myatt THEN DO:
			CREATE b-myatt.
			ASSIGN
			 b-myatt.WebSurferID = WebSurfer.WebSurferID
			 b-myatt.AttributeName = myName
			 { lib/create.i &tablename="b-myatt" }
			 no-error.
			IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		END.
		ASSIGN
		 b-myatt.AttributeValue = myvalue
		 b-myatt.ExpireDate = myexpires
		 b-myatt.IsPrivate = TRUE
		 no-error.
		IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		ELSE DO:
			IF NOT webSession.SurferVerified THEN DO:
				/*mark that attribute was set during this session*/
				pushSessionAttribute("PrivateSurferAttributesOK",myname).
			END.
			RETURN TRUE.
		END.
END FUNCTION. /*setPrivateSurferAttribute*/


FUNCTION setSessionAttribute RETURNS LOGICAL (
 INPUT myname AS CHAR,
 INPUT myvalue AS CHAR
):
	DEFINE BUFFER b-myatt FOR webSessionAttribute.

		FIND b-myatt OF webSession
		 WHERE b-myatt.AttributeName = myname
		 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
		 
		IF NOT AVAIL b-myatt THEN DO:
			CREATE b-myatt.
			ASSIGN
			 b-myatt.WebSessionID = WebSession.WebSessionID
			 b-myatt.AttributeName = myName
			 { lib/create.i &tablename="b-myatt" }
			 no-error.
			IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		END.
		ASSIGN
		 b-myatt.AttributeValue = myvalue
		 no-error.
		IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		ELSE RETURN TRUE.
END FUNCTION. /*setSessionAttribute*/

FUNCTION setSurferAttribute RETURNS LOGICAL (
 INPUT myname AS CHAR,
 INPUT myvalue AS CHAR,
 INPUT myexpires AS DATE
):
	DEFINE BUFFER b-myatt FOR webSurferAttribute.
	IF NOT AVAIL webSurfer THEN RETURN ?.
	
		FIND b-myatt OF webSurfer
		 WHERE b-myatt.AttributeName = myname
		 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
		 
		IF NOT AVAIL b-myatt THEN DO:
			CREATE b-myatt.
			ASSIGN
			 b-myatt.WebSurferID = WebSurfer.WebSurferID
			 b-myatt.AttributeName = myName
			 { lib/create.i &tablename="b-myatt" }
			 no-error.
			IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		END.
		ASSIGN
		 b-myatt.AttributeValue = myvalue
		 b-myatt.ExpireDate = myexpires
		 no-error.
		IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		ELSE RETURN TRUE.
END FUNCTION. /*setSurferAttribute*/

FUNCTION deleteSessionAttribute RETURNS LOGICAL (
 INPUT myname AS CHAR 
):
	DEFINE BUFFER b-myatt FOR webSessionAttribute.

	FIND b-myatt OF webSession
	 WHERE b-myatt.AttributeName = myname
	 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
	
	IF LOCKED b-myAtt THEN RETURN FALSE.
	IF NOT AVAIL b-myAtt THEN RETURN TRUE.
	ELSE DO:
		DELETE b-myatt NO-ERROR.
		IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		ELSE RETURN TRUE.
	END.
END FUNCTION. /*deleteSessionAttribute*/


FUNCTION deleteSurferAttribute RETURNS LOGICAL (
 INPUT myname AS CHAR 
):
	DEFINE BUFFER b-myatt FOR webSurferAttribute.
	IF NOT AVAIL webSurfer THEN RETURN ?.

	FIND b-myatt OF webSurfer
	 WHERE b-myatt.AttributeName = myname
	 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
	
	IF LOCKED b-myAtt THEN RETURN FALSE.
	IF NOT AVAIL b-myAtt THEN RETURN TRUE.
	ELSE DO:
		DELETE b-myatt NO-ERROR.
		IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		ELSE RETURN TRUE.
	END.
END FUNCTION. /*deleteSurferAttribute*/

FUNCTION incrementSurferAttribute RETURNS INTEGER (
 INPUT myname AS CHAR,
 INPUT myexpires AS DATE
):
 DEFINE VAR mycurrvalue AS INTEGER NO-UNDO.
 ASSIGN mycurrvalue = INTEGER(getSurferAttribute(myname)) NO-ERROR.
 IF ERROR-STATUS:ERROR THEN RETURN ?.
 ELSE DO:
 	IF setSurferAttribute(myname,STRING(mycurrvalue + 1),myexpires) THEN RETURN mycurrvalue + 1.
	ELSE RETURN ?. 
 END.
END FUNCTION. /*incrementSurferAttribute*/

FUNCTION listSurferAttributes RETURNS CHAR (
	INPUT myType AS CHAR
):
	DEFINE VAR myReturn AS CHAR NO-UNDO.
	DEFINE BUFFER b-myatt FOR webSurferAttribute.
	
	IF NOT AVAIL webSurfer THEN RETURN ?.
	
	CASE myType:
		WHEN "Private" THEN DO:
			FOR EACH b-myatt FIELDS (isPrivate AttributeName) OF WebSurfer
			 NO-LOCK:
			 	IF b-myatt.isPrivate THEN ASSIGN myreturn = myreturn + "," + b-myatt.AttributeName.
			END. /*each b-myatt*/
		END. /*Private attributes only*/
		
		WHEN "Public" THEN DO:
			FOR EACH b-myatt FIELDS (isPrivate AttributeName) OF WebSurfer
			 NO-LOCK:
			 	IF NOT b-myatt.isPrivate THEN ASSIGN myreturn = myreturn + "," + b-myatt.AttributeName.
			END. /*each b-myatt*/
		END. /*Public attributes only*/
		
		OTHERWISE DO: /*all attributes*/
			FOR EACH b-myatt FIELDS (isPrivate AttributeName) OF WebSurfer
			 NO-LOCK:
			 	ASSIGN myreturn = myreturn + "," + b-myatt.AttributeName.
			END. /*each b-myatt*/
		END. /*All attributes*/	
	END CASE. /*myType*/
	
	RETURN TRIM(myreturn,",").
	
END FUNCTION.

FUNCTION verifySurfer RETURNS LOGICAL ():
	IF NOT AVAIL webSession THEN RETURN FALSE.
    FIND CURRENT webSession EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL webSession THEN RETURN FALSE.
    ASSIGN webSession.surferVerified = TRUE NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN FALSE.
    FIND CURRENT webSession NO-LOCK.
	RETURN TRUE.
END FUNCTION. /*verifySurfer*/

FUNCTION pushSessionAttribute RETURNS LOGICAL (
 INPUT myName AS CHAR,
 INPUT myValue AS CHAR
):
	
	DEFINE BUFFER b-myatt FOR webSessionAttribute.
	
	DO TRANSACTION:
		FIND b-myatt OF webSession
		 WHERE b-myatt.AttributeName = myname
		 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
		 
		IF NOT AVAIL b-myatt THEN DO:
			CREATE b-myatt.
			ASSIGN
			 b-myatt.WebSession = WebSession.WebSessionID
			 b-myatt.AttributeName = myName
			 { lib/create.i &tablename="b-myatt" }
			 no-error.
			IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		END.
		ASSIGN
		 b-myatt.AttributeValue = push(b-myatt.AttributeValue,myvalue)
		 no-error.
		IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		ELSE RETURN TRUE.
	END. /*TRANSACTION*/
END FUNCTION. /*pushSessionAttribute*/


FUNCTION pushSurferAttribute RETURNS LOGICAL (
 INPUT myName AS CHAR,
 INPUT myValue AS CHAR,
 INPUT myExpires AS DATE
):
	
	DEFINE BUFFER b-myatt FOR webSurferAttribute.
	
	IF NOT AVAIL webSurfer THEN RETURN ?.
	
	DO TRANSACTION:
		FIND b-myatt OF webSurfer
		 WHERE b-myatt.AttributeName = myname
		 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
		 
		IF NOT AVAIL b-myatt THEN DO:
			CREATE b-myatt.
			ASSIGN
			 b-myatt.WebSurferID = WebSurfer.WebSurferID
			 b-myatt.AttributeName = myName
			 { lib/create.i &tablename="b-myatt" }
			 no-error.
			IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		END.
		ASSIGN
		 b-myatt.AttributeValue = push(b-myatt.AttributeValue,myvalue)
		 b-myatt.ExpireDate = myexpires
		 no-error.
		IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		ELSE RETURN TRUE.
	END. /*TRANSACTION*/
END FUNCTION. /*pushSurferAttribute*/


FUNCTION mergeSurfers RETURNS LOGICAL (
 INPUT mypermanent AS CHAR,
 INPUT mytemporary AS CHAR
):
	DEFINE BUFFER b-perm FOR WebSurfer. /*permanent websurfer*/
	DEFINE BUFFER b-temp FOR WebSurfer. /*websurfer to be merged into permanent*/
	
	IF mypermanent = mytemporary THEN RETURN TRUE. /*already refer to same surfer*/
	
	DO TRANSACTION:
		FIND b-perm 
		 WHERE b-perm.WebSurferID = mypermanent
		 EXCLUSIVE-LOCK NO-ERROR.
		IF AVAIL b-perm
		 THEN FIND  b-temp
		 WHERE b-temp.WebSurferID = mytemporary
		 EXCLUSIVE-LOCK NO-ERROR.
	
		IF NOT AVAIL b-temp THEN RETURN FALSE.
		/*don't merge two distinct surfers that have login ID's*/
		IF b-perm.loginID ne "" AND b-temp.loginID ne "" THEN RETURN FALSE.
		
		ASSIGN
		 b-perm.LastWebSessionID = b-perm.CurrentWebSessionID
		 b-perm.CurrentWebSessionID = b-temp.CurrentWebSessionID
		.
		IF b-perm.loginID = "" THEN ASSIGN b-perm.loginID = b-temp.loginID.
		IF b-perm.password = "" THEN ASSIGN b-perm.password = b-temp.password.
		
		RETURN TRUE.
	END. /*TRANSACTION*/
END FUNCTION. /*mergeSurfers*/


FUNCTION NewSurfer RETURNS CHAR ():
	DEFINE VAR mySurferID AS CHAR NO-UNDO.
	DEFINE BUFFER mySurfer FOR webSurfer. /*don't want to use live websurfer buffer*/
	
	ASSIGN mySurferID = STRING(NEXT-VALUE(webSurferSeq),"999999") + ENCODE(STRING(TIME) + STRING(TODAY) + STRING(RANDOM(1000,9999)) + STRING(ETIME)).
	IF CAN-FIND(WebSurfer WHERE WebSurfer.WebSurferID = mySurferID) THEN DO:
		RETURN NewSurfer(). /*handle cases where it chose the same as one that existed - RARE or never*/
	END.
	DO TRANSACTION:
		CREATE MySurfer NO-ERROR.
		IF ERROR-STATUS:ERROR THEN RETURN ?.
		
		ASSIGN
		 mySurfer.WebSurferID = mySurferID
		 { lib/create.i &tablename="mySurfer" }
		 NO-ERROR.
		IF ERROR-STATUS:ERROR THEN RETURN ?.
	
		RETURN mySurfer.WebSurferID.
	END. /*transaction*/
END FUNCTION.

FUNCTION CreateSurfer RETURNS LOGICAL ():
	DEFINE VAR mySurferID AS CHAR NO-UNDO.
	
	IF AVAIL webSurfer THEN RETURN FALSE. /*Only create if not already available*/

	FIND webSurfer
	 WHERE webSurfer.webSurferID = newSurfer()
	 EXCLUSIVE-LOCK NO-ERROR.
	IF AVAIL webSurfer THEN RETURN TRUE.
	ELSE RETURN FALSE.
END FUNCTION.


FUNCTION switchSurfer RETURNS LOGICAL(
 INPUT myNewSurfer AS CHAR 
):
	/*Change the webSession's webSurferID to the one passed in.*/
	
	DEFINE BUFFER newSurfer FOR webSurfer.
	
	IF myNewSurfer = ? or myNewSurfer = "" THEN RETURN FALSE.
	
	DO TRANSACTION:
		FIND newSurfer
		 WHERE newSurfer.webSurferID = myNewSurfer
		 EXCLUSIVE-LOCK NO-ERROR.
		IF NOT AVAIL newSurfer THEN RETURN FALSE.
		
		lockSession().
		ASSIGN
		 newSurfer.lastWebSessionID = newSurfer.currentWebSessionID
		 newSurfer.currentWebSessionID = webSession.WebSessionID
		 newSurfer.chg-dt = TODAY
		 newSurfer.chg-time = TIME
		 webSession.webSurferID = myNewSurfer
		 NO-ERROR.
		unLockSession().
		IF ERROR-STATUS:ERROR THEN RETURN FALSE.
		
		RELEASE newSurfer.
		
		FIND webSurfer 
		 WHERE webSurfer.webSurferID = myNewSurfer
		 NO-LOCK NO-ERROR.
		
		IF NOT AVAIL webSurfer THEN DO:
			undo.
			RETURN FALSE.
		END. /*couldn't get websurfer*/
		
	END. /*TRANSACTION*/
	RETURN TRUE.

END FUNCTION.


/*That's all*/

