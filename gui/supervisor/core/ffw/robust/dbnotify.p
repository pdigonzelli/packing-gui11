/*-----------------------------------------------------------------------*
  File........: dbnotify.p
  Version.....: 1.0 9/20/01
  Description : This is a sample procedure that you can customize and use 
  				for notification in the event that a database is disconnected
				from the WebSpeed agent for more than a given amount of time.
  Input Param : p-databasename - char - Name of the database (no ext necessary)
  Output Param: <none>
  Author......: S.E. Southwell -  BravePoint, Inc. (770) 449-9696
  Copyright...: FreeFramework 2001  - http://www.freeframework.org
  Created.....: 9/20/01
  Notes.......: This particular example uses the smtpmail.p routine by Paul Keary,
                Mario Paranhos, et al, but Blat and Sendmail versions are included
                in comments at the bottom.
				
				It's also conceivable that you could use a modem API to have the
				program actually dial a pager number and input some digits.
				
				Let your creativity be your guide, and please contribute your 
				innovations back to FreeFramework!
 *-----------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-databasename AS CHAR    NO-UNDO. 
DEFINE VAR v-success                  as LOGICAL NO-UNDO.
DEFINE VAR v-message                  AS CHAR    NO-UNDO.
DEFINE VAR v-adminemail               AS CHAR    NO-UNDO.
DEFINE VAR v-smtpserver               AS CHAR    NO-UNDO.

{ ffw/lib/ffw_global.i }
{ ffw/lib/agentsetting.i }
{ ffw/lib/lognote.i }

ASSIGN
 v-adminemail = GetAgentSetting("AdminEmail")
 v-smtpserver = GetAgentSetting("SMTPServer")
.
IF v-adminemail = "" THEN DO:
    LogNote("Error","Error running db-down notification procedure: AdminEmail directive not found in .ini file.  No email address to send to!").
    RETURN.
END.
IF v-SMTPServer = "" THEN DO:
    LogNote("Error","Error running db-down notification procedure: SMTPServer directive not found in .ini file.  No email server to send from!").
    RETURN.
END.

RUN VALUE(SEARCH("ffw/procs/smtpmail.p"))(
    v-smtpserver,
    v-adminemail,
    v-adminemail + ";FreeFrameWork Database Monitor",
    "","","",
    "Warning: Database Down! (" + p-databasename + ")",
    "The " + p-databasename + " database is disconnected from WebSpeed and could not be reconnected.",
    "",
    "text",
    OUTPUT v-success,
    OUTPUT v-message
) NO-ERROR.
IF ERROR-STATUS:ERROR THEN LogNote("Error","Error running db-down notification procedure: " + ERROR-STATUS:GET-MESSAGE(1)).
ELSE IF v-success THEN LogNote("Info","Emailed " + v-adminemail + ": " + v-message).
ELSE LogNote("Error","Unable to email " + v-adminemail + ": "  + v-message).


