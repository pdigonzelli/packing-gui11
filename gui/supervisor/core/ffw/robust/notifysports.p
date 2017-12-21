/*-----------------------------------------------------------------------*
  File........: notifysports.p
  Version.....: 1.0 (pre-beta) - 5/10/2000
  Description : This is a sample procedure that you can customize and use 
  				for notification in the event that a database is disconnected
				from the WebSpeed agent for more than a given amount of time.
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  BravePoint, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000  - http://www.freeframework.org
  Created.....: 5/10/2000
  Notes.......: This particular example uses the BLAT freeware program to send
  				an email to the author of this program.  If you were using any
				flavor of Unix, you might want to use mail or sendmail.
				
				It's also conceivable that you could use a modem API to have the
				program actually dial a pager number and input some digits.
				
				Let your creativity be your guide, and please contribute your 
				innovations back to FreeFramework!
                
                NOTE:  THIS IS DEPRECATED.  SEE THE .INI FOR DETAILS
 *-----------------------------------------------------------------------*/
&SCOPED-DEFINE DBNAME webstate
 
/* NT Version:  Requires BLAT */
/* 
output to value("{&dbname}down.txt").
put unformatted "WebSpeed is unable to reconnect to the {&dbname} database.  Please check the database and make sure it's running.".
output close.
os-command silent value('blat {&dbname}down.txt -t foo@bar.com -s "{&dbname} Down" -f webmaster@foo.org').
os-delete {&dbname}down.txt.
*/
/* UNIX / Linux version */

{ ffw/lib/email.i }
DEFINE VAR vtempfile AS CHAR NO-UNDO.
DEFINE STREAM downout.

ASSIGN vtempfile = "ffw_dbdown_{&dbname}.txt".
OUTPUT STREAM downout TO VALUE(vtempfile).
PUT STREAM downout UNFORMATTED "The {&dbname} database is disconnected from WebSpeed and could not be reconnected.".
OUTPUT STREAM downout CLOSE.
sendmail(
    "foo@bar.com",
    "webmaster@foo.org",
    "FreeFrameWork Monitor",
    "Database Down!",
    vtempfile

).


