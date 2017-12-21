/*-----------------------------------------------------------------------*
  File........: showerrorscreen.i
  Version.....: 1.04 - 12/21/2000
  Description : Function for showing a customized application error screen
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  BravePoint / USI (770) 449-9696
  Copyright...: FreeFramework 2000  - http://www.freeframework.org
  Created.....: 7/28/2000
  Notes.......:
 *-----------------------------------------------------------------------*/
&IF DEFINED(SHOWERRORSCREEN_I) = 0 &THEN /*MAKE SURE ONLY INCLUDED ONCE*/
 &GLOBAL-DEFINE SHOWERRORSCREEN_I YES
{ ffw/lib/lognote.i }

FUNCTION ShowErrorScreen RETURNS LOGICAL (
 INPUT myerrormsg AS CHAR
):

    DEFINE VARIABLE cntr AS INTEGER   NO-UNDO.
    DEFINE VARIABLE txt  AS CHARACTER NO-UNDO.

    /* Check to see if there are any errors. If so, output them one by one. */
    IF ERROR-STATUS:ERROR AND ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      ASSIGN myerrormsg = myerrormsg + "~n<br>" 
	  		+ "<H1>Error Messages</H1>~n~n":U .
      
      DO cntr = 1 TO ERROR-STATUS:NUM-MESSAGES:
        assign myerrormsg = myerrormsg +
        "<P>":U + html-encode(ERROR-STATUS:GET-MESSAGE(cntr)) + "</P>~n":U.
		LogNote("Note"," " + ERROR-STATUS:GET-MESSAGE(cntr) + "").
      END. /* DO cntr... */
    END. /* IF...NUM-MESSAGES > 0 */
    ASSIGN file-info:file-name = SEARCH("ffw/robust/wserrormsg.r").
    IF file-info:full-pathname ne ? THEN RUN VALUE(file-info:full-pathname) (myerrormsg) NO-ERROR.
    ELSE DO:
        {&OUT}
         "Content-type: text/html" HTTP-NEWLINE HTTP-NEWLINE
         "<small>The customizable error page, wserrormsg.r was not found.<br>"
         "Edit ffw/robust/wserrormsg.html and compile it so that your users "
         "will not have to see this ugly screen. </small><br>" skip
         "Problem:<br>" skip
         myerrormsg
         "<hr>For more info, look at your WebSpeed server log!"
        .
        
    END.
END FUNCTION.

&ENDIF

