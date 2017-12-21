/*-----------------------------------------------------------------------*
  File........: email.i (UNIX ONLY)
  Version.....: 1.0 
  Description : Email sending API
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell - BravePoint, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000  - http://www.freeframework.org
  Created.....: 1/6/2001
 *-----------------------------------------------------------------------*/
DEFINE STREAM b.

FUNCTION EmailValidate RETURNS LOGICAL (
 INPUT thisaddr AS CHAR
) :

  IF INDEX(thisaddr,"~@") > 1
   AND INDEX(ENTRY(2,thisaddr,"~@"),"~.") > 1
   AND LENGTH(ENTRY(2,thisaddr,"~@")) > 4
   THEN RETURN TRUE.
  ELSE RETURN FALSE.
END FUNCTION.


FUNCTION SendMail RETURNS CHARACTER (
 INPUT recip AS CHAR,
 INPUT sender AS CHAR,
 INPUT sendername AS CHAR,
 INPUT subject AS CHAR,
 INPUT sendfile AS CHAR
) :
DEFINE VAR messagebody AS CHAR NO-UNDO.
DEFINE VAR mailsent AS INTEGER NO-UNDO.
DEFINE VAR textback    AS CHAR NO-UNDO.
DEFINE VAR newsendfile AS CHAR NO-UNDO.
ASSIGN newsendfile = sendfile + ".new".

IF EmailValidate(recip) THEN DO:
    OUTPUT TO VALUE(newsendfile).
    PUT UNFORMATTED "Subject: " subject "~n~n~n".
    OUTPUT CLOSE.
    INPUT STREAM B THROUGH VALUE('cat ' + sendfile + ' >> ' + newsendfile).
    REPEAT:
        IMPORT STREAM B UNFORMATTED textback.
    END.
    INPUT STREAM B THROUGH VALUE('/usr/sbin/sendmail -f ' + sender + ' -F "' 
    + sendername + '" ' + recip + " <" + newsendfile).
    REPEAT:
        IMPORT STREAM B UNFORMATTED textback.
        {&out} textback.
    END.
    
END. /*RECIPIENT ADDRESS WAS GOOD*/
RETURN STRING(mailsent).
END FUNCTION.


