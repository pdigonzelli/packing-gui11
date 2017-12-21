/*-----------------------------------------------------------------------*
  File........: nettools.i
  Version.....: 1.0 1/09/02
  Description : Various networking related functions
  Author......: S.E. Southwell, Mario Paranhos -  BravePoint, Inc. (770) 449-9696
  Copyright...: FreeFramework 2002  - http://www.freeframework.org
  Created.....: 1/09/02
  Notes.......: Used by dbconnect.p
  
  ServerIsUp() - Test whether a given host is listening on a given TCP port.  Use it
                 for checking database servers, webservers, ftp, whatever. 
                 (by Mario Paranhos)
                 
  Other Ideas:  Possibly implement ping() nslookup() portFromService() etc.               
 *-----------------------------------------------------------------------*/
FUNCTION serverIsUp RETURN LOGICAL (
 INPUT pServer AS CHARACTER,
 INPUT pPort AS CHARACTER,
 OUTPUT pError AS CHARACTER
):
    DEFINE VARIABLE hSocket AS HANDLE NO-UNDO.
    DEFINE VARIABLE lStatus AS LOGICAL NO-UNDO.

    CREATE SOCKET hSocket.
    ASSIGN lStatus = hSocket:CONNECT("-H " + pServer + " -S " + pPort) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN pError = ERROR-STATUS:GET-MESSAGE(1).
        IF pError = ? THEN ASSIGN pError = "Unknown Error - Make sure /etc/hosts and /etc/services contain correct entries.".
    END.
    hSocket:DISCONNECT().
    DELETE OBJECT hSocket.
    ASSIGN hSocket = ?.
    IF NOT lStatus AND pError = ? THEN ASSIGN pError = "Unknown Error - Make sure /etc/hosts and /etc/services contain correct entries.".
    RETURN lStatus.
END FUNCTION. /* serverIsUp */ 

FUNCTION pingHost RETURNS LOGICAL (INPUT chrDBHost AS
CHARACTER):
DEFINE VARIABLE pingResponse AS CHARACTER NO-UNDO.
DEFINE VARIABLE pingValidResponseUNIX AS CHARACTER
NO-UNDO.
DEFINE VARIABLE pingValidResponseNT AS CHARACTER NO-UNDO.
DEFINE VARIABLE pingValidResponse AS CHARACTER NO-UNDO.
DEFINE VARIABLE pingUNIX AS CHARACTER NO-UNDO.
DEFINE VARIABLE pingNT AS CHARACTER NO-UNDO.
DEFINE VARIABLE pingCMD AS CHARACTER NO-UNDO.
DEFINE VARIABLE pingTimeOut AS INTEGER NO-UNDO.
DEFINE VARIABLE hostAlive AS LOGICAL NO-UNDO.

ASSIGN pingValidResponseUNIX = "9 bytes from"
pingValidResponseNT = "Reply from"
pingTimeOut = 50 /* Windows is in
milliseconds, UNIX seconds */
pingNT = "ping -l 1 -n 1 -w " +
STRING(pingTimeOut) + " " +
chrDBHost.
pingUNIX = "ping -s 1 -c 1 -w " +
STRING(pingTimeOut) + " " +
chrDBHost.
.

IF "{&opsys}" = "UNIX" THEN
ASSIGN pingCMD = pingUNIX
pingValidResponse = pingValidResponseUNIX.
ELSE
ASSIGN pingCMD = pingNT
pingValidResponse = pingValidResponseNT.

INPUT THROUGH VALUE(pingCMD).
pingLoop:
REPEAT:
IMPORT UNFORMATTED pingResponse.
IF pingResponse BEGINS pingValidResponse THEN
DO:
ASSIGN hostAlive = TRUE.
LEAVE pingLoop.
END.
END.
INPUT CLOSE.

RETURN hostAlive.
END FUNCTION. /* pingHost */ 