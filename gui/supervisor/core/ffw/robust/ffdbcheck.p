/*-----------------------------------------------------------------------*
  File........: ffdbcheck.p
  Version.....: 1.0 (pre-beta) - 6/4/2000
  Description : Checks if a given database is connected
  Input Param : p-dbname - name of database to check.
  Output Param: p-connected - logical yes or no, whether database is connected.
  Author......: S.E. Southwell - United Systems, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000  - http://www.freeframework.org
  Created.....: 6/4/2000
  Notes.......: This program is an external procedure, because for some reason, 
                external procedures are more robust at picking up on dropped database
                connections.  While you can check for connections in a persistent 
                procedure, and it sometimes works, it supposedly is more reliable 
                if it is checked in a freshly called procedure.  Because of the 
                simple nature of this procedure, and the propensity for its r-code
                to be stashed, this procedure shouldn't add too much overhead.
 *-----------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-dbname AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER p-connected AS LOGICAL NO-UNDO.

ASSIGN p-connected = CONNECTED(p-dbname).
RETURN.
/*That's all folks....  Pretty simple, huh?*/


