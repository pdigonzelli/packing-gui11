/*-----------------------------------------------------------------------*
  File........: dbconnect.p
  Version.....: 1.0 1/09/02
  Description : Connects to a given database in the tt-db table.
  Input Param : p-databasename - char - Name of the database (no ext necessary)
  Output Param: p-success - logical - Whether it worked or not
  Author......: S.E. Southwell -  BravePoint, Inc. (770) 449-9696
  Copyright...: FreeFramework 2002  - http://www.freeframework.org
  Created.....: 1/09/02
  Notes.......: 
 *-----------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-databasename AS CHAR    NO-UNDO. 
DEFINE OUTPUT PARAMETER p-success     AS LOGICAL NO-UNDO.

DEFINE VAR v-connecterror AS CHAR NO-UNDO.


{ ffw/lib/ffw_global.i }
{ ffw/lib/agentsetting.i }
{ ffw/lib/lognote.i }
{ ffw/lib/tt_ini.i }
{ ffw/lib/nettools.i }


/* Step 1:  Get the tt-db record for the db we need to connect to */

IF p-databasename = "" THEN DO:
    ASSIGN p-success = FALSE.
    RETURN.
END.

FIND tt-db 
 WHERE tt-db.databaseName = p-databasename
 NO-LOCK NO-ERROR.
IF NOT AVAIL tt-db THEN DO:
    ASSIGN p-success = FALSE.
    RETURN.
END.


/* Step 2:  If it's a shared-memory connection then go ahead and connect. */ 

IF tt-db.filename NE "" THEN DO:  /*Has a filename - direct connect*/
    CONNECT VALUE(tt-db.filename) VALUE (" " + tt-db.otherparams) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        LogNote("Error", ERROR-STATUS:GET-MESSAGE(1)).
        ASSIGN p-success = FALSE.
        RETURN.
    END.
    ELSE DO:
        ASSIGN p-success = TRUE.
        RETURN.
    END.
END. /*Had a filename - direct connect*/


/* Step 3:  If connecting remotely then test the host's availability first */

IF NOT ServerIsUp(tt-db.hostname, tt-db.servicename, OUTPUT v-connecterror) THEN DO:
    LogNote("Error", "Unable to connect to database server " + tt-db.hostname + " on service/port " + tt-db.servicename).
    LogNote("Error", "Error reported was: " + v-connecterror).
    ASSIGN p-success = FALSE.
    RETURN.
END.

/* Step 4: If host responds, then connect */

CONNECT VALUE(tt-db.databasename) 
 VALUE (
 " -N " + tt-db.network + 
 " -H " + tt-db.hostname + 
 " -S " + tt-db.servicename + " " +  
 tt-db.otherparams
 ) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    LogNote("Error", ERROR-STATUS:GET-MESSAGE(1)).
    ASSIGN p-success = FALSE.
    RETURN.
END.
ELSE DO:
    ASSIGN p-success = TRUE.
    RETURN.
END.
