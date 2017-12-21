/*------------------------------------------------------------------------
  File: chgdictdb.p
  Description: Changes the dictionary database alias (dictdb)
                This must be done in a seperate procedure from any procedures
                which actually use dictdb.
  Input Parameters: 
      p-dbName = Logical name of the database to switch to.
  Output Parameters:
      <none>
  Author: S.E. Southwell - BravePoint, Inc.  ses@bravepointdallas.com
  Copyright: The FreeFrameWork Project, Inc. 2002
  Created: 3/8/02
  Last Modified:
------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-dbname AS CHAR NO-UNDO.

/*Sanity Check*/
IF NOT CONNECTED(p-dbname) THEN RETURN ERROR.
CREATE ALIAS dictdb FOR DATABASE VALUE(p-dbname).
IF ERROR-STATUS:ERROR THEN RETURN ERROR.
IF LDBNAME("dictdb") NE p-dbname THEN RETURN ERROR.

