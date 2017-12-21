/*-----------------------------------------------------------------------*
  File........: connectsports.p
  Version.....: 1.03  5/10/2000
  Description : This is a sample procedure that you can use as a template for
  				setting up your own database connection procedures.
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  BravePoint, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000 - http://www.freeframework.org
  Created.....: 5/10/2000
  Note........: If you use a shared memory database connection, you will see
  				increased performance, however, if the database goes down, your 
				WebSpeed agents will self-destruct.  In that case the agents should
				be restarted by the broker based on demand, as soon as the database
				comes back up.
				
				See the Progress System Administration Reference for more info
				about database connection parameters.
                
                NOTE:  THIS IS DEPRECATED.  YOU'LL PROBABLY WANT TO USE THE NEW
                WAY OF CONNECTING.  SEE THE .INI FOR MORE DETAILS.
 *-----------------------------------------------------------------------*/
{ ffw/lib/ffw_global.i }
{ ffw/lib/lognote.i }

/*Use the CONNECT statement to connect to a database.*/  

/* CONNECT /home/ffw/databases/sports.db -N tcp -S sports -H localhost NO-ERROR.*/
CONNECT /home/ffw/databases/sports.db NO-ERROR.
IF ERROR-STATUS:ERROR THEN LogNote("Error",ERROR-STATUS:GET-MESSAGE(1)).

