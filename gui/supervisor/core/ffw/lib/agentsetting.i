/*-----------------------------------------------------------------------*
  File........: agentsetting.i
  Version.....: 1.04 - 12/21/2000
  Description : Contains temp-table definitions and method library for handling
  				agentsettings.  Agentsettings are name=value pairs stored in the
				tt-agentsetting temptable.  They are loaded in at startup from the 
				.ini file.
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell - United Systems, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000  - http://www.freeframework.org
  Created.....: 8/9/00
  Notes.......:
 *-----------------------------------------------------------------------*/
 
DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-agentsetting
 FIELD varname AS CHAR
 FIELD varvalue AS CHAR
 INDEX varname IS UNIQUE PRIMARY varname
.

FUNCTION setAgentSetting RETURNS LOGICAL (
 INPUT myName AS CHAR,
 INPUT myValue AS CHAR
):
	IF myName = "" or myName = ? THEN RETURN FALSE.
	ELSE DO TRANSACTION:
		FIND tt-agentsetting
		 WHERE tt-agentsetting.varname = myname
		 EXCLUSIVE-LOCK NO-ERROR.
		IF NOT AVAIL tt-agentsetting
		 THEN DO:
		 	CREATE tt-agentsetting.
			ASSIGN tt-agentsetting.varname = myname.
		END. 	
		ASSIGN tt-agentsetting.varvalue = myvalue.
	END. /*TRANSACTION*/
END FUNCTION. /*setAgentSetting*/

FUNCTION getAgentSetting RETURNS CHAR (
 INPUT myName AS CHAR
):
	FIND tt-agentsetting
	 WHERE tt-agentsetting.varname = myName
	 NO-LOCK NO-ERROR.
	IF NOT AVAIL tt-agentsetting OR tt-agentsetting.varvalue = ? THEN RETURN "".
	ELSE return tt-agentsetting.varvalue.
END FUNCTION. /*getAgentSetting*/


