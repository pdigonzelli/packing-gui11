/*-----------------------------------------------------------------------*
  File........: devcheck.i
  Version.....: 1.04 - 12/21/2000
  Description : Function to check whether WS is in Development mode or not
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell - Bravepoint / USI (770) 449-9696
  Copyright...: FreeFramework 2000  - http://www.freeframework.org
  Created.....: 5/10/2000
  Notes.......: 
  Last Modified: 
                12/20/2000 - Fixed so that proper version call is made whether the proversion
                returns version 3 or version 9 (same thing)
 *-----------------------------------------------------------------------*/
FUNCTION devCheck RETURNS LOGICAL ():
&IF "{&WEB-CONTEXT-EXCLUDE}" = "YES" &THEN
    RETURN {&DEVELOPMENT_MODE}.
&ELSE
	IF PROVERSION BEGINS "3" OR PROVERSION BEGINS "9" AND NOT WEB-CONTEXT:GET-CONFIG-VALUE("srvrAppMode":U) BEGINS "Development"
	 OR PROVERSION BEGINS "2" AND NOT WEB-CONTEXT:GET-CONFIG-VALUE("ENVIRONMENT":U) BEGINS "Development"
	  THEN RETURN FALSE.
	 ELSE RETURN TRUE.

&ENDIF
END FUNCTION.

