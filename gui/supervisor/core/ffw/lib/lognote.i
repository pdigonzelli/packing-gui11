/*-----------------------------------------------------------------------*
  File........: lognote.i
  Version.....: 1.0A - 7/28/2000
  Description : Log a note in the WebSpeed Error log (or elsewhere)
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  United Systems, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000 - http://www.freeframework.org
  Created.....: 7/28/2000
  Notes.......: Include this file in any program where you need to make a direct
  				log entry.
 *-----------------------------------------------------------------------*/
 
&IF DEFINED(LOGNOTE_I) = 0 &THEN  /*Make sure it's only included once*/

&GLOBAL-DEFINE LOGNOTE_I YES
FUNCTION LogNote RETURNS LOGICAL(
 INPUT myType AS CHAR,
 INPUT myText AS CHAR
):
	IF CAN-DO(v-logtypes,mytype) THEN MESSAGE "<FFW " + myType + ": " + mytext + "/>".
END FUNCTION.

&ENDIF


