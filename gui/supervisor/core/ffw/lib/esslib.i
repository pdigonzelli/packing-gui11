/*-----------------------------------------------------------------------*
  File........: esslib.i
  Version.....: Not yet assigned (8/3/2000)
  Description : Method library for state-keeping mechanisms inclusion in ESS programs
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  United Systems, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000 - http://www.freeframework.org
  Created.....: 9/22/1999
  Notes.......: If you make changes to this program that others could benefit
				from, please share your code with me: ses@usiatl.com

  Last Modified: 
  				8/3/2000 SES updated and cleaned up for use with FreeFrameWork
				10/12/99 SES updated to check security for administrator area objects.
--------------------------------------------------------------------------*/
	&IF DEFINED(FFWEBSTATE_I) = 0 &THEN
	{lib/ffwebstate.i}
	&ENDIF
	/*Use this to check security on an ESS object before running*/
	&if defined (SECUREOBJECT) NE 0 &then
	{lib/security.i}
	&endif
	/*----------------------------------------------------------*/
	
	
	PROCEDURE output-headers:
		RUN Get-Last-State.   
		/*for secure objects*/
		&if defined (SECUREOBJECT) NE 0 &then
		RUN Check-Object-Security.
  		IF runStatus = "done":U THEN RETURN.
		&endif
		/* end of administrator object section*/
		RUN ffw-output-header.   
		RETURN.   
	END.
	
	
	