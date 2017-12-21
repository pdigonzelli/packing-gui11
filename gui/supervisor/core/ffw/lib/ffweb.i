/*-----------------------------------------------------------------------*
  File........: ffweb.i
  Version.....: Not yet assigned (7/30/2000)
  Description : Method library common to all webobjects.
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  United Systems, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000 - http://www.freeframework.org
  Created.....: 7/14/1998
  Notes.......: If you make changes to this program that others could benefit
				from, please share your code with me: ses@usiatl.com
				
----------------------------------------------------------------------*/
&GLOBAL-DEFINE FFWEB_I

/*Include these files only if they have not yet been included*/

&IF DEFINED(FFDEFS_I) = 0 &THEN
{ lib/ffdefs.i }
&ENDIF
&IF DEFINED(FFFUNCS_I) = 0 &THEN
{ lib/fffuncs.i }
&ENDIF

/*That's all - simple huh?*/


