/*-----------------------------------------------------------------------
 File: session.i
 Purpose: Include for the pluspack logging module.
 Description:
 Author(s) :PerSDigre/PSC
 Created: April 1998
 Notes:    
 Modification History:    
 $Header: /cvsroot/freeframework/ffw1.2/ffw/plus/session.i,v 1.1 2002/08/21 16:14:25 freeframework Exp $
 $Log: session.i,v $
 Revision 1.1  2002/08/21 16:14:25  freeframework
 Initial import

 Revision 1.1.1.1  2001/03/23 14:50:43  slichtenberg
 initial load 1.03


This file contains sample code which may assist you in creating applications.
You may use the code as you see fit. If you modify the code or include it in another software program,
you will refrain from identifying Progress Software as the supplier of the code, or using any
Progress Software trademarks in connection with your use of the code.
THE CODE IS NOT SUPPORTED BY PROGRESS SOFTWARE AND IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
INCLUDING, WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR NONINFRINGEMENT.
-----------------------------------------------------------------------*/

def new global shared var hSession as handle no-undo.
FUNCTION plusLog        RETURNS CHAR (INPUT cMsg AS CHAR)                       in hSession.
function plusSetSession returns char (input cName as char,input cValue as char) in hSession.
function plusGetSession returns char (input cName as char)                      in hSession.
function plusSetGlobal  returns char (input cName as char,input cValue as char) in hSession.
function plusGetGlobal  returns char (input cName as char)                      in hSession.

if not valid-handle(hSession) then hSession = THIS-PROCEDURE.

FUNCTION plusLog        RETURNS CHAR (INPUT cMsg AS CHAR):
end function.