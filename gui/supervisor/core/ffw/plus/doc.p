/***************
 File: doc.p
 Purpose: program to display documentation files (HELP)
 Description:
 Author(s) :Per S Digre/PSC
 Created: April 1999
 Notes:    
 Modification History:    
 $Header: /cvsroot/freeframework/ffw1.2/ffw/plus/doc.p,v 1.1 2002/08/21 16:14:24 freeframework Exp $
 $Log: doc.p,v $
 Revision 1.1  2002/08/21 16:14:24  freeframework
 Initial import

 Revision 1.1.1.1  2001/03/23 14:50:25  slichtenberg
 initial load 1.03


This file contains sample code which may assist you in creating applications.
You may use the code as you see fit. If you modify the code or include it in another software program,
you will refrain from identifying Progress Software as the supplier of the code, or using any
Progress Software trademarks in connection with your use of the code.
THE CODE IS NOT SUPPORTED BY PROGRESS SOFTWARE AND IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
INCLUDING, WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR NONINFRINGEMENT.

*******************************************/
{plus/plus.i}
DEFINE NEW GLOBAL SHARED VARIABLE cPlusDir     AS CHARACTER NO-UNDO. /** PLUS directory    **/
DEFINE NEW GLOBAL SHARED VARIABLE cPlusHTML    AS CHARACTER NO-UNDO. /** PLUS HTML directory    **/
DEFINE STREAM s1.
DEFINE VARIABLE c1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE i1 AS INT NO-UNDO.

INPUT STREAM s1 FROM VALUE(cPlusDir + "doc/" + get-value("doc") + ".htm").
REPEAT:
  IMPORT STREAM s1 UNFORMATTED c1.
  c1 = REPLACE(c1,'/webspeed31A',get-config('wsRoot')).
  c1 = REPLACE(c1,"`get-config('wsRoot')`",get-config('wsRoot')).
  IF c1 MATCHES '*href="*.htm"*' AND NOT c1 MATCHES '*href="/*.htm"*' THEN DO:
    i1 = INDEX(c1,"href=").
    {&out} SUBSTRING(c1,1,i1 + 5).
    c1 = REPLACE(SUBSTRING(c1,i1 + 6),".htm","").
    {&out} "doc?doc=" c1.
    c1 = ''.
  END.
  ELSE IF (c1 MATCHES '*src="*.jpg"*' or c1 MATCHES '*src="*.gif"*') and not c1 MATCHES '*src="/*' THEN DO:
    assign
      i1 = INDEX(c1,' src="')
      c1 = SUBSTRING(c1,1,i1 + 5) + cPlusHTML + SUBSTRING(c1,i1 + 6).
  END.
  {&out} c1 SKIP.
END.
INPUT STREAM s1 CLOSE.

