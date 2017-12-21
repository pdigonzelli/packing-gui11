/*-----------------------------------------------------------------------
File: viewlogs.p
Purpose: Code to show current logs in a window programs.
Description:
Author(s) :Per S Digre/PSC
Created: April 2000
Notes:    
Modification History:    
$Header: /cvsroot/freeframework/ffw1.2/ffw/plus/viewlog.p,v 1.1 2002/08/21 16:14:25 freeframework Exp $
$Log: viewlog.p,v $
Revision 1.1  2002/08/21 16:14:25  freeframework
Initial import

Revision 1.1.1.1  2001/03/23 14:50:44  slichtenberg
initial load 1.03

This file contains sample code which may assist you in creating applications.
You may use the code as you see fit. If you modify the code or include it in another software program,
you will refrain from identifying Progress Software as the supplier of the code, or us1g any
Progress Software trademarks in connection with your use of the code.
THE CODE IS NOT SUPPORTED BY PROGRESS SOFTWARE AND IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
INCLUDING, WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR NONINFRINGEMENT.
-----------------------------------------------------------------------*/
{src/web/method/cgidefs.i}
{plus/plus.i}

DEF NEW GLOBAL SHARED VAR plusLog  AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR plusSess AS CHAR INIT ""    NO-UNDO.

fHeader().
{&OUT} '<textarea name=debug wrap=off cols=80 rows=20>~n'.
def stream s1.
def var c1 as char no-undo.
INPUT STREAM s1 FROM VALUE(plusLog + plusSess + ".log").
REPEAT:
  IMPORT STREAM s1 UNFORMATTED c1.
  IF c1 = ""
  THEN {&out} SKIP(1).
  ELSE {&out} c1 SKIP.
END.
INPUT STREAM s1 CLOSE.
{&OUT} '</textarea>~n'.
fFooter().
