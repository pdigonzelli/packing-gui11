/***************
 File: setup.p
 Purpose: Installation program
 Description:
 Author(s) :Per S Digre/PSC
 Created: April 1999
 Notes:    
 Modification History:    
 $Header: /cvsroot/freeframework/ffw1.2/ffw/plus/setup.p,v 1.1 2002/08/21 16:14:25 freeframework Exp $
 $Log: setup.p,v $
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

*******************************************/
{src/web/method/cgidefs.i}

def var cIn   as char no-undo.
def var cData as char no-undo init " ".
def var iPos   as int  no-undo.
def stream sIn.






procedure pAdd:
  input  stream sIn from value(search("webtools/webtools.dat")).
  repeat:
    assign cIn = "".
    import stream sIn unformatted cIn.
    assign cdata = cData + cIn + chr(10).
  end.
  input  stream sIn close.

  if not (cData matches "*plus/db*" and cData matches "*plus/am*") then do:
   assign
    iPos = index(cData,'<B><FONT SIZE = "+1">Session:</FONT></B><br>')
    iPos = if iPos < 10
           then index(cData,"<br>") + 5
           else r-index(substring(cData,1,iPos - 7),"<br>") + 5.
    substring(cData,iPos,0) = chr(10) +
      '<A HREF="../plus/am.p">App. Manager</A><br> ' + chr(10) +
      '<A HREF="../plus/db.p">Data Browser</A><br> ' + chr(10).
  end.
  output stream sIn to value(search("webtools/webtools.dat")).
  put    stream sIn unformatted cdata skip.
  output stream sIn close.
end procedure.

function fAdded returns log():
  def var cRet as log no-undo init false.
  input  stream sIn from value(search("webtools/webtools.dat")).
  repeat:
    import stream sIn unformatted cIn.
    if cIn matches '*plus/databr*' OR
       cIn matches '*plus/appman*' THEN cRet = true.
  end.
  input  stream sIn close.
  return cRet.
end function.


/**** Output header section  *****/


  if selfurl matches "*webtools/edtscrpt.*" then do:
    {&out} "
      <script language=javascript>
      window.open('../plus/setup.p','_top');
      </script>" skip.

    STOP.
  end.

  if not fAdded() then run pAdd.

if get-value("mode") > "" then do:
  set-user-field('doc','welcome').
  run plus/doc.p no-error.
end.
else do:
  {&cNo-Cache}
  output-content-type("text/html").
  {&out}
  '<HTML><HEAD><TITLE>WebSpeed WebTools</TITLE></HEAD>'
  '~n<FRAMESET COLS="152,*">'
  '~n<FRAMESET ROWS="64,*">'
    '~n<FRAME NAME="WS_header" SRC="../workshop.w?html=MainMenu" SCROLLING=no FRAMEBORDER=yes MARGINHEIGHT=0 MARGINWIDTH=0>'
    '~n<FRAME NAME="WS_index" SRC="../webtools/index.w?new" FRAMEBORDER=yes MARGINHEIGHT=3 MARGINWIDTH=3>'
  '~n</FRAMESET>'
  '~n<FRAME NAME="WS_main" SRC="setup.p?mode=welcome" FRAMEBORDER=yes MARGINHEIGHT=5 MARGINWIDTH=10>'
 '~n</FRAMESET></HTML>' skip.
end.
