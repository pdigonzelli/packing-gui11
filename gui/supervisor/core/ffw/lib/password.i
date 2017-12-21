/*-----------------------------------------------------------------------*
  File........: password.i
  Version.....: 1.0
  Description : Generates a (somewhat) pronouncable random password
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell - BravePoint, Inc. (770) 449-9696
  Copyright...: FreeFramework 2001  - http://www.freeframework.org
  Created.....: 10/12/01
  Notes.......:
 *-----------------------------------------------------------------------*/
FUNCTION rot13 RETURNS CHAR(
 INPUT mystring AS CHAR
):
    DEFINE VAR i AS INTEGER NO-UNDO.
    DEFINE VAR alist AS CHAR CASE-SENSITIVE NO-UNDO.
    DEFINE VAR blist AS CHAR CASE-SENSITIVE NO-UNDO.
    
    ASSIGN
     alist = "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z"
     blist = "n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c,d,e,f,g,h,i,j,k,l,m,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A,B,C,D,E,F,G,H,I,J,K,L,M"
    .
    DO i = 1 TO LENGTH(mystring):
        IF LOOKUP(SUBSTRING(mystring,i,1),alist) > 0 THEN ASSIGN SUBSTRING(mystring,i,1) = entry(lookup(SUBSTRING(mystring,i,1),alist),blist).        
    END. /*i*/
    RETURN mystring.
END FUNCTION. /*rot13*/

FUNCTION createPassword RETURNS CHAR():
    DEFINE VAR vowelllist AS CHAR NO-UNDO.
    DEFINE VAR vbeglist AS CHAR NO-UNDO.
    DEFINE VAR consonlist AS CHAR NO-UNDO.
    DEFINE VAR endlist AS CHAR NO-UNDO.
    DEFINE VAR myreturn AS CHAR NO-UNDO.
    DEFINE VAR cusslist AS CHAR NO-UNDO. /*words to filter out*/
    DEFINE VAR i AS INTEGER NO-UNDO.
    DEFINE VAR J AS INTEGER NO-UNDO.
    
    ASSIGN
     vbeglist = "a,a,e,e,i,o,oo,u"
     vowelllist = "a,a,e,e,i,i,o,o,u,u,ey,ee,oo,ui,ie,oi,io"
     consonlist = "b,br,bl,c,c,cr,cl,d,dr,d,f,fl,fr,g,gh,gl,gr,h,j,k,kl,kr,l,m,n,p,qu,r,rh,s,s,st,sk,sh,t,tr,th,v,w,wh,x,y,z"
     endlist = "ck,lk,lt,mp,nk,nt,pp,rsh,rt,ss,st,sh,tion,th,y"
     /* hate having to include these, but there's an outside chance that*/
     /* the generator comes up with something that would offend a customer*/
     /* and I'd rather not have to take the phone call. Rot13 encoded for modesty...*/
     cusslist = rot13("shpx,fuvg,qnza,ovgpu,phag,avttre,uryy,sneg,fcvp,snt,dhrre,xvyy,ungr,frk,qvpx,chffl") 
    .
    /*get some entropy in case -rand isn't used*/
    DO i = 1 to RANDOM(1,TIME MODULO 60):
        J = RANDOM(1,10000).
        if etime modulo 200 = 0 then leave.
    END.
    
    REPEAT:
        IF length(myreturn) > 0 AND CAN-DO(vbeglist,substring(myreturn,length(myreturn))) THEN ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(endlist)),endlist).
        CASE RANDOM(1,5):
            WHEN 1 THEN DO:
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(consonlist)),consonlist).
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(vowelllist)),vowelllist).
            END.
            WHEN 2 THEN DO:
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(vbeglist)),vbeglist).
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(consonlist)),consonlist).
            END.
            WHEN 3 THEN DO:
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(consonlist)),consonlist).
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(vowelllist)),vowelllist).
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(endlist)),endlist).
            END.
            WHEN 4 THEN DO:
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(vbeglist)),vbeglist).
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(consonlist)),consonlist).
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(vowelllist)),vowelllist).
            END.
            WHEN 5 THEN DO:
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(consonlist)),consonlist).
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(vowelllist)),vowelllist).
                ASSIGN myreturn = myreturn + entry(RANDOM(1,NUM-ENTRIES(consonlist)),consonlist).
            END.
        END CASE.
        IF length(myreturn) >= 7 THEN LEAVE.
    END. 
    /*CUSS-CHECK*/
    DO i = 1 to NUM-ENTRIES(cusslist):
        IF INDEX(myreturn,ENTRY(i,cusslist)) > 0 THEN RETURN createPassword().
    END.
    RETURN myreturn.
END FUNCTION.

