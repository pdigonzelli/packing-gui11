/*-----------------------------------------------------------------------*
File........: base36.i
Version.....: 1.0 (pre-beta) - 5/10/2000
Description : Adds the tobase36 and to10from36 functionality to 4gl programs
Input Param : <none>
Output Param: <none>
Author......: S.E. Southwell -  United Systems, Inc. (770) 449-9696
Copyright...: FreeFramework 2000 - http://www.freeframework.org
Created.....: 10/18/2000
Notes.......: If you make changes to this program that others could benefit
              from, please share your code with me: ses@usiatl.com
*-----------------------------------------------------------------------*/

FUNCTION ToBase36 RETURNS CHAR(
INPUT myvalue AS INTEGER
):
/*Converts base 10 to base 36*/
/*Run time is about 1 millisecond*/
    DEFINE VAR q AS INTEGER NO-UNDO. /*quotient*/
    DEFINE VAR r AS INTEGER NO-UNDO. /*remainder*/
    DEFINE VAR y AS CHAR NO-UNDO.    /*converted value*/

    ASSIGN q = myvalue.
    
    DO WHILE q > 0:
        ASSIGN
         r = q MODULO 36
         q = TRUNCATE((q / 36),0)
        .
        ASSIGN y = (if r >= 10 then Chr(65 + (r - 10)) ELSE STRING(r)) + Y.
                              
    END. /*q > 0*/
    RETURN y.
END FUNCTION.

FUNCTION To10from36 RETURNS INTEGER(
INPUT myvalue AS CHAR
):
/*Converts base 36 to base 10*/
    DEFINE VAR vl AS INTEGER NO-UNDO.
    DEFINE VAR vp AS INTEGER NO-UNDO.
    DEFINE VAR retval AS INTEGER NO-UNDO.
    DEFINE VAR vthisdigit AS CHAR NO-UNDO.
    DEFINE VAR vthisvalue AS INTEGER NO-UNDO.
    
    ASSIGN
     vl = length(myvalue)
     retval = 0
    .
    DO vp = vl to 1 by -1:
        ASSIGN vthisdigit = SUBSTRING(myvalue,vp,1).
        ASSIGN vthisvalue = INTEGER(vthisdigit) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN ASSIGN vthisvalue = ASC(vthisdigit) - 55.
        ASSIGN retval = retval + (vthisvalue * exp(36 ,(vl - vp))).
    END.
    RETURN retval.
END FUNCTION.

FUNCTION zeroPad RETURNS CHAR(
 INPUT mystring AS CHAR,
 INPUT mydigits AS INTEGER
):
    DO WHILE length(mystring) < mydigits:
        ASSIGN mystring = "0" + mystring.
    END.
    RETURN mystring.
END FUNCTION.


