/*-----------------------------------------------------------------------*
File........: unEscape.i
Version.....: 1.0 
Description : Contains unEscape() function for ensuring safe system calls
Author......: S.E. Southwell - BravePoint, Inc.
Copyright...: FreeFramework 2001 - http://www.freeframework.org
Created.....: November 18, 2001
Notes.......: Use the unEscape() function on email addresses or other incoming 
              strings prior to using those strings as part of any command that
              runs on the system, such as OS-COMMAND or INPUT THROUGH.
              
              This prevents malicious users from inserting commands into their 
              information and causing your machine to execute arbitrary code.
              
              NO WARRANTIES EXPRESSED OR IMPLIED.  USE AT OWN RISK AND IN ACCORDANCE 
              WITH FREEFRAMEWORK LICENSE 
Modified:
*-----------------------------------------------------------------------*/

FUNCTION unEscape RETURNS CHAR(
 INPUT myText AS CHAR
):
    DEFINE VAR badCharList AS CHAR NO-UNDO.
    DEFINE VAR charcount   AS INT  NO-UNDO.
    
    ASSIGN badCharList = ";,|,>,<,^,&,~n,~~,*,?,',~",[,],~{,~},~;,$,~`".
    DO charCount = 1 to NUM-ENTRIES(badCharList):
        ASSIGN myText = REPLACE(myText,ENTRY(charCount,badCharList),"").
    END. /*charCount*/
    RETURN myText.
END FUNCTION. /*unEscape*/

