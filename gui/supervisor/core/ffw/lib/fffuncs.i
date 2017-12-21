/*-----------------------------------------------------------------------*
  File........: fffuncs.i
  Version.....: Not yet assigned (7/30/2000)
  Description : Method library common to all webobjects.
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  United Systems, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000 - http://www.freeframework.org
  Created.....: 2/1/2000
  Notes.......: This file is heavily commented.  The best way to see what's 
  				going on here is to print this out and study the comments.
				
				If you make changes to this program that others could benefit
				from, please share your code with me: ses@usiatl.com
				
----------------------------------------------------------------------*/

&GLOBAL-DEFINE FFFUNCS_I


FUNCTION ErrorLog RETURNS LOGICAL (
 INPUT logstring AS CHAR 
) :
	ASSIGN logstring = "<!-- FFW message: Object {&web-file} says: ~"" + logstring + "~" -->".
	MESSAGE logstring.
RETURN TRUE.  

END FUNCTION.

FUNCTION DigitsOnly RETURNS CHARACTER (
INPUT mystring AS CHAR) :
/*------------------------------------------------------------------------------
  Returns:     Only the digits 0-9 of any given string, removing all alphanumeric 
                and delimiters.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR i AS INTEGER NO-UNDO.
DEFINE VAR retval AS CHAR NO-UNDO.
DO i = 1 TO LENGTH(mystring):
 IF CAN-DO({&DIGITS},SUBSTRING(mystring,i,1)) THEN ASSIGN retval = retval + SUBSTRING(mystring,i,1).
END.
 
RETURN retval.
END FUNCTION.

FUNCTION GetSetting RETURNS CHARACTER (
	INPUT aid AS CHAR,
	INPUT idt AS CHAR,
	INPUT st AS CHAR,
	INPUT sn AS CHAR
) :
/*------------------------------------------------------------------------------
  Returns:     The value of the setting record requested
  Parameters:  account id to which this setting applies
                type of account id listed 
                Setting Type
                Setting Name
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER f_setting FOR setting.
FIND f_setting
 WHERE f_setting.accountID = aid
 AND f_setting.IDType = idt
 AND f_setting.SettingType = st
 AND f_setting.SettingName = sn
 NO-LOCK NO-ERROR.
IF AVAIL f_setting THEN RETURN f_setting.SettingValue.
ELSE RETURN "". 
END FUNCTION.


/*------------------------------------------------------------------------------
  Returns:     The mysource list merged into the mytarget list
  Parameters:  mytarget - original list of items
  			   mysource - new list of items
  Notes:       
------------------------------------------------------------------------------*/
FUNCTION push RETURNS CHARACTER (
 INPUT mytarget AS CHAR,
 INPUT mysource AS CHAR
 ):
	DEFINE VAR mycount AS INTEGER NO-UNDO.
	
	IF mytarget = ? then assign mytarget = "".
	DO mycount = 1 TO NUM-ENTRIES(mysource):
		IF LOOKUP(ENTRY(mycount,mysource),mytarget) = 0
		 THEN ASSIGN mytarget = mytarget + "," + ENTRY(mycount,mysource).
	END.
	ASSIGN mytarget = TRIM(mytarget,",").
	RETURN mytarget.
END FUNCTION. /*PUSH*/

/*That's all*/

