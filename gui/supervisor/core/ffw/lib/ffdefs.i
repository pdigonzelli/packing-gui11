/*-----------------------------------------------------------------------*
  File........: ffdefs.i
  Version.....: Not yet assigned (8/2/2000)
  Description : Definitions common to all webobjects.
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  United Systems, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000 - http://www.freeframework.org
  Created.....: 8/2/2000
  Notes.......: This file is heavily commented.  The best way to see what's 
  				going on here is to print this out and study the comments.
				
				If you make changes to this program that others could benefit
				from, please share your code with me: ses@usiatl.com
				
----------------------------------------------------------------------*/

/* *************************  Definitions  ************************** */
&IF DEFINED(FFDEFS_I) = 0 &THEN  /*ONLY DEFINE THIS STUFF ONCE*/
&GLOBAL-DEFINE FFDEFS_I


/*So you can "can-do" and see if a given character is a digit*/
&GLOBAL-DEFINE DIGITS "0,1,2,3,4,5,6,7,8,9"


/*Days and months sometimes need to be displayed*/
&GLOBAL-DEFINE WEEKDAYS "Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday"
&GLOBAL-DEFINE MONTHS "January,February,March,April,May,June,July,August,September,October,November,December"


/*So you can have a consistent money format on your site.*/
&GLOBAL-DEFINE MONEY "$>>>,>>>,>>9.99"


/*So you can have a consistent time format on your site*/
&GLOBAL-DEFINE TIME "hh:mm:ss am"

/* So you can easily change paths going from development to deployment*/
/* Remember the trailing slash in the following 2 definitions*/
&GLOBAL-DEFINE WEBROOT "/"
&GLOBAL-DEFINE IMAGEROOT "/images/"


&ENDIF
