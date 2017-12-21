/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: adefext.i

Description:
	
	Defines all the product names and file extensions.

Author: David Lee

Date Created: May 26, 1993

Modifications
  9/16/93 wood   changed "Freeze Frame" to "FreezeFrame" (conforms to doc).
          
----------------------------------------------------------------------------*/

&GLOBAL ADEFEXTI "" /* allow to check if this file has already been included */

/* ------------------------ APPLICATION NAMES  ----------------------------- */

/* Application Names */

&GLOBAL-DEFINE  UIB_NAME 	"User Interface Builder"
&GLOBAL-DEFINE  UIB_SHORT_NAME  UIB

&GLOBAL-DEFINE  FF_NAME         "FreezeFrame"

/* --------------------------- TEMP FILE EXTENSIONS ------------------------ */

&GLOBAL-DEFINE  STD_TYP_UIB_DUP	      "dp"
&GLOBAL-DEFINE  STD_TYP_UIB_CLIP      "cb"
&GLOBAL-DEFINE  STD_TYP_UIB_LAST      "ef"
&GLOBAL-DEFINE  STD_TYP_UIB_RADIO     "rb"
&GLOBAL-DEFINE  STD_TYP_UIB_DBFIELD   "db"
&GLOBAL-DEFINE  STD_TYP_UIB_COMPILE   "cf"
&GLOBAL-DEFINE  STD_TYP_UIB_TEMPLATE  "ct"
&GLOBAL-DEFINE  STD_EXT_UIB           ".uib"
&GLOBAL-DEFINE  STD_EXT_UIB_QS        ".qs"
&GLOBAL-DEFINE  STD_EXT_UIB_WVX       ".wrx"

&GLOBAL-DEFINE  STD_EXT_FF            ".ff"
