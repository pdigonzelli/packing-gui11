/*-----------------------------------------------------------------------*
  File........: ffpplib.i
  Version.....: not yet assigned (8/4/2000)
  Description : FreeFrameWork PlusPack Library
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell - United Systems, Inc. (770) 449-9696
  Copyright...: FreeFramework 2000  - http://www.freeframework.org
  Created.....: 8/4/2000
  Notes.......: In anticipation of us renaming all of the Workshop extensions
  				as the FreeFrameWork PlusPack
                SWL 07/06/2001 - added definition for version from .ini file
 *-----------------------------------------------------------------------*/
 
 { ffw/lib/agentsetting.i } 	/*AGENT SETTINGS LIBRARY*/
 { ffw/lib/devcheck.i }		/*CHECK WHETHER IN DEVELOPMENT MODE OR NOT*/
 { ffw/lib/showerrorscreen.i }  /*SHOW AN ERROR SCREEN WHEN THERE'S A PROBLEM*/

 DEFINE VAR ffimages AS CHAR NO-UNDO. /*Path for images used by the FFW PlusPack*/
 DEFINE VAR ffstyles AS CHAR NO-UNDO. /*Path for styles used by the FFW PlusPack*/
 DEFINE VAR ffjspath AS CHAR NO-UNDO. /*Path for js files used by the FFW PlusPack*/
 DEFINE VAR ffversion AS CHAR NO-UNDO. /*Path for js files used by the FFW PlusPack*/
 
 /* Move these agentSettings into variables so that we don't have to run the 
 	function repeatedly inside the HTML output */ 
	
 ASSIGN
  ffimages = getAgentSetting("FF.ImagePath")
  ffstyles = getAgentSetting("FF.StylePath")
  ffjspath = getAgentSetting("FF.JSPath")
  ffversion = getAgentSetting("FF.version")
 .
 

 
