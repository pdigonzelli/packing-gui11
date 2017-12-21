TreeView4GL :
-------------
	Version 2.3b. Installation notes.


Unzip all to a working directory. 
Copy TreeView4gl.* to c:\windows\system

Register Treeview4gl.ocx

   	 c:\windows\System\regsvr32.exe c:\windows\system\Treeview4gl.ocx

You can run testocx.w against a sports database.

The use of this control is free. You can develop and distribute freely your applications with this control. An e-mail at m.fondacci@4gl.fr to inform of your treeview usage, comments and suggestions would be very appreciated.

This freeware is delivered as "as-is" software and no responsability can be assumed in any case of malfunctions.


I hope you enyou with TreeView.

m.fondacci@4gl.fr

---------------------------------------------------------------------------------------
History of updates :
-	17/03/2000 : bug corrected in addAfterNode. The inserted node was not 		    		     	     correctly inserted and appears as a child node.
-	17/03/2000 : New method : deleteHNode()
-	17/03/2000 : New attribute : expanded( hNode ) to control the expansion of a node
-	17/03/2000 : New attribute : TreeRefresh
-	17/03/2000 : update of the testocx.w demo program.
-	27/03/2000: 					Version 2.3
-	27/03/2000: new event OnCollapsed,
-	27/03/2000: Image index allow now an opened/closed node image
-	27/03/2000: update of the testocx.w demo program
-	27/03/2000: the name of Active-X is now Treeview4GL.ocx
-       31/03/2000: minor bug corrected. When open/close images specified,
		    the expand of the SELECTED node does not correctly
                    displays the good icon. (Version 2.3b)
-	14/04/2000: 					Version 2.4
                    new methods : imageSize(width, height) to change the size of icons
		    in the treeview,
                    resetImages() to reset to standard image list
                    the bitmaps added to the treeview are now transparent.
                    set of includes provided to facilitate the treeview usage with PROGRESS.

