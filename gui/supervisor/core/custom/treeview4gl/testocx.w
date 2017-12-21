&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 
            The is a simple illustration of using
            the 4GL TreeView in a PROGRESS procedure.
            
            Not all functions are used. More information 
            can be found on documentation.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 
            Marcel FONDACCI
            m.fondacci@4gl.fr
            
  Created: 
            January 2000
            
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

Def var hSelected as iNT.

def var hNode as int no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BUTTON-1 BUTTON-4 BUTTON-7 BUTTON-3 ~
BUTTON-8 BUTTON-5 BUTTON-9 BUTTON-10 bSize BUTTON-11 bReset BUTTON-12 ~
BUTTON-13 BUTTON-6 BUTTON-14 BUTTON-17 BUTTON-18 BUTTON-15 BUTTON-16 ZZ ~
dChar wLines wTrack wButtons wImages Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS ZZ dChar wLines wTrack wButtons wImages 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE ChTreeview AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chChTreeview AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bReset 
     LABEL "Reset to standard" 
     SIZE 26.2 BY 1.14.

DEFINE BUTTON bSize 
     LABEL "Set image width/height" 
     SIZE 26.4 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     LABEL "Fill with customers (clear() + addNodes())" 
     SIZE 66 BY 1.14.

DEFINE BUTTON BUTTON-10 
     LABEL "AddChildNode" 
     SIZE 21 BY 1.14 TOOLTIP "Add a child node for the selected one.".

DEFINE BUTTON BUTTON-11 
     LABEL "First Child" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-12 
     LABEL "Last Child" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-13 
     LABEL "Next SIBLING" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-14 
     LABEL "# of nodes" 
     SIZE 21 BY 1.14 TOOLTIP "INFO.".

DEFINE BUTTON BUTTON-15 
     LABEL "Scan" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-16 
     LABEL "DeletehNode" 
     SIZE 21 BY 1.14 TOOLTIP "Delete the selected Node.".

DEFINE BUTTON BUTTON-17 
     LABEL "Expand/unExpand Node" 
     SIZE 27.4 BY 1.14 TOOLTIP "Delete the selected Node.".

DEFINE BUTTON BUTTON-18 
     LABEL "Expand all nodes" 
     SIZE 27.4 BY 1.14 TOOLTIP "Delete the selected Node.".

DEFINE BUTTON BUTTON-3 
     LABEL "GotohNode" 
     SIZE 21 BY 1.14 TOOLTIP "Goto Selected Node (gethSelectedNode)".

DEFINE BUTTON BUTTON-4 
     LABEL "getSelectedNode" 
     SIZE 21 BY 1.14 TOOLTIP "Get all atributes of selected node.".

DEFINE BUTTON BUTTON-5 
     LABEL "AddAfterNode" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-6 
     LABEL "DeleteNode" 
     SIZE 21 BY 1.14 TOOLTIP "Delete the selected Node.".

DEFINE BUTTON BUTTON-7 
     LABEL "replaceImage" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-8 
     LABEL "clearImages" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-9 
     LABEL "AddImages" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE ZZ AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 58 BY 4.52
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE dChar AS CHARACTER FORMAT "X(256)":U 
     LABEL "KEY" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1
     BGCOLOR 3 FGCOLOR 10  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28.8 BY 7.1.

DEFINE VARIABLE wButtons AS LOGICAL INITIAL yes 
     LABEL "show buttons" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.8 BY .81 NO-UNDO.

DEFINE VARIABLE wImages AS LOGICAL INITIAL yes 
     LABEL "with icons" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.8 BY .81 NO-UNDO.

DEFINE VARIABLE wLines AS LOGICAL INITIAL yes 
     LABEL "with lines" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.8 BY .81 NO-UNDO.

DEFINE VARIABLE wTrack AS LOGICAL INITIAL no 
     LABEL "track lines" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-1 AT ROW 1.48 COL 6
     BUTTON-4 AT ROW 1.48 COL 76
     BUTTON-7 AT ROW 2.43 COL 110.4
     BUTTON-3 AT ROW 2.91 COL 76
     BUTTON-8 AT ROW 3.62 COL 110.4
     BUTTON-5 AT ROW 4.33 COL 76
     BUTTON-9 AT ROW 4.81 COL 110.4
     BUTTON-10 AT ROW 5.52 COL 76
     bSize AT ROW 6.05 COL 103.4
     BUTTON-11 AT ROW 6.95 COL 76
     bReset AT ROW 7.38 COL 103.8
     BUTTON-12 AT ROW 8.14 COL 76
     BUTTON-13 AT ROW 9.33 COL 76
     BUTTON-6 AT ROW 9.71 COL 110.4
     BUTTON-14 AT ROW 10.91 COL 110.4
     BUTTON-17 AT ROW 11.29 COL 76.4
     BUTTON-18 AT ROW 12.52 COL 76.4
     BUTTON-15 AT ROW 12.76 COL 110.4
     BUTTON-16 AT ROW 13.91 COL 110.4
     ZZ AT ROW 15.19 COL 74 NO-LABEL
     dChar AT ROW 20 COL 117.8 COLON-ALIGNED
     wLines AT ROW 20.29 COL 73.2
     wTrack AT ROW 21.38 COL 73.2
     wButtons AT ROW 22.38 COL 73.2
     wImages AT ROW 23.38 COL 73.2
     Btn_OK AT ROW 24.33 COL 117
     "AddNodes CHAR value" VIEW-AS TEXT
          SIZE 33 BY .95 AT ROW 14.1 COL 74.4
     " Images" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.48 COL 113
     RECT-1 AT ROW 1.95 COL 102.2
     SPACE(2.19) SKIP(17.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "4GL TreeView"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME ChTreeview ASSIGN
       FRAME        = FRAME Dialog-Frame:HANDLE
       ROW          = 2.67
       COLUMN       = 6
       HEIGHT       = 22.86
       WIDTH        = 67
       HIDDEN       = no
       SENSITIVE    = yes.
      ChTreeview:NAME = "ChTreeview":U .
/* ChTreeview OCXINFO:CREATE-CONTROL from: {6C00BE45-F188-11D2-8CE6-00A0D21A0A6B} type: TreeView4GL */
      ChTreeview:MOVE-AFTER(BUTTON-7:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* 4GL TreeView */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bReset Dialog-Frame
ON CHOOSE OF bReset IN FRAME Dialog-Frame /* Reset to standard */
DO:
  chChTreeview:TreeView4GL:ImageSize(16, 16).
  chChTreeview:TreeView4GL:resetImages().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bSize Dialog-Frame
ON CHOOSE OF bSize IN FRAME Dialog-Frame /* Set image width/height */
DO:
  chChTreeview:TreeView4GL:imageSize(30, 30).
  chChTreeview:TreeView4GL:addimage("3030a.bmp").
  chChTreeview:TreeView4GL:addimage("3030b.bmp").
  chChTreeview:TreeView4GL:addimage("3030c.bmp").

  chChTreeview:TreeView4GL:addimage("3030a.bmp").
  chChTreeview:TreeView4GL:addimage("3030b.bmp").
  chChTreeview:TreeView4GL:addimage("3030c.bmp").
  
  chChTreeview:TreeView4GL:addimage("3030a.bmp").
  chChTreeview:TreeView4GL:addimage("3030b.bmp").
  chChTreeview:TreeView4GL:addimage("3030c.bmp").
  
  chChTreeview:TreeView4GL:addimage("3030a.bmp").
  chChTreeview:TreeView4GL:addimage("3030b.bmp").
  chChTreeview:TreeView4GL:addimage("3030c.bmp").    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 Dialog-Frame
ON CHOOSE OF BUTTON-1 IN FRAME Dialog-Frame /* Fill with customers (clear() + addNodes()) */
DO:
Def var A   as char no-undo.
        
Def var i as int.
Def var j as int.       
 
/*  You can use a common level for all nodes with :
        A =        "0~tAll customers~t2~t3~t~t". 
    and incresae the level number by 1 on each node
    
*/
  
  chChTreeview:TreeView4GL:TreeRefresh = false.
  
  For each customer no-lock 
    by name:
        chChTreeview:TreeView4GL:addnodes( "0~t" + Customer.Name + "~t7~t4~t~tCustomer=" + string(Cust-num)).
        A = A  + "~n0~t" + Customer.Name + "~t7~t4~t~tCustomer=" + string(Cust-num).
        For each order of customer no-lock :
            chChTreeview:TreeView4GL:addnodes( "1~tOrder " + string(Order-Num) + "~t40,41~t~t~tOrder=" + string(Order-num)).
                     
            A = A + "~n1~t" + "Order " + string(Order-Num)
                     + "~t40,41~t~t~tOrder=" + string(Order-num).
            end.
        end.
   
   chChTreeview:TreeView4GL:TreeRefresh = true.
      
   a = substring(a, 2).
   
   zz:screen-value = "# of nodes:" + string(num-entries(A, "~n")) + "~n" + A.
   
/*   chChTreeview:TreeView4GL:clear().     
 *    chChTreeview:TreeView4GL:addnodes(a).*/
   
   apply "entry" to ChTreeview.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 Dialog-Frame
ON CHOOSE OF BUTTON-10 IN FRAME Dialog-Frame /* AddChildNode */
DO:
Def Var a as char.
Def Var NodeDesc as char no-undo.
Def Var thelevel as int no-undo.
Def var SelectedNode    as int NO-UNDO.

  a = chChTreeview:TreeView4GL:GetSelectedNode().
  
  SelectedNode = int( entry(1, A, chr(1)) ).
  
  NodeDesc = entry(2, a, chr(1) ).         /* Full description of Node */
  
  Thelevel =   INT(entry(1, NodeDesc, "~t" )) + 1.  
    
  
  chChTreeview:TreeView4GL:addafterNode(
                                    SelectedNode, 
                                    string(TheLevel)
                                    + "~t"
                                    + "Added Child"
                                    + "~t37~t38~t3~tChild"
                                    ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 Dialog-Frame
ON CHOOSE OF BUTTON-11 IN FRAME Dialog-Frame /* First Child */
DO:
def var A     as char no-undo.

def var ChildrenNumber as int no-undo.

  A = chChTreeview:TreeView4GL:GetSelectedNode().

  hSelected = int( entry(2, A, chr(1)) ).           /* Handle */
  ChildrenNumber =  chChTreeview:TreeView4GL:ChildrenNumber(hSelected).
  
  If ChildrenNumber > 0 then
        hNode =  chChTreeview:TreeView4GL:FirstChild( hSelected ).
  Message "Your node has"
            ChildrenNumber string(ChildrenNumber > 1, "children./child.")
            skip(1)
            (If hNode > 0 then
                "Node Info of first child is :   " +
                chChTreeview:TreeView4GL:GethNode( hNode )
             else ""
             )
             
         view-as alert-box.
            
                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 Dialog-Frame
ON CHOOSE OF BUTTON-12 IN FRAME Dialog-Frame /* Last Child */
DO:
def var A     as char no-undo.
def var ChildrenNumber as int no-undo.

  A = chChTreeview:TreeView4GL:GetSelectedNode().

  hSelected = int( entry(2, A, chr(1)) ).           /* Handle */
  ChildrenNumber =  chChTreeview:TreeView4GL:ChildrenNumber(hSelected).
  
  If ChildrenNumber > 0 then
        hNode =  chChTreeview:TreeView4GL:LastChild( hSelected ).
  Message "Your node has"
            ChildrenNumber string(ChildrenNumber > 1, "children./child.")
            skip(1)
            (If hNode > 0 then
                "Node Info of last child is :   " +
                chChTreeview:TreeView4GL:GethNode( hNode )
             else ""
             )
             
         view-as alert-box.
            
                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 Dialog-Frame
ON CHOOSE OF BUTTON-13 IN FRAME Dialog-Frame /* Next SIBLING */
DO:
def var A     as char no-undo.

def var ChildrenNumber as int no-undo.

  A = chChTreeview:TreeView4GL:GetSelectedNode().

  hSelected = int( entry(2, A, chr(1)) ).           /* Handle */
  ChildrenNumber =  chChTreeview:TreeView4GL:ChildrenNumber(hSelected).
  
  hNode =  chChTreeview:TreeView4GL:NextSibling( hSelected ).
        
  Message "Your node has"
            ChildrenNumber string(ChildrenNumber > 1, "children./child.")
            skip(1)
            (If hNode > 0 then
                "Node Info of next sibling child is :   " +
                chChTreeview:TreeView4GL:GethNode( hNode )
             else "No next sibling node."
             )
             
         view-as alert-box.
            
                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 Dialog-Frame
ON CHOOSE OF BUTTON-14 IN FRAME Dialog-Frame /* # of nodes */
DO:
  /* Specify 0 for the handle to have the total
     number of nodes
     */
  Message "The total # of nodes is"
            chChTreeview:TreeView4GL:ChildrenNumber(0)
            view-as alert-box info.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 Dialog-Frame
ON CHOOSE OF BUTTON-15 IN FRAME Dialog-Frame /* Scan */
DO:
  def var i as int.
  def var a as char.
  
  def var ok as logical.
  
  do i = 1 to chChTreeview:TreeView4GL:ChildrenNumber(0) :
        a =  chChTreeview:TreeView4GL:GetNode( I - 1).  /* Rel to 0 */
        message "Node number" entry(1, a, chr(1)) skip
                "Private Data" entry(6, a, "~t") 
                skip(1) 
                "Do you wish to continue ?" view-as alert-box QUESTION                
                buttons yes-no update OK.
        if not ok then leave.
        end.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-16 Dialog-Frame
ON CHOOSE OF BUTTON-16 IN FRAME Dialog-Frame /* DeletehNode */
DO:
def var i as char.

  i = chChTreeview:TreeView4GL:GetSelectedNode().
  chChTreeview:TreeView4GL:DeletehNode(int(entry(2, i, chr(1)))).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-17 Dialog-Frame
ON CHOOSE OF BUTTON-17 IN FRAME Dialog-Frame /* Expand/unExpand Node */
DO:
def var i     as char no-undo.
def var hNode as int no-undo.

  i = chChTreeview:TreeView4GL:GetSelectedNode().
  hNode = int(entry(2, i, chr(1))).
  
  
  chChTreeview:TreeView4GL:Expanded( hNode ) = 
                NOT chChTreeview:TreeView4GL:Expanded( hNode ).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-18 Dialog-Frame
ON CHOOSE OF BUTTON-18 IN FRAME Dialog-Frame /* Expand all nodes */
DO:
def var i     as int  no-undo.
def var a     as char no-undo.

def var hNode as int no-undo.
 
  /* Set the REFRESH MODE TO FALSE
     ===========================*/
      
  chChTreeview:TreeView4GL:TreeRefresh = false.
  
    do i = 1 to chChTreeview:TreeView4GL:ChildrenNumber(0) :
        a =  chChTreeview:TreeView4GL:GetNode( I - 1).  /* Rel to 0 */
        
        hNode = int(entry(2, a, chr(1))).       /* Get the node handle */
        
        If Not chChTreeview:TreeView4GL:Expanded( hNode ) then
                chChTreeview:TreeView4GL:Expanded( hNode ) = true.      
        end.

  /* Set the REFRESH MODE TO TRUE
     ==========================*/
             
   chChTreeview:TreeView4GL:TreeRefresh = true.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 Dialog-Frame
ON CHOOSE OF BUTTON-3 IN FRAME Dialog-Frame /* GotohNode */
DO:
  If hSelected > 0 then
          chChTreeview:TreeView4GL:GotohNode( hSelected ).
  else
          Message "You must FIRST select a node with"
                  "'gethSelectedNode' button."
                  view-as alert-box INFO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 Dialog-Frame
ON CHOOSE OF BUTTON-4 IN FRAME Dialog-Frame /* getSelectedNode */
DO:
def var a as char NO-UNDO.
  
  a =  chChTreeview:TreeView4GL:GetSelectedNode().
  hselected = int(entry(2, a, chr(1))).
  Message   "Node #" entry(1, a, chr(1))    skip
            "Node Handle =" hselected       skip
          
            replace(a, "~t", "<TAB>" )
            view-as alert-box INFO
            title "Selected Node".
            

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 Dialog-Frame
ON CHOOSE OF BUTTON-5 IN FRAME Dialog-Frame /* AddAfterNode */
DO:
Def Var a as char.
Def Var NodeDesc as char no-undo.
Def Var thelevel as char no-undo.
Def var SelectedNode    as int NO-UNDO.

  a = chChTreeview:TreeView4GL:GetSelectedNode().
  
  SelectedNode = int( entry(1, A, chr(1)) ).
  
  NodeDesc = entry(3, a, chr(1) ).         /* Full description of Node */
  
  Thelevel =   entry(1, NodeDesc, "~t" ).
  
  chChTreeview:TreeView4GL:addafterNode(
                                    SelectedNode, 
                                    TheLevel
                                    + "~t"
                                    + (If thelevel = "0" then 
                                                "Added Customer"
                                       else 
                                                "Added Order")
                                    + "~t1~t4~t3~tAAAA").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 Dialog-Frame
ON CHOOSE OF BUTTON-6 IN FRAME Dialog-Frame /* DeleteNode */
DO:
def var i as char.
  i = chChTreeview:TreeView4GL:GetSelectedNode().
  chChTreeview:TreeView4GL:DeleteNode(int(entry(1, i, chr(1)))).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 Dialog-Frame
ON CHOOSE OF BUTTON-7 IN FRAME Dialog-Frame /* replaceImage */
DO:
Def var BmpName as CHAR NO-UNDO.
  BmpName = search("hlp.bmp").
  If BmpName <> ? then
          chChTreeview:TreeView4GL:ReplaceImage(1, search("hlp.BMP")).
  else
          Message "'hlp.bmp' cannot be found is your PROPATH."
                  view-as alert-box INFO.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 Dialog-Frame
ON CHOOSE OF BUTTON-8 IN FRAME Dialog-Frame /* clearImages */
DO:
  /* Clear all images
     ==============*/
     
  chChTreeview:TreeView4GL:clearImages().
          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 Dialog-Frame
ON CHOOSE OF BUTTON-9 IN FRAME Dialog-Frame /* AddImages */
DO:
Def var BmpName as CHAR NO-UNDO.
  
  /*    Add images to the image list
        ==========================*/

  BmpName = search("drivenet.bmp").
  If BmpName <> ? then do :          
                    chChTreeview:TreeView4GL:addimage(search("hlp.bmp")).
                    chChTreeview:TreeView4GL:addimage(search("drivenet.bmp")).                      
                    chChTreeview:TreeView4GL:addimage(search("hlp.bmp")).
                    chChTreeview:TreeView4GL:addimage(search("drive.bmp")).
                    chChTreeview:TreeView4GL:addimage(search("drivedsc.bmp")).
                    chChTreeview:TreeView4GL:addimage(search("mapnet.bmp")).
                    chChTreeview:TreeView4GL:addimage(search("mycomp.bmp")).
                    chChTreeview:TreeView4GL:addimage(search("nethood.bmp")).
                    end.
    else
            Message "Images canot be found in your PROPATH."
                    view-as alert-box INFO.                    
          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ChTreeview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ChTreeview Dialog-Frame
PROCEDURE ChTreeview.TreeView4GL.OnChange .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    NodeInfo
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-NodeInfo AS CHARACTER NO-UNDO.


/*Message "OnChange" skip p-nodeinfo view-as alert-box.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ChTreeview Dialog-Frame
PROCEDURE ChTreeview.TreeView4GL.OnCollapsed .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    NodeInfo
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-NodeInfo AS CHARACTER NO-UNDO.

/* Here you can add your instructions in case of an node Collapse
   event
   */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ChTreeview Dialog-Frame
PROCEDURE ChTreeview.TreeView4GL.OnExpanded .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    NodeInfo
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-NodeInfo AS CHARACTER NO-UNDO.

/*
 *          Sample code to illustrate an icon change
 *          after 'Expanded event'
 *
 *          You should do the same in OnCollapsed event.
 *
 * Def var NodeNumber  as int  NO-UNDO.
 * 
 * NodeNumber =  int( entry(1, P-NodeInfo, chr(1)) ).
 * p-NodeInfo = entry(3, p-Nodeinfo, chr(1)).
 * 
 * /* Icon replace
 *    ==========*/
 *    
 * entry(3, p-NodeInfo, "~t") = "20".
 *    
 * chChTreeview:TreeView4GL:ChangeNode( NodeNumber, p-NodeInfo).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ChTreeview Dialog-Frame
PROCEDURE ChTreeview.TreeView4GL.OnKeyPress .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Key
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-Key AS INTEGER NO-UNDO.

dChar:screen-value in frame {&FRAME-NAME} = 
           "CTRL-" + chr(p-Key + asc("a") - 1).

/* message p-key view-as alert-box. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wButtons
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wButtons Dialog-Frame
ON VALUE-CHANGED OF wButtons IN FRAME Dialog-Frame /* show buttons */
DO:
  chChTreeview:TreeView4GL:Buttons = self:Checked.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wImages
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wImages Dialog-Frame
ON VALUE-CHANGED OF wImages IN FRAME Dialog-Frame /* with icons */
DO:
  chChTreeview:TreeView4GL:Images = self:Checked.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wLines
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wLines Dialog-Frame
ON VALUE-CHANGED OF wLines IN FRAME Dialog-Frame /* with lines */
DO:
  chChTreeview:TreeView4GL:Lines = self:Checked.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wTrack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wTrack Dialog-Frame
ON VALUE-CHANGED OF wTrack IN FRAME Dialog-Frame /* track lines */
DO:
  chChTreeview:TreeView4GL:Track = self:Checked.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "testocx.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chChTreeview = ChTreeview:COM-HANDLE
    UIB_S = chChTreeview:LoadControls( OCXFile, "ChTreeview":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "testocx.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  RUN control_load.
  DISPLAY ZZ dChar wLines wTrack wButtons wImages 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 BUTTON-1 BUTTON-4 BUTTON-7 BUTTON-3 BUTTON-8 BUTTON-5 BUTTON-9 
         BUTTON-10 bSize BUTTON-11 bReset BUTTON-12 BUTTON-13 BUTTON-6 
         BUTTON-14 BUTTON-17 BUTTON-18 BUTTON-15 BUTTON-16 ZZ dChar wLines 
         wTrack wButtons wImages Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


