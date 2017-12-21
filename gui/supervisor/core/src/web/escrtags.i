/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*-------------------------------------------------------------------------

File: src/web/escrtags.i

Description: Include file for eScript.

Notes:

Author: Matt Verrinder

Created: 8th October 2003

---------------------------------------------------------------------------*/

&GLOBAL-DEFINE xcScriptTagStart "<%":U    
&GLOBAL-DEFINE xcScriptTagEnd "%>":U    
&GLOBAL-DEFINE xcValueTagStart "<%=":U    
&GLOBAL-DEFINE xcValueTagEnd "%>":U    
    
/* eScript2 tags are:    
  <%=function([param-list])%>    
  <%if:function([param-list])%>...<%else:function%>...<%end:function%>    
  <%while:function([param-list])%>...<%end:function%>
  <%repeat:function([param-list])%>...<%end:function%>
  <%include:textname%>    
  <%discard:start%>...<%discard:end%>    
  <%call:function([param-list])%>...<%end:function%>
*/    
    
&GLOBAL-DEFINE xcFunctionDelimiter ":":u
    
&GLOBAL-DEFINE xcIfStart      "<%if":u    
&GLOBAL-DEFINE xcIfElse       "<%else":u    
&GLOBAL-DEFINE xcIfEnd        "<%end":u    
&GLOBAL-DEFINE xcRepeatStart  "<%repeat":u    
&GLOBAL-DEFINE xcRepeatEnd    "<%end":u    
&GLOBAL-DEFINE xcWhileStart   "<%while":u
&GLOBAL-DEFINE xcWhileEnd     "<%end":u
&GLOBAL-DEFINE xcDiscardStart "<%discard" + {&xcFunctionDelimiter} + "start%>":u    
&GLOBAL-DEFINE xcDiscardEnd   "<%discard" + {&xcFunctionDelimiter} + "end%>":u    
&GLOBAL-DEFINE xcFieldStart   "<%call":u    
&GLOBAL-DEFINE xcFieldEnd     "<%end":u    
    
&GLOBAL-DEFINE xcScriptCommentStart "<%--":U    
&GLOBAL-DEFINE xcScriptCommentEnd   "--%>":U    
&GLOBAL-DEFINE xcIncludeStart       "<%include":U    
&GLOBAL-DEFINE xcIncludeEnd         "%>":U    

