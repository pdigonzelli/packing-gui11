 /*-----------------------------------------------------------------------  
 File: appdoc.html  
 Purpose: Application documentation utility.  
 Description:  
 Author(s) :Per S Digre / PSC  
 Created: August 1998  
 File: setup.p  
 Notes:      
 Modification History:      
 $Header: /cvsroot/freeframework/ffw1.2/ffw/plus/amdoc.p,v 1.1 2002/08/21 16:14:24 freeframework Exp $  
 $Log: amdoc.p,v $
 Revision 1.1  2002/08/21 16:14:24  freeframework
 Initial import

 Revision 1.1.1.1  2001/03/23 14:50:24  slichtenberg
 initial load 1.03
  
  
  
This file contains sample code which may assist you in creating applications.  
You may use the code as you see fit. If you modify the code or include it in another software program,  
you will refrain from identifying Progress Software as the supplier of the code, or using any  
Progress Software trademarks in connection with your use of the code.  
THE CODE IS NOT SUPPORTED BY PROGRESS SOFTWARE AND IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,  
INCLUDING, WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR NONINFRINGEMENT.  
-----------------------------------------------------------------------*/  
{src/web/method/cgidefs.i}  
  
DEF NEW GLOBAL SHARED VAR hHTML AS HANDLE NO-UNDO.  
FUNCTION fRow        RETURNS CHAR(INPUT cData   AS CHAR)  IN hHTML.  
FUNCTION fBeginTable RETURNS CHAR(INPUT cLabels AS CHAR)  IN hHTML.  
  
  
DEFINE VARIABLE cLineData      AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cLineData2     AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cOut     AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cOut2    AS CHARACTER NO-UNDO.  
DEFINE VARIABLE i1       AS INTEGER  NO-UNDO.  
DEFINE VARIABLE iLine    AS INTEGER  NO-UNDO.  
DEFINE VARIABLE iComment AS INTEGER  NO-UNDO.  
DEFINE VARIABLE iBlock   AS INTEGER  NO-UNDO.  
DEFINE VARIABLE iBLines  AS INTEGER  NO-UNDO.  
DEFINE VARIABLE iMLines  AS INTEGER  NO-UNDO.  
DEFINE VARIABLE cTemp    AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cType    AS CHARACTER NO-UNDO.  
DEFINE VARIABLE iLastProc AS INTEGER NO-UNDO.  
DEFINE VARIABLE lHeader  AS LOG NO-UNDO INITIAL TRUE.  
  
DEFINE STREAM sIni.  
DEFINE TEMP-TABLE tLine  
   FIELDS t-line    AS INTEGER  
   FIELDS t-block   AS INTEGER  
   FIELDS t-comment AS INTEGER  
   FIELDS t-type    AS CHARACTER  
   FIELDS t-extra   AS CHARACTER  
   FIELDS t-data    AS CHARACTER.  
DEFINE BUFFER bLine FOR tLine.  
  
  
FUNCTION fCreateLine RETURNS CHARACTER (INPUT cType AS CHARACTER):  
  /*** $$ This function creates a Line ****/  
  IF cType = "Comm" AND lHeader THEN cType = "Head".  
  IF iComment > 0 AND cType = "Comm"  
  THEN  cLineData2 = cLineData2 + TRIM(html-encode(cLineData)) + " || ".  
  ELSE DO:  
    IF /* INDEX(cLineData2 + cLineData,"$$") > 0 OR */ cType <> "Comm" THEN  
                                /***** Double '$$' marks comment section for reporting *****/  
    DO TRANSACTION:  
      CREATE tLine.  
      ASSIGN  
           t-line    = iLine  
           t-block   = iLastProc  
           t-Comment = IF cType = "Comm" THEN 1 ELSE 0  
           t-type    = cType  
           t-data    = cLineData2 + html-encode(cLineData).  
  
    END.  
    ASSIGN cLineData2      = "".  
  END.  
END FUNCTION.  
  
  
FUNCTION fSet RETURNS CHARACTER (INPUT cIdent AS CHARACTER,INPUT pType AS CHARACTER):  
  /*** This function creates a data row Line ****/  
  IF cOut BEGINS cIdent THEN DO:  
    ASSIGN cType = pType + "," + cIdent  
           tLine.t-data = TRIM(REPLACE(tLine.t-data,REPLACE(cIdent,":",""),"")).  
    IF tLine.t-data BEGINS ":" THEN tLine.t-data = TRIM(SUBSTRING(tLine.t-data,2)).  
  END.  
END FUNCTION.  
  
  
PROCEDURE amDoc:  
  DEFINE INPUT PARAMETER pFile AS CHARACTER NO-UNDO.  
  {&OUT} '<BR CLASS=page><H2>Program: '  pFile  '</h2>~n'.  
  INPUT STREAM sIni FROM VALUE(pFile).  
  
  /***** Read the input data ********/  
    REPEAT:  
      ASSIGN cLineData = "".  
      IMPORT STREAM sIni UNFORMATTED cLineData.  
      ASSIGN iLine = iLine + 1  
             cLineData   = REPLACE(REPLACE(cLineData,CHR(9)," "),'|','').  
  
  
      IF INDEX(cLineData,"/~*") > 0 THEN iComment = iComment + 1.  
  
      IF INDEX(cLineData,"*~/") > 0 THEN DO:  
        ASSIGN iComment = iComment - 1.  
        ASSIGN lHeader  = FALSE.  
        fCreateLine("Comm").  
        IF iComment > 0 THEN NEXT.  
      END.  
  
      IF iComment > 0 THEN DO:  
          fCreateLine("Comm").  
          NEXT.  
      END.  
  
      IF cLineData MATCHES "*RUN *"         THEN fCreateLine("RUN").  
      IF cLineData MATCHES "*~{inc*"        THEN fCreateLine("inc").  
      IF cLineData MATCHES "* INPUT PARAM*" THEN fCreateLine("INPT").  
      ASSIGN cLineData   = TRIM(cLineData).  
  
      IF cLineData BEGINS "FUNCTION " THEN DO:  
        IF iBLines = 0 AND iLastProc > 0 THEN DO:  
          FIND tLine WHERE t-line = iLastProc.  
          ASSIGN t-extra = "Error (Not closed)".  
        END.  
        ASSIGN iBLines = 1  
               iLastProc = iLine.  
        fCreateLine("FUNC").  
      END.  
      IF cLineData BEGINS "PROCEDURE " THEN DO:  
        IF iBLines = 0 AND iLastProc > 0 THEN DO:  
          FIND tLine WHERE t-line = iLastProc.  
          ASSIGN t-extra = "Error (Not closed)".  
        END.  
        ASSIGN iBLines = 1  
               iLastProc = iLine.  
        fCreateLine("PROC").  
      END.  
  
      IF cLineData BEGINS "END FUNCTION" THEN DO:  
        FIND tLine WHERE t-line = iLastProc.  
        IF AVAILABLE tLine THEN t-extra = STRING(iBLines) + " lines.".  
        ASSIGN iBLines = 0  
               iLastProc = 0.  
      END.  
  
      IF cLineData BEGINS "END PROCEDURE" THEN DO:  
        FIND tLine WHERE t-line = iLastProc.  
        IF AVAILABLE tLine THEN t-extra = STRING(iBLines) + " lines.".  
        ASSIGN iBLines = 0  
               iLastProc = 0.  
      END.  
  
      IF iBLines > 0 THEN iBLines = iBLines + 1.  
                     ELSE iMLines = iMLines + 1.  
    END.  
    INPUT STREAM sIni CLOSE.  
    ASSIGN cOut = ""  
           cLineData  = "".  
  
/***** RCS History Section Preparation *******/  
  FOR EACH tLine EXCLUSIVE-LOCK WHERE  
             t-type = "head" AND  
             t-data BEGINS "Revision "  
               BY t-line TRANSACTION:  
    FIND bLine EXCLUSIVE-LOCK WHERE bLine.t-line = tLine.t-line + 1.  
  
    ASSIGN tLine.t-data = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(tLine.t-data,"    "," "),"    "," "),"  "," "),"  "," ")," ","|")  
                        + "|" + REPLACE(bLine.t-data,"|","!")  
           tLine.t-type = "Rev".  
    DELETE bLine.  
  END.  
  
  /***** Header Section *******/  
  {&out} fBeginTable("Header|Info").  
  FOR EACH tLine EXCLUSIVE-LOCK WHERE  
           tLine.t-type = "head" BY tLine.t-line TRANSACTION:  
    ASSIGN cOut = REPLACE(tLine.t-data," ","").  
    IF cOut BEGINS "Modification"  
    THEN ASSIGN  
        tLine.t-data = REPLACE(tLine.t-data,"Modification","")  
        cOut         = REPLACE(tLine.t-data," ","").  
    fSet("File:","head1a").  
    fSet("Created:","head1b").  
    fSet("$Header:","Head2a").  
    fSet("Author(s):","head3a").  
    fSet("Syntax:","head3b").  
    fSet("Purpose:","head4a").  
    fSet("Description:","head5a").  
    fSet("Notes:","head6a").  
    fSet("Modification History:","head7a").  
    fSet("Modification","head7a").  
    fSet("History:","head7a").  
    fSet("$Log:","delete").  
    fSet("/~*----","delete").  
    fSet("------","delete").  
    fSet("*****","delete").  
    ASSIGN tLine.t-type = cType.  
    IF cType BEGINS "delete" OR LENGTH(TRIM(tLine.t-data)) < 5 THEN DELETE tLine.  
  END.  
  
  FOR EACH tLine EXCLUSIVE-LOCK WHERE  
           tLine.t-type BEGINS "head"  
               BREAK BY tLine.t-type BY tLine.t-line  
               TRANSACTION:  
    ASSIGN cOut = cOut + (IF cOut > "" THEN '<br>' /* "||" */ ELSE "") + tLine.t-data.  
    IF LAST-OF(tLine.t-type) THEN DO:  
      {&out} fRow(ENTRY(2,tLine.t-type + ",#") + "|" + cOut).  
      ASSIGN cOut = "".  
    END.  
    DELETE tLine.  
  END.  
  {&out} "</table>" SKIP.  
  
  /**** Program lines *********/  
  CREATE tLine.  
  ASSIGN tLine.t-data  = "MAIN CODE BLOCK"  
         tLine.t-extra = STRING(iMLines) + " lines."  
         iLastProc = 0.  
  
  {&out} fBeginTable("Line| " + STRING(iLine) + " lines|Code").  
  FOR EACH tLine NO-LOCK WHERE tLine.t-type <> "Rev" BY tLine.t-block BY tLine.t-line:  
    IF tLine.t-comment > 0  
    THEN {&out} fRow(STRING(tLine.t-line,">>>9") + " | " + tLine.t-extra + " | |8" + tLine.t-data + "|9").  
    ELSE {&out} fRow(STRING(tLine.t-line,">>>9") + " | " + tLine.t-extra + " | "   + tLine.t-data).  
  END.  
  {&out} "</table>" SKIP.  
  
  /***** RCS History Section Writing *******/  
  {&out} fBeginTable("Rev|Date|Time|User|RCS - Revision Comment").  
  FOR EACH tLine EXCLUSIVE-LOCK WHERE tLine.t-type = "Rev" BY tLine.t-line:  
    {&out} fRow(TRIM(SUBSTRING(tLine.t-data,10))).  
    DELETE tLine.  
  END.  
  {&out} "</table>" SKIP.  
END PROCEDURE.  
  
  
DEFINE INPUT PARAMETER pFile AS CHARACTER NO-UNDO.  
RUN amDoc(pFile).  
