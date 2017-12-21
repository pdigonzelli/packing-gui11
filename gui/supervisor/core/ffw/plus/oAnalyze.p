/*-----------------------------------------------------------------------  
File: analyzer.p  
Purpose: Application code formatting utility.  
Description:  
Author(s) :Per S Digre / PSC  
Created: April 1999  
Notes:  
Modification History:      
$Header: /cvsroot/freeframework/ffw1.2/ffw/plus/oAnalyze.p,v 1.1 2002/08/21 16:14:24 freeframework Exp $  
$Log: oAnalyze.p,v $
Revision 1.1  2002/08/21 16:14:24  freeframework
Initial import

Revision 1.1.1.1  2001/03/23 14:50:33  slichtenberg
initial load 1.03
  
-----------------------------------------------------------------------*/  
  
DEFINE STREAM sIn.  
DEFINE STREAM sProg.  
  
  
/******* program parser **********************/  
/******* program parser **********************/  
/******* program parser **********************/  
  
DEFINE VARIABLE cIn       AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cIn2      AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cOut      AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cOut2     AS CHARACTER NO-UNDO.  
DEFINE VARIABLE c1        AS CHARACTER NO-UNDO.  
DEFINE VARIABLE c2        AS CHARACTER NO-UNDO.  
DEFINE VARIABLE c3        AS CHARACTER NO-UNDO.  
DEFINE VARIABLE c4        AS CHARACTER NO-UNDO.  
DEFINE VARIABLE i1        AS INTEGER  NO-UNDO.  
DEFINE VARIABLE i2        AS INTEGER  NO-UNDO.  
DEFINE VARIABLE i3        AS INTEGER  NO-UNDO.  
DEFINE VARIABLE i4        AS INTEGER  NO-UNDO.  
DEFINE VARIABLE iBlock    AS INTEGER  NO-UNDO.  
DEFINE VARIABLE iLine     AS INTEGER  NO-UNDO.  
DEFINE VARIABLE cLine     AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cQuote    AS CHARACTER NO-UNDO. /** Indicates type of Quote starter **/  
DEFINE VARIABLE iComment  AS INTEGER   NO-UNDO. /** Indicates commenting level     ***/  
DEFINE VARIABLE lHTML     AS LOG  NO-UNDO.  
DEFINE VARIABLE lTilde    AS LOG  NO-UNDO.  
DEFINE VARIABLE lScript   AS LOG  NO-UNDO.  
DEFINE VARIABLE cCommand  AS CHARACTER NO-UNDO.  
  
DEFINE VARIABLE cLineProg AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cLineComm AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cLineHTML AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cLineQuot AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cLastChar AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cCurrChar AS CHARACTER NO-UNDO.  
  
DEFINE VARIABLE cElemCmd  AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cElemData AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cLineCmd  AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cLineData AS CHARACTER NO-UNDO.  
  
DEFINE VARIABLE cOutputProg AS CHAR   NO-UNDO.  
DEFINE VARIABLE cBeautify  AS CHARACTER NO-UNDO.    /*** Can-do list of options ***/  
  /** pe - progress keyword expand    **/  
  /** pu - progress keyword uppercase **/  
  /** pl - progress keyword lowercase **/  
  /** hu - HTML     keyword uppercase **/  
  /** hl - HTML     keyword lowercase **/  
  
/**** call-back procedures ***  
analyzeElem(cCommand,cData)    1 command and corresponding data section  
analyzeLine(cCommands,cData)   chr(2) delimited strings of commands and data.  
  
Command name - Value  
CR    - Carriage Return (CR)  
PB    - Progress New Command Break (PB)  
PROG  - Progress code section  
COMM  - Commented code section  
QUOT  - Quoted string section  
HTML  - HTML code section  
SCRP  - Other script language section.  
******************************/  
  
  
/**** generic object definition section  ********/  
DEFINE VARIABLE hParent   AS HANDLE NO-UNDO.  
DEFINE VARIABLE cCallback AS CHAR   NO-UNDO.  
  
PROCEDURE callback:  
  DEFINE INPUT PARAMETER h1 AS HANDLE NO-UNDO.  
  DEFINE INPUT PARAMETER c1 AS CHAR   NO-UNDO.  
  ASSIGN hParent   = h1  
         cCallBack = c1.  
END PROCEDURE.  
  
  
  
PROCEDURE PROCESS:  
  DEFINE INPUT PARAMETER cInputFile  AS CHARACTER NO-UNDO.  
  /***** Initialize *****************/  
  INPUT  STREAM sIn FROM VALUE(cInputFile).  
  ASSIGN  
    lHtml    = cInputFile MATCHES "*.htm*"  
    iComment = 0  
    cQuote   = ""  
    lScript  = FALSE  
    lTilde   = FALSE.  
  
  IF cOutputProg > '' THEN OUTPUT STREAM sProg TO VALUE(cOutputProg).  
  
  
  /***** Read the input data ********/  
  
  REPEAT:  
    ASSIGN  
      cIn       = ""  
      cLine     = ""  
      cLineCmd  = ""  
      cLineData = ""  
      cLineComm = ""  
      cLineQuot = ""  
      cLineHTML = ""  
      cLineProg = "".  
    IMPORT STREAM sIn UNFORMATTED cIn.  
    ASSIGN cIn  = REPLACE(cIn,CHR(2)," ") + " "  
           iLine = iLine + 1.  
  
    DO i1 = 1 TO LENGTH(cIn):  
      ASSIGN cCurrChar = SUBSTRING(cIn,i1,1).  
      IF cQuote = ""  THEN cElemCmd = "PROG".  
                      ELSE cElemCmd = "QUOT".  
      IF iComment > 0 THEN cElemCmd = "COMM".  
      IF lHTML        THEN cElemCmd = "HTML".  
  
      CASE cElemCmd:  
       WHEN "HTML" THEN DO:  
        CASE cCurrChar:  
          WHEN "~`" THEN ASSIGN                 /**** Start of BACKTICK > CHANGE  ***/  
              lHTML     = FALSE.  
          WHEN "<"  THEN ASSIGN                 /**** New HTML command > Push last  ***/  
              cLineData = cLineData + cLineHTML  
              cLineHTML = cCurrChar.  
          WHEN ">"  THEN IF  
              cLineHTML MATCHES "*<SCRIPT*" AND  
              cLineHTML MATCHES "*SPEEDSCRIP*"     /**** SCRIPT SPEEDSCRIPT  ***/  
            THEN ASSIGN  
              lScript   = FALSE  
              lHTML     = FALSE.  
            ELSE IF cLineHTML MATCHES "*<SCRIPT*" THEN ASSIGN  
              lScript = TRUE                       /**** Other script section ***/  
              cLineData = cLineData + cLineHTML + cCurrChar + CHR(2)  
              cLineHTML = ""  
              cLineCmd  = cLineCmd + "HTML" + CHR(2).  
            ELSE ASSIGN  
              cLineHTML = cLineHTML + cCurrChar.  
          WHEN "/" THEN IF lScript AND cLastChar = "<" THEN ASSIGN  
              lScript = FALSE                       /**** END OF Other script section ***/  
              cLineData = cLineData + SUBSTRING(cLineHtml,1,LENGTH(cLineHtml) - 1) + CHR(2)  
              cLineHtml = "</"  
              cLineCmd  = cLineCmd + "SCRP" + CHR(2).  
            ELSE ASSIGN  
              cLineHTML = cLineHTML + cCurrChar.  
          WHEN ":" THEN IF  
            NOT lScript AND cLineHTML MATCHES "*javascript*" THEN ASSIGN  
              lScript = TRUE  
              cLineData = cLineData + cLineHTML + cCurrChar + CHR(2)  
              cLineHTML = ""  
              cLineCmd  = cLineCmd + "HTML" + CHR(2).  
            ELSE ASSIGN  
              cLineHTML = cLineHTML + cCurrChar.  
          WHEN "=" THEN IF  
            NOT lScript AND  
              CAN-DO("onClick,onChange,onBlur,onFocus,onSelect"  
                ,ENTRY(NUM-ENTRIES(cLineHTML," "),cLineHTML," ")) THEN ASSIGN  
              lScript = TRUE  
              cLineData = cLineData + cLineHTML + cCurrChar + CHR(2)  
              cLineHTML = ""  
              cLineCmd  = cLineCmd + "HTML" + CHR(2).  
            ELSE ASSIGN  
              cLineHTML = cLineHTML + cCurrChar.  
          OTHERWISE DO:  
            IF cLineHTML MATCHES "*!--WSS*"  
            THEN lHTML     = FALSE.                 /**** < ! WSS Speedscript COMMAND   ***/  
            ELSE cLineHTML = cLineHTML + cCurrChar.  
          END.  
        END CASE.  
        IF lHTML = FALSE THEN ASSIGN  
          cLineData = cLineData + cLineHTML + cCurrChar + CHR(2)  
          cLineHTML = ""  
          cLineCmd  = cLineCmd + (IF lScript THEN "SCRP" ELSE "HTML") + CHR(2).  
  
       END.  
  
       WHEN "COMM" THEN DO:  
        IF cLastChar + cCurrChar = "~/~*" THEN iComment = iComment + 1.  
        IF cLastChar + cCurrChar = "~*~/" THEN iComment = iComment - 1.  
        ASSIGN cLineComm = cLineComm + cCurrChar.  
        IF iComment = 0 THEN ASSIGN  
              cLineData = cLineData + cLineComm + CHR(2)  
              cLineComm = ""  
              cLineCmd  = cLineCmd + "COMM" + CHR(2).  
       END.  
  
       WHEN "QUOT" THEN DO:  
         ASSIGN cLineQuot = cLineQuot + cCurrChar.  
         IF cCurrChar = cQuote THEN ASSIGN  
              cQuote    = ""  
              cLineData = cLineData + cLineQuot + CHR(2)  
              cLineQuot = ""  
              cLineCmd  = cLineCmd + "QUOT" + CHR(2).  
       END.  
  
       WHEN "PROG" THEN DO:  
        IF lTilde  
        THEN ASSIGN cLineProg = cLineprog + cCurrChar.  
        ELSE CASE cCurrChar:  
          WHEN "~`" THEN ASSIGN  /**** End of BACKTICK > CHANGE  ***/  
              lHTML     = TRUE.  
          WHEN "'" OR WHEN '"' THEN ASSIGN      /**** Start Quoting  ***/  
              cQuote    = cCurrChar  
              cLineData = cLineData + cLineProg + CHR(2)  
              cLineProg = ""  
              cLineQuot = cCurrChar  
              cLineCmd  = cLineCmd + "PROG" + CHR(2).  
          WHEN ">" THEN DO:  
            IF cLineProg MATCHES "*--" THEN ASSIGN  
              lHTML     = TRUE  
              cLineHtml = SUBSTRING(cLineProg,LENGTH(cLineProg) - 2 + 1)  
              cLineProg = SUBSTRING(cLineProg,1,LENGTH(cLineProg) - 2).  
            ELSE IF cLineProg MATCHES "*~/SCRIPT*" THEN ASSIGN  
              lHTML     = TRUE  
              cLineHtml = SUBSTRING(cLineProg,LENGTH(cLineProg) - 8 + 1)  
              cLineProg = SUBSTRING(cLineProg,1,LENGTH(cLineProg) - 8).  
            ELSE   cLineProg = cLineprog + cCurrChar.  
          END.  
          OTHERWISE DO:  
            ASSIGN cLineProg = cLineprog + cCurrChar.  
          END.  
        END CASE.  
  
        IF cLastChar + cCurrChar = "~/~*" THEN ASSIGN  
              iComment  = 1  
              cLineComm = "~/~*"  
              cLineData = cLineData + SUBSTRING(cLineProg,1,LENGTH(cLineProg) - 2) + CHR(2)  
              cLineProg = ""  
              cLineCmd  = cLineCmd + "PROG" + CHR(2).  
              .  
  
        IF lHTML = TRUE THEN ASSIGN  
              cLineData = cLineData + cLineProg + CHR(2)  
              cLineProg = ""  
              cLineHTML = cLineHTML + cCurrChar  
              cLineCmd  = cLineCmd + "PROG" + CHR(2).  
       END.  
      END CASE.  
      ASSIGN cLastChar = IF lTilde THEN "X" ELSE cCurrChar  
             lTilde    = cCurrChar = "~~" AND NOT lTilde.  
    END.  
  
    CASE cElemCmd:  
      WHEN "HTML" THEN cLineData = cLineData + cLineHTML + cCurrChar.  
      WHEN "COMM" THEN cLineData = cLineData + cLineComm + cCurrChar.  
      WHEN "QUOT" THEN cLineData = cLineData + cLineQuot + cCurrChar.  
      WHEN "PROG" THEN cLineData = cLineData + cLineProg + cCurrChar.  
    END CASE.  
    ASSIGN cLineCmd  = cLineCmd + IF lScript AND cElemCmd = "HTML" THEN "SCRP" ELSE cElemCmd.  
  
    RUN processElements (cLineCmd,cLineData).  
  END.  
  IF cOutputProg > '' THEN OUTPUT STREAM sProg CLOSE.  
END PROCEDURE.  
  
  
FUNCTION fSplitCmd RETURNS LOG (INPUT-OUTPUT cCmd AS CHAR,INPUT-OUTPUT cData AS CHAR):  
  DEF VAR l1 AS LOG  NO-UNDO.  
  DEF VAR cBR AS CHAR NO-UNDO.  
  ASSIGN l1  = FALSE  
         cBR = CHR(2) + 'PB' + CHR(2).  
  IF INDEX(cData,'. ') > 0 THEN ASSIGN  
    cCmd  = cCmd + cBR + 'PROG'  
    cData = SUBSTRING(cData,1,INDEX(cData,'. ')) + cBR + SUBSTRING(cData,INDEX(cData,'. ') + 1)  
    l1 = TRUE.  
  IF INDEX(cData,': ') > 0 THEN ASSIGN  
    cCmd  = cCmd + cBR + 'PROG'  
    cData = SUBSTRING(cData,1,INDEX(cData,': ')) + cBR + SUBSTRING(cData,INDEX(cData,': ') + 1)  
    l1 = TRUE.  
  IF INDEX(cData,' then ') > 0 THEN ASSIGN  
    cCmd  = cCmd + cBR + 'PROG'  
    cData = SUBSTRING(cData,1,INDEX(cData,' then ')) + cBR + SUBSTRING(cData,INDEX(cData,' then ') + 1)  
    l1 = TRUE.  
  RETURN l1.  
END FUNCTION.  
  
  
PROCEDURE processElements:  
  DEF INPUT PARAM cLineCmd  AS CHAR NO-UNDO.  
  DEF INPUT PARAM cLineData AS CHAR NO-UNDO.  
  DEF VAR cRest AS CHAR NO-UNDO.  
  DEF VAR i1    AS INT  NO-UNDO.  
  DEF VAR i2    AS INT  NO-UNDO.  
  
  /*** inserting progress breaks ****/  
  ASSIGN i2 = NUM-ENTRIES(cLineCmd,CHR(2)).  
  DO i1 = i2 TO 1 BY -1:  
    ASSIGN cElemCmd  = ENTRY(i1,cLineCmd, CHR(2))  
           cElemData = ENTRY(i1,cLineData,CHR(2)).  
    IF cElemCmd = 'prog' THEN fSplitCmd(INPUT-OUTPUT cElemCmd,INPUT-OUTPUT cElemData).  
    ASSIGN ENTRY(i1,cLineCmd, CHR(2)) = cElemCmd  
           ENTRY(i1,cLineData,CHR(2)) = cElemData.  
  END.  
  
  /*** processing options per element ****/  
  DO i1 = 1 TO NUM-ENTRIES(cLineCmd,CHR(2)):  
    ASSIGN cElemCmd  = ENTRY(i1,cLineCmd, CHR(2))  
           cElemData = ENTRY(i1,cLineData,CHR(2)).  
    IF cBeautify > '' THEN RUN beautifyCmd(INPUT-OUTPUT cElemCmd,INPUT-OUTPUT cElemData).  
    IF ENTRY(1,cCallback) > '' THEN RUN VALUE(ENTRY(1,cCallback)) IN hParent (cElemCmd,cElemData).  
    IF cElemCmd <> "PB" THEN cLine = cLine + cElemData.  
  END.  
  
  /*** processing options per line ****/  
  IF cOutputProg > '' THEN PUT STREAM sProg UNFORMATTED cLine SKIP.  
  IF ENTRY(1,cCallback) > '' THEN RUN VALUE(ENTRY(1,cCallback)) IN hParent ('CR','CR').  
  IF ENTRY(2,cCallback) > '' THEN RUN VALUE(ENTRY(2,cCallback)) IN hParent (cLineCmd + CHR(2) + 'CR',cLineData + CHR(2) + 'CR').  
END PROCEDURE.  
  
/******* program beautifier **********************/  
/******* program beautifier **********************/  
/******* program beautifier **********************/  
  
  
PROCEDURE beautify:  
  DEFINE INPUT PARAMETER pBeautify   AS CHARACTER NO-UNDO.  
  DEFINE INPUT PARAMETER pOutputProg AS CHARACTER NO-UNDO.  
  ASSIGN  
    cBeautify   = pBeautify  
    cOutputProg = pOutputProg.  
END PROCEDURE.  
  
  
  
PROCEDURE beautifyCmd:  
  DEFINE INPUT-OUTPUT PARAMETER cElemCmd AS CHAR NO-UNDO.  
  DEFINE INPUT-OUTPUT PARAMETER cElemData AS CHAR NO-UNDO.  
  
  /**** Progress beautify *******/  
  IF cElemCmd = "Prog" THEN DO:  
    ASSIGN i2 = 1.  
    DO WHILE i2 < LENGTH(cElemData):  
      ASSIGN  
        c2 = SUBSTRING(cElemData,i2)  
        i3 = LENGTH(c2) + 1.  
      ASSIGN  i4 = INDEX(c2," ").  
      IF i4 > 0 AND i4 < i3 THEN i3 = i4.  
      ASSIGN  i4 = INDEX(c2,".").  
      IF i4 > 0 AND i4 < i3 THEN i3 = i4.  
      ASSIGN  i4 = INDEX(c2,",").  
      IF i4 > 0 AND i4 < i3 THEN i3 = i4.  
      ASSIGN  i4 = INDEX(c2,":").  
      IF i4 > 0 AND i4 < i3 THEN i3 = i4.  
      ASSIGN  i4 = INDEX(c2,"(").  
      IF i4 > 0 AND i4 < i3 THEN i3 = i4.  
      ASSIGN  i4 = INDEX(c2,")").  
      IF i4 > 0 AND i4 < i3 THEN i3 = i4.  
      IF i3 > 2 THEN DO:  
        ASSIGN cCommand = SUBSTRING(cElemData,i2,i3 - 1).  
        IF KEYWORD-ALL(cCommand) > "" THEN DO:  
          IF CAN-DO(cBeautify,'pe') THEN cCommand = KEYWORD-ALL(cCommand).  
          IF CAN-DO(cBeautify,'pu') THEN cCommand = CAPS(cCommand).  
          IF CAN-DO(cBeautify,'pl') THEN cCommand = LC(cCommand).  
        END.  
        ASSIGN SUBSTRING(cElemData,i2,i3 - 1) = cCommand  
               i3 = LENGTH(cCommand).  
      END.  
      ASSIGN i2 = i2 + i3.  
    END.  
  END.  
  
  
  /**** HTML beautify *******/  
  IF cElemCmd = "HTML" THEN DO:  
    ASSIGN i2 = 1.  
    DO WHILE i2 < LENGTH(cElemData):  
      ASSIGN  
        c2 = SUBSTRING(cElemData,i2)  
        i3 = LENGTH(c2) + 1.  
    ASSIGN  i4 = INDEX(c2," ").  
      IF i4 > 0 AND i4 < i3 THEN i3 = i4.  
      ASSIGN  i4 = INDEX(c2,">").  
      IF i4 > 0 AND i4 < i3 THEN i3 = i4.  
      IF i3 > 2 THEN DO:  
        ASSIGN cCommand = SUBSTRING(cElemData,i2,i3 - 1).  
        IF cCommand BEGINS "<" THEN DO:  
          IF CAN-DO(cBeautify,'hu') THEN cCommand = CAPS(cCommand).  
          IF CAN-DO(cBeautify,'hl') THEN cCommand = LC(cCommand).  
        END.  
        IF NUM-ENTRIES(cCommand,"=") > 1 THEN DO:  
          IF CAN-DO(cBeautify,'hu') THEN ENTRY(1,cCommand,"=") = CAPS(ENTRY(1,cCommand,"=")).  
          IF CAN-DO(cBeautify,'hl') THEN ENTRY(1,cCommand,"=") = LC(ENTRY(1,cCommand,"=")).  
        END.  
        ASSIGN SUBSTRING(cElemData,i2,i3 - 1) = cCommand  
               i3 = LENGTH(cCommand).  
      END.  
      ASSIGN i2 = i2 + i3.  
    END.  
  END.  
END PROCEDURE.  
  
