/*cleaninclude.p - removes redundant "{&out}" statements*/

DEFINE INPUT PARAMETER pFileName AS CHAR NO-UNDO.
DEFINE VAR vtextin AS CHAR NO-UNDO.
DEFINE VAR vLineCount AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE ttline
 FIELD lineno AS INTEGER
 FIELD linetext AS CHAR
 INDEX lineno IS UNIQUE PRIMARY lineno.
DEFINE BUFFER nextline FOR ttline.
DEFINE VAR vInOut AS LOGICAL NO-UNDO.

OS-RENAME VALUE(pFileName) VALUE(pFileName + ".bak").

INPUT FROM VALUE(pFileName + ".bak").
    REPEAT:
        IMPORT UNFORMATTED vTextin.
        ASSIGN vLineCount = vLineCount + 1.
        CREATE ttLine.
        ASSIGN
         ttLine.lineNo = vLineCount
         ttLine.lineText = vTextin
        .
        

    END.
INPUT CLOSE.
OUTPUT TO VALUE(pFileName).
    PUT UNFORMATTED "/* Cleaned by FFW Clean */".
    FOR EACH ttLine:
        IF ttLine.lineText BEGINS "~{&OUT~}" THEN DO:
            IF vInOut THEN DO:
                ASSIGN ttline.linetext = SUBSTRING(ttline.linetext,7,-1).
	    END.
             /* keep out since this is first line*/
             /*strip period if following line is also an out.*/
            FIND nextline WHERE nextline.lineno = ttline.lineno + 1
             NO-LOCK NO-ERROR.
            IF AVAIL nextline AND nextline.linetext BEGINS "~{&OUT~}" 
             THEN ASSIGN
              vInout = TRUE
              ttline.linetext = SUBSTRING(ttline.linetext,1,LENGTH(ttline.linetext) - 1)
             .
            ELSE vInOut = FALSE.      
        END. /*BEGINS WITH OUT*/
        ASSIGN ttLine.Linetext = REPLACE(ttLine.linetext,"/*Tag=<!--WSS*/","").
        ASSIGN ttLine.Linetext = REPLACE(ttLine.linetext,"/*Tag=-->*/","").
        ASSIGN ttLine.Linetext = REPLACE(ttLine.linetext,"/*Tag=<~%*/","").
        ASSIGN ttLine.Linetext = REPLACE(ttLine.linetext,"/*Tag=~%>*/","").
        ASSIGN ttLine.Linetext = REPLACE(ttLine.linetext,"/*Tag=`*/","").
        PUT UNFORMATTED ttLine.lineText "~n".
    END.
OUTPUT CLOSE.
