DEF VAR c1 AS CHAR NO-UNDO.
DEF STREAM s1.

DO WHILE TRUE:
INPUT STREAM s1 THROUGH value("c:\projects\plus\serve.plx").
REPEAT :
  IMPORT STREAM s1 c1.
  MESSAGE c1.
END.
INPUT STREAM s1 CLOSE.
PAUSE 3.
INPUT STREAM s1 THROUGH value("c:\projects\plus\chat.pl").
REPEAT :
  IMPORT STREAM s1 c1.
  MESSAGE c1.
END.
INPUT STREAM s1 CLOSE.
END.