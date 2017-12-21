/* htmlcomp.p - Compiles .html file(s) containing embedded speedscript */
DEFINE INPUT  PARAMETER ipc_Directory AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipc_FileName  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipc_XREFFile  AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER opc_Errors    AS CHARACTER  NO-UNDO.

def var vfilefulepath   as char no-undo.
def var vcompilefile    as char no-undo.
def var vobjtype        as char no-undo.

DEFINE VARIABLE tvc_FileBase     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tvc_FileAbsolute AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tvc_Attribute    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tvi_ErrorNum     AS INTEGER    NO-UNDO.
DEFINE VARIABLE tvc_Extension    AS CHARACTER  NO-UNDO.

IF ipc_Directory NE "" THEN DO:
   /* Passed a directory name */
   FILE-INFO:FILE-NAME = ipc_Directory.
   IF index(FILE-INFO:FILE-TYPE, "D") = 0 THEN DO:
      opc_Errors = SUBSTITUTE("Directory name &1 passed is not a directory",
                              ipc_Directory).
      RETURN.
   END.

   INPUT FROM OS-DIR (FILE-INFO:FULL-PATHNAME).
   ReadBlock:
   REPEAT:
      IMPORT 
         tvc_FileBase
         tvc_FileAbsolute
         tvc_Attribute
      .

      IF NUM-ENTRIES(tvc_FileBase, ".") GT 1 THEN
         tvc_Extension = ENTRY(NUM-ENTRIES(tvc_FileBase, "."), tvc_FileBase, ".").
      ELSE
         tvc_Extension = "".

      IF NOT (tvc_Extension BEGINS "htm") THEN
         /* Skip files without .htm extension */
         NEXT ReadBlock.

      assign
          vobjtype        = ""
          vcompilefile    = "".                    

      run webutil/e4gl-gen.p
          (input        tvc_FileAbsolute,
           input-output vobjtype,
           input-output vcompilefile)
           no-error.

      IF opc_Errors NE "" THEN
         opc_Errors = opc_Errors + "~n".
      opc_Errors = opc_Errors +
         SUBSTITUTE("Compiling &1",
                    tvc_FileBase).

      IF ipc_XREFFile NE "" THEN
         COMPILE VALUE(vcompilefile) SAVE XREF value(ipc_XREFFile) APPEND NO-ERROR.
      ELSE
         compile value(vcompilefile) save no-error.

      IF ERROR-STATUS:ERROR = TRUE THEN DO:
         DO tvi_ErrorNum = 1 TO ERROR-STATUS:NUM-MESSAGES:
            opc_Errors = opc_Errors +
               SUBSTITUTE("~n** &1",
                          ERROR-STATUS:GET-MESSAGE(tvi_ErrorNum)).
         END.
      END.

      os-delete value(vcompilefile).

   END.

END. /* Passed a directory name */
ELSE DO:
   /* Single file compilation */
   assign
       vobjtype        = ""
       vcompilefile    = "".                    

   run webutil/e4gl-gen.p
       (input        ipc_FileName,
        input-output vobjtype,
        input-output vcompilefile).

   IF opc_Errors NE "" THEN
      opc_Errors = opc_Errors + "~n".
   opc_Errors = opc_Errors +
      SUBSTITUTE("Compiling &1",
                 ipc_FileName).

   IF ipc_XREFFile NE "" THEN
      COMPILE VALUE(vcompilefile) SAVE XREF value(ipc_XREFFile) APPEND NO-ERROR.
   ELSE
      compile value(vcompilefile) save no-error.

   IF ERROR-STATUS:ERROR = TRUE THEN DO:
      DO tvi_ErrorNum = 1 TO ERROR-STATUS:NUM-MESSAGES:
         opc_Errors = opc_Errors +
            SUBSTITUTE("~n** &1",
                       ERROR-STATUS:GET-MESSAGE(tvi_ErrorNum)).
      END.
   END.

   os-delete value(vcompilefile).

END. /* Single file compilation */

