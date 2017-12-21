/*-----------------------------------------------------------------------*
  File........: rdefs.i
  Version.....: 1.04 - 12/21/2000
  Description : Definitions pertaining to robustness features.  Currently 
                included by robust.i
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell - USI
  Copyright...: FreeFramework 2000 
  Created.....: 3/22/2000
  Notes.......: 
 *-----------------------------------------------------------------------*/
 
DEFINE STREAM runlog.  /*Runlogging stuff*/
DEFINE STREAM CFGSTREAM. /*Configuration File*/

DEFINE VAR v-propath 	      AS CHAR NO-UNDO.
DEFINE VAR v-temprpath        AS CHAR NO-UNDO.    /*Where to put compile-on-the-fly .r files*/
DEFINE VAR v-speedscripttypes AS CHAR NO-UNDO.    /*Comma list of filetypes that get compiled as speedscript*/
DEFINE VAR v-profilingOn      AS LOGICAL NO-UNDO. /* Whether or not to run the profiler */ 
DEFINE VAR v-profilerhdl      AS HANDLE.          /* Application Profiler Proc*/ 
DEFINE VAR v-profilerfilename AS CHAR NO-UNDO.    /* Filename to write to */

DEFINE NEW GLOBAL SHARED VAR v-runlogpath 		AS CHAR    NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR v-resettable 		AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR v-htmlcompileonfly AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR v-checkinterval 	AS INTEGER NO-UNDO. /*how often to recycle the wait-for*/
DEFINE NEW GLOBAL SHARED VAR v-batchprocname 	AS CHAR    NO-UNDO. /*procedure to run (if any) during recycle*/
DEFINE NEW GLOBAL SHARED VAR v-webrunpath		AS CHAR    NO-UNDO. /*list of directories from which we can run webobjects*/

{ ffw/lib/tt_ini.i }

&IF "{&FFW_DBDOWNLOCKCHECK}" = "YES" OR "{&FFW_SYSDOWNLOCKCHECK}" = "YES" &THEN
DEFINE VAR v-lockfiletextin   AS CHAR NO-UNDO.
&ENDIF


&IF "{&FFW_DBDOWNLOCKCHECK}" = "YES" &THEN
DEFINE VAR v-dbdownlockcheck  AS CHAR NO-UNDO.
DEFINE VAR v-dbdownlockfile   AS CHAR NO-UNDO.
DEFINE VAR v-dbdownmessage    AS CHAR NO-UNDO.
DEFINE VAR v-dbdownlocknow    AS LOGICAL NO-UNDO.
&ENDIF
&IF "{&FFW_SYSDOWNLOCKCHECK}" = "YES" &THEN
DEFINE VAR v-sysdownlockcheck AS CHAR NO-UNDO.
DEFINE VAR v-sysdownlockfile  AS CHAR NO-UNDO.
DEFINE VAR v-sysdownmessage   AS CHAR NO-UNDO.
DEFINE VAR v-sysdownlocknow   AS LOGICAL NO-UNDO.
&ENDIF

