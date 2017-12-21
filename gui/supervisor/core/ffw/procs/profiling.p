/*------------------------------------------------------------------ 
    (c) 2002 Innov8 Computer Software, LLC. All rights reserved.  
    (c) 2001 Shane Hollaway
    Redistribution authorized under FreeFrameWork License.
    
    File Name: Profiling.p
    Author:    Shane Hollaway shane.hollaway@gbst.com
               Originally posted on the peg Aug 17, 2001
    Creation Date: 2/15/02

    Called From:        Anywhere, with RUN StartProfiling (ProgramName).
                        and RUN StopProfiling.
    Programs Called:    NONE.


    Inputs Parameters:  ProfileFileName - the name you want to give the
                        output file.
    Outputs Parameters: NONE.
    

    Program Purpose:  Generic start and stop profile routines
    
    Description Of Design: Two PROCEDURE's are used, one to start the
                           profiled session, and one to end it.  The
                           profiling files are named based on an input
                           parameter so you can have an output file named
                           differently from the running program.  Files
                           are stored in the -T directory and have a running
                           counter appended to them.
                           
                           Generally this should be a persistent procedure,
                           either called via a procedure handle or run
                           as a SESSION-SUPER without the handle.  In a pinch,
                           include this file and call the PROCEDURE's from
                           the running program. 
    
    Modification History:  Originally posted on the PEG to the
                           webspeed@peg.com list
                           
  -------------------------------------------------------------------*/

DEFINE VARIABLE RunCount AS INTEGER NO-UNDO.

PROCEDURE StartProfiling.

DEFINE INPUT PARAMETER ProfileFileName AS CHARACTER NO-UNDO.

    /* Enable Profiling */
    ASSIGN RunCount = RunCount + 1
           ProfileFileName = ProfileFileName + "-" + STRING(RunCount) + ".prof".
                                
    /* message 'Profile Sesson: ' + ProfileFileName. */
    
    ASSIGN PROFILER:DIRECTORY    = SESSION:TEMP-DIRECTORY
           PROFILER:FILE-NAME    = ProfileFileName
           PROFILER:DESCRIPTION  = string( today, "99/99/9999" ) + ' ' +
                                   string( time, 'hh:mm:ss' ) + ' ' + ProfileFileName
           PROFILER:LISTINGS     = yes
           PROFILER:TRACE-FILTER = "*"
           PROFILER:COVERAGE     = yes
           PROFILER:ENABLED      = yes
           PROFILER:PROFILING    = yes
           NO-ERROR.
           
END. /* Start Profiling */

PROCEDURE StopProfiling.

    /* Disable Profiling */
    
    PROFILER:WRITE-DATA().
    
    ASSIGN PROFILER:PROFILING    = no
           NO-ERROR.

END. /* Stop Profiling */
