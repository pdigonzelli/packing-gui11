
/*------------------------------------------------------------------ 
    (c) 2000 Bob Mirro. All rights reserved.  Permission
             given to the Free Framework to distribute
             freely so long as this copyright is kept
             intact.
    
    File Name:     StreamFile.p
    Author:        Bob Mirro
    Creation Date: 1999


    Inputs Parameters:  ip_filefullpath - full path name to the binary
                                          file to be sent out to the
                                          webstream.
    Outputs Parameters: none
    

    Program Purpose:    This program takes a binary file and
                        send it out to the browser.
    
    Design: Expect the full path name of the file, calling programs
            can mess with search() and the like if they want
            so it's not done here.  Also be *VERY* careful - the
            HTTP headers should be written out before this
            program executes.  This is constructed as a PROCEDURE
            mean to be run persistantly with potential for
            SUPER's.  Implemented as a CGI wrapper as messing
            with HTTP headers and raw data is best done directly
            in the 4GL.
    
    Modification History:  08/03/00 GC - took original program
                                    and wrote comments for submission
                                    to FFW.
  -------------------------------------------------------------------*/


{ src/web/method/wrap-cgi.i }

DEFINE STREAM inputfile.

PROCEDURE StreamFile:

DEFINE INPUT PARAM ip_filefullpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE    vfileline       AS RAW NO-UNDO.



/* Must use the Binary qualifier here to prevent code page
   tanslations on the way in. */
                      
INPUT STREAM inputfile FROM VALUE(ip_filefullpath) BINARY NO-ECHO.

/* Read the file in 1024 byte chunks.  Input must be UNFORMATTED
   in order to read the binary codes. Output directly to the
   browser with using the WEBSTREAM pre-processor. PUT must also
   use CONTROL to prevent code page translations on the way out. */
                                    
REPEAT:
   LENGTH(vfileline) = 1024. 
   IMPORT STREAM inputfile UNFORMATTED vfileline.
   PUT {&WEBSTREAM} CONTROL vfileline.
END. /* repeat */

/* Clean-up: Raw variable should be reset to deallocate the memory
             and the stream should be closed. */                                                                 
                                                                 
LENGTH(vfileline) = 0.
            
INPUT STREAM inputfile CLOSE.


END. /* Procedure StreamFile */
