/*-------------------------------------------------------------------
File........: smtpmail.p
Version.....: 5.3 - January 3, 2003
Description : Opens up an SMTP connection and sends an email message  
               to multiple recipients.
Input Param : 
     mailhub     char - settable SMTP server name/IP address
                 optionally append :XX where X is a port number of the
                 SMTP server.  25 is default port.
     EmailTo     CHAR - list of email addresses separated by 
                 semicolons or commas (All semicolons will 
                 be replaced by commas so don't include stuff
                 like Smith, John)  <me@myaddress.org>"My Name"
     EmailFrom   CHAR - email address of user originating the email, 
                 the SMTP server should require that this user 
                 is real. Format looks like:
                 <user>@<host>[;<descriptive name>]
                 Example:
                 foo@bar.com[;Mr. Foo Bar]
     EmailCC     CHAR - list of email addresses separated by 
                 semicolons or commas (All semicolons will 
                 be replaced by commas so don't include stuff
                 like Smith, John) <me@myaddress.org>"My Name"
     Attachments CHAR - Comma separated list of attachment descriptions 
                 Format looks like: 
       file[:type=<mimetype>][:charset=<charset>][:filetype=<filetype>]
                 Special Filetypes are:  BINARY, B64ENCODED if you use
                 B64ENCODED make sure to call smtpmail with RemoveEncodedFiles
                 as the EmailTo parameter after sending all your emails
                 or the encoded files will build up in your temporary
                 directory.
     LocalFiles  CHAR - comma separated list of filenames to the files 
                 described in Attachments.  The filenames in this  
                 parameter must either be in the Progress path or 
                 have the entire path specified.  In contrast to 
                 filenames in the Attachments are the filenames to 
                 show for the Attachments.
     Subject     CHAR - Subject line
     Body        CHAR - Actual text of the message can be whatever you  
                 want as long as you include the correct Header 
                 information in the MIMEHeader. 
                 If you are just sending plaintext you can leave 
                 the MIMEHeader blank, this is default setting 
                 If you wanted to send HTML you might use: 
                 type=text/html:charset=us-ascii:filetype=ascii
     MIMEHeader  CHAR - [type=<mimetype>][:charset=<chrset>][:filetype=<type>]
     BodyType    char - File/text.  Determines whether a file or text 
                 goes into the message body.

Output Param: 
     oSuccessful  LOGICAL - Yes the message was succesfully generated
                  No there was some error that prevented message from
                  being successful generated.
                  ? there may have been a problem with a recipient
                  or format of the email.
     vMessage    CHAR - Any error messages from the server

Author......: Paul C. Keary - plk@cpec.com - Coe Press Equipment Corp.
Contributors: 
                Scott Auge          - Amduus
                Mario Paranhos      - BravePoint, Inc. (770) 449-9696
                Sam Schroeder       - sschroeder@mn.rr.com 
                Geoff Crawford      - Innovative Client Servers
                Steven Lichtenberg  - Safemasters, Inc.
                Mark Bremmeyr       - I S Solutions, LTD.
                S.E. Southwell      - BravePoint, Inc. (770) 449-9696
                Steven Jellin       - ELCB Information Services
                Edgar Medrano       - Operadora de Desarrollo Social, S.A. de C.V.
                Chris Chaney        - Sumisho Computer Systems
                
Copyright...: FreeFramework 2001 - http://www.freeframework.org
License.....: Use freely - See http://www.freeframework.org/license.shtml for more detail
Created.....: 6/26/01
Notes.......: 
            This program was adapted from a combination of the work of  
            Scott Auge and distributed as part of his mailsdk           
            Scott had laid the groundwork for sending an email with     
            attachments. But his original code did not verify the
            servers response to make sure the data was being sent
            properly.  
            Mario's original work smtpmail.p was a big help in
            developing an understanding of how SMTP works.  His
            program was very elegant in the way it deals with Sockets
            however Scott's work had done much more in the area of 
            including attachments.
            
            SMTP Protocols taken from RFC821
            MIME Protocols taken from RFC1521
            SMTP AUTH taken from RFC2554

Disclaimer..: This program is not fully tested especially with  
              regard to the Reading of the Server Responses, and
              encoding   of the MIME message body . 
              
Modifications:
 Sam Schroeder - sschroeder@mn.rr.com                          
 Changed "~n" to CHR(13) + CHR(10) as RFC821 calls for         
 commands to end in <CRLF>, as carriage return and line feed.  
 NOTE: This is not fixed in the MIME section as I was not able
       to test this section.                                   
                                                               
                                                               
                                                               
 6/20/01 Geoff Crawford - for most robustness, it is           
         necessary to loop through the WRITE() on the          
         socket in case if the underlying transport            
         does the write in more than one step.  A              
         simple loop was added.                                
 6/28/01 Steven Lichtenberg - Added checks for server response 
         to ensure the conversation happens.  On initial       
         connection, there may be a significant lag before the 
         server responds. Looping through to recheck for       
         response was necessary                                
 7/3/01  Steven Lichtenberg - Modifications to more fully      
         incorporate Mario's original code andto make the      
         routine more modular.  Added robustness features to   
         ensure the complete/correct delivery of binary        
         attachments through the use of MEMPTR.  Also added    
         the ability to use either                             
         a file or passed text for the message body.  This     
         allows for the options of sending a file as an        
         attachment or as the message body.  Reworked to use   
         event driven principles for cleaner code and easier   
         maintenance.                                          
 7/9/01  Paul Keary - Replaced CHR(13) + CHR(10) with crlf     
         Decreased indentation of some of code to 2 spaces for 
         better readability on character screens.              
         Added code to set Encodedirectory based on OPSYS      
         Capitalized most of the Progress Keywords(anal,I know)
         Found a line with a single period just after the line 
         where cBoundary = "MAIL_BOUNDARY", it appears to have 
         been introduced when the ~n were orginally changed to 
         to CHR(13) + CHR(10).                                 
         Added alternative API that is activated by removing   
         preprocesser PublicVersion                            
         Found logic errors surrounding getsocket code and what
         happens if getsocket doesnot succeed.                 
 7/12/01 Found a problem with the filename used for binary file
         if the input file name is a full path.  On a Unix box 
         it tried to write a file like /tmp//u/htmldocs/...    
         Notice the 2 /'s after tmp.  Modified the code to use 
         a unique filename like en78422  instead of added a #  
         onto the end of the original local file               
         Also found attachbinlist was being delimited with a   
         '<' for binaryfiles instead of a ',' This appeared to 
         a typing error, I am surprised it wasn't noticed b4now
 7/12/01 Mario Paranhos - fixed some tiny bugs in the mime type
 8/30/01 Mark Bremmeyr - Found a problem when body is from a   
         file. Changed GetFile to process attachments and body 
         differently. Body text needed ~n before and after     
         the content of input file.                           
 9/19/01 Paul Keary - Fixed 2 reported problems 
         1. Sending a File as the Body of the message had a bug  
         where a blank line was not inserted before the body of
         the message.                                          
         2. Added a workaround for running in batch-mode based on
         Simon Prinsloo's suggestion of using a DO WHILE loop  
         Also added a secondary mechanism to leave the loop if 
         WriteData has not been called in the last 60 seconds  
         just in case the logical sending is not set to NO by  
         the Read-Handler.                                     
 9/20/01 Steve Southwell - Added the ability to have a descriptive 
         "from".  Reformatted header in FFW format.      
 10/3/01 Steve Southwell - Merged in some changes - did some cleanup
 1/11/02 Steve Southwell - Changed Error with newstate being set to 3 
         instead of 4.   Thanks to "Anonymous" for the fix.
 5/13/02 Paul Keary - Merged in special feature of setting Body to
         File:<FileName> to treat the body as a filename to insert
         in the email shortcut for setting BodyType = "File".
         Added line to reset start-Etime inside WriteData.
 5/13/02 Paul Keary - Added code to support special filetype of 
         B64ENCODED acts just as BINARY filetypes with exception of 
         how the temporary encoded files are treated.
         Purpose: Eliminate redundant encoding of files in the case
         of sending personalized emails to a group of email 
         addresses.
 5/15/02 Paul Keary - Added Jeff Pilant's code to determine TimeZone 
         from the Windows Registry.  Also removed tab characters
         from source code and replaced with spaces. 
 5/23/02 Paul Keary - Fixed bug in doFiles, if the encoded file already
         existed the attachbinlist variable was not be set correctly,
         moved assignment statement outside of do block where
         base64encode is called.
         Added code to not add / to EncodeDirectory if last char is \.
         on Win32 - temp files became C:\TEMP\/filename instead of 
         C:\TEMP\filename.
 8/22/02 Steve Southwell - Added RAW to length() when used to set 
         memptr sizes - Thanks to Rares Stanciulescu     
         Changed "HELO How are you?" to "HELO " followed by the local
         hostname.  Thanks to Bill Prew, David Craven, Scott Ziola, 
         and Hugh Cruickshank   
 9/19/02 Peter Kalmijn - Bug fix for first binary attachment corruption 
         posted to api@peg.com.  Change committed by SES.
 9/24/02 Paul Keary - Added optional capability to set boundary 
         parameter in the MIMEHeader variable.  Used to correct a bug
         Jeff Pilant found when trying to send an html formatted 
         email.  By using boundary=[Boundary] in conjunction with
         type=multipart/mixed (conditional on type value beginning 
         multipart)
12/18/02 Steven L. Jellin - Sorted out problem with multiple recipients and
         CC list.  Instead of just looping through the TO and CC variables, 
         each entry is sent to newstate and removed from the "," separated
         list. State has been left as 3 until ALL TO and CC entries are          
         completed.
01/02/03 Steve Southwell 
         - Various tweaks to handle multiline SMTP responses and incomplete packets.  
         - Added additional preprocessors for logging levels
            to avoid having to type "if loglevel < X" everywhere.  From now on, just
            use Logger1, logger2, or logger3.  Added sanity check on To and CC lists
            to make sure they get trimmed first.  
         - Added ability to specify port number as part of the mailhost
         - Added BCC functionality
         - Added Reply-To functionality (Thanks to Edgar Medrano)
         - Added enhanced error reporting and logging - use throwError(errorMessage) 
---------------------------------------------------------------*/


/********** forward declare functions ***********/

FUNCTION newstate RETURNS INTEGER
  (INPUT newstate AS INTEGER,
   INPUT pstring AS CHARACTER,
   INPUT hSocket AS HANDLE) FORWARD.
FUNCTION getfile RETURNS MEMPTR(INPUT filnm AS CHAR) FORWARD.
FUNCTION throwError RETURNS LOGICAL(INPUT myErrorText AS CHAR) FORWARD.

/******************** Variable definitions **************/

/* 7/9/01 PK - to support 2 API's for calling this procedure */
/* Uncomment the following line to use the defactor API as set on the
   FreeFrameWork, alternate version uses Mailhub defined within this 
   program and does not use BodyType or vMessage */
&SCOPED-DEFINE PublicVersion 1
&IF DEFINED(PublicVersion) > 0
&THEN
DEF INPUT PARAMETER mailhub         as char no-undo.
DEF INPUT PARAMETER EmailTo         AS CHAR NO-UNDO.
DEF INPUT PARAMETER EmailFrom       AS CHAR NO-UNDO.
DEF INPUT PARAMETER EmailCC         AS CHAR NO-UNDO.
DEF INPUT PARAMETER Attachments     AS CHAR NO-UNDO.
DEF INPUT PARAMETER LocalFiles      AS CHAR NO-UNDO.
DEF INPUT PARAMETER Subject         AS CHAR NO-UNDO.
DEF INPUT PARAMETER Body            AS CHAR NO-UNDO.
DEF INPUT PARAMETER MIMEHeader      AS CHAR NO-UNDO.
DEF INPUT PARAMETER BodyType        as char no-undo.

DEF OUTPUT PARAMETER oSuccessful    AS LOGICAL NO-UNDO.
DEF OUTPUT PARAMETER vMessage       AS CHAR NO-UNDO.

/*****************************************************************
   Added by Paul Keary on 7/9/01 
   This code can be used to simplify calls to smtpmail.p 
   Note the version that appeared on FreeFramework.org had EmailTo as
   the first Parameter, This version has EmailFrom as First Parameter
   to coincide with the default format in Outlook, either way is fine 
   this is more to be capatible with older code 
 ****************************************************************/
&ELSE
DEF INPUT PARAMETER EmailFrom           AS CHAR NO-UNDO.
DEF INPUT PARAMETER EmailTo           AS CHAR NO-UNDO.
DEF INPUT PARAMETER EmailCC           AS CHAR NO-UNDO.
DEF INPUT PARAMETER Attachments       AS CHAR NO-UNDO.
DEF INPUT PARAMETER LocalFiles        AS CHAR NO-UNDO.
DEF INPUT PARAMETER Subject           AS CHAR NO-UNDO.
DEF INPUT PARAMETER Body                AS CHAR NO-UNDO.
DEF INPUT PARAMETER MIMEHeader        AS CHAR NO-UNDO.

DEF OUTPUT PARAMETER oSuccessful      AS LOGICAL NO-UNDO.
DEF VAR BodyType                    AS CHAR INITIAL "TEXT" NO-UNDO.
DEF VAR Mailhub                     AS CHAR INITIAL "mail.foo.com" NO-UNDO.
DEF VAR vMessage                    AS CHAR NO-UNDO.
/* Added to provide BodyType parameter functionality without 
   changing the Signature of the Procedure call. 5/13/02 PK */
IF Body BEGINS "File:" THEN DO:
  BodyType = "File".
  Body = SUBSTRING(Body,6).
END.
&ENDIF
/* End of Alternate API call definition */


/* Configure These Parameters per your specific needs */
DEF VAR loglevel                    AS INTEGER NO-UNDO.
DEF VAR LogFile                     AS CHARACTER NO-UNDO.
DEF VAR EncodeDirectory             AS CHAR NO-UNDO.
DEF VAR timezone                    AS CHAR NO-UNDO.

DEF VAR cLocalFile                  AS CHARACTER NO-UNDO.
DEF VAR cBinaryFile                 AS CHARACTER NO-UNDO.
DEF VAR vLocalHostName              AS CHARACTER NO-UNDO.

/* Used to communicate with SMTP Socket */
DEF VAR hSocket                     AS HANDLE NO-UNDO.

DEF VAR ServResponse                AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR ServerCode                  AS INTEGER NO-UNDO.
DEF VAR vState                      AS INTEGER  NO-UNDO.
DEF VAR crlf                        AS CHAR NO-UNDO.


/* SWL 07/03/2001 counter for looping */
DEF VAR icnt                        AS INTEGER   NO-UNDO.
DEF VAR icnt1                       AS INTEGER   NO-UNDO.
DEF VAR filcnt                      AS INTEGER   NO-UNDO.
DEF VAR rcptcnt                     AS INTEGER   NO-UNDO.
DEF VAR rspcnt                      AS INTEGER   NO-UNDO.
DEF VAR AttachBinlist               AS CHARACTER NO-UNDO.
DEF VAR sending                     AS LOGICAL NO-UNDO.
DEF VAR start-etime                 AS INTEGER NO-UNDO.

/* SLJ 18/12/02 - Need a temp list of recipients to work through */
DEFINE VARIABLE EmailToTmp          AS CHARACTER NO-UNDO.
DEFINE VARIABLE EmailCCTmp          AS CHARACTER NO-UNDO.

/* SES 01/03/2003 */
DEFINE VARIABLE EmailReplyTo        AS CHARACTER NO-UNDO.

/****************************************************************/
/* Only Log attachments if debug level = 1                      */
/*   modify log locations as required                           */
/****************************************************************/
/* Closest we come to a Constant Variable in Progress.
   DO NOT change the value of crlf */
crlf = CHR(13) + CHR(10).

loglevel = 1.  /* Minimal logging = 3 Verbose loggin = 1 */

/* Get temp directory from environment, if found - Edgar Medrano & SES */
IF OS-GETENV("TMPDIR") <> ? AND TRIM(OS-GETENV("TMPDIR")) <> "" THEN DO:
  EncodeDirectory = TRIM(OS-GETENV("TMPDIR")). 
  /* Make sure EncodeDirectory ends in a DirSeparator  */
  IF SUBSTRING(EncodeDirectory,LENGTH(EncodeDirectory),1) <> "/" AND
   SUBSTRING(EncodeDirectory,LENGTH(EncodeDirectory),1) <> "~\" THEN
    EncodeDirectory = EncodeDirectory + "/".
    
  LogFile = EncodeDirectory + "socketemail.log".
END. /* found in environment */
ELSE DO: /* decide on a temp directory */
    /* 7/9/01 PK - Eliminate need to configure for most users */
    &IF OPSYS = "UNIX" &THEN
    EncodeDirectory = "/tmp/".
    LogFile = "/tmp/socketemail.log".
    &ELSE
    EncodeDirectory = SESSION:TEMP-DIRECTORY.
    LogFile = "socketemail.log". /* Write to default directory */
    &ENDIF
    
    /* Make sure EncodeDirectory ends in a /  */
    IF SUBSTRING(EncodeDirectory,LENGTH(EncodeDirectory),1) <> "/" AND
       SUBSTRING(EncodeDirectory,LENGTH(EncodeDirectory),1) <> "~\" THEN
      EncodeDirectory = EncodeDirectory + "/".
END. /* decide on a temp directory */



/* Determine which timezone we are in so that the Mail will have the
   correct SENT Time.
   (Moved original code into a Procedure call 5/15/2002) Paul Keary
   Jeff Pilant submitted a solution to determine TimeZone on Windows which
   was incorporated into this program. */
RUN get-tz(OUTPUT timezone).

DEFINE STREAM sLogfile.
&GLOBAL-DEFINE Stream STREAM sLogFile
&GLOBAL-DEFINE LOGGER PUT {&Stream} UNFORMATTED TODAY " " STRING (TIME, "hh:mm:ss") " "
&GLOBAL-DEFINE LOGGER1 IF logLevel <= 1 THEN {&LOGGER}
&GLOBAL-DEFINE LOGGER2 IF logLevel <= 2 THEN {&LOGGER}
&GLOBAL-DEFINE LOGGER3 IF logLevel <= 3 THEN {&LOGGER}

/* Trim incoming parameters to avoid issues with num-entries */
ASSIGN
 EmailFrom = trim(emailFrom," ,")
 EmailTo = trim(emailTo," ,")
.

/* No Point in doing anything if EmailFrom and EmailTo Not Known */
IF EmailFrom = "" or
   EmailTo = "" THEN DO:
  vmessage = "From or To is blank".
  RETURN.
END. /* if emailfrom = "" or emailto = "" */

/* Get the Reply-to if included (SES - 1/3/03) */
IF NUM-ENTRIES(EmailFrom,"^") > 1 THEN DO:
    ASSIGN 
     EmailReplyTo = entry(2,EmailFrom,"^")
     EmailFrom    = entry(1,EmailFrom,"^")
    .
    
END.

OUTPUT {&Stream} TO VALUE(LogFile) UNBUFFERED APPEND.


RUN ffw/procs/gethostname.p(OUTPUT vLocalHostName). /* get the local hostname from the OS */

IF vLocalHostName = "" THEN ASSIGN vLocalHostName = "localhost". /* better than nothing */
ASSIGN vLocalHostName = REPLACE(vLocalHostName," ",""). /* get rid of all spaces */

{&Logger2} SKIP "************** ****** New Group ******* ************"
          SKIP
          "Socket email started" SKIP
          "Input Parameters" SKIP
          "EmailFrom = " EmailFrom SKIP
          "EmailReplyTo = " EmailReplyTo SKIP
          "EmailTo = " EmailTo SKIP
          "EmailCC = " EmailCC SKIP
          "Attachments = " Attachments SKIP
          "LocalFiles = " LocalFiles SKIP
          "Subject = " Subject SKIP
          "Body = " Body SKIP
          "MIMEHeader = " MIMEHeader SKIP
          "LocalHostName = " vLocalHostName SKIP.
ELSE {&Logger} "Send Mail From " EmailFrom " to " EmailTo SKIP.

/* 5/13/02 PK Added to support reuse of base64encoded files over
   multiple calls to this procedure.  After the last call run this
   program again with "RemoveEncodedFiles" as the EmailTo and the
   temporary encoded files will be removed. */
IF EmailTo = "RemoveEncodedFiles" THEN DO:
  IF localfiles <> "" THEN DO filcnt = 1 TO NUM-ENTRIES(localfiles):
    RUN DoneWithFiles (INPUT ENTRY(filcnt,localfiles),
                       INPUT ENTRY(filcnt,attachments)) NO-ERROR.
    oSuccessful = YES.  /* Special Call - Don't send error back */
  END.
  RUN Cleanup.
  RETURN.
END.

/* process attachments and generate a comma separated list of
   file names for the output of base64encode.p . This is done prior
   to opening the socket to minimize the impact on resources
   (you do not need to have a socket open doing nothing for hours */
IF localfiles <> "" THEN DO filcnt =  1 TO NUM-ENTRIES(localfiles):
  {&logger2} "processing Attachment " ENTRY(filcnt,localfiles) skip.

  RUN dofiles(INPUT ENTRY(filcnt,localfiles),
              INPUT ENTRY(filcnt,attachments),
              INPUT-OUTPUT attachbinlist) NO-ERROR.

  IF vMessage <> "" THEN DO:
    RUN cleanup.
    RETURN.
  END. /* if return value <> "" then */
END. /* do icnt = 1 to num-entries(attachments) */


/****************************************************************/
/* Create the socket, log in the log file that it was succes-   */
/* ful or not.                                                  */
/****************************************************************/
sending = YES.
{&logger2} "opening socket" skip.
RUN getsocket(input loglevel,input mailhub,output hSocket).
IF vMessage <> "" THEN RETURN.

IF NOT THIS-PROCEDURE:PERSISTENT AND 
   NOT SESSION:BATCH-MODE THEN WAIT-FOR CLOSE OF THIS-PROCEDURE.
ELSE IF SESSION:BATCH-MODE THEN DO:
  /*********************************************************
    9/19/2001 by Paul Keary
    Progress will not wait-for input on a Socket while running in
    batch mode.  Simon Prinsloo [simonp@qbcon.com] proposed this
    simple code to workaround this problem. 
    ******************************************************/
  start-etime = ETIME.
  DO WHILE sending:
    PROCESS EVENTS.
    PAUSE 1.
    /* Build in timer in case sending is never set to NO 
       this will terminate the program after 60 seconds
       start-Etime will be reset by WriteData each time there
       is activity on the socket to allow for long transmissions */
    IF start-etime + 60000 < ETIME THEN DO:
      sending = NO.
      RUN Cleanup.
    END.
  END.
END.

/****************************************************************/
/*  used a readhandler to avoid timing ut issues with running   */
/*  in a non-event driven mode.  Also more fully complies with  */
/*  Mario's original program design.                            */
/****************************************************************/
PROCEDURE readhandler:
  DEF VAR vlength     AS INTEGER   NO-UNDO.
  DEF VAR str         AS CHARACTER NO-UNDO.
  DEF VAR v                              AS INTEGER   NO-UNDO.
  DEF VAR idx                            AS INTEGER   NO-UNDO.
  DEF VAR mData                          AS MEMPTR    NO-UNDO.
  DEF VAR vbuffer                        AS MEMPTR    NO-UNDO.
 
 /* Used to Build MIME Message Body */
  DEF VAR cTempValue                      AS CHARACTER NO-UNDO.
  DEF VAR cBoundary                       AS CHARACTER NO-UNDO.
  DEF VAR cMimeType                       AS CHARACTER NO-UNDO.
  DEF VAR cCharSet                        AS CHARACTER NO-UNDO.
  DEF VAR cFileType                       AS CHARACTER NO-UNDO.
  DEF VAR cFile                           AS CHARACTER NO-UNDO.
  DEF VAR cImportData                     AS CHARACTER NO-UNDO.
 
  DEF VAR smtpcmd                   AS CHAR FORMAT "x(90)" NO-UNDO.
  DEF VAR teststr                   AS CHARACTER NO-UNDO.

  DEF VAR cMonth                          AS CHARACTER NO-UNDO
         INIT "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".
  DEF VAR iLineCount                      AS INTEGER   NO-UNDO.
  DEF VAR iRespLines                      AS INTEGER   NO-UNDO.
  DEF VAR cExtraResp                      AS CHARACTER NO-UNDO.
  DEF VAR cThisLine                       AS CHARACTER NO-UNDO.
  DEF VAR cTextIn                         AS CHARACTER NO-UNDO.
  
  IF NOT VALID-HANDLE(hSocket) OR NOT hSocket:CONNECTED() THEN RETURN.

  vlength = hSocket:GET-BYTES-AVAILABLE().
  IF vlength > 0 THEN DO:
    SET-SIZE(vbuffer) = 0. /* re-initialize - See SF bug #467737*/
    SET-SIZE(vbuffer) = vlength + 1.
    hSocket:READ(vbuffer, 1, vlength, 1).
    ASSIGN
     str = cExtraResp + GET-STRING(vbuffer,1)
     iRespLines = num-entries(str,"~n")
     cExtraResp = "" 
    .
    {&logger1} "Number of lines: " iRespLines skip.
    {&logger1} "String is: " str skip.
    
    /* If response doesn't end with a newline, then we don't have the whole thing.    */
    /*  - In that case, stash the extra part of a line into a temporary variable that */
    /*    we can add to the beginning of the next packet we receive.  -SES            */
    
    IF SUBSTRING(str,length(str,"CHARACTER") - 1,2) NE crlf THEN DO:
        ASSIGN cExtraResp = ENTRY(iRespLines,str,"~n").
        ENTRY(iRespLines,str,"~n") = "".
        ASSIGN str = TRIM(str) + CRLF.
    END.
    
    /* If we have more than one line of response, see if it's a multiline response    */
    /* and ignore all but the last line - Thanks to Rajah, Askins, and Chaney - SES   */
    
    ASSIGN v = 0.   
    RESPLINECOUNT: DO iLineCount = 1 to iRespLines - 1: /* should be nothing after final CRLF */
        {&logger1} "Parsing response line " iLineCount skip.
        ASSIGN cThisLine = TRIM(ENTRY(iLineCount,str,"~n")).
        {&logger1} "This line is: " cThisLine skip.
        IF SUBSTRING(cThisLine,4,1,"CHARACTER") = "-" THEN NEXT RESPLINECOUNT.
        ELSE ASSIGN v = INTEGER(ENTRY(1,str," ")) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ThrowError(str). 
            LEAVE RESPLINECOUNT.
        END. /* not a valid response */
    END. /* Each line of response */  
    IF v = 0 OR v = ? THEN LEAVE.
    
    {&logger2} "vstate " vstate skip "str " str skip.
    SET-SIZE(vbuffer) = 0.

    CASE vState:
      
        /********************** 1 - Build message ***************/
      WHEN 1 THEN DO:
        {&logger2} vstate " " v " "  "HELO" skip.
        IF v = 220 THEN  /* send helo */
          vstate = newState(2, "HELO " + vLocalHostName + crlf,hsocket).
        ELSE throwError(str).
      END. /* when 1 */
      
      
        /********************** 2 - Build From *****************/
      WHEN 2 THEN DO:
        {&logger2} vstate " " v " "  "Mail From" skip.
        IF v = 250 THEN do:
          ASSIGN
          EmailTo    = REPLACE(EmailTo,";",",")
          EmailCC    = REPLACE(EmailCC,";",",")
          EmailToTmp = EmailTo  
          EmailCCTmp = EmailCC.
          vstate =  newState(3, "MAIL From: " + ENTRY(1,EmailFrom,";") +
                             crlf,hsocket).
          /* SLJ 18/12/02 - Count once */
          rcptcnt = num-entries(emailto) + num-entries(emailcc).
          {&logger2} "rcptcnt is: " rcptcnt skip.                        
        END. /* if v = 250 */
        ELSE throwError(str).
      END. /* when 2 */
      
        /******************** 3 - Assign to and cc **************/
      WHEN 3 THEN DO:
        ASSIGN
        icnt    = if icnt = 0 then 1 else
                  (if icnt = ? then 1 else icnt)
        icnt1   = if icnt1 = 0 then 1 else
                  (if icnt1 = ? then 1 else icnt1)
        smtpcmd = "".
        /************************************************************
         *** in case we get multiple responses back in the same packet,
         *** we need to parse the return string to determine how many
         *** responses we get back
         ***********************************************************/

        {&logger2} vstate " " v " "  "Mail to/CC" skip 
                   "rcptcnt " rcptcnt skip
                   "rspcnt " rspcnt " emailto " NUM-ENTRIES(emailto) 
                   " cc " NUM-ENTRIES(emailcc) skip.
        IF v = 250 THEN do:       /* loop through all to's and cc's  */
        
            IF NUM-ENTRIES(EmailToTmp) > 1 THEN DO:
                {&logger2} icnt " email to " ENTRY(1,emailtoTmp) skip.
                vstate = newState(3, "RCPT TO: " + ENTRY(1,EmailToTmp) +                              crlf,hsocket).
                ASSIGN emailtoTmp = SUBSTRING(emailto,INDEX(emailtoTmp,",") + 1).
            END. /* More than One TO */
            
            ELSE IF NUM-ENTRIES(EmailtoTmp) = 1 THEN DO:
                {&logger2} icnt " email to " emailtotmp skip.
                vstate = IF EmailCCTmp = "" THEN /* If cc stay as state 3 */
                            newState(4, "RCPT TO: " + EmailToTmp + crlf,hsocket)
                         ELSE
                            newState(3, "RCPT TO: " + EmailToTmp + crlf,hsocket).
                ASSIGN emailtoTmp = "". /* now its none */
            END. /* Last Or only one Entry */
            
            ELSE IF EmailCCTmp <> "" THEN DO:
                IF NUM-ENTRIES(EmailCCTmp) > 1 THEN DO:
                    {&logger2} icnt1 " email to " ENTRY(1,EmailCCTmp) skip.
                    vstate = newState(3, "RCPT To: " + ENTRY(1,ENTRY(1,EmailCCTmp),"^") +
                                    crlf,hsocket).
                    ASSIGN emailCCTmp = SUBSTRING(EmailCCTmp,INDEX(EmailCCTmp,",") + 1).
                
                END. /* Multiple CC's */
                ELSE DO:
                    {&logger2} icnt1 " email to " EmailCCTmp skip.
                    vstate = newState(4, "RCPT To: " + ENTRY(1,EmailCCTmp,"^") + crlf,hsocket).
                    ASSIGN EmailCCTmp = "".
                END. /* One or Last CC */
            END. /* CC'ing people */
    
        END. /*  IF v = 250 THEN */
        ELSE throwError(str).
/* SES removed 1/3/03 
        teststr = str.
        IF INDEX(teststr,"Recipient") <> 0 THEN
           rspcnt = rspcnt + 1.
        {&logger2} "end of 3: " str skip  "rspcnt " rspcnt 
                    " rcptcnt " rcptcnt skip.
        if rspcnt = rcptcnt then
          vstate = newstate(5,"DATA " + crlf, hsocket).
*/
      end. /* when 3 */
      
      /******************** 4 - Build header ********************/
      WHEN 4 THEN DO:
        {&logger2} vstate " " v " "  "Data" skip.
        IF v = 250 THEN
          vstate = newState(5, "DATA " + crlf,hsocket).
        ELSE throwError(str).
      END. /* when 4 */
      
      
      WHEN 5 THEN DO:
        {&logger2} vstate " " v " "
                               "build header/send data" skip.
        IF v = 354 THEN do:
          /* Build Email Header */
          smtpcmd = "From: " + 
             (IF NUM-ENTRIES(EmailFrom,";") > 1 THEN
             ENTRY(2,emailfrom,";") + " <" + ENTRY(1,emailfrom,";") + ">"
             ELSE EmailFrom) + crlf.
             
          /**************************************/
          /* Look for Reply-To                  */
          /**************************************/
          IF EmailReplyTo <> "" THEN DO:
            smtpcmd = smtpcmd + "Reply-To: " + EmailReplyTo + "~n".
          END.  /* IF ReplyTo <> "" THEN DO */
          
          /**************************************/
          /* Loop through all To's              */
          /**************************************/
          IF EmailTo <> "" THEN DO idx = 1 TO NUM-ENTRIES(EmailTo):
            smtpcmd = smtpcmd + "To: " + ENTRY(idx,EmailTo) + "~n".
          END.  /* IF EmailTo <> "" THEN DO idx = 1 */

          /*****************************/
          /* Loop through all CC's     */
          /*****************************/
          IF EmailCC <> "" THEN DO idx = 1 TO NUM-ENTRIES(EmailCC):
              IF NUM-ENTRIES(ENTRY(idx,EmailCC),"^") = 1 
               OR ENTRY(2,ENTRY(idx,EmailCC),"^") NE "B" 
               THEN ASSIGN smtpcmd = smtpcmd + "Cc: " + ENTRY(1,ENTRY(idx,EmailCC),"^") + "~n".
          END.  /* IF EmailCC <> "" THEN  */
               
          ASSIGN
          smtpcmd = smtpcmd + "Subject: " + Subject + "~n" 
          /* Sample format    Date: 27 March 2001 10:30:00 EST */
          smtpcmd = smtpcmd + "Date: " + STRING(DAY(TODAY)) + " " +
                    entry(MONTH(TODAY),cMonth) + " " +
                    STRING(YEAR(TODAY),"9999") + " " +
                    STRING(TIME,"hh:mm:ss") + " " + timezone + "~n".

          SET-SIZE(mData) = LENGTH(smtpcmd,"RAW") + 1.
          PUT-STRING(mData,1) = smtpcmd.
          RUN WriteData(input mData, input hsocket).
          SET-SIZE(mData) = 0.

          /********************************************************/
          /* Begin sending message                                */
          /********************************************************/
          /* Done building Email Hdr, Now do body of the message */
          /** Set up a boundary value if we have file attachments **/
          /** Create multi mime header for email **/
          IF Attachments <> "" THEN DO:
            ASSIGN
            cBoundary = "MAIL_BOUNDARY"
            smtpcmd = "MIME-Version: 1.0" + "~n" +
                      'Content-type: multipart/mixed;~n' +
                      '        boundary="' + cBoundary + '"~n' + 
                      "Content-Transfer-Encoding: 7bit~n".
            smtpcmd = smtpcmd + "This is a multi-part MIME Encoded message" +
            /* *Kalmijn*: put ~r~n before boundry marker instead of ~n~n. */
                      "~r~n--" + cBoundary + "~n".
            SET-SIZE(mData) = LENGTH(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(input mData, input hsocket).
            SET-SIZE(mData) = 0.
          END. /* IF Attachments <> "" THEN DO: */

          /** Do we have a MIME Type for this messsage **/
          IF MIMEHeader <> "" THEN DO:
            RUN ParseParm(INPUT MIMEHeader,
                          OUTPUT cMimetype,
                          OUTPUT cCharset,
                          OUTPUT cfiletype).
            smtpcmd = IF Attachments = "" THEN "Mime-Version: 1.0~n"
                                           ELSE "".
            smtpcmd = smtpcmd + "Content-Type: " + cMimeType.
            /* IF the message was multipart the charset may have
               been overridden */
            IF cCharSet <> "" THEN 
              smtpcmd = smtpcmd + "; charset=" + cCharSet.
            smtpcmd = smtpcmd + "~n" +
                      "Content-Transfer-Encoding: 7bit~n".
            SET-SIZE(mData) = LENGTH(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(INPUT mData, INPUT hsocket).
            SET-SIZE(mData) = 0.
          END.  /* IF MIMEHeader <> "" THEN DO: */

          
          /*********************************************************/
          /* Output the Message                                    */
          /*********************************************************/
          smtpcmd = "~n".
          IF bodytype = "file" THEN DO:
          /************************************************
            9/19/01 PK
            Write the blank line to the Socket to indicate the 
            start of the Body of the message. Bug was discovered
            by David De Beule [david@mips.be] on 8/28/2001 
            10/3/01 SES - Also was addressed by Mike Bremmeyr, but
            this was found to be a better fix.
          *****************************************************/
            SET-SIZE(mdata) = LENGTH(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(INPUT mData, INPUT hSocket).
            
            SET-SIZE(mData) = 0.
            {&logger1} "before getfile " GET-SIZE(mdata) skip.
            mData = getfile(body).
            {&logger1} "after getfile " GET-SIZE(mdata) skip.
            RUN WriteData(INPUT mData, INPUT hsocket).
            
            /* ses - 1/3/03 - nice try, but doesn't work yet */
            INPUT FROM VALUE(body).
            REPEAT:
                IMPORT UNFORMATTED cTextIn.
                SET-SIZE(mData) = 0.
                SET-SIZE(mData) = length(cTextin,"CHARACTER") + 2.
                /* escape out single periods, since they are end-of-message indicator */
                IF cTextin = "." then assign cTextin = "..".  
                PUT-STRING(mData,1) =  cTextin + "~n".
                RUN WriteData(INPUT mData, INPUT hsocket).
            END. /* repeat */
            */
            
            SET-SIZE(mData) = 0.
            SET-SIZE(mData) = 2.
            PUT-STRING(mData,1) = "~n".

          END. /* if bodytype = "file" */
          ELSE DO:
            smtpcmd = smtpcmd + Body + "~n".
            SET-SIZE(mData) = length(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
          END.  /*  else */
          
          RUN WriteData(INPUT mData, INPUT hsocket).
          SET-SIZE(mData) = 0.
          
          
          /*******************************************************/
          /* Process any files attachments.                      */
          /*******************************************************/
          /* LocalFiles holds comma separated list of files that are 
           in the Progress path or contain an fullpath.
             Attachments holds colon separated list of parameters of
             to use in sending file. The 1st parameter is the name of 
             file to use in generating a temporary file, the remaining
             parameters are all optional: 
             Type=text/plain   Charset=US-ASCII   FileType=ASCII
          */
          DO idx = 1 TO NUM-ENTRIES(LocalFiles):
            ASSIGN
            cFile = ENTRY(1,ENTRY(idx,Attachments),":")
            cLocalFile = ENTRY(idx,LocalFiles).
            
            /** set up the mime header **/
            /* Content-Type: <mimetype>; charset=<charset> */
            RUN parseParm(input entry(idx,attachments),
                          output cMimetype,
                          output cCharset,
                          output cfiletype).

            smtpcmd = "~n--" + cBoundary + "~n" +
                      "Content-type: " + cMimeType + "; ".
            IF LOOKUP(cFileType,"BINARY,B64ENCODED") = 0 THEN
              smtpcmd = smtpcmd + "charset=" + cCharSet.
            smtpcmd = smtpcmd + '~n        name="' + cFile + '"~n'.


            IF LOOKUP(cFileType,"BINARY,B64ENCODED") > 0 THEN
              smtpcmd = smtpcmd + 'Content-Transfer-Encoding: base64~n'.

            smtpcmd = smtpcmd + 'Content-Disposition: attachment;~n' +
                      '        filename="' + cFile + '"~n~n'.
            SET-SIZE(mData) = LENGTH(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(INPUT mData, INPUT hsocket).
            SET-SIZE(mData) = 0.

            /** now do the file **/
            mData = getfile(ENTRY(idx,attachbinlist)).
            RUN WriteData(INPUT mData, INPUT hsocket).
            SET-SIZE(mData) = 0.

            smtpcmd = "~n".
            SET-SIZE(mData) = LENGTH(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(INPUT mData, INPUT hsocket).
            SET-SIZE(mData) = 0.

            /** if we have a "Binary" file then try to delete 
                the encoded version **/
            IF cFileType = "Binary" THEN
              OS-DELETE VALUE(entry(idx,attachbinlist)) NO-ERROR.
          END. /** process each attachment  do idx - 1 to num-entries **/

          IF Attachments <> "" THEN DO:
            smtpcmd = '~n--' + cBoundary + '--~n~n'.
            SET-SIZE(mData) = length(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(input mData, input hsocket).
            SET-SIZE(mData) = 0.
          END. /* IF Attachments <> "" THEN DO: */
          vstate = newstate(6, crlf + "." + crlf, hsocket).
        END. /* if v = 354 */
        ELSE vState = -1.
      END. /* when 5 */
      WHEN 6 THEN DO:
        {&logger2} vstate " " v " "  "send quit" skip.
        IF v = 250 THEN
          vstate = newState(7,"QUIT" + crlf,hsocket).
        ELSE vState = -1.
      END. /* when 6 */
    END CASE. /* vstate */
  END. /* IF vlength > 0 THEN DO: */
  {&logger2} "Finish up " vstate skip.
  IF vState = 7 THEN vMESSAGE = "Email has been accepted for delivery.".
  IF vstate < 0 OR vstate = 7 THEN DO:
    RUN cleanup.
    if vstate = 7 then oSuccessful = YES.
    /* If running in batch mode then tell the WHILE loop to exit */
    sending = NO.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
  END.  /* IF vstate < 0 OR vstate = 7 THEN DO: */
END PROCEDURE.   /* readhandler */

PROCEDURE Cleanup.
  {&Logger} "End SMTP Session" SKIP.
  OUTPUT {&Stream} CLOSE. 

  IF VALID-HANDLE(hSocket) THEN DO:
    IF hSocket:CONNECTED() THEN hSocket:DISCONNECT() NO-ERROR.
    DELETE OBJECT hSocket.
  END.
END PROCEDURE. /* cleanup */

PROCEDURE WriteData:
  DEF INPUT PARAMETER mdata       AS memptr NO-UNDO.
  DEF INPUT PARAMETER hsocket     AS handle NO-UNDO.
  DEF VAR DataBytesWritten        AS INTEGER NO-UNDO.
  DEF VAR WriteSuccess            AS LOGICAL NO-UNDO.
  DEF VAR MessageSize             AS INTEGER NO-UNDO.
  DEF VAR mystring                AS CHARACTER NO-UNDO.

  start-Etime = ETIME. /* Reset timer because we had activity */
  ASSIGN
  MessageSize = GET-SIZE(mdata)
  DataBytesWritten = 0.
  IF messagesize = 0 THEN RETURN.

  /* 7/10/01 PK - Chomp off the Null at the end of the memptr data */
  IF GET-BYTE(mData,messagesize) = 0 OR
     GET-BYTE(mData,messagesize) = 255 THEN DO:  /* ||| Is this I18N compatible? - SES */
    {&logger2} "writedata chomp data" GET-BYTE(mData,messagesize) skip.
    messagesize = messagesize - 1.
  END.

  /* 6/20/01 GC - Loop continuously until the number of bytes
                  written is greater or equal to the message size */
  {&logger1} "writedata = before " DataBytesWritten " " 
              MessageSize " " hSocket:BYTES-WRITTEN skip
              /* GET-STRING(mdata,1,messagesize) SKIP */ . 
  DO WHILE DataBytesWritten < MessageSize:
    {&logger1} "writedata = in " DataBytesWritten " " 
                MessageSize " " hSocket:BYTES-WRITTEN skip.
    WriteSuccess = hSocket:WRITE(mdata, DataBytesWritten + 1,
                                 MessageSize - DataBytesWritten).
    /*IF NOT WriteSuccess THEN LEAVE. */
    IF WriteSuccess THEN 
      DataBytesWritten = DataBytesWritten + hSocket:BYTES-WRITTEN.
  END. /* DO WHILE */
  {&logger1} "writedata = after " DataBytesWritten " " MessageSize " "
              hSocket:BYTES-WRITTEN skip.
  SET-SIZE(mData) = 0.
END procedure. /* writeData */

/*******************************************************************/
/** Parse mime type and char set out of header                    **/
/** If nothing there, return the default                          **/
/*******************************************************************/
PROCEDURE ParseParm:
  DEF INPUT PARAMETER cString         AS CHARACTER NO-UNDO.
  DEF OUTPUT parameter cMimetype      as character no-undo.
  DEF OUTPUT parameter cCharset       as character no-undo.
  DEF OUTPUT parameter cFiletype      as character no-undo.

  DEF VAR c               AS CHARACTER NO-UNDO.
  DEF VAR i               AS INTEGER  NO-UNDO.
  DEF VAR lBoundary       AS CHARACTER NO-UNDO.

  ASSIGN
  cMimeType = "text/plain"
  cCharSet  = "US-ASCII"
  cFileType = "ASCII"
  lBoundary = "".

  DO i = 1 TO NUM-ENTRIES(cString,":"):
    c = ENTRY(i,cString,":").
    CASE ENTRY(1,c,"="):
      WHEN "Type" THEN DO:
        cMimeType = ENTRY(2,c,"=").
      END.  /*  WHEN "Type" THEN DO: */
      WHEN "CharSet" THEN DO:
        cCharSet = ENTRY(2,c,"=").
      END.  /*   WHEN "CharSet" THEN DO: */
      WHEN "FileType" THEN DO:
        cFileType = ENTRY(2,c,"=").
      END.  /* WHEN "FileType" THEN DO: */
      
      /* Special patch used to accept a Boundary parameter for
         multipart messages */
      WHEN "Boundary" THEN DO:
        lBoundary = ENTRY(2,c,"=").
      END.
    END CASE. /*  CASE ENTRY(1,c,"="): */
  END.  /* DO i = 1 TO NUM-ENTRIES(cString,":"): */
  
  IF lBoundary <> "" THEN DO:
    IF cMimeType BEGINS "multipart" THEN DO:
      cMimeType = cMimeType + ";~nboundary=" + lBoundary.
      cCharset = "".
    END.
  END.
END PROCEDURE. /** ParseParm **/

/*****************************************************************/
/*  Generate base 64 encoded binary files                       **/
/*****************************************************************/
PROCEDURE dofiles:
  DEF INPUT parameter localfile            as char no-undo.
  DEF INPUT parameter cattachment           as char no-undo.
  DEF INPUT-OUTPUT parameter attachbinlist as char no-undo.

  DEF VAR cLocalFile                       as char no-undo.
  DEF VAR Mimetype                     as character no-undo.
  DEF VAR ccharset                      as character no-undo.
  DEF VAR cFileType                       AS CHARACTER NO-UNDO.

  FILE-INFO:FILE-NAME = localfile.
  /***** file-info returns "F" + "RW" if the file is read/writable etc)
         check to be sure it is a valid file    ************/
  IF INDEX(FILE-INFO:FILE-TYPE,"F") = 0 THEN DO:
    vMessage = localfile + " Not a valid file".
    RETURN.
  END.

  RUN ParseParm(INPUT cAttachment,
                OUTPUT mimetype,
                OUTPUT cCHARSET,
                OUTPUT cfiletype).

  IF cFileType = "Binary" THEN DO:
    /* 7/12/02 PK if localfile includes a file path it will cause
       some problems trying to write the file in /tmp           
       Generate a unique file to use for the encoded version */

    /* cBinaryFile is used in this loop to check for existence of
       the new encoded file.  It must be reset to the desired value
       after the loop. */
    cBinaryFile = localfile.
    DO while cBinaryFile <> ?:
      /* Use of ETIME in the name is to further minimize the 
       likelihood that the same RANDOM # could be generated by
       2 processes at once, since there is a possibility that 
       between the time search runs and the encoded file is actually
         created the same filename could be generated */
      cLocalFile = EncodeDirectory + "en" + STRING(ETIME) + 
                   STRING(RANDOM(1,99999),"99999").
      cBinaryFile = SEARCH(cLocalFile). /* Check for existence */
    END.

    ASSIGN
    cBinaryFile = LocalFile. /* FILE-INFO:FULL-PATHNAME better? */
    /* 7/12/01 PK The delimiter in the else statement was a '<' */
    attachbinlist = IF attachbinlist = "" THEN cLocalFile
                    ELSE attachbinlist + "," + cLocalFile.
    &IF DEFINED(PublicVersion) > 0 &THEN
      RUN ffw/procs/b64encode.p(cBinaryFile, cLocalFile) NO-ERROR.
    &ELSE 
      RUN utils/base64encode.p(cBinaryFile, cLocalFile) NO-ERROR.
    &ENDIF
    IF ERROR-STATUS:ERROR THEN {&LOGGER} ERROR-STATUS:GET-MESSAGE(1).
  END.  /* IF cFileType = "Binary" THEN  */
  ELSE IF cFileType = "B64ENCODED" THEN DO:
    /* Make sure there is a base64 encoded version of the file in the
       encode directory. If not then create it now for reuse. */
       
    /* Make sure path delimiters are consistent with Unix */
    cLocalFile = REPLACE(localFile,"~\","/").
    IF NUM-ENTRIES(cLocalFile,"/") > 0 THEN 
      cLocalFile = ENTRY(NUM-ENTRIES(cLocalFile,"/"),cLocalFile,"/").
    cLocalFile = EncodeDirectory + "b64-" + cLocalFile.
    
    IF SEARCH(cLocalFile) = ? THEN DO:
      cBinaryFile = LocalFile.
      &IF DEFINED(PublicVersion) > 0 &THEN
        RUN ffw/procs/b64encode.p(cBinaryFile, cLocalFile) NO-ERROR.
      &ELSE 
        RUN utils/base64encode.p(cBinaryFile, cLocalFile) NO-ERROR.
      &ENDIF
      IF ERROR-STATUS:ERROR THEN {&LOGGER} ERROR-STATUS:GET-MESSAGE(1). 
    END.
    attachbinlist = IF attachbinlist = "" THEN cLocalFile
                    ELSE attachbinlist + "," + cLocalFile.
  END.
  ELSE attachbinlist = IF attachbinlist = "" THEN localfile
                       ELSE attachbinlist + "," + localfile.
END PROCEDURE. /* dofiles */

PROCEDURE DoneWithFiles.
  /* Used to cleanup pre-encoded files after calling program is done
     sending the same attachments to multiple recipients. */
  DEF INPUT parameter localfile            as char no-undo.
  DEF INPUT parameter cattachment           as char no-undo.

  DEF VAR cLocalFile                       as char no-undo.
  DEF VAR Mimetype                     as character no-undo.
  DEF VAR ccharset                      as character no-undo.
  DEF VAR cFileType                       AS CHARACTER NO-UNDO.

  RUN ParseParm(INPUT cAttachment,
                OUTPUT mimetype,
                OUTPUT cCHARSET,
                OUTPUT cfiletype).

  IF cFileType = "B64ENCODED" THEN DO:
    /* Make sure path delimiters are consistent with Unix */
    cLocalFile = REPLACE(localFile,"~\","/").
    IF NUM-ENTRIES(cLocalFile,"/") > 0 THEN 
      cLocalFile = ENTRY(NUM-ENTRIES(cLocalFile,"/"),cLocalFile,"/").
    cLocalFile = EncodeDirectory + "b64-" + cLocalFile.
    IF SEARCH(cLocalFile) <> ? THEN DO:
      {&LOGGER2} "Delete " cLocalFile.
      OS-DELETE VALUE(cLocalFile) NO-ERROR.
    END.
  END.
END.

PROCEDURE getsocket:
  DEF INPUT PARAMETER loglevel AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER mailhub  AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER hSocket AS HANDLE NO-UNDO.

  DEF VAR iPortNum AS INTEGER NO-UNDO.
  
  IF NUM-ENTRIES(mailhub,":") > 1 THEN ASSIGN iPortNum = INTEGER(ENTRY(2,mailhub,":")) NO-ERROR.
  IF iPortNum = 0 OR iPortNum = ? THEN ASSIGN iPortNum = 25.
  
  CREATE SOCKET hSocket.
  hSocket:SET-SOCKET-OPTION ("TCP-NODELAY", "FALSE").
  hSocket:SET-SOCKET-OPTION ("SO-LINGER", "FALSE").
  hSocket:SET-READ-RESPONSE-PROCEDURE ("readHandler",THIS-PROCEDURE).

  hSocket:CONNECT("-H " + entry(1,MailHub,":") + " -S " + string(iPortNum)) NO-ERROR.

  IF hSocket:CONNECTED() = FALSE THEN DO:
    {&LOGGER} "Unable to Connect to " + Mailhub + "~n".
    RUN CleanUp.
    vmessage = "No Connection".
    RETURN.
  END.  /* Cannot make a connection to mail server */
  {&LOGGER2} "Socket Connection Established.~n".
  vstate = 1.
END PROCEDURE. /* getsocket */

/******************* Functions ********************************/
FUNCTION newstate RETURNS INTEGER
  (INPUT newstate AS INTEGER,
   INPUT pstring AS CHARACTER,
   INPUT hSocket AS HANDLE):
  DEF VAR vState              AS INTEGER NO-UNDO.
  DEF VAR vbuffer             AS MEMPTR  NO-UNDO.
  DEF VAR DataBytesWritten    AS INTEGER NO-UNDO.
  DEF VAR WriteSuccess        AS LOGICAL NO-UNDO.
  DEF VAR MessageSize         AS INTEGER NO-UNDO.
  
  {&logger2} "newstate " newstate " pstring " pstring skip .
  
  ASSIGN 
  DataBytesWritten = 0
  MessageSize = LENGTH(pstring,"RAW").
        
  SET-SIZE(vbuffer) = 0.
  vState = newState.
  IF pstring = "" THEN RETURN -1.
  SET-SIZE(vbuffer) = LENGTH(pstring,"RAW") + 1.
  PUT-STRING(vbuffer,1) = pstring.
     /* 6/20/01 GC - Loop continuously until the number of bytes
                     written is greater or equal to the message size */
  {&logger2} "newstate - before loop " DataBytesWritten " " 
              MessageSize " " hSocket:BYTES-WRITTEN skip.
  DO WHILE DataBytesWritten < MessageSize:
    WriteSuccess = hSocket:WRITE(vbuffer, DataBytesWritten + 1, 
                                 MessageSize - DataBytesWritten).
    IF NOT WriteSuccess THEN LEAVE.
    DataBytesWritten = DataBytesWritten + hSocket:BYTES-WRITTEN.
  END. /* DO WHILE */
  {&logger2} "newstate - after loop " DataBytesWritten " "
             hSocket:BYTES-WRITTEN " " MessageSize skip.
  SET-SIZE(vbuffer) = 0.
  RETURN vstate.
END function.  /* newstate */

FUNCTION getfile RETURNS MEMPTR(INPUT filnm AS CHAR):
  DEF VAR hdata AS MEMPTR NO-UNDO.
    
  FILE-INFO:FILE-NAME = filnm.
  {&logger2} "in getfile " FILE-INFO:FILE-NAME skip 
              FILE-INFO:FULL-PATHNAME skip
              FILE-INFO:FILE-TYPE skip
              FILE-INFO:FILE-SIZE skip.
    
  IF INDEX(FILE-INFO:FILE-TYPE,"f") = 0 OR
     FILE-INFO:FILE-TYPE = ? THEN DO:
    SET-SIZE(hdata) = 0.
    RETURN hdata.
  END. /*     if file-info:file-type <> "f" then */
  ELSE DO:
    SET-SIZE(hdata) = 0.
    INPUT FROM VALUE(FILE-INFO:FILE-NAME) BINARY NO-MAP NO-CONVERT.
    SET-SIZE(hdata) = FILE-INFO:FILE-SIZE + 1.
    IMPORT UNFORMATTED hdata NO-ERROR.
    INPUT CLOSE.
    IF ERROR-STATUS:ERROR THEN
      SET-SIZE(hdata) = 0.
    RETURN hdata.
  END. /* else */
  /*  set-size(hdata) = 0. */
END FUNCTION. /* getfile */

FUNCTION throwError RETURNS LOGICAL(
 INPUT myErrorText AS CHAR
):
    ASSIGN
     vState = -1
     vMessage = myErrorText
    .

END FUNCTION. 

&IF OPSYS = "UNIX" &THEN
PROCEDURE get-tz:
  DEF OUTPUT PARAMETER oTimezone    AS CHARACTER NO-UNDO.
  INPUT THROUGH date +%Z NO-ECHO.
  SET oTimezone.
  INPUT CLOSE.
END.

&ELSEIF OPSYS = "WIN32" &THEN
/*
   Following Code was added to determine local TimeZone on Windows based 
   platform.  The actual code was contributed to the freeframework by
   Jeff Pilant. 
  
  Description: Reads the time zone registry key
  Input Parameters: <none>
  Output Parameters: offset -- character, 6 long, as "+99:99"

  Author: Jeff Pilant

  Acknoledgements:
    This code used information found in the windows api files:
      windows.i, windows.p, winfunc.i, winfunc.p
    created by Jurjen Dijkstra, 1997
               mailto:jurjen@global-shared.com
               http://www.global-shared.com
    language: Progress 8.2A

  Revision History:
  Version Date         Description
  1.0     09-NOV-2001  Created
  1.0a    15-May-2002  Merged into smtpmail.p  (Paul Keary)
*/
  
/* registry */
&GLOBAL-DEFINE HKEY_LOCAL_MACHINE -2147483646
&GLOBAL-DEFINE ERROR_SUCCESS                0
&GLOBAL-DEFINE MAX_PATH                   260
&GLOBAL-DEFINE REG-KEY "SYSTEM\CurrentControlSet\Control\TimeZoneInformation":U
&GLOBAL-DEFINE REG-ATT               "ActiveTimeBias":U

PROCEDURE RegOpenKeyA EXTERNAL "advapi32" :
  DEFINE INPUT  PARAMETER hkey       AS LONG.
  DEFINE INPUT  PARAMETER lpszSubKey AS CHAR.
  DEFINE OUTPUT PARAMETER phkResult  AS LONG.
  DEFINE RETURN PARAMETER lpResult   AS LONG.
END PROCEDURE.

PROCEDURE RegCloseKey EXTERNAL "advapi32" :
  DEFINE INPUT  PARAMETER hkey     AS LONG.
  DEFINE RETURN PARAMETER lpresult AS LONG.
END PROCEDURE.

PROCEDURE RegQueryValueExA EXTERNAL "advapi32" :
  DEFINE INPUT        PARAMETER hkey         AS LONG.
  DEFINE INPUT        PARAMETER lpValueName  AS CHAR.
  DEFINE INPUT        PARAMETER lpdwReserved AS LONG.
  DEFINE OUTPUT       PARAMETER lpdwType     AS LONG.
  DEFINE INPUT        PARAMETER lpbData      AS LONG. /* memptr */
  DEFINE INPUT-OUTPUT PARAMETER lpcbData     AS LONG.
  DEFINE RETURN       PARAMETER lpresult     AS LONG.
END PROCEDURE.

PROCEDURE get-tz:
/*------------------------------------------------------------------------------
  Purpose:     Get all top-level entries in main registry key
  Parameters:  output character -- Time Zone as +HH:MM
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER tzResult AS CHARACTER NO-UNDO.
  DEFINE VARIABLE tzStr      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE tzBias     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE tzHours    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE tzMinutes  AS INTEGER   NO-UNDO.

  /* Registry read vars */
  DEFINE VARIABLE hKey       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE reslt      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lth        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE Bias       AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE datatype   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE hBiasKey   AS INTEGER   NO-UNDO.

  RUN RegOpenKeyA({&HKEY_LOCAL_MACHINE},
                  {&Reg-Key},
                  OUTPUT hKey,
                  OUTPUT reslt).
  IF reslt = {&ERROR_SUCCESS} THEN
  DO:
    lth  = {&MAX_PATH} + 1.
    SET-SIZE(Bias) = lth.
    RUN RegQueryValueExA(hKey,
                         {&REG-ATT},
                         0,  /* reserved, must be 0 */
                         OUTPUT datatype,
                         GET-POINTER-VALUE(Bias),
                         INPUT-OUTPUT lth,
                         OUTPUT reslt).
    RUN RegCloseKey(hBiasKey,OUTPUT reslt).
    /* Convert value from DWORD to INTEGER */
    tzBias =                ASC(GET-BYTES(Bias, 4, 1)).
    tzBias = tzBias * 256 + ASC(GET-BYTES(Bias, 3, 1)).
    tzBias = tzBias * 256 + ASC(GET-BYTES(Bias, 2, 1)).
    tzBias = tzBias * 256 + ASC(GET-BYTES(Bias, 1, 1)).
    SET-SIZE(Bias)=0.       
    /* Convert value to +HHMM form */
    tzHours = INTEGER(- tzBias / 60).
    tzMinutes = - tzBias - 60 * tzHours.
    tzStr = TRIM(STRING(tzHours, "-99":U) + STRING(tzMinutes, "99":U)).
    IF tzHours >= 0 THEN tzStr = "+":U + tzStr.
    tzResult = tzStr.
  END.
  ELSE
    tzResult = "-0000":U. /* key not found in registry */
  RUN RegCloseKey(hKey,OUTPUT reslt).
END PROCEDURE.

&ELSE
/* Make the code portable if not on Unix or Win32 */
PROCEDURE get-tz:
  DEF OUTPUT PARAMETER oTimezone  AS CHARACTER NO-UNDO.
  /* If you are on a platform other than Unix or Win32 this code must
     be modified to determine your local timeZone.  */
  oTimezone = "EST".
END.
&ENDIF

PROCEDURE Get-SMTP-Date:
/*--------------------------------------------------------------------------
  Purpose:     generate an smtp date string
  Parameters:  OUTPUT CHAR Result
  Notes:       DDD "," DD MMM YYYY HH ":" MM ":" SS ( "+" / "-" ) HHMM
               CRLF
      As of 5/15/2002 This procedure is not currently used by this program.
      It was included because it generates the date in a slightly different
      format than the currently functioning code.  If someone encounters a
      problem with the date format on their smtp server - this code may 
      remedy that problem.   (Paul Keary)  Original Author: Jeff Pilant
---------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER R AS CHARACTER NO-UNDO.

  DEFINE VARIABLE S AS CHARACTER NO-UNDO.
  DEFINE VARIABLE M AS CHARACTER NO-UNDO.
  DEFINE VARIABLE D AS CHARACTER NO-UNDO.

  /*
    The date and time-of-day SHOULD express local time. 
    The zone specifies the offset from UTC.
      "+" (east) or "-" (west).
      "+0000" indicates a time zone at Universal Time.
      "-0000" indicates a local time zone. 
      "-0400" or "-0500" -- EDT or EST
  */
  
  M = "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec":U.
  D = "Sun,Mon,Tue,Wed,Thu,Fri,Sat":U.
  RUN get-tz(OUTPUT S).
  S = ENTRY(WEEKDAY(TODAY), D)      + ", ":U
    + STRING(DAY(TODAY), "99":U)    + " ":U
    + ENTRY(MONTH(TODAY), M)        + " ":U
    + STRING(YEAR(TODAY), "9999":U) + " ":U
    + STRING(TIME, "HH:MM:SS":U)    + " ":U
    + S.
  R = S.
END.
