/*-----------------------------------------------------------------------*
File........: ffw_global.i
Version.....: 1.03 - 1/25/2000
Description : Global Preprocessors that define how FFW compiles
Input Param : <none>
Output Param: <none>
Author......: S.E. Southwell -  Bravepoint / USI - (770) 449-9696
Copyright...: FreeFramework 2000 - http://www.freeframework.org
Created.....: 12/19/2000
Notes.......: This file is heavily commented.  The best way to see what's 
              going on here is to print this out and study the comments.
              
              If you make changes to this program that others could benefit
              from, please share your code with me: ses@bravepointdallas.com
              and the FFW project: ffw@peg.com
              
              By making some of these settings as preprocessors rather than
              features of the .ini file, you will save some runtime overhead.
              
              For instance, if you are using shared-memory database connections,
              turning off the database checking feature will make your webobjects
              run faster.
              
              After updating this file, recompile all of the .p files in robust directory
              and all of the .html files in the robust/webtools directory.
*-----------------------------------------------------------------------*/
/*Make sure this file is only included once*/
&IF DEFINED(FFW_GLOBAL_I) = 0 &THEN
&GLOBAL-DEFINE FFW_GLOBAL_I YES
/* Actual settings begin below: */

    /*Where is the .ini file located?  By default search for brokername.ini
      in the propath somewhere.  If you hard-code this, make sure to quote it.*/
    
        &GLOBAL-DEFINE FFW_INI_LOCATION  SEARCH(WEB-CONTEXT:CONFIG-NAME + ".ini")    
     
    /*FFW Installation path - If you install FreeFrameWork anywhere other than
     in your direct propath, then use the fully qualified path to the directory 
     in which you installed freeframework.  For instance, if you have:
     C:\mywebapp\ffw\webtools\workshop.r   - then your ffw installation
     path is C:\mywebapp\ffw
     The default setting of ./ffw should work, as long as you place the ffw
     directory in your propath.  */
    
        &GLOBAL-DEFINE FFW_INSTALL_PATH ffw

    /*Careful here.  Don't modify the default unless you're using a non-PSC version
        of WebSpeed - Note that by default this is a multi-line preprocessor.  If 
        you hard-code this to a specific directory, make sure that you quote it.*/
    
        &GLOBAL-DEFINE AGENT_WORK_DIR (IF PROVERSION BEGINS "2" THEN ~
        WEB-CONTEXT:GET-CONFIG-VALUE("DefaultDirectory":U)~
        ELSE WEB-CONTEXT:GET-CONFIG-VALUE("workdir":U))
    
    /*Whether to check database connections before running a program from the web*/
    
        &GLOBAL-DEFINE FFW_DBCHECK YES

    /*Whether to allow check for a dbdown.lock file and see if the database is down
        intentionally.  Default is yes. You can turn off in the .ini file, but you'll save 
        some overhead by turning off if you are sure you'll never need it.*/
        
        &GLOBAL-DEFINE FFW_DBDOWNLOCKCHECK YES
        
    /*Whether to allow check for a sysdown.lock file and see if the broker is down
        intentionally.  Default is yes. You can turn off in the .ini file, but you'll save 
        some overhead by turning off if you are sure you'll never need it.*/
        
        &GLOBAL-DEFINE FFW_SYSDOWNLOCKCHECK YES
        
    /*Whether to include the capability of runlogging - very valuable for trouble-
        shooting, this can be turned on and off from the .ini file, but you will 
        save a small amount of overhead by turning it off.  By default, we're gonna
        leave it turned on. - Set to YES or NO*/
    
        &GLOBAL-DEFINE FFW_RUNLOGGING YES
    
    /*Whether to include the capability of compiling speedscript on the fly.  This is
        turned on in the .ini file, but you can compile it out if you wish, for a small
        savings on overhead - Set to YES or NO*/
    
        &GLOBAL-DEFINE FFW_COMPILEONFLY YES
    
    /*If using a non-PSC version of WebSpeed (coming soon), set this to yes:*/
        
        &GLOBAL-DEFINE WEB_CONTEXT_EXCLUDE NO
        
            /*If you set the above to YES, then you need to set the below 
                to TRUE or FALSE:*/
                
                &GLOBAL-DEFINE DEVELOPMENT_MODE TRUE

    /*If you do not use mapped webobjects, set this to NO for a speed boost
        If you are not sure, then leave this at YES  */
        &GLOBAL-DEFINE MAPPEDOBJECTS YES
   
    /*If you have mulitple developers who would like seperate propaths, set this
        to yes.  Set to no, if you want to save overhead.  This has been tested
        and works very well with MS SourceSafe. */
        &GLOBAL-DEFINE MULTI_DEV_PROPATHS no
    
    /* If, and ONLY if you have your WebSpeed broker and agents sitting on a box
        with a public internet connection, then you should turn this ON so that 
        malicious users cannot point their webservers at your WebSpeed broker. See 
        the .ini file for more details on activating this*/
        &GLOBAL-DEFINE WEBSERVER_SIG_CHECK NO
        
    /* If you have a product such as eSales center from NxTrend, which has encrypted 
         source code for many of the include files and procedures, then you will need
         to set this to yes in order to use the FreeFrameWork shop compiler and html
         compile-on-the-fly functionality. */
        &GLOBAL-DEFINE XCODE_COMPILES YES
/*-------------------------------------------------------------------------------*/
/*NO REASON TO EDIT ANYTHING BELOW*/
/* YOU HAVE BEEN WARNED!!!!!!*/

DEFINE NEW GLOBAL SHARED VAR v-logtypes   AS CHAR NO-UNDO.    /*CAN-DO list of log entry types we can write to log*/


&IF "{&OPSYS}" = "WIN32" &THEN 
	&GLOBAL-DEFINE SLASH ~\
&ELSE
	&GLOBAL-DEFINE SLASH /
&ENDIF

/*Don't delete this "Endif"*/
&ENDIF
/*The END!*/


