/*-----------------------------------------------------------------------*
  File........: robust.i
  Version.....: 1.03 - 1/25/2001
  Description : Procedures and functions that add robustness enhancements
  				to the standard WebSpeed functionality
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell -  BravePoint / USI (770) 449-9696
  Copyright...: FreeFramework 2000,2001 - http://www.freeframework.org
  Created.....: 4/24/2000
  Notes.......: This file is heavily commented.  The best way to see what's 
  				going on here is to print this out and study the comments.
				
				If you make changes to this program that others could benefit
				from, please share your code with me: ses@bravepointdallas.com
                and the FFW project at ffw@peg.com
				
                Future features:
                 - Aliasing - translate one directory into another
                 - Reading .ini from a database, or transferring between file and db.
 *-----------------------------------------------------------------------*/
&GLOBAL-DEFINE EXCLUDE-Output-content-type  TRUE /*Because the standard version has state-aware crap in it*/
{ ffw/lib/ffw_global.i }        /*PATH SETTINGS AND OTHER GENERAL THINGS*/
{ ffw/lib/rdefs.i }  		/*ROBUSTNESS LIBRARY DEFINITIONS*/
{ ffw/lib/agentsetting.i } 	/*AGENT SETTINGS LIBRARY*/
{ ffw/lib/showerrorscreen.i }   /*FUNCTION FOR SHOWING CUSTOMIZED ERROR SCREEN*/
{ ffw/lib/filetools.i }         /*FOR COMPILING AND OTHER STUFF*/
{ ffw/lib/devcheck.i }          /*LIBRARY FOR CHECKING WHICH VERSION OF WEBSPEED*/

RUN ReadConfigOptions.  /*Get settings from the .ini file*/
RUN ConfigurationWarnings.  /*Error log warnings of security holes*/

&IF "{&FFW_RUNLOGGING}" = "YES" &THEN
IF v-runlogpath ne ""
 THEN LogNote("Note","Agent " 
&IF "{&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
 + MSNGRPID
&ELSE
 + ENTRY(3,WEB-CONTEXT:EXCLUSIVE-ID,":") 
&ENDIF
 + " started with runlogging turned on.").
&ENDIF

IF CAN-DO("Yes,Standby",getAgentSetting("Profiler")) THEN DO:
    RUN procs/profiling.p PERSISTENT SET v-profilerhdl NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        logNote("Note","Profiler unable to start: " + ERROR-STATUS:GET-MESSAGE(1)).
        ASSIGN v-profilingOn = FALSE. /*redundant probably, but here for clarity*/
    END. /*ERROR*/
    ELSE DO:
        SESSION:ADD-SUPER-PROCEDURE(v-profilerhdl).
        IF getAgentSetting("Profiler") = "yes" THEN DO:
            logNote("Note","Profiler started for this session.").
            ASSIGN
             v-profilingOn = TRUE 
             v-profilerFileName = getAgentSetting("ProfilerFilename")
            .
        END.
        ELSE logNote("Note","Profiler in standby mode for this session").
    END.
END.
 
PROCEDURE ConfigurationWarnings:
/*------------------------------------------------------------------------------
  Purpose:     Make a note in the server log about any WebSpeed configuration problems.
  Parameters:  None
  Notes:       
------------------------------------------------------------------------------*/
	DEFINE VAR v-numproblems AS INTEGER NO-UNDO.
	IF DevCheck() THEN DO:
		 LogNote("Caution","Agent started in DEVELOPMENT Mode.  You should change to Production mode prior to actual deployment.  If you run development mode in a deployment environment, you face security risks!").
		 ASSIGN v-numproblems = v-numproblems + 1.
	END.

&IF "{&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
	IF WEB-CONTEXT:GET-CONFIG-VALUE("srvrDebug":U) = "Enabled" THEN DO:
		 LogNote("WARNING","Agent started with debug mode ENABLED. You should not use this unless just for temporary debugging purposes. If you leave this setting on a production server, you face security risks!").
		 ASSIGN v-numproblems = v-numproblems + 1.
	END.
&ENDIF
	IF "{&FFW_DBCHECK}" NE "YES" THEN DO:
		 LogNote("WARNING","You have turned off database connection checking for this agent. If this agent should for some reason lose its database connection, it will stay disconnected!").
		 ASSIGN v-numproblems = v-numproblems + 1.
	END.
	IF v-htmlcompileonfly = TRUE AND NOT DevCheck() THEN DO:
		 LogNote("WARNING","HTML compile on-the-fly option is turned on in production mode.  This may not be very secure.").
		 ASSIGN v-numproblems = v-numproblems + 1.
	END. /*compile on fly used in production mode*/
	 
	IF v-numproblems > 0 THEN LogNote("Caution","Basic startup security check showed " + string(v-numproblems) + " possible problem(s).").
	IF v-numproblems = 0 THEN LogNote("Note","Basic startup security check showed no major problems.").
END PROCEDURE. /*configurationWarnings*/
 
PROCEDURE ReadConfigOptions:
/*------------------------------------------------------------------------------
  Purpose:     Read in Extra configuration options at agent startup.
  Parameters:  None
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR v-textin 	AS CHAR NO-UNDO.
    DEFINE VAR v-thisdb 	AS CHAR NO-UNDO.
    DEFINE VAR v-thisdbset 	AS CHAR NO-UNDO.
    DEFINE VAR v-importmode AS CHAR NO-UNDO.
    DEFINE VAR v-filename 	AS CHAR NO-UNDO. /*the config file name*/
    DEFINE VAR v-workdir    AS CHAR NO-UNDO. /*WORKING DIRECTORY*/
    DEFINE VAR v-ffwdir     AS CHAR NO-UNDO. /*Directory containing FFW code*/
    DEFINE VAR v-thisdev    AS CHAR NO-UNDO. /*Current developer*/
    DEFINE VAR v-thisdir    AS CHAR NO-UNDO. /*Developer's directory*/
    DEFINE VAR v-thispath   AS CHAR NO-UNDO. /*Checking paths while reading propath*/
    DEFINE VAR v-thisdbattr AS CHAR NO-UNDO. /*Attribute name of individual dbs*/
    DEFINE VAR v-thisattval AS CHAR NO-UNDO. /*Value of the current db attr.*/
    DEFINE VAR v-dlcdir    AS CHAR NO-UNDO. /*Where is DLC located on this machine?*/
    
	ASSIGN
	 v-importmode = "normal"
	 v-filename = {&FFW_INI_LOCATION}
	 v-workdir = RIGHT-TRIM( {&AGENT_WORK_DIR} , "/ ~\")
     v-ffwdir = RIGHT-TRIM("{&FFW_INSTALL_PATH}","/ ~\")
     v-dlcdir = whichDLC() /*in filetools.i*/
    .
    ASSIGN FILE-INFO:FILE-NAME = v-ffwdir.
    ASSIGN v-ffwdir = FILE-INFO:FULL-PATHNAME.
    IF v-ffwdir = ? THEN logNote("Warning","FFW_INSTALL_PATH not properly set, or not found.  Check ffw_global.i.").
    ELSE SetAgentSetting("ffwdir",v-ffwdir).
    
	IF v-filename = ? THEN DO:
		LogNote("ERROR",
        &IF "(&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
        "Your "
        &ELSE
        WEB-CONTEXT:CONFIG-NAME 
        &ENDIF
        + ".ini file was not found.  Ensure that the ini file is somewhere in the agents' path, and RESTART!").
		RETURN.
	END. /*brokername.ini file not found*/
	INPUT STREAM CFGSTREAM FROM VALUE(v-filename).
	READCFGBLOCK: REPEAT:
		IMPORT STREAM CFGSTREAM UNFORMATTED v-textin.
		IF trim(v-textin) = "" THEN NEXT READCFGBLOCK.
		IF SUBSTRING(trim(v-textin),1,1) = "#" THEN NEXT READCFGBLOCK.
		IF SUBSTRING(trim(v-textin),1,1) = ";" THEN NEXT READCFGBLOCK.
		ASSIGN v-textin = ENTRY(1,v-textin,"#"). /*allow comments at the end of a line*/
		CASE v-importmode:
			WHEN "normal" THEN CASE TRIM(ENTRY(1,v-textin,".")):
				WHEN "database" THEN DO:
                    IF NUM-ENTRIES(ENTRY(1,v-textin,"="),".") > 2 THEN DO:
                        ASSIGN
                         v-thisdb = TRIM(ENTRY(2,ENTRY(1,v-textin,"="),"."))
                         v-thisdbattr = TRIM(ENTRY(3,ENTRY(1,v-textin,"="),"."))
                         v-thisattval = TRIM(ENTRY(2,v-textin,"="))
                        .
                        FIND tt-db
                         WHERE tt-db.databaseName = v-thisdb
                         NO-ERROR.
                        IF NOT AVAIL tt-db THEN DO:
    						CREATE tt-db.
    						ASSIGN tt-db.databaseName = v-thisdb.
    					END.
                        CASE v-thisdbattr:
                            WHEN "filename" 
                             OR WHEN "file"
                             THEN ASSIGN tt-db.filename = v-thisattval.
                            WHEN "hostname" THEN ASSIGN tt-db.hostname = v-thisattval.
                            WHEN "servicename" THEN ASSIGN tt-db.servicename = v-thisattval.
                            WHEN "network" THEN ASSIGN tt-db.network = v-thisattval.
                            WHEN "otherparams" THEN ASSIGN tt-db.otherparams = v-thisattval.
                            OTHERWISE logNote("Error","Error parsing .ini file - Unknown parameter '" + v-thisdbattr + "' of database '" + v-thisdb + "'.").
                        END CASE.
                    END.
                    ELSE DO:
    					ASSIGN v-thisdb = TRIM(ENTRY(1,ENTRY(2,v-textin,"."),"=")).
    					FIND TT-DB 
    					 WHERE tt-db.databaseName = v-thisdb
    					 NO-ERROR.
    					IF NOT AVAIL tt-db THEN DO:
    						CREATE tt-db.
    						ASSIGN tt-db.databaseName = v-thisdb.
    					END.
    					ASSIGN
    					 tt-db.connectproc = TRIM(ENTRY(1,ENTRY(2,SUBSTRING(v-textin,INDEX(v-textin,"."),-1),"=")))
    					 tt-db.notifyproc =  (IF NUM-ENTRIES(ENTRY(2,SUBSTRING(v-textin,INDEX(v-textin,"."),-1),"=")) > 1 
                                            THEN TRIM(ENTRY(2,ENTRY(2,SUBSTRING(v-textin,INDEX(v-textin,"."),-1),"="))) 
                                            ELSE ?)
                        .
                    END.
				END. /*database*/
				WHEN "dbset" THEN DO:
					ASSIGN v-thisdbset = TRIM(ENTRY(1,ENTRY(2,v-textin,"."),"=")).
					FIND tt-dbset
					 WHERE tt-dbset.dbset = v-thisdbset
					 NO-ERROR.
					IF NOT AVAIL tt-dbset THEN DO:
						CREATE tt-dbset.
						ASSIGN tt-dbset.dbset = v-thisdbset.
					END.
					ASSIGN tt-dbset.dblist = TRIM(ENTRY(2,v-textin,"=")).
				END. /*dbset*/
				WHEN "objects" THEN DO:
					ASSIGN
					 v-thisdbset = TRIM(ENTRY(2,v-textin,".")," ~{")
					 v-importmode = "objects"
					.
				END. /*objects*/
				WHEN "redirect ~{" THEN DO:
					ASSIGN v-importmode = "redirects".
				END. /*redirect*/
				WHEN "IPlimit ~{" THEN DO:
					ASSIGN v-importmode = "iplimit".
				END. /*IPLimit*/
				WHEN "IPKill ~{" THEN DO:
					ASSIGN v-importmode = "ipkill".
				END. /*IPKill*/
				WHEN "Propath ~{" THEN DO:
					ASSIGN
					 v-importmode = "propath"
					 v-PROPATH = ""
					.
				END. /*Propath*/
				WHEN "DevPropath ~{" THEN DO:
					ASSIGN
					 v-importmode = "devpropath"
					.
				END. /*DevPropath*/
				WHEN "WebRunPath ~{" THEN DO:
					ASSIGN
					 v-importmode = "WebRunPath"
					 v-webRunPath = ""
					.
				END. /*WebRunPath*/
				OTHERWISE DO: /*Probably assigning a variable*/
					IF NUM-ENTRIES(v-textin,"=") > 1 
					 THEN CASE TRIM(ENTRY(1,v-textin,"=")): /*variable name*/
					 	WHEN "resettable" THEN DO:
							IF TRIM(ENTRY(2,v-textin,"=")) = "yes"
							 THEN ASSIGN v-resettable = TRUE.
							ELSE ASSIGN v-resettable = FALSE.
						END.
                        &IF "{&FFW_RUNLOGGING}" = "YES" &THEN 
						WHEN "runlogpath" THEN DO:
							ASSIGN v-runlogpath = TRIM(ENTRY(2,v-textin,"=")).
                            ASSIGN v-runlogpath = replace(v-runlogpath,"@~{WorkPath~}",v-workdir).
                            ASSIGN v-runlogpath = replace(v-runlogpath,"@~{FFWPath~}",v-ffwdir).
                            ASSIGN v-runlogpath = replace(v-runlogpath,"@~{DLCPath~}",v-dlcdir).
                            ASSIGN v-runlogpath = replace(v-runlogpath,"~\","/").
                            IF TRIM(v-runlogpath,"~\/") ne v-runlogpath 
                             THEN ASSIGN v-runlogpath = v-runlogpath + "{&SLASH}".
                            IF v-runlogpath NE "" THEN DO:
    							FILE-INFO:FILE-NAME = v-runlogpath.
    							IF FILE-INFO:FULL-PATHNAME = ? 
    							 OR INDEX(FILE-INFO:FILE-TYPE,"D") < 1
    							 THEN DO:
    								LogNote("Error"," RunLogPath: " + v-runlogpath + " is not found.").
    							 	ASSIGN v-runlogpath = "".	
    							END. /*BAD runlogpath*/			
                                SetAgentSetting("RunLogPath",v-runlogpath).		
                            END. /*No runlogpath*/
						END.
                        &ENDIF
                        
                        &IF "{&FFW_COMPILEONFLY}" = "YES" &THEN
						WHEN "htmlcompileonfly" THEN DO:
							CASE TRIM(ENTRY(2,v-textin,"=")) :
							  	WHEN "ALWAYS" THEN ASSIGN v-htmlcompileonfly = TRUE.
							  	WHEN "DEVONLY" THEN DO:
									IF DevCheck() THEN ASSIGN v-htmlcompileonfly = TRUE.
									 ELSE v-htmlcompileonfly = FALSE.
								END.
								OTHERWISE ASSIGN v-htmlcompileonfly = FALSE.
							END CASE. 
						END. /*HTML compile on the fly*/
                        &ENDIF
                        
						WHEN "checkInterval" THEN DO:
							ASSIGN v-checkInterval = INTEGER(TRIM(ENTRY(2,v-textin,"="))) NO-ERROR.
						END. /*checkinterval*/
						WHEN "batchProcName" THEN DO:
							ASSIGN v-batchProcName = TRIM(ENTRY(2,v-textin,"=")).
							ASSIGN FILE-INFO:FILE-NAME = v-batchProcName.
							IF FILE-INFO:FULL-PATHNAME = ? THEN ASSIGN v-batchProcName = "".
						END. /*checkinterval*/
						OTHERWISE DO:
							SetAgentSetting(TRIM(ENTRY(1,v-textin,"=")),TRIM(ENTRY(2,v-textin,"="))).
						END.
					END CASE. /*variable name*/
				END. /*OTHERWISE: ASSIGN A VARIABLE*/
			END CASE. /*import mode normal*/
			
			WHEN "webrunpath" THEN DO: /*import mode webrunpath*/
				IF TRIM(v-textin) = "~}" THEN DO:
					assign
					 v-webrunpath = trim(v-webrunpath,", ")
					 v-importmode = "normal"
					.
					NEXT READCFGBLOCK.
				END.
                ASSIGN v-thispath = trim(v-textin).
                ASSIGN v-thispath = replace(v-thispath,"@~{WorkPath~}",v-workdir).
                ASSIGN v-thispath = replace(v-thispath,"@~{FFWPath~}",v-ffwdir).
                ASSIGN v-thispath = replace(v-thispath,"@~{DLCPath~}",v-dlcdir).
                ASSIGN v-thispath = replace(v-thispath,"~\","/").
				ASSIGN v-webrunpath = v-webrunpath + "," + v-thispath.
			END. /*import mode webrunpath*/
            
			WHEN "propath" THEN DO: /*import mode propath*/
				IF TRIM(v-textin) = "~}" THEN DO:
					assign
					 propath = trim(v-propath,", ")
					 v-importmode = "normal"
					.
					NEXT READCFGBLOCK.
				END.
                ASSIGN v-thispath = trim(v-textin).
                ASSIGN v-thispath = replace(v-thispath,"@~{WorkPath~}",v-workdir).
                ASSIGN v-thispath = replace(v-thispath,"@~{FFWPath~}",v-ffwdir).
                ASSIGN v-thispath = replace(v-thispath,"@~{DLCPath~}",v-dlcdir).
                ASSIGN v-thispath = replace(v-thispath,"@~{ProPath~}",PROPATH).
                ASSIGN v-thispath = replace(v-thispath,"~\","/").
				ASSIGN FILE-INFO:FILE-NAME = ENTRY(1,v-thispath).
                /* ||| Still need to make sure it's a directory */
                IF FILE-INFO:FULL-PATHNAME = ? THEN LogNote("Warning","Propath contains '" + v-thispath + "', which is not found").
				ELSE ASSIGN v-propath = v-propath + "," + FILE-INFO:FULL-PATHNAME.
			END. /*import mode propath*/

            &IF "{&MULTI_DEV_PROPATHS}" = "YES" &THEN
			WHEN "devpropath" THEN DO: /*import mode devpropath*/
				IF TRIM(v-textin) = "~}" THEN DO:
					assign v-importmode = "normal".
					NEXT READCFGBLOCK.
				END.
                ASSIGN
                 v-thisdev = TRIM(ENTRY(1,v-textin,":"))
                 v-thispath = TRIM(SUBSTRING(v-textin,INDEX(v-textin,":") + 1,-1)) 
                .
                ASSIGN v-thispath = replace(v-thispath,"@~{WorkPath~}",v-workdir).
                ASSIGN v-thispath = replace(v-thispath,"@~{FFWPath~}",v-ffwdir).
                ASSIGN v-thispath = replace(v-thispath,"@~{DLCPath~}",v-dlcdir).
                ASSIGN v-thispath = replace(v-thispath,"~\","/").

                FIND tt-developer WHERE tt-developer.devname = v-thisdev
                 NO-ERROR.
                IF NOT AVAIL tt-developer THEN DO:
                    CREATE tt-developer.
                    ASSIGN tt-developer.devname = v-thisdev.
                END.
                ASSIGN tt-developer.mypropath = v-thispath.
			END. /*import mode devpropath*/
            &ENDIF            
			
            WHEN "ipkill" THEN DO: /*import mode ipKill*/
				IF TRIM(v-textin) = "~}" THEN DO:
					assign v-importmode = "normal".
					NEXT READCFGBLOCK.
				END.
				FIND tt-ipkill
				 WHERE tt-ipkill.ipaddr = trim(v-textin)
				 NO-ERROR.
				IF NOT AVAIL tt-ipkill THEN DO:
					CREATE tt-ipkill.
					ASSIGN tt-ipkill.ipaddr = trim(v-textin).
				END.
			END. /*import mode ipKill*/
			
			WHEN "iplimit" THEN DO: /*import mode iplimit*/
				IF TRIM(v-textin) = "~}" THEN DO:
					assign v-importmode = "normal".
					NEXT READCFGBLOCK.
				END.
				FIND tt-iplimit
				 WHERE tt-iplimit.resourcefrom = trim(entry(1,v-textin,":"))
				 NO-ERROR.
				IF NOT AVAIL tt-iplimit THEN DO:
					CREATE tt-iplimit.
					ASSIGN tt-iplimit.resourcefrom = trim(entry(1,v-textin,":")).
				END.
				IF num-entries(v-textin,":") > 1 THEN ASSIGN tt-iplimit.iplist = trim(entry(2,v-textin,":")).
				IF num-entries(v-textin,":") > 2 THEN ASSIGN tt-iplimit.errormessage = trim(entry(3,v-textin,":")).
			END. /*import mode IPLimit*/
			
			WHEN "redirects" THEN DO: /*import mode redirect*/
				IF TRIM(v-textin) = "~}" THEN DO:
					assign v-importmode = "normal".
					NEXT READCFGBLOCK.
				END.
				FIND tt-redirect
				 WHERE tt-redirect.resourcefrom = trim(entry(1,v-textin))
				 NO-ERROR.
				IF NOT AVAIL tt-redirect THEN DO:
					CREATE tt-redirect.
					ASSIGN tt-redirect.resourcefrom = trim(entry(1,v-textin)).
				END.
				IF num-entries(v-textin) > 1 THEN ASSIGN tt-redirect.resourceto = trim(entry(2,v-textin)).
				IF num-entries(v-textin) > 2 AND trim(entry(3,v-textin)) = "yes" THEN ASSIGN tt-redirect.log-fl = TRUE.
				IF num-entries(v-textin) > 3 AND trim(entry(4,v-textin)) = "P" THEN ASSIGN tt-redirect.perm-fl = TRUE.
			END. /*import mode redirects*/
			
			WHEN "objects" THEN DO: /*import mode objects*/
				IF TRIM(v-textin) = "~}" THEN DO:
					assign v-importmode = "normal".
					NEXT READCFGBLOCK.
				END.
				FIND tt-objectDb
				 WHERE tt-objectDB.dbSet = v-thisdbset
				 AND tt-objectDB.objectName = trim(v-textin)
				 NO-ERROR.
				IF NOT AVAIL tt-objectDB THEN DO:
				CREATE tt-objectDB.
				ASSIGN
				 tt-objectDB.dbSet = v-thisdbset
				 tt-objectDB.objectName = trim(v-textin)
				.
				END. /*created tt-objectDB*/
			END. /*import mode objects*/
			
		END CASE. /*MODE*/
		
	END. /*READCFGBLOCK*/
	INPUT STREAM CFGSTREAM CLOSE.
    
	/**** TEMPORARY .R FILE PATH ********/
    ASSIGN v-temprpath = getAgentSetting("TempRPath").
    ASSIGN v-temprpath = replace(v-temprpath,"@~{WorkPath~}",v-workdir).
    ASSIGN v-temprpath = replace(v-temprpath,"@~{FFWPath~}",v-ffwdir).
    ASSIGN v-temprpath = replace(v-temprpath,"@~{DLCPath~}",v-dlcdir).
    ASSIGN v-temprpath = replace(v-temprpath,"~\","/").
    SetAgentSetting("TempRPath",v-temprpath).

    /*Make sure tempRpath is kosher:*/
    ASSIGN FILE-INFO:FILE-NAME = v-temprpath.
    IF FILE-INFO:FULL-PATHNAME = ? THEN DO:  /*Is it found?*/
        logNote("Error","Your specified tempRPath directory '" + v-tempRPath + "' is not found.  Using working directory instead.").
        ASSIGN v-tempRpath = "".
    END.
    IF INDEX(FILE-INFO:FILE-TYPE,"D") < 1 THEN DO: /*Is it a directory?*/
        logNote("Error","Your specified tempRPath directory '" + v-tempRPath + "' is a file, and not a directory.  Using working directory instead.").
        ASSIGN v-tempRpath = "".
    END.     
    
    IF v-temprpath = "" THEN DO:  /*Use the working directory*/
        ASSIGN FILE-INFO:FILE-NAME = ".".
        ASSIGN v-temprpath = FILE-INFO:FULL-PATHNAME. 
    END. /*assigned it to working directory*/
    
    IF RIGHT-TRIM(v-temprpath,"~\/") EQ v-temprpath THEN ASSIGN v-temprpath = v-temprpath + "/".        

    /*Make sure that the temporary .r files are in the webrunpath and propath.*/
    IF v-temprpath ne ? then ASSIGN 
     v-webrunpath = v-webrunpath + "," + v-temprpath + "tmp_*"
     v-propath = v-propath + "," + v-temprpath
    .

    /**** LOG TYPES *****/
    ASSIGN v-logtypes = replace(getAgentSetting("LogTypes":U),", ",","). /*strip out spaces*/
    IF v-logtypes = "" THEN ASSIGN v-logtypes = "*".
 
    ASSIGN v-speedscripttypes = REPLACE(GetAgentSetting("speedscripttypes":U)," ","").
    IF v-speedscripttypes = "" THEN ASSIGN v-speedscripttypes = ".html,.htm". 

    &IF "{&FFW_SYSDOWNLOCKCHECK}" = "YES" &THEN
    ASSIGN
     v-sysdownlockcheck = GetAgentSetting("sysdownlockcheck":U)
     v-sysdownlockfile = GetAgentSetting("sysdownlockfile":U)
    .
    IF v-sysdownlockfile = "" THEN ASSIGN v-sysdownlockfile = "sysdown.lock".
    &ENDIF
    
    &IF "{&FFW_DBDOWNLOCKCHECK}" = "YES" &THEN
    ASSIGN
     v-dbdownlockcheck = GetAgentSetting("dbdownlockcheck":U)
     v-dbdownlockfile = GetAgentSetting("dbdownlockfile":U)
    .
    IF v-dbdownlockfile = "" THEN ASSIGN v-dbdownlockfile = "dbdown.lock".
    &ENDIF
    
END PROCEDURE. /*ReadConfigOptions*/


PROCEDURE run-web-object:
/*------------------------------------------------------------------------------
  Purpose:     Run a program the right way. 
  Parameters:  p_FileName = (CHAR) Name of application file user is requesting
  Notes:       
------------------------------------------------------------------------------*/
	DEFINE INPUT PARAMETER p_FileName AS CHAR NO-UNDO.
	
	DEFINE VAR rSearchFile     AS CHARACTER NO-UNDO.
	DEFINE VAR rFileExt        AS CHARACTER NO-UNDO.
	DEFINE VAR this-wo-hdl     AS HANDLE    NO-UNDO.
	DEFINE VAR v-mydbcount	   AS INTEGER   NO-UNDO.
	DEFINE VAR v-tempwfile	   AS CHAR 	    NO-UNDO.
	DEFINE VAR v-compiledonfly AS LOGICAL   NO-UNDO.
	DEFINE VAR v-compilermsg   AS CHAR      NO-UNDO.
	DEFINE VAR v-thisconnected AS LOGICAL 	NO-UNDO.
&IF "{&MULTI_DEV_PROPATHS}" = "YES" &THEN
    DEFINE VAR v-devuser        AS CHAR      NO-UNDO.
    DEFINE VAR v-propathreset   AS LOGICAL   NO-UNDO.
    DEFINE VAR v-realpropath    AS CHAR NO-UNDO.
&ENDIF    
&IF "{&FFW_RUNLOGGING}" = "YES" &THEN
	DEFINE VAR v-runlogfilename AS CHAR 	NO-UNDO.
	
	IF v-runlogpath NE ""
	 THEN ASSIGN v-runlogfilename = v-runlogpath + STRING(YEAR(TODAY)) 
			+ STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") 
			+ "_" 
            &IF "{&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
            + msngrpid
            &ELSE
            + entry(3,web-context:exclusive-id,":") 
            &ENDIF
            + ".log".
&ENDIF

/* If the whole system needs to be down for awhile, and NO webobjects get run, 
then we catch it, and just output our own customized error screen. */
&IF "{&FFW_SYSDOWNLOCKCHECK}" = "YES" &THEN
    /*First make sure that system is not intentionally down*/
    /* If so, just show error page and return. */
    IF v-sysdownlockcheck = "yes" THEN DO: /*From the .ini file*/
        FILE-INFO:FILE-NAME = SEARCH(v-sysdownlockfile).
        IF FILE-INFO:FULL-PATHNAME NE ? THEN DO:
            IF v-sysdownmessage = "" THEN DO: /*read it from the lock file*/
                /*Recycling the stream used for reading the .ini file*/
                INPUT STREAM CFGSTREAM FROM VALUE(FILE-INFO:FULL-PATHNAME).
                REPEAT:
                    IMPORT STREAM CFGSTREAM UNFORMATTED v-lockfiletextin.
                    ASSIGN v-sysdownmessage = v-sysdownmessage + "~n" + v-lockfiletextin.   
                END. /*REPEAT*/
                INPUT STREAM CFGSTREAM CLOSE.
            END. /*hadn't yet read the sysdown message*/
            ASSIGN v-sysdownlocknow = TRUE.
            ShowErrorScreen(v-sysdownmessage).
            RETURN. 
        END. /*LOCK FILE EXISTED*/
        ELSE DO: /*DB NOT INTENTIONALLY DOWN*/
            IF v-sysdownlocknow THEN ASSIGN  /*reset when lock file disappears*/
             v-sysdownlocknow = FALSE
             v-sysdownmessage = ""
            .
        END.
    END.
&ENDIF

/* If some databases are intentionally down for awhile...*/
&IF "{&FFW_DBDOWNLOCKCHECK}" = "YES" &THEN
    /*First make sure that any required databases are not intentionally down*/
    /* If so, just show error page and return. */
    IF v-dbdownlockcheck = "yes" THEN DO: /*From the .ini file*/
        FILE-INFO:FILE-NAME = SEARCH(v-dbdownlockfile).
        IF FILE-INFO:FULL-PATHNAME NE ? THEN DO:
            IF v-dbdownmessage = "" THEN DO: /*read it from the lock file*/
                /*Recycling the stream used for reading the .ini file*/
                INPUT STREAM CFGSTREAM FROM VALUE(FILE-INFO:FULL-PATHNAME).
                REPEAT:
                    IMPORT STREAM CFGSTREAM UNFORMATTED v-lockfiletextin.
                    ASSIGN v-dbdownmessage = v-dbdownmessage + "~n" + v-lockfiletextin.   
                END. /*REPEAT*/
                INPUT STREAM CFGSTREAM CLOSE.
            END. /*hadn't yet read the dbdown message*/
            ASSIGN v-dbdownlocknow = TRUE.
            
            /* ShowErrorScreen(v-dbdownmessage).
            RETURN. */
        END. /*LOCK FILE EXISTED*/
        ELSE DO: /*DB NOT INTENTIONALLY DOWN*/
            IF v-dbdownlocknow THEN ASSIGN  /*reset when lock file disappears*/
             v-dbdownlocknow = FALSE
             v-dbdownmessage = ""
            .
            /*Missing DB connections will be caught down below, or when the agent does its 
                proactive db check.*/
        END.
    END.
&ENDIF
		
            
            
	/* Next check whether or not the user's IP address is BANNED */
	FIND tt-ipkill
	 WHERE tt-ipkill.ipaddr = REMOTE_ADDR
	 NO-LOCK NO-ERROR.
	IF AVAIL tt-ipkill THEN DO:
		/* This would be fun, huh? Can't do it because it would facilitate DoS on others
		{&out} "HTTP/1.0 301 Moved Permanently~r~n".
		output-http-header("Location","http://" + REMOTE_ADDR).
		{&out} http-newline http-newline.
		*/
		{&out} "HTTP/1.0 204 No Content" HTTP-NEWLINE.
		{&out} "Content-type: text/html" HTTP-NEWLINE HTTP-NEWLINE.
		RETURN.
	END.
	
	/* Next check to see if this resource needs to be redirected */
	FIND tt-redirect
	 WHERE tt-redirect.resourcefrom = p_filename
	 NO-ERROR.
	IF AVAIL tt-redirect THEN DO:  /*this request needs to be redirected*/
		IF tt-redirect.perm-fl THEN {&out} "HTTP/1.0 301 Moved Permanently" http-newline.
		ELSE {&out} "HTTP/1.0 302 Moved Temporarily~r~n".
		output-http-header("Location",tt-redirect.resourceto).
		{&out} http-newline http-newline.
		IF tt-redirect.log-fl THEN 
		 LogNote("Note"," " + P_filename + " was redirected to: " + tt-redirect.resourceto + " (Ref: " + http_referer + ")").
		RETURN.
	END. /*request needed to be redirected*/


	/* Now check to see if the resource is IP address limited. */
	FIND tt-iplimit
	 WHERE tt-iplimit.resourcefrom = SUBSTRING(p_FileName, 1, R-INDEX(p_FileName, ".":U) - 1,
	                                "CHARACTER":U)
	 NO-ERROR.
	IF AVAIL tt-iplimit THEN DO: /*it is limited by IP.  Check to see that it complies*/
		IF NOT CAN-DO(tt-iplimit.iplist,REMOTE_ADDR) THEN DO:
			LogNote("WARNING"," " + P_filename + " was requested by an unauthorized IP address: " + REMOTE_ADDR + ".  Their access was denied. ").
			ShowErrorScreen(tt-iplimit.errormessage).
			RETURN.
		END. /*IP DID NOT MATCH - SECURITY VIOLATION*/	
	END. /*IP-LIMITED*/

&IF "{&MULTI_DEV_PROPATHS}" = "YES" &THEN
    /*IF IN DEVELOPMENT MODE, ADJUST THE PROPATH FOR THIS HIT*/
    IF devCheck() THEN DEVCHECK: DO:
        ASSIGN v-devuser = GET-VALUE("devuser").
        FIND FIRST tt-developer
         WHERE tt-developer.devname = v-devuser
         NO-ERROR.
        IF NOT AVAIL tt-developer THEN LEAVE DEVCHECK.
        ELSE DO:
            ASSIGN
             v-realpropath = propath
             v-propathreset = true
             propath = tt-developer.mypropath + "," + propath
            .
        END.
    END. /*DEVCHECK()*/
&ENDIF

/*So that workshop.html doesn't have to be compiled*/
IF p_FileName = "workshop" THEN ASSIGN p_fileName = "ffw/webtools/workshop.html".

/*******************************************************************/
/* At the end of this section, we should have a file spec in rSearchFile
   which can be run. */ 	
	/* Get File Extension and handle accordingly  */
	RUN adecomm/_osfext.p (INPUT p_FileName, OUTPUT rFileExt) NO-ERROR.
    IF CAN-DO(v-speedscripttypes,rFileExt) THEN DO:
        /*Find the highest one in the propath*/
        ASSIGN rSearchFile = SEARCH(p_filename).
        IF rSearchFile NE ?
         THEN DO: /*File was found*/
            /*Since the temp .r file may be in a different location, 
               we must check the webrunpath for the speedscript source file*/
        	ASSIGN FILE-INFO:FILE-NAME = rSearchFile.
        	IF NOT CAN-DO(v-WebRunPath,replace(FILE-INFO:FULL-PATHNAME,"~\","/")) THEN DO:
        		LogNote("Caution:"," " + replace(FILE-INFO:FULL-PATHNAME,"~\","/") + " was requested by " + remote_addr + " but was not in the WebRunPath of " + v-webRunPath + " and was rejected. (Ref: " + http_referer + ")").
        		ShowErrorScreen(SUBSTITUTE ("Unable to find Web object file '&1'",p_FileName )).  
        		RETURN.
        	END. /*NOT FOUND IN WebRunPath*/
            /*Now see if there's a .r sitting next to it*/
            ASSIGN rSearchFile = SEARCH(SUBSTRING(
             rSearchFile, 1, R-INDEX(rSearchFile, ".":U),
	         "CHARACTER":U) + "r":U).  /*Thanks, ST*/
            /*If .r not found, then COTF the file if possible.*/
            IF rSearchFile = ? AND v-htmlcompileonfly THEN DO:
    		    ASSIGN v-tempwfile = v-temprpath + "tmp_" + substring(entry(1,p_filename,"."),max(r-index(p_filename,"~/"),r-index(p_filename,"~\")) + 1,-1) + "_"
                &IF "{&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
                + MSNGRPID
                &ELSE
                + entry(3,WEB-CONTEXT:EXCLUSIVE-ID,":") 
                &ENDIF
                + "_" + string(time) /**/ .
    			ASSIGN v-compilermsg = htmlCompile(p_FileName,v-tempwfile).
    			IF v-compilermsg NE "OK" THEN DO:
    				logNote("Compiler",v-compilermsg + " ").
    				LogNote("Error",P_filename + " Cannot be run as a Web object (Ref: " + http_referer + ")").
    		    	ShowErrorScreen(v-compilermsg).
    				RETURN.
    			END. 
    			ELSE DO:
    				LogNote("Compiler",P_filename + " was compiled on the fly.").
    				ASSIGN
    				 /* P_filename = v-tempWfile + ".r" */ /* SES */
                     rSearchfile = v-tempWfile + ".r"
    				 v-compiledonfly = TRUE
    				.
    			END.
            END.
        END. /*File was found*/
        ELSE DO: /*File not found at all - Look for .r in propath*/
            ASSIGN rSearchFile = SEARCH(SUBSTRING(p_FileName, 1, R-INDEX(p_FileName, ".":U),
	         "CHARACTER":U) + "r":U).
        END. /* File not found at all - Looked for .r in propath*/
	END. /*Was an embedded speedscript object*/
    ELSE DO: /*run it as raw 4gl*/
        ASSIGN rSearchFile = p_Filename. /*Picks up any .r files on its own*/
    END. /*Run it as raw 4gl*/
/*********************************************************************/
    IF rSearchfile = ? THEN DO: /*Couldn't find squat!*/
        LogNote("Error",P_filename + " was not found. (Ref: " + http_referer + ")").
     	ShowErrorScreen("404: " + P_filename + " was not found.").
        RETURN.
    END.
	
/* We now know exactly which file we're needing to run*/
/*Make sure the file is in the propath - otherwise they can run any .p on your machine!*/
	
	RUN adecomm/_rsearch.p (INPUT rSearchFile, OUTPUT rSearchFile).
	IF rSearchFile ne ? THEN RUN webutil/_relname.p (INPUT rSearchFile, "MUST-BE-REL", OUTPUT rSearchFile).  
	IF rSearchFile = ? THEN DO:
		LogNote("Caution"," " + P_filename + " was requested by " + remote_addr + " but was not in the propath and was rejected. (Ref: " + http_referer + ")").
		ShowErrorScreen(SUBSTITUTE ("Unable to find Web object file '&1'",p_FileName )).  
		RETURN.
	END. /*NOT FOUND IN PROPATH*/
	
	/* Make sure that the object requested is in the WebRunPath, most importantly. */
	
	ASSIGN FILE-INFO:FILE-NAME = rSearchFile.
	IF NOT CAN-DO(v-WebRunPath,replace(FILE-INFO:FULL-PATHNAME,"~\","/")) THEN DO:
		LogNote("Caution:"," " + replace(FILE-INFO:FULL-PATHNAME,"~\","/") + " was requested by " + remote_addr + " but was not in the WebRunPath of " + v-webRunPath + " and was rejected. (Ref: " + http_referer + ")").
		ShowErrorScreen(SUBSTITUTE ("Unable to find Web object file '&1'",p_FileName )).  
		RETURN.
	END. /*NOT FOUND IN WebRunPath*/
	
	&IF "{&FFW_DBCHECK}" = "YES" &THEN
	/*Now check database connections prior to running. */
	FIND tt-objectdb
	 WHERE tt-objectdb.objectname = SUBSTRING(p_FileName, 1, R-INDEX(p_FileName, ".":U) - 1,
	                                "CHARACTER":U)
	 NO-ERROR.
	IF AVAIL tt-objectdb AND tt-objectdb.dbset ne "none" THEN DO: /*need to check connections*/

        &IF "{&FFW_DBDOWNLOCKCHECK}" = "YES" &THEN
        IF v-dbdownlocknow THEN DO:
            ShowErrorScreen(v-dbdownmessage).
            RETURN.    
        END. 
        &ENDIF
        
		FIND tt-dbset
		 WHERE tt-dbset.dbset = tt-objectdb.dbset
		 NO-ERROR.
		IF AVAIL tt-dbset THEN DO: /*there ARE databases this object needs to be connected to*/
			DO v-mydbcount = 1 to num-entries(tt-dbset.dblist): /*each database in the list*/
				RUN ffw/robust/ffdbcheck.p (entry(v-mydbcount,tt-dbset.dblist),OUTPUT v-thisconnected) NO-ERROR.
				IF NOT v-thisconnected THEN DO:
					LogNote("DBCheck",P_filename + " failed because " + entry(v-mydbcount,tt-dbset.dblist) + " was not connected. ").
					ShowErrorScreen("A required database was not connected.  The server is trying to correct the problem.  Please wait a few seconds and hit 'reload'.").
					/* DISCONNECT FROM THE MESSENGER, AND TELL THE BROKER THAT WE'RE LOCKED */
					WAIT-FOR WEB-NOTIFY OF DEFAULT-WINDOW PAUSE 2 EXCLUSIVE-WEB-USER.
					RUN Handle-DB-connect IN THIS-PROCEDURE(entry(v-mydbcount,tt-dbset.dblist)).
&IF "{&MULTI_DEV_PROPATHS}" = "YES" &THEN
					IF v-propathreset THEN propath = v-realpropath.
&ENDIF
					RETURN.
				END. /*NOT CONNECTED*/
			END. /*each database in the list*/
		END. /*database set was available*/
	END. /*object needed connection check*/
	&ENDIF	
	
	EXECUTE-BLOCK:   
	DO ON ERROR  UNDO EXECUTE-BLOCK, LEAVE EXECUTE-BLOCK  
	   ON ENDKEY UNDO EXECUTE-BLOCK, LEAVE EXECUTE-BLOCK
	   ON STOP   UNDO EXECUTE-BLOCK, LEAVE EXECUTE-BLOCK
	   ON QUIT                     , LEAVE EXECUTE-BLOCK:  
	    
        &IF "{&FFW_RUNLOGGING}" = "YES" &THEN
		IF v-runlogpath NE "" THEN DO: /*beginning runlog*/
		/*Log prior to beginning the run*/
		    OUTPUT STREAM runlog TO VALUE(v-runlogfilename) APPEND NO-ECHO.
			PUT STREAM runlog UNFORMATTED TODAY " - " STRING(TIME,"hh:mm:ss") " - " p_FileName " Started" SKIP.
			OUTPUT STREAM runlog CLOSE.
		END. /*Beginning runlog*/
		&ENDIF
          
		/*Log all runs of workshop*/
		IF p_FileName matches "workshop*" THEN 
		 LogNote("Note"," Workshop (" + P_filename + ") was run by " + remote_addr + " ").

        IF v-profilingOn THEN DO:
            RUN StartProfiling(v-profilerFileName) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN LogNote("Error","Unable to start profiling: " + ERROR-STATUS:GET-MESSAGE(1)).
        END.
         
		RUN VALUE(rSearchFile) /*PERSISTENT SET this-wo-hdl*/ NO-ERROR. 
		IF ERROR-STATUS:ERROR THEN DO:
	    	ShowErrorScreen(SUBSTITUTE ("Unable to run Web object '&1'",p_FileName)).
			LogNote("Error"," " + P_filename + " failed at run-time. Message: " + ERROR-STATUS:GET-MESSAGE(1) + ")").
            &IF "{&FFW_RUNLOGGING}" = "YES" &THEN
			IF v-runlogpath NE "" THEN DO: /* runlog error*/
			/*Log the run error*/
			    OUTPUT STREAM runlog TO VALUE(v-runlogfilename) APPEND NO-ECHO.
				PUT STREAM runlog UNFORMATTED TODAY " - " STRING(TIME,"hh:mm:ss") " - " p_FileName " Finished:Error" SKIP.
				OUTPUT STREAM runlog CLOSE.
			END. /*runlog error*/
            &ENDIF
		END. /* IF...ERROR... */
        
        IF v-profilingOn THEN RUN StopProfiling NO-ERROR.
        &IF "{&FFW_RUNLOGGING}" = "YES" &THEN
		IF v-runlogpath NE "" THEN DO: /* runlog after*/
		/*Log after the run*/
		    OUTPUT STREAM runlog TO VALUE(v-runlogfilename) APPEND NO-ECHO.
			PUT STREAM runlog UNFORMATTED TODAY " - " STRING(TIME,"hh:mm:ss") " - " p_FileName " Finished" SKIP.
			OUTPUT STREAM runlog CLOSE.
		END. /*runlog after*/
		&ENDIF
        
&IF "{&MULTI_DEV_PROPATHS}" = "YES" &THEN
        /*If we adjusted the propath for this user, then adjust it back */
        IF v-propathreset THEN propath = v-realpropath.
&ENDIF       
	END. /* EXECUTE-BLOCK: DO... */
    
    /* We're all stateless.  Delete the object. */
    /*
    IF VALID-HANDLE(this-wo-hdl) THEN RUN dispatch IN this-wo-hdl ("destroy":U) NO-ERROR.
	IF VALID-HANDLE(this-wo-hdl) THEN DELETE PROCEDURE this-wo-hdl.
    */
    
	/*IF .html was compiled on the fly, then delete the temporary .r file.*/
	IF v-compiledonfly THEN OS-DELETE VALUE(v-tempWfile + ".r").	
END PROCEDURE. /*run-web-object*/

&IF "{&FFW_DBCHECK}" = "YES" &THEN
PROCEDURE Handle-DB-Connect:
/*------------------------------------------------------------------------------
  Purpose:     Handle problem when a database is not connected. 
  Parameters:  p_databasename = the jname of the database that is not connected.
  Notes:       
------------------------------------------------------------------------------*/
	DEFINE INPUT PARAMETER p_databasename AS CHAR NO-UNDO.
	DEFINE VAR v-connectproc AS CHAR NO-UNDO.
	DEFINE VAR v-notifyproc AS CHAR NO-UNDO.
    DEFINE VAR v-reconnected AS CHAR NO-UNDO.
	
	FIND tt-db
	 WHERE tt-db.databasename = p_databasename
	 NO-ERROR.
	IF AVAIL tt-db THEN DO:
		IF tt-db.downsincedate = ? THEN ASSIGN
		 tt-db.downsincedate = TODAY
		 tt-db.downsincetime = TIME
		 tt-db.downperiods = tt-db.downperiods + 1
		.
		ASSIGN
		 v-connectproc = SEARCH(tt-db.connectproc)
		 v-notifyproc = SEARCH(tt-db.notifyproc)
		.
		IF v-connectproc NE ? AND v-connectproc ne "" THEN DO:
			ASSIGN tt-db.reconnectattempts = tt-db.reconnectattempts + 1.
			RUN VALUE(v-connectproc) NO-ERROR.
			IF ERROR-STATUS:ERROR
			 THEN LogNote("DBCheck"," Unable to reconnect " + P_databasename + ", because it either the connection procedure could not be found, or there was an error running it!)").
			IF CONNECTED(p_databasename) THEN DO:
			 	LogNote("DBCheck"," " + P_databasename + " was reconnected on try #" + string(tt-db.reconnectattempts) + ".)").
				ASSIGN
				 tt-db.downsincedate = ?
				 tt-db.downsincetime = ?
				 tt-db.reconnectattempts = 0
				.
			END. /*reconnected successfully*/
			ELSE DO: /*UNABLE TO RECONNECT*/
			 	LogNote("DBCheck"," " + P_databasename + " could not be reconnected on try #" + string(tt-db.reconnectattempts) + ".)").
				/* be nice and let somebody know about it */
				IF tt-db.lastnotifydate = ?
				 OR TODAY > tt-db.lastnotifydate
				 OR (TODAY = tt-db.lastnotifydate AND TIME > tt-db.lastnotifytime + 600)
				 THEN DO:
					IF v-notifyproc NE ? THEN DO:
						RUN VALUE(v-notifyproc) NO-ERROR.
				 		IF ERROR-STATUS:ERROR
						 THEN LogNote("DBCheck"," Unable to notify anyone that " + P_databasename + " is down, because it either the procedure could not be found, or there was an error running it!)").
						ELSE ASSIGN 
						 tt-db.lastnotifydate = TODAY
						 tt-db.lastnotifytime = TIME
						.
					END.
					ELSE DO: /*unable to notify anyone!*/
						RUN ffw/robust/dbnotify.p (p_databasename) NO-ERROR.
				 		IF ERROR-STATUS:ERROR
						 THEN LogNote("DBCheck"," Unable to notify anyone that " + P_databasename + " is down, because it either the procedure could not be found, or there was an error running it!)").
						ELSE ASSIGN 
						 tt-db.lastnotifydate = TODAY
						 tt-db.lastnotifytime = TIME
						.
				 		LogNote("DBCheck"," Unable to notify anyone that " + P_databasename + " is down, because it either has no notification procedure defined in the .ini file, or the procedure could not be found!)").
						/* ||| consider some last-ditch attempt to raise hell here.  Perhaps beep the PC speaker repeatedly? */
					END. /*unable to notify anyone!*/
				END. /*first time, or 10 minutes had passed*/
			END. /*UNABLE TO RECONNECT*/
		END. /*THERE WAS A VALID RECONNECTION PROCEDURE*/
		ELSE DO: /*THERE WAS NO VALID RECONNECTION PROCEDURE*/
            IF tt-db.filename ne "" 
             OR tt-db.hostname ne ""
             OR tt-db.servicename ne ""
             THEN DO:
			    ASSIGN tt-db.reconnectattempts = tt-db.reconnectattempts + 1.
                RUN ffw/robust/dbconnect.p (p_databasename, OUTPUT v-reconnected) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN LogNote("DBCheck","dbconnect.p could not be run.").


    			IF CONNECTED(p_databasename) THEN DO:
    			 	LogNote("DBCheck"," " + P_databasename + " was reconnected on try #" + string(tt-db.reconnectattempts) + ".)").
    				ASSIGN
    				 tt-db.downsincedate = ?
    				 tt-db.downsincetime = ?
    				 tt-db.reconnectattempts = 0
    				.
    			END. /*reconnected successfully*/
    			ELSE DO: /*UNABLE TO RECONNECT*/
    			 	LogNote("DBCheck"," " + P_databasename + " could not be reconnected on try #" + string(tt-db.reconnectattempts) + ".)").
    				/* be nice and let somebody know about it */
    				IF tt-db.lastnotifydate = ?
    				 OR TODAY > tt-db.lastnotifydate
    				 OR (TODAY = tt-db.lastnotifydate AND TIME > tt-db.lastnotifytime + 600)
    				 THEN DO:
    					IF v-notifyproc NE ? THEN DO:
    						RUN VALUE(v-notifyproc) NO-ERROR.
    				 		IF ERROR-STATUS:ERROR
    						 THEN LogNote("DBCheck"," Unable to notify anyone that " + P_databasename + " is down, because it either the procedure could not be found, or there was an error running it!)").
    						ELSE ASSIGN 
    						 tt-db.lastnotifydate = TODAY
    						 tt-db.lastnotifytime = TIME
    						.
    					END.
    					ELSE DO: /*unable to notify anyone!*/
    						RUN ffw/robust/dbnotify.p (p_databasename) NO-ERROR.
    				 		IF ERROR-STATUS:ERROR
    						 THEN LogNote("DBCheck"," Unable to notify anyone that " + P_databasename + " is down, because it either the procedure could not be found, or there was an error running it!)").
    						ELSE ASSIGN 
    						 tt-db.lastnotifydate = TODAY
    						 tt-db.lastnotifytime = TIME
    						.
    				 		LogNote("DBCheck"," Unable to notify anyone that " + P_databasename + " is down, because it either has no notification procedure defined in the .ini file, or the procedure could not be found!)").
    						/* ||| consider some last-ditch attempt to raise hell here.  Perhaps beep the PC speaker repeatedly? */
    					END. /*unable to notify anyone!*/
    				END. /*first time, or 10 minutes had passed*/
    			END. /*UNABLE TO RECONNECT*/




            END. /*Used alternate connection method*/
            ELSE DO: /*no reconnect proc and no alt method*/
	 		    LogNote("DBCheck"," Unable to attempt a reconnect to " + P_databasename + ", because it either has no reconnection procedure defined in the .ini file, or the procedure could not be found!)").
            END. /*no reconnect proc, and no alt method*/
		END. /*NO VALID RECONNECT PROCEDURE*/	
	END. /*TT-DB AVAIL*/
	ELSE DO:
 		LogNote("DBCheck"," Unable to attempt a reconnect to " + P_databasename + ", because it is not defined in the .ini file!)").
	END.
END PROCEDURE.
&ENDIF

PROCEDURE Check-Default-Databases:
	&IF "{&FFW_DBCHECK}" = "YES" &THEN
	DEFINE VAR v-mydbcount AS INTEGER NO-UNDO.
	DEFINE VAR v-thisconnected AS LOGICAL NO-UNDO.
&IF "{&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
    /* Not sure how to handle this for non-webspeed.  Send suggestions to ffw@peg.com*/
&ELSE
	/* TELL THE BROKER THAT WE'RE LOCKED, SO WE CAN HAVE A MINUTE TO 
		CHECK CONNECTIONS WITHOUT GETTING A NEW REQUEST.              */
	WAIT-FOR WEB-NOTIFY OF DEFAULT-WINDOW PAUSE 2 EXCLUSIVE-WEB-USER.
&ENDIF
	FIND tt-dbset
	 WHERE tt-dbset.dbset = "default"
	 NO-ERROR.
	IF AVAIL tt-dbset THEN DO: /*there a default set of databases*/
		DO v-mydbcount = 1 to num-entries(tt-dbset.dblist): /*each database in the list*/
			RUN ffw/robust/ffdbcheck.p (entry(v-mydbcount,tt-dbset.dblist),OUTPUT v-thisconnected) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                LogNote("DBCheck","Could not run proactive database check: " + ERROR-STATUS:GET-MESSAGE(1)).
            END.
			IF NOT v-thisconnected THEN DO:
				LogNote("DBCheck"," Proactive check says " + entry(v-mydbcount,tt-dbset.dblist) + " is not connected. Reconnecting...").
				RUN Handle-DB-connect IN THIS-PROCEDURE(entry(v-mydbcount,tt-dbset.dblist)).
			END. /*NOT CONNECTED*/
		END. /*each database in the list*/
	END. /*avail tt-dbset*/
	&ENDIF
END PROCEDURE.

/****************************************************************************
Function: output-content-type
Description: Sets and outputs the MIME Content-Type header followed by a
  blank line.  If the header was already output, no action is taken.
****************************************************************************/
FUNCTION output-content-type RETURNS LOGICAL
  (INPUT p_type AS CHARACTER) :
  IF output-content-type = "" THEN DO:
    ASSIGN output-content-type = (IF p_type = "" THEN ? ELSE p_type).
      
    IF output-content-type <> ? THEN
       output-http-header ("Content-Type":U, output-content-type).
    output-http-header ("", "").  /* blank line */

    /* Output an HTML "... generated by ..." comment at the top of every
       output page but only if HTML is being output.  BEGINS must be used
       to allow for options to the MIME type e.g.
         "text/html; charset=iso-8859-2" */
    &IF DEFINED(ALL_PAGE_HEADER) &THEN 
    IF "&ALL_PAGE_HEADER" > "" 
      AND (output-content-type BEGINS "text/html":U OR
       output-content-type BEGINS "text/x-server-parsed-html":U) THEN
        {&OUT} "{&ALL_PAGE_HEADER}" "~n~n":U.
    &ENDIF
        .
    /* If output-content-type is not ?, then a Content-Type header was
       output so return TRUE. */
    RETURN (output-content-type <> ?).
  END.

END FUNCTION. /* output-content-type */

PROCEDURE show-errors:
	/*empty stub - no longer needed, but still referenced by admweb.i, which we need*/
END PROCEDURE. /*show-errors*/

FUNCTION get-config RETURNS CHAR(
 INPUT myvarname AS CHAR
):
&IF "{&WEB_CONTEXT_EXCLUDE}" = "YES" &THEN
    RETURN getAgentSetting(myvarname).
&ELSE
    RETURN WEB-CONTEXT:GET-CONFIG-VALUE(myvarname).
&ENDIF

END FUNCTION.

PROCEDURE user-startup:
    DEFINE VAR v-startupproc AS CHAR NO-UNDO.
    ASSIGN v-startupproc = GetAgentSetting("StartupProc").
    IF v-startupproc ne "" THEN DO:
        FILE-INFO:FILE-NAME = SEARCH(v-startupproc).
        IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
            LogNote("Error","Startup procedure: " + v-startupproc + " could not be found.  Check the path in your .ini file, and restart the agents.").
            RETURN.
        END.
        RUN VALUE(v-startupproc) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            LogNote("Error","Startup procedure: " + v-startupproc + " failed with this error: " + ERROR-STATUS:GET-MESSAGE(1)).
            RETURN.
        END.
        ELSE DO:
            LogNote("Note","Startup procedure: " + v-startupproc + " was run successfully.").
        END.
    END.
    RETURN.
END PROCEDURE. /*user-startup*/


