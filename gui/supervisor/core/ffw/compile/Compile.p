/*------------------------------------------------------------------------
  File:        Compile.p
  Author:      RJM 
  Description: Utility used to compile Progress/WebSpeed programs.
------------------------------------------------------------------------*/
{src/web/method/cgidefs.i NEW}

/* define compile configuration file tags */
&scoped-define GLOBALOBJECT           GLOBALOBJECT
&scoped-define COMPILEOBJECT          COMPILEOBJECT
&scoped-define COMPILEOBJECTLIST      compileObjectList
&scoped-define OBJECTDESCRIPTION      objectDescription
&scoped-define COMPILEDIRECTORY       compileDirectory
&scoped-define SAVETODIRECTORY        saveToDirectory
&scoped-define DATABASELIST           databaseList
&scoped-define PROPATHFILE            propathFile
&scoped-define LOGFILEDIRECTORY       logFileDirectory
&scoped-define STARTPROGRESSCODE      startProgressCode
&scoped-define ENDPROGRESSCODE        endProgressCode
&scoped-define STARTOSSCRIPT          startOSScript
&scoped-define ENDOSSCRIPT            endOSScript
&scoped-define EXCLUDEDIRECTORYLIST   excludeDirectoryList
&scoped-define EXCLUDEFILENAMEFILE    excludeFilenameFile
&scoped-define FILEEXTENSIONLIST      fileExtensionList
&scoped-define COMPILEOPTIONS         compileOptions

&scoped-define NonOSSlash             (if opsys = "UNIX" then "~~~\" else "~~~/")
&scoped-define OSSlash                (if opsys = "UNIX" then "~~~/" else "~~~\")

def stream InputFile.   
def stream OutputFile.  
def stream GCompileLog.
def stream CompileLog.

def new global shared var hCompileProcedure as handle no-undo.

/* forward declare UDF - access ttCompileObject tag values */
function get-compile-object-tag returns char
    (input ipTagName as char, input ipTagType as char) forward.

/* forward declare UDF - parse parameter values from a connection string */
function get-connection-parameter returns char
    (input ipParameterName as char, input ipConnectionString as char) forward.

/* forward declare UDF - write messages to .log file */
function write-compile-log returns char
    (input ipLogMessageType as char, input ipLogMessage as char) forward.

/* forward declare UDF - write messages to global .log file */
function write-global-compile-log returns char
    (input ipLogMessageType as char, input ipLogMessage as char) forward.

/* forward declare UDF - decide where to write log messages */
function decide-write-log returns char
    (input ipLogType as char, ipLogMessageType as char, input ipLogMessage as char) forward.

/* TT used to store values for this compililation */
def temp-table ttCompileObject no-undo
    field ttTagName                 as char
    field ttTagType                 as char
    field ttTagValue                as char
    field ttTagNumber               as int
    index ttTagName                 is primary ttTagName ttTagType
    index ttTagNumber               ttTagNumber.

def buffer bttCompileObject for ttCompileObject.

def temp-table ttCompileFile no-undo
    field ttFilename                as char
    field ttFileFullPath            as char
    field ttFileAttribute           as char
    field ttFileNoExtension         as char
    field ttFileExtension           as char
    field ttFileDirectoryLevel      as int
    field ttFileParentDirectory     as char
    field ttFileSaveToDirectory     as char
    index ttFilename                is primary ttFileAttribute ttFileDirectoryLevel ttFilename.

def temp-table ttPropathVariable no-undo
    field ttVariableName            as char
    field ttVariableValue           as char
    field ttVariableLength          as int
    index ttVariableLength          is primary ttVariableLength descending
    index ttVariableName            ttVariableName.

def var vHoldPropath                as char no-undo.
def var vGlobalDate                 as date no-undo.
def var vGlobalTime                 as int no-undo.
def var vHoldDate                   as date no-undo.
def var vHoldTime                   as int no-undo.
def var vTime                       as int no-undo.
def var vCompileDuration            as int no-undo.
def var vCompileConfigurationFile   as char no-undo.
def var vLogFileFullPath            as char no-undo.
def var vDatabaseList               as char no-undo.
def var vDatabaseName               as char no-undo.
def var vDatabaseNumber             as int no-undo.
def var vInvalidPropathDirectory    as log no-undo.
def var vCompileObjectList          as char no-undo.
def var vErrorNumber                as int no-undo.
def var vListEntry                  as int no-undo.


/* store propath before beginning compile(s) */
vHoldPropath = propath.
    
/* set handle var so CompileFilesList.p can access IPs within this procedure*/
hCompileProcedure = this-procedure.


/* open stream to global summary log file (global log gets written to working directory */
run create-log-file
    (input "",
     input "Compile",
     output vLogFileFullPath).
/* if user can't write to global log file, leave now */
if return-value = "ERROR" then
    return.

/* open stream to global log file */
output stream GCompileLog to value(vLogFileFullPath).


assign
    vGlobalDate = today.
    vGlobalTime = time.
    
/* write global log file headers */
write-global-compile-log("SUMMARY",fill("-",75)).
write-global-compile-log("SUMMARY","   DESCRIPTION: COMPILATION SUMMARY").
write-global-compile-log("SUMMARY","    START DATE: " + string(vGlobalDate,"99/99/9999")).
write-global-compile-log("SUMMARY","    START TIME: " + string(vGlobalTime,"HH:MM:SSam")).
write-global-compile-log("SUMMARY",fill("-",75)).

                                                           
/* load temp-table with configuration file values used for this compile */
vCompileConfigurationFile = "compile.cfg".

run load-configuration-file
    (input vCompileConfigurationFile).    
/* if a value is returned, an error occured so leave now */
if return-value <> "" then do:
    write-global-compile-log("SUMMARY",return-value).
    return.
end.            

                                                           
vCompileObjectList = get-compile-object-tag("{&GLOBALOBJECT}","{&COMPILEOBJECTLIST}").

/* leave now if no compile objects listed to compile */
if vCompileObjectList = "" then do:
    write-global-compile-log("SUMMARY","  ***ABORTING: No Compile Objects listed in {&COMPILEOBJECTLIST}").
    return.
end.  /* if vCompileObjectList = "" */

/* output any invalid compile objects listed in {&COMPILEOBJECTLIST} to global log */
do vListEntry = 1 to num-entries(vCompileObjectList):
    if vCompileObjectList <> "ALL" and 
       not can-find (first ttCompileObject
        where
            ttCompileObject.ttTagName   = entry(vListEntry,vCompileObjectList) and
            ttCompileObject.ttTagType   = "{&COMPILEOBJECT}") then do:
        
        write-global-compile-log("SUMMARY","  ***WARNING: Invalid Compile Object Specified in {&COMPILEOBJECTLIST} (Entry " +  
                                 string(vListEntry) + ": " + 
                                 (if entry(vListEntry,vCompileObjectList) <> "" 
                                    then entry(vListEntry,vCompileObjectList)  
                                    else "BLANK ENTRY") + 
                                 ")").
    end. /* if an invalic compile object is found */
end. /* output any invalid compile objects listed in {&COMPILEOBJECTLIST} to global log */

              
/* run global OS script before any compile objects start compiling */
run run-os-script
    (input get-compile-object-tag("{&GLOBALOBJECT}","{&STARTOSSCRIPT}"),
     input "START",
     input "{&GLOBALOBJECT}").


/* run global Progress code before any compile objects start compiling */
run run-progress-code
    (input get-compile-object-tag("{&GLOBALOBJECT}","{&STARTPROGRESSCODE}"),
     input "START",
     input "{&GLOBALOBJECT}").

              
/* cycle thru all compile object tag names and execute compile */ 
for each ttCompileObject 
    where ttCompileObject.ttTagType = "{&COMPILEOBJECT}"
    by ttCompileObject.ttTagNumber:

    /* skip the GlobalObject, it is not an object that gets compiled */
    if ttCompileObject.ttTagValue = "{&GLOBALOBJECT}" then 
        next.

    /* only compile this object if it's included in {&COMPILEOBJECTLIST} */
    if vCompileObjectList <> "ALL" and lookup(ttCompileObject.ttTagValue,vCompileObjectList) = 0 then
        next.

    /* write summary info to global log */
    write-global-compile-log("SPACE","").
    write-global-compile-log("SUMMARY","COMPILE OBJECT: " + 
                             ttCompileObject.ttTagValue + " (" +
                             get-compile-object-tag(ttCompileObject.ttTagValue,"{&OBJECTDESCRIPTION}") + ")").


    /* determine log file path/filename for this compile object, if compile object 
       does not specify a directory, default to global object log directory */
    run create-log-file
        (input if get-compile-object-tag(ttCompileObject.ttTagValue,"{&LOGFILEDIRECTORY}") <> "" 
               then get-compile-object-tag(ttCompileObject.ttTagValue,"{&LOGFILEDIRECTORY}") 
               else get-compile-object-tag("{&GLOBALOBJECT}","{&LOGFILEDIRECTORY}"),
         input ttCompileObject.ttTagValue,
         output vLogFileFullPath).
    /* if user can't write to a log file for some reason, go to next compile object */
    if return-value = "ERROR" then
        next.

    /* open stream to compile .log file */
    output stream CompileLog to value(vLogFileFullPath).

    file-info:file-name = vLogFileFullPath.
    write-global-compile-log("SUMMARY","LOG FILE: " + file-info:full-pathname).


    assign
        vHoldDate   = today.
        vHoldTime   = time.
        
    /* write .log file headers */
    write-compile-log("HEADER",fill("-",75)).
    write-compile-log("HEADER","COMPILE OBJECT: " + ttCompileObject.ttTagValue).
    write-compile-log("HEADER","   DESCRIPTION: " + get-compile-object-tag(ttCompileObject.ttTagValue,"{&OBJECTDESCRIPTION}")).
    write-compile-log("HEADER","    START DATE: " + string(vHoldDate,"99/99/9999")).
    write-compile-log("HEADER","    START TIME: " + string(vHoldTime,"HH:MM:SSam")).
    write-compile-log("HEADER",fill("-",75)).


    /* write summary info to global log */
    write-global-compile-log("SUMMARY","START TIME: " + string(vHoldTime,"HH:MM:SSam")).


    write-compile-log("SPACE","").            


    /* run OS script before this compile object starts compiling */
    run run-os-script
        (input get-compile-object-tag(ttCompileObject.ttTagValue,"{&STARTOSSCRIPT}"),
         input "START",
         input "{&COMPILEOBJECT}").


    write-compile-log("SPACE","").


    /* disconnect all DBs */
    repeat vDatabaseNumber = num-dbs to 1 by -1:
        write-compile-log("DBDISCONNECT","Disconnecting Database: " + ldbname(vDatabaseNumber)).
        disconnect value(ldbname(vDatabaseNumber)).
    end. /* repeat */


    write-compile-log("SPACE","").

    
    /* connect to necessary DBs for this compile object */
    vDatabaseList = get-compile-object-tag(ttCompileObject.ttTagValue,"{&DATABASELIST}").

    if vDatabaseList = "" then
        write-compile-log("DBLIST","Connecting To Database: No Database parameters specified in {&DATABASELIST}").            

    do vListEntry = 1 to num-entries(vDatabaseList):
        /* parse out DB name from connection string, look for -ld first, else look for -db */
        vDatabaseName = get-connection-parameter("-ld",entry(vListEntry,vDatabaseList)).
        if vDatabaseName = "" then 
            vDatabaseName = get-connection-parameter("-db",entry(vListEntry,vDatabaseList)).

        /* if on DB name is specified */
        if vDatabaseName = "" then do:
            write-compile-log("DBERROR","Connecting To Database(" + string(vListEntry) + 
                              "): ***ERROR: No Database Name Specified [" + entry(vListEntry,vDatabaseList) + "]").            
            next.
        end. /* if vDatabaseName = "" */


        /* write to log file to display that db connection is being attempted */
        write-compile-log("DBCONNECT}","Connecting To Database(" + string(vListEntry) + "): " + 
                          vDatabaseName + " (" + entry(vListEntry,vDatabaseList) + ")").            


        /* connect to the DB */
        connect value(entry(vListEntry,vDatabaseList))
            no-error.   
        /* if DB connection errors occur */
        if error-status:error then do vErrorNumber = 1 to error-status:num-messages:
            write-compile-log("DBERROR","  ***ERROR: " + error-status:get-message(vErrorNumber)).            
        end. /* do vErrorNumber... */
        else if not connected(vDatabaseName) then
            write-compile-log("DBERROR","  ***ERROR: Database Connection Failed").            
    end. /* connect to DBs */


    write-compile-log("SPACE","").


    /* load file containing propath entries for this compile object (if a propath file is specified) */
    if get-compile-object-tag(ttCompileObject.ttTagValue,"{&PROPATHFILE}") <> "" then do:
        write-compile-log("PP}","Searching For Propath File: " + 
                           get-compile-object-tag(ttCompileObject.ttTagValue,"{&PROPATHFILE}")).        

        run load-propath-directories
            (input get-compile-object-tag(ttCompileObject.ttTagValue,"{&PROPATHFILE}"),
             input vHoldPropath).
        /* if a value is returned, an error occured */
        if return-value <> "" then 
            write-compile-log("PPERROR",return-value).            
    end. /* load file containing propath entries.. */
    else
        write-compile-log("PP}","Searching For Propath File: No File Specified").        

                                  
    write-compile-log("SPACE","").


    /* display all entries in propath to log file */
    write-compile-log("PP","Current Propath:").
    do vListEntry = 1 to num-entries(propath):
        /* decide if this is a valid directory specified in propath */
        file-info:file-name = entry(vListEntry,propath).
        if file-info:file-type = ? OR 
           (index(file-info:file-type,"D") = 0 and index(file-info:file-type,"F") = 0) then 
            vInvalidPropathDirectory = true.
        else
            vInvalidPropathDirectory = false.
                    
        write-compile-log("PP","     " + entry(vListEntry,propath) + 
                          (if entry(vListEntry,propath) = "." 
                             then " (" + file-info:full-pathname + ")" 
                             else "") +          
                          (if vInvalidPropathDirectory 
                              then " ***INVALID DIRECTORY***" 
                              else "")).            
    end. /* display propath */


    write-compile-log("SPACE","").            


    /* run Progress code before this compile object starts compiling */
    run run-progress-code
        (input get-compile-object-tag(ttCompileObject.ttTagValue,"{&STARTPROGRESSCODE}"),
         input "START",
         input ttCompileObject.ttTagValue).


    /* create a list of files to compile, then compile them) */
    run compile-files
        (input vLogFileFullPath).


    write-compile-log("SPACE","").            


    /* run Progress code after this compile object ends compiling */
    run run-progress-code
        (input get-compile-object-tag(ttCompileObject.ttTagValue,"{&ENDPROGRESSCODE}"),
         input "END",
         input ttCompileObject.ttTagValue).


    write-compile-log("SPACE","").            


    /* run OS script after this compile object ends compiling */
    run run-os-script
        (input get-compile-object-tag(ttCompileObject.ttTagValue,"{&ENDOSSCRIPT}"),
         input "END",
         input "{&COMPILEOBJECT}").


    vTime = time.
    if vHoldDate <> today 
        then vCompileDuration  = (86400 - vHoldTime) + vTime. 
        else vCompileDuration  = vTime - vHoldTime.

    /* write summary info to log file */
    write-compile-log("SPACE","").
    write-compile-log("HEADER",fill("-",75)).
    write-compile-log("HEADER","        END TIME: " + string(vTime,"HH:MM:SSam")).
    write-compile-log("HEADER","COMPILE DURATION: " + string(vCompileDuration,"HH:MM:SS")).
    write-compile-log("HEADER",fill("-",75)).

    /* write summary info to global log file */
    write-global-compile-log("SUMMARY","END TIME: " + string(vTime,"HH:MM:SSam")).
    write-global-compile-log("SUMMARY","COMPILE DURATION: " + string(vCompileDuration,"HH:MM:SS")).

    /* close stream to compile .log file */
    output stream CompileLog close.
end. /* for each ttCompileObject */


write-global-compile-log("SPACE","").


/* run global Progress code after all compile objects have ended compiling */
run run-progress-code
    (input get-compile-object-tag("{&GLOBALOBJECT}","{&ENDPROGRESSCODE}"),
     input "END",
     input "{&GLOBALOBJECT}").

/* run global OS script after all compile objects have ended compiling */
run run-os-script
    (input get-compile-object-tag("{&GLOBALOBJECT}","{&ENDOSSCRIPT}"),
     input "END",
     input "{&GLOBALOBJECT}").


vTime = time.
if vGlobalDate <> today 
    then vCompileDuration  = (86400 - vGlobalTime) + vTime. 
    else vCompileDuration  = vTime - vGlobalTime.

/* write summary info to global log file */
write-global-compile-log("SPACE","").
write-global-compile-log("SUMMARY",fill("-",75)).
write-global-compile-log("SUMMARY","        END TIME: " + string(vTime,"HH:MM:SSam")).
write-global-compile-log("SUMMARY","COMPILE DURATION: " + string(vCompileDuration,"HH:MM:SS")).
write-global-compile-log("SUMMARY",fill("-",75)).

/* close stream to global compile .log file */
output stream GCompileLog close.


/* restore propath to its original value */
propath = vHoldPropath.


/* dump contents of CompileFile TT for debugging purposes */
/*
run dump-compile-object-tt.
*/


procedure load-configuration-file:
/*------------------------------------------------------------------------
  Purpose:      Load a temp table with values from the compile configuration
                file. This temp table will be referenced when values from
                configuration are needed are needed
  Parameters:   Compile configuration filename
------------------------------------------------------------------------*/
def input param ipConfigurationFile as char no-undo. 

def var vFileLine                   as char no-undo.
def var vFileLineNumber             as int no-undo.
def var vTagName                    as char no-undo.
def var vTagType                    as char no-undo.
def var vTagValue                   as char no-undo.
def var vTagNumber                  as int no-undo.        

                   
    /* search for file, if not found then return error message */
    if search(ipConfigurationFile) <> ? then
        ipConfigurationFile = search(ipConfigurationFile).
    else 
        return "ABORTING COMPILE: " + ipConfigurationFile + " not found".

    /* display fullpath of configuration file in global log */
    file-info:file-name = ipConfigurationFile.
    write-global-compile-log("SPACE","").  
    write-global-compile-log("SUMMARY","CONFIGURATION FILE: " + file-info:full-pathname).  


    /* ensure the last char in this file is CR/NL */
    run check-end-of-file
        (input ipConfigurationFile).
    

    input stream InputFile from value(ipConfigurationFile) no-echo.
    repeat:
        import stream InputFile unformatted vFileLine.

        /* trim spaces from FileLine and increment line counter */
        assign
            vFileLine       = trim(vFileLine)
            vFileLineNumber = vFileLineNumber + 1.

        /* skip comments and blank lines */
        if substring(vFileLine,1,1) = "#" or vFileLine = "" then
            next.

        /* parse out compile object name (compile object names begin with "[") and 
           attribute tags ("=" is attribute delimiter) */
        if substring(vFileLine,1,1) = "[" OR
           (vFileLine matches "*=*" AND entry(1,vFileLine,"=") <> "")  then do:

            vTagNumber = vTagNumber + 1.

            /* if this is a compile object name */
            if substring(vFileLine,1,1) = "[" then do:
                /* strip out brackets from compile object names */
                vTagValue = replace(replace(vFileLine,"[",""),"]",""). 

                assign
                    vTagType    = "{&COMPILEOBJECT}"
                    vTagName    = vTagValue. 

                /* ensure compile object name has been specified */
                if vTagValue = "" then
                    return "ABORTING COMPILE: Blank Compile Object at line# " + 
                           string(vFileLineNumber).
                     
                /* compile object names are unique, ensure there are no duplicates */
                if can-find (first ttCompileObject
                    where ttCompileObject.ttTagName = vTagName 
                      and ttCompileObject.ttTagType = "{&COMPILEOBJECT}") then do:
                
                    return "ABORTING COMPILE: Duplicate Compile Object [" + 
                           vTagName + "] at line# " + string(vFileLineNumber).
                end. /* if can-find (first ttCompileObject... */
            end. /* if this is a compile object name */
            /* else this is attribute of current compile object */
            else do:
                assign
                    vTagType    = trim(entry(1,vFileLine,"="))
                    vTagValue   = trim(entry(2,vFileLine,"=")).
                 
                /* compile object attibutes are unique per compile object, ensure there are no duplicates */
                if can-find (first ttCompileObject
                    where
                        ttCompileObject.ttTagName  = vTagName and
                        ttCompileObject.ttTagType  = vTagType) then do:
                
                    return "ABORTING COMPILE: Duplicate Compile Attribute (" + 
                           vTagType + ") at line# " + string(vFileLineNumber).
                end. /* if can-find (first ttCompileObject */
            end. /* else this is attribute of current compile object */


            /* trim attribute values that are "lists" values (list delimter is comma) */
            if lookup(vTagType,"{&COMPILEOBJECTLIST},{&EXCLUDEDIRECTORYLIST},{&COMPILEOPTIONS},{&FILEEXTENSIONLIST}") <> 0 then do vListEntry = 1 to num-entries(vTagValue):
                if entry(vListEntry,vTagValue) <> "" then
                    vTagValue = replace(vTagValue,entry(vListEntry,vTagValue),trim(entry(vListEntry,vTagValue))).
            end. /* trim entries */

                
            /* create a TT record, replace all slashes with appropriate slash for OS */
            create ttCompileObject.
            assign  
                ttCompileObject.ttTagName       = vTagName
                ttCompileObject.ttTagType       = vTagType     
                ttCompileObject.ttTagValue      = replace(vTagValue,{&NonOSSlash},{&OSSlash})
                ttCompileObject.ttTagNumber     = vTagNumber.
        end. /* parse out compile object name OR attribute tags */
    end. /* repeat */
    input stream InputFile close.

    /* return blank to signify no errors occured */
    return "".
end procedure. /* load-configuration-file */
                                

procedure load-propath-directories:
/*------------------------------------------------------------------------
  Purpose:      Used to dynamically set Propath for each compile object
                by reading propath entries from a file
  Parameters:   (1) Filename containing propath entries
------------------------------------------------------------------------*/
def input param ipPropathFilename   as char no-undo.
def input param ipInitialPropath    as char no-undo.

def var vFileLine                   as char no-undo.
def var vListEntry                  as int no-undo.
def var vPropathDirectory           as char no-undo.
def var vCompileObjectPropath       as char no-undo.

    /* search for propath file, if not found then return error message */
    if search(ipPropathFilename) <> ? then
        ipPropathFilename = search(ipPropathFilename).
    else 
        return "  ***ERROR: File Not Found: " + ipPropathFilename + " not found".
                                             
    /* ensure the last char in this file is CR/NL */
    run check-end-of-file
        (input ipPropathFilename).
                                             

    /* delete all propath variable TT records */
    for each ttPropathVariable:
        delete ttPropathVariable.
    end.


    input stream InputFile from value(ipPropathFilename) no-echo.
    repeat:
        import stream InputFile unformatted vFileLine.

        /* trim spaces from FileLine */
        vFileLine = trim(vFileLine).

        /* skip comments and blank lines */
        if substring(vFileLine,1,1) = "#" or vFileLine = "" then
            next.

        /* create TT records for any variables defined in propath file.  Only
           one variable may be defined per line. A variable must beging with "$".
            A variable might look like this $MyPropathVariable=c:\temp  */
        if vFileLine matches "*=*" then do:
            /* ensure a variable name is defined before the "=" */
            if trim(entry(1,vFileLine,"=")) <> "" then do:
            
                find first ttPropathVariable 
                    where
                        ttPropathVariable.ttVariableName = trim(entry(1,vFileLine,"="))
                    no-error.
                if not avail ttPropathVariable then
                    create ttPropathVariable.
                
                assign
                    ttPropathVariable.ttVariableName    = trim(entry(1,vFileLine,"="))
                    ttPropathVariable.ttVariableValue   = trim(entry(2,vFileLine,"="))
                    ttPropathVariable.ttVariableLength  = length(trim(entry(1,vFileLine,"="))).                                                                  
            end. /* if entry <> "" */

            /* read next line of propath file */
            next.                
        end. /* if trim(entry(1,vFileLine,"=")) <> "" */


        /* handle multiple propath entries per line (delimited by commas) if necessary */
        do vListEntry = 1 to num-entries(vFileLine):
            /* skip black entries on a comma delimited line */
            if trim(entry(vListEntry,vFileLine)) = "" then
                next.
            
            /* get value of this propath directory */
            vPropathDirectory = trim(entry(vListEntry,vFileLine)).

            /* substitute user defined variables into into propath entry if necessary.
               Cycle through each propath variable with the longest variable names first */
            for each ttPropathVariable
                by ttPropathVariable.ttVariableLength 
                by ttPropathVariable.ttVariableName:
        
                vPropathDirectory = replace(vPropathDirectory,"$" + 
                                            ttPropathVariable.ttVariableName,ttPropathVariable.ttVariableValue).
            end. /* for each ttPropathVariable */

            /* build new propath, add default dir "." to beginning of propath, replace slashes depending on OS */
            vCompileObjectPropath = vCompileObjectPropath + 
                (if vCompileObjectPropath = "" then ".," else ",") + 
                replace(vPropathDirectory,{&NonOSSlash},{&OSSlash}).
        end. /* do vListEntry = 1 to num-entries(vFileLine) */
    end. /* repeat */
    input stream InputFile close.

    /* reset propath, then add initial value of propath to the end new propath */
    propath = "".
    propath = vCompileObjectPropath + 
             (if vHoldPropath begins "," then "" else ",") + 
             ipInitialPropath.

    return.
end procedure. /* load-propath-directories */
                                

procedure compile-files:
/*------------------------------------------------------------------------
  Purpose:      generate list of files to compile, then compile them
  Parameters:   
------------------------------------------------------------------------*/
def input param ipLogFilename           as char no-undo.

def var vCompileDirectory               as char no-undo.
def var vSaveToDirectory                as char no-undo.
def var vFileExtensionList              as char no-undo.
def var vExcludeFilenameList            as char no-undo.
def var vDirectoryLevel                 as int no-undo.
def var vCompileFileFullPath            as char no-undo.
def var vFileIsESSWebObject             as log no-undo.
def var vIncludeFileGenerated           as log no-undo.
def var vObjectType                     as char no-undo.
def var vCompileFiles                   as log no-undo.
def var vCreateXREFFiles                as log no-undo.
def var vXREFFileFullPath               as char no-undo.
def var vTotalFilesCompiled             as int no-undo.
def var vTotalFilesCompiledSuccessfully as int no-undo.
def var vTotalCompileErrors             as int no-undo.
def var vErrorNumber                    as int no-undo.


    write-compile-log("SPACE","").        

    assign
        vCompileDirectory = get-compile-object-tag(ttCompileObject.ttTagValue,"{&COMPILEDIRECTORY}").
        vSaveToDirectory  = get-compile-object-tag(ttCompileObject.ttTagValue,"{&SAVETODIRECTORY}").

    /* a compiledirectory must be specified */
    if vCompileDirectory = "" then do:
        write-global-compile-log("SUMMARY","  ***Aborting Compile Process: No Compile Directory Specified").        
        write-compile-log("ERROR","ERROR: No Compile Directory Specified").        
        return.
    end.        

    /* verify that compiledirectory is a valid directory */
    assign
        file-info:file-name     = vCompileDirectory
        vCompileDirectory       = file-info:full-pathname.
    if file-info:file-type = ? OR index(file-info:file-type,"D") = 0 then do:
        write-global-compile-log("SUMMARY","  ***Aborting Compile Process: Invalid {&COMPILEDIRECTORY} Specified (" + 
                                 get-compile-object-tag(ttCompileObject.ttTagValue,"{&COMPILEDIRECTORY}") + ")").        
        write-compile-log("ERROR","  ***ERROR: Invalid {&COMPILEDIRECTORY} Specified (" + 
                          get-compile-object-tag(ttCompileObject.ttTagValue,"{&COMPILEDIRECTORY}") + ")").        
        return.
    end. 
    else
        write-compile-log("HEADER","Compile Directory:       " + vCompileDirectory).        
                         

    /* default savetodirectory to compiledirectory if necessary */
    if vSaveToDirectory = "" then 
        vSaveToDirectory = vCompileDirectory.


    /* verify that vSaveToDirectory is a valid dir, if not default it to vCompileDirectory */
    assign
        file-info:file-name     = vSaveToDirectory
        vSaveToDirectory        = file-info:full-pathname.
    if file-info:file-type = ? OR index(file-info:file-type,"D") = 0 then do:
        write-compile-log("HEADER","  ***ERROR:Invalid {&SAVETODIRECTORY} Specified (Defaulting to {&COMPILEDIRECTORY}): " + 
                          vCompileDirectory).        
        
        vSaveToDirectory = vCompileDirectory.
    end.
    else
        write-compile-log("HEADER","SaveTo Directory:        " + vSaveToDirectory).        
    



    /* get list of file extensions that can be compiled.  Include files (.i) can 
       be included in this list so that they are displayed in the log file, but they 
       will never be compiled.  If this compile object does not specify a list of
       extensions, use the global object extension list.  If the global list is also
       empty, use hardcoded default list of extensions */
    vFileExtensionList = (if get-compile-object-tag(ttCompileObject.ttTagValue,"{&FILEEXTENSIONLIST}") <> "" 
                            then get-compile-object-tag(ttCompileObject.ttTagValue,"{&FILEEXTENSIONLIST}") 
                            else get-compile-object-tag("{&GLOBALOBJECT}","{&FILEEXTENSIONLIST}")).

    /* if no extensions have been specified, use hardcoded defaults */
    if vFileExtensionList = "" then do:
        vFileExtensionList = "i,p,w,htm,html".
        write-compile-log("EXTENTIONS","Compile File Extensions: " + 
                          "***{&FILEEXTENSIONLIST} not specified.  Defaulting to " + 
                          vFileExtensionList).                
    end. /* if vFileExtensionList = "" */
    else do:
        /* remove special characters if necessary (".","*") */
        assign
            vFileExtensionList = replace(vFileExtensionList,".","")
            vFileExtensionList = replace(vFileExtensionList,"*","").

        write-compile-log("EXTENTIONS","Compile File Extensions: " + vFileExtensionList).                
    end.



    /* if "NO-COMPILE" option is specified, set var used to run compile process to FALSE */
    vCompileFiles = (lookup("NO-COMPILE",get-compile-object-tag(ttCompileObject.ttTagValue,"{&COMPILEOPTIONS}")) = 0 AND 
                     lookup("NO-COMPILE",get-compile-object-tag("{&GLOBALOBJECT}","{&COMPILEOPTIONS}")) = 0). 
    if not vCompileFiles then
        compiler:error = false.


    /* if "WRITE-XREF" option is specified, set var used to run compile with xref to TRUE */
    vCreateXREFFiles = (lookup("WRITE-XREF",get-compile-object-tag(ttCompileObject.ttTagValue,"{&COMPILEOPTIONS}")) <> 0 OR 
                        lookup("WRITE-XREF",get-compile-object-tag("{&GLOBALOBJECT}","{&COMPILEOPTIONS}")) <> 0). 
    if vCreateXREFFiles then do:
        vXREFFileFullPath = replace(ipLogFilename,".log",".xrf").
        os-delete value(vXREFFileFullPath).
    end.        
        

    write-compile-log("SPACE","").            


    /* load list of files that should not attempt to be compiled */
    run load-exclude-filename-list
        (input get-compile-object-tag(ttCompileObject.ttTagValue,"{&EXCLUDEFILENAMEFILE}"),
         output vExcludeFilenameList).


    /* remove all compile files from TT before repopulating it */
    for each ttCompileFile:
        delete ttCompileFile.
    end.        


    /* create a TT record for the initial directory to begin compiling */
    create ttCompileFile.
    assign
        ttCompileFile.ttFilename                = "{&COMPILEDIRECTORY}"
        ttCompileFile.ttFileFullPath            = vCompileDirectory
        ttCompileFile.ttFileAttribute           = "D"
        ttCompileFile.ttFileNoExtension         = ""
        ttCompileFile.ttFileExtension           = ""
        ttCompileFile.ttFileDirectoryLevel      = 0
        ttCompileFile.ttFileParentDirectory     = "".


    write-compile-log("SPACE","").


    /* run CompileFileList.p recursively (if necessary) for all subdirectories under starting directory */
    vDirectoryLevel = 0.
    run CompileFileList.p
        (input-output table ttCompileFile,
         input-output vDirectoryLevel,
         input vCompileDirectory,
         input vFileExtensionList,
         input (lookup("NO-RECURSE",get-compile-object-tag(ttCompileObject.ttTagValue,"{&COMPILEOPTIONS}")) <> 0 OR
                lookup("NO-RECURSE",get-compile-object-tag("{&GLOBALOBJECT}","{&COMPILEOPTIONS}")) <> 0),
         input get-compile-object-tag(ttCompileObject.ttTagValue,"{&EXCLUDEDIRECTORYLIST}"),
         input vExcludeFilenameList).


    write-compile-log("SPACE","").


    /* go thru all TT records and determine where files should be saved to after 
       they're compiled, assuming the savetodirectory is different than the 
       compiledirectory. I will assume that if the savetodir is different, then 
       it is a parallel directory structure mirroring the compiledirectory */
    if vSaveToDirectory <> vCompileDirectory then do:
        for each ttCompileFile
            where ttCompileFile.ttFileAttribute = "F":
            
            ttCompileFile.ttFileSaveToDirectory = replace(ttCompileFile.ttFileParentDirectory,vCompileDirectory,vSaveToDirectory).
        end. /* for each ttCompileFile */
    end. /* if vSaveToDirectory <> vCompileDirectory */


    /* go thru all files listed in TT and compile them */
    for each ttCompileFile
        where ttCompileFile.ttFileAttribute = "F":

        /* decide if this is an ESS file or not */
        vFileIsESSWebObject = lookup(ttCompileFile.ttFileExtension,"HTM,HTML") <> 0.

        /* get full path of file to be compiled */
        vCompileFileFullPath = ttCompileFile.ttFileFullPath.
        
        
        /* output "compiling" message to log, display "savetodirectory" if different from compiledirectory */        
        write-compile-log("COMPILING","Compiling... " + vCompileFileFullPath + 
                          (if vFileIsESSWebObject then " (ESS)" else "") + 
                          (if vSaveToDirectory <> vCompileDirectory  
                             then " ---> " + ttCompileFile.ttFileSaveToDirectory 
                             else "")).


        /* if this is and ESS file, then generate temp file before compile it */
        if vFileIsESSWebObject then do:
            /* initialize vars because they are input-output params */
            assign
                vObjectType             = ""
                vCompileFileFullPath    = "".                    
        
            run webutil/e4gl-gen.p
                (input ttCompileFile.ttFileFullPath,
                 input-output vObjectType,
                 input-output vCompileFileFullPath)
                 no-error.

            /* set flag if this ESS file generates an include file */
            vIncludeFileGenerated = lookup("INCLUDE",vObjectType) > 0.

            /* if include file is generated, move it to where it's supposed to be 
               saved if NO-COMPILE is NOT specified */
            if vIncludeFileGenerated and vCompileFiles then do:
                /* only copy include file to saveto directory if it's different than parent directory */
                if ttCompileFile.ttFileSaveToDirectory <> ttCompileFile.ttFileParentDirectory then do:
                    os-copy value(vCompileFileFullPath) value(replace(vCompileFileFullPath,
                                                              ttCompileFile.ttFileParentDirectory,
                                                              ttCompileFile.ttFileSaveToDirectory)).
                end. /*  if ttCompileFile.ttFileSaveToDirectory <> ttCompileFile.ttFileParentDirectory */
                
                write-compile-log("","  ***Include File Generated ---> " + 
                                  replace(vCompileFileFullPath,
                                          ttCompileFile.ttFileParentDirectory,
                                          ttCompileFile.ttFileSaveToDirectory)).
            end. /* if vIncludeFileGenerated */
        end. /* if vFileIsESSWebObject and vCompileFiles */
        else
            vIncludeFileGenerated = FALSE.
            
        

        /* only execute the actual compile if NO-COMPILE is NOT specified */
        if vCompileFiles then do:
            /* don't attempt to compile generated include files */
            if NOT vIncludeFileGenerated then do:
                compile value(vCompileFileFullPath) save into value(ttCompileFile.ttFileSaveToDirectory) 
                    no-error.
    
                /* display errors to the log file if they occur */
                if compiler:error OR compiler:warning then do vErrorNumber = 1 to error-status:num-messages:
                    write-compile-log("COMPILEERROR","     " +
                                      replace(error-status:get-message(vErrorNumber),compiler:file-name,"")).
        
                    /* keep track of total errors encountered for this "compile" */
                    vTotalCompileErrors = vTotalCompileErrors + 1.                        
                end. /* if compiler error */
            end. /* if NOT vIncludeFileGenerated */

            /* increment total number of files compiled */
            vTotalFilesCompiled = vTotalFilesCompiled + 1.

            /* increment total number of files compile successfully */
            if not compiler:error then
                vTotalFilesCompiledSuccessfully = vTotalFilesCompiledSuccessfully + 1.
        end. /* if vCompileFiles */
        

        /* if WRITE-XREF option is specified and there are no compile errors and this 
           is not generated include file */
        if vCreateXREFFiles and NOT compiler:error and NOT vIncludeFileGenerated then do:
            compile value(vCompileFileFullPath) xref value(vXREFFileFullPath) append
                no-error.
            compiler:error = false.              
        end.  /* if vCreateXREFFiles */

        
        /* remove temp file generated above if necessary */
        if vFileIsESSWebObject then do:
            /* only delete temp file if this is not a generated include file OR if
               it is an include file and the saveto and parent directories are different*/
            if NOT vIncludeFileGenerated OR 
               (vIncludeFileGenerated AND (ttCompileFile.ttFileSaveToDirectory <> ttCompileFile.ttFileParentDirectory)) then
                os-delete value(vCompileFileFullPath).
        end. /* if vFileIsESSWebObject */
    end. /* for each ttCompileFile */       


    /* only log statistics if files where actually compiled */
    if vCompileFiles then do:
        /* output compile summary info to global log */
        write-global-compile-log("SUMMARY","  Compilation finished with (" +
                                 trim(string(vTotalCompileErrors)) + ") errors in (" +
                                 trim(string(vTotalFilesCompiled - vTotalFilesCompiledSuccessfully)) + ") files").
        write-global-compile-log("SUMMARY","  (" + trim(string(vTotalFilesCompiledSuccessfully)) + ") out of (" +
                                 trim(string(vTotalFilesCompiled)) + ") files compiled successfully").
    
    
        /* output compile summary info to log */
        write-compile-log("SPACE","").
        write-compile-log("HEADER","Compilation finished with (" +
                          trim(string(vTotalCompileErrors)) + ") errors in (" +
                          trim(string(vTotalFilesCompiled - vTotalFilesCompiledSuccessfully)) + ") files").
        write-compile-log("HEADER","(" + trim(string(vTotalFilesCompiledSuccessfully)) + ") out of (" +
                          trim(string(vTotalFilesCompiled)) + ") files compiled successfully").
        write-compile-log("SPACE","").            
    end. /* if vcompile */

    /* else log that NO-COMPILE option was specified */
    else do:
        write-global-compile-log("SUMMARY","  ***Aborting Compile Process: NO-COMPILE option specified").        
        write-compile-log("SPACE","").
        write-compile-log("HEADER","  ***Aborting Compile Process: NO-COMPILE option specified").        
        write-compile-log("SPACE","").
    end. /* else log NO-COMPILE option */


    /* log that WRITE-XREF option was specifed and display path to XREF file */
    if vCreateXREFFiles then do:
        write-global-compile-log("SUMMARY","  ***WRITE-XREF option specified: " + vXREFFileFullPath).
        write-compile-log("HEADER","***WRITE-XREF option specified: " + vXREFFileFullPath).
        /* write-compile-log("SPACE",""). */
    end.  /* if WRITE-XREF specified */
end procedure. /* compile-files */

                                
procedure create-log-file:
/*------------------------------------------------------------------------
  Purpose:      Determine log directory/filename, then determine if file 
                can be written to that directory
  Parameters:   (1)Directory name specified in .dat file
                (2)Name of this compile object
                (3)OUTPUT: Full path name of log file being written to
------------------------------------------------------------------------*/
def input param  ipLogFileDirectory     as char no-undo.
def input param  ipCompileObjectName    as char no-undo.
def output param opLogFileFullPath      as char no-undo.

def var vLogFileDirectory               as char no-undo.
    
    vLogFileDirectory = ipLogFileDirectory.

    /* is a log file directory is specified */
    if vLogFileDirectory <> "" then do:
        assign            
            file-info:file-name     = vLogFileDirectory
            vLogFileDirectory       = file-info:full-pathname.

        /* if it is an invalid directory */
        if file-info:file-type = ? OR index(file-info:file-type,"D") = 0 then 
            write-global-compile-log("SUMMARY","  ***Invalid {&LOGFILEDIRECTORY} specified (" + 
                                     ipLogFileDirectory + ")  Redirecting output...").

        /* else it is a valid directory */
        else do:
            opLogFileFullPath = vLogFileDirectory + {&OSSlash} + ipCompileObjectName + ".log".

            /* ensure user can write to this directory */
            os-copy value(this-procedure:file-name) value(opLogFileFullPath).
            if os-error = 0 then 
                return opLogFileFullPath.
            else
                write-global-compile-log("SUMMARY","  ***Cannot write to directory (" + 
                                         ipLogFileDirectory + ")  Redirecting output...").
        end. /* else it is a valid directory */
    end. /* if vLogFileDirectory <> "" */
    

    /* default to creating log file in working directory. use compile object 
       name as the filename */
    opLogFileFullPath = ipCompileObjectName + ".log".
    
    /* ensure user can at least write to working directory */
    os-copy value(this-procedure:file-name) value(opLogFileFullPath).
    if os-error = 0 then 
        return opLogFileFullPath.
    else 
        return "ERROR".
end procedure. /* create-log-file */
                                

procedure run-progress-code:
/*------------------------------------------------------------------------
  Purpose:      Run a file containing custom program logic before and/or
                after the files are compiled.
  Parameters:   (1) Filename contain Progress code
                (2) When to run this Progress code (START or END)
                (3) Log file type (global or compile object)
------------------------------------------------------------------------*/
def input param ipProgressCodeFilename  as char no-undo. 
def input param ipProgressCodeRunMode   as char no-undo.    
def input param ipLogType               as char no-undo.

def var vErrorNumber                    as int no-undo.

    /* output message to a .log file if a progress program is not specified */
    if ipProgressCodeFilename = "" then do:
        /* decide if message is written to global log or compile object log */
        decide-write-log(ipLogType,"HEADER","Searching For Progress Code File(" + 
                         ipProgressCodeRunMode + " COMPILE): No File Specified" ).            
        return.
    end. /* if ipProgressCodeFilename = "" */

    /* decide if message is written to global log or compile object log */
    decide-write-log(ipLogType,"HEADER","Searching For Progress Code File(" + 
                     ipProgressCodeRunMode + " COMPILE): " + ipProgressCodeFilename ).            
    
    /* search for file, if not found then return  */
    if search(ipProgressCodeFilename) = ? then do:
        /* decide if message is written to global log or compile object log */
        decide-write-log(ipLogType,"LOGICERROR","  ***ERROR: File Not Found: " + ipProgressCodeFilename).            
        return.
    end. /* if search(ipProgressCodeFilename) = ? */
    else
        ipProgressCodeFilename = search(ipProgressCodeFilename).


    /* if called by global object and "NO-PROGRESSCODE" option is specified, don't execute progress code */
    if ipLogType = "{&GLOBALOBJECT}" then do:
        if lookup("NO-PROGRESSCODE",get-compile-object-tag("{&GLOBALOBJECT}","{&COMPILEOPTIONS}")) <> 0 then do:
            decide-write-log(ipLogType,"SUMMARY","  ***Aborting Progress Code Execution: NO-PROGRESSCODE option specified").        
            return.
        end.            
    end. 
    /* else if called by compile object and "NO-PROGRESSCODE" option is specified for this compile object
       OR the global object, don't execute progress code */
    else do:
        if lookup("NO-PROGRESSCODE",get-compile-object-tag(ttCompileObject.ttTagValue,"{&COMPILEOPTIONS}")) <> 0 or
           lookup("NO-PROGRESSCODE",get-compile-object-tag("{&GLOBALOBJECT}","{&COMPILEOPTIONS}")) <> 0  then do:
        
            decide-write-log(ipLogType,"HEADER","  ***Aborting Progress Code Execution: NO-PROGRESSCODE option specified").        
            return.
        end.    
    end. /* non-global object and NO-PROGRESSCODE */


    /* check syntax of program before running it */
    compile value(ipProgressCodeFilename)
        no-error.
    /* log any errors encountered when checking syntax */    
    if compiler:error then do:
        do vErrorNumber = 1 to error-status:num-messages:
            decide-write-log(ipLogType,"LOGICERROR","  ***ERROR: " + 
                             replace(error-status:get-message(vErrorNumber),compiler:file-name,"")).
        end. /* log errors */
        return.
    end. /* if compiler error */


    /* run file containing progress code. */
    run value(ipProgressCodeFilename)
        (input ipLogType,
         input ipProgressCodeRunMode)
        no-error.
    /* log any errors encoutnered when running progress code */
    if error-status:error then do vErrorNumber = 1 to error-status:num-messages:
        decide-write-log(ipLogType,"LOGICERROR","  ***ERROR: " + error-status:get-message(vErrorNumber)).            
    end. /* if error-status:error */    

    return.
end procedure. /* run-progress-code */


procedure run-os-script:
/*------------------------------------------------------------------------
  Purpose:      Run a custom OS script file containing logic before and/or
                after the files are compiled.
  Parameters:   (1) OS Script Filename 
                (2) When to run this script (START or END)
                (3) Log file type (global or compile object)
------------------------------------------------------------------------*/
def input param ipOSScriptFilename  as char no-undo. 
def input param ipOSScriptRunMode   as char no-undo.    
def input param ipLogType           as char no-undo.

def var vOSScriptOutput             as char no-undo.

    /* output message to a .log file if an OS script is not specified */
    if ipOSScriptFilename = "" then do:
        /* decide if message is written to global log or compile object log */
        decide-write-log(ipLogType,"HEADER","Searching For OS Script File(" + 
                         ipOSScriptRunMode + " SCRIPT): No File Specified").            
        return.
    end. /* if ipOSScriptFilename = "" */
    
    /* decide if message is written to global log or compile object log */
    decide-write-log(ipLogType,"HEADER","Searching For OS Script File(" + 
                     ipOSScriptRunMode + " SCRIPT): " + ipOSScriptFilename ).            
    
    /* search for file, if not found then return  */
    if search(ipOSScriptFilename) = ? then do:
        /* decide if message is written to global log or compile object log */
        decide-write-log(ipLogType,"LOGICERROR","  ***ERROR: File Not Found: " + 
                         ipOSScriptFilename).            
        return.
    end. /* if search(ipOSScriptFilename) = ? */
    else
        ipOSScriptFilename = search(ipOSScriptFilename).        

    
    /* if called by global object and "NO-OSSCRIPT" option is specified, don't execute OS script */
    if ipLogType = "{&GLOBALOBJECT}" then do:
        if lookup("NO-OSSCRIPT",get-compile-object-tag("{&GLOBALOBJECT}","{&COMPILEOPTIONS}")) <> 0 then do:
            decide-write-log(ipLogType,"SUMMARY","  ***Aborting OS Script Execution: NO-OSSCRIPT option specified").        
            return.
        end.            
    end. 
    /* else if called by compile object and "NO-OSSCRIPT" option is specified for this compile object
       OR the global object, don't execute OS script */
    else do:
        if lookup("NO-OSSCRIPT",get-compile-object-tag(ttCompileObject.ttTagValue,"{&COMPILEOPTIONS}")) <> 0 or
           lookup("NO-OSSCRIPT",get-compile-object-tag("{&GLOBALOBJECT}","{&COMPILEOPTIONS}")) <> 0  then do:

            decide-write-log(ipLogType,"HEADER","  ***Aborting OS Script Execution: NO-OSSCRIPT option specified").        
            return.
        end.
    end. 
    

    /* run custom OS script file */
    input stream InputFile through value(ipOSScriptFilename) no-echo.    
    repeat:
        import stream InputFile unformatted vOSScriptOutput.

        decide-write-log(ipLogType,"SCRIPTERROR","   SCRIPT OUTPUT ---> " + 
                         vOSScriptOutput).            
    end. /* repeat */
    input stream InputFile close.

    return.
end procedure. /* run-os-script */


procedure load-exclude-filename-list:
/*------------------------------------------------------------------------
  Purpose:      Load names of files that are NOT to be compiled.
  Parameters:   (1) Filename containing files to be excluded
                (2) CSV list of files to be excluded from compile
------------------------------------------------------------------------*/
def input param ipExcludeFile           as char no-undo. 
def output param opExcludeFilenameList  as char no-undo.    

def var vFileLine                       as char no-undo.
def var vListEntry                      as int no-undo.

    /* output message to a .log file */
    if ipExcludeFile = "" then do:
        write-compile-log("HEADER","Searching For {&EXCLUDEFILENAMEFILE}: No File Specified").            
        return.
    end. /* if ipExcludeFile = "" */


    write-compile-log("HEADER","Searching For {&EXCLUDEFILENAMEFILE}: " + ipExcludeFile).            

                
    /* search for file, if not found then return  */
    if search(ipExcludeFile) = ? then do:
        write-compile-log("LOGICERROR","  ***ERROR: File Not Found: " + ipExcludeFile).            
        return.
    end. /* if file not found */
    else    
        ipExcludeFile = search(ipExcludeFile).


    /* ensure the last char in this file is CR/NL */
    run check-end-of-file
        (input ipExcludeFile).

    
    input stream InputFile from value(ipExcludeFile) no-echo.
    repeat:
        import stream InputFile unformatted vFileLine.

        /* trim file line */
        vFileLine = trim(vFileLine).

        /* skip comments and blank lines */
        if substring(vFileLine,1,1) = "#" or vFileLine = "" then
            next.

        /* handle multiple filename entries per line */
        do vListEntry = 1 to num-entries(vFileLine):
           /* skip black entries on a comma delimited line */
            if trim(entry(vListEntry,vFileLine)) = "" then
                next.
            else
                opExcludeFilenameList = opExcludeFilenameList +
                                       (if opExcludeFilenameList = "" then "" else ",") +
                                       replace(trim(entry(vListEntry,vFileLine)),{&NonOSSlash},{&OSSlash}).
        end. /* do vListEntry = 1 to num-entries(vFileLine) */
    end. /* repeat */
    input stream InputFile close.

    return.
end procedure. /* load-exclude-filename-list */
                                

procedure check-end-of-file:
/*------------------------------------------------------------------------
  Purpose:      Determine if last char in a file CR or NL.  If not, then 
                append a CR to the file
  Parameters:   (1)Filename
------------------------------------------------------------------------*/
def input param ipFilename     as char no-undo.

def var vFileLength            as int no-undo.

    input stream InputFile from value(ipFilename) no-echo.
    /* read last char in this file */
    seek stream InputFile to end.
    vFileLength = seek(InputFile).
    seek stream InputFile to vFileLength - 1.
    readkey stream InputFile.
    input stream InputFile close.     
    
    /* add a CR char to the end of file if necessary */
    if lastkey <> 10 and lastkey <> 13 then do:
        file-info:file-name = ipFilename.
        write-global-compile-log("SUMMARY","  ***Appending CR to " + 
                                 file-info:full-pathname).            

        output stream OutputFile to value(ipFilename) append.
        put stream OutputFile unformatted
            chr(13).
        output stream OutputFile close.
    end. /* if lastkey <> 10 and lastkey <> 13 */
end procedure. /* check-end-of-file */
                                

procedure write-compile-log-IP:
/*------------------------------------------------------------------------
  Purpose:      An Internal Procedure version of the write-compile-log 
                UDF to work around the problem of core dumps when using 
                UDFs from remote procedures.
  Parameters:   
------------------------------------------------------------------------*/
def input param ipLogType       as char no-undo. 
def input param ipLogMessage    as char no-undo.    

    /* call standard UDF */
    write-compile-log(ipLogType,ipLogMessage).            

    return.
end procedure. /* write-compile-log-IP */


procedure write-global-compile-log-IP:
/*------------------------------------------------------------------------
  Purpose:      An Internal Procedure version of the write-global-compile-log 
                UDF to work around the problem of core dumps when using 
                UDFs from remote procedures.
  Parameters:   
------------------------------------------------------------------------*/
def input param ipLogType       as char no-undo. 
def input param ipLogMessage    as char no-undo.    

    /* call standard UDF */
    write-global-compile-log(ipLogType,ipLogMessage).            

    return.
end procedure. /* write-global-compile-log-IP */


procedure get-compile-object-tag-IP:
/*------------------------------------------------------------------------
  Purpose:      An Internal Procedure version of the get-compile-object-tag 
                UDF to work around the problem of core dumps when using UDFs.
  Parameters:   (1) tagname value of TT record
                (2) tagtype value of TT record
                (3) OUTPUT: tagvalue field of TT record 
------------------------------------------------------------------------*/
def input param ipTagName       as char no-undo. 
def input param ipTagType       as char no-undo.    
def output param opTagValue     as char no-undo.

    /* call standard UDF */
    opTagValue = get-compile-object-tag(ipTagName,ipTagType).

    return.
end procedure. /* get-compile-object-tag-IP */
                                

procedure dump-compile-object-tt:
/*------------------------------------------------------------------------
  Purpose:      Display values of ttCompileObject (for debugging)
  Parameters:   
------------------------------------------------------------------------*/
    output stream OutputFile to CompileObjects.txt.

    put stream OutputFile unformatted
        "(" string(today,"99/99/99") " " string(time,"HH:MM:SSam") ") "
        skip(1).

    for each ttCompileObject 
        where ttCompileObject.ttTagType = "{&COMPILEOBJECT}"
        by ttCompileObject.ttTagNumber:

        put stream OutputFile unformatted 
            "[" ttCompileObject.ttTagType "]"
            " "
            fill("-",24 - length(trim(ttCompileObject.ttTagType)) - 2)
            " "
            ttCompileObject.ttTagValue  
            skip.

        for each bttCompileObject 
            where bttCompileObject.ttTagName    =  ttCompileObject.ttTagValue and
                  bttCompileObject.ttTagType    <> "{&COMPILEOBJECT}"
            by bttCompileObject.ttTagNumber: 

            put stream OutputFile unformatted 
                bttCompileObject.ttTagType
                " "
                fill("-",24 - length(trim(bttCompileObject.ttTagType)))
                " "
                bttCompileObject.ttTagValue  
                skip.
        end. /* for each bttCompileObject*/

        put stream OutputFile unformatted
            skip(1).
    end. /* for each ttCompileObject*/

    output stream OutputFile close.
end procedure. /* dump-compile-object-tt */
                                                       

/*------------------------------------------------------------------------
  Purpose:      Provide access to ttCompileObject TT values
  Parameters:   (1) tagname value of TT record
                (2) tagtype value of TT record
------------------------------------------------------------------------*/
function get-compile-object-tag returns char
    (input ipTagName    as char,
     input ipTagType    as char):

    def buffer bttCompileObject for ttCompileObject.

    find first bttCompileObject
        where bttCompileObject.ttTagName    = ipTagName and
              bttCompileObject.ttTagType    = ipTagType
        no-error.
        
    if avail bttCompileObject then
        return bttCompileObject.ttTagValue.
    
    return "".        
end function. /* get-compile-object-tag */


/*------------------------------------------------------------------------
  Purpose:      Parse a startup parameter from a connection string
  Parameters:   (1) The parameter name whose value you want
                (2) The connection string to parse
------------------------------------------------------------------------*/
function get-connection-parameter returns char
    (input ipParameterName      as char,
     input ipConnectionString   as char):

    def var vParameterValue     as char no-undo.
    def var vListEntry          as int no-undo.

    /* used spaces to delimit connection string */
    vListEntry = lookup(ipParameterName,ipConnectionString," ").            

    /* if this param is in the connection string, then return the 
       next value in the string (if there is a "next" value) */
    if vListEntry <> 0 and vListEntry < num-entries(ipConnectionString," ") then do:
        /* get parameter value */
        vParameterValue = entry(vListEntry + 1,ipConnectionString," ").

        /* if -db param is specified AND the parameter value has a slash OR dot, 
           parse DB name from that parameter value */
        if ipParameterName = "-db" then do: 
            /* parse DB filename from fullpath to DB if there is a slash */
            if index(vParameterValue,{&OSSlash}) <> 0 then 
                vParameterValue = substring(vParameterValue,r-index(vParameterValue,{&OSSlash}) + 1).

            /* parse DB filename if there is a dot */
            if index(vParameterValue,".") <> 0 then 
                vParameterValue = substring(vParameterValue,1,index(vParameterValue,".") - 1).
        end. /* if ipParameterName = "-db" */
        
        return vParameterValue.
    end. /* if this param is in the connection string */

    return "".        
end function. /* get-connection-parameter */


/*------------------------------------------------------------------------
  Purpose:      Write message to the log file for each compile object
  Parameters:   (1)Type of entry being logged
                (2)Actual message being logged
------------------------------------------------------------------------*/
function write-compile-log returns char
    (input ipLogMessageType     as char,
     input ipLogMessage         as char):

    if ipLogMessageType = "SPACE" then
        ipLogMessage = " ".

    put stream CompileLog unformatted
        ipLogMessage 
        skip.

    put stream CompileLog
        control null(0).

    return "".        
end function. /* write-compile-log */


/*------------------------------------------------------------------------
  Purpose:      Write message to the global log file 
  Parameters:   (1)Type of entry being logged
                (2)Actual message being logged
------------------------------------------------------------------------*/
function write-global-compile-log returns char
    (input ipLogMessageType     as char,
     input ipLogMessage         as char):

    if ipLogMessageType = "SPACE" then
        ipLogMessage = " ".

    put stream GCompileLog unformatted
        ipLogMessage 
        skip.

    put stream GCompileLog
        control null(0).

    return "".        
end function. /* write-global-compile-log */


/*------------------------------------------------------------------------
  Purpose:      Decide which log file to write messages to
  Parameters:   (1)Type of entry being logged
                (2)Actual message being logged
------------------------------------------------------------------------*/
function decide-write-log returns char
    (input ipLogType            as char,
     input ipLogMessageType     as char,
     input ipLogMessage         as char):

    if ipLogType = "{&GLOBALOBJECT}" then 
        write-global-compile-log(ipLogMessageType,ipLogMessage).
    else 
        write-compile-log(ipLogMessageType,ipLogMessage). 

    return "".        
end function. /* decide-write-log */

