/*------------------------------------------------------------------------
  File:        CompileFileList.p
  Author:      RJM 
  Description: Populate a temp-table will all files in a directory ree 
               that should be compiled.
------------------------------------------------------------------------*/
def temp-table ttCompileFile no-undo
    field ttFilename                    as char
    field ttFileFullPath                as char
    field ttFileAttribute               as char
    field ttFileNoExtension             as char
    field ttFileExtension               as char
    field ttFileDirectoryLevel          as int
    field ttFileParentDirectory         as char
    field ttFileSaveToDirectory         as char
    index ttFilename                    is primary ttFileAttribute ttFileDirectoryLevel ttFilename.

def input-output param table            for ttCompileFile.
def input-output param ipDirectoryLevel as int no-undo.
def input param ipDirectoryFullPath     as char no-undo.
def input param ipFileExtensionList     as char no-undo.
def input param ipNoRecursion           as log no-undo.
def input param ipExcludeDirectoryList  as char no-undo.
def input param ipExcludeFilenameList   as char no-undo.

def buffer bttCompileFile for ttCompileFile.

def temp-table ttSubDirectory no-undo
    field ttDirectoryFullPath           as char
    index ttDirectoryFullPath           is primary ttDirectoryFullPath.

def new global shared var hCompileProcedure as handle no-undo.

def var vFilename                       as char no-undo.
def var vFileFullPath                   as char no-undo.
def var vFileAttribute                  as char no-undo.
def var vFileNoExtension                as char no-undo.
def var vFileExtension                  as char no-undo.
def var vRIndex                         as int no-undo.
def var vFileSize                       as char no-undo.
def var vListEntry                      as int no-undo.                    


/* log which directories are searched through for files */
run write-compile-log-IP in hCompileProcedure
    (input "SEARCH",
     input "Searching Directory... " + ipDirectoryFullPath).


/* loop through all directories specified in exclude directory list */
do vListEntry = 1 to num-entries(ipExcludeDirectoryList):
    /* if directory is on the exclude directory (EXCLUDEDIR) list, then leave */
    if index(ipDirectoryFullPath,entry(vListEntry,ipExcludeDirectoryList)) > 0 then do:
        run write-compile-log-IP in hCompileProcedure
            (input "EXCLUDE",
             input "  ***Aborting Search Process: Directory found in excludeDirectoryList (" + 
                   entry(vListEntry,ipExcludeDirectoryList) + ")").

        return.
    end. /* if index(ipDirectoryFullPath,entry(vListEntry,ipExcludeDirectoryList)) > 0 */
end. /* do vListEntry = 1 to num-entries(ipExcludeDirectoryList) */


/* loop thru all files and subdirecties in this directory */
input from os-dir(ipDirectoryFullPath).
repeat:
    import vFilename vFileFullPath vFileAttribute.

    /* set fileattr to "D" or "F" */
    vFileAttribute = (if index(vFileAttribute,"D") <> 0 then "D" 
                      else if index(vFileAttribute,"F") <> 0 then "F"
                      else "").

    /* only include directories and files */
    if vFileAttribute = "" then
        next.

    /* exclude "." and ".." directories */
    if vFileAttribute = "D" and vFilename begins "." then
        next.

    /* if this is a file, only include valid extentions */
    if vFileAttribute = "F" then do:
        vRIndex = r-index(vFilename,".").

        /* only proceed if this is a valid file name.  file name must 
           contain a "." and the "." cannot be first or last character */
        if vRIndex <= 1 or vRIndex = length(vFilename) then
            next.

        /* parse filename and extension into variables */
        assign
            vFileNoExtension    = substring(vFilename,1,vRIndex - 1)
            vFileExtension      = substring(vFilename,vRIndex + 1).
            
        /* check this extension against list of valid extendions */
        if lookup(vFileExtension,ipFileExtensionList) = 0 then
            next.                
    end. /* if vFileAttribute = "F" */
    /* else directorys don't need these variables set */
    else
        assign
            vFileNoExtension    = "" 
            vFileExtension      = "".

            
    /* add this directory or file to the TT */
    create ttCompileFile.
    assign
        ttCompileFile.ttFilename                = vFilename
        ttCompileFile.ttFileFullPath            = vFileFullPath
        ttCompileFile.ttFileAttribute           = vFileAttribute
        ttCompileFile.ttFileNoExtension         = vFileNoExtension
        ttCompileFile.ttFileExtension           = vFileExtension
        ttCompileFile.ttFileDirectoryLevel      = ipDirectoryLevel
        ttCompileFile.ttFileParentDirectory     = ipDirectoryFullPath
        ttCompileFile.ttFileSaveToDirectory     = ipDirectoryFullPath.
        
    /* populate TT with all subdirectories in this directory */
    if ttCompileFile.ttFileAttribute = "D" then do:
        create ttSubDirectory.
        assign 
            ttSubDirectory.ttDirectoryFullPath = ttCompileFile.ttFileFullPath.
    end. /* if ttCompileFile.ttFileAttribute = "D" */
end. /* repeat */
input close.
  

/* log files that where found */
FILE-BLOCK:
for each ttCompileFile 
    where ttCompileFile.ttFileAttribute         = "F" and
          ttCompileFile.ttFileDirectoryLevel    = ipDirectoryLevel:

    /* get size (in bytes) of file */
    run get-filesize
        (input ttCompileFile.ttFileFullPath,
         output vFileSize).

    /* log which valid files were found in this directory */    
    run write-compile-log-IP in hCompileProcedure
        (input "FOUND",
         input "      FOUND: " + ttCompileFile.ttFilename + " " + 
               fill("-",60 - (length(trim(ttCompileFile.ttFilename)) + 
               length(trim(vFileSize)))) + " " + vFileSize).

    /* if file is on exlude file (EXCLUDEFILE) list, delete it */ 
    if index(ipExcludeFilenameList,ttCompileFile.ttFilename) > 0 then do vListEntry = 1 to num-entries(ipExcludeFilenameList):
        if index(ttCompileFile.ttFileFullPath,entry(vListEntry,ipExcludeFilenameList)) > 0 then do:
            run write-compile-log-IP in hCompileProcedure
                (input "EXCLUDEFILE",
                 input "        ***File Removed From Compile: File found in excludeFilenameFile (" + 
                       entry(vListEntry,ipExcludeFilenameList) + ")").

            delete ttCompileFile.
            next FILE-BLOCK.
        end. /* if index(ttCompileFile.ttFileFullPath,entry(vListEntry,ipExcludeFilenameList)) > 0 */
    end. /* if index(ipExcludeFilenameList,ttCompileFile.ttFilename) > 0 */

    /* if this an include file, delete it */
    if ttCompileFile.ttFileExtension = "I" then
        delete ttCompileFile.    
end. /* for each ttCompileFile */             
  
    

/* determine if Embedded SpeedScript files exist */
for each ttCompileFile 
    where ttCompileFile.ttFileAttribute         = "F" and
          ttCompileFile.ttFileDirectoryLevel    = ipDirectoryLevel and
         (ttCompileFile.ttFileExtension         = "HTML" OR 
          ttCompileFile.ttFileExtension         = "HTM"):

    /* try to find a file (HTML Mapping WebObject) with same filename and a .w extension */
    find first bttCompileFile
        where bttCompileFile.ttFileAttribute        = "F" and
              bttCompileFile.ttFileDirectoryLevel   = ttCompileFile.ttFileDirectoryLevel and
              bttCompileFile.ttFileNoExtension      = ttCompileFile.ttFileNoExtension and
              bttCompileFile.ttFileExtension        = "W"
        no-error.

    /* if .w file (HTML Mapping WebObject) found, this .htm/.html file doesn't get compiled */
    if avail bttCompileFile then 
        delete ttCompileFile.
    /* if there is no .w file (HTML Mapping WebObject) in this directory, then
       search propath for it.  If a file is found, then delete from this TT record
       and output message to investigate this file */
    else if search(ttCompileFile.ttFileNoExtension + ".w") <> ? then do:
        run write-compile-log-IP in hCompileProcedure
            (input "INVESTIGATE",
             input "  **INVESTIGATE: " + search(ttCompileFile.ttFilename)).

        delete ttCompileFile.
    end. /* if search(ttCompileFile.ttFileNoExtension + ".w") <> ? */      
end. /* for each ttCompileFile */        


/* if "NO-RECURSE" option specified, then leave before recursing through subdirectories */
if ipNoRecursion then do:
    run write-compile-log-IP in hCompileProcedure
        (input "HEADER",
         input "  ***Aborting Recursive Search Process: NO-RECURSE option specified").
       
    return.
end. /* if ipNoRecursion */


/* run CompileFilesList.p recursively for all subdirectories in this directory */
for each ttSubDirectory:
    ipDirectoryLevel = ipDirectoryLevel + 1.

    run CompileFileList.p
        (input-output table ttCompileFile,
         input-output ipDirectoryLevel,
         input ttSubDirectory.ttDirectoryFullPath,
         input ipFileExtensionList,
         input ipNoRecursion,
         input ipExcludeDirectoryList,
         input ipExcludeFilenameList).
end. /* for each ttSubDirectory */




procedure get-filesize:
/*------------------------------------------------------------------------
  Purpose:      Determine size of file for debugging purposes (incase
                source code file has been modified AFTER it was compiled)
  Parameters:   
------------------------------------------------------------------------*/
def input param ipFileFullPath     as char no-undo. 
def output param opFileSize    as char no-undo.

def var vFileSize               as int no-undo.
/*
    input from value(ipFileFullPath).
    seek input to end.
    vFileSize = seek(input).
    input close.
*/
    /* use new V9 file-info feature to obtain filesize */
    assign
        file-info:filename  = ipFileFullPath
        vFileSize           = file-info:file-size.

    opFileSize = "[" + trim(string(vFileSize,">>,>>>,>>9")) + " bytes]".
end procedure. /* get-filesize */


