/*-----------------------------------------------------------------------*
  File........: tt_ini.i
  Version.....: 1.04 - 12/21/2000
  Description : Defines temptables used by robust.i and showcfg.html
  Input Param : <none>
  Output Param: <none>
  Author......: S.E. Southwell - BravePoint / USI (770) 449-9696
  Copyright...: FreeFramework 2000  - http://www.freeframework.org
  Created.....: 12/21/2000
  Notes.......:
------------------------------------------------------------------------*/

DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-DB 
 FIELD databaseName      AS CHAR
 FIELD ConnectProc       AS CHAR
 FIELD NotifyProc        AS CHAR
 FIELD DownSinceDate     AS DATE
 FIELD DownSinceTime     AS INTEGER
 FIELD LastNotifyDate    AS DATE
 FIELD LastNotifyTime    AS INTEGER
 FIELD ReconnectAttempts AS INTEGER
 FIELD DownPeriods       AS INTEGER
 FIELD HostName          AS CHAR
 FIELD Network           AS CHAR
 FIELD ServiceName       AS CHAR
 FIELD FileName          AS CHAR
 FIELD OtherParams       AS CHAR
 INDEX databasename IS UNIQUE PRIMARY databasename
.

DEFINE NEW GLOBAL SHARED TEMP-TABLE TT-DBSet
 FIELD DBSet            AS CHAR
 FIELD DBList           AS CHAR
 INDEX dbset IS UNIQUE PRIMARY DBSet
.

DEFINE NEW GLOBAL SHARED TEMP-TABLE TT-ObjectDB
 FIELD ObjectName       AS CHAR
 FIELD DBSet            AS CHAR
 INDEX ObjectSet IS UNIQUE PRIMARY ObjectName DBSet
.

DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-redirect
 FIELD resourcefrom     AS CHAR
 FIELD resourceto 	    AS CHAR
 FIELD log-fl 		    AS LOGICAL
 FIELD perm-fl		    AS LOGICAL
 INDEX resfrom IS UNIQUE PRIMARY resourcefrom
.

DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-iplimit
 FIELD resourcefrom     AS CHAR
 FIELD iplist           AS CHAR
 FIELD errormessage     AS CHAR
 INDEX resfrom IS UNIQUE PRIMARY resourcefrom
.

DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-ipkill
 FIELD ipaddr           AS CHAR
 INDEX ipaddr IS UNIQUE PRIMARY ipaddr
.

&IF "{&MULTI_DEV_PROPATHS}" = "YES" &THEN
DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-developer
 FIELD devname          AS CHAR
 FIELD mypropath        AS CHAR
 INDEX devname IS UNIQUE PRIMARY devname
.
&ENDIF
