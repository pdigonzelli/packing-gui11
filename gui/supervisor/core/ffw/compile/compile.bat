@echo off

::
:: Set necessary Progress related variables
::
set DLC=d:\programs\progressv91
set PROMSGS=%DLC%\promsgs
set PROCFG=%DLC%\progress.cfg

::
:: Run compiler utility
::
%DLC%/bin/_progres -b -p Compile.p -pf Compile.pf



