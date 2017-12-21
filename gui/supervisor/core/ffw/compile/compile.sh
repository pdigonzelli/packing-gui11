#!/bin/bash

DLC=/home/ffw/dlc; export DLC
PROMSGS=$DLC/promsgs; export PROMSGS
PROCFG=$DLC/progress.cfg; export PROCFG

# Run compiler utility

$DLC/bin/_progres  -p compile.p -pf compile.pf



