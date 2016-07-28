#!/usr/bin/env zsh
filename=sdeExtraction/allSolarSystems.txt
ls sde/fsd/universe/*/*/*/*/ -d | awk -F'/' '{print $7}' > $filename
