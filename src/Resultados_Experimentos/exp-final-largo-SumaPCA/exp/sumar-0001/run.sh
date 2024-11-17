#!/bin/bash
tabulador="	"
echo "timestamp	event"  >  z-SHlog.txt 
fecha0=$(date +"%Y%m%d %H%M%S") 
echo "$fecha0""$tabulador""SH_START" >> z-SHlog.txt 
~/install/memcpu & 
memcpu_PID=$! 
source /home/$USER/.venv/bin/activate 
nice -n 15 Rscript --vanilla SUMA_Dimensionalidad_Script_Exp.R  ~/labo2024v2/   parametros.yml  2>&1 | tee z-SHoutfile.txt 
fecha1=$(date +"%Y%m%d %H%M%S") 
echo "$fecha1""$tabulador""SH_END" >> z-SHlog.txt 
kill -SIGTERM $memcpu_PID 
deactivate 
