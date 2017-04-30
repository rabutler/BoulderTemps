@echo off
>batchOutput.txt (
	cd C:\Users\Wheatgrass\Documents\GitHub\BoulderTemps\
	"C:\Program Files\R\R-3.3.1\bin\Rscript.exe" plotTMax.R
	"C:\Program Files\Git\bin\git.exe" add .
	"C:\Program Files\Git\bin\git.exe" commit -m "scheduled commit"
	"C:\Program Files\Git\bin\git.exe" push
)