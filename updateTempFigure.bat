@echo off
>batchOutput.txt 2>&1(
	cd C:\Users\Alan\Documents\projects\BoulderTemps\
	"C:\Program Files\R\R-3.5.1\bin\Rscript.exe" plotTMax.R
	"C:\Program Files\Git\bin\git.exe" add .
	"C:\Program Files\Git\bin\git.exe" commit -m "scheduled commit"
	"C:\Program Files\Git\bin\git.exe" push
	cd C:\Users\Alan\Documents\projects\site\
	"C:\Program Files\Git\bin\git.exe" add .
	"C:\Program Files\Git\bin\git.exe" commit -m "scheduled commit"
	"C:\Program Files\Git\bin\git.exe" push
)