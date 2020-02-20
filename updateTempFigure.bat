@echo off
>batchOutput.txt 2>&1(
	cd C:\Users\Alan\Documents\projects\BoulderTemps
	"Rscript.exe" main.R
	"C:\Program Files\Git\bin\git.exe" add .
	"C:\Program Files\Git\bin\git.exe" commit -m "scheduled commit"
	"C:\Program Files\Git\bin\git.exe" push
	cd C:\Users\Alan\Documents\projects\site\
	"C:\Program Files\Git\bin\git.exe" add .
	"C:\Program Files\Git\bin\git.exe" commit -m "scheduled commit"
	"C:\Program Files\Git\bin\git.exe" push
)