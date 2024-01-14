'========================================================
'AUTOLOG - Automatic logfile naming for logging purposes.
'By Grogster at The Back Shed forums, March 2014

'Version 1A - Initial submission to library.
'========================================================

'This code will auto-generate a filename for daily logging purposes.

'In the event that you need to log something, and in the likely event that the
'logger will need to run for a few days, then this routine will come in handy.
'At the start of your code, you define an array to hold all the month references, and
'whenever you need to log some data, you call the LogDta routine.  This routine will
'build a filename from the current date, and write your data to the log file with
'a date-and-time reference.

'The routine will rebuild the filename reference any time you call it, so as the day, month
'and year change, so will the logfile name, all automatically, with no input from you 
'required other then to make sure the initial system date and time are correct.
'Actual output will be in the directory B:\LOG\ in this example, but you can change
'that to whatever you need, by altering this in the routine.

'The routine could be a GOSUB, a defined subroutine passing the data you want to log
'as an argument, or even an interrupt if the data to log was short.

'An example of LFILE$ as built by the routine, would be B:\LOG\04MAR14.LOG

'Date-and-time stamping is almost essential for most logging applications, so you 
'can know exactly when something happened, and this routine stamps any data with that
'inforamtion as part of the process before saving it all to the log file.

'------------------


DIM MTH$(12) length 3 'Setup array for handling the months of the year
MTH$(1)="JAN":MTH$(2)="FEB":MTH$(3)="MAR":MTH$(4)="APR":MTH$(5)="MAY":MTH$(6)="JUN"
MTH$(7)="JUL":MTH$(8)="AUG":MTH$(9)="SEP":MTH$(10)="OCT":MTH$(11)="NOV":MTH$(12)="DEC"

'
'---YOUR MAIN PROGRAM CODE HERE
'

LogDta:
  LFILE$="B:\LOG\" + MID$(DATE$,1,2) + MTH$(VAL(MID$(DATE$,4,2))) + MID$(DATE$,9,2) + ".LOG"
  LDT$=DATE$ + "," + TIME$ + ">" + " " + {DATA YOU WANT LOGGED HERE} + CHR$(13)
  OPEN LFILE$ FOR APPEND AS #3:FlagSD=1
  PRINT #3,LDT$:CLOSE #3:FlagSD=0 
