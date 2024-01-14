' Demonstration of random file access.
' Geoff Graham   July 2013
'
' Using this program you can append to a file (to add some data in the first place)
' then read/write records using random record numbers.  The first record in
' the file is record number 1, the second is 2, etc.

RecLen = 64
OPEN "test.dat" FOR RANDOM AS #1
DO
	abort: PRINT
	PRINT "Number of records in the file =" LOF(#1)/RecLen
	INPUT "Command (r = read,w = write, a = append, q = quit): ", cmd$
	IF cmd$ = "q" THEN CLOSE #1 : END
	IF cmd$ = "a" THEN
		SEEK #1, LOF(#1) + 1
	ELSE
		INPUT "Record Number: ", nbr
		IF nbr < 1 or nbr > LOF(#1)/RecLen THEN PRINT "Invalid record" : GOTO abort
		SEEK #1, RecLen * (nbr - 1) + 1
	ENDIF
	IF cmd$ = "r" THEN
		PRINT "The record = " INPUT$(RecLen, #1)
	ELSE
		LINE INPUT "Enter the data to be written: ", dat$
		PRINT #1,dat$ + SPACE$(RecLen - LEN(dat$));
	ENDIF
LOOP
