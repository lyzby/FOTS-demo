'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Demonstration of parsing the NMEA string produced by a GPS module
' Geoff Graham - July 2013
'
' All GPS modules are guaranteed to produce the GPRMC string
' See http://home.mira.net/~gnb/gps/nmea.html for details of the string
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
max = 20                                  ' maximum nbr of params
DIM arg$(max)                             ' used to hold the data fields

OPEN "COM1:4800" AS #1

DO                                        ' loop forever
  GetGPSData                              ' get the next line
  IF arg$(0) = "GPRMC" THEN               ' GPRMC contains lat/long
    IF arg$(2) = "A" THEN                 ' "A" means locked on to satellites
      PRINT "Latitude = ";  LEFT$(arg$(3), 2); " "; MID$(arg$(3), 3);
      PRINT " "; arg$(4);
      PRINT "   Longitude = "; LEFT$(arg$(5), 3); " "; MID$(arg$(5), 4);
      PRINT " "; arg$(6)
    ELSE
      PRINT "GPS searching..."
    ENDIF
  ENDIF
LOOP


''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' subroutine to load the GPS data fields into the array arg$()
' returns with the array populated
SUB GetGPSData
  DO
    DO WHILE INPUT$(1, #1) <> "$" : LOOP  ' wait for the start
    FOR i = 0 TO max
      arg$(i) = ""                        ' clear ready for data
      DO                                  ' loops until a specific exit
        x$ = INPUT$(1, #1)                ' get the character
        IF x$ = "," THEN EXIT             ' new data item, increment i
        IF x$ = "*" THEN EXIT SUB         ' we have all the data so exit
        arg$(i) = arg$(i) + x$            ' add to the data
      LOOP                                ' keep going
    NEXT i                                ' increment i
  LOOP
END SUB