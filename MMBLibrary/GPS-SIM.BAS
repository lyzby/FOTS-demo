' GPS Simulator (Geoff Graham, Sept 2013)

' Configuration settings
ComPort$ = "COM1:4800"
SecondsToGetLock = 10
SpeedInKnots = 5

Cls
Print
Print "GPS Simulator (Geoff Graham, Sept 2013)"
Print "Simulates an EM-408 GPS module made by GlobalSat Technology."
Print
Print "Output is at " Mid$(ComPort$,6) " baud on I/O pin 16 (COM1: Tx pin)"
Print "The output will simulate the startup with" SecondsToGetLock " seconds to get a"
Print "lock.  It will then simulate travelling east at" SpeedInKnots " knots."
LoopBack:
If lon <> 0 Then Close #1
Print
Print "Press any key to start followed by any key to stop."

' wait for the key press
Do : Loop While Inkey$ = ""

' initialise
t$ = Time$
sec = 0
lat = 3128.1234
lon = 11600.1234

Open ComPort$ As #1

' output the startup messages typically produced by the EM-408
PrintToGPS "$PSRFTXT,Version:GSW3.2.4_3.1.00.12-SDK003P1.00a"
PrintToGPS "$PSRFTXT,Version2:F-GPS-03-0701301"
PrintToGPS "$PSRFTXT,WAAS Enable"
PrintToGPS "$PSRFTXT,TOW:  0"
PrintToGPS "$PSRFTXT,WK:   1399"
PrintToGPS "$PSRFTXT,POS:  6378137 0 0"
PrintToGPS "$PSRFTXT,CLK:  96250"
PrintToGPS "$PSRFTXT,CHNL: 12"
PrintToGPS "$PSRFTXT,Baud rate: " + Mid$(ComPort$,6)

' now pretend that we are searching to get a fix on the satellites
Do
  Do : Loop While t$ = Time$
  t$ = Time$
  sec = sec + 1
  If Inkey$ <> "" Then GoTo LoopBack
  SendGGA "V"
  SendGSA "V"
  SendRMC "V"
  If sec Mod 5 = 0 Then SendGSV "V"
  If Inkey$ <> "" Then GoTo LoopBack
Loop While sec < 20

' now pretend that we have a fix and are moving east
Do
  Do : Loop While t$ = Time$
  t$ = Time$
  sec = sec + 1
  If Inkey$ <> "" Then GoTo LoopBack
  SendGGA "A", lat, lon
  SendGSA "A"
  SendRMC "A", lat, lon
  If sec Mod 5 = 0 Then SendGSV "A"
  lon = lon + SpeedInKnots/3600		' this simulates movement east
  If Inkey$ <> "" Then GoTo LoopBack
Loop


' output the NMEA RMC message
Sub SendRMC status$, lat, lon
  Local msg$, tim$, dat$
  tim$ = Time$
  tim$ = Left$(tim$, 2) + Mid$(tim$, 4, 2) + Right$(tim$, 2) + ".000"
  dat$ = Date$
  dat$ = Left$(dat$, 2) + Mid$(dat$, 4, 2) + Right$(dat$, 2)
  msg$ = "GPRMC," + tim$ + "," + status$ + "," + Format$(lat, "%09.4f") + ",S,"
  msg$ = msg$ + Format$(lon, "%010.4f")
  If status$ = "V" Then
    msg$ = msg$ + ",E,0.00,90," + dat$ + ",,"
  Else
    msg$ = msg$ + ",E," + Format$(SpeedInKnots, "%04.2f") + ",90," + dat$ + ",,"
  EndIf
  SendToGPS msg$
End Sub

' output the GSV message
Sub SendGSV status$
  If status$ = "V" Then
    SendToGPS "GPGSV,3,1,12,20,00,000,43,10,00,000,,31,00,000,,27,00,000,"
    SendToGPS "GPGSV,3,2,12,19,00,000,,07,00,000,,04,00,000,,24,00,000,"
    SendToGPS "GPGSV,3,3,12,16,00,000,,28,00,000,,26,00,000,,29,00,000,"
  Else
    SendToGPS "GPGSV,3,1,12,11,75,324,36,01,59,146,27,32,58,161,34,20,56,209,30"
    SendToGPS "GPGSV,3,2,12,23,52,301,40,25,42,101,,13,23,311,23,17,19,237,23"
    SendToGPS "GPGSV,3,3,12,31,12,136,,19,08,358,13,14,06,136,,27,05,350,"
  EndIf
End Sub

' output the GGA message
Sub SendGGA status$, lat, lon
  Local msg$, tim$
  tim$ = Time$
  tim$ = Left$(tim$, 2) + Mid$(tim$, 4, 2) + Right$(tim$, 2) + ".000"
  msg$ = "GPGGA," + tim$
  If status$ = "V" Then
       msg$ = msg$ + ",,,,,0,00,,,M,0.0,M,,0000"
  Else
    msg$ = msg$ + "," + Format$(lat, "%09.4f") + ",S," + Format$(lon, "%010.4f")
    msg$ = msg$ + ",E,1,05,3.4,25.0,M,-29.3,M,,0000"
  EndIf
  SendToGPS msg$
End Sub

' output the GSA message
Sub SendGSA status$
  If status$ = "V" Then
    SendToGPS "GPGSA,A,1,,,,,,,,,,,,,,,"
  Else
    SendToGPS "GPGSA,A,3,23,20,13,11,32,,,,,,,,4.7,3.4,3.4"
  EndIf
End Sub

' take a message and add the checksum and output
Sub SendToGPS msg$
  Local cs, m$, i
  For i = 1 To Len(msg$)
    cs = (cs Xor Asc(Mid$(msg$, i, 1))) And &hFF
  Next i
  m$ = Hex$(cs)
  If Len(m$) = 1 Then m$ = "0" + m$
  PrintToGPS "$" + msg$ + "*" + m$
End Sub

' print the message to the COM port and the screen
Sub PrintToGPS msg$
  Print #1, msg$
  Print msg$
End Sub

