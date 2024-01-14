'Program to use GPS Module for Astronomical Fix
'Written by Ray Alger Sept 2014
GoTo START
INVTZ:
Cls
Print "Invalid Time Zone!"
Pause 1000
START:
TZV = 0
GoSub TITLE
Print@(1,420)"GMT +/- offset(default +10)"
Print@(1,400)"Local Time Zone";
Input LTZ$
If LTZ$ = "" Then LTZ$ = "+10"
LTZ = Val(LTZ$)
If LTZ>13 Or LTZ<-12 Then GoTo INVTZ
If LTZ>0 Then LTZ$ = "+"+Str$(LTZ) Else LTZ$ = Str$(LTZ)
TZV = 1
GoSub TITLE
Print@(1,240)"Please wait a minute, getting satelite fix!   "
Dim M(12) 'accumulated days for each month
Dim Para$(10) 'GPS parameters
M(1) = 0:M(2) = 31:M(3) = 59:M(4) = 90:M(5) = 120:M(6) = 151
M(7) = 181:M(8) = 212:M(9) = 243:M(10) = 273:M(11) = 304:M(12) = 334
NYL = -57.2907 'Normal year loss seconds
LYG = 179.2646 'Leap year gain seconds
REF = 23992.27 'Greenwich mean sidereal seconds @ 00hr 1 Jan 2000
CF = 1.0027379 'Conversion factor solar to sidereal
Pin(19) = 1
SetPin 19,8
Open "COM1:4800,100,HANDL1" As #1 'interupt if character received
GoSub INIT 'Initialize variables
SO = 0 'supress object info display
BEGIN:
K$=Inkey$
If K$ = "~" Then GoTo SHUTDN 'Exit Code
Pause 200
If K$ = "c" Then GoTo KEYACT1 'Hot Key = Action
If K$ = "C" Then GoTo KEYACT1
'Additional action Keys
'can be added here
GoTo BEGIN
TITLE:
Cls
Print "        ******** Maximite GPS Astrofix  V1.1 ********     ";
If TZV = 0 Then Return
Print "Time Zone(";LTZ$;")"
Print@(1,420)"Press [C] to enter stellar co-ordinates, Press [~] to Exit"
Return
SHUTDN:
Close #1 'stop com port
Pin(19) = 0 'disable GPS module
End

KEYACT1:
Close #1 'stop com port
Print@(1,240) "Enter co-ordinates for celestial object"
Print "Right Ascension?"
Input"Hours";A1
Input"Minutes";B1
Input"Seconds";C1
ORA = A1*3600+B1*60+C1 'Object RA seconds
Print "Declination? ";
Input"+ or -";X$ 'north or south of celestial equator
If X$ = "+" Then X = 1
If X$ = "-" Then X = -1
Input"Degrees";A2
Input"Minutes";B2
Input"Seconds";C2
ODE = (A2+B2/60+C2/3600)*X 'Object Declination in degrees
SO = 1 'Allow object info display
GoSub TITLE
Open "Com1:4800,100,HANDL1" As #1 'start com port
GoTo BEGIN

INIT:
For I = 1 To 10 'Clear parameter array strings
Para$(I) = ""
Next I
J = 1 'initial parameter counter
CT = 0 'initial collect trigger
MO = 0 'month
Return

HANDL1:
C$ = Input$(1,#1)
If CT = 1 Then GoTo COLL '1 = Collect Chars.
If C$ = "$" Then CT = 1 ' Collect trigger
GoTo ERT 'exit
COLL:
If C$ = "," Then GoTo NPAR ' inc J for next parameter
If C$ = "*" Then GoTo ESTR 'End of string go check
Para$(J) = Para$(J) + C$
GoTo ERT 'exit
NPAR:
J = J + 1
If J = 11 Then GoTo ESTR 'enough go check Para string
GoTo ERT 'exit
ESTR:
If Para$(1) ="GPRMC" Then GoTo RMC
If Para$(1) ="GPGGA" Then GoTo GGA
If Para$(1) ="GPGSV" Then GoTo GSV
IERT: GoSub INIT 'initial
ERT: IReturn

'Process RMC string - calculate Mean Sidereal Time
'example string..
'$GPRMC,114041.000,A,3750.5790,S,14510.0725,E,0.24,31.06,260814,,*2F
RMC:
If Para$(3)<>"A" Then GoTo IERT 'initial and exit
Print@(1,240)"                                                  "
YR = Val(Right$(Para$(10),2)) 'get date and time
MO = Val(Mid$(Para$(10),3,2))
DY = Val(Left$(Para$(10),2))
HR = Val(Left$(Para$(2),2))
MN = Val(Mid$(Para$(2),3,2))
SE = Val(Mid$(Para$(2),5,2))
'Code to manipulate co-ordinates
If Para$(7) = "E" Then TZA = 1 'Time zone adjust
If Para$(7) = "W" Then TZA = -1
LLG = Val(Left$(Para$(6),3)) + Val(Mid$(Para$(6),4,7))/60
LLG = LLG * TZA 'Local Longitude
ADJL = 86400 * LLG/360 'Seconds adjust for local longitude
If Para$(5) = "N" Then DEC = 1
If Para$(5) = "S" Then DEC = -1
LLT = Val(Left$(Para$(4),2)) + Val(Mid$(Para$(4),3,7))/60
LLT = LLT * DEC 'Local Latitude
'Code to calculate local Solar and Sidereal time
LYR = Int(YR/4) + 1 'leap years
NYR = YR - LYR 'normal years
LORG = NYR*NYL + LYR*LYG 'calc sidsecs lost or gained since 00hr 1 Jan 2000
NREF = LORG + REF 'new reference for 00hr 1 Jan current year
DYS = M(MO) + DY -1 ' add days to date up to 00hrs
If Int(YR/4) = YR/4 And MO>2 Then DYS = DYS + 1 'Allow for Leap day
GMTS = HR*3600 + MN*60 + SE 'Greenwich Mean Time Seconds
T = GMTS + LTZ*3600 'Local Mean Solar Time Seconds
GoSub S2HMS
S$ = Format$(S,"%02.0f")
Print@(1,20)"   Local Solar Time = ";H$;":";M$;":";S$
SSEL = GMTS * CF 'Sidsecs elapsed since 00hr today
SSEL = SSEL + ADJL 'adjust for longitude
SSEA = DYS * 236.5554 'sid secs advanced from 00hr Jan 1
T = NREF + SSEA + SSEL 'starting point plus elapsed
GoSub S2HMS
Print@(1,40)"Local Sidereal Time =";H;" Hrs";M;" Mins ";S$;" Secs  "
Print@(1,60);
LLT$ = Format$(Abs(LLT),"%2.3f")
Print"Local Position = ";LLT$;" Deg ";Para$(5);" ";LLG;" Deg ";Para$(7);"  "
Print
If SO = 0 Then GoTo IERT
LRA = H*3600+M*60+S 'local RA secs
OML = ORA-LRA 'object RA minus local RA
T = Abs(OML)
GoSub S2HMS
If OML>0 Then Y$ = "East" Else Y$ = "West"
Print@(1,100)"Object Co-ordinates-"
Print
Print "RA = ";A1;" Hrs";B1;" Mins";C1;" Secs"
Print "DEC = ";A2;" Degs";B2;" Mins";C2;" Secs"
Print@(1,160)"Object is-"
Print
Print H$;" Hrs ";M$;" Mins ";S$;" Secs ";Y$;" of local meridian  "
ANG = ODE-LLT
POM = ODE*LLT 'plus or minus
If ANG < 0 Then Z$ = "South" Else Z$ = "North"
DOA = Abs(ANG) 'Degrees of angle
MOA = (DOA - Int(DOA))*60 'Minutes of angle
SOA = (MOA - Int(MOA))*60 'Seconds of angle
DOA = Int(DOA)
MOA = Int(MOA)
SOA = Cint(SOA*10)/10
Print "On arc )";DOA;" Degs";MOA;" Mins";SOA;" Secs ";Z$;" of zenith"
If ANG >= 90 Or ANG <= -90 Then GoTo OBNR 'object never rises
ELA = 90-Abs(ANG) 'object elevation at local meridian
WAD = 90-Abs(ODE) 'working angle degs
WAR = Rad(WAD) 'radians
LLR = Rad(Abs(LLT)) 'local latitude radians
HYP = Sin(WAR) 'some trigonometry
ADJ = Cos(WAR)
ADJ = ADJ*Tan(LLR)
If ADJ>HYP Then GoTo OBNS 'object never sets
OPP = Sqr(HYP^2-ADJ^2) 'Pythagoras
If ADJ = 0 Then GoTo EQUA 'Dec or Lat = 0 degs.
VAR = Atn(OPP/ADJ) '+/- visible hour angle radians
VAD = Deg(VAR) 'degs.
If POM >= 0 Then VAD = 180-VAD 'angle > 90
GoTo RESULT
OBNR:
Print "Object never rises, not visible from this location."
GoTo REND
OBNS:
Print "Object never sets, always visible in night sky."
GoTo REND
RESULT:
RAS = VAD/15*3600 'RA secs
DIF = Abs(OML)-RAS 'difference in RA secs
If DIF>0 Then GoTo OBNV 'not visible
If OML > 0 Then T = Abs(OML) + RAS Else T = Abs(DIF)
GoSub S2HMS
Print "Visible in night sky, setting in ";H$;" Hrs ";M$;" Mins ";S$;" Secs"
GoTo REND
OBNV:
If OML>0 Then GoTo OYTR 'object yet to rise
Print "Not currently visible, already set.        "
GoTo REND
OYTR:
T = Abs(DIF)
GoSub S2HMS
Print "Not visible, rising in ";H$;" Hrs ";M$;" Mins ";S$;" Secs  "
'Code required for bearing and elevation
REND:
'allow add code before routine end
GoTo IERT

S2HMS:
H = T/3600
M = (H-Int(H))*60
S = (M-Int(M))*60
H = Int(H)
H = (H/24 - Int(H/24)) * 24 'Modulus 24
M = Int(M)
S = Cint(S*10)/10
H$ = Format$(H,"%02.0f")
M$ = Format$(M,"%02.0f")
S$ = Format$(S,"%02.1f")
Return

GSV:
Print@(1,380)"Satilites available ";Para$(4)", "
GoTo IERT

GGA:
Print@(150,380)"Using ";Para$(8);"  "
GoTo IERT

