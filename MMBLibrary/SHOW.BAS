' * Show command v1.1 by Juri Fossaroli *
a$=MM.CmdLine$
If a$="" Then Input "input filename to show";a$
If a$="/?" Or a$="/i" Or a$="/I" Then
  ? "Show command v1.1 by Juri Fossaroli"
  ? "Use this command to print text files on screen":?
  ? "Usage:":?:?">Show [option] filename":?
  ? "Options:":?
  ? "/? or /i or /I : this help"
  ? "/c or /C       : display text continuosly, normally text is showed with"
  ? "                 a pause every page"
  ? "/h or /H       : display the file in Hex format (alway paused)":?
  ? "if no filename is specified then you'll be asked for one, you can use an"
  ? "option too placing it before the filename"
  ? "If no options and the file is a BMP file it will be displayed":?:End
EndIf
opt$=Left$(a$,3):pause=1:hexmode=0:max=2800:blip=0:ln=0:ext=1:ext$=Right$(a$,4)
If opt$="/c " Or opt$="/C " Then pause=0:a$=Right$(a$,Len(a$)-3):ext=0
If opt$="/h " Or opt$="/H " Then b$="":max=0:hexmode=1:a$=Right$(a$,Len(a$)-3)
If ext=1 And LCase$(ext$)=".bmp" And max>0 Then
       LoadBMP a$
       GoSub waitkey
       End
EndIf
Open a$ For input As 1
If hexmode=1 GoTo hxx
Do
a$=Input$(1,#1)
If a$=Chr$(13) Then max=max-(79-Pos)
max=max-1:? a$;
If pause=1 And max=0 Then GoSub waitkey
If max=0 Then max=2800
If Eof(#1)=-1 Then Close 1:End
Loop

hxx:
maxx=0
Do
a$=Input$(1,#1):z$="":zz$=""
If maxx <4096 Then z$="0"
If maxx <256 Then z$="00"
If maxx <16 Then z$="000"
If max=0 Then Font 1,,1:? " 0x"+z$+Hex$(maxx)+Tab(2);:Font 1,,0:? " : ";
If Asc(a$)<16 Then zz$="0"
? zz$+Hex$(Asc(a$))+" ";
If Asc(a$)>127 Then ot=1
a$=Chr$(Asc(a$) And 127)
If Asc(a$)=127 Then a$=" "
If Asc(a$)<32 Then a$=" "
If ot=1 Then a$=Chr$(128)+Chr$(199)+a$+CLR$(7,0):ot=0
b$=b$+a$:max=max+1:If max=16 Then max=0:? Chr$(34)+b$+Chr$(34):b$="":ln=ln+1
If Eof(#1)=-1 And max<16 Then ? Tab(60)Chr$(34)+b$+Chr$(34):Close 1:End
If Eof(#1)=-1 Then Close 1:End
If ln=35 Then ln=0: GoSub waitkey
maxx=maxx+1
Loop

waitkey:
If blip=1 Then Print @(0,35*12)Space$(29)"Press a key to continue"Space$(28)
If blip=500 Then Print @(0,35*12)Space$(60)
If blip=999 Then blip=0
blip=blip+1:If Asc(Inkey$)=0 GoTo waitkey
? @(0,34*12)"":Return

