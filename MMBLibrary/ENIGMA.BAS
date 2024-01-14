  'ENIGMA for Maximite
  'written by Ray Alger April 2015
 'Output file added by Hugh Buckle May 2015
  OFileName$="EnigOut.txt"
  
  Dim RD$(10):Dim D$(4):Dim R(4):Dim V(5):Dim RS(4):Dim RP(4)
  Dim PB(26):Dim PBD(26):Dim PBS(26):Dim REF(26):Dim KO(8,2)
  Dim ROT$(10):Dim REF$(2):Dim FOR0(26):Dim REV0(26):Dim FOR1(26)
  Dim REV1(26):Dim FOR2(26):Dim REV2(26):Dim FOR3(26):Dim REV3(26)
  
  'Rotor display data
  RD$(1)="  I ":RD$(2)=" II ":RD$(3)=" III":RD$(4)=" IV ":RD$(5)="  V "
  RD$(6)=" VI ":RD$(7)=" VII":RD$(8)="VIII":RD$(9)="beta":RD$(10)="gamm"
  
  'rotor knock on data
  KO(1,1)=16:KO(2,1)=4:KO(3,1)=21:KO(4,1)=9:KO(5,1)=25
  KO(1,2)=16:KO(2,2)=4:KO(3,2)=21:KO(4,2)=9:KO(5,2)=25
  KO(6,1)=25:KO(7,1)=25:KO(8,1)=25
  KO(6,2)=12:KO(7,2)=12:KO(8,2)=12
  
  'Rotor data
  ROT$(1)="EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'I
  ROT$(2)="AJDKSIRUXBLHWTMCQGZNPYFVOE" 'II
  ROT$(3)="BDFHJLCPRTXVZNYEIWGAKMUSQO" 'III
  ROT$(4)="ESOVPZJAYQUIRHXLNFTGKDCMWB" 'IV
  ROT$(5)="VZBRGITYUPSDNHLXAWMJQOFECK" 'V
  ROT$(6)="JPGVOUMFYQBENHZRDKASXLICTW" 'VI
  ROT$(7)="NZJHGRCXMYSWBOUFAIVLPEKQDT" 'VII
  ROT$(8)="FKQHTLXOCBJSPDZRAMEWNIUYGV" 'VIII
  ROT$(9)="LEYJVCNIXWPBQMDRTAKZGFUHOS" 'beta
  ROT$(10)="FSOKANUERHMBTIYCWLQPZXVGJD" 'gamma
  
  'Reflector data
  REF$(1)="ENKQAUYWJICOPBLMDXZVFTHRGS" 'Thin B
  REF$(2)="RDOBJNTKVEHMLFCWZAXGYIPSUQ" 'Thin C
  
  'Plug board data
  PB$ ="QWERTZUIOASDFGHJKPYXCVBNML"
  For J=0 To 25:PBD(J)=Asc(Mid$(PB$,J+1,1))-65:Next J
  DAY$="C VI I VII XEL ML YE JH WQ"
  TRI$="JEX GFE"
  CIPHER$="YKDMD SZZPA BFCMJ EZIGN MSROU GPDSO HHSIU JJLSC NBBTY UOTX"
  
Function MOD26(X)
  X=Cint(X) 'fix error
  X=26*(X/26-Int(X/26))
  If X<0 Then X=X+26
  MOD26=X
End Function
  
  'Screen setup
START: Cls:Print" **********  Maximite M4 ENIGMA   **********"
  Print@(0,15)"REFLECTOR ->"
  Print@(0,30)"ROTORS USED  -->"
  DP=30
  GoSub DRAW
  Print@(0,60)"RING SETTINGS ->"
  DP=60
  GoSub DRAW
  Print@(20,120)"ROTORS -->"
  DP=120
  GoSub DRAW
  Line(125,95)-(130,155),,B:Line(175,95)-(180,155),,B
  Line(225,95)-(230,155),,B:Line(275,95)-(280,155),,B
  For J=1 To 6
    L=J*10
    Line(126,90+L)-(129,90+L):Line(176,90+L)-(179,90+L)
    Line(226,90+L)-(229,90+L):Line(276,90+L)-(279,90+L)
  Next J
  Line(1,325)-(330,390),,B
  Print@(36,385)"PLUG BOARD"
  For J=0 To 25:PB(J)=J:Next J 'Init Plug array
  For J=0 To 25:PBS(J)=0:Next J 'Init Plug status
  GoSub SHOW
  Line(1,175)-(331,240),,B
  Print@(36,235)"LAMP BOARD"
  CODE=26 'no lamp
  GoSub LAMP
  GoSub REFSET
  GoSub ROTSET
  GoSub RINGSET
  GoSub PLUGSET
  GoSub SETROT
  'HFBmod 20150509 start - add output file
    gosub OpenOutFile
  'HFBmod 20150509 end
  
  'rotor offset arrays
  For J=0 To 25
    T=Asc(Mid$(ROT$(R(0)),J+1,1))-65
    U=Asc(Mid$(ROT$(R(1)),J+1,1))-65
    V=Asc(Mid$(ROT$(R(2)),J+1,1))-65
    W=Asc(Mid$(ROT$(R(3)),J+1,1))-65
    FOR0(J)=T-J
    REV0(T)=J-T
    FOR1(J)=U-J
    REV1(U)=J-U
    FOR2(J)=V-J
    REV2(V)=J-V
    FOR3(J)=W-J
    REV3(W)=J-W
  Next J
  
  'knock on data
  KO3A=KO(R(3),1):KO3B=KO(R(3),2)
  KO2A=KO(R(2),1):KO2B=KO(R(2),2)  

  'refelector data
  For J=0 To 25:REF(J)=Asc(Mid$(REF$(RF),J+1,1))-65:Next J
  Print@(0,250)"Text IN-"
  Print@(0,280)"Text OUT-"
CLRPAD: PL$="":CI$="":CCNT=0:TB=0
  Print@(0,265)"                                                                 "
  Print@(0,295)"                                                                 "
  Print@(0,410,2)"Enter text, Press[Esc] to change settings     "
ENCRYPT:
  TB=TB+1:If TB>60 Then GoTo PADFUL
  GoSub LETTER
  If K=27 Then GoTo ESCAPE
  If K>25 Then GoTo ENCRYPT
  'rotor steps
  If RP(3)=KO3A Or RP(3)=KO3B Then GoTo ADV2
  If RP(2)=KO2A Or RP(2)=KO2B Then GoTo ADV1 ' R2 double step
  GoTo ADV3
ADV1: RP(1)=RP(1)+1
  If RP(1)>25 Then RP(1)=RP(1)-26
ADV2: RP(2)=RP(2)+1
  If RP(2)>25 Then RP(2)=RP(2)-26
ADV3: RP(3)= RP(3)+1
  If RP(3)>25 Then RP(3)=RP(3)-26
  'rotor display
  DP=120:D$(1)="  "+Chr$(RP(1)+65)+" "
  D$(2)="  "+Chr$(RP(2)+65)+" ":D$(3)="  "+Chr$(RP(3)+65)+" "
  GoSub VIEW
  
  'scramble
  PL$=PL$+Chr$(K+65)
  CODE=PB(K)
  X=MOD26(CODE+RP(3)-RS(3))
  X=MOD26(CODE+FOR3(X))
  CODE=X
  X=MOD26(X+RP(2)-RS(2))
  X=MOD26(CODE+FOR2(X))
  CODE=X
  X=MOD26(X+RP(1)-RS(1))
  X=MOD26(CODE+FOR1(X))
  CODE=X
  X=MOD26(X+RP(0)-RS(0))
  X=MOD26(CODE+FOR0(X))
  CODE=REF(X) 'Reflect
  X=MOD26(CODE+RP(0)-RS(0))
  X=MOD26(CODE+REV0(X))
  CODE=X
  X=MOD26(CODE+RP(1)-RS(1))
  X=MOD26(CODE+REV1(X))
  CODE=X
  X=MOD26(X+RP(2)-RS(2))
  X=MOD26(CODE+REV2(X))
  CODE=X
  X=MOD26(X+RP(3)-RS(3))
  X=MOD26(CODE+REV3(X))
  CODE=PB(X)
  CI$=CI$+Chr$(CODE+65)
  
  'HFBmod 20150509 start - add output file
  'Write encrypted code to output file in groups of 5 characters
  'New line after 12 character groups
  Print #1,Chr$(CODE+65);
  IO=IO+1
  if IO>4 then ' Space after 5 characters
    IO=0
    IG=IG+1
    If IG>12 then
      IG=0
      Print #1, Chr$(13) ' new line
    else
      Print #1, " ";
    endif
  endif
  
  'HFBmod 20150509 end

  GoSub LAMP
  CODE=26 'no lamp
  Pause 500
  GoSub LAMP
  Print@(0,265) PL$
  Print@(0,295) CI$
  CCNT=CCNT+1:If CCNT<5 Then GoTo ENCRYPT
  CCNT=0
  PL$=PL$+" "
  CI$=CI$+" "
  GoTo ENCRYPT
  
ESCAPE:
  Print@(0,410,2)"Press [A] to adjust rotors, [S] to change setup, [Esc] to exit"
  GoSub LETTER
  'HFBmod 20150509 start - add output file
  If K=18 Then 'Chr$(18+65)= "S"
    Close #1
    GoTo START
  endif
  If K=27 Then ' Esc key
    Close #1
end
  endif
  'HFBmod 20150509 end
  If K<>0 Then GoTo ESCAPE
  Print@(0,410)"                                                              "
  GoSub ADJROT
  'HFBmod 20150509 start - add output file
  'Clear the output file after the rotors are adjusted
  Close #1
  gosub OpenOutFile
  'HFBmod 20150509 end
  GoTo CLRPAD
  
  'HFBmod 20150509 start - add output file
OpenOutFile:
  Open OFileName$ for output as #1
  IO=0:IG=0  ' reset output character and group counts
Return
  'HFBmod 20150509 end

PADFUL:
  Print@(0,410,2)"Pad Full, copy message, [Enter] to Continue   "
  GoSub LETTER
  If K<> 28 Then GoTo PADFUL
  'HFBmod 20150509 start - add output file
  print #1, Chr$(13) ' new line in output file
  IO=0:IG=0
  'HFBmod 20150509 end
  GoTo CLRPAD
  
REFSET:
  Print@(80,15,2)"Select the Reflector (B or C)"
  GoSub LETTER
  If K<1 Or K>2 Then GoTo REFSET
  RF=K
  Print@(80,15)Chr$(K+65);"                             "
  Return
  
ROTSET:
  For J=0 To 3
    D$(J)="    ":R(J)=0:V(J)=0
  Next J
  J=9:V(0)=2:DP=30
  Print@(0,45,2)"Press [Space] to change Rotor, [Enter] for next Rotor"
ROTOR0:
  GoSub VIEW
  GoSub LETTER
  If K<>26 Then GoTo CRR0
  D$(0)=RD$(J)
  J=J+1:If J>10 Then J=9
  GoTo ROTOR0
CRR0: If K<>28 Then GoTo ROTOR0
  If D$(0)="    " Then GoTo ROTOR0
  J=J-1:If J=8 Then J=10
  R(0)=J:V(0)=0:V(1)=2:J=1:I=1
ROTOR:
  GoSub VIEW
  GoSub LETTER
  If K<>26 Then GoTo CRR1
  If J=R(1) Or J=R(2) Then GoTo NXT1
  GoTo SKIP
NXT1: J=J+1:If J>8 Then J=1
  If J=R(1) Or J=R(2) Then GoTo NXT1
SKIP: D$(I)=RD$(J)
  J=J+1:If J>8 Then J=1
  GoTo ROTOR
CRR1: If K<>28 Then GoTo ROTOR
  If D$(I)="    " Then GoTo ROTOR
  J=J-1:If J=0 Then J=8
  R(I)=J:V(I)=0:V(I+1)=2:J=1
  I=I+1:If I<4 Then GoTo ROTOR
  Print@(0,45)"                                                     "
  GoSub VIEW
  Return
  
RINGSET:
  Print@(0,75,2)"Rotor Ring Setting (A to Z), [Enter] for next Rotor"
  For J=0 To 3
    D$(J)="  A ":RS(J)=0:V(J)=0
  Next J
  J=0:V(0)=2:DP=60
RING:
  GoSub VIEW
  GoSub LETTER
  If K=28 Then GoTo CRS
  If K>25 Then GoTo RING
  D$(J)="  "+Chr$(K+65)+" "
  RS(J)=K 'offset 0-25
  GoTo RING
CRS: V(J)=0:V(J+1)=2
  J=J+1:If J<4 Then GoTo RING
  Print@(0,75)"                                                   "
  GoSub VIEW
  Return
  
PLUGSET:
  Print@(0,310,2)"PLUG ? To PLUG ?  ([Enter] when all done)"
PL1: GoSub LETTER
  If K=28 Then GoTo PRET
  If K>25 Then GoTo PL1
  Print@(30,310)Chr$(K+65)
  H=K
PL2: GoSub LETTER
  If K=28 Then GoTo PRET
  If K>25 Then GoTo PL2
  Print@(90,310)Chr$(K+65)
  Pause 50
  If K<>H Then GoTo SWAP
  PB(PB(H))=PB(H):PB(H)=H 'plugboard letter restore
SWAP: If PB(H)<>H Or PB(K)<>K Then GoTo FORBID
  PB(H)=K:PB(K)=H 'plugboard letter swap
  GoSub SHOW
  GoTo PLUGSET
FORBID:
  Print@(0,310)"NOT ALLOWED PLUG ALREADY USED!           "
  Pause 3000
  GoTo PLUGSET
PRET: Print@(0,310)"                                         "
  Return
  
SETROT:
  For J=0 To 3
    D$(J)="  A ":RP(J)=0:V(J)=0
  Next J
ADJROT:
  Print@(0,160,2)"Set Rotor Start Position (A-Z), [Enter] for next Rotor"
  J=0:V(0)=2:DP=120
ROTPOS:
  GoSub VIEW
  GoSub LETTER
  If K=28 Then GoTo CRP
  If K>25 Then GoTo ROTPOS
  RP(J)=K
  D$(J)="  "+Chr$(RP(J)+65)+" "
  GoTo ROTPOS
CRP: V(J)=0:V(J+1)=2
  J=J+1:If J<4 Then GoTo ROTPOS
  Print@(0,160)"                                                      "
  GoSub VIEW
  Return
  
VIEW:
  Print@(100,DP,V(0))D$(0):Print@(150,DP,V(1))D$(1)
  Print@(200,DP,V(2))D$(2):Print@(250,DP,V(3))D$(3)
DRAW:
  Line(99,DP-1)-(124,DP+11),,B:Line(149,DP-1)-(174,DP+11),,B
  Line(199,DP-1)-(224,DP+11),,B:Line(249,DP-1)-(274,DP+11),,B
  Return
  
SHOW:
  For J = 0 To 8
    If PB(PBD(J))<>PBD(J) Then V=2 Else V=0
    Print@((J)*36+7,330,V) "[";Chr$(PBD(J)+65);Chr$(PB(PBD(J))+65);"]"
  Next J
  For J = 0 To 7
    If PB(PBD(J+9))<>PBD(J+9) Then V=2 Else V=0
    Print@((J)*36+17,350,V) "[";Chr$(PBD(J+9)+65);Chr$(PB(PBD(J+9))+65);"]"
  Next J
  For J = 0 To 8
    If PB(PBD(J+17))<>PBD(J+17) Then V=2 Else V=0
    Print@((J)*36+2,370,V) "[";Chr$(PBD(J+17)+65);Chr$(PB(PBD(J+17))+65);"]"
  Next J
  Return
  
LETTER: 'Wait for key press
  K$=Inkey$
  If K$="" Then GoTo LETTER
  K=Asc(K$)
  If K=13 Then K=28 'CR
  If K=32 Then K=26 'SP
  If K>25 And K<29 Then GoTo LRET
  If K>64 And K<91 Then GoTo UPC 'ucase
  If K>96 And K<123 Then GoTo LOC 'lcase
  GoTo LETTER
LOC: K=K-32 'conv to ucase
UPC: K=K-65 'bound 0 to 25
LRET: Return
  
LAMP:
  For J = 0 To 8
    If PBD(J)=CODE Then V=2 Else V=0
    Print@((J)*36+10,180,V) "(";Chr$(PBD(J)+65);")"
  Next J
  For J = 0 To 7
    If PBD(J+9)=CODE Then V=2 Else V=0
    Print@((J)*36+20,200,V) "(";Chr$(PBD(J+9)+65);")"
  Next J
  For J = 0 To 8
    If PBD(J+17)=CODE Then V=2 Else V=0
    Print@((J)*36+5,220,V) "(";Chr$(PBD(J+17)+65);")"
  Next J
  Return
