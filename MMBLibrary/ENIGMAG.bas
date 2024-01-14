  'Enigma Pseudo-random Setup Generator
  'Created by Hugh Buckle May 2015
  'For use with Ray Alger's ENIGMA.bas and ENIGMAD.bas
  
  dim RD$(8),RD(8),A(26)
  RD$(1)="  I  ":RD$(2)=" II  ":RD$(3)=" III ":RD$(4)=" IV  "
  RD$(5)="  V  ":RD$(6)=" VI  ":RD$(7)=" VII ":RD$(8)=" VIII"
  Letters$="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  
  Initialize
  ?
  'Set the reflector to B or C
  ? "Reflector     = " chr$(int(rnd()*2 + 66))
  SetRo RO$
  ? "Rotor order   = " RO$
  SetRings Ring$
  ? "Ring setup    = " Ring$
  SetPlugBoard PB$
  ? "Plug board    = " PB$
  SetRotors RS$
  ? "Rotor setting = " RS$
  SetMsgKey MK$
  ? "Message Key   = " MK$
  
Sub Initialize
  'Get seed and randomise
  local i,b,a$
  input "seed"; a$
  For i=1 to len(a$)
    b=b + Asc(mid$(a$,i,1))
  next
  randomize b
end sub 'Initialize
  
Sub SetRo(RO$)
' Set the wheel order
  local i,j
  i=int(rnd()*2+1)
  if i=1 then RO$="beta " else RO$="gamm "
  for i=1 to 3
  'Sets a unique rotor number (3 out of 8)
    do
      j=int(rnd()*8+1)
    loop until rd(j)=0
    rd(j)=1
    RO$=RO$+rd$(j)
  next
end Sub 'WO
  
Sub SetRings(Ring$)
'Set each of the rings
  local i,j
  for i= 1 to 4
    j=int(rnd()*26+1)
    a$=mid$(Letters$,j,1)
    Ring$=Ring$+"  "+A$+"  "
  next
End Sub 'SetRings
  
sub SetPlugBoard(Plugs$)
'Select plugboard pairs
  local L$,NumPlugs,LettersAvail,i,j
  NumPlugs=int(rnd()*10+1)
  L$=Letters$
  LettersAvail=26
  for i=1 to NumPlugs
    GetLetter(a$,L$,LettersAvail)
    Plugs$=Plugs$+A$
    GetLetter(a$,L$,LettersAvail)
    Plugs$=Plugs$+A$+" "
  Next
End Sub 'Set PlugBoard
  
Sub SetRotors(RS$)
'Create rotor initial settings
  local i,j
  for i= 1 to 4
    j=int(rnd()*26+1)
    a$=mid$(Letters$,j,1)
    RS$=RS$+"  "+A$+"  "
  next
End Sub 'SetRings
  
Sub SetMsgKey(MK$)
'Set a message key
  For i=1 to 4
    j=int(rnd()*26+1)
    a$=mid$(Letters$,j,1)
    MK$=MK$+A$
  next
  MK$=MK$+" "
  For i=1 to 4
    j=int(rnd()*26+1)
    a$=mid$(Letters$,j,1)
    MK$=MK$+A$
  next
end Sub 'SetMsgKey
  
Sub GetLetter(a$,L$,LettersAvail)
'Selects a unique letter from the alphabet
  local j
  j=int(rnd()*LettersAvail+1)
  A$=Mid$(L$,j,1)
  if j>1 and j<Len(L$) then
    L$=Left$(L$,j-1)+Mid$(L$,j+1)
  else
    if j=1 then
      L$=Mid$(L$,2)
    else
      L$=Left$(L$,Len(L$)-1)
    Endif
  endif
  LettersAvail=LettersAvail-1
End Sub 'Get letter
