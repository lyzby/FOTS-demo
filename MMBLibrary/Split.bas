  ' Subroutine to split a delimited string of characters into individual
  ' substrings and return those substrings in a string array.
  ' As you can't pass arrays in MM Basic, the returned array will always
  ' be named splitup$()
  ' The program automatically calculates the number of substrings in the string
  ' and the size of the largest substring, then scales the array accordingly.
  ' The number of substrings is returned in array position 0 and the maximum
  ' length of each substring is returned in array position 1
  
  Print "Testing code for split$ subroutine"
  Print
  Print "Test string is  'abc,defg,hijkl,+123.678,-5' with comma delimiter"
  a$="abc,defg,hijkl,+123.678,-5"
  Print "Usage is  SPLIT$ arg$,delimiter$"
  
  split$ a$,","
  
  Print "The splitup array that is created is a one dimensional array that is "
  Print "the number of substrings in the argument string + 2 in depth, "
  Print "each of which equals the largest substring in characters long"
  Print "Splitup$(0) has the number of substrings extracted in it"
  Print "Splitup$(1) is the maximum string length of any substring"
  Print "Splitup$(2) through Splitup$(n) contain the delimited substrings"
  Print " of the argument string"
  Print
  Print "Test run"
  Print "First substring of argument string at position 2 is ",splitup$(2)
  Print "Second substring of argument string at position 3 is ",splitup$(3)
  Print "Third substring of argument string at position 4 is ",splitup$(4)
  Print "Fourth substring of argument string at position 5 is ",splitup$(5)
  Print "Fifth substring of argument string at position 6 is ",splitup$(6)
  ' End of test code
  
  ' SPLIT subroutine
Sub split$(arg1$,arg2$)
  ' arg1$ is the string to be split
  ' arg2$ is the delimiter to split on
  Local whole$,delim$
  Local numels,maxlen,wholelen,ellen
  whole$=arg1$
  delim$=arg2$
  If Instr(whole$,delim$) <> 0 Then
    numels = 1                         ' delimiter found
    maxlen = 1
    Do While Instr(whole$,delim$) <> 0 ' now count up elements
      ellen = Instr(whole$,delim$)-1   ' and maximum size so we
      If ellen > maxlen Then
        maxlen = ellen                 ' can dimension array
      EndIf
      numels = numels + 1
      wholelen = Len(whole$)
      whole$ = Right$(whole$,wholelen-(ellen + 1))
    Loop
    If Len(whole$) > maxlen Then
      maxlen = Len(whole$)
    EndIf
    
    whole$ = arg1$
    Erase Splitup$
    Dim splitup$(numels+2) length maxlen
    splitup$(0) = Str$(numels)
    splitup$(1) = Str$(maxlen)
    For x = 2 To numels+1
      wholelen = Len(whole$)
      If x = numels+1 Then
        ellen = Len(whole$)
        splitup$(x) = whole$
      Else
        ellen = Instr(whole$,delim$) - 1
        splitup$(x)=Left$(whole$,ellen)
        whole$ = Right$(whole$,wholelen-(ellen+1))
      EndIf
    Next x
  Else
    Print "Error: No delimiter in string"
  EndIf
End Sub 
