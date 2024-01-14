  ' A version of CSVPARS by Andrew Rich VK4TEC
  ' March 2013
  ' Subroutine to parse a file of CSV data
  ' The relevant data must start with $ and end with *

  Open "CSVPars3.tst" For input As 1
  Max=10
  Dim Arg$(10)
  nmea_sentence
  For i=1 To 10
    Print i, Arg$(i)
  Next
  Close #1
  End


Sub nmea_sentence
  Do
    msg$ ="$" ' subroutine start
    Do While Input$(1, #1) <> "$" : Loop ' wait for the start
    For i = 0 To max
      arg$(i) = "" ' clear ready for data
      Do ' loops until a specific exit
        x$ = Input$(1, #1)
        ' Print x$
        msg$ = msg$ + x$ ' get the character
        If x$ = "," Then Exit ' new data field, increment i
        If x$ = "*" Then Return ' we have all the data so return
        arg$(i) = arg$(i) + x$
        'Print arg$(i) ' add to the data
      Loop
      'Print arg$(i) ' loop back for the next char
    Next i ' move to the next data field
    ' Print "Corrupt data..." ' exceeded max data items
    'Print msg$
  Loop

End Sub
