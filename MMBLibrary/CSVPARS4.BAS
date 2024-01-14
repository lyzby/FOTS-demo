  ' CSV Parse by Hugh Buckle
  ' March 2013
  ' Parses a CSV string into an array
  ' It is intended to be used with comma or tab delimiter.
  ' Loads each variable from a$ into array CSV$()
  ' Maximum size of the array is preset in CSVMax
  '
  ' Limitations:
  ' - Assumes no quoted strings
  ' - Consecutive delimiters leave a nul entry in the array

  ' Test using comma delimiter
  CSVDelimiter$=","
  a$="one,%,-,two,three,Now is the time,,,/p,1234.56"

  CSVMax=12                ' Get the first 12 values from the CSV file
  Dim CSV$(CSVMax)
  ParseCSV(a$,CSVDelimiter$,CSVMax)
  Print "Cell","Value"
  Print "----","-----"
  For i=1 To CSVMax
    ? i,CSV$(i)
  Next
End

Sub ParseCSV(p$,d$,n)
  ' Parse the CSV valuess into array CSV$()
  ' Using d$ as the delimiter
  Local i
  Strt=1
  For i=1 To n
    Ptr=Instr(Strt,p$,d$)
    If Ptr=0 Then Ptr=Len(a$)+1         'Last item has no delimiter
    CSV$(i)=Mid$(p$,Strt,Ptr-Strt)
    If Ptr>=Len(p$) Then Exit For
    Strt=Ptr+1
  Next
End Sub

