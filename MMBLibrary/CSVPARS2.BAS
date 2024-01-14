  
  ' A version of CSVPARS by TZAdvantage
  ' March 2013
  ' A function to parse a CSV string
  
  'Required Global variable for GetFieldArray function
  'Dimension this variable so that it can hold all values
  Dim GetFieldArray$(12)
  ' Value is a minimal GPS record
  Value$ =  "$GPRMC,123519,A,4807.038,N,01131.000,E,022.4,084.4,230394,0 03.1,W*6A"
  
  Cls
  Print "GPS Record:"
  Print Value$
  Print
  
  N = GetFieldArray(Value$)
  'A list of all values
  Print "List of all fields"
  Print "------------------"
  For Index = 0 To N - 1
    Print Index, GetFieldArray$(Index)
  Next
  Print "------------------"
  Print
  
  'or directly use a field
  Speed = Val(GetFieldArray$(7))
  Print "A field used by its index"
  Print "-------------------------"
  Print "Speed = " Speed
  Print "-------------------------"
  
  End
  
  
  '  =============================================================================
  ' GetFieldArray
  ' A function to split a string into an Array of fields
  '
  ' Author:  TZ Advantage
  ' Date:    5 march 2013
  ' Version: 1.0
  '
  ' This function requires the use of a global variable
  ' named GetFieldArray$(). It should be dimensioned large
  ' enough to contain all fields.
  '
  ' Best used in situations when all fields are needed as
  ' it will be the fastest.
  ' Consider using the function GetField()
  ' when only one or two fields are needed or speed is of no concern
  '
  ' Function parameters:
  ' Record$:    A string containing the delimited fields
  ' Delimiter$: (Optional) delimiter character, when omitted a comma is used
  ' KeepQuotes: (Optional) [0|1]
  '             Use value 1 if double quotes around field values have to be kept,
  '             when omitted or value 0 is used double quotes are discarded
  '  =============================================================================
Function GetFieldArray( Record$, Delimiter$, KeepQuotes )
  Local Index, Char, InQuote, Count
  
  InQuote = 0
  Count = 0
  
  ' If no delimiter is provided use a comma
  If Delimiter$ = "" Then Delimiter$ = ","
  
  ' Loop through all the characters
  For Index = 1 To Len(Record$)
    Char = Asc(Mid$(Record$, Index, 1))
    
    If Char = 34 Then
      ' Flip InQuote between 0 and 1.
      ' A 1 will signal that the next characters
      ' are within quotes and delimiters should be ignored
      InQuote = Not InQuote
    EndIf
    
    ' Only check for delimiters when not within two quotes
    If Not InQuote And Instr(Delimiter$, Chr$(char)) >= 1  Then
      Count = Count + 1
    Else
      
      ' Only collect characters for the requested field.
      'Add the character and only add quotes when KeepQuotes is 1
      If Char <> 34 Or KeepQuotes Then
        GetFieldArray$(Count) = GetFieldArray$(Count) + Chr$(char)
      EndIf
    EndIf
  Next
  
  GetFieldArray = Count + 1
End Function
