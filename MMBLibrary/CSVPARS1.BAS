   ' CSV Parsing routine
   ' CircuitGizmos March 2013
   ' ------------------------
   ' A routine to parse a CSV string
   ' Optionally specify
   ' - index to the entry required
   ' - the delimiter
   ' - suppress quotes


   Print "CSV parsing test"
   Print

   Print "Data with space and comma"
   cline$ = "12 0 in,out "
   Print cline$
   Print "________"
   Print FIELD$( cline$, 0 )
   Print FIELD$( cline$, 1 )
   Print FIELD$( cline$, 2 )
   Print FIELD$( cline$, 3 )
   Print FIELD$( cline$, 4 )
   Print FIELD$( cline$, 5 )
   Print
   Print

   Print "Data with space, comma, and tab"
   cline$ = "12" + Chr$(9) + "0 in,out"
   Print cline$
   Print "________"
   Print FIELD$( cline$, 0 )
   Print FIELD$( cline$, 1 )
   Print FIELD$( cline$, 2 )
   Print FIELD$( cline$, 3 )
   Print FIELD$( cline$, 4 )
   Print FIELD$( cline$, 5 )
   Print
   Print

   Print "Data with just comma"
   Print cline$
   Print "________"
   Print FIELD$( cline$, 0, ",")
   Print FIELD$( cline$, 1, ",")
   Print FIELD$( cline$, 2, ",")
   Print
   Print

   Print "Data with quotes"
   cline$ = "12," + Chr$(34) + "0 in" + Chr$(34) + ",out,fred," + Chr$(34) + "zest step" + Chr$(34) + ",buh"
   Print CLINE$
   Print "________"
   Print FIELD$( cline$, 1 )
   Print FIELD$( cline$, 2 )
   Print FIELD$( cline$, 3 )
   Print FIELD$( cline$, 4 )
   Print FIELD$( cline$, 5 )
   Print FIELD$( cline$, 6 )
   Print
   Print

   Print "Data with quotes supressed"
   Print CLINE$
   Print "________"
   Print FIELD$( cline$, 2,,1 )
   Print FIELD$( cline$, 5,,1 )
   Print
   Print

   Print "Data with quotes"
   cline$ = Chr$(34) + "0 in" + Chr$(34) + ",out,fred," + Chr$(34) + "zest step" + Chr$(34)
   Print CLINE$
   Print "________"
   Print FIELD$( cline$, 1 )
   Print FIELD$( cline$, 2 )
   Print FIELD$( cline$, 3 )
   Print FIELD$( cline$, 4 )



   '----------------------------------------------------------- -------------
   '
   ' FIELD - CSV field parsing
   ' field_string$ = String to parse
   ' field_index   = Index to requested field
   ' field_delim$  = Optional field delimeters
   ' field_supress = Suppress quote in quoted field
   '
   ' Parsing a CVS string - CircuitGizmos March 2013
   '----------------------------------------------------------- -------------

Function FIELD$( field_string$, field_index, field_delim$, field_supress )
   Local field_loop
   Local field_dcount
   Local field_dloop
   Local field_length
   Local field_field$

   field_length = Len(field_string$)
   field_field$ = ""
   field_dcount = 1
   field_loop = 1

   ' Delimiter defaults: SPACE COMMA TAB
   If field_delim$ = "" Then field_delim$ = " ," + Chr$(9)

   Do
      field_delim = 0
      ' Process inside paren (no delimiters)
      If Mid$( field_string$, field_loop, 1 ) = Chr$(34) Then
         Do
            If field_dcount = field_index Then
               field_field$ = field_field$ + Mid$( field_string$, field_loop, 1 )
            EndIf
            field_loop = field_loop + 1
         Loop Until (Mid$( field_string$, field_loop, 1 ) = Chr$(34)) Or (field_loop > field_length)
      EndIf

      ' Search through delimiters
      For field_dloop = 1 To Len(field_delim$)
         ' If char is a delim
         If Mid$( field_string$, field_loop, 1 ) = Mid$( field_delim$, field_dloop, 1 ) Then
            field_dcount = field_dcount + 1
            field_delim = 1
         EndIf
      Next field_dloop

      ' Add to returned string if not delim
      If field_delim = 0 Then
         If field_dcount = field_index Then
            field_field$ = field_field$ + Mid$( field_string$, field_loop, 1 )
         EndIf
      EndIf

      field_loop = field_loop + 1
   Loop Until field_loop > field_length

   ' Suppress quote / return field
   For field_loop = 1 To Len(field_field$)
      If (Mid$( field_field$, field_loop, 1 ) = Chr$(34)) And (field_supress <> 0) Then
         '
      Else
         FIELD$ = FIELD$ + Mid$( field_field$, field_loop, 1 )
      EndIf
   Next field_loop

End Function
