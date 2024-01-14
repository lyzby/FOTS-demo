  ' MMBasic Program Source Formatter v2.3
  ' Hugh Buckle  March 2013 - updated April 2013
  
  '**********************************************************
  ' You can run this version from the prompt using the Implied RUN Command
  '
  '   FORMAT InFileName[.BAS] OutFileName[.BAS] [Indent] [/p]
  '
  ' You can use either a comma or space between parameters
  '
  '   Where: OutFileName must not be the same as InFileName
  '          Indent will default to 2 characters.
  '          /p lists the result to the screen - press a key to continue.
  '   You will be prompted if:
  '          The input file doesn't exist
  '          The output file exists or is the same as the input file
  '          Indent is outside the range 1 to 6.
  '
  ' You can also run the program in the normal way using the RUN command
  ' in which case you will be prompted for file names, indent and pause.
  
  ' FORMAT's standard set of rules:
  ' - Lines starting with a Label are aligned to the left margin
  '   as are SUB, END SUB, FUNCTION and END FUNCTION.
  ' - The first non-label line is indented to the first level.
  ' - Lines following a single line IF, DO and FOR are not further indented.
  ' - Multiline DO and FOR have their respective LOOP and NEXT aligned.
  '   Intervening lines are indented one level.
  ' - The NEXT that closes multiple levels of FOR is aligned
  '   under the first FOR.
  ' - Multiline IF statements have their ELSE, ELSEIF and ENDIF aligned
  '   under the relevant IF. Intervening statements are indented one level.
  ' - Nesting increases the indent one level.
  ' - For numbered programs, the left margin is the longest number plus one.
  ' - Other than setting the indent, the source line is not altered. However,
  '   if the line starts with a colon (after the line number and its optional
  '   colon), then it is removed.
  ' - No attempt is made to check the program for errors.
  '**********************************************************
  
  true=1
  false=0
  Restore 'JD Addition
  
  CMDParmFile$ = "Format.dat"  'File of command line parms in format
  'InFileName[.bas] OutFileName[.bas] [Indent] [/P]
  
  ' Data contains operators, used to differentiate
  ' between a label and any other statement.
  Data "=","+","-","*","/","<",">","\","^"
  Operators=9
  Dim Operator$(Operators)
  For I=1 To Operators
    Read Operator$(i)
  Next
  
  ' Data contains MMBasic commands which could be interpreted as labels
  Data "PRINT","RETURN","CLS","RESTORE","CLEAR","EXIT","FILES","MEMORY","NEW"
  Data "KEYDOWN","TROFF","TRON","DO","LOOP","NEXT"
  Commands=15
  Dim Command$(Commands)
  For i=1 To Commands
    Read Command$(i)
  Next
  
Mainline:
  '**********************************************************
  ' Initialisation gets input and output file names, indent and pause.
  ' Each line is read in turn and, working on a copy of the line,
  ' the indent of the current and next line is found. The original line
  ' is indented with an appropriate number of blanks and written out.
  ' If the first line contains a line number then the program skips through
  ' to the last line to find the maximum sized line number. Lines are then
  ' processed as before, adding a padded line number and indent to the code.
  '**********************************************************
  
  initialise(Ifile$, Ofile$, Indent)
  Print
  Print "Formatting ";ifile$;" and writing to ";Ofile$
  Print String$(78,"_")
  CheckForLineNumbers
  
  CLI = Indent                      'Current line indent
  NLI = Indent                      'Next line indent
  
  Do While Not Eof(#1)
    CLI = NLI
    Line Input #1, iline$              ' Get next line
    oline$ = StripBlanks$(iline$)      ' Strip off leading blanks
    If LineNumbers Then ExtractLineNo  ' Remove the line number
    cline$ = oline$                    ' Make working copy
    AdjustIndent                       ' Find current & next line indents
    oline$ = Space$(CLI) + Oline$      ' Indent current line
    If LineNumbers Then Oline$ = LineNo$ + Oline$
    Print #2, oline$                   ' Write to output file
    If PauseList$="Y" Then
      Print oline$                     ' Print a copy to screen
      wait                             ' Pause at a screenful
    Else
      Print ".";
    EndIf
  Loop
  Close #1
  Close #2
  Print
  Print String$(78,"_")
  Print "Formatting finished. Please test the output file."
  If MasterFile$ <> "" Then 'JD Addition
    UpTo = UpTo + 1
    Chain MasterFile$
  EndIf
  End 'Mainline
  
Function StripBlanks$(A$)
  '**********************************************************
  ' Strips blanks, tabs and colons from the beginning of the line.
  '**********************************************************
  Local i
  i=1
  Do While Mid$(A$,i,1)=" " Or Mid$(A$,i,1)=Chr$(9) Or Mid$(A$,i,1)=":"
    i=i+1
  Loop
  StripBlanks$ = Mid$(A$,i)  ' Copies from i to end of a$ to the function
End Function 'StripBlanks
  
Function Variable(a$,s)
  '**********************************************************
  ' Checks the character after a command name to see if it's a variable.
  ' s points to the character after the command name
  '**********************************************************
  Variable=True
  b$=Mid$(a$,s,1)
  If Len(a$)<s Or b$=" " Or b$=":" Then Variable=False
End Function 'Variable
  
Sub GetNextNonBlankChar(a$,p,b$)
  '**********************************************************
  ' Returns b$ either empty indicating end of line or
  ' containing the next non-blank character.
  ' A comment (single quote or REM) also indicates end of line.
  ' Any text inside double quotes is skipped.
  ' On entry, p points to the next char to inspect in a$.
  ' On exit, P points to the next character after the one in b$.
  '**********************************************************
  Do
    If p>=Len(a$) Or Mid$(a$,p,1)="'" Or UCase$(Mid$(a$,p,3))="REM" Then
      b$=""
    Else
      b$=Mid$(a$,p,1)
      If b$=Chr$(34) Then
        Do                         ' skip over quoted text
          p=p+1
        Loop Until Mid$(a$,p,1)=Chr$(34) Or p = Len(a$)
      EndIf
      p=p+1
    EndIf
  Loop Until b$="" Or b$<>" "
End Sub 'GetNextNonBlankChar
  
Sub ExtractLineNo
  '**********************************************************
  ' Saves the line number if present and pads it to
  ' the length of the longest one. If some of the code uses line
  ' numbers and some not, then those without line numbers are
  ' indented as if they had a line number.
  ' If the line number is followed by one or more spaces and
  ' a colon, the spaces between line number and colon are removed.
  '**********************************************************
  Local j
  If Left$(Oline$,1)>="0" And Left$(Oline$,1)<="9" Then
    LineNo$=Left$(Oline$,Instr(1,Oline$," "))
    j=Len(LineNo$)
    GetNextNonBlankChar(Oline$,j,b$)
    If b$=":" Then
      LineNo$=Left$(LineNo$,Len(LineNo$)-1)+": "
      Oline$=Mid$(Oline$,j)
    EndIf
    
    If Len(LineNo$) < LineNoLen Then ' pad to longest line no
      LineNo$ = LineNo$ + Space$(LineNoLen-Len(LineNo$))
    EndIf
    Oline$=Right$(Oline$,Len(Oline$)-Instr(1,Oline$," "))
    Oline$=StripBlanks$(Oline$)
  Else
    LineNo$=Space$(LineNoLen)
  EndIf
End Sub 'ExtractLineNo
  
Sub AdjustIndent
  '**********************************************************
  ' For each command that affects the indent,
  ' adjust both current (CLI) and next line indent (NLI).
  ' Repeat until comment or end-of-line reached to handle multi-statement lines.
  '**********************************************************
  EOL=False
  ForNest=False         'Nest level of FOR/NEXT
  DoFound=False
  Do
    Found=False
    Test_LineEnd(Found,EOL)
    If Not Found Then Test_Sub_Fn(Found)
    If Not Found Then Test_Do(Found)
    If Not Found Then Test_Loop(Found)
    If Not Found Then Test_For(Found)
    If Not Found Then Test_Next(Found)
    If Not Found Then Test_If_Then(Found,MultiLine)
    If Not Found Then Test_ElseIf(Found,MultiLine)
    If Not Found Then Test_Else(Found,MultiLine)
    If Not Found Then Test_EndIf(Found)
    If Not Found Then Test_for_label(Found)
    ' Look for other statements in the line
    If Not EOL Then
      FlushToColon
      Cline$=Stripblanks$(Cline$)
      If Cline$="" Then EOL=True
    EndIf
  Loop Until EOL
End Sub 'AdjustIndent
  
Sub Test_LineEnd(Found,EOL)
  '**********************************************************
  ' Test if finished processing this line
  '**********************************************************
  If Len(CLine$)<=1 Or Left$(Cline$,1)="'" Or Left$(UCase$(Cline$),4)="REM " Then
    Found=True
    EOL=True
  EndIf
End Sub 'Test_LineEnd
  
Sub FlushToColon
  '**********************************************************
  ' Find the next statement on a multi-statement line
  '**********************************************************
  Local i
  i=1
  Do
    GetNextNonBlankChar(Cline$,i,a$)
  Loop Until a$ = ":" Or a$ = ""
  If a$="" Then
    Cline$=""      ' reached EOL
  Else
    Cline$=Right$(Cline$,Len(Cline$)-i+1)
  EndIf
End Sub 'FlushToColon
  
Sub Test_Sub_Fn(Found)
  '**********************************************************
  ' SUB & FUNCTION start & end stmts are aligned to the left margin
  '**********************************************************
  If UCase$(Left$(Cline$,4))="SUB " Then
    Found = True
  ElseIf UCase$(Left$(Cline$,7))="END SUB" Then
    Found = True
  ElseIf UCase$(Left$(Cline$,9))="FUNCTION " Then
    Found = True
  ElseIf UCase$(Left$(Cline$,12))="END FUNCTION" Then
    Found = True
  EndIf
  If Found Then
    NLI = CLI       'save current indent
    CLI = 0         'Move this line to the left margin
  EndIf
End Sub 'Test_Sub_Fn
  
Sub Test_Do(Found)
  '**********************************************************
  'Print the DO at the current indent but indent the next line further
  '**********************************************************
  If UCase$(Left$(Cline$,2))="DO" And Not Variable(Cline$,3) Then
    NLI = NLI + Indent
    DoFound=True
    Found = True
  EndIf
End Sub 'Test_Do
  
Sub Test_Loop(Found)
  '**********************************************************
  'If LOOP, print this and the next line one less indent
  '**********************************************************
  If UCase$(Left$(Cline$,4))="LOOP" And Not Variable(Cline$,5) Then
    If DoFound Then
      NLI=CLI
    Else
      CLI = CLI - Indent   'remove one level of indent
      NLI = CLI            'make the next line indent the same
    EndIf
    Found = True
  EndIf
End Sub 'Test_Loop
  
Sub Test_For(Found)
  '**********************************************************
  'Print the FOR at the current indent but indent the next line one indent
  '**********************************************************
  If UCase$(Left$(Cline$,3))="FOR" And Not Variable(Cline$,4) Then
    NLI = NLI + Indent
    ForNest=ForNest+1
    Found = True
  EndIf
End Sub 'Test_For
  
Sub Test_Next(Found)
  '**********************************************************
  'If NEXT, print this and the next line one less indent
  '**********************************************************
  If UCase$(Left$(Cline$,4))="NEXT" And Not Variable(Cline$,5) Then
    If ForNest Then
      NLI = CLI
      ForNest=ForNest-1
    Else
      CLI = CLI - Indent   'remove one level of indent
      NLI = CLI            'make the next line indent the same
    EndIf
    Ptr = 5
    ' Look for a comma which closes multiple For levels
    Do
      GetNextNonBlankChar(Cline$,Ptr,a$)
      If a$ = "," Then
        If ForNest Then
          ForNest=ForNest-1
        Else
          CLI = CLI - Indent  'remove another level of indent
          NLI = CLI           'make the next line indent the same
        EndIf
      EndIf
    Loop Until a$ = "" Or a$ = ":" ' Both end the NEXT statement
    Found=True
  EndIf
End Sub 'Test_Next
  
Sub Test_If_Then(Found,ML)
  '**********************************************************
  'Print the IF/ELSE at the current indent but indent the next line further
  '**********************************************************
  If UCase$(Left$(Cline$,3))="IF " Then
    ' test for one-line IF/THEN.
    ' "Then" will be followed by a non-blank character
    Ptr = Instr(4,UCase$(Cline$)," THEN")
    Ptr=Ptr+6
    GetNextNonBlankChar(Cline$, Ptr, b$)
    ' if statement continues on next line, indent the next line
    If b$="" Or b$=" " Then
      NLI = CLI + Indent
      ML=1              'Indicate a multi-line IF
    EndIf
    Found=True
  EndIf
End Sub 'Test_If_Then
  
Sub Test_ElseIf(Found,ML)
  '**********************************************************
  ' If on entry, MultiLn=1, the previous THEN/ELSE was a multiline statement
  '**********************************************************
  If UCase$(Left$(Cline$,6))="ELSEIF" And Not Variable(Cline$,7) Then
    If ML = 1 Then CLI = CLI - Indent
    ML=0
    ' test for one-line ELSEIF/THEN.
    ' The ELSE will be followed by a non-blank character
    Ptr = Instr(4,UCase$(Cline$)," THEN")
    Ptr=Ptr+6
    GetNextNonBlankChar(Cline$, Ptr, b$)
    If b$="" Or b$<>" " Then
      NLI = CLI + Indent
      ML=1
    EndIf
    Found=True
  EndIf
End Sub 'Test_ElseIf
  
Sub Test_Else(Found,ML)
  '**********************************************************
  ' If on entry, MultiLn=1, the previous THEN/ELSE was a multiline statement
  '**********************************************************
  If UCase$(Left$(Cline$,4))="ELSE" And Not Variable(Cline$,5) Then
    If ML = 1 Then CLI = CLI - Indent
    ML=0
    ' test for one-line ELSE/THEN.
    ' The ELSE will be followed by a non-blank character
    Ptr = Instr(4,UCase$(Cline$)," THEN")
    Ptr=Ptr+6
    GetNextNonBlankChar(Cline$, Ptr, b$)
    If b$="" Or b$<>" " Then
      NLI = CLI + Indent
      ML=1
    EndIf
    Found=True
  EndIf
End Sub 'Test_Else
  
Sub Test_EndIf(Found)
  '**********************************************************
  'If ENDIF, print this and the next line one less indent
  '**********************************************************
  If UCase$(Left$(Cline$,5))="ENDIF" And Not Variable(Cline$,6) Then
    CLI = CLI - Indent   'remove one level of indent
    NLI = CLI            'make the next line indent the same
    Found=True
  EndIf
End Sub 'Test_EndIf
  
Sub Test_for_Label(Found)
  '**********************************************************
  ' A line starting with a label is aligned to the left margin
  ' A label is terminated with a colon and musn't
  ' - have an imbedded blank or operator.
  ' - be an MMBasic command terminated with a colon.
  '**********************************************************
  Found=False
  p=Instr(1,Cline$,":")                       ' Look for first colon
  If p<>0 Then
    Found=True       'Provided there is no imbedded blank or operator
    For i=1 To p-1
      a$=Mid$(Cline$,i,1)
      If a$=" " Or IsOperator(a$) Then
        Found=False
        Exit For
      EndIf
    Next
  EndIf
  ' Check that it is not a one word MMBasic command
  If found And p<>0 Then
    For i=1 To Commands
      If UCase$(Left$(Cline$,p-1))=Command$(i) Then
        Found=False
        Exit For
      EndIf
    Next
  EndIf
  If Found=True Then
    CLI=0
  EndIf
End Sub 'Test_for_Label
  
Function IsOperator(a$)
  '**********************************************************
  ' Tests a$ to see if it is an operator
  '**********************************************************
  Local i
  IsOperator=False
  For i=1 To Operators
    If a$=Operator$(i) Then
      IsOperator=True
      Exit For
    EndIf
  Next
End Function 'IsOperator
  
Sub Initialise(Ifile$,Ofile$,Indent)
  '**********************************************************
  ' Input and Output file names, indent and pause switch can come from
  ' 4 sources (in order of precedence:
  '  - MM.CMDLine$, the implied RUN command
  '  - CMDParm$, a variable passed by a program that chains to Format.
  '  - FORMAT.DAT file in the same format as MM.CMDline$
  '  - User prompts
  ' If file names are missing from the first 2, the user is prompted to
  ' enter them. If indent and pause switch are omitted, defaults are used.
  '**********************************************************
  a$=MM.CmdLine$
  If a$<>"" Then
    Print "Getting parms from MM.CMDLine$."
    GetCMDLine(a$)
  ElseIf CMDParm$<>"" Then
    ' CMDParm$ is intended to be passed by a Chaining program
    Print "Getting parms from CMDParm$."
    GetCMDLine(CMDParm$)
  Else
    a$=GetParmsFromFile$(CMDParmFile$)
    If MM.Errno=0 Then Print "Getting parms from file ";CMDParmFile$
    If a$<>"" Then GetCMDLine(a$)
  EndIf
  Do
    If Ifile$="" Then
      Input "Give me the Input  filename (.BAS assumed) - 'Exit' to exit: ", Ifile$
    EndIf
    If LCase$(Ifile$)="exit" Then End
    CheckInputFileName(Ifile$)
  Loop Until Ifile$<>""
  Do
    If Ofile$="" Then
      Input "Give me the Output filename (.BAS assumed) - 'Exit' to exit: ", Ofile$$
    EndIf
    If LCase$(Ofile$)="exit" Then End
    CheckOutputFilename(Ofile$, Ifile$)
  Loop Until Ofile$<>""
  If Indent = 0 Then
    GetIndent(Indent)
    If Indent=0 Then End
  EndIf
  If PauseList$="" Then GetPause(PauseList$)
End Sub 'Initialise
  
Function GetParmsFromFile$(DATFile$)
  '**********************************************************
  'Gets the parameters from a file provided by Renumber
  '**********************************************************
  Local a$
  Option error continue
  Open DATFile$ For input As #3
  If MM.Errno = 0 Then
    Line Input #3,a$
    GetParmsFromFile$=a$
    Close #3
  EndIf
  Option error abort
End Function 'GetParmsFromFile
  
Sub GetCMDLine(a$)
  '**********************************************************
  ' MMBasic 4.3A onward which supports implied Run command
  ' User can start the program with FORMAT infilename outfilename [Indent] [/p]
  ' /p lists the output a screenful at a time. Press any key to continue.
  ' On exit Parm$(1) = infilename, Parm$(2) = outfilename, Parm3 = Indent 0r 0
  '**********************************************************
  Local i
  If a$<>"" Then
    ParmNo=5                 ' Examine up to 5 parms
    ' Set the delimiter between parms
    If Instr(1,a$,",") Then ParmDelimiter$="," Else ParmDelimiter$=" "
    Dim Parm$(ParmNo)
    ParseParm(a$,ParmNo,ParmDelimiter$)
    Ifile$=Parm$(1)
    Ofile$=Parm$(2)
    Indent=2
    PauseList$="N"
    For i=3 To ParmNo
      If Val(Parm$(i)) > 0 Then Indent=Val(Parm$(3))
      If UCase$(Parm$(i))="/P" Then PauseList$="Y"
    Next
  EndIf
  Erase Parm$               'Not needed any more - save the space
End Sub 'GetCMDLine
  
Sub ParseParm(p$,PNo,Delim$)
  '**********************************************************
  ' Parse the parameters on the Implied Run Command line,
  ' from CMDparm$ or the DAT file.
  '**********************************************************
  Local i,Strt,Ptr
  Strt=1
  For i=1 To PNo
    Ptr=Instr(Strt,p$,Delim$)
    If Ptr=0 Then Ptr=Len(p$)+1
    Parm$(i)=Mid$(p$,Strt,Ptr-Strt)
    If Ptr>=Len(p$) Then Exit For
    Strt=Ptr
    FindNextParmDelimiter(p$,Strt,Delim$)
  Next
  If UCase$(Parm$(3))="/P" Then   ' user opted to omit Indent so
    Parm$(4)=Parm$(3)           ' shift pause list parm to it's proper place
    Parm$(3)=""                ' and clear the Indent parm
  EndIf
End Sub 'ParseParm
  
Sub FindNextParmDelimiter(a$,p,Delim$)
  '**********************************************************
  ' Fine the start of the next parameter
  '**********************************************************
  Do
    p=p+1
  Loop Until Mid$(a$,p,1)<>Delim$ Or p=Len(a$)
End Sub 'FindNextParmDelimiter
  
Sub CheckInputFilename(i$)
  '**********************************************************
  ' Adds .BAS if an extension is not provided and checks that is accessible
  '**********************************************************
  If Instr(1,i$,".")=0 Then i$=i$+".bas"
  Option error continue
  Open i$ For INPUT As #1
  If MM.Errno <> 0 Then
    Print i$ " doesn't exist."
    Print
    i$=""
  EndIf
  Option error abort
End Sub 'CheckInputFilename
  
Sub CheckOutputFileName(o$,i$)
  '**********************************************************
  ' Adds .BAS if an extension is not provided and checks that it does't alread exit.
  ' If it exists, asks user if ok to overwrite.
  '**********************************************************
  If Instr(1, o$, ".") = 0 Then o$ = o$ + ".bas"
  If LCase$(o$)=LCase$(i$) Then
    Print "You cannot write to the input file - give me another."
    Print
    o$=""
  Else
    ' If old file exists, ask if it should be replaced. If not, get another filename
    Option error continue
    Open o$ For input As #2
    If MM.Errno = 0 Then
      Print "OK to overwrite "+o$+" Y/N";:Input ""; Reply$
      If LCase$(Left$(Reply$,1)) = "y" Then
        Close #2
        Open o$ For output As #2
      Else
        Close #2
        Print
        o$=""
      EndIf
    Else
      Open o$ For output As #2
    EndIf
    Option error abort
  EndIf
End Sub 'CheckOutputFileName
  
Sub GetIndent(i)
  '**********************************************************
  ' Asks user for number of spaces to use for each level of indent
  '**********************************************************
  Do
    Input "What indent should I use? (Range 1-6, 0 to exit): ", i
    If i<0 Or i>6 Then
      Print "Indent needs to be in the range 1 to 6, or type 0 to exit"
      Print
    EndIf
  Loop Until i>=0 And i<=6
End Sub 'GetIndent
  
Sub GetPause(a$)
  '**********************************************************
  ' Asks user if screen listing should be paused at end of each screenful.
  '**********************************************************
  Do
    Input "Pause the listing at the end of each screenful (y/n)?", a$
    a$ = UCase$(a$)
    If a$ <> "Y" And a$ <> "N" Then
      Print "Please answer Y or N."
      Print
    EndIf
  Loop Until a$ = "Y" Or a$ = "N"
End Sub 'GetPause
  
Sub PrintInstructions
  '**********************************************************
  Print @(MM.HRes/2-12*10,1) "Program Source Formatter v2.0"
  Print @(MM.HRes/2-12*10,12) "============================="
  Print "This program reads a basic program file and creates a new one following"
  Print "these indent rules:"
  Print " - Lines starting with a Label are aligned to the left margin"
  Print "   as are SUB, END SUB, FUNCTION and END FUNCTION."
  Print " - The first non-label line is indented to the first level."
  Print " - Lines following a single line IF, DO and FOR are not further indented."
  Print " - Multiline DO and FOR have their respective LOOP and NEXT aligned."
  Print "   Intervening lines are indented one level."
  Print " - Multiline IF statements have their ELSE, ELSEIF and ENDIF aligned"
  Print "   under the relevant IF. Intervening statements are indented one level."
  Print " - Nesting increases the indent one level."
  Print " - For numbered programs, the left margin is the longest number plus one."
  Print " - Other than setting the indent, the source line is not altered."
  Print " - No attempt is made to check the program for errors."
  Print
  Print "From MMBasic V4.3a you can invoke FORMAT from the Basic prompt using"
  Print "the implied RUN command. The syntax is:"
  Print
  Print "   FORMAT InFileName[.BAS] OutFileName[.BAS] [Indent] [/p]"
  Print
  Print "Where:   InFileName must not be the same as OutFileName"
  Print "         Indent defaults to 2 and"
  Print "         switch /p pauses the listing at a screenful."
  Print
End Sub 'PrintInstructions
  
Sub CheckForLineNumbers
  '**********************************************************
  ' Check for line numbers and find the longest number
  ' Length of longest number + 1 will be used as the left margin
  '**********************************************************
  Local i
  LineNumbers=False
  Do
    Line Input #1, Iline$
    ILine$=StripBlanks$(ILine$)  'Remove any leading blanks
    If Left$(ILine$,1)>="0" And Left$(ILine$,1)<="9" Then
      LineNumbers=True
      i=Instr(1,ILine$," ")
      If i>LineNoLen Then LineNoLen=i
    EndIf
  Loop Until Eof(#1)
  Close #1
  Open Ifile$ For Input As #1
End Sub 'CheckForLineNumbers
  
Sub wait
  '**********************************************************
  ' Causes the program to pause at the end of each screenful.
  ' Any keystroke causes the program to continue.
  '**********************************************************
  w=w+1
  If w>30 Then
    w=0
    Do
      k$=Inkey$
    Loop Until k$<>""
  EndIf
End Sub 'Wait
  
  
