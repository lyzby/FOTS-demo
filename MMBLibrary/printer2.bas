'============================================================================= 
'DOT-MATRIX PRINTER ROUTINE 

'Concept by CircuitGizmos with BUSY line suggestions by robert.rozee 
'of TBS forums - Thanks guys! 

'FULL INFORMATION THREAD CAN BE FOUND HERE: 
 'http://www.thebackshed.com/forum/forum_posts.asp?TID=6520&P N=0&TPN=1 

'Version 1.0 - Initial submission to library.
'Version 1.1 - Description text rewrite by robert.rozee
'              Add BUSY timeout.  Add Paper-Out timeout.

'Works with B/W MM and Colour MM.  Not "Officially" supported on the MicroMite. 
'============================================================================== 

'This routine was developed out of CircuitGizmos orignal idea to use a 
'dot-matrix printer within MMBasic.  I have refined the routine, 
'incorporating ideas from others and make use of the BUSY line from the 
'printer to ensure that data is clocked to the printer only when it is 
'not busy - this ensures no incorrect printouts result from sending data 
'to the printer faster than the printer can deal with it. 

'Any data you want printed is sent to the subroutine in P$.  The sub will 
'clock the data to the printer making use of the BUSY line, and then return 
'to the main program.  NOTE: the subroutine will loop forever while the 
'printer remains BUSY, so if this is likely to happen (an example is if the 
'paper runs out) it would be a good idea to add some sort of timeout to the 
'do:loop part of the code and suitable error reporting. 

'I am using port D for this, but you could use other pins. 
'IT IS VITAL that you select 5v tolerant pins for any pins that the printer 
'will be driving itself - BUSY and PAPER OUT.  the parallel-printer standard is 
'that all data lines sourced from the printer are idle-high at 5v, and if you 
'connect these directly to any MM 3v3 pins, you will kill the MM chip.  For 
'output pins (data 0..7, STROBE, RESET) things becomes more complicated.  if 5v 
'tolerant pins are used with open-collector outputs there is no issue, but this 
'is not guaranteed to drive every printer (however, it should be ok with most). 
'if pins are not driven as open collector, there is the possibility of current 
'flowing back through the MM's internal protection diodes lifting Vcc above  
'3v3.  in this case consider using a level-shifting buffer. 

'it is recommended that 100R resistors be placed in series with all line (both 
'input and output) as an extra layer of protection. 
'YOU HAVE BEEN WARNED! 

'In my example, I am using port D2-D13 for the printer interface. 
'D2-D9 is the 8-bit parallel data bus (data 0 through data 7), D10 is 
'the STROBE line, and D11 is the RESET line used to reset the printer when 
'you first run the code.  D12 is the BUSY line, and D13 is the PAPER OUT line 
'which is not currently used in this version of the code (but may be later). 

'What happens with the use of the BUSY line, is that the code waits for the 
'printer to signal that it is NOT busy (in other words, that it is ready for 
'another byte of data) BEFORE each byte is sent.  When exactly characters are 
'printed depends upon the printer and its buffer size.  Some printers may print 
'characters as they arrive, some may buffer a one or two, some may buffer a 
'whole line or more and only print when a CR or LF is received.   

'As for the circuit, you should put a 100-ohm resistor in series with ALL 
'data and control pins in use, to limit any current into or out of the 
'MM port.  There is no need for any kind of pull-up on the printer-port 
'as this is taken care of by the printer electronics. While the IEEE1284 
'specifications say driven lines need to be able to sink AND source up to 
'14mA, the input current is usually just a TTL load with something like 
'10k pullups within the printer. 

'//////////////// 
'It is important that you set the port to open-collector outputs(xx,9),  
'as this along with the 100R resistors, limits the current on any one 
'printer line. 
'//////////////// (not a solution with a micromite) 

'Usage is simple: Call PRINTER(P$,T,CR,LF), where P$ is the text you want the 
'printer to print out, and T is the timeout in ms when the sub will end if the
'printer has been delayed too long.  CR is 1 if you want a CR added to P$, LF is
'1 if you want a LF added to P$.  Generally speaking, the routine returns within 
'250ms for a standard 80 character line, but if timing is important, then 
'you will need to check this aspect BEFORE using this routine.
'DO NOT ignore any arguments - you must supply the relevant data.


'Grogster @ The Backshed Forums.
'08/06/2014, 8:52PM(V 1.0)
'18/06/2014, 7:01PM(V 1.1)



'===== PUT THIS AT THE START OF YOUR CODE, TO INITIALIZE THE PRINTER ===== 

setpin D2,9: setpin D3,9: setpin D4,9: setpin D5,9 
setpin D6,9: setpin D7,9: setpin D8,9: setpin D9,9 
port(D2,8)=1 pin(D10)=1: setpin D10,9 

pin(D11)=1: setpin D11,9: pause 1:pulse D11,1:pause 1      ' reset printer 

setpin D12,2: setpin D13,2      ' these are inputs and must be 5v tolerant 


' {MAIN CODE HERE} 


'======================= 
'PRINTER SUBROUTINE CODE 
'======================= 

sub PRINTER (P$,T,CR,LF) 
  Y=0:TOE=0:POE=0:T=T*2:PO$=P$
  If CR=1 and LF=0 then PO$=P$ + chr$(13)
  If CR=0 and LF=1 then PO$=P$ + chr$(10)
  If CR=1 and LF=1 then PO$=P$ + chr$(13) + chr$(10)
  for X=1 to len(PO$) 
    do
      pause 0.5
      Y=Y+1
    loop until pin(D12)=0 or Y=T or pin(D13)=1
    If Y=T then TOE=1:exit sub 'Check TOE once the sub ends, to see if there was a TimeOut Error.
    If pin(D13)=1 then POE=1:exit sub 'Check POE once the sub ends, to see if there was a PaperOut Error.
    C$=mid$(PO$,X,1) 
    port(D2,8)=asc(C$) 
    pause 1:pulse D10,1:Y=0
  next 
end sub   



