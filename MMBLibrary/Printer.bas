'=============================================================================
'DOT-MATRIX PRINTER ROUTINE

'Concept by CircuitGizmos with BUSY line suggestions by robert.rozee
'of TBS forums - Thanks guys!

'FULL INFORMATION THREAD CAN BE FOUND HERE:
'http://www.thebackshed.com/forum/forum_posts.asp?TID=6520&PN=0&TPN=1

'Version 1.0 - Initial submission to library.

'Works with B/W MM, Colour MM, and MicroMite.
'=============================================================================

'This routine was developed out of CircuitGizmos orignal idea to use a
'dot-matrix printer within MMBasic.  I have refined the routine, and
'incorporated ideas from others and make use of the BUSY line from the
'printer to ensure that data is clocked to the printer correctly, so that
'no incorrect printouts result of sending data to the printer faster than
'the printer can deal with it.

'Any data you want printed, is sent to the subroutine in P$.  The sub will
'clock the data to the printer making use of the BUSY line, and then return
'to the main program.

'I am using port D for this, but you could use other pins.
'IT IS VITAL that you select 5v tolerant pins for your printer control
'port, as the parallel-printer standard is that all data lines are idle-high
'at 5v, and if you connect these to any MM 3v3 pins, you will kill the MM chip.

'YOU HAVE BEEN WARNED!

'In my example, I am using port D2-D13 for the printer interface.
'D2-D9 is the 8-bit parallel data bus(data 0 through data 7), D10 is
'the STROBE line, and D11 is the RESET line used to reset the printer when
'you first run the code.  D12 is the BUSY line, and D13 is the PAPER OUT line
'which is not currently used in this version of the code(but will be later).

'What happens with the use of the BUSY line, is that the code sends a byte to
'the printer, then waits for the printer itself to signal that it is NOT busy
'(in other words, that it is ready for another byte of data), then it clocks
'another byte to the printer.  Only once the full message has been sent, and
'then you send the printer a CR(decimal 13), does the printer actually print
'out what has been clocked into its data buffer up to that point.

'Different printers have different data buffer sizes, so you will need to
'check this with your specific printer if your messages are expected to be
'long.  Most printers can accept up to 80 bytes(characters) without any
'kind of buffer overrun.  

'As for the circuit, you should put a 100-ohm resistor in series with ALL
'data and control pins in use, to limit the current into or out of the
'MM port.  There is no need for any kind of pull-up on the printer-port
'as this is taken care of by the printer electronics.

'It is important that you set the port to open-collector outputs(xx,9), 
'as this along with the 100R resistors, limits the current on any one
'printer line.

'Useage is simple: Call PRINTER(P$), where P$ is the text you want the
'printer to print out.  Generally speaking, the routine returns within
'250ms for a standard 80 character line, but if timing is important, then
'you will need to check this aspect BEFORE using this routine.

'Grogster @ The Backshed Forums - 08/06/2014, 8:52PM



'===== PUT THIS AT THE START OF YOUR CODE, TO INITIALIZE THE PRINTER =====

setpin D2,9:setpin D3,9:setpin D4,9:setpin D5,9:setpin D6,9
setpin D7,9:setpin D8,9:setpin D9,9:setpin D10,9:pin(D10)=1
setpin D11,9:setpin D12,2:setpin D13,2:port(D2,8)=1
pin(D11)=1:pause 1:pulse D11,1:pause 1


' {MAIN CODE HERE}


'=======================
'PRINTER SUBROUTINE CODE
'=======================

sub PRINTER (P$)
  PO$=P$ + chr$(10)
  for X=1 to len(PO$)
    do:loop until pin(D12)=0
    C$=mid$(PO$,X,1)
    port(D2,8)=asc(C$)
    pause 1:pulse D10,1
  next
end sub  