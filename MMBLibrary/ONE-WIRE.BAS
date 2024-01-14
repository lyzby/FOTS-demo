'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Demonstrate how to use the one-wire functions
' Geoff Graham   July 2013
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

' use a defined function to get the temperature
' The sensor is connected to the pin 18
Print "The temperature is:" GetTemp(18)


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Function to get the temperature from a Dallas DS18B20.
' The DS18B20 is connected to the pin specified by PinNbr
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Function GetTemp (PinNbr)
  Local T1, T2, b, t
  OWReset PinNbr                        ' reset
  OWWrite PinNbr, 1, 2, &hcc, &h44      ' start conversion
  Pause 100
  t = Timer
  Do
    If Timer - t > 1000 Then Error "Sensor not responding"
    OWRead PinNbr, 4 , 1 , b            ' conversion done?
  Loop Until b = 1
  OWWrite PinNbr, 1, 2, &hcc, &hbe      ' command read data
  OWRead PinNbr, 2, 2, T1, T2           ' get the data
  GetTemp = ((T2 And &b111) * 256 + T1) / 16
  If T2 And &b1000 Then GetTemp = -GetTemp ' adjust if negative
End Function
