ASTROFIX.BAS

Here is a program that may be of use to the budding astronomer or others who might be interested.
It uses the EM408 GPS module with a 47K resistor between module pin1(enable) and pin2(Gnd).
 
Connection to the Maximite are..
EM408    pin1(enable) to Maximite    Pin19
  "      pin2(Gnd)        "          GND
  "      pin3(Rx)         "          Pin16 (This connection is not essential)
  "      pin4(Tx)         "          Pin15
  "      pin5(+3.3V)      "          +3.3V
 
When you run the program it will ask for your time zone, just hit enter for default UTC+10 (Sydney Melb.no allowance for daylight saving).
Once the time zone is set the program will wait for the module to get a satellite lock.
When a lock is achieved the program will commence calculating local solar and local sidereal time.
It allows the equatorial co-ordinates of a celestial object to be entered, it will then track its position in the sky.
Sorry no graphics, just sidereal hours east or west of the local meridian and the angle of arc north or south from zenith.
It will also indicate the object's visibility in the night sky, as some objects may never be visible, while other may always be visible.
The accuracy appears to be within a second or two, which should be adequate for the casual observer.
Originally written with V4.3A; works on V4.5 colour or mono.

Ray Alger