'====================================================
'     SHOW NAME OF DAY OF WEEK FROM THE MM RTC
'====================================================
'Function written by TBS forum member TZAdvantage.
'Array and RTC handling by TBS forum member Grogster.
'Developed in January 2014, on The Back Shed Forums.
'----------------------------------------------------

'This code will setup and call a function to calculate the name of the day
'of the week from the settings of the RTC in the MM or MM clone.  The function was
'written by TZAdvantage on the TBS(The Back Shed) forums, and is accurate out
'to the year 2300 and beyond.

'The array handling was added to TZAdvantage's source code by TBS member
'Grogster, and that part of the code works out what the name of the day
'is based on the number returned by TZAdvantage's code.

'The code calculates the values for "Year", "Month" and "Day" from the
'built-in MM RTC, by extracting the number references from DATE$ and saving
'the result as a numeric value rather then a string.
'These values are then passed to the function, which calculates what day
'of the week it is from 0 to 6, with 0 being Sunday, and 6 being Saturday.
'By printing DOW$(DayOfWeek(Year, Month, Day)), the actual printed output
'will be the name of the day of the week as decided by the function.  
'The function is called as part of the print line, and so long as the 
'current date is correct in the MM clock, then the current day of the week
'will also be shown correctly.


dim DOW$(6) length 9
DOW$(0)="Sunday":DOW$(1)="Monday":DOW$(2)="Tuesday":DOW$(3)= "Wednesday"
DOW$(4)="Thursday":DOW$(5)="Friday":DOW$(6)="Saturday"

Year=VAL(MID$(DATE$,7,4)):Month=VAL(MID$(DATE$,4,2)):Day=VAL (MID$(DATE$,1,2)) 
print DOW$(DayOfWeek(Year, Month, Day)) 


end 


function DayOfWeek(year, month, day)  
  a = int((14-month)/12)  
  m = month + 12*a - 2  
  y = year - a  
  DayOfWeek = (day + y + int(y/4)-int(y/100)+int(y/400)+int(31*m/12)) mod 7  
end function  