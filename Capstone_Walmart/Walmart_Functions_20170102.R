isHolidayShoppingDay <- function(x, dayBefore = 10, SuperBowl = TRUE, XMAS = TRUE, 
                                 LD = TRUE, TG = TRUE, HLN = TRUE) 
{
  # Check number of dates being passed
  length_x <- length(x)
  
  #Number of day before holidays, which are considered holiday shopping days
  n <- dayBefore - 1
  
  # Chek for SuperBowl
  checkForSuperBowl <- SuperBowl
  
  # Check for Christmas
  checkForXMAS <- XMAS
  
  # Check for Labor Day
  checkForLD <- LD
  
  # Check for ThanksGiving
  checkForTG <- TG
  
  # Check for Halloween
  checkForHLN <- HLN
  
  # Convert to ymd format
  iDate <- ymd(x)
  
  # Initialize return vector of date being holiday shopping day or not
  vector_IsHolidayShopping <- vector(mode = 'numeric', length = length_x)
  
  
  # Check dates in the input vector 1 by 1
  for(i in 1:length_x)  
  {
    #print("Date is ")
    #print(iDate[i])
    
    reviewDate <- iDate[i]
    #print("I am in loop 1")
    #print(reviewDate)
    
    # Set switch of SuperBowl as FALSE
    switchSB = as.numeric(0)
    
    # Set switch of Christmas as FALSE
    switchXMAS = as.numeric(0)
    
    # Set switch of Labor Day as FALSE
    switchLD = as.numeric(0)
    
    # Set switch of ThanksGiving as FALSE
    switchTG = as.numeric(0)
    
    # Set switch of Halloween as FALSE
    switchHLN = as.numeric(0)
    
    # Check for the input date, if holiday falls in next n days
    for (j in 0:n)
    {
      
      #print("I am in loop 2")
      j_reviewDate <- reviewDate + j
      #print(j_reviewDate)
      
      if (checkForSuperBowl == TRUE )
      {
        # SuperBowl is always in Feb between day 1 to 7 on Sunday
        if ((month(j_reviewDate) == 2) & (day(j_reviewDate)<=7) & 
            (weekdays.Date(j_reviewDate) == "Sunday"))
        {
          #print("Go shop for SuperBowl")
          switchSB = as.numeric(1)
          
          #go to the next input date
          break
        } else {
          #print("No SuperBowl shopping")
        } # End of SuperBowl logic
      }
      
      if (checkForXMAS == TRUE )
      {          
        # Christmas is always on 25-Dec
        if ((month(j_reviewDate) == 12) & (day(j_reviewDate) == 25))
        {
          #print("Merry Christmas")
          switchXMAS = as.numeric(1)
          break
        } else {
          #print("No Christmas")
        } # End of Christmas logic
      }
      
      if (checkForLD == TRUE )
      {
        # Labor Day is always in Sep between day 1 to 7 on Monday
        if ((month(j_reviewDate) == 9) & (day(j_reviewDate)<=7) & 
            (weekdays.Date(j_reviewDate) == "Monday"))
        {
          #print("Go shop for Labor Day")
          switchLD = as.numeric(1)
          
          #go to the next input date
          break
        } else {
          #print("No Labor Day shopping")
        } # End of Labor Day logic
      }
      
      if (checkForTG == TRUE )
      {
        # ThanksGiving is always in Nov between day 22 to 28 on Thursday
        if ((month(j_reviewDate) == 11) & (day(j_reviewDate)>=22) 
            & (day(j_reviewDate)<=28) & (weekdays.Date(j_reviewDate) == "Thursday"))
        {
          #print("Go shop for ThanksGiving")
          switchTG = as.numeric(1)
          
          #go to the next input date
          break
        } else {
          #print("No ThanksGiving shopping")
        } # End of ThanksGiving logic
      }
      
      if (checkForHLN == TRUE )
      {          
        # Halloween is always on 31-Oct
        if ((month(j_reviewDate) == 10) & (day(j_reviewDate) == 31))
        {
          #print("Trick and Treat")
          switchHLN = as.numeric(1)
          break
        } else {
          #print("No trick and treat")
        } # End of Halloween logic
      }            
      
      
    }
    #set return value for the input date in the return vector
    
    #print("switchSB is")
    #print(switchSB)    
    
    #print("switchXMAS is")
    #print(switchXMAS)  
    
    if (switchSB == 1 | switchXMAS == 1 | switchLD == TRUE | switchTG == TRUE 
        | switchHLN == TRUE)
    {
      
      vector_IsHolidayShopping[i] <- as.numeric(1)
      
    }
    
    
  }
  #print(vector_IsHolidayShopping)
  return(vector_IsHolidayShopping)
}
