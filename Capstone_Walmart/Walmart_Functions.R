# load libraries
library(lubridate)
library(tidyr)
library(dplyr)
library(timeDate)
library(ggplot2)
library(stringr)
library(rpart)
library(rpart.plot)

func_ContinousDayOfSevereRain_BeforeToday <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  
  vector_ContinousDayOfSevereRain_BeforeToday <- vector(mode = 'numeric', length = length_x)
  
  
  for(i in 2:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    for(j in 1:10)
    {
      #print(j)
      
      if ((i-j)>=1)
      {
        if((x$station_nbr[i-j] == x$station_nbr[i]) && (x$date[i-j] = x$date[i]-j) && (x$severeRain[i] == 0) )
        {
          if((x$severeRain[i-j] == 1))
          {
            vector_ContinousDayOfSevereRain_BeforeToday[i] <- as.integer(j)
          } else{
            break
          }
        }
      }
    }
  }
  
  #  print(counter_of_buying_days)
  
  
  x$ContinousDayOfSevereRain_BeforeToday <- vector_ContinousDayOfSevereRain_BeforeToday
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}
############
func_ContinousDayOfSevereRain_AfterToday <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  
  vector_ContinousDayOfSevereRain_AfterToday <- vector(mode = 'numeric', length = length_x)
  
  
  for(i in 1:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    for(j in 1:10)
    {
      #print(j)
      
      if ((i+j)<=length_x)
      {
        if((x$station_nbr[i+j] == x$station_nbr[i]) && (x$date[i+j] = x$date[i]+j) && (x$severeRain[i] == 0) )
        {
          if((x$severeRain[i+j] == 1))
          {
            vector_ContinousDayOfSevereRain_AfterToday[i] <- as.integer(j)
          } else{
            break
          }
        }
      }
    }
  }
  
  #  print(counter_of_buying_days)
  
  
  x$ContinousDayOfSevereRain_AfterToday <- vector_ContinousDayOfSevereRain_AfterToday
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}
##########
func_LessRainThanYesterday <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  # Initialize return vector of day is 3 days after severe rain
  vector_LessRainThanYesterday <- vector(mode = 'numeric', length = length_x)
  
  counter_of_buying_days <- as.integer(0)
  
  for(i in 2:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    
    if ((i-1)>=1)
    {
      if((x$station_nbr[i-1] == x$station_nbr[i]) && (x$date[i-1] = x$date[i]-1))
      {
        if ( x$preciptotal[i] < x$preciptotal[i-1])
        {
          vector_LessRainThanYesterday[i] <- x$preciptotal[i-1] - x$preciptotal[i]
        }
      }
    }
  }
  
  
  #  print(counter_of_buying_days)
  
  
  x$LessRainThanYesterday <- vector_LessRainThanYesterday
  
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}

############
func_LessRainThanTomorrow <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  
  vector_LessRainThanTomorrow <- vector(mode = 'numeric', length = length_x)
  
  counter_of_buying_days <- as.integer(0)
  
  for(i in 1:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    
    if ((i+1)<=length_x)
    {
      if((x$station_nbr[i+1] == x$station_nbr[i]) && (x$date[i+1] = x$date[i]+1))
      {
        if ( x$preciptotal[i+1] > x$preciptotal[i])
        {
          vector_LessRainThanTomorrow[i] <- x$preciptotal[i+1] - x$preciptotal[i]
        }
      }
    }
  }
  
  
  #  print(counter_of_buying_days)
  
  
  x$LessRainThanTomorrow <- vector_LessRainThanTomorrow
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}
#############
func_Days3AfterSevereRain <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  # Initialize return vector of day is 3 days before severe Rain
  vector_Days3AfterSevereRain <- vector(mode = 'numeric', length = length_x)
  
  counter_of_buying_days <- as.integer(0)
  
  for(i in 2:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    for(j in 1:3)
    {
      #print(j)
      
      if ((i-j)>=1)
      {
        if((x$station_nbr[i-j] == x$station_nbr[i]) && (x$date[i-j] = x$date[i]-j) && (x$severeRain[i-j] == 1) && (x$severeRain[i] == 0))
        {
          #         print("station number is ")
          #         print(x$station_nbr[i])
          
          #         print("Date is ")
          #         print(x$date[i])
          
          counter_of_buying_days <- counter_of_buying_days + 1
          vector_Days3AfterSevereRain[i] <- as.integer(1)
          break
        }
      }
    }
  }
  
  #  print(counter_of_buying_days)
  
  
  x$Days3AfterSevereRain <- vector_Days3AfterSevereRain
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}

#############
func_Days3BeforeSevereRain <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  # Initialize return vector of day is 3 days before severe rain
  vector_Days3BeforeSevereRain <- vector(mode = 'numeric', length = length_x)
  
  counter_of_buying_days <- as.integer(0)
  
  for(i in 1:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    for(j in 1:3)
    {
      #print(j)
      
      if ((i+j)<=length_x)
      {
        if((x$station_nbr[i+j] == x$station_nbr[i]) && (x$date[i+j] = x$date[i]+j) && (x$severeRain[i+j] == 1) && (x$severeRain[i] == 0) )
        {
          #         print("station number is ")
          #         print(x$station_nbr[i])
          
          #         print("Date is ")
          #         print(x$date[i])
          
          counter_of_buying_days <- counter_of_buying_days + 1
          vector_Days3BeforeSevereRain[i] <- as.integer(1)
          break
        }
      }
    }
  }
  
  #  print(counter_of_buying_days)
  
  
  x$Days3BeforeSevereRain <- vector_Days3BeforeSevereRain
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}
############
func_ContinousDayOfSevereSnow_BeforeToday <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  
  vector_ContinousDayOfSevereSnow_BeforeToday <- vector(mode = 'numeric', length = length_x)
  
  
  for(i in 2:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    for(j in 1:10)
    {
      #print(j)
      
      if ((i-j)>=1)
      {
        if((x$station_nbr[i-j] == x$station_nbr[i]) && (x$date[i-j] = x$date[i]-j) && (x$severeSnow[i] == 0) )
        {
          if((x$severeSnow[i-j] == 1))
          {
            vector_ContinousDayOfSevereSnow_BeforeToday[i] <- as.integer(j)
          } else{
            break
          }
        }
      }
    }
  }
  
  #  print(counter_of_buying_days)
  
  
  x$ContinousDayOfSevereSnow_BeforeToday <- vector_ContinousDayOfSevereSnow_BeforeToday
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}
###############
func_ContinousDayOfSevereSnow_AfterToday <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  
  vector_ContinousDayOfSevereSnow_AfterToday <- vector(mode = 'numeric', length = length_x)
  
  
  for(i in 1:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    for(j in 1:10)
    {
      #print(j)
      
      if ((i+j)<=length_x)
      {
        if((x$station_nbr[i+j] == x$station_nbr[i]) && (x$date[i+j] = x$date[i]+j) && (x$severeSnow[i] == 0) )
        {
          if((x$severeSnow[i+j] == 1))
          {
            vector_ContinousDayOfSevereSnow_AfterToday[i] <- as.integer(j)
          } else{
            break
          }
        }
      }
    }
  }
  
  #  print(counter_of_buying_days)
  
  
  x$ContinousDayOfSevereSnow_AfterToday <- vector_ContinousDayOfSevereSnow_AfterToday
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}
############
func_LessSnowThanYesterday <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  # Initialize return vector of day is 3 days before severe snow
  vector_LessSnowThanYesterday <- vector(mode = 'numeric', length = length_x)
  
  counter_of_buying_days <- as.integer(0)
  
  for(i in 2:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    
    if ((i-1)>=1)
    {
      if((x$station_nbr[i-1] == x$station_nbr[i]) && (x$date[i-1] = x$date[i]-1))
      {
        if ( x$snowfall[i] < x$snowfall[i-1])
        {
          vector_LessSnowThanYesterday[i] <- x$snowfall[i-1] - x$snowfall[i]
        }
      }
    }
  }
  
  
  #  print(counter_of_buying_days)
  
  
  x$LessSnowThanYesterday <- vector_LessSnowThanYesterday
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}
################
func_LessSnowThanTomorrow <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  # Initialize return vector of day is 3 days before severe snow
  vector_LessSnowThanTomorrow <- vector(mode = 'numeric', length = length_x)
  
  counter_of_buying_days <- as.integer(0)
  
  for(i in 1:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    
    if ((i+1)<=length_x)
    {
      if((x$station_nbr[i+1] == x$station_nbr[i]) && (x$date[i+1] = x$date[i]+1))
      {
        if ( x$snowfall[i+1] > x$snowfall[i])
        {
          vector_LessSnowThanTomorrow[i] <- x$snowfall[i+1] - x$snowfall[i]
        }
      }
    }
  }
  
  
  #  print(counter_of_buying_days)
  
  
  x$LessSnowThanTomorrow <- vector_LessSnowThanTomorrow
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}
###############

func_Days3AfterSevereSnow <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  # Initialize return vector of day is 3 days before severe snow
  vector_Days3AfterSevereSnow <- vector(mode = 'numeric', length = length_x)
  
  counter_of_buying_days <- as.integer(0)
  
  for(i in 2:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    for(j in 1:3)
    {
      #print(j)
      
      if ((i-j)>=1)
      {
        if((x$station_nbr[i-j] == x$station_nbr[i]) && (x$date[i-j] = x$date[i]-j) && (x$severeSnow[i-j] == 1) && (x$severeSnow[i] == 0))
        {
          #         print("station number is ")
          #         print(x$station_nbr[i])
          
          #         print("Date is ")
          #         print(x$date[i])
          
          counter_of_buying_days <- counter_of_buying_days + 1
          vector_Days3AfterSevereSnow[i] <- as.integer(1)
          break
        }
      }
    }
  }
  
  #  print(counter_of_buying_days)
  
  
  x$Days3AfterSevereSnow <- vector_Days3AfterSevereSnow
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}

############

func_Days3BeforeSevereSnow <- function(w) 
{
  # Check number of dates being passed
  length_w <- nrow(w)
  
  w$seqnum <- as.integer(c(1:length_w))
  
  x <- w %>% arrange(station_nbr, date)
  length_x = length_w
  
  # Initialize return vector of day is 3 days before severe snow
  vector_Days3BeforeSevereSnow <- vector(mode = 'numeric', length = length_x)
  
  counter_of_buying_days <- as.integer(0)
  
  for(i in 1:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #     print(i)
    #     print("*******")
    
    
    
    
    for(j in 1:3)
    {
      #print(j)
      
      if ((i+j)<=length_x)
      {
        if((x$station_nbr[i+j] == x$station_nbr[i]) && (x$date[i+j] = x$date[i]+j) && (x$severeSnow[i+j] == 1) && (x$severeSnow[i] == 0) )
        {
          #         print("station number is ")
          #         print(x$station_nbr[i])
          
          #         print("Date is ")
          #         print(x$date[i])
          
          counter_of_buying_days <- counter_of_buying_days + 1
          vector_Days3BeforeSevereSnow[i] <- as.integer(1)
          break
        }
      }
    }
  }
  
  #  print(counter_of_buying_days)
  
  
  x$Days3BeforeSevereSnow <- vector_Days3BeforeSevereSnow
  
  x_seq <- x %>% arrange(seqnum)
  x_seq$seqnum <- NULL
  
  return(x_seq)
  
}


###########
isSevereWeather <- function(x) 
{
  # Check number of dates being passed
  length_x <- nrow(x)
  j <- as.integer(0)
  
  # Initialize return vector of date being holiday shopping day or not
  vector_isSevereWeather <- vector(mode = 'numeric', length = length_x)
  
  
  # Check dates in the input vector 1 by 1
  for(i in 1:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #print(i)
    severeRain_i <- x$severeRain[i]
    severeSnow_i <- x$severeSnow[i]
    
    if(severeRain_i == 1 | severeSnow_i == 1)
    {
      j <- j+1
      #   print("severe weather")
      vector_isSevereWeather[i] = as.integer(1)
      
    }
  }
  #print(j)
  return(vector_isSevereWeather)
}
###########
isSevereRain <- function(x) 
{
  # Check number of dates being passed
  length_x <- nrow(x)
  j <- as.integer(0)
  
  # Initialize return vector of date being holiday shopping day or not
  vector_isSevereRain <- vector(mode = 'numeric', length = length_x)
  
  
  # Check dates in the input vector 1 by 1
  for(i in 1:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #print(i)
    codesum_i <- x$codesum[i]
    preciptotal_i <- x$preciptotal[i]
    
    if((str_detect(codesum_i, "RA") | str_detect(codesum_i, "SN")) && preciptotal_i > 1)
    {
      j <- j+1
      #   print("severe rain")
      vector_isSevereRain[i] = as.integer(1)
      
    }
  }
  #print(j)
  return(vector_isSevereRain)
}
############
isMildSnow <- function(x) 
{
  # Check number of dates being passed
  length_x <- nrow(x)
  j <- as.integer(0)
  
  # Initialize return vector of date being holiday shopping day or not
  vector_isMildSnow <- vector(mode = 'numeric', length = length_x)
  
  
  # Check dates in the input vector 1 by 1
  for(i in 1:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #print(i)
    codesum_i <- x$codesum[i]
    snowfall_i <- x$snowfall[i]
    
    if((str_detect(codesum_i, "SN") | str_detect(codesum_i, "SG")) && (snowfall_i > 0 & snowfall_i <= 2))
    {
      j <- j+1
      #   print("mild snow")
      vector_isMildSnow[i] = as.integer(1)
      
    }
  }
  #print(length_x)
  #print(j)
  return(vector_isMildSnow)
}
################
isSevereSnow <- function(x) 
{
  # Check number of dates being passed
  length_x <- nrow(x)
  j <- as.integer(0)
  
  # Initialize return vector of date being holiday shopping day or not
  vector_isSevereSnow <- vector(mode = 'numeric', length = length_x)
  
  
  # Check dates in the input vector 1 by 1
  for(i in 1:length_x)  
  {
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
    #print(i)
    codesum_i <- x$codesum[i]
    snowfall_i <- x$snowfall[i]
    
    if((str_detect(codesum_i, "SN") | str_detect(codesum_i, "SG")) && snowfall_i > 2)
    {
      j <- j+1
      #   print("severe snow")
      vector_isSevereSnow[i] = as.integer(1)
      
    }
  }
  #print(j)
  return(vector_isSevereSnow)
}


##########


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
    #Logic to monitor progress, as the process may take longer time
    if (i == 10 | i == 100 | i == 1000 | i == 10000 | i == 100000 | i == 1000000 | i == 2000000 | i == 3000000 | i == 4000000 | i == length_x )
    {
      print(Sys.time())
      print("Processed percentage is ")
      print(i*100/length_x)
    }
    
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

# Print("This is Walmart_Function.R)
print(Sys.time())
