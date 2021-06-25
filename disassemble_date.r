#takes a dataframe with a date column and creates columns for: 
#day (day of the month), month, year, isWeekend (0 or 1 flag), 
#and Weekday (1 to 7 flag)

#Two important points - the function expects that:
#1- you have a column named "Date" that carries date information
#2- Your date is formatted as %d/%m/%Y (example: 15/03/2016)

disassemble_date <- function (df) {
  
  #correct date format
  df$Date<-as.Date(df$Date, format = "%d/%m/%Y")
  
  #create year,month, day columns
  df$Year<- format(df$Date, format = "%Y")
  df$Month<- format(df$Date, format = "%m")
  df$Day<- format(df$Date, format = "%d")
  
  #correct format to numeric for year,month, day columns
  #and add isWeekend and weekday columns
  for (i in 1:nrow(df)) {
    df$Year2[i]<- as.numeric(df$Year[i])
    df$Month2[i]<- as.numeric(df$Month[i])
    df$Day2[i]<- as.numeric(df$Day[i])
    df$Weekday[i]<- as.numeric(strftime(as.Date(df$Date[i], "%d-%m-%Y"), "%u"))
    if (isWeekend(df$Date[i], wday=1:5)==TRUE) {
      df$isWeekend[i]=1
    }
    else if (isWeekend(df$Date[i], wday=1:5)!=TRUE) {
      df$isWeekend[i]=0
    }
  }
  
  #assign the correctly formatted columns and delete extra columns
  df$Year<-df$Year2
  df$Month<-df$Month2
  df$Day<-df$Day2
  df<- subset(df, select = -c(Year2, Month2, Day2))
  
  return(df)
}

#test
test <- as.data.frame(read.csv("test.csv"))
test <- disassemble_date(test)
