#Adding missing days to a time series
# while working with time series data, I noticed that datasets often have 
# missing units of time. For example, if the unit is day, you would see 
# certain dates missing from the sequence, like in the below example
# (notice the date sequence):

#Date         Temperature   Wind Speed    Rainfall
#01/01/2001   20            1.3           13
#02/01/2001   21            1.4           9
#04/01/2001   19            1.3           15
#05/01/2001   20            1.7           14
#09/01/2001   18            1.0           10

# I created the below function to add extra rows filled with the missing dates.
# It takes a dataframe of any size, looks at the sequence of days and 
# adds rows with the missing units accordingly.
# the added rows will have the date value only, and NA in the other columns.

#Two important points - the function expects that:
#1- you have a column named "Date" that carries date information
#2- Your date is formatted as %d/%m/%Y (example: 15/03/2016)

fix_time_series_rows<- function (df) {
  
  #fix date format
  df$Date<-as.Date(df$Date, format = "%d/%m/%Y")
  
  #pick original row counts
  row_count=nrow(df)
  
  for (i in 1:nrow(df)) {
    
    #break when we reach the last row
    if (i==row_count) {
      break
    }
    
    #check the number of missing days and add rows for them
    else if (as.numeric(df[i+1,"Date"]-df[i,"Date"]) != 1) {
      missing_days<- as.numeric(df[i+1,"Date"]-df[i,"Date"])-1
      for (j in 1:missing_days) {
        df<- add_row(df, "Date" = df$Date[i]+j)
      }
    }
  }
  #reorder the days
  df<- df[order(as.Date(df$Date, format="%d/%m/%Y")),]
  
  #fix row IDs
  rownames(df)<- c(1:nrow(df))
  
  return(df)
}

#test
test <- as.data.frame(read.csv("test.csv"))
test <- fix_time_series_rows(test)
