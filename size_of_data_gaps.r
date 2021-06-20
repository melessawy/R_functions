# I was working on a big sequence dataset (daily readings) that had missing values. 
# While scrolling in the data, you might see a random missing day reading, 
# and you might also see a group of missing readings for a sequence of days.
# I was asked to report a list of missing chunks, showing the row index of the 
# first missing value in the chunk, and the size of the chunk in rows.
# I created this function to facilitate the process. 
# It is tested on R dataframes and matrices, works fine on both.
# The result will be sorted from the biggest chunks to the smallest ones.
# The function takes 2 inputs, first is the names of the matrix or the data frame, and it's mandatory.
# The second input is the column nuber, and it's optional. 
# If the column nuber is not given, the function will report missing chunks in all columns.

# Example function output
#     	index 		gap_size
#     	 6056 		      29
#     	 6230 		      27
#     	 6101 		      13

data_gaps<- function (df,col_num) {
  reps<- rle(as.numeric(as.matrix(is.na(df[,col_num]))))
  lengths<- as.matrix(reps[[1]])
  values<- as.matrix(reps[[2]])
  data_gaps<- matrix(ncol = 2)
  data_gaps<- data.frame(data_gaps)
  colnames(data_gaps)<- c("index", "gap_size")
  for (t in 1:nrow(values)) {
    if (values[t]==1) {
      loc<- sum(lengths[1:(t-1)])
      data_gaps<- add_row(data_gaps, "index" = loc+1, "gap_size" = lengths[t])
    }
    else {
      next
    }
  }
  data_gaps<- na.omit(data_gaps)
  data_gaps<- data_gaps %>% arrange(desc(gap_size))
  return(data_gaps)
}

