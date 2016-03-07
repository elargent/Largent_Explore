# --------------------------------------------------- #
# ------------------- Homework #7 ------------------- #
# --------------------------------------------------- #

##Prof G: Nice work!

require(ggplot2)
require(grid)

# Note: explanations of what each supporting function does are 
# found in the code preceding each function. No descriptions
# are given in explore in order to minimize clutter. 

# - indf: a dataframe comprised of variables of any type
# - inbin: a vector comprised of integers that represents bin sizes
# - incor: a numeric value between 0 and 1 that represents a lower bound for correlation values 

explore <- function(indf,inbin,incor) {
  hist_bin(indf,inbin)
  bar_cat_bin(indf)
  r2_values <- r2_num_df(indf)
  return(return_list(indf,incor))
}

# --------------------------------------------------- #
# --------------------------------------------------- #
# --------------------------------------------------- #

# This function takes a data.frame and a vector comprised of integers. If a column
# is of class 'numeric', then it creates blue histograms, with a red
# line indicating the mean, using the standard 'count' and 'density' y-axises for each
# column and each bin size. This means the the number of charts the function creates 
# equals length(data.frame)*length(bin)*2.

hist_bin <- function(indf,inbin) {
  for (i in 1:length(indf)) {
    if (any(class(indf[[i]]) == "numeric")) {
      for (j in 1:length(inbin)){
        hist_count <- ggplot(indf, aes(x=indf[[i]]))
        hist_count <- hist_count + geom_histogram(color='blue',fill='blue',binwidth=inbin[j])
        hist_count <- hist_count + geom_vline(aes(xintercept=mean(indf[[i]])),color="red")
        hist_count <- hist_count + labs(x=colnames(indf[i]),y="count")
        hist_count <- hist_count + theme(axis.title=element_text(face="bold"))
        print(hist_count)
        
        hist_density <- ggplot(indf, aes(x=indf[[i]]))
        hist_density <- hist_density + aes(y=..density..)
        hist_density <- hist_density + geom_histogram(color='blue',fill='blue',binwidth=inbin[j])
        hist_density <- hist_density + geom_vline(aes(xintercept=mean(indf[[i]])),color="red")
        hist_density <- hist_density + labs(x=colnames(indf[i]),y="density")
        hist_density <- hist_density + theme(axis.title=element_text(face="bold"))
        print(hist_density)
      }
    }
  }
}

# This function takes a data.frame and then creates bar charts for all columns that are
# of class 'factor' or some sort of binary object.

bar_cat_bin <- function(indf) {
  for (i in 1:length(indf)) {
    if (any(class(indf[[i]]) == "factor") | length(unique(indf[[i]])) <= 2) {
      bar_plot <- ggplot(indf, aes(x=indf[[i]]))
      bar_plot <- bar_plot + geom_bar(fill='grey',color='grey')
      bar_plot <- bar_plot + labs(x=colnames(indf[i]),y="count")
      bar_plot <- bar_plot + theme(axis.title=element_text(face="bold"))
      print(bar_plot)
    }
  }
}

# This function takes a data.frame and then returns a new data.frame object. This
# data.frame is composed of one column, listing two different variables of type
# numeric or integer (this is how I interpret 'numerical' in the hw assignment), 
# and then their corresponding r2 value is in the next column. Since these values aren't
# stored, the function isn't that useful. That said, the same approach is used when calculating
# the r2 values in the return_list function.
  
r2_num_df <- function(indf) {
  count <- 1;
  outdf = indf[1]
  for (i in 1:length(indf)) {
    if (any(class(indf[[i]]) == "numeric" | class(indf[[i]]) == "integer")) {
      outdf[count] <- indf[i]
      count <- count + 1
    }
  }
  if (count == 1) {
    outdf <- data.frame()
  }
  tempV = c();
  for(i in 1:(length(outdf)-1)) {
    for(j in (i+1):length(outdf)) {
      temp1 <- paste(colnames(outdf[i]),colnames(outdf[j]),sep="-")
      temp2 <- summary(lm(outdf[[i]] ~ outdf[[j]], data = outdf))$r.squared
      tempV = rbind(tempV,c(temp1,temp2))
    }
  }
  outdf <- data.frame(tempV)
  colnames(outdf) <- c("Variables","Pearson_Corr")
  return(outdf)
}

# This function takes a data.frame and numerical variable and returns a list 
# that contains the following:
#   1) A frequency table for all factor and binary class variables in the data.frame
#   2) Summary statistics for numerical variables in outdf
#   3) A data.frame of R-Squared values, and the names of the vars being considered, 
#      for all non-duplicated numerical variable pairs.
#   4) A data.frame of Pearson coefficient values, and the names of the vars being 
#      considered, for all non-duplicated numerical variable pairs when the absolut value 
#      of the Pearson coefficient is greater than incor.

return_list <- function(indf,incor) {
  out_list <- list()
  # Frequency table for factor and binary variables
  for (i in 1:length(indf)) {
    if (any(class(indf[[i]]) == "factor") | length(unique(indf[[i]])) <= 2) {
      out_list <- rbind(out_list,list(table(indf[i])))
    }
  }
  
  # Create summary statistics for each numerical variable in indf
  for (i in 1:length(indf)) {
    if (any(class(indf[[i]]) == "numeric" | class(indf[[i]]) == "integer")) {
      out_list <- rbind(out_list,list(summary(indf[i])))
    }
  }
  
  # Create new data.frame comprised only of columns of class 'numeric' or 'integer'
  count <- 1
  outdf = indf[1]
  for (i in 1:length(indf)) {
    if (any(class(indf[[i]]) == "numeric" | class(indf[[i]]) == "integer")) {
      outdf[count] <- indf[i]
      count <- count + 1
    }
  }
  if (count == 1) {
    outdf <- data.frame()
  }
  
  # Calculate r-squared value for all possible pairs of variables in outdf
  tempV = c();
  for(i in 1:(length(outdf)-1)) {
    for(j in (i+1):length(outdf)) {
      temp1 <- paste(colnames(outdf[i]),colnames(outdf[j]),sep="-")
      temp2 <- summary(lm(outdf[[i]] ~ outdf[[j]], data = outdf))$r.squared
      tempV = rbind(tempV,c(temp1,temp2))
    }
  }
  out_list <- rbind(out_list,list(data.frame(tempV)))
  
  # Calculate Pearson coefficient value for all possible pairs of variables in outdf. However,
  # only the variables for which the absolute value of the Pearson coefficient is greater than
  # the incor value that is passed through to the function. 
  tempV = c();
  for(i in 1:(length(outdf)-1)) {
    for(j in (i+1):length(outdf)) {
      temp1 <- paste(colnames(outdf[i]),colnames(outdf[j]),sep="-")
      temp2 <- cor(outdf[i],outdf[j], method="pearson")
      if (abs(temp2) > incor) {
        tempV = rbind(tempV,c(temp1,temp2))
      }
    }
  }
  out_list <- rbind(out_list,list(data.frame(tempV)))
  
  return(out_list)
}

# --------------------------------------------------- #
# --------------------------------------------------- #
# --------------------------------------------------- #

# Create new version of diamonds that contains a logical column

new_diamonds <- diamonds
new_diamonds$vs <- as.logical(rep_len(mtcars$vs, nrow(new_diamonds)))

test1 <- explore(new_diamonds,c(5,20,50),.25)
test2 <- explore(mtcars,c(5,20,50),.25)

for (i in 1:length(test1)) {print(test1[i])}
for (i in 1:length(test2)) {print(test2[i])}
