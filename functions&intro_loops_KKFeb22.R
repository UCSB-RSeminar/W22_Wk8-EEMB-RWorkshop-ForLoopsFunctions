
library(ggpubr) # pretty clean ggplot themes 
library(ggsci) # color shemes
library(plyr) # ddply 

# Overview --------

# A. Format of a function in R, what is a function?
# B. Some ways of applying functions to your dataset:
	# 1. lapply (code available, not discussed in detail)
	# 2. tapply and ddply (some code available, not discussed in detail)
  # 3. Tidyverse  (? not used myself)
  # 4. loops :) 
# C. Introduction to loops


# A. Format of a function in R, what is a function? ------------
# Writing a function in R, Here's the syntax:

# Functions can be used in many ways in your data analyses.
# Two ways they are frequently used are 
# (1) to summarize data (now tidyverse does it for us nowdays, yey!), how about something very specific?
# (2) to do math to many iterations of something inside a loop. 
# (3) replace "copy paste"
# What is the one, two, ir a few variables that change, while everything else stays consistent?


sq<-function(x)	{
  
	f<-x^2
	f
	
}
	
#and of course it can be executed on a vector:	
v<-c(4,1,1,3,6,7)	
# without a function
c(4^2, 1^2, 1^2, 3^2, 6^2, 7^2)
sq(v)
output.1<-sq(v)

# additional useful things to know about functions

# a) defaults
sq<-function(x = 2)	{

  f<-x^2
  f
  
}

# b) return multiple objects

sq<-function(x = 2)	{
  
  f<-x^2
  f2<-x^3
  
  f
  f2

}

sq(3)
sq(2)

# them moment something is "returned" the function has done its job. 
# multiple returns


# c) where do my objects live? Environments in R 
# what is 'f' variable now? 
f

sq<-function(x = 2)	{
  
  f<-x^2
  f2<-x^3
  
  f
  # f2
  
  assign("accessible_f", f, envir = .GlobalEnv)
  
}

sq(2)
accessible_f
f


#  Lots of functions already exist in R and you don't have to write them.
# 1. sq() # click tab to see available arguments. 
# 2. lets think about ggplot
	


# B. Some ways of applying functions to your dataset: ----------
# One of the simplest ways to apply a function to a set of data is to use lapply, 
# which applies a function to a list.  So if we have a sneech population with two phenotypes, with and without stars, we can easily figure out some simple stats with R's built-in functions, e.g.:
mylist<-list(rainiscool=c(14,32,9,44), cofeeisdelicious=c(99,144,50,77))

# l apply = list apply
lapply(mylist, mean) # no quotes, mean = function. 
lapply(mylist, sd) # no quotes, sd = function. 
lapply(mylist, mean)[2] # coffeeisdelicious only 

# but we might want to do something with a table, a step up from a list.  
# let's find out the median number of star-bellied sneeches on each beach.
# tapply applies a function to a table:

# t apply = table apply

tapply(mytable,list(col1,col2),median) # no quotes, can be done with tidyverse

# the first variable of the list creates the rows of the table and the second creates the columns
# As long as we're on the topic, here are a few other useful functions for summarizing data that already exist as commands:
rowSums(mytable)
colSums(mytable)

#  ddply splits data frame, applies function, and gives back results in a new table/dataframe, like tidyverse
#  the best resource to compare ddply, plyr, tidyverse: 
# https://stackoverflow.com/questions/54774280/plyrddply-equivalent-in-dplyr







# *********************************************
# *********************************************
# My most used function: ggplot example ------- 

# 1. Function 
# Function to format ggplots, this function is called in deltaIC function and used in stand alone plots below
# Args:
#   plot: saved ggplot, variable name, must provide 
#   title: the title of the plot; default= "" (no title)
#	y_title: the title of y axis, must provide 
#	x_title: the title of x axis, must provide
#	print: logical, TRUE = print (default), FALSE = do not print the plot
# size_text : the size of x and y texts, default 15 (nice and big)

# Returns: formatted ggplot. Formatting themes include: axis, text size, label sizes, no gridlines, panel border around the plot


ggformat<-function(plot, title="", y_title, x_title, print=TRUE, size_text = 15){
  
  plot_name<-deparse(substitute(plot)) # coming in plot is name of ggplot object ('holding the existing plot'). deparse allows to use the name only
  
  plot<- plot + # its like adding copy-paste lines to the ggplot
    theme_classic()+
    ggtitle(title)+							
    ylab(y_title)+ 							
    xlab(x_title)+ 							
    theme(axis.text.y=element_text(size=size_text, colour= 'black'),
          axis.text.x=element_text(size=size_text, colour= 'black'),
          axis.line.y=element_line(colour = 'black',size=0.5),
          axis.line.x=element_line(colour = 'black',size=0.5),
          axis.ticks.y=element_line(size=0.5),
          # axis.ticks.x=element_line(size=0.5), 
          axis.ticks.x.bottom = element_line(size=0.5, colour = "black"))+
    theme(axis.title.y=element_text(size=size_text),
          axis.title.x=element_text(size=size_text),
          panel.border = element_rect(linetype = "solid",fill=NA, colour = "black"))
  
  if (print==TRUE){	
    print(plot)	
  }
  
  assign(plot_name, plot, envir=parent.frame()) # the environment part - making it accessible to the session I would use it. 
  
}



# 2. how I use  this function
source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")
# add all the libraries to the a file and can do the same :) 
# plotting fish heart rate with temperature


data<-read.csv("/Users/kristakraskura/Github_repositories/Plots-formatting/abt_example_data.csv")
str(data)
data$channel<-as.factor(data$channel)

yeyplots <- ggplot(data, aes(x = temp_mean, y = bpm, group=channel, color = channel))+
  geom_point()+
  geom_line()+
  scale_color_uchicago()+
  geom_vline(xintercept = 24, lty = 2, color="red")
  # theme_bw()
  # theme_dark()
  # theme_light()
  # theme_pubclean() # little too clean?
  # theme_pubr() # looks good, now the names, and size, and maybe even font?

ggformat(yeyplots,
         x_title = expression(Temperature~degree~C),
         y_title = expression(paste(italic(ln)(f[Hmax]))), print = TRUE, size_text = 20)
# ok the legend.. keep changing
yeyplots<-yeyplots+theme(legend.position = "none")
yeyplots


# *********************************************
# *********************************************
# C. Introduction to loops ---------------

# Simplest of loops:
x<-c(8,12,10,9,11)
x.2<-c()

for (i in 1:length(x)){
	x.0 <- 2*x[i] # taking ith item from teh x list, and applying the calculation
  x.2<-append(x.0, x.2) # adding the new object, to the list with each loop iteration
}

x.2
x.0 # what if we dont append? 

# cool loop tricks that can be handy 

# a. visualizing the loop
for (i in 1:length(x)){
  x.0 <- 2*x[i] 
  # print(x.0)
  # print(i)
  # print(x)
  # print(x[i])
}

# b. writing messages (my favorite)
for (i in 1:length(x)){
  x.0 <- 2*x[i] 
  message(paste("loop iteration", i, "produces x.0: ", x.0))
  # print(i)
  # print(x)
  # print(x[i])
}

# c. working with loops conditionally : if -- doe something different
for (i in 1:length(x)){
  x.0 <- 2*x[i] 
  
  if(x[i]>10){
    x.0 <- x[i] 
    message(paste("loop iteration", i, " double dont multiply x. getting:", x.0))
  }
}

# d. working with loops conditionally : if -- stop 
for (i in 1:length(x)){
  x.0 <- 2*x[i] 
  
  if(x[i]==9){
    stop(paste("loop iteration", i, " x = 0 = STOP!")) # write error message
  }
}
