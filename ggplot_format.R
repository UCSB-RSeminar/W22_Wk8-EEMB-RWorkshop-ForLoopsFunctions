

## 
# Color pallettes 

# Function to format ggplots, this function is called in deltaIC function and used in stand alone plots below
# ----------------------------------------------------
# Args:
#   plot: saved ggplot, variable name, must provide 
#   title: the title of the plot; default= "" (no title)
#	y_title: the title of y axis, must provide 
#	x_title: the title of x axis, must provide
#	print: logical, TRUE = print (default), FALSE = do not print the plot
#
# Returns: formatted ggplot. Formatting themes include: axis, text size, label sizes, no gridlines, panel border around the plot
	


ggformat<-function(plot, title="", y_title, x_title, print=TRUE, size_text = 15){
	
	plot_name<-deparse(substitute(plot))
  
	plot<- plot +
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
	
	assign(plot_name, plot, envir=parent.frame())
	
}

	
# m = lm model constructed beforehand
lmEqn <- function(m, x, y){
    
    eq <- substitute(italic(target) == a + b %.% italic(input)*","~~italic(r)^2~"="~r2, 
         list(target = y,
              input = x,
              a = format(as.vector(coef(m)[1]), digits = 2), 
              b = format(as.vector(coef(m)[2]), digits = 2), 
             r2 = format(summary(m)$r.squared, digits = 3)
            )
          )
    as.character(as.expression(eq))                
}


# CORRELATION PLOTS VITH LINEAR REGRESSION FIT (LM)
# add option for error bars 
# numeric x and numeric y 




# LINE PLOTS 
# numeric y, dicxrete x 






# BOX PLOT 
##p1<-ggplot(data=dataMMR, aes(x=as.factor(sex), y=mo2, fill=as.factor(temp_treatm)))+
##	geom_point(size=2, alpha=0.5, position=position_dodge(width=0.75))+
##	geom_boxplot(alpha=0.7, color="black")+
##	scale_fill_manual(labels=c("9","14", "18"), values=c("grey", "darkorange","red2"), guide_legend(title="TEMP"))+
##	theme_classic()+
##	scale_y_continuous(breaks=seq(0,30,5))+
##	xlab("SEX")+ 
##	ggtitle("MMR - swim")+							
##	ylab(expression(MMR[swim]~(mg~O[2]~kg^-1~min^-1)))+ 							
##	theme(axis.text.y=element_text(size=15, colour= 'black'),
##		axis.text.x=element_text(size=15, colour= 'black'),
##		axis.line.y=element_line(colour = 'black',size=0.5),
##		axis.line.x=element_line(colour = 'black',size=0.5),
##		axis.ticks.y=element_line(size=0.5),
##		axis.ticks.x=element_line(size=0))+
##	theme(axis.title.y=element_text(size=15),
##		axis.title.x=element_text(size=15),
##		panel.border = element_rect(linetype = "solid",fill=NA, colour = "black"))
		
		
