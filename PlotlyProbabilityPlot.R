library(ggplot2)
library(tidyr)
library(tibble)
library(cowplot)
library(plotly)
library(htmlwidgets)



# reads the CSV file Unknown_Combined_probabilities.csv and restructures it

comb_prob <- Unknown_Combined_Probabilities[Unknown_Combined_Probabilities$Model %in% 'Combined',]

comb_prob <- subset(comb_prob,select=-c(Model)) # drop the model column
comb_prob <- as.matrix(comb_prob) # convert to matrix
rownames(comb_prob) <- comb_prob[,1] # add specimen id as rowname
comb_prob <- comb_prob[,-1] # drop specimen id column
comb_prob <- as.data.frame(comb_prob)



# loop over imported file, reshape and create a ggplot for each specimen saved into plot_list

plot_list <- list() # to save ggplot objects
fig_list <- list() # to save plotly interactive objects

# control x and y axis text size plus title size and position
my_theme <- theme(axis.text.x = element_text(angle=90,hjust = 1, vjust = 0.5,size = 8),
                  axis.text.y = element_text(size = 8),
                  plot.title = element_text(size=8,hjust = 0.5,vjust=-10),
                  aspect.ratio = 1, # square plot
                  panel.border = element_rect(color = "black",fill=NA)
                  #plot.margin = unit(c(0,0,0,0),"pt") # reduce margin around plot - theme_get()$plot.margin
) 


xaxis_labels <-c("A","Al","C","D","Me","Mt","N","Na","Nm","Nt","S","T")

for (row in 1:nrow(comb_prob)) {
  
  comb_prob2 <- (comb_prob[row,]) %>%
    rownames_to_column() %>%
    gather(colname, value, -rowname)
  
  specimen_ID <- comb_prob2[1,1]
  #  comb_prob2 <- comb_prob2[-1,]
  comb_prob2 <- comb_prob2[,-1]
  comb_prob2$value <- as.numeric(comb_prob2$value)
  
  plot_list[[row]] <- ggplot(data=comb_prob2,aes(x=colname,y=value)) + 
    geom_bar(stat="identity",fill="lightgray", width=0.6) + 
    geom_bar(data = comb_prob2[which.max(comb_prob2$value),], fill = "blue",stat="identity", width = 0.6) +
    scale_y_continuous(limits=c(0,1)) + 
    scale_x_discrete(labels=xaxis_labels) +
    theme(panel.background = element_rect(fill = "white", colour = "black",size = 1, linetype = "solid"),panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.title.x = element_blank(),
          axis.title.y = element_blank()) + #ggtitle(paste0(specimen_ID)) + # have removed ggtitle and added geom_text as subplot does not bring through individual titles
    my_theme + geom_text(label=specimen_ID,x=6.5,y=0.97,size=3)
  
  
  
  # create interactive plotly objects  
  
  # assign(paste0("plotly_fig",row),ggplotly(plot_list[[row]])) # create interactive plotly object from each graph
  fig_list[[row]] <- assign(paste0("plotly_fig",row),plotly_build(ggplotly(plot_list[[row]]))) # create a list of each plotly object
  
}


# now plot all the individual plots in one 5 x 5 panel

pdf(file="classification.pdf", width= 8, height=11)
plot_grid(plotlist=plot_list,nrow=5,ncol=5)
dev.off()

# save to interactive html

interactive_fig <- subplot(fig_list,nrows=5)
saveWidget(interactive_fig, "interactive_fig.html", selfcontained = F, libdir = "lib")
zip("interactive_fig.zip",c("interactive_fig.html","lib"))
interactive_fig2 <- subplot(fig_list,nrows=5,shareX = TRUE, shareY = TRUE)
saveWidget(interactive_fig2, "interactive_fig2.html", selfcontained = F, libdir = "lib")
zip("interactive_fig2.zip",c("interactive_fig2.html","lib"))



