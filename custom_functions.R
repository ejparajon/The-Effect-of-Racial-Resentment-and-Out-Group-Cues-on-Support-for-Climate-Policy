#Creating a ggplot theme for use in several figures throughout the code.
plot_theme = theme(text = element_text(size=11),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.background = element_blank(),
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   axis.text = element_text(color="black"),
                   axis.line = element_line(colour = "black"),
                   strip.text.y = element_text(angle = 0), axis.title.y = element_text(angle=90, hjust=.5, vjust=.5),
                   axis.title = element_text(lineheight=1.1), 
                   legend.position = "bottom", 
                   legend.key = element_rect(colour = "transparent",  fill = "transparent") 
) 

#Creating a function to easily summarize data
pctgroup <- function(data,...,rd = 1) {
  dplyr::select(data,...) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(Percentage = round(100*n/sum(n),rd))
}

#Mode function
mode_func<-function(x, na.rm = FALSE) {
  if(na.rm){ #if na.rm is TRUE, remove NA values from input x
    x = x[!is.na(x)]
  }
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}
