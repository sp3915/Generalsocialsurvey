library(tidyverse)
library(patchwork)
library(tidyverse)
library(knitr)

plot_missing <- function(datset,percent = TRUE){
  
  missing.values <- datset %>%
    gather(key = "key", value = "val") %>%
    mutate(is.missing = is.na(val)) %>%
    group_by(key, is.missing) %>%
    summarise(num.missing = n()) %>%
    filter(is.missing==T) %>%
    select(-is.missing) %>%
    arrange(desc(num.missing)) 
  
  if (percent == FALSE){
    plot_1 <- missing.values %>%
      ggplot() +
      geom_bar(aes(x=reorder(key, -num.missing), y=num.missing), stat = 'identity',fill="orange",alpha = 0.4) +
      labs(x='variable', y="number of missing values", title='Number of missing values') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  if (percent == TRUE){
    plot_1 <- missing.values %>%
      ggplot() +
      geom_bar(aes(x=reorder(key, -num.missing), y=(num.missing*100/nrow(datset))), stat = 'identity',fill="orange",alpha = 0.4) +
      labs(x='variable', y="% row missing", title='Number of missing values') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  missing_patterns <- data.frame(is.na(datset)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  true_val <- apply(missing_patterns[1:(length(missing_patterns)-1)], 1,sum)
  complete_case <- which(true_val == 0)
  
  missing_patterns$idu <- factor(as.numeric(row.names(missing_patterns)))
  color_area <- replicate(length(missing_patterns$idu), "orange")
  color_area[complete_case] <- "blue"
  
  if(percent == FALSE){
    plot_2 <- ggplot(missing_patterns, aes(x = fct_rev(idu), y=count)) +
      geom_bar(stat='identity', fill = color_area, alpha = 0.4) +
      coord_flip()+ylab("Row Count")+xlab("")
  }
  if(percent == TRUE){
    plot_2 <- ggplot(missing_patterns, aes(x = fct_rev(idu), y=count*100/sum(count))) +
      geom_bar(stat='identity', fill = color_area, alpha = 0.4) +
      coord_flip()+ylab("% row")+xlab("")
  }
  
  if (length(missing.values$key) == 0){
    plot_3 <-ggplot() + geom_tile() + theme_void()
  } else{
    df_new= missing_patterns %>% pivot_longer(missing.values$key, names_to="keys", values_to="vals")
    
    df_new$keys <- factor(df_new$keys, levels = missing.values$key)
    
    y_cord <- length(missing_patterns$idu)-complete_case+1
    x_cord <- (length(missing.values$key))/2+0.5
    plot_3 <- ggplot(df_new, aes(x= fct_reorder(keys, count), y=fct_rev(idu), fill=vals))+geom_tile(color="white")+ scale_fill_manual(values=c("lightgreen", "red"))+annotate("text", x=x_cord,y=y_cord, label = "Complete Cases")+ylab("Missing Patterns")+xlab("Variable")
    
  }
  
  return((plot_1 | plot_spacer()) / (plot_3 | plot_2))
  
}

gss_ca = gss_cat
