library(here)
library(tidyverse)

sugary_bev <- read.csv("./rawdata/june1data.csv")

# Plot percentage of sugary or zero sugar sales accounted for by zero sugar drinks over the counts by site
sugary_bev %>%  
  mutate(percent_zero = ZeroCal/(ZeroCal+Sugary)) %>% 
  ggplot(aes(x=Count,y=percent_zero,col=Site))+
    geom_point()+
    geom_line()

# See if sales differ by days of the week
sugary_bev %>%  
  mutate(percent_zero = ZeroCal/(ZeroCal+Sugary)) %>% 
  group_by(DofW) %>% 
  summarise(Average=mean(percent_zero,na.rm = T))
  
# See if sales differ by site

# Averages
sugary_bev %>%  
  mutate(percent_zero = ZeroCal/(ZeroCal+Sugary)) %>% 
  group_by(Site) %>% 
  summarise(Average=mean(percent_zero,na.rm = T))

# Side-by-Side boxplots
sugary_bev %>%  
  mutate(percent_zero = ZeroCal/(ZeroCal+Sugary)) %>% 
  group_by(Site) %>% 
  ggplot(aes(Site,percent_zero))+
    geom_boxplot()

