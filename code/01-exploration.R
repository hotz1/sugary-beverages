library(here)
library(tidyverse)

# Read in data
sugary_bev <- read.csv(file = "./rawdata/june1data.csv", 
  colClasses = c("DofW" = "factor", "Site" = "factor", "Intervention" = "factor"))

# Plot percentage of sugary or zero sugar sales accounted for by zero sugar drinks over the counts by site
sugary_bev %>%  
  mutate(percent_zero = ZeroCal/(ZeroCal+Sugary)) %>% 
  ggplot(aes(x=Count,y=percent_zero,col=Site))+
    geom_point()+
    geom_smooth(se=F)+
    scale_fill_continuous(guide = guide_legend()) +
    theme(legend.position="bottom")

# See if compute mean proportion of sales by days of the week
sugary_bev %>%  
  mutate(percent_zero = ZeroCal/(ZeroCal+Sugary)) %>% 
  group_by(DofW) %>% 
  summarise(Average=mean(percent_zero,na.rm = T))

# Plot the total sales of bottled drinks by day of the week
sugary_bev %>%
  drop_na(c(ZeroCal, Sugary)) %>%
  mutate(Day = case_when(DofW == "1" ~ "Monday",
                         DofW == "2" ~ "Tuesday",
                         DofW == "3" ~ "Wednesday",
                         DofW == "4" ~ "Thursday",
                         DofW == "5" ~ "Friday",
                         DofW == "6" ~ "Saturday",
                         DofW == "7" ~ "Sunday",
                         .default = as.character(DofW))) %>%
  mutate(Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", 
                                      "Thursday", "Friday", 
                                      "Saturday", "Sunday"))) %>%
  mutate(TotalBottled = ZeroCal + Sugary) %>%
  ggplot(aes(x = forcats::fct_rev(Day), y = TotalBottled)) +
  geom_boxplot(aes(col=Site)) +
  theme_bw() +
  labs(x = "Day of the Week", 
       y = "Bottled Drinks Sold",
       title = "Total Sales per day") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()

# Plot the proportion of zero cal bottles to total bottles by day of the week
sugary_bev %>%
  drop_na(c(ZeroCal, Sugary)) %>%
  mutate(Day = case_when(DofW == "1" ~ "Monday",
                         DofW == "2" ~ "Tuesday",
                         DofW == "3" ~ "Wednesday",
                         DofW == "4" ~ "Thursday",
                         DofW == "5" ~ "Friday",
                         DofW == "6" ~ "Saturday",
                         DofW == "7" ~ "Sunday",
                         .default = as.character(DofW))) %>%
  mutate(Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", 
                                      "Thursday", "Friday", 
                                      "Saturday", "Sunday"))) %>%
  mutate(ZeroCalProp = ZeroCal/(ZeroCal + Sugary)) %>%
  ggplot(aes(x = forcats::fct_rev(Day), y = ZeroCalProp)) +
  geom_boxplot(aes(col=Site)) +
  theme_bw() +
  labs(x = "Day of the Week", 
       y = "Proportion of Zero Calorie Sales",
       title = "Proportion of bottled beverage\n sales with zero calories per day") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(0,1) +
  coord_flip()
  
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

# Check if sales differ by days of the week across sites
sugary_bev %>%  
  mutate(percent_zero = ZeroCal/(ZeroCal+Sugary)) %>% 
  group_by(DofW) %>% 
  ggplot(aes(x=Count,y=percent_zero,col=DofW))+
  geom_point()+
  geom_smooth(se=F)+
  facet_wrap(~Site)+
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom")

# Scatterplot of sugary drinks and zero-calorie
sugary_bev %>%  
  ggplot(aes(x=ZeroCal,y=Sugary))+
    geom_point(aes(col=Site))+
    labs(title="Zero-Calorie vs Sugared Drink Sales",
         x="Zero-Calorie Sales",
         y="Sugared Drink Sales")
