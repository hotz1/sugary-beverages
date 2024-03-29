library(here)
library(tidyverse)
library(visdat)
library(naniar)
library(GGally)
library(viridis)

# Read in data
sugary_bev <- read.csv(file = "./rawdata/june1data.csv", 
  colClasses = c("DofW" = "factor", "Site" = "factor", "Intervention" = "factor"))

# Check for missing data
vis_miss(sugary_bev)

sugary_bev %>% 
  group_by(Site) %>% 
  miss_var_summary() %>% 
  arrange(desc(n_miss))

# Get rid of rows with total sales less than the drink sales summed and missing totals
sugary_bev$totals <-rowSums(sugary_bev[,5:9],na.rm = T)
sugary_bev =sugary_bev[!is.na(sugary_bev$Total),]
sugary_bev = sugary_bev[!(sugary_bev$Total<sugary_bev$totals),]


# Side by Side Boxplots

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
  ggplot(aes(x = forcats::fct_rev(Day), y = Total)) +
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
  mutate(ZeroCalProp = ZeroCal/Total) %>%
  ggplot(aes(x = forcats::fct_rev(Day), y = ZeroCalProp)) +
  geom_boxplot(aes(col=Site)) +
  theme_bw() +
  labs(x = "Day of the Week", 
       y = "Proportion of Total Sales \nComing From Zero Calorie Sales",
       title = "Proportion of total bottled beverage\n sales with zero calories per day") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(0,1) +
  coord_flip()

# Plots of (total) drink sales per day by site split by weekday/weekend
sugary_bev %>%
  drop_na(Total, ZeroCal, Sugary) %>%
  mutate(Weekend = if_else(DofW %in% c(1:5), "Weekday", "Weekend")) %>%
  pivot_longer(cols = c(ZeroCal, Sugary, Total), 
               names_to = "DrinkType", values_to = "DrinkSales") %>% 
  mutate(DrinkType = factor(DrinkType, levels = c("Total", "ZeroCal", "Sugary"))) %>%
  ggplot(aes(x = Count, y = DrinkSales, colour = Weekend)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(se=FALSE) + 
  labs(x = "Day of the Experiment", y = "Number of Drinks Sold",
       title = "Number of Total Drink Sales and\nZero-Calorie Drink Sales Daily per Site",
       colour = "Date of Week") +
  theme_bw() + 
  scale_color_manual(values = c("#D7191C", "#2C7BB6")) +
  facet_grid(rows = vars(DrinkType), cols = vars(Site)) + 
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill = "#FFD70040"))

sugary_bev %>%
  drop_na(Total, ZeroCal, Sugary) %>%
  mutate(Weekend = if_else(DofW %in% c(1:5), "Weekday", "Weekend")) %>%
  pivot_longer(cols = c(ZeroCal, Sugary, Total), 
               names_to = "DrinkType", values_to = "DrinkSales") %>% 
  mutate(DrinkType = factor(DrinkType, levels = c("Total", "ZeroCal", "Sugary"))) %>%
  ggplot(aes(x = Count, y = DrinkSales, colour = DrinkType)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(se=FALSE) + 
  labs(x = "Day of the Experiment", y = "Number of Drinks Sold",
       title = "Number of Total Drink Sales and\nZero-Calorie Drink Sales Daily per Site",
       colour = "Drink Type") +
  theme_bw() + 
  scale_color_manual(values = c("#FC8D59", "#FFFFBF", "#91BFDB")) +
  facet_grid(rows = vars(Weekend), cols = vars(Site)) + 
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill = "#B2ABD240"))
  
# See if sales differ by site

# Plot percentage of zero sugar sales accounted for by zero sugar drinks over the counts by site
sugary_bev %>%
  mutate(percent_zero = ZeroCal/Total) %>%
  ggplot(aes(x=Count,y=percent_zero,col=Site))+
  geom_point(alpha=0.5)+
  geom_smooth(se=F)+
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom")+
  theme_minimal()+
  labs(x = "Day of the Experiment",
       y = "Proportion of Total Sales")

# Plot percentage of sugary sales accounted for by zero sugar drinks over the counts by site
sugary_bev %>%
  mutate(percent_sugar = Sugary/Total) %>%
  ggplot(aes(x=Count,y=percent_sugar,col=Site))+
  geom_point(alpha=0.5)+
  geom_smooth(se=F)+
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom")+
  theme_minimal()+
  labs(x = "Day of the Experiment",
       y = "Proportion of Total Sales")

# See if compute mean proportion of sales by days of the week
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

# Correlation
ggpairs(sugary_bev,columns=c(5,6,10),aes(col=Site,alpha=0.5))+
  scale_fill_viridis(discrete = TRUE,aesthetics = c("fill","col"))



