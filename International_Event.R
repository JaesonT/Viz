

library(gcookbook)
library(ggplot2)
library(tidyr)
library(dplyr)

Main <- read.csv('MainGate.csv')
HelpMe <- Main %>% pivot_longer (cols=c('Car', 'Motorcycle','Total'),
                                                names_to="Type",
                                              values_to='Count')
  
HelpMe$Group <- ifelse(HelpMe$Type %in% c("Car", "Motorcycle"), "Vehicle Type", "Total")

ggplot(HelpMe, aes(x = as.factor(Day), y = Count, fill = Type)) +
  labs(title = "First Gate",
       x = 'Day of the Week', y = 'Number of Vehicles') +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(panel.grid.major = element_line(size = 0.5, color = 'grey80'),
        panel.grid.minor = element_line(size = 0.25, color = 'grey90')) +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = c("1" = "Mon", "2" = "Tue", "3" = "Wed",
                              "4" = "Thu", "5" = "Fri", "6" = "Sat", 
                              "7" = "Sun")) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  facet_wrap(~ Group, scales = "free_y")

ergate <- read.csv('2gate.csv')
ilovefurries <- ergate %>% pivot_longer (cols=c('Car', 'Motorcycle','Total'),
                                         names_to="Type",
                                         values_to='Count')

ilovefurries$Group <- ifelse(ilovefurries$Type %in% c("Car","Motorcycle"), "Vehicle Type", "Total")

ggplot(ilovefurries, aes(x = as.factor(Day), y = Count, fill = Type)) +
  labs(title = "Second Gate",
       x = 'Day of the Week', y = 'Number of Vehicles') +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(panel.grid.major = element_line(size = 0.5, color = 'grey80'),
        panel.grid.minor = element_line(size = 0.25, color = 'grey90')) +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = c("1" = "Mon", "2" = "Tue", "3" = "Wed",
                              "4" = "Thu", "5" = "Fri", "6" = "Sat", 
                              "7" = "Sun")) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  facet_wrap(~ Group, scales = "free_y")

totall <- read.csv('Total.csv')
Meth <- totall %>% pivot_longer (cols=c('Car', 'Motorcycle','Total'),
                                 names_to="Type",
                                 values_to='Count')

Meth$Group <- ifelse(Meth$Type %in% c("Car", "Motorcycle"), "Vehicle Type", "Total")

ggplot(Meth, aes(x = as.factor(Day), y = Count, fill = Type)) +
  labs(title = "Tunghai University",
       x = 'Day of the Week', y = 'Number of Vehicles') +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(panel.grid.major = element_line(size = 0.5, color = 'grey80'),
        panel.grid.minor = element_line(size = 0.25, color = 'grey90')) +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = c("1" = "Mon", "2" = "Tue", "3" = "Wed",
                              "4" = "Thu", "5" = "Fri", "6" = "Sat", 
                              "7" = "Sun")) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  facet_wrap(~ Group, scales = "free_y")


