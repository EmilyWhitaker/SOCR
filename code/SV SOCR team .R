# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)
library(pastecs)
library('unikn')
library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)

#load in data 

SOCRSVs = read_csv('data/SOCR_SVteam.csv')
SOCRSVs$Date=mdy(SOCRSVs$Date)
SOCRSVs$month<- month(SOCRSVs$Date)
SOCRSVs= subset(SOCRSVs, select = c(Company, Date, Agreements, User, Task))
SOCRSVs_noNA <- na.omit(SOCRSVs)


timeline_SV <- ggplot(SOCRSVs_noNA, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  aes(color = Company)+ 
 # ylim(0,200)+
  geom_vline(xintercept = as.numeric(as.Date("2021-12-31")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-03-31")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-06-30")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-09-30")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2021-10-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-11-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-02-28")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-04-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-05-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-07-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-08-31")), linetype=3)+
  theme_bw() +
  facet_grid('User')
timeline_SV
ggsave("Romil_Nivi_companies_alltime.png", plot = last_plot(), height = 10, width = 12, units = "in")




SOCRSV_Romil <-filter(SOCRSVs_noNA, User == 'Romil')
#SOCRtrack_perperson_Maddy  <- aggregate(SOCRtrack_Maddy["Agreements"], by=SOCRtrack_Maddy["Date"], sum)
SOCRSV_Romil_2_Task  <- aggregate(SOCRSV_Romil$Agreements, by = list(SOCRSV_Romil$Date, SOCRSV_Romil$User,SOCRSV_Romil$Task), FUN = sum)

SOCRSV_Romil_2_Task<-SOCRSV_Romil_2_Task %>%
  rename( 'Date'=Group.1, 
          'User'= Group.2,
          'Task' = Group.3,
          "Agreements"= x)


SOCRSV_Romil_Total <- aggregate(SOCRSV_Romil$Agreements, by = list(SOCRSV_Romil$Date, SOCRSV_Romil$User), FUN = sum)

SOCRSV_Romil_Total<-SOCRSV_Romil_Total %>%
  rename( 'Date'=Group.1, 
          'User'= Group.2,
          "Agreements"= x)

SOCRSV_Nivedita <-filter(SOCRSVs_noNA, User == 'Nivedita')
SOCRSV_Nivedita_2_Task  <- aggregate(SOCRSV_Nivedita$Agreements, by = list(SOCRSV_Nivedita$Date, SOCRSV_Nivedita$User,SOCRSV_Nivedita$Task), FUN = sum)

SOCRSV_Nivedita_2_Task<-SOCRSV_Nivedita_2_Task %>%
  rename( 'Date'=Group.1, 
          'User'= Group.2,
          'Task' = Group.3,
          "Agreements"= x)


SOCRSV_Nivedita_Total <- aggregate(SOCRSV_Nivedita$Agreements, by = list(SOCRSV_Nivedita$Date, SOCRSV_Nivedita$User), FUN = sum)

SOCRSV_Nivedita_Total<-SOCRSV_Nivedita_Total %>%
  rename( 'Date'=Group.1, 
          'User'= Group.2,
          "Agreements"= x)



SOCR_SVTeam_Totals<- rbind (SOCRSV_Nivedita_Total, SOCRSV_Romil_Total)


SOCR_SVTeam_Totals_time <- ggplot(SOCR_SVTeam_Totals, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  #ylim(0,200)+
  geom_vline(xintercept = as.numeric(as.Date("2021-12-31")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-03-31")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-06-30")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-09-30")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2021-10-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-11-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-02-28")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-04-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-05-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-07-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-08-31")), linetype=3)+
  theme_bw() +
  facet_grid('User')
SOCR_SVTeam_Totals_time

ggsave("Romil_Nivi_totals_alltime.png", plot = last_plot(), height = 10, width = 12, units = "in")




SOCR_SVTeam_Totals_Task<- rbind (SOCRSV_Nivedita_2_Task, SOCRSV_Romil_2_Task)


SOCR_SVTeam_Totals_Task_time <- ggplot(SOCR_SVTeam_Totals_Task, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  aes(color = Task)+ 
  #ylim(0,200)+
  geom_vline(xintercept = as.numeric(as.Date("2021-12-31")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-03-31")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-06-30")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-09-30")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2021-10-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-11-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-02-28")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-04-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-05-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-07-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-08-31")), linetype=3)+
  theme_bw() +
  facet_grid('User')
SOCR_SVTeam_Totals_Task_time

ggsave("Romil_Nivi_totals_byTask_alltime.png", plot = last_plot(), height = 10, width = 12, units = "in")


