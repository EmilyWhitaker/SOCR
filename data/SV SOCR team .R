# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); 
library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)
library(pastecs)
library('unikn')
library(tidyverse)
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
  ylim(0,200)+
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


timeline_SV_limit <- ggplot(SOCRSVs_noNA, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  aes(color = Company)+ 
  ylim(0,200)+
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
timeline_SV_limit
ggsave("Romil_Nivi_companies_alltime_ylim.png", plot = last_plot(), height = 10, width = 12, units = "in")




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





# company trends

SOCRSVs_noNA

SOCRSV_ConvergeOne <-filter(SOCRSVs_noNA, Company == 'ConvergeOne')
SOCR_SVTeam_Totals_Company_CO <- ggplot(SOCRSV_ConvergeOne, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("ConvergeOne Files")+
  #aes(color = Task)+ 
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
  theme_bw()
  #facet_grid('User')
SOCR_SVTeam_Totals_Company_CO

ggsave("ConvergeOne.png", plot = last_plot(), height = 10, width = 12, units = "in")




SOCRSV_SynapseFI <-filter(SOCRSVs_noNA, Company == 'SynapseFI')
SOCR_SVTeam_Totals_Company_SynapseFI <- ggplot(SOCRSV_SynapseFI, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("SynapseFI Files")+
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
  theme_bw()
#facet_grid('User')
SOCR_SVTeam_Totals_Company_SynapseFI

ggsave("SynapseFI.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_DSG <-filter(SOCRSVs_noNA, Company == 'DSG')
SOCR_SVTeam_Totals_Company_DSG <- ggplot(SOCRSV_DSG, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("DSG Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_DSG

ggsave("DSG.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_TrustedHealth <-filter(SOCRSVs_noNA, Company == 'TrustedHealth')
SOCR_SVTeam_Totals_Company_TrustedHealth <- ggplot(SOCRSV_TrustedHealth, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("TrustedHealth Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_TrustedHealth

ggsave("TrustedHealth.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOCRSV_DataRobot <-filter(SOCRSVs_noNA, Company == 'DataRobot')
SOCR_SVTeam_Totals_Company_DataRobot <- ggplot(SOCRSV_DataRobot, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("DataRobot Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_DataRobot

ggsave("DataRobot.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Fiberlight <-filter(SOCRSVs_noNA, Company == 'Fiberlight')
SOCR_SVTeam_Totals_Company_Fiberlight <- ggplot(SOCRSV_Fiberlight, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Fiberlight Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Fiberlight

ggsave("Fiberlight.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Packetfabric <-filter(SOCRSVs_noNA, Company == 'Packetfabric')
SOCR_SVTeam_Totals_Company_Packetfabric <- ggplot(SOCRSV_Packetfabric, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Packetfabric Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Packetfabric

ggsave("Packetfabric.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_NewRelic <-filter(SOCRSVs_noNA, Company == 'NewRelic')
SOCR_SVTeam_Totals_Company_NewRelic <- ggplot(SOCRSV_NewRelic, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("NewRelic Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_NewRelic

ggsave("NewRelic.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_BCMOne <-filter(SOCRSVs_noNA, Company == 'BCMOne')
SOCR_SVTeam_Totals_Company_BCMOne <- ggplot(SOCRSV_BCMOne, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("BCMOne Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_BCMOne

ggsave("BCMOne.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Dwellworks <-filter(SOCRSVs_noNA, Company == 'Dwellworks')
SOCR_SVTeam_Totals_Company_Dwellworks <- ggplot(SOCRSV_Dwellworks, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Dwellworks Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Dwellworks

ggsave("Dwellworks.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Pros_Holding <-filter(SOCRSVs_noNA, Company == 'Pros Holding')
SOCR_SVTeam_Totals_Company_Pros_Holding <- ggplot(SOCRSV_Pros_Holding, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Pros Holding Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Pros_Holding

ggsave("Pros_Holding.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Aveo <-filter(SOCRSVs_noNA, Company == 'Aveo')
SOCR_SVTeam_Totals_Company_Aveo <- ggplot(SOCRSV_Aveo, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Aveo Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Aveo

ggsave("Aveo.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOCRSV_EngageSmart <-filter(SOCRSVs_noNA, Company == 'EngageSmart')
SOCR_SVTeam_Totals_Company_EngageSmart <- ggplot(SOCRSV_EngageSmart, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("EngageSmart Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_EngageSmart

ggsave("EngageSmart.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Ravenswood <-filter(SOCRSVs_noNA, Company == 'Ravenswood')
SOCR_SVTeam_Totals_Company_Ravenswood <- ggplot(SOCRSV_Ravenswood, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Ravenswood Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Ravenswood

ggsave("Ravenswood.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Onapsis <-filter(SOCRSVs_noNA, Company == 'Onapsis')
SOCR_SVTeam_Totals_Company_Onapsis <- ggplot(SOCRSV_Onapsis, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Onapsis Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Onapsis

ggsave("Onapsis.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Sectigo <-filter(SOCRSVs_noNA, Company == 'Sectigo')
SOCR_SVTeam_Totals_Company_Sectigo <- ggplot(SOCRSV_Sectigo, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Sectigo Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Sectigo

ggsave("Sectigo.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Tapad <-filter(SOCRSVs_noNA, Company == 'Tapad')
SOCR_SVTeam_Totals_Company_Tapad <- ggplot(SOCRSV_Tapad, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Tapad Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Tapad

ggsave("Tapad.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOCRSV_Aldevron <-filter(SOCRSVs_noNA, Company == 'Aldevron')
SOCR_SVTeam_Totals_Company_Aldevron <- ggplot(SOCRSV_Aldevron, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Aldevron Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Aldevron

ggsave("Aldevron.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOCRSV_Skyhawk <-filter(SOCRSVs_noNA, Company == 'Skyhawk')
SOCR_SVTeam_Totals_Company_Skyhawk <- ggplot(SOCRSV_Skyhawk, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Skyhawk Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Skyhawk

ggsave("Skyhawk.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOCRSV_AbioMed<-filter(SOCRSVs_noNA, Company == 'AbioMed')
SOCR_SVTeam_Totals_Company_AbioMed <- ggplot(SOCRSV_AbioMed, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("AbioMed Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_AbioMed

ggsave("AbioMed.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_ParentCo<-filter(SOCRSVs_noNA, Company == 'Parent Co')
SOCR_SVTeam_Totals_Company_ParentCo <- ggplot(SOCRSV_ParentCo, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Parent Co Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_ParentCo

ggsave("ParentCo.png", plot = last_plot(), height = 10, width = 12, units = "in")



SOCRSV_Redox<-filter(SOCRSVs_noNA, Company == 'Redox')
SOCR_SVTeam_Totals_Company_Redox <- ggplot(SOCRSV_Redox, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Redox Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Redox

ggsave("Redox.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_TradeStation <-filter(SOCRSVs_noNA, Company == 'TradeStation')
SOCR_SVTeam_Totals_Company_TradeStation <- ggplot(SOCRSV_TradeStation, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("TradeStation Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_TradeStation

ggsave("TradeStation.png", plot = last_plot(), height = 10, width = 12, units = "in")



SOCRSV_Haivision <-filter(SOCRSVs_noNA, Company == 'Haivision')
SOCR_SVTeam_Totals_Company_Haivision <- ggplot(SOCRSV_Haivision, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Haivision Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Haivision

ggsave("Haivision.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Haivision <-filter(SOCRSVs_noNA, Company == 'Haivision')
SOCR_SVTeam_Totals_Company_Haivision <- ggplot(SOCRSV_Haivision, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Haivision Files")+
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
  theme_bw()
SOCR_SVTeam_Totals_Company_Haivision

ggsave("Haivision.png", plot = last_plot(), height = 10, width = 12, units = "in")



