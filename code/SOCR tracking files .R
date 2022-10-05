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


SOCRtrack_Nas = read_csv('data/SOCR2forreal_NAs.csv')
SOCRtrack_Nas$Date=mdy(SOCRtrack_Nas$Date)
SOCRtrack_Nas$month<- month(SOCRtrack_Nas$Date)


SOCRtrack_Nas= subset(SOCRtrack_Nas, select = c(Company, Date, Agreements, User, Task))

timeline <- ggplot(SOCRtrack_Nas, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  aes(color = Company)+ 
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
  
timeline
ggsave("MMTE.png", plot = last_plot(), height = 10, width = 12, units = "in")

#totals per week per person

SOCRtrack1 = read_csv('data/SOCR2forreal.csv')
SOCRtrack1$Date=mdy(SOCRtrack1$Date)
SOCRtrack1$month<- month(SOCRtrack1$Date)

SOCRtrack1= subset(SOCRtrack1, select = c(Company, Date, Agreements, User, Task))

SOCRtrack_Maddy <-filter(SOCRtrack1, User == 'Maddy')
#SOCRtrack_perperson_Maddy  <- aggregate(SOCRtrack_Maddy["Agreements"], by=SOCRtrack_Maddy["Date"], sum)
SOCRtrack_Maddy_2  <- aggregate(SOCRtrack_Maddy$Agreements, by = list(SOCRtrack_Maddy$Date, SOCRtrack_Maddy$User, SOCRtrack_Maddy$Task), FUN = sum)

SOCRtrack_Maddy_2<-SOCRtrack_Maddy_2 %>%
  rename( 'Date'=Group.1, 
          'User'= Group.2,
          'Task' = Group.3,
          "Agreements"= x)

SOCRtrack_Emily_1=subset(SOCRtrack1, User== 'Emily')
SOCRtrack_Emily_2  <- aggregate(SOCRtrack_Emily_1$Agreements, by = list(SOCRtrack_Emily_1$Date, SOCRtrack_Emily_1$User,SOCRtrack_Emily_1$Task ), FUN = sum)


SOCRtrack_Emily_2<-SOCRtrack_Emily_2 %>%
  rename( 'Date'=Group.1, 
          'User'= Group.2,
          'Task' = Group.3,
          "Agreements"= x)

SOCRtrack_Mat_1=subset(SOCRtrack1, User== 'Mat')
SOCRtrack_Mat_2  <- aggregate(SOCRtrack_Mat_1$Agreements, by = list(SOCRtrack_Mat_1$Date, SOCRtrack_Mat_1$User,SOCRtrack_Mat_1$Task ), FUN = sum)


SOCRtrack_Mat_2<-SOCRtrack_Mat_2 %>%
  rename( 'Date'=Group.1, 
          'User'= Group.2,
          'Task' = Group.3,
          "Agreements"= x)

SOCRtrack_Tanya_1=subset(SOCRtrack1, User== 'Tanya')
SOCRtrack_Tanya_2  <- aggregate(SOCRtrack_Tanya_1$Agreements, by = list(SOCRtrack_Tanya_1$Date, SOCRtrack_Tanya_1$User,SOCRtrack_Tanya_1$Task ), FUN = sum)


SOCRtrack_Tanya_2<-SOCRtrack_Tanya_2 %>%
  rename( 'Date'=Group.1, 
          'User'= Group.2,
          'Task' = Group.3,
          "Agreements"= x)


SOCR_work_pp_ME<- rbind (SOCRtrack_Maddy_2, SOCRtrack_Emily_2)


SOCR_work_pp_MMTE<- rbind (SOCRtrack_Maddy_2, SOCRtrack_Emily_2,SOCRtrack_Tanya_2,SOCRtrack_Mat_2 )



#SOCR_work_pp2<-SOCR_work_pp_ME %>%
#  rename( 'Maddy'=Agreements.x, 
#          'Emily'= Agreements.y)


timeline2 <- ggplot(SOCR_work_pp_ME, aes(x= Date, y=Agreements))+
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
timeline2

ggsave("MaddyandEmilyOCR.png", plot = last_plot(), height = 10, width = 12, units = "in")


timeline_all_OCR <- ggplot(SOCR_work_pp_MMTE, aes(x= Date, y=Agreements))+
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
timeline_all_OCR

ggsave("AllOCR.png", plot = last_plot(), height = 10, width = 12, units = "in")


##### clients 

SOCRtrack_Nas


SOCRSV_TripleLift <-filter(SOCRtrack_Nas, Company == 'TripleLift')

SOCR_Totals_Company_TripleLift <- ggplot(SOCRSV_TripleLift, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("TripleLift Files")+
  geom_point(size=4)+
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
SOCR_Totals_Company_TripleLift

ggsave("TripleLift.png", plot = last_plot(), height = 10, width = 12, units = "in")



SOCRSV_CyberArk<-filter(SOCRtrack_Nas, Company == 'CyberArk')

SOCR_Totals_Company_CyberArk<- ggplot(SOCRSV_CyberArk, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("CyberArk Files")+
  geom_point(size=4)+
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
SOCR_Totals_Company_CyberArk

ggsave("CyberArk.png", plot = last_plot(), height = 10, width = 12, units = "in")



SOCRSV_Accelya<-filter(SOCRtrack_Nas, Company == 'Accelya')
SOCRSV_Accelya_noNA <- na.omit(SOCRSV_Accelya)

SOCR_Totals_Company_Accelya<- ggplot(SOCRSV_Accelya_noNA, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Accelya Files")+
  geom_point(size=4)+
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
SOCR_Totals_Company_Accelya

ggsave("Accelya.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Rokt<-filter(SOCRtrack_Nas, Company == 'Rokt')
SOCRSV_Rokt_noNA <- na.omit(SOCRSV_Rokt)

SOCR_Totals_Company_Rokt <- ggplot(SOCRSV_Rokt_noNA, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Rokt Files")+
  geom_point(size=4)+
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
SOCR_Totals_Company_Rokt

ggsave("Rokt.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOCRSV_AVMed<-filter(SOCRtrack_Nas, Company == 'AVMed')
SOCRSV_AVMed_noNA <- na.omit(SOCRSV_AVMed)

SOCR_Totals_Company_AVMed <- ggplot(SOCRSV_AVMed_noNA, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("AVMed Files")+
  geom_point(size=4)+
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
SOCR_Totals_Company_AVMed

ggsave("AVMed.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Ceros<-filter(SOCRtrack_Nas, Company == 'Ceros')
SOCRSV_Ceros_noNA <- na.omit(SOCRSV_Ceros)

SOCR_Totals_Company_Ceros <- ggplot(SOCRSV_Ceros_noNA, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Ceros Files")+
  geom_point(size=4)+
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
SOCR_Totals_Company_Ceros

ggsave("Ceros.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCRSV_Guideline<-filter(SOCRtrack_Nas, Company == 'Guideline')
SOCRSV_Guideline_noNA <- na.omit(SOCRSV_Guideline)

SOCR_Totals_Company_Guideline <- ggplot(SOCRSV_Guideline_noNA, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Guideline Files")+
  geom_point(size=4)+
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
SOCR_Totals_Company_Guideline

ggsave("Guideline.png", plot = last_plot(), height = 10, width = 12, units = "in")



SOCRSV_Gsquared<-filter(SOCRtrack_Nas, Company == 'Gsquared')
SOCRSV_Gsquared_noNA <- na.omit(SOCRSV_Gsquared)

SOCR_Totals_Company_Gsquared <- ggplot(SOCRSV_Gsquared_noNA, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("G Squared Files")+
  geom_point(size=4)+
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
SOCR_Totals_Company_Gsquared

ggsave("Gsquared.png", plot = last_plot(), height = 10, width = 12, units = "in")



