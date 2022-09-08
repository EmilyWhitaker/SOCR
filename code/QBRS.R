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

##### Q1 2022

SOCR_work_by_Task_Type_Q1 <- subset(SOCR_work_by_Task_Type, SOCR_work_by_Task_Type$Date> "2022-01-01" & SOCR_work_by_Task_Type$Date < "2022-03-31")

SOCR_AllTeam_Totals_Task_Q1 <- ggplot(SOCR_work_by_Task_Type_Q1, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Quarter 1 2022")+
  geom_point(size=4)+
  aes(color = Task)+ 
  #ylim(0,200)+
  theme_bw() +
  facet_grid('User')
SOCR_AllTeam_Totals_Task_Q1
ggsave("Task_Q1__Tasks2022.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOSOCR_work_pp_no_task_Q1 <- subset(SOCR_work_by_Person, SOCR_work_by_Person$Date> "2022-01-01" & SOCR_work_by_Person$Date < "2022-03-31")

SOCR_AllTeam_Totals_Q1 <- ggplot(SOSOCR_work_pp_no_task_Q1, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Quarter 1 2022")+
  geom_point(size=4)+
 # aes(color = Task)+ 
  #ylim(0,200)+
  theme_bw() +
  facet_grid('User')
SOCR_AllTeam_Totals_Q1
ggsave("Task_Q1_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")

####### Q2 2022

SOCR_work_by_Task_Type_Q2 <- subset(SOCR_work_by_Task_Type, SOCR_work_by_Task_Type$Date> "2022-04-01" & SOCR_work_by_Task_Type$Date < "2022-06-31")

SOCR_AllTeam_Totals_Task_Q2 <- ggplot(SOCR_work_by_Task_Type_Q2, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Quarter 2 2022")+
  geom_point(size=4)+
  aes(color = Task)+ 
  #ylim(0,200)+
  theme_bw() +
  facet_grid('User')
SOCR_AllTeam_Totals_Task_Q2
ggsave("Task_Q2_Tasks2022.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOSOCR_work_pp_no_task_Q2 <- subset(SOCR_work_by_Person, SOCR_work_by_Person$Date> "2022-04-01" & SOCR_work_by_Person$Date < "2022-06-30")

#SOSOCR_work_pp_no_task_Q2_noNA <- na.omit(SOSOCR_work_pp_no_task_Q2)

SOCR_AllTeam_Totals_Q2 <- ggplot(SOSOCR_work_pp_no_task_Q2_noNA, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Quarter 2 2022")+
  geom_point(size=4)+
  # aes(color = Task)+ 
  #ylim(0,200)+
  theme_bw() +
  facet_grid('User')
SOCR_AllTeam_Totals_Q2
ggsave("Task_Q2_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")

###### Q3 

SOCR_work_by_Task_Type_Q3 <- subset(SOCR_work_by_Task_Type, SOCR_work_by_Task_Type$Date> "2022-07-01" & SOCR_work_by_Task_Type$Date < "2022-09-30")

SOCR_work_by_Task_Type_Q3 <- ggplot(SOCR_work_by_Task_Type_Q3, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Quarter 3 2022")+
  geom_point(size=4)+
  aes(color = Task)+ 
  #ylim(0,200)+
  theme_bw() +
  facet_grid('User')
SOCR_work_by_Task_Type_Q3
ggsave("Task_Q3_Tasks2022.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOSOCR_work_pp_no_task_Q3 <- subset(SOCR_work_by_Person, SOCR_work_by_Person$Date> "2022-07-01" & SOCR_work_by_Person$Date < "2022-09-30")

#SOSOCR_work_pp_no_task_Q2_noNA <- na.omit(SOSOCR_work_pp_no_task_Q2)

SOCR_AllTeam_Totals_Q3 <- ggplot(SOSOCR_work_pp_no_task_Q3, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Quarter 3 2022")+
  geom_point(size=4)+
  # aes(color = Task)+ 
  #ylim(0,200)+
  theme_bw() +
  facet_grid('User')
SOCR_AllTeam_Totals_Q3
ggsave("Task_Q3_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")

######months 

SOCR_work_by_Task_Type_Aug <- subset(SOCR_work_by_Task_Type, SOCR_work_by_Task_Type$Date> "2022-08-01" & SOCR_work_by_Task_Type$Date < "2022-08-31")

SOCR_work_by_Task_Type_Aug22 <- ggplot(SOCR_work_by_Task_Type_Aug, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("August 2022")+
  geom_point(size=4)+
  aes(color = Task)+ 
  #ylim(0,200)+
  theme_bw() +
  facet_grid('User')
SOCR_work_by_Task_Type_Aug22
ggsave("Task_Aug_22_Tasks2022.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOSOCR_work_pp_no_task_aug <- subset(SOCR_work_by_Person, SOCR_work_by_Person$Date> "2022-08-01" & SOCR_work_by_Person$Date < "2022-08-31")

#SOSOCR_work_pp_no_task_Q2_noNA <- na.omit(SOSOCR_work_pp_no_task_Q2)

SOCR_AllTeam_Totals_aug <- ggplot(SOSOCR_work_pp_no_task_aug, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("August 2022")+
  geom_point(size=4)+
  # aes(color = Task)+ 
  #ylim(0,200)+
  theme_bw() +
  facet_grid('User')
SOCR_AllTeam_Totals_aug
ggsave("Task_aug_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")


###### Client based QBRs


SOCR_work_All<- rbind (SOCRSVs, SOCRtrack_Nas)


SOCR_work_All$Date=ymd(SOCR_work_All$Date)

SOCR_work_client_Q2 <- subset(SOCR_work_All, SOCR_work_All$Date < "2022-06-31")



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

