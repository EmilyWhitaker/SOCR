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

#write.csv(SOSOCR_work_pp_no_task_Q1,"SOSOCR_work_pp_no_task_Q1.csv", row.names = FALSE)


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
#write.csv(SOCR_work_by_Task_Type_Q2,"SOCR_work_by_Task_Type_Q2.csv", row.names = FALSE)



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



SOCR_work_by_Task_Type_Agg_2 = read_csv('SOCR_work_by_Task_Type_Agg.csv')
SOCR_work_by_Task_Type_Agg_2$Date=mdy(SOCR_work_by_Task_Type_Agg_2$Date)


#SOCR_work_by_Task_Type_Agg$Date=ymd(SOCR_work_by_Task_Type_Agg$Date)
#write.csv(SOCR_work_by_Task_Type_Agg,"SOCR_work_by_Task_Type_Agg.csv", row.names = FALSE)

SOCR_work_by_Task_Type_Agg_Q2 <- subset(SOCR_work_by_Task_Type_Agg_2, SOCR_work_by_Task_Type_Agg_2$Date> "2022-04-01" & SOCR_work_by_Task_Type_Agg_2$Date < "2022-06-30")

SOCR_Task_q2 <- ggplot(SOCR_work_by_Task_Type_Agg_Q2, aes(x= Date, y=Agreements, colour=Task))+
  geom_line()+
  geom_point(size=4)+
  #ylim(0,600)+
  ggtitle("Quarter 2 2022")+
  geom_vline(xintercept = as.numeric(as.Date("2021-10-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-11-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-02-28")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-04-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-05-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-07-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-08-31")), linetype=3)+
  theme_bw() 
SOCR_Task_q2

ggsave("AllWorkTypes_Q2_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCR_work_by_Task_Type_Agg_Q1 <- subset(SOCR_work_by_Task_Type_Agg_2, SOCR_work_by_Task_Type_Agg_2$Date> "2022-01-01" & SOCR_work_by_Task_Type_Agg_2$Date < "2022-03-31")

SOCR_Task_q1 <- ggplot(SOCR_work_by_Task_Type_Agg_Q1, aes(x= Date, y=Agreements, colour=Task))+
  geom_line()+
  geom_point(size=4)+
  #ylim(0,600)+
  ggtitle("Quarter 1 2022")+
  geom_vline(xintercept = as.numeric(as.Date("2021-10-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-11-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-02-28")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-04-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-05-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-07-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-08-31")), linetype=3)+
  theme_bw() 
SOCR_Task_q1

ggsave("AllWorkTypes_Q1_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")


###### Q3 

SOCR_work_by_Task_Type_Q3 <- subset(SOCR_work_by_Task_Type, SOCR_work_by_Task_Type$Date> "2022-07-01" & SOCR_work_by_Task_Type$Date < "2022-09-30")


SOCR_work_by_Task_Type_Q3_graph <- ggplot(SOCR_work_by_Task_Type_Q3, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Quarter 3 2022")+
  geom_point(size=4)+
  aes(color = Task)+ 
  #ylim(0,200)+
  theme_bw() +
  facet_grid('User')
SOCR_work_by_Task_Type_Q3_graph
ggsave("Task_Q3_Tasks2022.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOSOCR_work_pp_no_task_Q3 <- subset(SOCR_work_by_Person, SOCR_work_by_Person$Date> "2022-07-01" & SOCR_work_by_Person$Date < "2022-09-30")

#write.csv(SOSOCR_work_pp_no_task_Q3,"SOSOCR_work_pp_no_task_Q3.csv", row.names = FALSE)


#SOSOCR_work_pp_no_task_Q2_noNA <- na.omit(SOSOCR_work_pp_no_task_Q2)

SOCR_AllTeam_Totals_Q3 <- ggplot(SOSOCR_work_pp_no_task_Q3, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("Quarter 3 2022")+
  geom_point(size=4)+
  # aes(color = Task)+ 
  ylim(0,700)+
  theme_bw() +
  facet_grid('User')
SOCR_AllTeam_Totals_Q3
ggsave("Task_Q3_2022_ylim.png", plot = last_plot(), height = 10, width = 12, units = "in")

######months 

SOCR_work_by_Task_Type_Sept <- subset(SOCR_work_by_Task_Type, SOCR_work_by_Task_Type$Date> "2022-09-01" & SOCR_work_by_Task_Type$Date < "2022-09-30")

SOCR_work_by_Task_Type_Spet22 <- ggplot(SOCR_work_by_Task_Type_Sept, aes(x= Date, y=Agreements))+
  geom_line()+
  ggtitle("September 2022")+
  geom_point(size=4)+
  aes(color = Task)+ 
  ylim(0,200)+
  theme_bw() +
  facet_grid('User')
SOCR_work_by_Task_Type_Spet22
ggsave("Task_Sept_22_Tasks2022_ylim.png", plot = last_plot(), height = 10, width = 12, units = "in")


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





###########


SOCR_work_by_Task_Type_Agg_Q1 <- subset(SOCR_work_by_Task_Type_Agg, SOCR_work_by_Task_Type_Agg$Date> "2022-01-01" & SOCR_work_by_Task_Type_Agg$Date < "2022-03-31")

SOCR_Totals_Task_time_Q1 <- ggplot(SOCR_work_by_Task_Type_Agg_Q1, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  # ylim(0,700)+
  ggtitle("File Counts Per Weekly Work Type Q1")+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-04-01")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-07-01")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-09-30")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2021-10-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-11-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-03-01")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-04-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-06-01")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-08-01")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-09-01")), linetype=3)+
  theme_bw() +
  facet_grid('Task')
SOCR_Totals_Task_time_Q1

ggsave("AllTaskType_Q1.png", plot = last_plot(), height = 10, width = 12, units = "in")


SOCR_work_by_Task_Type_Agg_H1 <- subset(SOCR_work_by_Task_Type_Agg, SOCR_work_by_Task_Type_Agg$Date> "2022-01-01" & SOCR_work_by_Task_Type_Agg$Date < "2022-06-30")


SOCR_Totals_Task_time_H1 <- ggplot(SOCR_work_by_Task_Type_Agg_H1, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  # ylim(0,700)+
  ggtitle("File Counts Per Weekly Work Type H1")+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-04-01")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-07-01")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2022-09-30")), linetype=1)+
  geom_vline(xintercept = as.numeric(as.Date("2021-10-31")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-11-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-03-01")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-04-30")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-06-01")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-08-01")), linetype=3)+
  geom_vline(xintercept = as.numeric(as.Date("2022-09-01")), linetype=3)+
  theme_bw() +
  facet_grid('Task')
SOCR_Totals_Task_time_H1

ggsave("AllTaskType_H1.png", plot = last_plot(), height = 10, width = 12, units = "in")




SOCR_work_by_Task_Type_Agg_H1 <- subset(SOCR_work_by_Task_Type_Agg, SOCR_work_by_Task_Type_Agg$Date> "2022-04-01" & SOCR_work_by_Task_Type_Agg$Date < "2022-06-30")


SOCR_Totals_Task_time <- ggplot(SOCR_work_by_Task_Type_Agg, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  # ylim(0,700)+
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
  facet_grid('Task')
SOCR_Totals_Task_time

ggsave("AllTaskType.png", plot = last_plot(), height = 10, width = 12, units = "in")









###### Client based QBRs

#SOSOCR_work_pp_no_task_Q3 <- subset(SOCR_work_by_Person, SOCR_work_by_Person$Date> "2022-07-01" & SOCR_work_by_Person$Date < "2022-09-30")

SOCRSVs_noNA_Q1<- subset(SOCRSVs_noNA, SOCRSVs_noNA$Date> "2022-01-01" & SOCRSVs_noNA$Date < "2022-03-31")

timeline_SV_Q1 <- ggplot(SOCRSVs_noNA_Q1, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+  
  ggtitle("Smart Value and Party Fixes Q1 2022")+
  aes(color = Company)+ 
  # ylim(0,200)+
  theme_bw() +
  facet_grid('User')
timeline_SV_Q1
ggsave("Task_SVs_Q1_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOCRSVs_noNA_Q2<- subset(SOCRSVs_noNA, SOCRSVs_noNA$Date> "2022-04-01" & SOCRSVs_noNA$Date < "2022-06-30")

timeline_SV_Q2 <- ggplot(SOCRSVs_noNA_Q2, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Smart Value and Party Fixes Q2 2022")+
  aes(color = Company)+ 
  # ylim(0,200)+
  theme_bw() +
  facet_grid('User')
timeline_SV_Q2
ggsave("Task_SVs_Q2_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOCRSVs_noNA_Q3<- subset(SOCRSVs_noNA, SOCRSVs_noNA$Date> "2022-07-01" & SOCRSVs_noNA$Date < "2022-09-30")

timeline_SV_Q3 <- ggplot(SOCRSVs_noNA_Q3, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Smart Value and Party Fixes Q3 2022- ylimit")+
  aes(color = Company)+ 
  ylim(0,200)+
  theme_bw() +
  facet_grid('User')
timeline_SV_Q3
ggsave("Task_SVs_Q3_ylim_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOCRSVs_noNA_Aug<- subset(SOCRSVs_noNA, SOCRSVs_noNA$Date> "2022-08-01" & SOCRSVs_noNA$Date < "2022-08-31")

timeline_SV_Aug <- ggplot(SOCRSVs_noNA_Aug, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("Smart Value and Party Fixes August, y limit")+
  aes(color = Company)+ 
  ylim(0,200)+
  theme_bw() +
  facet_grid('User')
timeline_SV_Aug
ggsave("Task_SVs_Aug_2022_limit.png", plot = last_plot(), height = 10, width = 12, units = "in")

##### OCR QBRS
SOCRtrack_Nas

SOCRtrack_Nas_Q1<- subset(SOCRtrack_Nas, SOCRtrack_Nas$Date> "2022-01-01" & SOCRtrack_Nas$Date < "2022-03-31")

timeline_OCR_Q1 <- ggplot(SOCRtrack_Nas_Q1, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+  
  ggtitle("OCR Fixes Q1 2022")+
  aes(color = Company)+ 
  # ylim(0,200)+
  theme_bw() +
  facet_grid('User')
timeline_OCR_Q1
ggsave("Task_OCR_Q1_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOCRtrack_Nas_Q2<- subset(SOCRtrack_Nas, SOCRtrack_Nas$Date> "2022-04-01" & SOCRtrack_Nas$Date < "2022-06-30")

timeline_OCR_Q2 <- ggplot(SOCRtrack_Nas_Q2, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("OCR Fixes Q2 2022")+
  aes(color = Company)+ 
  # ylim(0,200)+
  theme_bw() +
  facet_grid('User')
timeline_OCR_Q2
ggsave("Task_OCR_Q2_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")

SOCRtrack_Nas_Q3<- subset(SOCRtrack_Nas, SOCRtrack_Nas$Date> "2022-07-01" & SOCRtrack_Nas$Date < "2022-09-30")

timeline_OCR_Q3 <- ggplot(SOCRtrack_Nas_Q3, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("OCR Fixes Q3 2022- ylimit")+
  aes(color = Company)+ 
  #ylim(0,200)+
  theme_bw() +
  facet_grid('User')
timeline_OCR_Q3
ggsave("Task_OCR_Q3_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")

##### monthlies 

SOCRtrack_Nas_Jan<- subset(SOCRtrack_Nas, SOCRtrack_Nas$Date> "2022-01-01" & SOCRtrack_Nas$Date < "2022-01-31")

timeline_OCR_Jan <- ggplot(SOCRtrack_Nas_Jan, aes(x= Date, y=Agreements))+
  geom_line()+
  geom_point(size=4)+
  ggtitle("OCR Fixes January")+
  aes(color = Company)+ 
 # ylim(0,100)+
  theme_bw() +
  facet_grid('User')
timeline_OCR_Jan
ggsave("OCR_Jan_2022.png", plot = last_plot(), height = 10, width = 12, units = "in")





