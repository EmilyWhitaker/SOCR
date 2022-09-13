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

########

Jira_df = read_csv('data/Jira.csv')
Jira_df= subset(Jira_df, select = c(`Issue Type`, `Issue key`, Assignee, Reporter, Status, Created, Updated, File_Count))
Jira_df <- Jira_df %>%
  rename( 'Issue_Type'=`Issue Type`, 
          'Issue_Key'= `Issue key`)

Jira_df$Created <- mdy(Jira_df$Created)
Jira_df$Updated <- mdy(Jira_df$Updated)
Jira_df$Month_Created <- as.numeric(format(Jira_df$Created,'%m'))
Jira_df$Month_Updated <- as.numeric(format(Jira_df$Updated,'%m'))


Creation_Date_files <-  ggplot(data = Jira_df, aes(x = Month_Created)) +
  geom_bar()+
  theme_bw() 

Creation_Date_files
ggsave("CreationDateofTicket.png", plot = last_plot(), height = 10, width = 12, units = "in")


Updated_Date_files  <- ggplot(data = Jira_df, aes(x = Month_Updated, y=File_Count)) +
  geom_bar()+
  theme_bw() 
Updated_Date_files

ggsave("UpdatedDateofTicket.png", plot = last_plot(), height = 10, width = 12, units = "in")






bar chart for the tickets 

ill do a dumb pie 






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


