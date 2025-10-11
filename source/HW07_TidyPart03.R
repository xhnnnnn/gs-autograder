# Author: Olivia Devitt
# Date: 2025-10-10
# Purpose: HW07_TidyPart03.R
# ------------------------------------------------------------------------------

#1. 
library(tidyverse)
#a 
shuttle <- readRDS("shuttle.RDS")
shuttle$TempC <-(shuttle$Temp - 32)*5/9


shuttle_plot <- ggplot(shuttle,aes(x=TempC,y=Incidents))+ geom_point(color="black") +
  geom_smooth(method = "loess",  formula = 'y ~ x',color='blue') + 
  labs(x = 'Temperature (°C)',y='Incidents', title = 'Incidents vs. Temperature (°C)') +
  theme_minimal()



#2. 
rex <- read.csv("rex.csv")
bonelevels = paste0("Bone",1:12)
rex$Bone <- factor(rex$Bone,levels=bonelevels,ordered=TRUE)

library(dplyr)

rex_summary<-rex %>% group_by(Bone) %>% summarize(n=n(),mean=mean(Oxygen),sd=sd(Oxygen),lcl=mean(Oxygen)-2*sd(Oxygen)/sqrt(n),ucl=mean(Oxygen)+2*sd/sqrt(n)) %>% ungroup()

rex_plot<-ggplot(rex,aes(x=Bone,y=Oxygen)) +
  geom_point(color="black",position=position_jitter()) +
  geom_pointrange(data=rex_summary, aes(x=Bone,y=mean,ymin=lcl,ymax=ucl),color="red")+
  labs(title="Oxygen Isotopic Composition of Vertebrate Bone Phosphate", subtitle="in 12 bones of a single T.rex speciment",x="Bone ID",y="Oxygen Isoopic Composition")+
  theme_minimal()


#3
load("blocks.RData")
B1$Block <- "B1"
B2$Block <- "B2"
B3$Block <- "B3"
B4$Block <- "B4"
grazer <-rbind(B1,B2,B3,B4)
grazer$Treat <- factor(grazer$Treat, levels=c("C","f","fF","L","Lf","LfF"), ordered=TRUE)
grazer_plot= ggplot(grazer, aes(x=grazer$Treat,y=grazer$Cover,color=Treat,shape=Treat)) + 
                      geom_point() + labs(x = "Treat",y="Cover") +
                      facet_wrap(~Block,nrow=2,ncol=2) +
                    scale_color_manual(values = c("C"   = "#d95f02",
                      "f"   = "#e6ab02",
                      "fF"  = "#1b9e77",
                      "L"   = "#007BA7",
                      "Lf"  = "#7570b3",
                      "LfF" = "#e7298a"))+
                      theme_minimal() +
                      theme(strip.background=element_rect(fill = "gray"))

grazer_summary<-grazer %>% group_by(Block,Treat) %>% summarize(grazer_means = Cover, .groups="drop")
grazer_summary$Treat <- factor(grazer_summary$Treat, levels=c("C","f","fF","L","Lf","LfF"), ordered=TRUE)
grazer_summary <- grazer_summary%>% arrange(Block,Treat)

grazer_means <- grazer_summary %>% pivot_wider(names_from= Treat, values_from=grazer_means)

grazer_summary$Block<-factor(grazer_summary$Block,levels=c("B1","B2","B3","B4"))
grazer_summary$Treat<-factor(grazer_summary$Treat,levels=c("C","f","fF","L","Lf","LfF"),ordered=TRUE)

grazer_summary_plot<-ggplot(grazer_summary,aes(x=Block,y=grazer_means,group=Treat,color=Treat,shape=Treat))+
  geom_line() + geom_point() + labs(x="Block",y="Cover Means") +
scale_color_manual(values = c("C"   = "#d95f02",
                              "f"   = "#e6ab02",
                              "fF"  = "#1b9e77",
                              "L"   = "#007BA7",
                              "Lf"  = "#7570b3",
                              "LfF" = "#e7298a"))+
  theme_minimal()



#5

WorldPhones_df <-as.data.frame(WorldPhones)
WorldPhones_df$Year <-as.numeric(rownames(WorldPhones))
WorldPhones_df <- WorldPhones_df[,c("Year",colnames(WorldPhones))]
WorldPhones_transformed<-WorldPhones_df %>% pivot_longer(cols=-Year,names_to="Region",values_to="Count")

ggplot(WorldPhones_transformed, aes(x=Year,y=Count,color=Region,linetype=Region)) + geom_line()+
  labs(x="Year",y="Count")+ theme_minimal()

