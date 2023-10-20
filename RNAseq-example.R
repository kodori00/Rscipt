
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(xlsx)

options(scipen = 999)

setwd("~/Desktop")

pp<-read_csv("gWATFKO.csv") # define the file


ncol(pp) # check how many different samples

brks = seq(1000, 60000, by=1000)

dat1<-pp %>%dplyr::select(,1)
dat1<-rename(dat1, data = 1)
dat1<-data.frame(table(cut(dat1$data, breaks = seq(from = 0, to = 60000, by = 1000,),dig.lab=10)))%>%
  mutate(brks)%>%rename(s1 = 2)%>%relocate(brks,.before = 2)%>%select(2,3)

dat2<-pp %>%dplyr::select(,2)
dat2<-rename(dat2, data = 1)
dat2<-data.frame(table(cut(dat2$data, breaks = seq(from = 0, to = 60000, by = 1000,),dig.lab=10)))%>%
  mutate(brks)%>%rename(s2 = 2)%>%relocate(brks,.before = 2)%>%select(2,3)

dat3<-pp %>%dplyr::select(,3)
dat3<-rename(dat3, data = 1)
dat3<-data.frame(table(cut(dat3$data, breaks = seq(from = 0, to = 60000, by = 1000,),dig.lab=10)))%>%
  mutate(brks)%>%rename(s3 = 2)%>%relocate(brks,.before = 2)%>%select(2,3)

dat4<-pp %>%dplyr::select(,4)
dat4<-rename(dat4, data = 1)
dat4<-data.frame(table(cut(dat4$data, breaks = seq(from = 0, to = 60000, by = 1000,),dig.lab=10)))%>%
  mutate(brks)%>%rename(s4 = 2)%>%relocate(brks,.before = 2)%>%select(2,3)

dat5<-pp %>%dplyr::select(,5)
dat5<-rename(dat5, data = 1)
dat5<-data.frame(table(cut(dat5$data, breaks = seq(from = 0, to = 60000, by = 1000,),dig.lab=10)))%>%
  mutate(brks)%>%rename(s5 = 2)%>%relocate(brks,.before = 2)%>%select(2,3)

dat6<-pp %>%dplyr::select(,6)
dat6<-rename(dat6, data = 1)
dat6<-data.frame(table(cut(dat6$data, breaks = seq(from = 0, to = 60000, by = 1000,),dig.lab=10)))%>%
  mutate(brks)%>%rename(s6 = 2)%>%relocate(brks,.before = 2)%>%select(2,3)

dat7<-pp %>%dplyr::select(,7)
dat7<-rename(dat7, data = 1)
dat7<-data.frame(table(cut(dat7$data, breaks = seq(from = 0, to = 60000, by = 1000,),dig.lab=10)))%>%
  mutate(brks)%>%rename(s7 = 2)%>%relocate(brks,.before = 2)%>%select(2,3)

dat8<-pp %>%dplyr::select(,8) 
dat8<-rename(dat8, data = 1)
dat8<-data.frame(table(cut(dat8$data, breaks = seq(from = 0, to = 60000, by = 1000,),dig.lab=10)))%>%
  mutate(brks)%>%rename(s8 = 2)%>%relocate(brks,.before = 2)%>%select(2,3)

rf<-merge(dat1,dat2, by= "brks")

rf<-merge(rf,dat3, by= "brks")

rf<-merge(rf,dat4, by= "brks")

rf<-merge(rf,dat5, by= "brks")

rf<-merge(rf,dat6, by= "brks")

rf<-merge(rf,dat7, by= "brks")

rf<-merge(rf,dat8, by= "brks")


rf2 <- rf[,-1]
rownames(rf2) <- rf[,1]


RowSD <- function(x) {
  sqrt(rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1))
}

sum<-rf2%>%mutate(mean = rowMeans(rf2), sd = RowSD(rf2))%>%select(mean,sd)

write.csv(sum, "result-adipocyte.csv")

# to get X axis data

ui<-select(rf, 1)

write.csv(ui, 'y-axis.csv')