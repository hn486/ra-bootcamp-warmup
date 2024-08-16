## file: analysis
## author: hitomi

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(gtable)
library(arsenal)

setwd("C:/Users/hitomi/Documents/Project/Boot camp/warmup training package/01_data")
df=read_csv("master_hitomi.csv")

setwd("C:/Users/hitomi/Documents/Project/Boot camp/warmup training package/01_data/intermediate")
dfc=read_csv("master.csv")

#(a)記述統計
countna=df |> summarise(across(everything(), ~ sum(is.na(.))))
table(countna)

table_one=tableby(group ~ ., data=df) 
summary(table_one, title = "Gapminder Data")

df |> summarise(tot4yrgrads, groups())

##gradrate
##semerate

#(b)散布図4年卒業率
##女子学生割合
##白人学生割合
##学費

#(c)回帰分析




st(df) |> 
  group_by(group1, group2, group3)                          %>%
  summarise_each(funs(mean, sd, length, se), dv1, dv2, dv3) %>%
  ungroup



###
fig1=data.frame()
for (i in unique(sem$year)){
  print(i)
  local=sem[which(sem$year ==i & sem$semester==1), ]
  totalyear=sem[which(sem$year ==i), ]
  fig1[i, "prop"]=nrow(local)/nrow(totalyear)
  fig1[i, "nokori"]=(nrow(totalyear)-nrow(local))/nrow(totalyear)
}


fig1=na.omit(fig1)
fig1 <- cbind(year = rownames(fig1), fig1)
fig1=fig1[order(fig1$year), ]
fig1$year=as.numeric(fig1$year)
fig1$prop=as.numeric(fig1$prop)
rownames(fig1) <- 1:nrow(fig1)

g1=ggplot()+geom_line(
  data = fig1,
  aes(x =year, y=prop, linetype = "longdash"))



###
fig2=data.frame()
for (i in unique(grad$year)){
  print(i)
  local=grad[which(grad$year ==i), ]$tot4yrgrads
  print(local)
  
  local=sum(local)
  totalyear=grad[which(grad$year ==i), ]
  totalyear=sum(totalyear$totcohortsize)
  fig2[i, "prop"]=local/totalyear}
fig2=na.omit(fig2)
fig2 <- cbind(year = rownames(fig2), fig2)
fig2=fig2[order(fig2$year), ]
fig2$year=as.numeric(fig2$year)
fig2$prop=as.numeric(fig2$prop)
rownames(fig2) <- 1:nrow(fig2)



sf=abs(round(mean(fig2$prop-fig1$prop),3))
g3=ggplot()+
  geom_line(data = fig1,aes(x =year, y=prop, col="prop semester"))+
  geom_line(data = fig2,aes(x =year, y=prop+sf, col="grad rate"))+
  scale_y_continuous(name = "prop semester",sec.axis = 
                       sec_axis( ~ .-sf, name="grad rate"))+
  scale_color_brewer(type = "qual", palette = "Dark2") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  theme(legend.title=element_blank())+
  theme_minimal()

pdf("combined.pdf")
plot(g3)
dev.off()




