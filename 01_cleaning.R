## file: cleaning
## author: hitomi

library(tidyverse)
library(dplyr)
library(readxl)

#(a) Semester Dataの整形
setwd("C:/Users/hitomi/Documents/Project/Boot camp/warmup training package/01_data/raw/semester_dummy")
sem1 = read_csv("semester_data_1.csv", skip=1)
sem2 = read_csv("semester_data_2.csv") |>
  rename(unitid="x1", instnm="x2", semester="x3", quarter="x4", year="x5", Y="x6")
sem = bind_rows(sem1, sem2) |> select(-Y)
rm(sem1, sem2)

sem=sem |> 
  group_by(unitid) |> 
  mutate(meansem=mean(semester)) |> 
  mutate(never=if_else(meansem==0, 1, 0))

table(sem$meansem)


#(b) Gradrate Dataの整形
setwd("C:/Users/hitomi/Documents/Project/Boot camp/warmup training package/01_data/raw/outcome")
# 1991から2016まで結合
file_list = list.files(pattern = ".xlsx")
grad = NULL
for (i in 1:length(file_list)){
  alldata = read_excel(file_list[i])
  grad = rbind(grad, alldata)
}
rm(alldata, file_list)
# データ型の整理、変数の整理
grad=grad |>
  mutate(totcohortsize=as.numeric(totcohortsize)) |>
  mutate(m_4yrgrads=as.numeric(m_4yrgrads)) |>
  mutate(women_gradrate_4yr=women_gradrate_4yr/100) |>
  mutate(men_gradrate_4yr=m_4yrgrads/m_cohortsize) |>
  mutate(tot_gradrate_4yr=totcohortsize/tot4yrgrads/100) |>
  mutate(tot_gradrate_4yr=signif(tot_gradrate_4yr, 3)) |>
  filter(year>=1991 & year<=2010)
#grad=na.omit(grad)

#(c) Covariates Dataの整形
setwd("C:/Users/hitomi/Documents/Project/Boot camp/warmup training package/01_data/raw/covariates")
cov=read_excel("covariates.xlsx") |> 
  rename(unitid=university_id)

#last4 = cov |> pull(unitid) |> str_sub(start = -4)
#unique(last4)
#rm(last4)
summary(str_length(cov$unitid))
cov$unitid=str_sub(cov$unitid, end=6)

print(unique(cov$category))
cov=cov |> 
  pivot_wider(names_from=category, 
              values_from=value) |>
  mutate_all(function(x) as.numeric(as.character(x))) |>
  filter(year>=1991 & year<=2010 & year!=1994) |>
  filter(unitid %in% grad$unitid)

#(d) Master Dataの作成
df=left_join(grad, cov, join_by(unitid, year))
df=left_join(df, sem, join_by(unitid, year))

setwd("C:/Users/hitomi/Documents/Project/Boot camp/warmup training package/01_data")
write_csv(df, "master_hitomi.csv")

