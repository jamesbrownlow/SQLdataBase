library("dplyr")
library('dbplyr')
library("RSQLite")
library("DBI")
library('tigerstats')

trials <- DBI::dbConnect(RSQLite::SQLite(), "trials.db")
dbListTables(trials)
tbl_participants <- tbl(trials, "participants")
tbl_AZ = tbl_participants %>% filter(trial=="Pfizer",  infected =='B1.351')

table = xtabs(~infected+treatment, data=tbl_AZ)
rowPerc(table)
colPerc(table)

dd2 <- tbl_AZ %>% group_by(infected,treatment) %>% 
  summarize(count=n()) %>% mutate(prcnt=count/sum(count))

basicC <- ggplot(dd2,aes(x=treatment,y=count,fill=infected))
basicC + geom_bar(stat="identity",position="dodge")
#Now for percentage plot

basicCC <- ggplot(dd2,aes(x=infected,y=prcnt*100,fill=treatment)) 
basicCC + geom_bar(stat="identity", position = "dodge")

