library(RCurl) 
URL <- "https://data.cityofnewyork.us/api/views/erm2-nwe9/rows.csv" 
x <- getURL(URL, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)


#test <- read.csv("/Users/nadinekhattak/Downloads/311_Service_Requests_from_2010_to_Present (1).csv")
test1 <- read.csv("/Users/nadinekhattak/Downloads/311_Service_Requests_from_2010_to_Present (2).csv")
test2 <- read.csv("/Users/nadinekhattak/Downloads/311_Service_Requests_from_2010_to_Present.csv")
table(test1$Created.Date)
summary(test1$Created.Date)

foo <- data.frame(do.call('rbind', strsplit(as.character(test1$Created.Date),' ',fixed=TRUE)))
foo2 <- data.frame(do.call('rbind', strsplit(as.character(test2$Created.Date),' ',fixed=TRUE)))

table(foo$X1)
foo$X1 <- as.character(foo$X1)
foo2$X1 <- as.character(foo2$X1)
table(foo2$X1)
table(foo$X1)
str(foo2)
library(dplyr)
foo_filter <- foo %>%
                filter(X1 == "01/01/2017")


foo_filter1 <- foo2 %>%
  filter(X1 == "01/01/2017")
df2013 <- read.csv("/Users/nadinekhattak/Downloads/311_Service_Requests_from_2013.csv")
df2014 <- read.csv("/Users/nadinekhattak/Downloads/311_Service_Requests_from_2014 2.csv")
df2015 <- read.csv("/Users/nadinekhattak/Downloads/311_Service_Requests_from_2015.csv")
df2016 <- test2
df2017ytd <- test1

table(df2016$Complaint.Type, df2016$Borough)

df_comb <- rbind(df2014,df2015, df2016, df2017ytd)
df_comb1 <-rbind(df_comb, df2013)

names(df_comb)

library(stringr)
library(reshape)

df_comb2$Created.Date <- str_split_fixed(df_comb1$Created.Date, "\\/", 2)

df_comb1$month <- sapply(strsplit(as.character(df_comb1$Created.Date),'/'), "[", 1)
df_comb1$day <- sapply(strsplit(as.character(df_comb1$Created.Date),'/'), "[", 2)
df_comb1$year <- sapply(strsplit(as.character(df_comb1$Created.Date),'/'), "[", 3)

df_comb1$year <- gsub(" ", "_", df_comb1$year)

df_comb1$year <- sapply(strsplit(as.character(df_comb1$year),'_'), "[", 1)
df_comb1$time <- sapply(strsplit(as.character(df_comb1$Created.Date),' '), "[", 1)
df_comb1$am_pm <- sapply(strsplit(as.character(df_comb1$Created.Date),' '), "[", 2)
df_comb1$am_pm1 <- sapply(strsplit(as.character(df_comb1$Created.Date),' '), "[", 3)


str(df_comb1)

names(df_comb1)

head(df_comb1, n=20)

df_comb2 <- df_comb1[c(1:9,17,19,25,54:57,59:60)]

str(df_comb2)

df_comb2[is.na(df_comb2)] <- 0

year.comp_type <- as.data.frame(table(df_comb2$year, df_comb2$Complaint.Type))

head(year.comp_type, n=50)
library(tidyr)
year.comp_type.long <- spread(year.comp_type, Var1, Freq)


head(year.comp_type.long, n=20)
str(year.comp_type.long)






head(year.comp_type.long1)
str(year.comp_type.long1)


year.comp_type.long1$sum <- rowSums(year.comp_type.long1[2:6])

year.comp_type.long1$perc.change <- ((year.comp_type.long1$`2016` - year.comp_type.long1$`2013`)/year.comp_type.long1$`2013`) * 100


year.comp_type.long1$perc.change <- gsub("NaN", "0", year.comp_type.long1$perc.change)
year.comp_type.long1$perc.change <- gsub("Inf", "1", year.comp_type.long1$perc.change)

year.comp_type.long1 <- year.comp_type.long1 %>%
  arrange(desc(`perc.change`))

year.comp_type.long1

# s1 <- df2017ytd %>%
#         filter(Complaint.Type == "Homeless Person Assistance")
# head(s1)
# 
# s2 <- df2017ytd %>%
#   filter(Complaint.Type == "Posting Advertisement")
# 
# head(s2)
# 
# s3 <- df2016 %>%
#   filter(Complaint.Type == "Literature Request")
# 
# head(s3)
# 
# s4 <- df2016 %>%
#   filter(Complaint.Type == "PAINT/PLASTER")
# 
# head(s4)
# 
# s5 <- df2016 %>%
#   filter(Complaint.Type == "DPR Internal")
# 
# head(s5)
# 

#5 largest % increase, 2013-2016
#10 largest 2016
#10 largest 2017 ytd

# borough

##top 5 

year.comp_type.long_top5_percChange <-  year.comp_type.long1[1:5,]

year.comp_type.long_top5_percChange

year.comp_type.long_top5_percChange.long <- melt(year.comp_type.long_top5_percChange)

year.comp_type.long_top5_percChange.long

library(ggplot2)



perc_change <- ggplot(data=year.comp_type.long_top5_percChange.long, aes(x=variable, y=value, group= Var2, color= Var2)) +
  geom_line() +
  geom_point()

year.comp_type.long1_arrange_total <- year.comp_type.long1 %>%
                                        arrange(desc(`2017`)) %>%
                                        slice(1:10) %>%
                                        select(-sum)


year.comp_type.long1_arrange_total


year.comp_type.long1_arrange_total.long <- melt(year.comp_type.long1_arrange_total)

year.comp_type.long1_arrange_total.long

top2017 <- ggplot(data=year.comp_type.long1_arrange_total.long, aes(x=variable, y=value, group= Var2, color= Var2)) +
  geom_line() +
  geom_point()

top2017

year.comp_type.long1_arrange_total_2016 <- year.comp_type.long1 %>%
            arrange(desc(`2016`)) %>%
            slice(1:10) %>%
            select(-sum)


year.comp_type.long1_arrange_total_2016


year.comp_type.long1_arrange_total_2016.long <- melt(year.comp_type.long1_arrange_total_2016)

year.comp_type.long1_arrange_total_2016.long

top2016 <- ggplot(data=year.comp_type.long1_arrange_total_2016.long, aes(x=variable, y=value, group= Var2, color= Var2)) +
  geom_line() +
  geom_point()

top2016

str(year.comp_type.long1_arrange_total_2016.long)

table(year.comp_type.long1_arrange_total_2016.long$Var2)

b1 <- as.data.frame(table(df_comb2$Borough, df_comb2$Complaint.Type))
b1

b2 <- b1 %>%
        group_by(Var1, Var2) %>%
        summarise(Frequency = sum(Freq))

b3 <- b2 %>%
        arrange(Var1, desc(Frequency))
b3_bronx <- b3 %>%
              filter(Var1=="BRONX") %>%
              arrange(desc(Frequency)) %>%
              slice(1:5) %>%
              inner_join(b2, by=c("Var2"))

b3_bronx

table(b1$Var1)

b3_brooklyn <- b3 %>%
  filter(Var1=="BROOKLYN") %>%
  arrange(desc(Frequency)) %>%
  slice(1:5) %>%
  inner_join(b2, by=c("Var2"))



b3_brooklyn


b3_manh <- b3 %>%
  filter(Var1=="MANHATTAN") %>%
  arrange(desc(Frequency)) %>%
  slice(1:5) %>%
  inner_join(b2, by=c("Var2"))


b3_manh

b3_queens <- b3 %>%
  filter(Var1=="QUEENS") %>%
  arrange(desc(Frequency)) %>%
  slice(1:5) %>%
  inner_join(b2, by=c("Var2"))


b3_queens

b3_si <- b3 %>%
  filter(Var1=="STATEN ISLAND") %>%
  arrange(desc(Frequency)) %>%
  slice(1:5) %>%
  inner_join(b2, by=c("Var2"))


b3_si

borough_top5 <- rbind(b3_bronx, b3_brooklyn, b3_manh, b3_queens, b3_si)

head(b3_bronx)

bronx_comp <- ggplot(data=b3_bronx, aes(x=Var2, y=Frequency.y, fill=Var1.y)) + 
  geom_bar(stat="identity", position=position_dodge()) 

borough_top5_1 <- borough_top5 %>%
                    inner_join(b2, by=c("Var2"))

borough_top5_1

brooklyn_comp <- ggplot(data=b3_brooklyn, aes(x=Var2, y=Frequency.y, fill=Var1.y)) + 
  geom_bar(stat="identity", position=position_dodge()) 

brooklyn_comp

manh_comp <- ggplot(data=b3_manh, aes(x=Var2, y=Frequency.y, fill=Var1.y)) + 
  geom_bar(stat="identity", position=position_dodge()) 

manh_comp


bor_comp <- ggplot(data=borough_top5_1, aes(x=Var2, y=Frequency.y, fill=Var1.y)) + 
  geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid(Var1.y ~. )

bor_comp

# am_pm <- df_comb2 %>%
#           group_by(am_pm1) %>%
#           summarise(frequency = sum())


am_pm <- as.data.frame(table(df_comb2$am_pm1, df_comb2$year))

str(am_pm)

am_pm1 <- am_pm %>%
          group_by(Var1, Var2) %>%
          summarise(frequency = sum(Freq))
head(am_pm1)

am_pm1

am_pm2 <- ggplot(data=am_pm1, aes(x=Var2, y=frequency, fill=Var1)) + 
  geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

am_pm2


###

manh_time <- df_comb2 %>%
  filter(Borough == "MANHATTAN")

str(manh_time)

mt_1 <- as.data.frame(table(manh_time$year, manh_time$Complaint.Type))
head(mt_1)

mt_2 <- mt_1 %>%
  filter(Var1 != '2017') %>%
  group_by(Var1, Var2) %>%
  summarise(freq = sum(Freq)) %>%
  spread(Var1, freq) %>%
  mutate(perc_ch = ((`2016` - `2013`)/`2013`) * 100)

  
  
  

mt_2$perc_ch <- gsub("Inf", "1", mt_2$perc_ch)
mt_2$perc_ch <- gsub("NaN", "0", mt_2$perc_ch)

mt_3 <- mt_2 %>%
          arrange(desc(perc_ch)) %>%
          slice(1:10) %>%
          data.frame
  
str(mt_2)

table(mt_3$perc_ch)

(29-18)/18

mt_2

mt4 <- melt(mt_3)

mt3

mt4$variable <- gsub("X", "", mt4$variable)

manh_top2016 <- ggplot(data=mt4, aes(x=variable, y=value, group= Var2, color= Var2)) +
  geom_line() +
  geom_point()

manh_top2016
mt_2

mt_5 <- mt_2 %>%
  arrange(desc(`2016`)) %>%
  slice(1:10) %>%
  data.frame


mt_6 <- melt(mt_5)

mt_6$variable <- gsub("X", "", mt_6$variable)

manh_top2016_count <- ggplot(data=mt_6, aes(x=variable, y=value, group= Var2, color= Var2)) +
  geom_line() +
  geom_point()

manh_top2016_count

