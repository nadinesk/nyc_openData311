dev.copy(png, file = "members.png", width=800)  

library(RCurl) 
library(dplyr)
library(stringr)
library(reshape)
library(tidyr)
library(ggplot2)


setwd("/Users/nadinekhattak/Desktop/dev/nyc_openData311")

## Read Files
df2017ytd <- read.csv("/Users/nadinekhattak/Downloads/311_Service_Requests_from_2010_to_Present (2).csv")
df2016 <- read.csv("/Users/nadinekhattak/Downloads/311_Service_Requests_from_2010_to_Present.csv")
df2013 <- read.csv("/Users/nadinekhattak/Downloads/311_Service_Requests_from_2013.csv")
df2014 <- read.csv("/Users/nadinekhattak/Downloads/311_Service_Requests_from_2014 2.csv")
df2015 <- read.csv("/Users/nadinekhattak/Downloads/311_Service_Requests_from_2015.csv")

# df2016 <- test2
# df2017ytd <- test1

## Combine Files
df_comb <- rbind(df2014,df2015, df2016, df2017ytd)
df_comb1 <-rbind(df_comb, df2013)

## Split date column to month, day, year, time, and AM/PM
df_comb1$month <- sapply(strsplit(as.character(df_comb1$Created.Date),'/'), "[", 1)
df_comb1$day <- sapply(strsplit(as.character(df_comb1$Created.Date),'/'), "[", 2)
df_comb1$year <- sapply(strsplit(as.character(df_comb1$Created.Date),'/'), "[", 3)

df_comb1$time <- sapply(strsplit(as.character(df_comb1$Created.Date),' '), "[", 1)
df_comb1$am_pm <- sapply(strsplit(as.character(df_comb1$Created.Date),' '), "[", 2)
df_comb1$am_pm1 <- sapply(strsplit(as.character(df_comb1$Created.Date),' '), "[", 3)

## Get only columns needed

names(df_comb1)

df_comb2 <- df_comb1[c(1:9,17,19,25,54:57,59:60)]

str(df_comb2)

## Replace NA's to 0's
df_comb2[is.na(df_comb2)] <- 0

## Look at the number of each type of complaint per year
year.comp_type <- as.data.frame(table(df_comb2$year, df_comb2$Complaint.Type))

head(year.comp_type, n=50)

## Reshape from long to wide format
year.comp_type.long <- spread(year.comp_type, Var1, Freq)

## Get row sums (total for each complaint)

year.comp_type.long1$sum <- rowSums(year.comp_type.long1[2:6])

## Get percentage change of each complaint type, from 2013 to 2016
year.comp_type.long1$perc.change <- ((year.comp_type.long1$`2016` - year.comp_type.long1$`2013`)/year.comp_type.long1$`2013`) * 100

## Replace percentage changes of NaN with 0
year.comp_type.long1$perc.change <- gsub("NaN", "0", year.comp_type.long1$perc.change)
## Replace percentage changes of Inf with 1
year.comp_type.long1$perc.change <- gsub("Inf", "1", year.comp_type.long1$perc.change)

## Sort data descending by percentage change, and keep only the top five rows
year.comp_type.long_top5_percChange <- year.comp_type.long1 %>%
  arrange(desc(`perc.change`))%>%
  slice(1:5) %>%
  select(-`2017`, -sum)

## Reshape data from wide to long
year.comp_type.long_top5_percChange.long <- melt(year.comp_type.long_top5_percChange)

## Create bar chart showing change in complaint types over year, for complaint types with  five highest percentage changes 
ggplot(data=year.comp_type.long_top5_percChange.long, aes(x=variable, y=value, group= Var2, color= Var2)) +
  geom_line() +
  geom_point()

year.comp_type.long_top5_percChange.long

## Get the ten most frequent complaint types for 2017 YTD
year.comp_type.long1_arrange_total <- year.comp_type.long1 %>%
                                        arrange(desc(`2017`)) %>%
                                        slice(1:10) %>%
                                        select(-sum)


year.comp_type.long1_arrange_total


year.comp_type.long1_arrange_total.long <- melt(year.comp_type.long1_arrange_total)

year.comp_type.long1_arrange_total.long


## Create bar chart showing change in complaint types over year, for most frequent complaint types in 2017 YTD 
ggplot(data=year.comp_type.long1_arrange_total.long, aes(x=variable, y=value, group= Var2, color= Var2)) +
  geom_line() +
  geom_point()

year.comp_type.long1_arrange_total.long

## Get the ten most frequent complaint types for 2017 YTD
year.comp_type.long1_arrange_total_2016 <- year.comp_type.long1 %>%
            arrange(desc(`2016`)) %>%
            slice(1:10) %>%
            select(-`2017`,-sum)


year.comp_type.long1_arrange_total_2016


year.comp_type.long1_arrange_total_2016.long <- melt(year.comp_type.long1_arrange_total_2016)

year.comp_type.long1_arrange_total_2016.long

## Create bar chart showing change in complaint types over year, for most frequent complaint types in 2016
ggplot(data=year.comp_type.long1_arrange_total_2016.long, aes(x=variable, y=value, group= Var2, color= Var2)) +
  geom_line() +
  geom_point()

year.comp_type.long1_arrange_total_2016.long


# Get frequency of each complaint type by borough

b1 <- as.data.frame(table(df_comb2$Borough, df_comb2$Complaint.Type))
b1

## Group and Sum
b2 <- b1 %>%
        group_by(Var1, Var2) %>%
        summarise(Frequency = sum(Freq))

b3 <- b2 %>%
        arrange(Var1, desc(Frequency))

## Get the Bronx's top five complaints, and add back in the number of those complaints for the other boroughs (the inner join)
b3_bronx <- b3 %>%
              filter(Var1=="BRONX") %>%
              arrange(desc(Frequency)) %>%
              slice(1:5) %>%
              inner_join(b2, by=c("Var2"))

b3_bronx

## Get Brooklyn's top five complaints, and add back in the number of those complaints for the other boroughs (the inner join)
b3_brooklyn <- b3 %>%
  filter(Var1=="BROOKLYN") %>%
  arrange(desc(Frequency)) %>%
  slice(1:5) %>%
  inner_join(b2, by=c("Var2"))

## Plot Brooklyn's top 5 complaints
ggplot(data=b3_brooklyn, aes(x=Var2, y=Frequency.y, fill=Var1.y)) + 
  geom_bar(stat="identity", position=position_dodge()) 

b3_brooklyn



## Get Manhattan's top five complaints, and add back in the number of those complaints for the other boroughs (the inner join)
b3_manh <- b3 %>%
  filter(Var1=="MANHATTAN") %>%
  arrange(desc(Frequency)) %>%
  slice(1:5) %>%
  inner_join(b2, by=c("Var2"))

## Plot Manhattan's top 5 complaints
ggplot(data=b3_manh, aes(x=Var2, y=Frequency.y, fill=Var1.y)) + 
  geom_bar(stat="identity", position=position_dodge()) 

b3_manh

## Get Queen's top five complaints, and add back in the number of those complaints for the other boroughs (the inner join)
b3_queens <- b3 %>%
  filter(Var1=="QUEENS") %>%
  arrange(desc(Frequency)) %>%
  slice(1:5) %>%
  inner_join(b2, by=c("Var2"))

## Plot Queen's top 5 complaints
ggplot(data=b3_queens, aes(x=Var2, y=Frequency.y, fill=Var1.y)) + 
  geom_bar(stat="identity", position=position_dodge()) 

b3_queens

## Get Staten Island's top five complaints, and add back in the number of those complaints for the other boroughs (the inner join)
b3_si <- b3 %>%
  filter(Var1=="STATEN ISLAND") %>%
  arrange(desc(Frequency)) %>%
  slice(1:5) %>%
  inner_join(b2, by=c("Var2"))

## Plot Staten Island's top 5 complaints
ggplot(data=b3_si, aes(x=Var2, y=Frequency.y, fill=Var1.y)) + 
  geom_bar(stat="identity", position=position_dodge()) 

b3_si


## Combine all top 5's together
borough_top5 <- rbind(b3_bronx, b3_brooklyn, b3_manh, b3_queens, b3_si)

# borough_top5_1 <- borough_top5 %>%
#   inner_join(b2, by=c("Var2"))

## Plot all boroughs' top 5 complaints
ggplot(data=borough_top5_1, aes(x=Var2, y=Frequency.y, fill=Var1.y)) + 
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

