install.packages("data.table")
install.packages("tidyverse")
install.packages("ggthemes")
install.packages("RColorBrewer")
library(data.table)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
setwd("~/Desktop/Cato_OECD_Trade_Data_AH/Cleaned Data")

us_china <- fread("ICIO2018_2015_USChina_Dependence.csv", header = TRUE)
us_china[[2]] <- NULL
us_china <- us_china[, lapply(.SD, sum), by = c("COUNTRY")]
us_china <- us_china %>% arrange(-CHN_26)
us_china_CHN_26 <- head(us_china, 11)

#2015 China Input Output Graphs  
ggplot(data=us_china_CHN_26, aes(x=reorder(COUNTRY, -CHN_26), y=(CHN_26/990057.916276037)*100, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round((CHN_26/990057.916276037)*100, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("% Total Inputs") + 
  ggtitle("Top 10 Foreign Suppliers for China's Computer, Electronic, and Optical Inputs (2015)") +
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Switzerland", "China", "Germany", "Japan", "Korea", "Malaysia", "Philippines", "Singapore", "Thailand", "Taiwan", "United States"))

us_china <- us_china %>% filter(us_china$COUNTRY != "ROW") %>% arrange(-CHN_27) 
us_china_CHN_27 <- head(us_china, 11)

ggplot(data=us_china_CHN_27, aes(x=reorder(COUNTRY, -CHN_27), y=(CHN_27/636707.7316)*100, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round((CHN_27/636707.7316)*100, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("% Total Inputs") + 
  ggtitle("Top 10 Foreign Suppliers for China's Electrical Equipment Inputs (2015)") +
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Australia", "Canada", "Chile", "China", "Germany", "Japan", "Korea", "Malaysia", "Thailand", "Taiwan", "United States"))

#2015 US Input Output Graphs  
us_data <- fread("ICIO2018_2015_USA_Dependence.csv", header = TRUE)
us_data[[2]] <- NULL
us_data <- us_data[, lapply(.SD, sum), by = c("COUNTRY")]
us_data <- us_data %>% filter(us_data$COUNTRY != "ROW") 
us_china_USA_26 <- us_data %>% arrange(-USA_26) %>% head(.,11)

ggplot(data=us_china_USA_26, aes(x=reorder(COUNTRY, -USA_26), y=(USA_26/101618.525)*100, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round((USA_26/101618.525)*100, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("% Total Inputs") +
  ggtitle("Top 10 Foreign Suppliers for US Computer, Electronic, and Optical Inputs (2015)") + 
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Canada", "Switzerland", "China", "Germany", "Japan", "Korea", "Mexico", "Malaysia", "Thailand", "Taiwan", "USA"))

 us_china_USA_27 <- us_data %>% arrange(-USA_27) %>% head(.,11)

ggplot(data=us_china_USA_27, aes(x=reorder(COUNTRY, -USA_27), y=(USA_27/68259.08211)*100, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round((USA_27/68259.08211)*100, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("% Total Inputs") +
  ggtitle("Top 10 Foreign Suppliers for US Electrical Equipment Inputs (2015)") + 
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Canada", "China", "Germany", "France", "United Kingdom", "Italy", "Japan", "Korea", "Mexico", "Taiwan", "USA"))

#2005 Data Cleaning
io_2005 <- fread("ICIO2018_2005.csv", header = TRUE)
to_del = c("01T03", "05T06", "07T08", "9", "10T12", "13T15", "16", "17T18", "19", "20T21", "22", "23", "24", "25", "28", "29", "30", "31T33", "35T39", "41T43", "45T47", "49T53", "55T56", "58T60", "61", "62T63", "64T66", "68", "69T82", "84", "85", "86T88", "90T96", "97T98", "TAXSUB")
cols_to_remove <- grep(pattern="01T03|05T06|07T08|9|10T12|13T15|16|17T18|19|20T21|22|23|24|25|28|29|30|31T33|35T39|41T43|45T47|49T53|55T56|58T60|61|62T63|64T66|68|69T82|84|85|86T88|90T96|97T98|TAXSUB",
                       x=colnames(io_2005))
io_2005[, (cols_to_remove):=NULL]
write.csv(io_2005, file = "io_2005_clean.csv")

#Re-upload
io_2005_clean_2 <- fread("io_2005_clean_revised.csv", header = TRUE)
io_2005_clean_2[[2]] <- NULL
io_2005_cleaner <- io_2005_clean_2[, lapply(.SD, sum), by = c("COUNTRY")]

#2005 China Input Output Graphs  
io_2005_temp <- io_2005_cleaner %>% arrange(-CHN_26)
io_2005_CHN_26 <- head(io_2005_temp, 11)

ggplot(data=io_2005_CHN_26, aes(x=reorder(COUNTRY, -CHN_26), y=(CHN_26/263363.0574)*100, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round((CHN_26/263363.0574)*100, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("% Total Inputs") + 
  ggtitle("Top 10 Foreign Suppliers for China's Computer, Electronic, and Optical Inputs (2005)") + 
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("China", "Germany", "France", "Japan", "Korea", "Malaysia", "Philippines", "Singapore", "Thailand", "Taiwan", "USA"))

io_2005_clean_3 <- fread("io_2005_clean_revised.csv", header = TRUE)
io_2005_clean_3[[2]] <- NULL
io_2005_cleaner <- io_2005_clean_3[, lapply(.SD, sum), by = c("COUNTRY")]
io_2005_temp <- io_2005_cleaner %>% filter(COUNTRY != "ROW") %>% arrange(-CHN_27)
io_2005_CHN_27 <- head(io_2005_temp, 20)

ggplot(data=io_2005_CHN_27, aes(x=reorder(COUNTRY, -CHN_27), y=(CHN_27/128619.4645)*100, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round((CHN_27/128619.4645)*100, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("% Total Inputs") + 
  ggtitle("Top 10 Foreign Suppliers for China's Electical Equipment Inputs (2005)") + 
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Australia", "Chile", "China", "Germany", "Japan", "Korea", "Malaysia", "Singapore", "Thailand", "Taiwan", "USA"))

#2005 US Input Output Graphs  
io_2005_clean_4 <- fread("io_2005_clean_revised.csv", header = TRUE)
io_2005_clean_4[[2]] <- NULL
io_2005_cleaner <- io_2005_clean_4[, lapply(.SD, sum), by = c("COUNTRY")]
io_2005_temp <- io_2005_cleaner %>% filter(COUNTRY != "ROW") %>% arrange(-USA_26)
io_2005_USA_26 <- head(io_2005_temp, 11)

ggplot(data=io_2005_USA_26, aes(x=reorder(COUNTRY, -USA_26), y=(USA_26/186038.4634)*100, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round((USA_26/186038.4634)*100, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("% Total Inputs") + 
  ggtitle("Top 10 Foreign Suppliers for US Computer, Electronic, and Optical Inputs (2005)") + 
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Canada", "China", "Germany", "Japan", "Korea", "Mexico", "Malaysia", "Thailand", "Taiwan", "USA"))

io_2005_temp <- io_2005_cleaner %>% filter(COUNTRY != "ROW") %>% arrange(-USA_27)
io_2005_USA_27 <- head(io_2005_temp, 11)

ggplot(data=io_2005_USA_27, aes(x=reorder(COUNTRY, -USA_27), y=(USA_27/65271.85301)*100, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round((USA_27/65271.85301)*100, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("% Total Inputs") + 
  ggtitle("Top 10 Foreign Suppliers for US Electrical Equipment Inputs (2005)") +
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Canada", "China", "Germany", "Great Britain", "Italy", "Japan", "Korea", "Mexico", "Russia", "Taiwan", "USA"))

