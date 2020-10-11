install.packages("data.table")
install.packages("tidyverse")
install.packages("ggthemes")
install.packages("RColorBrewer")
library(data.table)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
setwd("~/Desktop/Cato_OECD_Trade_Data_AH/Cleaned Data")

#China Input Output Graphs
us_china <- fread("ICIO2018_2015_USChina_Dependence.csv", header = TRUE)
us_china[[2]] <- NULL
us_china <- us_china[, lapply(.SD, sum), by = c("COUNTRY")]
write.csv(us_china, file = "clean_data.csv")
us_china <- us_china %>% filter(us_china$COUNTRY != "CN1" & us_china$COUNTRY != "CN2") %>% arrange(-CN1_26) 
us_china_CN1_26 <- head(us_china, 10)

ggplot(data=us_china_CN1_26, aes(x=reorder(COUNTRY, -CN1_26), y=CN1_26/1000, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round(CN1_26/1000, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("Input Value (billion USD)") + 
  ggtitle("Top 10 Foreign Suppliers for China's Computer, Electronic, and Optical Inputs", subtitle = "Activities excluding export processing") + 
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Switzerland", "Germany", "Japan", "Korea", "Malaysia", "Philippines", "Singapore", "Thailand", "Taiwan", "United States"))


us_china <- us_china %>% arrange(-CN2_26) 
us_china_CN2_26 <- head(us_china, 10)

ggplot(data=us_china_CN2_26, aes(x=reorder(COUNTRY, -CN2_26), y=CN2_26/1000, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round(CN2_26/1000, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("Input Value (billion USD)") + 
  ggtitle("Top 10 Foreign Suppliers for China's Computer, Electronic, and Optical Inputs", subtitle = "Export processing activities") + 
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Switzerland", "Germany", "Japan", "Korea", "Malaysia", "Philippines", "Singapore", "Thailand", "Taiwan", "United States"))

us_china <- us_china %>% arrange(-CN1_27)  %>% filter(us_china$COUNTRY != "ROW")
us_china_CN1_27 <- head(us_china, 10)

ggplot(data=us_china_CN1_27, aes(x=reorder(COUNTRY, -CN1_27), y=CN1_27/1000, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round(CN1_27/1000, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("Input Value (billion USD)") +
  ggtitle("Top 10 Foreign Suppliers for China's Electrical Equipment Inputs", subtitle = "Activities excluding export processing") +
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Australia", "Canada", "Chile", "Germany", "Japan", "Korea", "Malaysia", "Thailand", "Taiwan", "United States"))

us_china <- us_china %>% arrange(-CN2_27) 
us_china_CN2_27 <- head(us_china, 10)

ggplot(data=us_china_CN2_27, aes(x=reorder(COUNTRY, -CN2_27), y=CN2_27/1000, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round(CN2_27/1000, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("Input Value (billion USD)") +
  ggtitle("Top 10 Foreign Suppliers for China's Electrical Equipment Inputs", subtitle = "Export processing activities") +
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Australia", "Canada", "Chile", "Germany", "Japan", "Korea", "Malaysia", "Thailand", "Taiwan", "United States"))


us_china <- fread("ICIO2018_2015_USChina_Dependence.csv", header = TRUE)
us_china[[2]] <- NULL
us_china <- us_china[, lapply(.SD, sum), by = c("COUNTRY")]
us_china <- us_china %>% filter(us_china$COUNTRY != "CN1" & us_china$COUNTRY != "CN2") %>% arrange(-CHN_26)
us_china_CHN_26 <- head(us_china, 10)

ggplot(data=us_china_CHN_26, aes(x=reorder(COUNTRY, -CHN_26), y=CHN_26/1000, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round(CHN_26/1000, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("Input Value (billion USD)") + 
  ggtitle("Top 10 Foreign Suppliers for China's Computer, Electronic, and Optical Inputs", subtitle = "All intermediates, value added, and output") +
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Switzerland", "Germany", "Japan", "Korea", "Malaysia", "Philippines", "Singapore", "Thailand", "Taiwan", "United States"))

us_china <- us_china %>% filter(us_china$COUNTRY != "ROW") %>% arrange(-CHN_27) 
us_china_CHN_27 <- head(us_china, 10)

ggplot(data=us_china_CHN_27, aes(x=reorder(COUNTRY, -CHN_27), y=CHN_27/1000, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round(CHN_27/1000, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("Input Value (billion USD)") + 
  ggtitle("Top 10 Foreign Suppliers for China's Electrical Equipment Inputs", subtitle = "All intermediates, value added, and output") +
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Australia", "Canada", "Chile", "Germany", "Japan", "Korea", "Malaysia", "Thailand", "Taiwan", "United States"))


#US Input Output Graphs  
us_data <- fread("ICIO2018_2015_USA_Dependence.csv", header = TRUE)
us_data[[2]] <- NULL
us_data <- us_data[, lapply(.SD, sum), by = c("COUNTRY")]
us_data <- us_data %>% filter(us_data$COUNTRY != "ROW" & us_data$COUNTRY != "USA") 
us_china_USA_26 <- us_data %>% arrange(-USA_26) %>% head(.,10)

ggplot(data=us_china_USA_26, aes(x=reorder(COUNTRY, -USA_26), y=USA_26/1000, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round(USA_26/1000, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("Input Value (billion USD)") +
  ggtitle("Top 10 Foreign Suppliers for US Computer, Electronic, and Optical Inputs", subtitle = "All intermediates, value added, and output") + 
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Canada", "Switzerland", "China", "Germany", "Japan", "Korea", "Mexico", "Malaysia", "Thailand", "Taiwan"))

us_china_USA_27 <- us_data %>% arrange(-USA_27) %>% head(.,10)

ggplot(data=us_china_USA_27, aes(x=reorder(COUNTRY, -USA_27), y=USA_27/1000, fill = COUNTRY)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round(USA_27/1000, digits = 1)), vjust=-0.25) +
  xlab("Country") + 
  ylab("Input Value (billion USD)") +
  ggtitle("Top 10 Foreign Suppliers for US Electrical Equipment Inputs", subtitle = "All intermediates, value added, and output") + 
  scale_fill_brewer(palette="RdBu", name = "Country", labels = c("Canada", "China", "Germany", "France", "United Kingdom", "Italy", "Japan", "Korea", "Mexico", "Taiwan"))


#################

country_data <- fread("ICIO2018_2015_ByCountry.csv", header = TRUE)
sorted_countries_26 <- country_data[order(-TOTAL_26)]
sorted_countries_26[[2]] <- NULL
merged_26 <- sorted_countries_26[, lapply(.SD, sum), by = c("COUNTRY")]
ordered_26 <- merged_26[order(-TOTAL_26)]
spliced_26 <- ordered_26[1:10]

#Most relied on countries for computer + electronic inputs
spliced_26 %>% ggplot(aes(x=reorder(COUNTRY, -TOTAL_26),y=TOTAL_26/1000, fill = COUNTRY)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label = round(TOTAL_26/1000, digits = 0)), vjust=-0.25) +
  xlab("Country") + 
  ylab("Input Value (billion USD)") + 
  ggtitle("Most Relied on Countries for Computer, Electronic, and Optical Product Inputs") + 
  scale_fill_brewer(palette="RdBu")

sorted_countries_27 <- country_data[order(-TOTAL_26)]
sorted_countries_27[[2]] <- NULL
merged_27 <- sorted_countries_27[, lapply(.SD, sum), by = c("COUNTRY")]
ordered_27 <- merged_27[order(-TOTAL_27)]
spliced_27 <- ordered_27[1:10]

#Most relied on countries for electrical equipment inputs
spliced_27 %>% ggplot(aes(x=reorder(COUNTRY, -TOTAL_27),y=TOTAL_27/1000, fill = COUNTRY)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label = round(TOTAL_27/1000, digits = 0)), vjust=-0.25) +
  xlab("Country") + 
  ylab("Input Value (billion USD)") + 
  ggtitle("Most Relied on Countries for Electrical Equipment Inputs") + 
  scale_fill_brewer(palette="RdBu")

countries <- c("AUS","AUT","BEL","CAN","CHL","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA","ARG","BRA","BRN","BGR","KHM","CHN","COL","CRI","HRV","CYP","IND","IDN","HKG","KAZ","MYS","MLT","MAR","PER","PHL","ROU","RUS","SAU","SGP","ZAF","TWN","THA","TUN","VNM","ROW")

#computers, electronics, and optical products country codes
comp_electronics <- c("AUS_26","AUT_26","BEL_26","CAN_26","CHL_26","CZE_26","DNK_26","EST_26","FIN_26","FRA_26","DEU_26","GRC_26","HUN_26","ISL_26","IRL_26","ISR_26","ITA_26","JPN_26","KOR_26","LVA_26","LTU_26","LUX_26","MEX_26","NLD_26","NZL_26","NOR_26","POL_26","PRT_26","SVK_26","SVN_26","ESP_26","SWE_26","CHE_26","TUR_26","GBR_26","USA_26","ARG_26","BRA_26","BRN_26","BGR_26","KHM_26","CHN_26","COL_26","CRI_26","HRV_26","CYP_26","IND_26","IDN_26","HKG_26","KAZ_26","MYS_26","MLT_26","MAR_26","PER_26","PHL_26","ROU_26","RUS_26","SAU_26","SGP_26","ZAF_26","TWN_26","THA_26","TUN_26","VNM_26","ROW_26")

#electric equipment country codes
electric <- c("AUS_27","AUT_27","BEL_27","CAN_27","CHL_27","CZE_27","DNK_27","EST_27","FIN_27","FRA_27","DEU_27","GRC_27","HUN_27","ISL_27","IRL_27","ISR_27","ITA_27","JPN_27","KOR_27","LVA_27","LTU_27","LUX_27","MEX_27","NLD_27","NZL_27","NOR_27","POL_27","PRT_27","SVK_27","SVN_27","ESP_27","SWE_27","CHE_27","TUR_27","GBR_27","USA_27","ARG_27","BRA_27","BRN_27","BGR_27","KHM_27","CHN_27","COL_27","CRI_27","HRV_27","CYP_27","IND_27","IDN_27","HKG_27","KAZ_27","MYS_27","MLT_27","MAR_27","PER_27","PHL_27","ROU_27","RUS_27","SAU_27","SGP_27","ZAF_27","TWN_27","THA_27","TUN_27","VNM_27","ROW_27")




sapply(data, function(x) any(grepl(x, v2)))

data2 <- data.frame()

for (i in 1:length(countries)) {
  i_temp <- data %>% filter(grepl(countries[i], data$INPUTS))
  write.csv(i_temp, file = "i_temp.csv")
}

