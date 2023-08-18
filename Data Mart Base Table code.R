# Installing packages
install.packages("haven")
install.packages("anytime")
install.packages("readxl")
install.packages("tidyverse")
install.packages("shiny")
install.packages("lubridate")

# Loading packages to R
library(haven)
require(haven)
library(anytime)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(data.table)

#######################################################################################
#                                       											                        #
#           				DATA PREPARATION FOR DEMOGRAPHICS DATASET       		              #									  
#                                        											                        #
#######################################################################################

# Importing the data file "RawDataIDemographics.sas7bdat" to a dataframe 'demographics'
demographics <- read_sas("C:/Users/ythonukunuru/Desktop/R/RawDataIDemographics.sas7bdat")

# Creating a data frame
demographics_df <- data.frame(demographics)

# Inspecting above created data frame
sapply(demographics_df, class)
head(demographics_df)
tail(demographics_df,n=12)
str(demographics_df)

# Defining a function to insert '-' in date related columns
fun_insert <- function(x, pos, insert) {       
                        gsub(paste0("^(.{", pos, "})(.*)$"),
                             paste0("\\1", insert, "\\2"),
                             x)
                      }

# Using above created fucntion to insert '-' in variables FirstPay, FirstAct, FirstAct, FirstSp,FirstCa,FirstGa,FirstPo
demographics_df$FirstPay <- fun_insert(x = demographics_df$FirstPay, pos =4, insert = "-")
demographics_df$FirstPay <- fun_insert(x = demographics_df$FirstPay, pos =7, insert = "-")
demographics_df$FirstAct <- fun_insert(x = demographics_df$FirstAct, pos =4, insert = "-")
demographics_df$FirstAct <- fun_insert(x = demographics_df$FirstAct, pos =7, insert = "-")
demographics_df$FirstSp <- fun_insert(x = demographics_df$FirstSp, pos =4, insert = "-")
demographics_df$FirstSp <- fun_insert(x = demographics_df$FirstSp, pos =7, insert = "-")
demographics_df$FirstCa <- fun_insert(x = demographics_df$FirstCa, pos =4, insert = "-")
demographics_df$FirstCa <- fun_insert(x = demographics_df$FirstCa, pos =7, insert = "-")
demographics_df$FirstGa <- fun_insert(x = demographics_df$FirstGa, pos =4, insert = "-")
demographics_df$FirstGa <- fun_insert(x = demographics_df$FirstGa, pos =7, insert = "-")
demographics_df$FirstPo <- fun_insert(x = demographics_df$FirstPo, pos =4, insert = "-")
demographics_df$FirstPo <- fun_insert(x = demographics_df$FirstPo, pos =7, insert = "-")

# ABove step had added '-' to NULL values, so treating this issue by using below step
demographics_df$FirstPay <- gsub('NULL-', 'NULL', demographics_df$FirstPay)
demographics_df$FirstAct <- gsub('NULL-', 'NULL', demographics_df$FirstAct)
demographics_df$FirstSp <- gsub('NULL-', 'NULL', demographics_df$FirstSp)
demographics_df$FirstCa <- gsub('NULL-', 'NULL', demographics_df$FirstCa)
demographics_df$FirstGa <- gsub('NULL-', 'NULL', demographics_df$FirstGa)
demographics_df$FirstPo <- gsub('NULL-', 'NULL', demographics_df$FirstPo)


# Changing data type of date related columns to 'Date' data type
demographics_df$RegDate <- as.Date(demographics_df$RegDate)
demographics_df$FirstPay <- as.Date(demographics_df$FirstPay)
demographics_df$FirstAct <- as.Date(demographics_df$FirstAct)
demographics_df$FirstSp <- as.Date(demographics_df$FirstSp)

# Encountered an error 'string is not in a standard unambiguous format', so handling other variables using 'anydate' library
demographics_df$FirstCa <- anydate((demographics_df$FirstCa))
demographics_df$FirstGa <- anydate((demographics_df$FirstGa))
demographics_df$FirstPo <- anydate((demographics_df$FirstPo))

# Verfying above modification 
sapply(demographics_df, class)

# Reading individual sheets in excel worksheet 'Appendices Group Assignment'
excel_sheets("C:/Users/ythonukunuru/Desktop/R/Appendices Group Assignment.xlsx")
countries <- read_excel("C:/Users/ythonukunuru/Desktop/R/Appendices Group Assignment.xlsx", sheet = 2)
languages <- read_excel("C:/Users/ythonukunuru/Desktop/R/Appendices Group Assignment.xlsx", sheet = 3)
applicationsinfo <- read_excel("C:/Users/ythonukunuru/Desktop/R/Appendices Group Assignment.xlsx", sheet = 4)


# Creating data frames for above tibbles
countries_df <- data.frame(countries)
languages_df <- data.frame(languages)
applicationsinfo_df <- data.frame(applicationsinfo)

# Merging 'demographics_df' with data fromaes created for countries,languages,applications info
demographics_merged1_df <- merge(demographics_df, countries_df, by='Country')
demographics_merged2_df <- merge(demographics_merged1_df,languages_df, by='Language')
demographics_merged3_df <- merge(demographics_merged2_df,applicationsinfo_df, by='ApplicationID')

# Dropping columns ApplicationID,Language,Country 
demographics_merged_df <- demographics_merged3_df %>% select(-1,-2,-3)

# Renaming column names 
colnames(demographics_merged_df)[which(names(demographics_merged_df) == 'Country.Name')] <- 'Country'
colnames(demographics_merged_df)[which(names(demographics_merged_df) == 'Language.Description')] <- 'Language'
colnames(demographics_merged_df)[which(names(demographics_merged_df) == 'Application.Description')] <- 'Application'

# Defining males and females in data set by replacing 0 with 'Female' and 1 with 'Male'
demographics_merged_df$Gender <- gsub('0', 'Female', demographics_merged_df$Gender)
demographics_merged_df$Gender <- gsub('1', 'Male', demographics_merged_df$Gender)

# Changing Gender column data type from numeric to character and verifying changes
demographics_merged_df$Gender <- as.character(demographics_merged_df$Gender)
sapply(demographics_merged_df, class)

# Checking duplicates of User IDs
sum(duplicated.data.frame(demographics_merged_df$UserID))

# Checking for missing values and counting them in all columns
sum(is.na(demographics_merged_df$UserID))
sum(is.na(demographics_merged_df$RegDate))
sum(is.na(demographics_merged_df$FirstPay))
sum(is.na(demographics_merged_df$FirstAct))
sum(is.na(demographics_merged_df$FirstSp))
sum(is.na(demographics_merged_df$FirstCa))
sum(is.na(demographics_merged_df$FirstGa))
sum(is.na(demographics_merged_df$FirstPo))
sum(is.na(demographics_merged_df$Gender))
sum(is.na(demographics_merged_df$Country))
sum(is.na(demographics_merged_df$Language))
sum(is.na(demographics_merged_df$Application))

# Checking range of dates to make sure they are within the required time frame 
min(demographics_merged_df$RegDate, na.rm = TRUE)
max(demographics_merged_df$RegDate, na.rm = TRUE)
min(demographics_merged_df$FirstPay, na.rm = TRUE)
max(demographics_merged_df$FirstPay, na.rm = TRUE)
min(demographics_merged_df$FirstAct, na.rm = TRUE)
max(demographics_merged_df$FirstAct, na.rm = TRUE)
min(demographics_merged_df$FirstSp, na.rm = TRUE)
max(demographics_merged_df$FirstSp, na.rm = TRUE)
min(demographics_merged_df$FirstCa, na.rm = TRUE)
max(demographics_merged_df$FirstCa, na.rm = TRUE)
min(demographics_merged_df$FirstGa, na.rm = TRUE)
max(demographics_merged_df$FirstGa, na.rm = TRUE)
min(demographics_merged_df$FirstPo, na.rm = TRUE)
max(demographics_merged_df$FirstPo, na.rm = TRUE)

# 'FirstAct' has two null values, so excluding those two values from data frame
demographics_cleaned_df <- demographics_merged_df %>%
  drop_na(FirstAct)

#######################################################################################
#                                       											                        #
#       DATA CLEANING AND DATA MANUPULTAION FOR User DAILY AGGREGATION DATASET       	#	              #									  
#                                        											                        #
#######################################################################################

# Reading the SAS file 'UserDailyAggregation' into R
daily_agg <- read_sas("C:/Users/ythonukunuru/Desktop/R/RawDataIIUserDailyAggregation.sas7bdat")

# Checking the structure and data types of the dataset
str(daily_agg)
head(daily_agg)
tail(daily_agg)

# Look for missing and inconsistent values
sum(is.na(daily_agg))
unique(nchar(daily_agg$UserID))
unique(daily_agg$ProductID) 

#creating a vector for the date format
date_format <- c("%Y-%m-%d")

#Converting the Date column to date format
daily_agg$Date<- as.Date(parse_date_time(daily_agg$Date, date_format))

#Adding a Month column in the dataframe
daily_agg$Month <- month(ymd(daily_agg$Date))

head(daily_agg)

# Filtering out records before first_pay
demogs <- demographics %>% select(UserID, FirstPay)
demogs$FirstPay <- as.Date(parse_date_time(demogs$FirstPay, date_format))
daily_agg_demogs <- daily_agg %>% left_join(demogs, by = "UserID")
daily_agg_cleaned <- daily_agg_demogs %>% filter(Date > FirstPay)

# Filter out zero values
daily_agg_cleaned <- daily_agg_cleaned %>% filter((Stakes + Winnings) > 0)

# Creating DFs per metric per product
summary_df <- daily_agg_cleaned %>%
                  group_by(UserID) %>%
                  summarize(Total_Stakes = round(sum(Stakes),2), Avg_Stakes = round(mean(Stakes),2), 
                  Total_Winnings = round(sum(Winnings),2), Avg_Winnings = round(mean(Winnings),2), 
                  Total_Bets = sum(Bets), Avg_Bets = round(mean(Bets),1),
                  Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                  Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                  Betting_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))

summary_prod1 <- daily_agg_cleaned %>%
                      filter(ProductID == 1) %>%
                      group_by(UserID) %>%
                      summarize(P1_Total_Stakes = round(sum(Stakes),2), P1_Avg_Stakes = round(mean(Stakes),2), 
                      P1_Total_Winnings = round(sum(Winnings),2), P1_Avg_Winnings = round(mean(Winnings),2), 
                      P1_Total_Bets = sum(Bets), P1_Avg_Bets = round(mean(Bets),1),
                      P1_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                      P1_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                      P1_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))

summary_prod2 <- daily_agg_cleaned %>%
                      filter(ProductID == 2) %>%
                      group_by(UserID) %>%
                      summarize(P2_Total_Stakes = round(sum(Stakes),2), P2_Avg_Stakes = round(mean(Stakes),2), 
                      P2_Total_Winnings = round(sum(Winnings),2), P2_Avg_Winnings = round(mean(Winnings),2), 
                      P2_Total_Bets = sum(Bets), P2_Avg_Bets = round(mean(Bets),1),
                      P2_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                      P2_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                      P2_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))


summary_prod3 <- daily_agg_cleaned %>%
                      filter(ProductID == 3) %>%
                      group_by(UserID) %>%
                      summarize(P3_Total_Stakes = round(sum(Stakes),2), P3_Avg_Stakes = round(mean(Stakes),2), 
                      P3_Total_Winnings = round(sum(Winnings),2), P3_Avg_Winnings = round(mean(Winnings),2), 
                      P3_Total_Bets = sum(Bets), P3_Avg_Bets = round(mean(Bets),1),
                      P3_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                      P3_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                      P3_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))


summary_prod4 <- daily_agg_cleaned %>%
                      filter(ProductID == 4) %>%
                      group_by(UserID) %>%
                      summarize(P4_Total_Stakes = round(sum(Stakes),2), P4_Avg_Stakes = round(mean(Stakes),2), 
                      P4_Total_Winnings = round(sum(Winnings),2), P4_Avg_Winnings = round(mean(Winnings),2), 
                      P4_Total_Bets = sum(Bets), P4_Avg_Bets = round(mean(Bets),1),
                      P4_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                      P4_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                      P4_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))


summary_prod5 <- daily_agg_cleaned %>%
                      filter(ProductID == 5) %>%
                      group_by(UserID) %>%
                      summarize(P5_Total_Stakes = round(sum(Stakes),2), P5_Avg_Stakes = round(mean(Stakes),2), 
                      P5_Total_Winnings = round(sum(Winnings),2), P5_Avg_Winnings = round(mean(Winnings),2), 
                      P5_Total_Bets = sum(Bets), P5_Avg_Bets = round(mean(Bets),1),
                      P5_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                      P5_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                      P5_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))


summary_prod6 <- daily_agg_cleaned %>%
                      filter(ProductID == 6) %>%
                      group_by(UserID) %>%
                      summarize(P6_Total_Stakes = round(sum(Stakes),2), P6_Avg_Stakes = round(mean(Stakes),2), 
                      P6_Total_Winnings = round(sum(Winnings),2), P6_Avg_Winnings = round(mean(Winnings),2), 
                      P6_Total_Bets = sum(Bets), P6_Avg_Bets = round(mean(Bets),1),
                      P6_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                      P6_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                      P6_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))

summary_prod7 <- daily_agg_cleaned %>%
                      filter(ProductID == 7) %>%
                      group_by(UserID) %>%
                      summarize(P7_Total_Stakes = round(sum(Stakes),2), P7_Avg_Stakes = round(mean(Stakes),2), 
                      P7_Total_Winnings = round(sum(Winnings),2), P7_Avg_Winnings = round(mean(Winnings),2), 
                      P7_Total_Bets = sum(Bets), P7_Avg_Bets = round(mean(Bets),1),
                      P7_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                      P7_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                      P7_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))

summary_prod8 <- daily_agg_cleaned %>%
                      filter(ProductID == 8) %>%
                      group_by(UserID) %>%
                      summarize(P8_Total_Stakes = round(sum(Stakes),2), P8_Avg_Stakes = round(mean(Stakes),2), 
                      P8_Total_Winnings = round(sum(Winnings),2), P8_Avg_Winnings = round(mean(Winnings),2), 
                      P8_Total_Bets = sum(Bets), P8_Avg_Bets = round(mean(Bets),1),
                      P8_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                      P8_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                      P8_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))


# Joining all products
summary_list <- list(summary_df, summary_prod2, summary_prod3, summary_prod4,
                     summary_prod5, summary_prod6, summary_prod7, summary_prod8)

summary_product <- purrr::reduce(summary_list, dplyr::left_join, by = 'UserID')

# Summary By Month
summary_Feb <- daily_agg_cleaned %>%
                    filter(Month == 2) %>%
                    group_by(UserID) %>%
                    summarize(Feb_Total_Stakes = round(sum(Stakes),2), Feb_Avg_Stakes = round(mean(Stakes),2), 
                    Feb_Total_Winnings = round(sum(Winnings),2), Feb_Avg_Winnings = round(mean(Winnings),2), 
                    Feb_Total_Bets = sum(Bets), Feb_Avg_Bets = round(mean(Bets),1),
                    Feb_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                    Feb_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                    Feb_Betting_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))

summary_Mar <- daily_agg_cleaned %>%
                    filter(Month == 3) %>%
                    group_by(UserID) %>%
                    summarize(Mar_Total_Stakes = round(sum(Stakes),2), Mar_Avg_Stakes = round(mean(Stakes),2), 
                    Mar_Total_Winnings = round(sum(Winnings),2), Mar_Avg_Winnings = round(mean(Winnings),2), 
                    Mar_Total_Bets = sum(Bets), Mar_Avg_Bets = round(mean(Bets),1),
                    Mar_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                    Mar_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                    Mar_Betting_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))


summary_Apr <- daily_agg_cleaned %>%
                    filter(Month == 4) %>%
                    group_by(UserID) %>%
                    summarize(Apr_Total_Stakes = round(sum(Stakes),2), Apr_Avg_Stakes = round(mean(Stakes),2), 
                    Apr_Total_Winnings = round(sum(Winnings),2), Apr_Avg_Winnings = round(mean(Winnings),2), 
                    Apr_Total_Bets = sum(Bets), Apr_Avg_Bets = round(mean(Bets),1),
                    Apr_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                    Apr_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                    Apr_Betting_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))


summary_May <- daily_agg_cleaned %>%
                    filter(Month == 5) %>%
                    group_by(UserID) %>%
                    summarize(May_Total_Stakes = round(sum(Stakes),2), May_Avg_Stakes = round(mean(Stakes),2), 
                    May_Total_Winnings = round(sum(Winnings),2), May_Avg_Winnings = round(mean(Winnings),2), 
                    May_Total_Bets = sum(Bets), May_Avg_Bets = round(mean(Bets),1),
                    May_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                    May_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                    May_Betting_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))


summary_Jun <- daily_agg_cleaned %>%
                    filter(Month == 6) %>%
                    group_by(UserID) %>%
                    summarize(Jun_Total_Stakes = round(sum(Stakes),2), Jun_Avg_Stakes = round(mean(Stakes),2), 
                    Jun_Total_Winnings = round(sum(Winnings),2), Jun_Avg_Winnings = round(mean(Winnings),2), 
                    Jun_Total_Bets = sum(Bets), Jun_Avg_Bets = round(mean(Bets),1),
                    Jun_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                    Jun_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                    Jun_Betting_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))


summary_Jul <- daily_agg_cleaned %>%
                    filter(Month == 7) %>%
                    group_by(UserID) %>%
                    summarize(Jul_Total_Stakes = round(sum(Stakes),2), Jul_Avg_Stakes = round(mean(Stakes),2), 
                    Jul_Total_Winnings = round(sum(Winnings),2), Jul_Avg_Winnings = round(mean(Winnings),2), 
                    Jul_Total_Bets = sum(Bets), Jul_Avg_Bets = round(mean(Bets),1),
                    Jul_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                    Jul_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                    Jul_Betting_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))

summary_Aug <- daily_agg_cleaned %>%
                    filter(Month == 8) %>%
                    group_by(UserID) %>%
                    summarize(Aug_Total_Stakes = round(sum(Stakes),2), Aug_Avg_Stakes = round(mean(Stakes),2), 
                    Aug_Total_Winnings = round(sum(Winnings),2), Aug_Avg_Winnings = round(mean(Winnings),2), 
                    Aug_Total_Bets = sum(Bets), Aug_Avg_Bets = round(mean(Bets),1),
                    Aug_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                    Aug_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                    Aug_Betting_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))

summary_Sep <- daily_agg_cleaned %>%
                    filter(Month == 9) %>%
                    group_by(UserID) %>%
                    summarize(Sep_Total_Stakes = round(sum(Stakes),2), Sep_Avg_Stakes = round(mean(Stakes),2), 
                    Sep_Total_Winnings = round(sum(Winnings),2), Sep_Avg_Winnings = round(mean(Winnings),2), 
                    Sep_Total_Bets = sum(Bets), Sep_Avg_Bets = round(mean(Bets),1),
                    Sep_Avg_StakePerBet = round((sum(Stakes)/sum(Bets)),2), 
                    Sep_Avg_WinningsPerBet = round((sum(Winnings)/sum(Bets)),2),
                    Sep_Betting_Net_Revenue = round((sum(Stakes)-sum(Winnings)),2))

# Creating a list with all the above created summaries per month and joining them all into one data frame
monthly_list <- list(summary_Feb, summary_Mar, summary_Apr, summary_May, summary_Jun, summary_Jul, summary_Aug, summary_Sep)
monthly_summary_df <- purrr::reduce(monthly_list, dplyr::left_join, by = 'UserID')


# Average Count of Days Active Per Product
days_active_df <- daily_agg_cleaned %>%
                      group_by(Month) %>%
                      count(UserID, ProductID)

days_active_by_product <- days_active_df %>%
                              group_by(UserID) %>%
                              summarise(Betting_Avg_Days_Active = round(mean(n),0))

# Count of Products ever Played
monthly_product_count <- daily_agg_cleaned %>%
                              group_by(UserID, Month) %>%
                              summarize(Betting_Product_Count = n_distinct(ProductID))

avg_monthly_product_count <- monthly_product_count %>%
                              group_by(UserID) %>%
                              summarize(Avg_Monthly_Product_Count = round(mean(Betting_Product_Count),1))


product_count_df <- daily_agg_cleaned %>%
                          group_by(UserID) %>%
                          summarize(Betting_Product_Count_Ever = n_distinct(ProductID))

# User Activity
activity_df <- daily_agg_cleaned %>%
                  group_by(UserID) %>%
                  summarize(Betting_Date_First_Played = min(Date), Date_Last_Played = max(Date),
                  Betting_Last_Played_in_Days = as.numeric(difftime(as.Date("2005-09-30"), max(Date), "days")))

# Merging aLL data
all_df_list <- list(activity_df, avg_monthly_product_count, days_active_by_product, product_count_df, summary_product, monthly_summary_df)
agg_data_all <- purrr::reduce(all_df_list, dplyr::left_join, by = 'UserID')


#######################################################################################
#                                       											                        #
#       DATA CLEANING AND DATA MANUPULTAION OF POKER CHIP CONVERSION DATASET       		              #									  
#                                        											                        #
#######################################################################################

# Reading PokerChipConversions dataset in SAS environment
PokerChipConverstion_DS3 <- read_sas("C:/Users/ythonukunuru/Desktop/R/RawDataIIIPokerChipConversions.sas7bdat")

# Inspecting above data set
head(PokerChipConverstion_DS3)
str(PokerChipConverstion_DS3)

# Checking missing values
PokerChipConverstion_DS3 %>% 
      select(UserID,TransDateTime,TransType,TransAmount) %>% 
      filter(!complete.cases(.)) # No missing data was found

# Defining a fucntion to specify number of digits to show after a decimal
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

# Limiting digits after decimal by using custom fucntion
PokerChipConverstion_DS3$TransAmount <- specify_decimal(PokerChipConverstion_DS3$TransAmount,2)

# Convert Character data type back to numeric data type
PokerChipConverstion_DS3$TransAmount <- as.numeric(PokerChipConverstion_DS3$TransAmount)

# Modifying "TransDateTime" column's data type from character to DateTime format using lubridate
PokerChipConverstion_DS3$TransDateTime <- ymd_hms(PokerChipConverstion_DS3$TransDateTime)

# Creating Date and Month Column
PokerChipConverstion_DS3$Day <- day(PokerChipConverstion_DS3$TransDateTime)
PokerChipConverstion_DS3$Month <- month(PokerChipConverstion_DS3$TransDateTime)

# Changing the values of "TransType" column into either "Buy" or "Sell"
class(PokerChipConverstion_DS3$TransType)
class(PokerChipConverstion_DS3$TransDateTime)

# Replacing '124' with 'Buy' and '24' with 'Sell'
PokerChipConverstion_DS3$TransType <-gsub(124,"Buy",PokerChipConverstion_DS3$TransType)
PokerChipConverstion_DS3$TransType <-gsub(24,"Sell",PokerChipConverstion_DS3$TransType)

# Creating 'Sell' and 'Buy' columns to fill in the corresponding values from "TransType" Column
# By creating these two columns we are going to perform aggregations.
PokerChipConverstion_DS3$Sell <- ifelse(PokerChipConverstion_DS3$TransType == "Sell",PokerChipConverstion_DS3$TransAmount,0)
PokerChipConverstion_DS3$Buy <- ifelse(PokerChipConverstion_DS3$TransType == "Buy",PokerChipConverstion_DS3$TransAmount,0)

# Verifying changes
str(PokerChipConverstion_DS3)

# Creating first data set for basic aggregations
Poker_basic_aggregations <- PokerChipConverstion_DS3 %>% 
                                group_by(UserID) %>% 
                                summarise(Mean_PokerChips_Sold = mean(Sell),Mean_PokerChips_Buy = mean(Buy),Total_PokerChips_Sold = sum(Sell),Total_PokerChips_Buy = sum(Buy),Balance = (Total_PokerChips_Buy - Total_PokerChips_Sold),Count_PokerChips_Transactions=n(),Last_PokerChip_Transaction = max(TransDateTime),First_PokerChip_Transaction = min(TransDateTime),Count_PokerBuy = sum(TransType=='Buy'),Count_PokerSell = sum(TransType=='Sell'))

# Creating data set for getting Average Buy & Sell per Month
Month_Details = PokerChipConverstion_DS3 %>%
                      group_by(UserID,Month) %>%
                      summarise(Avg_Sell= mean(Sell),Avg_Buy = mean(Buy),No_of_Trans = n())

# Creating data set - BUY per MONTH
PokerBuy_perMonth =  spread(Month_Details,"Month", 'Avg_Buy',fill = 0)

PokerBuy_perMonth['Avg_Sell'] = PokerBuy_perMonth['No_of_Trans'] = NULL
names(PokerBuy_perMonth) = c('UserID','February','March','April','May','June','July','August','September','October')

PokerBuy_perMonth = PokerBuy_perMonth %>%
                        group_by(UserID) %>%
                        summarise(February_AvgPokerBuy = sum(February), March_AvgPokerBuy = sum(March), April_AvgPokerBuy = sum(April),
                        May_AvgPokerBuy = sum(May), June_AvgPokerBuy = sum(June), July_AvgPokerBuy = sum(July),
                        August_AvgPokerBuy = sum(August), Sept_AvgPokerBuy = sum(September), Oct_AvgPokerBuy = sum(October))



# Creating data set - SELL per MONTH
PokerSell_perMonth =  spread(Month_Details,"Month", 'Avg_Sell',fill = 0.0)

PokerSell_perMonth['Avg_Buy'] = PokerSell_perMonth['No_of_Trans'] = NULL
names(PokerSell_perMonth) = c('UserID','February','March','April','May','June','July','August','September','October')

PokerSell_perMonth = PokerSell_perMonth %>%
                        group_by(UserID) %>%
                        summarise(February_AvgPokerSell = sum(February), March_AvgPokerSell = sum(March), April_AvgPokerSell = sum(April),
                        May_AvgPokerSell = sum(May), June_AvgPokerSell = sum(June), July_AvgPokerSell = sum(July),
                        August_AvgPokerSell = sum(August), Sept_AvgPokerSell = sum(September), Oct_AvgPokerSell = sum(October))


# Creating data set for the No of Transactions per Month
trans_Month =  spread(Month_Details,"Month", 'No_of_Trans',fill = 0)
trans_Month['Avg_Sell'] = trans_Month['Avg_Buy'] = NULL
names(trans_Month) = c('UserID','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct')

trans_Month = trans_Month %>%
                    group_by(UserID) %>%
                    summarise(February_Nbr_PokerTrans = sum(Feb), March_Nbr_PokerTrans = sum(Mar), April_Nbr_PokerTrans = sum(Apr),
                    May_Nbr_PokerTrans = sum(May), June_Nbr_PokerTrans = sum(Jun), July_Nbr_PokerTrans = sum(Jul),
                    August_Nbr_PokerTrans = sum(Aug), Sept_Nbr_PokerTrans = sum(Sep), Oct_Nbr_PokerTrans = sum(Oct))


# Merging all the data sets into final table.
# This will create a single observation per customer
Overall =  merge(Poker_basic_aggregations,trans_Month,'UserID')
Overall = merge(Overall,PokerBuy_perMonth,'UserID')
Overall = merge(Overall,PokerSell_perMonth,'UserID')

# Checking variable types in the final DF 'overall' 
str(Overall)

# Limiting values after decimals in all columns 
is.num <- sapply(Overall, is.numeric)
Overall[is.num] <- lapply(Overall[is.num], round, 2)

#######################################################################################
#                                       											                        #
#           			JOINING ALL CLEANED DATASETS TO CREATE BASE TABLE       		              #									  
#                                        											                        #
#######################################################################################

# Joining cleaned demographics data set with user aggregation data set
demog_useragg_merged_df <- merge(demographics_cleaned_df,agg_data_all, by='UserID')

# Using left join to join 'demog_useragg_merged_df' with 'overall' df
consolidated_datamart_df <- left_join(demog_useragg_merged_df,Overall, by='UserID')

# Renaming few column names
rename(consolidated_datamart_df, Date_Last_Played = Betting_Date_Last_Played)
rename(consolidated_datamart_df, Poker_Balance = Balance)


# Changing date formats to Y-M-D for consistency
consolidated_datamart_df$Last_PokerChip_Transaction <- as.Date(parse_date_time(consolidated_datamart_df$Last_PokerChip_Transaction, date_format))
consolidated_datamart_df$First_PokerChip_Transaction <- as.Date(parse_date_time(consolidated_datamart_df$First_PokerChip_Transaction, date_format))


# Checking if there are any duplicates in 'UserId' columns
n_distinct(consolidated_datamart_df$UserID)

# Checking data type of all the columns in data mart DF
sapply(consolidated_datamart_df, class)

# Saving consolidated Data Mart DF to an 'RDATA' file 
save(consolidated_datamart_df,file="DataMart_BaseTable.RData")
