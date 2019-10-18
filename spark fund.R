
#### ---- SPARK FUND CASE STUDY ---- ####


# Loading datasets
companies <- read.delim("companies.txt", stringsAsFactors = FALSE)
rounds <- read.csv("rounds2.csv", stringsAsFactors = FALSE)
mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE, check.names = F)

str(companies)

require(dplyr)
glimpse(rounds)

View(mapping)

names(companies)
names(rounds)
names(mapping)
# , check.names = F to keep column names from distorting

# Check NA's
sum(is.na(companies))
sum(is.na(rounds))
sum(is.na(mapping))

colSums(is.na(rounds))

# Check blanks
colSums(companies == "")
colSums(rounds == "")
colSums(mapping == "")


# Table 1 ----

# To avoid error due to case sensitive nature of R
rounds$company_permalink <- tolower(rounds$company_permalink)  
companies$permalink <- tolower(companies$permalink)


# Or we can do so in just one go -->>
companies <- sapply(companies, tolower)  
companies <- as.data.frame(companies, stringsAsFactors = F)

rounds <- sapply(rounds, tolower)  
rounds <- as.data.frame(rounds, stringsAsFactors = F)

# But keep an eye on structure as sapply outupt is matrix
str(rounds)
rounds$raised_amount_usd <- as.numeric(rounds$raised_amount_usd)

str(rounds)
?mutate_if
# Or better use mutate_if of dplyr
mapping <- mutate_if(mapping, is.character, tolower)
str(mapping)

# Q.1 No. of unique companies in rounds
length(unique(rounds$company_permalink))
#  66368

# Q.2 No. of unique companies in companies
k <- distinct(companies, permalink)
count(k)
# 66368


# Q.3 In the companies data frame, which column can be used as the unique key for each company?
# Count distinct values in each column and find out
sapply(companies, n_distinct)


n_distinct(companies$permalink)

# Q.4 Are there any companies in the companies file which are not present in rounds?
sum(!companies$permalink %in% rounds$company_permalink) # The NOT operator -->> "!"



# Q.5 Merge the two data frames as master_frame.
#     How many observations are present in master_frame?

master_frame <- merge(rounds, companies, by = 'permalink') 
# error because common column name is different in both datasets

# by parameter ???

master_frame <- merge(rounds, companies, by.x = 'company_permalink', by.y = 'permalink')

nrow(master_frame)
summary(master_frame)



# Table 2 ----

# Summarising along funding type to find average funding of each type

type_wise_avg <- master_frame %>% group_by(funding_round_type) %>%
  summarise(avg_funding = mean(raised_amount_usd, na.rm = TRUE)) %>% 
  arrange(avg_funding)

type_wise_avg
# We can see average funding for each of funding types - "venture", "angel", "seed", "private_equity"
# and answer Q.1 to Q.4


# Q.5 The most suitable investment type for Spark.
#     (Average funding per investment round between 5 million to 15 million USD)

filter(type_wise_avg, avg_funding > 5000000 & avg_funding < 15000000)

# We find that venture type funding is most suitable for Spark Funds
# having average funding per round between 5 million and 15 million



# Table 3 ----

# Q.1 to Q.3 - Top 3 english speaking countries

# Filter data for venture type funding only 
venture_data <- filter(master_frame, funding_round_type == "venture") 

# Summarising by country code to find total funding of each country code

country_wise_gp <- venture_data %>% group_by(country_code) %>%
  summarise(total_rounds = n(),
            total_funding = sum(raised_amount_usd, na.rm = TRUE)) %>%
  arrange(desc(total_funding))
# Which to consider for best countries, total_funding or total_rounds ???

head(country_wise_gp, 10)
# From the given list of countries where English is an official language,
# by general convention for country codes, we have -

# Top English speaking country = United States (USA)
# 2nd English speaking country = United Kingdom (GBR)
# 3rd English speaking country = India (IND)

master_frame <- filter(venture_data, country_code == "usa" |
                         country_code == "gbr" |
                         country_code == "ind")

rm(companies, country_wise_gp, rounds, type_wise_avg, venture_data)

# Table 5 ----

# Lets map the business sectors from mapping file.
# Lets see how many match
sum(master_frame$category_list %in% mapping$category_list)

# Lets see how many don't match
sum(!master_frame$category_list %in% mapping$category_list)

# Lets see why so many don't match
master_frame$category_list[!master_frame$category_list %in% mapping$category_list]

# Seems like sub-sectors given after main sector separated by"|"
# This must cause mismatch



library(stringr)
# Splitting category_list column on basis of occurence of "|" into columns
# and assigning 1st column to "primary_sector" column in master_frame 
?str_split

sectors <- str_split(master_frame$category_list,"[|]", simplify = T)

?str_split

sectors <- as.data.frame(sectors, stringsAsFactors = FALSE)

master_frame$primary_sector <- sectors$V1

# Lets see mismatch again 
sum(!master_frame$primary_sector %in% mapping$category_list)

master_frame$primary_sector[!master_frame$primary_sector %in% 
                              mapping$category_list]

# Lets see vice-versa i.e. which sectors in mapping are not in masterframe
mapping$category_list[!mapping$category_list %in% 
                        master_frame$primary_sector]


# We see that category_list contains distorted names of many categories
# e.g.
#       "Natural Language Processing" spelled as   "0tural Language Processing"
#       "Nanotechnology"              spelled as   "0notechnology"
#       "Natural Resources"           spelled as   "0tural Resources"

# So here is a common anomaly where somehow pattern "na" is changed to "0"
# in all the strings containing "na"

# Correcting this anomaly
mapping$category_list <- str_replace_all(mapping$category_list,
                                         "0", "na")

# Check again
sum(!master_frame$primary_sector %in% mapping$category_list)

# Few still remain. Lets see which are they
master_frame$primary_sector[!master_frame$primary_sector %in% mapping$category_list]

mapping$category_list[!mapping$category_list %in% master_frame$primary_sector]


# Now, "0" in category name "Enterprise 2.0"  is also replaced by "na" 
# which was not meant to be replaced. Correfct it.
mapping$category_list <- str_replace_all(mapping$category_list,
                                         "2.na", "2.0")

# Check where all "0" are now.
mapping$category_list[which(str_detect(mapping$category_list, "0"))]

# Anomaly removed.

require(reshape2)
# Convert mapping from wide to long format
?melt

long_mapping <- melt(mapping, id.vars = "category_list",
                     value.name = "value")

# Cleaning long mapping
new_mapping <- long_mapping[long_mapping$value == 1, ]

# Check for blank
View(new_mapping[new_mapping$category_list =="", ])

# Removing rows with blanks, and 3rd column altogether.
new_mapping <- new_mapping[new_mapping$category_list != "", -3]

colnames(new_mapping)[2] <- 'main_sector'

# Merge master_frame with new_mapping; clean & long form of mapping

# Mapping by outer merge as inner merge is causing loss of data
mapped_master <- merge(master_frame, new_mapping,
                       by.x = "primary_sector",
                       by.y = "category_list", all.x = T)

# We have deliberately kept all.x merge because ??

# inner merge is causing loss of data whose main sector is not available
# which may lead to erroneous result (like % of total, total count, total sum etc.)


rm(mapping, long_mapping, new_mapping)
# Now we know -
#           Most suitable funding type for Spark Funds - venture
#           Top 3 English speaking countries - USA, GBR and IND
#           Range of funding preferred by Spark Funds - Between 5 million and 15 million


# 1. Making data frames for each of Top 3 countries with desired filters
usa_df <- filter(mapped_master, country_code == "usa")
usa_gp <- group_by(usa_df, main_sector)
usa_summary <- summarise(usa_gp, total_of_investments = sum(raised_amount_usd, na.rm = T),
                         no_of_investments = n())
arrange(usa_summary, desc(total_of_investments, no_of_investments))

 
gbr_df <- filter(mapped_master, country_code == "gbr")
gbr_gp <- group_by(gbr_df, main_sector)
gbr_summary <- summarise(gbr_gp, total_of_investments = sum(raised_amount_usd, na.rm = T),
                         no_of_investments = n())
arrange(gbr_summary, desc(total_of_investments, no_of_investments))


ind_df <- filter(mapped_master, country_code == "ind")
ind_gp <- group_by(ind_df, main_sector)
ind_summary <- summarise(ind_gp, total_of_investments = sum(raised_amount_usd, na.rm = T),
                         no_of_investments = n())
arrange(ind_summary, desc(total_of_investments, no_of_investments))


# Table 5 can be answered now.

###### --------- ########### --------- ######