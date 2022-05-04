rm(list=ls())
# Final Project Data Wrangling


# First dataset: survey responses from Kaggle
survey <- read.csv("Desktop/Data Wrangling/Group Project/survey_responses.csv")
View(survey)


# Second dataset: scraping majors and rankings from webpage
library(xml2)
page <- read_html("https://www.topuniversities.com/university-rankings-articles/university-subject-rankings/which-subjects-have-happiest-students")
rankings <- xml_text(xml_find_all(page,"//div[@class='clearfix text-formatted field field--name-body field--type-text-with-summary field--label-hidden field__item']/h2[@aria-level='2']"))
rankings

library(tidyr)
rank <- extract_numeric(rankings)
rank

major <- gsub('[.0-9]+', '', rankings)
major

major_and_rank <-data.frame(major,rank)
View(major_and_rank)

as.factor(survey$Department)

# Merging survey and major_and_rank
## adding column that transforms each department to major corresponding with major_and_rank
survey$Major[which(survey$Department=="School of Science" |
               survey$Department== "School of Public Health") ]<-"Natural Sciences"
survey$Major[which(survey$Department=="School of Engineering")]<-
  "Engineering"
survey$Major[which(survey$Department=="School of Business School" | survey$Department=="School of Risk Management" )]<-
  "Accounting, finance and business"
survey$Major[which(survey$Department=="School of Arts & Social Science")]<-
  "Arts and design"
survey$Major[which(survey$Department=="School of Design & Environment")]<- 
  "Arts and design"
survey$Major[which(survey$Department=="School of Computing")]<-
  "Computing"
survey$Major[which(survey$Department=="School of Exchange Students" | survey$Department=="School of Foreign College")]<-
  "Humanities" 
survey$Major[which(survey$Department=="School of International Science" | survey$Department=="School of Public Policy" |
                   survey$Department=="School of Multi Disciplinary Programme" )]<- 
  "Humanities" 
survey$Major[which(survey$Department=="School of Medicine1" | survey$Department=="School of Medicine2" | 	
                     survey$Department=="School of Dentistry")]<-
  "Natural Sciences"
survey$Major[which(survey$Department=="School of Law")]<- 
  "Accounting, finance and business"
survey$Major[which(survey$Department=="School of Systems Science")]<-
  "Engineering"
survey$Major[which(survey$Department=="School of Conservatory Music") |survey$Department=="School of Conservatory of Music"]<-
  "Arts and design"

library(stringr)
major_and_rank$major <- str_trim(major_and_rank$major)

###changing the names so that dataframes = eachother
major_and_rank$major <- ifelse(major_and_rank$major == "Natural Sciences","NS", major_and_rank$major)
survey$Major <- ifelse(survey$Major == "Natural Sciences","NS", survey$Major)

major_and_rank$major <- ifelse(major_and_rank$major == "Mathematics","M", major_and_rank$major)
survey$Major <- ifelse(survey$Major == "Mathematics","M", survey$Major)

major_and_rank$major <- ifelse(major_and_rank$major == "Computing","C", major_and_rank$major)
survey$Major <- ifelse(survey$Major == "Computing","C", survey$Major)

major_and_rank$major <- ifelse(major_and_rank$major == "Media and communication","MC", major_and_rank$major)
survey$Major <- ifelse(survey$Major == "Media and communication","MC", survey$Major)

major_and_rank$major <- ifelse(major_and_rank$major == "Arts and design","AD", major_and_rank$major)
survey$Major <- ifelse(survey$Major == "Arts and design","AD", survey$Major)

major_and_rank$major <- ifelse(major_and_rank$major == "Modern Languages","ML", major_and_rank$major)
survey$Major <- ifelse(survey$Major == "Modern Languages","ML", survey$Major)

major_and_rank$major <- ifelse(major_and_rank$major == "Engineering","E", major_and_rank$major)
survey$Major <- ifelse(survey$Major == "Engineering","E", survey$Major)

major_and_rank$major <- ifelse(major_and_rank$major == "Sport and exercise science","SES", major_and_rank$major)
survey$Major <- ifelse(survey$Major == "Sport and exercise science","SES", survey$Major)

major_and_rank$major <- ifelse(major_and_rank$major == "Humanities","H", major_and_rank$major)
survey$Major <- ifelse(survey$Major == "Humanities","H", survey$Major)

major_and_rank$major <- ifelse(major_and_rank$major == major_and_rank$major[4],"AFB", major_and_rank$major)
survey$Major <- ifelse(survey$Major == "Accounting, finance and business","AFB", survey$Major)



survey_with_major <- merge(survey,major_and_rank, by.x = "Major", by.y = "major", all.x = TRUE)
names(survey_with_major)[names(survey_with_major)=="rank"] <- "Major_Rank"



unique(major_and_rank$major)
unique(survey$Major)

# Third dataset: Wikipedia scraping of Nationality happiness

library(xml2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rvest)

url_nationality <- "https://en.wikipedia.org/wiki/World_Happiness_Report"
wiki<-url_nationality %>% httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
                  read_html()
class(wiki)
wiki

##irrelevant
body<-xml_text(xml_find_all(wiki,"//div[@class='mw-body']"))
body


#https://rama100.github.io/lecture-notes/Simple_Web_Scraping.nb.html
#####scraping the table
wiki %>% 
  html_nodes("table.wikitable") %>% 
  .[[2]] %>% #to choose the 2019 table
  # convert table into a dataframe
  html_table(fill = TRUE) -> happiness_2019
head(happiness_2019)
View(happiness_2019)

clean_happiness <- subset(happiness_2019,select = c('Overall rank', 'Country or region', 'Score'))
View(clean_happiness)

#Matching column names in order to merge
names(clean_happiness)[names(clean_happiness)=="Country or region"] <- "Nationality"

#The countries that are present in our original dataset include:
#"Indonesia"                 "Singapore"                 "Malaysia"                 
#"Viet Nam"                  "Hong Kong"                 "Bangladesh"               
#"India"                     "Myanmar"                   "Philippines"              
#"Canada"                    "United States"             "Poland"                   
#"China"                     "Mexico"                    "Australia"                
#"Taiwan"                    "Korea, Republic of"        "Thailand"                 
#"Switzerland"               "Pakistan"                  "Nepal"                    
#"Israel"                    "Sri Lanka"                 "Germany"                  
#"British National Overseas" "France"                    "Japan"                    
#"United Kingdom"            "Denmark"                   "Turkey"                   
#"Netherlands"              

merged_data <- merge(survey_with_major, clean_happiness, by= "Nationality")
names(merged_data)[names(merged_data)=="Score"] <- "Nationality_Score"
names(merged_data)[names(merged_data)=="Overall rank"] <- "Nationality_Rank"

View(merged_data)


# lienar regression, chi squared, or ANOVA for finding corr of student life satisfaction (this is because target is categorical
# show me the assumption and check for multicollinearity if using regression)
# can switch target variable in the middle




# Analysis

# corr between Stress_Level and Student_Life_Satisfaction???







