# Content
## 0. Preparation
## 1. Create and clean datasets
## 2. Create Agenda Setting Lists
## 3. Create Dataset for Analysis
## 4. Regression Discontinuity Analyses
## 5. Further codes described in thesis

# 0. Preparation

## 0.1 load packages
library(Rcrawler)
library(plyr)
library(rlang)
library(tidyverse)
library(readxl)
library(WriteXLS)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(tidyr)
library(plotly)
library(purrr)
library(lubridate)
library(rddtools)
library(quanteda)
library(quanteda.corpora)
library(readtext)

## 0.2 import stop-words
mystop <- read_xlsx("Data/stopwords.xlsx")
mystop <- gather(mystop, key=type, value=word)
mystop <- mystop$word
mystop <- mystop[!is.na(mystop)]

## 0.3 Import country characteristics from Excel
countrycharacteristics <- read_xls("countrycharacteristics.xls")

## 0.3 create useful country and year lists
countries <- c("Slovenia",    "France",   "Czech Republic", "Sweden", 
               "Spain",       "Belgium",  "Hungary",        "Poland", 
               "Denmark",     "Cyprus",   "Ireland",        "Lithuania", 
               "Greece",      "Italy",    "Latvia",         "Luxembourg", 
               "Netherlands", "Slovakia", "Malta",          "Estonia", 
               "Bulgaria")

countriesx <- as.list(countries[5:20])
countriesy1 <- as.list(countries[1:16])
countriesy2 <- as.list(countries[2:17])
countriesy3 <- as.list(countries[3:18])
countriesy4 <- as.list(countries[4:19])
countriesB <- as.list(countries[5:14])
countriesJ <- as.list(countries[14:20])
countriesjan <- as.list(countries[c(5, 7, 9, 11, 13, 15, 17, 19)])
countriescrisis <- as.list(countries[c(5, 6, 7, 8, 16, 17, 18)])

halfyears <- c("2008-01-01", "2008-07-01", "2009-01-01", "2009-07-01", "2010-01-01", "2010-07-01", 
               "2011-01-01", "2011-07-01", "2012-01-01", "2012-07-01", "2013-01-01", "2013-07-01", 
               "2014-01-01", "2014-07-01", "2015-01-01", "2015-07-01", "2016-01-01", "2016-07-01", 
               "2017-01-01", "2017-07-01")
halfyearsx <- as.list(halfyears[3:20])
halfyearsy <- as.list(halfyears[2:19])
halfyearsz <- as.list(halfyears[3:18])

years <- c("2010", "2010", "2011", "2011", "2012", "2012", "2013", "2013", 
           "2014", "2014", "2015", "2015", "2016", "2016", "2017", "2017")
yearsy <- c("2009", "2010", "2010", "2011", "2011", "2012", "2012", "2013", 
            "2013", "2014", "2014", "2015", "2015", "2016", "2016", "2017")


#1. Create and clean datasets

## 1.1 Presidency texts
### Presidency Programmes
prog <- lapply(excel_sheets(path = "Data/Presidency Programmes.xlsx"),
               read_excel, path = "Data/Presidency Programmes.xlsx")
prog <- ldply (prog, data.frame)
prog <- fill(prog, country, date, .direction = "down")
prog$date <- ymd(prog$date)
prog <- prog %>% unnest_tokens(word,text)

#### Nest
nested_prog <- prog %>%
  nest(word) %>%
  mutate(text = map(data, unlist), 
         text = map_chr(text, paste, collapse = " "),
         type = "Programme") 

### speeches to parliament
speech <- lapply(excel_sheets(path = "Data/Presidency Speeches EP.xlsx"),
                 read_excel, path = "Data/Presidency Speeches EP.xlsx")
speech <- ldply (speech, data.frame)
speech <- fill(speech, country, date, speaker, .direction = "down")
speech$date <- ymd(speech$date)
speech <- speech %>% unnest_tokens(word,text)

#### Nest
nested_speech <- speech %>%
  nest(word) %>%
  mutate(text = map(data, unlist), 
         text = map_chr(text, paste, collapse = " "),
         type = "Plenary") 

### combine programmes and speeches
nested_agenda <- full_join(select(nested_prog, country, text_prog = text), select(nested_speech, country, text_speech=text), by="country")
nested_agenda <- nested_agenda %>% unite(text, text_prog, text_speech, sep=" ")
rm(speech, prog, nested_speech, nested_prog)

### clean and create n-grams
agenda_corp <- corpus(nested_agenda)
docnames(agenda_corp) <- paste(agenda_corp$documents$country)
rm(nested_agenda)
agenda_toks <- tokens(agenda_corp)
agenda_toks_nostop <- tokens_select(agenda_toks, mystop, selection= 'remove', padding = TRUE)
agenda_ngram <-tokens_ngrams(agenda_toks_nostop, n=2:4)
rm(agenda_corp, agenda_toks_nostop)

## 1.2 Commission Work Programmes

CWPs <- readtext(paste0("/Users/mariuskat/Dropbox/Hertie/Thesis/Commission Work Programmes/*.pdf"))
CWPs$date <- c(2009:2018)
CWPs <- CWPs %>% unnest_tokens(word,text)

### Nest
CWPs <- CWPs %>%
  nest(word) %>%
  mutate(text = map(data, unlist), 
         text = map_chr(text, paste, collapse = " "),
         type = "Work Programme") 

### clean and create n-grams
CWPs <- corpus(CWPs)
docnames(CWPs) <- paste(CWPs$documents$date)
CWPs <- tokens(CWPs)
CWPs_nostop <- tokens_select(CWPs, mystop, selection= 'remove', padding = TRUE)
CWPs_ngrams <-tokens_ngrams(CWPs_nostop, n=2:4)
CWPs_ngrams <- dfm(CWPs_ngrams)
CWPs_tidy <- tidy(CWPs_ngrams)

## 1.3 Commission Press Releases

### Mine releases from the internet (done on March 7th)
dir.create("../PRhtmls")
PR_data <- data.frame(title=character(),date=character(), text=character(), stringsAsFactors = FALSE)

#### Type 1
for (i in 1:614) {
  url <- paste("http://europa.eu/rapid/search-result.htm?query=18&locale=en&page=",i, sep="")
  Rcrawler (Website = url,
            urlregexfilter= "http://europa.eu/rapid/press-release[[:print:]]{2,}_en.htm$",  DIR = "/Users/mariuskat/Dropbox/Hertie/Thesis/Data/PRhtmls", 
            MaxDepth=1, ExtractCSSPat = c(
              ".title", ".date", ".content"), PatternsName = c("title", "date", "text"))
  temp<-data.frame(t(do.call("rbind", DATA)))
  temp <-t(temp)
  PR_data <- rbind(PR_data, temp)
}            

#### Type 2
for (i in 616:1669) {
  url <- paste("http://europa.eu/rapid/search-result.htm?query=18&locale=en&page=",i, sep="")
  Rcrawler (Website = url,
            urlregexfilter= "http://europa.eu/rapid/press-release[[:print:]]{2,}_en.htm$",  DIR = "/Users/mariuskat/Dropbox/Hertie/Thesis/Data/PRhtmls", 
            MaxDepth=1, ExtractCSSPat = c(
              ".A_Standard__33__20_Titre", ".A_Standard__32__20_Date", "#contentPressRelease"), PatternsName = c("title", "date", "text"))
  temp<-data.frame(t(do.call("rbind", DATA)))
  temp <-t(temp)
  PR_data <- rbind(PR_data, temp)
} 

#### Type 3
for (i in 1670:2439) {
  url <- paste("http://europa.eu/rapid/search-result.htm?query=18&locale=en&page=",i, sep="")
  Rcrawler (Website = url,
            urlregexfilter= "http://europa.eu/rapid/press-release[[:print:]]{2,}_en.htm$",  DIR = "/Users/mariuskat/Dropbox/Hertie/Thesis/Data/PRhtmls", 
            MaxDepth=1, ExtractCSSPat = c(
              ".A_Standard__33__20_Titre", ".A_Standard__32__20_Date", ".A__35__20_Lieu_5f_Date_P4","#contentPressRelease"), PatternsName = c("title", "date","dateplace", "text"))
  temp<-data.frame(t(do.call("rbind", DATA)))
  temp <-t(temp)
  PR_data <- rbind(PR_data, temp)
} 

#### Type 4
for (i in 2440:2905) {
  url <- paste("http://europa.eu/rapid/search-result.htm?query=18&locale=en&page=",i, sep="")
  Rcrawler (Website = url,
            urlregexfilter= "http://europa.eu/rapid/press-release[[:print:]]{2,}_en.htm$",  DIR = "/Users/mariuskat/Dropbox/Hertie/Thesis/Data/PRhtmls", 
            MaxDepth=1, ExtractCSSPat = c(
              "h1", "#contentPressRelease p:nth-child(2)", ".A__35__20_Lieu_5f_Date_P4", "#contentPressRelease"), PatternsName = c("title", "date", "dateplace", "text"))
  temp<-data.frame(t(do.call("rbind", DATA)))
  temp <-t(temp)
  PR_data <- rbind(PR_data, temp)
} 


### Clean press releases
prdata2 <- PR_data
prdata2 <- prdata2 %>% separate(date, into = c("city", "date"), sep = ",")
prdata2$date <- map(prdata2$date, function(y) gsub("rd |nd |th |st", " ", y))
prdata2$date <- map(prdata2$date, function(y) gsub("augu|Augu", "August", y))
prdata2$date <- dmy(prdata2$date)
prdata2 <- prdata2 %>% fill(date)
prdata2 <- prdata2 %>% mutate(month = floor_date(date, unit = "month"))
prdata2 <- filter(prdata2, date >= as.Date("2008-01-01"))
prdata2$text <- map(prdata2$text, function(y) gsub("\\n|\\r|\\t", " ", y))
prdata2 <- subset(prdata2, !duplicated(text))


# 2. Create Agenda Setting Lists

## 2.1 create lists of items introduced in press releases per half year

### Group PRs per half year
prforcontrol <- prdata2 %>% select(text, month)
prforcontrol <- prforcontrol %>% mutate(halfyear = floor_date(month, unit = "halfyear"))
prforcontrol <- prforcontrol %>% select(halfyear, text)
prforcontrol <- prforcontrol %>% unnest_tokens(word, text)
prforcontrol <- prforcontrol %>% nest(word) %>% 
  mutate(text = map(data, unlist),
         text = map_chr(text, paste, collapse = " "))

### Select relevant half years, clean and put in n-grams
prforcontrol <- prforcontrol[1:19,]
prforcontrol_corp <- corpus(prforcontrol)
docnames(prforcontrol_corp) <- paste(prforcontrol_corp$documents$halfyear)
rm(prforcontrol)
prforcontrol_toks <- tokens(prforcontrol_corp)
rm(prforcontrol_corp)
prforcontrol_toks_nostop <- tokens_select(prforcontrol_toks, mystop, selection= 'remove', padding = TRUE)
prforcontrol_ngram <-tokens_ngrams(prforcontrol_toks_nostop, n=2:4)
rm(prforcontrol_toks_nostop)

### Create lists of items introduced per half year
filterPRs <- function(halfyear1, halfyear2) {
  anti_join(as.tibble(prforcontrol_ngram[[halfyear1]]), as.tibble(prforcontrol_ngram[[halfyear2]]))
}

filteredPRs <- map2(halfyearsx, halfyearsy, filterPRs)
attr(filteredPRs, "names") <- halfyearsx


## 2.2 Create Agenda Setting lists per country

list_AS <- function(x, y1, y2, y3, y4, halfyear, year) {
  country <- as.tibble(agenda_ngram[[x]]) %>% group_by(value) %>% summarise(n=n())
  incom <- filter(CWPs_tidy, CWPs_tidy$document == year)
  anti_join(filter(country, country$n > 1), as.tibble(filteredPRs[[halfyear]]), by="value") %>% 
    anti_join(as.tibble(c(agenda_ngram[[y1]], agenda_ngram[[y2]], agenda_ngram[[y3]], agenda_ngram[[y4]])), by="value") %>% 
    anti_join(incom, by= c("value" = "term"))
}

AS_lists <-   pmap(list(  x = countriesx,
                          y1 = countriesy1,
                          y2 = countriesy2,
                          y3 = countriesy3,
                          y4 = countriesy4,
                          halfyear = halfyearsy,
                          year = years),
                   list_AS)
attr(AS_lists, "names") <- countriesx

## 2.3 Put country-lists into common dataset and remove n-grams that only occur as part of larger n-gram

### Put in common dataset
ASfull <- data.frame()
for(i in countriesx) {
  AS_lists[[i]] <- mutate(AS_lists[[i]], country = i)
  ASfull <- rbind(ASfull, AS_lists[[i]])
}

### Remove n-grams that occur only as part of larger n-grams
AS_sep <- ASfull %>% separate(value, c("word1", "word2", "word3", "word4"), sep= "_")
AS_sep <- AS_sep[!(is.na(AS_sep$word4)),]
AS_check <- rbind(select(unite(AS_sep, value, c("word1", "word2")), c("value", "n", "country")), 
                  select(unite(AS_sep, value, c("word2", "word3")), c("value", "n", "country")), 
                  select(unite(AS_sep, value, c("word3", "word4")), c("value", "n", "country")), 
                  select(unite(AS_sep, value, c("word1", "word2", "word3")), c("value", "n", "country")), 
                  select(unite(AS_sep, value, c("word2", "word3", "word4")), c("value", "n", "country")))
ASfull <- anti_join(ASfull, AS_check)

AS_sep2 <- ASfull %>% separate(value, c("word1", "word2", "word3", "word4"), sep= "_")
AS_sep2 <-AS_sep2[!(is.na(AS_sep2$word3))&is.na(AS_sep2$word4),]

AS_check2 <- rbind(select(unite(AS_sep2, value, c("word1", "word2")), c("value", "n", "country")), 
                   select(unite(AS_sep2, value, c("word2", "word3")), c("value", "n", "country")))
ASfull <- anti_join(ASfull, AS_check2)
rm(AS_sep, AS_sep2, AS_check, AS_check2)


# 3. Create Dataset for Analysis

## 3.1 Put press releases in a format suited for checking the frequency with which AS-list items occur

### clean press releases and put in n-grams
ref_corp <- corpus(as.character(prdata2$text))
ref_toks <- tokens(ref_corp)
attr(ref_toks, "names") <- prdata2$month
ref_toks_nostop <- tokens_select(ref_toks, mystop, selection= 'remove', padding = TRUE)
ref_ngram <-tokens_ngrams(ref_toks_nostop, n=2:4)
rm(ref_toks_nostop)
ref_dfm <- dfm(ref_ngram)
rm(ref_ngram)

### Put n-grams in single dataset
ref_tidy <- tidy(ref_dfm)
rm(ref_dfm)
ref_tidy2 <- rename(ref_tidy, date=document)
ref_tidy2$date <- as.character(ref_tidy2$date)
ref_tidy2 <- ref_tidy2 %>% group_by(date, term) %>% summarise_all(sum)
ref_tidy3 <- spread(ref_tidy2, key = date, value= count)

## 3.2 After hand-coding council configurations, import AS list from Excel and prepare for analysis

ASfull <- read_xls("Data/ASfull.xls")
ASfull <- select(ASfull, value, country, council_configuration)
ASfull <- rename(ASfull, term=value)

## 3.3 For each AS-list item, indicate per month the frequency with which it occurs in press releases

AS_full_joined <- left_join(ASfull, ref_tidy3)

## 3.4 Make months relative to start of presidency

numbered_presidencies <- data.frame(months = months$date)

a <- -17
for(i in countriesx) {
  a <- a-6
  numbered_presidencies[i] <- c(a:(a+122))
}

numbered_presidencies <- gather(numbered_presidencies, key=country, value=presmonth, -months)
AS_full_joined2 <- gather(AS_full_joined, key=months, value=appearances, -c(term, country, council_configuration))
AS_full <- left_join(numbered_presidencies, AS_full_joined2, by = c("country", "months") )
AS_full <- left_join(AS_full, months, by = c("months" = "date"))

## 3.5 Add relevant variables and remove outliers

AS_full$perc <- if_else(is.na(AS_full$appearances), 0, as.double(AS_full$appearances)/AS_full$n*100)
AS_outliers <- filter(AS_full, AS_full$perc>0.075)
AS_full <- anti_join(AS_full, AS_outliers, by = "term")
AS_full <- left_join(AS_full, countrycharacteristics, by = c("country" = "Country"))
AS_full$new <- if_else(as.double(AS_full$`Accession year`) < 2004, "no", "yes")


# 4. Regression Discontinuity Analysis

## 4.1 For all countries combined

### Filter out months with observations for less than 5 countries
AS_full_all <- AS_full[! (AS_full$presmonth < -89 |AS_full$presmonth >76),]

### Results for equal weight for all words
rdd_all_avg <- rdd_data(AS_full_all$perc, AS_full_all$presmonth, cutpoint=0)
reg_all_avg <- rdd_reg_lm(rdd_object = rdd_all_avg, order=1)
print(reg_all_avg)
summary(reg_all_avg)

### Results and graph with for equal weight per country
rdd_all_avg <- AS_full_all %>% group_by(country, presmonth) %>% summarise_at("perc", mean)
rdd_all_avg <- rdd_data(rdd_all_avg$perc, rdd_all_avg$presmonth, cutpoint=0)
reg_all_avg <- rdd_reg_lm(rdd_object = rdd_all_avg, order=1)
print(reg_all_avg)
plot(reg_all_avg, xlab="Number of Months before/after the Presidency", ylab="Average Percentage")
summary(reg_all_avg)

#### Plot the Placebo RDD graph
plotPlacebo(reg_all_avg)

### Results when removing 'unclassified' council configurations
AS_full2 <- filter(AS_full_all, AS_full_all$council_configuration != "Unclassified")

rdd_all_avg <- AS_full2 %>% group_by(country, presmonth) %>% summarise_at("perc", mean)
rdd_all_avg <- rdd_data(rdd_all_avg$perc, rdd_all_avg$presmonth, cutpoint=0)
reg_all_avg <- rdd_reg_lm(rdd_object = rdd_all_avg, order=1)
print(reg_all_avg)
plot(reg_all_avg, xlab="Number of Months before/after the Presidency", ylab="Average Percentage")
summary(reg_all_avg)

#### Plot the Placebo RDD graph
plotPlacebo(reg_all_avg)

### Results for Barosso and Juncker Commission Presidencies

rdd_all_avg <- filter(AS_full_all, AS_full_all$country %in% countriesB) %>% group_by(presmonth, country) %>% summarise_at("perc", mean)
rdd_all_avg <- rdd_data(rdd_all_avg$perc, rdd_all_avg$presmonth, cutpoint=0)
reg_all_avg <- rdd_reg_lm(rdd_object = rdd_all_avg, order=1)
print(reg_all_avg)
summary(reg_all_avg)

rdd_all_avg <- filter(AS_full_all, AS_full_all$country %in% countriesJ) %>% group_by(presmonth, country) %>% summarise_at("perc", mean)
rdd_all_avg <- rdd_data(rdd_all_avg$perc, rdd_all_avg$presmonth, cutpoint=0)
reg_all_avg <- rdd_reg_lm(rdd_object = rdd_all_avg, order=1)
print(reg_all_avg)
summary(reg_all_avg)

### Differences between January and July presidencies

rdd_all_avg <- filter(AS_full, AS_full$country %in% countriesjan) %>% group_by(presmonth, country) %>% summarise_at("perc", mean)
rdd_all_avg <- rdd_data(rdd_all_avg$perc, rdd_all_avg$presmonth, cutpoint=0)
reg_all_avg <- rdd_reg_lm(rdd_object = rdd_all_avg, order=1)
print(reg_all_avg)
summary(reg_all_avg)

rdd_all_avg <- filter(AS_full, !(AS_full$country %in% countriesjan)) %>% group_by(presmonth, country) %>% summarise_at("perc", mean)
rdd_all_avg <- rdd_data(rdd_all_avg$perc, rdd_all_avg$presmonth, cutpoint=0)
reg_all_avg <- rdd_reg_lm(rdd_object = rdd_all_avg, order=1)
print(reg_all_avg)
summary(reg_all_avg)

### Differences between crisis Presidencies and non-crisis Presidencies

rdd_all_avg <- filter(AS_full, AS_full$country %in% countriescrisis) %>% group_by(presmonth, country) %>% summarise_at("perc", mean)
rdd_all_avg <- rdd_data(rdd_all_avg$perc, rdd_all_avg$presmonth, cutpoint=0)
reg_all_avg <- rdd_reg_lm(rdd_object = rdd_all_avg, order=1)
print(reg_all_avg)
plot(reg_all_avg)
summary(reg_all_avg)

rdd_all_avg <- filter(AS_full, !(AS_full$country %in% countriescrisis)) %>% group_by(presmonth, country) %>% summarise_at("perc", mean)
rdd_all_avg <- rdd_data(rdd_all_avg$perc, rdd_all_avg$presmonth, cutpoint=0)
reg_all_avg <- rdd_reg_lm(rdd_object = rdd_all_avg, order=1)
print(reg_all_avg)
plot(reg_all_avg)
summary(reg_all_avg)


## 4.2 Results per country

### Formula for country results including all observations
createRDDs_country <- function(MSs) {
  rdd_country <- filter(AS_full, AS_full$country == MSs)
  rdd_object=rdd_data(rdd_country$perc, rdd_country$presmonth, cutpoint=0)
}

### Formula for country results with only clearly classified council configuration topics
createRDDs_country2 <- function(MSs) {
  rdd_country <- filter(AS_full, AS_full$country == MSs & AS_full$council_configuration != "Unclassified")
  rdd_object=rdd_data(rdd_country$perc, rdd_country$presmonth, cutpoint=0)
}


### graph countryresults (chage formula used according to need)

#### Put relevant data in dataset
countryresults <- data.frame()
for(i in countriesx) {
    x <- summary(rdd_reg_lm(createRDDs_country2(i)))
    x <- as.data.frame(x$coefficients)
    x$country <- i
    countryresults <- rbind(countryresults, x)
}

#### Clean up dataset, add relevant variables and make ready for graph
countryresults <- rownames_to_column(countryresults)
countryresults2 <- countryresults
countryresults2$min <- countryresults2$Estimate - 1.96*countryresults2$`Std. Error`
countryresults2$max <- countryresults2$Estimate + 1.96*countryresults2$`Std. Error`
countryresults2 <- countryresults2 %>%  gather(key="type", value=value, -c(rowname, country))
countryresults2$rowname <- map(countryresults2$rowname, function(y) gsub("\\d", "", y))
countryresults2 <- unite(countryresults2, observation, c("rowname", "type"))
countryresults2 <- spread(countryresults2, key="observation", value="value")
countryresults2$percmax <- countryresults2$D_max/countryresults2$`(Intercept)_Estimate`*100
countryresults2$percest <- countryresults2$D_Estimate/countryresults2$`(Intercept)_Estimate`*100
countryresults2$percmin <- countryresults2$D_min/countryresults2$`(Intercept)_Estimate`*100

#### Create graph
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
ggplot(countryresults2, aes(x=reorder(country, percest), percest, ymin=percmin, ymax=percmax)) + 
  geom_pointrange() + geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff, show.legend=FALSE) +
  labs(x = "Country", y="Presidency Effect (% Intercept Estimate)") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## 4.3 Council Configuration Results

### create list of existing council configurations
council_configurations <- AS_full %>% count(council_configuration)
council_configurations <- as.list(council_configurations$council_configuration)

### Formula to get results per council configuration
createRDDs_council <- function(councilconf) {
  rdd_council <- filter(AS_full_all, AS_full_all$council_configuration == councilconf) %>% group_by(presmonth, country) %>% summarise_at("perc", mean)
  rdd_object=rdd_data(rdd_council$perc, rdd_council$presmonth, cutpoint=0)
}

### Formula to get results per council configuration for Barroso Commissions only
createRDDs_council2 <- function(councilconf) {
  rdd_council <- filter(AS_full_all, !(AS_full_all$country %in% countriesJ) & AS_full_all$council_configuration == councilconf)
  rdd_object=rdd_data(rdd_council$perc, rdd_council$presmonth, cutpoint=0)
}

### Put relevant data in dataset (use formula according to need)
councilresults <- data.frame()
for(i in council_configurations) {
  x <- summary(rdd_reg_lm(createRDDs_council(i)))
  x <- as.data.frame(x$coefficients)
  x$council <- i
  councilresults <- rbind(councilresults, x)
}

### Clean up dataset, add relevant variables, prepare for graph
councilresults <- rownames_to_column(councilresults)
councilresults2 <- councilresults
councilresults2$min <- councilresults2$Estimate - 1.96*councilresults2$`Std. Error`
councilresults2$max <- councilresults2$Estimate + 1.96*councilresults2$`Std. Error`
councilresults2 <- councilresults2 %>%  gather(key="type", value=value, -c(rowname, council))
councilresults2$rowname <- map(councilresults2$rowname, function(y) gsub("\\d", "", y))
councilresults2 <- unite(councilresults2, observation, c("rowname", "type"))
councilresults2 <- spread(councilresults2, key="observation", value="value")
councilresults2$percmax <- councilresults2$D_max/councilresults2$`(Intercept)_Estimate`*100
councilresults2$percest <- councilresults2$D_Estimate/councilresults2$`(Intercept)_Estimate`*100
councilresults2$percmin <- councilresults2$D_min/councilresults2$`(Intercept)_Estimate`*100

### Graph results
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
ggplot(councilresults2, aes(x=reorder(council, percest), percest, ymin=percmin, ymax=percmax)) + 
  geom_pointrange() + geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff, show.legend=FALSE) +
  labs(x = "Council Configuration", y="Presidency Effect (% Intercept Estimate)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## 4.4 Per Geographical location

### formula
createRDDs_location <- function(x) {
  rdd_council <- filter(AS_full, AS_full$location == x)
  rdd_object=rdd_data(rdd_council$perc, rdd_council$presmonth, cutpoint=0)
}

### dataset
locationresults <- data.frame()
for(i in c("East", "West", "South")) {
  x <- summary(rdd_reg_lm(createRDDs_location(i)))
  x <- as.data.frame(x$coefficients[(1:2), ])
  x$council <- i
  locationresults <- rbind(locationresults, x)
}

### prepare dataset for graph
locationresults <- rownames_to_column(locationresults)
locationresults2 <- locationresults[,(-c(4:5))]
locationresults2$min <- locationresults2$Estimate - 1.96*locationresults2$`Std. Error`
locationresults2$max <- locationresults2$Estimate + 1.96*locationresults2$`Std. Error`
locationresults2 <- locationresults2 %>%  gather(key="type", value=value, -c(1, 4))
locationresults2$rowname <- map(locationresults2$rowname, function(y) gsub("\\d", "", y))
locationresults2 <- unite(locationresults2, observation, c("rowname", "type"))
locationresults2 <- spread(locationresults2, key="observation", value="value")
locationresults2$percmax <- locationresults2$D_max/locationresults2$`(Intercept)_Estimate`*100
locationresults2$percest <- locationresults2$D_Estimate/locationresults2$`(Intercept)_Estimate`*100
locationresults2$percmin <- locationresults2$D_min/locationresults2$`(Intercept)_Estimate`*100

### graph
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
ggplot(locationresults2, aes(x=reorder(council, percest), percest, ymin=percmin, ymax=percmax)) + 
  geom_pointrange() + geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff, show.legend=FALSE) +
  labs(x = "Geographical Location", y="Presidency Effect (% Intercept Estimate)")


# 5. Further codes described in thesis

## 5.1 attempt at topic modelling
agenda_dfm <- dfm(agenda_ngram) %>% 
  dfm_remove(c('*-time', '*-timeUpdated', 'GMT', 'BST')) %>% 
  dfm_trim(min_docfreq = 0.1, max_docfreq = 0.9)
agenda_dfm <- agenda_dfm[ntoken(agenda_dfm) > 0,]

dtm <- convert(agenda_dfm, to = "topicmodels")
lda <- LDA(dtm, k = 15)

terms(lda, 10)

## 5.2 show press releases per month  + relation Juncker and press releases
months <- prdata2 %>% count(month)

months <- months %>%  rownames_to_column()
months$rowname <- as.double(months$rowname) - 83
months$Juncker <- if_else(months$rowname>=0, 1, 0)
summary(lm(months$n ~ months$Juncker))
rdd_months <- rdd_reg_lm(rdd_object = rdd_data(months$n, months$rowname, cutpoint=0), order=1)
summary(rdd_months)
plot(rdd_months ,
     xlab = "Months before and after Juncker took office", 
     ylab="Number of Press Releases")

## 5.3 show distribution of frequency distribution of AS items
AS_full %>% filter(AS_full$perc>0) %>% ggplot(aes(x=perc)) + 
  geom_histogram(bins=100) + scale_y_log10() +
  labs(x="perc", y="frequency")

## 5.4 show number of AS-items per country and per council configuration
ASfull %>% group_by(country) %>% summarise(n=n()) %>% ggplot(aes(x=reorder(country, -n), y=n)) + 
  geom_bar(stat="identity") +
  labs(x="", y="Number of Items on AS-list") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ASfull %>% group_by(council_configuration) %>% summarise(n=n()) %>% ggplot(aes(x=reorder(council_configuration, -n), y=n)) + 
  geom_bar(stat="identity") +
  labs(x="", y="Sum of Number of Items on all AS-lists")

## 5.5 generate graph to demonstrate bias due to control for external events
bias <- data.frame(item1= rnorm(1000,-30,10), item2=rnorm(1000,-10, 10), item3=rnorm(1000,10, 10), item4=rnorm(1000,30, 10))
bias <- gather(bias, key=word, value=value)
bias %>% ggplot(aes(x=value, color=word)) + geom_density() +
  labs(x = "Time before/after start of Presidency", y="Probability of being mentioned in a press release")

## 5.6 browse to see examples of items of domestic interest being put on the agenda
AS_dom <- AS_full[!(AS_full$presmonth< -9 | AS_full$presmonth>10),]
AS_dom$pre <- if_else(AS_dom$presmonth <1, 1, 0)
AS_dom$appearances <- if_else(is.na(AS_dom$appearances), 0, AS_dom$appearances)
AS_dom2 <- AS_dom %>% group_by(term, country, pre) %>% summarise_at(c("perc", "appearances"), mean)
AS_dom2 <- AS_dom2 %>% gather(key = type, value = value, -c(term, country, pre))
AS_dom2 <- AS_dom2 %>% unite(measurement, type, pre)
AS_dom2 <- AS_dom2 %>% spread(key = measurement, value = value)
AS_dom2$perc <- AS_dom2$`perc_1`/(AS_dom2$`perc_0` + 0.00000000001)
AS_dom2 <- AS_dom2[, c("term", "country", "appearances_0", "appearances_1", "perc")]

## 5.7 correlations pres-effect and other country-level variables. 
resultstable <- read_xls("countryresults.xls")
summary(lm(resultstable$`Presidency effect (%)` ~ resultstable$`Population (rounded to 100.000)[1]`))
summary(lm(resultstable$`Presidency effect (%)` ~ resultstable$GDP))
summary(lm(resultstable$`Presidency effect (%)` ~ resultstable$`Accession year`))



