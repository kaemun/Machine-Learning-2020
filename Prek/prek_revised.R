library(tidyverse)
setwd("Z:/Interns/2019/Simon/R")

#########################
## Simon Hyungjun Park ##
#########################

##### dc iit : cleaner functions #####

# reading raw data files: DC geocoded individual income tax file
clean_readdta <- function(year){
  mylist <- list.files(path = "./IIT DC Geocoded/Original/", pattern = "\\<geo")
  myfile <- paste0("IIT DC Geocoded/Original/", mylist[grep(year, mylist)])
  read_csv(myfile, col_types = cols(.default = "c"))
}

# # basic filtering
# clean_filter <- function(.data){
#   .data %>%
#     # stayed whole year
#     filter(stay == 12 | is.na(stay)) %>%
#     # filtering data points with missing ssn
#     filter(!is.na(ssn) & nchar(as.numeric(ssn)) < 10)
# }

# basic data cleaning
clean_dta <- function(.data){
  .data %>%
    # cleaning ward
    mutate(ward = str_extract(ward, "[[:digit:]]")) %>% 
    # cleaning ssn
    filter(!is.na(ssn)) %>% 
    mutate(ssn = formatC(as.integer(ssn), width = 9, format = "d", flag = "0")) %>%
    # cleaning census tract
    mutate(census_t = ifelse(is.na(census_t), NA, formatC(as.integer(census_t), width = 6, format = "d", flag = "0"))) %>% 
    # remove dollar sign
    mutate_at(vars(7:11), ~ifelse(is.na(.), "0", str_remove_all(., "[$,-]")) %>% as.numeric)
}

# dob cleaning
clean_dob <- function(.data, fmt){
  .data %>%
    # remove weird entries
    mutate_at(vars(contains("dob")), ~(ifelse(nchar(.) < 7, NA, ifelse(nchar(.) == 7, paste0("0", .), .)))) %>%
    # change the class to Date and saving in desired format as Character
    mutate_at(vars(contains("dob")), ~(format(as.Date(., fmt), "%Y%m%d"))) %>%
    # remove weird entries
    mutate_at(vars(contains("dob")), ~(ifelse(. < "19000000" | . > "20200000", NA, .)))
}

# type cleaning for later years
clean_type <- function(.data) {
  type_lv <- c("SINGLE", "JOINT", "SAMSEP", "SEPART", "HOH", "QWID", "DEPEND", "DJOINT", "DSMSEP", "MFSSIN", "HOHLNK")
  .data[["type"]] <- factor(.data[["type"]], levels = type_lv)
  .data[["type"]] <- as.numeric(.data[["type"]])
  .data
}


#####

##### dc iit : data cleaning #####
# 
dc2001 = clean_readdta(2001) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m %d %Y")

dc2002 = clean_readdta(2002) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m %d %Y")

dc2003 = clean_readdta(2003) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m %d %Y")

dc2004 = clean_readdta(2004) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m %d %Y")

dc2005 = clean_readdta(2005) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m %d %Y")

dc2006 = clean_readdta(2006) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m %d %Y")

dc2007 = clean_readdta(2007) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m %d %Y")

dc2008 = clean_readdta(2008) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m %d %Y")

dc2009 = clean_readdta(2009) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m %d %Y")

dc2010 = clean_readdta(2010) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m %d %Y")

dc2011 = clean_readdta(2011) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m %d %Y")

dc2012 = clean_readdta(2012) %>%
  select(ssn = "Id_Entity", fy = "YEAR", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m/%d/%Y")

dc2013 = clean_readdta(2013) %>%
  select(ssn = "Id_Entity", fy = "Yr_Form", stay = "Number_Of_Months_In_Dc", type = "Filing_Status",
         ward = "MAR_WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Credit_For_Child",
         dep_dob = contains("_dob")) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m/%d/%Y")

dc2014 = clean_readdta(2014) %>%
  select(ssn = "Id", fy = "IdType", stay = "Number_Of_Months_In_Dc", type = "FilingStatus",
         ward = "MAR_WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Child_Care_Credit",
         dep_dob = contains("_DOB")) %>%
  mutate(fy = 2014) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m/%d/%Y") %>%
  clean_type()

dc2015 = clean_readdta(2015) %>%
  select(ssn = "Id", fy = "IdType", stay = "Number_Of_Months_In_Dc", type = "FilingStatus",
         ward = "MAR_WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "DC_EITC_Creditwithchild", kid_cd = "Child_Care_Credit",
         dep_dob = contains("_DOB")) %>%
  mutate(fy = 2015) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m/%d/%Y") %>%
  clean_type()

dc2016 = clean_readdta(2016) %>%
  select(ssn = "Id", fy = "IdType", stay = "Partyear_Resident", type = "FilingStatus",
         ward = "MAR_WARD", census_t = matches('CENSUS_TRA'),
         wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
         low_cd = "Dc_Low_Income_Credit", earn_cd = "DC_EITC_Creditwithchild", kid_cd = "Child_Care_Credit",
         dep_dob = contains("_DOB")) %>%
  # Comments:
  # z.rawdc[[16]]$Partyear_Resident[z.rawdc[[16]]$Partyear_Resident=="False"]
  # notice z.rawdc[[16]]$Partyear_Resident[z.rawdc[[16]]$Partyear_Resident==""]
  mutate(stay = ifelse(stay == "False", 12, NA)) %>%
  mutate(fy = 2016) %>%
  # clean_filter() %>%
  clean_dta() %>%
  clean_dob("%m/%d/%Y") %>%
  clean_type()

rm(list = ls()[grep("clean_", ls())])

#####

##### dc iit : save clean df #####

for (.y in 2001:2016) { 
  cat(crayon::yellow("#####", paste0("dc", .y), "data", "saved", "#####", "\n"))
  paste0("dc", str_sub(.y, -4)) %>% get %>%
    select("Id" = ssn, "Year" = fy, "Number_Of_Months_In_Dc" = stay, "Filing_Status" = type, 
           "Ward" = ward, "Census_Tract" = census_t,
           "Wages_Salaries_Tips" = wage, "Fed_Adjusted_Gross_Income" = adj_inc,
           "Dc_Low_Income_Credit" = low_cd, "Dc_Earned_EITC" = earn_cd, "Dc_Child_Care_Credit" = kid_cd,
           "Dep_DOB_" = contains("_dob")) %>%
    write_csv(paste0("IIT DC Geocoded/dc_iit_", .y, ".csv"), na = "")
}


# check
for(.y in 2001:2016){
  cat(crayon::yellow("\n", "######", .y, "#####", "\n"))
  mydta <- paste0("dc", .y) %>% get
  mydta %>% group_by(stay) %>% summarise(n = n()) %>% filter(stay == 12 | is.na(stay)) %>% print
  cat(crayon::silver("\n", "ssn digits", "\n"))
  mydta[["ssn"]] %>% nchar %>% unique %>% print
  cat(crayon::silver("\n", "census tracts", "\n"))
  (mydta %>% select(census_t) %>% arrange(census_t)) %>% unique %>% filter(!is.na(census_t)) %>% all.equal(tracts) %>% print
  # cat(crayon::silver("\n", "dob", "\n"))
  # (mydta %>% arrange(dep_dob3))[["dep_dob3"]] %>% unique %>% head(5) %>% print
  # (mydta %>% arrange(desc(dep_dob3)))[["dep_dob3"]] %>% unique %>% head(5) %>% print
}

# 2010 Census tracts: https://opendata.dc.gov/datasets/6969dd63c5cb4d6aa32f15effb8311f3_8
tracts10 <- sf::st_read('https://opendata.arcgis.com/datasets/6969dd63c5cb4d6aa32f15effb8311f3_8.geojson', quiet = T) %>% 
  select(census_t = TRACT, GEOID) %>% 
  mutate_if(is.factor, as.character)

# rm(list = ls()[grep("dc2{1}", ls())])

# read clean dc iit
dc_readdta <- function(year){
  mylist <- list.files(path = "./IIT DC Geocoded/", pattern = "\\<dc_iit")
  myfile <- paste0("IIT DC Geocoded/", mylist[grep(year, mylist)])
  read_csv(myfile, col_types = cols(.default = "c")) %>% 
    mutate(Wages_Salaries_Tips = as.numeric(Wages_Salaries_Tips),
           Fed_Adjusted_Gross_Income = as.numeric(Fed_Adjusted_Gross_Income),
           Dc_Earned_EITC = as.numeric(Dc_Earned_EITC),
           Dc_Low_Income_Credit = as.numeric(Dc_Low_Income_Credit),
           Dc_Child_Care_Credit = as.numeric(Dc_Child_Care_Credit)) %>% 
    select(ssn = "Id", fy = "Year", stay = "Number_Of_Months_In_Dc", type = "Filing_Status", 
           ward = "Ward", census_t = "Census_Tract",
           wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
           low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Dc_Child_Care_Credit",
           dep_dob = contains("Dep_DOB_"))
}
#####

##### fed iit : data cleaning amd save clean df #####

clean_readdta_fed <- function(year){
  mylist <- list.files(path = "./IIT Federal/Original/", pattern = "\\<fed")
  myfile <- paste0("IIT Federal/Original/", mylist[grep(year, mylist)])
  mydata <- read_csv(myfile, col_types = cols(.default = "c"))
}

# fed2006 <- clean_readdta_fed(2006)

clean_fed1 <- function(.data){
  ##basic data cleaning
  .data %>% 
    # select intetered variables
    select(ssn = "SSN1", filer_dob = "Date_of_Birth_Taxpayer", fy = "Tax_Period",
           home_kid = "Number_of_Children_at_Home", dep_dob_fed = contains("_DOB")) %>%
    # cleaning fiscal year
    mutate(fy = str_sub(fy, -4)) %>%
    # cleaning ssn
    mutate(ssn = ifelse(is.na(ssn), NA, formatC(as.integer(ssn), width = 9, format = "d", flag = "0"))) %>% 
    # cleaning dob
    mutate_at(vars(contains("dep")), ~(str_remove(., " ") %>% as.Date("%d%b%Y") %>% format('%Y%m%d'))) %>% 
    # remove weird entries
    mutate_at(vars(contains("dep")), ~(ifelse(. < "19000000" | . > "20200000", NA, .)))
}

clean_fed2 <- function(.data){
  ##basic data cleaning
  .data %>% 
    # select intetered variables
    select(ssn = "SSN1", filer_dob = "Date_of_Birth_Taxpayer", fy = "Tax_Period",
           home_kid = "Number_of_Children_at_Home", dep_dob_fed = contains("_DOB")) %>%
    # cleaning fiscal year
    mutate(fy = str_sub(fy, -4)) %>%
    # cleaning ssn
    mutate(ssn = ifelse(is.na(ssn), NA, formatC(as.integer(ssn), width = 9, format = "d", flag = "0"))) %>% 
    # cleaning dob
    mutate_at(vars(contains("dob")), ~(str_remove(., " ") %>% as.Date("%d%b%Y") %>% format('%Y%m%d'))) %>% 
    # remove weird entries
    mutate_at(vars(contains("dep")), ~(ifelse(. < "19000000" | . > "20200000", NA, .)))
}

# Cleaning
for(.y in 2006:2012){ cat(crayon::silver("\n", .y, "data", "\n"))
  assign(paste0("fed", .y), clean_readdta_fed(.y) %>% clean_fed1()) }
for(.y in 2013:2017){ cat(crayon::silver("\n", .y, "data", "\n"))
  assign(paste0("fed", .y), clean_readdta_fed(.y) %>% clean_fed2()) }

# Check
for(.y in 2006:2017){
  cat(crayon::silver("\n", .y, "data", "\n"))
  paste0("fed", .y) %>% get %>% 
    mutate_at(5:8, ~(ifelse(is.na(.), 0, as.numeric(.) %/% 15000000))) %>% 
    mutate(home_kid = as.numeric(home_kid),
           kid_n = dep_dob_fed1 + dep_dob_fed2 + dep_dob_fed3 + dep_dob_fed4) %>% 
    mutate(error = (home_kid - kid_n)) %>% 
    count(error) %>% 
    print()
}

# save clean fed df
for(.y in 2006:2017){
  cat(crayon::yellow("#####", paste0("fed", .y), "data", "saved", "#####", "\n"))
  paste0("fed", str_sub(.y, -4)) %>% get %>% 
    select("Id" = ssn, "Filer_DOB" = filer_dob, "Year" = fy, "Number_of_Children_at_Home" = home_kid,
           "Dep_DOB_" = contains("dep_dob")) %>%
    write_csv(paste0("IIT Federal/fed_iit_", .y, ".csv"), na = "")
}

# reading clean fed df
fed_readdta <- function(year){
  mylist <- list.files(path = "./IIT Federal/", pattern = "\\<fed_iit")
  myfile <- paste0("IIT Federal/", mylist[grep(year, mylist)])
  read_csv(myfile, col_types = cols(.default = "c")) %>%
    select(ssn = "Id", filer_dob = "Filer_DOB", fy = "Year", home_kid = "Number_of_Children_at_Home",
           dep_dob_fed = contains("Dep_DOB"))
}

#####

#######################
##### restoration #####
#######################

##### Comments on Age #####

# Your child must meet legal requirements for age eligibility in order to enroll at DCPS:
# PK3: Your child must turn 3 years of age on or before September 30 to be eligible.
# PK4: Your child must turn 4 years of age on or before September 30 to be eligible.

#####

##### part I   : loading clean data #####

# read clean dc iit
dc_readdta <- function(year){
  mylist <- list.files(path = "./IIT DC Geocoded/", pattern = "\\<dc_iit")
  myfile <- paste0("IIT DC Geocoded/", mylist[grep(year, mylist)])
  read_csv(myfile, col_types = cols(.default = "c")) %>% 
    mutate(Wages_Salaries_Tips = as.numeric(Wages_Salaries_Tips),
           Fed_Adjusted_Gross_Income = as.numeric(Fed_Adjusted_Gross_Income),
           Dc_Earned_EITC = as.numeric(Dc_Earned_EITC),
           Dc_Low_Income_Credit = as.numeric(Dc_Low_Income_Credit),
           Dc_Child_Care_Credit = as.numeric(Dc_Child_Care_Credit)) %>% 
    select(ssn = "Id", fy = "Year", stay = "Number_Of_Months_In_Dc", type = "Filing_Status", 
           ward = "Ward", census_t = "Census_Tract",
           wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
           low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Dc_Child_Care_Credit",
           dep_dob = contains("Dep_DOB_")) %>% 
    # remove filers missing ssn, exclude partyear residents
    filter(!is.na(ssn), stay == 12 | is.na(stay)) %>% 
    # dealing with duplicates (choose the bigger amount, since the tax will be imposed on higher income)
    group_by(ssn) %>% arrange(desc(wage)) %>% filter(row_number() == 1) %>% ungroup
}

for(.y in 2001:2016){ cat(crayon::yellow("#####", paste0("dc", .y), "data", "#####", "\n"))
  assign(paste0("dc", .y), dc_readdta(.y)) }

# reading clean fed df
fed_readdta <- function(year){
  mylist <- list.files(path = "./IIT Federal/", pattern = "\\<fed_iit")
  myfile <- paste0("IIT Federal/", mylist[grep(year, mylist)])
  read_csv(myfile, col_types = cols(.default = "c")) %>%
    select(ssn = "Id", filer_dob = "Filer_DOB", fy = "Year", home_kid = "Number_of_Children_at_Home",
           dep_dob_fed = contains("Dep_DOB"))
}

for(.y in 2006:2017){ cat(crayon::yellow("#####", paste0("fed", .y), "data", "#####", "\n"))
  assign(paste0("fed", .y), fed_readdta(.y)) }

#####

##### part II  : aggregate list of filers + years of residence #####

# aggregating ssn & fy from 2003~2016 dc iit and 2006~2017 federal iit
lls.resid <- tibble("ssn" = NA, "fy" = NA) %>% filter(row_number() != 1)
for(.i in grep("dc20", ls())){ cat(crayon::yellow("#####", ls()[.i], "data", "#####", "\n"))
  lls.resid <- bind_rows(lls.resid, ls()[.i] %>% get %>% select(ssn, fy) %>% unique) }

# remove duplicates
lls.resid <- lls.resid %>% 
  unique() %>% 
  arrange(fy) %>%
  print()

# make a wide list of dummy variables that tracks residency
wls.resid <- lls.resid %>%
  fastDummies::dummy_cols(select_columns = "fy") %>%
  select(-fy) %>% 
  group_by(ssn) %>%
  summarise_all(sum) %>% 
  mutate(fy_count = rowSums(.[-1])) %>% 
  print()

rm(... = lls.resid)

#####

##### part III : aggregate list of filers + filer's dob #####

# aggregating ssn & filer dob from 2006~2017 federal iit
lls.filer_dob <- tibble("ssn" = NA, "filer_dob" = NA) %>% filter(row_number() != 1)
for(.i in grep("fed20", ls())){ cat(crayon::yellow("#####", ls()[.i], "data", "#####", "\n"))
  lls.filer_dob <- bind_rows(lls.filer_dob, ls()[.i] %>% get %>% select(ssn, filer_dob) %>% unique) }

# remove duplicates
lls.filer_dob <- lls.filer_dob %>% 
  filter(!is.na(filer_dob)) %>% unique %>% arrange(filer_dob) %>% print()

# are there many errors?
lls.filer_dob %>% count(ssn) %>% count(n) %>% print()

# filers with no errors
ls1.filer_dob <- lls.filer_dob %>%
  group_by(ssn) %>%
  filter(n() == 1) %>% 
  ungroup() %>% 
  print()

# filers that have 2 different dob
ls2.filer_dob <- lls.filer_dob %>% 
  group_by(ssn) %>% 
  filter(n() == 2) %>% 
  mutate(error = as.numeric(filer_dob) - lag(as.numeric(filer_dob))) %>% 
  # if the error is in few months range, we use the data
  filter(error < 19999) %>% select(-error) %>% 
  # in fact, most of the case it's only few days different / one of the error being empty months or dates
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  print()

# filers that have 3 different dob  
ls3.filer_dob <- lls.filer_dob %>% 
  group_by(ssn) %>% 
  filter(n() == 3) %>% 
  # there are only handful of errors, and I looked at them individually and decided to use them since only few days off
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  print()

# the aggregate list of filer_dob without errors
lls.filer_dob <- bind_rows(... = ls1.filer_dob, ls2.filer_dob, ls3.filer_dob) %>% print()
rm(... = ls1.filer_dob, ls2.filer_dob, ls3.filer_dob)

#####

##### part IV  : aggregate list of filers with single child + child's dob #####

#####      IV-1: DC iit data #####

# aggregating ssn & number of child from 2006~2016 dc iit
lls.dc_n_child <- tibble("ssn" = NA) %>% filter(row_number() != 1)
for(.y in 2006:2016){ cat(crayon::yellow("#####", paste0("dc", .y), "data", "#####", "\n"))
  lls.dc_n_child <- bind_rows(
    lls.dc_n_child, 
    paste0("dc", .y) %>% get %>%
      inner_join(
        paste0("dc", .y) %>% get %>%
          select(ssn, fy, contains("dep_")) %>%
          # remove dependents older than 23 years old, the cutoff is to capture 9 years old in 2003 (2017 23yrs = 2003 9 yrs)
          mutate_at(vars(-ssn, -fy), ~(ifelse(. < paste0(as.numeric(fy) - 23, 1001), NA, 1))) %>%
          mutate(n_child = rowSums(.[-1:-2], na.rm = T)) %>%
          select(ssn, n_child),
        by = "ssn") %>% 
      mutate_at(vars(contains("dep")), ~(ifelse(. < paste0(as.numeric(fy) - 23, 1001), NA, .))) %>%
      mutate_at(vars(contains("dep")), ~(as.numeric(.))) %>% 
      mutate(child_dob = rowSums(.[-1:-11], na.rm = T)) %>% 
      select(-contains("dep"))
  )
}
lls.dc_n_child %>% print()

# single child filers at given year
lls.dcsc <- lls.dc_n_child %>%
  filter(n_child == 1) %>% 
  select(ssn, fy, child_dob)

# filers who reported different dob for the single child at times
lls.dcsc_error <- lls.dcsc %>% 
  group_by(ssn) %>% 
  summarise(sd = sd(child_dob)) %>% 
  filter(sd != 0 | is.nan(sd)) %>% 
  select(ssn)

# are there many errors?
lls.dcsc_error %>% summarise(n = n())

# list of dob without errors
lls.dcsc_dob <- lls.dcsc %>%
  anti_join(lls.dcsc_error, by = "ssn") %>% 
  select(ssn, child_dob) %>% 
  group_by(ssn) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# wide list of child_n
wls.dcsc <- lls.dcsc %>%
  anti_join(lls.dcsc_error, by = "ssn") %>% 
  arrange(fy) %>% 
  fastDummies::dummy_cols(select_columns = "fy") %>% 
  select(-fy, -child_dob) %>% 
  group_by(ssn) %>% 
  summarise_all(sum) %>% 
  mutate(count = rowSums(.[-1])) %>% 
  left_join(lls.dcsc_dob, by = "ssn") %>% 
  select(ssn, child_dob, count, fy_2006:fy_2016) %>% 
  print()

# dob interpolation
lls.dcsc_flag <- wls.dcsc %>% 
  gather(key = "fy", value = "flag", starts_with("fy_")) %>% 
  mutate(fy = str_sub(fy, -4)) %>% 
  mutate(sch_yr = str_sub(child_dob, -4)) %>% 
  mutate(sch_yr = ifelse(sch_yr < "0930", 0, 1)) %>% 
  mutate(grade = as.numeric(fy) - as.numeric(str_sub(child_dob, 1, 4)) - 1 + as.numeric(sch_yr)) %>% 
  group_by(ssn) %>%
  mutate(prev3 = lag(flag, n = 3L)) %>%
  mutate(prev2 = lag(flag, n = 2L)) %>%
  mutate(prev1 = lag(flag, n = 1L)) %>%
  mutate(next1 = lead(flag, n = 1L)) %>%
  mutate(next2 = lead(flag, n = 2L)) %>%
  mutate(next3 = lead(flag, n = 3L)) %>%
  select(ssn, child_dob, count, fy, grade, prev3:prev1, flag, next1:next3) %>% 
  ungroup %>% 
  print()

# interpolate the data points if the filer is only missing when
# (1) the filer has given a birth to a child recently (age 0-2) and 
# (2) the filer appears +1, +2, -1, -2 years
lls.dcsc_manip <- lls.dcsc_flag %>%
  filter(flag == 0) %>%
  filter(-1 < grade, grade < 3) %>%
  filter(prev1 == 1, next1 == 1) %>%
  filter(prev2 == 1 | is.na(prev2), prev3 == 1 | is.na(prev3)) %>%
  filter(next2 == 1 | is.na(next2), next3 == 1 | is.na(next3)) %>%
  mutate(flag = 1) %>% 
  print()

# number of interpolations (manipulations) made
cat(crayon::green(summarise(lls.dcsc_manip, n())[[1]], "data points have been interpolated, i.e. flag value was changed from 0 to 1", "\n"))

# effectively making interpolation     
lls.dcsc_itp <- lls.dcsc_flag %>% 
  anti_join(lls.dcsc_manip, by = c("ssn", "fy")) %>% 
  bind_rows(lls.dcsc_manip) %>% 
  select(ssn, child_dob, count, fy, flag) %>% 
  filter(flag == 1) %>% 
  print()

wls.dcsc_itp <- lls.dcsc_itp %>% 
  spread(key = "fy", value = "flag", sep = "_") %>% 
  print()

# Any technical coding errors?
lls.dcsc_itp %>% filter(child_dob > 30000000) %>% print()

# 
# # wide list of single child
# wls.dc_sc <- wls.dc_n_child %>% 
#   # only include filers who had a single child at any point
#   semi_join(wls.dc_n_child %>% filter_at(vars(-ssn), any_vars(. == 1)), by = "ssn") %>% 
#   # and exclude filers who had more than 1 child at any point
#   anti_join(wls.dc_n_child %>% filter_at(vars(-ssn), any_vars(.  > 1)), by = "ssn") %>% 
#   print()
# 
# # aggregating ssn & dob of child from 2007~2016 dc iit
# wls.dc_dob <- tibble("ssn" = NA) %>% filter(row_number() != 1)
# for(.y in 2006:2016){ cat(crayon::yellow("#####", paste0("dc", .y), "data", "#####", "\n"))
#   wls.dc_dob <- bind_rows(
#     wls.dc_dob, 
#     paste0("dc", .y) %>% get %>%
#       semi_join(wls.dc_sc, by = "ssn") %>% 
#       select(ssn, fy, contains("dep_")) %>%
#       # remove adult dependents
#       mutate_at(vars(-ssn, -fy), ~(ifelse(. < paste0(as.numeric(fy) - 23, 1001), NA, .)))
#   )
# }
# 
# # long list of child dob
# lls.dc_dob <- wls.dc_dob %>%
#   filter_at(vars(-ssn, -fy), any_vars(!is.na(.))) %>% 
#   mutate(child_dob = rowSums(.[-1:-2] %>% mutate_if(is.character, as.numeric), na.rm = T)) %>% 
#   select(ssn, fy, child_dob) %>% 
#   print()
# 
# 
# # Any data entry errors?
# lls.dc_dob %>%
#   group_by(ssn) %>% 
#   summarise(min_dob = min(child_dob, na.rm = T), max_dob = max(child_dob, na.rm = T)) %>% 
#   mutate(error = max_dob - min_dob) %>%
#   mutate(error = ifelse(error == 0, 0, ifelse(error < 10000, 1, 2))) %>% 
#   count(error) %>% 
#   print()
# cat(crayon::green("1 represents a simple mistake 2 represents a seemingly odd data entry", "\n"))
# 
# # list of the dob of a single child filers without errors
# lls.dcsc_dob <- lls.dc_dob %>%
#   group_by(ssn) %>% 
#   summarise(min_dob = min(child_dob, na.rm = T), max_dob = max(child_dob, na.rm = T)) %>% 
#   mutate(error = max_dob - min_dob) %>%
#   filter(error < 1999) %>% 
#   # again, using the same mechanism as before (for filer dob) to deal with errors
#   select(ssn, child_dob = max_dob) %>% 
#   print()

#####

#### the followings have been disregarded because fed data do not have 'better' or 'more' information on child dob
# #####      IV-2: Fed iit data #####
# 
# # aggregating ssn & dep dob from 2006~2017 federal iit
# lls.fed_n_child <- tibble("ssn" = NA) %>% filter(row_number() != 1)
# for(.y in 2006:2017){ cat(crayon::yellow("#####", paste0("fed", .y), "data", "#####", "\n"))
#   lls.fed_n_child <- bind_rows(
#     lls.fed_n_child, 
#     paste0("fed", .y) %>% get %>%
#       select(ssn, fy, contains("dep_")) %>%
#       # remove dependents older than 23 years old, the cutoff is to capture 9 years old in 2003 (2017 23yrs = 2003 9 yrs)
#       mutate_at(vars(-ssn, -fy), ~(ifelse(. < paste0(as.numeric(fy) - 23, 1001), NA, 1))) %>%
#       mutate(n_child = rowSums(.[-1:-2], na.rm = T)) %>%
#       select(ssn, fy, n_child)
#   )
# }
# 
# # are there many errors?
# lls.fed_n_child %>% unique %>% count(ssn, fy) %>% ungroup %>% count(n) %>% print()
# 
# # the aggregate list of filer_dob without errors (take the maximum of n_child)
# lls.fed_n_child <- lls.fed_n_child %>% 
#   group_by(ssn, fy) %>% 
#   summarise(n_child = max(n_child, na.rm = T)) %>% 
#   ungroup() %>% 
#   print()
# 
# # wide list of child_n
# wls.fed_n_child <- lls.fed_n_child %>%
#   spread(key = "fy", value = "n_child", sep = "_") %>% 
#   print()
# 
# # wide list of single child
# wls.fed_sc <- wls.fed_n_child %>% 
#   # only include filers who had a single child at any point
#   semi_join(wls.fed_n_child %>% filter_at(vars(-ssn), any_vars(. == 1)), by = "ssn") %>% 
#   # and exclude filers who had more than 1 child at any point
#   anti_join(wls.fed_n_child %>% filter_at(vars(-ssn), any_vars(.  > 1)), by = "ssn") %>% 
#   print()
# 
# # aggregating ssn & dob of child from 2006~2017 fed iit
# wls.fed_dob <- tibble("ssn" = NA) %>% filter(row_number() != 1)
# for(.y in 2006:2016){ cat(crayon::yellow("#####", paste0("fed", .y), "data", "#####", "\n"))
#   wls.fed_dob <- bind_rows(
#     wls.fed_dob, 
#     paste0("fed", .y) %>% get %>%
#       semi_join(wls.fed_sc, by = "ssn") %>% 
#       select(ssn, fy, contains("dep_")) %>%
#       # remove adult dependents
#       mutate_at(vars(-ssn, -fy), ~(ifelse(. < paste0(as.numeric(fy) - 23, 1001), NA, .)))
#   )
# }
# 
# lls.fed_dob <- wls.fed_dob %>%
#   filter_at(vars(-ssn, -fy), any_vars(!is.na(.))) %>% 
#   mutate(child_dob = rowSums(.[-1:-2] %>% mutate_if(is.character, as.numeric), na.rm = T)) %>% 
#   select(ssn, fy, child_dob) %>% 
#   print()
# 
# # Any technical coding errors?
# lls.fed_dob %>% filter(child_dob > 30000000) %>% print()
# 
# # Any data entry errors?
# lls.fed_dob %>%
#   group_by(ssn) %>% 
#   summarise(min_dob = min(child_dob, na.rm = T), max_dob = max(child_dob, na.rm = T)) %>% 
#   mutate(error = max_dob - min_dob) %>%
#   mutate(error = ifelse(error == 0, 0, ifelse(error < 10000, 1, 2))) %>% 
#   count(error) %>% 
#   print()
# cat(crayon::green("1 represents a simple mistake 2 represents a seemingly odd data entry", "\n"))
# 
# # list of the dob of a single child filers without errors
# lls.fedsc_dob <- lls.dc_dob %>%
#   group_by(ssn) %>% 
#   summarise(min_dob = min(child_dob, na.rm = T), max_dob = max(child_dob, na.rm = T)) %>% 
#   mutate(error = max_dob - min_dob) %>%
#   filter(error < 1999) %>% 
#   select(ssn, child_dob = max_dob) %>% 
#   print()
# 
# #####
# #####      IV-3: DC + Fed #####
# 
# # there are no errors!
# bind_rows(lls.dcsc_dob, lls.fedsc_dob) %>% 
#   group_by(ssn) %>% 
#   summarise(min_dob = min(child_dob, na.rm = T), max_dob = max(child_dob, na.rm = T)) %>% 
#   mutate(error = max_dob - min_dob) %>% 
#   filter(error != 0) %>% 
#   print()
# 
# # final list of child_dob
# lls.child_dob <- bind_rows(lls.dcsc_dob, lls.fedsc_dob) %>% unique() %>% print()
# 
# rm(... = lls.dc_n_child, lls.dcsc_dob, lls.dc_dob, wls.dc_n_child, wls.dc_sc, wls.dc_dob,
#    lls.fed_n_child, lls.fedsc_dob, lls.fed_dob, wls.fed_n_child, wls.fed_sc, wls.fed_dob)
# 
# #####
# ##### part V   : interpolation #####
# 
# # load the ssn, fiscal year, residency data and choose the relevant years
# wls.itp_b <- wls.resid %>%
#   inner_join(lls.child_dob, by = "ssn") %>%
#   select(ssn, child_dob, fy_2001:fy_2016) %>%
#   print()
# 
# # make a long list of flags, which represents if they show up in certain year, also create variables that indicate whether or not
# # the filer was in the city in 2 years before and 2 years after
# lls.itp_flag <- wls.itp_b %>%
#   gather(key = "fy", value = "flag", starts_with("fy_")) %>%
#   mutate(fy = str_sub(fy, -4)) %>%
#   mutate(sch_yr = str_sub(child_dob, -4)) %>%
#   mutate(sch_yr = ifelse(sch_yr < "0930", 0, 1)) %>%
#   mutate(grade = as.numeric(fy) - as.numeric(str_sub(child_dob, 1, 4)) - 1 + as.numeric(sch_yr)) %>%
#   group_by(ssn) %>%
#   mutate(prev3 = lag(flag, n = 3L)) %>%
#   mutate(prev2 = lag(flag, n = 2L)) %>%
#   mutate(prev1 = lag(flag, n = 1L)) %>%
#   mutate(next1 = lead(flag, n = 1L)) %>%
#   mutate(next2 = lead(flag, n = 2L)) %>%
#   mutate(next3 = lead(flag, n = 3L)) %>%
#   ungroup %>%
#   print()
# 
# # interpolate the data points if the filer is only missing when
# # (1) the filer has given a birth to a child recently (age 0-2) and
# # (2) the filer appears +1, +2, -1, -2 years
# lls.itp_manip <- lls.itp_flag %>%
#   filter(flag == 0) %>%
#   filter(-1 < grade & grade < 3) %>%
#   filter(prev1 == 1, next1 == 1) %>%
#   filter(prev2 == 1 | is.na(prev2), prev3 == 1 | is.na(prev3)) %>%
#   filter(next2 == 1 | is.na(next2), next3 == 1 | is.na(next3)) %>%
#   mutate(flag = 1) %>%
#   print()
# 
# # number of interpolations (manipulations) made
# cat(crayon::green(summarise(lls.itp_manip, n())[[1]], "data points have been interpolated, i.e. flag value was changed from 0 to 1", "\n"))
# 
# # create wide df, interpolated, of filer's residency
# wls.itp_a <- lls.itp_flag %>%
#   anti_join(lls.itp_manip, by = c("ssn", "fy")) %>%
#   bind_rows(lls.itp_manip) %>%
#   select(-sch_yr, -grade, -prev3:-next3) %>%
#   spread(key = "fy", value = "flag", sep = "_") %>%
#   print()
# 
# # interpolated df to test
# lls.resid_a <- wls.itp_a %>%
#   gather(key = "fy", value = "flag", starts_with("fy_")) %>%
#   mutate(fy = str_sub(fy, -4)) %>%
#   group_by(ssn) %>%
#   mutate(prev3 = lag(flag, n = 3L)) %>%
#   mutate(prev2 = lag(flag, n = 2L)) %>%
#   mutate(prev1 = lag(flag, n = 1L)) %>%
#   mutate(next1 = lead(flag, n = 1L)) %>%
#   mutate(next2 = lead(flag, n = 2L)) %>%
#   mutate(next3 = lead(flag, n = 3L)) %>%
#   ungroup %>%
#   print()
# 
# # tracking filers who resided for 5 years
# lls.resid_a %>% filter(flag == 1, prev2 == 1, prev1 == 1, next1 == 1, next2 == 1) %>% count(cohort = fy, name = "size") %>%
#   t() %>% as.data.frame(optional = T) %>% print.data.frame()
# 
# lls.cohort_a <- lls.resid_a %>%
#   filter(flag == 1, prev2 == 1, prev1 == 1, next1 == 1, next2 == 1) %>%
#   mutate(sch_yr = str_sub(child_dob, -4)) %>%
#   mutate(sch_yr = ifelse(sch_yr < "0930", 0, 1)) %>%
#   mutate(grade = as.numeric(fy) - as.numeric(str_sub(child_dob, 1, 4)) - 1 + as.numeric(sch_yr)) %>%
#   select(-flag, -prev3:-next3, -sch_yr, cohort = fy) %>%
#   print()
# 
# # sample size
# cat(crayon::green("group size by cohorts", "\n"))
# smpl_n_a <- inner_join(
#   lls.cohort_a %>% filter(grade == 3) %>% count(cohort, name = "treatment"),
#   lls.cohort_a %>% filter(grade == 7 | grade == 8 | grade == 9) %>% count(cohort, name = "control"),
#   by = "cohort") %>% print.data.frame(row.names = F)
# 
# # save df before interpolation as well
# lls.resid_b <- wls.itp_b %>%
#   gather(key = "fy", value = "flag", starts_with("fy_")) %>%
#   mutate(fy = str_sub(fy, -4)) %>%
#   group_by(ssn) %>%
#   mutate(prev3 = lag(flag, n = 3L)) %>%
#   mutate(prev2 = lag(flag, n = 2L)) %>%
#   mutate(prev1 = lag(flag, n = 1L)) %>%
#   mutate(next1 = lead(flag, n = 1L)) %>%
#   mutate(next2 = lead(flag, n = 2L)) %>%
#   mutate(next3 = lead(flag, n = 3L)) %>%
#   ungroup %>%
#   print()
# 
# # tracking filers who resided for 5 years
# lls.resid_b %>% filter(flag == 1, prev2 == 1, prev1 == 1, next1 == 1, next2 == 1) %>% count(cohort = fy, name = "size") %>%
#   t() %>% as.data.frame(optional = T) %>% print.data.frame()
# 
# lls.cohort_b <- lls.resid_b %>%
#   filter(flag == 1, prev2 == 1, prev1 == 1, next1 == 1, next2 == 1) %>%
#   mutate(sch_yr = str_sub(child_dob, -4)) %>%
#   mutate(sch_yr = ifelse(sch_yr < "0930", 0, 1)) %>%
#   mutate(grade = as.numeric(fy) - as.numeric(str_sub(child_dob, 1, 4)) - 1 + as.numeric(sch_yr)) %>%
#   select(-flag, -prev3:-next3, -sch_yr, cohort = fy) %>%
#   print()
# 
# # treatment group size
# cat(crayon::green("group size by cohorts", "\n"))
# smpl_n_b <- inner_join(
#   lls.cohort_b %>% filter(grade == 3) %>% count(cohort, name = "treatment"),
#   lls.cohort_b %>% filter(grade == 7 | grade == 8 | grade == 9) %>% count(cohort, name = "control"),
#   by = "cohort") %>% print.data.frame(row.names = F)
# 
# # rm(... = wls.resid, lls.itp_flag, lls.itp_manip)
# # wls.itp_a, wls.itp_b
# # lls.resid_a, lls.resid_b
# 
# #####

##### part VI  : ward restoration #####

# 2010 Census tracts: https://opendata.dc.gov/datasets/6969dd63c5cb4d6aa32f15effb8311f3_8
tracts10 <- sf::st_read('https://opendata.arcgis.com/datasets/6969dd63c5cb4d6aa32f15effb8311f3_8.geojson', quiet = T) %>% 
  select(census_t = TRACT, GEOID) %>% 
  mutate_if(is.factor, as.character)

# aggregating ward & tracts from 2001~2016 dc iit
lls.geo <- tibble("ward" = NA, "census_t" = NA) %>% filter(row_number() != 1) 
for(.y in 2001:2016){ cat(crayon::yellow("#####", paste0("dc", .y), "#####", "\n"))
  lls.geo <- bind_rows(lls.geo, paste0("dc", .y) %>% get %>% select(ward, census_t)) }

# how many of them are weird?
lls.geo %>% 
  count(ward, census_t) %>% 
  group_by(census_t) %>% 
  filter(n() != 1) %>% 
  arrange(desc(n), .by_group = T) %>%
  spread(key = "ward", value = "n", sep = "_", fill = "") %>% 
  print.data.frame(row.names = F)

# choose the most frequent ones as the correct one  
sf.ward <- tracts10 %>% 
  left_join(lls.geo %>% 
              count(ward, census_t) %>% 
              group_by(census_t) %>% 
              arrange(desc(n), .by_group = T) %>%
              filter(row_number() == 1) %>% 
              select(-n), by = "census_t") %>% 
  print()

rm(... = tracts10, lls.geo)

#####

##### part VII : restoring & saving restored geocoded 'single child' dc iit data ####

# save lists
for (.i in grep("dcsc_itp", ls())){
  cat(crayon::yellow("#####", "restored", ls()[.i] %>% str_remove("lls."), "list saved", "#####", "\n"))
  .f <- paste0(ls()[.i] %>% str_replace("[.]", "_"))
  ls()[.i] %>% get %>% write_csv(paste0("IIT DC Geocoded/List/", .f, ".csv")) 
}
sf::write_sf(sf.ward, paste0("IIT DC Geocoded/List/", "sf_ward", ".shp"))

# load lists
for(.f in list.files(path = "./IIT DC Geocoded/List", pattern = "ls")) { cat(crayon::yellow("#####", "load", .f, "#####", "\n"))
  assign(.f %>% str_remove(".csv") %>% str_replace("[_]", "."), read_csv(paste0("IIT DC Geocoded/List/", .f), col_types = cols(.default = "c"))) }

# restore data
for(.y in 2001:2016){
  cat(crayon::yellow("#####", paste0("dc", .y), "data", "restored", "#####", "\n"))
  assign(paste0("res", .y), 
         paste0("dc", .y) %>% get %>% 
           select(-contains("dob"), -ward) %>% 
           left_join(lls.filer_dob, by = "ssn") %>% 
           left_join(lls.dcsc_itp %>% select(ssn, child_dob), by = "ssn") %>% 
           left_join(lls.ward %>% select(ward, census_t), by = "census_t") %>% 
           select(ssn:type, ward, census_t, wage:kid_cd, filer_dob, child_dob) %>% 
           # deal with duplicates
           group_by(ssn) %>% arrange(desc(wage), .by_group = T) %>% filter(row_number() == 1) %>% ungroup
        )
}

# save restored df
for (.y in 2001:2016) { 
  cat(crayon::yellow("#####", "restored", .y, "data", "saved", "#####", "\n"))
  paste0("res", str_sub(.y, -4)) %>% get %>%
    select("Id" = ssn, "Year" = fy, "Number_Of_Months_In_Dc" = stay, "Filing_Status" = type, 
           "Ward" = ward, "Census_Tract" = census_t,
           "Wages_Salaries_Tips" = wage, "Fed_Adjusted_Gross_Income" = adj_inc,
           "Dc_Low_Income_Credit" = low_cd, "Dc_Earned_EITC" = earn_cd, "Dc_Child_Care_Credit" = kid_cd,
           "Filer_DOB" = filer_dob, "Child_DOB" = child_dob) %>%
    write_csv(paste0("IIT DC Geocoded/dc_res_iit_", .y, ".csv"), na = "")
}

# rm(list = ls())

#####

####################
##### analysis #####
####################

##### load final data #####

res_readdta <- function(year){
  mylist <- list.files(path = "./IIT DC Geocoded/", pattern = "\\<dc_res_iit_")
  myfile <- paste0("IIT DC Geocoded/", mylist[grep(year, mylist)])
  read_csv(myfile, col_types = cols(.default = "c")) %>% 
    mutate(Wages_Salaries_Tips = as.numeric(Wages_Salaries_Tips),
           Fed_Adjusted_Gross_Income = as.numeric(Fed_Adjusted_Gross_Income),
           Dc_Earned_EITC = as.numeric(Dc_Earned_EITC),
           Dc_Low_Income_Credit = as.numeric(Dc_Low_Income_Credit),
           Dc_Child_Care_Credit = as.numeric(Dc_Child_Care_Credit),
           Child_DOB = as.numeric(Child_DOB)
           ) %>% 
    select(ssn = "Id", fy = "Year", stay = "Number_Of_Months_In_Dc", type = "Filing_Status", 
           ward = "Ward", census_t = "Census_Tract",
           wage = "Wages_Salaries_Tips", adj_inc = "Fed_Adjusted_Gross_Income",
           low_cd = "Dc_Low_Income_Credit", earn_cd = "Dc_Earned_EITC", kid_cd = "Dc_Child_Care_Credit",
           filer_dob = "Filer_DOB", child_dob = "Child_DOB")
}

for(.y in 2001:2016){ cat(crayon::yellow("#####", "load restored geocoded single child dc iit", .y, "data", "#####", "\n"))
  assign(paste0("res", .y), res_readdta(.y)) }

for(.f in list.files(path = "./IIT DC Geocoded/List", pattern = "dob")) { cat(crayon::yellow("#####", "load", .f, "#####", "\n"))
  assign(.f %>% str_remove(".csv"), read_csv(paste0("IIT DC Geocoded/List/", .f), col_types = cols(.default = "c"))) }

#####

##### panel generation #####

sp_panel_gen <- function (baseyear, timespan = -2:2, balanced = T, info = TRUE) {
  
  lls.dcsc_itp <- read_csv("IIT DC Geocoded/List/lls_dcsc_itp.csv", col_types = cols(.default = "c")) %>% 
    mutate_at(vars(-ssn), ~(as.numeric(.)))
  
  ls.join <- lls.dcsc_itp %>% select(ssn)

  # lls.dcsc_itp <- lls.dcsc_itp %>% gather(key = "fy", value = "flag", starts_with("fy_")) %>% mutate(fy = str_sub(fy, -4) %>% as.numeric())
  # 
  # lls.join <- lls.dcsc_itp %>% 
  #   filter(flag == 1) %>% 
  #   filter(fy == baseyear) %>% 
  #   select(ssn, fy)
  
  # create datalist for 5 year span
  ls.fy <- sapply(timespan, function(x) baseyear + x)
  ls.dta <- sapply(timespan, function(x) paste0("res", baseyear + x))
  
  # # interpolate data?
  # if (interpolate == T) {
  #   cohort_gen <- lls_cohort_a
  # } else if (interpolate == F) {
  #   cohort_gen <- lls_cohort_b
  # }
  # 
  # cohort_gen <- cohort_gen %>% select(ssn, grade, cohort)

  panel.dta <- bind_rows(
    ls.dta[1] %>% get %>%
      full_join(ls.join, by = "ssn"),
    ls.dta[2] %>% get %>%
      full_join(ls.join, by = "ssn"),
    ls.dta[3] %>% get %>%
      full_join(ls.join, by = "ssn"),
    ls.dta[4] %>% get %>%
      full_join(ls.join, by = "ssn"),
    ls.dta[5] %>% get %>%
      full_join(ls.join, by = "ssn"),
  )
  
  panel.dta2 <- panel.dta %>% 
    mutate(age = .y - as.numeric(str_sub(filer_dob, 1, 4))) %>% 
    mutate(age_gr = (age %/% 5 + 1) * 5) %>% 
    mutate(sch_yr = str_sub(child_dob, -4)) %>% 
    mutate(sch_yr = ifelse(sch_yr < "0930", 0, 1)) %>% 
    arrange(ssn, fy) %>% 
    mutate_at(vars(stay, type, ward, census_t, age, child_dob, age_gr), 
              ~(ifelse(is.na(.) == T, lag(.), .))) %>% 
    mutate(grade = as.numeric(fy) - as.numeric(str_sub(child_dob, 1, 4)) - 1 + as.numeric(sch_yr)) %>% 
    filter(0 < grade, grade < 14)
  
  if (balanced == T) {
    panel.dta3 <- panel.dta2 %>% group_by(ssn) %>% filter(n() == 5) %>% ungroup
  } else if (interpolate == F) {
    panel.dta3 <- panel.dta2
  }
  
  if (info == T) {
    sample_size <- panel.dta3 %>% 
      select(-age) %>% 
      group_by(age = grade, fy) %>% 
      summarise(sample_n = n()) %>% 
      spread(key = "age", value = "sample_n", sep = "_") %>% 
      ungroup
    
    cat(crayon::silver("\n#######################################################################################\n"))
    cat(crayon::green("5-Year Panel Data for", baseyear, "Cohorts", "\n", crayon::cyan(": sample size\n")))
    panel.dta %>% count(ssn, name = "balanced") %>% count(balanced, name = "observations") %>% print.data.frame(row.names = F)
    print.data.frame(sample_size, row.names = F)
    cat(crayon::silver("#######################################################################################\n\n"))
  }

  return(invisible(panel.dta))
}

for(.y in 2003:2014) assign(paste0("sp", .y), sp_panel_gen(.y))



