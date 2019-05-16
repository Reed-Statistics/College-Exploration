# This file includes our procedure to query, transform, and tidy the
# IPEDS data, and then join it to the mobility report card data.

# We first need to install the development version of the
# package from github (the CRAN version is out of date)
#install_github("ipeds", "jbryer")

# Load the package
library(ipeds)
library(readxl)
library(fuzzyjoin)
library(tidyverse)

# The package includes a dataset with the names of datasets that
# can be queried--take a look at this to get an idea of our
# options of what we can download
data(surveys)
#View(surveys)

# The SurveyID column gives us options of parameters for the
# getIPEDSSurvey() function. year is the only other parameter.

# An example call--grabbing the Institutional Characteristics
# dataset from 2009
hd_2009 <- getIPEDSSurvey("HD", 2009)

# surveys$SurveyID is a list of all the files we want to download,
# so we can lapply over all of them and then outer_join to end up
# with a dataset that includes all variables for all colleges.
# Then, we manually look over the data dictionary, choosing 40-80 variables
# that seem most important and creating our own, Shiny-app ready
# codebook. Notes from that procedure are included here:
# https://www.google.com/url?q=https://docs.google.com/spreadsheets/d/1faKL5jnrqPrR-LFPYc1GiqalItODRYZVdYm4qDU3Mbo/edit?usp%3Dsharing_eip%26ts%3D5c5e0ea4&source=gmail&ust=1549754411096000&usg=AFQjCNHqgnWuo960gW-76jQ6KHeM7vJPAw

# A link to the dictionary files:
# https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx
# filter by 2016 and click the dictionary

# The `ipeds` package is not well-maintained but provides functions that
# will be very useful for the group. Essentially, these functions rely on
# the "surveys" dataset being loaded into the enviroment, but this dataset
# is out of date. We recreate the "surveys" data with updated SurveyIDs using
# a table provided by IPEDS at 
# https://nces.ed.gov/ipeds/use-the-data/download-access-database
# and adapt some of the functions in the repo  to better fit our needs

# store the basic query url as a variable
ipedsDataUrl <- 'http://nces.ed.gov/ipeds/datacenter/data/'

# The following two functions have been adapted from commit 7bf7162 from
# https://github.com/jbryer/ipeds/blob/master/R/getIPEDSSurvey.R

# This function takes in the name of a dataset, queries the IPEDS
# database, saves a copy of the dataset returned, and then returns
# the dataset. It is a wrapper function for `downloadIPEDSSurvey`
# with more informative error/warning messages. This function is 
# generally left as provided, except the surveyId and year arguments 
# have been merged and the filepaths have been changed slightly
getIPEDSSurvey <- function(surveyId, dir= 'data',
                           stringsAsFactors=FALSE, ...) {
  s = surveys[which(tables_16$surveyID==surveyId),]
  if(nrow(s) != 1) {
    stop(paste('IPEDS survey with id', surveyId, 'not found'))
  }
  file = surveyId
  dest = paste(dir, "/ipeds_source/", file, '.csv', sep="")
  if(!file.exists(dest)) {
    r = try(downloadIPEDSSurvey(surveyId, dir=dir), FALSE)
    if(class(r) == "try-error") {
      r = NULL
    }
  } else {
    r = read.csv(dest, stringsAsFactors=stringsAsFactors, ...)
  }
  if(!is.null(r)) {
    names(r) = tolower(names(r))
  }
  r
}

# This function takes in the name of a dataset, queries the IPEDS
# database, saves a copy of the dataset returned, and then returns
# the dataset. This function is a bit less friendly to play with
# than getIPEDSSurvey. This function is generally left as provided,
# except the surveyId and year arguments have been merged and some
# filepaths were changed.
downloadIPEDSSurvey <- function(surveyId, dir= 'data',
                                stringsAsFactors=FALSE, ...) {
  s = surveys[which(tables_16$surveyID==surveyId),]
  #dir = system.file(package="ipeds")
  file = surveyId
  url = paste(ipedsDataUrl, file, '.zip', sep='')
  dir.create(paste(dir, '/ipeds_source/', sep=''), showWarnings=FALSE)
  dest = paste(dir, "/ipeds_source/", file, '.zip', sep="")
  download.file(url, dest, mode="wb")
  unzip(dest, exdir=paste(dir, "/ipeds_source", sep=""))
  unlink(dest)
  fname <- paste(dir, "/ipeds_source/", file, ".csv", sep="")
  if(!file.exists(fname)) {
    # Check to see if the filename is in lowercase
    fname <- paste(dir, "/ipeds_source/", tolower(file), ".csv", sep="")
  }
  r = read.csv(fname, stringsAsFactors=stringsAsFactors, ...)
  return(r)
}

# Read in the appropriate table
tables_16 <- read_excel("data/ipeds/tables_16.xlsx", 
                        sheet = "Tables16")

# We want this table to look as much like "surveys" as possible. However,
# we don't want to split up the TableName column as done in the original
# package, because we're redefining the functions to take in a parameter that
# supplies both of the year and table name.
tables_16 <- tables_16 %>%
  select(surveyID = TableName, Survey = Survey, Title = TableTitle)

# run the querying function on every survey released in 2016
# NOTE: Even with a fast internet connection, this will
# take several minutes to run. The resulting file
# is over half a gigabyte
surveys_2016 <- lapply(tables_16$surveyID, getIPEDSSurvey)

# save the surveys_2016 object--I won't push the result because it's 500MB
save(surveys_2016, file = "data/ipeds_source/surveys_2016.Rda")

# We now import the `varnames.csv` dataframe which contains the subset
# of variables that we've manually chosen as ones we're interested in
varnames <- read_csv("data/varnames.csv")

# Specifically, we're interested in the subsurvey and lower_varname columns.
# The subsurvey column will help us identify an element (dataframe) in the
# `surveys_2016` list, and the associated lower_varname entry is the variable
# in that dataframe we're interested in.

# each of the dataframes we're pulling from
relevant_tables <- unique(varnames$subsurvey)

# We would like a list where each element is a list with the name of the 
# dataset in the first entry and the names of the variables we want from that
# dataset in the second entry
relevant_tables_and_vars <- list()

for (i in 1:length(relevant_tables)) {
  relevant_tables_and_vars[[i]] <- list()
  # put the name of the table in the first entry
  relevant_tables_and_vars[[i]][[1]] <- relevant_tables[i]
  # pull out all of the relevant variables and put them in the second entry
  relevant_tables_and_vars[[i]][[2]] <- c("unitid", varnames$lower_varname[varnames$subsurvey == relevant_tables[i]])
}

# The following gives the indices in the surveys_2016 list of the tables we want
table_indices <- match(relevant_tables, tables_16$surveyID)

# Make a dataframe that has both the id and index
survey_id_and_index <- data_frame(relevant_tables, table_indices)

# A few of the tables we're interested in have several rows for every unique 
# school id.. using the codebooks, we select the aggregate statistics
# from each dataset to reduce down to one row per school
surveys_2016[[44]] <- surveys_2016[[44]] %>%
  filter(facstat == 0)

surveys_2016[[14]] <- surveys_2016[[14]] %>%
  filter(efalevel == 1)

surveys_2016[[29]] <- surveys_2016[[29]] %>%
  filter(grtype == 2)

surveys_2016[[24]] <- surveys_2016[[24]] %>%
  filter(cipcode == 99)

# We are ready to join the variables from these datasets.

# First, make a list similar to surveys_2016 with only the unitid and the
# variables we want selected
master_colleges_list <- list()

for (i in 1:length(relevant_tables_and_vars)) {
  # Pull out the relevant dataframe from surveys_2016
  master_colleges_list[[i]] <- surveys_2016[[survey_id_and_index$table_indices[i]]] %>%
    # Select only the variables we want
    select(relevant_tables_and_vars[[i]][[2]])
}

# Save the master_colleges_list object before we move on so that
# we can confirm that we've joined accurately later
save(master_colleges_list, file = "data/master_colleges_list.Rda")

# This call will iteratively full_join each dataset in the list
colleges_raw <- Reduce(function(x, y) {full_join(x, y, by = "unitid")}, 
                       master_colleges_list) #%>%
# get rid of variables used to normalize data to first normal form
#select(-facstat, -grtype)

# We have our data!
str(colleges_raw)

# Save the data
save(colleges_raw, file = "shiny/colleges_raw.Rda")

colleges <- colleges_raw

# standardize missingness
colleges$hloffer[colleges$hloffer == -3] <- NA
colleges$locale[colleges$locale == -3] <- NA
colleges$instsize[colleges$instsize < 0 ] <- NA
colleges$roomcap[colleges$roomcap == "." ] <- NA
colleges$rmbrdamt[colleges$rmbrdamt == "." ] <- NA
colleges$tuition2[colleges$tuition2 == "." ] <- NA
colleges$tuition3[colleges$tuition3 == "." ] <- NA
colleges <- colleges %>%
  mutate(relaffil = ifelse(relaffil == -2, 0,
                           ifelse(relaffil == -1, NA, 1)),
         ft_ug = ifelse(ft_ug == 1, 1,
                        ifelse(ft_ug == 2, 0, NA)),
         ftgdnidp = ifelse(ftgdnidp == 1, 1,
                           ifelse(ftgdnidp == 2, 0, NA)),
         pt_ug = ifelse(pt_ug == 1, 1,
                        ifelse(pt_ug == 2, 0, NA)),
         ptgdnidp = ifelse(ptgdnidp == 1, 1,
                           ifelse(ptgdnidp == 2, 0, NA)),
         athassoc = ifelse(athassoc == 1, 1,
                           ifelse(athassoc == 2, 0, "Yes")),
         assoc1 = ifelse(assoc1 == 1, 1,
                         ifelse(assoc1 == 0, 0, NA)))

# these cover variables 1-33 (up through ret_pcf)


# clean 34-64

# confirm 35-40 works, could update to be name-based for these features
# standardize NA values to 0, since dormsize = NA <-> dormsize = 0
for (i in 35:40) {
  colleges[i][is.na(colleges[i])] <- 0
  #colleges[i][colleges[i] == "."] <- 0
}

# if both # of faculty and act scores are missing, get rid of the row
colleges <- colleges %>%
  filter(!is.na(sistotl) & !is.na(actcm25))

# reduce missingness by iteratively dropping rows and columns with
# certain percentages of missingness
cut_missingness <- function(df, col_pct, row_pct) {
  # we want to check, per column, how many observations are missing
  pct_missing_cols <- colSums(is.na(df))/nrow(df)
  
  # drop columns with col_pct or more missing
  df <- df[pct_missing_cols <= col_pct]
  
  # we want to check, per row, how many observations are missing
  pct_missing_rows <- rowSums(is.na(df))/ncol(df)
  
  # drop schools with row_pct or more missing
  df <- df[pct_missing_rows <= row_pct,]
  
  df
}

# run the function with lenient missingness standards
colleges <- cut_missingness(colleges, .95, .95)


# "clean up the dataset"--make more informative names
colleges <- colleges %>%
  # derive new variables scaled to student populations
  mutate(gr_prop_male = grtotlm / eftotlt,
         gr_prop_fem = grtotlw / eftotlt,
         prop_admit = admssn / eftotlt,
         prof_total = sisprof + sisascp + sisastp,
         instr_total = sisinst + sislect,
         prop_prof = prof_total / eftotlt,
         prop_instr = instr_total / eftotlt,
         prop_alien = efnralt / eftotlt) %>%
  # get rid of unscaled variables
  #  select(-c("chrtstat", "section", "grtotlm", "grtotlt", 
  #            "grtotlw", "admssn", "applcn", 
  #            "sisprof", "sisascp", "sisastp", 
  #            "efnralt", "cohort", "line")) %>%
  # cut down variables and rename others
  select(school_id = unitid,
         school_name = instnm,
         state = stabbr,
         highest_degree = hloffer,
         urban = locale,
         school_size = instsize,
         longitude = longitud,
         latitude = latitude,
         rel_affil = relaffil,
         full_time_undergrad = ft_ug,
         part_time_grad = ftgdnidp,
         part_time_undergrad = pt_ug,
         part_time_grad_prof = ptgdnidp,
         dorm_capacity = roomcap,
         room_board = rmbrdamt,
         NAA_member = athassoc,
         NCAA_member = assoc1,
         in_state_tuition = tuition2,
         out_state_tuition = tuition3,
         total_enrolled = eftotlt,
         male_enrolled = eftotlm,
         female_enrolled = eftotlw,
         native_enrolled = efaiant,
         asian_enrolled = efasiat,
         black_enrolled = efbkaat,
         hispanic_enrolled = efhispt,
         pacific_enrolled = efnhpit,
         white_enrolled = efwhitt,
         mixed_enrolled = ef2mort,
         retention_rate = ret_pcf,
         student_faculty_ratio = stufacr,
         percent_out_state = scfa13p,
         gr_prop_male,
         gr_prop_fem,
         prop_admit,
         prop_prof,
         prop_instr,
         pct_fin_aid = uagrntp,
         avg_fin_aid = uagrnta,
         first_years_enrolled = enrlt, # make into a prop
         act_25 = actcm25,
         act_75 = actcm75,
         total_programs = ptotal,
         total_assoc = passoc,
         total_bachelor = pbachl,
         total_masters = pmastr,
         total_doc_research = pdocrs,
         total_doc_prof = pdocpp,
         control,
         carnegie) %>%
  # scale enrollment variables
  mutate(prop_female = female_enrolled / total_enrolled,
         prop_male = male_enrolled / total_enrolled,
         prop_native = native_enrolled / total_enrolled, 
         prop_black = black_enrolled / total_enrolled,
         prop_asian = asian_enrolled / total_enrolled,
         prop_pacific = pacific_enrolled / total_enrolled,
         prop_white = white_enrolled / total_enrolled,
         prop_mixed = mixed_enrolled / total_enrolled)
# get rid of unscaled variables
#  select(-c("female_enrolled", "male_enrolled",
#            "native_enrolled", "black_enrolled",
#            "asian_enrolled", "pacific_enrolled",
#            "white_enrolled", "mixed_enrolled"))


colleges$unitid <- as.character(colleges$unitid)
colleges$highest_degree <- as.factor(colleges$highest_degree)
colleges$urban <- as.factor(colleges$urban)
colleges$school_size <- as.factor(colleges$school_size)
colleges$full_time_undergrad <- as.logical(colleges$full_time_undergrad)
colleges$part_time_grad <- as.logical(colleges$part_time_grad)
colleges$part_time_undergrad <- as.logical(colleges$part_time_undergrad)
colleges$part_time_grad_prof <- as.logical(colleges$part_time_grad_prof)
colleges$dorm_capacity <- as.numeric(colleges$dorm_capacity)
colleges$room_board <- as.numeric(colleges$room_board)
colleges$NAA_member <- as.logical(colleges$NAA_member)
colleges$NCAA_member <- as.logical(colleges$NCAA_member)
colleges$in_state_tuition <- as.numeric(colleges$in_state_tuition)
colleges$out_state_tuition <- as.numeric(colleges$out_state_tuition)
colleges$control <- as.factor(colleges$control)
colleges$carnegie <- as.factor(colleges$carnegie)

df <- colleges

#############
# Count NA in variables
isna <- apply(df, 2, is.na)

tots <- apply(isna, 2, sum)

tots

#############
# Impute missing numeric values by median of present cases for that feature

median_impute <- function(df) {
  
  for (i in 4:dim(df)[[2]]) {
    df[[i]] <- as.numeric(df[[i]])
    df[[i]][is.na(df[[i]])] <- median(df[[i]][!is.na(df[[i]])])
  }
  
  df
  
}

df <- median_impute(df)

isna <- apply(df, 2, is.na)

tots <- apply(isna, 2, sum)

tots

imputed_colleges <- df

# load in the opportunity insights data
mrc <- read_csv("data/mrc_table1.csv")

# load in imputed_colleges.Rda
#load("data/ipeds/colleges/colleges.Rda")
colleges <- imputed_colleges

# join the datasets based on an approximate match of the 
# institution name column using a conservative max distance
# so that nrow(colleges) = nrow(colleges_mrc), though some of
# the entries in `colleges_mrc` are missing.
colleges_mrc <- stringdist_join(colleges, 
                                mrc, 
                                by = c(school_name = "name"),
                                mode = "left",
                                method = "osa",
                                max_dist = .5,
                                ignore_case = TRUE)

# remove mcr columns that we don't need
colleges_mrc <- colleges_mrc %>%
  mutate(state = state.x) %>%
  select(-c("super_opeid", "name", "czname", "state.x", "state.y"))

save(colleges_mrc, file = "data/colleges_mrc.Rda")
