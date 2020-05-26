#install.packages("rvest")
#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("purrr")
#install.packages("tibble")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("XML")

library(rvest)
library(tidyverse)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)

rm(list=ls())
#set locality to avoid invalid multibyte character errors
Sys.setlocale("LC_CTYPE", "C")
#Sys.getlocale()

www <- 'http://cherryblossom.org/'
url <- paste0(www, 'results/2012/2012cucb10m-m.htm')
doc <- read_html(url)
years <- 1999:2012

# We need a function to read the URLs, read the the table nodes marked with 'pre', 
# then split the strings based on new lines. This will need to be adjusted between
# male and female, and also for some years

# List of the male URLS
male_urls <- c(
  'results/1999/cb99m.html',
  'results/2000/Cb003m.htm',
  'results/2001/oof_m.html',
  'results/2002/oofm.htm',
  'results/2003/CB03-M.HTM',
  'results/2004/men.htm',
  'results/2005/CB05-M.htm',
  'results/2006/men.htm',
  'results/2007/men.htm',
  'results/2008/men.htm',
  'results/2009/09cucb-M.htm',
  'results/2010/2010cucb10m-m.htm',
  'results/2011/2011cucb10m-m.htm',
  'results/2012/2012cucb10m-m.htm'
)

#List of female URLS
female_urls <- c(
  'results/1999/cb99f.html',
  'results/2000/Cb003f.htm',
  'results/2001/oof_f.html',
  'results/2002/ooff.htm',
  'results/2003/CB03-F.HTM',
  'results/2004/women.htm',
  'results/2005/CB05-F.htm',
  'results/2006/women.htm',
  'results/2007/women.htm',
  'results/2008/women.htm',
  'results/2009/09cucb-F.htm',
  'results/2010/2010cucb10m-f.htm',
  'results/2011/2011cucb10m-f.htm',
  'results/2012/2012cucb10m-f.htm'
)

################################
## FUNCTIONS
################################

#Updating function to recognize 1999 as a new line split.
#Also must change function for 2000 to see the font tag for its data.
extract_res_table <- function(url, year = 2001, female = TRUE) {
  selector <- if (year == 2000) 'font' else 'pre'
  regexp <- if (year == 1999) '\\n' else '\\r\\n'
  
  result <- read_html(url) %>% 
    html_nodes(selector)

  
  if (year == 2000) result <- result[[4]]
  
  result <- result %>% 
    html_text()
  
  if (year == 2009 && female == FALSE) {

    return(result)
  } 
  
  result %>% 
    str_split(regexp) %>% 
    .[[1]]
}

#Implementing column finding function from Nolan and Lang
findColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}

selectCols = function(shortColNames, headerRow, searchLocs) {
  sapply(shortColNames, function(shortName, headerRow, searchLocs){
    
    startPos = regexpr(shortName, headerRow)[[1]]
    
    if (startPos == -1) return( c(NA, NA) )
    
    index = sum(startPos >= searchLocs)
    c(searchLocs[index] + 1, searchLocs[index + 1])
  }, 
  
  headerRow = headerRow, searchLocs = searchLocs )
}

#Modify extract variables function.
extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time"))
  {
    
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data 
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    # Remove footnotes and blank rows
    footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
    if ( length(footnotes) > 0 ) body = body[ -footnotes ]
    blanks = grep("^[[:blank:]]*$", body)
    if (length(blanks) > 0 ) body = body[ -blanks ]
    
    
    # Obtain the starting and ending positions of variables   
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    return(Values)
  }

################################
## Dataframe Construction
################################

#concatenate URLS
male_urls <- paste0(www, male_urls)
female_urls <- paste0(www, female_urls)

#apply the extract_res_table function to the mens URLS and build a list with each row being a string of the year run.
men_tables <- map2(male_urls, years, extract_res_table, female = FALSE)
names(men_tables) <- years
women_tables <- map2(female_urls, years, extract_res_table, female = TRUE)
names(women_tables) <- years

#We need to split on a new line entry for 1999 for men
#men_tables[[1]] <- str_split(men_tables[[1]], '\\n')[[1]]

#Create directories for male and female
dir.create('male')
dir.create('female')

#Write the text files to the directories for each year
walk2(men_tables,
      paste('male', paste(years, 'txt', sep = '.'), sep = '/'),
      writeLines)
walk2(women_tables,
      paste('female', paste(years, 'txt', sep = '.'), sep = '/'),
      writeLines)

#Reads in formatted female text files
wfilenames <- list.files('female', pattern = '.txt$', full.names = TRUE)
female_files <- map(wfilenames, readLines)
names(female_files) <- str_match(wfilenames, 'female/(.*).txt')[ ,2]


#Reads in formatted male text files
mfilenames <- list.files('male', pattern = '.txt$', full.names = TRUE)
male_files <- map(mfilenames, readLines)
names(male_files) <- str_match(mfilenames, 'male/(.*).txt')[ ,2]


#adjusting the import files for header and spacer rows for year = 2001
male_2001 <- male_files$`2001`
female_2001 <- female_files$`2001`

eq_idx_2001 <- str_which(male_2001, '^===')
spacer_row_2001 <- male_2001[eq_idx_2001]
header_row_2001 <- male_2001[eq_idx_2001 - 1] %>% str_to_lower()

female_files$`2001`[2] <- header_row_2001
female_files$`2001`[3] <- spacer_row_2001

#Summary

#male_files <- iconv(male_files, "latin1", "ASCII", sub="")
male_summary <- map(male_files, extractVariables)
female_summary <- map(female_files, extractVariables)

#Format runner ages to numeric 
male_runner_age <- map(male_summary, ~ as.numeric(.x[ ,'ag']))
female_runner_age <- map(female_summary, ~ as.numeric(.x[ ,'ag']))

#graphics
male_runner_age %>% 
  enframe(name = "year", value = "age") %>% 
  unnest(cols =c(age)) %>% 
  filter(age, age > 7) %>% 
  ggplot(aes(year, age)) + geom_boxplot() + ggtitle("Men's Ages 1999-2012")

female_runner_age %>% 
  enframe(name = "year", value = "age") %>% 
  unnest(cols =c(age)) %>% 
  filter(age, age > 7) %>% 
  ggplot(aes(year, age)) + geom_boxplot() + ggtitle("Female's Ages 1999-2012")