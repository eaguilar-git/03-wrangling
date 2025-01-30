#+ Wrangling: tidy variables
#+ Jan 29, 2025

# Setup ----------------
# Packages
  library(tidyverse)
  library(knitr)

# Custom function
  fctr = function(...) {
    args = rlang::list2(...)
    rhs = map(args, rlang::f_rhs)
    cases = case_when( !!!args )
    exec(fct_relevel, cases, !!!rhs)
  }  

# Data
  df <- read_csv('water.csv')

  
# Get the data you want ----------
# Select variables
  df2 <-
    df %>%
    select(
      hh_id, age,   # items I want
      hv201:hv215,  # range of vars I want
      -hv213       # don't want this
    )
  
# Select cases (to keep)
  df3 <-
    df %>%
    filter(
      sex_binary == "male",  # males only -- vertical line | is or and & is and-- , assumes &
      age <= 10  # check: is this a good idea?
    ) 
  
    rm(df2, df3) ## these would delete the 2 dfs i created
    
# mutate basics ------------------  
# Create columns 
    # mutuate creates new columns / adds new variables that we want
  df2 <-
    df %>%
    mutate(
      rural = if_else(hv025 == 'rural', 1, 0),
      age_months = age * 12,
      age_dev = age - median(age, na.rm = T),
      .keep = "none",
      .before = 1,
    )
    
# Place the columns
  df2 <- 
    df %>%
    mutate(
      r_id = row_number(),
      int_year = hv007,
      .before = 1 # use column number or name; also .after
    )
  
# Decide what to keep
  df2 <-
    df %>%
    mutate(
      r_id = row_number(),
      age_yrs = age * 12,
      .keep = 'none' # alts: 'used', 'unused'
    )

  
# NAs and Strings ----------------
# unwanted strings
  df2 <-
    df %>%
    mutate(
      watEx1 = as.numeric(water_mins), # changes strings to NA
      waterFetchMins = case_when(
        water_mins == 'on premises' ~ 0,
        water_mins %in% c('990', "don't know") ~ NA, 
        TRUE ~ as.numeric(water_mins)
      ),
      .keep = 'used'
    )  # review the Warning messages; check: summary(df2$waterFetchMins)

# Combine or split strings
  df2 <-
    df %>%
    mutate(
      r_id = row_number(),
      region_x_rid = paste(region_id, r_id, sep = '_'),
      region2 = str_extract(region_x_rid, "[^_]+"),
      rid2 = str_extract(region_x_rid, "[^_]*$"),
      .keep = 'used'
    )

# Change values to/from missing
  df2 <-
    df %>%
    mutate(
      attend1 = na_if(hv121, '9'),
      attend2 = if_else(hv121 == '9', NA, hv121),
      attend3 = if_else(is.na(attend2), 'dk/nr', attend2),
      .keep = 'used'
    )
  
# Factors and labels -------------  
count(df, hh_income)
  
# sequence a factor variable
  df2 <-
    df %>%
    mutate(
      inc_quint = fct_relevel(
        hh_income, # source variable
        'poorest','poorer', 'middle','richer','richest'
      )
    )
  
# create ordered categories
  df2 <-
    df %>%
    mutate(
      ageFact = fctr(
        age <= 9 ~ '6-9 years',
        age %in% 10:12 ~ '10-12',
        age >= 13 ~ '13-15' # vs TRUE
      ),
      .keep = 'used'
    )
  
# Repeat across columns ----------  
# Mutate across
  df2 <-
    df %>%
    mutate(
      across(where(is.character), ~na_if(.x, '9')) ### when you want everything to be numeric!
    )
  ## look into what .x does
  
# Grouping -----------------------
# Aggregate: group and summarize
  df2 <-
    df %>%
    group_by(hh_income) %>%
    summarize(
      mean_edu = mean(eduyrs, na.rm = T)
    )
  
# Group level stats: group and mutate
  df2 <-
    df %>%
    group_by(hh_income) %>%
    mutate(
      mean_edu = mean(eduyrs, na.rm = T)
    ) %>%
    ungroup()
  