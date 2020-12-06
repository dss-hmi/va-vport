# This scripts develops and tests the function for detecting trajectories of therapy engagments

rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console
# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library("ggplot2") # graphing
library("tidyr") # data manipulation
library("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a 

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
ds <- tibble::tribble(
  ~id, ~tx, ~date,~target,
  1,"CPT","2019-01-01",1,
  1,"CPT","2019-02-01",1,
  1,"CPT","2019-02-13",2,
  1,"CPT","2019-02-25",3,
  1,"CPT","2019-03-20",1,
  1,"CPT","2019-03-30",2,
  2,"ACT","2019-01-01",1,
  2,"ACT","2019-03-01",1,
  2,"ACT","2019-05-01",1,
  2,"ACT","2019-05-12",2,
  )

ds <- ds %>% 
  mutate(
    tx = factor(tx)
    ,date = as.Date(date)
  )
ds %>% glimpse()
# ---- inspect-data -------------------------------------------------------------

# ---- tweak-data -------------------------------------------------
minint <- 14
ds1 <- ds %>% 
  group_by(id,tx) %>% 
  mutate(
    tx_sn = row_number(date)
    ,tsp = as.integer(date - lag(date))
    ,ttn = as.integer( lead(date) - date)
    ,first = (tsp > minint) | (is.na(tsp))
    ,last = (ttn > minint) | (is.na(ttn))
    ,fd = case_when(first ~ date)
  ) %>%  
  fill(fd) %>% 
  group_by(id,tx,fd) %>%
  mutate(
    seq_num = row_number()
  ) %>% 
  group_by(id,tx,fd) %>% 
  mutate(
    ld = max(date,na.rm =T)
  ) %>% 
  ungroup()


ds2 <- ds1 %>% 
  left_join(
    ds1 %>% 
      group_by(id,tx,fd) %>% 
      distinct(id,tx,fd) %>% 
      group_by(id)
      mutate()
      
  )
ds1

# ---- task-2 ------------------------
# task: create a variable that counts the sequential number of distinct treatments within each person (new variable must match the target)
d1 <- tibble::tribble(
  ~person_id, ~tx,~target,
  1, "A",1,
  1, "A",1,
  1, "B",2,
  1, "B",2,
  2, "A",1,
  2, "A",1,
  2, "B",2,
  2, "B",2,
  2, "B",2,
  2, "C",3,
  2, "C",3,
)
d1

# suboptimal implementaion:
d2 <- d1 %>% 
  group_by(person_id,tx) %>% 
  distinct(person_id,tx) %>%
  group_by(person_id) %>% 
  mutate(
    tx_number = row_number()
  )
d2

d3 <-d1 %>% 
  left_join(d2)
d3

# Single dplyr step solution

d4 <- d1 %>% 
  group_by(person_id, tx) %>% 
  mutate(
    group_mark = row_number()==1L
  ) %>% 
  group_by(person_id) %>% 
  mutate(
    group_counter = cumsum(group_mark)
  ) %>% 
  ungroup()
d4
cumsum(!(d4$target==d4$group_counter))
# ---- basic-graph -------------------------


