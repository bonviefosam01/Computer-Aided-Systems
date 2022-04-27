#SE370 AY20-2
#Lesson 23 Relational Data 2

install.packages('tidyverse')
install.packages('readxl')
library(tidyverse)
library(readxl)
library(ggplot2)

#set the name of the file
filename <- "2019_ncaa_basketball.xlsx"

#reads in the entire workbook
#https://readxl.tidyverse.org/articles/articles/readxl-workflows.html
data <- filename %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = filename)

#look at the resulting object
data$ranking
data$coaches

#join the coaches to the ranking dataset 
basketball <- data$coaches %>%
  left_join(data$ranking, by = c('school' = 'TEAM'))

#or can be done with:
#full_data <- left_join(data$ranking, data$coaches, by = c('TEAM' = "SCHOOL"))

#make a plot that shows the comparison between coaches salary and team ranking
ggplot(basketball, mapping = aes(x = salary, y = RK)) + geom_point() + theme_minimal()

#trying myself:
ggplot(basketball, aes(x = basketball$RK, y = basketball$salary)) + geom_point()

#From the top 50 teams by rank, how are the conferences represented? Make a bar plot
#flip the coord
top_50 <- basketball %>% 
  filter(RK <= 50) %>% 
  group_by(CONF) %>% 
  summarise(count = n())

top_50

ggplot(top_50, aes(x = reorder(CONF, count), y =count)) + 
  geom_bar(stat = 'identity') + coord_flip() + theme_minimal()


ggplot(plot_df, aes(x = reorder(CONF, count), y = count)) + geom_bar(stat = 'identity') + 
  theme_minimal() + coord_flip()

ggplot(plot_df, aes(x = CONF, y = count)) + geom_bar() + 
  theme_minimal() + coord_flip()


#---sqlite demo (nothing for you to do here, just so you can see how it works)
install.packages('RSQLite')
install.packages('DBI')
library(RSQLite)
library(DBI)
#create (or connect to) a sqlite db
ncaa <- dbConnect(RSQLite::SQLite(), 'ncaa_db.sqlite')

#save tables to db
dbWriteTable(ncaa, 'ranking', data$ranking)
dbWriteTable(ncaa, 'coaches', data$coaches)

#show the tables
dbListTables(ncaa)

#pull table from db
coaches <- tbl(ncaa, 'coaches') %>%
  collect()

#BELOW WAS GIVEN

#do the same join we did above, but execute on the database
ranking_coach <- tbl(ncaa, 'coaches') %>%
  left_join(tbl(ncaa, 'ranking'), c('school' = 'TEAM')) %>%
  collect()

#add in additional filters before you collect() to execute everything on the DB
ranking_coach_sub <- tbl(ncaa, 'coaches') %>%
  left_join(tbl(ncaa, 'ranking'), c('school' = 'TEAM')) %>%
  filter(is.na(CONF) == FALSE) %>%
  group_by(CONF) %>%
  summarise(salary = mean(salary, na.rm = TRUE)) %>%
  collect()

ggplot(data = ranking_coach_sub, aes(x = reorder(CONF, salary), y = salary)) + 
  geom_bar(stat = 'identity') +
  coord_flip() + theme_minimal() +
  ylab('Conference')





