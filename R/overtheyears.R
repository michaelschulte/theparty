# over the years ...

library(tidyverse)
library(lubridate)

history <- tibble(name = c('#Andreas', '#Andreas_Frau', '#Andreas_Baby', '#Matthias', 
                           '#Matthias_Frau', 'Susa', 'Chrisi', 'Jesko', '#Yann', '#Elisa',
                           '#Simon', '#Sascha', 'Andrea', '#Toni', '#Tomas', 'Paul', '#Katrin',
                           '#Christoph', '#Sandi', 'Ruth', 'Nina', 'Markus', '#Jürgen', 'Ingrid',
                           'Rima', '#Stefan', 'Frank', '#Michael', '#Karoline', '#Samuel', '#Liv', '#Rainer',
                           '#Anne', '#Johannes', 'Dries', 'Laura', '#Eva', '#Stefan', '# Flori', '#Dirk'),
                  earliest_meet =  c("1996-12-31", "20220604", "20220604", "20220604", 
                                     "20220604", "1972-12-31", "2002-01-01", "2004-01-01", "2017-06-17", "20030531",
                                     "2001-09-01", "1991-09-20", "1972-12-31", "1992-02-01", "2012-10-01", "1991-09-20", "1998-01-20",
                                     "2002-12-12", "2002-12-12", "1992-09-01", "1996-01-01", "1996-01-01", "1991-09-20", "1972-12-31",
                                     "1972-12-31", "1991-09-20", "1972-12-31", "2002-10-31", "2012-08-11", "2007-06-16", "1972-12-31", "2012-10-01",
                                     "2012-10-01", "2009-07-10", "1972-12-31", "1972-12-31", "2002-12-12", "2002-12-12", "20060217", "2011-04-01"))

history <- 
history %>%
  mutate_at(vars(contains('earliest_meet')), .funs = ymd) %>%
  mutate(today = ymd(20220604),
         diff = today - earliest_meet)

hist <- data.frame(Salzburg = c(ymd('20010101'), ymd('20020101')), 
      Fribourg = c(ymd('20010101'), ymd('20020101')),
      Berlin = c(ymd('20010101'), ymd('20020101')))

rownames(hist) <- c("Max", "Min")

example <- data.frame(Salzburg = c(ymd('20010101')),
                      Fribourg = c(ymd('20010101')), 
                      Berlin = c(ymd('20010101')),
                      row.names = ('Ellias'))
elias <- rbind(hist, example)
radarchart((elias))

student1_data <- df[c("Max", "Min", "Student.1"), ]
student1_data <- student1_data[,1:3]

radarchart(student1_data)
