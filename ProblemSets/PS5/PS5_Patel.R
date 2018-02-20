print('PS5')

#Worked on with Jordan H

print('Number 3')

install.packages(dplyr)
install.packages(Quandl)
install.packages(rvest)

library("dplyr")
library("rvest")

#set variables
#read url with read_html
imdb <- read_html("http://www.imdb.com/showtimes/cinema/US/ci100180205?ref_=inth_tny_th")

#set up features of data set
title <- imdb %>% html_nodes(".info span a") %>% html_text
time <- imdb %>% html_nodes("time") %>% html_text

#make data frame to visualize contents
mydf <- data.frame(title, time)

list(mydf)

print ('PS5')
print ('Number 4')

install.packages(dplyr)
install.packages(Quandl)
install.packages(rvest)

library("rvest")
library("dplyr")

#set variable
#read url with read_html
imdb <- read_html("http://www.imdb.com/showtimes/cinema/US/ci100180205?ref_=inth_tny_th")

#set up features of data set
title <- imdb %>% html_nodes(".info span a") %>% html_text
time <- imdb %>% html_nodes("time") %>% html_text

#make data frame to visualize contents
mydf <- data.frame(title, time)

list(mydf)
