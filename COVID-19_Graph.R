




confirmedraw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
str(confirmedraw) # Check latest date at the end of data
deathsraw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recoveredraw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

# DATA CLEANING: To create country level and global combined data
# Convert each data set from wide to long AND aggregate at country level
library(tidyr)
library(dplyr)
library(ggplot2)
confirmed <- confirmedraw %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
deaths <- deathsraw %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
recovered <- recoveredraw %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered=sum(recovered))
summary(confirmed)

# Final data: combine all three
country <- full_join(confirmed, deaths) %>% full_join(recovered)

# Date variable
# Fix date variable and convert from character to date
str(country) # check date character
country$date <- country$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
str(country) # check date Date
# Create new variable: number of days
country <- country %>% group_by(Country.Region) %>% mutate( cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)

# Aggregate at world level
world <- country %>% group_by(date) %>% summarize(confirmed=sum(confirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% mutate(days = date - first(date) + 1)
# Extract specific country: Iran
Iran <- country %>% filter(Country.Region=="Iran")

Iran<-Iran[order(as.Date(Iran$date, format="%d/%m/%Y")),]


# Create new variable: daily confirmed
Iran=Iran %>% group_by(Country.Region) %>%mutate(dailyconfirmed=confirmed - lag(confirmed, default = first(confirmed)))

# Create new variable: daily death
Iran=Iran %>% group_by(Country.Region) %>%mutate(dailydeaths=deaths - lag(deaths, default = first(deaths)))


#Specifying the time interval 
Iran2=Iran %>% slice(1:350)
summary(Iran2)

# Iran daily confirmed & deaths. geom_bar
ggplot(Iran2, aes(x=date))+geom_col(aes(y=dailyconfirmed,fill = "dailyconfirmed"), size = 0.5 ,color = "black")+
  geom_col(aes(y=dailydeaths,fill = "dailydeaths"), size = 0.5,color = "black")+labs(title ="Daily COVID-19 cases and death of Iran", x= "Date", y= "Daily confirmed & deaths")+
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual( "",values = c( "dailyconfirmed"="bisque4","dailydeaths"="red"),
                     labels = c( "daily confirmed","daily deaths")) +   
  theme_minimal()+
  theme(axis.title.x = element_text(color = "black", size = 35,face = "italic"),
        axis.title.y = element_text(color = "black", size = 35,face = "italic"))+
  theme(axis.text = element_text(color = "black", size = 35),
        axis.text.x = element_text(face = "bold"),axis.text.y = element_text(face = "bold"))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 45,hjust = 0.5))+
  theme(legend.title = element_text(family = "Playfair",
                                    color = "black",
                                    size = 14, face = "bold"))+
  theme(legend.text=element_text(size=35, face = "bold"))




# Iran daily confirmed & deaths. geom_line
ggplot(Iran2, aes(x=date))+geom_area(aes(y=dailyconfirmed,fill = "dailyconfirmed"), size = 0.5 ,color = "black",size = 2)+
  geom_area(aes(y=dailydeaths,fill = "dailydeaths"), size = 0.5,color = "black")+labs(title ="Daily COVID-19 cases and death of Iran", x= "Date", y= "Daily confirmed & deaths")+
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual( "",values = c( "dailyconfirmed"="bisque4","dailydeaths"="red",size = 2),
                     labels = c( "daily confirmed","daily deaths")) +   
  theme_minimal()+
  theme(axis.title.x = element_text(color = "black", size = 35,face = "italic"),
        axis.title.y = element_text(color = "black", size = 35,face = "italic"))+
  theme(axis.text = element_text(color = "black", size = 35),
        axis.text.x = element_text(face = "bold"),axis.text.y = element_text(face = "bold"))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 45,hjust = 0.5))+
  theme(legend.title = element_text(family = "Playfair",
                                    color = "black",
                                    size = 14, face = "bold"))+
  theme(legend.text=element_text(size=35, face = "bold"))


