rm(list = ls())

suppressMessages({
  library(tidyverse)
  library(highcharter)
  library(dplyr)
  library(viridisLite)
  library(forecast)
  library(treemap)
  library(RJSONIO)
  library(RCurl)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyjs)
  library(shinythemes)
  library(shiny)
})
# from the website
#json.file <-fromJSON("https://pomber.github.io/covid19/timeseries.json", flatten = T)

x <- jsonlite::fromJSON("https://pomber.github.io/covid19/timeseries.json", flatten = T) 

y <- x %>%  map_if(is.data.frame, list) %>% as_tibble()

All.df <- list()

for (i in seq_along(y)) {
  data<- as.data.frame(y[[i]][[1]])
  # beepr::beep()
  filename <-gsub(" ","",names(y)[i])
  data$Country <- filename
  All.df[[i]]<- assign(filename, data)
}

Main.df<- do.call(rbind, All.df)
Main.df[is.na(Main.df)]<-0
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
to.remove <- ls()
to.remove <- c(to.remove[!grepl("Main.df", to.remove)], "to.remove")
rm(list=to.remove)
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
Main.df<- Main.df %>% mutate(id = seq_len(n())) %>% select(c(id ,Country), everything())
names(Main.df)<- c("id", "Country", "Dates","Confirmed","Deaths", "Recovered" )
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

Main.df <- Main.df %>% 
  group_by(Country) %>% 
  mutate(RowIDNum = row_number()) %>% 
  ungroup()

  
Main.df <- Main.df %>% 
  mutate(Dates = ifelse(RowIDNum>345,gsub("2020","2021",Dates, ignore.case = T),Dates),
         Dates = lubridate::ymd(Dates),
         Confirmed = as.numeric(Confirmed),
         Deaths = as.numeric(Deaths),
         Recovered = as.numeric(Recovered)) %>% 
  mutate(Year_Month = format(as.Date(Dates), "%Y-%m"))

Main.df<- Main.df %>% 
mutate(Country = ifelse(Country=="Congo(Brazzaville)","Congo",
ifelse(Country=="Congo(Kinshasa)","DRC",
ifelse(Country=="BurkinaFaso","Burkina Faso",
ifelse(Country=="CaboVerde","Cabo Verde",
ifelse(Country=="Coted'Ivoire","Ivory Coast",
ifelse(Country=="SouthAfrica","South Africa",
ifelse(Country=="SouthSudan","South Sudan",
ifelse(Country=="SierraLeone","Sierra Leone",     
ifelse(Country=="DRC","Ivory Coast",
ifelse(Country=="Mayotte,France","Mayotte",
ifelse(Country=="EquatorialGuinea","Equatorial Guinea",
ifelse(Country=="SaoTomeandPrincipe","Sao Tome and Principe",
ifelse(Country=="CentralAfricanRepublic","Central African Republic",Country))))))))))))))

Main.df<- Main.df %>% 
  mutate(AfricaCheck = ifelse(Country=="Algeria",1,
ifelse(Country=="Benin",1,
ifelse(Country=="Mayotte",1,
ifelse(Country=="Burkina Faso",1,
ifelse(Country=="Cameroon",1,
ifelse(Country=="Cabo Verde",1,
ifelse(Country=="Chad",1,
ifelse(Country=="Congo",1,
ifelse(Country=="Ivory Coast",1,
ifelse(Country=="Egypt",1,
ifelse(Country=="Eritrea",1,
ifelse(Country=="Gambia",1,
ifelse(Country=="Guinea",1,
ifelse(Country=="Kenya",1,
ifelse(Country=="Liberia",1,
ifelse(Country=="Madagascar",1,
ifelse(Country=="Malawi",1,
ifelse(Country=="Mauritania",1,
ifelse(Country=="Mozambique",1,
ifelse(Country=="Niger",1,
ifelse(Country=="Senegal",1,
ifelse(Country=="Sierra Leone",1,
ifelse(Country=="South Africa",1,
ifelse(Country=="Togo",1,
ifelse(Country=="Uganda",1,
ifelse(Country=="Zambia",1,
ifelse(Country=="Angola",1,
ifelse(Country=="Botswana",1,
ifelse(Country=="Burundi",1,
ifelse(Country=="Eswatini",1,
ifelse(Country=="Seychelles",1,
ifelse(Country=="Central African Republic",1,0)))))))))))))))))))))))))))))))))

Main.df<- Main.df %>% 
  mutate(AfricaCheck = 
ifelse(Country=="Comoros",1,
ifelse(Country=="DRC",1,
ifelse(Country=="Djibouti",1,
ifelse(Country=="Equatorial Guinea",1,
ifelse(Country=="Ethiopia",1,
ifelse(Country=="Gabon",1,
ifelse(Country=="Ghana",1,
ifelse(Country=="Guinea-Bissau",1,
ifelse(Country=="Lesotho",1,
ifelse(Country=="Libya",1,
ifelse(Country=="Mali",1,
ifelse(Country=="Mauritius",1,
ifelse(Country=="Morocco",1,
ifelse(Country=="Namibia",1,
ifelse(Country=="Nigeria",1,
ifelse(Country=="Rwanda",1,
ifelse(Country=="Sao Tome and Principe",1,
ifelse(Country=="Somalia",1,
ifelse(Country=="South Sudan",1,
ifelse(Country=="Sudan",1,
ifelse(Country=="Tanzania",1,
ifelse(Country=="Tunisia",1,
ifelse(Country=="Zimbabwe",1,AfricaCheck))))))))))))))))))))))))
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
### Auto select Yesterdays Date
require("lubridate")
stamp <- as.Date(now()) ; stamp_mon <- month(stamp) ; stamp_day <- day(stamp)
stamp_yest <- paste(stamp_day - 1, sep = "") ; stamp_year <- year(stamp)
stamp_caption <- paste(stamp_year,paste("",stamp_mon,sep = ""),stamp_yest, sep = "-")
stamp_caption<- "2020-10-31"

Monthly <- Main.df %>% 
  group_by(Country,Year_Month) %>% 
  #filter(Dates==stamp_caption) %>% 
  summarise(Confirmed = round(max(Confirmed, na.rm = T),0),
            Deaths = round(max(Deaths, na.rm = T),0),
            Recovered = round(max(Recovered, na.rm = T),0))

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

Totals <- Monthly %>% 
  group_by(Country, Year_Month) %>% 
  summarise(Confirmed = max(Confirmed, na.rm = T),
            Deaths = max(Deaths, na.rm = T),
            Recovered = max(Recovered, na.rm = T)) %>% 
  group_by(Year_Month) %>% 
  #ungroup() %>% 
  summarise(TConfirmed = sum(Confirmed, na.rm = T),
            TDeaths = sum(Deaths, na.rm = T),
            TRecovered = sum(Recovered, na.rm = T)) %>% 
  tidyr::gather(Categories, TotalCases, TConfirmed:TRecovered)

thm <- hc_theme(
  colors = c('red', 'green', 'blue'),
  chart = list(
    backgroundColor = NULL#,
    #divBackgroundImage = "http://media3.giphy.com/media/FzxkWdiYp5YFW/giphy.gif"
  ),
  title = list(
    style = list(
      color = '#333333',
      fontFamily = "Lato"
    )
  ),
  subtitle = list(
    style = list(
      color = '#666666',
      fontFamily = "Shadows Into Light"
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = 'Tangerine',
      color = 'black'
    ),
    itemHoverStyle = list(
      color = 'gray'
    )   
  )
)
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
Overall <- Main.df %>% 
  mutate(Month = lubridate::month(Dates))
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

Africa <- Overall %>% 
  filter(AfricaCheck==1) %>% 
  filter(Dates==stamp_caption) %>% 
  group_by(Country,AfricaCheck) %>% 
  #group_by(Country) %>% 
  summarise(Cnf = round(max(Confirmed, na.rm = T),0),
            Rec = round(max(Recovered, na.rm = T),0),
            Dth = round(max(Deaths, na.rm = T),0)) %>% 
  mutate(Active = Cnf - (Rec+Dth))

World <- Overall %>% 
  group_by(Country,AfricaCheck) %>% 
  summarise(Cnf = round(max(Confirmed, na.rm = T),0),
            Rec = round(max(Recovered, na.rm = T),0),
            Dth = round(max(Deaths, na.rm = T),0)) %>% 
  mutate(Active = Cnf - (Rec+Dth))


confirmed_color <- "purple"
active_color <- "#1197fb"
recovered_color <- "forestgreen"
death_color <- "#ff6601"
# active_color <- "#ef9b0f"

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
saveRDS(Main.df, file = "C:\\Users\\Cyrus Muriithi\\Documents\\GithubRepos\\shiny_trend\\CoronaAfrica\\Corona.RDS")

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------