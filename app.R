########################################################################################

### Packages
library(shiny)
library(shinydashboard)
library(rvest)
library(sjmisc)
library(tibble)
library(reactable)
library(ggplot2)
library(stringr)

########################################################################################

### UI

ui <- dashboardPage(

  skin = "black",
  
  title = "2022 MLB Dashboard",
  
  dashboardHeader(
    title = span(tagList(icon("circle-notch"), "2022 MLB Dashboard")),
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    
    sidebarMenu(
      menuItem("Standings", tabName = "standings", icon = icon("list")),
      menuItem("Team tiers", tabName = "tiers", icon = icon("layer-group")),
      menuItem("Past seasons", tabName = "past", icon = icon("calendar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "standings",
              
              box(htmlOutput("notes"), width = 13, solidHeader = FALSE),
              
              hr(style="border-color: black"),
  
              htmlOutput("update", style = "text-align: right; margin: 3px"),
              
              reactableOutput("table")
      ),
      
      tabItem(tabName = "tiers",
              
              plotOutput("graph", height = "650px")
      ),
      
      tabItem(tabName = "past",
              
              selectInput("year",
                          label = NULL,
                          choices = c("2021", "2020", "2019", "2018", "2017",
                                      "2016", "2015", "2014", "2013", "2012",
                                      "2011", "2010", "2009", "2008", "2007",
                                      "2006", "2005", "2004", "2003"),
                          selected = "2021",
                          width = "100%"),
              
              plotOutput("pastgraph", height = "650px"),
              
              hr(),
              
              reactableOutput("pasttable"))
    )
  )
)

########################################################################################

##### 2022

### 2022 Baseball Reference

bref = read_html("https://www.baseball-reference.com/leagues/majors/2022-playoff-odds.shtml")

bref_page = bref %>%
  html_elements("td") %>%
  html_text() %>%
  data.frame()

bref_page = bref_page[-c(1:16),] %>%
  data.frame()

bref_page = bref_page[-which(bref_page$. == "NL East" |
                               bref_page$. == "NL Central" |
                               bref_page$. == "NL West" |
                               bref_page$. == "AL East" |
                               bref_page$. == "AL Central" |
                               bref_page$. == "AL West"),] %>%
  data.frame()

bref_team = bref_page[seq(from = 1, to = 842, by = 29),]
bref_proj_w = bref_page[seq(from = 13, to = 854, by = 29),]
bref_proj_l = bref_page[seq(from = 14, to = 855, by = 29),]

bref_data = data.frame(bref_team = as.factor(bref_team),
                       bref_proj_w = as.numeric(bref_proj_w),
                       bref_proj_l = as.numeric(bref_proj_l))
bref_data = bref_data[order(bref_data$bref_team),]
row.names(bref_data) = NULL

for (i in 1:30) {
  if (bref_data$bref_proj_w[i] + bref_data$bref_proj_l[i] == 163) {
    bref_data$bref_proj_w[i] = bref_data$bref_proj_w[i] - 0.5
    bref_data$bref_proj_l[i] = bref_data$bref_proj_l[i] - 0.5
  }
  
  if (bref_data$bref_proj_w[i] + bref_data$bref_proj_l[i] == 161) {
    bref_data$bref_proj_w[i] = bref_data$bref_proj_w[i] + 0.5
    bref_data$bref_proj_l[i] = bref_data$bref_proj_l[i] + 0.5
  }
}



### 2022 FanGraphs

fg = read_html("https://www.fangraphs.com/depthcharts.aspx?position=Standings")

fg_page = fg %>%
  html_elements("td") %>%
  html_text() %>%
  data.frame()

fg_page = fg_page[80:709,] %>%
  data.frame()

fg_team = fg_page[seq(from = 1, to = 610, by = 21),]

for (i in 1:30) {
  if (str_contains(fg_team[i], "Diamondbacks")) {
    fg_team[i] = "Arizona Diamondbacks"
  }
  if (str_contains(fg_team[i], "Braves")) {
    fg_team[i] = "Atlanta Braves"
  }
  if (str_contains(fg_team[i], "Orioles")) {
    fg_team[i] = "Baltimore Orioles"
  }
  if (str_contains(fg_team[i], "Red Sox")) {
    fg_team[i] = "Boston Red Sox"
  }
  if (str_contains(fg_team[i], "Cubs")) {
    fg_team[i] = "Chicago Cubs"
  }
  if (str_contains(fg_team[i], "White Sox")) {
    fg_team[i] = "Chicago White Sox"
  }
  if (str_contains(fg_team[i], "Reds")) {
    fg_team[i] = "Cincinnati Reds"
  }
  if (str_contains(fg_team[i], "Guardians")) {
    fg_team[i] = "Cleveland Guardians"
  }
  if (str_contains(fg_team[i], "Rockies")) {
    fg_team[i] = "Colorado Rockies"
  }
  if (str_contains(fg_team[i], "Tigers")) {
    fg_team[i] = "Detroit Tigers"
  }
  if (str_contains(fg_team[i], "Astros")) {
    fg_team[i] = "Houston Astros"
  }
  if (str_contains(fg_team[i], "Royals")) {
    fg_team[i] = "Kansas City Royals"
  }
  if (str_contains(fg_team[i], "Angels")) {
    fg_team[i] = "Los Angeles Angels"
  }
  if (str_contains(fg_team[i], "Dodgers")) {
    fg_team[i] = "Los Angeles Dodgers"
  }
  if (str_contains(fg_team[i], "Marlins")) {
    fg_team[i] = "Miami Marlins"
  }
  if (str_contains(fg_team[i], "Brewers")) {
    fg_team[i] = "Milwaukee Brewers"
  }
  if (str_contains(fg_team[i], "Twins")) {
    fg_team[i] = "Minnesota Twins"
  }
  if (str_contains(fg_team[i], "Mets")) {
    fg_team[i] = "New York Mets"
  }
  if (str_contains(fg_team[i], "Yankees")) {
    fg_team[i] = "New York Yankees"
  }
  if (str_contains(fg_team[i], "Athletics")) {
    fg_team[i] = "Oakland Athletics"
  }
  if (str_contains(fg_team[i], "Phillies")) {
    fg_team[i] = "Philadelphia Phillies"
  }
  if (str_contains(fg_team[i], "Pirates")) {
    fg_team[i] = "Pittsburgh Pirates"
  }
  if (str_contains(fg_team[i], "Padres")) {
    fg_team[i] = "San Diego Padres"
  }
  if (str_contains(fg_team[i], "Giants")) {
    fg_team[i] = "San Francisco Giants"
  }
  if (str_contains(fg_team[i], "Mariners")) {
    fg_team[i] = "Seattle Mariners"
  }
  if (str_contains(fg_team[i], "Cardinals")) {
    fg_team[i] = "St. Louis Cardinals"
  }
  if (str_contains(fg_team[i], "Rays")) {
    fg_team[i] = "Tampa Bay Rays"
  }
  if (str_contains(fg_team[i], "Rangers")) {
    fg_team[i] = "Texas Rangers"
  }
  if (str_contains(fg_team[i], "Blue Jays")) {
    fg_team[i] = "Toronto Blue Jays"
  }
  if (str_contains(fg_team[i], "Nationals")) {
    fg_team[i] = "Washington Nationals"
  }
}

fg_proj_w = fg_page[seq(from = 16, to = 625, by = 21),]
fg_proj_l = fg_page[seq(from = 17, to = 626, by = 21),]

fg_data = data.frame(fg_team = as.factor(fg_team),
                     fg_proj_w = as.numeric(fg_proj_w),
                     fg_proj_l = as.numeric(fg_proj_l))
fg_data = fg_data[order(fg_data$fg_team),]
row.names(fg_data) = NULL



### 2022 FiveThirtyEight

fte = read_html("https://projects.fivethirtyeight.com/2022-mlb-predictions/")

fte_page = fte %>%
  html_elements("td") %>%
  html_text() %>%
  data.frame()


fte_team = fte_page[seq(from = 1, to = 262, by = 9),]

for (i in 1:30) {
  if (str_contains(fte_team[i], "Diamondbacks")) {
    fte_team[i] = "Arizona Diamondbacks"
  }
  if (str_contains(fte_team[i], "Braves")) {
    fte_team[i] = "Atlanta Braves"
  }
  if (str_contains(fte_team[i], "Orioles")) {
    fte_team[i] = "Baltimore Orioles"
  }
  if (str_contains(fte_team[i], "Red Sox")) {
    fte_team[i] = "Boston Red Sox"
  }
  if (str_contains(fte_team[i], "Cubs")) {
    fte_team[i] = "Chicago Cubs"
  }
  if (str_contains(fte_team[i], "White Sox")) {
    fte_team[i] = "Chicago White Sox"
  }
  if (str_contains(fte_team[i], "Reds")) {
    fte_team[i] = "Cincinnati Reds"
  }
  if (str_contains(fte_team[i], "Guardians")) {
    fte_team[i] = "Cleveland Guardians"
  }
  if (str_contains(fte_team[i], "Rockies")) {
    fte_team[i] = "Colorado Rockies"
  }
  if (str_contains(fte_team[i], "Tigers")) {
    fte_team[i] = "Detroit Tigers"
  }
  if (str_contains(fte_team[i], "Astros")) {
    fte_team[i] = "Houston Astros"
  }
  if (str_contains(fte_team[i], "Royals")) {
    fte_team[i] = "Kansas City Royals"
  }
  if (str_contains(fte_team[i], "Angels")) {
    fte_team[i] = "Los Angeles Angels"
  }
  if (str_contains(fte_team[i], "Dodgers")) {
    fte_team[i] = "Los Angeles Dodgers"
  }
  if (str_contains(fte_team[i], "Marlins")) {
    fte_team[i] = "Miami Marlins"
  }
  if (str_contains(fte_team[i], "Brewers")) {
    fte_team[i] = "Milwaukee Brewers"
  }
  if (str_contains(fte_team[i], "Twins")) {
    fte_team[i] = "Minnesota Twins"
  }
  if (str_contains(fte_team[i], "Mets")) {
    fte_team[i] = "New York Mets"
  }
  if (str_contains(fte_team[i], "Yankees")) {
    fte_team[i] = "New York Yankees"
  }
  if (str_contains(fte_team[i], "Athletics")) {
    fte_team[i] = "Oakland Athletics"
  }
  if (str_contains(fte_team[i], "Phillies")) {
    fte_team[i] = "Philadelphia Phillies"
  }
  if (str_contains(fte_team[i], "Pirates")) {
    fte_team[i] = "Pittsburgh Pirates"
  }
  if (str_contains(fte_team[i], "Padres")) {
    fte_team[i] = "San Diego Padres"
  }
  if (str_contains(fte_team[i], "Giants")) {
    fte_team[i] = "San Francisco Giants"
  }
  if (str_contains(fte_team[i], "Mariners")) {
    fte_team[i] = "Seattle Mariners"
  }
  if (str_contains(fte_team[i], "Cardinals")) {
    fte_team[i] = "St. Louis Cardinals"
  }
  if (str_contains(fte_team[i], "Rays")) {
    fte_team[i] = "Tampa Bay Rays"
  }
  if (str_contains(fte_team[i], "Rangers")) {
    fte_team[i] = "Texas Rangers"
  }
  if (str_contains(fte_team[i], "Blue Jays")) {
    fte_team[i] = "Toronto Blue Jays"
  }
  if (str_contains(fte_team[i], "Nationals")) {
    fte_team[i] = "Washington Nationals"
  }
}

fte_proj_record = unlist(str_split(fte_page[seq(from = 5, to = 266, by = 9),], "-"))
fte_proj_w = fte_proj_record[seq(from = 1, to = 59, by = 2)]
fte_proj_l = fte_proj_record[seq(from = 2, to = 60, by = 2)]

fte_data = data.frame(fte_team = as.factor(fte_team),
                      fte_proj_w = as.numeric(fte_proj_w),
                      fte_proj_l = as.numeric(fte_proj_l))
fte_data = fte_data[order(fte_data$fte_team),]
row.names(fte_data) = NULL



### 2022 current wins and losses

espn = read_html("https://www.espn.com/mlb/standings")

espn_page = espn %>%
              html_elements("td") %>%
              html_text() %>%
              data.frame()

espn_team = espn_page[c(2:6, 8:12, 14:18, 218:222, 224:228, 230:234),]

for (i in 1:30) {
  if (str_contains(espn_team[i], "Diamondbacks")) {
    espn_team[i] = "Arizona Diamondbacks"
  }
  if (str_contains(espn_team[i], "Braves")) {
    espn_team[i] = "Atlanta Braves"
  }
  if (str_contains(espn_team[i], "Orioles")) {
    espn_team[i] = "Baltimore Orioles"
  }
  if (str_contains(espn_team[i], "Red Sox")) {
    espn_team[i] = "Boston Red Sox"
  }
  if (str_contains(espn_team[i], "Cubs")) {
    espn_team[i] = "Chicago Cubs"
  }
  if (str_contains(espn_team[i], "White Sox")) {
    espn_team[i] = "Chicago White Sox"
  }
  if (str_contains(espn_team[i], "Reds")) {
    espn_team[i] = "Cincinnati Reds"
  }
  if (str_contains(espn_team[i], "Guardians")) {
    espn_team[i] = "Cleveland Guardians"
  }
  if (str_contains(espn_team[i], "Rockies")) {
    espn_team[i] = "Colorado Rockies"
  }
  if (str_contains(espn_team[i], "Tigers")) {
    espn_team[i] = "Detroit Tigers"
  }
  if (str_contains(espn_team[i], "Astros")) {
    espn_team[i] = "Houston Astros"
  }
  if (str_contains(espn_team[i], "Royals")) {
    espn_team[i] = "Kansas City Royals"
  }
  if (str_contains(espn_team[i], "Angels")) {
    espn_team[i] = "Los Angeles Angels"
  }
  if (str_contains(espn_team[i], "Dodgers")) {
    espn_team[i] = "Los Angeles Dodgers"
  }
  if (str_contains(espn_team[i], "Marlins")) {
    espn_team[i] = "Miami Marlins"
  }
  if (str_contains(espn_team[i], "Brewers")) {
    espn_team[i] = "Milwaukee Brewers"
  }
  if (str_contains(espn_team[i], "Twins")) {
    espn_team[i] = "Minnesota Twins"
  }
  if (str_contains(espn_team[i], "Mets")) {
    espn_team[i] = "New York Mets"
  }
  if (str_contains(espn_team[i], "Yankees")) {
    espn_team[i] = "New York Yankees"
  }
  if (str_contains(espn_team[i], "Athletics")) {
    espn_team[i] = "Oakland Athletics"
  }
  if (str_contains(espn_team[i], "Phillies")) {
    espn_team[i] = "Philadelphia Phillies"
  }
  if (str_contains(espn_team[i], "Pirates")) {
    espn_team[i] = "Pittsburgh Pirates"
  }
  if (str_contains(espn_team[i], "Padres")) {
    espn_team[i] = "San Diego Padres"
  }
  if (str_contains(espn_team[i], "Giants")) {
    espn_team[i] = "San Francisco Giants"
  }
  if (str_contains(espn_team[i], "Mariners")) {
    espn_team[i] = "Seattle Mariners"
  }
  if (str_contains(espn_team[i], "Cardinals")) {
    espn_team[i] = "St. Louis Cardinals"
  }
  if (str_contains(espn_team[i], "Rays")) {
    espn_team[i] = "Tampa Bay Rays"
  }
  if (str_contains(espn_team[i], "Rangers")) {
    espn_team[i] = "Texas Rangers"
  }
  if (str_contains(espn_team[i], "Blue Jays")) {
    espn_team[i] = "Toronto Blue Jays"
  }
  if (str_contains(espn_team[i], "Nationals")) {
    espn_team[i] = "Washington Nationals"
  }
}

espn_current_w = espn_page[c(seq(from = 30, to = 74, by = 11),
                             seq(from = 96, to = 140, by = 11),
                             seq(from = 162, to = 206, by = 11),
                             seq(from = 246, to = 290, by = 11),
                             seq(from = 312, to = 356, by = 11),
                             seq(from = 378, to = 422, by = 11)),]
espn_current_l = espn_page[c(seq(from = 31, to = 75, by = 11),
                             seq(from = 97, to = 141, by = 11),
                             seq(from = 163, to = 207, by = 11),
                             seq(from = 247, to = 291, by = 11),
                             seq(from = 313, to = 357, by = 11),
                             seq(from = 379, to = 423, by = 11)),]

espn_data = data.frame(espn_team = as.factor(espn_team),
                       espn_current_w = as.numeric(espn_current_w),
                       espn_current_l = as.numeric(espn_current_l))
espn_data = espn_data[order(espn_data$espn_team),]
row.names(espn_data) = NULL



### 2022 merge data frames

data = cbind(espn_data[,1:3], bref_data[-1], fg_data[,-1], fte_data[,-1])
colnames(data)[1] = "team"

data$avg_w = round(rowMeans(data[,c(4,6,8)]), 2)
data$avg_l = round(rowMeans(data[,c(5,7,9)]), 2)



### 2022 team wRC+

fg_batting = read_html("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=8&season=2022&month=0&season1=2022&ind=0&team=0,ts&rost=&age=&filter=&players=0")

fg_batting_page = fg_batting %>%
  html_elements("span") %>%
  html_elements("div") %>%
  html_elements("tbody") %>%
  html_elements("tr") %>%
  html_elements("td") %>%
  html_text() %>%
  data.frame()

fg_batting_team = fg_batting_page[seq(from = 2, to = 640, by = 22),]

for (i in 1:30) {
  if (str_contains(fg_batting_team[i], "ARI")) {
    fg_batting_team[i] = "Arizona Diamondbacks"
  }
  if (str_contains(fg_batting_team[i], "ATL")) {
    fg_batting_team[i] = "Atlanta Braves"
  }
  if (str_contains(fg_batting_team[i], "BAL")) {
    fg_batting_team[i] = "Baltimore Orioles"
  }
  if (str_contains(fg_batting_team[i], "BOS")) {
    fg_batting_team[i] = "Boston Red Sox"
  }
  if (str_contains(fg_batting_team[i], "CHC")) {
    fg_batting_team[i] = "Chicago Cubs"
  }
  if (str_contains(fg_batting_team[i], "CHW")) {
    fg_batting_team[i] = "Chicago White Sox"
  }
  if (str_contains(fg_batting_team[i], "CIN")) {
    fg_batting_team[i] = "Cincinnati Reds"
  }
  if (str_contains(fg_batting_team[i], "CLE")) {
    fg_batting_team[i] = "Cleveland Guardians"
  }
  if (str_contains(fg_batting_team[i], "COL")) {
    fg_batting_team[i] = "Colorado Rockies"
  }
  if (str_contains(fg_batting_team[i], "DET")) {
    fg_batting_team[i] = "Detroit Tigers"
  }
  if (str_contains(fg_batting_team[i], "HOU")) {
    fg_batting_team[i] = "Houston Astros"
  }
  if (str_contains(fg_batting_team[i], "KCR")) {
    fg_batting_team[i] = "Kansas City Royals"
  }
  if (str_contains(fg_batting_team[i], "LAA")) {
    fg_batting_team[i] = "Los Angeles Angels"
  }
  if (str_contains(fg_batting_team[i], "LAD")) {
    fg_batting_team[i] = "Los Angeles Dodgers"
  }
  if (str_contains(fg_batting_team[i], "MIA")) {
    fg_batting_team[i] = "Miami Marlins"
  }
  if (str_contains(fg_batting_team[i], "MIL")) {
    fg_batting_team[i] = "Milwaukee Brewers"
  }
  if (str_contains(fg_batting_team[i], "MIN")) {
    fg_batting_team[i] = "Minnesota Twins"
  }
  if (str_contains(fg_batting_team[i], "NYM")) {
    fg_batting_team[i] = "New York Mets"
  }
  if (str_contains(fg_batting_team[i], "NYY")) {
    fg_batting_team[i] = "New York Yankees"
  }
  if (str_contains(fg_batting_team[i], "OAK")) {
    fg_batting_team[i] = "Oakland Athletics"
  }
  if (str_contains(fg_batting_team[i], "PHI")) {
    fg_batting_team[i] = "Philadelphia Phillies"
  }
  if (str_contains(fg_batting_team[i], "PIT")) {
    fg_batting_team[i] = "Pittsburgh Pirates"
  }
  if (str_contains(fg_batting_team[i], "SDP")) {
    fg_batting_team[i] = "San Diego Padres"
  }
  if (str_contains(fg_batting_team[i], "SFG")) {
    fg_batting_team[i] = "San Francisco Giants"
  }
  if (str_contains(fg_batting_team[i], "SEA")) {
    fg_batting_team[i] = "Seattle Mariners"
  }
  if (str_contains(fg_batting_team[i], "STL")) {
    fg_batting_team[i] = "St. Louis Cardinals"
  }
  if (str_contains(fg_batting_team[i], "TBR")) {
    fg_batting_team[i] = "Tampa Bay Rays"
  }
  if (str_contains(fg_batting_team[i], "TEX")) {
    fg_batting_team[i] = "Texas Rangers"
  }
  if (str_contains(fg_batting_team[i], "TOR")) {
    fg_batting_team[i] = "Toronto Blue Jays"
  }
  if (str_contains(fg_batting_team[i], "WSN")) {
    fg_batting_team[i] = "Washington Nationals"
  }
}

fg_batting_wrc = fg_batting_page[seq(from = 18, to = 656, by = 22),]

fg_batting_data = data.frame(team = as.factor(fg_batting_team),
                             team_wrc = as.numeric(fg_batting_wrc))
fg_batting_data = fg_batting_data[order(fg_batting_data$team),]
row.names(fg_batting_data) = NULL

data$team_wrc = fg_batting_data$team_wrc



### 2022 team FIP-

fg_pitching = read_html("https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=1&season=2022&month=0&season1=2022&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=2022-01-01&enddate=2022-12-31")

fg_pitching_page = fg_pitching %>%
  html_elements("span") %>%
  html_elements("div") %>%
  html_elements("tbody") %>%
  html_elements("tr") %>%
  html_elements("td") %>%
  html_text() %>%
  data.frame()

fg_pitching_team = fg_pitching_page[seq(from = 2, to = 611, by = 21),]

for (i in 1:30) {
  if (str_contains(fg_pitching_team[i], "ARI")) {
    fg_pitching_team[i] = "Arizona Diamondbacks"
  }
  if (str_contains(fg_pitching_team[i], "ATL")) {
    fg_pitching_team[i] = "Atlanta Braves"
  }
  if (str_contains(fg_pitching_team[i], "BAL")) {
    fg_pitching_team[i] = "Baltimore Orioles"
  }
  if (str_contains(fg_pitching_team[i], "BOS")) {
    fg_pitching_team[i] = "Boston Red Sox"
  }
  if (str_contains(fg_pitching_team[i], "CHC")) {
    fg_pitching_team[i] = "Chicago Cubs"
  }
  if (str_contains(fg_pitching_team[i], "CHW")) {
    fg_pitching_team[i] = "Chicago White Sox"
  }
  if (str_contains(fg_pitching_team[i], "CIN")) {
    fg_pitching_team[i] = "Cincinnati Reds"
  }
  if (str_contains(fg_pitching_team[i], "CLE")) {
    fg_pitching_team[i] = "Cleveland Guardians"
  }
  if (str_contains(fg_pitching_team[i], "COL")) {
    fg_pitching_team[i] = "Colorado Rockies"
  }
  if (str_contains(fg_pitching_team[i], "DET")) {
    fg_pitching_team[i] = "Detroit Tigers"
  }
  if (str_contains(fg_pitching_team[i], "HOU")) {
    fg_pitching_team[i] = "Houston Astros"
  }
  if (str_contains(fg_pitching_team[i], "KCR")) {
    fg_pitching_team[i] = "Kansas City Royals"
  }
  if (str_contains(fg_pitching_team[i], "LAA")) {
    fg_pitching_team[i] = "Los Angeles Angels"
  }
  if (str_contains(fg_pitching_team[i], "LAD")) {
    fg_pitching_team[i] = "Los Angeles Dodgers"
  }
  if (str_contains(fg_pitching_team[i], "MIA")) {
    fg_pitching_team[i] = "Miami Marlins"
  }
  if (str_contains(fg_pitching_team[i], "MIL")) {
    fg_pitching_team[i] = "Milwaukee Brewers"
  }
  if (str_contains(fg_pitching_team[i], "MIN")) {
    fg_pitching_team[i] = "Minnesota Twins"
  }
  if (str_contains(fg_pitching_team[i], "NYM")) {
    fg_pitching_team[i] = "New York Mets"
  }
  if (str_contains(fg_pitching_team[i], "NYY")) {
    fg_pitching_team[i] = "New York Yankees"
  }
  if (str_contains(fg_pitching_team[i], "OAK")) {
    fg_pitching_team[i] = "Oakland Athletics"
  }
  if (str_contains(fg_pitching_team[i], "PHI")) {
    fg_pitching_team[i] = "Philadelphia Phillies"
  }
  if (str_contains(fg_pitching_team[i], "PIT")) {
    fg_pitching_team[i] = "Pittsburgh Pirates"
  }
  if (str_contains(fg_pitching_team[i], "SDP")) {
    fg_pitching_team[i] = "San Diego Padres"
  }
  if (str_contains(fg_pitching_team[i], "SFG")) {
    fg_pitching_team[i] = "San Francisco Giants"
  }
  if (str_contains(fg_pitching_team[i], "SEA")) {
    fg_pitching_team[i] = "Seattle Mariners"
  }
  if (str_contains(fg_pitching_team[i], "STL")) {
    fg_pitching_team[i] = "St. Louis Cardinals"
  }
  if (str_contains(fg_pitching_team[i], "TBR")) {
    fg_pitching_team[i] = "Tampa Bay Rays"
  }
  if (str_contains(fg_pitching_team[i], "TEX")) {
    fg_pitching_team[i] = "Texas Rangers"
  }
  if (str_contains(fg_pitching_team[i], "TOR")) {
    fg_pitching_team[i] = "Toronto Blue Jays"
  }
  if (str_contains(fg_pitching_team[i], "WSN")) {
    fg_pitching_team[i] = "Washington Nationals"
  }
}

fg_pitching_fip = fg_pitching_page[seq(from = 15, to = 624, by = 21),]

fg_pitching_data = data.frame(team = as.factor(fg_pitching_team),
                              team_fip = as.numeric(fg_pitching_fip))
fg_pitching_data = fg_pitching_data[order(fg_pitching_data$team),]
row.names(fg_pitching_data) = NULL

data$team_fip = fg_pitching_data$team_fip



### 2022 order data by average projected wins

data = data[order(rowMeans(data[,c(4,6,8)]), decreasing = TRUE),]
row.names(data) = NULL



### 2022 add column for division

data = add_column(data,
                  division = NA,
                  .after = 1)

for (i in 1:30) {
  if (str_contains(data$team[i], "Diamondbacks")) {
    data$division[i] = "NL West"
  }
  if (str_contains(data$team[i], "Braves")) {
    data$division[i] = "NL East"
  }
  if (str_contains(data$team[i], "Orioles")) {
    data$division[i] = "AL East"
  }
  if (str_contains(data$team[i], "Red Sox")) {
    data$division[i] = "AL East"
  }
  if (str_contains(data$team[i], "Cubs")) {
    data$division[i] = "NL Central"
  }
  if (str_contains(data$team[i], "White Sox")) {
    data$division[i] = "AL Central"
  }
  if (str_contains(data$team[i], "Reds")) {
    data$division[i] = "NL Central"
  }
  if (str_contains(data$team[i], "Guardians")) {
    data$division[i] = "AL Central"
  }
  if (str_contains(data$team[i], "Rockies")) {
    data$division[i] = "NL West"
  }
  if (str_contains(data$team[i], "Tigers")) {
    data$division[i] = "AL Central"
  }
  if (str_contains(data$team[i], "Astros")) {
    data$division[i] = "AL West"
  }
  if (str_contains(data$team[i], "Royals")) {
    data$division[i] = "AL Central"
  }
  if (str_contains(data$team[i], "Angels")) {
    data$division[i] = "AL West"
  }
  if (str_contains(data$team[i], "Dodgers")) {
    data$division[i] = "NL West"
  }
  if (str_contains(data$team[i], "Marlins")) {
    data$division[i] = "NL East"
  }
  if (str_contains(data$team[i], "Brewers")) {
    data$division[i] = "NL Central"
  }
  if (str_contains(data$team[i], "Twins")) {
    data$division[i] = "AL Central"
  }
  if (str_contains(data$team[i], "Mets")) {
    data$division[i] = "NL East"
  }
  if (str_contains(data$team[i], "Yankees")) {
    data$division[i] = "AL East"
  }
  if (str_contains(data$team[i], "Athletics")) {
    data$division[i] = "AL West"
  }
  if (str_contains(data$team[i], "Phillies")) {
    data$division[i] = "NL East"
  }
  if (str_contains(data$team[i], "Pirates")) {
    data$division[i] = "NL Central"
  }
  if (str_contains(data$team[i], "Padres")) {
    data$division[i] = "NL West"
  }
  if (str_contains(data$team[i], "Giants")) {
    data$division[i] = "NL West"
  }
  if (str_contains(data$team[i], "Mariners")) {
    data$division[i] = "AL West"
  }
  if (str_contains(data$team[i], "Cardinals")) {
    data$division[i] = "NL Central"
  }
  if (str_contains(data$team[i], "Rays")) {
    data$division[i] = "AL East"
  }
  if (str_contains(data$team[i], "Rangers")) {
    data$division[i] = "AL West"
  }
  if (str_contains(data$team[i], "Blue Jays")) {
    data$division[i] = "AL East"
  }
  if (str_contains(data$team[i], "Nationals")) {
    data$division[i] = "NL East"
  }
}



### 2022 add column for league

data = add_column(data,
                  league = ifelse(data$division == "AL East" |
                                    data$division == "AL Central" |
                                    data$division == "AL West",
                                  "AL", "NL"),
                  .before = "division")



### 2022 add column for overall rank

data = add_column(data,
                  rank = seq(from = 1, to = 30, by = 1),
                  .before = 1)



### 2022 data table for dashboard

dashboard_data = data[,-c(7:12)]

colnames(dashboard_data) = c("Rank",
                             "Team",
                             "League",
                             "Division",
                             "Current W",
                             "Current L",
                             "Projected W",
                             "Projected L",
                             "wRC+",
                             "FIP-")

dashboard_data$`Projected W` = round(dashboard_data$`Projected W`)
dashboard_data$`Projected L` = round(dashboard_data$`Projected L`)



### 2022 data for team tiers graph

plot_data = dashboard_data
plot_data$Team = as.character(plot_data$Team)

for (i in 1:30) {
  if (str_contains(plot_data$Team[i], "Diamondbacks")) {
    plot_data$Team[i] = "Diamondbacks"
  }
  if (str_contains(plot_data$Team[i], "Braves")) {
    plot_data$Team[i] = "Braves"
  }
  if (str_contains(plot_data$Team[i], "Orioles")) {
    plot_data$Team[i] = "Orioles"
  }
  if (str_contains(plot_data$Team[i], "Red Sox")) {
    plot_data$Team[i] = "Red Sox"
  }
  if (str_contains(plot_data$Team[i], "Cubs")) {
    plot_data$Team[i] = "Cubs"
  }
  if (str_contains(plot_data$Team[i], "White Sox")) {
    plot_data$Team[i] = "White Sox"
  }
  if (str_contains(plot_data$Team[i], "Reds")) {
    plot_data$Team[i] = "Reds"
  }
  if (str_contains(plot_data$Team[i], "Guardians")) {
    plot_data$Team[i] = "Guardians"
  }
  if (str_contains(plot_data$Team[i], "Rockies")) {
    plot_data$Team[i] = "Rockies"
  }
  if (str_contains(plot_data$Team[i], "Tigers")) {
    plot_data$Team[i] = "Tigers"
  }
  if (str_contains(plot_data$Team[i], "Astros")) {
    plot_data$Team[i] = "Astros"
  }
  if (str_contains(plot_data$Team[i], "Royals")) {
    plot_data$Team[i] = "Royals"
  }
  if (str_contains(plot_data$Team[i], "Angels")) {
    plot_data$Team[i] = "Angels"
  }
  if (str_contains(plot_data$Team[i], "Dodgers")) {
    plot_data$Team[i] = "Dodgers"
  }
  if (str_contains(plot_data$Team[i], "Marlins")) {
    plot_data$Team[i] = "Marlins"
  }
  if (str_contains(plot_data$Team[i], "Brewers")) {
    plot_data$Team[i] = "Brewers"
  }
  if (str_contains(plot_data$Team[i], "Twins")) {
    plot_data$Team[i] = "Twins"
  }
  if (str_contains(plot_data$Team[i], "Mets")) {
    plot_data$Team[i] = "Mets"
  }
  if (str_contains(plot_data$Team[i], "Yankees")) {
    plot_data$Team[i] = "Yankees"
  }
  if (str_contains(plot_data$Team[i], "Athletics")) {
    plot_data$Team[i] = "Athletics"
  }
  if (str_contains(plot_data$Team[i], "Phillies")) {
    plot_data$Team[i] = "Phillies"
  }
  if (str_contains(plot_data$Team[i], "Pirates")) {
    plot_data$Team[i] = "Pirates"
  }
  if (str_contains(plot_data$Team[i], "Padres")) {
    plot_data$Team[i] = "Padres"
  }
  if (str_contains(plot_data$Team[i], "Giants")) {
    plot_data$Team[i] = "Giants"
  }
  if (str_contains(plot_data$Team[i], "Mariners")) {
    plot_data$Team[i] = "Mariners"
  }
  if (str_contains(plot_data$Team[i], "Cardinals")) {
    plot_data$Team[i] = "Cardinals"
  }
  if (str_contains(plot_data$Team[i], "Rays")) {
    plot_data$Team[i] = "Rays"
  }
  if (str_contains(plot_data$Team[i], "Rangers")) {
    plot_data$Team[i] = "Rangers"
  }
  if (str_contains(plot_data$Team[i], "Blue Jays")) {
    plot_data$Team[i] = "Blue Jays"
  }
  if (str_contains(plot_data$Team[i], "Nationals")) {
    plot_data$Team[i] = "Nationals"
  }
}

########################################################################################

### Function to scrape and clean data

scrape.and.clean = function(year) {
  
  ### XXXX wins and losses
  
  standingsXXXX = read_html(paste0("https://www.espn.com/mlb/standings/_/season/", year))
  
  standingsXXXX_page = standingsXXXX %>%
    html_elements("td") %>%
    html_text() %>%
    data.frame()
  
  if (as.numeric(year) < 2013) {
    standingsXXXX_team = standingsXXXX_page[c(2:6, 8:12, 14:17, 206:210, 212:217, 219:223),]
  }
  
  if (as.numeric(year) >= 2013) {
    standingsXXXX_team = standingsXXXX_page[c(2:6, 8:12, 14:18, 218:222, 224:228, 230:234),]
  }

  for (i in 1:30) {
    if (str_contains(standingsXXXX_team[i], "Diamondbacks")) {
      standingsXXXX_team[i] = "Arizona Diamondbacks"
    }
    if (str_contains(standingsXXXX_team[i], "Braves")) {
      standingsXXXX_team[i] = "Atlanta Braves"
    }
    if (str_contains(standingsXXXX_team[i], "Orioles")) {
      standingsXXXX_team[i] = "Baltimore Orioles"
    }
    if (str_contains(standingsXXXX_team[i], "Red Sox")) {
      standingsXXXX_team[i] = "Boston Red Sox"
    }
    if (str_contains(standingsXXXX_team[i], "Cubs")) {
      standingsXXXX_team[i] = "Chicago Cubs"
    }
    if (str_contains(standingsXXXX_team[i], "White Sox")) {
      standingsXXXX_team[i] = "Chicago White Sox"
    }
    if (str_contains(standingsXXXX_team[i], "Reds")) {
      standingsXXXX_team[i] = "Cincinnati Reds"
    }
    if (str_contains(standingsXXXX_team[i], "Indians")) {
      standingsXXXX_team[i] = "Cleveland Indians"
    }
    if (str_contains(standingsXXXX_team[i], "Guardians")) {
      standingsXXXX_team[i] = "Cleveland Guardians"
    }
    if (str_contains(standingsXXXX_team[i], "Rockies")) {
      standingsXXXX_team[i] = "Colorado Rockies"
    }
    if (str_contains(standingsXXXX_team[i], "Tigers")) {
      standingsXXXX_team[i] = "Detroit Tigers"
    }
    if (str_contains(standingsXXXX_team[i], "Astros")) {
      standingsXXXX_team[i] = "Houston Astros"
    }
    if (str_contains(standingsXXXX_team[i], "Royals")) {
      standingsXXXX_team[i] = "Kansas City Royals"
    }
    if (str_contains(standingsXXXX_team[i], "Angels")) {
      standingsXXXX_team[i] = "Los Angeles Angels"
    }
    if (str_contains(standingsXXXX_team[i], "Dodgers")) {
      standingsXXXX_team[i] = "Los Angeles Dodgers"
    }
    if (str_contains(standingsXXXX_team[i], "Marlins")) {
      standingsXXXX_team[i] = "Miami Marlins"
    }
    if (str_contains(standingsXXXX_team[i], "Brewers")) {
      standingsXXXX_team[i] = "Milwaukee Brewers"
    }
    if (str_contains(standingsXXXX_team[i], "Twins")) {
      standingsXXXX_team[i] = "Minnesota Twins"
    }
    if (str_contains(standingsXXXX_team[i], "Mets")) {
      standingsXXXX_team[i] = "New York Mets"
    }
    if (str_contains(standingsXXXX_team[i], "Yankees")) {
      standingsXXXX_team[i] = "New York Yankees"
    }
    if (str_contains(standingsXXXX_team[i], "Athletics")) {
      standingsXXXX_team[i] = "Oakland Athletics"
    }
    if (str_contains(standingsXXXX_team[i], "Phillies")) {
      standingsXXXX_team[i] = "Philadelphia Phillies"
    }
    if (str_contains(standingsXXXX_team[i], "Pirates")) {
      standingsXXXX_team[i] = "Pittsburgh Pirates"
    }
    if (str_contains(standingsXXXX_team[i], "Padres")) {
      standingsXXXX_team[i] = "San Diego Padres"
    }
    if (str_contains(standingsXXXX_team[i], "Giants")) {
      standingsXXXX_team[i] = "San Francisco Giants"
    }
    if (str_contains(standingsXXXX_team[i], "Mariners")) {
      standingsXXXX_team[i] = "Seattle Mariners"
    }
    if (str_contains(standingsXXXX_team[i], "Cardinals")) {
      standingsXXXX_team[i] = "St. Louis Cardinals"
    }
    if (str_contains(standingsXXXX_team[i], "Rays")) {
      standingsXXXX_team[i] = "Tampa Bay Rays"
    }
    if (str_contains(standingsXXXX_team[i], "Rangers")) {
      standingsXXXX_team[i] = "Texas Rangers"
    }
    if (str_contains(standingsXXXX_team[i], "Blue Jays")) {
      standingsXXXX_team[i] = "Toronto Blue Jays"
    }
    if (str_contains(standingsXXXX_team[i], "Nationals")) {
      standingsXXXX_team[i] = "Washington Nationals"
    }
    if (str_contains(standingsXXXX_team[i], "Expos")) {
      standingsXXXX_team[i] = "Montreal Expos"
    }
  }
  
  if (as.numeric(year) < 2013) {
    standingsXXXX_w = standingsXXXX_page[c(seq(from = 29, to = 73, by = 11),
                                           seq(from = 95, to = 139, by = 11),
                                           seq(from = 161, to = 194, by = 11),
                                           seq(from = 235, to = 279, by = 11),
                                           seq(from = 301, to = 356, by = 11),
                                           seq(from = 378, to = 422, by = 11)),]
    standingsXXXX_l = standingsXXXX_page[c(seq(from = 30, to = 74, by = 11),
                                           seq(from = 96, to = 140, by = 11),
                                           seq(from = 162, to = 195, by = 11),
                                           seq(from = 236, to = 280, by = 11),
                                           seq(from = 302, to = 357, by = 11),
                                           seq(from = 379, to = 423, by = 11)),]
  }
  
  if (as.numeric(year) >= 2013) {
    standingsXXXX_w = standingsXXXX_page[c(seq(from = 30, to = 74, by = 11),
                                           seq(from = 96, to = 140, by = 11),
                                           seq(from = 162, to = 206, by = 11),
                                           seq(from = 246, to = 290, by = 11),
                                           seq(from = 312, to = 356, by = 11),
                                           seq(from = 378, to = 422, by = 11)),]
    standingsXXXX_l = standingsXXXX_page[c(seq(from = 31, to = 75, by = 11),
                                           seq(from = 97, to = 141, by = 11),
                                           seq(from = 163, to = 207, by = 11),
                                           seq(from = 247, to = 291, by = 11),
                                           seq(from = 313, to = 357, by = 11),
                                           seq(from = 379, to = 423, by = 11)),]
  }
  
  standingsXXXX_data = data.frame(team = as.factor(standingsXXXX_team),
                                  w = as.numeric(standingsXXXX_w),
                                  l = as.numeric(standingsXXXX_l))
  standingsXXXX_data = standingsXXXX_data[order(standingsXXXX_data$team),]
  row.names(standingsXXXX_data) = NULL
  
  
  
  ### XXXX team wRC+
  
  battingXXXX = read_html(paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=8&season=", year, "&month=0&season1=", year, "&ind=0&team=0,ts&rost=&age=&filter=&players=0"))
  
  battingXXXX_page = battingXXXX %>%
    html_elements("span") %>%
    html_elements("div") %>%
    html_elements("tbody") %>%
    html_elements("tr") %>%
    html_elements("td") %>%
    html_text() %>%
    data.frame()
  
  battingXXXX_team = battingXXXX_page[seq(from = 2, to = 640, by = 22),]
  
  for (i in 1:30) {
    if (str_contains(battingXXXX_team[i], "ARI")) {
      battingXXXX_team[i] = "Arizona Diamondbacks"
    }
    if (str_contains(battingXXXX_team[i], "ATL")) {
      battingXXXX_team[i] = "Atlanta Braves"
    }
    if (str_contains(battingXXXX_team[i], "BAL")) {
      battingXXXX_team[i] = "Baltimore Orioles"
    }
    if (str_contains(battingXXXX_team[i], "BOS")) {
      battingXXXX_team[i] = "Boston Red Sox"
    }
    if (str_contains(battingXXXX_team[i], "CHC")) {
      battingXXXX_team[i] = "Chicago Cubs"
    }
    if (str_contains(battingXXXX_team[i], "CHW")) {
      battingXXXX_team[i] = "Chicago White Sox"
    }
    if (str_contains(battingXXXX_team[i], "CIN")) {
      battingXXXX_team[i] = "Cincinnati Reds"
    }
    if (str_contains(battingXXXX_team[i], "CLE") & as.numeric(year) < 2022) {
      battingXXXX_team[i] = "Cleveland Indians"
    }
    if (str_contains(battingXXXX_team[i], "CLE") & as.numeric(year) >= 2022) {
      battingXXXX_team[i] = "Cleveland Guardians"
    }
    if (str_contains(battingXXXX_team[i], "COL")) {
      battingXXXX_team[i] = "Colorado Rockies"
    }
    if (str_contains(battingXXXX_team[i], "DET")) {
      battingXXXX_team[i] = "Detroit Tigers"
    }
    if (str_contains(battingXXXX_team[i], "HOU")) {
      battingXXXX_team[i] = "Houston Astros"
    }
    if (str_contains(battingXXXX_team[i], "KCR")) {
      battingXXXX_team[i] = "Kansas City Royals"
    }
    if (str_contains(battingXXXX_team[i], "ANA") & as.numeric(year) < 2005) {
      battingXXXX_team[i] = "Los Angeles Angels"
    }
    if (str_contains(battingXXXX_team[i], "LAA") & as.numeric(year) >= 2005) {
      battingXXXX_team[i] = "Los Angeles Angels"
    }
    if (str_contains(battingXXXX_team[i], "LAD")) {
      battingXXXX_team[i] = "Los Angeles Dodgers"
    }
    if (str_contains(battingXXXX_team[i], "FLA") & as.numeric(year) < 2012) {
      battingXXXX_team[i] = "Miami Marlins"
    }
    if (str_contains(battingXXXX_team[i], "MIA") & as.numeric(year) >= 2012) {
      battingXXXX_team[i] = "Miami Marlins"
    }
    if (str_contains(battingXXXX_team[i], "MIL")) {
      battingXXXX_team[i] = "Milwaukee Brewers"
    }
    if (str_contains(battingXXXX_team[i], "MIN")) {
      battingXXXX_team[i] = "Minnesota Twins"
    }
    if (str_contains(battingXXXX_team[i], "NYM")) {
      battingXXXX_team[i] = "New York Mets"
    }
    if (str_contains(battingXXXX_team[i], "NYY")) {
      battingXXXX_team[i] = "New York Yankees"
    }
    if (str_contains(battingXXXX_team[i], "OAK")) {
      battingXXXX_team[i] = "Oakland Athletics"
    }
    if (str_contains(battingXXXX_team[i], "PHI")) {
      battingXXXX_team[i] = "Philadelphia Phillies"
    }
    if (str_contains(battingXXXX_team[i], "PIT")) {
      battingXXXX_team[i] = "Pittsburgh Pirates"
    }
    if (str_contains(battingXXXX_team[i], "SDP")) {
      battingXXXX_team[i] = "San Diego Padres"
    }
    if (str_contains(battingXXXX_team[i], "SFG")) {
      battingXXXX_team[i] = "San Francisco Giants"
    }
    if (str_contains(battingXXXX_team[i], "SEA")) {
      battingXXXX_team[i] = "Seattle Mariners"
    }
    if (str_contains(battingXXXX_team[i], "STL")) {
      battingXXXX_team[i] = "St. Louis Cardinals"
    }
    if (str_contains(battingXXXX_team[i], "TBR") & as.numeric(year) >= 2008) {
      battingXXXX_team[i] = "Tampa Bay Rays"
    }
    if (str_contains(battingXXXX_team[i], "TBD") & as.numeric(year) < 2008) {
      battingXXXX_team[i] = "Tampa Bay Rays"
    }
    if (str_contains(battingXXXX_team[i], "TEX")) {
      battingXXXX_team[i] = "Texas Rangers"
    }
    if (str_contains(battingXXXX_team[i], "TOR")) {
      battingXXXX_team[i] = "Toronto Blue Jays"
    }
    if (str_contains(battingXXXX_team[i], "WSN")) {
      battingXXXX_team[i] = "Washington Nationals"
    }
    if (str_contains(battingXXXX_team[i], "MON")) {
      battingXXXX_team[i] = "Montreal Expos"
    }
  }
  
  battingXXXX_wrc = battingXXXX_page[seq(from = 18, to = 656, by = 22),]
  
  battingXXXX_data = data.frame(team = as.factor(battingXXXX_team),
                                team_wrc = as.numeric(battingXXXX_wrc))
  battingXXXX_data = battingXXXX_data[order(battingXXXX_data$team),]
  row.names(battingXXXX_data) = NULL
  
  
  
  ### XXXX team FIP-
  
  pitchingXXXX = read_html(paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=1&season=", year, "&month=0&season1=", year, "&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=", year, "-01-01&enddate=", year, "-12-31"))
  
  pitchingXXXX_page = pitchingXXXX %>%
    html_elements("span") %>%
    html_elements("div") %>%
    html_elements("tbody") %>%
    html_elements("tr") %>%
    html_elements("td") %>%
    html_text() %>%
    data.frame()
  
  pitchingXXXX_team = pitchingXXXX_page[seq(from = 2, to = 611, by = 21),]
  
  for (i in 1:30) {
    if (str_contains(pitchingXXXX_team[i], "ARI")) {
      pitchingXXXX_team[i] = "Arizona Diamondbacks"
    }
    if (str_contains(pitchingXXXX_team[i], "ATL")) {
      pitchingXXXX_team[i] = "Atlanta Braves"
    }
    if (str_contains(pitchingXXXX_team[i], "BAL")) {
      pitchingXXXX_team[i] = "Baltimore Orioles"
    }
    if (str_contains(pitchingXXXX_team[i], "BOS")) {
      pitchingXXXX_team[i] = "Boston Red Sox"
    }
    if (str_contains(pitchingXXXX_team[i], "CHC")) {
      pitchingXXXX_team[i] = "Chicago Cubs"
    }
    if (str_contains(pitchingXXXX_team[i], "CHW")) {
      pitchingXXXX_team[i] = "Chicago White Sox"
    }
    if (str_contains(pitchingXXXX_team[i], "CIN")) {
      pitchingXXXX_team[i] = "Cincinnati Reds"
    }
    if (str_contains(pitchingXXXX_team[i], "CLE") & as.numeric(year) < 2022) {
      pitchingXXXX_team[i] = "Cleveland Indians"
    }
    if (str_contains(pitchingXXXX_team[i], "CLE") & as.numeric(year) >= 2022) {
      pitchingXXXX_team[i] = "Cleveland Guardians"
    }
    if (str_contains(pitchingXXXX_team[i], "COL")) {
      pitchingXXXX_team[i] = "Colorado Rockies"
    }
    if (str_contains(pitchingXXXX_team[i], "DET")) {
      pitchingXXXX_team[i] = "Detroit Tigers"
    }
    if (str_contains(pitchingXXXX_team[i], "HOU")) {
      pitchingXXXX_team[i] = "Houston Astros"
    }
    if (str_contains(pitchingXXXX_team[i], "KCR")) {
      pitchingXXXX_team[i] = "Kansas City Royals"
    }
    if (str_contains(pitchingXXXX_team[i], "ANA") & as.numeric(year) < 2005) {
      pitchingXXXX_team[i] = "Los Angeles Angels"
    }
    if (str_contains(pitchingXXXX_team[i], "LAA") & as.numeric(year) >= 2005) {
      pitchingXXXX_team[i] = "Los Angeles Angels"
    }
    if (str_contains(pitchingXXXX_team[i], "LAD")) {
      pitchingXXXX_team[i] = "Los Angeles Dodgers"
    }
    if (str_contains(pitchingXXXX_team[i], "FLA") & as.numeric(year) < 2012) {
      pitchingXXXX_team[i] = "Miami Marlins"
    }
    if (str_contains(pitchingXXXX_team[i], "MIA") & as.numeric(year) >= 2012) {
      pitchingXXXX_team[i] = "Miami Marlins"
    }
    if (str_contains(pitchingXXXX_team[i], "MIL")) {
      pitchingXXXX_team[i] = "Milwaukee Brewers"
    }
    if (str_contains(pitchingXXXX_team[i], "MIN")) {
      pitchingXXXX_team[i] = "Minnesota Twins"
    }
    if (str_contains(pitchingXXXX_team[i], "NYM")) {
      pitchingXXXX_team[i] = "New York Mets"
    }
    if (str_contains(pitchingXXXX_team[i], "NYY")) {
      pitchingXXXX_team[i] = "New York Yankees"
    }
    if (str_contains(pitchingXXXX_team[i], "OAK")) {
      pitchingXXXX_team[i] = "Oakland Athletics"
    }
    if (str_contains(pitchingXXXX_team[i], "PHI")) {
      pitchingXXXX_team[i] = "Philadelphia Phillies"
    }
    if (str_contains(pitchingXXXX_team[i], "PIT")) {
      pitchingXXXX_team[i] = "Pittsburgh Pirates"
    }
    if (str_contains(pitchingXXXX_team[i], "SDP")) {
      pitchingXXXX_team[i] = "San Diego Padres"
    }
    if (str_contains(pitchingXXXX_team[i], "SFG")) {
      pitchingXXXX_team[i] = "San Francisco Giants"
    }
    if (str_contains(pitchingXXXX_team[i], "SEA")) {
      pitchingXXXX_team[i] = "Seattle Mariners"
    }
    if (str_contains(pitchingXXXX_team[i], "STL")) {
      pitchingXXXX_team[i] = "St. Louis Cardinals"
    }
    if (str_contains(pitchingXXXX_team[i], "TBD") & as.numeric(year) < 2008) {
      pitchingXXXX_team[i] = "Tampa Bay Rays"
    }
    if (str_contains(pitchingXXXX_team[i], "TBR") & as.numeric(year) >= 2008) {
      pitchingXXXX_team[i] = "Tampa Bay Rays"
    }
    if (str_contains(pitchingXXXX_team[i], "TEX")) {
      pitchingXXXX_team[i] = "Texas Rangers"
    }
    if (str_contains(pitchingXXXX_team[i], "TOR")) {
      pitchingXXXX_team[i] = "Toronto Blue Jays"
    }
    if (str_contains(pitchingXXXX_team[i], "WSN")) {
      pitchingXXXX_team[i] = "Washington Nationals"
    }
    if (str_contains(pitchingXXXX_team[i], "MON")) {
      pitchingXXXX_team[i] = "Montreal Expos"
    }
  }
  
  pitchingXXXX_fip = pitchingXXXX_page[seq(from = 15, to = 624, by = 21),]
  
  pitchingXXXX_data = data.frame(team = as.factor(pitchingXXXX_team),
                                 team_fip = as.numeric(pitchingXXXX_fip))
  pitchingXXXX_data = pitchingXXXX_data[order(pitchingXXXX_data$team),]
  row.names(pitchingXXXX_data) = NULL
  
  
  
  ### XXXX merge data frames
  
  dataXXXX = cbind(standingsXXXX_data, battingXXXX_data[,-1], pitchingXXXX_data[,-1])
  colnames(dataXXXX) = c("team", "w", "l", "team_wrc", "team_fip")
  
  dataXXXX = dataXXXX[order(dataXXXX$w, decreasing = TRUE),]
  row.names(dataXXXX) = NULL
  
  dataXXXX = add_column(dataXXXX,
                        division = NA,
                        .after = 1)
  
  for (i in 1:30) {
    if (str_contains(dataXXXX$team[i], "Diamondbacks")) {
      dataXXXX$division[i] = "NL West"
    }
    if (str_contains(dataXXXX$team[i], "Braves")) {
      dataXXXX$division[i] = "NL East"
    }
    if (str_contains(dataXXXX$team[i], "Orioles")) {
      dataXXXX$division[i] = "AL East"
    }
    if (str_contains(dataXXXX$team[i], "Red Sox")) {
      dataXXXX$division[i] = "AL East"
    }
    if (str_contains(dataXXXX$team[i], "Cubs")) {
      dataXXXX$division[i] = "NL Central"
    }
    if (str_contains(dataXXXX$team[i], "White Sox")) {
      dataXXXX$division[i] = "AL Central"
    }
    if (str_contains(dataXXXX$team[i], "Reds")) {
      dataXXXX$division[i] = "NL Central"
    }
    if (str_contains(dataXXXX$team[i], "Indians")) {
      dataXXXX$division[i] = "AL Central"
    }
    if (str_contains(dataXXXX$team[i], "Guardians")) {
      dataXXXX$division[i] = "AL Central"
    }
    if (str_contains(dataXXXX$team[i], "Rockies")) {
      dataXXXX$division[i] = "NL West"
    }
    if (str_contains(dataXXXX$team[i], "Tigers")) {
      dataXXXX$division[i] = "AL Central"
    }
    if (str_contains(dataXXXX$team[i], "Astros") & as.numeric(year)<2013) {
      dataXXXX$division[i] = "NL Central"
    }
    if (str_contains(dataXXXX$team[i], "Astros") & as.numeric(year)>=2013) {
      dataXXXX$division[i] = "AL West"
    }
    if (str_contains(dataXXXX$team[i], "Royals")) {
      dataXXXX$division[i] = "AL Central"
    }
    if (str_contains(dataXXXX$team[i], "Angels")) {
      dataXXXX$division[i] = "AL West"
    }
    if (str_contains(dataXXXX$team[i], "Dodgers")) {
      dataXXXX$division[i] = "NL West"
    }
    if (str_contains(dataXXXX$team[i], "Marlins")) {
      dataXXXX$division[i] = "NL East"
    }
    if (str_contains(dataXXXX$team[i], "Brewers")) {
      dataXXXX$division[i] = "NL Central"
    }
    if (str_contains(dataXXXX$team[i], "Twins")) {
      dataXXXX$division[i] = "AL Central"
    }
    if (str_contains(dataXXXX$team[i], "Mets")) {
      dataXXXX$division[i] = "NL East"
    }
    if (str_contains(dataXXXX$team[i], "Yankees")) {
      dataXXXX$division[i] = "AL East"
    }
    if (str_contains(dataXXXX$team[i], "Athletics")) {
      dataXXXX$division[i] = "AL West"
    }
    if (str_contains(dataXXXX$team[i], "Phillies")) {
      dataXXXX$division[i] = "NL East"
    }
    if (str_contains(dataXXXX$team[i], "Pirates")) {
      dataXXXX$division[i] = "NL Central"
    }
    if (str_contains(dataXXXX$team[i], "Padres")) {
      dataXXXX$division[i] = "NL West"
    }
    if (str_contains(dataXXXX$team[i], "Giants")) {
      dataXXXX$division[i] = "NL West"
    }
    if (str_contains(dataXXXX$team[i], "Mariners")) {
      dataXXXX$division[i] = "AL West"
    }
    if (str_contains(dataXXXX$team[i], "Cardinals")) {
      dataXXXX$division[i] = "NL Central"
    }
    if (str_contains(dataXXXX$team[i], "Rays")) {
      dataXXXX$division[i] = "AL East"
    }
    if (str_contains(dataXXXX$team[i], "Rangers")) {
      dataXXXX$division[i] = "AL West"
    }
    if (str_contains(dataXXXX$team[i], "Blue Jays")) {
      dataXXXX$division[i] = "AL East"
    }
    if (str_contains(dataXXXX$team[i], "Nationals")) {
      dataXXXX$division[i] = "NL East"
    }
    if (str_contains(dataXXXX$team[i], "Expos")) {
      dataXXXX$division[i] = "NL East"
    }
  }
  
  dataXXXX = add_column(dataXXXX,
                        league = ifelse(dataXXXX$division == "AL East" |
                                          dataXXXX$division == "AL Central" |
                                          dataXXXX$division == "AL West",
                                        "AL", "NL"),
                        .before = "division")
  
  dataXXXX = add_column(dataXXXX,
                        rank = seq(from = 1, to = 30, by = 1),
                        .before = 1)
  
  colnames(dataXXXX) = c("Rank", "Team", "League", "Division",
                         "W", "L", "wRC+", "FIP-")
  
  
  
  ### XXXX data for graph
  
  dataXXXX_plot = dataXXXX
  dataXXXX_plot$Team = as.character(dataXXXX_plot$Team)
  
  for (i in 1:30) {
    if (str_contains(dataXXXX_plot$Team[i], "Diamondbacks")) {
      dataXXXX_plot$Team[i] = "Diamondbacks"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Braves")) {
      dataXXXX_plot$Team[i] = "Braves"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Orioles")) {
      dataXXXX_plot$Team[i] = "Orioles"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Red Sox")) {
      dataXXXX_plot$Team[i] = "Red Sox"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Cubs")) {
      dataXXXX_plot$Team[i] = "Cubs"
    }
    if (str_contains(dataXXXX_plot$Team[i], "White Sox")) {
      dataXXXX_plot$Team[i] = "White Sox"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Reds")) {
      dataXXXX_plot$Team[i] = "Reds"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Indians")) {
      dataXXXX_plot$Team[i] = "Indians"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Guardians")) {
      dataXXXX_plot$Team[i] = "Guardians"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Rockies")) {
      dataXXXX_plot$Team[i] = "Rockies"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Tigers")) {
      dataXXXX_plot$Team[i] = "Tigers"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Astros")) {
      dataXXXX_plot$Team[i] = "Astros"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Royals")) {
      dataXXXX_plot$Team[i] = "Royals"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Angels")) {
      dataXXXX_plot$Team[i] = "Angels"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Dodgers")) {
      dataXXXX_plot$Team[i] = "Dodgers"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Marlins")) {
      dataXXXX_plot$Team[i] = "Marlins"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Brewers")) {
      dataXXXX_plot$Team[i] = "Brewers"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Twins")) {
      dataXXXX_plot$Team[i] = "Twins"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Mets")) {
      dataXXXX_plot$Team[i] = "Mets"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Yankees")) {
      dataXXXX_plot$Team[i] = "Yankees"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Athletics")) {
      dataXXXX_plot$Team[i] = "Athletics"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Phillies")) {
      dataXXXX_plot$Team[i] = "Phillies"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Pirates")) {
      dataXXXX_plot$Team[i] = "Pirates"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Padres")) {
      dataXXXX_plot$Team[i] = "Padres"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Giants")) {
      dataXXXX_plot$Team[i] = "Giants"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Mariners")) {
      dataXXXX_plot$Team[i] = "Mariners"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Cardinals")) {
      dataXXXX_plot$Team[i] = "Cardinals"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Rays")) {
      dataXXXX_plot$Team[i] = "Rays"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Rangers")) {
      dataXXXX_plot$Team[i] = "Rangers"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Blue Jays")) {
      dataXXXX_plot$Team[i] = "Blue Jays"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Nationals")) {
      dataXXXX_plot$Team[i] = "Nationals"
    }
    if (str_contains(dataXXXX_plot$Team[i], "Expos")) {
      dataXXXX_plot$Team[i] = "Expos"
    }
  }
  
  return(list(dataXXXX, dataXXXX_plot))
}

########################################################################################

### Server
server <- function(input, output, session) {
  
  output$notes = renderText(
    paste0("<b><u>Notes:</u></b>",
    "<br><br>",
    "<b>Projected wins/losses:</b> Average of projected wins/losses from ",
    a("Baseball Reference", href = "https://www.baseball-reference.com/leagues/majors/2022-playoff-odds.shtml", target = "_blank"),
    ", ",
    a("Fangraphs", href = "https://www.fangraphs.com/depthcharts.aspx?position=Standings", target = "_blank"),
    ", and ",
    a("FiveThirtyEight", href = "https://projects.fivethirtyeight.com/2022-mlb-predictions/", target = "_blank"),
    ". <b>Rank</b> is based on projected wins.",
    "<br>",
    "<b>wRC+:</b> Weighted runs created plus, a measure of offensive value. League average is 100. Values greater than 100 are above average, and vice versa. See ",
    a("here", href = "https://library.fangraphs.com/offense/wrc/", target = "_blank"),
    " for more details.",
    "<br>",
    "<b>FIP-:</b> Fielding independent pitching minus, a measure of pitching value. League average is 100. Values less than 100 are above average, and vice versa. See ",
    a("here", href = "https://library.fangraphs.com/pitching/era-fip-xfip/", target = "_blank"),
    " for more details.")
  )
  
  output$update = renderText(
    paste0("<u>Last update:</u> Thu July 21 2022 11:45PM EDT")
  )
  
  output$table = renderReactable(
    reactable(dashboard_data,
              columns = list(
                "Rank" = colDef(minWidth = 60,
                                style = list(background = "#D7E4EB", fontWeight = 600)),
                "Team" = colDef(minWidth = 150,
                                style = list(background = "#D7E4EB", fontWeight = 600)),
                "Current W" = colDef(defaultSortOrder = "desc"),
                "Current L" = colDef(defaultSortOrder = "asc"),
                "Projected W" = colDef(defaultSortOrder = "desc",
                                       style = list(color = "#951209", fontWeight = 600)),
                "Projected L" = colDef(defaultSortOrder = "asc",
                                       style = list(color = "#951209", fontWeight = 600)),
                "wRC+" = colDef(defaultSortOrder = "desc"),
                "FIP-" = colDef(defaultSortOrder = "asc")
              ),
              bordered = TRUE,
              highlight = TRUE,
              resizable = FALSE,
              filterable = FALSE,
              searchable = FALSE,
              striped = TRUE,
              defaultPageSize = 30,
              showSortable = TRUE,
              wrap = FALSE,
              defaultColDef = colDef(
                align = "center",
                headerStyle = list(background = "#D7E4EB", fontWeight = "bold",
                                   borderBottom = "1px solid black",
                                   borderTop = "1px solid black")
              ))
  )
  
  output$graph = renderPlot(
    ggplot() +
      geom_hline(yintercept = 100, linetype = "dotted", color = "#951209", size = 1) +
      geom_vline(xintercept = 100, linetype = "dotted", color = "#951209", size = 1) +
      geom_abline(intercept = -30, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = -20, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = -10, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = 0, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = 10, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = 20, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = 30, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_label(aes(x = plot_data$`wRC+`,
                     y = plot_data$`FIP-`,
                     label = plot_data$Team),
                 fontface = "bold",
                 size = 3) +
      theme(plot.background = element_rect(fill = "#ecf0f5"),
            plot.title=element_text(family="sans", face="bold", size=20, hjust=0.5, color="darkslategray"), 
            axis.title.x=element_text(family="sans", face="bold", size=16, color="darkslategray", hjust=0.5, vjust = 0.9),
            axis.title.y=element_text(family="sans", face="bold", size = 16, color="darkslategray", hjust=0.5),
            axis.text.x=element_text(family="sans", face="plain", size = 12, color="darkslategray"),
            axis.text.y=element_text(family="sans", face="plain", size=12, color="darkslategray"),
            panel.background=element_rect(fill="#ecf0f5"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.line = element_line(color = "darkslategray"),
            axis.ticks=element_blank()) +
      ylim(rev(range(dashboard_data$`FIP-`))) +
      labs(title = paste0("wRC+ vs. FIP-, ", format(Sys.time(), "%B %d")), x = "wRC+", y = "FIP-"),
    res = 96
  )
  
  output$yearlabel = renderText(
    paste0("<b><u>Year:</u></b><p></p>")
  )
  
  get_all_data = reactive(
    scrape.and.clean(input$year)
  )
  
  get_plot_data = reactive(
    as.data.frame(get_all_data()[2], optional = TRUE)
  )
  
  get_table_data = reactive(
    as.data.frame(get_all_data()[1], optional = TRUE)
  )
  
  wrc = reactive(
    get_plot_data()$'wRC+'
  )
  
  fip = reactive(
    get_plot_data()$'FIP-'
  )
  
  team = reactive(
    get_plot_data()$'Team'
  )
  
  output$pastgraph = renderPlot(
    ggplot() +
      geom_hline(yintercept = 100, linetype = "dotted", color = "#951209", size = 1) +
      geom_vline(xintercept = 100, linetype = "dotted", color = "#951209", size = 1) +
      geom_abline(intercept = -30, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = -20, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = -10, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = 0, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = 10, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = 20, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_abline(intercept = 30, slope = -1, linetype = "solid", color = "darkslategray", size = 0.15) +
      geom_label(aes(x = wrc(),
                     y = fip(),
                     label = team()),
                 fontface = "bold",
                 size = 3) +
      theme(plot.background = element_rect(fill = "#ecf0f5"),
            plot.title=element_text(family="sans", face="bold", size=20, hjust=0.5, color="darkslategray"), 
            axis.title.x=element_text(family="sans", face="bold", size=16, color="darkslategray", hjust=0.5, vjust = 0.9),
            axis.title.y=element_text(family="sans", face="bold", size = 16, color="darkslategray", hjust=0.5),
            axis.text.x=element_text(family="sans", face="plain", size = 12, color="darkslategray"),
            axis.text.y=element_text(family="sans", face="plain", size=12, color="darkslategray"),
            panel.background=element_rect(fill="#ecf0f5"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.line = element_line(color = "darkslategray"),
            axis.ticks=element_blank()) +
      ylim(rev(range(fip()))) +
      labs(title = paste0("wRC+ vs. FIP-, ", input$year), x = "wRC+", y = "FIP-"),
    res = 96
  )
  
  output$pasttable = renderReactable(
    reactable(get_table_data(),
              columns = list(
                "Rank" = colDef(minWidth = 60,
                                style = list(background = "#D7E4EB", fontWeight = 600)),
                "Team" = colDef(minWidth = 150,
                                style = list(background = "#D7E4EB", fontWeight = 600)),
                "W" = colDef(defaultSortOrder = "desc"),
                "L" = colDef(defaultSortOrder = "asc"),
                "wRC+" = colDef(defaultSortOrder = "desc"),
                "FIP-" = colDef(defaultSortOrder = "asc")
              ),
              bordered = TRUE,
              highlight = TRUE,
              resizable = FALSE,
              filterable = FALSE,
              searchable = FALSE,
              striped = TRUE,
              defaultPageSize = 30,
              showSortable = TRUE,
              wrap = FALSE,
              defaultColDef = colDef(
                align = "center",
                headerStyle = list(background = "#D7E4EB", fontWeight = "bold",
                                   borderBottom = "1px solid black",
                                   borderTop = "1px solid black")
              ))
  )
}

########################################################################################

### Run app
shinyApp(ui = ui, server = server)

########################################################################################




