########################################################################################

### Packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
library(sjmisc)
library(reactable)
library(janitor)

########################################################################################

### UI

ui <- dashboardPage(

  skin = "black",
  
  title = "2025 MLB Dashboard",
  
  dashboardHeader(
    title = span(tagList(icon("baseball"), "MLBdash")),
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    
    sidebarMenu(
      menuItem("2025 standings", tabName = "standings", icon = icon("list")),
      menuItem("2025 team tiers", tabName = "tiers", icon = icon("layer-group")),
      menuItem("Past seasons", tabName = "past", icon = icon("calendar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "standings",
              
              box(htmlOutput("notes"), width = 13, solidHeader = FALSE),
              
              reactableOutput("table")
      ),
      
      tabItem(tabName = "tiers",
              
              plotOutput("graph", height = "650px")
      ),
      
      tabItem(tabName = "past",
              
              selectInput("year",
                          label = NULL,
                          choices = as.character(seq(from = 2024, to = 1998, by = -1)),
                          selected = "2024",
                          width = "100%"),
              
              plotOutput("pastgraph", height = "650px"),
              
              hr(),
              
              reactableOutput("pasttable"))
    )
  )
)

########################################################################################

##### 2025

### 2025 Baseball Reference

bref <- read_html("https://www.baseball-reference.com/leagues/majors/2025-playoff-odds.shtml")

bref_data <- (bref %>%
                html_elements("table"))[[2]] %>%
                html_table() %>%
                clean_names() %>%
                select(x_2, contains("current"), contains("average")) %>%
                rename(team = 1, current_w = 2, current_l = 3, bref_proj_w = 4, bref_proj_l = 5) %>%
                filter(!str_detect(str_to_lower(bref_proj_w), "east|central|west|average"),
                       !str_detect(team, "Tm")) %>%
                mutate(team = as.factor(team),
                       current_w = as.numeric(current_w),
                       current_l = as.numeric(current_l),
                       bref_proj_w = as.numeric(bref_proj_w),
                       bref_proj_l = as.numeric(bref_proj_l)) %>%
                arrange(team) %>%
                mutate(bref_proj_w = ifelse(bref_proj_w + bref_proj_l == 163,
                                            bref_proj_w - 0.5, bref_proj_w),
                       bref_proj_l = ifelse(bref_proj_w + bref_proj_l == 163,
                                            bref_proj_l - 0.5, bref_proj_l),
                       bref_proj_w = ifelse(bref_proj_w + bref_proj_l == 161,
                                            bref_proj_w + 0.5, bref_proj_w),
                       bref_proj_l = ifelse(bref_proj_w + bref_proj_l == 161,
                                            bref_proj_l + 0.5, bref_proj_l))



### 2025 Baseball Prospectus

bp <- read_html("https://www.baseballprospectus.com/standings/")

bp_data <- bp %>%
            html_elements("table") %>%
            html_table() %>%
            lapply(rename, team = 1) %>%
            bind_rows() %>%
            filter(!str_detect(str_to_lower(team), "east|central|west")) %>%
            select(team, bp_proj_w = SimW, bp_proj_l = SimL) %>%
            mutate(team = ifelse(str_detect(team, "Sacramento"), "AthleticsATH", team)) %>%
            mutate(team = as.factor(team),
                   bp_proj_w = as.numeric(bp_proj_w),
                   bp_proj_l = as.numeric(bp_proj_l)) %>%
            arrange(team)



### 2025 merge data frames

data <- bind_cols(bref_data, bp_data %>% select(-team)) %>%
          select(team, contains("current"), contains("bref"), contains("bp")) %>%
          mutate(avg_w = rowMeans(cbind(bref_proj_w, bp_proj_w)),
                 avg_l = rowMeans(cbind(bref_proj_l, bp_proj_l)))



### 2025 team batting

bref_batting <- read_html("https://www.baseball-reference.com/leagues/majors/2025-standard-batting.shtml")

bref_batting_data <- bref_batting %>%
                      html_element("table") %>%
                      html_table() %>%
                      select(team = Tm, ops = 'OPS+') %>%
                      filter(team %in% bref_data$team) %>%
                      mutate(team = as.factor(team),
                             ops = as.numeric(ops))

data <- data %>% left_join(bref_batting_data, by = "team")



### 2025 team pitching

bref_pitching <- read_html("https://www.baseball-reference.com/leagues/majors/2025-standard-pitching.shtml")

bref_pitching_data <- bref_pitching %>%
                      html_element("table") %>%
                      html_table() %>%
                      select(team = Tm, era = 'ERA+') %>%
                      filter(team %in% bref_data$team) %>%
                      mutate(team = as.factor(team),
                             era = as.numeric(era))

data <- data %>% left_join(bref_pitching_data, by = "team")



### 2025 clean data

data <- data %>%
          arrange(desc(avg_w)) %>%
          mutate(division = case_when(
            str_detect(str_remove_all(team, " "), c("Orioles|RedSox|Yankees|BlueJays|Rays")) ~ "AL East",
            str_detect(str_remove_all(team, " "), c("Tigers|Twins|WhiteSox|Guardians|Royals")) ~ "AL Central",
            str_detect(str_remove_all(team, " "), c("Angels|Rangers|Astros|Athletics|Mariners")) ~ "AL West",
            str_detect(str_remove_all(team, " "), c("Nationals|Phillies|Mets|Braves|Marlins")) ~ "NL East",
            str_detect(str_remove_all(team, " "), c("Cardinals|Cubs|Brewers|Reds|Pirates")) ~ "NL Central",
            str_detect(str_remove_all(team, " "), c("Diamondbacks|Dodgers|Giants|Padres|Rockies")) ~ "NL West",
          )) %>%
          mutate(league = fct_collapse(division,
                                       "AL" = c("AL East", "AL Central", "AL West"),
                                       "NL" = c("NL East", "NL Central", "NL West"))) %>%
          mutate(rank = row_number()) %>%
          select(rank, team, league, division, everything())



### 2025 data table for dashboard

dashboard_data <- data %>%
                    select(-contains("bref"), -contains("bp")) %>%
                    clean_names(case = "title") %>%
                    rename("Current W" = "Current w",
                           "Current L" = "Current l",
                           "Projected W" = "Avg w",
                           "Projected L" = "Avg l",
                           "OPS+" = "Ops",
                           "ERA+" = "Era") %>%
                    mutate(`Projected W` = round(`Projected W`),
                           `Projected L` = round(`Projected L`))



### 2025 data for team tiers graph

plot_data <- dashboard_data %>%
              mutate(Team = str_remove(str_remove(Team, " "),
                                       "Arizona|Atlanta|Baltimore|Boston|Chicago|Cincinnati"),
                     Team = str_remove(str_remove(Team, " "),
                                       "Cleveland|Colorado|Detroit|Houston|KansasCity"),
                     Team = str_remove(str_remove(Team, " "),
                                       "LosAngeles|Miami|Milwaukee|Minnesota|NewYork"),
                     Team = str_remove(str_remove(Team, " "),
                                       "Oakland|Philadelphia|Pittsburgh|SanDiego|SanFrancisco"),
                     Team = str_remove(str_remove(Team, " "),
                                       "Seattle|St.Louis|TampaBay|Texas|Toronto|Washington")) %>%
              mutate(Team = case_when(
                Team == "WhiteSox" ~ "White Sox",
                Team == "RedSox" ~ "Red Sox",
                Team == "BlueJays" ~ "Blue Jays",
                .default = Team
              ))



########################################################################################

### Function to scrape and clean data

scrape_and_clean = function(year) {
  
  ### XXXX wins and losses
  
  standingsXXXX <- read_html(paste0("https://www.baseball-reference.com/leagues/majors/",
                                   year, "-standings.shtml")) %>%
                      html_elements("table") %>%
                      html_table() %>%
                      bind_rows() %>%
                      select(team = Tm, w = W, l = L) %>%
                      arrange(team) %>%
                      mutate(team = as.factor(team))
  
  
  
  ### XXXX team batting
  
  battingXXXX <- read_html(paste0("https://www.baseball-reference.com/leagues/majors/",
                                 year, "-standard-batting.shtml")) %>%
                  html_element("table") %>%
                  html_table() %>%
                  select(team = Tm, ops = 'OPS+') %>%
                  filter(team %in% standingsXXXX$team) %>%
                  mutate(team = as.factor(team),
                         ops = as.numeric(ops))
  
  
  
  ### XXXX team pitching
  
  pitchingXXXX <- read_html(paste0("https://www.baseball-reference.com/leagues/majors/",
                                   year, "-standard-pitching.shtml")) %>%
                    html_element("table") %>%
                    html_table() %>%
                    select(team = Tm, era = 'ERA+') %>%
                    filter(team %in% standingsXXXX$team) %>%
                    mutate(team = as.factor(team),
                           era = as.numeric(era))
  
  
  
  ### XXXX clean data
  
  dataXXXX <- standingsXXXX %>%
                left_join(battingXXXX, by = "team") %>%
                left_join(pitchingXXXX, by = "team") %>%
                arrange(desc(w)) %>%
                mutate(division = case_when(
                  str_detect(str_remove_all(team, " "), c("Orioles|RedSox|Yankees|BlueJays|Rays")) ~ "AL East",
                  str_detect(str_remove_all(team, " "), c("Tigers|Twins|WhiteSox|Guardians|Indians|Royals")) ~ "AL Central",
                  str_detect(str_remove_all(team, " "), c("Angels|Rangers|Astros|Athletics|Mariners")) ~ "AL West",
                  str_detect(str_remove_all(team, " "), c("Nationals|Expos|Phillies|Mets|Braves|Marlins")) ~ "NL East",
                  str_detect(str_remove_all(team, " "), c("Cardinals|Cubs|Brewers|Reds|Pirates")) ~ "NL Central",
                  str_detect(str_remove_all(team, " "), c("Diamondbacks|Dodgers|Giants|Padres|Rockies")) ~ "NL West",
                )) %>%
                mutate(division = ifelse(year < 2013 & str_detect(team, "Astros"), "NL Central", division)) %>%
                mutate(league = fct_collapse(division,
                                             "AL" = c("AL East", "AL Central", "AL West"),
                                             "NL" = c("NL East", "NL Central", "NL West"))) %>%
                mutate(rank = row_number()) %>%
                select(Rank = rank, Team = team, League = league, Division = division,
                       W = w, L = l, "OPS+" = ops, "ERA+" = era)
  
  
  
  ### XXXX data for team tiers graph
  
  dataXXXX_plot = dataXXXX %>% 
                    mutate(Team = str_remove(str_remove(Team, " "),
                                            "Arizona|Atlanta|Baltimore|Boston|Chicago|Cincinnati"),
                          Team = str_remove(str_remove(Team, " "),
                                            "Cleveland|Colorado|Detroit|Houston|KansasCity"),
                          Team = str_remove(str_remove(Team, " "),
                                            "LosAngeles|Anaheim|Miami|Florida|Milwaukee|Minnesota|NewYork"),
                          Team = str_remove(str_remove(Team, " "),
                                            "Oakland|Philadelphia|Pittsburgh|SanDiego|SanFrancisco"),
                          Team = str_remove(str_remove(Team, " "),
                                            "Seattle|St.Louis|TampaBay|Texas|Toronto|Washington|Montreal")) %>%
                    mutate(Team = case_when(
                      Team == "WhiteSox" ~ "White Sox",
                      Team == "RedSox" ~ "Red Sox",
                      Team == "BlueJays" ~ "Blue Jays",
                      Team == "AngelsofAnaheim" ~ "Angels",
                      Team == "DevilRays" ~ "Devil Rays",
                      .default = Team
                    ))
  
  return(list(dataXXXX, dataXXXX_plot))
}

########################################################################################

### Server
server <- function(input, output, session) {
  
  output$notes = renderText(
    paste0("<b>Projected wins/losses:</b> Average of projected wins/losses from ",
    a("Baseball Reference", href = "https://www.baseball-reference.com/leagues/majors/2023-playoff-odds.shtml", target = "_blank"),
    " and ",
    a("Baseball Prospectus", href = "https://www.baseballprospectus.com/standings/", target = "_blank"),
    ". <b>Rank</b> is based on projected wins.",
    "<br>",
    "<b>OPS+:</b> League-normalized on-base plus slugging. Measures offensive performance. League average is 100. Greater than 100 is above average, and vice versa. See ",
    a("here", href = "https://www.mlb.com/glossary/advanced-stats/on-base-plus-slugging-plus", target = "_blank"),
    " for more details.",
    "<br>",
    "<b>ERA+:</b> League-normalized earned run average. Measures pitching performance. League average is 100. Greater than 100 is above average, and vice versa. See ",
    a("here", href = "https://www.mlb.com/glossary/advanced-stats/earned-run-average-plus", target = "_blank"),
    " for more details.")
  )
  
  output$table = renderReactable(
    reactable(dashboard_data,
              columns = list(
                "Rank" = colDef(minWidth = 60,
                                style = list(background = "#D7E4EB", fontWeight = 600)),
                "Team" = colDef(minWidth = 150,
                                style = list(background = "#D7E4EB", fontWeight = 600)),
                "League" = colDef(style = list(background = "#D7E4EB", fontWeight = 600)),
                "Division" = colDef(style = list(background = "#D7E4EB", fontWeight = 600)),
                "Current W" = colDef(defaultSortOrder = "desc"),
                "Current L" = colDef(defaultSortOrder = "asc"),
                "Projected W" = colDef(defaultSortOrder = "desc",
                                       style = list(color = "darkred", fontWeight = 1000)),
                "Projected L" = colDef(defaultSortOrder = "asc",
                                       style = list(color = "darkred", fontWeight = 1000)),
                "OPS+" = colDef(defaultSortOrder = "desc"),
                "ERA+" = colDef(defaultSortOrder = "desc")
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
                                   borderBottom = "3px solid black",
                                   borderTop = "3px solid black")
              ))
  )
  
  output$graph = renderPlot(
    ggplot() +
      geom_hline(yintercept = 100, linetype = "longdash", color = "#951209", linewidth = 0.5) +
      geom_vline(xintercept = 100, linetype = "longdash", color = "#951209", linewidth = 0.5) +
      geom_abline(intercept = 150, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 160, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 170, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 180, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 190, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 200, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 210, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 220, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 230, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 240, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 250, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_label(aes(x = plot_data$`OPS+`,
                     y = plot_data$`ERA+`,
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
      ylim(range(dashboard_data$`ERA+`)) +
      labs(title = paste0("OPS+ vs. ERA+, ", format(Sys.time(), "%B %d")), x = "OPS+", y = "ERA+"),
    res = 96
  )
  
  output$yearlabel = renderText(
    paste0("<b><u>Year:</u></b><p></p>")
  )
  
  get_all_data = reactive(
    scrape_and_clean(input$year)
  )
  
  get_plot_data = reactive(
    as.data.frame(get_all_data()[2], optional = TRUE)
  )
  
  get_table_data = reactive(
    as.data.frame(get_all_data()[1], optional = TRUE)
  )
  
  ops = reactive(
    get_plot_data()$'OPS+'
  )
  
  era = reactive(
    get_plot_data()$'ERA+'
  )
  
  team = reactive(
    get_plot_data()$'Team'
  )
  
  output$pastgraph = renderPlot(
    ggplot() +
      geom_hline(yintercept = 100, linetype = "longdash", color = "#951209", linewidth = 0.5) +
      geom_vline(xintercept = 100, linetype = "longdash", color = "#951209", linewidth = 0.5) +
      geom_abline(intercept = 150, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 160, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 170, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 180, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 190, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 200, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 210, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 220, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 230, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 240, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_abline(intercept = 250, slope = -1, linetype = "dashed", color = "darkslategray", linewidth = 0.15) +
      geom_label(aes(x = ops(),
                     y = era(),
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
      ylim(range(era())) +
      labs(title = paste0("OPS+ vs. ERA+, ", input$year), x = "OPS+", y = "ERA+"),
    res = 96
  )
  
  output$pasttable = renderReactable(
    reactable(get_table_data(),
              columns = list(
                "Rank" = colDef(minWidth = 60,
                                style = list(background = "#D7E4EB", fontWeight = 600)),
                "Team" = colDef(minWidth = 150,
                                style = list(background = "#D7E4EB", fontWeight = 600)),
                "League" = colDef(style = list(background = "#D7E4EB", fontWeight = 600)),
                "Division" = colDef(style = list(background = "#D7E4EB", fontWeight = 600)),
                "W" = colDef(defaultSortOrder = "desc"),
                "L" = colDef(defaultSortOrder = "asc"),
                "OPS+" = colDef(defaultSortOrder = "desc"),
                "ERA+" = colDef(defaultSortOrder = "desc")
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
                                   borderBottom = "3px solid black",
                                   borderTop = "3px solid black")
              ))
  )
}

########################################################################################

### Run app
shinyApp(ui = ui, server = server)

########################################################################################




