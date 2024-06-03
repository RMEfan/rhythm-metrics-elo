library(shiny)
library(ggplot2)
library(flexdashboard)
library(bslib)
library(dplyr)
library(markdown)
library(shinythemes)

library(vroom)
userss <- vroom("Users.csv", col_types = "iciddicTi")
countries <- userss %>%
  select(Location) %>%
  distinct() %>%
  arrange(Location)
  
players_select <- userss %>%
  select(Username) %>%
  distinct() %>%
  arrange(Username)


#p_1 <- sidebarPanel(
  
#  sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
 #             value=min(1000, nrow(dataset)), step=500, round=0),
  
#  selectInput('x', 'X', names(dataset)),
 # selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
 # selectInput('color', 'Color', c('None', names(dataset))),
  
 # checkboxInput('jitter', 'Jitter'),
#  checkboxInput('smooth', 'Smooth'),
  
#  selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
#selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
#)


#mainPanel(
#  plotOutput('plot')
#)

# LEADERBOARD PAGE
lb_user_input <- textInput("user_lookup", 
                            label = "Player", 
                            value = "", 
                            placeholder = "Name or ID",
                            width = "100%")
lb_num_rows_input <- div(style="display: inline-block;vertical-align:bottom; width: 45%;",
                         selectInput("user_num_rows", 
                            label = "Players per page", 
                            c(25, 50, 100, 250, 500), 
                            selected = 25,
                            width = "100%"))
lb_page_input <- div(style="display: inline-block;vertical-align:bottom; width: 45%;",
                     numericInput("user_page",
                            label = "Page",
                            value = 1,
                            min = 1,
                            max = 1000,
                            step = 1,
                            width = "100%"))
lb_rank_min_input <- div(style="display: inline-block;vertical-align:middle; width: 45%;", 
                         numericInput("user_rank_min",
                            label = "Min. rank",
                            value = 1,
                            min = 1,
                            step = 1,
                            width = "100%"))
lb_rank_max_input <- div(style="display: inline-block;vertical-align:middle; width: 45%;", 
                         numericInput("user_rank_max",
                            label = "Max. rank",
                            value = 10000000,
                            min = 1,
                            step = 1,
                            width = "100%"))
lb_maps_min_input <- div(style="display: inline-block;vertical-align:middle; width: 45%;", 
                         numericInput("user_maps_min",
                                      label = "Min. maps played",
                                      value = 1,
                                      min = 1,
                                      step = 1,
                                      width = "100%"))
lb_maps_max_input <- div(style="display: inline-block;vertical-align:middle; width: 45%;", 
                         numericInput("user_maps_max",
                                      label = "Max. maps played",
                                      value = 10000,
                                      min = 1,
                                      step = 1,
                                      width = "100%"))
lb_location_input <- selectInput(
                            "user_locations",
                            "Location(s)",
                            countries,
                            multiple = TRUE,
                            width = "100%")
empty <- div(style="display: inline-block;vertical-align:middle; width: 5%;", "")
lb_params_input <- sidebarPanel(h3("Search"), 
                          lb_user_input, 
                          lb_num_rows_input, empty, lb_page_input,
                          lb_rank_min_input, empty, lb_rank_max_input,
                          lb_maps_min_input, empty, lb_maps_max_input,
                          lb_location_input,
                          width = 3)

lb_table <- mainPanel(tableOutput('lb_user_tbl'))

# DISTRIBUTIONS PAGE
distrib_rank_min_input <- div(style="display: inline-block;vertical-align:middle; width: 45%;", 
                         numericInput("distrib_rank_min",
                                      label = "Min. rank",
                                      value = 1,
                                      min = 1,
                                      step = 1,
                                      width = "100%"))
distrib_rank_max_input <- div(style="display: inline-block;vertical-align:middle; width: 45%;", 
                         numericInput("distrib_rank_max",
                                      label = "Max. rank",
                                      value = 100000,
                                      min = 1,
                                      step = 1,
                                      width = "100%"))
distrib_rme_min_input <- div(style="display: inline-block;vertical-align:middle; width: 45%;", 
                              numericInput("distrib_rme_min",
                                           label = "Min. RME",
                                           value = 200,
                                           min = 1,
                                           step = 1,
                                           width = "100%"))
distrib_rme_max_input <- div(style="display: inline-block;vertical-align:middle; width: 45%;", 
                              numericInput("distrib_rme_max",
                                           label = "Max. RME",
                                           value = 3400,
                                           min = 1,
                                           step = 1,
                                           width = "100%"))
distrib_rank_log_input <- checkboxInput("distrib_rank_log",
                                        label = "Rank: log plot",
                                        value = FALSE,
                                        width = NULL)
distrib_maps_min_input <- div(style="display: inline-block;vertical-align:middle; width: 45%;", 
                         numericInput("distrib_maps_min",
                                      label = "Min. maps played",
                                      value = 30,
                                      min = 1,
                                      step = 1,
                                      width = "100%"))
distrib_maps_max_input <- div(style="display: inline-block;vertical-align:middle; width: 45%;", 
                         numericInput("distrib_maps_max",
                                      label = "Max. maps played",
                                      value = 10000,
                                      min = 1,
                                      step = 1,
                                      width = "100%"))
distrib_location_input <- selectInput(
  "distrib_locations",
  "Location(s)",
  countries,
  multiple = TRUE,
  width = "100%")
empty <- div(style="display: inline-block;vertical-align:middle; width: 5%;", "")
distrib_params_input <- sidebarPanel(h3("Filter Players"),  
                                     distrib_rank_min_input, empty, distrib_rank_max_input,
                                     distrib_rank_log_input,
                                     distrib_rme_min_input, empty, distrib_rme_max_input,
                                     distrib_maps_min_input, empty, distrib_maps_max_input,
                                     distrib_location_input,
                                width = 3)

ditrib_graphs <- mainPanel(plotOutput("distrib_scatter"), plotOutput("distrib_hist"))


# SCORE HISTOGRAM
#scores_user_input <- selectInput(
#          "scores_users",
#          "Select user(s) (default: all users)",
#          choices = players_select,
#          multiple = TRUE,
#          width = "100%")
#scores_params_input <- sidebarPanel(h3("Filter Players"), 
#                                    scores_user_input)
#scores_graph <- mainPanel(plotOutput("scores_graph"))

# RATING HISTORY
hist_user_input <- selectInput(
  "hist_users",
  "Select user(s)",
  players_select,
  multiple = TRUE,
  width = "100%")

#hist_user_input <- selectizeInput("hist_users",
 #                         choices = "RMEfan",
 #                         "Select user(s)",
 #                         multiple = TRUE,
 #                         width = "100%")

hist_params_input <- sidebarPanel(h3("Filter Players"), 
                                  hist_user_input)
#hist_graph <- mainPanel(plotOutput("rating_history_graph"))


ui <- fluidPage(page_navbar(
  title = "Rhythm Metrics Elo",
  theme = shinytheme("cosmo"),
  inverse = TRUE,
  
  nav_panel(title = "Homepage", includeMarkdown("homepage.md")
            ),
  nav_panel(title = "Leaderboard", 
            h2("RME Leaderboard"),
            sidebarLayout(lb_params_input, lb_table)
            ),
  nav_panel(title = "RME Distribution", 
            h2("Distribution of players in the RME system"),
            sidebarLayout(distrib_params_input, ditrib_graphs)
            ),
 # nav_panel(title = "Score Distribution", 
 #           h2("Distribution of scores by selected players"),
  #          sidebarLayout(scores_params_input, scores_graph)
 #          ),
  nav_panel(title = "Rating History", 
            div(style="display: inline-block;vertical-align:middle; width: 100%;", ""),
            hist_params_input,
            #sidebarLayout(hist_params_input, hist_graph)),
            plotOutput("rating_history_graph", width = "100%")),
  nav_spacer(),
  nav_menu(
    title = "Other",
    align = "right",
    nav_item(tags$a("Source code", href = "https://github.com/RMEfan/rhythm-metrics-elo")),
    nav_item(tags$a("Created with Shiny", href = "https://shiny.posit.co"))
  )
)
)

#fluidPage(ui
#)