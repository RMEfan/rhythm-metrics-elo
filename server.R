library(shiny)
library(ggplot2)
library(rsconnect)
library(tidyr)
library(vroom)
base_users <- vroom("Users.csv", col_types = "iciddicTi")
base_scores <- vroom("Scores.csv", col_types = "icTiicci")
base_history <- vroom("Players.csv", col_types = "DiciddicTi")


function(input, output, session) {
  lb_page <- reactive({ 
    input$user_page %>%
      as.numeric() %>%
      replace_na(1) %>%
      min(nrow(base_users)+1) %>%
      max(1)
  })

  num_rows <- reactive({
    input$user_num_rows %>%
      as.numeric() %>%
      replace_na(25)
  })
    
  # search users in the Users dataset by username (case insensitive) or user ID
  #output$yes <- renderText({num_rows()})
  output$lb_user_tbl <- renderTable({
                          base_users %>%
                            # match name/user id
                          filter(grepl(casefold(input$user_lookup), casefold(Username)) | 
                                   (input$user_lookup == "") |
                                   (input$user_lookup == UserID),
                            # num global rank within bounds
                                 GlobalRank >= input$user_rank_min,
                                 GlobalRank <= input$user_rank_max,
                            # num maps played within bounds
                                 Games >= input$user_maps_min,
                                 Games <= input$user_maps_max,
                            # location matches
                                 (is.null(input$user_locations)) |
                                   (Location %in% input$user_locations)) %>%
                          mutate("Maps Played" = Games, 
                                 "RME Rank" = RMERanking, 
                                 "pp Rank" = GlobalRank,
                                 "pp" = PP) %>%
                          select("RME Rank", Username, RME, "pp Rank", "pp", "Maps Played", Location) %>%
                          # paged table
                          slice(((lb_page() - 1) * num_rows()):
                                  (lb_page() * num_rows()) )
                        },
                        striped = TRUE,
                        spacing = 'xs',
                        digits = 1,
                        align = 'l',
                        hover = TRUE,
                        width = '100%')
  
  output$distrib_scatter <- renderPlot({
    filtered_users <- base_users %>% filter(
      # num global rank within bounds
                              GlobalRank >= input$distrib_rank_min,
                              GlobalRank <= input$distrib_rank_max,
                              # num maps played within bounds
                              Games >= input$distrib_maps_min,
                              Games <= input$distrib_maps_max,
                              # location matches
                              (is.null(input$distrib_locations)) |
                                (Location %in% input$distrib_locations)) %>%
                             mutate("MapsPlayed" = Games)
    
    p <- ggplot(filtered_users, aes(x = GlobalRank, y = RME, color = MapsPlayed)) + 
              #scale_colour_gradient2(low = "#67c9ff", mid = "#ffffff", midpoint=1000, high = "#f2bbfc") + 
              scale_color_gradientn(colours = c("#00ee99", "#00e7df", "#009edf", "#0053d8", "#000cd0", "#3500c9", "#7200c1", "#aa00ba"),
                                    limits = c(1, 6000),
                                    transform = "log10") +
    
              geom_point(size = .7) +
              coord_cartesian(xlim = c(input$distrib_rank_min,input$distrib_rank_max),
                              ylim = c(500, 3300)) +
      theme(text = element_text(size = 15))
    
    # log plot
    if (input$distrib_rank_log) {
      p <- p + scale_x_continuous(trans='log10', labels = scales::comma)
    } else {
      p <- p + scale_x_continuous(labels = scales::comma)
    }
    
    p <- p + labs(title="Rank vs RME Rating", 
                 subtitle=paste("Players with", input$distrib_maps_min, "-", input$distrib_maps_max, "scores"), 
                 y = "RME Rating", 
                 x = "Global Rank, log plot")
      
    
   p
   })
  
  output$distrib_hist <- renderPlot({
    filtered_users <- base_users %>% filter(
      # num global rank within bounds
      GlobalRank >= input$distrib_rank_min,
      GlobalRank <= input$distrib_rank_max,
      # num maps played within bounds
      Games >= input$distrib_maps_min,
      Games <= input$distrib_maps_max,
      # location matches
      (is.null(input$distrib_locations)) |
        (Location %in% input$distrib_locations)) %>%
      mutate("MapsPlayed" = Games)
    RME_histogram <- ggplot(filtered_users, aes(x = RME)) + geom_histogram(binwidth = 50, colour = "black", fill = "lightblue") 
    RME_histogram <- RME_histogram + labs(title="RME Rating histogram", 
                                          subtitle=paste("Players rank ", input$distrib_rank_min, " - ", input$distrib_rank_max, ",",
                                                         "\n      with ", input$distrib_maps_min, " - ", input$distrib_maps_max, " scores", 
                                                         sep = ""), 
                                          y="Number of players", 
                                          x="RME Rating") +
      theme(text = element_text(size = 15))
    RME_histogram
    
  })
  
  
  output$rating_history_graph <- renderPlot({
    selected_players <- input$hist_users
    selected_hist <- base_history %>%
      filter(Username %in% selected_players)
    
    
    hist_graph <- ggplot(selected_hist, 
                                 aes(x = RatingPeriod, 
                                     y = RME,
                                     color = Username)) +
      geom_line(lwd = 1.1) + 
      labs(title="Rating history of selected players",
           x = "Month", 
           y = "RME") + 
      guides(color = guide_legend(title = "Player")) +
      theme(text = element_text(size = 15))

    hist_graph
  },
  width = 1000)
  
  #output$scores_graph <- renderPlot({
  #  players_set <- input$scores_users
  #  scoress <- base_scores %>%
  #    filter(Score > 100, 
  #           Score < 1300000,
  #           (Username %in% players_set) | (length(players_set) == 0))
  #  hist <- ggplot(scoress, aes(x = Score)) + geom_histogram(binwidth = 10000, colour = "black", fill = "lightblue") 
  #  hist <- hist + labs(title="Score distribution", 
 #                                         subtitle=paste(players_set, 
  #                                                       collapse = ", "), 
  #                                        y="Number of scores", 
  #                                        x="Score") +
 #     theme(text = element_text(size = 15)) + 
  #    scale_x_continuous(labels = scales::comma) +
 #     scale_y_continuous(labels = scales::comma)
 #   hist
 # })
  
  
  #output$
  
  
  
}