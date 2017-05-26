
library(dplyr)
library(shiny)
library(ggplot2)
library(DT)
library(feather)
library(shinyURL)

res <- read_feather("data/shiny_spray_master")
#res <- read_feather("/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Spray Chart Tool/spray_chart/shiny_spraychart/data/shiny_spray_master")

# tooltip code modified from https://gitlab.com/snippets/16220

theme_battedball_grey <- function(base_size = 12, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(face = "bold", size = 14),
      panel.grid = element_blank()
    )
}

ui <- fluidPage(
  titlePanel("The Interactive Spray Chart Tool"),
  fluidRow(
    column(12,
           p("Please be patient while initial graphs load. Data includes games from 2010-03-30 through ", max(as.Date(res$game_date, "%Y-%m-%d"))),
           sidebarLayout(
             sidebarPanel(
               shinyURL.ui(label = "Save Selections & Share URL"), 
               radioButtons("chartType", "Type of Player to Chart",
                            choices = c("Batters", "Pitchers"),
                            inline = TRUE, 
                            selected = "Batters"),
               radioButtons("compType", "Type of Comparison",
                            choices = c("Different Players", "Same Player Over Time"),
                            inline = FALSE, 
                            selected = "Different Players"),
               radioButtons("statcast", "Include Statcast Filters?", 
                            choices = c("Include (Show Only 2015-Present)", "Exclude (Show All Years)"),
                            selected = "Exclude (Show All Years)"),
               conditionalPanel(condition = "input.chartType == 'Batters' & input.compType == 'Different Players'",
                                selectizeInput("player_b", "Batter (select up to 2)",
                                               choices = c("All", sort(as.character(unique(res$Name)))),
                                               selected = c("Mike Trout", "Bryce Harper"), 
                                               multiple = TRUE)),
               conditionalPanel(condition = "input.chartType == 'Batters' & input.compType == 'Different Players'",
                                selectizeInput("pitcher_b", "Pitcher",
                                               choices = c("All", sort(as.character(unique(res$`Pitcher Name`)))),
                                               selected = "All", 
                                               multiple = TRUE)),
               conditionalPanel(condition = "input.chartType == 'Pitchers' & input.compType == 'Different Players'",
                                selectizeInput("player_p", "Batter",
                                               choices = c("All", sort(as.character(unique(res$Name)))),
                                               selected = "All",
                                               multiple = TRUE)),
               conditionalPanel(condition = "input.chartType == 'Pitchers' & input.compType == 'Different Players'", 
                                selectizeInput("pitcher_p", "Pitcher (select up to 2)",
                                               choices = c("All", sort(as.character(unique(res$`Pitcher Name`)))),
                                               selected = c("Clayton Kershaw", "Yu Darvish"), 
                                               multiple = TRUE)),
               conditionalPanel(condition = "input.chartType == 'Batters' & input.compType == 'Same Player Over Time'",
                                selectizeInput("player_b_s", "Batter",
                                               choices = sort(as.character(unique(res$Name))),
                                               selected = c("Mike Trout"),
                                               multiple = FALSE)),
               conditionalPanel(condition = "input.chartType == 'Batters' & input.compType == 'Same Player Over Time'",
                                selectizeInput("pitcher_b_s", "Pitcher",
                                               choices = c("All", sort(as.character(unique(res$`Pitcher Name`)))),
                                               selected = "All", 
                                               multiple = TRUE)),
               conditionalPanel(condition = "input.chartType == 'Pitchers' & input.compType == 'Same Player Over Time'",
                                selectizeInput("player_p_s", "Batter",
                                               choices = c("All", sort(as.character(unique(res$Name)))),
                                               selected = "All",
                                               multiple = TRUE)),
               conditionalPanel(condition = "input.chartType == 'Pitchers' & input.compType == 'Same Player Over Time'", 
                                selectizeInput("pitcher_p_s", "Pitcher",
                                               choices = sort(as.character(unique(res$`Pitcher Name`))),
                                               selected = c("Clayton Kershaw"), 
                                               multiple = FALSE)),
               conditionalPanel(condition = "input.compType == 'Different Players'",
                                dateRangeInput("dates", "Date Range", 
                                               start = max(as.Date(res$game_date, "%Y-%m-%d")-30), 
                                               end = max(as.Date(res$game_date, "%Y-%m-%d")),
                                               min = min(as.Date(res$game_date, "%Y-%m-%d")),
                                               max = max(as.Date(res$game_date, "%Y-%m-%d")),
                                               language = "en", separator = " to ", width = NULL)),
               conditionalPanel(condition = "input.compType == 'Same Player Over Time'", dateRangeInput("datescomp1", "First Date Range", 
                                                                                                        start = max(as.Date(res$game_date, "%Y-%m-%d")-395),
                                                                                                        end = max(as.Date(res$game_date, "%Y-%m-%d")-365),
                                                                                                        min = min(as.Date(res$game_date, "%Y-%m-%d")),
                                                                                                        max = max(as.Date(res$game_date, "%Y-%m-%d")),
                                                                                                        language = "en", separator = " to ", width = NULL)),
               conditionalPanel(condition = "input.compType == 'Same Player Over Time'", dateRangeInput("datescomp2", "Second Date Range",                                                                                                        start = max(as.Date(res$game_date, "%Y-%m-%d")-30), 
                                                                                                        end = max(as.Date(res$game_date, "%Y-%m-%d")),
                                                                                                        min = min(as.Date(res$game_date, "%Y-%m-%d")),
                                                                                                        max = max(as.Date(res$game_date, "%Y-%m-%d")),
                                                                                                        language = "en", separator = " to ", width = NULL)),
               selectInput("stand_b", "Batter Stands", 
                           choices = c("R", "L"),
                           selected = c("R", "L"),
                           multiple = TRUE),
               selectInput("p_throws", "Pitcher Throws", 
                           choices = c("R", "L"),
                           selected = c("R", "L"),
                           multiple = TRUE),
               selectInput("pitchType", "Pitch Type", 
                           choices = c("FF", "FT", "FT", "SI", "FA", "FO", "FS", "CU", "CH", "SL", "KC", "EP", "SC"),
                           selected = c("FF", "FT", "FT", "SI", "FA", "FO", "FS", "CU", "CH", "SL", "KC", "EP", "SC"),
                           multiple = TRUE),
               sliderInput("p_velo", "Pitch Velocity", 
                           min = 50, 
                           max = 110, 
                           c(50, 110), 
                           step = .1,
                           post = "mph"),
               selectInput("outcome", "Outcome Type", 
                           choices = c("1B", "2B", "3B", "HR", "Out"),
                           selected = c("1B", "2B", "3B", "HR", "Out"),
                           multiple = TRUE),
               sliderInput("p_horizontal", "Pitch Horizontal Location (ft.), Catcher's View", 
                           min = -4,
                           max = 4,
                           c(-4,4),
                           step = .1,
                           post = "ft."),
               sliderInput("p_vertical", "Pitch Vertical Location (ft.), Catcher's View", 
                           min = -2,
                           max = 7,
                           c(-2, 7),
                           step = .1,
                           post = "ft."),
               conditionalPanel(condition = "input.statcast == 'Include (Show Only 2015-Present)'", sliderInput("h_velo", "Launch Velocity", 
                                                                                                                min = 0, 
                                                                                                                max = 120, 
                                                                                                                c(0, 120), 
                                                                                                                step = .1,
                                                                                                                post = "mph")),
               conditionalPanel(condition = "input.statcast == 'Include (Show Only 2015-Present)'", sliderInput("l_angle", "Launch Angle", 
                                                                                                                min = -85, 
                                                                                                                max = 85, 
                                                                                                                c(-85, 85),
                                                                                                                step = .1,
                                                                                                                post = " degrees")),
               conditionalPanel(condition = "input.statcast == 'Include (Show Only 2015-Present)'", selectInput("barrels", "Barrels",
                                                                                                                choices = c("barrel", "non-barrel"),
                                                                                                                selected = c("barrel", "non-barrel"), 
                                                                                                                multiple = TRUE)),
               selectInput("zone", "Spray Zone (from left to right)", 
                           choices = sort(unique(res$Zone), decreasing = FALSE),
                           selected = sort(unique(res$Zone), decreasing = FALSE),
                           multiple = TRUE),
               width = 3
               
             ), 
             
             mainPanel(
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               ),
               conditionalPanel(condition = "input.chartType == 'Batters' & input.compType == 'Different Players'",
                                tabsetPanel(tabPanel("Batter Comparison", 
                                                     div(style = "position:relative", plotOutput(outputId = "coolplot_hitters", hover = hoverOpts("plot_hover_batters", delay = 100, delayType = "debounce")), uiOutput("hover_info_batters")),
                                                     br(), br(), br(), br(), br(), br(), DT::dataTableOutput(outputId = 'b_results')),
                                            tabPanel("About", 
                                                     br(), h1("About the Interactive Spray Chart Tool"), 
                                                     br(), p("This spray chart tool is maintained by Bill Petti (@BillPetti, billpetti.github.io). The data that feeds the tool comes courtesy of MLBAM and baseballsavant.com. Distance prior to 2015 is caculated based on stringer positioning of where battted balls are picked up by a fielder. Distance from 2015 to the present is based on Statcast measures unless the data was missing. In that case, caculated distance based on stringer positioning of the ball is used."),
                                                     br(), p("Please note, the data is by it's nature incomplete. I have tried to include as much data as possible, but some coordinates for batted balls are incorrect and have been removed from the tool. The plotting data is also based on human plotting and judgement. The data is also limited to those cases where information about the pitch is complete (i.e. the coordinates for where it crossed home plate)."),
                                                     br(), p("Data begins with the 2010 season and is updated daily during the season."),
                                                     br(), p("This is a very large app, so it may take a while to initially load. For now, multiple selections will execute one by one, so if you are removing a number of pitch types the charts will reload sequentially for each type you remove. I am working on optimiizing this behavior."),
                                                     h2("Definitions"), 
                                                     br(), p("Barrel's are defined by MLBAM and are based on the combination of launch angle and launch speed off the bat. Barrel % is simply the percentage of batted balls displayed that fall into the category of a barrel"), 
                                                     br(), p("Estimated Weighted On-base Average (wOBA) is also based on launch angle and launch speed off the bat. It was derived by MLBAM and places a run expectancy value on different combinations of launch speed and angle."),
                                                     br(), p("Spray Zone takes the field of play and breaks it into 13 zones based on the horizontal angle of the ball off the bat, relative to dead center field. From left field to right field, the first zone is 1 (so, anything less than -55 degrees relative to home plate) and the last zone is 13 (anything greater than 55 degrees relative to home plate). Zone 7 represents balls hit between -5 and 5 degrees relative to home plate."),
                                                     h2("Links"), 
                                                     br(), a("Barrel", href = "http://m.mlb.com/glossary/statcast/barrel"),
                                                     br(), a("Estimated wOBA", href = "http://m.mlb.com/glossary/statcast/expected-woba"), 
                                                     br(), a("MLBAM", href = "http://www.mlbam.com"), 
                                                     br(), a("Baseball Savant", href = "http://www.mlbam.com"),
                                                     br(), a("Click here for the original tool (data through 2016--does not include Statcast data)", href = "http://public.tableau.com/views/SprayChartComparison/About?%3Aembed=y&%3AshowVizHome=no"),
                                                     br(), a("My website", href = "https://billpetti.github.io"), 
                                                     br(), a("Find me on Twitter", href = "https://twitter.com/BillPetti")
                                            )
                                )),
               conditionalPanel(condition = "input.chartType == 'Pitchers' & input.compType == 'Different Players'",
                                tabsetPanel(tabPanel("Pitcher Comparison", 
                                                     div(style = "position:relative", plotOutput(outputId = "coolplot_pitchers", hover = hoverOpts("plot_hover_pitchers", delay = 100, delayType = "debounce")), uiOutput("hover_info_pitchers")),
                                                     br(), br(), br(), br(), br(), br(), DT::dataTableOutput(outputId = 'p_results')),
                                            tabPanel("About", 
                                                     br(), h1("About the Interactive Spray Chart Tool"), 
                                                     br(), p("This spray chart tool is maintained by Bill Petti (@BillPetti, billpetti.github.io). The data that feeds the tool comes courtesy of MLBAM and baseballsavant.com. Distance prior to 2015 is caculated based on stringer positioning of where battted balls are picked up by a fielder. Distance from 2015 to the present is based on Statcast measures unless the data was missing. In that case, caculated distance based on stringer positioning of the ball is used."),
                                                     br(), p("Please note, the data is by it's nature incomplete. I have tried to include as much data as possible, but some coordinates for batted balls are incorrect and have been removed from the tool. The plotting data is also based on human plotting and judgement. The data is also limited to those cases where information about the pitch is complete (i.e. the coordinates for where it crossed home plate)."),
                                                     br(), p("Data begins with the 2010 season and is updated daily during the season."),
                                                     br(), p("This is a very large app, so it may take a while to initially load. For now, multiple selections will execute one by one, so if you are removing a number of pitch types the charts will reload sequentially for each type you remove. I am working on optimiizing this behavior."),
                                                     h2("Definitions"), 
                                                     br(), p("Barrel's are defined by MLBAM and are based on the combination of launch angle and launch speed off the bat. Barrel % is simply the percentage of batted balls displayed that fall into the category of a barrel"), 
                                                     br(), p("Estimated Weighted On-base Average (wOBA) is also based on launch angle and launch speed off the bat. It was derived by MLBAM and places a run expectancy value on different combinations of launch speed and angle."),
                                                     br(), p("Spray Zone takes the field of play and breaks it into 13 zones based on the horizontal angle of the ball off the bat, relative to dead center field. From left field to right field, the first zone is 1 (so, anything less than -55 degrees relative to home plate) and the last zone is 13 (anything greater than 55 degrees relative to home plate). Zone 7 represents balls hit between -5 and 5 degrees relative to home plate."),
                                                     h2("Links"), 
                                                     br(), a("Barrel", href = "http://m.mlb.com/glossary/statcast/barrel"),
                                                     br(), a("Estimated wOBA", href = "http://m.mlb.com/glossary/statcast/expected-woba"), 
                                                     br(), a("MLBAM", href = "http://www.mlbam.com"), 
                                                     br(), a("Baseball Savant", href = "http://www.mlbam.com"),
                                                     br(), a("Click here for the original tool (data through 2016--does not include Statcast data)", href = "http://public.tableau.com/views/SprayChartComparison/About?%3Aembed=y&%3AshowVizHome=no"),
                                                     br(), a("My website", href = "https://billpetti.github.io"), 
                                                     br(), a("Find me on Twitter", href = "https://twitter.com/BillPetti")
                                            )
                                )), 
               conditionalPanel(condition = "input.chartType == 'Batters' & input.compType == 'Same Player Over Time'",
                                tabsetPanel(tabPanel("Batter Comparison Over Time", 
                                                     div(style = "position:relative", plotOutput(outputId = "coolplot_hitters_same", hover = hoverOpts("plot_hover_batters_same", delay = 100, delayType = "debounce")), uiOutput("hover_info_batters_same")),
                                                     br(), br(), br(), br(), br(), br(), DT::dataTableOutput(outputId = 'b_results_same')),
                                            tabPanel("About", 
                                                     br(), h1("About the Interactive Spray Chart Tool"), 
                                                     br(), p("This spray chart tool is maintained by Bill Petti (@BillPetti, billpetti.github.io). The data that feeds the tool comes courtesy of MLBAM and baseballsavant.com. Distance prior to 2015 is caculated based on stringer positioning of where battted balls are picked up by a fielder. Distance from 2015 to the present is based on Statcast measures unless the data was missing. In that case, caculated distance based on stringer positioning of the ball is used."),
                                                     br(), p("Please note, the data is by it's nature incomplete. I have tried to include as much data as possible, but some coordinates for batted balls are incorrect and have been removed from the tool. The plotting data is also based on human plotting and judgement. The data is also limited to those cases where information about the pitch is complete (i.e. the coordinates for where it crossed home plate)."),
                                                     br(), p("Data begins with the 2010 season and is updated daily during the season."),
                                                     br(), p("This is a very large app, so it may take a while to initially load. For now, multiple selections will execute one by one, so if you are removing a number of pitch types the charts will reload sequentially for each type you remove. I am working on optimiizing this behavior."),
                                                     h2("Definitions"), 
                                                     br(), p("Barrel's are defined by MLBAM and are based on the combination of launch angle and launch speed off the bat. Barrel % is simply the percentage of batted balls displayed that fall into the category of a barrel"), 
                                                     br(), p("Estimated Weighted On-base Average (wOBA) is also based on launch angle and launch speed off the bat. It was derived by MLBAM and places a run expectancy value on different combinations of launch speed and angle."),
                                                     br(), p("Spray Zone takes the field of play and breaks it into 13 zones based on the horizontal angle of the ball off the bat, relative to dead center field. From left field to right field, the first zone is 1 (so, anything less than -55 degrees relative to home plate) and the last zone is 13 (anything greater than 55 degrees relative to home plate). Zone 7 represents balls hit between -5 and 5 degrees relative to home plate."),
                                                     h2("Links"), 
                                                     br(), a("Barrel", href = "http://m.mlb.com/glossary/statcast/barrel"),
                                                     br(), a("Estimated wOBA", href = "http://m.mlb.com/glossary/statcast/expected-woba"), 
                                                     br(), a("MLBAM", href = "http://www.mlbam.com"), 
                                                     br(), a("Baseball Savant", href = "http://www.mlbam.com"),
                                                     br(), a("Click here for the original tool (data through 2016--does not include Statcast data)", href = "http://public.tableau.com/views/SprayChartComparison/About?%3Aembed=y&%3AshowVizHome=no"),
                                                     br(), a("My website", href = "https://billpetti.github.io"), 
                                                     br(), a("Find me on Twitter", href = "https://twitter.com/BillPetti")
                                            )
                                )),
               conditionalPanel(condition = "input.chartType == 'Pitchers' & input.compType == 'Same Player Over Time'",
                                tabsetPanel(tabPanel("Pitcher Comparison Over Time", 
                                                     div(style = "position:relative", plotOutput(outputId = "coolplot_pitchers_same", hover = hoverOpts("plot_hover_pitchers_same", delay = 100, delayType = "debounce")), uiOutput("hover_info_pitchers_same")),
                                                     br(), br(), br(), br(), br(), br(), DT::dataTableOutput(outputId = 'p_results_same')),
                                            tabPanel("About", 
                                                     br(), h1("About the Interactive Spray Chart Tool"), 
                                                     br(), p("This spray chart tool is maintained by Bill Petti (@BillPetti, billpetti.github.io). The data that feeds the tool comes courtesy of MLBAM and baseballsavant.com. Distance prior to 2015 is caculated based on stringer positioning of where battted balls are picked up by a fielder. Distance from 2015 to the present is based on Statcast measures unless the data was missing. In that case, caculated distance based on stringer positioning of the ball is used."),
                                                     br(), p("Please note, the data is by it's nature incomplete. I have tried to include as much data as possible, but some coordinates for batted balls are incorrect and have been removed from the tool. The plotting data is also based on human plotting and judgement. The data is also limited to those cases where information about the pitch is complete (i.e. the coordinates for where it crossed home plate)."),
                                                     br(), p("Data begins with the 2010 season and is updated daily during the season."),
                                                     br(), p("This is a very large app, so it may take a while to initially load. For now, multiple selections will execute one by one, so if you are removing a number of pitch types the charts will reload sequentially for each type you remove. I am working on optimiizing this behavior."),
                                                     h2("Definitions"), 
                                                     br(), p("Barrel's are defined by MLBAM and are based on the combination of launch angle and launch speed off the bat. Barrel % is simply the percentage of batted balls displayed that fall into the category of a barrel"), 
                                                     br(), p("Estimated Weighted On-base Average (wOBA) is also based on launch angle and launch speed off the bat. It was derived by MLBAM and places a run expectancy value on different combinations of launch speed and angle."),
                                                     br(), p("Spray Zone takes the field of play and breaks it into 13 zones based on the horizontal angle of the ball off the bat, relative to dead center field. From left field to right field, the first zone is 1 (so, anything less than -55 degrees relative to home plate) and the last zone is 13 (anything greater than 55 degrees relative to home plate). Zone 7 represents balls hit between -5 and 5 degrees relative to home plate."),
                                                     h2("Links"), 
                                                     br(), a("Barrel", href = "http://m.mlb.com/glossary/statcast/barrel"),
                                                     br(), a("Estimated wOBA", href = "http://m.mlb.com/glossary/statcast/expected-woba"), 
                                                     br(), a("MLBAM", href = "http://www.mlbam.com"), 
                                                     br(), a("Baseball Savant", href = "http://www.mlbam.com"),
                                                     br(), a("Click here for the original tool (data through 2016--does not include Statcast data)", href = "http://public.tableau.com/views/SprayChartComparison/About?%3Aembed=y&%3AshowVizHome=no"),
                                                     br(), a("My website", href = "https://billpetti.github.io"), 
                                                     br(), a("Find me on Twitter", href = "https://twitter.com/BillPetti")
                                            ))
                                , width = 9)
             )))))

server <- function(input, output, session) {
  
  shinyURL.server(session)
  
  # different players
  
  filtered <- reactive({
    
    # batters
    
    if(input$chartType == "Batters" & input$pitcher_b == "All" & input$statcast == "Include (Show Only 2015-Present)" & input$compType == "Different Players")
      
      filtered <- res %>%
        filter(Name %in% input$player_b, 
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               launch_angle >= input$l_angle[1],
               launch_angle <= input$l_angle[2], 
               launch_speed >= input$h_velo[1],
               launch_speed <= input$h_velo[2], 
               barrel_code %in% input$barrels,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone,
               as.Date(res$game_date, "%Y-%m-%d") >= input$dates[1] & 
                 as.Date(res$game_date, "%Y-%m-%d") <= input$dates[2])
    
    if(input$chartType == "Batters" & input$pitcher_b != "All" & input$statcast == "Include (Show Only 2015-Present)" & input$compType == "Different Players")
      
      filtered <- res %>%
        filter(Name %in% input$player_b, 
               `Pitcher Name` %in% input$pitcher_b,
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               launch_angle >= input$l_angle[1],
               launch_angle <= input$l_angle[2], 
               launch_speed >= input$h_velo[1],
               launch_speed <= input$h_velo[2], 
               barrel_code %in% input$barrels,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone,
               as.Date(res$game_date, "%Y-%m-%d") >= input$dates[1] & 
                 as.Date(res$game_date, "%Y-%m-%d") <= input$dates[2])
    
    if(input$chartType == "Batters" & input$pitcher_b == "All" & input$statcast != "Include (Show Only 2015-Present)" & input$compType == "Different Players")
      
      filtered <- res %>%
        filter(Name %in% input$player_b, 
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone,
               as.Date(res$game_date, "%Y-%m-%d") >= input$dates[1] & 
                 as.Date(res$game_date, "%Y-%m-%d") <= input$dates[2])
    
    if(input$chartType == "Batters" & input$pitcher_b != "All" & input$statcast != "Include (Show Only 2015-Present)" & input$compType == "Different Players")
      
      filtered <- res %>%
        filter(Name %in% input$player_b, 
               `Pitcher Name` %in% input$pitcher_b,
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone,
               as.Date(res$game_date, "%Y-%m-%d") >= input$dates[1] & 
                 as.Date(res$game_date, "%Y-%m-%d") <= input$dates[2])
    
    # pitchers
    
    if(input$chartType != "Batters" & input$player_p == "All" & input$statcast == "Include (Show Only 2015-Present)" & input$compType == "Different Players")
      
      filtered <- res %>%
        filter(`Pitcher Name` %in% input$pitcher_p, 
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               launch_angle >= input$l_angle[1],
               launch_angle <= input$l_angle[2], 
               launch_speed >= input$h_velo[1],
               launch_speed <= input$h_velo[2], 
               barrel_code %in% input$barrels,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone,
               as.Date(res$game_date, "%Y-%m-%d") >= input$dates[1] & 
                 as.Date(res$game_date, "%Y-%m-%d") <= input$dates[2])
    
    if(input$chartType != "Batters" & input$player_p != "All" & input$statcast == "Include (Show Only 2015-Present)" & input$compType == "Different Players")
      
      filtered <- res %>%
        filter(Name %in% input$player_p,
               `Pitcher Name` %in% input$pitcher_p,
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               launch_angle >= input$l_angle[1],
               launch_angle <= input$l_angle[2],
               launch_speed >= input$h_velo[1],
               launch_speed <= input$h_velo[2],
               barrel_code %in% input$barrels,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone,
               as.Date(res$game_date, "%Y-%m-%d") >= input$dates[1] &
                 as.Date(res$game_date, "%Y-%m-%d") <= input$dates[2])
    
    if(input$chartType != "Batters" & input$player_p == "All" & input$statcast != "Include (Show Only 2015-Present)" & input$compType == "Different Players")
      
      filtered <- res %>%
        filter(`Pitcher Name` %in% input$pitcher_p,
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone,
               as.Date(res$game_date, "%Y-%m-%d") >= input$dates[1] &
                 as.Date(res$game_date, "%Y-%m-%d") <= input$dates[2])
    
    if(input$chartType != "Batters" & input$player_p != "All" & input$statcast != "Include (Show Only 2015-Present)" & input$compType == "Different Players")
      
      filtered <- res %>%
        filter(`Pitcher Name` %in% input$pitcher_p,
               Name %in% input$player_p,
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone,
               as.Date(res$game_date, "%Y-%m-%d") >= input$dates[1] &
                 as.Date(res$game_date, "%Y-%m-%d") <= input$dates[2])
    
    # same player
    
    # batters
    
    if(input$chartType == "Batters" & input$pitcher_b_s == "All" & input$statcast == "Include (Show Only 2015-Present)" & input$compType == "Same Player Over Time")
      
      filtered <- res %>%
        mutate(`Date Range` = ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp1[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp1[2], paste0(input$datescomp1[1], " to ", input$datescomp1[2]), ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp2[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp2[2], paste0(input$datescomp2[1], " to ", input$datescomp2[2]), "remove"))) %>%
        filter(Name %in% input$player_b_s, 
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               launch_angle >= input$l_angle[1],
               launch_angle <= input$l_angle[2], 
               launch_speed >= input$h_velo[1],
               launch_speed <= input$h_velo[2], 
               barrel_code %in% input$barrels,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone, 
               `Date Range` != "remove")
    
    if(input$chartType == "Batters" & input$pitcher_b_s != "All" & input$statcast == "Include (Show Only 2015-Present)" & input$compType == "Same Player Over Time")
      
      filtered <- res %>%
        mutate(`Date Range` = ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp1[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp1[2], paste0(input$datescomp1[1], " to ", input$datescomp1[2]), ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp2[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp2[2], paste0(input$datescomp2[1], " to ", input$datescomp2[2]), "remove"))) %>%
        filter(Name %in% input$player_b_s, 
               `Pitcher Name` %in% input$pitcher_b_s,
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               launch_angle >= input$l_angle[1],
               launch_angle <= input$l_angle[2], 
               launch_speed >= input$h_velo[1],
               launch_speed <= input$h_velo[2], 
               barrel_code %in% input$barrels,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone, 
               `Date Range` != "remove")
    
    if(input$chartType == "Batters" & input$pitcher_b_s == "All" & input$statcast != "Include (Show Only 2015-Present)" & input$compType == "Same Player Over Time")
      
      filtered <- res %>%
        mutate(`Date Range` = ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp1[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp1[2], paste0(input$datescomp1[1], " to ", input$datescomp1[2]), ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp2[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp2[2], paste0(input$datescomp2[1], " to ", input$datescomp2[2]), "remove"))) %>%
        filter(Name %in% input$player_b_s, 
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone, 
               `Date Range` != "remove")
    
    if(input$chartType == "Batters" & input$pitcher_b_s != "All" & input$statcast != "Include (Show Only 2015-Present)" & input$compType == "Same Player Over Time")
      
      filtered <- res %>%
        mutate(`Date Range` = ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp1[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp1[2], paste0(input$datescomp1[1], " to ", input$datescomp1[2]), ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp2[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp2[2], paste0(input$datescomp2[1], " to ", input$datescomp2[2]), "remove"))) %>%
        filter(Name %in% input$player_b_s, 
               `Pitcher Name` %in% input$pitcher_b_s,
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone, 
               `Date Range` != "remove")
    
    # pitchers
    
    if(input$chartType != "Batters" & input$player_p_s == "All" & input$statcast == "Include (Show Only 2015-Present)" & input$compType == "Same Player Over Time")
      
      filtered <- res %>%
        mutate(`Date Range` = ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp1[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp1[2], paste0(input$datescomp1[1], " to ", input$datescomp1[2]), ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp2[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp2[2], paste0(input$datescomp2[1], " to ", input$datescomp2[2]), "remove"))) %>%
        filter(`Pitcher Name` %in% input$pitcher_p_s, 
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               launch_angle >= input$l_angle[1],
               launch_angle <= input$l_angle[2], 
               launch_speed >= input$h_velo[1],
               launch_speed <= input$h_velo[2], 
               barrel_code %in% input$barrels,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone, 
               `Date Range` != "remove")
    
    if(input$chartType != "Batters" & input$player_p_s != "All" & input$statcast == "Include (Show Only 2015-Present)" & input$compType == "Same Player Over Time")
      
      filtered <- res %>%
        mutate(`Date Range` = ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp1[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp1[2], paste0(input$datescomp1[1], " to ", input$datescomp1[2]), ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp2[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp2[2], paste0(input$datescomp2[1], " to ", input$datescomp2[2]), "remove"))) %>%
        filter(Name %in% input$player_p_s,
               `Pitcher Name` %in% input$pitcher_p_s,
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               launch_angle >= input$l_angle[1],
               launch_angle <= input$l_angle[2],
               launch_speed >= input$h_velo[1],
               launch_speed <= input$h_velo[2],
               barrel_code %in% input$barrels,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone, 
               `Date Range` != "remove")
    
    if(input$chartType != "Batters" & input$player_p_s == "All" & input$statcast != "Include (Show Only 2015-Present)" & input$compType == "Same Player Over Time")
      
      filtered <- res %>%
        mutate(`Date Range` = ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp1[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp1[2], paste0(input$datescomp1[1], " to ", input$datescomp1[2]), ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp2[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp2[2], paste0(input$datescomp2[1], " to ", input$datescomp2[2]), "remove"))) %>%
        filter(`Pitcher Name` %in% input$pitcher_p_s,
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone, 
               `Date Range` != "remove")
    
    if(input$chartType != "Batters" & input$player_p_s != "All" & input$statcast != "Include (Show Only 2015-Present)" & input$compType == "Same Player Over Time")
      
      filtered <- res %>%
        mutate(`Date Range` = ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp1[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp1[2], paste0(input$datescomp1[1], " to ", input$datescomp1[2]), ifelse(as.Date(game_date, "%Y-%m-%d") >= input$datescomp2[1] & as.Date(game_date, "%Y-%m-%d") <= input$datescomp2[2], paste0(input$datescomp2[1], " to ", input$datescomp2[2]), "remove"))) %>%
        filter(`Pitcher Name` %in% input$pitcher_p,
               Name %in% input$player_p_s,
               stand %in% input$stand_b,
               p_throws %in% input$p_throws,
               pitch_type %in% input$pitchType,
               release_speed >= input$p_velo[1],
               release_speed <= input$p_velo[2],
               Outcome_Type %in% input$outcome,
               plate_x >= input$p_horizontal[1],
               plate_x <= input$p_horizontal[2],
               plate_z >= input$p_vertical[1],
               plate_z <= input$p_vertical[2],
               Zone %in% input$zone, 
               `Date Range` != "remove")
    
    filtered
    
  })
  
  # different players
  # hitters 
  
  output$coolplot_hitters <- renderPlot({
    
    outcome_palette <- c("1B" = "#A2C8EC", "2B" = "#006BA4", "3B" = "#FF940E", "Out" = "#595959", "HR" = "#C85200") 
    
    g <- ggplot(filtered(), aes(hc_x, hc_y_rotated)) +
      geom_point(aes(fill = Outcome_Type), color = "grey20", alpha = .75, shape = 21, size = 2, stroke = 1) +
      xlim(0,250) +
      ylim(-250, 0) +
      geom_curve(x = 33, xend = 223, y = -100, yend = -100, curvature = -.65) +
      geom_segment(x=128, xend = 33, y=-208, yend = -100) +
      geom_segment(x=128, xend = 223, y=-208, yend = -100) +
      geom_curve(x = 83, xend = 173, y = -155, yend = -156, curvature = -.65, linetype = "dotted") +
      scale_fill_manual(values = outcome_palette, "Outcomes", drop = FALSE) + 
      coord_fixed() +
      labs(caption = "Designed and maintained by Bill Petti\nbillpetti.github.io\nData courtesy of MLBAM/baseballsavant.com") +
      theme_battedball_grey() +
      theme(strip.text.x = element_text(face = "bold", size = 22), 
            legend.position = "bottom", legend.text = element_text(size = 17),
            legend.title = element_text(size = 17))
    
    if (length(input$player_b != 1))
      return(g + facet_wrap(~Name, ncol = 2, nrow = 1))
    
    return(g)
  }, height = 525, width = 1000)
  
  output$b_results <- DT::renderDataTable({
    
    table <- filtered() %>%
      group_by(Name) %>%
      summarise(`Batted Balls` = n(), 
                `Barrel %` = round(mean(barrel, na.rm = TRUE), 3), 
                `Average Launch Angle (degrees)` = round(mean(launch_angle, na.rm = TRUE), 1), 
                `Average Launch Speed (mph)` = round(mean(launch_speed, na.rm = TRUE), 1),
                `Estimated wOBA on Contact` = round(mean(estimated_woba_using_speedangle, na.rm = TRUE), 3),
                `Actual wOBA on Contact` = round(mean(woba_value, na.rm = TRUE), 3), 
                `SLG on Contact` = round((sum(hit_type, na.rm = TRUE)/`Batted Balls`), 3))
    
  }, options = list(dom = 't',
                    rownames = FALSE, 
                    autoWidth = FALSE,
                    columnDefs = list(list(width = '100px', targets = 1), list(className = 'dt-center', targets = 2:8))
  )
  )
  
  # pitchers 
  
  output$coolplot_pitchers <- renderPlot({
    
    outcome_palette <- c("1B" = "#A2C8EC", "2B" = "#006BA4", "3B" = "#FF940E", "Out" = "#595959", "HR" = "#C85200") 
    
    g <- ggplot(filtered(), aes(hc_x, hc_y_rotated)) +
      geom_point(aes(fill = Outcome_Type), color = "grey20", alpha = .75, shape = 21, size = 2, stroke = 1) +
      xlim(0,250) +
      ylim(-250, 0) +
      geom_curve(x = 33, xend = 223, y = -100, yend = -100, curvature = -.65) +
      geom_segment(x=128, xend = 33, y=-208, yend = -100) +
      geom_segment(x=128, xend = 223, y=-208, yend = -100) +
      geom_curve(x = 83, xend = 173, y = -155, yend = -156, curvature = -.65, linetype = "dotted") +
      scale_fill_manual(values = outcome_palette, "Outcomes", drop = FALSE) + 
      coord_fixed() +
      labs(caption = "Designed and maintained by Bill Petti\nbillpetti.github.io\nData courtesy of MLBAM/baseballsavant.com") +
      theme_battedball_grey() +
      theme(strip.text.x = element_text(face = "bold", size = 22), 
            legend.position = "bottom", legend.text = element_text(size = 17),
            legend.title = element_text(size = 17))
    
    if (length(input$pitcher_p != 1))
      return(g + facet_wrap(~`Pitcher Name`, ncol = 2, nrow = 1))
    
    return(g)
  }, height = 525, width = 1000)
  
  output$p_results <- DT::renderDataTable({
    
    table <- filtered() %>%
      group_by(`Pitcher Name`) %>%
      summarise(`Batted Balls` = n(), 
                `Barrel %` = round(mean(barrel, na.rm = TRUE), 3), 
                `Average Launch Angle (degrees) Against` = round(mean(launch_angle, na.rm = TRUE), 1), 
                `Average Launch Speed (mph) Against` = round(mean(launch_speed, na.rm = TRUE), 1),
                `Estimated wOBA Against on Contact` = round(mean(estimated_woba_using_speedangle, na.rm = TRUE), 3),
                `Actual wOBA Against on Contact` = round(mean(woba_value, na.rm = TRUE), 3), 
                `SLG Against on Contact` = round((sum(hit_type, na.rm = TRUE)/`Batted Balls`), 3))
    
  }, options = list(dom = 't',
                    rownames = FALSE, 
                    autoWidth = FALSE,
                    columnDefs = list(list(width = '100px', targets = 1), list(className = 'dt-center', targets = 2:8))
  )
  )
  
  output$hover_info_batters <- renderUI({
    hover <- input$plot_hover_batters
    point <- nearPoints(filtered(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Date: </b>", point$game_date, "<br/>",
                    "<b> Batter: </b>", point$Name, "<br/>",
                    "<b> Pitcher: </b>", point$`Pitcher Name`, "<br/>",
                    "<b> Outcome: </b>", point$Outcome_Type, "<br/>",
                    "<b> Distance (ft.): </b>", point$estimated_distance, "<br/>",
                    "<b> Launch Angle (degrees): </b>", point$launch_angle, "<br/>", 
                    "<b> Launch Speed (mph): </b>", point$launch_speed, "<br/>", 
                    "<b> Pitch Velocity (mph): </b>", point$release_speed, "<br/>",
                    "<b> Horizontal Pitch Location (ft.): </b>", point$plate_x, "<br/>",
                    "<b> Vertical Pitch Location (ft.): </b>", point$plate_z, "<br/>",
                    "<b> Estimated wOBA: </b>", point$estimated_woba_using_speedangle, "<br/>",
                    "<b> Type of Contact: </b>", point$barrel_code, "<br/>"
      ))))
  })
  
  output$hover_info_pitchers <- renderUI({
    hover <- input$plot_hover_pitchers
    point <- nearPoints(filtered(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Date: </b>", point$game_date, "<br/>",
                    "<b> Batter: </b>", point$Name, "<br/>",
                    "<b> Pitcher: </b>", point$`Pitcher Name`, "<br/>",
                    "<b> Outcome: </b>", point$Outcome_Type, "<br/>",
                    "<b> Distance (ft.): </b>", point$estimated_distance, "<br/>",
                    "<b> Launch Angle (degrees): </b>", point$launch_angle, "<br/>", 
                    "<b> Launch Speed (mph): </b>", point$launch_speed, "<br/>", 
                    "<b> Pitch Velocity (mph): </b>", point$release_speed, "<br/>",
                    "<b> Horizontal Pitch Location (ft.): </b>", point$plate_x, "<br/>",
                    "<b> Vertical Pitch Location (ft.): </b>", point$plate_z, "<br/>",
                    "<b> Estimated wOBA: </b>", point$estimated_woba_using_speedangle, "<br/>",
                    "<b> Type of Contact: </b>", point$barrel_code, "<br/>"
      ))))
  })  
  
  # same player
  # hitters 
  
  output$coolplot_hitters_same <- renderPlot({
    
    outcome_palette <- c("1B" = "#A2C8EC", "2B" = "#006BA4", "3B" = "#FF940E", "Out" = "#595959", "HR" = "#C85200") 
    
    g <- ggplot(filtered(), aes(hc_x, hc_y_rotated)) +
      geom_point(aes(fill = Outcome_Type), color = "grey20", alpha = .75, shape = 21, size = 2, stroke = 1) +
      xlim(0,250) +
      ylim(-250, 0) +
      geom_curve(x = 33, xend = 223, y = -100, yend = -100, curvature = -.65) +
      geom_segment(x=128, xend = 33, y=-208, yend = -100) +
      geom_segment(x=128, xend = 223, y=-208, yend = -100) +
      geom_curve(x = 83, xend = 173, y = -155, yend = -156, curvature = -.65, linetype = "dotted") +
      scale_fill_manual(values = outcome_palette, "Outcomes", drop = FALSE) + 
      coord_fixed() +
      labs(caption = "Designed and maintained by Bill Petti\nbillpetti.github.io\nData courtesy of MLBAM/baseballsavant.com") +
      theme_battedball_grey() +
      theme(strip.text.x = element_text(face = "bold", size = 18), 
            legend.position = "bottom", legend.text = element_text(size = 17),
            legend.title = element_text(size = 17)) +
      facet_wrap(~`Date Range`, ncol = 2, nrow = 1)
    
    return(g)
  }, height = 525, width = 1000)
  
  output$b_results_same <- DT::renderDataTable({
    
    table <- filtered() %>%
      group_by(`Date Range`) %>%
      summarise(`Batted Balls` = n(), 
                `Barrel %` = round(mean(barrel, na.rm = TRUE), 3), 
                `Average Launch Angle (degrees)` = round(mean(launch_angle, na.rm = TRUE), 1), 
                `Average Launch Speed (mph)` = round(mean(launch_speed, na.rm = TRUE), 1),
                `Estimated wOBA on Contact` = round(mean(estimated_woba_using_speedangle, na.rm = TRUE), 3),
                `Actual wOBA on Contact` = round(mean(woba_value, na.rm = TRUE), 3), 
                `SLG on Contact` = round((sum(hit_type, na.rm = TRUE)/`Batted Balls`), 3))
    
  }, options = list(dom = 't',
                    rownames = FALSE, 
                    autoWidth = FALSE,
                    columnDefs = list(list(width = '100px', targets = 1), list(className = 'dt-center', targets = 2:8))
  )
  )
  
  # pitchers 
  
  output$coolplot_pitchers_same <- renderPlot({
    
    outcome_palette <- c("1B" = "#A2C8EC", "2B" = "#006BA4", "3B" = "#FF940E", "Out" = "#595959", "HR" = "#C85200") 
    
    g <- ggplot(filtered(), aes(hc_x, hc_y_rotated)) +
      geom_point(aes(fill = Outcome_Type), color = "grey20", alpha = .75, shape = 21, size = 2, stroke = 1) +
      xlim(0,250) +
      ylim(-250, 0) +
      geom_curve(x = 33, xend = 223, y = -100, yend = -100, curvature = -.65) +
      geom_segment(x=128, xend = 33, y=-208, yend = -100) +
      geom_segment(x=128, xend = 223, y=-208, yend = -100) +
      geom_curve(x = 83, xend = 173, y = -155, yend = -156, curvature = -.65, linetype = "dotted") +
      scale_fill_manual(values = outcome_palette, "Outcomes", drop = FALSE) + 
      coord_fixed() +
      labs(caption = "Designed and maintained by Bill Petti\nbillpetti.github.io\nData courtesy of MLBAM/baseballsavant.com") +
      theme_battedball_grey() +
      theme(strip.text.x = element_text(face = "bold", size = 18), 
            legend.position = "bottom", legend.text = element_text(size = 17),
            legend.title = element_text(size = 17)) +
      facet_wrap(~`Date Range`, ncol = 2, nrow = 1)
    
    return(g)
  }, height = 525, width = 1000)
  
  output$p_results_same <- DT::renderDataTable({
    
    table <- filtered() %>%
      group_by(`Date Range`) %>%
      summarise(`Batted Balls` = n(), 
                `Barrel %` = round(mean(barrel, na.rm = TRUE), 3), 
                `Average Launch Angle (degrees) Against` = round(mean(launch_angle, na.rm = TRUE), 1), 
                `Average Launch Speed (mph) Against` = round(mean(launch_speed, na.rm = TRUE), 1),
                `Estimated wOBA Against on Contact` = round(mean(estimated_woba_using_speedangle, na.rm = TRUE), 3),
                `Actual wOBA Against on Contact` = round(mean(woba_value, na.rm = TRUE), 3), 
                `SLG Against on Contact` = round((sum(hit_type, na.rm = TRUE)/`Batted Balls`), 3))
    
  }, options = list(dom = 't',
                    rownames = FALSE, 
                    autoWidth = FALSE,
                    columnDefs = list(list(width = '100px', targets = 1), list(className = 'dt-center', targets = 2:8))
  )
  )
  
  output$hover_info_batters_same <- renderUI({
    hover <- input$plot_hover_batters_same
    point <- nearPoints(filtered(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Date: </b>", point$game_date, "<br/>",
                    "<b> Batter: </b>", point$Name, "<br/>",
                    "<b> Pitcher: </b>", point$`Pitcher Name`, "<br/>",
                    "<b> Outcome: </b>", point$Outcome_Type, "<br/>",
                    "<b> Distance (ft.): </b>", point$estimated_distance, "<br/>",
                    "<b> Launch Angle (degrees): </b>", point$launch_angle, "<br/>", 
                    "<b> Launch Speed (mph): </b>", point$launch_speed, "<br/>", 
                    "<b> Pitch Velocity (mph): </b>", point$release_speed, "<br/>",
                    "<b> Horizontal Pitch Location (ft.): </b>", point$plate_x, "<br/>",
                    "<b> Vertical Pitch Location (ft.): </b>", point$plate_z, "<br/>",
                    "<b> Estimated wOBA: </b>", point$estimated_woba_using_speedangle, "<br/>",
                    "<b> Type of Contact: </b>", point$barrel_code, "<br/>"
      ))))
  })
  
  output$hover_info_pitchers_same <- renderUI({
    hover <- input$plot_hover_pitchers_same
    point <- nearPoints(filtered(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Date: </b>", point$game_date, "<br/>",
                    "<b> Batter: </b>", point$Name, "<br/>",
                    "<b> Pitcher: </b>", point$`Pitcher Name`, "<br/>",
                    "<b> Outcome: </b>", point$Outcome_Type, "<br/>",
                    "<b> Distance (ft.): </b>", point$estimated_distance, "<br/>",
                    "<b> Launch Angle (degrees): </b>", point$launch_angle, "<br/>", 
                    "<b> Launch Speed (mph): </b>", point$launch_speed, "<br/>", 
                    "<b> Pitch Velocity (mph): </b>", point$release_speed, "<br/>",
                    "<b> Horizontal Pitch Location (ft.): </b>", point$plate_x, "<br/>",
                    "<b> Vertical Pitch Location (ft.): </b>", point$plate_z, "<br/>",
                    "<b> Estimated wOBA: </b>", point$estimated_woba_using_speedangle, "<br/>",
                    "<b> Type of Contact: </b>", point$barrel_code, "<br/>"
      ))))
  })  
  
  
}

shinyApp(ui = ui, server = server)
