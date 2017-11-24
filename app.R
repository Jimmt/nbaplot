library(shiny)
library(dplyr)
library(plotly)
library(httr)
library(jsonlite)
library(stringr)

year <- "2017"

teams.request <- GET(paste0("http://data.nba.net/10s/prod/v1/", year, "/teams.json"))
teams <- flatten(as.data.frame(fromJSON(content(teams.request, "text"))))
players.request <- GET(paste0("http://data.nba.net/10s/prod/v1/", year, "/players.json"))
players <- flatten(as.data.frame(fromJSON(content(players.request, "text"))))

players <- players %>% mutate(name = paste(league.standard.firstName, league.standard.lastName))
teams <- teams %>% filter(league.standard.isNBAFranchise == TRUE)
stats <- scan("stats.txt", what="", sep="\n")

my.ui <- fluidPage(theme = "style.css",
  headerPanel("NBA Graphs"),
  
  sidebarLayout(  
    sidebarPanel(  
      selectInput("team", "Team", choices = teams$league.standard.fullName, selected = "Dallas Mavericks"),
      htmlOutput("selectPlayer"),
      selectInput("stat", "Stat", choices = stats, selected = stats[1])
    ),
    mainPanel(   
      htmlOutput("playerPic"),
      plotlyOutput("plot")
    )
  )
)

my.server <- function(input, output){
  # options(warn = 2) 
  output$selectPlayer <- renderUI({
    url.name <- teams %>% filter(league.standard.fullName == input$team) %>% select(league.standard.urlName)
    roster.request <- GET(paste0("http://data.nba.net/data/10s/prod/v1/", year, "/teams/", url.name, "/roster.json"));
    roster <- flatten(as.data.frame(fromJSON(content(roster.request, "text"))))
    roster.players <- roster %>% left_join(players, "league.standard.personId")
  # View(roster.players)
    selectInput("player", 
                "Player", 
                choices = roster.players$name, selected = "JJ Barea")
  })
  
  output$playerPic <- renderText({
    if(!is.null(input$player)){
      row <- players %>% filter(name == input$player)
      last.name <- gsub("\\.", "", row$league.standard.lastName)
      last.name <- gsub(" ", "_", last.name)
      first.name <- gsub("\\.", "", row$league.standard.firstName) 
      first.name <- gsub(" ", "_", first.name)
      src <- paste0("https://nba-players.herokuapp.com/players/", last.name, "/", first.name)
      c("<center><img src='", src, "'></center>")
    }
 }) 

  output$plot <- renderPlotly({
    if(!is.null(input$player)){
      player.id <- players %>% filter(name == input$player) %>% select(league.standard.personId)
      single.player.request <- GET(paste0("http://data.nba.net//data/10s/prod/v1/", year ,"/players/", player.id, "_profile.json"))
      single.player <- flatten(as.data.frame(fromJSON(content(single.player.request, "text"))))
      single.player <- single.player %>% select(starts_with("league.standard.stats.regularSeason"))
      names(single.player) <- str_replace(names(single.player), "league.standard.stats.regularSeason.season.", "")

      stat.category <- single.player[, 2 + match(input$stat, stats)]
      print(match(input$stat, stats))
      plot_ly(data = single.player, type = "scatter", mode = "lines+markers", x = ~seasonYear, y = as.numeric(stat.category), hoverinfo = "y") %>%
        layout(xaxis = list(title = "Year"), yaxis = list(title = input$stat))
    } else {
      plot_ly(type = "scatter", mode = "lines", x = c(), y = c())
    }
  })
}

shinyApp(ui = my.ui, server = my.server)
