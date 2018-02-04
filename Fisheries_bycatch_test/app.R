library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
load("fisheries_master_table_corrected.Rdata")


# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  tags$h1("NOP Bycatch Interactive Viewer"),
  tags$br(), # line break
  
  # Application title -----------------------------------------------
  titlePanel("Bycatch Patterns"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
  
    
    # FROM HERE
    
    
    
      
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("IMDB rating" = "imdb_rating", 
                              "IMDB number of votes" = "imdb_num_votes", 
                              "Critics Score" = "critics_score", 
                              "Audience Score" = "audience_score", 
                              "Runtime" = "runtime"), 
                  selected = "audience_score"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("IMDB rating" = "imdb_rating", 
                              "IMDB number of votes" = "imdb_num_votes", 
                              "Critics Score" = "critics_score", 
                              "Audience Score" = "audience_score", 
                              "Runtime" = "runtime"), 
                  selected = "critics_score"),
      
      # Select variable for color -----------------------------------
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("Title Type" = "title_type", 
                              "Genre" = "genre", 
                              "MPAA Rating" = "mpaa_rating", 
                              "Critics Rating" = "critics_rating", 
                              "Audience Rating" = "audience_rating"),
                  selected = "mpaa_rating"),
      
      # Set alpha level ---------------------------------------------
      sliderInput(inputId = "alpha", 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      # Set point size ----------------------------------------------
      sliderInput(inputId = "size", 
                  label = "Size:", 
                  min = 0, max = 5, 
                  value = 2),
      
      # Enter text for plot title ---------------------------------------------
      # UI REMEMBER THIS IS IN THE SIDEBAR PANEL
      textInput(inputId = "plot_title",
                label = "Plot title",
                placeholder = "Enter text"),
      
      # Horizontal line for visual separation -----------------------
      hr(),
      
      # Select which types of movies to plot ------------------------
      checkboxGroupInput(inputId = "selected_type",
                         label = "Select movie type(s):",
                         choices = c("Documentary", "Feature Film", "TV Movie"),
                         selected = "Feature Film"),
      
      # Select sample size ----------------------------------------------------
      numericInput(inputId = "n_samp", 
                   label = "Sample size:", 
                   min = 1, max = nrow(movies), 
                   value = 50)
    ),
    
    # Output: -------------------------------------------------------
    mainPanel(
      
      tags$img(src = "https://scontent-lax3-2.xx.fbcdn.net/v/t1.0-9/22814071_10155860578604993_7372161764712795307_n.jpg?oh=e738c3f4cb97d8bbe5f6275feeb92b83&oe=5AEA8E5D", 
               width = "400px", height = "400px"),
      
      
      # Show scatterplot --------------------------------------------
      plotOutput(outputId = "scatterplot"),
      br(),        # a little bit of visual separation
      
      # Print number of obs plotted ---------------------------------
      uiOutput(outputId = "n"),
      br(), br(),    # a little bit of visual separation
      
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "moviestable")
    )
  )
)