library(shiny)


about <- list(
  about_one <- p("This dashboard is developed using the 
           R package Shiny to provide researchers an easy platform to play with 
           the soil datasets that CSIRO is responsible for managing."),
  
  about_two <- p("For researchers who do not have much R programming experiences or 
               simply cannot commit the amount of time needed to write 
               complex R scripts, it would be very handy for them to have an 
               application that is built on top of R and does the dirty job for them."),
  
  about_three <- p("And that's exactly what this dashboard is trying to deliver. 
                 With this dashboard, a researcher can do the following easily:"),
  
  about_three_list <- tags$ol(
    tags$li("Load a dataset of interest;"), 
    tags$li("View basic summary information about that dataset;"), 
    tags$li("View the data points from the selected dataset in a data table;"),
    tags$li("Filter the dataset by meaningful columns and the values in those columns;"),
    tags$li("View the data points on an interactive map according to their geolocations;"),
    tags$li("Select areas of a map and have the data points in the selected areas displayed;"),
    tags$li("Zoom in and out of the map and have the data table updated accordingly"),
    tags$li("Choose between different plots and generate them dynamically.")
  )
)

how_to <- list(
  how_to_one <- p("To get started, simply click on the \"Explore\" tab on the left."),
  how_to_two <- p("Please note that after collapsing or expanding the tab menu, 
                the data points on the map may appear to have disappeared. 
                Simply move around the map and the data points shall reappear.")
)
