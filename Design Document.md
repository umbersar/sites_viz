[TOC]

This is the design document for soil data visulisation web dashboard, Soil Data Explorer, which is written in R and Shiny. In this design document, we introduce the project structure, the functionalities, and the underlying design patterns.

## Project Structure

The project is organised as below:

```markdown
data/
src/
ui.R
server.R
README.md
```

At the root level, we have two folders, two R scripts, and the `README` markdown file. Our web dashboard, a Shiny application, simply consists of these two R scirpts. As the names suggest, `ui.R` defines the ui layout of the dashboard, and `server.R` defines the "server side" logic of the web dashboard. More on this later. 

As for the two folders, the `data/` folder stores all the datasets in `.csv` format and the `src/` folder is used to store all the R scripts that `ui.R` and `server.R` use and reply on. 

**Note** 

All the .csv files in the `data/` folder must have the columns: `X`, `o_longitude_GDA94`, and `o_latitude_GDA94`. The column `X`, which stores all the indexes in natural number form, serves as an ID for each column. As such, we do not  allow missing values in `X`. The columns `o_longitude_GDA94` and `o_latitude_GDA94` however allow missing values. In fact, all the data points with incomplete geolocations are filtered out and can be viewed as an independent data table in the dashboard. 

When a new .csv dataset is added to the `data/` folder, its name needs be added to the `datasets.R` script, located in the `src/` folder. 

## Functionalities 

In this section, we break down the different sections provided in Soil Data Explorer dashboard.

### Data Selection

`Data Selection` section allows the user to load a dataset from a set of datasets. When a dataset is loaded, the data points are breaking into two datatables:

1. A datatable for data points with a missing or incomplete geolocation, and
2. A main datatable with data points that have a complete geolocation

The UI layout is as follows.

![Data Selection](./images/data_selection.png)

<center>Data Selection UI</center>

Data is only loaded after the `Load Dataset` button is clicked. 

### DataTable Data Summary

In the `DataTable Data Summary` section, we have a dropdown select input whose options are the names of the columns in the main datatable. After a column has been selected, by clicking on the `Show Summary` button, a basic statistical summary of the selected column will show up. 


As can be seen from the figures above, when the `Show Summary` button is clicked, the button text changes to `Hide Summary`.

### Dataset Trimming

As all the data points come from relational data tables, it is understandable that not every column is going to be meaningful. Moreover, some datasets might contain too many columns, distracting a user from focusing on the ones that matter. Therefore, we want to offer the user the flexibility to hide any columns of his or her choice.  

In the dashboard, by selecting the names of the columns and clicking on the `Hide Selected Columns` button, the selected columns will be hidden. This applies to both datatables in the dashboard. When the download button is clicked, only the current visible columns will be included.

To undo the "data trimming", simply remove all the selected columns and click the `Hide Selected Columns` afterwards.

### Data Filtering

In this section, we allow a user to filter the current datatable further by selecting a column and a value from that column as the filter. For example, a user may only wish to see all the data points from the agency 199. 

<p style="text-align:center;">
  <img src="./images/data_filter_before.png"/>
  <br>
  <em>Data Filtering - Before Click</em>
</p>


Notice that a user can choose between applying the filter to the whole dataset and applying the filter to only the current datatable. When the `Apply Filter` button is clicked, the data points that meet the filter criteria will be highlighted on the geo map (which will be introduced later) and displayed in the datatable. 

<p style="text-align:center;">
  <img src="./images/data_filter_map_after.png"/>
  <br>
  <em>Geo Map - After Filtering</em>
</p>


After a filter has been applied, the button will change from `Apply Filter` to `Remove Filter`. By clicking on the `Remove Filter` button, any applied filter will be removed. The highlighted data points on the geo map will of course be removed accordingly.

### Geolocation Map

Geolocation Map is used to display all the data points wtih complete and valid geolocations in the loaded dataset. 

<p style="text-align:center;">
  <img src="./images/geo_map_normal.png"/>
  <br>
  <em>Geo Map</em>
</p>




Moreover, this map supports the following advanced features:

1. Allow a user to select areas of the map: when there are data points in the selected data points, the selected data points will be highlighted and that the datatable will be updated accordingly. As a conscious decision, we do not allow data points to be selected by multiple shapes. When overlaps occur, the user will be prompted. 

2.  When a filter is being applied, the data points that meet the filter criteria will be highlighted. 

3. As a user moves the map, this includes zooming in and out, the main datatable will be updated: those data points in the current map bound will be displayed. The exceptions to this general rule are:

   i.  When a filter is being applied, moving the map around will further filter the datatable.

   ii. When there are areas of the map being selected, moving around the map will not update the datatable. 

<p style="text-align:center;">
  <image src="./images/geo_map_zoom.gif"></image>
  <br>
  <em>Geo Map - Zoom In and Out</em>
</p>


### DataTable

For the two datatables, besides the built-in functions like page length options and pagination, download buttons are attached so that a user can download any of the two datatables in their current states.

For the `Missing Geolocations` section, the datatable for all the data points with incomplete or missing geolocations is initially hidden. A user can view the datatable by clicking on the `Show Incomplete Geolocations` button to display the datatable. After the datatable is shown, the button will change to `Hide Incomplete Geolocations`. 



### Plot

As for the plot section, a user can choose between the various plots and generate any one of them with the current set of data points in the main datatable. 

<p style="text-align:center;">
  <image src="./images/plot_selection.gif"></image>
  <br>
  <em>Plot - Selection</em>
</p>


When a plot is selected, only those columns that meet the criteria of the current selected plot will be provided. For example, for a boxplot, only (continuous) numerical values make sense. As such, we filter out the columns that do not make sense for a boxplot. 

A plot is only generated when the `Plot` button is pressed. 



## Design Patterns

### UI

The following UI patterns are particularly interesting in this project:

* Show/hide UI elements on events
* Update UI on events
* Dynamically generate UI elements

<p style="text-align: center">
  <img src="./images/ui_show_n_hide.gif"/>
  <br>    
  <em>Show and Hide UI Elements</em>
</p>


It is useful to show/hide certain UI elements when certain events take place, like the pressing of a button. In Shiny, to do this, the API `conditionalPanel` comes in handy:

```R
# ui.R
# ...
conditionalPanel(
	condition = "output.condition",
  p("ui elements to show/hide on output.condition")
)
actionButton(
  inputId = "button_press",
  label = "Show"
)

# server.R
# ...
rv <- reactiveValues(
	condition = FALSE
)

output$condition <- reactive({
    rv$condition
})

observeEvent(input$button_press, {
  rv$condition <- !rv$condition
  if (rv$condition) {
    updateActionButton(session, inputId = "inputId", label = "Hide")
  }
  else {
    updateActionButton(session, inputId = "inputId", label = "Show")
  }
  outputOptions(output, "condition")
})
```

When the `input$button` event is triggered, the code block in the observeEvent function will be executed. The change of `rv$condition`, a reactive value, will trigger the change of `output$condition`. Finally, with the API `outputOptions`, we are sending the newest value `output.condition` to the UI front-end. The UI element(s) in `conditionalPanel` will be shown or hidden according to the value of `output.condition`.

Also shown in the above example is the updating of an UI element. Shiny provides APIs for updating almost all UI elements. This allows the updating of texts on buttons, choices for dropdown selection, and so on. 

Now, last but not least, dynamic generation of UI elements can be done in the following manner:

```R
# ui.R
# ...
uiOutput("plotInputs")

# server.R
# ...
observeEvent(createPlotInputs(), {
  output$plotInputs <- renderUI({
    createPlotInputs()
  })
})

createPlotInputs <- reactive({
  inputTagList <- list()
  inputTagList <- tagSetChildren(
    inputTagList, 
    selectInput(
      inputId = "inputId", 
      label = "inputLabel",
      choices = c(...)
    )
  )
  return(inputTagList)
})
```

For more details, please refer to the official Shiny documentation. 

### Event Handling and Reactivity

<p style="text-align:center;">
  <img src="./images/reactivity_chart.svg"/>
  <br>
  <em>Reactivity Diagram</em>
</p>

Besides getting each component to work smoothly and fast even with a large set of data points (more on this later), another tricky thing about the dashboard is the relationship between the different components. In the above diagram, we can see how the major components interconnect with each other. 

In contrast to static data, Shiny has this concept of reactive values. With reactive values, in combination of obsevers, we have a very intuitive pattern of automatically do something as those reactive values change.

```R
# server.R
rv <- reactiveValues(
	data = list()
)

observeEvent(rv$data, {
  # do something
})
```

In the above code snippet, whenever the reactive value `rv$data` gets changed, the code block surrounded by the curly braces inside the observeEvent will be executed.  

In our dashboard, when a dataset is loaded, the central reactive data (i.e. reactive values) will be initialised. The changes in the cenral reactive data are initiated by all the events that take place in the dashboard, like the pressing of certain buttons. When the central reactive data changes, other parts that reply on the reactive data will be updatedd accordingly and automatically with the help of the `observeEvent` observer.  

For more on reactivity, please refer to the following materials:

* https://shiny.rstudio.com/articles/reactivity-overview.html
* https://mastering-shiny.org/why-reactivity.html

### Interactive Geolocations Map

The map provided in the dashboard is powered by the JavaScript package `leaflet`. Combined with the package `leaflet.extras` package, we can create a map with drawing tools straight out of the box. More importantly, when a shape gets drawn or deleted, or when the map is zoomed or moved, the `leaflet` map sends events to `server.R`. By handling these events, we are then able to then create a fully interactive map. For example, when a new shape is drawn on the map, by handling the drawing event, which provides the bounds of the newly drawn shape, we are then able to filter out only those data points that are in the included in the newly drawn shape. Other events work in a similar manner. 

<p style="text-align:center;">
  <image src="./images/geo_map_zoom.gif"></image>
  <br>
  <em>Geo Map - Zoom In and Out</em>
</p>

<p style="text-align:center;">
  <image src="./images/geo_map_selection.gif"></image>
  <br>
  <em>Geo Map - Selection</em>
</p>

<p style="text-align:center;">
  <image src="./images/geo_map_deletion.gif"></image>
  <br>
  <em>Geo Map - Deletion</em>
</p>
As these shapes are drawn or deleted, the main datatable will update as well as these events have triggered the reactive values to update.

One major problem with `leaflet`, however, is its performance. Each shape, this includes points and markers, is its own independent HTML element. As the map runs on the client side, when we have tens of thousands of data points, it will simply overwhelming for the browser to both render the tens of thousands of HTML elements and be responsive. To get around this problem, the package `leafgl`, currently still under development, is used. With the package `leafgl`, each layer is regarded as a complete unit. This means, we can simply add data points to a layer to avoid having too many HTML elements. 

Moreover, the package `sp` is used for finding out which data points are included in a selected region quickly. For more information, please refer to the following sources:

* https://cran.r-project.org/web/packages/sp/vignettes/over.pdf
* https://github.com/r-spatial/leafgl
* https://rstudio.github.io/leaflet/shiny.html

