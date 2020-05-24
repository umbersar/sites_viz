# sites_viz

## Pacakges
The following packages are used for this project:

```
* shiny
* shinydashboard
* leaflet
* dplyr
* plyr
* sf
* sp
* leafgl
* DT
```

**Note**

The package `leafgl` is used in conjunction with `leaflet` to create maps. 
It delivers fantastic performance. 

While `leaflet` is great, it does need 
help when it comes to performance, 
especially when the number of data points that we have is astronomical.
The reason why `leaflet` suffers from having many data points is that 
each point is an individual DOM element. 
Browsers simply cannot cope well with an HTML page that's got thousands of DOM elements. 

The downside with `leafgl` is that it is pre-alpha and 
under heavy development at the moment.
As such, I recommend download a working version as early as possible. 
Here is a link to `leafgl`'s git repo: 
https://github.com/r-spatial/leafgl

In the case that `leafgl` can no longer function, 
I would sugget future developers to look into adding each point (marker) onto a canvas first 
and then render the canvas. This avoids having tens of thousands of DOM elements.

## Structure
```
ui.R
server.R
data/
src/
```
For the time being, all the datasets are provided in a `.csv` format and are 
stored within the `src/` folder. Every dataset has the following three columns:

* `X`: index of a data point, which we use as ID for each data point
* `o_longitude_GDA94` 
* `o_latitude_GDA94`

In the `src/` folder, R scripts that ui.R and server.R depend on can be found.

To run the app, open either `ui.R` or `server.R` in RStudio. After the file has been opened, 
you will find a special `Run App` button on the top right corner of the script. 
Clicking on `Run App` will start up the application. 


Alternatively, one could use `shift + command + return` on Mac or `shift + control + enter` on Windows.



For more details on the project, please refer to the design document.