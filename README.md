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
app.R
/data
/features
```
For the time being, all the datasets are provided in a `.csv` format and are 
stored within the `/data` folder.


In the `/features` folder, you can find features that are being developed and tested, 
such as high-performance maps and advanced data tables.


To run the app, open `app.R` in RStudio. After the file has been opened, 
you will find a special `Run App` button on the top right corner of the script. 
Clicking on `Run App` will start up the application. 


Alternatively, one could use `shift + command + return` on Mac or `shift + control + enter` on Windows.
