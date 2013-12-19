UNDP-BCPR-Project
=================

Exploratory Data Analysis of Actors in Georgian Conflict

These are some initial data probing graphs and cleaning of data, to eventually arrive at a conclusion about agents causing the conflicts in the Georgian 2012 elections

To run Actor1 and Actor2 Map visualization:

- clone the repo in its entirety
- cd into "actorsMap" from the terminal
- make sure that the .html, .csv and .json files are there (do not rename any of these)
- start your own server. From the command line, still under the actorsMap/ directory, type and run:
 "python -m SimpleHTTPServer 8008"
    - this will start a local server in port 8008
- Open your browser and go to port 8008 by typing into the URL "http://localhost:8008/map.html"
 
About this map:
- Hovering on the countries shows their name and average goldstein number, dragging the map moves the globe
- Countries are colored by their ratio of appearance as Actor1 or Actor2 within the years 2011-2013
    - the darkest purples mean that the country has a low ratio
    - the darkest greens mean that the country has a high ratio
