# Install packages
install.packages(c("tidyverse","sf","tmap","lubridate", "leaflet", "DT", "scales"))

# load packages
lapply(c( "tidyverse", "sf", "tmap", "lubridate","leaflet", "DT", "scales"), library, character.only = TRUE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Simple features -------------------------------------------------------------------------------------------------------------------

# What do points, strings polygon , multipolygon look like
point <- st_point(c(13, 15))
plot(point)

points <- rbind(c(10,10), c(10,9), c(11,9), c(11,10))
mp <- st_multipoint(points)
plot(mp)

line <- st_linestring(mp)
plot(line)

points2 <- rbind(c(11,11), c(12,11))
mls <- st_multilinestring(list(points, points2))
plot(mls)

pol_points <- rbind(c(10,10), c(10,9), c(11,9), c(11,10), c(10,10))
pol <- st_polygon(list(pol_points))
plot(pol)


pol_points2 <- rbind(c(12,12), c(13,13), c(14,12), c(12,12))
mpol <- st_multipolygon(list(list(pol_points), list(pol_points2)))
plot(mpol)

gc <- st_geometrycollection(list(point, mpol, mls))
plot(gc)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Choropleth data set up -----------------------------------------------------------------------------------------------------------------

# full resolution link (THIS MAY RUN A LITTLE SLOW)
# https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_April_2019_Boundaries_EW_BFC/FeatureServer/0/query?where=ctyua19cd%20like%20'%25E%25'&outFields=*&outSR=4326&f=geojson


# download England Shape file
england_UA <- sf::st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Counties_and_Unitary_Authorities_April_2019_Boundaries_GB_BUC/MapServer/0/query?where=UPPER(ctyua19cd)%20like%20'%25E%25'&outFields=*&outSR=4326&f=geojson")

# Downlaod covid inections data
infections <- read.csv(url("http://www.arcgis.com/sharing/rest/content/items/b684319181f94875a6879bbc833ca3a6/data"),header = TRUE)

# load ONS codes 
ons_codes <- read.csv(file = "ons_codes.csv", header = TRUE)

# left join infectiosn data to the relvant location in England
england_cases <- left_join(x = england_UA,
                           y = infections[,c(1,3)],
                           by = c("ctyua19cd"="GSS_CD"))

# left join the region of each loaction in England
england_cases <- left_join(x = england_cases,
                           y = ons_codes[,c(2:3)],
                           by = c("ctyua19cd"="organisation_id"))

# Add column that states if authority is in London or not
england_cases <- england_cases %>% mutate(roe = ifelse(test = england_cases$region_name == "London",yes = "London",no = "Rest of England"))

# add column with abbrevbiated unitary authority name 
england_cases$name_abbr <- abbreviate(england_cases$ctyua19nm)

# Divide "Hackney & City of London" infection numbers into respective local authorities "Hackney" and "City of London"
# population of Hackney 280,000. population of CoL 10,000. 10/280 = 0.035

Hackney_CoL <- england_cases[which(england_cases$ctyua19nm == "Hackney"),][["TotalCases"]]

england_cases[which(england_cases$ctyua19nm == "Hackney"),][["TotalCases"]] <- round(Hackney_CoL * (1-0.035))
england_cases[which(england_cases$ctyua19nm == "City of London"),][["TotalCases"]] <- round(Hackney_CoL * 0.035)
 
# have a look at the map
england_cases  %>%
  tm_shape() +
  tm_borders(lwd = 1, lty = 1)

# something not quite right about the shape of the map. What?
st_is_valid(england_cases, reason = TRUE)

# Which location is row 134?
england_cases[[134,3]]

# Have a look at Hampshire. What is the matter?
hampshire <- england_cases  %>% filter(ctyua19nm == "Hampshire") %>%
  tm_shape()+
  tm_polygons()

# Save a large image so that we can see the "Ring Self-intersection" error
tmap_save (tm = hampshire, filename = "hamshire.svg", units =  "px", width = 10000,height = 10000)




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# LEAFLET PACKAGE (interactive maps) -----------------------------------------------------------------------------------------------

# leaflet translates into javascript for you

leaflet(england_cases) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addPolygons(weight = 1, fillOpacity = 0.2 )

leaflet(england_cases) %>%
  addTiles() %>%
  addPolygons(weight = 1, fillOpacity = 0.2)

leaflet(england_cases) %>%
  addTiles() %>%
  addPolygons(weight = 1, fillOpacity = 0.2 )%>%
  addMiniMap()

# create a colour scale based on the data in the TotalCases column
pal <- colorNumeric(palette = c("green", "orange", "red"), domain =  england_cases$TotalCases)
pal(100)

leaflet(england_cases) %>%
  addTiles() %>%
  addPolygons(weight = 1,color = ~pal(TotalCases),fillOpacity = 0.5)%>%
  addMiniMap()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# GGPLOT2 --------------------------------------------------------------------------------------------------------------------------

england_cases %>%
ggplot(aes(x = long, y = lat)) +
  geom_sf(aes(fill = TotalCases)) + # the counties
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  theme_classic()

england_cases %>% filter(region_name == "London") %>%
  ggplot(aes(x = long, y = lat)) +
  geom_sf(aes(fill = TotalCases)) + # the counties
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  theme_classic()

england_cases %>%
  ggplot(aes(x = long, y = lat)) +
  geom_sf(aes(fill = TotalCases)) + # the counties
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  theme_classic() +
  facet_wrap(~region_name) #,scales = "free")

england_cases %>%
  ggplot(aes(x = long, y = lat)) +
  geom_sf(aes(fill = TotalCases)) + # the counties
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  theme_classic() +
  facet_wrap(~roe) #,scales = "free")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# TMAP -----------------------------------------------------------------------------------------------------------------------------


# break_up <- round(as.numeric(summary(england_cases$TotalCases)[1:6])) # <-- could be used to split data for scales

london <- england_cases %>% filter(region_name == "London")%>%
  tm_shape() +
  tm_fill("TotalCases", style = "cont",  title = "Covid infections") +
  tm_borders(lwd = 1, lty = 1) +
  tm_text("name_abbr", size = 0.4, col = "black")+
  tm_style("col_blind")+
  tm_layout(frame = FALSE,legend.title.size= 0.5,legend.text.size = 0.4, legend.position = c("left","bottom"))

tmap_save (tm = london, filename = "london.svg", units =  "px", width = 1000,height = 1000)

# compare London with East of England
Lond_Vs_EoE <- england_cases %>% filter(region_name == "London"|region_name == "East of England")%>%
  tm_shape() +
  tm_fill("TotalCases", style = "cont",  title = "Covid infections") +
  tm_facets(by = "region_name")+
  tm_style("col_blind")+
  tm_borders(lwd = 1, lty = 1) +
  tm_text("name_abbr", size = 0.3, col = "black")+
  tm_layout(legend.outside.position = "bottom" , legend.outside.size = .2)

tmap_save (tm = Lond_Vs_EoE, filename = "Lond_Vs_EoE.svg", units =  "px", width = 1000,height = 1000)

ldn_vs_roe <- england_cases %>%
  tm_shape() +
  tm_fill("TotalCases", style = "cont",  title = "Covid infections") +
  tm_facets(by = "roe")+
  tm_style("col_blind")+
  tm_borders(lwd = 1, lty = 1) +
  #tm_text("name_abbr", size = 0.5, col = "black") +
  tm_layout(legend.outside.position = "bottom" , legend.outside.size = .2)

tmap_save (tm = ldn_vs_roe, filename = "ldn_vs_roe.svg", units =  "px", width = 1000,height = 1000)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
