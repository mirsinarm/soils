# This is a test of reading shapefiles into R, using rgdal.  Information obtained from:
# http://www.r-bloggers.com/things-i-forget-reading-a-shapefile-in-r-with-readogr/

library(maptools)
library(spatstat)
library(RMySQL) # http://www.r-bloggers.com/accessing-mysql-through-r/
library(dismo)
library(sp)
library(rgdal)
library(soilDB)
library(plyr)

# note:  readShapeSpatial requires the .shp, .shx, and .dbf files to be all in the same directory

##############################################################
###                         Step 1                         ###
##############################################################
# Read in the shape file, downloaded in this case from the
# Web Soil Survey:  
# http://websoilsurvey.sc.egov.usda.gov/App/WebSoilSurvey.aspx

california <- readOGR(dsn=".", layer="gsmsoilmu_a_ca")
plot(california, col=as.factor(california@data$MUKEY)) # plots each mukey separately
unique(california@data$MUKEY) # lists the soil mukeys in the Cal data


##############################################################
###                         Step 2                         ###
##############################################################
# Downloading GBIF data for a set of species in R
# Here, my list of species is just in SQL, and is a data.frame of
# the form (genus, species, serpentine) for the columns.
# It shouldn't matter if anything is upper vs. lowercase.
# The serpentine column contains basically a yes vs. no for whether
# the species occurs on serpentine.
#
# NOTE:  Here, the serpentine data was taken from Safford, Viers,
# and Harrison 2005, Madrono, "Serpentine endemism in the California
# flora: a database of serpentine affinity"
#
# However, I've only used presence in the database to indicate
# serpentine affinity, have NOT used their actual codes for strict,
# weak, etc.



# first, connect to the SQL database:
soils.db = dbConnect(MySQL(), user='root', password='', dbname='soils', host='localhost')
dbListTables(soils.db)
dbListFields(soils.db, "eriogonum")





target.genus <- "eriogonum"

# then ask for the database listing species in whatever table you want:
query <- paste("SELECT * FROM ", target.genus, sep='')
sp <- dbGetQuery(soils.db, query)
species.list <- sp[, 1:2]
data <- data.frame() # MUST INITIALIZE THIS DATAFRAME, because below I add data to it 

# actually get the GBIF data associated with the species in the table:
for (i in 1:nrow(species.list)) {
  data <- rbind(data, gbif(species.list[i, 'genus'], species.list[i, 'species'], geo=TRUE))
}

# data <- read.csv("./eriogonum_gbif.csv") # can clean up a bit with Google Refine
# data <- data[,c('species', "continent", "adm1", "adm2", "lat", "lon")]
# 
# # write the gbif data to sql
# dbWriteTable(soils.db, paste(target.genus, "_gbif", sep=''), data)



##############################################################
###                         Step 3                         ###
##############################################################
# Clean up the GBIF data.
# get the data from SQL, but 1) only selecting the species, lat, and lon, and 
# removing any lat-lon that have fewer than 5 digits

query <- paste("SELECT species, lat, lon FROM ", paste(target.genus, "_gbif ", sep=""), "WHERE char_length(lat)>6 AND char_length(lon)>7", sep='')
sp <- dbGetQuery(soils.db, query)
sp <- unique(sp) # REALLY NOT SURE that this is a good idea!!!

# prepare the matrix with species and lat-lon coordinates
loc <- data.frame(data$species, data$lat, data$lon)
colnames(loc) <- c("species", "lat", "lon")

### CLEAN THE DATA:
###  1.  remove missing data
# can also just do subset(loc, !is.na(loc$lon) & !is.na(loc$lat))

loc <- loc[complete.cases(loc), ] # gets rid of data where both lat and lon are NA
loc <- subset(loc, lon!=0)
loc <- subset(loc, lat!=0)



###  2.  remove data that has been rounded, i.e., to 35.0000, which is not a useful datapoint
###      to compare to soils
###  TO-DO: should also remove the data that are, e.g., 36.20000; maybe just grep the rows?
loc.round <- loc
loc.round$lat <- round(loc$lat)
match.lat <- loc.round$lat != loc$lat # if the rounded number does not equal the original number, then it probably wasn't an integer!
loc <- loc[match.lat, ]

loc.round <- loc
loc.round$lon <- round(loc$lon)
match.lon <- loc.round$lon != loc$lon
loc <- loc[match.lon, ]
# 
# quicksplit <- function(x) { strsplit(as.character(x), split='')}
# 
# 
# x <- loc[1, "lat"]
# 
# dec.stripper <- function(x) {
#   # get rid of any rows that have too many trailing zeroes (i.e., have fewer than 4 digits after the decimal point)
#   # first, split the latitude/longitude into before-decimal and after-decimal, and take only the after:
#   after <- strsplit(as.character(x), split='[.]')[[1]][2]
# 
#   # then, split the after and get its length: (this also helps rule out problems with - longitudes)
#   if(length(strsplit(after, split='')[[1]]) < 4) {return (FALSE)}
#   else {return (TRUE)}
# }


###  3.  fix data that has been entered incorrectly
#  IF AND ONLY IF I am certain that the data has been entered incorrectly:
#  This would be okay if I were just looking at California and thus knew the range
#  of acceptable values, BUT have to be careful if looking at a larger area
# convert data that has been entered incorrectly into the proper format:
# for(i in 1:nrow(loc)) {
#   if(loc[i, "lon"] > 0) { 
#     val <- -loc[i, 'lon']
#     loc[i, "lon"] <- val
#     print("changed value")
#   }
# }

###  4.  remove points that are very far away from the main area, i.e.,
###      in the ocean
### NOTE:  HARD-CODED ALERT!!!!!!!!!!!!!!!!!!!!
# far.away <- loc$lat > -130
# loc <- loc[far.away, ]


###  5.  remove data points with impossible values, largely because I don't
###      know how to deal with them

# if the latitude is impossible, remove it:
impossible <- loc$lat > 90
loc <- loc[!impossible, ]



##############################################################
###                         Step 4                         ###
##############################################################
#  Prepare for the actual overlapping of points with polygons!

# turn points into a maptools:::SpatialPoints
pts <- loc[, 3:2]
coordinates(pts) <- c("lon", "lat")

# set the projections to be the same
proj4string(pts) <- proj4string(california) # set the projections the same

##############################################################
###                         Step 5                         ###
##############################################################
#  Do the actual overlapping, and return the list of mukeys!

# use over() to figure out which polygon the points fall into
poly.overlap <- over(pts, as(california, "SpatialPolygons")) # returns the actual mukey, I think

# combine the mukey data with the species and coordinates:
loc2 <- cbind(loc, poly.overlap, NA)
colnames(loc2) <- c('species', 'lat', 'lon', 'poly.overlap', 'MUKEY')

# use the poly.overlap to figure out what the corresponding mukey is:
for(i in 1:length(poly.overlap)) {
  p <- poly.overlap[i] # getting the actual value of the overlapping polygon
  if(!is.na(p)) { # as long as it isn't NA
    mukey <- as.character(california@data$MUKEY[[p]])
    loc2[i, "MUKEY"] <- mukey
#     print(paste("added mukey: ", mukey, ", to: ", i, " where p = ", p, sep=""))
  }
}

# create a list of species, coords, and mukey
sp.soil <- loc2[!is.na(poly.overlap), ] # lists species, coords, and mukey
occ.mukeys <- unique(sp.soil[, "MUKEY"])


##############################################################
###                         Step 6                         ###
##############################################################
#  Look at a pretty graph to see where the components are and
#  where the GBIF points are!
plot(california)
points(sp.soil$lon, sp.soil$lat, col="RED")



##############################################################
###                         Step 7                         ###
##############################################################
#  Query the soil database to get attributes of the soils at
#  each mukey.
#
#  As a refresher:  a MUKEY is the smallest (?) area in the 
#  database, though it can have multiple components; those 
#  components can be (and usually are) different soil types, i.e.
#  serpentine or not.
#
# This is the example script from "dylan's blog", 
# http://casoilresource.lawr.ucdavis.edu/drupal/node/1031
#
# A very useful website:  http://sdmdataaccess.nrcs.usda.gov/queryhelp.aspx

#  START BY:  Converting the string of MUKEY integers into a string separated by a comma!
cs <- paste(occ.mukeys, collapse=",")

# Define the query to submit to STATSGO and get the attributes listed in the select statement
# to try to get only STATSGO results:
# comppct_r is the component percentage in that mapunit; I think it might be the "representative" % rather than the high or low
q <- paste("SELECT component.mukey, component.cokey, legend.areasymbol, comppct_r, compname, taxclname, taxorder, taxsuborder,
           taxgrtgroup, taxsubgrp, chorizon.ph1to1h2o_l, chorizon.chkey
           FROM legend
           INNER JOIN mapunit ON mapunit.lkey = legend.lkey
           LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
           LEFT OUTER JOIN chorizon ON component.cokey = chorizon.cokey
           WHERE legend.areasymbol ='US'
           AND component.mukey IN (", cs, ")")

#  Actually submit the query; returns a data.frame
res <- SDA_query(q)


##############################################################
###                         Step 8                         ###
##############################################################
###  THESE ARE NOT USED YET but probably will be soon, as I 
###  figure out a better way to figure out what soil the point
###  data occurs in ...
###
###  I stole these from somewhere, but I'm forgetting where (oops)
# 
# # function for computing the total component percentages by strata (suborder)
f.sum <- function(i) {
  n <- nrow(i)
  s <- sum(i$comppct_r)
  return(data.frame(pct=s, n=n))
}

# # function for picking the largest suborder from within each map unit (mukey)
f.largest <- function(i) {
  i.sorted <- i[order(i$pct, decreasing = TRUE), ]
  top.suborder <- i.sorted$taxsuborder[1]
  top.suborder.pct <- i.sorted$pct[1]
  return(data.frame(suborder=top.suborder, pct=top.suborder.pct))
}
# 
# # tabulate percentage of suborder-level taxa within each map unit (mukey)
comp.suborder.sums <- ddply(res, .(mukey, taxclname), f.sum, .progress='text')
# 
# # keep the largest suborder, and its associated total percentage
comp.suborder <- ddply(comp.suborder.sums, .(mukey), f.largest, .progress='text')
# 
# # tabulate occurrence of suborders ... not that this does not take into account map unit polygon area
table(comp.suborder$suborder)





##############################################################
###                         Step 9                         ###
##############################################################
#  Link the sp.soil MUKEYs for each species with the res soil information
#  THIS IS ESSENTIAL.  I tried it without doing it and it collapsed.
colnames(sp.soil) <- c("species", "lat", "lon", "poly.overlap", "mukey")
df <- merge(sp.soil, res, by="mukey")


##############################################################
###                        Step 10                         ###
##############################################################
#  Get the list of serpentine soils
serps <- grepl("SERPENT", df$taxclname) # returns a list match/notmatch for serpentine soils
serp.df <- df[serps,]

#  Save the database to SQL, because that makes it easier to go back.
dbWriteTable(soils.db, paste(target.genus, "_soil_serps", sep=''), df)



##############################################################
###                        Step 11                         ###
##############################################################
#  Summarize the results.
resnames <- c("genus", "species", "serp_from_Harrison", "num_serp_pts", "num_non_pts", "perc_pts_serp")
results <- data.frame(matrix(nrow=nrow(sp), ncol=length(resnames)))  # creates the actual results matrix
colnames(results) <- resnames

# input the genus, species, and Harrison data into the results matrix:
results[, 1:3] <- sp[]

# for each species in the ORIGINAL LIST (NOT the unique 
# names derived from GBIF, which include subspecies and stuff)
for(i in 1:nrow(sp)) {
  sp.names <- sp[i, "species"]
  
  # count the number of points in the serp.df:
  serp.matches <- sum(grepl(sp[i, "species"], serp.df$species))
  results[i, "num_serp_pts"] <- serp.matches
  
  # count the number of non-serpentine points in df:
  matches <- sum(grepl(sp[i, "species"], df$species))
  results[i, "num_non_pts"] <- matches
  
  # what percentage of points are serpentine?
  results[i, "perc_pts_serp"] <- serp.matches/matches
}

##############################################################
###                        Step 12                         ###
##############################################################
# Save the results tables to SQL

dbWriteTable(soils.db, paste(target.genus, "_results", sep=''), results) # HOWEVER, I bet there's a better way to do this in SQL without having to generate a separate table

##############################################################
###                        Step 13                         ###
##############################################################
#  Look at more pretty graphs!

plot(california)
points(df$lon, df$lat, col="BLUE")
points(serp.df$lon, serp.df$lat, col="RED")











# will return all the data from antirrhinum_results and ceanothus_results, provided that the column names are the same
q <- 'SELECT * FROM antirrhinum_results UNION SELECT * FROM ceanothus_results;'
sp <- dbGetQuery(soils.db, query)



# 
q <- 'select perc_pts_serp FROM serpentine WHERE serp_from_Harrison = "serpentine";'
serpentine <- dbGetQuery(soils.db, q)
serpentine <- subset(serpentine, !is.na(serpentine$perc_pts_serp) & serpentine$perc_pts_serp != 0)

q <- 'select perc_pts_serp FROM serpentine WHERE serp_from_Harrison = "nonserpentine";'
nonserp <- dbGetQuery(soils.db, q)
nonserp <- subset(nonserp, !is.na(nonserp$perc_pts_serp) & nonserp$perc_pts_serp != 0)


dev.off()
par(mfrow=c(2, 1))
hist(serpentine[, 1], main="Percent of occurrences on serpentine soil, SERPENTINE plants", col="RED")
hist(nonserp[, 1], main="Percent of occurrences on serpentine soil, NONSERPENTINE species", col="BLUE")


