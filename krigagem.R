# -------------------------------------------------------------------------- #
# Topicos Especiais de Banco de Dados - EACH/USP
# Jully Anne Mota
# Marcello V. Tedardi
# Avaliacao inicial do algoritmo de krigagen
# Baseada na aula da disciplina HEP5825 - Análise de Dados Espaciais usando o R
# -------------------------------------------------------------------------- #

# Libraries
library(geoR)
library(maptools)
library(sf)
library(sp)
library(spdep)
library(MASS)
library(rgdal)
library(raster)
library(DBI)
library(odbc)
library(RPostgreSQL)
library(rpostgis)
library(gWidgets)
library(gWidgetstcltk)
library(rgeos)
library(maptools)
library(gstat)

# Connection with PostgreSQL
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = "", 
                 port = 5432,
                 user = "", 
                 host = "",
                 password = pw)
rm(pw) # removes the password

#Queries (test)
#dbGetQuery(con, "SELECT version();")
#dbGetQuery(con, "SELECT * FROM SAO_PAULO;")

# close the connection
#dbDisconnect(con)
#dbUnloadDriver(drv)
#wdbExistsTable(con, "sao_paulo")

# TRUE
#pgPostGIS(con)
sao_paulo.shp <- pgGetGeom(con, "sao_paulo")
inquerito.shp <- pgGetGeom(con, "inquerito")

plot(sao_paulo.shp)
plot(inquerito.shp)

#define o mapa
inquerito.shp <- inquerito.shp[ !(inquerito.shp$id %in% c(37, 97, 52, 77, 47, 46, 74, 78, 49, 62)), ]
inquerito.shp$id
v1 <- na.omit(inquerito.shp$lat)
v2 <- na.omit(inquerito.shp$long)
v1_name <- 'lat'
v2_name <- 'lon'


df <- data.frame(v1, v2)
names(df) <- c(v1_name, v2_name)

coordinates(df)= ~ lon+lat
proj4string(df) <- CRS("+init=epsg:4326")
df$data <- inquerito.shp$total

summary(df$data)

plot(sao_paulo.shp, axes=T)
plot(df, axes=T, add=T)
plot(TheGrid, add= T)

bubble(df, zcol='data', fill=TRUE, do.sqrt=FALSE, maxsize=3)

#df$data <- rev(df$data)

#plota os dados no espaco

#variograma
TheVariogram=variogram(data~1, data=df)
plot(TheVariogram)
TheVariogramModel <- vgm(psill=70000, model="Exp", nugget=0.0001, range=6)
plot(TheVariogram, model=TheVariogramModel) 
FittedModel <- fit.variogram(TheVariogram, model=TheVariogramModel)
plot(TheVariogram, model=FittedModel)

gr <- gridlines(df)

# Then finally, add a lat/long grid:
TheGStat <- gstat(TheGStat, id="Sine", model=FittedModel )  

Columns=seq(from=-46.8, to=-46.4, by=0.01)
Rows=seq(from=-23.8259, to=-23.4, by=0.01)

TheGrid <- expand.grid(x=Columns,y=Rows)
coordinates(TheGrid) <- ~ x+y
proj4string(TheGrid) <- CRS("+init=epsg:4326")
gridded(TheGrid) <- TRUE

plot(sao_paulo.shp, axes=T)
plot(df, axes=T, add=T)
plot(TheGrid, cex=0.5, add=T)
points(df, pch=1, col='red', cex=0.7)
title("Interpolation Grid and Sample Points")

TheSurface <- predict(TheGStat, model=FittedModel, newdata=TheGrid)
par(mar=c(2,2,2,2))
image(TheSurface, col=terrain.colors(20))
contour(TheSurface, add=TRUE, drawlabels=FALSE, col='brown')
points(TheData, pch=4, cex=0.5)
title('Prediction')



