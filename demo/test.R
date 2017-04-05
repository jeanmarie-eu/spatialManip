library(spatialManip)

# datasets
library(gridding)
library(catchment)
library(river)
data(catchment_norway)
data(river_norway)
data(seNorge)

##############################
# extract one spatial object #
##############################

# spatial polygons
tmp_catchment <- extract(obj=catchment_norway, indice=10,name="test")
sp::plot(tmp_catchment)

# spatial lines
tmp_river <- extract(obj=river_norway, indice=10,name="test")
sp::plot(tmp_river)

# spatial grid
tmp_seNorge <- extract(obj=seNorge, indice=list(offset=c(1,1),count=c(100,50)))
sp::plot(tmp_seNorge)


###############################
# minmax                      #
###############################
minmax(catchment_norway)
minmax(catchment_norway,2)
minmax(tmp_catchment)
minmax(river_norway)
minmax(river_norway,4)
minmax(tmp_river)
minmax(seNorge)

##############################
# cellDim                    #
##############################

cellDim(catchment_norway,cellsize=1000,4)
cellDim(tmp_catchment,cellsize=1000)
cellDim(tmp_river,cellsize=1000)

##############################
# location                   #
##############################

points <- matrix(0,1,2)
points[1,] <- c(0,7000000)
location(seNorge,points)

###########
# contain #
###########

# spatial grid
i_seNorge <- contain(tmp_catchment,seNorge)

# spatial lines
i_river <- contain(tmp_catchment,river_norway)
i_nonRiver <- contain(tmp_catchment,river_norway,REVERSE=TRUE)


######################################################
# spatial2grid: build a grid that contain the object #
######################################################

# spatial polygons
tmp <- spatial2grid(obj=tmp_catchment,cellsize=c(1000,1000))
sp::plot(tmp)

# spatial lines
tmp <- spatial2grid(obj=tmp_river,cellsize=c(1000,1000))
sp::plot(tmp)

#########################
# build  spatial object #
#########################

# a spatial grid
tmp <- minmax(seNorge)
x <- xGridBuild(xmin=tmp[1,1],xmax=tmp[1,2],ymin=tmp[2,1],ymax=tmp[2,2],cellsize=20000)
tmp_norge <- construct(type="grid",x,proj4S=crs("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")) #+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs
sp::plot(tmp_norge)


##########
# plot   #
##########
library(sp)

coordsLim <- sp::bbox(tmp_catchment)
sp::plot(tmp_catchment,asp=1,xlim=coordsLim[1,],ylim=coordsLim[2,])
points(coordinates(seNorge)[i_seNorge,])
box()
axis(2, ylim=coordsLim[2,])
mtext("UTM 33 North (meters)",side=2,line=2.5)
axis(1, ylim=coordsLim[1,])
mtext("UTM 33 East (meters)",side=1,line=2.5)
