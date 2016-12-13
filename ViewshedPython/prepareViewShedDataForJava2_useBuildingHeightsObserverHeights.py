#Create data ready for the Java intervisibility network code to chew on
#Observer points will be used to create rasters.
#They'll be buffered at 2.75km and, for any within single-part dissolved buffers
#A single output raster will be made covering them all so a 15km radius can be searched
from PyQt4.QtCore import *
from PyQt4.QtGui import *
from qgis.core import *
from qgis.gui import *
from qgis.analysis import *
from doViewshedHack import *
import processing
import timeit
import os

def run_script(iface):

	#time whole thing
	start = time.time()

	print(os.chdir('C:/Data/WindFarmViewShed'))
	#print(processing.alglist('buffer'))

	#Empty raster folder first. It won't overwrite if there and I see no overwrite option in the merge algorithm. I keep on forgetting it's not overwriting...
	#ViewShedJava/SimpleViewShed/data/rasters/
	#ViewShedJava/SimpleViewShed/data/rasters_w_buildingheight/

	#load turbines
	

	# turbines = QgsVectorLayer(
	# 	'C:/Data/WindFarmViewShed/Tests/PythonTests/testData/turbineInNameAll.shp',
	# 	'turbines','ogr')
	# print(turbines.isValid())

	#Load from CSV
	#Relative uri paths nope!
	uri = "file:///C:/Data/WindFarmViewShed/ViewshedPython/Data/turbinesFinal_reducedColumns_tipHeightsComplete.csv?type=csv&xField=Feature.Easting&yField=Feature.Northing&spatialIndex=no&subsetIndex=no&watchFile=no&crs=EPSG:27700"
	#run on pre-selected subset
	#uri = "file:///C:/Data/WindFarmViewShed/ViewshedPython/Data/turbines_subset.csv?type=csv&xField=Feature.Easting&yField=Feature.Northing&spatialIndex=no&subsetIndex=no&watchFile=no&crs=EPSG:27700"
	lyr = QgsVectorLayer(uri,'turbinescsv','delimitedtext')
	print(lyr.isValid())

	#subset turbines for testing
	# turbines = QgsVectorLayer('Point?crs=EPSG:27700', 'turbinesJavaDataPrep', 'memory')
	# print(turbines.isValid())

	# pr = turbines.dataProvider()
	
	# #give new layer matching fields so the feature addition keeps the original values
	# pr.addAttributes(lyr.fields())			
	# turbines.updateFields()
	
	# #If there's only one polygon part, it defaults to not being multipart but it doesn't break. 
	# #So. if testing, at least one turbine needs to be far enough away to make a multipart.
	# #features = lyr.getFeatures(QgsFeatureRequest().setFilterExpression( u'"X" in (2138,2545)' ))

	# #Picked for being in different batches but overlapping housing selection
	# features = lyr.getFeatures(QgsFeatureRequest().setFilterExpression( u'"index" in (2357,2222)' ))
	# # features = lyr.getFeatures(QgsFeatureRequest().setFilterExpression( u'"X" in (2138,1783,1936,2545)' ))
	# # features = turbines.getFeatures(QgsFeatureRequest().setFilterExpression( u'"Location" = \'Wick\'' ))

	# pr.addFeatures([feature for feature in features])

	#or keep them all
	turbines = lyr

	#Try a shapefile. The above doesn't seem to allow multipart to single
	# turbines = QgsVectorLayer(
	# 	'C:/Data/WindFarmViewShed/Tests/PythonTests/testData/turbineSample.shp',
	# 	# 'C:/Data/WindFarmViewShed/Tests/PythonTests/testData/turbineInNameAll.shp',
	# 	'turbines','ogr')
	# print(turbines.isValid())

	#QgsMapLayerRegistry.instance().addMapLayers([turbines])

	######################
	#load houses
	# houses = QgsVectorLayer('C:/Data/WindFarmViewShed/Tests/PythonTests/testData/rawGeocodedNewRoS2.shp','houses','ogr')
	# print(houses.isValid())

	#Load from CSV
	#uri = "file:///C:/Data/WindFarmViewShed/ViewshedPython/Data/geocodedOldNewRoS.csv?type=csv&xField=newRoS_eastings&yField=newRoS_northings&spatialIndex=no&subsetIndex=no&watchFile=no&crs=EPSG:27700"
	uri = "file:///C:/Data/WindFarmViewShed/ViewshedPython/Data/houses_finalMay2016.csv?type=csv&xField=eastingsFinal&yField=northingsFinal&spatialIndex=no&subsetIndex=no&watchFile=no&crs=EPSG:27700"

	houses = QgsVectorLayer(uri,'housesJavaDataPrep','delimitedtext')
	print(houses.isValid())



	# for field in houses.fields():
	# 	print(field.typeName())

	# houses.updateExtents()
	# QgsMapLayerRegistry.instance().addMapLayers([houses])


	#CREATE SPATIAL INDEX FOR HOUSING DATA
	#make spatial index of housing points - use to quickly reduce points for PiP test
	#To buffer bounding box
	#Adapted from
	#http://nathanw.net/2013/01/04/using-a-qgis-spatial-index-to-speed-up-your-code/

	before = time.time()
	print('Starting spatial index...')

	#dictionary comprehension. e.g. http://www.diveintopython3.net/comprehensions.html
	#Creates dictionary of IDs and their qgsFeatures. Nice!

	allfeatures = {feature.id(): feature for (feature) in houses.getFeatures()}

	# print(type(allfeatures))#dict
	# print(type(allfeatures[1111]))#qgis._core.QgsFeature

	index = QgsSpatialIndex()
	map(index.insertFeature, allfeatures.values())

	print('Spatial index done: ' + str(time.time() - before))

	##############
	#Load DEM terrain data footprint for getting file refs
	#(made in createVectorBritishNationalGridRefLayer.py)
	#Load version with CEDA building data flag
	footprint = QgsVectorLayer(
		'C:/Data/MapPolygons/Generated/NationalGrid5kmSquares_for_OSterrain/NationalGrid5kmSquares_for_OSterrain_w_CEDAflag.shp','footprint','ogr')
	print(footprint.isValid())



	####################
	# 1. Create 2.75km dissolved/single-part buffer features around observer points
	# To identify groups of observers to work with.
	# This is to save on processing time/disc space for raster creation
	# E.g down from ~2600 individual rasters to ~140.
	#This'll dissolve. We still need to turn them into single parts
	geometryanalyzer = QgsGeometryAnalyzer()
	
	geometryanalyzer.buffer(turbines, 
		'C:/Data/WindFarmViewShed/Tests/PythonTests/testData/SinglePart_2point75kmBuffers.shp', 
		2750, False, True, -1)

	parts = QgsVectorLayer(
		'C:/Data/WindFarmViewShed/Tests/PythonTests/testData/SinglePart_2point75kmBuffers.shp',
		'parts','ogr')
	print(parts.isValid())

	#QgsMapLayerRegistry.instance().addMapLayers([parts])

	#Just the one currently!
	print(parts.featureCount())

	#But many geometries
	#http://gis.stackexchange.com/questions/138163/exploding-multipart-features-in-qgis-using-python
	onePart = parts.getFeatures().next()
	geom = onePart.geometry()

	geoms = []

	for poly in geom.asMultiPolygon():
		print type(poly)

		#Surely a better way! I can't seem to drop the last method
		wktstuff = QgsGeometry.fromPolygon(poly).exportToWkt()
		# wktstuff = QgsGeometry.fromMultiPolygon(poly)
		gem = QgsGeometry.fromWkt(wktstuff)

		print type(wktstuff)
		print type(gem)

		geoms.append(gem)

		

	print 'Number of buffers that will have their own observer/target/raster set: ' + str(len(geoms))

	# # New layer to stick the single-parts into
	# buffLyr = QgsVectorLayer('Polygon?crs=EPSG:27700', 'buffbuff', 
	# 	'C:/Data/WindFarmViewShed/Tests/PythonTests/testData/separatePolygons.shp')
	buffLyr = QgsVectorLayer('Polygon?crs=EPSG:27700', 'buffbuff','memory')
	# buffLyr = QgsVectorLayer('C:/Data/WindFarmViewShed/Tests/PythonTests/testData/separatePolygons.shp','turbines','ogr')
	print(buffLyr.isValid())

	pr = buffLyr.dataProvider()
	# print type(pr)

	for thing in geoms:

		# print type(thing)

		b = QgsFeature()

		# print type(b)

		b.setGeometry(thing)
		pr.addFeatures([b])

	#QgsVectorFileWriter(buffLyr, "buffzz", pr.fields(), QGis.WKBPoint, pr.crs(), "ESRI Shapfile")

	#Check in QGIS
	#QgsMapLayerRegistry.instance().addMapLayers([buffLyr])


	####################
	# polygon subset function with flag for spatial index
	def GetFeaturesInBuffer(inputbuff, layer, newlayername, spatialindex = 0):
		
		if spatialindex != 0:

			print 'Using spatial index...'

			#Use spatial index made above to get bounding box of housing data quickly
			#Before subsetting
			ids = spatialindex.intersects(inputbuff.geometry().boundingBox())

			#Use those IDs to make a new layer
			box = QgsVectorLayer('Point?crs=EPSG:27700', newlayername, 'memory')

			pr = box.dataProvider()
			#f = houses.

			#give new layer matching fields so the feature addition keeps the original values
			pr.addAttributes(layer.fields())			
			box.updateFields()			
			
			#Get the features in the bounding box by feature id that we should got from the spatial index check
			#http://gis.stackexchange.com/questions/130439/how-to-efficiently-access-the-features-returned-by-qgsspatialindex
			request = QgsFeatureRequest()	
			request.setFilterFids(ids)

			subset = layer.getFeatures(request)
			blob = [feature for feature in subset]

			#Add those features to housesInBox layer
			pr.addFeatures(blob)

			#replace input layer with box. Which should work with function scope, right?
			#layer reference is pointer to original at this level, can be overwritten...
			layer = box

		featuresInBuffer = QgsVectorLayer('Point?crs=EPSG:27700', newlayername, 'memory')
		pr = featuresInBuffer.dataProvider()

		# print(type(layer.fields()))

		#give new layer matching fields so the feature addition keeps the original values
		pr.addAttributes(layer.fields())			
		featuresInBuffer.updateFields()
		
		#So let's use geometry intersect instead.

		# for feature in housesInBox.getFeatures():
		# 	if feature.geometry().intersects(inputbuff):
		# 		pr.addFeatures([feature])

		#should be 433 points... yup!
		#print(housesInBuffer.featureCount())

		#Now can I do that in one line? Yup!
		features = [feature for feature in layer.getFeatures() if feature.geometry().intersects(inputbuff.geometry())]

		print('Number of features in this buffer: ' + str(len(features)))

		# print(type(features))
		# print(type(features[0]))
		
		pr.addFeatures(features)

		#check if features have orig attributes...
		#for ft in features:
		#	print('feat: ' + ft.attributes()[1])

		return(featuresInBuffer)


	#############################
	# CYCLE THROUGH THESE BUFFERS TO SUBSET HOUSING AND TURBINE DATA
	# AND OUTPUT RASTERS

	for buffr in buffLyr.getFeatures():

		print('id:' + str(buffr.id()))

		#Cheat to look at the one we want...
		if buffr.id()  < 35:
			continue

		#####################
		# TURBINES FIRST
		# As we'll use these to create larger 15km buffers that select houses and rasters
		before = time.time()
		print('Starting turbine intersect...')

		turbinesInBuffer = GetFeaturesInBuffer(buffr, turbines, 'TurbinesInBuffer')

		# turbinesInBuffer.updateExtents()
		# QgsMapLayerRegistry.instance().addMapLayers([turbinesInBuffer])

		print('Turbine intersect done: ' + str(time.time() - before))

		filename = ('ViewShedJava/SimpleViewShed/data/observers/' + str(buffr.id()) + '.csv')
		# filename = ('C:/Data/WindFarmViewShed/Tests/PythonTests/testData/turbines/' + str(buffr.id()) + '.csv')

		#Save as CSV with coordinates
		QgsVectorFileWriter.writeAsVectorFormat(turbinesInBuffer, filename, "utf-8", None, "CSV", layerOptions ='GEOMETRY=AS_WKT')

		#Use these to make the new larger viewshed buffer
		geometryanalyzer.buffer(turbinesInBuffer, 
		# 'Data/temp/15kmBuffer.shp', 
		'ViewShedJava/SimpleViewShed/data/temp/15kmBuffer' + str (buffr.id()) + '.shp', 
		15000, False, True, -1)

		#viewBuff = 0

		viewBuff = QgsVectorLayer(
			# 'Data/temp/15kmBuffer.shp', 
			'ViewShedJava/SimpleViewShed/data/temp/15kmBuffer' + str (buffr.id()) + '.shp',
			'viewbuffer','ogr')
		print(viewBuff.isValid())

		# viewBuff.updateExtents()
		# QgsMapLayerRegistry.instance().addMapLayers([viewBuff])

		#USE VIEWBUFF TO SUBSET HOUSES AND RASTER GRID
		before = time.time()
		print('Starting housing intersect...')

		# housesInBuffer = getHousesInBufferZone()
		# housesInBuffer = GetFeaturesInBuffer(houses, 'housesInBuffer')
		# #Use spatial index
		#Snicker.
		bigBuff = viewBuff.getFeatures().next()


		housesInBuffer = GetFeaturesInBuffer(bigBuff, houses, 'housesInBuffer', index)

		# housesInBuffer.updateExtents()
		# QgsMapLayerRegistry.instance().addMapLayers([housesInBuffer])

		print('Housing intersect done: ' + str(time.time() - before))

		# filename = ('C:/Data/WindFarmViewShed/Tests/PythonTests/testData/housing/' + str(buffr.id()) + '.csv')
		filename = ('ViewShedJava/SimpleViewShed/data/targets/' + str(buffr.id()) + '.csv')

		#Save as CSV with coordinates
		QgsVectorFileWriter.writeAsVectorFormat(housesInBuffer, filename, "utf-8", None, "CSV", layerOptions ='GEOMETRY=AS_WKT')




		# ################
		# # OUTPUT RASTERS
		def writeVirtualRaster(filenametif, bh_flag):

			# First we need the list of filenames from the buffer intersect with the footprint file
			squares = [feature for feature in footprint.getFeatures() 
				if feature.geometry().intersects(bigBuff.geometry())
				and feature.attributes()[4] != NULL]

			#This is where the filename/grid ref is.
			print('Number of DEM squares in this buffer: ' + str(len(squares)))

			#Create virtual raster from the list of filenames
			#Note, this is unicode: print(type(squares[0].attributes()[4]))
			#convert unicode to string
			#http://stackoverflow.com/questions/1207457/convert-a-unicode-string-to-a-string-in-python-containing-extra-symbols
			

			if bh_flag:

				# listOfFiles = [
				# 	'C:/Data/Terrain5_OS_DEM_Scotland/Zips/allRasterFilesShared/' +
				# 	(square.attributes()[4].encode('ascii','ignore') + 
				# 	'.asc') if square.attributes()[5] == 0 else
				# 	'C:/Data/BuildingHeight_alpha/rasters/' +
				# 	(square.attributes()[4].encode('ascii','ignore') + 
				# 	'.tif')
				# 	for square in squares]

				#CEDA building height version, two switches: is there building height data? Is it mastermap or CEDA?
				#(Having already deferred to mastermap in the grid flagging)
				listOfFiles = [
					'C:/Data/Terrain5_OS_DEM_Scotland/Zips/allRasterFilesShared/' +
					(square.attributes()[4].encode('ascii','ignore') + 
					'.asc') if square.attributes()[5] == 0 else
					('C:/Data/BuildingHeight_alpha/rasters/' +
					(square.attributes()[4].encode('ascii','ignore') + 
					'.tif') if square.attributes()[6] == 1 else 
					 'C:/Data/BuildingHeight_CEDA/rasters/' +
					(square.attributes()[4].encode('ascii','ignore') + 
					'.tif'))
					for square in squares]

			else:
				listOfFiles = [
				'C:/Data/Terrain5_OS_DEM_Scotland/Zips/allRasterFilesShared/' +
				(square.attributes()[4].encode('ascii','ignore') + 
				'.asc') 
				for square in squares]


			print "list of DEM files we're gonna attempt to get:"

			for file in listOfFiles:
				print file

			#Use turbine feature ID as reference
			#And add 
			# filename = ('ViewShedJava/SimpleViewShed/data/rasters/' +
			# str(buffr.id()) + 
			# '.vrt')

			#Oh good - can take lists too!
			#processing.runalg('gdalogr:buildvirtualraster', listOfFiles, 0, False, False, filename)
			#Output direct to merged tif


			#Merge!
			processing.runalg('gdalogr:merge', listOfFiles, False, False, 5, filenametif)



			#Reload the tif to get the coordinate metadata and write into the filename
			#Cos trying to get it in Java is horrific

		

		###########
		# First DEM
		before = time.time()

		filenametif1 = ('ViewShedJava/SimpleViewShed/data/rasters/' +
			str(buffr.id()) + 
			'.tif')

		#print filenametif

		#Flag is for "save with building height data where available"
		writeVirtualRaster(filenametif1, False)
		print(str('DEM raster ' + str(buffr.id())) + ': ' + str(time.time() - before) + ' seconds to merge')


		########
		#Re-run again for building height version
		before = time.time()

		filenametif2 = ('ViewShedJava/SimpleViewShed/data/rasters_w_buildingheight/' +
			str(buffr.id()) + 
			'.tif')

		#print filenametif

		#Flag is for "save with building height data where available"
		writeVirtualRaster(filenametif2, True)
		print(str('DEM-plus-buildings raster ' + str(buffr.id())) + ': ' + str(time.time() - before) + ' seconds to merge')


		#############
		#reload to get coord metadata, store in own file
		#Use non-building-height version as we'll also get DEM info while here
		
		raster = QgsRasterLayer(filenametif1,'getcoords')
		print raster.isValid()

		# index = 0

		# for line in lines:
		# 	print str(index) + ': ' + line
		# 	index += 1
		# print raster.metadata()

		#line 30 for extent coords
		#bottom-left : top-right
		lines = raster.metadata().splitlines()
		#We just need bottom left
		line = lines[30].split(":")[0]
		# #strip out <p> tag
		line  = line.replace('<p>','')

		# print line

		# write in own file
		text_file = open(
			"C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/coords/" + str(buffr.id()) + ".txt",
			"w")
		text_file.write(line)
		text_file.close()


	print('total time: ' + str((time.time() - start)/60) + ' minutes')
