#Load lines of sight produced in viewShed_R/lines_houseToTurbine15km.R
#To check if line crosses any building height data
from PyQt4.QtCore import *
from PyQt4.QtGui import *
from qgis.core import *
from qgis.gui import *
from qgis.analysis import *
from doViewshedHack import *
import processing
import fnmatch
import shutil
import timeit
import os
import timeit


def run_script(iface):

	start = time.time()

	print(os.chdir('C:/Data/temp/QGIS'))
	#print(processing.alglist('line'))

	#Get all shapefile names
	matches = []

	#Get folder names with shapefiles in
	for root, dirnames, filenames in os.walk('linesOfSightShapefiles'):
		for filename in fnmatch.filter(filenames, '*.shp'):
			matches.append(filename)

	print ("Total line shapefiles: " + str(len(matches)))

	#matches = matches[2450:2452]

	for match in matches:
		print match
		#print match.split(".")[0]

	######
	# GET THE TWO POLYGON LAYERS TO INTERSECT LINES OF SIGHT WITH
	mastermapGrid = QgsVectorLayer(
		'C:/Data/WindFarmViewShed/QGIS/ReportOutputs/BH_mastermap_grid.shp',
		'mastermapGrid','ogr')
	print(mastermapGrid.isValid())

	CEDA_convexHulls = QgsVectorLayer(
		'C:/Data/BuildingHeight_CEDA/ConvexHulls/dissolve_Intersect_w_GRID_convexHullsOverLayerID.shp',
		'mastermapGrid','ogr')
	print(CEDA_convexHulls.isValid())

	#cycle over line files
	for match in matches:

		before = time.time()

		#Get pre-formed line shapefile from R
		linez = QgsVectorLayer(
			('C:/Data/temp/QGIS/linesOfSightShapefiles/' + match),
			'mastermapGrid','ogr')
		print(linez.isValid())

		#See if lines cross any areas where there could be building height data
		processing.runalg('saga:linepolygonintersection', linez, mastermapGrid, 1, ("C:/Data/temp/QGIS/linesOfSightIntersects_mastermap/" + match.split(".")[0] + ".csv"))
		processing.runalg('saga:linepolygonintersection', linez, CEDA_convexHulls, 1, ("C:/Data/temp/QGIS/linesOfSightIntersects_CEDA/" + match.split(".")[0] + ".csv"))

		print(('saved : ' + match + ", ") + str(time.time() - before) + " seconds. " + str((time.time() - start)/60) + " mins total.")


	print("Total time: " + str((time.time() - start)/60) + " mins")

