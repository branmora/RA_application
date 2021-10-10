#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Run script for geospatial data matching

# This code performs matching between schools based on geographical distance.
# We match the nearest school with data from the National Standarized Test dataset
# to the nearest school that did not participate during the 2019 run.

# CONTENT
    # Import libraries and folders
    # Data cleaning
    # Data visualization
    # Matching based on geographical distance
    # Export datasets

# Libraries ------------------------------------------------------------------

# Importar librerias
import numpy as np
import pandas as pd
import geopandas as gpd 
import folium 
import shapely
from shapely.ops import nearest_points
from shapely.geometry import LineString
from pyprojroot import here

# We load school dataset (with x and y coordinates)
bd_nivel_ie =    pd.read_stata(here() / "data_clean.dta")

# Data cleaning --------------------------------------------------------------
# We keep relevant variables
bd_nivel_ie = bd_nivel_ie[["cod_mod_anexo","NLAT_IE","NLONG_IE",'ind_eib_lengua1', "ind_eib_lengua2","ind_lenguaje_ece_prim", "ind_mate_ece_prim","ind_lenguaje_ece_sec", "ind_mate_ece_sec", "D_NIV_MOD", "eib"]]

# We generate an indicator of participation in ECE (National Standarized test)
bd_nivel_ie['ece'] = np.where(bd_nivel_ie['ind_lenguaje_ece_prim'].isnull(), 0, 1)

# We keep target propulation (students in primary school)
primaria = (bd_nivel_ie['D_NIV_MOD'] == 'Primaria')
bd_nivel_ie = bd_nivel_ie[primaria]

# We generate datasets for schools that participated in ECE 2018 or 2019
no_ece = bd_nivel_ie["ece"] == 0
ece = bd_nivel_ie["ece"] == 1

bd_no_ece = bd_nivel_ie[no_ece]
bd_ece = bd_nivel_ie[ece]

# We convert datasets to GeoPandas
def create_gdf(df, x='NLONG_IE', y='NLAT_IE'): return gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df[y], df[x]), crs={'init':'EPSG:4326'})
no_ece_gdf = create_gdf(bd_no_ece)
ece_gdf = create_gdf(bd_ece)

# Data visualization ------------------------------------------------------------------

# We visualize the data on a map using folium library
m = folium.Map([-11.8965667, -77.043225], 
               tiles="CartoDb dark_matter")
locs_ece = zip(ece_gdf.NLAT_IE, ece_gdf.NLONG_IE)
locs_no_ece = zip(no_ece_gdf.NLAT_IE, no_ece_gdf.NLONG_IE)
for location in locs_ece: 
    folium.CircleMarker(location=location, color="red", radius=4).add_to(m)
for location in locs_no_ece: 
    folium.CircleMarker(location=location, color="white", radius = 2).add_to(m)    

m.save(here() / "map_points_primaria.html")

# Matching based on geographical distance ---------------------------------------------

# Define a function to calculate the nearest school
def calculate_nearest(row, destination, val, col="geometry"):
    dest_unary = destination["geometry"].unary_union
    nearest_geom = nearest_points(row[col], dest_unary)
    match_geom = destination.loc[destination.geometry == nearest_geom[1]]
    match_value = match_geom[val].to_numpy()[0]
    return match_value

# We generate the closest geometry
no_ece_gdf["geom_cercano"] = no_ece_gdf.apply(calculate_nearest, destination=ece_gdf, val="geometry", axis=1)
no_ece_gdf["cod_mod_anexo_cercano"] = no_ece_gdf.apply(calculate_nearest, destination=ece_gdf, val="cod_mod_anexo", axis=1)

# Create LineString Geometry
no_ece_gdf['line'] = no_ece_gdf.apply(lambda row: LineString([row['geometry'], row['geom_cercano']]), axis=1)

# Create Geodataframe Geometry 
line_gdf = no_ece_gdf[["cod_mod_anexo_cercano", "line"]].set_geometry('line')
line_gdf = line_gdf.geometry.map(lambda polygon: shapely.ops.transform(lambda x, y: (y, x), polygon)) # Swap x and y coordinates
line_gdf.crs = crs={"init":"epsg:4326"} # We determine reference coordinate

# Export datasets --------------------------------------------------------------

# We keep school ID, and matched school ID
nearest_neighbor = no_ece_gdf[["cod_mod_anexo","cod_mod_anexo_cercano"]]

# We merge with standarized test data
nearest_neighbor = pd.merge(left= nearest_neighbor, right= bd_ece, how = 'left', left_on = 'cod_mod_anexo_cercano', right_on = 'cod_mod_anexo')

# Change varnames for nearest matched school
nearest_neighbor = nearest_neighbor[["cod_mod_anexo_x","ece","ind_lenguaje_ece_prim", "ind_mate_ece_prim"]]
nearest_neighbor = nearest_neighbor.rename(columns={"cod_mod_anexo_x": "cod_mod_anexo", "ece": "ece_imputado", "ind_lenguaje_ece_prim": "ind_leng_prim_imp", "ind_mate_ece_prim": "ind_mate_prim_imp"})

# Save data
nearest_neighbor.to_csv(here() / "imputacion_ece_primaria.csv")
