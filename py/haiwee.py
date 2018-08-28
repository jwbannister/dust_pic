#!/home/john/.virtualenvs/owens_offlake/bin/python

#import sys
#import os
#import pandas as pd
#import geopandas as gpd
#from sqlalchemy import create_engine
#from matplotlib import pyplot as plt
#from matplotlib.collections import PatchCollection
#import shapely
#from descartes import PolygonPatch
from jwb_data import *
import requests
import csv
import pandas as pd

teom_query = "SELECT t.datetime, t.deployment, t.pm10_std_avg AS pm10, " + \
        "m.ws_wvc AS ws, m.wd_wvc AS wd, " + \
        "(t.datetime - '1 second'::interval)::date AS date, " + \
        "flags.field_is_invalid(m.deployment_id, 116, m.datetime) " + \
        "AS invalid_ws, " + \
        "flags.field_is_invalid(m.deployment_id, 117, t.datetime) " + \
        "AS invalid_wd " + \
        "FROM teom.avg_1hour_validated t " + \
        "JOIN (SELECT i.deployment, a.* FROM teom.teom_analog_1hour a " + \
              "JOIN instruments.deployments i " + \
              "ON a.deployment_id=i.deployment_id) m " + \
        "ON t.deployment=m.deployment AND t.datetime=m.datetime " + \
        "WHERE t.deployment IN ('Haiwee', 'T2-1') " + \
        "AND (t.datetime - '1 second'::interval)::date " + \
        "BETWEEN '2015-07-01'::date AND '2017-07-31'::date"
teom_data = pull_owens_data(teom_query)

mfile_sites = ["ShellCut", "DirtySox", "Olancha", "Stanley"]
mfile_query = "SELECT datetime, site AS deployment, teom AS pm10, " + \
        "aspd AS ws, dir AS wd, " + \
        "(datetime - '1 second'::interval)::date AS date " + \
        "FROM archive.mfile_data " + \
        "WHERE site IN ('" + "', '".join(mfile_sites) + "') " + \
        "AND (datetime - '1 second'::interval)::date " + \
        "BETWEEN '2015-07-01'::date AND '2017-07-31'::date"
mfile_data = pull_owens_data(mfile_query)

# pull Coso Junction data straight from EPA API
#link = "https://aqs.epa.gov/api/rawData?user=jbannister@airsci.com&" + \
#       "pw=tealcrane32&format=DMCSV&bdate=20150701&edate=20170731&" + \
#       "state=06&county=027&site=1001"
#a = pd.read_csv(link)

site_locations = pull_site_locations(mfile_sites)

poly_df = pull_lakebed_polygons()
poly_map = gpd.GeoDataFrame.plot(poly_df, facecolor='none', edgecolor='black')
area_extents = get_extents(poly_map)
plt.close(1)

fig, ax1 = plt.subplots(figsize=(8, 10.5))
bm = Basemap(llcrnrlon=area_extents['llcrnrlon'],
        llcrnrlat=area_extents['llcrnrlat'],
        urcrnrlon=area_extents['urcrnrlon'],
        urcrnrlat=area_extents['urcrnrlat'], epsg=2228)
background = bm.arcgisimage(service='World_Imagery', xpixels=500, verbose=True)

dca_patches = []
for poly in poly_df.geom:
    for subpoly in poly:
        mpoly = shapely.ops.transform(bm, poly)
        dca_patches.append(PolygonPatch(mpoly))
dcas = ax1.add_collection(PatchCollection(dca_patches))

site_lon = []
site_lat = []
for site in site_locations.geom:
    msite = shapely.ops.transform(bm, site)
    site_lon.append(msite.x)
    site_lat.append(msite.y)
sites = ax1.scatter(site_lon, site_lat, color='red')

query1 <- paste0("SELECT i.deployment, m.site, m.datetime, m.dir, ",
                 "m.aspd, m.teom, ",
                 "st_y(st_transform(i.geom, 26911)) AS y, ",
                 "st_x(st_transform(i.geom, 26911)) AS x ",
                 "FROM archive.mfile_data m ",
                 "JOIN instruments.deployments i ",
                 "ON m.deployment_id=i.deployment_id ",
                 "WHERE (m.datetime - ('1 second')::interval)::date='",
                 day, "'::date AND m.site IN ('",
                 paste0(mfile_sites, collapse="', '"), "')")
mfile_df <- query_db("owenslake", query1)
mfile_df <- mfile_df[complete.cases(mfile_df), ]
site_labels <- query_db("owenslake", query2)

if __name__ == '__main__':
    sys.exit(main())
