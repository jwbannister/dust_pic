#!/home/john/.virtualenvs/owens_offlake/bin/python
"""Gather all relevant information to give complete dust picture for Owens Lake shoreline exceedance day."""
import sys
import os
import pandas as pd
import geopandas as gpd
from sqlalchemy import create_engine
from matplotlib import pyplot as plt
from matplotlib.collections import PatchCollection
import shapely
from descartes import PolygonPatch
execfile('/home/john/code/python/mapping.py')

def main():
    """Main entry poin for the script."""
    pass

def pull_site_locations(sites):
    host = os.getenv("PSQL_HOST_OWENS")
    pw = os.getenv("PSQL_PASSWORD_OWENS")
    engine = create_engine("postgresql://airproc:"+pw+"@"+host+"/owenslake")
    proj = "+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
    query = "SELECT DISTINCT i.deployment, m.site, i.geom " +\
            "FROM archive.mfile_data m JOIN instruments.deployments i " +\
            "ON m.deployment_id=i.deployment_id " +\
            "WHERE m.site IN (%s);" %("'"+"', '".join(sites)+"'")
    df = gpd.GeoDataFrame.from_postgis(query, con=engine, crs=proj)
    return df

mfile_sites = ["LonePine", "Keeler", "NorthBch", "LizardTl", "MillSite",
"ShellCut", "DirtySox", "Olancha", "Stanley"]
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
