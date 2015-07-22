#     I would like to combine data from topographical maps, flood insurance data, rainfall data, 
# ground water level data in recent years to provide a statistical likelyhood for extreme flooding 
# due to extreme precipitation events experienced in localized areas (by watershed) on the front 
# range in Colorado. This information is to help set insurance premiums, provide a priority order 
# for municipalities for infrastruction improvement, help first responders avoid high risk roadways 
# in extreme precipitation events, and inform homeowners of the need for flood mitigation 
# requirements before a flood.
#     I propose to do this using data blending techniques to incorporate the multiple data 
# sets and extract from the blended data the peak water level from each precipitation event. The 
# peak water level will then be compared to local area flood levels, storm and sewer drain 
# capacities and flow rates, and scored to indicate likehood of flooding by comparing areas to 
# cumlative insurance claim information. Individual property insurance information is not available.
# CLUE (Comprehensive Loss Underwriting Exchange) reports are NOT public record - however, maybe 
# I can make an estimate for each location based on the county and the cumlative report.
#
# See http://bsa.nfipstat.fema.gov/reports/1040.htm#08 for cumlative loss statistics by county for 
# Colorado since 1/1/1978 to 4/30/2015
# See http://www.frascona.com/resource/jag403clue.htm for information on CLUE reports
#     Also see https://msc.fema.gov/portal for FEMA flood maps - maybe use insead of CLUE reports.
#
# Idea is not unique, see http://coloradofloodrisk.state.co.us/Pages/RiskMAPHome.aspx
# Only done for a few watersheds! So this could be a useful project!
# See http://coloradofloodrisk.state.co.us/StatusMaps/RiskMAPStatus/Pages/RiskMAPStatus.aspx
#
# Example of public information on storm and sewer drain requierments:
# Fort Collins, CO
# http://colocode.com/fcstormwater/fcstormpdf/fcstorm_vol1.pdf p. 8, 23 - 25, etc
# Aurora, CO
# https://www.auroragov.org/cs/groups/public/documents/document/001861.pdf 
# (has formula to calculate and information on coefficients (after p 78)).
# Evans, CO
# http://www.evanscolorado.gov/publicworks/drainage-study &
# http://www.evanscolorado.gov/water/stormwater-system-white-paper
# Denver
# http://www.denvergov.org/portals/711/documents/StormMasterPlan/
# StormDrainageDesignTechnicalCriteria.pdf
# Jefferson County, CO
# planning-and-zoning-regulations-storm-drainage-design-and-technical-criteria.pdf

# Predicting flash floods initial data 
# 17-20 July 2015
# Written by Cristina Moody

# Packages
# install.packages("strptime");
install.packages("rgdal");
install.packages("rgeos");
install.packages("maptools");
install.packages("rworldmap");
# Libraries
library(strptime);
library(rgdal);
library(rgeos);
library(maptools);
library(rworldmap);

# Getting around
wDir <- sprintf("%s%s", getwd(), "/");
dDir <- "/Users/Cristina/Downloads/";

# From http://www.ncdc.noaa.gov/cdo-web/datasets#PRECIP_HLY
# Hourly precipitation data for the following counties
# DENVER, ARAPAHOE
COPrecip_DenverArapahoe <- read.csv(file = sprintf("%s%s", dDir, "573023.csv"), 
                                    head = T, sep = ",", stringsAsFactors = F);
# CASTLE ROCK, GOLDEN, MORRISON, EVERGREEN, NEDERLAND, BOULDER, LONGMONT, PLEASANT VIEW, WHEATRIDGE
COPrecip_NEARBY <- read.csv(file = sprintf("%s%s", dDir, "573030.csv"),
                            head = T, sep = ",", stringsAsFactors = F);

# from http://nwis.waterdata.usgs.gov/co/nwis/
# peak?search_criteria=county_cd&submitted_form=introduction
COSurfaceH2O <- read.csv(file = sprintf("%s%s", dDir, "peak"), 
                         head = T, sep = "\t", stringsAsFactors = F); 
# Realized that this only has number of measurements per location! 
# Need to go to http://waterdata.usgs.gov/co/nwis/measurement to get surface water measurements.
# Still need COSurfaceH2O to correlate between Precipitation measurements and surface water from
# http://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_all_00060_00065_00010_00095_00300_00400=
# on&cb_00060=on&cb_00065=on&cb_00010=on&cb_00095=on&cb_00300=on&cb_00400=on&format=
# rdb&site_no=06711565&period=&begin_date=2007-10-01&end_date=2015-07-21
# Preparing data for blending
COSurfaceH2O$peak_begin_date2 <- strptime(COSurfaceH2O$peak_begin_date, format = "%Y-%m-%d");
COSurfaceH2O$peak_end_date2 <- strptime(COSurfaceH2O$peak_end_date, format = "%Y-%m-%d");
#
# Header removed from COSurfaceH2O
#
# US Geological Survey
# retrieved: 2015-07-17 22:14:18 EDT
# URL: http://nwis.waterdata.usgs.gov/co/nwis/peak
#

# The Site File stores location and general information about groundwater,
# surface water, and meteorological sites
# for sites in Colorado.
#
# The following selected fields are included in this output:
#
#  agency_cd       -- Agency
#  site_no         -- Site identification number
#  station_nm      -- Site name
#  site_tp_cd      -- Site type
#  lat_va          -- DMS latitude
#  long_va         -- DMS longitude
#  dec_lat_va      -- Decimal latitude
#  dec_long_va     -- Decimal longitude
#  coord_meth_cd   -- Latitude-longitude method
#  coord_acy_cd    -- Latitude-longitude accuracy
#  coord_datum_cd  -- Latitude-longitude datum
#  dec_coord_datum_cd -- Decimal Latitude-longitude datum
#  district_cd     -- District code
#  state_cd        -- State code
#  county_cd       -- County code
#  country_cd      -- Country code
#  land_net_ds     -- Land net location description
#  map_nm          -- Name of location map
#  map_scale_fc    -- Scale of location map
#  alt_va          -- Altitude of Gage/land surface
#  alt_meth_cd     -- Method altitude determined
#  alt_acy_va      -- Altitude accuracy
#  alt_datum_cd    -- Altitude datum
#  huc_cd          -- Hydrologic unit code
#  basin_cd        -- Drainage basin code
#  topo_cd         -- Topographic setting code
#  data_types_cd   -- Flags for the type of data collected
#  instruments_cd  -- Flags for instruments at site
#  construction_dt -- Date of first construction
#  inventory_dt    -- Date site established or inventoried
#  drain_area_va   -- Drainage area
#  contrib_drain_area_va -- Contributing drainage area
#  tz_cd           -- Mean Greenwich time offset
#  local_time_fg   -- Local standard time flag
#  reliability_cd  -- Data reliability code
#  gw_file_cd      -- Data-other GW files
#  nat_aqfr_cd     -- National aquifer code
#  aqfr_cd         -- Local aquifer code
#  aqfr_type_cd    -- Local aquifer type code
#  well_depth_va   -- Well depth
#  hole_depth_va   -- Hole depth
#  depth_src_cd    -- Source of depth data
#  project_no      -- Project number
#  rt_bol          -- Real-time data flag
#  peak_begin_date -- Peak-streamflow data begin date
#  peak_end_date   -- Peak-streamflow data end date
#  peak_count_nu   -- Peak-streamflow data count
#  qw_begin_date   -- Water-quality data begin date
#  qw_end_date     -- Water-quality data end date
#  qw_count_nu     -- Water-quality data count
#  gw_begin_date   -- Field water-level measurements begin date
#  gw_end_date     -- Field water-level measurements end date
#  gw_count_nu     -- Field water-level measurements count
#  sv_begin_date   -- Site-visit data begin date
#  sv_end_date     -- Site-visit data end date
#  sv_count_nu     -- Site-visit data count
#
#
# query started 2015-07-17 22:14:18 EDT
#
# there are 1188 sites matching the search criteria.
#
#
char_COSurfaceH2O <- COSurfaceH2O[1,]; # Character information for each column. Saving and removing.
# agency_cd site_no station_nm site_tp_cd lat_va long_va dec_lat_va dec_long_va coord_meth_cd
# 1        5s     15s        50s         7s    11s     12s        16n         16n            1s
# coord_acy_cd coord_datum_cd dec_coord_datum_cd district_cd state_cd county_cd country_cd
# 1           1s            10s                10s          3s       2s        3s         2s
# land_net_ds map_nm map_scale_fc alt_va alt_meth_cd alt_acy_va alt_datum_cd huc_cd basin_cd
# 1         23s    20s           7s     8s          1s         3s          10s    16s       2s
# topo_cd data_types_cd instruments_cd construction_dt inventory_dt drain_area_va
# 1      1s           30s            30s              8s           8s            8s
# contrib_drain_area_va tz_cd local_time_fg reliability_cd gw_file_cd nat_aqfr_cd aqfr_cd
# 1                    8s    6s            1s             1s        30s         10s      8s
# aqfr_type_cd well_depth_va hole_depth_va depth_src_cd project_no rt_bol peak_begin_date
# 1           1s            8s            8s           1s        12s     3n             10d
# peak_end_date peak_count_nu qw_begin_date qw_end_date qw_count_nu gw_begin_date gw_end_date
# 1           10d            8n           10d         10d          8n           10d         10d
# gw_count_nu sv_begin_date sv_end_date sv_count_nu peak_end_date2 peak_begin_date2
# 1          8n           10d         10d          8n           <NA>             <NA>

COSurfaceH2O <- COSurfaceH2O[2:1189,]; # Removing first row

# Getting S Platte River Surface Flow data from
# http://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_all_00060_00065_00010_00095_00300_00400=
# on&cb_00060=on&cb_00065=on&cb_00010=on&cb_00095=on&cb_00300=on&cb_00400=on&format=
# rdb&site_no=06711565&period=&begin_date=2007-10-01&end_date=2015-07-21
SPlatteSurface <- read.csv(file = sprintf("%s%s", dDir, "SPLATTE_SURFACE_LEVEL.csv"), 
                           head = F, sep = "\t", stringsAsFactors = F, skip = 33);
namesSP <- read.csv(file = sprintf("%s%s", dDir, "SPLATTE_SURFACE_LEVEL.csv"), 
                     head = T, sep = "\t", stringsAsFactors = F, skip = 31);
names(SPlatteSurface) <- names(namesSP);
SPlatteSurface$datetime <- strptime(SPlatteSurface$datetime, "%Y-%m-%d %H:%M");

COPrecip_DenverArapahoe$DATE <- strptime(COPrecip_DenverArapahoe$DATE , "%Y%m%d %H:%M");
COPrecip_NEARBY$DATE <- strptime(COPrecip_NEARBY$DATE , "%Y%m%d %H:%M");
COPrecip_DenverArapahoe$LATITUDE_NO   <- as.numeric(COPrecip_DenverArapahoe$LATITUDE);
COPrecip_DenverArapahoe$LONGITUDE_NO  <- as.numeric(COPrecip_DenverArapahoe$LONGITUDE);
COPrecip_NEARBY$LONGITUDE_NO  <- as.numeric(COPrecip_NEARBY$LONGITUDE);
COPrecip_NEARBY$LATITUDE_NO   <- as.numeric(COPrecip_NEARBY$LATITUDE);
COSurfaceH2O$LATITUDE_NO  <- as.numeric(COSurfaceH2O$dec_lat_va);
COSurfaceH2O$LONGITUDE_NO <- as.numeric(COSurfaceH2O$dec_long_va);

COSurfaceH2O$LONG_LAT <- paste(COSurfaceH2O$dec_long_va, COSurfaceH2O$dec_lat_va, sep = "_");
COPrecip_DenverArapahoe$LONG_LAT <- paste(COPrecip_DenverArapahoe$LONGITUDE, 
                                          COPrecip_DenverArapahoe$LATITUDE, sep = "_");
COPrecip_NEARBY$LONG_LAT <- paste(COPrecip_NEARBY$LONGITUDE, 
                                          COPrecip_NEARBY$LATITUDE, sep = "_");
# Removing unknown locations
COPrecip2_NEARBY <- COPrecip_NEARBY[!(COPrecip_NEARBY$LATITUDE=="unknown"),];
# Getting altitude in the same units for all tables
COSurfaceH2O$ALT_METERS <- 1200/3937 * as.numeric(COSurfaceH2O$alt_va); # Converting ft to m
COPrecip2_NEARBY$ALT_METERS <- as.numeric(COPrecip2_NEARBY$ELEVATION);
COPrecip_DenverArapahoe$ALT_METERS <- as.numeric(COPrecip_DenverArapahoe$ELEVATION);


# Saving data frames in R
save(COPrecip_DenverArapahoe, COPrecip2_NEARBY, COSurfaceH2O, 
     file = sprintf("%s%s", wDir, "InitialPrecipSurface.Rda"));

# Creating new data frame combining Precipitation Data nad Surface water by latitude and longitude.
COPrecip <- rbind(COPrecip_DenverArapahoe, COPrecip2_NEARBY);

# Specific location information
PrecipTime <- COPrecip_DenverArapahoe[
  abs(COPrecip_DenverArapahoe$LATITUDE_NO - 39.7667) < 0.0001  
  | abs(COPrecip_DenverArapahoe$LONGITUDE_NO + 104.88333) < 0.001,];



# First Plot
# Adding State map from
# http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
# Adapted from 
# http://stackoverflow.com/questions/27697504/ocean-latitude-longitude-point-distance-from-shore
# WGS84 long/lat
wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0";
# ESRI:54009 world mollweide projection, units = meters
# see http://www.spatialreference.org/ref/esri/54009/
mollweide <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs";
USstates <- readOGR(dsn = sprintf("%s%s", dDir, "ne_10m_admin_1_states_provinces"), 
                       layer = "ne_10m_admin_1_states_provinces", p4s = wgs.84);
USstates.moll <- spTransform(USstates, CRS(mollweide));

PrecipD.points <- SpatialPoints(COPrecip_DenverArapahoe[, c("LONGITUDE_NO","LATITUDE_NO")], 
                                proj4string = CRS(wgs.84));
PrecipS.points <- SpatialPoints(PrecipTime[, c("LONGITUDE_NO","LATITUDE_NO")],  
                                proj4string = CRS(wgs.84));
PrecipN.points <- SpatialPoints(COPrecip2_NEARBY[, c("LONGITUDE_NO","LATITUDE_NO")], 
                                proj4string = CRS(wgs.84));
Surf.points <- SpatialPoints(COSurfaceH2O[, c("LONGITUDE_NO","LATITUDE_NO")], 
                             proj4string = CRS(wgs.84));
svg("ColoradoPrecipitationSurfaceWater.svg", width = 14, height = 10);
plot(USstates, xlim = range(-109,-102), ylim = range(37, 41), 
     xlab = "longitude (deg)", ylab = "latitude (deg)", main = "Colorado", 
     sub = "Surface Water (blue) & Precipitation (red/orange) Measurement Locations");
points(Surf.points, pch = 2, col = "blue");
points(PrecipD.points, pch = 2, col = "red");
points(PrecipS.points, pch = 3, col = "dark red");
points(PrecipN.points, pch = 2, col = "orange");
dev.off();

# Second plot of precipitation over time for specific site (STAPLETON in DENVER)
PrecipTime <- PrecipTime[ PrecipTime$HPCP < 99999, ]; # Removing invalid measurements
# Picking the maximum precipitation event
rainDate <- PrecipTime[which(PrecipTime$HPCP == max(PrecipTime$HPCP)),]$DATE;
dev.off();
svg("SPlatte_SurfaceWater.svg", width = 14, height = 10);
plot( SPlatteSurface$datetime, SPlatteSurface$X02_00060, ylab = "Discharge (ft3/s)", xlab = "Date");
dev.off()


