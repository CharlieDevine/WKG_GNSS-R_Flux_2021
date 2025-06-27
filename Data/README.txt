##########################
--------------------------
Public Data Sources/links
--------------------------
##########################

------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------- Satellite Vegetation Optical Depth (VOD) ---------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
Land Parameter Data Record v.3 (LPDRv3)
Link: http://files.ntsg.umt.edu/data/LPDR_v3/GeoTif/
- X-band (10.65 GHz) from AMSR-E/AMSR2
- 25 km pixel resolution
- 1-3 day revisit frequency
- Daytime and nighttime observations (ascending and descending satellite overpass, respectfully)
- Temporally smoothed using 24-day moving average
------------------------------------------------------------------------------------------------------------------------------------
Land Parameter Retrieval Model v.1 (LPRMv1) Level 3
Link: https://hydro1.gesdisc.eosdis.nasa.gov/data/WAOB/LPRM_AMSR2_DS_A_SOILM3.001/ (ascending overpass)
Link: https://hydro1.gesdisc.eosdis.nasa.gov/data/WAOB/LPRM_AMSR2_DS_D_SOILM3.001/ (descending overpass)
- X- (10.7 GHz), C1- (6.9 GHz), and C2-band (7.3 GHz) from AMSR2 and GCOM-W1
- 10 km (downscaled) pixel resolution
- 1-3 day revisit frequency
- Daytime and nighttime observations 
- No temporal smoothing
------------------------------------------------------------------------------------------------------------------------------------
Soil Moisture Active-Passive (SMAP) Enhanced L3 Radiometer Global and Polar Grid 9km EASE-Grid Soil Moisture, v.5
Link: https://nsidc.org/data/spl3smp_e/versions/5
- L-band (1.4 GHz); Dual Channel Algorithm (DCA)
- 9 km (downscaled) pixel resolution
- Evening and morning observations
- No temporal smoothing


---------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------- AmeriFlux Eddy Covariance/Meteorology ---------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------
FLUXNET2015 Eddy Covariance/Meteorology for Walnut Gulch - Kendall Grassland
Link: https://fluxnet.org/doi/FLUXNET2015/US-Wkg
---------------------------------------------------------------------------------------------------------------------------------
AmeriFlux BASE Eddy Covariance/Meteorology for Walnut Gulch - Kendall Grassland
Link: https://ameriflux.lbl.gov/doi/AmeriFlux/US-Wkg/


--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------- Vegetation Greenness/Phenology ---------------------------------------------
--------------------------------------------------------------------------------------------------------------------------
PhenoCam Network Green Chromatic Coordinate (GCC)  for Walnut Gulch - Kendall Grassland
Link: https://phenocam.nau.edu/webcam/sites/kendall/


------------------------------------------------------------------------------------------------------------
--------------------------------------------- Vegetation Cover ---------------------------------------------
------------------------------------------------------------------------------------------------------------
Rangeland Analysis Platform (RAP) Vegetation Cover (v3) / Plant Functional Type (PFT)
Link: http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-cover/v3/


-----------------------------------------------------------------------------------------------------------------------------
--------------------------------------------- NMRI from Plate Boundary Observatory (PBO) ------------------------------------
-----------------------------------------------------------------------------------------------------------------------------
PBO Vegetation Product (archived)
Link: https://www.kristinelarson.net/portfolio/the-cryosphere-and-gps/



##############################################
----------------------------------------------
Directory structure for raw data preprocessing
----------------------------------------------
##############################################

- Raw/large datasets are stored outside of the repository
- Processed datasets are stored within the repository (~/WKG_GNSS-R_Flux_2021/Data/)

Basic directory structure:
- Desktop/ (or another directory)
	- Data/
	- Github/
		- WKG_GNSS-R_Flux_2021/


Detailed directory structure:
- Data/
	- FLUXNET2015/
		- AMF_US-Wkg_FLUXNET_FULLSET_HH_2004-2021_3-5.csv
	- LPDR_v3/
		- Original/
			- Annual subdirectories/
		- VOD/
			- Annual subdirectories/
	- LPRM/
		- GeoTIFF/
			- WGS84/
			- Annual subdirectories/
		- Original/
			- Ascending/
				- Annual subdirectories/
			- Descending/
				- Annual subdirectories/
	- SMAP_9km/
		- GeoTIFF/
			- Global/
		- HDF5/
		- SM_09km_EASE2_2015_04_08.tif
	- PBO_H2O_data/
		- Site subdirectories/
		- veg_metadata.csv
- Github/
	- WKG_GNSS-R_Flux_2021/
		- Code/
		- Data/
			- Ameriflux_BASE/
			- Flux_Footprints/
			- FLUXNET2015/
			- GNSS_GPS/
			- GridCenterCoordinates/
			- Phenocamr/
			- RAP_Site_VegCover/
			- Shapefiles/
			- Timeseries_CSVs/
		- Figures/
