# **<ins>Raw data sources/links<ins>**
## *Satellite Vegetation Optical Depth (VOD)*
### <a href="http://files.ntsg.umt.edu/data/LPDR_v3/GeoTif/"> Land Parameter Data Record v.3 (LPDRv3) 
- X-band (10.65 GHz) from AMSR-E/AMSR2
- 25 km pixel resolution
- 1-3 day revisit frequency
- Daytime and nighttime observations (ascending and descending satellite overpass, respectfully)
- Temporally smoothed using 24-day moving average

### <a href="https://disc.gsfc.nasa.gov/datasets/LPRM_AMSR2_DS_A_SOILM3_001/summary?keywords=AMSR2%2FGCOM-W1%20surface%20soil%20moisture%20(LPRM)%20L3%201%20day%2010%20km%20x%2010%20km"> Land Parameter Retrieval Model v.1 (LPRMv1) Level 3
- X- (10.7 GHz), C1- (6.9 GHz), and C2-band (7.3 GHz) from AMSR2 and GCOM-W1
- 10 km (downscaled) pixel resolution
- 1-3 day revisit frequency
- Daytime and nighttime observations 
- No temporal smoothing

### <a href="https://nsidc.org/data/spl3smp_e/versions/5"> Soil Moisture Active-Passive (SMAP) Enhanced L3 Radiometer Global and Polar Grid 9km EASE-Grid Soil Moisture, v.5
- L-band (1.4 GHz); Dual Channel Algorithm (DCA)
- 9 km (downscaled) pixel resolution
- Evening and morning observations
- No temporal smoothing

## *AmeriFlux Eddy Covariance/Meteorology*
### <a href="https://fluxnet.org/doi/FLUXNET2015/US-Wkg"> FluxNet2015 (Walnut Gulch - Kendall Grassland)

### <a href="https://ameriflux.lbl.gov/doi/AmeriFlux/US-Wkg/"> AmeriFlux BASE (Walnut Gulch - Kendall Grassland)

## *Vegetation Greenness/Phenology*
### <a href="https://phenocam.nau.edu/webcam/roi/kendall/GR_1000/"> PhenoCam Network Green Chromatic Coordinate (GCC) (Walnut Gulch - Kendall Grassland)

## *Vegetation Cover*
### <a href="http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-cover/v3/"> Rangeland Analysis Platform (RAP) Vegetation Cover (v3) / Plant Functional Type (PFT)

## *NMRI from the Plate Boundary Observatory (PBO)*
### <a href="https://www.kristinelarson.net/portfolio/the-cryosphere-and-gps/"> PBO Vegetation Product (archived)

***
***
***

# **<ins>Directory structure for raw data preprocessing<ins>**
- Raw/large datasets are stored outside of the repository
- Processed datasets are stored within the repository (~/WKG_GNSS-R_Flux_2021/Data/)

**<ins>Basic directory structure:<ins>**
- Desktop/ (or another directory)
	- Data/
	- Github/
		- WKG_GNSS-R_Flux_2021/

**<ins>Detailed directory structure:<ins>**
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