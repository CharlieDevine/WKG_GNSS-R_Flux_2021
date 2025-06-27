--------------------------
--------------------------
Data preprocessing scripts
--------------------------
--------------------------

--------------------------------------------------
Scripts beginning with 00_ can be run in any order
--------------------------------------------------
- Reformatting and spatial normalization of gridded satellite vegetation optical depth (VOD) datasets
- Temporal and variable subsetting of FLUXNET2015 hourly data
- Downloading AmeriFlux BASE data
- Downloading PhenoCam Network GCC data
 

--------------------------------------------------------------
Run 01_Timeseries_Extraction.R after 00_ scripts have been run
--------------------------------------------------------------
- Extracts single-pixel values from VOD datasets
- Timeseries .csv files are output to /GitHub_Root_Diretory/Data/Timeseries_CSVs/
