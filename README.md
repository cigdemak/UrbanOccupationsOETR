# UrbanOccupationsOETR
Shiny application for the Industrialisation and Urban Growth from the mid-nineteenth century Ottoman Empire to Contemporary Turkey in a Comparative Perspective, 1850-2000 project at this adress: http://midas.ku.edu.tr/UrbanOccupationsOETR/

This document explains the UrbanOccupationsOETR Shiny Application folder content and gives brief instructions for implementing the new data into the shiny application.

# Main Folder

## Code files
For Shiny applications, you need three R scripts called global.R, ui.R and server.R.
In global.R file, we load the libraries used (e.g., shiny, leaflet), read the necessary data to be utilized (e.g., shape files, excel files), and create global variables (that they can be used anywhere and never change) as needed. In ui.R file, user interface elements are created (e.g., buttons, boxes to be displayed, colour and shape of the elements on the page to be displayed to the user). In server.R file, there are functions whose inputs may be related to the inputs coming from the user (i.e., input from the buttons, selections created in ui.R). 

In UrbanOccuptionsOETR application, each side tab’s user interface is created in separate scripts for practical reasons since when the code gets longer, working with separate files is easier. 
-	Occupational Structure tab’s user interface is created in the file named ui_occupational_analysis.R.
-	Population Geography tab’s user interface is created in the file named ui_population_analysis.R.
-	Agricultural Production tab’s user interface is created in the file named ui_agricultural_analysis.R.
Then these three files are sourced in the ui.R file.

# data Folder
In data folder, there are three census data folders: agriculture_data, occupation_data and population_data, one folder contains all the shape files used: map_data and one file that contains the indexes (i.e., relations between shape and excel files) and the names of the places: index_data.
index_data Folder:
In this folder, there are three excel files for each country with the file names as follows: 
- district_index_”country”
- subdistrict_index_”country”
- point_index_”country”
Inside these excel files, you will see each sheet contains the indexes of a different year and the sheets are named with the file name underscore and the year (e.g., in the subdistrict_index_bul excel file, the sheet names are subdistrict_index_bul_1905, subdistrict_index_bul_1934 etc.).  

You will find an R Workspace file for The Ottoman Empire’s point data, because of the Ottoman name display problem, I had to save the names in an RData file. In global.R, RData file used for OE point data instead of csv file.
Each sheet has two columns: “NAME” and “LOCATION_ID”. “NAME” column contains the names of the locations and “LOCATION_ID” contains the indexes of the locations. This column should be numeric and sorted increasingly before saving. The location names on the map in the Shiny Application is the same as in the “NAME” column. Names in this column can be changed freely (change to an old name, Ottoman name,  in capital letters, multiple names, parentheses, numbers etc.) but any change in “LOCATION_ID” column should be reflected in excel and shape files as well.

Indexes should be unique. There cannot be two separate names in the “NAME” column with the same index in the “LOCATION_ID” column or there cannot be two separate indexes in the “LOCATION_ID” column with the same name in the “NAME” column. This is very important in order to map the data correctly, therefore, especially for point and subdistrict data, location names should be considered with the related upper level location name for example to have unique name and location id correspondance.

# map_data Folder:
This folder contains all the shape files. For each map, there should be eight files:
XML Document, SBX, SBN, DBF, CPG, SHP, SHX, PRJ files.
In global.R, we read only .shp files but all the information in the other files load automatically, thus all these eight files must stay under the same folder together. 
When loading the shape files, name of the location index column should be checked and changed as “MAP_ID”, so the code will know which column it should look for to match the excel data with the map.
•	“LOCATION_ID” column in index_data files 
•	“MAP_ID” column in map_data files and 
•	“LOCATION_INDEX” columns
 in census data excel files contains the same information and they all need to be numeric.

# occupation_data Folder:
In this folder, you will find the excel files of district and subdistrict level economic census data for each country with the names as follows:
-	economic_data_district_”country”
-	economic_data_subdistrict_”country”
In these files, you will need the following columns: “YEAR”, “LOCATION_INDEX”, “Male”, “Female”, “Both”, “PST CONC”. Please do not change the column names since it will affect the Shiny Application’s modularity. If you need to change the column names, you must modify the code in server.R, ui.R and global.R scripts. You can change the places of the columns or add additional columns as long as you keep the aforementioned seven columns. For instance, economic_data_district_tur excel file contains more than these seven columns but has also these seven columns with the exact same names specified above. 
You will also find an excel file named pstv which contains the pst codes corresponding names. It is used for displaying the names of the PST codes on the user interface.

# population_data Folder:
In this folder, you will find the excel files of district, subdistrict and point level population/ethnicity/religion census data for each country with the names as follows:
-population_data_district_”country”
-population_data_subdistrict_”country”
-point_data_”country”

In these files, for district and subdistrict level data you will need the following columns: “YEAR”, “LOCATION_INDEX”, “Male”, “Female”, “Both”, “ER_Code”. Please do not change the column names since it will affect the Shiny Application’s modularity. If you need to change the column names, you must modify the code in server.R, ui.R and global.R scripts. You can change the places of the columns or add additional columns as long as you keep the aforementioned seven columns. 
For point level data, you will need the following columns: “YEAR”, “LOCATION_INDEX”, “ER_Code”, “E_coord”, “N_coord” columns. Last two specifies the locations latitude and longitude coordinates, respectively. In addition to these columns you may have “Male”, “Female”, “Both”, “Household Count”, “Household Size” columns. Data filters will depend on the columns you provide.
For example, Turkey’s point data have “Female”, “Male” and “Both” columns but Ottoman Empire’s point data have “Male”, “Household Count” and “Household Size” columns. Filters appear with respect to column names of the data. That is why Ottoman Empire point data cannot be filtered by female gender and Turkey’s data cannot be filtered by the household count or household size.

# When new data arrives…
When there is a new data to be added to the Shiny Application, the following four main changes should be done:
1.	In the index_data file, the corresponding excel sheet must be created with the sheet name and the column names explained above. Before save, numeric index column should be sorted increasingly.
2.	In the map_data, the corresponding shape files must be added. When reading the shape files, check the name of the location index column, change it to “MAP_ID” if it is not. Check if the indexes are numeric and consistent with the index data and the census data.
3.	In the corresponding census data folder (either agriculture_data, occupation_data or population_data folder), data should be added to the related columns as rows in the corresponding level and country excel file.
4.	All the added data in the previous steps should be loaded/read in the global.R file.

# Notes
Abbreviations of country names in the file names and in the scripts: 
-	The Ottoman Empire:  OE, oe
-	Bulgaria: BUL, bul
-	Turkey: TUR, tur

In the data folder, there is an RData file which stores all index, map, and census data to be read in global.R for speed concerns when launching the application for the first time. In global.R file, if the data will be read from this RData file the if condition is FALSE (which means data will be read from the RData file, excels and shape files will not be read) but if any data is changed please set it to TRUE and run the glabal.R once (so that excel and shape files will be read and will be saved in an Rdata file again) then set the if condition to FALSE again for fast start of the application.
