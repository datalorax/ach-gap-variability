# Achievement gap data

The `achievement-gaps-geocoded.csv` file in this folder was used for all analyses in the paper. Below is a brief description of the data fields.
The `scripts/dataprep.R` provides all the code used to create the file. 
See more information on the files that went into this file below.

## Data fields

| Column Name | Description|
|:------------|:-----------|
|`state`| The corresponding state. One of `"CA"`, `"OR"` or `"WA"`|
|`district_id`| Numeric identifier for the district|
|`school_id`| Numeric identifier for the school|
|`ncessch`| [NCES](https://nces.ed.gov) school identifier |
|`city`| City in which the school is or was located |
|`cnty`| County code in which the school is or was located |
|`nmcnty`| County name in which the school is or was located |
|`lat`| Latitude of the school|
|`lon`| Longitude of the school|
|`content`| Test area. One of `"ELA"` for English/Language Arts or "Math" |
|`grade`| Grade level|
|`n`| The total number of students in the grade level
|`v_hisp`| Hispanic/White achievement gap estimate, interpreted in standard deviation units |
|`v_econ`| Economically Disadvantaged/All Students achievement gap estimate, interpreted in standard deviation units |

Note that missing values are reported as `NA`

## Data provenance
Below is a description of the source files used to produce the `achievement-gaps-geocoded.csv` file. 

**State files retrieved from:**

| State| Link |
|:----:|:-----|
|  CA  | https://caaspp.cde.ca.gov/sb2018/ResearchFileList |
|  OR  | https://www.oregon.gov/ode/educator-resources/assessment/Pages/Assessment-Group-Reports.aspx |
|  WA  | http://reportcard.ospi.k12.wa.us/DataDownload.aspx |

**Geographic data retrieved from**

* CCD: https://nces.ed.gov/ccd/Data/zip/ccd_sch_029_1718_w_0a_03302018_csv.zip
* EDGE: https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICSCH_1617.zip

**The specific names of the files used in these analyses are**
* California
	+ sb_ca2018_all_csv_v3.txt
	+ sb_ca2018entities_csv.txt
	+ Subgroups.txt
* Oregon
	+ pagr_schools_ela_raceethnicity_1718.xlsx
	+ pagr_schools_ela_tot_ecd_ext_gnd_lep_1718.xlsx
	+ pagr_schools_math_raceethnicity_1718.xlsx
	+ pagr_schools_math_tot_ecd_ext_gnd_lep_1718.xlsx
* Washington
	+ Report_Card_Assessment_Data_2017-18_School_Year.csv
* Common Core of Data 
	+ ccd_sch_029_1718_w_0a_03302018.csv
* EDGE (geographies)
	+ EDGE_GEOCODE_PUBLICSCH_1617.xlsx

If you are trying to access these files and having a hard time tracking them down from the links above, please contact me. They are too large to load on GitHub, but they are all public files.
