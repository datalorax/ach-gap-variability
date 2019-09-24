# Achievement gap data

The `achievement-gaps-geocoded.csv` file in this folder was used for all analyses in the paper. This includes achievement gap data for all schools reporting the percent proficient data on students coded as Hispanic and White OR Economically Disadvantaged and All Students across California, Oregon, and Washington from the 2014-15 to 2017-18 school years. Below is a brief description of the data fields. 

The `scripts/dataprep.R` script provides all the code used to create the file. 

## Data fields

| Column Name | Description|
|:------------|:-----------|
|`year`| The academic school year. One of "1415", "1516", "1617", or "1718" |
|`state`| The corresponding state. One of `"CA"`, `"OR"` or `"WA"`|
|`district_id`| Numeric identifier for the district|
|`school_id`| Numeric identifier for the school|
|`ncessch`| [NCES](https://nces.ed.gov) school identifier |
|`content`| Test area. One of `"ELA"` for English/Language Arts or "Math" |
|`grade`| Grade level|
|`lat`| Latitude of the school|
|`lon`| Longitude of the school|
|`city`| City in which the school is or was located |
|`cnty`| County code in which the school is or was located |
|`nmcnty`| County name in which the school is or was located |
|`cbsatype`| Type of core based statistical area. One of "Metropolitan", "Micropolitan", or "Non-CBSA" |
|`cbsa` | Core based statistical area |
|`nmcbsa`| Name of the CBSA|
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
|  WA  | https://www.k12.wa.us/data-reporting/data-portal |

**Geographic data retrieved from**

* CCD: https://nces.ed.gov/ccd/pubschuniv.asp
* EDGE: https://nces.ed.gov/programs/edge/Geographic/SchoolLocations

**The specific names of the files used in these analyses are**
* California
	+ sb_ca2015_all_csv_v3.txt
	+ sb_ca2015entities_csv.txt
	+ sb_ca2016_all_csv_v3.txt
	+ sb_ca2016entities_csv.txt
	+ sb_ca2017_all_csv_v3.txt
	+ sb_ca2017entities_csv.txt
	+ sb_ca2018_all_csv_v3.txt
	+ sb_ca2018entities_csv.txt
	+ Subgroups.txt
* Oregon
	+ pagr_schools_ela_raceethnicity_1415.xlsx
	+ pagr_schools_ela_raceethnicity_1516.xlsx
	+ pagr_schools_ela_raceethnicity_1617.xlsx
	+ pagr_schools_ela_raceethnicity_1718.xlsx
	+ pagr_schools_ela_tot_ecd_ext_gnd_lep_1415.xlsx
	+ pagr_schools_ela_tot_ecd_ext_gnd_lep_1516.xlsx
	+ pagr_schools_ela_tot_ecd_ext_gnd_lep_1617.xlsx
	+ pagr_schools_ela_tot_ecd_ext_gnd_lep_1718.xlsx
	+ pagr_schools_math_raceethnicity_1415.xlsx
	+ pagr_schools_math_raceethnicity_1516.xlsx
	+ pagr_schools_math_raceethnicity_1617.xlsx
	+ pagr_schools_math_raceethnicity_1718.xlsx
	+ pagr_schools_math_tot_ecd_ext_gnd_lep_1415.xlsx
	+ pagr_schools_math_tot_ecd_ext_gnd_lep_1516.xlsx
	+ pagr_schools_math_tot_ecd_ext_gnd_lep_1617.xlsx
	+ pagr_schools_math_tot_ecd_ext_gnd_lep_1718.xlsx
* Washington
	+ Report_Card_Assessment_Data_2014-15_School_Year.csv
	+ Report_Card_Assessment_Data_2015-16_School_Year.csv
	+ Report_Card_Assessment_Data_2016-17_School_Year.csv
	+ Report_Card_Assessment_Data_2017-18_School_Year.csv
* Common Core of Data 
	+ ccd_sch_129_1415_w_0216161a.txt
	+ ccd_sch_129_1516_w_2a_011717.csv
	+ ccd_sch_129_1617_w_1a_11212017.csv
	+ ccd_sch_029_1718_w_0a_03302018.csv
* EDGE (geographies)
	+ EDGE_GEOCODE_PUBLICSCH_1617.xlsx

If you are trying to access these files and having a hard time tracking them down from the links above, please contact me. They are too large to load on GitHub, but they are all public files.
