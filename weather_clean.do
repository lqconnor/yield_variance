clear all
cd "C:\Research\Crop Insurance and yield variance\Scripts\yld_var_git"
set more off
set matsize 11000
set maxvar 32000

import delimited weather.csv

drop if year < 1989

*egen ID = group(st_cnty commodity), label
*xtset ID
*drop ID
destring hdd vpd vpd_dew, ignore(NaN) replace

rename w_ppt s_ppt

save "C:\Research\Crop Insurance and yield variance\Data\weather.dta", replace
