clear all
cd "C:\Research\Crop Insurance and yield variance\Scripts\yld_var_git"
set more off
set matsize 11000
set maxvar 32000

import delimited irrigated.csv

drop if year < 1989

*egen ID = group(st_cnty commodity), label
*xtset ID
*drop ID

duplicates tag st_cnty year commodity, generate(newvar)
bysort st_cnty year commodity: gen tot = sum(newvar)
drop if tot > 1 
drop newvar
drop tot

save "C:\Research\Crop Insurance and yield variance\Data\irrigated.dta", replace
