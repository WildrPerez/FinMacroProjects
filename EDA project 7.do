********************************************************************************
********************************************************************************
**** EDA project: Trade impact in economic income: role of productivity ***
********************************************************************************
********************************************************************************

clear all
set more off
capture log close

********************************************************************************
* Start log file
log using EDA_projectWP, replace

********************************************************************************
** Importing data from PennWorldTable

cd "C:\Users\enriq\Dropbox\University of Nottingham\Module programs\Economic Data Analysis\2020\EDA project\Data"

import excel "C:\Users\enriq\Dropbox\University of Nottingham\Module programs\Economic Data Analysis\2020\EDA project\Data\pwt91.xlsx", sheet("Data") firstrow clear

keep country countrycode year hc pop rgdpna cn ctfp csh_x csh_m

gen export = abs(csh_x)
gen import = abs(csh_m)
gen ln_trade = ln(export+import)

gen ln_cn = ln((cn/pop))

gen ln_hc = ln(hc)
gen ln_rGDP = ln(rgdpna)
gen income_pc = ln((rgdpna/pop))

gen ln_ctfp = ln(ctfp) 

save Penn.dta, replace
*bro

********************************************************************************
********************************************************************************
** Indication Panel Data Base Structure  

use Penn.dta, clear

cap ssc install Kountry
kountry countrycode, from(iso3c) geo(un)
encode NAMES_STD, gen(code)

xtset code year 

********************************************************************************
** Treatment of missing values

global xvar ln_trade income_pc ln_ctfp ln_hc ln_cn  

lab var ln_hc "Human Capital Index"
lab var income_pc "Income per Capita at constant 2011"
lab var ln_cn "Capital stock at current PPPs"
lab var ln_ctfp "TFP level at current PPPs (USA=1)"
lab var ln_trade "Share of merchandise exports and imports at current PPPs"

drop if ln_trade ==.
drop if ln_hc ==.
drop if ln_ctfp ==.
drop if ln_cn ==.

sum $xvar code

by code: gen count=_N

********************************************************************************
** Identifiying Outliers

graph box ln_hc //
graph save "Graph" "C:\Graphs\Graph1.gph", replace
graph box income_pc //
graph save "Graph" "C:\Graphs\Graph4.gph", replace
graph box ln_cn //
graph save "Graph" "C:\Graphs\Graph5.gph", replace
graph box ln_ctfp //
graph save "Graph" "C:\Graphs\Graph6.gph", replace
graph box ln_trade //
graph save "Graph" "C:\Graphs\Graph7.gph", replace

graph combine ///
C:\Graphs\Graph1.gph ///
C:\Graphs\Graph4.gph ///
C:\Graphs\Graph5.gph ///
C:\Graphs\Graph6.gph ///
C:\Graphs\Graph7.gph, rows(3) altshrink ///
title("", size(medium))

********************************************************************************
** Treatment of Outliers

cap ssc install winsor2

winsor2 income_pc, replace cut(2 99)

drop if countrycode == "ZWE"
drop if countrycode == "MLT"

quietly sort ln_trade
quietly sum ln_trade, detail
winsor2 ln_trade, replace cut(5 95)

sort ln_ctfp
sum ln_ctfp, detail
winsor2 ln_ctfp, replace cut(5 95)

quietly sort ln_cn
quietly sum ln_cn, detail
winsor2 ln_cn, replace cut(5 95)

save Penn.dta, replace

****************** New Graph ***************************************************

graph box ln_hc //
graph save "Graph" "C:\Graphs\Graph1.gph", replace
graph box income_pc //
graph save "Graph" "C:\Graphs\Graph4.gph", replace
graph box ln_cn //
graph save "Graph" "C:\Graphs\Graph5.gph", replace
graph box ln_ctfp //
graph save "Graph" "C:\Graphs\Graph6.gph", replace
graph box ln_trade //
graph save "Graph" "C:\Graphs\Graph7.gph", replace

graph combine ///
C:\Graphs\Graph1.gph ///
C:\Graphs\Graph4.gph ///
C:\Graphs\Graph5.gph ///
C:\Graphs\Graph6.gph ///
C:\Graphs\Graph7.gph, rows(3) altshrink ///
title("", size(medium))

********************************************************************************
** Graph of Data Description

use Penn.dta, clear

egen mean_tpf=mean(ctfp), by(NAMES_STD)
egen mean_lp=mean(income_pc), by(NAMES_STD)
egen mean_GDP=mean(rgdpna), by(NAMES_STD)
egen mean_hc=mean(hc), by(NAMES_STD)
egen mean_trade=mean(ln_trade), by(NAMES_STD)
egen mean_k=mean(cn), by(NAMES_STD)

lab var mean_hc "Human Capital Index"
lab var mean_GDP "Real GDP at constant 2011"
lab var mean_lp "Income Per Capita at constant 2011"
lab var mean_k "Capital stock at current PPPs"
lab var mean_tpf "TFP level at current PPPs (USA=1)"
lab var mean_trade "Share of merchandise exports and imports at current PPPs"

graph twoway (scatter mean_tpf mean_trade) (lfit mean_tpf mean_trade) //
graph save "Graph" "C:\Graphs\Graph11.gph", replace
graph twoway (scatter mean_lp mean_trade) (lfit mean_lp mean_trade) //
graph save "Graph" "C:\Graphs\Graph12.gph", replace
graph twoway (scatter mean_hc mean_trade) (lfit mean_hc mean_trade)  //
graph save "Graph" "C:\Graphs\Graph13.gph", replace
graph twoway (scatter mean_lp mean_tpf) (lfit mean_lp mean_tpf)  //
graph save "Graph" "C:\Graphs\Graph14.gph", replace

graph combine ///
C:\Graphs\Graph11.gph ///
C:\Graphs\Graph12.gph ///
C:\Graphs\Graph13.gph ///
C:\Graphs\Graph14.gph, rows(2) altshrink ///
title("", size(medium))

********************************************************************************
********************************************************************************
********************************************************************************

* Set data as panel data

global code code
global year year
global ylist1 income_pc

global xlist1 ln_trade ln_hc ln_cn 

sort $code $year
xtset $code $year
quietly xtdescribe

cap ssc install xtbalance, replace
xtbalance, rang(1974 2017) miss(_all)
xtset $code $year

xtsum $ylist1 $xlist1
xtsum ln_ctfp

global xlist4  ln_ctfp ln_trade income_pc ln_hc ln_cn 
correlate $xlist4

xtline ln_trade 
xtline income_pc 

save Penn.dta, replace

********************************************************************************
** Testing for Unit Root Test Panel

xtunitroot llc income_pc
xtunitroot llc ln_ctfp
xtunitroot llc ln_hc
xtunitroot llc ln_trade
xtunitroot llc ln_cn

********************************************************************************
********************************************************************************
**********        Panel Data estimation - Model         ************************
********************************************************************************
********************************************************************************

** Hausman test for fixed versus random effects model
quietly xtreg $ylist1 $xlist1, fe
estimates store fixed1
quietly xtreg $ylist1 $xlist1, re
estimates store random1
hausman fixed1 random1, sigmamore

** Breusch-Pagan LM test for random effects versus OLS
quietly xtreg $ylist1 $xlist1
xttest0

reg $ylist1 $xlist1
reg D.($ylist1 $xlist1)
xtreg $ylist1 $xlist1, fe
xtreg $ylist1 $xlist1, re 

********************************************************************************
********************************************************************************
************   Quantile panel data regression    *******************************
********************************************************************************
********************************************************************************

net from http://www.stata-journal.com/software/sj15-3
net install st0406

xtsktest income_pc ln_trade ln_hc ln_cn, reps(500) seed(123)

*xtqreg $ylist1 $xlist1, i(ln_ctfp) quantile(.1(.1).9) 

cap ssc install coefplot
tempname a b
forvalues quantile = 0.1(0.1)0.9 {
    xtqreg $ylist1 $xlist1,q(`quantile') i(ln_ctfp)
    matrix `a' = r(table)
    matrix `b' = `a'[1,1] \ `a'[5..6,1]
    matrix colnames `b' = "q`quantile'"
    matrix result1 = nullmat(result1), `b'
    }
coefplot matrix(result1[1]), ci((2 3)) vertical

********************************************************************************
********************************************************************************
log close

translate "C:\Users\enriq\Dropbox\University of Nottingham\Module programs\Economic Data Analysis\2020\EDA project\Data\EDA_projectWP.smcl" "C:\Users\enriq\Dropbox\University of Nottingham\Module programs\Economic Data Analysis\2020\EDA project\Data\pdflogEDA.pdf", translator(smcl2pdf)

clear
