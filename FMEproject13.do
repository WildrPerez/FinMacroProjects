set more off
clear all 


********************************************************************************
********************       Setting Working file     ****************************
********************************************************************************

cd "C:\Users\florb\Dropbox\FME project\Code\Stata"

* Kike
 *cd "C:\Users\enriq\Dropbox\University of Nottingham\Module programs\Financial and Macro Econometrics\FME project\Code\Stata"


********************************************************************************
********************       Importing data   ************************************
********************************************************************************

cap log close

log using FMEproject, replace // open log file

import excel GLdata1.xls, firstrow

*Kike
 *import excel "C:\Users\enriq\Dropbox\University of Nottingham\Module programs\Financial and Macro Econometrics\FME project\Code\Stata\GLdata1.xls", sheet("IFS Online") firstrow

********************************************************************************
********************       Unit roots test   ***********************************
********************************************************************************

cap gen t = _n
tset t


global X "ex M0UK RUK PUK YUK forusuk MUS RUS PUS YUS M0UK1 M0UK2"

  foreach var of global X{
	
	destring  `var', replace

 }
  foreach var of global X{
	
 	generate ln_`var'=ln(`var')
	dfuller  ln_`var', lag(4)
	pperron  ln_`var'
	generate dln_`var'=ln_`var'-ln_`var'[_n-1]
	dfuller  dln_`var', lag(4)
	pperron  dln_`var'
 }

* All variables in first differences are stationary

********************************************************************************
****************   Generating revelant variables    ***************************
********************************************************************************

gen dm = ln_MUS - ln_M0UK
gen dm2 =ln_MUS -ln_M0UK2
gen dy = ln_YUS-ln_YUK

gen fundamentals= dm2 - dy
	dfuller  fundamentals, lag(4)
	pperron  fundamentals
	dfuller  D.fundamentals, lag(4)
	pperron  D.fundamentals

gen dp = ln_PUS- ln_PUK
	dfuller  dp, lag(4)
	pperron  dp
	dfuller  D.dp, lag(4)
	pperron  D.dp
gen dr = ln_RUS-ln_RUK
gen dr1 = RUK-RUS

gen ar1 = L.ln_ex
gen changeEX = 100*(ln_ex - L.ln_ex)

********************************************************************************
**********************       Describing data     *******************************
********************************************************************************

tsline ln_ex, title("Logarithm Pound-Dollar Spot Exchange Rate")

tsline dr1, title("Excess Returns Pound-Dollar (percent)")

tsline RUK RUS, title("Short Term Interest Rates Differentials (Annual percent)")

twoway (tsline PUK PUS, yaxis(2)) (tsline dp), title("Index Prices UK and UK")

tsline changeEX, title("Change in the Pound-Dollar Spot Exchange Rate (percent)")

twoway (tsline ln_M0UK2, yaxis(2)) (tsline ln_MUS), title("Monetary base UK and US")

tsline ln_YUK ln_YUS, title("GDP per capita UK and US")

********************************************************************************
***********************        Models estimations  *****************************
********************************************************************************
********************************************************************************

********************************************************************************
***********************    Efficient Market Hypothesis    **********************
********************************************************************************

*********************   Model checks *******************************************
*Selecting number of lags
varsoc ln_ex ln_forusuk, maxlag(24)
*1,2 and 22 lags are the optimal, unlike the paper that chooses 12

vecrank ln_ex ln_forusuk, lag(6) trend(none)
vec ln_ex ln_forusuk, lag(6) rank(1) noetable trend(none)
eststo Memh 
ereturn list



vecstable
veclmar, mlag(6) 

*We don't reject the null of no autocorrelation

***********************AIC for Model weights*******************************************

gen aic1 = .  

forval i=1(3)150 {   // i will vary from 1 to 52 in steps of 1
local start = 133     //  start of estimation sample will change constantly with i
local end  = 240 + `i' - 1  // ditto for end of sample 
local h = `end' + 1  //  period just after end of sample
quietly vec ln_ex ln_forusuk if t>=`start' & t<=`end', lag(6) rank(1) noetable trend(none)
quietly replace aic1 = e(aic) if t==`h'
}

********************************************************************************
***********************    Monetary Fundamentals Model    **********************
********************************************************************************

*******************      Selecting optimal number of lags    *******************


varsoc ln_ex fundamentals, maxlag(24)

vecrank ln_ex fundamentals, lag(2) trend(none)

vec ln_ex fundamentals, lag(2) rank(1) noetable trend(none)
eststo Mmf
ereturn list
*We run checks on VEC
vecstable
veclmar

***********************AIC for Model weights************************************

gen aic2 = .  

forval i=1(3)150 {   // i will vary from 1 to 52 in steps of 1
local start = 133     //  start of estimation sample will change constantly with i
local end  = 240 + `i' - 1  // ditto for end of sample 
local h = `end' + 1  //  period just after end of sample
quietly vec ln_ex fundamentals if t>=`start' & t<=`end', lag(2) rank(1) noetable
quietly replace aic2 = e(aic) if t==`h'
}

********************************************************************************
*************************       Parity Power Purchase     **********************
********************************************************************************

********************       Selecting optimal number of lags     ****************

varsoc ln_ex dp, maxlag(24)

vecrank ln_ex dp, lag(2) trend(none)
*results from Johansen tests are not conclusive

vec ln_ex dp, lag(2) rank(1) noetable trend(none)
eststo Mp
ereturn list
*We run checks on VEC
vecstable
veclmar

***********************AIC for Model weights************************************

gen aic3 = .  

forval i=1(3)150 {   // i will vary from 1 to 150 in steps of 3
local start = 133     //  start of estimation sample will change constantly with i
local end  = 240 + `i' - 1  // ditto for end of sample 
local h = `end' + 1  //  period just after end of sample
quietly vec ln_ex dp if t>=`start' & t<=`end', lag(2) rank(1) noetable trend(none)
quietly replace aic3 = e(aic) if t==`h'
}

********************************************************************************
***************************       Autoregresive AR(P)     **********************
********************************************************************************

arima dln_ex, ar(12) nolog
eststo Ma
ereturn list
***********************AIC for Model weights************************************
gen aic4 = .  

forval i=1(3)150 {   // i will vary from 1 to 52 in steps of 1
local start = 133     //  start of estimation sample will change constantly with i
local end  = 240 + `i' - 1  // ditto for end of sample 
local h = `end' + 1  //  period just after end of sample
quietly arima dln_ex if t>=`start' & t<=`end', ar(12) nolog
 quietly replace aic4 = -2*(e(ll)/e(N)) + 2*(e(k)/e(N)) if t==`h'
}

********************************************************************************
*******************************       Random Walk      *************************
********************************************************************************

arima dln_ex, ar(1) nolog noconstant
eststo Mrw
ereturn list
***********************AIC for Model weights************************************

gen aic5 = . 
 forval i=1(3)150 {   
local start = 133     //  start of estimation sample will change constantly with i
local end  = 240 + `i' - 1  // ditto for end of sample 
local h = `end' + 1  //  period just after end of sample
 quietly arima dln_ex if t>=`start' & t<=`end', ar(1) nolog noconstant
 quietly replace aic5 = -2*(e(ll)/e(N)) + 2*(e(k)/e(N)) if t==`h'
 }

********************************************************************************
*******************************      Replication Table1 ************************
********************************************************************************

*esttab Memh Mmf Mp Ma Mrw using table3, ar2
*VEC option doesnt let you store r2 , or f test
*estimates table Model1 Model2 Model3 Model4 , star(0.1 0.05 0.001) stats(N r2 r2_a)

********************************************************************************
*****************************    WEIGHTS AIC   *********************************
********************************************************************************

*-------------------------- AIC Weights ----------------------------

cap drop delta1 
cap drop delta2 
cap drop delta3 
cap drop delta4
cap drop delta11 
cap drop delta12 
cap drop delta13 
cap drop delta14
cap drop sum_aic
cap drop w_aic_emh 
cap drop w_aic_mfm 
cap drop w_aic_ppp 
cap drop w_aic_ar

gen delta1 = aic1-aic5
gen delta2 = aic2-aic5
gen delta3 = aic3-aic5
gen delta4 = aic4-aic5


*forval i=1/4 { 
*replace delta`i' = 0 if (delta`i'>= .)
*}

gen delta11 = exp(delta1)
gen delta12 = exp(delta2)
gen delta13 = exp(delta3)
gen delta14 = exp(delta4)

gen sum_aic = (delta11+delta12+delta13+delta14)

gen w_aic_emh = delta11/sum_aic
gen w_aic_mfm = delta12/sum_aic
gen w_aic_ppp = delta13/sum_aic
gen w_aic_ar  = delta14/sum_aic

summarize w_aic_emh w_aic_mfm w_aic_ppp w_aic_ar

*---------------------------     line Graph    ---------------------------------

tsline aic1 aic2 aic3 aic4 aic5 if t>=240 & t<=360, title("AIC values models") saving(graph11.gph,replace)
*//graph save "Graph" "C:\graphs\Graph11.gph", replace

twoway(tsline w_aic_emh w_aic_mfm w_aic_ppp, yaxis(2)) (tsline w_aic_ar) if t>=240 & t<=360, title("Recursive US-UK AIC Model Weights") saving(graph12.gph, replace)

tsline w_aic_emh if t>=240 & t<=360, title("Efficient Market Hypothesis") saving(graph13.gph, replace)

tsline w_aic_mfm  if t>=240 & t<=360, title("Monetary Fundamentals Model") saving(graph14.gph, replace)

tsline w_aic_ppp if t>=240 & t<=360, title("Purchase Power Parity")  saving(graph15.gph, replace)

tsline w_aic_ar if t>=240 & t<=360, title("Autoregressive Model") saving(graph16.gph, replace)


graph combine ///
graph13.gph ///
graph14.gph ///
graph15.gph ///
graph16.gph, rows(2) altshrink ///
title("", size(medium))	


********************************************************************************
********************************************************************************
****************************   RECURSIVE FORECASTS 1 step ahead ****************
********************************************************************************
********************************************************************************

***********************    Recursive forecats - EMH    *************************
*************************H=1****************************************************
gen f_emh = .    

forval i=1/52 {   // i will vary from 1 to 52 in steps of 1
local start = 133     //  start of estimation sample will change constantly with i
local end  = 390 + `i' - 1  // ditto for end of sample 
local h = `end' + 1  //  period just after end of sample
quietly vec ln_ex ln_forusuk if t>=`start' & t<=`end', lag(2) rank(1) noetable
quietly fcast compute temp1, step(1)
quietly replace f_emh  =  temp1ln_ex if t==`h'
drop temp1ln_ex temp1ln_ex_SE temp1ln_ex_LB temp1ln_ex_UB temp1ln_forusuk temp1ln_forusuk_SE temp1ln_forusuk_LB temp1ln_forusuk_UB
}

gen RMSE_emh= sqrt((ln_ex-f_emh)^2) if t>=390
tsline RMSE_emh if t>=390, title("Efficient Market Hypothesis -Recursive forecast") saving(RMSE_EMH.gph, replace)


**********************     Recursive forecast - MFM       ***********************

gen f_mfm = .  
 
forval i=1/52 {   // i will vary from 1 to 52 in steps of 1
local start = 133     
local end  = 390 + `i' - 1  // ditto for end of sample 
local h = `end' + 1  //  period just after end of sample
quietly vec ln_ex fundamentals if t>=`start' & t<=`end', lag(9) rank(1) noetable trend(none)
quietly fcast compute temp1, step(1)
quietly replace f_mfm  =  temp1ln_ex if t==`h'
drop temp1ln_ex temp1ln_ex_SE temp1ln_ex_LB temp1ln_ex_UB temp1fundamentals temp1fundamentals_SE temp1fundamentals_LB temp1fundamentals_UB
}

gen RMSE_mfm = sqrt((ln_ex-f_mfm)^2) if t>=390



*************************      Recursive forecats - PPP      *********************

 gen f_ppp = . 

 forval i=1/52 {   // i will vary from 1 to 52 in steps of 1
 local start = 133     //  start of estimation sample will change constantly with i
 local end  = 390 + `i' - 1  // ditto for end of sample 
 local h = `end' + 1  //  period just after end of sample
 quietly vec ln_ex dp if t>=`start' & t<=`end', lag(16) rank(1) noetable
 quietly fcast compute f3, step(1)
 quietly replace f_ppp  =  f3ln_ex if t==`h'
 drop f3ln_ex f3ln_ex_SE f3ln_ex_LB f3ln_ex_UB f3dp f3dp_SE f3dp_LB f3dp_UB
 }

 gen RMSE_ppp = sqrt((ln_ex-f_ppp)^2) if t>=390
	*change xaxis for time and months
	su RMSE_ppp
	ret li
	


***************************    Recursive forecats - AR     *********************

 gen f_ar1 = . 

 forval i=1/52 {   // i will vary from 1 to 52 in steps of 1
 local start = 133     //  start of estimation sample will change constantly with i
 local end  = 390 + `i' - 1  // ditto for end of sample 
 local h = `end' + 1  //  period just after end of sample
 quietly arima dln_ex if t>=`start' & t<=`end', ar(1) nolog
 quietly replace aic4 = -2*(e(ll)/e(N)) +  2*(e(k)/e(N)) if t==`h'
 quietly predict f4
 quietly replace f_ar1  =  f4 if t==`h'
 drop f4
 }

 gen RMSE_ar1 = sqrt((dln_ex-f_ar1)^2) if t>=390
	*change xaxis for time and months
	su RMSE_ar1
	ret li


 ************************    Recursive forecats - RW     ***********************
  gen f_rw = .  

 forval i=1/52 {   // i will vary from 1 to 52 in steps of 1
 local start = 133     //  start of estimation sample will change constantly with i
 local end  = 390 + `i' - 1  // ditto for end of sample 
 local h = `end' + 1  //  period just after end of sample
 quietly arima dln_ex if t>=`start' & t<=`end', ar(1) nolog noconstant
 quietly predict f5
 quietly replace f_rw  =  f5 if t==`h'
 drop f5
 }

gen RMSE_rw = sqrt((ln_ex-f_rw)^2) if t>=390
	*change xaxis for time and months
	su RMSE_rw
	ret li

******************  Recursive forecats - equal weighted average (WA)   *********

 gen f_wa= (f_ar1+f_emh+f_mfm+f_ppp)/4

 gen RMSE_wa = sqrt((ln_ex-f_wa)^2) if t>=390
	su RMSE_wa
	ret li
	
******************  Recursive forecats - AIC weights (AICW)   *********

egen mw_aic_ar= mean(w_aic_ar) if t>=133 & t<=390
replace mw_aic_ar= L.mw_aic_ar if (mw_aic_ar>=.)
egen mw_aic_emh= mean (w_aic_emh) if t>=133 & t<=390
replace mw_aic_emh= L.mw_aic_emh if (mw_aic_emh>=.)
egen mw_aic_mfm= mean (w_aic_mfm) if t>=133 & t<=390
replace mw_aic_mfm= L.mw_aic_mfm if (mw_aic_mfm>=.)
egen mw_aic_ppp = mean (w_aic_ppp) if t>=133 & t<=390
replace mw_aic_ppp= L.mw_aic_ppp if (mw_aic_ppp>=.)

 gen f_aicw= (f_ar1*mw_aic_ar)+(f_emh*mw_aic_emh)+(f_mfm*mw_aic_mfm)+(f_ppp*mw_aic_ppp)

 gen RMSE_aicw = sqrt((ln_ex-f_aicw)^2) if t>=390
	su RMSE_aicw 
	ret li
*-----------------------------     Ratios RMSE    ------------------------------
cap drop EMH1
cap drop MFM1
cap drop PPP1
cap drop AR1
cap drop WA1
cap drop AICW1

gen EMH1 = exp(RMSE_emh/RMSE_rw)
gen MFM1 = exp(RMSE_mfm/RMSE_rw)
gen PPP1 = exp(RMSE_ppp/RMSE_rw)
gen AR1  = exp(RMSE_ar1/RMSE_rw)
gen WA1  = exp(RMSE_wa/RMSE_rw)
gen AICW1 = exp(RMSE_aicw/RMSE_rw)

tsline EMH1  MFM1 PPP1 AR1 WA1 AICW1 if t>=390, title("RMSE all models") saving(graph11.gph,replace)
*//graph save "Graph" "C:\graphs\Graph11.gph", replace

tsline AICW1 if t>=390, title("RMSE all models") saving(graph12.gph,replace)
*//graph save "Graph" "C:\graphs\Graph11.gph", replace

*-----------------------------     Table RMSE    -------------------------------

sum RMSE_emh RMSE_mfm RMSE_ppp RMSE_ar1 RMSE_wa RMSE_aicw

sum EMH1 MFM1 PPP1 AR1 WA1 AICW1

sum RMSE_emh RMSE_mfm RMSE_ppp RMSE_ar1 RMSE_wa RMSE_aicw RMSE_rw

	
*-----------------------------    Graph combine   ------------------------------
* Crear una carpeta en el escritorio con el nombre graphs

tsline RMSE_emh if t>=390, title("Efficient Market Hypothesis") //
*graph save "Graph" "C:\graphs\Graph1.gph", replace

tsline RMSE_mfm  if t>=390, title("Monetary Fundamentals Model") //
graph save "Graph" "C:\graphs\Graph2.gph", replace

tsline RMSE_ppp if t>=390, title("Purchase Power Parity")  //
graph save "Graph" "C:\graphs\Graph3.gph", replace

tsline RMSE_ar1 if t>=390, title("Autoregresive model") //
graph save "Graph" "C:\graphs\Graph4.gph", replace

tsline RMSE_rw if t>=390, title("Random Walk") //
graph save "Graph" "C:\graphs\Graph5.gph", replace

tsline RMSE_wa if t>=390, title("Weighted Average") //
graph save "Graph" "C:\graphs\Graph6.gph", replace


graph combine ///
C:\graphs\Graph1.gph ///
C:\graphs\Graph2.gph ///
C:\graphs\Graph3.gph ///
C:\graphs\Graph4.gph ///
C:\graphs\Graph5.gph ///
C:\graphs\Graph6.gph, rows(3) altshrink ///
title("", size(medium))

*------------------------   Multiple line Graph  -------------------------------

tsline RMSE_emh RMSE_mfm RMSE_ppp RMSE_ar1 if t>=390, title("All models") //
graph save "Graph" "C:\graphs\Graph10.gph", replace

tsline RMSE_rw RMSE_wa if t>=390, title("RW model") //
graph save "Graph" "C:\graphs\Graph9.gph", replace

graph combine ///
C:\graphs\Graph10.gph ///
C:\graphs\Graph9.gph, rows(2) altshrink ///
title("", size(medium))

********************************************************************************
********************************************************************************
****************************   RECURSIVE FORECASTS 4 steps ahead ****************
********************************************************************************
********************************************************************************

***********************    Recursive forecats - EMH    *************************

gen f_emh4 = .    

forval i=1/52 {   
local start = 133    
local end  = 390 + `i' - 1   
local h = `end' + 4 
quietly vec ln_ex ln_forusuk if t>=`start' & t<=`end', lag(2) rank(1) noetable
quietly fcast compute temp1, step(4)
quietly replace f_emh4  =  temp1ln_ex if t==`h'
drop temp1ln_ex temp1ln_ex_SE temp1ln_ex_LB temp1ln_ex_UB temp1ln_forusuk temp1ln_forusuk_SE temp1ln_forusuk_LB temp1ln_forusuk_UB
}

gen RMSE_emh4= sqrt((ln_ex-f_emh4)^2) if t>=390
tsline RMSE_emh4 if t>=390, title("Efficient Market Hypothesis -Recursive forecast") saving(RMSE_EMH4.gph, replace)
*add subtitle

**********************     Recursive forecast - MFM       ***********************

gen f_mfm4 = .  
 
forval i=1/52 {   // i will vary from 1 to 52 in steps of 1
local start = 133     
local end  = 390 + `i' - 1  // ditto for end of sample 
local h = `end' + 4  //  period just after end of sample
quietly vec ln_ex fundamentals if t>=`start' & t<=`end', lag(9) rank(1) noetable trend(none)
quietly fcast compute temp1, step(4)
quietly replace f_mfm4  =  temp1ln_ex if t==`h'
drop temp1ln_ex temp1ln_ex_SE temp1ln_ex_LB temp1ln_ex_UB temp1fundamentals temp1fundamentals_SE temp1fundamentals_LB temp1fundamentals_UB
}

gen RMSE_mfm4 = sqrt((ln_ex-f_mfm4)^2) if t>=390



*************************      Recursive forecats - PPP      *********************

 gen f_ppp4 = . 

 forval i=1/52 {   // i will vary from 1 to 52 in steps of 1
 local start = 133     //  start of estimation sample will change constantly with i
 local end  = 390 + `i' - 1  // ditto for end of sample 
 local h = `end' + 4 //  period just after end of sample
 quietly vec ln_ex dp if t>=`start' & t<=`end', lag(16) rank(1) noetable
 quietly fcast compute f3, step(4)
 quietly replace f_ppp4  =  f3ln_ex if t==`h'
 drop f3ln_ex f3ln_ex_SE f3ln_ex_LB f3ln_ex_UB f3dp f3dp_SE f3dp_LB f3dp_UB
 }

 gen RMSE_ppp4 = sqrt((ln_ex-f_ppp4)^2) if t>=390
	*change xaxis for time and months
	su RMSE_ppp4
	ret li
	


***************************    Recursive forecats - AR     *********************

 gen f_ar4 = . 

 forval i=1/52 {   // i will vary from 1 to 52 in steps of 1
 local start = 133     //  start of estimation sample will change constantly with i
 local end  = 390 + `i' - 1  // ditto for end of sample 
 local h = `end' + 4  //  period just after end of sample
 quietly arima dln_ex if t>=`start' & t<=`end', ar(1) nolog
 quietly predict f4
 quietly replace f_ar4  =  f4 if t==`h'
 drop f4
 }

 gen RMSE_ar4 = sqrt((dln_ex-f_ar4)^2) if t>=390
	*change xaxis for time and months
	su RMSE_ar4
	ret li



 ************************    Recursive forecats - RW     ***********************
  gen f_rw4 = .  

 forval i=1/52 {   // i will vary from 1 to 52 in steps of 1
 local start = 133     //  start of estimation sample will change constantly with i
 local end  = 390 + `i' - 1  // ditto for end of sample 
 local h = `end' + 4  //  period just after end of sample
 quietly arima dln_ex if t>=`start' & t<=`end', ar(1) nolog noconstant
 quietly predict f5
 quietly replace f_rw4  =  f5 if t==`h'
 drop f5
 }

gen RMSE_rw4 = sqrt((ln_ex-f_rw4)^2) if t>=390
	*change xaxis for time and months
	su RMSE_rw4
	ret li

******************  Recursive forecats - equal weighted average (WA)   *********

 gen f_wa4= (f_ar4+f_emh4+f_mfm4+f_ppp4)/4

 gen RMSE_wa4 = sqrt((ln_ex-f_wa4)^2) if t>=390
	*change xaxis for time and months
	su RMSE_wa4
	ret li
	
******************  Recursive forecats - AIC weights (AICW)   *********

 gen f_aicw4= f_ar4*mw_aic_ar + f_emh4*mw_aic_emh + f_mfm4*mw_aic_mfm + f_ppp4*mw_aic_ppp
 

 gen RMSE_aicw4 = sqrt((ln_ex-f_aicw4)^2) if t>=390
	*change xaxis for time and months
	su RMSE_aicw4
	ret li
*-----------------------------     Ratios RMSE    ------------------------------
gen EMH4 = exp(RMSE_emh4/RMSE_rw4) 
gen MFM4 = exp(RMSE_mfm4/RMSE_rw4)
gen PPP4 = exp(RMSE_ppp4/RMSE_rw4)
gen AR4 = exp(RMSE_ar4/RMSE_rw4)
gen WA4 = exp(RMSE_wa4/RMSE_rw4)
gen AICW4 = exp(RMSE_aicw4/RMSE_rw4)
*-----------------------------     Table RMSE    -------------------------------

sum EMH4 MFM4 PPP4 AR4 WA4 AICW4
	
*-----------------------------    Graph combine   ------------------------------
* Crear una carpeta en el escritorio con el nombre graphs

tsline RMSE_emh if t>=390, title("Efficient Market Hypothesis") //
*graph save "Graph" "C:\graphs\Graph1.gph", replace

tsline RMSE_mfm  if t>=390, title("Monetary Fundamentals Model") //
graph save "Graph" "C:\graphs\Graph2.gph", replace

tsline RMSE_ppp if t>=390, title("Purchase Power Parity")  //
graph save "Graph" "C:\graphs\Graph3.gph", replace

tsline RMSE_ar1 if t>=390, title("Autoregresive model") //
graph save "Graph" "C:\graphs\Graph4.gph", replace

tsline RMSE_rw if t>=390, title("Random Walk") //
graph save "Graph" "C:\graphs\Graph5.gph", replace

tsline RMSE_wa if t>=390, title("Weighted Average") //
graph save "Graph" "C:\graphs\Graph6.gph", replace


graph combine ///
C:\graphs\Graph1.gph ///
C:\graphs\Graph2.gph ///
C:\graphs\Graph3.gph ///
C:\graphs\Graph4.gph ///
C:\graphs\Graph5.gph ///
C:\graphs\Graph6.gph, rows(3) altshrink ///
title("", size(medium))

*------------------------   Multiple line Graph  -------------------------------

tsline RMSE_emh RMSE_mfm RMSE_ppp RMSE_ar1 if t>=390, title("All models") //
graph save "Graph" "C:\graphs\Graph10.gph", replace

tsline RMSE_rw RMSE_wa if t>=390, title("RW model") //
graph save "Graph" "C:\graphs\Graph9.gph", replace

graph combine ///
C:\graphs\Graph10.gph ///
C:\graphs\Graph9.gph, rows(2) altshrink ///
title("", size(medium))


save FMEproject, replace
log close








