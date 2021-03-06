clear all

cd "D:\Bases de primarios\Supply and Demand Shocks during\Stata"

unicode analyze "MPT - trimestral.do"
unicode encoding set "latin1"
unicode translate "MPT - trimestral.do"

set more off

global a "D:\Bases de primarios\Supply and Demand Shocks during\Stata\Almacen"

use "$a\enaho01a-20201t-500.dta", clear



* Condición de actividad
************************
gen v03ConAct = .
replace v03ConAct = 1 if (p501==1 | p502==1 | p503==1 | p504==1) & (p507==1 | p507==2 | p507==3 | p507==4 | p507==6)
replace v03ConAct = 1 if (p501==1 | p502==1 | p503==1 | p504==1) & (p507==5 | p507==7) & p513t >= 15
replace v03ConAct = 2 if (p501==1 | p502==1 | p503==1 | p504==1) & (p507==5 | p507==7) & (p513t < 15) & p545 == 1 & (p550==1 | p550==2 | p550==3 | p550==4 | p550==5 | p550==6)
replace v03ConAct = 2 if (p501==1 | p502==1 | p503==1 | p504==1) & (p507==5 | p507==7) & (p513t < 15) & p545 == 2 & (p546==1 | p546==2) 
replace v03ConAct = 2 if (p501==1 | p502==1 | p503==1 | p504==1) & (p507==5 | p507==7) & (p513t < 15) & p545 == 2 & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==1 & p548==1 & p549==10 
replace v03ConAct = 2 if (p501==1 | p502==1 | p503==1 | p504==1) & (p507==5 | p507==7) & (p513t < 15) & p545 == 2 & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==1 & p548==1 & p549==11 & (p550==1 | p550==2 | p550==3 | p550==4 | p550==5 | p550==6)
replace v03ConAct = 2 if (p501==2 | p502==2 | p503==2 | p504==2) & p545==1 & (p550==1 | p550==2 | p550==3 | p550==4 | p550==5 | p550==6)
replace v03ConAct = 2 if (p501==2 | p502==2 | p503==2 | p504==2) & p545==2 & (p546==1 | p546==2)
replace v03ConAct = 2 if (p501==2 | p502==2 | p503==2 | p504==2) & p545==2 & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==1 & p548==1 & p549==10
replace v03ConAct = 2 if (p501==2 | p502==2 | p503==2 | p504==2) & p545==2 & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==1 & p548==1 & p549==11 & (p550==1 | p550==2 | p550==3 | p550==4 | p550==5 | p550==6)
replace v03ConAct = 3 if (p501==1 | p502==1 | p503==1 | p504==1) & (p507==5 | p507==7) & (p513t < 15) & (p545 == 1) & (p550==7)
replace v03ConAct = 3 if (p501==1 | p502==1 | p503==1 | p504==1) & (p507==5 | p507==7) & (p513t < 15) & (p545 == 2) & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==1 & p548==1 & (p549==1 | p549==2 | p549==3 | p549==4 | p549==5 | p549==6 | p549==7 | p549==8 | p549==9)
replace v03ConAct = 3 if (p501==1 | p502==1 | p503==1 | p504==1) & (p507==5 | p507==7) & (p513t < 15) & p545 == 2 & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==1 & p548==2
replace v03ConAct = 3 if (p501==1 | p502==1 | p503==1 | p504==1) & (p507==5 | p507==7) & (p513t < 15) & p545 == 2 & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==2
replace v03ConAct = 3 if (p501==1 | p502==1 | p503==1 | p504==1) & (p507==5 | p507==7) & (p513t < 15) & p545 == 2 & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==1 & p548==1 & p549==11 & p550==7
replace v03ConAct = 3 if (p501==2 | p502==2 | p503==2 | p504==2) & p545==1 & p550==7
replace v03ConAct = 3 if (p501==2 | p502==2 | p503==2 | p504==2) & p545==2 & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==1 & p548==1 & (p549==1 | p549==2 | p549==3 | p549==4 | p549==5 | p549==6 | p549==7 | p549==8 | p549==9)
replace v03ConAct = 3 if (p501==2 | p502==2 | p503==2 | p504==2) & p545==2 & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==1 & p548==2
replace v03ConAct = 3 if (p501==2 | p502==2 | p503==2 | p504==2) & p545==2 & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==2
replace v03ConAct = 3 if (p501==2 | p502==2 | p503==2 | p504==2) & p545==2 & (p546==3 | p546==4 | p546==5 | p546==6 | p546==7 | p546==8) & p547==1 & p548==1 & p549==11 & p550==7

* Esta línea empalma ocu500 con la recodificación del MTP, el INEI toma los missing como si fueran PEA OCUPADA
replace v03ConAct = 1 if v03ConAct ==.

label define v03ConAct 1 "Ocupado" 2 "Desocupado" 3 "no pea"
label values v03ConAct v03ConAct

gen ocu5001 = 1 if ocu500==1
replace ocu5001 = 2 if (ocu500==2 | ocu500==3)
replace ocu5001 = 3 if ocu500==4

label define ocu5001 1 "Ocupado" 2 "Desocupado" 3 "no pea"
label values ocu5001 ocu5001

* Rama de la actividad económica de la ocupación principal (v03ConAct=1)
************************************************************************

gen v05RamAct = .
replace v05RamAct =1  if (p506>=111 & p506<=113) | (p506==121) | (p506==122) | (p506==130) | (p506==140) | (p506==150) | (p506==200) | (p506==500)
replace v05RamAct =2  if (p506==1010) | (p506==1020) | (p506==1030) | (p506==1110) | (p506==1120) | (p506==1200) | (p506==1310) | (p506==1320) | (p506==1410) | (p506==1421) | (p506==1422) | (p506==1429)
replace v05RamAct =3  if (p506>=1511 & p506<=1515) | (p506==1520) | (p506>=1531 & p506<=1533) | (p506>=1541 & p506<=1544) | (p506==1549) | (p506>=1551 & p506<=1554) | (p506==1600) | (p506==1711) /*
					*/ | (p506==1712) | (p506>=1721 & p506<=1723) | (p506==1729) | (p506==1730) | (p506==1810) | (p506==1820) | (p506==1920)| (p506==2029) | (p506==2109) | (p506==2211) | (p506==2212) /*
					*/ | (p506==2219) | (p506==2221) | (p506==2222) | (p506==2520) | (p506==3312) | (p506==3313) | (p506==3320) | (p506==3330) | (p506==3610) | (p506>=3691 & p506<=3694) | (p506==3699)
replace v05RamAct =4  if (p506==1911) | (p506==1912) | (p506==2010) | (p506==2021)| (p506==2022) | (p506==2023) | (p506==2101) | (p506==2102) | (p506==2213) | (p506==2310) | (p506==2320) | (p506==2330) /*
					*/ | (p506>=2411 & p506<=2413) | (p506>=2421 & p506<=2424) | (p506==2429) | (p506==2430) | (p506==2511) | (p506==2519) | (p506==2610) | (p506>=2691 & p506<=2696) | (p506==2699) /*
					*/ | (p506==2710) | (p506==2720) | (p506==2731) | (p506==2732) | (p506>=2811 & p506<=2813) | (p506>=2891 & p506<=2893) | (p506==2899) | (p506>=2911 & p506<=2915) /*
					*/ | (p506==2919) | (p506>=2921 & p506<=2927) | (p506==2929) | (p506==2930) | (p506==2999) | (p506==3000) | (p506==3110) | (p506==3120) | (p506==3130) | (p506==3140) | (p506==3150) /*
					*/ | (p506==3190) | (p506==3210) | (p506==3220) | (p506==3230) | (p506==3311) | (p506==3410) | (p506==3420) | (p506==3430) | (p506==3511) | (p506==3512) | (p506==3520) | (p506==3530) /*
					*/ | (p506==3591) | (p506==3592) | (p506==3599) | (p506==3710) | (p506==3720) | (p506==7250)
replace v05RamAct =5  if (p506==4510) | (p506==4520) | (p506==4530) | (p506==4540) | (p506==4550)


replace v05RamAct =6  if (p506==2230) | (p506==5010) | (p506==5030) | (p506==5040) | (p506==5050) | (p506==5110) | (p506==5121) | (p506==5122) | (p506==5131) | (p506==5139) /*
					*/ | (p506>=5141 & p506<=5143) | (p506==5149) | (p506==5150) | (p506==5190) | (p506==5211) | (p506==5219) | (p506==5220) | (p506>=5231 & p506<=5234) | (p506==5239) | (p506==5240) /*
					*/ | (p506==5251) | (p506==5252) | (p506==5259) | (p506==5270) | (p506==7130)
replace v05RamAct =7  if (p506==4010) | (p506==4020) | (p506==4030) | (p506==4100) | (p506==6010) | (p506>=6021 & p506<=6023) | (p506==6030) | (p506==6110) | (p506==6120) | (p506==6210) | (p506==6220) /*
					*/ | (p506>=6301 & p506<=6304) | (p506==6309) | (p506==6411) | (p506==6412) | (p506==6420) | (p506==6511) | (p506==6519) | (p506==6591) | (p506==6592) | (p506==6599) /*
					*/ | (p506>=6601 & p506<=6603) | (p506==6711) | (p506==6712) | (p506==6719) | (p506==6720) | (p506==7010) | (p506==7020) | (p506>=7111 & p506<=7113)  | (p506>=7121 & p506<=7123) /*
					*/ | (p506==7129) | (p506==7210) | (p506==7220) | (p506==7230) | (p506==7240) | (p506==7290) | (p506==7310) | (p506==7320) | (p506>=7411 & p506<=7414) | (p506==7421) | (p506==7422) /*
					*/ | (p506==7430) | (p506>=7491 & p506<=7493) | (p506==7495) | (p506==7499) | (p506>=7511 & p506<=7514) | (p506>=7521 & p506<=7523) | (p506==7530) | (p506==8010) | (p506==8021) /*
					*/ | (p506==8022) | (p506==8030) | (p506==8090) | (p506==8511) | (p506==8512) | (p506==8519) | (p506==8520) | (p506==8531) | (p506==8532) | (p506==9000) | (p506==9111) | (p506==9112) /*
					*/ | (p506==9120) | (p506==9191) | (p506==9192) | (p506==9199) | (p506>=9211 & p506<=9214) | (p506==9219) | (p506==9220) | (p506>=9231 & p506<=9233) | (p506==9241) | (p506==9249) /*
					*/ | (p506==9900)
replace v05RamAct =8  if (p506==5020) | (p506==5260) | (p506==5510) | (p506==5520) | (p506==7494) | (p506>=9301 & p506<=9303) | (p506==9309)
replace v05RamAct =9  if (p506==9500)
replace v05RamAct =10 if (p506==9999)

label define v05RamAct 1 "Agricult., ganadería, silvicultura y pesca" 2 "Minería" 3 "Industria de bienes de consumo" 4 "Industria de bienes intermedios y de capital" 5 "Construcción" 6 "Comercio por mayor y menor" 7 "Servicios no personales" 8 "Servicios personales" 9 "Hogares" 10 "No especificado"
label values v05RamAct v05RamAct

tab v05RamAct [iw=fac500] if (v03ConAct ==1), m

* Ingreso laboral mensual de las ocupaciones principal y secundarias (En nuevos soles). Solo para la PEA ocupada (v03ConAct =1)
*******************************************************************************************************************************
 
gen v06IngLab1 = (p524a1 + p529t + p530a + p536 + p538a1 + p540t + p541a + p543 + p544t)  if (v05RamAct==1), clear
gen v06IngLab2 = (p524a1 + p529t + p530a + p536 + p538a1 + p540t + p541a + p543 + p544t)  if (v05RamAct==2), replace
gen v06IngLab3 = (p524a1 + p529t + p530a + p536 + p538a1 + p540t + p541a + p543 + p544t)  if (v05RamAct==3), replace
gen v06IngLab4 = (p524a1 + p529t + p530a + p536 + p538a1 + p540t + p541a + p543 + p544t)  if (v05RamAct==4), replace
gen v06IngLab5 = (p524a1 + p529t + p530a + p536 + p538a1 + p540t + p541a + p543 + p544t)  if (v05RamAct==5), replace
gen v06IngLab6 = (p524a1 + p529t + p530a + p536 + p538a1 + p540t + p541a + p543 + p544t)  if (v05RamAct==6), replace
gen v06IngLab7 = (p524a1 + p529t + p530a + p536 + p538a1 + p540t + p541a + p543 + p544t)  if (v05RamAct==7), replace
gen v06IngLab8 = (p524a1 + p529t + p530a + p536 + p538a1 + p540t + p541a + p543 + p544t)  if (v05RamAct==8), replace
gen v06IngLab9 = (p524a1 + p529t + p530a + p536 + p538a1 + p540t + p541a + p543 + p544t)  if (v05RamAct==9), replace
gen v06IngLab10 =(p524a1 + p529t + p530a + p536 + p538a1 + p540t + p541a + p543 + p544t)  if (v05RamAct==10), replace



tab v06IngLab [iw=fac500]



