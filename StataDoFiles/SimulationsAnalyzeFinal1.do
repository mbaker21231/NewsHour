/* Analyze the simulations */

clear all
set more off
cd /user/mjbaker/TV/ad_sample/TablesFigures
use /user/mjbaker/TV/ad_sample/DataWithSims2

/* Ultimately, we will present stuff just as we did before */

bysort market: egen countPrice=count(pps)
keep if countPrice>1

bysort market timeslot: gen last=_n==_N /* Useful for some tables */

/* let's use the dummies to get shares at the appropriate market level */

/* SHARE MAXIMIZATION PROFILES */

local lnewsloc
local nnewsloc
local otherlloc
local slnewsloc
local snnewsloc
local sotherlloc

scalar redraws=50

forvalues i=1/`=redraws' {
	local lnewsloc "`lnewloc' lnews_sim`i'"
	local nnewsloc "`nnewsloc' nnews_sim`i'"
	local otherlloc "`otherlloc' otherl_sim`i'"
}

egen lnews_MSM=rowmean(`lnewsloc')	/* Here "MSM" is a mnemonic for mean - share maxing profile */
egen nnews_MSM=rowmean(`nnewsloc')
egen otherl_MSM=rowmean(`otherlloc')

local sharesloc
local pricesloc
local surplsloc
local ppvloc

*Prices are in logs!
* what if we try and include price-per-viewer.


forvalues i=1/`=redraws' {
	replace prices_sim`i'=exp(prices_sim`i')
	gen ppv_sim`i'=prices_sim`i'*8*60/(shares_sim`i'*ACS_HH)
}	

forvalues i=1/`=redraws' {
	local sharesloc "`sharesloc' shares_sim`i'"
	local pricesloc "`pricesloc' prices_sim`i'"
	local surplsloc "`surplsloc' surp_sim`i'"
	local ppvloc "`ppvloc' ppv_sim`i'"
}

egen shares_MSM=rowmean(`sharesloc')
egen prices_MSM=rowmean(`pricesloc')
egen surpls_MSM=rowmean(`surplsloc')
egen ppv_MSM=rowmean(`ppvloc')

/* A first thing we might do is round the information so we see what is going on */

replace lnews_MSM=round(lnews_MSM)
replace nnews_MSM=round(nnews_MSM)
replace otherl_MSM=round(otherl_MSM)

gen cases=.
replace cases=1 if timeslot==3 & lnews_MSM
replace cases=2 if timeslot==3 & nnews_MSM
replace cases=3 if timeslot==3 & otherl_MSM
replace cases=4 if timeslot==3 & otherc

replace cases=5 if timeslot==4 & lnews_MSM
replace cases=6 if timeslot==4 & nnews_MSM
replace cases=7 if timeslot==4 & otherl_MSM
replace cases=8 if timeslot==4 & otherc

replace cases=9 if timeslot==5 & lnews_MSM
replace cases=10 if timeslot==5 & nnews_MSM
replace cases=11 if timeslot==5 & otherl_MSM
replace cases=12 if timeslot==5 & otherc

replace cases=13 if timeslot==6 & lnews_MSM
replace cases=14 if timeslot==6 & nnews_MSM
replace cases=15 if timeslot==6 & otherl_MSM
replace cases=16 if timeslot==6 & otherc

replace cases=17 if timeslot==7 & lnews_MSM
replace cases=18 if timeslot==7 & nnews_MSM
replace cases=19 if timeslot==7 & otherl_MSM
replace cases=20 if timeslot==7 & otherc

replace cases=21 if timeslot==8 & lnews_MSM
replace cases=22 if timeslot==8 & nnews_MSM
replace cases=23 if timeslot==8 & otherl_MSM
replace cases=24 if timeslot==8 & otherc

label def caseLabel 1 "5:00 - Local news" ///
		    2 "       Natl. news" ///
		    3 "    Entertainment" ///
		    4 "      Other cable" ///
		    5 "5:30 - Local news" ///
		    6 "       Natl. news" ///
		    7 "    Entertainment" ///
		    8 "      Other cable" ///
		    9 "6:00 - Local news" ///
		    10 "       Natl. news" ///
		    11 "    Entertainment" ///
		    12 "      Other cable" ///
		    13 "6:30 - Local news" ///
		    14 "       Natl. news" ///
		    15 "    Entertainment" ///
		    16 "      Other cable" ///
		    17 "7:00 - Local news" ///
		    18 "       Natl. news" ///
		    19 "    Entertainment" ///
		    20 "      Other cable" ///
		    21 "7:30 - Local news" ///
		    22 "       Natl. news" ///
		    23 "    Entertainment" ///
		    24 "      Other cable" 
label val cases caseLabel
label var cases "Program types and times"

latabstat shares_MSM, by(cases) statistics(mean) longstub format(%9.5f) columns(variables)

tabout cases using SharesPrices_MSM.tex, sum c(mean shares_MSM N shares_MSM mean prices_MSM mean ppv_MSM N prices_MSM) ///
	clab( Share Share Price-per-Second Price-per-Viewer Prices) ///
	style(tex) format(4c 0c 2m 2m 0c) replace ///
	botf(botSharesPricesMSM.tex) topf(topSharesPricesMSM.tex) ptot(none)

bysort market timeslot: gen timeind=_n==_N

/* Generate aggregate tables now */

bysort market timeslot: egen slnews_MSM=total(shares_MSM*lnews_MSM)
bysort market timeslot: egen snnews_MSM=total(shares_MSM*nnews_MSM)
bysort market timeslot: egen sotherl_MSM=total(shares_MSM*otherl_MSM)
bysort market timeslot: egen sotherc_MSM=total(shares_MSM*otherc)
bysort market timeslot: egen stotal_MSM=total(shares_MSM)

la var timeslot "Time Slot"
la def timeslotLabel 3 "5:00-5:30" 4 "5:30-6:00" 5 "6:00-6:30" 6 "6:30-7:00"  7 "7:00-7:30" 8 "7:30-8:00" 
la val timeslot timeslotLabel

la var slnews_MSM "Local news"
la var snnews_MSM "Nat. news"
la var sotherl_MSM "Entertainment"
la var sotherc_MSM "Other cable"
la var stotal_MSM "Total"
	
latabstat slnews snnews sotherl sotherc if timeind, by(timeslot) statistics(mean) longstub columns(variables) format(%9.3f)

gen timeind2=0
bysort market timeslot: replace timeind2=timeslot*2-5 if _n==_N-1
bysort market timeslot: replace timeind2=timeslot*2-4 if _n==_N

label def complabel 1 "5:00 - actual   " ///
		    2 "       simulated" ///
		    3 "5:30 - actual   " ///
		    4 "       simulated" ///
		    5 "6:00 - actual   " ///
		    6 "       simulated" ///
		    7 "6:30 - actual   " ///
		    8 "       simulated" ///
		    9 "7:00 - actual   " ///
		    10 "       simulated" ///
		    11 "7:30 - actual  "  ///
		    12 "       simulated"
		    
tabout timeslot if timeind==1 using TotalShares_MSM.tex, ///
	c(mean slnews_MSM mean snnews_MSM mean sotherl_MSM mean sotherc_MSM mean stotal_MSM) ///
	clab(Local_news Nat'l_news Entertainment Other_cable Total) ///
	format(4c 4c 4c 4c 4c) style(tex) topf(topTotalSharesMSM.tex) botf(botTotalSharesMSM.tex) replace sum

replace slnews_MSM=slnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace sotherl_MSM=sotherl if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace snnews_MSM=snnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace stotal_MSM=stotal if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace sotherc_MSM=sotherc if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
	
label val timeind2 complabel
label var timeind2 "Time Slots"	
	
tabout timeind2 if timeind2>0 using TotalShares_MSMcomp.tex, ///
	c(mean slnews_MSM mean snnews_MSM mean sotherl_MSM mean sotherc_MSM mean stotal_MSM) ///
	clab(Local_news Nat'l_news Entertainment Other_cable Total) ///
	format(4c 4c 4c 4c 4c) style(tex) topf(topTotalSharesMSM.tex) botf(botTotalSharesMSM.tex) replace sum

graph hbar (mean) slnews_MSM sotherl_MSM snnews_MSM sotherc_MSM if timeind2>0, over(timeind2) stack ///
	legend(label(1 "Local News") label(2 "Entertainment") label(3 "National News") label(4 "Other Cable")) 
graph export "/user/mjbaker/TV/ad_sample/TablesFigures/SharesComp_MSM.eps", as(eps) preview(off) replace

bysort market timeslot: egen totlnews_MSM=total(lnews_MSM)
bysort market timeslot: egen tototherl_MSM=total(otherl_MSM)
bysort market timeslot: egen totnnews_MSM=total(nnews_MSM)

bysort market timeslot: egen totlnews=total(lnews)
bysort market timeslot: egen totnnews=total(nnews)
bysort market timeslot: egen tototherl=total(otherl)

bysort market timeslot: egen totalls=total(local_station)

gen freqlnews_MSM=totlnews_MSM/totalls*100
gen freqnnews_MSM=totnnews_MSM/totalls*100
gen freqotherl_MSM=tototherl_MSM/totalls*100

gen freqlnews=totlnews/totalls*100
gen freqnnews=totnnews/totalls*100
gen freqotherl=tototherl/totalls*100

replace totlnews_MSM=totlnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace tototherl_MSM=tototherl if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace totnnews_MSM=totnnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11

replace freqlnews_MSM=freqlnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace freqotherl_MSM=freqotherl if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace freqnnews_MSM=freqnnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11


tabout timeind2 using BroadCastCount_MSMcomp.tex if timeind2>0, ///
	c(mean totlnews_MSM mean freqlnews_MSM mean totnnews_MSM mean freqnnews_MSM mean tototherl_MSM mean freqotherl_MSM) ///
	clab(Local_news Freq Nat'l_news Freq Entertainment Freq) ///
	sum format(1c 1p 1c 1p 1c 1p) style(tex) topf(topBroadCastCountMSM.tex) botf(botBroadCastCountMSM.tex)	///
	replace
	
/* Mean number of "other" "local news" and "national news" broadcasts per timeslot */

bysort stationid: egen sumGame=total(game)
gen gamer=0
replace gamer=1 if sumGame>4

generat indicator=2 if gamer
replace indicator=0 if lnews_MSM & gamer
replace indicator=1 if nnews_MSM  & gamer
tab indicator
label define pt 0 "Local news" 1 "National News" 2 "Entertainment"
label val indicator pt

preserve
keep if gamer
gen ts=timeslot-2
sqset indicator stationid ts 
sqtab , ranks(1/10)

sqindexplot, color(red cyan green) legend(label(1 "Local News") label(2 "National News") label(3 "Entertainment"))  title("Viewers") ytitle("Stations") xmtick(0(.5)6) xlabel(0(.5)6.5) ///
	xlabel(0 " " .5  "5:00" 1 " " 1.5 "5:30" 2 " " 2.5 "6:00" 3 " " 3.5 "6:30" 4 " " 4.5 "7:00" 5 " " 5.5 "7:30" 6 " " 6.5 "8:00") xtitle("Time slots")  legend(pos(6))
graph save g1, replace
graph export "/user/mjbaker/TV/ad_sample/TablesFigures/SequencePlot_MSM.eps", as(eps) preview(off) replace

/* Last thing we might want to do is provide some information on lost revenues */
/* First, percentage difference */

gen ppdiff=100*(prices_MSM-pps)/(pps+prices_MSM)/2
gen psdiff=100*(shares_MSM-si)/(shares_MSM+si)/2

tabout timeslot using PriceComp_MSM.tex if pps!=., sum c(mean pps mean si mean prices_MSM mean shares_MSM mean ppdiff mean psdiff N psdiff) ///
	clab(pps share sim'd_pps sim'd_share \%_pps_diff.\%_share_diff. N) ///
	style(tex) format(2m 4c 2m 4c 2p 2p 0c) replace ///
	botf(botPriceCompMSM.tex) topf(topPriceCompMSM.tex) ptot(none)
/* Reshape so we can tabulate sequences */
keep stationid timeslot indicator
reshape wide indicator, i(stationid) j(timeslot)

gen seq=11
replace seq=1 if indicator3==3 & indicator4==3 & indicator5==1 & indicator6==3 & indicator7==3 & indicator8==3
replace seq=2 if indicator3==1 & indicator4==1 & indicator5==1 & indicator6==3 & indicator7==3 & indicator8==3
replace seq=3 if indicator3==3 & indicator4==1 & indicator5==1 & indicator6==3 & indicator7==3 & indicator8==1
replace seq=4 if indicator3==3 & indicator4==1 & indicator5==1 & indicator6==3 & indicator7==3 & indicator8==3
replace seq=5 if indicator3==3 & indicator4==1 & indicator5==3 & indicator6==1 & indicator7==3 & indicator8==3
replace seq=6 if indicator3==3 & indicator4==3 & indicator5==1 & indicator6==1 & indicator7==3 & indicator8==3
replace seq=7 if indicator3==3 & indicator4==3 & indicator5==1 & indicator6==3 & indicator7==3 & indicator8==1
replace seq=8 if indicator3==1 & indicator4==1 & indicator5==2 & indicator6==1 & indicator7==1 & indicator8==1
replace seq=9 if indicator3==1 & indicator4==1 & indicator5==3 & indicator6==3 & indicator7==1 & indicator8==3
replace seq=10 if indicator3==1 & indicator4==3 & indicator5==1 & indicator6==1 & indicator7==3 & indicator8==1

label define seqlab 1 "eeleee" 2 "llleee" 3 "elleel" 4 "elleee" 5 "elelee" 6 "eellee" 7 "eeleel" 8 "llnlll" 9 "llelee" 10 "lellel" 11 "other"
label val seq seqlab
label variable seq "Programming lineups"

tabout seq using CommonSeqs_MSM.tex, style(tex) cells(freq col cum) format(0 1) oneway replace ///
	topf(top5.tex) botf(bot.tex) clab(No. Col_% Cum_%)

restore
/* So goes things from the perspective of Viewers - not enough local news! */
/* What about stations? */



local lnewsloc
local nnewsloc
local otherlloc
local slnewsloc
local snnewsloc
local sotherlloc

forvalues i=1/`=redraws' {
	local lnewsloc "`lnewloc' lnews_prm`i'"
	local nnewsloc "`nnewsloc' nnews_prm`i'"
	local otherlloc "`otherlloc' otherl_prm`i'"
}

egen lnews_PMP=rowmean(`lnewsloc')	/* Here "PMP" is a mnemonic for mean - share maxing profile */
egen nnews_PMP=rowmean(`nnewsloc')
egen otherl_PMP=rowmean(`otherlloc')

/* Prices are in logs! */
forvalues i=1/`=redraws' {
	replace prices_prm`i'=exp(prices_prm`i')
	gen ppv_prm`i'=prices_prm`i'*8*60/(shares_prm`i'*ACS_HH)
}	

local sharesloc
local pricesloc
local surpsloc
local ppvloc

forvalues i=1/`=redraws' {
	local sharesloc "`sharesloc' shares_prm`i'"
	local pricesloc "`pricesloc' prices_prm`i'"
	local surpsloc "`surpsloc' surp_prm`i'"
	local ppvloc "`ppvloc' ppv_prm`i'"
}


egen shares_PMP=rowmean(`sharesloc')
egen prices_PMP=rowmean(`pricesloc')
egen surpls_PMP=rowmean(`surpsloc')
egen ppv_PMP=rowmean(`ppvloc')

replace lnews_PMP=round(lnews_PMP)
replace nnews_PMP=round(nnews_PMP)
replace otherl_PMP=round(otherl_PMP)

capture drop cases
gen cases=.
replace cases=1 if timeslot==3 & lnews_PMP
replace cases=2 if timeslot==3 & nnews_PMP
replace cases=3 if timeslot==3 & otherl_PMP
replace cases=4 if timeslot==3 & otherc

replace cases=5 if timeslot==4 & lnews_PMP
replace cases=6 if timeslot==4 & nnews_PMP
replace cases=7 if timeslot==4 & otherl_PMP
replace cases=8 if timeslot==4 & otherc

replace cases=9 if timeslot==5 & lnews_PMP
replace cases=10 if timeslot==5 & nnews_PMP
replace cases=11 if timeslot==5 & otherl_PMP
replace cases=12 if timeslot==5 & otherc

replace cases=13 if timeslot==6 & lnews_PMP
replace cases=14 if timeslot==6 & nnews_PMP
replace cases=15 if timeslot==6 & otherl_PMP
replace cases=16 if timeslot==6 & otherc

replace cases=17 if timeslot==7 & lnews_PMP
replace cases=18 if timeslot==7 & nnews_PMP
replace cases=19 if timeslot==7 & otherl_PMP
replace cases=20 if timeslot==7 & otherc

replace cases=21 if timeslot==8 & lnews_PMP
replace cases=22 if timeslot==8 & nnews_PMP
replace cases=23 if timeslot==8 & otherl_PMP
replace cases=24 if timeslot==8 & otherc

label val cases caseLabel

latabstat shares_PMP, by(cases) statistics(mean) longstub format(%9.5f) columns(variables)

tabout cases using SharesPrices_PMP.tex, sum c(mean shares_PMP N shares_PMP mean prices_PMP mean ppv_PMP N prices_PMP) ///
	clab(Share Share Price-per-Second Price-per-Viewer Prices) ///
	style(tex) format(4c 0c 2m 2m 0c) replace ///
	botf(botSharesPricesPRM.tex) topf(topSharesPricesPRM.tex) ptot(none)

bysort market timeslot: egen slnews_PMP=total(shares_PMP*lnews_PMP)
bysort market timeslot: egen snnews_PMP=total(shares_PMP*nnews_PMP)
bysort market timeslot: egen sotherl_PMP=total(shares_PMP*otherl_PMP)
bysort market timeslot: egen sotherc_PMP=total(shares_PMP*otherc)
bysort market timeslot: egen stotal_PMP=total(shares_PMP)

la var slnews_PMP "Local news (sim)"
la var snnews_PMP "Nat. news "
la var sotherl_PMP "Entertainment"
la var sotherc_PMP "Other cable"
la var stotal_PMP "Total"
	
tabout timeslot if timeind using TotalShares_PMP.tex, ///
	c(mean slnews_PMP mean snnews_PMP mean sotherl_PMP mean sotherc_PMP mean stotal_PMP) ///
	clab(Local_news Nat'l_news Entertainment Other_cable Total) ///
	format(4c 4c 4c 4c 4c) style(tex) topf(topTotalSharesPRM.tex) botf(botTotalSharesPRM.tex) replace sum

replace slnews_PMP=slnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace sotherl_PMP=sotherl if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace snnews_PMP=snnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace stotal_PMP=stotal if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace sotherc_PMP=sotherc if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11

tabout timeind2 if timeind2>0 using TotalShares_PMPcomp.tex, ///
	c(mean slnews_PMP mean snnews_PMP mean sotherl_PMP mean sotherc_PMP mean stotal_PMP) ///
	clab(Local_news Nat'l_news Entertainment Other_cable Total) ///
	format(4c 4c 4c 4c 4c) style(tex) topf(topTotalSharesPRM.tex) botf(botTotalSharesPRM.tex) replace sum

graph hbar (mean) slnews_PMP sotherl_PMP snnews_PMP sotherc_PMP if timeind2>0, over(timeind2) stack legend(label(1 "Local News") label(2 "Entertainment") label(3 "National News") label(4 "Other Cable"))
graph export "/user/mjbaker/TV/ad_sample/TablesFigures/SharesComp_PMP.eps", as(eps) preview(off) replace


bysort market timeslot: egen totlnews_PMP=total(lnews_PMP)
bysort market timeslot: egen tototherl_PMP=total(otherl_PMP)
bysort market timeslot: egen totnnews_PMP=total(nnews_PMP)

gen freqlnews_PMP=totlnews_PMP/totalls*100
gen freqnnews_PMP=totnnews_PMP/totalls*100
gen freqotherl_PMP=tototherl_PMP/totalls*100

replace totlnews_PMP=totlnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace tototherl_PMP=tototherl if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace totnnews_PMP=totnnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11

replace freqlnews_PMP=freqlnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace freqotherl_PMP=freqotherl if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace freqnnews_PMP=freqnnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11


tabout timeind2 using BroadCastCount_PMPcomp.tex if timeind2>0, ///
	c(mean totlnews_PMP mean freqlnews_PMP mean totnnews_PMP mean freqnnews_PMP mean tototherl_PMP mean freqotherl_PMP) ///
	clab(Local_news Freq Nat'l_news Freq Entertianment Freq) ///
	sum format(1c 1p 1c 1p 1c 1p) style(tex) topf(topBroadCastCountPRM.tex) botf(botBroadCastCountPRM.tex)	///
	replace


tabout timeslot using BroadCastCount_PMP.tex if timeind, ///
	c(mean totlnews_PMP mean freqlnews_PMP mean totnnews_PMP mean freqnnews_PMP mean tototherl_PMP mean freqotherl_PMP) ///
	clab(Local_news Freq Nat'l_news Freq Entertainment Freq) ///
	sum format(1c 1p 1c 1p 1c 1p) style(tex) topf(topBroadCastCountPRM.tex) botf(botBroadCastCountPRM.tex)	///
	replace

tabout timeslot using BroadCastCountL_PMP.tex if timeind==1 & (ACS_HH>1000000), ///
	c(mean totlnews_PMP mean freqlnews_PMP mean totnnews_PMP mean freqnnews_PMP mean tototherl_PMP mean freqotherl_PMP) ///
	clab(Local_news Freq Nat'l_news Freq Entertainment Freq) ///	
	sum format(1c 1p 1c 1p 1c 1p) style(tex) topf(top4.tex) botf(bot.tex)	///
	replace
	
/* Mean number of "other" "local news" and "national news" broadcasts per timeslot */

drop indicator
preserve

generat indicator=2 if gamer
replace indicator=0 if lnews_PMP & gamer
replace indicator=1 if nnews_PMP & gamer
tab indicator

label val indicator pt

keep if gamer
gen ts=timeslot-2
sqset indicator stationid ts 
sqtab , ranks(1/10)

sqindexplot, color(red cyan green) legend(label(1 "Local News") label(2 "National News") label(3 "Entertainment")) title("Station joint profit") ytitle("Stations") xmtick(0(.5)6) xlabel(0(.5)6.5) ///
	xlabel(0 " " .5  "5:00" 1 " " 1.5 "5:30" 2 " " 2.5 "6:00" 3 " " 3.5 "6:30" 4 " " 4.5 "7:00" 5 " " 5.5 "7:30" 6 " " 6.5 "8:00") xtitle("Time slots")  legend(pos(6))
graph save g2, replace 
graph export "/user/mjbaker/TV/ad_sample/TablesFigures/SequencePlot_PRM.eps", as(eps) preview(off) replace

gen ppdiff=100*(prices_PMP-pps)/(pps+prices_PMP)/2
gen psdiff=100*(shares_PMP-si)/(shares_PMP+si)/2

tabout timeslot using PriceComp_PMP.tex if pps!=., sum c(mean pps mean si mean prices_PMP mean shares_PMP mean ppdiff mean psdiff N psdiff) ///
	clab(pps share sim'd_pps sim'd_share \%_pps_diff.\%_share_diff. N)  ///
	style(tex) format(2m 4c 2m 4c 2p 2p 0c) replace ///
	botf(botPriceCompPMP.tex) topf(topPriceCompPMP.tex) ptot(none)

restore

/* Perspective of advertisers */

local lnewsloc
local nnewsloc
local otherlloc
local slnewsloc
local snnewsloc
local sotherlloc

forvalues i=1/`=redraws' {
	local lnewsloc "`lnewloc' lnews_sum`i'"
	local nnewsloc "`nnewsloc' nnews_sum`i'"
	local otherlloc "`otherlloc' otherl_sum`i'"
}

egen lnews_SMP=rowmean(`lnewsloc')	/* Here "MSM" is a mnemonic for mean - share maxing profile */
egen nnews_SMP=rowmean(`nnewsloc')
egen otherl_SMP=rowmean(`otherlloc')

local sharesloc
local pricesloc
local surplsloc
local ppvloc


forvalues i=1/`=redraws' {
	replace prices_sum`i'=exp(prices_sum`i')
	gen ppv_sum`i'=prices_sum`i'*8*60/(shares_sum`i'*ACS_HH)

}	

forvalues i=1/`=redraws' {
	local sharesloc "`sharesloc' shares_sum`i'"
	local pricesloc "`pricesloc' prices_sum`i'"
	local surplsloc "`surplsloc' surp_sum`i'"
	local ppvloc "`ppvloc' ppv_sum`i'"
}


egen shares_SMP=rowmean(`sharesloc')
egen prices_SMP=rowmean(`pricesloc')
egen surpls_SMP=rowmean(`surpsloc')
egen ppv_SMP=rowmean(`ppvloc')

replace lnews_SMP=round(lnews_SMP)
replace nnews_SMP=round(nnews_SMP)
replace otherl_SMP=round(otherl_SMP)

capture drop cases
gen cases=.
replace cases=1 if timeslot==3 & lnews_SMP
replace cases=2 if timeslot==3 & nnews_SMP
replace cases=3 if timeslot==3 & otherl_SMP
replace cases=4 if timeslot==3 & otherc

replace cases=5 if timeslot==4 & lnews_SMP
replace cases=6 if timeslot==4 & nnews_SMP
replace cases=7 if timeslot==4 & otherl_SMP
replace cases=8 if timeslot==4 & otherc

replace cases=9 if timeslot==5 & lnews_SMP
replace cases=10 if timeslot==5 & nnews_SMP
replace cases=11 if timeslot==5 & otherl_SMP
replace cases=12 if timeslot==5 & otherc

replace cases=13 if timeslot==6 & lnews_SMP
replace cases=14 if timeslot==6 & nnews_SMP
replace cases=15 if timeslot==6 & otherl_SMP
replace cases=16 if timeslot==6 & otherc

replace cases=17 if timeslot==7 & lnews_SMP
replace cases=18 if timeslot==7 & nnews_SMP
replace cases=19 if timeslot==7 & otherl_SMP
replace cases=20 if timeslot==7 & otherc

replace cases=21 if timeslot==8 & lnews_SMP
replace cases=22 if timeslot==8 & nnews_SMP
replace cases=23 if timeslot==8 & otherl_SMP
replace cases=24 if timeslot==8 & otherc

la val cases caseLabel

latabstat shares_SMP, by(cases) statistics(mean) longstub format(%9.5f) columns(variables)

tabout cases using SharesPrices_SMP.tex, sum c(mean shares_SMP N shares_SMP mean prices_SMP mean ppv_SMP N prices_SMP) ///
	clab(Share Share Price-per-Second Price-per-Viewer Prices) ///
	style(tex) format(4c 0c 2m 2m 0c) replace ///
	botf(botSharesPricesSMP.tex) topf(topSharesPricesSMP.tex) ptot(none)

bysort market timeslot: egen slnews_SMP=total(shares_SMP*lnews_SMP)
bysort market timeslot: egen snnews_SMP=total(shares_SMP*nnews_SMP)
bysort market timeslot: egen sotherl_SMP=total(shares_SMP*otherl_SMP)
bysort market timeslot: egen sotherc_SMP=total(shares_SMP*otherc)
bysort market timeslot: egen stotal_SMP=total(shares_SMP)

la var slnews_SMP "Local news (sim)"
la var snnews_SMP "Nat. news "
la var sotherl_SMP "Entertainment"
la var sotherc_SMP "Other cable"
la var stotal_SMP "Total"
	
tabout timeslot if timeind using TotalShares_SMP.tex, ///
	c(mean slnews_SMP mean snnews_SMP mean sotherl_SMP mean sotherc_SMP mean stotal_SMP) ///
	clab(Local_news Nat'l_news Entertainment Other_cable Total) ///
	format(4c 4c 4c 4c 4c) style(tex) topf(topTotalSharesSMP.tex) botf(botTotalSharesSMP.tex) replace sum

replace slnews_SMP=slnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace sotherl_SMP=sotherl if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace snnews_SMP=snnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace stotal_SMP=stotal if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace sotherc_SMP=sotherc if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11

tabout timeind2 if timeind2>0 using TotalShares_SMPcomp.tex, ///
	c(mean slnews_SMP mean snnews_SMP mean sotherl_SMP mean sotherc_SMP mean stotal_SMP) ///
	clab(Local_news Nat'l_news Entertainment Other_cable Total) ///
	format(4c 4c 4c 4c 4c) style(tex) topf(topTotalSharesSMP.tex) botf(botTotalSharesSMP.tex) replace sum

graph hbar (mean) slnews_SMP sotherl_SMP snnews_SMP sotherc_SMP if timeind2>0, over(timeind2) stack legend(label(1 "Local News") label(2 "Other Local") label(3 "National News") label(4 "Other Cable"))
graph export "/user/mjbaker/TV/ad_sample/TablesFigures/SharesComp_SMP.eps", as(eps) preview(off) replace

bysort market timeslot: egen totlnews_SMP=total(lnews_SMP)
bysort market timeslot: egen tototherl_SMP=total(otherl_SMP)
bysort market timeslot: egen totnnews_SMP=total(nnews_SMP)

gen freqlnews_SMP=totlnews_SMP/totalls*100
gen freqnnews_SMP=totnnews_SMP/totalls*100
gen freqotherl_SMP=tototherl_SMP/totalls*100

replace totlnews_SMP=totlnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace tototherl_SMP=tototherl if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace totnnews_SMP=totnnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11

replace freqlnews_SMP=freqlnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace freqotherl_SMP=freqotherl if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11
replace freqnnews_SMP=freqnnews if timeind2==1 | timeind2==3 | timeind2==5 | timeind2==7 | timeind2==9 | timeind2==11

tabout timeind2 using BroadCastCount_SMPcomp.tex if timeind2>0, ///
	c(mean totlnews_SMP mean freqlnews_SMP mean totnnews_SMP mean freqnnews_SMP mean tototherl_SMP mean freqotherl_SMP) ///
	clab(Local_news Freq Nat'l_news Freq Entertainment Freq) ///
	sum format(1c 1p 1c 1p 1c 1p) style(tex) topf(topBroadCastCountSMP.tex) botf(botBroadCastCountSMP.tex)	///
	replace

tabout timeslot using BroadCastCount_SMP.tex if timeind, ///
	c(mean totlnews_SMP mean freqlnews_SMP mean totnnews_SMP mean freqnnews_SMP mean tototherl_SMP mean freqotherl_SMP) ///
	clab(Local_news Freq Nat'l_news Freq Entertainment Freq) ///
	sum format(1c 1p 1c 1p 1c 1p) style(tex) topf(topBroadCastCountSMP.tex) botf(botBroadCastCountSMP.tex)	///
	replace

tabout timeslot using BroadCastCountL_SMP.tex if timeind==1 & (ACS_HH>1000000), ///
	c(mean totlnews_SMP mean freqlnews_SMP mean totnnews_SMP mean freqnnews_SMP mean tototherl_SMP mean freqotherl_SMP) ///
	clab(Local_news Freq Nat'l_news Freq Entertainment Freq) ///	
	sum format(1c 1p 1c 1p 1c 1p) style(tex) topf(top4.tex) botf(bot.tex)	///
	replace
	
/* Mean number of "other" "local news" and "national news" broadcasts per timeslot */

preserve

generat indicator=2 if gamer
replace indicator=0 if lnews_SMP & gamer
replace indicator=1 if nnews_SMP & gamer
tab indicator
label val indicator pt

keep if gamer
gen ts=timeslot-2
sqset indicator stationid ts 
sqtab , ranks(1/10)

sqindexplot, color(red cyan green) legend(label(1 "Local News") label(2 "National News") label(3 "Entertainment")) title("Advertiser Surplus") ytitle("Stations") xmtick(0(.5)6) xlabel(0(.5)6.5) ///
	xlabel(0 " " .5  "5:00" 1 " " 1.5 "5:30" 2 " " 2.5 "6:00" 3 " " 3.5 "6:30" 4 " " 4.5 "7:00" 5 " " 5.5 "7:30" 6 " " 6.5 "8:00") xtitle("Time slots") legend(pos(6))
graph save g3, replace
graph export "/user/mjbaker/TV/ad_sample/TablesFigures/SequencePlot_SMP.eps", as(eps) preview(off) replace

/* recreate original for use in combination */

drop indicator
generat indicator=2 if gamer
replace indicator=0 if lnews & gamer
replace indicator=1 if nnews & gamer
tab indicator
label val indicator pt

sqindexplot, color(red cyan green) legend(label(1 "Local News") label(2 "National News") label(3 "Entertainment"))  title("Actual") ytitle("Stations") xmtick(0(.5)6) xlabel(0(.5)6.5) ///
	xlabel(0 " " .5  "5:00" 1 " " 1.5 "5:30" 2 " " 2.5 "6:00" 3 " " 3.5 "6:30" 4 " " 4.5 "7:00" 5 " " 5.5 "7:30" 6 " " 6.5 "8:00") xtitle("Time slots")  legend(pos(6))
graph save g0, replace
graph export "/user/mjbaker/TV/ad_sample/TablesFigures/SequencePlot_Actual.eps", as(eps) preview(off) replace


gen ppdiff=100*(prices_SMP-pps)/(pps+prices_SMP)/2
gen psdiff=100*(shares_SMP-si)/(shares_SMP+si)/2

tabout timeslot using PriceComp_SMP.tex if pps!=., sum c(mean pps mean si mean prices_SMP mean shares_SMP mean ppdiff mean psdiff N psdiff) ///
	clab(Actual_pps Simulated_pps Percentage_diffference N) ///
	style(tex) format(2m 4c 2m 4c 2p 2p 0c) replace ///
	botf(botPriceCompSMP.tex) topf(topPriceCompSMP.tex) ptot(none)

restore

grc1leg g0.gph g1.gph g2.gph g3.gph, title("Sequence plots") subtitle("Actual, Surplus, Profit, and Viewership Maximizing Programming Lineups")
graph export "/user/mjbaker/TV/ad_sample/TablesFigures/SequencePlot_Comb.eps", as(eps) preview(off) replace






