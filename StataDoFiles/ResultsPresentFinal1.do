/* We just want a way to present the results of the model and talk about summary statistics */

clear all
use "/user/mjbaker/TV/ad_sample/AveragedDataDynamicREr.dta"
cd /user/mjbaker/TV/ad_sample/TablesFigures

//gen keeper1=lnpps!=.
//bysort market: egen keeper2=total(keeper1)
//keep if keeper2>1

bysort market: gen last=_n==_N
gen Size=1
replace Size=2 if ACS_HH>1000000
replace Size=3 if ACS_HH>3000000

gen cable_station=1-local_station
bysort market timeslot: egen tlocal=total(local_station)
bysort market timeslot: egen tcable=total(cable_station)
bysort market timeslot: egen ttotal=count(si)

bysort market: gen counter=.
bysort market: replace counter=1 if _n==1
bysort market: replace counter=2 if _n==2
bysort market: replace counter=3 if _n==3

gen stationCount=tlocal
replace stationCount=tlocal if counter==1
replace stationCount=tcable if counter==2
replace stationCount=ttotal if counter==3

label define statype 1 "Local Stations" 2 "Cable Stations" 3 "Total Stations"
label val counter statype
label var counter "Station Type"

tabout counter if (Size==1 & counter!=.) using MarketChars.tex, style(tex) sum ///
	c(mean stationCount sd stationCount min stationCount max stationCount) npos(lab) ///
	clab ( mean sd min max) replace ptot(none) topf(topMarketChars.tex) topstr(14cm) h2(&\multicolumn{4}{c}{Small Markets $<$ 2.5 mil. (N=40)} \\) 
tabout counter if (Size==2 & counter!=.) using MarketChars.tex, style(tex) sum ///
	c(mean stationCount sd stationCount min stationCount max stationCount) npos(label) ///
	clab( mean sd min max) append ptot(none) h2(& & & & \\ &\multicolumn{4}{c}{Mid-size Markets 2.5 to 5 mil. (N=43)} \\)
tabout counter if (Size==3 & counter!=.) using MarketChars.tex, style(tex) sum ///
	c(mean stationCount sd stationCount min stationCount max stationCount) ///
	clab( mean sd min max) append ptot(none) botf(botMarketChars.tex) h2(& & & & \\ &\multicolumn{4}{c}{Large Markets $>$ 5 mil. (N=18)} \\ )

gen cases=.
replace cases=1 if timeslot==3 & lnews
replace cases=2 if timeslot==3 & nnews
replace cases=3 if timeslot==3 & otherl
replace cases=4 if timeslot==3 & otherc

replace cases=5 if timeslot==4 & lnews
replace cases=6 if timeslot==4 & nnews
replace cases=7 if timeslot==4 & otherl
replace cases=8 if timeslot==4 & otherc

replace cases=9 if timeslot==5 & lnews
replace cases=10 if timeslot==5 & nnews
replace cases=11 if timeslot==5 & otherl
replace cases=12 if timeslot==5 & otherc

replace cases=13 if timeslot==6 & lnews
replace cases=14 if timeslot==6 & nnews
replace cases=15 if timeslot==6 & otherl
replace cases=16 if timeslot==6 & otherc

replace cases=17 if timeslot==7 & lnews
replace cases=18 if timeslot==7 & nnews
replace cases=19 if timeslot==7 & otherl
replace cases=20 if timeslot==7 & otherc

replace cases=21 if timeslot==8 & lnews
replace cases=22 if timeslot==8 & nnews
replace cases=23 if timeslot==8 & otherl
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
label var cases "Broadcasts and Times"

/* Let's add in a price per viewer variable */

gen ppv=pps*8*60/(si*ACS_HH)

tabout cases using SharesPrices.tex, sum c(mean si N si mean pps mean ppv N pps) ///
	clab(Share Share Price-per-Second Price-per-viewer Prices) ///
	style(tex) format(4c 0c 2m 2m 0c) replace ///
	botf(botSharesPrices.tex) topf(topSharesPrices.tex) ptot(none)

bysort market timeslot: gen timeind=_n==_N

/* Generate aggregate tables now */

//bysort market timeslot: egen stotal=total(si)

la var timeslot "Time Slot"
la def timeslotLabel 3 "5:00-5:30" 4 "5:30-6:00" 5 "6:00-6:30" 6 "6:30-7:00"  7 "7:00-7:30" 8 "7:30-8:00" 
la val timeslot timeslotLabel

la var slnews "Local news (sim)"
la var snnews "Nat. news "
la var sotherl "Entertainment"
la var sotherc "Other cable"
la var stotal "Total"
	
tabout timeslot if timeind using TotalShares.tex, ///
	c(mean slnews mean snnews mean sotherl mean sotherc mean stotal) ///
	clab(Local_news Nat'l_news Other_local Other_cable Total) ///
	format(4c 4c 4c 4c 4c) style(tex) topf(topTotalShares.tex) botf(botTotalShares.tex) replace sum

bysort market timeslot: egen totlnews=total(lnews)
bysort market timeslot: egen totnnews=total(nnews)
bysort market timeslot: egen tototherl=total(otherl)
bysort market timeslot: egen tototherc=total(otherc)

gen freqlnews=totlnews/tlocal*100
gen freqnnews=totnnews/tlocal*100
gen freqotherl=tototherl/tlocal*100

tabout timeslot using BroadCastCount.tex if timeind, ///
	c(mean totlnews mean freqlnews mean totnnews mean freqnnews mean tototherl mean freqotherl) ///
	clab(Local_news Freq Nat'l_news Freq Other_local Freq) ///
	sum format(1c 1p 1c 1p 1c 1p) style(tex) topf(topBroadCastCount.tex) botf(botBroadCastCount.tex)	///
	replace

bysort market timeslot: gen lastm=_n==_N

graph hbar (mean) slnews sotherl snnews sotherc if lastm, over(timeslot) stack ///
	legend(label(1 "Local News") label(2 "Entertainment") label(3 "National News") label(4 "Other Cable"))
graph export "/user/mjbaker/TV/ad_sample/TablesFigures/SharesPic.eps", as(eps) preview(off) replace

graph hbar (mean)totlnews tototherl totnnews if lastm, over(timeslot) stack ///
	legend(label(1 "Local News") label(2 "Entertainment") label(3 "National News"))
graph export "/user/mjbaker/TV/ad_sample/TablesFigures/BroadcastsPic.eps", as(eps) preview(off) replace	
	 

bysort market timeslot: 







