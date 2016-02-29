/* Basic program to set up the data for dynamic viewership model */

	clear all
	set matsize 1000
	set seed 8675309

	use "/user/mjbaker/TV/NielsenKantarPanel.dta" if year==2010 & sample==1 & h!=20

	capture log close

	cd "/user/mjbaker/TV/ad_sample"
	
	set more off

	gen timeslot=1 if h==16 & half==1
	replace timeslot=2 if h==16 & half==2
	replace timeslot=3 if h==17 & half==1
	replace timeslot=4 if h==17 & half==2
	replace timeslot=5 if h==18 & half==1
	replace timeslot=6 if h==18 & half==2
	replace timeslot=7 if h==19 & half==1
	replace timeslot=8 if h==19 & half==2

/* Here is a list of the biggest channels */

	local bigChans AMC APL BRVO CMD CNB CNN DISC DSNY ENT ESPN ESP2 FAM FOOD ///
		FX FXNC HALL HGTV HIST HLN LIF MNBC MTV NAN NICK NKJR SPK ///
		SYFY TBSC TCM TLC TNT TOON TRAV TRU TVL USA VH1
	
	gen bigchannel=0
	foreach v of local bigChans {
		disp "`v'"
		replace bigchannel=1 if Nstat=="`v'"
	}


/* Before dropping anything, get the data straight */

	bysort NDMACode: egen TotalPop1=max(TotalPop)
	replace TotalPop=TotalPop1

	gen selector=pps!=.
	bysort NDMAC: egen selector1=sum(selector)
	keep if selector1>0

// Let's try a different selection rule 
//	keep if bigchannel | local_station


// Impute Cable Station Viewing 

// Leaving this little block of code out for the time being

	egen double minviewers=min(viewers), by(NDMAC)
	replace viewers=1/2*minviewers if viewers==. & affil=="" 

// Make program viewing shares, non-viewing shares shares and within-group shares

	scalar adjfactor=2		// Revisit this at some point! What is the usual solution?
	egen double Mpop=mean(TotalPop), by(NDMACode)
	replace Mpop=Mpop*adjfactor
	label var Mpop "Mean TotalPop by DMA"
//	gen si = viewers/Mpop		

	/* Alternative viewership */
	replace ACS_HH=ACS_HH*2
	gen double si = viewers/ACS_HH
	
	egen double S=sum(si), by(NDMACode dow timeslot week)
	egen double maxS=max(S), by(NDMACode)
	replace sample=0 if maxS>1

	keep if sample==1		// Update the data

// Make categorical dummy for different sorts of programming

	* lnews, nnews , otherl otherc

	rename lnews l_all
	gen lnews =cond(l_all==1 & bc==1,1,0)
	gen cablenews=cond(Nstat=="CNN"|Nstat=="BBCA"|Nstat=="HLN"|Nstat=="WGN"|Nstat=="FXNC",1,0)
	gen nnews=cond(news==1 & l_all==0 & (bc==1|cablenews==1),1,0)
	gen otherl=cond(news==0 & bc==1,1,0)
	gen otherc=cond(bc==0 & lnews==0 & nnews==0 & otherl==0,1,0)

// Data to be used - the rest of the data functions as instruments

	gen pricedata=cond( pps!=. ,1,0)
	gen viewdata=cond(si!=. ,1,0)
	replace viewdata=0 if timeslot==1 | timeslot==2	/* Modification made here */

/* Might as well get rid of the viewdata at this point */

/* Let's make a sequence of dummies that reflects different viewership preferences at different times, perhaps */

	keep if viewdata
	
/* Render data into averages so we can actually wrassle with this problem! */

	egen stationid=group(NDMAName Nstat)

	bysort stationid timeslot : egen double meansi=mean(si)
	bysort stationid timeslot : egen double meanpps=mean(pps)
	bysort stationid timeslot : egen double meanlnews=mean(lnews)
	bysort stationid timeslot : egen double meanotherl=mean(otherl)
	bysort stationid timeslot : egen double meannnews=mean(nnews)
	bysort stationid timeslot : egen double meanotherc=mean(otherc)
	bysort stationid timeslot : gen keep =_n==_N
	
	keep if keep
	
/* Let's really trim this bad boy down */

	keep NDMAName NDMACode Nstat stationid timeslot affiliation local_station ///
		ACS_HH ACS_HHBlack ACS_HHHispanic ACS_MedianHHIncome ACS_HHWhite ACS_MedianAge ///
		Mpop meansi meanpps meanlnews meanotherl meannnews meanotherc
	
/* Fill in some other stuff just to be sure */
	sort NDMACode stationid timeslot
	tsset stationid timeslot
	tsfill, full
	
	local stringReplace NDMAName Nstat affiliation
	local numberReplace stationid timeslot local_station ACS_HH ACS_HHBlack ACS_HHHispanic ACS_MedianHHIncome ACS_HHWhite ACS_MedianAge Mpop NDMACode
	forvalues i=3/8 {
		foreach word of local stringReplace {
			bysort stationid: replace `word'=`word'[_n+1] if `word'==""
			bysort stationid: replace `word'=`word'[_n-1] if `word'==""
		}
		foreach word of local numberReplace {
			bysort stationid: replace `word'=`word'[_n+1] if `word'==.
			bysort stationid: replace `word'=`word'[_n-1] if `word'==.
		}
	}
	
	/* we also have to fill in stuff that varies */

	gen double lnsi=ln(meansi)
	bysort stationid: ipolate lnsi timeslot, gen(lnsid) epolate
	replace lnsi=lnsid if lnsi==.
	replace meansi=exp(lnsi) if meansi==.
	drop lnsid
	
	/* stuff to replace with averages */
	
	local AverageReplace meanlnews meanotherl meannnews meanotherc meanpps
	foreach word of local AverageReplace {
		bysort stationid: ipolate `word' timeslot, gen(holder) epolate
		replace `word'=holder if `word'==.
		drop holder
	}
	
	gen lnews=meanlnews>.5
	gen nnews=meannnews>.5
	gen otherl=lnews==0 & nnews==0 & local_station
	gen otherc=(lnews==0 & nnews==0 & otherl==0)	
	drop meanlnews meanotherl meannnews meanotherc
	rename meansi si
	rename meanpps pps
	
// A different sort of selection rule - keep the top 55 channels 
/*
	bysort NDMAC Nstat: egen aveView=mean(si)
	gen sorter=-aveView
	sort NDMAC timeslot sorter
	bysort NDMAC timeslot: gen n=_n

// Here is the selection rule 

	keep if n<=55
	sort NDMAC timeslot Nstat
*/
// Make within-class total shares:  
	
	bysort NDMAC timeslot: egen double slnews=total(si*lnews)
	bysort NDMAC timeslot: egen double snnews=total(si*nnews)
	bysort NDMAC timeslot: egen double sotherl=total(si*otherl)
	bysort NDMAC timeslot: egen double sotherc=total(si*otherc)
	
	bysort NDMAC timeslot: egen double stotal=total(si)
	gen double so=1-stotal

	gen double owngroupshare=lnews*slnews+nnews*snnews+otherl*sotherl+otherc*sotherc
			  
	gen double ln_swg=ln(si/owngroupshare) /* Log within-group share */
	
	gen double ln_swgXslnews=ln_swg*lnews   /* Interaction terms */
	gen double ln_swgXsnnews=ln_swg*nnews   /* Note we don't use these in ML estimation */ 
	gen double ln_swgXsotherl=ln_swg*otherl
	
// Dummies for station affiliation

	gen NBC=(aff=="NBC")
	gen CBS=(aff=="CBS")
	gen ABC=(aff=="ABC")
	gen FOX=(aff=="FOX")
	gen CW= (aff=="CW")
	gen TEL=(aff=="TEL")
	gen UNI=(aff=="UNI")
	gen AZA=(aff=="AZA")
	gen MNT=(aff=="MNT")
	gen PBS=(aff=="PBS")
	gen TLF=(aff=="TLF")

/* Dummy variable defining who is in the (static, one-shot) game and who isn't: */

	egen market=group(NDMACode)
	
	egen mt=group(NDMACode timeslot)	
	
	sort market mt stationid

	tab timeslot, gen(timed)
	tab NDMACode, gen(markket)
	
	global affils NBC ABC CBS FOX TEL UNI MNT 

	global lnshares ln_swg ln_swgXslnews ln_swgXsotherl ln_swgXsnnews // ln_swgXsother -> the omitted category
	
	gen double dln=ln(si)-ln(so)
	gen double lnpps=ln(pps)
	
	/* Get a price model in order */
	
	gen double lnsiXnnews=lnsi*nnews
	gen double lnsiXotherl=lnsi*otherl

	sort market stationid mt

/* Save the data for the next step */

	save /user/mjbaker/TV/ad_sample/AveragedData.dta, replace
	
/* Checking the data */

	bysort market timeslot: egen channels=count(si)
	
/* Technical Notes: 	
	this do file uses no Mata functions
	this do file uses data NielsenKantarPanel.dta
	this do file creates data AveragedData.dta */
