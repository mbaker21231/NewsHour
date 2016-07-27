/* First, open up the simulations so we get an answer */

clear all
set more off
use "/user/mjbaker/TV/ad_sample/AveragedDataDynamicREr.dta", clear
quietly do "/user/mjbaker/TV/ad_sample/AveragedDataDynamicMataFinal1.do"

/* First, let's use our usual code to keep the observations we need */

//gen keeper1=lnpps!=.
//bysort market: egen keeper2=total(keeper1)
//keep if keeper2>1

/* A little block of code to make a game-marker that pulls in everyone who 
   makes decisions regardless of whether they have a local news broadcast or not
   in their profile */
   
bysort stationid: egen totalG=total(game)
gen gameMarker=totalG>4

sort market stationid timeslot


mata
mata matuse /user/mjbaker/TV/ad_sample/Simulations, replace
mata matuse /user/mjbaker/TV/ad_sample/Results, replace

draws=25

st_view(datamkts=.,.,"market")
st_view(datagame=.,.,"game")
st_view(datagameMarker=.,.,"gameMarker")
markets=uniqrows(asarray_keys(ShareMaxProfiles)[,2])     
nummarkets=uniqrows(datamkts)
lnews_sim=J(rows(datamkts),draws,.)		/* Note that sim is not a mnemonic for sim, but for share maxing */
otherl_sim=J(rows(datamkts),draws,.)
nnews_sim=J(rows(datamkts),draws,.)
lnews_sum=J(rows(datamkts),draws,.)
otherl_sum=J(rows(datamkts),draws,.)
nnews_sum=J(rows(datamkts),draws,.)
lnews_prm=J(rows(datamkts),draws,.)
otherl_prm=J(rows(datamkts),draws,.)
nnews_prm=J(rows(datamkts),draws,.)

shares_sim=J(rows(datamkts),draws,.)
prices_sim=J(rows(datamkts),draws,.)
surp_sim=J(rows(datamkts),draws,.)

shares_sum=J(rows(datamkts),draws,.)
prices_sum=J(rows(datamkts),draws,.)
surp_sum=J(rows(datamkts),draws,.)

shares_prm=J(rows(datamkts),draws,.)
prices_prm=J(rows(datamkts),draws,.)
surp_prm=J(rows(datamkts),draws,.)
trouble=J(rows(datamkts),draws,.)

weights=J(rows(datamkts),draws,.)

end
/* Once again shuffle through the Nash equilibria and pick out only the unique ones */

mata
	NashProfilesNew=asarray_create("real",4)
	 NashProfitsNew=asarray_create("real",3)
	  NashSharesNew=asarray_create("real",3)
	indexes=asarray_keys(NashProfiles)	  

	for (d=1;d<=draws;d++) {
		for (i=1;i<=rows(markets);i++) {	
			places=mm_which(nummarkets[i,]:==datamkts)
			if (rows(places)>0) {
				Count=rows(uniqrows(select(indexes[,4],(indexes[,1]:==i):*(indexes[,2]:==d))))
				if (Count==1) {
					asarray(NashProfilesNew,(i,d,1,1),asarray(NashProfiles,(i,d,1,1)))
					asarray(NashProfilesNew,(i,d,2,1),asarray(NashProfiles,(i,d,2,1)))			
					asarray(NashProfilesNew,(i,d,3,1),asarray(NashProfiles,(i,d,3,1)))			
					asarray(NashProfitsNew,(i,d,1),asarray(NashProfits,(i,d,1)))			
					asarray(NashSharesNew,(i,d,1),asarray(NashShares,(i,d,1)))			
				}
				else {
					asarray(NashProfilesNew,(i,d,1,1),asarray(NashProfiles,(i,d,1,1)))
					asarray(NashProfilesNew,(i,d,2,1),asarray(NashProfiles,(i,d,2,1)))			
					asarray(NashProfilesNew,(i,d,3,1),asarray(NashProfiles,(i,d,3,1)))			
					asarray(NashProfitsNew,(i,d,1),asarray(NashProfits,(i,d,1)))			
					asarray(NashSharesNew,(i,d,1),asarray(NashShares,(i,d,1)))						
					pop=2
					tick=1
					do {
						noGo=0
						for (j=1;j<=tick;j++) {
							if (asarray(NashProfiles,(i,d,1,pop))==asarray(NashProfilesNew,(i,d,1,j))) noGo=1
						}
						if (noGo==0) {
							tick++
							asarray(NashProfilesNew,(i,d,1,tick),asarray(NashProfiles,(i,d,1,pop)))
							asarray(NashProfilesNew,(i,d,2,tick),asarray(NashProfiles,(i,d,2,pop)))			
							asarray(NashProfilesNew,(i,d,3,tick),asarray(NashProfiles,(i,d,3,pop)))			
							asarray(NashProfitsNew,(i,d,tick),asarray(NashProfits,(i,d,pop)))			
							asarray(NashSharesNew,(i,d,tick),asarray(NashShares,(i,d,pop)))							
						}
						pop++	
					} while (pop<=Count)
				}	
			}
		}
	}
	mata drop NashProfiles
	mata drop NashProfits
	mata drop NashShares
end

/* COde to deal with Nash equilibria */

mata
counter=2

for (d=1;d<=draws;d++) {
	for (i=1;i<=rows(markets);i++) {								/* Markets variable is same as before essentially */
		places=mm_which(nummarkets[i,]:==datamkts)
		if (rows(places)>0) {
			gamePlaces=mm_which((nummarkets[i,]:==datamkts):*datagameMarker)
			gameP=mm_which((nummarkets[i,]:==datamkts):*datagame)
		
			lnews_simp=colshape(asarray(ShareMaxProfiles,(d,markets[i],1)),1)
			otherl_simp=colshape(asarray(ShareMaxProfiles,(d,markets[i],2)),1)
			nnews_simp=colshape(asarray(ShareMaxProfiles,(d,markets[i],3)),1)
	
			lnews_sump=colshape(asarray(SurpMaxProfiles,(d,markets[i],1)),1)
			otherl_sump=colshape(asarray(SurpMaxProfiles,(d,markets[i],2)),1)
			nnews_sump=colshape(asarray(SurpMaxProfiles,(d,markets[i],3)),1)

			lnews_prmp=colshape(asarray(ProfMaxProfiles,(d,markets[i],1)),1)
			otherl_prmp=colshape(asarray(ProfMaxProfiles,(d,markets[i],2)),1)
			nnews_prmp=colshape(asarray(ProfMaxProfiles,(d,markets[i],3)),1)
	
			shares_simp=colshape(asarray(ShareMax,(d,markets[i],1)),1)
			prices_simp=colshape(asarray(ShareMax,(d,markets[i],2)),1)
			surp_simp=colshape(asarray(ShareMax,(d,markets[i],3)),1)

			shares_sump=colshape(asarray(SurpMax,(d,markets[i],1)),1)
			prices_sump=colshape(asarray(SurpMax,(d,markets[i],2)),1)
			surp_sump=colshape(asarray(SurpMax,(d,markets[i],3)),1)		/* For now the wrong number! */
	
			shares_prmp=colshape(asarray(ProfMax,(d,markets[i],1)),1)
			prices_prmp=colshape(asarray(ProfMax,(d,markets[i],2)),1)
			surp_prmp=colshape(asarray(ProfMax,(d,markets[i],3)),1)

			lnews_sim[places,d]=lnews_simp
			otherl_sim[places,d]=otherl_simp
			nnews_sim[places,d]=nnews_simp

			lnews_sum[places,d]=lnews_sump
			otherl_sum[places,d]=otherl_sump
			nnews_sum[places,d]=nnews_sump

			lnews_prm[places,d]=lnews_prmp
			otherl_prm[places,d]=otherl_prmp
			nnews_prm[places,d]=nnews_prmp
	
			shares_sim[places,d]=shares_simp
			prices_sim[gamePlaces,d]=prices_simp
			surp_sim[gamePlaces,d]=surp_simp
	
			shares_sum[places,d]=shares_sump
			prices_sum[gamePlaces,d]=prices_sump
			surp_sum[gamePlaces,d]=surp_sump

			shares_prm[places,d]=shares_prmp
			prices_prm[gamePlaces,d]=prices_prmp
			surp_prm[gamePlaces,d]=surp_prmp
			/* Deal with Multiple Nash Equilibria by making a sampling weight */
			Count=rows(uniqrows(select(indexes[,4],(indexes[,1]:==i):*(indexes[,2]:==d))))
			weights[places,d]=J(rows(places),1,1/Count)
			
		}
	}
//	idChecker=problemNoter[,1]#J(6,1,1)
//	trouble[,d]=colshape(problemNoter[,counter::counter+5],1)		/* Commented out for now, for better or worse...do we need it? */
	counter=counter+6
}

end

/* Resampling */

mata
	redraws=50
	mid=panelsetup(datamkts,1)
	obsToUse=J(rows(mid),redraws,.)
	for (i=1;i<=rows(mid);i++) {
		probs=weights[mid[i,1],]:/rowsum(weights[mid[i,1],])
		obsToUse[i,]=resampler(probs,redraws)
	}
	
end
	
/* Now, putting all the samples in the correct place (along with a count of Nash equilibria */

mata
	lnews_sim2=J(rows(datamkts),redraws,.)		/* Note that sim is not a mnemonic for sim, but for share maxing */
	otherl_sim2=J(rows(datamkts),redraws,.)
	nnews_sim2=J(rows(datamkts),redraws,.)
	lnews_sum2=J(rows(datamkts),redraws,.)
	otherl_sum2=J(rows(datamkts),redraws,.)
	nnews_sum2=J(rows(datamkts),redraws,.)
	lnews_prm2=J(rows(datamkts),redraws,.)
	otherl_prm2=J(rows(datamkts),redraws,.)
	nnews_prm2=J(rows(datamkts),redraws,.)

	shares_sim2=J(rows(datamkts),redraws,.)
	prices_sim2=J(rows(datamkts),redraws,.)
	surp_sim2=J(rows(datamkts),redraws,.)

	shares_sum2=J(rows(datamkts),redraws,.)
	prices_sum2=J(rows(datamkts),redraws,.)
	surp_sum2=J(rows(datamkts),redraws,.)

	shares_prm2=J(rows(datamkts),redraws,.)
	prices_prm2=J(rows(datamkts),redraws,.)
	surp_prm2=J(rows(datamkts),redraws,.)
	trouble2=J(rows(datamkts),redraws,.)			/* Not Currently In Use! */

	weights2=J(rows(datamkts),redraws,.)





for (i=1;i<=redraws;i++) {
	for (j=1;j<=rows(mid);j++) {
		columnToUse=obsToUse[j,i]
		lnews_sim2[mid[j,1]::mid[j,2],i]=lnews_sim[mid[j,1]::mid[j,2],columnToUse]		/* Note that sim is not a mnemonic for sim, but for share maxing */
		otherl_sim2[mid[j,1]::mid[j,2],i]=otherl_sim[mid[j,1]::mid[j,2],columnToUse]
		nnews_sim2[mid[j,1]::mid[j,2],i]=nnews_sim[mid[j,1]::mid[j,2],columnToUse]
		lnews_sum2[mid[j,1]::mid[j,2],i]=lnews_sum[mid[j,1]::mid[j,2],columnToUse]
		otherl_sum2[mid[j,1]::mid[j,2],i]=otherl_sum[mid[j,1]::mid[j,2],columnToUse]
		nnews_sum2[mid[j,1]::mid[j,2],i]=nnews_sum[mid[j,1]::mid[j,2],columnToUse]
		lnews_prm2[mid[j,1]::mid[j,2],i]=lnews_prm[mid[j,1]::mid[j,2],columnToUse]
		otherl_prm2[mid[j,1]::mid[j,2],i]=otherl_prm[mid[j,1]::mid[j,2],columnToUse]
		nnews_prm2[mid[j,1]::mid[j,2],i]=nnews_prm[mid[j,1]::mid[j,2],columnToUse]

		shares_sim2[mid[j,1]::mid[j,2],i]=shares_sim[mid[j,1]::mid[j,2],columnToUse]
		prices_sim2[mid[j,1]::mid[j,2],i]=prices_sim[mid[j,1]::mid[j,2],columnToUse]
		surp_sim2[mid[j,1]::mid[j,2],i]=surp_sim[mid[j,1]::mid[j,2],columnToUse]

		shares_sum2[mid[j,1]::mid[j,2],i]=shares_sum[mid[j,1]::mid[j,2],columnToUse]
		prices_sum2[mid[j,1]::mid[j,2],i]=prices_sum[mid[j,1]::mid[j,2],columnToUse]
		surp_sum2[mid[j,1]::mid[j,2],i]=surp_sum[mid[j,1]::mid[j,2],columnToUse]

		shares_prm2[mid[j,1]::mid[j,2],i]=shares_prm[mid[j,1]::mid[j,2],columnToUse]
		prices_prm2[mid[j,1]::mid[j,2],i]=prices_prm[mid[j,1]::mid[j,2],columnToUse]
		surp_prm2[mid[j,1]::mid[j,2],i]=surp_prm[mid[j,1]::mid[j,2],columnToUse]
		trouble2[mid[j,1]::mid[j,2],i]=trouble[mid[j,1]::mid[j,2],columnToUse]			/* Not Currently In Use! */

		weights2[mid[j,1]::mid[j,2],i]=weights[mid[j,1]::mid[j,2],columnToUse]
	}
}

end

/* At this point, we are pulling everything into Stata and not really doing anything else. */




/* First, generate an actual surplus - only one of the three things that is not really observed */

mata: st_matrix("coefs",mean(b_start))
	scalar lnews_pelast=coefs[1,28]
	scalar otherl_pelast=coefs[1,29]
	scalar nnews_pelast=coefs[1,30]
	gen surp=(lnews*(1/lnews_pelast-1)+otherl*(1/otherl_pelast-1)+nnews*(1/nnews_pelast-1))*pps
 
getmata (lnews_sim*)=lnews_sim2 (otherl_sim*)=otherl_sim2 (nnews_sim*)=nnews_sim2
getmata (lnews_sum*)=lnews_sum2 (otherl_sum*)=otherl_sum2 (nnews_sum*)=nnews_sum2
getmata (lnews_prm*)=lnews_prm2 (otherl_prm*)=otherl_prm2 (nnews_prm*)=nnews_prm2

getmata (shares_sim*)=shares_sim2 (prices_sim*)=prices_sim2 (surp_sim*)=surp_sim2
getmata (shares_sum*)=shares_sum2 (prices_sum*)=prices_sum2 (surp_sum*)=surp_sum2
getmata (shares_prm*)=shares_prm2 (prices_prm*)=prices_prm2 (surp_prm*)=surp_prm2

getmata (weights*)=weights2

//	getmata (trouble*)=trouble /* Commented out for now. */

mata: st_numscalar("redraws",redraws)

forvalues i=1/`=redraws' {
	label variable lnews_sim`i' "Local news profile at share-maxing profile, sim `i'"
	label variable otherl_sim`i' "Other programming profile at share-maxing profile, sim `i'"
	label variable nnews_sim`i' "National news programming profile at share-maxing profile, sim `i'"
	label variable lnews_sum`i' "Local news profile at the ad surplus-maxing profile, sim `i'"
	label variable otherl_sum`i' "Other programming profile at the ad surplus-maxing profile, sim `i'"
	label variable nnews_sum`i' "National news programming profile at the ad surplus-maxing profile, sim `i'"
	label variable lnews_prm`i' "Local news profile at the profit-maxing profile, sim `i'"
	label variable otherl_prm`i' "Other programming profile at the profit-maxing profile, sim `i'"
	label variable nnews_prm`i' "National news programming profile at the profit-maxing profile, sim `i'"
	
	label variable shares_sim`i' "Actual shares for each station at the share-maxing profile, sim `i'"
	label variable prices_sim`i' "(Log) prices at the share-maxing profile, sim `i'"
	label variable surp_sim`i' "Advertising surplus generated (not log) at the share-maxing profile, sim `i'"
	label variable shares_sum`i' "Actual shares for each station at the ad surplus-maxing profile, sim `i'"
	label variable prices_sum`i' "(Log) prices at the ad surplus-maxing profile, sim `i'"
	label variable surp_sum`i' "Advertising surplus generated (not log) at the ad surplus- maxing profile, sim `i'"
	label variable shares_prm`i' "Actual shares for each station at the profit-maxing profile, sim `i'"
	label variable prices_prm`i' "(Log) prices at the profit-maximizing profile, sim `i'"
	label variable surp_prm`i' "Advertising surplus generated (not log) at the profit-maxing profile, sim `i'"
//	label variable trouble`i' "This draw has a problem - replace by resampling"
	label variable weights`i' "Sampling weight due to multiple equilibria, sim `i'"
}

label variable surp "Actual surplus contribution of station at observed action profile"

/* Save data */

/* Create markers for troubling observations */
/* 
gen haveagoodone=0
forvalues i=1/25 {
	bysort market: egen tottrouble`i'=total(trouble`i')
	gen totprob`i'=tottrouble`i'>0
	replace haveagoodone=haveagoodone+totprob`i'
}
*/

	
/* Check and see that we have at least one good simulation from the bunch */


save /user/mjbaker/TV/ad_sample/DataWithSims2, replace













