/* We are finally ready to simulate some results given our "Final" estimation results */
/* Along these lines, we return to to the collateAckerberg.do program and just run it */
/* mainly to organize the data for ease of simulation */

clear all
use "/user/mjbaker/TV/ad_sample/AveragedDataDynamicREr.dta", clear	/* Use revised data */
quietly do "/user/mjbaker/TV/ad_sample/AveragedDataDynamicMataFinal1.do"
set more off

/* One thing we have to do for whatever reason is recreate the game variable */

capture gen game=(lnews | otherl) & lnpps!=. // Depends on data set being used!

sort market stationid mt timeslot

mata
st_view(lnews=.,.,"lnews","keeper")
st_view(nnews=.,.,"nnews","keeper")
st_view(otherl=.,.,"otherl","keeper")
st_view(otherc=.,.,"otherc","keeper")
st_view(si=.,.,"si","keeper")
st_view(mt=.,.,"mt","keeper")
st_view(id=.,.,"stationid","keeper")
st_view(M=.,.,"market","keeper")
st_view(time=.,.,"timeslot","keeper")
st_view(game=.,.,"game","keeper")
st_view(lnpps=.,.,"lnpps","keeper")
//st_view(pop=.,.,"Mpop","keeper")	/* Careful here - we need a new pop definition */
st_view(pop=.,.,"ACS_HH","keeper")
st_view(l_ACS_HH=.,.,"l_ACS_HH","keeper")

st_view(iev1=.,.,"ievr1","keeper")
st_view(iev2=.,.,"ievr2","keeper")
st_view(iep1=.,.,"iepr1","keeper")	/* So we should be okay without changing these names below */
st_view(iep2=.,.,"iepr2","keeper")  	/* Use revised market and station fes */

st_view(lnewsn=.,.,"lnewsn","keeper")
st_view(otherln=.,.,"otherln","keeper")
st_view(nnewsn=.,.,"nnewsn","keeper")
st_view(othercn=.,.,"othercn","keeper")
st_view(dln=.,.,"dln","keeper")

lnviewn=ln(pop:*si)

m=panelsetup(M,1)

/* In CollateAckerberg.do, we use starting values */
/* here we instead use the results from estimateion */

mata matuse /user/mjbaker/TV/ad_sample/Results
//mata matuse /user/mjbaker/TV/ad_sample/DynoStarts21
//mata matuse /user/mjbaker/TV/ad_sample/betaPDynoStarts

/* First, let's reform the data in "long" form */
end
mata
	
	timeslots=6
	lnewsLong=J(rows(uniqrows(id)),timeslots,.)
	nnewsLong=J(rows(uniqrows(id)),timeslots,.)
	otherlLong=J(rows(uniqrows(id)),timeslots,.)
	othercLong=J(rows(uniqrows(id)),timeslots,.)
	slnewsLong=J(rows(uniqrows(id)),timeslots,.)
	snnewsLong=J(rows(uniqrows(id)),timeslots,.)
	sotherlLong=J(rows(uniqrows(id)),timeslots,.)
	sothercLong=J(rows(uniqrows(id)),timeslots,.)
	gameLong=J(rows(uniqrows(id)),timeslots,.)
	popLong=J(rows(uniqrows(id)),timeslots,.)  /* Not necessary but convenient */
	l_ACS_HHLong=J(rows(uniqrows(id)),timeslots,.)
	
	siLong=J(rows(uniqrows(id)),timeslots,.)
	lnewsLongLag=J(rows(uniqrows(id)),timeslots,.)
	nnewsLongLag=J(rows(uniqrows(id)),timeslots,.)
	otherlLongLag=J(rows(uniqrows(id)),timeslots,.)
	otherCLongLag=J(rows(uniqrows(id)),timeslots,.)
	siLongLag=J(rows(uniqrows(id)),timeslots,.)
	idAct=id
	idToUse=colshape(idAct,timeslots)[,1]
	totlnews=J(rows(uniqrows(id)),timeslots,.)
	totnnews=J(rows(uniqrows(id)),timeslots,.)
	
	/* Placeholder for market! */
	marketIdLong=J(rows(uniqrows(id)),1,.)
	statIdLong  =J(rows(uniqrows(id)),1,.)
	
	lnppsLong=J(rows(uniqrows(id)),timeslots,.)
	lnviewnLong=J(rows(uniqrows(id)),timeslots,.)
	
	iev1Long=J(rows(uniqrows(id)),timeslots,.)
	iev2Long=J(rows(uniqrows(id)),timeslots,.)
	iep1Long=J(rows(uniqrows(id)),timeslots,.)
	iep2Long=J(rows(uniqrows(id)),timeslots,.)
	
	lnewsnLong=J(rows(uniqrows(id)),timeslots,.)
	otherlnLong=J(rows(uniqrows(id)),timeslots,.)
	nnewsnLong=J(rows(uniqrows(id)),timeslots,.)
	othercnLong=J(rows(uniqrows(id)),timeslots,.)	
	
	dlnLong=J(rows(uniqrows(id)),timeslots,.)	

/* Do this only once in a function before doing anything else */
	counter=1
	for (i=1;i<=rows(m);i++) {
		lnewsmp  =panelsubmatrix(lnews,i,m)
		nnewsmp  =panelsubmatrix(nnews,i,m)
		otherlmp =panelsubmatrix(otherl,i,m)
		othercmp =panelsubmatrix(otherc,i,m)
		idmp=panelsubmatrix(id,i,m)
		mtmp=panelsubmatrix(mt,i,m)
		simp=panelsubmatrix(si,i,m)
		timemp=panelsubmatrix(time,i,m)
		Mp    =panelsubmatrix(M,i,m)
		gamep=panelsubmatrix(game,i,m)
		lnppsp=panelsubmatrix(lnpps,i,m)
		lnviewnp=panelsubmatrix(lnviewn,i,m)
		iev1p=panelsubmatrix(iev1,i,m)
		iev2p=panelsubmatrix(iev2,i,m)
		iep1p=panelsubmatrix(iep1,i,m)
		iep2p=panelsubmatrix(iep2,i,m)
		popp=panelsubmatrix(pop,i,m)
		l_ACS_HHp=panelsubmatrix(l_ACS_HH,i,m)
		lnewsnp=panelsubmatrix(lnewsn,i,m)
		otherlnp=panelsubmatrix(otherln,i,m)
		nnewsnp=panelsubmatrix(nnewsn,i,m)
		othercnp=panelsubmatrix(othercn,i,m)
		dlnp=panelsubmatrix(dln,i,m)		
		positioner=colshape(idmp,timeslots)

/* Make the above into histories - manufacture the data so we don't have */
/* to keep sending around all these interactions */

		obs    =uniqrows(idmp)
		lnewsm1=J(rows(obs),1,0)
		nnewsm1=J(rows(obs),1,0)
		otherlm1=J(rows(obs),1,0)
		othercm1=J(rows(obs),1,0)
		sim1   =J(rows(obs),1,0)
		for (k=4;k<=8;k++) {
			lnewsm1=lnewsm1,select(lnewsmp,timemp:==k-1)
			nnewsm1=nnewsm1,select(nnewsmp,timemp:==k-1)
			otherlm1=otherlm1,select(otherlmp,timemp:==k-1)
			othercm1=othercm1,select(othercmp,timemp:==k-1)
			sim1   =sim1,select(simp,timemp:==k-1)
		}
		lnewsc=J(rows(obs),0,0)
		nnewsc=J(rows(obs),0,0)
		sic   =J(rows(obs),0,0) 
		otherlc=J(rows(obs),0,0)
		othercc=J(rows(obs),0,0)
		gamec=J(rows(obs),0,0)
		lnppsc=J(rows(obs),0,0)
		lnviewnc=J(rows(obs),0,0)
		iev1c=J(rows(obs),0,0)
		iev2c=J(rows(obs),0,0)
		iep1c=J(rows(obs),0,0)
		iep2c=J(rows(obs),0,0)
		popc=J(rows(obs),0,0)
		l_ACS_HHc=J(rows(obs),0,0)
		lnewsnc=J(rows(obs),0,0)
		otherlnc=J(rows(obs),0,0)
		nnewsnc=J(rows(obs),0,0)
		othercnc=J(rows(obs),0,0)
		dlnc=J(rows(obs),0,0)		
		
		
		for (k=3;k<=8;k++) {
			lnewsc=lnewsc,select(lnewsmp,timemp:==k)
			nnewsc=nnewsc,select(nnewsmp,timemp:==k)
			otherlc=otherlc,select(otherlmp,timemp:==k)
			othercc=othercc,select(othercmp,timemp:==k)
			sic  =sic,select(simp,timemp:==k)
			gamec=gamec,select(gamep,timemp:==k)
			lnppsc=lnppsc,select(lnppsp,timemp:==k)
			lnviewnc=lnviewnc,select(lnviewnp,timemp:==k)
			iev1c=iev1c,select(iev1p,timemp:==k)
			iev2c=iev2c,select(iev2p,timemp:==k)
			iep1c=iep1c,select(iep1p,timemp:==k)
			iep2c=iep2c,select(iep2p,timemp:==k)
			popc=popc,select(popp,timemp:==k)
			l_ACS_HHc=l_ACS_HHc,select(l_ACS_HHp,timemp:==k)
			lnewsnc=lnewsnc,select(lnewsnp,timemp:==k)
			otherlnc=otherlnc,select(otherlnp,timemp:==k)
			nnewsnc=nnewsnc,select(nnewsnp,timemp:==k)
			othercnc=othercnc,select(othercnp,timemp:==k)
			dlnc=dlnc,select(dlnp,timemp:==k)			
		}

	lnewsLong[counter::counter+rows(positioner)-1,.]=lnewsc
	nnewsLong[counter::counter+rows(positioner)-1,.]=nnewsc
	otherlLong[counter::counter+rows(positioner)-1,.]=otherlc
	othercLong[counter::counter+rows(positioner)-1,.]=othercc
	siLong[counter::counter+rows(positioner)-1,.]=sic
	lnewsLongLag[counter::counter+rows(positioner)-1,.]=lnewsm1
	nnewsLongLag[counter::counter+rows(positioner)-1,.]=nnewsm1
	otherlLongLag[counter::counter+rows(positioner)-1,.]=otherlm1
	otherCLongLag[counter::counter+rows(positioner)-1,.]=othercm1
	siLongLag[counter::counter+rows(positioner)-1,.]=sim1
	gameLong[counter::counter+rows(positioner)-1,.]=gamec
	statIdLong[counter::counter+rows(positioner)-1,.]=positioner[,1]

	lnppsLong[counter::counter+rows(positioner)-1,.]=lnppsc
	lnviewnLong[counter::counter+rows(positioner)-1,.]=lnviewnc

	iev1Long[counter::counter+rows(positioner)-1,.]=iev1c
	iev2Long[counter::counter+rows(positioner)-1,.]=iev2c
	iep1Long[counter::counter+rows(positioner)-1,.]=iep1c
	iep2Long[counter::counter+rows(positioner)-1,.]=iep2c
	popLong[counter::counter+rows(positioner)-1,.]=popc
	l_ACS_HHLong[counter::counter+rows(positioner)-1,.]=l_ACS_HHc
	
	lnewsnLong[counter::counter+rows(positioner)-1,.]=lnewsnc
	otherlnLong[counter::counter+rows(positioner)-1,.]=otherlnc
	nnewsnLong[counter::counter+rows(positioner)-1,.]=nnewsnc	
	othercnLong[counter::counter+rows(positioner)-1,.]=othercnc	

	/* Group shares */
	
	dlnLong[counter::counter+rows(positioner)-1,.]=dlnc	

	/* Group shares */

	sl=colsum(lnewsc:*sic)	
	so=colsum(otherlc:*sic)
	sn=colsum(nnewsc:*sic)
	sc=colsum(othercc:*sic)
	slnewsLong[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,sl)
	snnewsLong[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,sn)
	sotherlLong[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,so)
	sothercLong[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,sc)

	/* Compute running totals */
	
	ctln=J(1,6,0)
	ctnn=J(1,6,0)
	totl=0
	totn=0
		for (k=1;k<=6;k++) {
			ctln[k]=colsum(lnewsm1[,k]:*sim1[,k])+totl
			ctnn[k]=colsum(nnewsm1[,k]:*sim1[,k])+totn
		}

	totlnews[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,runningsum(ctln))
	totnnews[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,runningsum(ctnn))
	
	marketIdLong[counter::counter+rows(positioner)-1,.]=colshape(Mp,6)[,1]
	
	counter=counter+rows(positioner)
	
	}
	
		// code tidbit rowshape(totln',1)'
		
		
	/* Now that we have rendered the data in wide format, we can start to think about */
	/* lags, interactions, etc. */
	
	lnewslnews=lnewsLong:*lnewsLongLag
	lnewsnnews=lnewsLong:*nnewsLongLag
	nnewslnews=nnewsLong:*lnewsLongLag
	nnewsnnews=nnewsLong:*nnewsLongLag
	
	/* As these all should be in logs, we have to change this */
	
	siLongLag=ln(siLongLag)
	_editmissing(siLongLag,0)
	
	siXlnln=siLongLag:*lnewslnews
	siXlnnn=siLongLag:*lnewsnnews
	siXnnln=siLongLag:*nnewslnews
	siXnnnn=siLongLag:*nnewsnnews
	
	lnewsRT=lnewsLong:*ln(1:+totlnews)
	nnewsRT=nnewsLong:*ln(1:+totnnews)

end

/* Up to this point, all we have done is get the data - replete with lags - 
   into wide format.   */
	
/* simulating shares - XB (without sigmas) */	

mata	
	bo=b_start
	bo[,1::4]=bo[,1::4]:^2
	bo=mean(bo)
	bpo=bo[(28::37)]

	betaDynoStart=bo[(5..23,27)]

// A quick fix to a little problem

	xBDyno=J(rows(lnewsLong),cols(lnewsLong),0)
	
	for (i=1;i<=cols(lnewsLong);i++) {
		xDyno=lnewsLong[,i],otherlLong[,i],nnewsLong[,i],
			lnewslnews[,i],lnewsnnews[,i],nnewslnews[,i],nnewsnnews[,i],
			siLongLag[,i],siXlnln[,i],siXlnnn[,i],siXnnln[,i],siXnnnn[,i],
			lnewsRT[,i],nnewsRT[,i],l_ACS_HHLong[,i],lnewsnLong[,i],otherlnLong[,i],nnewsnLong[,i],othercnLong[,i],J(rows(lnewsLong),1,1)
	
		xBDyno[,i]=xDyno*betaDynoStart'
	}
	
/* Now just xB terms in place given model parameter estimates */

/* Can we get back shares this way? Always useful to have this check in the program */

	swgLong=siLong:/(slnewsLong:*lnewsLong:+
		     snnewsLong:*nnewsLong:+
		     sotherlLong:*otherlLong:+
		     sothercLong:*othercLong)
	lnswgLong=ln(swgLong)
	xBpUE=ln(siLong):-ln(1:-(slnewsLong:+snnewsLong:+sotherlLong:+sothercLong)):-
			  bo[1]:*lnswgLong:*lnewsLong:-
			  bo[2]:*lnswgLong:*otherlLong-
			  bo[3]:*lnswgLong:*nnewsLong:-
			  bo[4]:*lnswgLong:*othercLong			/* These are "combined" XB+u1+u2+e errors */
			  
/* Below we will need the actual model error, so we need to extract this from the above */

/* Now, let's see if we can get shares back */

	simSharesLong=J(rows(lnewsLong),cols(lnewsLong),.)
	mLong=panelsetup(marketIdLong,1)

	for (i=1;i<=rows(mLong);i++){
		xBpUEp=panelsubmatrix(xBpUE,i,mLong)
		lnewsLongp=panelsubmatrix(lnewsLong,i,mLong)
		otherlLongp=panelsubmatrix(otherlLong,i,mLong)
		nnewsLongp=panelsubmatrix(nnewsLong,i,mLong)
		othercLongp=panelsubmatrix(othercLong,i,mLong)
		for (t=1;t<=timeslots;t++) {
			simSharesLong[mLong[i,1]::mLong[i,2],t]=
			eshares_up(xBpUEp[,t],lnewsLongp[,t],otherlLongp[,t],
			nnewsLongp[,t],othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
		}
	}
	
	/* Looks good so far!!!! */
	/* First, observed utilities are (we have to first make a dependent variable: */
	
	dln=ln(siLong):-ln(1:-(slnewsLong:+snnewsLong:+sotherlLong:+sothercLong))
	
	XBVngLong=dln-bo[1]*lnswgLong:*lnewsLong-bo[2]*lnswgLong:*otherlLong-
		bo[3]*lnswgLong:*nnewsLong-bo[4]*lnswgLong:*othercLong

	sdstav=exp(bo[24])
	sdmarv=exp(bo[25])
	sdmodv=exp(bo[26])
	
	draws=25
	
end
mata
	/* Draws are a little bit trickier to save here - it would be fruitful to save them as 
	   three dimensional objects...Associative array, perhaps? Or just a stacked matrix? (stacked matrix) */

	/* Viewership error components */
	UvsLong=J(1,draws,1)#iev1Long
	UvmtLong=J(1,draws,1)#iev2Long
	UvmodLong=invnormal(runiform(rows(lnewsLong),timeslots*draws))*sdmodv

	/* Alternatively  one could use UvmodObsLong computed above */

	UvmodObsLong=xBpUE:-xBDyno:-UvmtLong[,1::6]:-UvsLong[,1::6]
	
	/* Brute force */
	UvmodObsLong4=J(rows(lnewsLong),timeslots,0)

	for (t=1;t<=timeslots;t++) {
		yvt=ln(siLong[,t]):-ln(1:-slnewsLong[,t]:-snnewsLong[,t]:-sotherlLong[,t]:-sothercLong[,t])
		sigmaStuff=bo[1]:*lnswgLong[,t]:*lnewsLong[,t]:+bo[2]:*lnswgLong[,t]:*otherlLong[,t]:+
			bo[3]:*lnswgLong[,t]:*nnewsLong[,t]:+bo[4]:*lnswgLong[,t]:*othercLong[,t]
		XVt=lnewsLong[,t],otherlLong[,t],nnewsLong[,t],
			lnewsLong[,t]:*lnewsLongLag[,t],lnewsLong[,t]:*nnewsLongLag[,t],
			nnewsLong[,t]:*lnewsLongLag[,t],nnewsLong[,t]:*nnewsLongLag[,t],
			siLongLag[,t],siXlnln[,t],siXlnnn[,t],siXnnln[,t],siXnnnn[,t],
			lnewsLong[,t]:*ln(1:+totlnews[,t]),nnewsLong[,t]:*ln(1:+totnnews[,t]),l_ACS_HHLong[,t],
			lnewsnLong[,t],otherlnLong[,t],nnewsnLong[,t],othercnLong[,t],J(rows(lnewsLong),1,1)
		UvmodObsLong4[,t]=yvt:-sigmaStuff:-XVt*betaDynoStart':-UvsLong[,t]:-UvmtLong[,t]
		if (t==1) Checker=XVt*betaDynoStart':+UvsLong[,t]:+UvmtLong[,t]:+UvmodObsLong4[,t]
	}
	
	
	
	/* Schematic for bo: 
	1. mu_l, 2. mu_o, 3. mu_n, 4. mu_c, 5. eta_l, 6. eta_o,
	7. eta_n, 8. eta_ll, 9. eta_ln, 10. eta_nl, 11. eta_nn
	12. lam_own 13. lam_ll, 14. lam_ln, 15. lam_nl, 16. lam_nn
	17. rho_l, 18. rho_n , 22. alpha */	
	
	/* So, when I switch to lnews, I get (isn't this all just taken care of?) */
	/* Draw for those currently doing "other" */
	/* We have to go through and replace everything with how it was actually calculated !*/
	/* And we want to "do the opposite" of what we have done for lnews */
	/* It should be sufficient to replace otherl with lnews and vice versa */
	/* Now these are the "switching" values which we aren't really using */

	/* These are counterfactual viewership utilities (complete)   */
	/* Here what we really want are the random effects parameters */

end
	
mata
	
	/* Now, we have viewerships - what do we do next? */

	
	/* We have the usual station-market-model breakdown of the variance components */
	/* We need to draw two sets of values: E-terms for use in the traditional simulator */
	/* And U terms for use in the importance sampler */

	sdstap=exp(bpo[7])
	sdmarp=exp(bpo[8])
	sdmodp=exp(bpo[9])
	
	UpsLong=J(1,draws,1)#iep1Long
	UpmtLong=J(1,draws,1)#iep2Long
	UpmodLong=invnormal(runiform(rows(lnewsLong),timeslots*draws))*sdmodp

	/* Alternatively  one could use UvmodObsLong computed above */
end
mata	
	
	/* Mark stations that make at least one decision */
	
	gameMarker=rowsum(gameLong):>0
	decCounter=rowsum(gameLong)
	
	/* It turns out there are either five or six decisions for each type. */
	/* All possible decisions for a "six" type */
	/* Let's make an associative array holding this stuff */
	
	Bcs=asarray_create("real",2)
	sharesBcsR=asarray_create("real",3) 	/* A different associative array than before            */
						/* No need this time around to save the simulator stuff */
	
	gameIds=select(statIdLong,gameMarker)
	lnewsBcs=select(lnewsLong,gameMarker)
	nnewsBcs=select(nnewsLong,gameMarker)
	otherlBcs=select(otherlLong,gameMarker)
	totalDecs=select(decCounter,gameMarker)
	
	for (i=1;i<=rows(gameIds);i++) {
		if (totalDecs[i]==6) {
			strats=stratmat(6)'
			asarray(Bcs,(gameIds[i,1],1),strats)
			asarray(Bcs,(gameIds[i],2),1:-strats)
			asarray(Bcs,(gameIds[i],3),J(rows(strats),cols(strats),0))
		}
		if (totalDecs[i]==5) {
			nnewsPos=mm_which(nnewsBcs[i,1::6])
			stratsFree=stratmat(5)'
			if (nnewsPos==1) lnewsp=J(rows(stratsFree),1,0),stratsFree
			else if (nnewsPos==6) lnewsp=stratsFree,J(rows(strats),1,0)
			else lnewsp=stratsFree[,1::nnewsPos-1],J(rows(stratsFree),1,0),stratsFree[,nnewsPos::5]
			otherlp=1:-lnewsp
			otherlp[,nnewsPos]=J(rows(otherlp),1,0)
			nnewsp=J(rows(lnewsp),6,0)
			nnewsp[,nnewsPos]=J(rows(lnewsp),1,1)
			asarray(Bcs,(gameIds[i],1),lnewsp)
			asarray(Bcs,(gameIds[i],2),otherlp)
			asarray(Bcs,(gameIds[i],3),nnewsp)
		}
	}
end

/* Now, the errors that we haved used are the six simulated viewership shares. */
/* Simulate shares from unilateral deviations - these are required for drawing pricing errors ! */
mata
	counter=1
	for (c=1;c<=draws;c++) {
		c,counter
		for (i=1;i<=rows(mLong);i++) {
			gameMarkerp=panelsubmatrix(gameMarker,i,mLong)
			playersp=colsum(gameMarkerp)
		
			if (playersp>0) {
				statIdLongp=panelsubmatrix(statIdLong,i,mLong)
				lnewsLongp=panelsubmatrix(lnewsLong,i,mLong)
				nnewsLongp=panelsubmatrix(nnewsLong,i,mLong)
				otherlLongp=panelsubmatrix(otherlLong,i,mLong)
				othercLongp=panelsubmatrix(othercLong,i,mLong)
				nsToChange=select(statIdLongp,gameMarkerp)
				UvmtLongp=panelsubmatrix(UvmtLong,i,mLong)
				UvmodLongp=panelsubmatrix(UvmodLong,i,mLong)
				UvsLongp=panelsubmatrix(UvsLong,i,mLong)
				XBVngLongp=panelsubmatrix(XBVngLong,i,mLong)
				UvmodObsLongp=panelsubmatrix(UvmodObsLong4,i,mLong)
				l_ACS_HHLongp=panelsubmatrix(l_ACS_HHLong,i,mLong)
			
				for (k=1;k<=rows(nsToChange);k++) {
					pId=nsToChange[k]
					place=mm_which(statIdLongp:==pId)
					lnewsHat=asarray(Bcs,(pId,1))
					otherlHat=asarray(Bcs,(pId,2))
					nnewsHat=asarray(Bcs,(pId,3))
					sharesToPlace=J(rows(lnewsHat),6,0)
					soPlace=J(rows(lnewsHat),6,0)
					sgPlace=J(rows(lnewsHat),6,0)
					lnewsOrig=lnewsLongp[place,]
					otherlOrig=otherlLongp[place,]
				
					XBVplaceHold=J(rows(lnewsHat),cols(lnewsHat),.)
					XBVplaceHoldMean=J(rows(lnewsHat),cols(lnewsHat),.)
					
					for (q=1;q<=rows(lnewsHat);q++) {
						lnewsLongp[place,]=lnewsHat[q,]
						otherlLongp[place,]=otherlHat[q,]
						nnewsLongp[place,]=nnewsHat[q,]
						lnewsLongLagp=J(rows(lnewsLongp),1,0)
						nnewsLongLagp=J(rows(lnewsLongp),1,0)
						otherlLongLagp=J(rows(lnewsLongp),1,0)
						siLagp=J(rows(lnewsLongp),1,0)
						totlnewsp=J(rows(lnewsLongp),1,0)
						totnnewsp=J(rows(nnewsLongp),1,0)
						
						/* Logged totals */
						
						lnewsnLongp=lnewsLongp:*ln(1:+colsum(lnewsLongp))
						otherlnLongp=otherlLongp:*ln(1:+colsum(otherlLongp))
						nnewsnLongp=nnewsLongp:*ln(1:+colsum(nnewsLongp))
						othercnLongp=othercLongp:*ln(1:+colsum(othercLongp))
						
						for (t=1;t<=timeslots;t++) {
							if (t!=1) siLagp=ln(siLagp)			
							XV=lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
								lnewsLongLagp:*lnewsLongp[,t],
								nnewsLongLagp:*lnewsLongp[,t],
								lnewsLongLagp:*nnewsLongp[,t], 					
								nnewsLongLagp:*nnewsLongp[,t],
								siLagp,
								siLagp:*lnewsLongLagp:*lnewsLongp[,t],
								siLagp:*nnewsLongLagp:*lnewsLongp[,t],
								siLagp:*lnewsLongLagp:*nnewsLongp[,t],			
								siLagp:*nnewsLongLagp:*nnewsLongp[,t],	
								lnewsLongp[,t]:*ln(1:+totlnewsp),nnewsLongp[,t]:*ln(1:+totnnewsp),
								l_ACS_HHLongp[,t],lnewsnLongp[,t],otherlnLongp[,t],
								nnewsnLongp[,t],othercnLongp[,t],J(rows(lnewsLongp),1,1)
							
							/* First, set beta*XV as given for everyone else */
							
								XBV=XV*betaDynoStart':+UvmtLongp[,t]:+
									UvsLongp[,t]:+UvmodObsLongp[,t]
						
								if (lnewsHat[q,t]!=lnewsOrig[t]) {
									XBV[place,]=XV[place,]*betaDynoStart':+UvmtLongp[place,t]:+
										UvsLongp[place,t]:+UvmodLongp[place,counter+t-1]

								}
					
							XBVplaceHold[q,t]=XBV[place]
							XBVplaceHoldMean[q,t]=XBV[place]-UvmodLongp[place,counter+t-1]
					
							sharesP=eshares_up(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
								othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
							sharesToPlace[q,t]=sharesP[place,]
							soPlace[q,t]=1-sum(sharesP)
							
							slPlace=sum(lnewsLongp[,t]:*sharesP)
							snPlace=sum(nnewsLongp[,t]:*sharesP)
							solPlace=sum(otherlLongp[,t]:*sharesP)
							scPlace=sum(othercLongp[,t]:*sharesP)
							
							sgPlace[q,t]=lnewsLongp[place,t]:*slPlace:+
								nnewsLongp[place,t]:*snPlace:+
								otherlLongp[place,t]:*solPlace:+
								othercLongp[place,t]:*scPlace
							
						/* Store the XBV variable for later use... */
						/* Also store its normal density for later use */
						
						/* Re-initialize lagged variables */
		
							siLagp=sharesP
							totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
							totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))			
							lnewsLongLagp=lnewsLongp[,t]
							nnewsLongLagp=nnewsLongp[,t]
							otherlLongLagp=otherlLongp[,t]	
						}
						asarray(sharesBcsR,(pId,c,1),sharesToPlace)
						asarray(sharesBcsR,(pId,c,2),soPlace)
						asarray(sharesBcsR,(pId,c,3),sgPlace)
						asarray(Bcs,(pId,4),XBVplaceHold)
						asarray(Bcs,(pId,5),XBVplaceHoldMean)
						lnewsLongp[place,]=lnewsOrig
						otherlLongp[place,]=otherlOrig				
					}		
				}	
			}	
		}	
	counter=counter+timeslots	
	}
end

/* Now, get some set of errors */


mata

/* Recover actual pricing errors */

	UpmodObs=lnppsLong:-lnewsLong:*lnviewnLong:*bpo[1]:-otherlLong:*lnviewnLong:*bpo[2]:-
		nnewsLong:*lnviewnLong:*bpo[3]:-lnewsLong:*bpo[4]:-otherlLong:*bpo[5]:-l_ACS_HHLong:*bpo[6]:-bpo[10]:-
		UpsLong[,1::6]:-UpmtLong[,1::6]

end

/* Let's just try to get one station straight here first - try 12267                   */
/* For each station, we are going to save a series of draws for the error terms        */
/* We are already saving shares and other stuff, so we can save UpmodObs as above, and */

mata

	  priceErrsR=asarray_create("real",2)
	  simPricesR=asarray_create("real",2)
	priceBoundsR=asarray_create("real",2)
	problemNoter=J(rows(statIdLong),draws*6+1,0)
	
	counter=1
	for (d=1;d<=draws;d++) {
		d
		gameList=J(0,2,.)
		for (i=1;i<=rows(statIdLong);i++) {
			if (gameMarker[i]==1) {
				idp=statIdLong[i]
				gameList=gameList \ (idp,i)
				lnewsHat=asarray(Bcs,(idp,1))
				otherlHat=asarray(Bcs,(idp,2))
				nnewsHat=asarray(Bcs,(idp,3))
				sharesHat=asarray(sharesBcsR,(idp,d,1))
				lnewsAct=lnewsLong[i,]
				otherlAct=otherlLong[i,]
				nnewsAct=nnewsLong[i,]
				errMarker=(lnewsAct:==lnewsHat):*(otherlAct:==otherlHat)	
				errPdraws=J(1,6,0)

				problemNoter[i,1]=idp
	
				p=mm_which(((rowsum(errMarker[,1::5])):==5):*(errMarker[,timeslots]:==0))	/* Same until last period */
			
				actPayMean=(lnewsHat[p,]*bpo[1]:+otherlHat[p,]*bpo[2]:+nnewsHat[p,]*bpo[3]):*ln(popLong[i,]:*sharesHat[p,]):+
					lnewsHat[p,]:*bpo[4]:+otherlHat[p,]*bpo[5]:+bpo[6]:*l_ACS_HHLong[i,]:+bpo[10]:+
					UpsLong[i,counter::counter+timeslots-1]:+UpmtLong[i,counter::counter+timeslots-1]:+
					UpmodObs[i,1::timeslots]:*errMarker[p,]
				Bound=J(1,timeslots,.)	
				Bound[timeslots]=ln((rowsum(exp(lnppsLong[i,]))-rowsum(exp(actPayMean[1::timeslots-1])))/exp(actPayMean[timeslots]) )
				errPdraws[,timeslots]=exp(bpo[8])*invnormal(runiform(1,1)*normal(Bound[timeslots]/exp(bpo[8])))
				
			/* Recursion for drawing errors */
			
				for (t=timeslots-1;t>=1;t--) {
					if (nnewsAct[t]!=1) {
						if (t>1) {
							p=mm_which((rowsum(errMarker[,1::t-1]):==t-1):*(errMarker[,t]:==0))
							useErrs=J(rows(p),timeslots,0)
							useErrs[,1::t-1]=J(rows(p),1,UpmodObs[i,1::t-1])
							useErrs[,t+1::cols(useErrs)]=
								errMarker[p,t+1::cols(errPdraws)]:*UpmodObs[i,t+1::cols(errPdraws)]:+
								(1:-errMarker[p,t+1::cols(errPdraws)]):*errPdraws[,t+1::cols(errPdraws)]
						}
						else {
							p=mm_which(errMarker[,t]:==0)
							useErrs=J(rows(p),timeslots,0)	
							useErrs[,t+1::cols(useErrs)]=
								errMarker[p,t+1::cols(errPdraws)]:*UpmodObs[i,t+1::cols(errPdraws)]:+
								(1:-errMarker[p,t+1::cols(errPdraws)]):*errPdraws[,t+1::cols(errPdraws)]						
						}

						actPayMean=(lnewsHat[p,]*bpo[1]:+otherlHat[p,]*bpo[2]:+nnewsHat[p,]*bpo[3]):*ln(popLong[i,]:*sharesHat[p,]):+
							lnewsHat[p,]:*bpo[4]:+otherlHat[p,]*bpo[5]:+bpo[6]:*l_ACS_HHLong[i,]:+bpo[10]:+
							UpsLong[i,counter::counter+timeslots-1]:+UpmtLong[i,counter::counter+timeslots-1]:+
							useErrs

						if (t>1) min=min(rowsum(exp(lnppsLong[i,])):-rowsum(exp(actPayMean[,(1..t-1,t+1..timeslots)])))
						else min=min(rowsum(exp(lnppsLong[i,])):-rowsum(exp(actPayMean[,t+1..timeslots])))

						Bound[t]=ln(min/exp(actPayMean[rows(actPayMean),t]) )
						
						errPdraws[,t]=exp(bpo[8])*invnormal(runiform(1,1)*normal(Bound[t]/exp(bpo[8])))
			
						if (errPdraws[,t]==.) {
							printf("+");displayflush();
							errPdraws[,t]=-20
							Bound[t]=-20
							problemNoter[i,counter+t]=1			
						}
					}
					else {
						errPdraws[,t]=UpmodObs[i,t]
					}
				}
			
			asarray(priceErrsR,(statIdLong[i],d),errPdraws)
			asarray(priceBoundsR,(statIdLong[i],d),Bound)
		
			simPps=(lnewsHat*bpo[1]:+otherlHat*bpo[2]:+nnewsHat*bpo[3]):*ln(sharesHat:*popLong[i,])+
				lnewsHat*bpo[4]:+otherlHat*bpo[5]:+bpo[6]:*l_ACS_HHLong[i,]:+bpo[10]:+
				UpsLong[i,counter::counter+timeslots-1]:+UpmtLong[i,counter::counter+timeslots-1]:+
				errMarker:*UpmodObs[i,1::timeslots]:+(1:-errMarker):*errPdraws
			
			asarray(simPricesR,(statIdLong[i],d),simPps)	
			}
		}
	counter=counter+timeslots
	}
end

/* We will need to eventually find Nash equilibrium stuff, but for now let's see if we could find share-maximizing stuff */
set more off
mata
	/* the identifier in the array will hold market, profile, Nash number */
	/* The "actual" market profile will always be first (a useful check!) */
	
	ShareMax=asarray_create("real",3)
	ShareMaxProfiles=asarray_create("real",3)

	/* The number of actions to try */
	
	counter=1
	for (d=1;d<=draws;d++) {			/* get one draw straight first */
		for (i=1;i<=rows(mLong);i++) {		/* rows(mLong) */
		i,d
			gameMarkerp=panelsubmatrix(gameMarker,i,mLong)
			playersp=colsum(gameMarkerp)
			if (playersp>0) {
				statIdLongp=panelsubmatrix(statIdLong,i,mLong)	/* For each market, determine who is in the game */
				Gamers=select(statIdLongp,gameMarkerp)		/* Get their ids */
				posofGamers=mm_which(gameMarkerp)		/* Who is where I guess */
				
				UvmtLongp=panelsubmatrix(UvmtLong,i,mLong)
				UvmodLongp=panelsubmatrix(UvmodLong,i,mLong)
				UvsLongp=panelsubmatrix(UvsLong,i,mLong)
				XBVngLongp=panelsubmatrix(XBVngLong,i,mLong)
				UvmodObsLongp=panelsubmatrix(UvmodObsLong4,i,mLong)
		
				UpmodObsp=panelsubmatrix(UpmodObs,i,mLong)
				UpsLongp=panelsubmatrix(UpsLong,i,mLong)
				UpmtLongp=panelsubmatrix(UpmtLong,i,mLong)
				
				lnewsLongp=panelsubmatrix(lnewsLong,i,mLong)
				otherlLongp=panelsubmatrix(otherlLong,i,mLong)
				nnewsLongp=panelsubmatrix(nnewsLong,i,mLong)
				othercLongp=panelsubmatrix(othercLong,i,mLong)
				popLongp=panelsubmatrix(popLong,i,mLong)
				l_ACS_HHLongp=panelsubmatrix(l_ACS_HHLong,i,mLong)
				
				/* Before doing anything else, let's generate a slew of profiles for market participants */
				/* that seem plausible.  That's one possibility. Another is to do an MCMC search for what we are after. */
				
				/* Here is an mcmc search */
			
				otherlOld=otherlLongp	/* Actual actions */
				lnewsOld=lnewsLongp	/* In all three dimensions */
				nnewsOld=nnewsLongp
				othercOld=othercLongp
				
				/* initialize list of Nash eqs with observed eq */
	
			
				/* Guess at a new profile - this time from small list */
				resets=0
				for (s=1;s<=10000;s++) {	
					targetPlayerN=round(1+(playersp-1)*runiform(1,1))	/* pick a player */
					targetPlayer=Gamers[targetPlayerN]			/* get his station id */
					
					/* Potential profiles for now as before */
					
					lnewsPlayer=asarray(Bcs,(targetPlayer,1))		/* Profiles for the player */
					otherlPlayer=asarray(Bcs,(targetPlayer,2))
					nnewsPlayer=asarray(Bcs,(targetPlayer,3))
					othercPlayer=1:-lnewsPlayer:-otherlPlayer:-nnewsPlayer
					
					if (any(othercPlayer:<0) | any(othercPlayer:>1)) printf("what the...")

					/* Pick a row             */
						
					newStrat=round(1+(rows(lnewsPlayer)-1)*runiform(1,1))

					/* Replace the strategy */

					targetPos=posofGamers[targetPlayerN]
					lnewsTry=lnewsOld
					nnewsTry=nnewsOld
					otherlTry=otherlOld
					othercTry=othercOld
					
						/*First try a deviation */
					
					if (s!=1) {
						lnewsTry[targetPos,]=lnewsPlayer[newStrat,]
						nnewsTry[targetPos,]=nnewsPlayer[newStrat,]
						otherlTry[targetPos,]=otherlPlayer[newStrat,]
						/* First strategy is as given */
					}
					
					/* But let's introduce a reset some of the time so we don't wander off */
					
					if (runiform(1,1)>.998) {
						lnewsTry=lnewsLongp
						otherlTry=otherlLongp
						nnewsTry=nnewsLongp
						othercTry=othercLongp						
						lnewsTry[targetPos,]=lnewsPlayer[newStrat,]
						nnewsTry[targetPos,]=nnewsPlayer[newStrat,]
						otherlTry[targetPos,]=otherlPlayer[newStrat,]
						othercTry[targetPos,]=othercPlayer[newStrat,]
					}
					/* Now, find the shares at this new strategy */

					/* Find the shares block begins - we need another loop outside of this one which
					   should be a do-while loop to see if we can find beneficial unilateral deviations */
					  
					/* Probably want this in an if-block so we get payoffs at Nash eq: */
				
//					if (runiform(1,1)>.995) colsum(lnewsTry),colsum(lnewsLongp)
					
					sharesTry=J(rows(lnewsTry),0,.)
					totlnewsp=J(rows(lnewsTry),1,0)
					totnnewsp=J(rows(nnewsTry),1,0)
					lnewsLongLagp=J(rows(lnewsTry),1,0)
					nnewsLongLagp=J(rows(lnewsTry),1,0)
					siLagp=J(rows(lnewsTry),1,0)
					
					lnewsnTry=lnewsTry:*ln(1:+colsum(lnewsTry))
					otherlnTry=otherlTry:*ln(1:+colsum(otherlTry))
					nnewsnTry=nnewsTry:*ln(1:+colsum(nnewsTry))
					othercnTry=othercTry:*ln(1:+colsum(othercTry))
					
					for (t=1;t<=timeslots;t++) {
						if (t!=1) siLagp=ln(siLagp)
						XV=lnewsTry[,t],otherlTry[,t],nnewsTry[,t],
							lnewsLongLagp:*lnewsTry[,t],
							nnewsLongLagp:*lnewsTry[,t],
							lnewsLongLagp:*nnewsTry[,t], 					
							nnewsLongLagp:*nnewsTry[,t],siLagp,
							siLagp:*lnewsLongLagp:*lnewsTry[,t],
							siLagp:*nnewsLongLagp:*lnewsTry[,t],
							siLagp:*lnewsLongLagp:*nnewsTry[,t],			
							siLagp:*nnewsLongLagp:*nnewsTry[,t],	
							lnewsTry[,t]:*ln(1:+totlnewsp),nnewsTry[,t]:*ln(1:+totnnewsp),l_ACS_HHLongp[,t],
							lnewsnTry[,t],otherlnTry[,t],nnewsnTry[,t],othercnTry[,t],
							J(rows(lnewsLongp),1,1)

					/* First, set beta*XV as given for everyone else */
						
						errCheck=lnewsTry[,t]:!=lnewsLongp[,t]
						XBVact=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
							UvsLongp[,t]:+UvmodObsLongp[,t]
						XBVsim=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
							UvsLongp[,t]:+UvmodLongp[,counter+t-1]

						XBV=(1:-errCheck):*XBVact:+errCheck:*XBVsim
						
						sharesTry=sharesTry,eshares_up(XBV,lnewsTry[,t],otherlTry[,t],nnewsTry[,t],
							othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
												
										/* Re-initialize lagged variables */
						siLagp=sharesTry[,t]
						totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesTry[,t]))
						totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesTry[,t]))			
						lnewsLongLagp=lnewsTry[,t]
						nnewsLongLagp=nnewsTry[,t]
						otherlLongLagp=otherlTry[,t]	
					}
					/* Now, compute log-prices at the deviation */
					
					XBP=bpo[1]:*lnewsTry:*ln(sharesTry:*popLongp)+
					    bpo[2]:*otherlTry:*ln(sharesTry:*popLongp)+
					    bpo[3]:*nnewsTry:*ln(sharesTry:*popLongp)+
					    bpo[4]:*lnewsTry:+bpo[5]:*otherlTry:+bpo[6]:*l_ACS_HHLongp:+bpo[10]:+
					    UpsLongp[,counter::counter+timeslots-1]:+UpmtLongp[,counter::counter+timeslots-1]
					
					XBP=XBP[posofGamers,]
					
					keys=statIdLongp[posofGamers]
					errCheckp=select(errCheck,gameMarkerp)
					
					Errsp=J(rows(keys),timeslots,.)
					for (t=1;t<=rows(keys);t++) Errsp[t,]=asarray(priceErrsR,(keys[t],d))

					pricesTry=XBP:+(1:-errCheckp):*select(UpmodObsp,gameMarkerp):+errCheckp:*Errsp					
					if (hasmissing(pricesTry)) printf("what the!")
					/* Now, a little block to compute surplus */
					
					errCheckp=select(errCheck,gameMarkerp)
					lnewsCheckp=select(lnewsLongp,gameMarkerp)
					otherlCheckp=select(otherlLongp,gameMarkerp)
					nnewsCheckp=select(nnewsLongp,gameMarkerp)				
					
					surpTry=XBP:+(1:-errCheckp):*select(UpmodObsp,gameMarkerp):+errCheckp:*Errsp
					surpTry=bpo[1]:/(1-bpo[1]):*lnewsCheckp:*exp(surpTry):+
						bpo[2]:/(1-bpo[2]):*otherlCheckp:*exp(surpTry):+
						bpo[3]:/(1-bpo[3]):*nnewsCheckp:*exp(surpTry)				
					
					if (s==1) {
						valold=sum(sharesTry)
						siOld=sharesTry
						surpOld=surpTry
						pricesOld=pricesTry
						printf("To begin:\n")
						printf("Players: %9.0g\n", playersp)
						valold,sum(exp(pricesTry)),sum(surpTry)
					}
					else {
						valtry=sum(sharesTry)
						if (valtry>.99*valold & s<5000) {
							lnewsOld=lnewsTry
							otherlOld=otherlTry
							nnewsOld=nnewsTry
							siOld=sharesTry
							surpOld=surpTry
							valold=valtry
							pricesOld=pricesTry
					//		printf("+");displayflush()
						}
						else if (valtry>valold) {
							lnewsOld=lnewsTry
							otherlOld=otherlTry
							nnewsOld=nnewsTry
							siOld=sharesTry
							surpOld=surpTry
							valold=valtry
							pricesOld=pricesTry
					//		printf("+");displayflush()
						}
						
					}
				}
				
			asarray(ShareMax,(d,i,1),siOld)
			asarray(ShareMax,(d,i,2),pricesOld)
			asarray(ShareMax,(d,i,3),surpOld)
			asarray(ShareMaxProfiles,(d,i,1),lnewsOld)
			asarray(ShareMaxProfiles,(d,i,2),otherlOld)
			asarray(ShareMaxProfiles,(d,i,3),nnewsOld)
			colsum(lnewsLongp)
			colsum(lnewsOld)
			colsum(nnewsLongp)
			colsum(nnewsOld)
			sum(siOld),sum(exp(pricesOld)),sum(surpOld)
			hasmissing(siOld)		
			}
		}
	counter=counter+timeslots
	}
end

/* Let's see if we can find profit maximizing outputs now - those that maximize total industry profits (for observed firms only). */
set more off
mata:
	ProfMax        =asarray_create("real",3)
	ProfMaxProfiles=asarray_create("real",3)

	/* Hold the "plausible" points at which to check for a unilateral deviation */

	counter=1
	for (d=1;d<=draws;d++) {
		for (i=1;i<=rows(mLong);i++) {		/* rows(mLong) */
		i,d
			gameMarkerp=panelsubmatrix(gameMarker,i,mLong)
			playersp=colsum(gameMarkerp)
			if (playersp>0) {
				statIdLongp=panelsubmatrix(statIdLong,i,mLong)	/* For each market, determine who is in the game */
				Gamers=select(statIdLongp,gameMarkerp)		/* Get their ids */
				posofGamers=mm_which(gameMarkerp)		/* Who is where I guess */
				
				UvmtLongp=panelsubmatrix(UvmtLong,i,mLong)
				UvmodLongp=panelsubmatrix(UvmodLong,i,mLong)
				UvsLongp=panelsubmatrix(UvsLong,i,mLong)
				XBVngLongp=panelsubmatrix(XBVngLong,i,mLong)
				UvmodObsLongp=panelsubmatrix(UvmodObsLong4,i,mLong)
		
				UpmodObsp=panelsubmatrix(UpmodObs,i,mLong)
				UpsLongp=panelsubmatrix(UpsLong,i,mLong)
				UpmtLongp=panelsubmatrix(UpmtLong,i,mLong)
				
				lnewsLongp=panelsubmatrix(lnewsLong,i,mLong)
				otherlLongp=panelsubmatrix(otherlLong,i,mLong)
				nnewsLongp=panelsubmatrix(nnewsLong,i,mLong)
				othercLongp=panelsubmatrix(othercLong,i,mLong)
				popLongp=panelsubmatrix(popLong,i,mLong)
				l_ACS_HHLongp=panelsubmatrix(l_ACS_HHLong,i,mLong)
				
				/* Before doing anything else, let's generate a slew of profiles for market participants */
				/* that seem plausible.  That's one possibility. Another is to do an MCMC search for what we are after. */
				
				/* Here is an mcmc search */
			
				otherlOld=otherlLongp	/* Actual actions */
				lnewsOld=lnewsLongp	/* In all three dimensions */
				nnewsOld=nnewsLongp
				othercOld=othercLongp
				
				/* initialize list of Nash eqs with observed eq */
	
			
				/* Guess at a new profile - this time from small list */
				resets=0
				for (s=1;s<=5000;s++) {	
					targetPlayerN=round(1+(playersp-1)*runiform(1,1))	/* pick a player */
					targetPlayer=Gamers[targetPlayerN]			/* get his station id */
					
					/* Potential profiles for now as before */
					
					lnewsPlayer=asarray(Bcs,(targetPlayer,1))		/* Profiles for the player */
					otherlPlayer=asarray(Bcs,(targetPlayer,2))
					nnewsPlayer=asarray(Bcs,(targetPlayer,3))
					othercPlayer=1:-lnewsPlayer:-otherlPlayer:-nnewsPlayer

					/* Pick a row             */
						
					newStrat=round(1+(rows(lnewsPlayer)-1)*runiform(1,1))

					/* Replace the strategy */

					targetPos=posofGamers[targetPlayerN]
					lnewsTry=lnewsOld
					nnewsTry=nnewsOld
					otherlTry=otherlOld
					othercTry=othercOld
					
						/*First try a deviation */
					
					if (s!=1) {
						lnewsTry[targetPos,]=lnewsPlayer[newStrat,]
						nnewsTry[targetPos,]=nnewsPlayer[newStrat,]
						otherlTry[targetPos,]=otherlPlayer[newStrat,]
						othercTry[targetPos,]=othercPlayer[newStrat,]
						/* First strategy is as given */
					}
					
					/* But let's introduce a reset some of the time so we don't wander off */
					
					if (runiform(1,1)>.99) {
						lnewsTry=lnewsLongp
						otherlTry=otherlLongp
						nnewsTry=nnewsLongp
						lnewsTry[targetPos,]=lnewsPlayer[newStrat,]
						nnewsTry[targetPos,]=nnewsPlayer[newStrat,]
						otherlTry[targetPos,]=otherlPlayer[newStrat,]
						othercTry[targetPos,]=othercPlayer[newStrat,]
					}
					/* Now, find the shares at this new strategy */

					/* Find the shares block begins - we need another loop outside of this one which
					   should be a do-while loop to see if we can find beneficial unilateral deviations */
					  
					/* Probably want this in an if-block so we get payoffs at Nash eq: */
					
					sharesTry=J(rows(lnewsTry),0,.)
					totlnewsp=J(rows(lnewsTry),1,0)
					totnnewsp=J(rows(nnewsTry),1,0)
					lnewsLongLagp=J(rows(lnewsTry),1,0)
					nnewsLongLagp=J(rows(lnewsTry),1,0)
					siLagp=J(rows(lnewsTry),1,0)
					
					lnewsnTry=lnewsTry:*ln(1:+colsum(lnewsTry))
					otherlnTry=otherlTry:*ln(1:+colsum(otherlTry))
					nnewsnTry=nnewsTry:*ln(1:+colsum(nnewsTry))
					othercnTry=othercTry:*ln(1:+colsum(othercTry))					
					
					for (t=1;t<=timeslots;t++) {
						if (t!=1) siLagp=ln(siLagp)
						XV=lnewsTry[,t],otherlTry[,t],nnewsTry[,t],
							lnewsLongLagp:*lnewsTry[,t],
							nnewsLongLagp:*lnewsTry[,t],
							lnewsLongLagp:*nnewsTry[,t], 					
							nnewsLongLagp:*nnewsTry[,t],siLagp,
							siLagp:*lnewsLongLagp:*lnewsTry[,t],
							siLagp:*nnewsLongLagp:*lnewsTry[,t],
							siLagp:*lnewsLongLagp:*nnewsTry[,t],			
							siLagp:*nnewsLongLagp:*nnewsTry[,t],	
							lnewsTry[,t]:*ln(1:+totlnewsp),nnewsTry[,t]:*ln(1:+totnnewsp),l_ACS_HHLongp[,t],
							lnewsnTry[,t],otherlnTry[,t],nnewsnTry[,t],othercnTry[,t],
							J(rows(lnewsLongp),1,1)

					/* First, set beta*XV as given for everyone else */
						
						errCheck=lnewsTry[,t]:!=lnewsLongp[,t]
						XBVact=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
							UvsLongp[,t]:+UvmodObsLongp[,t]
						XBVsim=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
							UvsLongp[,t]:+UvmodLongp[,counter+t-1]

						XBV=(1:-errCheck):*XBVact:+errCheck:*XBVsim
						
						sharesTry=sharesTry,eshares_up(XBV,lnewsTry[,t],otherlTry[,t],nnewsTry[,t],
							othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
												
										/* Re-initialize lagged variables */
						siLagp=sharesTry[,t]
						totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesTry[,t]))
						totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesTry[,t]))			
						lnewsLongLagp=lnewsTry[,t]
						nnewsLongLagp=nnewsTry[,t]
						otherlLongLagp=otherlTry[,t]	
					}
					
					/* Now, compute log-prices at the deviation */
					
					XBP=bpo[1]:*lnewsTry:*ln(sharesTry:*popLongp)+
					    bpo[2]:*otherlTry:*ln(sharesTry:*popLongp)+
					    bpo[3]:*nnewsTry:*ln(sharesTry:*popLongp)+
					    bpo[4]:*lnewsTry:+bpo[5]:*otherlTry:+bpo[6]:*l_ACS_HHLongp:+bpo[10]:+
					    UpsLongp[,counter::counter+timeslots-1]:+UpmtLongp[,counter::counter+timeslots-1]
					
					XBP=XBP[posofGamers,]
					
					keys=statIdLongp[posofGamers]
					errCheckp=select(errCheck,gameMarkerp)
					
					Errsp=J(rows(keys),timeslots,.)
					for (t=1;t<=rows(keys);t++) Errsp[t,]=asarray(priceErrsR,(keys[t],d))

					pricesTry=XBP:+(1:-errCheckp):*select(UpmodObsp,gameMarkerp):+errCheckp:*Errsp
					
					/* Now a block to compute surpluses */

					errCheckp=select(errCheck,gameMarkerp)
					lnewsCheckp=select(lnewsLongp,gameMarkerp)
					otherlCheckp=select(otherlLongp,gameMarkerp)
					nnewsCheckp=select(nnewsLongp,gameMarkerp)				
				
					surpTry=XBP:+(1:-errCheckp):*select(UpmodObsp,gameMarkerp):+errCheckp:*Errsp
					surpTry=bpo[1]:/(1-bpo[1]):*lnewsCheckp:*exp(surpTry):+
						bpo[2]:/(1-bpo[2]):*otherlCheckp:*exp(surpTry):+
						bpo[3]:/(1-bpo[3]):*nnewsCheckp:*exp(surpTry)						
					
					
					if (s==1) {
						valold=sum(exp(pricesTry))
						siOld=sharesTry
						piOld=pricesTry
						surpOld=surpTry
						printf("To begin:\n")
						printf("Local stations: %2.0f\n",playersp)
						valold,sum(siOld),sum(surpOld)
					}
					else {
						valtry=sum(exp(pricesTry))
						if (valtry>valold) {
							lnewsOld=lnewsTry
							otherlOld=otherlTry
							nnewsOld=nnewsTry
							siOld=sharesTry
							surpOld=surpTry
							piOld=pricesTry
							valold=valtry
						}
						
					}
				}
			asarray(ProfMax,(d,i,1),siOld)
			asarray(ProfMax,(d,i,2),piOld)
			asarray(ProfMax,(d,i,3),surpOld)
			asarray(ProfMaxProfiles,(d,i,1),lnewsOld)
			asarray(ProfMaxProfiles,(d,i,2),otherlOld)
			asarray(ProfMaxProfiles,(d,i,3),nnewsOld)
			colsum(lnewsLongp)
			colsum(lnewsOld)
			colsum(nnewsLongp)
			colsum(nnewsOld)
			valold,sum(siOld),sum(surpOld)
			}
		}
	counter=counter+timeslots
	}
end	

/* Finally, the same set of things for the surplus-maximizing choices */

mata:
	SurpMax        =asarray_create("real",3)
	SurpMaxProfiles=asarray_create("real",3)

	/* Hold the "plausible" points at which to check for a unilateral deviation */

	counter=1
	for (d=1;d<=draws;d++) {
		for (i=1;i<=rows(mLong);i++) {		/* rows(mLong) */
		i,d
			gameMarkerp=panelsubmatrix(gameMarker,i,mLong)
			playersp=colsum(gameMarkerp)
			if (playersp>0) {
				statIdLongp=panelsubmatrix(statIdLong,i,mLong)	/* For each market, determine who is in the game */
				Gamers=select(statIdLongp,gameMarkerp)		/* Get their ids */
				posofGamers=mm_which(gameMarkerp)		/* Who is where I guess */

				UvmtLongp=panelsubmatrix(UvmtLong,i,mLong)
				UvmodLongp=panelsubmatrix(UvmodLong,i,mLong)
				UvsLongp=panelsubmatrix(UvsLong,i,mLong)
				XBVngLongp=panelsubmatrix(XBVngLong,i,mLong)
				UvmodObsLongp=panelsubmatrix(UvmodObsLong4,i,mLong)
		
				UpmodObsp=panelsubmatrix(UpmodObs,i,mLong)
				UpsLongp=panelsubmatrix(UpsLong,i,mLong)
				UpmtLongp=panelsubmatrix(UpmtLong,i,mLong)
				
				lnewsLongp=panelsubmatrix(lnewsLong,i,mLong)
				otherlLongp=panelsubmatrix(otherlLong,i,mLong)
				nnewsLongp=panelsubmatrix(nnewsLong,i,mLong)
				othercLongp=panelsubmatrix(othercLong,i,mLong)
				popLongp=panelsubmatrix(popLong,i,mLong)
				l_ACS_HHLongp=panelsubmatrix(l_ACS_HHLong,i,mLong)
				
				/* Before doing anything else, let's generate a slew of profiles for market participants */
				/* that seem plausible.  That's one possibility. Another is to do an MCMC search for what we are after. */
				
				/* Here is an mcmc search */
			
				otherlOld=otherlLongp	/* Actual actions */
				lnewsOld=lnewsLongp	/* In all three dimensions */
				nnewsOld=nnewsLongp
				othercOld=othercLongp
				
				/* initialize list of Nash eqs with observed eq */
	
			
				/* Guess at a new profile - this time from small list */
				resets=0
				for (s=1;s<=5000;s++) {	
					targetPlayerN=round(1+(playersp-1)*runiform(1,1))	/* pick a player */
					targetPlayer=Gamers[targetPlayerN]			/* get his station id */
					
					/* Potential profiles for now as before */
					
					lnewsPlayer=asarray(Bcs,(targetPlayer,1))		/* Profiles for the player */
					otherlPlayer=asarray(Bcs,(targetPlayer,2))
					nnewsPlayer=asarray(Bcs,(targetPlayer,3))
					othercPlayer=1:-lnewsPlayer:-otherlPlayer:-nnewsPlayer

					/* Pick a row             */
						
					newStrat=round(1+(rows(lnewsPlayer)-1)*runiform(1,1))

					/* Replace the strategy */

					targetPos=posofGamers[targetPlayerN]
					lnewsTry=lnewsOld
					nnewsTry=nnewsOld
					otherlTry=otherlOld
					othercTry=othercOld
					
						/*First try a deviation */
					
					if (s!=1) {
						lnewsTry[targetPos,]=lnewsPlayer[newStrat,]
						nnewsTry[targetPos,]=nnewsPlayer[newStrat,]
						otherlTry[targetPos,]=otherlPlayer[newStrat,]
						othercTry[targetPos,]=othercPlayer[newStrat,]
						/* First strategy is as given */
					}
					
					/* But let's introduce a reset some of the time so we don't wander off */
					
					if (runiform(1,1)>.99) {
						lnewsTry=lnewsLongp
						otherlTry=otherlLongp
						nnewsTry=nnewsLongp
						lnewsTry[targetPos,]=lnewsPlayer[newStrat,]
						nnewsTry[targetPos,]=nnewsPlayer[newStrat,]
						otherlTry[targetPos,]=otherlPlayer[newStrat,]
						othercTry[targetPos,]=othercPlayer[newStrat,]
					}
					/* Now, find the shares at this new strategy */

					/* Find the shares block begins - we need another loop outside of this one which
					   should be a do-while loop to see if we can find beneficial unilateral deviations */
					  
					/* Probably want this in an if-block so we get payoffs at Nash eq: */
					
					sharesTry=J(rows(lnewsTry),0,.)
					totlnewsp=J(rows(lnewsTry),1,0)
					totnnewsp=J(rows(nnewsTry),1,0)
					lnewsLongLagp=J(rows(lnewsTry),1,0)
					nnewsLongLagp=J(rows(lnewsTry),1,0)
					siLagp=J(rows(lnewsTry),1,0)

					lnewsnTry=lnewsTry:*ln(1:+colsum(lnewsTry))
					otherlnTry=otherlTry:*ln(1:+colsum(otherlTry))
					nnewsnTry=nnewsTry:*ln(1:+colsum(nnewsTry))
					othercnTry=othercTry:*ln(1:+colsum(othercTry))
					
					for (t=1;t<=timeslots;t++) {
						if (t!=1) siLagp=ln(siLagp)
						XV=lnewsTry[,t],otherlTry[,t],nnewsTry[,t],
							lnewsLongLagp:*lnewsTry[,t],
							nnewsLongLagp:*lnewsTry[,t],
							lnewsLongLagp:*nnewsTry[,t], 					
							nnewsLongLagp:*nnewsTry[,t],siLagp,
							siLagp:*lnewsLongLagp:*lnewsTry[,t],
							siLagp:*nnewsLongLagp:*lnewsTry[,t],
							siLagp:*lnewsLongLagp:*nnewsTry[,t],			
							siLagp:*nnewsLongLagp:*nnewsTry[,t],	
							lnewsTry[,t]:*ln(1:+totlnewsp),nnewsTry[,t]:*ln(1:+totnnewsp),l_ACS_HHLongp[,t],
							lnewsnTry[,t],otherlnTry[,t],nnewsnTry[,t],othercnTry[,t],
							J(rows(lnewsLongp),1,1)

					/* First, set beta*XV as given for everyone else */
						
						errCheck=lnewsTry[,t]:!=lnewsLongp[,t]
						XBVact=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
							UvsLongp[,t]:+UvmodObsLongp[,t]
						XBVsim=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
							UvsLongp[,t]:+UvmodLongp[,counter+t-1]

						XBV=(1:-errCheck):*XBVact:+errCheck:*XBVsim
						
						sharesTry=sharesTry,eshares_up(XBV,lnewsTry[,t],otherlTry[,t],nnewsTry[,t],
							othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
												
										/* Re-initialize lagged variables */
						siLagp=sharesTry[,t]
						totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesTry[,t]))
						totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesTry[,t]))			
						lnewsLongLagp=lnewsTry[,t]
						nnewsLongLagp=nnewsTry[,t]
						otherlLongLagp=otherlTry[,t]	
					}
					
					/* Now, compute log-prices at the deviation */
					
					XBP=bpo[1]:*lnewsTry:*ln(sharesTry:*popLongp)+
					    bpo[2]:*otherlTry:*ln(sharesTry:*popLongp)+
					    bpo[3]:*nnewsTry:*ln(sharesTry:*popLongp)+
					    bpo[4]:*lnewsTry:+bpo[5]:*otherlTry:+bpo[6]:*l_ACS_HHLongp:+bpo[10]:+
					    UpsLongp[,counter::counter+timeslots-1]:+UpmtLongp[,counter::counter+timeslots-1]
					
					XBP=XBP[posofGamers,]
					
					keys=statIdLongp[posofGamers]
					errCheckp=select(errCheck,gameMarkerp)
					lnewsCheckp=select(lnewsLongp,gameMarkerp)
					otherlCheckp=select(otherlLongp,gameMarkerp)
					nnewsCheckp=select(nnewsLongp,gameMarkerp)
					
					Errsp=J(rows(keys),timeslots,.)
					for (t=1;t<=rows(keys);t++) Errsp[t,]=asarray(priceErrsR,(keys[t],d))

					pricesTry=XBP:+(1:-errCheckp):*select(UpmodObsp,gameMarkerp):+errCheckp:*Errsp

					surpTry=XBP:+(1:-errCheckp):*select(UpmodObsp,gameMarkerp):+errCheckp:*Errsp
					surpTry=bpo[1]:/(1-bpo[1]):*lnewsCheckp:*exp(surpTry):+
						bpo[2]:/(1-bpo[2]):*otherlCheckp:*exp(surpTry):+
						bpo[3]:/(1-bpo[3]):*nnewsCheckp:*exp(surpTry)
			
					
					if (s==1) {
						valold=sum(surpTry)
						siOld=sharesTry
						suOld=surpTry
						piOld=pricesTry
						printf("To begin:\n")
						printf("Players: %9.0g\n",playersp)
						sum(siOld),sum(exp(piOld)),sum(suOld)
					}
					else {
						valtry=sum(surpTry)
						if (valtry>valold) {
							lnewsOld=lnewsTry
							otherlOld=otherlTry
							nnewsOld=nnewsTry
							siOld=sharesTry
							suOld=surpTry
							piOld=pricesTry
							valold=valtry
						}
						
					}
				}
			asarray(SurpMax,(d,i,1),siOld)
			asarray(SurpMax,(d,i,2),piOld)
			asarray(SurpMax,(d,i,3),suOld)
			asarray(SurpMaxProfiles,(d,i,1),lnewsOld)
			asarray(SurpMaxProfiles,(d,i,2),otherlOld)
			asarray(SurpMaxProfiles,(d,i,3),nnewsOld)
			colsum(lnewsLongp)
			colsum(lnewsOld)
			colsum(nnewsLongp)
			colsum(nnewsOld)
			sum(siOld),sum(exp(piOld)),sum(suOld)
			}
		}
	counter=counter+timeslots
	}
end

/* Need to also compute the Nash equilibria, particularly in considering a sampling weight */

mata
	NashProfiles=asarray_create("real",4)
	NashProfits  =asarray_create("real",3)
	NashShares =asarray_create("real",3)

	/* Hold the "plausible" points at which to check for a unilateral deviation */
	
	PriceMaxSps=asarray_create("real",2) 
	
	/* The number of actions to try */
	
	actsToTry=10
	
	counter=1
	for (d=1;d<=draws;d++) {
	
		for (i=1;i<=rows(mLong);i++) {
		i,d
			gameMarkerp=panelsubmatrix(gameMarker,i,mLong)
			playersp=colsum(gameMarkerp)
			if (playersp>0) {
				statIdLongp=panelsubmatrix(statIdLong,i,mLong)	/* For each market, determine who is in the game */
				Gamers=select(statIdLongp,gameMarkerp)		/* Get their ids */
				posofGamers=mm_which(gameMarkerp)		/* Who is where I guess */

				UvmtLongp=panelsubmatrix(UvmtLong,i,mLong)
				UvmodLongp=panelsubmatrix(UvmodLong,i,mLong)
				UvsLongp=panelsubmatrix(UvsLong,i,mLong)
				XBVngLongp=panelsubmatrix(XBVngLong,i,mLong)
				UvmodObsLongp=panelsubmatrix(UvmodObsLong4,i,mLong)
				
				/* New code to get likely deviation spots */
				for (g=1;g<=playersp;g++) {
					pricesToUse=asarray(simPricesR,(Gamers[g],d))
					sharesToUse=asarray(sharesBcsR,(Gamers[g],d,1))
					maxindex(rowsum(exp(pricesToUse)),actsToTry,iP=.,w=.)
					asarray(PriceMaxSps,(Gamers[g],d),iP)	/* Store for each player a list of potential better actions */
				}
				
				UpmodObsp=panelsubmatrix(UpmodObs,i,mLong)
				UpsLongp=panelsubmatrix(UpsLong,i,mLong)
				UpmtLongp=panelsubmatrix(UpmtLong,i,mLong)
				
				lnewsLongp=panelsubmatrix(lnewsLong,i,mLong)
				otherlLongp=panelsubmatrix(otherlLong,i,mLong)
				nnewsLongp=panelsubmatrix(nnewsLong,i,mLong)
				othercLongp=panelsubmatrix(othercLong,i,mLong)
				popLongp=panelsubmatrix(popLong,i,mLong)
				l_ACS_HHLongp=panelsubmatrix(l_ACS_HHLong,i,mLong)
				
				/* Before doing anything else, let's generate a slew of profiles for market participants */
				/* that seem plausible.  That's one possibility. Another is to do an MCMC search for what we are after. */
				
				/* Here is an mcmc search */
			
				otherlOld=otherlLongp	/* Actual actions */
				lnewsOld=lnewsLongp	/* In all three dimensions */
				nnewsOld=nnewsLongp
				othercOld=othercLongp
				
				/* initialize list of Nash eqs with observed eq */
	
				neq=1
				asarray(NashProfiles,(i,d,1,neq),lnewsOld)
				asarray(NashProfiles,(i,d,2,neq),otherlOld)
				asarray(NashProfiles,(i,d,3,neq),nnewsOld)	
				
				/* Guess at a new profile - this time from small list */
				
				for (s=1;s<=1000;s++) {	
					targetPlayerN=round(1+(playersp-1)*runiform(1,1))	/* pick a player */
					targetPlayer=Gamers[targetPlayerN]			/* get his station id */
					
					/* Potential profiles for now as before */
					
					lnewsPlayer=asarray(Bcs,(targetPlayer,1))		/* Profiles for the player */
					otherlPlayer=asarray(Bcs,(targetPlayer,2))
					nnewsPlayer=asarray(Bcs,(targetPlayer,3))
					othercPlayer=1:-lnewsPlayer:-otherlPlayer:-nnewsPlayer

					/* Pick a row             */
					/* This isn't quite right */
						
					newStrat=round(1+(rows(lnewsPlayer)-1)*runiform(1,1))

					/* Replace the strategy */

					targetPos=posofGamers[targetPlayerN]
					lnewsTry=lnewsOld
					nnewsTry=nnewsOld
					otherlTry=otherlOld
					othercTry=othercOld
						/*First try a deviation */
					
					lnewsTry[targetPos,]=lnewsPlayer[newStrat,]
					nnewsTry[targetPos,]=nnewsPlayer[newStrat,]
					otherlTry[targetPos,]=otherlPlayer[newStrat,]
					othercTry[targetPos,]=othercPlayer[newStrat,]
					
					/* Now, find the shares at this new strategy */

					/* Find the shares block begins - we need another loop outside of this one which
					   should be a do-while loop to see if we can find beneficial unilateral deviations */
					  
					fail=0
					g=1	/* Counter for the "gamer"         */

					/* Probably want this in an if-block so we get payoffs at Nash eq: */
					
					simPpsInit=J(rows(lnewsTry),cols(lnewsTry),0)
					lnewsInit=lnewsTry
					nnewsInit=nnewsTry
					otherlInit=otherlTry
					othercInit=othercTry
					do {  
						rowsToTry=asarray(PriceMaxSps,(Gamers[g],d))				// Select set of strategies that give high payoffs, given what others are doing.
						playerPos=mm_which(statIdLongp:==Gamers[g])				// testing player g to see if they can profitably deviate from the given strategy	
						a=1									// counter to choose from the rowsToTry
						firstTime=1								// marker for first time through. 
						do {
							if (firstTime!=1) {
								lnewsDev=asarray(Bcs,(Gamers[g],1))[rowsToTry[a],]
								otherlDev=asarray(Bcs,(Gamers[g],2))[rowsToTry[a],]
								lnewsTry[playerPos,]=lnewsDev
								otherlTry[playerPos,]=otherlDev
							}


							/* Simulate shares at the deviation */
							
							lnewsLongLagp=J(rows(lnewsLongp),1,0)
							nnewsLongLagp=J(rows(lnewsLongp),1,0)
							otherlLongLagp=J(rows(lnewsLongp),1,0)
							siLagp=J(rows(lnewsLongp),1,0)
							totlnewsp=J(rows(lnewsLongp),1,0)
							totnnewsp=J(rows(nnewsLongp),1,0)	
						
							/* Compute the totals for Ackerberg's Stuff */
							
							lnewsnTry=lnewsTry:*ln(1:+colsum(lnewsTry))
							otherlnTry=otherlTry:*ln(1:+colsum(otherlTry))
							nnewsnTry=nnewsTry:*ln(1:+colsum(nnewsTry))
							othercnTry=othercTry:*ln(1:+colsum(othercTry))
						
							sharesTry=J(rows(lnewsLongp),0,0)
							for (t=1;t<=timeslots;t++) {
								if (t!=1) siLagp=ln(siLagp)
								XV=lnewsTry[,t],otherlTry[,t],nnewsTry[,t],
									lnewsLongLagp:*lnewsTry[,t],
									nnewsLongLagp:*lnewsTry[,t],
									lnewsLongLagp:*nnewsTry[,t], 					
									nnewsLongLagp:*nnewsTry[,t],siLagp,
									siLagp:*lnewsLongLagp:*lnewsTry[,t],
									siLagp:*nnewsLongLagp:*lnewsTry[,t],
									siLagp:*lnewsLongLagp:*nnewsTry[,t],			
									siLagp:*nnewsLongLagp:*nnewsTry[,t],	
									lnewsTry[,t]:*ln(1:+totlnewsp),nnewsTry[,t]:*ln(1:+totnnewsp),l_ACS_HHLongp[,t],
									lnewsnTry[,t],otherlnTry[,t],nnewsnTry[,t],othercnTry[,t],J(rows(lnewsLongp),1,1)

							/* First, set beta*XV as given for everyone else */
									
									errCheck=lnewsTry[,t]:!=lnewsLongp[,t]
									XBVact=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
										UvsLongp[,counter+t-1]:+UvmodObsLongp[,t]
									XBVsim=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
										UvsLongp[,counter+t-t]:+UvmodLongp[,counter+t-1]

									XBV=(1:-errCheck):*XBVact:+errCheck:*XBVsim
									
									sharesTry=sharesTry,eshares_up(XBV,lnewsTry[,t],otherlTry[,t],nnewsTry[,t],
												othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
												
										/* Re-initialize lagged variables */
									siLagp=sharesTry[,t]
									totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsTry[,t]:*sharesTry[,t]))	/*Shouldn't this be lnewsTry? */
									totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsTry[,t]:*sharesTry[,t]))	/*Same here...*/		
									lnewsLongLagp=lnewsTry[,t]
									nnewsLongLagp=nnewsTry[,t]
									otherlLongLagp=otherlTry[,t]	
							}
							/* We now have shares so we can now compute prices */
							errCheck=lnewsTry:!=lnewsLongp
							
							/* Get pricing errors */
							UpmodErrp=J(rows(UpmodObsp),timeslots,.)
							
							for (k=1;k<=playersp;k++) {
								UpmodErrp[posofGamers[k],]=asarray(priceErrsR,(Gamers[k],d))			/* Was not iterating on d before - are now */
							}
							
							Errs=errCheck:*UpmodErrp:+(1:-errCheck):*UpmodObsp[,1::timeslots]	/* Okay given structure of UpmodErrp - subtle change from simulation */
							simPps=(lnewsTry*bpo[1]:+otherlTry*bpo[2]:+nnewsTry*bpo[3]):*ln(sharesTry:*popLongp)+
								lnewsTry*bpo[4]:+otherlTry*bpo[5]:+l_ACS_HHLongp:*bpo[6]:+bpo[10]:+
								UpsLongp[,counter::counter+timeslots-1]:+UpmtLongp[,counter::counter+timeslots-1]:+
								Errs
											
							if (rowsum(exp(simPps[playerPos,]))>rowsum(exp(simPpsInit[playerPos,])) & firstTime!=1) fail=1
							if (firstTime==1) {
								firstTime=0
								simPpsInit=simPps
								sharesInit=sharesTry
							}
							else a++
							
						} while (a<=actsToTry & fail==0)
						g++

					} while (g<=rows(Gamers) & fail==0)
					if (fail==0) {
						printf("New Equilibrium Found!\n") 
						neq++
						asarray(NashProfiles,(i,d,1,neq),lnewsInit)
						asarray(NashProfiles,(i,d,2,neq),otherlInit)
						asarray(NashProfiles,(i,d,3,neq),nnewsInit)
						asarray(NashShares,(i,d,neq),sharesInit)
						asarray(NashProfits,(i,d,neq),simPpsInit) 
						
					}
				}
			}
		}
	counter=counter+timeslots
	}
end	
mata: mata matsave /user/mjbaker/TV/ad_sample/Simulations SurpMax SurpMaxProfiles ShareMax ShareMaxProfiles ProfMax ProfMaxProfiles NashProfiles NashProfits NashShares, replace    







