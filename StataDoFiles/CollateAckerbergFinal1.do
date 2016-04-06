// Trying to construct everything as we go - we will need to first create the data looping over panels

clear all
cd C:\users\mjbaker\documents\github\newshour
use ".AverageDataDynamicdta", clear
quietly do "/user/mjbaker/TV/ad_sample/AveragedDataDynamicMataFinal1.do"
set more off

/* One thing we have to do for whatever reason is recreate the game variable */

capture gen game=(lnews | otherl) & lnpps!=. // Depends on data set being used!

sort market stationid mt timeslot

mata
st_view(lnews=.,.,"lnews")
st_view(nnews=.,.,"nnews")
st_view(otherl=.,.,"otherl")
st_view(otherc=.,.,"otherc")
st_view(si=.,.,"si")
st_view(mt=.,.,"mt")
st_view(id=.,.,"stationid")
st_view(M=.,.,"market")
st_view(time=.,.,"timeslot")
st_view(game=.,.,"game")
st_view(lnpps=.,.,"lnpps")	
//st_view(pop=.,.,"Mpop")
st_view(pop=.,.,"ACS_HH")	 /* Simplest way to change pop definition */
st_view(l_ACS_HH=.,.,"l_ACS_HH") /* read in log too as it is now also an explanatory variable */
st_view(lnewsn=.,.,"lnewsn")
st_view(otherln=.,.,"otherln")
st_view(nnewsn=.,.,"nnewsn")
st_view(othercn=.,.,"othercn")
st_view(dln=.,.,"dln")

lnviewn=ln(pop:*si)

m=panelsetup(M,1)

/* Note we are using the "new" estimates now where things are done correctly */

mata matuse /user/mjbaker/TV/ad_sample/DynoStarts23

	bo[,1::4]=bo[,1::4]:^2
	bo=bo[rows(bo),]

/* The simplest thing to do to draw the individual level effects seems to be to do so as follows */

	betaDynoStart=bo[(5..23,27)]

	sdstav=exp(bo[24])
	sdmarv=exp(bo[25])
	sdmodv=exp(bo[26])
	draws=20

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

	popLong[counter::counter+rows(positioner)-1,.]=popc
	l_ACS_HHLong[counter::counter+rows(positioner)-1,.]=l_ACS_HHc
	
	lnewsnLong[counter::counter+rows(positioner)-1,.]=lnewsnc
	otherlnLong[counter::counter+rows(positioner)-1,.]=otherlnc
	nnewsnLong[counter::counter+rows(positioner)-1,.]=nnewsnc	
	othercnLong[counter::counter+rows(positioner)-1,.]=othercnc	

	/* Group shares */
	
	dlnLong[counter::counter+rows(positioner)-1,.]=dlnc

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

/* As all of these variables enter the model as logs, we should first change "siLongLag" to reflect this */

	siLongLag=ln(siLongLag)
	_editmissing(siLongLag,0)
	
	siXlnln=siLongLag:*lnewslnews
	siXlnnn=siLongLag:*lnewsnnews
	siXnnln=siLongLag:*nnewslnews
	siXnnnn=siLongLag:*nnewsnnews
	
	lnewsRT=lnewsLong:*ln(1:+totlnews)
	nnewsRT=nnewsLong:*ln(1:+totnnews)	/* this is how the variables enter in the regression */

end

/* Up to this point, all we have done is get the data - replete with lags - 
   into wide format.   */
	


/**************** Here we are just checking that our ideas are sound and that the share simulators gives good answers ***********/
/********************************************************************************************************************************/
/* simulating shares - XB (without sigmas) */	

mata	

	xBDyno=J(rows(lnewsLong),cols(lnewsLong),0)
	
	for (i=1;i<=cols(lnewsLong);i++) {
		xDyno=lnewsLong[,i],otherlLong[,i],nnewsLong[,i],
			lnewslnews[,i],lnewsnnews[,i],nnewslnews[,i],nnewsnnews[,i],
			siLongLag[,i],siXlnln[,i],siXlnnn[,i],siXnnln[,i],siXnnnn[,i],
			lnewsRT[,i],nnewsRT[,i],l_ACS_HHLong[,i],lnewsnLong[,i],otherlnLong[,i],nnewsnLong[,i],othercnLong[,i],J(rows(lnewsLong),1,1)
	
		xBDyno[,i]=xDyno*betaDynoStart'
	}
	
/* Now just xB terms in place given model parameter estimates */

/* Can we get back shares this way? */

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
	simSharesLong2=J(rows(lnewsLong),cols(lnewsLong),.)
	mLong=panelsetup(marketIdLong,1)

	for (i=1;i<=rows(mLong);i++){
		xBpUEp=panelsubmatrix(xBpUE,i,mLong)
		lnewsLongp=panelsubmatrix(lnewsLong,i,mLong)
		otherlLongp=panelsubmatrix(otherlLong,i,mLong)
		nnewsLongp=panelsubmatrix(nnewsLong,i,mLong)
		othercLongp=panelsubmatrix(othercLong,i,mLong)
		for (t=1;t<=timeslots;t++) {
			simSharesLong[mLong[i,1]::mLong[i,2],t]=
			esharesStable(xBpUEp[,t],lnewsLongp[,t],otherlLongp[,t],
			nnewsLongp[,t],othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
			simSharesLong2[mLong[i,1]::mLong[i,2],t]=
				eshares_up(xBpUEp[,t],lnewsLongp[,t],otherlLongp[,t],
				nnewsLongp[,t],othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
	
		}
	}
	
	/* Would it be prudent to perhaps introduce a correction term for those predictions that seem to be off by a good margin? */
	
	/* Looks good so far. Now, to simulate shares we have to draw 1) market-level errors,
		2) station level errors, and 3) station-market level errors */
	/* First, observed utilities are (we have to first make a dependent variable: */
	
	dln=ln(siLong):-ln(1:-(slnewsLong:+snnewsLong:+sotherlLong:+sothercLong))
	
	XBVngLong=dln-bo[1]*lnswgLong:*lnewsLong-bo[2]*lnswgLong:*otherlLong-
		bo[3]*lnswgLong:*nnewsLong-bo[4]*lnswgLong:*othercLong

	
	/* We are also going to save our prediction erors for use later */
	
	errs=siLong:-simSharesLong2

end

/************** Share simulations concluded ************************/
/*Simulated shares are very, very close to actual shares, which is good */

mata
	/* Draws are a little bit trickier to save here - it would be fruitful to save them as 
	   three dimensional objects...Associative array, perhaps? Or just a stacked matrix? (stacked matrix) */

/* Make draws by each panel of fixed effects, and then convert them to the Panel format we have been favoring */
	
	iev1=paneldraw(id,draws)
	iev2=paneldraw(mt,draws)
	iev1=sdstav*invnormal(iev1)
	iev2=sdmarv*invnormal(iev2)

	UvsLong=J(rows(lnewsLong),0,0)
	UvmtLong=J(rows(lnewsLong),0,0)
	
	for (i=1;i<=draws;i++) {
		UvsLong=UvsLong,colshape(iev1[,i],timeslots)
		UvmtLong=UvmtLong,colshape(iev2[,i],timeslots)
	}

	
	UvmodLong=invnormal(runiform(rows(lnewsLong),timeslots*draws))*sdmodv

	/* Brute force */
	UvmodObsLong4=J(rows(lnewsLong),timeslots*draws,0)

	counter=0
	for (d=1;d<=draws;d++) {
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
			UvmodObsLong4[,counter+t]=yvt:-sigmaStuff:-XVt*betaDynoStart':-UvsLong[,counter+t]:-UvmtLong[,counter+t]		/* This is critical with more expansive RE simulation */
		}
		counter=counter+timeslots
	}
	
	UvmodObsLong=xBpUE:-xBDyno
	UvmodObsLong=J(1,draws,1)#UvmodObsLong:-UvsLong:-UvmtLong
	
	
	/* Schematic for bo: 
	1. mu_l, 2. mu_o, 3. mu_n, 4. mu_c, 5. eta_l, 6. eta_o,
	7. eta_n, 8. eta_ll, 9. eta_ln, 10. eta_nl, 11. eta_nn
	12. lam_own 13. lam_ll, 14. lam_ln, 15. lam_nl, 16. lam_nn
	17. rho_l, 18. rho_n , msize, zeta_l, zeta_o, zeta_n, zeta_c ,alpha */	
	
	/* So, when I switch to lnews, I get (isn't this all just taken care of?) */
	/* Draw for those currently doing "other" */
	/* We have to go through and replace everything with how it was actually calculated !*/
	/* And we want to "do the opposite" of what we have done for lnews */
	/* It should be sufficient to replace otherl with lnews and vice versa */
	/* Now these are the "switching" values which we aren't really using */
	
end
	
	/* Note that XBVgLong should be the first variable that we are storing in the Ackerberg sampler */
	
mata
	
	/* Now, we have viewerships - what do we do next? */
	/* Well, we need price information */
	
	mata matuse /user/mjbaker/TV/ad_sample/betaPDynoStarts, replace
//	bpo=bo[(24..33)]
	
	/* We have the usual station-market-model breakdown of the variance components */
	/* We need to draw two sets of values: E-terms for use in the traditional simulator */
	/* And U terms for use in the importance sampler */

	sdstap=exp(bpo[7])
	sdmarp=exp(bpo[8])
	sdmodp=exp(bpo[9])
	
	/* Drawing by panel, and then rearranging so that we have the draws in the right format */
	
	iep1=paneldraw(id,draws)
	iep2=paneldraw(mt,draws)
	iep1=sdstap*invnormal(iep1)
	iep2=sdmarp*invnormal(iep2)	
	
	
	UpsLong=J(rows(lnewsLong),0,0)
	UpmtLong=J(rows(lnewsLong),0,0)
	
	for (i=1;i<=draws;i++) {
		UpsLong=UpsLong,colshape(iep1[,i],timeslots)
		UpmtLong=UpmtLong,colshape(iep2[,i],timeslots)
	}
	
	UpmodLong=invnormal(runiform(rows(lnewsLong),timeslots*draws))*sdmodp
	
	/* A trick to replace the above */
	
	

	/* Alternatively  one could use UvmodObsLong computed above */
end
mata	
	/* Now, how do we get pricing errors in the dynamic setting?    */
	/* Really, things are dictated by the viewership model, which   */
	/* moves forward. In the very last period, we have */
	
	/* Let's just make a few generic matrices here to give us fictional strategies */
	
	/* Mark stations that make at least one decision */
	
	gameMarker=rowsum(gameLong):>0
	decCounter=rowsum(gameLong)
	
	/* It turns out there are either five or six decisions for each type. */
	/* All possible decisions for a "six" type */
	/* Let's make an associative array holding this stuff */
	
	Bcs=asarray_create("real",2)
	sharesBcs=asarray_create("real",3)
	uVinfo   =asarray_create("real",3)
	
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
			else if (nnewsPos==6) lnewsp=stratsFree,J(rows(stratsFree),1,0)
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
mata

/* Loop over markets and recursively construct data */
/* First, we need to extract true errors from model */
/* Recall that XbvMeanDyno holds XB variables       */
/* So: let's recap what is constructed here. First, */
/* we have a basic variable */

	counter=1
	sniffTest=J(0,3,.)

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
				UvmodObsLongp=panelsubmatrix(UvmodObsLong,i,mLong)			/* So, down here we are using UvmodObsLong4 */
				errsp=panelsubmatrix(errs,i,mLong)
				
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
							
								XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
									UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
						
								if (lnewsHat[q,t]!=lnewsOrig[t]) {
									XBV[place,]=XV[place,]*betaDynoStart':+UvmtLongp[place,counter+t-1]:+
										UvsLongp[place,counter+t-1]:+UvmodLongp[place,counter+t-1]

								}
					
							XBVplaceHold[q,t]=XBV[place]
							XBVplaceHoldMean[q,t]=XBV[place]-UvmodLongp[place,counter+t-1]
					
							sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
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
						asarray(sharesBcs,(pId,c,1),sharesToPlace)
						asarray(sharesBcs,(pId,c,2),soPlace)
						asarray(sharesBcs,(pId,c,3),sgPlace)
						asarray(Bcs,(pId,4),XBVplaceHold)
						asarray(Bcs,(pId,5),XBVplaceHoldMean)
						lnewsLongp[place,]=lnewsOrig
						otherlLongp[place,]=otherlOrig	
						sniffTest=sniffTest \ (c,mean(rowsum(sharesToPlace)),rowsum(siLong[k,]))
									
					}		
				}	
			}	
		}	
	counter=counter+timeslots	
	}
end
/* So there we have simulated all the shares from deviating -                  */
/* Now, the errors that we haved used are the six simulated viewership shares. */

/* Organize shares and Ackerberg information */
/* The information we need is in Bcs, which has three elements per stationId - the lnews profile, 
   the other profile, and the national news profile. shareBcs has shares corresponding to that */
   
   /* create looper */
mata:   
	uv=J(rows(statIdLong),0,.)			/* For correct sampling, we really need uvg and uvsi                                      */
	uvg=J(rows(statIdLong),0,.)			/* to be clear, si should be hypothetical share and sg should be hypothetical group share */
	uvsi=J(rows(statIdLong),0,.)
	uvso=J(rows(statIdLong),0,.)
	uvsg=J(rows(statIdLong),0,.)
	uvre1=J(rows(statIdLong),0,.)
	uvre2=J(rows(statIdLong),0,.)
	uvre1g=J(rows(statIdLong),0,.)
	uvre2g=J(rows(statIdLong),0,.)
   
	looper=uniqrows(asarray_keys(sharesBcs)[,1])
	counter=1
  
	for (c=1;c<=draws;c++) {
		ackToAdd=J(rows(statIdLong),timeslots,.)
		ackToAddg=J(rows(statIdLong),timeslots,.)
		ackToAddsi=J(rows(statIdLong),timeslots,.)
		ackToAddso=J(rows(statIdLong),timeslots,.)
		ackToAddsg=J(rows(statIdLong),timeslots,.)
		ackToAddre1=J(rows(statIdLong),timeslots,.)
		ackToAddre2=J(rows(statIdLong),timeslots,.)
		
		for (i=1;i<=rows(looper);i++) {
			statId=looper[i]
			lnewsHat=asarray(Bcs,(statId,1))
			otherlHat=asarray(Bcs,(statId,2))
			nnewsHat=asarray(Bcs,(statId,3))
			
			siHat=asarray(sharesBcs,(statId,c,1))
			soHat=asarray(sharesBcs,(statId,c,2))
			sgHat=asarray(sharesBcs,(statId,c,3))
			XBVs=asarray(Bcs,(statId,4))
			XBVsm=asarray(Bcs,(statId,5))
			idPos=mm_which(statId:==statIdLong)
			lnewsAct=lnewsLong[idPos,]
			otherlAct=otherlLong[idPos,]
			nnewsAct=nnewsLong[idPos,]
			Mpop=select(popLong,statIdLong:==statId)
		
			for (t=6;t>=2;t--) {
				if (nnewsAct[,t]!=1) {
					sameButOneProfs=mm_which((rowsum(lnewsAct[,1::t-1]:==lnewsHat[,1::t-1]):==t-1):*
						(lnewsAct[,t]:!=lnewsHat[,t]))
					sameButOneProfs=min(sameButOneProfs)

					ackToAddsi[idPos,t]=siHat[sameButOneProfs,t]	/* save the shares */
					ackToAddso[idPos,t]=soHat[sameButOneProfs,t]	
					ackToAddsg[idPos,t]=sgHat[sameButOneProfs,t]

					ackToAdd[idPos,t]=XBVs[sameButOneProfs,t]	/* This is wrong! */
					mu=lnewsHat[sameButOneProfs,t]*bo[1]+
					   otherlHat[sameButOneProfs,t]*bo[2]+
					   nnewsHat[sameButOneProfs,t]*bo[3]+
					   (1-lnewsHat[sameButOneProfs,t]-otherlHat[sameButOneProfs,t]-nnewsHat[sameButOneProfs,t])*bo[4]
					/* I think for the correct problem we really only need the following with the jacobian term tacked on */
					ackToAddg[idPos,t]=lnnd(XBVs[sameButOneProfs,t],XBVsm[sameButOneProfs,t],bo[22])-ln(1/(1-mu)-
						mu*siHat[sameButOneProfs,t]/sgHat[sameButOneProfs,t]-siHat[sameButOneProfs,t])
				}
				else {
					ackToAdd[idPos,t]=0
					ackToAddg[idPos,t]=0
					ackToAddsi[idPos,t]=0
					ackToAddso[idPos,t]=0
					ackToAddsg[idPos,t]=0
				}
			}
			if (nnewsAct[1]!=1) {
				sameButOneProfs=mm_which(lnewsAct[1]:!=lnewsHat[,1])
				sameButOneProfs=min(sameButOneProfs)
				ackToAdd[idPos,1]=XBVs[sameButOneProfs,t]
					mu=lnewsHat[sameButOneProfs,1]*bo[1]+
					   otherlHat[sameButOneProfs,1]*bo[2]+
					   nnewsHat[sameButOneProfs,1]*bo[3]+
					   (1-lnewsHat[sameButOneProfs,1]-otherlHat[sameButOneProfs,1]-nnewsHat[sameButOneProfs,1])*bo[4]			
				
				ackToAddg[idPos,1]=lnnd(XBVs[sameButOneProfs,1],XBVsm[sameButOneProfs,1],bo[26])-ln(1/(1-mu)-
						mu*siHat[sameButOneProfs,1]/sgHat[sameButOneProfs,1]-siHat[sameButOneProfs,1])
				ackToAddsi[idPos,1]=siHat[sameButOneProfs,1]
				ackToAddso[idPos,1]=soHat[sameButOneProfs,1]
				ackToAddsg[idPos,1]=sgHat[sameButOneProfs,1]
			}
			else {
					ackToAdd[idPos,1]=0
					ackToAddg[idPos,1]=0
					ackToAddsi[idPos,1]=0
					ackToAddso[idPos,1]=0
					ackToAddsg[idPos,1]=0
			}
			ackToAddre1[idPos,]=UvsLong[idPos,counter::counter+timeslots-1]
			ackToAddre2[idPos,]=UvmtLong[idPos,counter::counter+timeslots-1]
		}
		uv=uv,ackToAdd
		uvg=uvg,ackToAddg
		uvsi=uvsi,ackToAddsi
		uvso=uvso,ackToAddso
		uvsg=uvsg,ackToAddsg
		uvre1=uvre1,ackToAddre1
		uvre2=uvre2,ackToAddre2
		counter=counter+timeslots  
	}
	/* Sampling weights for random effects */
		
		uvre1g=lnnd(uvre1,0,ln(sdstav)):*1:/rowsum(gameLong)	/* downweighted for multiple counts */
		
	/* Weights for the second, market level effect (can be used again below) */
	
	uvre2Weights=J(rows(statIdLong),timeslots,.)
	for (i=1;i<=rows(mLong);i++) {
		gP=panelsubmatrix(gameLong,i,mLong)
		gPtotal=colsum(gP)
		weightsP=(gPtotal:*gP)
	uvre2Weights[mLong[i,1]::mLong[i,2],.]=weightsP
	}
		uvre2g=lnnd(uvre2,0,ln(sdmarv)):*1:/rowsum(uvre2Weights)
	
	
	
end
/* Now ackView and ackViewG hold the mean terms for viewership and also the mean importance sampling weights */

mata
/* Recover actual pricing errors - this could be a source of problems...*/

	UpmodObs=J(rows(lnewsLong),0,0)
	for (i=1;i<=draws*timeslots;i=i+timeslots) {
		UpmodObs=UpmodObs,lnppsLong:-lnewsLong:*lnviewnLong:*bpo[1]:-otherlLong:*lnviewnLong:*bpo[2]:-
			nnewsLong:*lnviewnLong:*bpo[3]:-lnewsLong:*bpo[4]:-otherlLong:*bpo[5]:-l_ACS_HHLong:*bpo[6]:-bpo[10]:-
			UpsLong[,i::i+timeslots-1]:-UpmtLong[,i::i+timeslots-1]
	}
end

/* Let's just try to get one station straight here first - try 12267                   */
/* For each station, we are going to save a series of draws for the error terms        */
/* We are already saving shares and other stuff, so we can save UpmodObs as above, and */

mata

	  priceErrs=asarray_create("real",2)
	  simPrices=asarray_create("real",2)
	priceBounds=asarray_create("real",2)
	
	up=J(rows(statIdLong),0,.)
	upg=J(rows(statIdLong),0,.)
	upb=J(rows(statIdLong),0,.)
	vup=J(rows(statIdLong),0,.)
	upre1=J(rows(statIdLong),0,.)
	upre2=J(rows(statIdLong),0,.)

/* For troubleshooting */
	Troublers=J(0,6,.)
	allDraws=J(0,6,.)


	counter=1
	for (d=1;d<=draws;d++) {
		uptoAdd=J(rows(statIdLong),6,.)
		uptoAddg=J(rows(statIdLong),6,.)
		uptoAddb=J(rows(statIdLong),6,.)
		vuptoAdd=J(rows(statIdLong),6,.)
		upre1toAdd=J(rows(statIdLong),6,.)
		upre2toAdd=J(rows(statIdLong),6,.)
		gameList=J(0,2,.)
		for (i=1;i<=rows(statIdLong);i++) {
			if (gameMarker[i]==1) {
				idp=statIdLong[i]
				gameList=gameList \ (idp,i)
				lnewsHat=asarray(Bcs,(idp,1))
				otherlHat=asarray(Bcs,(idp,2))
				nnewsHat=asarray(Bcs,(idp,3))
				sharesHat=asarray(sharesBcs,(idp,d,1))
				lnewsAct=lnewsLong[i,]
				otherlAct=otherlLong[i,]
				nnewsAct=nnewsLong[i,]
			
				problem=0 
				Try=1
			
				do {	
					errMarker=(lnewsAct:==lnewsHat):*(otherlAct:==otherlHat)	
					errPdraws=J(1,6,0)
	
					p=mm_which(((rowsum(errMarker[,1::5])):==5):*(errMarker[,timeslots]:==0))	/* Same until last period */
			
					actPayMean=(lnewsHat[p,]*bpo[1]:+otherlHat[p,]*bpo[2]:+nnewsHat[p,]*bpo[3]):*ln(popLong[i,]:*sharesHat[p,]):+
						lnewsHat[p,]:*bpo[4]:+otherlHat[p,]*bpo[5]:+bpo[6]:*l_ACS_HHLong[i,]:+bpo[10]:+
						UpsLong[i,counter::counter+timeslots-1]:+UpmtLong[i,counter::counter+timeslots-1]
					Bound=J(1,timeslots,.)	

					Bound[timeslots]=lnppsLong[i,timeslots]-actPayMean[timeslots]
					errPdraws[,timeslots]=exp(bpo[9])*invnormal(runiform(1,1)*normal(Bound[timeslots]/exp(bpo[9])))
				
			/* Fill in stuff for Ackerberg */
					if (nnewsLong[i,timeslots]!=1) {
						uptoAdd[i,timeslots]=actPayMean[timeslots]+errPdraws[timeslots]
						meanP=actPayMean[timeslots]
						uptoAddg[i,timeslots]=lnnd(uptoAdd[i,timeslots],meanP,bpo[9])-
							ln(normal(Bound[timeslots]/exp(bpo[9])))

						uptoAddb[i,timeslots]=actPayMean[timeslots]+Bound[timeslots]
						vuptoAdd[i,timeslots]=ln(popLong[i,1]:*sharesHat[p,timeslots])	/* should be equal to viewership used above */
					
					}
					else {
						uptoAdd[i,timeslots]=0
						uptoAddg[i,timeslots]=0
						uptoAddb[i,timeslots]=0
						vuptoAdd[i,timeslots]=0
					}
			
			/* Recursion for drawing errors */
			/* First, pull out the correct entry of the model errors */
			
					UpmodObz=UpmodObs[i,counter::counter+timeslots-1]
					for (t=timeslots-1;t>=1;t--) {
						if (nnewsAct[t]!=1) {
							if (t>1) {
								p=mm_which((rowsum(errMarker[,1::t-1]):==t-1):*(errMarker[,t]:==0))
								useErrs=J(rows(p),timeslots,0)
								useErrs[,1::t-1]=J(rows(p),1,UpmodObs[i,1::t-1])
								useErrs[,t+1::cols(useErrs)]=
									errMarker[p,t+1::cols(errPdraws)]:*UpmodObz[1,t+1::cols(errPdraws)]:+
									(1:-errMarker[p,t+1::cols(errPdraws)]):*errPdraws[,t+1::cols(errPdraws)]
							}
							else {
								p=mm_which(errMarker[,t]:==0)
								useErrs=J(rows(p),timeslots,0)	
								useErrs[,t+1::cols(useErrs)]=
									errMarker[p,t+1::cols(errPdraws)]:*UpmodObz[1,t+1::cols(errPdraws)]:+
									(1:-errMarker[p,t+1::cols(errPdraws)]):*errPdraws[,t+1::cols(errPdraws)]						
							}

							actPayMean=(lnewsHat[p,]*bpo[1]:+otherlHat[p,]*bpo[2]:+nnewsHat[p,]*bpo[3]):*ln(popLong[i,]:*sharesHat[p,]):+
								lnewsHat[p,]:*bpo[4]:+otherlHat[p,]*bpo[5]:+bpo[6]:*l_ACS_HHLong[i,]:+bpo[10]:+
								UpsLong[i,counter::counter+timeslots-1]:+UpmtLong[i,counter::counter+timeslots-1]
							
							max1=sum(exp(lnppsLong[i,t::timeslots]))
							max2=max(rowsum(exp(actPayMean[,t+1::timeslots]:+useErrs[,t+1::timeslots])))
			
							Bound[t]=ln(max1-max2)-actPayMean[rows(actPayMean),t] 		
							allDraws=allDraws \ (d,i,marketIdLong[i],idp,t,max1-max2)
							errPdraws[,t]=exp(bpo[9])*invnormal(runiform(1,1)*normal(Bound[t]/exp(bpo[9])))
							if (errPdraws[,t]==.) {
								printf("+");displayflush();(d,idp,t)
								problem=1
								Troublers=Troublers \ (d,i,marketIdLong[i],idp,t,max1-max2)
								errPdraws[,t]=-20
								Bound[t]=-20
							Try++
							Try
							}	
			
							uptoAdd[i,t]=actPayMean[rows(actPayMean),t]+errPdraws[,t]
							meanP=actPayMean[rows(actPayMean),t]
							uptoAddg[i,t]=lnnd(uptoAdd[i,t],meanP,bpo[9])-
								min((ln(normal((uptoAdd[i,t]-meanP)/exp(bpo[9]))),-ln(normal(-35))))
							uptoAddb[i,t]=meanP+Bound[t]
							vuptoAdd[i,t]=min(popLong[i,1]:*sharesHat[p,t])					
						}
						else {
							errPdraws[,t]=UpmodObz[1,t]
							uptoAdd[i,t]=0
							uptoAddg[i,t]=0
							uptoAddb[i,t]=0
							vuptoAdd[i,t]=0
						}
					}
				} while (problem==1 & Try<2)		/* See if it makes a difference here at all */


			asarray(priceErrs,(statIdLong[i],d),errPdraws)
			asarray(priceBounds,(statIdLong[i],d),Bound)
		
			simPps=(lnewsHat*bpo[1]:+otherlHat*bpo[2]:+nnewsHat*bpo[3]):*ln(sharesHat:*popLong[i,])+
				lnewsHat*bpo[4]:+otherlHat*bpo[5]:+bpo[6]:*l_ACS_HHLong[i,]:+bpo[10]:+
				UpsLong[i,counter::counter+timeslots-1]:+UpmtLong[i,counter::counter+timeslots-1]:+
				errMarker:*UpmodObz[1,1::timeslots]:+(1:-errMarker):*errPdraws
			
			asarray(simPrices,(statIdLong[i],d),simPps)
			
			upre1toAdd[i,]=UpsLong[i,counter::counter+timeslots-1]
			upre2toAdd[i,]=UpmtLong[i,counter::counter+timeslots-1]	
			}
		}
	up=up,uptoAdd
	upg=upg,uptoAddg
	upb=upb,uptoAddb
	vup=vup,vuptoAdd
	upre1=upre1,upre1toAdd
	upre2=upre2,upre2toAdd
	counter=counter+timeslots
	}

	/* Organize sampling weights on the other stuff */
	
	upre1g=lnnd(upre1,0,ln(sdstap)):*1:/rowsum(gameLong)	/* downweighted for multiple counts */
		
	/* Weights developed above. */
	
	upre2g=lnnd(upre2,0,ln(sdmarp)):*1:/rowsum(uvre2Weights)
end

/* How would we go about simulating a game under these circumstances? */
/* Finding max shares or surpluses? */
/* First: get the players together. */
/* In any of the cases, it would seem like a good idea to */
/* Preprocess - pick out a handfull of strategies that seem to generate big unilateral deviations */
/* See what happens here */

mata
	/* the identifier in the array will hold market, profile, Nash number */
	/* The "actual" market profile will always be first (a useful check!) */
	
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
					pricesToUse=asarray(simPrices,(Gamers[g],d))
					sharesToUse=asarray(sharesBcs,(Gamers[g],d,1))
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
										UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
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
								UpmodErrp[posofGamers[k],]=asarray(priceErrs,(Gamers[k],d))			/* Was not iterating on d before - are now */
							}
							
							Errs=errCheck:*UpmodErrp:+(1:-errCheck):*UpmodObsp[,counter::counter+timeslots-1]	/* Okay given structure of UpmodErrp */
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

/* One thing to note - are we trying something a bit too restrictive here? */


mata matsave /user/mjbaker/TV/ad_sample/AckerbergInfo up upg upb upre1 upre2 upre1g upre2g uv uvg vup uvsi uvso uvsg uvre1 uvre2 uvre1g uvre2g uvre2Weights Troublers, replace 
mata matsave /user/mjbaker/TV/ad_sample/AckerbergObjs NashProfiles NashShares NashProfits sharesBcs priceErrs simPrices priceBounds, replace
end










