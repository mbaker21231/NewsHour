// Trying to construct everything as we go - we will need to first create the data looping over panels

clear all
use "/user/mjbaker/TV/ad_sample/AveragedDataDynamicRE.dta", clear
do "/user/mjbaker/TV/ad_sample/AveragedDataDynamicMata.do"
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
st_view(pop=.,.,"ACS_HH")
st_view(l_ACS_HH=.,.,"l_ACS_HH")
st_view(iev1=.,.,"iev1")
st_view(iev2=.,.,"iev2")
st_view(iep1=.,.,"iep1")
st_view(iep2=.,.,"iep2")

lnviewn=ln(pop:*si)


m=panelsetup(M,1)

//mata matuse /user/mjbaker/TV/ad_sample/DynoStarts23

mata matuse /user/mjbaker/TV/ad_sample/Results
	bo=b_start
	bo[,1::4]=bo[,1::4]:^2
	bo=mean(bo)
	bpo=bo[(24..33)]

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
		l_ACS_HHLongp=panelsubmatrix(l_ACS_HH,i,m)
		
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
		l_ACS_HHLongc=J(rows(obs),0,0)
		
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
			l_ACS_HHLongc=l_ACS_HHLongc,select(l_ACS_HHLongp,timemp:==k)
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
	l_ACS_HHLong[counter::counter+rows(positioner)-1,.]=l_ACS_HHLongc

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
	
	siLongLag=ln(siLongLag)
	_editmissing(siLongLag,0)	
	
	siXlnln=siLongLag:*lnewslnews
	siXlnnn=siLongLag:*lnewsnnews
	siXnnln=siLongLag:*nnewslnews
	siXnnnn=siLongLag:*nnewsnnews
	
	lnewsRT=lnewsLong:*totlnews	/*  This seems to be a problem - do we use this? */
	nnewsRT=nnewsLong:*totnnews

mata matuse /user/mjbaker/TV/ad_sample/AckerbergInfo, replace 
mata matuse /user/mjbaker/TV/ad_sample/AckerbergObjs, replace

/* There is a huge amount of redundance in the Nash profiles        */
/* Let's start by just running through and eliminating redundancies */

	markets=uniqrows(marketIdLong)
	indexes=asarray_keys(NashProfiles)
	draws=max(indexes[,2])

	/* Clean up */

	NashProfilesNew=asarray_create("real",4)
	 NashProfitsNew=asarray_create("real",3)
	  NashSharesNew=asarray_create("real",3)
	
	for (i=1;i<=rows(markets);i++) {
		for (d=1;d<=draws;d++) {
			gameMarkerp=select(gameLong[,1],marketIdLong:==i)
			if (colsum(gameMarkerp)>0) {
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


/* Try this: */

mata
	Up=J(rows(up)*timeslots,0,.)
	Upg=J(rows(up)*timeslots,0,.)
	Upb=J(rows(up)*timeslots,0,.)
	Uv=J(rows(up)*timeslots,0,.)
	Uvg=J(rows(up)*timeslots,0,.)
	Vup=J(rows(up)*timeslots,0,.)
	Uvsi=J(rows(up)*timeslots,0,.)
	Uvsg=J(rows(up)*timeslots,0,.)
	Uvso=J(rows(up)*timeslots,0,.)
	Uvre1=J(rows(up)*timeslots,0,.)
	Uvre2=J(rows(up)*timeslots,0,.)
	Upre1=J(rows(up)*timeslots,0,.)
	Upre2=J(rows(up)*timeslots,0,.)
	Uvre1g=J(rows(up)*timeslots,0,.)
	Uvre2g=J(rows(up)*timeslots,0,.)
	Upre1g=J(rows(up)*timeslots,0,.)
	Upre2g=J(rows(up)*timeslots,0,.)
	


	counter=1
	for (d=1;d<=draws;d++) {
		Up=Up,colshape(up[,counter::counter+timeslots-1],1)
		Upg=Upg,colshape(upg[,counter::counter+timeslots-1],1)
		Upb=Upb,colshape(upb[,counter::counter+timeslots-1],1)
		Uv=Uv,colshape(uv[,counter::counter+timeslots-1],1)
		Uvg=Uvg,colshape(uvg[,counter::counter+timeslots-1],1)
		Vup=Vup,colshape(vup[,counter::counter+timeslots-1],1)
		Uvsi=Uvsi,colshape(uvsi[,counter::counter+timeslots-1],1)
		Uvsg=Uvsg,colshape(uvsg[,counter::counter+timeslots-1],1)
		Uvso=Uvso,colshape(uvso[,counter::counter+timeslots-1],1)
		Uvre1=Uvre1,colshape(uvre1[,counter::counter+timeslots-1],1)
		Uvre2=Uvre2,colshape(uvre2[,counter::counter+timeslots-1],1)
		Upre1=Upre1,colshape(upre1[,counter::counter+timeslots-1],1)
		Upre2=Upre2,colshape(upre2[,counter::counter+timeslots-1],1)
		Uvre1g=Uvre1g,colshape(uvre1g[,counter::counter+timeslots-1],1)
		Uvre2g=Uvre2g,colshape(uvre2g[,counter::counter+timeslots-1],1)
		Upre1g=Upre1g,colshape(upre1g[,counter::counter+timeslots-1],1)
		Upre2g=Upre2g,colshape(upre2g[,counter::counter+timeslots-1],1)
		
		counter=counter+timeslots
	}
	
/* So now we have all that organized as it should be               */
/* We now just have to get the error terms going as they should be */
/* We will again do it in log terms. We can allocate the probability */
/* to the last observation or spread it out. We first need the model */
/* standard deviation */

	/* First, make a station id before we cycle through the errors */
	
	Keys=uniqrows(asarray_keys(priceErrs)[,1])
	stationid=statIdLong#J(6,1,1)
	sdmodp=exp(bpo[9])
	
	ErrsToInt=J(rows(stationid),draws,.)
	ErrBounds=J(rows(stationid),draws,.)
	for (i=1;i<=rows(Keys);i++) {
		for (d=1;d<=draws;d++) {
			errToPut=asarray(priceErrs,(Keys[i],d))[1,]
			bouToPut=asarray(priceBounds,(Keys[i],d))[1,]
			minPos=min(mm_which(bouToPut:!=.))
			errToPut=errToPut[minPos]
			bouToPut=bouToPut[minPos]
			place=max(mm_which(stationid:==Keys[i]))
			ErrsToInt[place,d]=errToPut
			ErrBounds[place,d]=bouToPut
		}
	}
	
	/* Now, the last part of the sampler is the normal density, which goes as */

	lnE=ln(normal(ErrBounds/sdmodp))
	
	/* How do we get the game stuff into order? */
	/* First, make a key */
	
	Keys=asarray_keys(NashProfilesNew)
	
	market=marketIdLong#J(timeslots,1,1)
	
	gameMarkId=uniqrows(Keys[,1])
	NashW=J(rows(market),draws,.)
	
	for (i=1;i<=rows(gameMarkId);i++) {
		WtoAdd=J(1,draws,.)
		for (d=1;d<=draws;d++) {
			WtoAdd[d]=rows(select(Keys[,4],(Keys[,1]:==gameMarkId[i]):*(Keys[,2]:==d):*(Keys[,3]:==1)))
		}
		maxPos=max(mm_which(market:==gameMarkId[i]))
		NashW[maxPos,]=ln(1:/WtoAdd)
	}	

	/* We now should have all of our simulations in order */
	/* Let's save them in one bunch */
	
	/* Sampling weights */

	weights1=1:/rowsum(gameLong)#J(1,6,1)
	weights1=colshape(weights1,1)

	mLong=panelsetup(marketIdLong,1)
	weights2=J(rows(statIdLong),timeslots,.)
	for (i=1;i<=rows(mLong);i++) {
		gP=panelsubmatrix(gameLong,i,mLong)
		gPtotal=colsum(gP)
		weightsP=(gPtotal:*gP)
	weights2[mLong[i,1]::mLong[i,2],.]=weightsP
	}	
	_editvalue(weights2,0,.)
	weights2=colshape(1:/weights2,1)

	mata matsave /user/mjbaker/TV/ad_sample/gsAndus NashW Up Upg Upb Uv Uvg Uvsi Uvsg Uvso Uvre1 Uvre2 Upre1 Upre2 Uvre1g Uvre2g Upre1g Upre2g Vup market id mt weights1 weights2, replace
end
