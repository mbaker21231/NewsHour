clear all
set more off
cd C:\Users\mjbaker\Documents\Github\NewsHour
use AveragedDataDynamicRer.dta, clear

gen ordinit = _n
gen game=(lnews | otherl) & lnpps!=.
bysort stationid: egen countl = total(lnews)
bysort stationid: gen lasts = _n == _N
replace game = 0 if countl == 0
bysort stationid: egen totalGame = total(game)
gen gameMarker = totalGame>4 & countl>0
sort market stationid mt timeslot
assert ordinit == _n

mata:
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
    st_view(pop=.,.,"ACS_HH")
    st_view(l_ACS_HH=.,.,"l_ACS_HH")

    st_view(iev1=.,.,"ievr1")
    st_view(iev2=.,.,"ievr2")
    st_view(iep1=.,.,"iepr1")
    st_view(iep2=.,.,"iepr2")

    st_view(lnewsn=.,.,"lnewsn")
    st_view(otherln=.,.,"otherln")
    st_view(nnewsn=.,.,"nnewsn")
    st_view(othercn=.,.,"othercn")
    st_view(dln=.,.,"dln")

    lnviewn=ln(pop:*si)
    m=panelsetup(M,1)
end

mata:
    mata matuse Results, replace
    
    bo = b_start
    bo[,1::4] = bo[,1::4]:^2
    bo=mean(bo)
    bpo = bo[(28::37)]
    
    sdstav = exp(bo[24])
    sdmarv = exp(bo[25])
    sdmodv = exp(bo[26])
    
    betaDynoStart = bo[(5..23,27)]
	
	sdstap=exp(bpo[7])
	sdmarp=exp(bpo[8])
	sdmodp=exp(bpo[9])
end

do MataFunctions\MataReshape.do

mata:
    timeslots = 6
    iev1Long = J(rows(uniqrows(id)),timeslots,.)
    iev2Long = J(rows(uniqrows(id)),timeslots,.)
    iep1Long = J(rows(uniqrows(id)),timeslots,.)
    iep2Long = J(rows(uniqrows(id)),timeslots,.)

    counter = 1
    
    for (i=1;i<=rows(m);i++) {
        iev1p=panelsubmatrix(iev1,i,m)
        iev2p=panelsubmatrix(iev2,i,m)
        iep1p=panelsubmatrix(iep1,i,m)
        iep2p=panelsubmatrix(iep2,i,m)
        idmp = panelsubmatrix(id,i,m)
        obs = uniqrows(idmp)
        timemp = panelsubmatrix(time,i,m)           
        iev1c=J(rows(obs),0,0)
        iev2c=J(rows(obs),0,0)
        iep1c=J(rows(obs),0,0)
        iep2c=J(rows(obs),0,0)
        positioner=colshape(idmp,timeslots)
        
        for (k=3;k<=8;k++) {
            iev1c=iev1c,select(iev1p,timemp:==k)
            iev2c=iev2c,select(iev2p,timemp:==k)
            iep1c=iep1c,select(iep1p,timemp:==k)
            iep2c=iep2c,select(iep2p,timemp:==k)           
        }
        iev1Long[counter::counter+rows(positioner)-1,.]=iev1c
        iev2Long[counter::counter+rows(positioner)-1,.]=iev2c
        iep1Long[counter::counter+rows(positioner)-1,.]=iep1c
        iep2Long[counter::counter+rows(positioner)-1,.]=iep2c      
        counter = counter+rows(positioner)
    }
end


mata: decCounter = rowsum(gameLong)
mata: mm_freq(decCounter)


do MataFunctions\stratmat.do
mata:
    decCounter = rowsum(gameLong)
    gameMarker = rowsum(gameLong):>0
   
    Bcs = asarray_create("real", 2)
    sharesBcs = asarray_create("real", 3)

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

mata: 
    draws = 25
    mLong = panelsetup(marketIdLong, 1)
    
    UvsLong  = J(1,draws,1)#iev1Long
    UvmtLong = J(1,draws,1)#iev2Long
    
    swgLong=siLong:/(slnewsLong:*lnewsLong:+
        snnewsLong:*nnewsLong:+
        sotherlLong:*otherlLong:+
        sothercLong:*othercLong)
    lnswgLong = ln(swgLong)
    dln=ln(siLong):-ln(1:-(slnewsLong:+snnewsLong:+sotherlLong:+sothercLong))
    
    XBVngLong=dln-bo[1]*lnswgLong:*lnewsLong-bo[2]*lnswgLong:*otherlLong-
        bo[3]*lnswgLong:*nnewsLong-bo[4]*lnswgLong:*othercLong
    
    UvmodLong = invnormal(runiform(rows(lnewsLong),timeslots*draws))*sdmodv
    
    xBDyno=J(rows(lnewsLong),cols(lnewsLong),0)
    
    for (i=1;i<=cols(lnewsLong);i++) {
        xDyno=lnewsLong[,i],otherlLong[,i],nnewsLong[,i],
            lnewslnews[,i],lnewsnnews[,i],nnewslnews[,i],nnewsnnews[,i],
            siLongLag[,i],siXlnln[,i],siXlnnn[,i],siXnnln[,i],siXnnnn[,i],
            lnewsRT[,i],nnewsRT[,i],l_ACS_HHLong[,i],lnewsnLong[,i],otherlnLong[,i],nnewsnLong[,i],othercnLong[,i],J(rows(lnewsLong),1,1)

        xBDyno[,i]=xDyno*betaDynoStart'
    }
    
    xBpUE=ln(siLong):-ln(1:-(slnewsLong:+snnewsLong:+sotherlLong:+sothercLong)):-
        bo[1]:*lnswgLong:*lnewsLong:-
        bo[2]:*lnswgLong:*otherlLong-
        bo[3]:*lnswgLong:*nnewsLong:-
        bo[4]:*lnswgLong:*othercLong			/* These are "combined" XB+u1+u2+e errors */
    
    UvmodObsLong = J(1,draws,1)#xBpUE:-J(1,draws,1)#xBDyno:-UvmtLong-UvsLong
                
    sniffTest = J(0,3,.)
    counter = 1
end


do MataFunctions\sharemakers.do

do MataFunctions\SimUniDevs.do

mata: 
	UpsLong=J(1,draws,1)#iep1Long
	UpmtLong = J(1,draws,1)#iep2Long
	UpmodLong = invnormal(runiform(rows(lnewsLong),timeslots*draws))*sdmodp
end



mata:
	UpmodObs=lnppsLong:-lnewsLong:*lnviewnLong:*bpo[1]:-otherlLong:*lnviewnLong:*bpo[2]:-
		nnewsLong:*lnviewnLong:*bpo[3]:-lnewsLong:*bpo[4]:-otherlLong:*bpo[5]:-l_ACS_HHLong:*bpo[6]:-bpo[10]:-
		UpsLong[,1::6]:-UpmtLong[,1::6]
end

mata:
	priceErrsR=asarray_create("real",2)
	simPricesR=asarray_create("real",2)
	priceBoundsR=asarray_create("real",2)
	problemNoter=J(rows(statIdLong),draws*6+1,0)
end

mata:
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
				sharesHat=asarray(sharesBcs,(idp,d,1))
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
				UvmodObsLongp=panelsubmatrix(UvmodObsLong,i,mLong)
		
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
					
					//if (any(othercPlayer:<0) | any(othercPlayer:>1)) printf("what the...")

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
			//		if (hasmissing(pricesTry)) printf("what the!")
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



