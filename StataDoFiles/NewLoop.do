mata: counter = 1

/*for (c=1;c<=draws;c++) {*/
mata:

	 c=1
	/*for (i=1;i<=rows(mLong);i++) {*/
		i=1
		gameMarkerp = panelsubmatrix(gameMarker,i,mLong)
		playersp = colsum(gameMarkerp)
		
/*		if (playersp>0) { */
		
			statIdLongp=panelsubmatrix(statIdLong,i,mLong)
            lnewsLongp=panelsubmatrix(lnewsLong,i,mLong)
            nnewsLongp=panelsubmatrix(nnewsLong,i,mLong)
            otherlLongp=panelsubmatrix(otherlLong,i,mLong)
            othercLongp=panelsubmatrix(othercLong,i,mLong)
			lnppsLongp=panelsubmatrix(lnppsLong,i,mLong)
			marketIdLongp = panelsubmatrix(marketIdLong,i,mLong)
			
            nsToChange=select(statIdLongp,gameMarkerp)
			
            UvmtLongp=panelsubmatrix(UvmtLong,i,mLong)
            UvmodLongp=panelsubmatrix(UvmodLong,i,mLong)
            UvsLongp=panelsubmatrix(UvsLong,i,mLong)
			UpsLongp=panelsubmatrix(UpsLong,i,mLong)
			UpmtLongp=panelsubmatrix(UpmtLong,i,mLong)
			UpmodobsLongp=panelsubmatrix(UpmodObs,i,mLong)
            
			XBVngLongp=panelsubmatrix(XBVngLong,i,mLong)
            UvmodObsLongp=panelsubmatrix(UvmodObsLong,i,mLong)
            l_ACS_HHLongp=panelsubmatrix(l_ACS_HHLong,i,mLong)	
			popp = round(exp(max(l_ACS_HHLongp)))
			
			gameList = J(0,2,.)
end			
/*			for (k=1;k<=rows(nsToChange);k++) { */

mata:
				k=1

				pId = nsToChange[k]
				gameList = gameList \ (pId,i)
	            place=mm_which(statIdLongp:==pId)
                lnewsHat=asarray(Bcs,(pId,1))
                otherlHat=asarray(Bcs,(pId,2))
                nnewsHat=asarray(Bcs,(pId,3))
                
				lnewsAct=lnewsLongp[place,]
                otherlAct=otherlLongp[place,]
				nnewsAct=nnewsLong[place,]
                
				XBVplaceHold=J(rows(lnewsHat),cols(lnewsHat),.)
                XBVplaceHoldMean=J(rows(lnewsHat),cols(lnewsHat),.)
				
			    errMarker=(lnewsAct:==lnewsHat):*(otherlAct:==otherlHat)	
                
				errPdraws=J(1,timeslots,0)
				errVdraws=J(1,timeslots,0)
				pBound=J(1,timeslots,.)
				vBound=J(1,timeslots,.)
				vBound[timeslots] = 1000                  /* effectively unbounded at the end */
				
				errVdraws[1,timeslots]=sdmarv*rnormal(1,1,0,1) /* First error draw we are okay with being unbounded (hence bound of 1000) */

                p=mm_which(((rowsum(errMarker[,1::5])):==5):*(errMarker[,timeslots]:==0))  /* Picks out the sole strategy that deviates at the end. */
				
				shareHat = loopShareGenerator(place, p, timeslots)
				
	            actPayMean=(lnewsHat[p,]*bpo[1]:+otherlHat[p,]*bpo[2]:+nnewsHat[p,]*bpo[3]):*ln(popp:*shareHat):+
                    lnewsHat[p,]:*bpo[4]:+otherlHat[p,]*bpo[5]:+bpo[6]:*l_ACS_HHLongp[place,]:+bpo[10]:+
                    UpsLongp[place,counter::counter+timeslots-1]:+UpmtLongp[place,counter::counter+timeslots-1]

				pBound[timeslots] = lnppsLongp[place,timeslots]-actPayMean[timeslots]
				
                errPdraws[,timeslots]=exp(bpo[9])*invnormal(runiform(1,1)*normal(pBound[timeslots]/exp(bpo[9])))			
				
end                    			

/* We now have our first draws...to get our second draw, we now have to work backwards through the loop. */
/* We now have our first two sets of error terms. */
/* The function PriceShareGenerator returns price and viewership using ACTUAL (observed) errors up to some cutpoint */
/* What we want to do is locate the UPPER BOUND on Viewership implied by the model. Let's try this... */





mata:








end

	
