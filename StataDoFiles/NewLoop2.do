/* Since the error drawer seems to be working, we now start a new file where we start to do some looping */

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
				vBound[timeslots] = 6.5                  /* effectively unbounded at the end */
				
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

/* Now, we back up one time period and see if we can find an upper bound on the counterfactual viewership ERROR TERM */


/* Start with a very high error term and see if it is too high */

mata:
				for (t=timeslots-1;t>=1;t--) {
					vBound[t] = 10
					errVdraws[t] = vBound[t]
					PriceShareGenerator(t, 0, 1, 58, S=., P=.)
					check = rowsum(exp(lnppsLongp[place,t::timeslots]))-max(rowsum(exp(P[,t+1::timeslots])))

					if (check < 0) {
						Up   = 10
						Down = -10
						Dist = (Up - Down) / 2
						XX   = (Up + Down) / 2
						its = 0
						do {
							Dist = Dist / 2
							errVdraws[t]= XX					
							PriceShareGenerator(t, 0, 1, 58, S=., P=.)
							check = rowsum(exp(lnppsLongp[place,t::timeslots]))-max(rowsum(exp(P[,t+1::timeslots])))				
							if (check < 0) {
								XX = XX - Dist
							}
							else {
								XX = XX + Dist
							}
							its++
						} while (abs(check)>.01 & its < 20)
					vBound[t] = XX
					}
					errVdraws[t] = sdmodv*invnormal(normal(vBound[t])*runiform(1,1))
					PriceShareGenerator(t, 0, 1, place, S=., P=.)
					pBound[t] = rowsum(exp(lnppsLongp[place,t::timeslots]))-max(rowsum(exp(P[,t::timeslots])))
					errPdraws[t] = sdmodp*invnormal(normal(pBound[t])*runiform(1,1))
				}
end

/* Once out of the loop, we can now draw values for the viewership error term for the truncated normal: */
mata:

end

				


	
