/* Since the error drawer seems to be working, we now start a new file where we start to do some looping */

mata: counter = 1
set seed 123

mata: 
    priceErrs=asarray_create("real",2)
    priceBounds=asarray_create("real",2)
	viewErrs=asarray_create("real",2)
	viewBounds=asarray_create("real",2)

mata:
for (c=1;c<=draws;c++) {
	for (i=1;i<=rows(mLong);i++) {
		gameMarkerp = panelsubmatrix(gameMarker,i,mLong)
		playersp = colsum(gameMarkerp)
		if (playersp>0) { 
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
			
			for (k=1;k<=rows(nsToChange);k++) { 		
			c,i,k
				pId = nsToChange[k]
				gameList = gameList \ (pId,i)
	            place=mm_which(statIdLongp:==pId)
                lnewsHat=asarray(Bcs,(pId,1))
                otherlHat=asarray(Bcs,(pId,2))
                nnewsHat=asarray(Bcs,(pId,3))
                
				lnewsAct=lnewsLongp[place,]
                otherlAct=otherlLongp[place,]
				nnewsAct=nnewsLongp[place,]
                
				XBVplaceHold=J(rows(lnewsHat),cols(lnewsHat),.)
                XBVplaceHoldMean=J(rows(lnewsHat),cols(lnewsHat),.)
				
			    errMarker=(lnewsAct:==lnewsHat):*(otherlAct:==otherlHat)	
                
				errPdraws=J(1,timeslots,0)
				errVdraws=J(1,timeslots,0)
				pBound=J(1,timeslots,.)
				vBound=J(1,timeslots,.)
				vBound[timeslots] = 6.5                  /* effectively unbounded at the end */
	
				do {
				
					errVdraws[1,timeslots]=sdmodv*rnormal(1,1,0,1) /* First error draw we are okay with being unbounded (hence bound of 1000) */

					p=mm_which(((rowsum(errMarker[,1::5])):==5):*(errMarker[,timeslots]:==0))  /* Picks out the sole strategy that deviates at the end. */
				
					shareHat = loopShareGenerator(place, p, timeslots)
				
					actPayMean=(lnewsHat[p,]*bpo[1]:+otherlHat[p,]*bpo[2]:+nnewsHat[p,]*bpo[3]):*ln(popp:*shareHat):+
						lnewsHat[p,]:*bpo[4]:+otherlHat[p,]*bpo[5]:+bpo[6]:*l_ACS_HHLongp[place,]:+bpo[10]:+
						UpsLongp[place,counter::counter+timeslots-1]:+UpmtLongp[place,counter::counter+timeslots-1]

					pBound[timeslots] = lnppsLongp[place,timeslots]-actPayMean[timeslots]
				
					errPdraws[,timeslots]=exp(bpo[9])*invnormal(runiform(1,1)*normal(pBound[timeslots]/exp(bpo[9])))			

					problemflag=0
					for (t=timeslots-1;t>=1;t--) {
						vBound[t] = 10
						pBound[t] = 10
						errVdraws[t] = vBound[t]
						PriceShareGenerator(t, 0, 1, place, S=., P=.)
						check = rowsum(exp(lnppsLongp[place,t::timeslots]))-max(rowsum(exp(P[,t::timeslots])))

						if (check < 0) {
							Up   = 10
							Down = -10
							Dist = (Up - Down) / 2
							XX   = (Up + Down) / 2
							its = 0
							do {
								Dist = Dist / 2
								errVdraws[t]= XX					
								PriceShareGenerator(t, 0, 1, place, S=., P=.)
								check = rowsum(exp(lnppsLongp[place,t::timeslots]))-max(rowsum(exp(P[,t::timeslots])))				
								if (check < 0) {
									XX = XX - Dist
								}
								else {
									XX = XX + Dist
								}
								its++
							} while (abs(check)>.01 & its < 40)
						vBound[t] = XX
						if (check<0) vBound[t] = vBound[t]-1.1
						}
						errVdraws[t] = sdmodv*invnormal(normal(vBound[t]/sdmodv)*runiform(1,1))
						vBound[t] = 10
						pBound[t] = 10
						errVdraws[t] = vBound[t]
						PriceShareGenerator(t, 0, 1, place, S=., P=.)
						check = rowsum(exp(lnppsLongp[place,t::timeslots]))-max(rowsum(exp(P[,t::timeslots])))

						if (check < 0) {
							Up   = 10
							Down = -10
							Dist = (Up - Down) / 2
							XX   = (Up + Down) / 2
							its = 0
							do {
								Dist = Dist / 2
								errVdraws[t]= XX					
								PriceShareGenerator(t, 0, 1, place, S=., P=.)
								check = rowsum(exp(lnppsLongp[place,t::timeslots]))-max(rowsum(exp(P[,t::timeslots])))				
								if (check < 0) {
									XX = XX - Dist
								}
								else {
									XX = XX + Dist
								}
								its++
							} while (abs(check)>.01 & its < 40)
						vBound[t] = XX
						if (check<0) vBound[t] = vBound[t]-1.1
						}
						errVdraws[t] = sdmodv*invnormal(normal(vBound[t]/sdmodv)*runiform(1,1))
						errPdraws[t] = sdmodp*invnormal(normal(pBound[t]/sdmodp)*runiform(1,1))
					}
					checkerator = (nnewsAct:!=1):*(pBound:==.)
					problemflag = rowsum(checkerator)>0
					if (problemflag) {
						printf("redrawing");displayflush()
					}
				} while (problemflag==1)
			asarray(priceErrs,(statIdLong[i],c),errPdraws)
            asarray(priceBounds,(statIdLong[i],c),pBound)
			asarray(viewErrs,(statIdLong[i],c),errVdraws)
			asarray(viewBounds,(statIdLong[i],c),vBound)
			}
		}
	}
	counter = counter + timeslots
}
end

				


	
