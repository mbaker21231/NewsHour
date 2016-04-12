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
			
            nsToChange=select(statIdLongp,gameMarkerp)
			
            UvmtLongp=panelsubmatrix(UvmtLong,i,mLong)
            UvmodLongp=panelsubmatrix(UvmodLong,i,mLong)
            UvsLongp=panelsubmatrix(UvsLong,i,mLong)
			UpsLongp=panelsubmatrix(UpsLong,i,mLong)
			UpmtLongp=panelsubmatrix(UpmtLong,i,mLong)
            
			XBVngLongp=panelsubmatrix(XBVngLong,i,mLong)
            UvmodObsLongp=panelsubmatrix(UvmodObsLong,i,mLong)
            l_ACS_HHLongp=panelsubmatrix(l_ACS_HHLong,i,mLong)	
			popp = round(exp(max(l_ACS_HHLongp)))
			
/*			for (k=1;k<=rows(nsToChange);k++) { */
				k=1
				pId = nsToChange[k]
				gameList = gameList \ (pId,i)
	            place=mm_which(statIdLongp:==pId)
                lnewsHat=asarray(Bcs,(pId,1))
                otherlHat=asarray(Bcs,(pId,2))
                nnewsHat=asarray(Bcs,(pId,3))
                lnewsAct=lnewsLongp[place,]
                otherAct=otherlLongp[place,]
				nnewsAct=nnewsLong[place,]
                
				XBVplaceHold=J(rows(lnewsHat),cols(lnewsHat),.)
                XBVplaceHoldMean=J(rows(lnewsHat),cols(lnewsHat),.)
				
			    errMarker=(lnewsAct:==lnewsHat):*(otherlAct:==otherlHat)	
                
				errPdraws=J(1,6,0)
				errVdraws=J(1,6,0)
				pBound=J(1,6,.)
				vBound=J(1,6,.)
				
				errVdraws[1,timeslots]=sdmarv*rnormal(1,1,0,1)

                p=mm_which(((rowsum(errMarker[,1::5])):==5):*(errMarker[,timeslots]:==0))		
                    			
				/* Make a share */				
	
	            lnewsLongp[place,]=lnewsHat[p,]
                otherlLongp[place,]=otherlHat[p,]
                nnewsLongp[place,]=nnewsHat[p,]
                lnewsLongLagp=J(rows(lnewsLongp),1,0)
                nnewsLongLagp=J(rows(lnewsLongp),1,0)
                otherlLongLagp=J(rows(lnewsLongp),1,0)
                siLagp=J(rows(lnewsLongp),1,0)
                totlnewsp=J(rows(lnewsLongp),1,0)
                totnnewsp=J(rows(nnewsLongp),1,0)
    
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

                    XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                        UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
                    if (t==timeslots) {					
						XBV[place,]=XV[place,]*betaDynoStart':+UvmtLongp[place,counter+t-1]:+
							UvsLongp[place,counter+t-1]:+UvmodLongp[place,counter+t-1]:+errVdraws[1,6]
					}

                    sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
						othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
                    siLagp=sharesP
                    totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                    totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                    lnewsLongLagp=lnewsLongp[,t]
                    nnewsLongLagp=nnewsLongp[,t]
                    otherlLongLagp=otherlLongp[,t]
               }							
	
				shareHat = sharesP[place]
								
                actPayMean=(lnewsHat[p,]*bpo[1]:+otherlHat[p,]*bpo[2]:+nnewsHat[p,]*bpo[3]):*ln(popp:*shareHat):+
                    lnewsHat[p,]:*bpo[4]:+otherlHat[p,]*bpo[5]:+bpo[6]:*l_ACS_HHLongp[place,]:+bpo[10]:+
                    UpsLongp[place,counter::counter+timeslots-1]:+UpmtLongp[place,counter::counter+timeslots-1]								
				
				pBound[timeslots]=lnppsLongp[place,timeslots]-actPayMean[timeslots]
	            errPdraws[,timeslots]=exp(bpo[9])*invnormal(runiform(1,1)*normal(pBound[timeslots]/exp(bpo[9])))
	
				/* Major loop to compute shares, draws errors, etc. etc. */
				/* We still have to fill in Ackerberg sampling weights   */
				
/*				for t=timeslots-1;t>=1;t--) { */
					t=5
					lnewsLongp[place,]=lnewsHat[p,]
					otherlLongp[place,]=otherlHat[p,]
					nnewsLongp[place,]=nnewsHat[p,]
					lnewsLongLagp=J(rows(lnewsLongp),1,0)
					nnewsLongLagp=J(rows(lnewsLongp),1,0)
					otherlLongLagp=J(rows(lnewsLongp),1,0)
					siLagp=J(rows(lnewsLongp),1,0)
					totlnewsp=J(rows(lnewsLongp),1,0)
					totnnewsp=J(rows(nnewsLongp),1,0)
    
					lnewsnLongp=lnewsLongp:*ln(1:+colsum(lnewsLongp))
					otherlnLongp=otherlLongp:*ln(1:+colsum(otherlLongp))
					nnewsnLongp=nnewsLongp:*ln(1:+colsum(nnewsLongp))
					othercnLongp=othercLongp:*ln(1:+colsum(othercLongp))				

                    p=mm_which((rowsum(errMarker[,1::t-1]):==t-1):*(errMarker[,t]:==0))
					errV=0
				    for (tt=1;tt<=t;tt++) {
						if (tt!=1) siLagp=ln(siLagp)
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
	
						XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
							UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
						
						if (tt==t) {					
							XBV[place,]=XV[place,]*betaDynoStart':+UvmtLongp[place,counter+t-1]:+
								UvsLongp[place,counter+t-1]:+UvmodLongp[place,counter+t-1]:+errV
						}
						
						sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
							othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
						siLagp=sharesP
						totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
						totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
						lnewsLongLagp=lnewsLongp[,t]
						nnewsLongLagp=nnewsLongp[,t]
						otherlLongLagp=otherlLongp[,t]
					}											  
											
				
				
				
				
				
				
				
				
				
				
				
end				
				
								
