/* Run through line 194 */
/* Very first hypothetical shares... */
/* First market is 67*6 stations */

keep in 1/402 

mata: 
	c=1
	i=1
	
	gameMarkerp=panelsubmatrix(gameMarker,i,mLong)
    playersp=colsum(gameMarkerp)	
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
        UvmodObsLongp=panelsubmatrix(UvmodObsLong,i,mLong)
        l_ACS_HHLongp=panelsubmatrix(l_ACS_HHLong,i,mLong)	
end

mata:
		k=1
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
end			
	
	
	/* q = 7 is the actual strategy followed by the station...*/
	
mata:
			q=7
				lnewsLongp[place,]=lnewsHat[q,]
                otherlLongp[place,]=otherlHat[q,]
                nnewsLongp[place,]=nnewsHat[q,]
                lnewsLongLagp=J(rows(lnewsLongp),1,0)
                nnewsLongLagp=J(rows(lnewsLongp),1,0)
                otherlLongLagp=J(rows(lnewsLongp),1,0)
                siLagp=J(rows(lnewsLongp),1,0)
                totlnewsp=J(rows(lnewsLongp),1,0)
                totnnewsp=J(rows(nnewsLongp),1,0)
  
/* It's possible that you can't compute these like this... */  
/* And that it must be redone for every profile - which seems likely to be true */

                lnewsnLongp=lnewsLongp:*ln(1:+colsum(lnewsLongp))
                otherlnLongp=otherlLongp:*ln(1:+colsum(otherlLongp))
                nnewsnLongp=nnewsLongp:*ln(1:+colsum(nnewsLongp))
                othercnLongp=othercLongp:*ln(1:+colsum(othercLongp))		
end	

/****FIRST ITERATION OF T BLOCK *****////
			
mata:
				t=1
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end


mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]



end

/* SECOND ITERATION OF T BLOCK ***/



mata:
				t=2
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end
mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]
end


/* Third iteration ****/

mata:
				t=3
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end
mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]
end


/* FOURTH ITERATION ****/

mata:
				t=4
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end
mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]
end


/* FIFTH ITERATION **/

mata:
				t=5
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end
mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]
end

/* SIXTH ITERATION **/

mata:
				t=6
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end
mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]
end


/* LESSON...IT APPEARS THAT EVERYTHING IS WORKING HERE - at least when we are using actual data. */
/* NOW, WHAT IF WE CHANGE a q? For the first player in the first, game, strategy 11 is identical to
   what the player actually does, but with a different strategy in the first position...a test */
   

   
   
   
   
/* Q = 11 test */


   
   
   
   
mata:
			q=11
				lnewsLongp[place,]=lnewsHat[q,]
                otherlLongp[place,]=otherlHat[q,]
                nnewsLongp[place,]=nnewsHat[q,]
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
end	

/****FIRST ITERATION OF T BLOCK *****////
			
mata:
				t=1
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end


mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]



end

/* After the first iteration, there is almost NO deviation between predicted and actual shares. */
/* the predicted share is much lower than the actual - that seems plausible. What next? */


/* SECOND ITERATION OF T BLOCK ***/


mata:
				t=2
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end
mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]
end


/* At this point, our man is starting to drift a little bit...what gives? */


/* Third iteration ****/

mata:
				t=3
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end
mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]
end



/* Even more drift...why? */ 
/* FOURTH ITERATION ****/

mata:
				t=4
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end
mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]
end

/* SO far, everything seems very plausible */

/* FIFTH ITERATION **/

mata:
				t=5
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end
mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]
end

/* We are still good...no problem at all really and all results seem to be in the plausible range.  */

/* SIXTH ITERATION **/

mata:
				t=6
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
end			

mata:
                                XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
end					
mata:
                            sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                                othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
end
mata:
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

                            siLagp=sharesP
                            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
                            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
                            lnewsLongLagp=lnewsLongp[,t]
                            nnewsLongLagp=nnewsLongp[,t]
                            otherlLongLagp=otherlLongp[,t]
end


/* What we've learned...the simulated shares really aren't all that bad in this case. */
/* Let's rerun everything and see what we get... */
/* here is an entire q-loop */


///**** Q LOOP REDUX *****////////////


mata:

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
	
                       

                    }

end
   

/* The lesson seems to be: for this particular case, the simulated shares seem to be okay.... what if we now run
   this for the second station? */
   
   
   
 /*** K==2 second station ***/
 mata:
				k=2
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
  
  end 
  
  /* All of this suggests to me that we are barking up the wrong tree - it seems as though simulated shares are
     pretty accurate - or at least shares from unilateral deviations */ 
	 /* what is going wrong, I don't know. */ 
