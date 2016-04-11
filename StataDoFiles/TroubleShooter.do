/* Run through line 194 */
/* Very first hypothetical shares... */

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
	
mata:
			q=1
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
