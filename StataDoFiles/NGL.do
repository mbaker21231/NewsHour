mata:
	d = 1
	i = 1
end


mata: 
	errVdraws = J(rows(UvmodObsLongp),timeslots, 0)
	errPdraws = J(rows(UpmodobsLongp),timeslots, 0)
	
	for (i=1;i<=rows(Gamers);i++) {
		pId = statIdLong[posofGamers[i]]
		errVAdd = asarray(viewErrs, (pId, d))
		errPAdd = asarray(priceErrs, (pId, d))
		errVdraws[posofGamers[i],] = errVAdd
		errPdraws[posofGamers[i],] = errPAdd
	}
end

mata:
	
	gameMarkerp=panelsubmatrix(gameMarker,i,mLong)
    playersp=colsum(gameMarkerp)
	
	statIdLongp=panelsubmatrix(statIdLong,i,mLong)   
    Gamers=select(statIdLongp,gameMarkerp)           
    posofGamers=mm_which(gameMarkerp)                

    UvmtLongp=panelsubmatrix(UvmtLong,i,mLong)
    UvmodLongp=panelsubmatrix(UvmodLong,i,mLong)
    UvsLongp=panelsubmatrix(UvsLong,i,mLong)
    XBVngLongp=panelsubmatrix(XBVngLong,i,mLong)
    UvmodObsLongp=panelsubmatrix(UvmodObsLong,i,mLong)

    UpmodobsLongp=panelsubmatrix(UpmodObs,i,mLong)
    UpsLongp=panelsubmatrix(UpsLong,i,mLong)
    UpmtLongp=panelsubmatrix(UpmtLong,i,mLong)

    lnewsLongp=panelsubmatrix(lnewsLong,i,mLong)
    otherlLongp=panelsubmatrix(otherlLong,i,mLong)
    nnewsLongp=panelsubmatrix(nnewsLong,i,mLong)
    othercLongp=panelsubmatrix(othercLong,i,mLong)
    popLongp=panelsubmatrix(popLong,i,mLong)
    l_ACS_HHLongp=panelsubmatrix(l_ACS_HHLong,i,mLong)
    popp = round(exp(max(l_ACS_HHLongp)))                

    neq=1
    asarray(NashProfiles,(i,d,1,neq),lnewsOld)
    asarray(NashProfiles,(i,d,2,neq),otherlOld)
    asarray(NashProfiles,(i,d,3,neq),nnewsOld)	

end
	
/* At the beginning of the s loop - start by putting strategies back where they were */

mata:
    otherlOld=otherlLongp
    lnewsOld=lnewsLongp
    nnewsOld=nnewsLongp
    othercOld=othercLongp
end

/* Propose a change */
mata:

     targetPlayerN=round(1+(playersp-1)*runiform(1,1))
     targetPlayer=Gamers[targetPlayerN]
                    
     lnewsPlayer=asarray(Bcs,(targetPlayer,1))
     otherlPlayer=asarray(Bcs,(targetPlayer,2))
     nnewsPlayer=asarray(Bcs,(targetPlayer,3))
                    
     newStrat=round(1+(rows(lnewsPlayer)-1)*runiform(1,1))

	 targetPos=posofGamers[targetPlayerN]
     
	 lnewsHat=lnewsOld
     nnewsHat=nnewsOld
     otherlHat=otherlOld
     othercHat=othercOld

	 /* lnewsHat has been perturbed */
	 
     lnewsHat[targetPos,]=lnewsPlayer[newStrat,]
     nnewsHat[targetPos,]=nnewsPlayer[newStrat,]
     otherlHat[targetPos,]=otherlPlayer[newStrat,]

     errMarker=(lnewsLongp:==lnewsHat):*(otherlLongp:==otherlHat)
	 
	 errsPTU = errMarker:*UpmodobsLongp[,counter::counter+timeslots-1] :+ (1:-errMarker):*errPdraws
     errsVTU = errMarker:*UvmodObsLongp[,counter::counter+timeslots-1] :+ (1:-errMarker):*errVdraws
	 
	 
	 
end
