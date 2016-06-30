/* Edits - getting the game straight */
mata:
    NashProfiles=asarray_create("real", 4)
    counter=1
end

mata:
    for (d=1;d<=draws;d++) {
        for (i=1;i<=rows(mLong);i++) {
            gameMarkerp=panelsubmatrix(gameMarker,i,mLong)
            playersp=colsum(gameMarkerp)
			i,d
            if (playersp>0) {
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
				lnppsLongp = panelsubmatrix(lnppsLong, i, mLong)

                neq=1

                asarray(NashProfiles,(i,d,1,neq),lnewsLongp)
                asarray(NashProfiles,(i,d,2,neq),otherlLongp)
                asarray(NashProfiles,(i,d,3,neq),nnewsLongp)
                EVdraws=J(rows(UvmodObsLongp),timeslots,0)
	            EPdraws=J(rows(UvmodObsLongp),timeslots,0)
	
                for (z=1; z<=playersp;z++) {
		            EVdraws[posofGamers[z],] = asarray(viewErrs, (Gamers[z],d))
                    EPdraws[posofGamers[z],] = asarray(priceErrs,(Gamers[z],d))
	            }

                for (s=1;s<=25;s++) {
                    phlnewsLongp  = lnewsLongp
                    phnnewsLongp  = nnewsLongp
                    photherlLongp = otherlLongp
                    
                    lnewsHat=lnewsLongp
                    nnewsHat=nnewsLongp
                    otherlHat=otherlLongp
					
					/* Initial perturbation */

                    for (pp=1;pp<=playersp;pp++) {
					
                        targetPlayer=Gamers[pp]
     					lnewsPlayer=asarray(Bcs,(targetPlayer,1))
                        otherlPlayer=asarray(Bcs,(targetPlayer,2))
                        nnewsPlayer=asarray(Bcs,(targetPlayer,3))
                        targetPos=posofGamers[pp]
					
      			    	selector = rowsum(lnewsLongp[targetPos,]:==lnewsPlayer) :>= 0
		    		    lnewsPlayer = select(lnewsPlayer, selector)
			    	    otherlPlayer = select(otherlPlayer, selector)
				        nnewsPlayer  = select(nnewsPlayer, selector)
  
                        newStrat=round(1+(rows(lnewsPlayer)-1)*runiform(1,1))
  
	     				lnewsHat[targetPos,]=lnewsPlayer[newStrat,]
                        nnewsHat[targetPos,]=nnewsPlayer[newStrat,]
                        otherlHat[targetPos,]=otherlPlayer[newStrat,]
                   } 
				
				   currentvals = J(playersp,1,0)
				   tries = 1
                   maxtries = 20
                   
				   do {
                       lnewsOrig = lnewsHat
                       for (pp=1;pp<=playersp;pp++) {
    	                   targetPlayer=Gamers[pp]
                           lnewsPlayer=asarray(Bcs,(targetPlayer,1))
                           otherlPlayer=asarray(Bcs,(targetPlayer,2))
                           nnewsPlayer=asarray(Bcs,(targetPlayer,3))
                           targetPos=posofGamers[pp]
					
      			           selector = rowsum(lnewsLongp[targetPos,]:==lnewsPlayer) :>= 0
		    		       lnewsPlayer = select(lnewsPlayer, selector)
			    	       otherlPlayer = select(otherlPlayer, selector)
				           nnewsPlayer  = select(nnewsPlayer, selector)
							
						   for (g=1;g<=rows(lnewsPlayer);g++) {
                               lnewsOld = lnewsHat
						       nnewsOld = nnewsHat
						       otherlOld = otherlHat

							   lnewsHat[targetPos,]=lnewsPlayer[g,]
                               nnewsHat[targetPos,]=nnewsPlayer[g,]
                               otherlHat[targetPos,]=otherlPlayer[g,]
                    
					           errMarker=(lnewsHat:==phlnewsLongp):*(otherlHat:==photherlLongp)					
	              			   veToUse = UvmodObsLongp[,counter::counter + timeslots - 1]:*errMarker :+ EVdraws:*(1 :- errMarker)
					           peToUse = UpmodobsLongp[,counter::counter + timeslots - 1]:*errMarker :+ EPdraws:*(1 :- errMarker)
    
               	               lnewsLongLagp=J(rows(lnewsLongp),1,0)
                               nnewsLongLagp=J(rows(lnewsLongp),1,0)
                               otherlLongLagp=J(rows(lnewsLongp),1,0)
                               siLagp=J(rows(lnewsLongp),1,0)
                               totlnewsp=J(rows(lnewsLongp),1,0)
                               totnnewsp=J(rows(nnewsLongp),1,0)
    
                               lnewsnLongp=lnewsHat:*ln(1:+colsum(lnewsHat))
                               otherlnLongp=otherlHat:*ln(1:+colsum(otherlHat))
                               nnewsnLongp=nnewsHat:*ln(1:+colsum(nnewsHat))
                               othercnLongp=othercLongp:*ln(1:+colsum(othercLongp))

              				   simShares = J(rows(lnewsLongp),timeslots,.)
                               for (t=1;t<=timeslots ;t++) {
                                   if (t!=1) siLagp=ln(siLagp)
                                   XV=lnewsHat[,t],otherlHat[,t],nnewsHat[,t],
                                       lnewsLongLagp:*lnewsHat[,t],
                                       nnewsLongLagp:*lnewsHat[,t],
                                       lnewsLongLagp:*nnewsHat[,t],
                                       nnewsLongLagp:*nnewsHat[,t],
                                       siLagp,
                                       siLagp:*lnewsLongLagp:*lnewsHat[,t],
                                       siLagp:*nnewsLongLagp:*lnewsHat[,t],
                                       siLagp:*lnewsLongLagp:*nnewsHat[,t],
                                       siLagp:*nnewsLongLagp:*nnewsHat[,t],
                                       lnewsHat[,t]:*ln(1:+totlnewsp),nnewsHat[,t]:*ln(1:+totnnewsp),
                                       l_ACS_HHLongp[,t],lnewsnLongp[,t],otherlnLongp[,t],
                                       nnewsnLongp[,t],othercnLongp[,t],J(rows(lnewsHat),1,1)
 
                                   XBV=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                       UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]:+veToUse[,t]

                                   sharesP=esharesStable(XBV,lnewsHat[,t],otherlHat[,t],nnewsHat[,t],
                                       othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
                                   simShares[,t] = sharesP
							
					               siLagp=sharesP

                                   totlnewsp=totlnewsp:+J(rows(lnewsHat),1,colsum(lnewsHat[,t]:*sharesP))
                                   totnnewsp=totnnewsp:+J(rows(nnewsHat),1,colsum(nnewsHat[,t]:*sharesP))
                                   lnewsLongLagp=lnewsHat[,t]
                                   nnewsLongLagp=nnewsHat[,t]
                                   otherlLongLagp=otherlHat[,t]
                               }
						
					           simPrice = (lnewsHat[posofGamers[pp],]*bpo[1]:+otherlHat[posofGamers[pp],]*bpo[2]:+
					               nnewsHat[posofGamers[pp],]*bpo[3]):*ln(popp:*simShares[posofGamers[pp],]):+
                                   lnewsHat[posofGamers[pp],]:*bpo[4]:+otherlHat[posofGamers[pp],]*bpo[5]:+
					               bpo[6]:*l_ACS_HHLongp[posofGamers[pp],]:+bpo[10]:+
                                   UpsLongp[posofGamers[pp], counter::counter+timeslots-1]:+
					               UpmtLongp[posofGamers[pp], counter::counter+timeslots-1] :+
			                       peToUse[posofGamers[pp],]
					
					           if (rowsum(exp(simPrice)) < currentvals[pp]) {
	 				               lnewsHat = lnewsOld
	 				               nnewsHat  = nnewsOld
						           otherlHat = otherlOld
					           }
							   else {
									 currentvals[pp] = rowsum(exp(simPrice))
							   }
						   }
					   }
					   tries++
				   } while ( (lnewsHat != lnewsOrig) & (tries < maxtries ))
				       if (lnewsHat != phlnewsLongp) {
                           neq++  
                           asarray(NashProfiles,(i,d,1,neq),lnewsHat)
                           asarray(NashProfiles,(i,d,2,neq),otherlHat)
                           asarray(NashProfiles,(i,d,3,neq),nnewsHat)
				       }
				   }
     			   lnewsLongp  = phlnewsLongp
				   nnewsLongp  = nnewsLongp
				   otherlLongp = photherlLongp			
                }	
			}		
        }
		counter = counter + timeslots
	}				
					
    mata matsave NashStuff NashProfiles, replace					
					
					
					
					

					
					
					

end
