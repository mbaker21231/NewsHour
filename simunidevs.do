mata: 
    for (c=1;c<=draws;c++) {

        for (i=1;i<=rows(mLong);i++) {
            gameMarkerp=panelsubmatrix(gameMarker,i,mLong)
            playersp=colsum(gameMarkerp)
            if (playersp>0) {
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
                
                for (k=1;k<=rows(nsToChange);k++) {
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
                }
            }
        }
        counter=counter+timeslots
    }
end
end