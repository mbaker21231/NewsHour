mata:    
	void PriceShareGenerator(real scalar cutT, cutFlagV, cutFlagP, real scalar place, S, P, allS) {
    
        external counter, timeslots, popp
        external lnewsLongp, otherlLongp, nnewsLongp, othercLongp
        external lnewsHat, otherlHat, nnewsHat
        external l_ACS_HHLongp
        external UvsLongp, UvmodObsLongp, UvmtLongp, UvmodLongp, 
            errVdraws, errPdraws, errMarker, UpmodobsLongp, UpmtLongp, UpsLongp
        external betaDynoStart, bo, bpo

        phlnewsLongp = lnewsLongp
        phnnewsLongp = nnewsLongp
        photherlLongp = otherlLongp
        phUvmodObsLongp = UvmodObsLongp
        phUpmodobsLongp = UpmodobsLongp

        shares = J(rows(lnewsLongp),0,.)

        UpmodObz=UpmodobsLongp[place,counter::counter+timeslots-1]
        UvmodObz=UvmodObsLongp[place,counter::counter+timeslots-1]

        if (cutT>1) {
            p=mm_which((rowsum(errMarker[,1::cutT-1]):==cutT-1):*(errMarker[,cutT]:==0))
            usePErrs=J(rows(p),timeslots,0)
            useVErrs=J(rows(p),timeslots,0)
     
            usePErrs[,1::cutT]=J(rows(p),1,UpmodObz[1,1::cutT])
            usePErrs[,cutT::cols(usePErrs)]=
                errMarker[p,cutT::cols(errPdraws)]:*UpmodObz[1,cutT::cols(errPdraws)]:+
                (1:-errMarker[p,cutT::cols(errPdraws)]):*errPdraws[,cutT::cols(errPdraws)]
                
            useVErrs[,1::cutT]=J(rows(p),1,UvmodObz[1,1::cutT])
            useVErrs[,cutT::cols(useVErrs)]=
                errMarker[p,cutT::cols(errVdraws)]:*UvmodObz[1,cutT::cols(errVdraws)]:+
                (1:-errMarker[p,cutT::cols(errVdraws)]):*errVdraws[,cutT::cols(errVdraws)]
        }
        else {
            p=mm_which(errMarker[,cutT]:==0)
            if (rows(p)==0) p=1::rows(errMarker)

            usePErrs=J(rows(p),timeslots,0)
            useVErrs=J(rows(p),timeslots,0)
            usePErrs[,cutT::cols(usePErrs)]=
                errMarker[p,cutT::cols(errPdraws)]:*UpmodObz[1,cutT::cols(errPdraws)]:+
                (1:-errMarker[p,cutT::cols(errPdraws)]):*errPdraws[,cutT::cols(errPdraws)]
                
            useVErrs[,cutT::cols(useVErrs)]=
                errMarker[p,cutT::cols(errVdraws)]:*UvmodObz[1,cutT::cols(errVdraws)]:+
                (1:-errMarker[p,cutT::cols(errVdraws)]):*errVdraws[,cutT::cols(errVdraws)]
       }

        if (cutFlagP == 1) usePErrs[,cutT]=J(rows(p),1,0)
        if (cutFlagV == 1) useVErrs[,cutT]=J(rows(p),1,0)

        Shares = J(rows(p),timeslots,0)

        for (k=1;k<=rows(p);k++) {
            UvmodObsLongp[place,counter::counter+timeslots-1] = useVErrs[k,]
            lnewsLongp[place,]=lnewsHat[p[k],]
            otherlLongp[place,]=otherlHat[p[k],]
            nnewsLongp[place,]=nnewsHat[p[k],]
        
            lnewsLongLagp=J(rows(lnewsLongp),1,0)
            nnewsLongLagp=J(rows(lnewsLongp),1,0)

            siLagp=J(rows(lnewsLongp),1,0)
            totlnewsp=J(rows(lnewsLongp),1,0)
            totnnewsp=J(rows(nnewsLongp),1,0)
    
            lnewsnLongp=lnewsLongp:*ln(1:+colsum(lnewsLongp))
            otherlnLongp=otherlLongp:*ln(1:+colsum(otherlLongp))
            nnewsnLongp=nnewsLongp:*ln(1:+colsum(nnewsLongp))
            othercnLongp=othercLongp:*ln(1:+colsum(othercLongp))	
        
            shares = J(rows(lnewsLongp),0,.)

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

                sharesP=esharesStable(XBV,lnewsLongp[,t],otherlLongp[,t],nnewsLongp[,t],
                    othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
                siLagp=sharesP
                shares = shares, sharesP

            totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsLongp[,t]:*sharesP))
            totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsLongp[,t]:*sharesP))
            lnewsLongLagp=lnewsLongp[,t]
            nnewsLongLagp=nnewsLongp[,t]
        }

            Shares[k,.] = shares[place,]

        }

        actPayMean=(lnewsHat[p,]*bpo[1]:+otherlHat[p,]*bpo[2]:+nnewsHat[p,]*bpo[3]):*ln(popp:*Shares):+
            lnewsHat[p,]:*bpo[4]:+otherlHat[p,]*bpo[5]:+bpo[6]:*l_ACS_HHLongp[place,]:+bpo[10]:+
            UpsLongp[place,counter::counter+timeslots-1]:+UpmtLongp[place,counter::counter+timeslots-1]
rows(actPayMean)
        lnewsLongp = phlnewsLongp
        otherlLongp = photherlLongp
        nnewsLongp = phnnewsLongp
        UvmodObsLongp = phUvmodObsLongp  
    UpmodobsLongp = phUpmodobsLongp 
        
        S = Shares
        
        P = actPayMean+usePErrs
        
        allS = shares
        
    }
end
