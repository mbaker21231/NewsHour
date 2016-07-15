mata:
    real scalar loopShareGenerator(place, p, T) {

        external counter
        external lnewsLongp, otherlLongp, nnewsLongp, othercLongp
        external lnewsHat, otherlHat, nnewsHat
        external l_ACS_HHLongp
        external UvsLongp, UvmodObsLongp,UvmtLongp,UvmodLongp,errVdraws
        external betaDynoStart, bo

        phlnewsLongp = lnewsLongp
        phnnewsLongp = nnewsLongp
        photherlLongp = otherlLongp

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

        for (t=1;t<=T;t++) {
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
            if (t==T) {
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
        
        lnewsLongp = phlnewsLongp
        otherlLongp = photherlLongp
        nnewsLongp = phnnewsLongp

        return(sharesP[place])
}
end
end