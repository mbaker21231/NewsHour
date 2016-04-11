capture cd "C:\Users\Matthew Baker\Documents\GitHub\NewsHour"
capture cd "C:\Users\mjbaker\Documents\GitHub\NewsHour"
clear all
use AveragedDataDyno.dta
set more off

capture gen game= (lnews | otherl) & lnpps!=.
sort market stationid mt timeslot

mata:
    st_view(lnews=.,.,"lnews")
    st_view(nnews=.,.,"nnews")
    st_view(otherl=.,.,"otherl")
    st_view(otherc=.,.,"otherc")
    st_view(si=.,.,"si")
    st_view(mt=.,.,"mt")
    st_view(id=.,.,"stationid")
    st_view(M=.,.,"market")
    st_view(time=.,.,"timeslot")
    st_view(game=.,.,"game")
    st_view(lnpps=.,.,"lnpps")
    st_view(pop=.,.,"ACS_HH") 
    st_view(l_ACS_HH=.,.,"l_ACS_HH")
    st_view(lnewsn=.,.,"lnewsn")
    st_view(otherln=.,.,"otherln")
    st_view(nnewsn=.,.,"nnewsn")
    st_view(othercn=.,.,"othercn")
    st_view(dln=.,.,"dln")

    lnviewn=ln(pop:*si)
    m=panelsetup(M,1)
end

mata:
    mata matuse DynoStarts23

    bo[,1::4]=bo[,1::4]:^2
    bo=bo[rows(bo),]


    betaDynoStart=bo[(5..23,27)]

    sdstav=exp(bo[24])
    sdmarv=exp(bo[25])
    sdmodv=exp(bo[26])
    
    draws=5
end

do MataFunctions\MataReshape.do

mata:
    swgLong=siLong:/(slnewsLong:*lnewsLong:+
        snnewsLong:*nnewsLong:+
        sotherlLong:*otherlLong:+
        sothercLong:*othercLong)
    
    lnswgLong=ln(swgLong)   

    dln=ln(siLong):-ln(1:-(slnewsLong:+snnewsLong:+sotherlLong:+sothercLong))    
    
    XBVngLong=dln-bo[1]*lnswgLong:*lnewsLong-bo[2]*lnswgLong:*otherlLong-
        bo[3]*lnswgLong:*nnewsLong-bo[4]*lnswgLong:*othercLong
                
    xBpUE=ln(siLong):-ln(1:-(slnewsLong:+snnewsLong:+sotherlLong:+sothercLong)):-
        bo[1]:*lnswgLong:*lnewsLong:-
        bo[2]:*lnswgLong:*otherlLong:-
        bo[3]:*lnswgLong:*nnewsLong:-
        bo[4]:*lnswgLong:*othercLong
                
    xBDyno=J(rows(lnewsLong),cols(lnewsLong),0)
    for (i=1;i<=cols(lnewsLong);i++) {
        xDyno=lnewsLong[,i],otherlLong[,i],nnewsLong[,i],
            lnewslnews[,i],lnewsnnews[,i],nnewslnews[,i],nnewsnnews[,i],
            siLongLag[,i],siXlnln[,i],siXlnnn[,i],siXnnln[,i],siXnnnn[,i],
            lnewsRT[,i],nnewsRT[,i],l_ACS_HHLong[,i],lnewsnLong[,i],otherlnLong[,i],
            nnewsnLong[,i],othercnLong[,i],J(rows(lnewsLong),1,1)
        xBDyno[,i]=xDyno*betaDynoStart'
    }              
end

mata: XBVngLong[1::10,]
mata: hasmissing(XBVngLong)

do MataFunctions\paneldraw.do

mata:
    iev1=paneldraw(id,draws)
    iev2=paneldraw(mt,draws)
    iev1=sdstav*invnormal(iev1)
    iev2=sdmarv*invnormal(iev2)

    UvsLong=J(rows(lnewsLong),0,0)
    UvmtLong=J(rows(lnewsLong),0,0)
    
    for (i=1;i<=draws;i++) {
        UvsLong=UvsLong,colshape(iev1[,i],timeslots)
        UvmtLong=UvmtLong,colshape(iev2[,i],timeslots)
    }

    UvmodLong=invnormal(runiform(rows(lnewsLong),timeslots*draws))*sdmodv:-2
end

mata:
    UvmodObsLong=xBpUE:-xBDyno
    UvmodObsLong=J(1,draws,1)#UvmodObsLong:-UvsLong:-UvmtLong
end

mata:
    mata matuse betaPDynoStarts, replace
    
    sdstap=exp(bpo[7])
    sdmarp=exp(bpo[8])
    sdmodp=exp(bpo[9])    
    
end

mata:
    iep1=paneldraw(id,draws)
    iep2=paneldraw(mt,draws)
    iep1=sdstap*invnormal(iep1)
    iep2=sdmarp*invnormal(iep2)	

    UpsLong=J(rows(lnewsLong),0,0)
    UpmtLong=J(rows(lnewsLong),0,0)
    
    for (i=1;i<=draws;i++) {
        UpsLong=UpsLong,colshape(iep1[,i],timeslots)
        UpmtLong=UpmtLong,colshape(iep2[,i],timeslots)
    }
    
    UpmodLong=invnormal(runiform(rows(lnewsLong),timeslots*draws))*sdmodp
end

mata:
    gameMarker = rowsum(gameLong):>0
    sum(gameMarker)
end

mata:
	lnewsSome = rowsum(lnewsLong):>0
    gameMarker = gameMarker:*lnewsSome
    gameLong   = gameLong:*gameMarker
    decCounter = rowsum(gameLong)
    sum(gameMarker)
end

mata:
    Bcs      =asarray_create("real", 2)
    sharesBcs=asarray_create("real", 3)
    uVinfo   =asarray_create("real", 3) 
end

do MataFunctions\stratmat.do

mata:
    gameIds=select(statIdLong,gameMarker)
    lnewsBcs=select(lnewsLong,gameMarker)
    nnewsBcs=select(nnewsLong,gameMarker)
    otherlBcs=select(otherlLong,gameMarker)
    totalDecs=select(decCounter,gameMarker)
    
    for (i=1;i<=rows(gameIds);i++) {
        if (totalDecs[i]==6) {
            strats=stratmat(6)'
            asarray(Bcs,(gameIds[i,1],1),strats)
            asarray(Bcs,(gameIds[i],2),1:-strats)
            asarray(Bcs,(gameIds[i],3),J(rows(strats),cols(strats),0))
        }
        if (totalDecs[i]==5) {
            nnewsPos=mm_which(nnewsBcs[i,1::6])
            stratsFree=stratmat(5)'
            if (nnewsPos==1) lnewsp=J(rows(stratsFree),1,0),stratsFree
            else if (nnewsPos==6) lnewsp=stratsFree,J(rows(stratsFree),1,0)
            else lnewsp=stratsFree[,1::nnewsPos-1],J(rows(stratsFree),1,0),stratsFree[,nnewsPos::5]
            otherlp=1:-lnewsp
            otherlp[,nnewsPos]=J(rows(otherlp),1,0)
            nnewsp=J(rows(lnewsp),6,0)
            nnewsp[,nnewsPos]=J(rows(lnewsp),1,1)
            asarray(Bcs,(gameIds[i],1),lnewsp)
            asarray(Bcs,(gameIds[i],2),otherlp)
            asarray(Bcs,(gameIds[i],3),nnewsp)
        }
    }
end

mata:
    mLong   = panelsetup(marketIdLong,1)
    counter = 1
    sniffTest = J(0,3,.)
end
do MataFunctions\sharemakers.do


mata: 
	BestJump = J(0,5,0)
    for (c=1;c<=draws;c++) {
		c
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
  						shoo = select(siLong,statIdLong:==pId)
							
						BestJump = BestJump \ (max(rowsum(sharesToPlace:-shoo)),pId,c,i,pId)
						
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


/* Big conclusion ----- simulated unilateral deviations seem to be working okay... */
/* Next part is the sampling weights for the viewership error terms. Here goes     */

mata:
    uv      = J(rows(statIdLong),0,.)
    uvg     = J(rows(statIdLong),0,.)
    uvsi    = J(rows(statIdLong),0,.)
    uvso    = J(rows(statIdLong),0,.)
    uvsg    = J(rows(statIdLong),0,.)
    uvre1   = J(rows(statIdLong),0,.)
    uvre2   = J(rows(statIdLong),0,.)
    uvre1g  = J(rows(statIdLong),0,.)
    uvre2g  = J(rows(statIdLong),0,.)
   
    looper  = uniqrows(asarray_keys(sharesBcs)[,1])
    counter = 1
end

do MataFunctions\lnnd.do


/* Why are we running this loop at this point? */

mata:
    for (c=1;c<=draws;c++) {
        ackToAdd=J(rows(statIdLong),timeslots,.)
        ackToAddg=J(rows(statIdLong),timeslots,.)
        ackToAddsi=J(rows(statIdLong),timeslots,.)
        ackToAddso=J(rows(statIdLong),timeslots,.)
        ackToAddsg=J(rows(statIdLong),timeslots,.)
        ackToAddre1=J(rows(statIdLong),timeslots,.)
        ackToAddre2=J(rows(statIdLong),timeslots,.)

        for (i=1;i<=rows(looper);i++) {
            statId=looper[i]
            lnewsHat=asarray(Bcs,(statId,1))
            otherlHat=asarray(Bcs,(statId,2))
            nnewsHat=asarray(Bcs,(statId,3))

            siHat=asarray(sharesBcs,(statId,c,1))
            soHat=asarray(sharesBcs,(statId,c,2))
            sgHat=asarray(sharesBcs,(statId,c,3))
            XBVs=asarray(Bcs,(statId,4))
            XBVsm=asarray(Bcs,(statId,5))
            idPos=mm_which(statId:==statIdLong)
            lnewsAct=lnewsLong[idPos,]
            otherlAct=otherlLong[idPos,]
            nnewsAct=nnewsLong[idPos,]
            Mpop=select(popLong,statIdLong:==statId)
            for (t=6;t>=2;t--) {
                if (nnewsAct[,t]!=1) {
                    sameButOneProfs=mm_which((rowsum(lnewsAct[,1::t-1]:==lnewsHat[,1::t-1]):==t-1):*
                        (lnewsAct[,t]:!=lnewsHat[,t]))
                    sameButOneProfs=min(sameButOneProfs)

                    ackToAddsi[idPos,t]=siHat[sameButOneProfs,t]	/* save the shares */
                    ackToAddso[idPos,t]=soHat[sameButOneProfs,t]	
                    ackToAddsg[idPos,t]=sgHat[sameButOneProfs,t]

                    ackToAdd[idPos,t]=XBVs[sameButOneProfs,t]	/* This is wrong! */
                    mu=lnewsHat[sameButOneProfs,t]*bo[1]+
                        otherlHat[sameButOneProfs,t]*bo[2]+
                        nnewsHat[sameButOneProfs,t]*bo[3]+
                        (1-lnewsHat[sameButOneProfs,t]-otherlHat[sameButOneProfs,t]-nnewsHat[sameButOneProfs,t])*bo[4]

                    ackToAddg[idPos,t]=lnnd(XBVs[sameButOneProfs,t],XBVsm[sameButOneProfs,t],bo[22])-ln(1/(1-mu)-
                        mu*siHat[sameButOneProfs,t]/sgHat[sameButOneProfs,t]-siHat[sameButOneProfs,t])
                }
                else {
                    ackToAdd[idPos,t]=0
                    ackToAddg[idPos,t]=0
                    ackToAddsi[idPos,t]=0
                    ackToAddso[idPos,t]=0
                    ackToAddsg[idPos,t]=0
                }
            }
   
            if (nnewsAct[1]!=1) {
                sameButOneProfs=mm_which(lnewsAct[1]:!=lnewsHat[,1])
                sameButOneProfs=min(sameButOneProfs)
                ackToAdd[idPos,1]=XBVs[sameButOneProfs,t]
                mu=lnewsHat[sameButOneProfs,1]*bo[1]+
                    otherlHat[sameButOneProfs,1]*bo[2]+
                    nnewsHat[sameButOneProfs,1]*bo[3]+
                    (1-lnewsHat[sameButOneProfs,1]-otherlHat[sameButOneProfs,1]-nnewsHat[sameButOneProfs,1])*bo[4]			

                ackToAddg[idPos,1]=lnnd(XBVs[sameButOneProfs,1],XBVsm[sameButOneProfs,1],bo[26])-ln(1/(1-mu)-
                    mu*siHat[sameButOneProfs,1]/sgHat[sameButOneProfs,1]-siHat[sameButOneProfs,1])
                ackToAddsi[idPos,1]=siHat[sameButOneProfs,1]
                ackToAddso[idPos,1]=soHat[sameButOneProfs,1]
                ackToAddsg[idPos,1]=sgHat[sameButOneProfs,1]
            }
            else {
                ackToAdd[idPos,1]=0
                ackToAddg[idPos,1]=0
                ackToAddsi[idPos,1]=0
                ackToAddso[idPos,1]=0
                ackToAddsg[idPos,1]=0
            }

            ackToAddre1[idPos,]=UvsLong[idPos,counter::counter+timeslots-1]
            ackToAddre2[idPos,]=UvmtLong[idPos,counter::counter+timeslots-1]
        }

        uv=uv,ackToAdd
        uvg=uvg,ackToAddg
        uvsi=uvsi,ackToAddsi
        uvso=uvso,ackToAddso
        uvsg=uvsg,ackToAddsg
        uvre1=uvre1,ackToAddre1
        uvre2=uvre2,ackToAddre2
        counter=counter+timeslots  
    }
end

/* the above are simply the sampling weights for the viewership model simulated terms. Some are large, but */
/* it doesn't seem like things are really all that bad there...Do we ever get missing values? */

mata:
    uvre1g=lnnd(uvre1,0,ln(sdstav)):*1:/rowsum(gameLong)    
    uvre2Weights=J(rows(statIdLong),timeslots,.)
    for (i=1;i<=rows(mLong);i++) {
        gP=panelsubmatrix(gameLong,i,mLong)
        gPtotal=colsum(gP)
        weightsP=(gPtotal:*gP)
    uvre2Weights[mLong[i,1]::mLong[i,2],.]=weightsP
    }

    uvre2g=lnnd(uvre2,0,ln(sdmarv)):*1:/rowsum(uvre2Weights)    
end 

mata:
    UpmodObs=J(rows(lnewsLong),0,0)
    for (i=1;i<=draws*timeslots;i=i+timeslots) {
        UpmodObs=UpmodObs,lnppsLong:-lnewsLong:*lnviewnLong:*bpo[1]:-otherlLong:*lnviewnLong:*bpo[2]:-
            nnewsLong:*lnviewnLong:*bpo[3]:-lnewsLong:*bpo[4]:-otherlLong:*bpo[5]:-l_ACS_HHLong:*bpo[6]:-bpo[10]:-
            UpsLong[,i::i+timeslots-1]:-UpmtLong[,i::i+timeslots-1]
    }
end

/* Histogram of the above seems reasonable */
/* We probably should include this in what we are doing... */

/* Note the below code applies to 58, 61, 63, 171, 172,178,181, 183, etc. */

mata:
    priceErrs=asarray_create("real",2)
    simPrices=asarray_create("real",2)
    priceBounds=asarray_create("real",2)

    up=J(rows(statIdLong),0,.)
    upg=J(rows(statIdLong),0,.)
    upb=J(rows(statIdLong),0,.)
    vup=J(rows(statIdLong),0,.)
    upre1=J(rows(statIdLong),0,.)
    upre2=J(rows(statIdLong),0,.)

    Troublers=J(0,6,.)
    allDraws=J(0,6,.)

    counter=1
    for (d=1;d<=draws;d++) {
        uptoAdd=J(rows(statIdLong),6,.)
        uptoAddg=J(rows(statIdLong),6,.)
        uptoAddb=J(rows(statIdLong),6,.)
        vuptoAdd=J(rows(statIdLong),6,.)
        upre1toAdd=J(rows(statIdLong),6,.)
        upre2toAdd=J(rows(statIdLong),6,.)
        gameList=J(0,2,.)
        for (i=1;i<=rows(statIdLong);i++) {
            if (gameMarker[i]==1) {
                idp=statIdLong[i]
                gameList=gameList \ (idp,i)
                lnewsHat=asarray(Bcs,(idp,1))
                otherlHat=asarray(Bcs,(idp,2))
                nnewsHat=asarray(Bcs,(idp,3))
                sharesHat=asarray(sharesBcs,(idp,d,1))
                lnewsAct=lnewsLong[i,]
                otherlAct=otherlLong[i,]
                nnewsAct=nnewsLong[i,]
            
                problem=0 
                Try=1
            
                do {
                    errMarker=(lnewsAct:==lnewsHat):*(otherlAct:==otherlHat)	
                    errPdraws=J(1,6,0)

                    p=mm_which(((rowsum(errMarker[,1::5])):==5):*(errMarker[,timeslots]:==0))	/* Same until last period */

                    actPayMean=(lnewsHat[p,]*bpo[1]:+otherlHat[p,]*bpo[2]:+nnewsHat[p,]*bpo[3]):*ln(popLong[i,]:*sharesHat[p,]):+
                        lnewsHat[p,]:*bpo[4]:+otherlHat[p,]*bpo[5]:+bpo[6]:*l_ACS_HHLong[i,]:+bpo[10]:+
                        UpsLong[i,counter::counter+timeslots-1]:+UpmtLong[i,counter::counter+timeslots-1]
                    Bound=J(1,timeslots,.)	

                    Bound[timeslots]=lnppsLong[i,timeslots]-actPayMean[timeslots]
                    errPdraws[,timeslots]=exp(bpo[9])*invnormal(runiform(1,1)*normal(Bound[timeslots]/exp(bpo[9])))

                    if (nnewsLong[i,timeslots]!=1) {
                        uptoAdd[i,timeslots]=actPayMean[timeslots]+errPdraws[timeslots]
                        meanP=actPayMean[timeslots]
                        uptoAddg[i,timeslots]=lnnd(uptoAdd[i,timeslots],meanP,bpo[9])-
                            ln(normal(Bound[timeslots]/exp(bpo[9])))

                        uptoAddb[i,timeslots]=actPayMean[timeslots]+Bound[timeslots]
                        vuptoAdd[i,timeslots]=ln(popLong[i,1]:*sharesHat[p,timeslots])	/* should be equal to viewership used above */
                    }
                    else {
                        uptoAdd[i,timeslots]=0
                        uptoAddg[i,timeslots]=0
                        uptoAddb[i,timeslots]=0
                        vuptoAdd[i,timeslots]=0
                    }

                    UpmodObz=UpmodObs[i,counter::counter+timeslots-1]
                    for (t=timeslots-1;t>=1;t--) {
                        if (nnewsAct[t]!=1) {
                            if (t>1) {
                                p=mm_which((rowsum(errMarker[,1::t-1]):==t-1):*(errMarker[,t]:==0))
                                useErrs=J(rows(p),timeslots,0)
                                useErrs[,1::t-1]=J(rows(p),1,UpmodObs[i,1::t-1])
                                useErrs[,t+1::cols(useErrs)]=
                                    errMarker[p,t+1::cols(errPdraws)]:*UpmodObz[1,t+1::cols(errPdraws)]:+
                                    (1:-errMarker[p,t+1::cols(errPdraws)]):*errPdraws[,t+1::cols(errPdraws)]
                            }
                            else {
                                p=mm_which(errMarker[,t]:==0)
                                useErrs=J(rows(p),timeslots,0)	
                                useErrs[,t+1::cols(useErrs)]=
                                    errMarker[p,t+1::cols(errPdraws)]:*UpmodObz[1,t+1::cols(errPdraws)]:+
                                    (1:-errMarker[p,t+1::cols(errPdraws)]):*errPdraws[,t+1::cols(errPdraws)]						
                            }

                            actPayMean=(lnewsHat[p,]*bpo[1]:+otherlHat[p,]*bpo[2]:+nnewsHat[p,]*bpo[3]):*ln(popLong[i,]:*sharesHat[p,]):+
                                lnewsHat[p,]:*bpo[4]:+otherlHat[p,]*bpo[5]:+bpo[6]:*l_ACS_HHLong[i,]:+bpo[10]:+
                                UpsLong[i,counter::counter+timeslots-1]:+UpmtLong[i,counter::counter+timeslots-1]

                            max1=sum(exp(lnppsLong[i,t::timeslots]))
                            max2=max(rowsum(exp(actPayMean[,t+1::timeslots]:+useErrs[,t+1::timeslots])))

                            Bound[t]=ln(max1-max2)-actPayMean[rows(actPayMean),t] 		
                            allDraws=allDraws \ (d,i,marketIdLong[i],idp,t,max1-max2)
                            errPdraws[,t]=exp(bpo[9])*invnormal(runiform(1,1)*normal(Bound[t]/exp(bpo[9])))
                            if (errPdraws[,t]==.) {
                                printf("+");displayflush();
                                problem=1
                                Troublers=Troublers \ (d,i,marketIdLong[i],idp,t,max1-max2)
                                errPdraws[,t]=-20
                                Bound[t]=-20
                                Try++
                            }

                            uptoAdd[i,t]=actPayMean[rows(actPayMean),t]+errPdraws[,t]
                            meanP=actPayMean[rows(actPayMean),t]
                            uptoAddg[i,t]=lnnd(uptoAdd[i,t],meanP,bpo[9])-
                                min((ln(normal((uptoAdd[i,t]-meanP)/exp(bpo[9]))),-ln(normal(-35))))
                            uptoAddb[i,t]=meanP+Bound[t]
                            vuptoAdd[i,t]=min(popLong[i,1]:*sharesHat[p,t])					
                        }
                        else {
                            errPdraws[,t]=UpmodObz[1,t]
                            uptoAdd[i,t]=0
                            uptoAddg[i,t]=0
                            uptoAddb[i,t]=0
                            vuptoAdd[i,t]=0
                        }
                    }
                } while (problem==1 & Try<12)    /* See if it makes a difference here at all */


            asarray(priceErrs,(statIdLong[i],d),errPdraws)
            asarray(priceBounds,(statIdLong[i],d),Bound)

            simPps=(lnewsHat*bpo[1]:+otherlHat*bpo[2]:+nnewsHat*bpo[3]):*ln(sharesHat:*popLong[i,])+
                lnewsHat*bpo[4]:+otherlHat*bpo[5]:+bpo[6]:*l_ACS_HHLong[i,]:+bpo[10]:+
                UpsLong[i,counter::counter+timeslots-1]:+UpmtLong[i,counter::counter+timeslots-1]:+
                errMarker:*UpmodObz[1,1::timeslots]:+(1:-errMarker):*errPdraws
            
            asarray(simPrices,(statIdLong[i],d),simPps)
            
            upre1toAdd[i,]=UpsLong[i,counter::counter+timeslots-1]
            upre2toAdd[i,]=UpmtLong[i,counter::counter+timeslots-1]	
            }
        }
        up=up,uptoAdd
        upg=upg,uptoAddg
        upb=upb,uptoAddb
        vup=vup,vuptoAdd
        upre1=upre1,upre1toAdd
        upre2=upre2,upre2toAdd
        counter=counter+timeslots
    }
end

mata: 
    upre1g=lnnd(upre1,0,ln(sdstap)):*1:/rowsum(gameLong)     /* spread across observations */
    upre2g=lnnd(upre2,0,ln(sdmarp)):*1:/rowsum(uvre2Weights)
end



mata:
    NashProfiles=asarray_create("real",4)
    NashProfits  =asarray_create("real",3)
    NashShares =asarray_create("real",3)

    PriceMaxSps=asarray_create("real",2) 
    
    actsToTry=10
    timeslots = 6

end

mata:
	UvmodObsLong4=J(rows(lnewsLong),timeslots*draws,0)
	counter=0
	for (d=1;d<=draws;d++) {
		for (t=1;t<=timeslots;t++) {
			yvt=ln(siLong[,t]):-ln(1:-slnewsLong[,t]:-snnewsLong[,t]:-sotherlLong[,t]:-sothercLong[,t])
			sigmaStuff=bo[1]:*lnswgLong[,t]:*lnewsLong[,t]:+bo[2]:*lnswgLong[,t]:*otherlLong[,t]:+
				bo[3]:*lnswgLong[,t]:*nnewsLong[,t]:+bo[4]:*lnswgLong[,t]:*othercLong[,t]
			XVt=lnewsLong[,t],otherlLong[,t],nnewsLong[,t],
				lnewsLong[,t]:*lnewsLongLag[,t],lnewsLong[,t]:*nnewsLongLag[,t],
				nnewsLong[,t]:*lnewsLongLag[,t],nnewsLong[,t]:*nnewsLongLag[,t],
				siLongLag[,t],siXlnln[,t],siXlnnn[,t],siXnnln[,t],siXnnnn[,t],
				lnewsLong[,t]:*ln(1:+totlnews[,t]),nnewsLong[,t]:*ln(1:+totnnews[,t]),l_ACS_HHLong[,t],
				lnewsnLong[,t],otherlnLong[,t],nnewsnLong[,t],othercnLong[,t],J(rows(lnewsLong),1,1)
			UvmodObsLong4[,counter+t]=yvt:-sigmaStuff:-XVt*betaDynoStart':-UvsLong[,counter+t]:-UvmtLong[,counter+t]		/* This is critical with more expansive RE simulation */
		}
		counter=counter+timeslots
	}


end

mata:
	counter = 1
    for (d=1;d<=draws;d++) {

        for (i=1;i<=rows(mLong);i++) {
            gameMarkerp=panelsubmatrix(gameMarker,i,mLong)
            playersp=colsum(gameMarkerp)
d,i,playersp
            if (playersp>0) {
                statIdLongp=panelsubmatrix(statIdLong,i,mLong)   
                Gamers=select(statIdLongp,gameMarkerp)           
                posofGamers=mm_which(gameMarkerp)                

                UvmtLongp=panelsubmatrix(UvmtLong,i,mLong)
                UvmodLongp=panelsubmatrix(UvmodLong,i,mLong)
                UvsLongp=panelsubmatrix(UvsLong,i,mLong)
                XBVngLongp=panelsubmatrix(XBVngLong,i,mLong)
                UvmodObsLongp=panelsubmatrix(UvmodObsLong4,i,mLong)

                for (g=1;g<=playersp;g++) {
                    pricesToUse=asarray(simPrices,(Gamers[g],d))
                    sharesToUse=asarray(sharesBcs,(Gamers[g],d,1))
                    maxindex(rowsum(exp(pricesToUse)),actsToTry,iP=.,w=.)
                    asarray(PriceMaxSps,(Gamers[g],d),iP)   
                }

                UpmodObsp=panelsubmatrix(UpmodObs,i,mLong)
                UpsLongp=panelsubmatrix(UpsLong,i,mLong)
                UpmtLongp=panelsubmatrix(UpmtLong,i,mLong)

                lnewsLongp=panelsubmatrix(lnewsLong,i,mLong)
                otherlLongp=panelsubmatrix(otherlLong,i,mLong)
                nnewsLongp=panelsubmatrix(nnewsLong,i,mLong)
                othercLongp=panelsubmatrix(othercLong,i,mLong)
                popLongp=panelsubmatrix(popLong,i,mLong)
                l_ACS_HHLongp=panelsubmatrix(l_ACS_HHLong,i,mLong)

                otherlOld=otherlLongp
                lnewsOld=lnewsLongp
                nnewsOld=nnewsLongp
                othercOld=othercLongp

                neq=1
                asarray(NashProfiles,(i,d,1,neq),lnewsOld)
                asarray(NashProfiles,(i,d,2,neq),otherlOld)
                asarray(NashProfiles,(i,d,3,neq),nnewsOld)

                for (s=1;s<=1000;s++) {
                    targetPlayerN=round(1+(playersp-1)*runiform(1,1))
                    targetPlayer=Gamers[targetPlayerN]

                    lnewsPlayer=asarray(Bcs,(targetPlayer,1))
                    otherlPlayer=asarray(Bcs,(targetPlayer,2))
                    nnewsPlayer=asarray(Bcs,(targetPlayer,3))
                    
                    newStrat=round(1+(rows(lnewsPlayer)-1)*runiform(1,1))

                    targetPos=posofGamers[targetPlayerN]
                    lnewsTry=lnewsOld
                    nnewsTry=nnewsOld
                    otherlTry=otherlOld
                    othercTry=othercOld

                    lnewsTry[targetPos,]=lnewsPlayer[newStrat,]
                    nnewsTry[targetPos,]=nnewsPlayer[newStrat,]
                    otherlTry[targetPos,]=otherlPlayer[newStrat,]
 
                    fail=0
                    g=1     

                    simPpsInit=J(rows(lnewsTry),cols(lnewsTry),0)
                    lnewsInit=lnewsTry
                    nnewsInit=nnewsTry
                    otherlInit=otherlTry
                    othercInit=othercTry
                    do {  
                        rowsToTry=asarray(PriceMaxSps,(Gamers[g],d)) 
                        playerPos=mm_which(statIdLongp:==Gamers[g]) 
                        a=1                 
                        firstTime=1         

                        do {
                            if (firstTime!=1) {
                                lnewsDev=asarray(Bcs,(Gamers[g],1))[rowsToTry[a],]
                                otherlDev=asarray(Bcs,(Gamers[g],2))[rowsToTry[a],]
                                lnewsTry[playerPos,]=lnewsDev
                                otherlTry[playerPos,]=otherlDev
                            }

                            lnewsLongLagp=J(rows(lnewsLongp),1,0)
                            nnewsLongLagp=J(rows(lnewsLongp),1,0)
                            otherlLongLagp=J(rows(lnewsLongp),1,0)
                            siLagp=J(rows(lnewsLongp),1,0)
                            totlnewsp=J(rows(lnewsLongp),1,0)
                            totnnewsp=J(rows(nnewsLongp),1,0)

                            lnewsnTry=lnewsTry:*ln(1:+colsum(lnewsTry))
                            otherlnTry=otherlTry:*ln(1:+colsum(otherlTry))
                            nnewsnTry=nnewsTry:*ln(1:+colsum(nnewsTry))
                            othercnTry=othercTry:*ln(1:+colsum(othercTry))

                            sharesTry=J(rows(lnewsLongp),0,0)
                            for (t=1;t<=timeslots;t++) {
                                if (t!=1) siLagp=ln(siLagp)
								XV=lnewsTry[,t],otherlTry[,t],nnewsTry[,t],
                                    lnewsLongLagp:*lnewsTry[,t],
                                    nnewsLongLagp:*lnewsTry[,t],
                                    lnewsLongLagp:*nnewsTry[,t], 
                                    nnewsLongLagp:*nnewsTry[,t],siLagp,
                                    siLagp:*lnewsLongLagp:*lnewsTry[,t],
                                    siLagp:*nnewsLongLagp:*lnewsTry[,t],
                                    siLagp:*lnewsLongLagp:*nnewsTry[,t],
                                    siLagp:*nnewsLongLagp:*nnewsTry[,t],
                                    lnewsTry[,t]:*ln(1:+totlnewsp),nnewsTry[,t]:*ln(1:+totnnewsp),l_ACS_HHLongp[,t],
                                    lnewsnTry[,t],otherlnTry[,t],nnewsnTry[,t],othercnTry[,t],J(rows(lnewsLongp),1,1)

                                    errCheck=lnewsTry[,t]:!=lnewsLongp[,t]
                                    XBVact=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                    UvsLongp[,counter+t-1]:+UvmodObsLongp[,counter+t-1]
                                    XBVsim=XV*betaDynoStart':+UvmtLongp[,counter+t-1]:+
                                        UvsLongp[,counter+t-t]:+UvmodLongp[,counter+t-1]

                                    XBV=(1:-errCheck):*XBVact:+errCheck:*XBVsim

                                    sharesTry=sharesTry,eshares_up(XBV,lnewsTry[,t],otherlTry[,t],nnewsTry[,t],
                                                                   othercLongp[,t],bo[1],bo[2],bo[3],bo[4])

                                    siLagp=sharesTry[,t]

                                    totlnewsp=totlnewsp:+J(rows(lnewsLongp),1,colsum(lnewsTry[,t]:*sharesTry[,t]))
                                    totnnewsp=totnnewsp:+J(rows(nnewsLongp),1,colsum(nnewsTry[,t]:*sharesTry[,t]))

                                    lnewsLongLagp=lnewsTry[,t]
                                    nnewsLongLagp=nnewsTry[,t]
                                    otherlLongLagp=otherlTry[,t]

                            }

                            errCheck=lnewsTry:!=lnewsLongp

                            UpmodErrp=J(rows(UpmodObsp),timeslots,.)

                            for (k=1;k<=playersp;k++) {
                                UpmodErrp[posofGamers[k],]=asarray(priceErrs,(Gamers[k],d))
                            }

                            Errs=errCheck:*UpmodErrp:+(1:-errCheck):*UpmodObsp[,counter::counter+timeslots-1]
                            simPps=(lnewsTry*bpo[1]:+otherlTry*bpo[2]:+nnewsTry*bpo[3]):*ln(sharesTry:*popLongp)+
                                lnewsTry*bpo[4]:+otherlTry*bpo[5]:+l_ACS_HHLongp:*bpo[6]:+bpo[10]:+
                                UpsLongp[,counter::counter+timeslots-1]:+UpmtLongp[,counter::counter+timeslots-1]:+
                                Errs

                            if (rowsum(exp(simPps[playerPos,]))>rowsum(exp(simPpsInit[playerPos,])) & firstTime!=1) fail=1
                            if (firstTime==1) {
                                firstTime=0
                                simPpsInit=simPps
                                sharesInit=sharesTry
                            }
                            else a++
                            
                        } while (a<=actsToTry & fail==0)
                        g++

                    } while (g<=rows(Gamers) & fail==0)
                    if (fail==0) {
                        printf("New Equilibrium Found!\n") 
                        neq++
                        asarray(NashProfiles,(i,d,1,neq),lnewsInit)
                        asarray(NashProfiles,(i,d,2,neq),otherlInit)
                        asarray(NashProfiles,(i,d,3,neq),nnewsInit)
                        asarray(NashShares,(i,d,neq),sharesInit)
                        asarray(NashProfits,(i,d,neq),simPpsInit) 
                        
                    }
                }
            }
        }
    counter=counter+timeslots
        
    fh = fopen("Notifier\record", "rw")
    fwrite(fh, "Something")
    fclose(fh)
    }

mata matsave AckerbergInfo up upg upb upre1 upre2 upre1g upre2g uv uvg vup uvsi uvso uvsg uvre1 uvre2 uvre1g uvre2g uvre2Weights Troublers, replace 
mata matsave AckerbergObjs NashProfiles NashShares NashProfits sharesBcs priceErrs simPrices priceBounds, replace
end
	
	
end
