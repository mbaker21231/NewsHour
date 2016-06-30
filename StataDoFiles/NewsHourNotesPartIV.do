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
    
    draws=20
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

/* The above should in principle be okay, but might could be checked */

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
	mata matsave REffects UpsLong UpmtLong UvsLong UvmtLong, replace
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
	mata matsave BCS Bcs, replace
end

mata:
    mLong   = panelsetup(marketIdLong,1)
    counter = 1
end

do MataFunctions\sharemakers.do

do MataFunctions\loopShareGenerator.do

do MataFunctions\PriceShareGenerator.do

/* Construction Area */

mata:
    counter = 1
    stata("set seed 5150")
    priceErrs   = asarray_create("real", 2)
    priceBounds = asarray_create("real", 2)
    viewErrs    = asarray_create("real", 2)
    viewBounds  = asarray_create("real", 2)
    sharesBCS   = asarray_create("real", 3)
end

mata:
    UpmodObs=J(rows(lnewsLong),0,0)
    for (i=1;i<=draws*timeslots;i=i+timeslots) {
        UpmodObs=UpmodObs,lnppsLong:-lnewsLong:*lnviewnLong:*bpo[1]:-otherlLong:*lnviewnLong:*bpo[2]:-
            nnewsLong:*lnviewnLong:*bpo[3]:-lnewsLong:*bpo[4]:-otherlLong:*bpo[5]:-l_ACS_HHLong:*bpo[6]:-bpo[10]:-
            UpsLong[,i::i+timeslots-1]:-UpmtLong[,i::i+timeslots-1]
    }
end

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
                    vBound[timeslots] = 6.5                  
        
                    do {
                    
                        errVdraws[1,timeslots]=sdmodv*rnormal(1,1,0,1) 

                        p=mm_which(((rowsum(errMarker[,1::5])):==5):*(errMarker[,timeslots]:==0))  /*the sole deviate strategy */
                    
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
                            PriceShareGenerator(t, 0, 1, place, S=., P=., allShares = .)
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
                                    PriceShareGenerator(t, 0, 1, place, S=., P=., allShares = .)
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
                            PriceShareGenerator(t, 0, 1, place, S=., P=., allShares=.)
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
                                    PriceShareGenerator(t, 0, 1, place, S=., P=., allShares=.)
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
                asarray(priceErrs,(pId,c),errPdraws)
                asarray(priceBounds,(pId,c),pBound)
                asarray(viewErrs,(pId,c),errVdraws)
                asarray(viewBounds,(pId,c),vBound)
          
                s0 = 1:-colsum(allShares)
                sl = colsum(lnewsLongp:*allShares)
                sn = colsum(nnewsLongp:*allShares)
                so = colsum(otherlLongp:*allShares)
                sc = colsum(othercLongp:*allShares)
                sg=colsum(lnewsLongp[place,]:*sl:+nnewsLongp[place,]:*sn:+
                    otherlLongp[place,]:*so:+othercLongp[place,]:*sc)  
                asarray(sharesBCS,(pId,c,1),allShares[place,])
                asarray(sharesBCS,(pId,c,2),sg)
                asarray(sharesBCS,(pId,c,3),s0)
                }
            }
        }
        counter = counter + timeslots
    }
end

mata:
	mata matsave PVDraws priceErrs priceBounds viewErrs viewBounds sharesBCS, replace
end

do MataFunctions\lnnd.do

mata:
	mata matuse PVDraws, replace
end

mata:
    upre1 = UpmtLong
    upre2 = UpsLong
    uvre1 = UvmtLong
    uvre2 = UvsLong
    
    uvre2Weights=J(rows(statIdLong),timeslots,.)
    for (i=1;i<=rows(mLong);i++) {
        gP=panelsubmatrix(gameLong,i,mLong)
        gPtotal=colsum(gP)
        weightsP=(gPtotal:*gP)
    uvre2Weights[mLong[i,1]::mLong[i,2],.]=weightsP
    }
    
    upre1g =lnnd(upre1,0,ln(sdstap)):*1:/rowsum(gameLong)     
    upre2g =lnnd(upre2,0,ln(sdmarp)):*1:/rowsum(uvre2Weights)
    uvre1g=lnnd(uvre1,0,ln(sdstav)):*1:/rowsum(gameLong)  
    uvre2g=lnnd(uvre2,0,ln(sdmarv)):*1:/rowsum(uvre2Weights)   
end

mata:
	up  = J(rows(statIdLong), timeslots*draws, 0)
	upg = J(rows(statIdLong), timeslots*draws, 0)
	upb = J(rows(statIdLong), timeslots*draws, 0)
end

mata:
looper = 1
    for (c=1; c<=draws; c++ ) {
        for (i=1; i<=rows(statIdLong); i++) {
            pId = statIdLong[i]
            if (gameMarker[i] == 1) {
                siHat   = asarray(sharesBCS,(pId,c,1))
                sgHat   = asarray(sharesBCS,(pId,c,2))
                pErr    = asarray(priceErrs,(pId,c))
                pB      = asarray(priceBounds,(pId,c))
            
                lnewsp  = lnewsLong[i, ]
                otherlp = otherlLong[i, ]
                nnewsp  = nnewsLong[i, ]
            
                pop    = popLong[i, ]
                logPop = l_ACS_HHLong[i,]
            
                upmt  = upre1[i, looper::looper + timeslots - 1]
                ups = upre2[i, looper::looper + timeslots - 1]
            
                meanPHat = otherlp*bpo[1]:*ln(siHat:*pop) :+ lnewsp*bpo[2]:*ln(siHat:*pop) :+ nnewsp*bpo[3]:*ln(siHat:*pop) :+
                    bpo[4]:*otherlp :+ bpo[5]:*lnewsp :+ bpo[6]:*logPop :+ ups :+ upmt :+ bpo[10] 
                pHat = meanPHat :+ pErr
                gup =  lnnd(pErr, 0, ln(sdmodp)) :- ln(normal(pB))
            
                up[i, looper::looper + timeslots - 1]  = pHat
                upg[i, looper::looper + timeslots - 1] = gup
                upb[i, looper::looper + timeslots - 1] = meanPHat :+ pB
            }
        }
        looper = looper + timeslots
    }

end

mata:
	uv  = J(rows(statIdLong), timeslots*draws, 0)
	uvg = J(rows(statIdLong), timeslots*draws, 0)
	uvb = J(rows(statIdLong), timeslots*draws, 0)
end

mata:
looper = 1
    for (c=1; c<=draws; c++ ) {
        for (i=1; i<=rows(statIdLong); i++) {
            pId = statIdLong[i]
            if (gameMarker[i] == 1) {
                siHat   = asarray(sharesBCS, (pId,c,1))
                sgHat   = asarray(sharesBCS, (pId,c,2))
                vErr    = asarray(viewErrs, (pId,c))
                vB      = asarray(viewBounds, (pId,c))   
            
                lnewsp  = lnewsLong[i, ]
                otherlp = otherlLong[i, ]
                nnewsp  = nnewsLong[i, ]
                othercp = othercLong[i, ]
                
                lnewsLp  = lnewsLongLag[i, ]
                otherlLp = otherlLong[i, ]
                nnewsLp  = nnewsLongLag[i, ]
                lnsiLp     = 0, ln(siLong[i,1::5 ])
                totlnewsp = totlnews[i, ]
                totnnewsp = totnnews[i, ]
                lnewsnp   = exp(lnewsnLong[i, ] :- 1)
                nnewsnp   = nnewsnLong[i, ]
                othercnp  = othercnLong[i, ]
                otherlnp  = exp(otherlnLong[i, ] :- 1)
            
                logPop = l_ACS_HHLong[i,]

                uvmt  = uvre1[i, looper::looper + timeslots - 1]
                uvs    = uvre2[i, looper::looper + timeslots - 1]
            
                uvHat = otherlp*bo[5] :+ lnewsp*bo[6] :+ nnewsp*bo[7] :+ 
                        otherlp:*lnewsLp*bo[8] :+ otherlp:*nnewsLp*bo[9] :+
                        nnewsp:*lnewsLp*bo[10] :+ nnewsp:*nnewsLp*bo[11]  :+ 
                        lnsiLp:*bo[12] :+ lnsiLp:*otherlp:*lnewsLp*bo[13] :+
                        lnsiLp:*otherlp:*nnewsLp*bo[14] :+ lnsiLp:*nnewsp:*lnewsLp*bo[15] :+
                        lnsiLp:*nnewsp:*nnewsLp*bo[16] :+ totlnewsp:*bo[17] :+ totnnewsp*bo[18] :+
                        logPop*bo[19] :+ ln(1 :+ lnewsnp :+ otherlp :- lnewsp)*bo[20] :+ 
                        ln(1 :+ lnewsnp :+ lnewsp :- otherlp)*bo[21] :+ nnewsnp*bo[22] :+ othercnp*bo[23] :+ bo[27]
                
                uv[i, looper::looper + timeslots - 1]  = uvHat :+ vErr
                
                muv = otherlp:*bo[1] :+ lnewsp:*bo[2] :+ nnewsp:*bo[3] :+ othercp:*bo[4]
                
                guv =  lnnd(vErr, 0, ln(sdmodv)) :- ln(normal(vB)) :- ln(1 :- muv) :+ 
                        ln(1 :- muv:*siHat:/sgHat :- (1 :- muv):*siHat)
                
                uvg[i, looper::looper + timeslots - 1] = guv
                uvb[i, looper::looper + timeslots - 1] = muv :+ vB
            }
        }
        looper = looper + timeslots
    }

end

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
