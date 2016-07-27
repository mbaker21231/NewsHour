cd C:\Users\mjbaker\Documents\GitHub\NewsHour
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
        bo[2]:*lnswgLong:*otherlLong-
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
	    UvmodLong=invnormal(runiform(rows(lnewsLong),timeslots*draws))*sdmodv
    UvmodObsLong=xBpUE:-xBDyno
    UvmodObsLong=J(1,draws,1)#UvmodObsLong:-UvsLong:-UvmtLong
	
    mata matuse betaPDynoStarts, replace
    
    sdstap=exp(bpo[7])
    sdmarp=exp(bpo[8])
    sdmodp=exp(bpo[9])   	
	
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
	
	    lnewsSome  = rowsum(lnewsLong):>0
    gameMarker = gameMarker:*lnewsSome
    gameLong   = gameLong:*gameMarker
    decCounter = rowsum(gameLong)
    
    sum(gameMarker)
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
   mLong   = panelsetup(marketIdLong,1)
    counter = 1
end

do MataFunctions\sharemakers.do
do MataFunctions\PriceShareGenerator.do
do MataFunctions\loopShareGenerator.do

mata:
    counter = 1
    stata("set seed 5150")
    priceErrs   = asarray_create("real", 2)
    priceBounds = asarray_create("real", 2)
    viewErrs    = asarray_create("real", 2)
    viewBounds  = asarray_create("real", 2)
    sharesBCS   = asarray_create("real", 3)
end

mata
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
                asarray(priceErrs,(statIdLong[i],c),errPdraws)
                asarray(priceBounds,(statIdLong[i],c),pBound)
                asarray(viewErrs,(statIdLong[i],c),errVdraws)
                asarray(viewBounds,(statIdLong[i],c),vBound)
          
                s0 = 1:-colsum(allShares)
                sl = colsum(lnewsLongp:*allShares)
                sn = colsum(nnewsLongp:*allShares)
                so = colsum(otherlLongp:*allShares)
                sc = colsum(othercLongp:*allShares)
                sg=colsum(lnewsLongp[place,]:*sl:+nnewsLongp[place,]:*sn:+
                    otherlLongp[place,]:*so:+othercLongp[place,]:*sc)  
                asarray(sharesBCS,(statIdLong[i],c,1),allShares[place,])
                asarray(sharesBCS,(statIdLong[i],c,2),sg)
                asarray(sharesBCS,(statIdLong[i],c,3),s0)
                }
            }
        }
        counter = counter + timeslots
    }




end





