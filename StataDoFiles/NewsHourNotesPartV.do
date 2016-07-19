cd C:\Users\mjbaker\Documents\Github\NewsHour

use "AveragedDataDyno.dta", clear

capture gen game = (lnews | otherl) & lnpps != .
bysort stationid: egen gameCount = total(game)
bysort stationid: egen lnewsCount = total(lnews)
replace game = 0 if lnewsCount == 0 

bysort stationid: egen gameCheck = total(game)
replace gameCheck = gameCheck > 0 
bysort stationid: gen lastObs = _n == _N
tab gameCheck if lastObs
drop gameCheck

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
    mata matuse DynoStarts23, replace
    bo[,1::4] = bo[,1::4]:^2
    mata matuse betaPDynoStarts, replace
end

quietly do MataFunctions\MataReshape.do

mata:
    mata matuse usANDgs, replace
	mata matuse NashStuff, replace
end

mata:
    markets=uniqrows(marketIdLong)
    indexes=asarray_keys(NashProfiles)
    draws=max(indexes[,2])
    NashProfilesNew=asarray_create("real",4)
end

mata:
    for (i=1;i<=rows(markets);i++) {
	i
        for (d=1;d<=draws;d++) {
            gameMarkerp=select(gameLong[,1],marketIdLong:==i)
            if (colsum(gameMarkerp)>0) {
                Count=rows(uniqrows(select(indexes[,4],(indexes[,1]:==i):*(indexes[,2]:==d))))
                if (Count==1) {
                    asarray(NashProfilesNew,(i,d,1,1),asarray(NashProfiles,(i,d,1,1)))
                    asarray(NashProfilesNew,(i,d,2,1),asarray(NashProfiles,(i,d,2,1)))
                    asarray(NashProfilesNew,(i,d,3,1),asarray(NashProfiles,(i,d,3,1)))
                }
                else {
                    asarray(NashProfilesNew,(i,d,1,1),asarray(NashProfiles,(i,d,1,1)))
                    asarray(NashProfilesNew,(i,d,2,1),asarray(NashProfiles,(i,d,2,1)))
                    asarray(NashProfilesNew,(i,d,3,1),asarray(NashProfiles,(i,d,3,1)))
                    pop=2
                    tick=1
                    do {
                        noGo=0
                        for (j=1;j<=tick;j++) {
                            if (asarray(NashProfiles,(i,d,1,pop))==asarray(NashProfilesNew,(i,d,1,j))) noGo=1
                        }
                        if (noGo==0) {
                            tick++
                            asarray(NashProfilesNew,(i,d,1,tick),asarray(NashProfiles,(i,d,1,pop)))
                            asarray(NashProfilesNew,(i,d,2,tick),asarray(NashProfiles,(i,d,2,pop)))
                            asarray(NashProfilesNew,(i,d,3,tick),asarray(NashProfiles,(i,d,3,pop)))
                        }
                        pop++
                    } while (pop<=Count)
                }
            }
        }
    }   
end 

mata:
	mata drop NashProfiles
end

mata:
    Keys=asarray_keys(NashProfilesNew)
    market=marketIdLong#J(timeslots,1,1)
    gameMarkId=uniqrows(Keys[,1])
    NashW=J(rows(market),draws,.)
    
    for (i=1;i<=rows(gameMarkId);i++) {
        WtoAdd=J(1,draws,.)
        for (d=1;d<=draws;d++) {
            WtoAdd[d]=rows(select(Keys[,4],(Keys[,1]:==gameMarkId[i]):*(Keys[,2]:==d):*(Keys[,3]:==1)))
        }
        maxPos=max(mm_which(market:==gameMarkId[i]))
        NashW[maxPos,]=ln(1:/WtoAdd)
    }
end


mata:
    weights1=1:/rowsum(gameLong)#J(1,6,1)
    weights1=colshape(weights1,1)

    mLong=panelsetup(marketIdLong,1)
    weights2=J(rows(statIdLong),timeslots,.)
    for (i=1;i<=rows(mLong);i++) {
        gP=panelsubmatrix(gameLong,i,mLong)
        gPtotal=colsum(gP)
        weightsP=(gPtotal:*gP)
    weights2[mLong[i,1]::mLong[i,2],.]=weightsP
    }
    _editvalue(weights2,0,.)
    weights2=colshape(1:/weights2,1)
end

mata:
    mata matsave gsAndus NashW Up Upg Upb Uv Uvb Uvg Uvsi Uvsg Uvre1 Uvre2 Upre1 Upre2 Uvre1g Uvre2g Upre1g Upre2g market id mt weights1 weights2, replace
end



































