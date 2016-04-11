/* Run NewsHourNOtesPartIV through line 493 */
/* Then: */

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
end

mata:
	d=1
        uptoAdd=J(rows(statIdLong),6,.)
        uptoAddg=J(rows(statIdLong),6,.)
        uptoAddb=J(rows(statIdLong),6,.)
        vuptoAdd=J(rows(statIdLong),6,.)
        upre1toAdd=J(rows(statIdLong),6,.)
        upre2toAdd=J(rows(statIdLong),6,.)
        gameList=J(0,2,.)
end

mata:
		i=58
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
end		

/* We now enter the "Try" loop, which seems to have been giving us problems. */
/* this is for iteration i=58 */

mata:           
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
                } while (problem==1 & Try<12)
end




mata:
		i=61
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
end		

/* We now enter the "Try" loop, which seems to have been giving us problems. */
/* this is for iteration i=61 */

mata:           
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
                } while (problem==1 & Try<12)
end



/* i=172 is a problem...why? */
mata:
		i=172
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
end		

/* We now enter the "Try" loop, which seems to have been giving us problems. */
/* this is for iteration i=63 */
 
mata:           
                do {
					problem = 0
                    errMarker=(lnewsAct:==lnewsHat):*(otherlAct:==otherlHat)	
                    errPdraws=J(1,6,0)

                    p=mm_which(((rowsum(errMarker[,1::5])):==5):*(errMarker[,timeslots]:==0))	/* Same until last period */

                    actPayMean=(lnewsHat[p,]*bpo[1]:+otherlHat[p,]*bpo[2]:+nnewsHat[p,]*bpo[3]):*ln(popLong[i,]:*sharesHat[p,]):+
                        lnewsHat[p,]:*bpo[4]:+otherlHat[p,]*bpo[5]:+bpo[6]:*l_ACS_HHLong[i,]:+bpo[10]:+
                        UpsLong[i,counter::counter+timeslots-1]:+UpmtLong[i,counter::counter+timeslots-1]
                    Bound=J(1,timeslots,.)	

                    Bound[timeslots]=lnppsLong[i,timeslots]-actPayMean[timeslots]-Try/10
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

                            Bound[t]=ln(max1-max2)-actPayMean[rows(actPayMean),t]-Try/10 		
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
                            vuptoAdd[i,t]=min(popLong[i,1]:*sharesHat[p,t])		/* This or the other one up top should be in logs! */			
                        }
                        else {
                            errPdraws[,t]=UpmodObz[1,t]
                            uptoAdd[i,t]=0
                            uptoAddg[i,t]=0
                            uptoAddb[i,t]=0
                            vuptoAdd[i,t]=0
                        }
                    }
                } while (problem==1 & Try<5000)
end


