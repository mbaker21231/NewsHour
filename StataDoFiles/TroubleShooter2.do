/* Run through line 194 */
/* Very first hypothetical shares... */
/* First market is 67*6 stations */

/* Best Jump seems to generate entirely plausible results. */

/* So, let's focus on the pricing problem and what we seem to be getting wrong here... */
/* First, run NewsHourNotesPartIV through line 318 */

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



/* Outer loop: */


mata:
    c = 1
        ackToAdd=J(rows(statIdLong),timeslots,.)
        ackToAddg=J(rows(statIdLong),timeslots,.)
        ackToAddsi=J(rows(statIdLong),timeslots,.)
        ackToAddso=J(rows(statIdLong),timeslots,.)
        ackToAddsg=J(rows(statIdLong),timeslots,.)
        ackToAddre1=J(rows(statIdLong),timeslots,.)
        ackToAddre2=J(rows(statIdLong),timeslots,.)
end

mata:
		i = 1
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
			
end:

/* t block goes backwards ... */
/* Run the whole block first */

mata:
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



end



/* Now, we are out of the time loop */
/* So, this has to do with the very first observation. */

mata:
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
end		
			
/* Second part of t block - another if statement  - Something seems wierd here...*/
mata:
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
end
			
