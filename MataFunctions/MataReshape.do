mata:
	
	timeslots=6
	lnewsLong=J(rows(uniqrows(id)),timeslots,.)
	nnewsLong=J(rows(uniqrows(id)),timeslots,.)
	otherlLong=J(rows(uniqrows(id)),timeslots,.)
	othercLong=J(rows(uniqrows(id)),timeslots,.)
	slnewsLong=J(rows(uniqrows(id)),timeslots,.)
	snnewsLong=J(rows(uniqrows(id)),timeslots,.)
	sotherlLong=J(rows(uniqrows(id)),timeslots,.)
	sothercLong=J(rows(uniqrows(id)),timeslots,.)
	gameLong=J(rows(uniqrows(id)),timeslots,.)
	popLong=J(rows(uniqrows(id)),timeslots,.)  /* Not necessary but for convenience and completeness */
	l_ACS_HHLong=J(rows(uniqrows(id)),timeslots,.)
	
	siLong=J(rows(uniqrows(id)),timeslots,.)
	lnewsLongLag=J(rows(uniqrows(id)),timeslots,.)
	nnewsLongLag=J(rows(uniqrows(id)),timeslots,.)
	otherlLongLag=J(rows(uniqrows(id)),timeslots,.)
	otherCLongLag=J(rows(uniqrows(id)),timeslots,.)
	siLongLag=J(rows(uniqrows(id)),timeslots,.)
	idAct=id
	idToUse=colshape(idAct,timeslots)[,1]
	totlnews=J(rows(uniqrows(id)),timeslots,.)
	totnnews=J(rows(uniqrows(id)),timeslots,.)
	
	/* Placeholder for market! */
	
	marketIdLong=J(rows(uniqrows(id)),1,.)
	statIdLong  =J(rows(uniqrows(id)),1,.)
	
	lnppsLong=J(rows(uniqrows(id)),timeslots,.)
	lnviewnLong=J(rows(uniqrows(id)),timeslots,.)
	
	lnewsnLong=J(rows(uniqrows(id)),timeslots,.)
	otherlnLong=J(rows(uniqrows(id)),timeslots,.)
	nnewsnLong=J(rows(uniqrows(id)),timeslots,.)
	othercnLong=J(rows(uniqrows(id)),timeslots,.)
	
	dlnLong=J(rows(uniqrows(id)),timeslots,.)

/* Do this only once in a function before doing anything else */

	counter=1
	for (i=1;i<=rows(m);i++) {
		lnewsmp  =panelsubmatrix(lnews,i,m)
		nnewsmp  =panelsubmatrix(nnews,i,m)
		otherlmp =panelsubmatrix(otherl,i,m)
		othercmp =panelsubmatrix(otherc,i,m)
		idmp=panelsubmatrix(id,i,m)
		mtmp=panelsubmatrix(mt,i,m)
		simp=panelsubmatrix(si,i,m)
		timemp=panelsubmatrix(time,i,m)
		Mp    =panelsubmatrix(M,i,m)
		gamep=panelsubmatrix(game,i,m)
		lnppsp=panelsubmatrix(lnpps,i,m)
		lnviewnp=panelsubmatrix(lnviewn,i,m)
		popp=panelsubmatrix(pop,i,m)
		l_ACS_HHp=panelsubmatrix(l_ACS_HH,i,m)
		lnewsnp=panelsubmatrix(lnewsn,i,m)
		otherlnp=panelsubmatrix(otherln,i,m)
		nnewsnp=panelsubmatrix(nnewsn,i,m)
		othercnp=panelsubmatrix(othercn,i,m)
		dlnp=panelsubmatrix(dln,i,m)
		positioner=colshape(idmp,timeslots)

/* Make the above into histories - manufacture the data so we don't have */
/* to keep sending around all these interactions */

		obs    =uniqrows(idmp)
		lnewsm1=J(rows(obs),1,0)
		nnewsm1=J(rows(obs),1,0)
		otherlm1=J(rows(obs),1,0)
		othercm1=J(rows(obs),1,0)
		sim1   =J(rows(obs),1,0)
		for (k=4;k<=8;k++) {
			lnewsm1=lnewsm1,select(lnewsmp,timemp:==k-1)
			nnewsm1=nnewsm1,select(nnewsmp,timemp:==k-1)
			otherlm1=otherlm1,select(otherlmp,timemp:==k-1)
			othercm1=othercm1,select(othercmp,timemp:==k-1)
			sim1   =sim1,select(simp,timemp:==k-1)
		}
		lnewsc=J(rows(obs),0,0)
		nnewsc=J(rows(obs),0,0)
		sic   =J(rows(obs),0,0) 
		otherlc=J(rows(obs),0,0)
		othercc=J(rows(obs),0,0)
		gamec=J(rows(obs),0,0)
		lnppsc=J(rows(obs),0,0)
		lnviewnc=J(rows(obs),0,0)
		popc=J(rows(obs),0,0)
		l_ACS_HHc=J(rows(obs),0,0)
		lnewsnc=J(rows(obs),0,0)
		otherlnc=J(rows(obs),0,0)
		nnewsnc=J(rows(obs),0,0)
		othercnc=J(rows(obs),0,0)
		dlnc=J(rows(obs),0,0)
		
		for (k=3;k<=8;k++) {
			lnewsc=lnewsc,select(lnewsmp,timemp:==k)
			nnewsc=nnewsc,select(nnewsmp,timemp:==k)
			otherlc=otherlc,select(otherlmp,timemp:==k)
			othercc=othercc,select(othercmp,timemp:==k)
			sic  =sic,select(simp,timemp:==k)
			gamec=gamec,select(gamep,timemp:==k)
			lnppsc=lnppsc,select(lnppsp,timemp:==k)
			lnviewnc=lnviewnc,select(lnviewnp,timemp:==k)
			popc=popc,select(popp,timemp:==k)
			l_ACS_HHc=l_ACS_HHc,select(l_ACS_HHp,timemp:==k)
			lnewsnc=lnewsnc,select(lnewsnp,timemp:==k)
			otherlnc=otherlnc,select(otherlnp,timemp:==k)
			nnewsnc=nnewsnc,select(nnewsnp,timemp:==k)
			othercnc=othercnc,select(othercnp,timemp:==k)
			dlnc=dlnc,select(dlnp,timemp:==k)
		}

	lnewsLong[counter::counter+rows(positioner)-1,.]=lnewsc
	nnewsLong[counter::counter+rows(positioner)-1,.]=nnewsc
	otherlLong[counter::counter+rows(positioner)-1,.]=otherlc
	othercLong[counter::counter+rows(positioner)-1,.]=othercc
	siLong[counter::counter+rows(positioner)-1,.]=sic
	lnewsLongLag[counter::counter+rows(positioner)-1,.]=lnewsm1
	nnewsLongLag[counter::counter+rows(positioner)-1,.]=nnewsm1
	otherlLongLag[counter::counter+rows(positioner)-1,.]=otherlm1
	otherCLongLag[counter::counter+rows(positioner)-1,.]=othercm1
	siLongLag[counter::counter+rows(positioner)-1,.]=sim1
	gameLong[counter::counter+rows(positioner)-1,.]=gamec
	statIdLong[counter::counter+rows(positioner)-1,.]=positioner[,1]

	lnppsLong[counter::counter+rows(positioner)-1,.]=lnppsc
	lnviewnLong[counter::counter+rows(positioner)-1,.]=lnviewnc

	popLong[counter::counter+rows(positioner)-1,.]=popc
	l_ACS_HHLong[counter::counter+rows(positioner)-1,.]=l_ACS_HHc
	
	lnewsnLong[counter::counter+rows(positioner)-1,.]=lnewsnc
	otherlnLong[counter::counter+rows(positioner)-1,.]=otherlnc
	nnewsnLong[counter::counter+rows(positioner)-1,.]=nnewsnc	
	othercnLong[counter::counter+rows(positioner)-1,.]=othercnc	

	/* Group shares */
	
	dlnLong[counter::counter+rows(positioner)-1,.]=dlnc

	sl=colsum(lnewsc:*sic)	
	so=colsum(otherlc:*sic)
	sn=colsum(nnewsc:*sic)
	sc=colsum(othercc:*sic)
	slnewsLong[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,sl)
	snnewsLong[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,sn)
	sotherlLong[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,so)
	sothercLong[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,sc)
	

	/* Compute running totals */
	
	ctln=J(1,6,0)
	ctnn=J(1,6,0)
	totl=0
	totn=0
		for (k=1;k<=6;k++) {
			ctln[k]=colsum(lnewsm1[,k]:*sim1[,k])+totl
			ctnn[k]=colsum(nnewsm1[,k]:*sim1[,k])+totn
		}

	totlnews[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,runningsum(ctln))
	totnnews[counter::counter+rows(positioner)-1,.]=J(rows(positioner),1,runningsum(ctnn))
	
	marketIdLong[counter::counter+rows(positioner)-1,.]=colshape(Mp,6)[,1]
	
	counter=counter+rows(positioner)
	
	}
	
		// code tidbit rowshape(totln',1)'
		
		
	/* Now that we have rendered the data in wide format, we can start to think about */
	/* lags, interactions, etc. */
	
	lnewslnews=lnewsLong:*lnewsLongLag
	lnewsnnews=lnewsLong:*nnewsLongLag
	nnewslnews=nnewsLong:*lnewsLongLag
	nnewsnnews=nnewsLong:*nnewsLongLag

/* As all of these variables enter the model as logs, we should first change "siLongLag" to reflect this */

	siLongLag=ln(siLongLag)
	_editmissing(siLongLag,0)
	
	siXlnln=siLongLag:*lnewslnews
	siXlnnn=siLongLag:*lnewsnnews
	siXnnln=siLongLag:*nnewslnews
	siXnnnn=siLongLag:*nnewsnnews
	
	lnewsRT=lnewsLong:*ln(1:+totlnews)
	nnewsRT=nnewsLong:*ln(1:+totnnews)	/* this is how the variables enter in the regression */

end
