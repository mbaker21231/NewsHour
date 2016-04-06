/* Now, we are going to go for the gusto here and get some model estimates  */
/* The first thing we need to do is open up the data and run the Mata files */

	clear all 	
	set more off 
	set seed 8675309
        do "/user/mjbaker/TV/ad_sample/AveragedDataDynamicMataFinal1.do" 
	use "/user/mjbaker/TV/ad_sample/AveragedDataDyno.dta
	
	sort market stationid mt timeslot	/* Dont' ever forget to sort the fucking data */
	
	/* Once again, we only want data that is actually used in the model */
	
	/* Make a mata variable that will help with this */
	gen keeper=1
	gen game=lnpps!=.&(lnews | otherl)
	//mata: keeper=st_data(.,"keeper")
	
	/* we also need to retain the order of the data but we need some time-series operations */
	
	gen useOrd=_n
	
	tsset stationid timeslot

	/* We also need static dynamic info to get the distributions of the drawn */
	/* Alternatives. Here are how we have to change things to get Xprime, say */
	/* for each observation. */
	/* List of explanatory variables: */
	/* 	X[,1::3]=lnews,otherl,nnews - just swap lnews and otherl here */
	
	gen otherlPrime=lnews&game
	gen lnewsPrime =otherl&game
	gen nnewsPrime =nnews&game
	
	/*      X[,4::6]=Dy.lnewslnews,Dy.lnewsnnews,Dy.nnewslnews,
		If currently broadcasting lnews, set the first two to zero */

	gen lnewslnewsPrime=otherl*l.lnews
	gen lnewsnnewsPrime=otherl*l.nnews
	gen nnewslnewsPrime=nnews*l.lnews
	gen nnewsnnewsPrime=nnews*l.nnews
	gen lsiPrime=l.si
	replace lsiPrime=ln(lsiPrime)
	replace lsiPrime=0 if lsiPrime==.
	
	gen siXlnlnPrime=lnewslnewsPrime*lsi
	gen siXlnnnPrime=lnewsnnewsPrime*lsi
	gen siXnnlnPrime=nnewslnewsPrime*lsi
	gen siXnnnnPrime=nnewsnnewsPrime*lsi
	gen lnewstotPrime=otherl*ln(1+totslnews)
	gen nnewstotPrime=nnews*ln(1+totsnnews)		/* Very clever how one switches and the other doesn't */	

	sort useOrd
	 
	mata:
		st_view(lnewsPrime=.,.,"lnewsPrime")
		st_view(otherlPrime=.,.,"otherlPrime")
		st_view(nnewsPrime=.,.,"nnewsPrime")
		st_view(lnewslnewsPrime=.,.,"lnewslnewsPrime")
		st_view(lnewsnnewsPrime=.,.,"lnewsnnewsPrime")
		st_view(nnewslnewsPrime=.,.,"nnewslnewsPrime")
		st_view(nnewsnnewsPrime=.,.,"nnewsnnewsPrime")
		st_view(lsiPrime=.,.,"lsiPrime")
		st_view(siXlnlnPrime=.,.,"siXlnlnPrime")
		st_view(siXlnnnPrime=.,.,"siXlnnnPrime")
		st_view(siXnnlnPrime=.,.,"siXnnlnPrime")
		st_view(siXnnnnPrime=.,.,"siXnnnnPrime")
		st_view(lnewstotPrime=.,.,"lnewstotPrime")
		st_view(nnewstotPrime=.,.,"nnewstotPrime")
//		st_view(iev1=.,.,"iev1")
//		st_view(iev2=.,.,"iev2")
//		st_view(iep1=.,.,"iep1")
//		st_view(iep2=.,.,"iep2")

	/* Set up the stuff for estimation: */

		st_view(lnewslnews=.,.,"lnewslnews","keeper")
		st_view(lnewsnnews=.,.,"lnewsnnews","keeper")
		st_view(nnewslnews=.,.,"nnewslnews","keeper")
		st_view(nnewsnnews=.,.,"nnewsnnews","keeper")
		st_view(lsi=.,.,"lsi","keeper")
		st_view(siXlnln=.,.,"siXlnln","keeper")
		st_view(siXlnnn=.,.,"siXlnnn","keeper")
		st_view(siXnnln=.,.,"siXnnln","keeper")
		st_view(siXnnnn=.,.,"siXnnnn","keeper")
		st_view(lnewstot=.,.,"lnewstot","keeper")
		st_view(nnewstot=.,.,"nnewstot","keeper")
		st_view(l_ACS_HH=.,.,"l_ACS_HH","keeper")
		st_view(nnewsn=.,.,"nnewsn","keeper")
		st_view(lnewsn=.,.,"lnewsn","keeper")
		st_view(otherln=.,.,"otherln","keeper")
		st_view(othercn=.,.,"othercn","keeper")
		
		
		
		Dy=dynoInfoInit(lnewslnews, lnewsnnews, nnewslnews, nnewsnnews,
			lsi,siXlnln, siXlnnn,siXnnln, siXnnnn, lnewstot, nnewstot,l_ACS_HH,
			lnewsn,otherln,nnewsn,othercn)


	/* Just use the price information as-is, because it is essentially */
	/* Already structured to drop all the missing values */
	
	mata matuse /user/mjbaker/TV/ad_sample/MataPriceData
	P=priceInfoInit(lnppst,lnewst,otherlt,nnewst,lnviewt,l_ACS_HHt,idt,mtt,mat)
	
	/* Initiate everything pertaining to selection effects */
//	st_view(iev1=.,.,"iev1")
//	st_view(iev2=.,.,"iev2")
//	st_view(iep1=.,.,"iep1")
//	st_view(iep2=.,.,"iep2")
	st_view(game=.,.,"game")
	st_view(stationid=.,.,"stationid")
//	st_view(Mpop=.,.,"Mpop")
	st_view(Mpop=.,.,"ACS_HH")
	st_view(si=.,.,"si")

	mata matuse /user/mjbaker/TV/ad_sample/gsAndus, replace 
	end
	
	/* We need to get the data into comparable orders, so */
	
	mata
	ord=order((market,id,mt),1..3)
	end
	
	mata
	NashW=select(NashW,game)
	_editmissing(NashW,0)
	Up=select(Up,game)
	Upb=select(Upb,game)
	Upg=select(Upg,game)
	Uvsi=select(Uvsi,game)
	Uvsg=select(Uvsg,game)
	Uvso=select(Uvso,game)
	Uv=select(Uv,game)
	Uvg=select(Uvg,game)
	Uvre1=select(Uvre1,game)
	Uvre2=select(Uvre2,game)
	Upre1=select(Upre1,game)
	Upre2=select(Upre2,game)
	weights1=select(weights1,game)
	weights2=select(weights2,game)
	Uvre1g=select(Uvre1g,game)
	Uvre2g=select(Uvre2g,game)
	Upre1g=select(Upre1g,game)
	Upre2g=select(Upre2g,game)
	
	Mpop=select(Mpop,game)

	lnewsPrime=select(lnewsPrime,game)
	nnewsPrime=select(nnewsPrime,game)
	otherlPrime=select(otherlPrime,game)
	lnewslnewsPrime=select(lnewslnewsPrime,game)
	_editmissing(lnewslnewsPrime,0)
	lnewsnnewsPrime=select(lnewsnnewsPrime,game)
	_editmissing(lnewsnnewsPrime,0)
	nnewslnewsPrime=select(nnewslnewsPrime,game)
	_editmissing(nnewslnewsPrime,0)
	nnewsnnewsPrime=select(nnewsnnewsPrime,game)
	_editmissing(nnewsnnewsPrime,0)
	lsiPrime=select(lsiPrime,game)
	_editmissing(lsiPrime,0)
	siXlnlnPrime=select(siXlnlnPrime,game)
	_editmissing(siXlnlnPrime,0)			/*These all need to be fixed */
	siXlnnnPrime=select(siXlnnnPrime,game)
	_editmissing(siXlnnnPrime,0)
	siXnnlnPrime=select(siXnnlnPrime,game)
	_editmissing(siXnnlnPrime,0)
	siXnnnnPrime=select(siXnnnnPrime,game)
	_editmissing(siXnnnnPrime,0)
	lnewstotPrime=select(lnewstotPrime,game)
	nnewstotPrime=select(nnewstotPrime,game)
	
	l_ACS_HHPrime=select(l_ACS_HH,game)
	
//	iev1=select(iev1,game)
//	iev2=select(iev2,game)
//	iep1=select(iep1,game)
//	iep2=select(iep2,game)
	
	GW=gsAndwsInit(NashW,Up,Upb,Upg,Uv,Uvg,Uvsi,Uvsg,Uvso,
		lnewsPrime,otherlPrime,nnewsPrime,lnewslnewsPrime,
		lnewsnnewsPrime,nnewslnewsPrime,nnewsnnewsPrime,lsiPrime,siXlnlnPrime,
		siXlnnnPrime,siXnnlnPrime,siXnnnnPrime,lnewstotPrime,nnewstotPrime,Uvre1,Uvre2,Upre1,Upre2,weights1,weights2,
		Uvre1g,Uvre2g,Upre1g,Upre2g,ln(Uvsi:*Mpop),l_ACS_HHPrime)	

	end
			
	mata:
	
	Z=moptimize_init()
	moptimize_init_trace_dots(Z,"on")
	moptimize_init_trace_params(Z,"on")
	moptimize_init_evaluator(Z,&logLikelihoodPriceViewComplete())	/* Note that this is different when using mcmc */
	moptimize_init_evaluatortype(Z,"d0")				/* Changed to a d0-type evaluator */
	moptimize_init_which(Z,"max")

	moptimize_init_eq_indepvars(Z,1,"")	
	moptimize_init_eq_indepvars(Z,2,"")	
	moptimize_init_eq_indepvars(Z,3,"")	
	moptimize_init_eq_indepvars(Z,4,"")     
	moptimize_init_eq_indepvars(Z,5,"")     
	moptimize_init_eq_indepvars(Z,6,"")     
	moptimize_init_eq_indepvars(Z,7,"")     
	moptimize_init_eq_indepvars(Z,8,"")	
	moptimize_init_eq_indepvars(Z,9,"")	
	moptimize_init_eq_indepvars(Z,10,"")
	moptimize_init_eq_indepvars(Z,11,"")
	moptimize_init_eq_indepvars(Z,12,"")
	moptimize_init_eq_indepvars(Z,13,"")
	moptimize_init_eq_indepvars(Z,14,"")
	moptimize_init_eq_indepvars(Z,15,"")
	moptimize_init_eq_indepvars(Z,16,"")
	moptimize_init_eq_indepvars(Z,17,"")
	moptimize_init_eq_indepvars(Z,18,"")
	moptimize_init_eq_indepvars(Z,19,"")
	moptimize_init_eq_indepvars(Z,20,"")
	moptimize_init_eq_indepvars(Z,21,"")
	moptimize_init_eq_indepvars(Z,22,"")
	moptimize_init_eq_indepvars(Z,23,"")
	moptimize_init_eq_indepvars(Z,24,"")
	moptimize_init_eq_indepvars(Z,25,"")
	moptimize_init_eq_indepvars(Z,26,"")
	moptimize_init_eq_indepvars(Z,27,"")
	moptimize_init_eq_indepvars(Z,28,"")
	moptimize_init_eq_indepvars(Z,29,"")
	moptimize_init_eq_indepvars(Z,30,"")
	moptimize_init_eq_indepvars(Z,31,"")	
	moptimize_init_eq_indepvars(Z,32,"")
	moptimize_init_eq_indepvars(Z,33,"")
	moptimize_init_eq_indepvars(Z,34,"")	
	moptimize_init_eq_indepvars(Z,35,"")
	moptimize_init_eq_indepvars(Z,36,"")	
	moptimize_init_eq_indepvars(Z,37,"")
	
	moptimize_init_touse(Z,"keeper")
	moptimize_init_depvar(Z,1,"dln")
	moptimize_init_depvar(Z,2,"ln_swg")
	moptimize_init_depvar(Z,3,"lnews")
	moptimize_init_depvar(Z,4,"otherl")
	moptimize_init_depvar(Z,5,"nnews")
	moptimize_init_depvar(Z,6,"otherc")

	st_view(ma=.,.,"market","keeper")
	st_view(id=.,.,"stationid","keeper")
	st_view(mt=.,.,"mt","keeper")
	
	m=panelsetup(ma,1)
	moptimize_init_userinfo(Z,1,id)
	moptimize_init_userinfo(Z,2,mt)
	moptimize_init_userinfo(Z,3,m)
	moptimize_init_userinfo(Z,4,Dy)
	moptimize_init_userinfo(Z,5,P)
	moptimize_init_userinfo(Z,6,GW)
	moptimize_init_userinfo(Z,7,si)
	moptimize_evaluate(Z)
	mata matuse /user/mjbaker/TV/ad_sample/betaPDynoStarts
	mata matuse /user/mjbaker/TV/ad_sample/DynoStarts23
//	mata matuse /user/mjbaker/TV/ad_sample/Results
//	bo=b_start[rows(b_start),]
//	bo[,1::4]=bo[,1::4]:^2 /*Unnecessary because the function uses squared values */
	
	bv=bo,bpo
	Vstart=diag(sqrt(abs(bv)))
	
	alginfo="mwg","d0","moptimize"
	b_start=amcmc(alginfo,&logLikelihoodPriceViewComplete(),bv,Vstart,401,1,1,.4,arate=.,vals=.,lambda=.,.,Z,"noisy")

	mata matsave /user/mjbaker/TV/ad_sample/Results b_start, replace
end	

mata:	
	struct gsAndws {
		real matrix NashW,Up,Upb,Upg,Uv,Uvg,Uvsi,Uvsg,Uvso,
			lnewsPrime,otherlPrime,nnewsPrime,
			lnewslnewsPrime,lnewsnnewsPrime,nnewslnewsPrime,
			nnewsnnewsPrime,lsiPrime,siXlnlnPrime,siXlnnnPrime,
			siXnnlnPrime,siXnnnnPrime,lnewstotPrime,nnewstotPrime,
			Uvre1,Uvre2,Upre1,Upre2,weights1,weights2,Uvre1g,Uvre2g,Upre1g,Upre2g,lnview,l_ACS_HH	
	}
	struct gsAndws gsAndwsInit(NashW,Up,Upb,Upg,Uv,Uvg,Uvsi,Uvsg,Uvso,
		lnewsPrime,otherlPrime,nnewsPrime,lnewslnewsPrime,
		lnewsnnewsPrime,nnewslnewsPrime,nnewsnnewsPrime,lsiPrime,siXlnlnPrime,
		siXlnnnPrime,siXnnlnPrime,siXnnnnPrime,lnewstotPrime,nnewstotPrime,Uvre1,Uvre2,Upre1,Upre2,weights1,weights2,
		Uvre1g,Uvre2g,Upre1g,Upre2g,lnview,l_ACS_HH)
	{	
		struct gsAndws scalar GW
	
		GW.NashW=NashW
		GW.Up=Up
		GW.Upb=Upb
		GW.Upg=Upg
		GW.Uv=Uv
		GW.Uvg=Uvg
		GW.Uvsi=Uvsi
		GW.Uvsg=Uvsg
		GW.Uvso=Uvso
		GW.lnewsPrime=lnewsPrime
		GW.otherlPrime=otherlPrime
		GW.nnewsPrime=nnewsPrime
		GW.lnewslnewsPrime=lnewslnewsPrime
		GW.lnewsnnewsPrime=lnewsnnewsPrime
		GW.nnewslnewsPrime=nnewslnewsPrime
		GW.nnewsnnewsPrime=nnewsnnewsPrime
		GW.lsiPrime=lsiPrime
		GW.siXlnlnPrime=siXlnlnPrime
		GW.siXlnnnPrime=siXlnnnPrime
		GW.siXnnlnPrime=siXnnlnPrime
		GW.siXnnnnPrime=siXnnnnPrime
		GW.lnewstotPrime=lnewstotPrime
		GW.nnewstotPrime=nnewstotPrime
		GW.Uvre1=Uvre1
		GW.Uvre2=Uvre2
		GW.Upre1=Upre1
		GW.Upre2=Upre2
		GW.weights1=weights1
		GW.weights2=weights2
		GW.Uvre1g=Uvre1g
		GW.Uvre2g=Uvre2g
		GW.Upre1g=Upre1g
		GW.Upre2g=Upre2g
		GW.lnview=lnview
		GW.l_ACS_HH=l_ACS_HH
	return(GW)
}	
end	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
