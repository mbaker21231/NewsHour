// Building a model with lags and stuff 

clear all
use "/user/mjbaker/TV/ad_sample/AveragedData.dta", clear
quietly do "/user/mjbaker/TV/ad_sample/AveragedDataDynamicMataFinal1.do"

set more off 
set seed 5150
tsset stationid timeslot

gen double lsi=l.lnsi
replace lsi=0 if lsi==.


// Generate some more expansive interactions:

gen lnewslnews=lnews*l.lnews
gen lnewsnnews=lnews*l.nnews
gen nnewslnews=nnews*l.lnews
gen nnewsnnews=nnews*l.nnews
replace lnewslnews=0 if lnewslnews==.
replace lnewsnnews=0 if lnewsnnews==.
replace nnewslnews=0 if nnewslnews==.
replace nnewsnnews=0 if nnewsnnews==.

gen double siXlnln=lnewslnews*lsi
gen double siXlnnn=lnewsnnews*lsi
gen double siXnnln=nnewslnews*lsi
gen double siXnnnn=nnewsnnews*lsi

gen double totslnews=0
sort stationid timeslot
bysort stationid: replace totslnews=slnews[_n-1] if _n==2
bysort stationid: replace totslnews=totslnews[_n-1]+slnews[2] if _n==3
bysort stationid: replace totslnews=totslnews[_n-1]+slnews[3] if _n==4
bysort stationid: replace totslnews=totslnews[_n-1]+slnews[4] if _n==5
bysort stationid: replace totslnews=totslnews[_n-1]+slnews[5] if _n==6



gen double totsnnews=0
sort stationid timeslot
bysort stationid: replace totsnnews=snnews[_n-1] if _n==2
bysort stationid: replace totsnnews=totsnnews[_n-1]+snnews[2] if _n==3
bysort stationid: replace totsnnews=totsnnews[_n-1]+snnews[3] if _n==4
bysort stationid: replace totsnnews=totsnnews[_n-1]+snnews[4] if _n==5
bysort stationid: replace totsnnews=totsnnews[_n-1]+snnews[5] if _n==6

gen double lnewstot=lnews*ln(1+totslnews)
gen double nnewstot=nnews*ln(1+totsnnews)

gen l_ACS_HH=ln(ACS_HH)

/* New stuff added following Ackerberg's next awesome paper */

bysort mt: egen total_lnews=total(lnews)
bysort mt: egen total_nnews=total(nnews)
bysort mt: egen total_otherl=total(otherl)
bysort mt: egen total_otherc=total(otherc)

gen double lnewsn=lnews*ln(1+total_lnews)
gen double nnewsn=nnews*ln(1+total_nnews)
gen double otherln=otherl*ln(1+total_otherl)
gen double othercn=otherc*ln(1+total_otherc)

/* Just for shits and giggles, as estimated without the Ackerberg terms */

a2reg dln ln_swg ln_swgXslnews ln_swgXsotherl ln_swgXsnnews lnews otherl nnews ///
	lnewslnews lnewsnnews nnewslnews nnewsnnews lsi siXlnln siXlnnn siXnnln siXnnnn ///
	lnewstot nnewstot l_ACS_HH, individual(stationid) unit(mt)

/* Now with the Ackerberg terms */

a2reg dln ln_swg ln_swgXslnews ln_swgXsotherl ln_swgXsnnews lnews otherl nnews ///
	lnewslnews lnewsnnews nnewslnews nnewsnnews lsi siXlnln siXlnnn siXnnln siXnnnn ///
	lnewstot nnewstot l_ACS_HH lnewsn otherln nnewsn othercn, individual(stationid) unit(mt)

mat bDyno=e(b)
mat sdmarDy=e(sdunit)
mat sdstaDy=e(sdind)
mat sdmodDy=e(rmse)
mat alphavDy=e(constant)

mata
	bDyno=st_matrix("bDyno")
	sdmarDy=st_matrix("sdmarDy")
	sdstaDy=st_matrix("sdstaDy")
	sdmodDy=st_matrix("sdmodDy")
	alphavDy=st_matrix("alphavDy")
end

/* Estimate the model by maximum likelihood */

	sort market stationid mt

	mata:

	st_view(ma=.,.,"market")
	st_view(id=.,.,"stationid")
	st_view(mt=.,.,"mt")
	
/* While not needed helpful to have around */
	
	boDyno=bDyno
	boDyno[1]=bDyno[1]+bDyno[2]
	boDyno[2]=bDyno[1]+bDyno[3]
	boDyno[3]=bDyno[1]+bDyno[4]
	boDyno[4]=bDyno[1]

	Z=moptimize_init()
	moptimize_init_trace_dots(Z,"on")
	moptimize_init_trace_params(Z,"on")
	moptimize_init_evaluator(Z,&logLikelihood2DynaA())			/* Note that this is different when using mcmc */
	moptimize_init_evaluatortype(Z,"d0")				/* Changed to a d0-type evaluator */
	moptimize_init_which(Z,"max")

	moptimize_init_eq_indepvars(Z,1,"")	/* mu_l */
	moptimize_init_eq_indepvars(Z,2,"")	/* mu_o */
	moptimize_init_eq_indepvars(Z,3,"")	/* mu_n */
	moptimize_init_eq_indepvars(Z,4,"")	/* mu_c - the constant */
	moptimize_init_eq_indepvars(Z,5,"")     /* eta_l */
	moptimize_init_eq_indepvars(Z,6,"")     /* eta_o */
	moptimize_init_eq_indepvars(Z,7,"")	/* eta_n */
	
	moptimize_init_eq_indepvars(Z,8,"")     /* eta_ll */
	moptimize_init_eq_indepvars(Z,9,"")	/* eta_ln */
	moptimize_init_eq_indepvars(Z,10,"")	/* eta_nl */
	moptimize_init_eq_indepvars(Z,11,"")    /* eta_nn */
	
	moptimize_init_eq_indepvars(Z,12,"")    /*lam_own */
	moptimize_init_eq_indepvars(Z,13,"")	/*lam_ll  */
	moptimize_init_eq_indepvars(Z,14,"")	/*lam_ln  */
	moptimize_init_eq_indepvars(Z,15,"")	/*lam_nl  */
	moptimize_init_eq_indepvars(Z,16,"") 	/*lam_nn  */
	
	moptimize_init_eq_indepvars(Z,17,"")    /*rho_l   */
	moptimize_init_eq_indepvars(Z,18,"")    /*rho_n   */
	moptimize_init_eq_indepvars(Z,19,"")    /* size effect */

	moptimize_init_eq_indepvars(Z,20,"")	/* Block for Ackerberg effects which we are calling zeta */
	moptimize_init_eq_indepvars(Z,21,"")
	moptimize_init_eq_indepvars(Z,22,"")
	moptimize_init_eq_indepvars(Z,23,"")
	
	
	moptimize_init_eq_indepvars(Z,24,"")     /* ln(sdsta) */
	moptimize_init_eq_indepvars(Z,25,"")     /* ln(sdmar) */
	moptimize_init_eq_indepvars(Z,26,"")     /* ln(sdmod) */
	moptimize_init_eq_indepvars(Z,27,"")     /* alpha - model constant */

	moptimize_init_depvar(Z,1,"dln")
	moptimize_init_depvar(Z,2,"ln_swg")
	moptimize_init_depvar(Z,3,"lnews")
	moptimize_init_depvar(Z,4,"otherl")
	moptimize_init_depvar(Z,5,"nnews")
	moptimize_init_depvar(Z,6,"otherc")
	moptimize_init_depvar(Z,7,"l_ACS_HH")
	
	/* Other information for "dynamics" */
	
	st_view(lnewslnews=.,.,"lnewslnews")
	st_view(lnewsnnews=.,.,"lnewsnnews")
	st_view(nnewslnews=.,.,"nnewslnews")
	st_view(nnewsnnews=.,.,"nnewsnnews")
	st_view(siXlnln=.,.,"siXlnln")
	st_view(siXlnnn=.,.,"siXlnnn")
	st_view(siXnnln=.,.,"siXnnln")
	st_view(siXnnnn=.,.,"siXnnnn")
	st_view(lnewstot=.,.,"lnewstot")
	st_view(nnewstot=.,.,"nnewstot")
	st_view(si=.,.,"si")
	st_view(lsi=.,.,"lsi")
	st_view(l_ACS_HH=.,.,"l_ACS_HH")
	st_view(lnewsn=.,.,"lnewsn")
	st_view(otherln=.,.,"otherln")
	st_view(nnewsn=.,.,"nnewsn")
	st_view(othercn=.,.,"othercn")
	
	Dy=dynoInfoInit(lnewslnews, lnewsnnews, nnewslnews, nnewsnnews,
		lsi,siXlnln, siXlnnn,siXnnln, siXnnnn, lnewstot, nnewstot,l_ACS_HH,
		lnewsn,otherln,nnewsn,othercn)
	
	m=panelsetup(ma,1)
	moptimize_init_userinfo(Z,1,id)
	moptimize_init_userinfo(Z,2,mt)
	moptimize_init_userinfo(Z,3,m)
	moptimize_init_userinfo(Z,4,Dy)
	moptimize_init_userinfo(Z,5,si)
	
	/* User information to pack together */

	moptimize_evaluate(Z)
//	moptimize(Z)

	stata("set more off")
	
	bo=boDyno[1..23],sdmarDy,sdstaDy,sdmodDy,alphavDy
//	bo[1..4]=-0.5,0.5,0.5,0.5

	alginfo="mwg","d0","moptimize"
	b_start=amcmc(alginfo,&logLikelihood2DynaA(),bo,diag(abs(bo)/2),201,101,1,.4,arate=.,vals=.,lambda=.,.,Z)
	bo=b_start[rows(b_start),]
	drawsbo=b_start  // For the time being, just see what happens if we use the linear estimates. 
	mata matsave /user/mjbaker/TV/ad_sample/DynoStarts23 bo drawsbo vals arate, replace
end	
/* Let's quickly check and see how we do recovering shares at this point... */
mata
	mata matuse /user/mjbaker/TV/ad_sample/DynoStarts23, replace

	bo[,1::4]=bo[,1::4]:^2
	bo=bo[rows(bo),]
	
	st_view(ln_swg=.,.,"ln_swg")
	st_view(lnews=.,.,"lnews")
	st_view(otherl=.,.,"otherl")
	st_view(nnews=.,.,"nnews")
	st_view(otherc=.,.,"otherc")
	st_view(dln=.,.,"dln")

	xBpUE=dln:-  bo[1]:*ln_swg:*lnews:-
			  bo[2]:*ln_swg:*otherl-
			  bo[3]:*ln_swg:*nnews:-
			  bo[4]:*ln_swg:*otherc			/* These are "combined" XB+u1+u2+e errors */
			  
/* Below we will need the actual model error, so we need to extract this from the above */

/* Now, let's see if we can get shares back */

	timeslots=6
	lnewsLong=lnews
	otherlLong=otherl
	nnewsLong=nnews
	othercLong=otherc
	
	lnewsLong=colshape(lnewsLong,6)
	otherlLong=colshape(otherlLong,6)
	nnewsLong=colshape(nnewsLong,6)
	othercLong=colshape(othercLong,6)
	xBpUE=colshape(xBpUE,6)

	simSharesLong=J(rows(lnewsLong),cols(lnewsLong),.)
	simSharesLong2=J(rows(lnewsLong),cols(lnewsLong),.)
	simSharesLong3=J(rows(lnewsLong),cols(lnewsLong),.)
	simSharesLong4=J(rows(lnewsLong),cols(lnewsLong),.)
	marketIdLong=ma
	marketIdLong=colshape(marketIdLong,6)
	mLong=panelsetup(marketIdLong,1)
	

	for (i=1;i<=rows(mLong);i++){
		xBpUEp=panelsubmatrix(xBpUE,i,mLong)
		lnewsLongp=panelsubmatrix(lnewsLong,i,mLong)
		otherlLongp=panelsubmatrix(otherlLong,i,mLong)
		nnewsLongp=panelsubmatrix(nnewsLong,i,mLong)
		othercLongp=panelsubmatrix(othercLong,i,mLong)
		for (t=1;t<=timeslots;t++) {
			simSharesLong[mLong[i,1]::mLong[i,2],t]=
				esharesStable(xBpUEp[,t],lnewsLongp[,t],otherlLongp[,t],
				nnewsLongp[,t],othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
			simSharesLong2[mLong[i,1]::mLong[i,2],t]=
				eshares_up(xBpUEp[,t],lnewsLongp[,t],otherlLongp[,t],
				nnewsLongp[,t],othercLongp[,t],bo[1],bo[2],bo[3],bo[4])
			simSharesLong3[mLong[i,1]::mLong[i,2],t]=
				esharesMoreStable(xBpUEp[,t],lnewsLongp[,t],otherlLongp[,t],
				nnewsLongp[,t],othercLongp[,t],bo[1],bo[2],bo[3],bo[4])	
			simSharesLong4[mLong[i,1]::mLong[i,2],t]=
				esharesQuadPres(xBpUEp[,t],lnewsLongp[,t],otherlLongp[,t],
				nnewsLongp[,t],othercLongp[,t],bo[1],bo[2],bo[3],bo[4])					
	
		}
	}

	siLong=si
	siLong=colshape(siLong,6)

	
end
	save /user/mjbaker/TV/ad_sample/AveragedDataDyno.dta, replace
