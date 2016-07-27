// Fit a preliminary pricing model 

	clear all
	use "/user/mjbaker/TV/ad_sample/AveragedDataDyno.dta", clear
        do "/user/mjbaker/TV/ad_sample/AveragedDataDynamicMata.do"  /* Load Mata programs */	

	set more off
	sort market stationid mt
	
	gen lnview=ln(ACS_HH*si)			/* Corrected the mistake here - we use viewership with an adjustment factor */
	gen lnviewl=lnews*lnview
	gen lnviewo=otherl*lnview
	gen lnviewn=nnews*lnview
	
	keep if lnpps!=.

	a2reg lnpps lnviewl lnviewo lnviewn lnews otherl l_ACS_HH, individual(stationid) unit(mt) 
	
	mat sdmodp=e(rmse)
	mat sdstap=e(sdind)
	mat bop=e(b)
	mat sdmarp=e(sdunit)
	mat alphap=e(constant)

	mata:

	st_view(ma=.,.,"market")
	st_view(id=.,.,"stationid")
	st_view(mt=.,.,"mt")	
	
	bop=st_matrix("bop")

	sdmarp=st_matrix("sdmarp")
	sdstap=st_matrix("sdstap")
	sdmodp=st_matrix("sdmodp")
	alphap=st_matrix("alphap")	
	
	bop=bop,alphap
	
	Z=moptimize_init()
	moptimize_init_trace_dots(Z,"on")
	moptimize_init_trace_params(Z,"on")
	moptimize_init_evaluator(Z,&logLikelihoodPrice())			/* Note that this is different when using mcmc */
	moptimize_init_evaluatortype(Z,"d0")				/* Changed to a d0-type evaluator */
	moptimize_init_which(Z,"max")

	moptimize_init_eq_indepvars(Z,1,"")	/* nu_l */
	moptimize_init_eq_indepvars(Z,2,"")	/* nu_o */
	moptimize_init_eq_indepvars(Z,3,"")	/* nu_n */
	moptimize_init_eq_indepvars(Z,4,"")     /* gamma_l */
	moptimize_init_eq_indepvars(Z,5,"")     /* gamma_o */
	moptimize_init_eq_indepvars(Z,6,"")	/* omega_p */
	moptimize_init_eq_indepvars(Z,7,"")     /* ln(sdsta) */
	moptimize_init_eq_indepvars(Z,8,"")     /* ln(sdmar) */
	moptimize_init_eq_indepvars(Z,9,"")	/* ln(mod)   */
	moptimize_init_eq_indepvars(Z,10,"")	/* alphap    */
	
	moptimize_init_depvar(Z,1,"lnpps")
	moptimize_init_depvar(Z,2,"lnews")
	moptimize_init_depvar(Z,3,"otherl")
	moptimize_init_depvar(Z,4,"nnews")
	moptimize_init_depvar(Z,5,"lnview")
	moptimize_init_depvar(Z,6,"l_ACS_HH")
	
	m=panelsetup(ma,1)
	moptimize_init_userinfo(Z,1,id)
	moptimize_init_userinfo(Z,2,mt)
	moptimize_init_userinfo(Z,3,m)
	
	/* User information to pack together */

	moptimize_evaluate(Z)
	end
	set more off
	mata
	boinit=st_matrix("bop"),sdmarp,sdstap,sdmodp,alphap

	alginfo="mwg","d0","moptimize"
	bp_start=amcmc(alginfo,&logLikelihoodPrice(),boinit,I(10),80,20,2/3,.4,arate=.,vals=.,lambda=.,.,Z,"noisy")
	bop=bp_start[rows(bp_start),]

	Z1=moptimize_init()
	moptimize_init_trace_dots(Z1,"on")
	moptimize_init_trace_params(Z1,"on")
	moptimize_init_evaluator(Z1,&logLikelihoodPrice())		/* Note that this is different when using mcmc */
	moptimize_init_evaluatortype(Z1,"d0")				/* Changed to a d0-type evaluator */
	moptimize_init_which(Z1,"max")

	moptimize_init_eq_indepvars(Z1,1,"")	/* nu_l */
	moptimize_init_eq_indepvars(Z1,2,"")	/* nu_o */
	moptimize_init_eq_indepvars(Z1,3,"")	/* nu_n */
	moptimize_init_eq_indepvars(Z1,4,"")     /* eta_l */
	moptimize_init_eq_indepvars(Z1,5,"")     /* eta_o */
	moptimize_init_eq_indepvars(Z1,6,"")	/* eta_n */
	moptimize_init_eq_indepvars(Z1,7,"")     /* omega */
	moptimize_init_eq_indepvars(Z1,8,"")     /* ln(sdsta) */
	moptimize_init_eq_indepvars(Z1,9,"")     /* ln(sdmar) */
	moptimize_init_eq_indepvars(Z1,10,"")	 /* ln(sdmod) */
	
	moptimize_init_depvar(Z1,1,"lnpps")
	moptimize_init_depvar(Z1,2,"lnews")
	moptimize_init_depvar(Z1,3,"otherl")
	moptimize_init_depvar(Z1,4,"nnews")
	moptimize_init_depvar(Z1,5,"lnview")
	moptimize_init_depvar(Z1,6,"l_ACS_HH")
	
	m=panelsetup(ma,1)
	moptimize_init_userinfo(Z1,1,id)
	moptimize_init_userinfo(Z1,2,mt)
	moptimize_init_userinfo(Z1,3,m)
	
	moptimize_init_eq_coefs(Z1,1,bop[1])
	moptimize_init_eq_coefs(Z1,2,bop[2])
	moptimize_init_eq_coefs(Z1,3,bop[3])
	moptimize_init_eq_coefs(Z1,4,bop[4])
	
	moptimize_init_eq_coefs(Z1,5,bop[5])
	moptimize_init_eq_coefs(Z1,6,bop[6])
	moptimize_init_eq_coefs(Z1,7,bop[7])
	moptimize_init_eq_coefs(Z1,8,bop[8])
	moptimize_init_eq_coefs(Z1,9,bop[9])
	moptimize_init_eq_coefs(Z1,10,bop[10])
	moptimize(Z1)
	bpo=moptimize_result_coefs(Z1)
end
	/* rename the data so we don't overwrite data we are using for other stuff */
	/* the "t" is a mnemonic for "truncated" */
mata	
	st_view(lnppst=.,.,"lnpps")
	st_view(lnewst=.,.,"lnews")
	st_view(otherlt=.,.,"otherl")
	st_view(nnewst=.,.,"nnews")
	st_view(lnviewt=.,.,"lnview")
	st_view(l_ACS_HHt=.,.,"l_ACS_HH")

		mat=m
		idt=id
		mtt=mt

	/* Save results and data */
	
	alginfo="mwg","d0","moptimize"
	b_start=amcmc(alginfo,&logLikelihoodPrice(),bpo,diag(abs(bpo)/4),220,20,1,.4,arate=.,vals=.,lambda=.,.,Z,"noisy")
	bpo=b_start[rows(b_start),]
	drawsbpo=b_start
	
	mata matsave /user/mjbaker/TV/ad_sample/betaPDynoStarts bpo drawsbpo,replace
	mata matsave /user/mjbaker/TV/ad_sample/MataPriceData lnppst lnewst otherlt nnewst lnviewt l_ACS_HHt mat idt mtt,replace
	
	end
	exit, clear
	
