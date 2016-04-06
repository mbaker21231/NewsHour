// Present estimation results from the two models.

use "/user/mjbaker/TV/ad_sample/AveragedData.dta", clear
set more off

cd "/user/mjbaker/TV/ad_sample/TablesFigures"

gen keeper1=lnpps!=.
bysort market: egen keeper2=total(keeper1)
keep if keeper2>1

mata
//	mata matuse /user/mjbaker/TV/ad_sample/DynoStarts22, replace
	mata matuse /user/mjbaker/TV/ad_sample/betaPDynoStarts, replace
	mata matuse /user/mjbaker/TV/ad_sample/DynoStarts23, replace
end

capture program drop et
prog def et, eclass
	if replay() {
		syntax [anything] [EForm(string) Level(real 95) ]
		eret di, eform(`eform') level(`level')
	}
	else {
		args b V N_total N_markets N_statns N_lnews N_otherl N_nnews
		local cn mu_l mu_o mu_n mu_c local_news local_other nat_news b2b_ll b2b_ln b2b_nl b2b_nn l_share l_share_ll l_share_ln l_share_nl l_share_nn cum_ln cum_nn size_v zeta_l zeta_o zeta_n zeta_c alphav sd_Vsta sd_Vmt sd_Vmod nu_l nu_o nu_n gamma_l gamma_o size_p alphap sd_Psta sd_Pmt sd_Pmod 
		local ce Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership Viewership RE_v RE_v RE_v Revenue Revenue Revenue Revenue Revenue Revenue Revenue Re_p Re_p Re_p
		
		mat colnames `b'=`cn'
		mat colnames `V'=`cn'
		mat rownames `V'=`cn'
		mat coleq `b'=`ce'
		mat roweq `V'=`ce'
		mat coleq `V'=`ce'

		ereturn post b V
		ereturn local cmd "et"
		ereturn local properties "b V"
		ereturn scalar N_total=N_total
		ereturn scalar N_markets=N_markets
		ereturn scalar N_statns=N_statns
		ereturn scalar N_lnews=N_lnews
		ereturn scalar N_otherl=N_otherl
		ereturn scalar N_nnews=N_nnews
		ereturn display
	}
end

/* Some variables for summary stats */

	sum si
	scalar N_total=r(N)
	quietly tab market
	scalar N_markets=r(r)

	gen foo=lnpps!=.
	quietly tab foo if foo==1 & timeslot==6
	scalar N_statns=r(N)
	
	quietly tab lnews if lnews==1
	scalar N_lnews=r(N)
	quietly tab otherl if otherl==1
	scalar N_otherl=r(N)
	quietly tab nnews if nnews==1
	scalar N_nnews=r(N)
	
mata: 
	b_start=drawsbo,drawsbpo[101::200,]						/* Have to conform! */
	b_start[,1::4]=b_start[,1::4]:^2
	b_start[,(24,25,26,34,35,36)]=exp(b_start[,(24,25,26,34,35,36)])

/* Ordering variable to get all the data in the right order for presentation */

	ord=1..23,27,24..26,28..33,37,34..36
	b_start=b_start[,ord]


	b=mean(b_start)									/* Are we actually presenting what we used for simulation? */
	V=variance(b_start)
	st_matrix("b",b)
	st_matrix("V",V)
end	
	et b V
	
est store eq_by_eq_ML

mata: 
mata matuse /user/mjbaker/TV/ad_sample/Results, replace
	b_start[,1::4]=b_start[,1::4]:^2
	b_start[,(24,25,26,34,35,36)]=exp(b_start[,(24,25,26,34,35,36)])
	b_start=b_start[,ord]
	
	b=mean(b_start)
	V=variance(b_start)
	st_matrix("b",b)
	st_matrix("V",V)
end
	et b V
	
est store completemodel

esttab eq_by_eq_ML completemodel using EstimationResults, wide  se compress ///
	scalars("N_total Total obs." "N_markets Markets" "N_statns Stations" "N_lnews Local news" "N_otherl Other local" "N_nnews Nat'l news") ///
	coeflabels(mu_l "Mu: local news" mu_o "Mu: other local" mu_n "Mu: national news" ///
	mu_c "Mu: other cable" local_news "Local news?" local_other "Local other?" ///
	nat_news "National news?" l_share "share(t-1)" b2b_ll "loc. news(t),loc. news(t-1)" ///
	b2b_ln "loc. news(t),nat. news(t-1)" b2b_nl "nat. news(t),loc. news(t-1)" ///
	b2b_nn "nat. news(t),nat. news(t-1)" eta_l "Local news" eta_o "Other local" eta_n "National news" ///
	l_share_ll "shares: loc. news(t),loc. news(t-1)"	///
	l_share_ln "shares: loc. news(t),nat. news(t-1)"	///
	l_share_nl "shares: nat. news(t),loc. news(t-1)"	///
	l_share_nn "shares: nat. news(t),nat. news(t-1)"	///
	cum_ln "Cum. share local news" cum_nn "Cum. share nat. news" alphav "constant" alphap "constant" ///
	sd_Vsta "Viewership: log(sd) station RE" sd_Vmt "Viewership: log(sd) market RE" sd_Vmod "Viewership: log(sd) model" ///
	sd_Psta "Revenue: log(sd) station RE" sd_Pmt "Revenue: log(sd) market RE" sd_Pmod "Revenue: log(sd) model" ///
	nu_l "Local news viewer elast." nu_o "Other viewer elast." nu_n "Nat. news viewer elast." ///
	size_v "Market Size" size_p "Market Size" zeta_l "ln(1+no. loc. news)" zeta_o "ln(1+no. other)" zeta_n "ln(1+no. nat. news)" zeta_c "ln(1+other cable)" ///
	gamma_l "Local news?" gamma_o "Other local?") tex nonum replace ///
	mtitles("Eq-by-Eq ML" "Full Model") cells("b(fmt(3)) se(fmt(3))") ///
	fragment prehead(\begin{longtable}{lcccc}\label{estimationResults}) postfoot(\caption{Estimation results: starting values and results from full model}\end{longtable}) 

/* Estimation results - a new, fancier table */

capture program drop etview
prog def etview, eclass
	if replay() {
		syntax [anything] [EForm(string) Level(real 95) ]
		eret di, eform(`eform') level(`level')
	}
	else {
		args bv Vv 
		local cn mu_l       mu_o         mu_n         mu_c         local_news local_other nat_news b2b_ll   b2b_ln   b2b_nl   b2b_nn   l_share  l_share_ll l_share_ln l_share_nl l_share_nn cum_ln        cum_nn        size_v   zeta_l   zeta_o  zeta_n   zeta_c   alphav   sd_Vsta sd_Vmt sd_Vmod  
		local ce Substitution Substitution Substitution Substitution Program_Type Program_Type Program_Type     Dynamics Dynamics Dynamics Dynamics Dynamics Dynamics   Dynamics   Dynamics   Dynamics   Breaking_News Breaking_News Controls Controls Controls Controls Controls Controls Rand_FX Rand_FX Rand_FX 
		mat colnames `bv'=`cn'
		mat colnames `Vv'=`cn'
		mat rownames `Vv'=`cn'
		mat coleq `bv'=`ce'
		mat roweq `Vv'=`ce'
		mat coleq `Vv'=`ce'

		ereturn post bv Vv
		ereturn local cmd "etview"
		ereturn local properties "b V"
		ereturn display
	}
end
capture program drop etprice
prog def etprice, eclass
	if replay() {
		syntax [anything] [EForm(string) Level(real 95) ]
		eret di, eform(`eform') level(`level')
	}
	else {
		args bp Vp N_total N_markets N_statns N_lnews N_otherl N_nnews
		local cn nu_l nu_o nu_n gamma_l gamma_o size_p alphap sd_Psta sd_Pmt sd_Pmod 
		local ce Revenue Revenue Revenue Revenue Revenue Revenue Revenue Rand_FX Rand_FX Rand_FX
		
		mat colnames `bp'=`cn'
		mat colnames `Vp'=`cn'
		mat rownames `Vp'=`cn'
		mat coleq `bp'=`ce'
		mat roweq `Vp'=`ce'
		mat coleq `Vp'=`ce'

		ereturn post bp Vp
		ereturn local cmd "et"
		ereturn local properties "b V"
		ereturn scalar N_total=N_total
		ereturn scalar N_markets=N_markets
		ereturn scalar N_statns=N_statns
		ereturn scalar N_lnews=N_lnews
		ereturn scalar N_otherl=N_otherl
		ereturn scalar N_nnews=N_nnews
		ereturn display
	}
end

mata
	mata matuse /user/mjbaker/TV/ad_sample/betaPDynoStarts, replace
	mata matuse /user/mjbaker/TV/ad_sample/DynoStarts23, replace
	b_start=drawsbo,drawsbpo[101::200,]						/* Have to conform! */
	b_start[,1::4]=b_start[,1::4]:^2
	b_start[,(24,25,26,34,35,36)]=exp(b_start[,(24,25,26,34,35,36)])

/* Ordering variable to get all the data in the right order for presentation */

	ord=1..23,27,24..26,28..33,37,34..36
	b_start=b_start[,ord]
	b=mean(b_start)									/* Are we actually presenting what we used for simulation? */
	V=variance(b_start)
end	

mata: 
	bv=b[1..27]
	Vv=V[1..27,1..27]
	bp=b[28..37]
	Vp=V[28..37,28..37]
	st_matrix("bv",bv)
	st_matrix("Vv",Vv)
	st_matrix("bp",bp)
	st_matrix("Vp",Vp)
end 
	etview bv Vv
	est store mlview
	etprice bp Vp
	est store mlprice
mata
mata matuse /user/mjbaker/TV/ad_sample/Results, replace
	b_start[,1::4]=b_start[,1::4]:^2
	b_start[,(24,25,26,34,35,36)]=exp(b_start[,(24,25,26,34,35,36)])
	b_start=b_start[,ord]
	
	b=mean(b_start)
	V=variance(b_start)
	st_matrix("b",b)
	st_matrix("V",V)
end
mata: 
	bv=b[1..27]
	Vv=V[1..27,1..27]
	bp=b[28..37]
	Vp=V[28..37,28..37]
	st_matrix("bv",bv)
	st_matrix("Vv",Vv)
	st_matrix("bp",bp)
	st_matrix("Vp",Vp)
end	

	etview bv Vv
	est store fullview
	etprice bp Vp
	est store fullprice

esttab fullview mlview using EstimationResultsFancy, wide  se compress ///
	coeflabels(mu_l "Local news" mu_o "Entertainment" mu_n "National news" ///
	mu_c "Other cable" local_news "Local news dummy" local_other "Entertainment dummy" ///
	nat_news "National news dummy" l_share "share prev. timeslot" b2b_ll "loc. news after loc. news" ///
	b2b_ln "loc. news after nat. news" b2b_nl "nat. news after loc. news" ///
	b2b_nn "nat. news after nat. news" eta_l "Local news" eta_o "Entertainment" eta_n "National news" ///
	l_share_ll "loc. news after loc. news X share"	///
	l_share_ln "loc. news after nat. news X share"	///
	l_share_nl "nat. news after loc. news X share"	///
	l_share_nn "nat. news after loc. news X share"	///
	cum_ln "Cum. share local news" cum_nn "Cum. share nat. news" alphav "constant" ///
	sd_Vsta "log(sd) station RE" sd_Vmt "log(sd) market RE" sd_Vmod "log(sd) model" ///
	size_v "Market Size" zeta_l "ln(1+no. loc. news)" zeta_o "ln(1+no. enter.)" zeta_n "ln(1+no. nat. news)" zeta_c "ln(1+other cable)") ///
	nonum replace starlevels(+ 0.05 * 0.01)  noobs ///
	mtitles("Eq-by-Eq ML" "Full Model") cells("b(star fmt(3)) se(fmt(3))") fragment prehead(\begin{longtable}{lcccc}\label{estimationResults})  tex 

esttab fullprice mlprice using EstimationResultsFancy, wide se compress ///
	scalars("N_total Total obs." "N_markets Markets" "N_statns Stations" "N_lnews Local news" "N_otherl Other local" "N_nnews Nat'l news") ///
	coeflabels( alphap "constant" ///
	sd_Psta "log(sd) station RE" sd_Pmt "log(sd) market RE" sd_Pmod "log(sd) model" ///
	nu_l "Local news viewer elast." nu_o "Entertainment viewer elast." nu_n "Nat. news viewer elast." ///
	size_p "Market Size" ///
	gamma_l "Local news dummy" gamma_o "Entertainment dummy") tex nonum append ///
	cells("b(star fmt(3)) se(fmt(3))") starlevels(+ 0.05 * 0.01) collabels("""""""") ///
	postfoot(\caption{Estimation results: starting values and results from full model}\end{longtable}) 









