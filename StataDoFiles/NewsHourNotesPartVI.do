
clear all
cd C:\Users\mjbaker\Documents\Github\NewsHour
use AveragedDataDyno.dta
set more off
set seed 8675309
capture gen game= (lnews | otherl) & lnpps!=.
bysort stationid: egen countl=total(lnews)
gen game2 = game
replace game2 = 0 if countl ==0
sort market stationid mt timeslot

gen useOrd =_n
tsset stationid timeslot
gen otherlPrime=lnews&game
gen lnewsPrime =otherl&game
gen nnewsPrime =nnews&game
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
gen nnewstotPrime=nnews*ln(1+totsnnews)
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

    st_view(lnewslnews=.,.,"lnewslnews")
    st_view(lnewsnnews=.,.,"lnewsnnews")
    st_view(nnewslnews=.,.,"nnewslnews")
    st_view(nnewsnnews=.,.,"nnewsnnews")
    st_view(lsi=.,.,"lsi")
    st_view(siXlnln=.,.,"siXlnln")
    st_view(siXlnnn=.,.,"siXlnnn")
    st_view(siXnnln=.,.,"siXnnln")
    st_view(siXnnnn=.,.,"siXnnnn")
    st_view(lnewstot=.,.,"lnewstot")
    st_view(nnewstot=.,.,"nnewstot")
    st_view(l_ACS_HH=.,.,"l_ACS_HH")
    st_view(nnewsn=.,.,"nnewsn")
    st_view(lnewsn=.,.,"lnewsn")
    st_view(otherln=.,.,"otherln")
    st_view(othercn=.,.,"othercn")
end

mata: 
    struct dynoInfo {
        real matrix lnewslnews, lnewsnnews, nnewslnews, nnewsnnews, lsi,
            siXlnln, siXlnnn, siXnnln, siXnnnn, lnewstot, nnewstot, l_ACS_HH,
            lnewsn,otherln,nnewsn,othercn
    }
    
    struct dynoInfo dynoInfoInit(real matrix lnewslnews, lnewsnnews, nnewslnews, nnewsnnews,
        lsi,siXlnln, siXlnnn,siXnnln, siXnnnn, lnewstot, nnewstot,l_ACS_HH,lnewsn,otherln,nnewsn,othercn)
    {
        struct dynoInfo scalar Dy
    
        Dy.lnewslnews=lnewslnews
        Dy.lnewsnnews=lnewsnnews
        Dy.nnewslnews=nnewslnews
        Dy.nnewsnnews=nnewsnnews
        Dy.lsi=lsi
        Dy.siXlnln=siXlnln
        Dy.siXlnnn=siXlnnn
        Dy.siXnnln=siXnnln
        Dy.siXnnnn=siXnnnn
        Dy.lnewstot=lnewstot
        Dy.nnewstot=nnewstot
        Dy.l_ACS_HH=l_ACS_HH
        Dy.lnewsn=lnewsn
        Dy.otherln=otherln
        Dy.nnewsn=nnewsn
        Dy.othercn=othercn
        return(Dy)
    }
end

mata:
    Dy=dynoInfoInit(lnewslnews, lnewsnnews, nnewslnews, nnewsnnews,
        lsi,siXlnln, siXlnnn,siXnnln, siXnnnn, lnewstot, nnewstot,l_ACS_HH,
        lnewsn,otherln,nnewsn,othercn)
end

mata:
   struct priceInfo {
        real matrix y,lnewst,otherlt,nnewst,lnviewt,l_ACS_HHt,id,mt,m
    }
    struct priceInfo priceInfoInit(y,lnewst,otherlt,nnewst,lnviewt,l_ACS_HHt,id,mt,m)
    {
        struct priceInfo scalar pInfo
    
        pInfo.y=y
        pInfo.lnewst=lnewst
        pInfo.otherlt=otherlt
        pInfo.nnewst=nnewst
        pInfo.lnviewt=lnviewt
        pInfo.id=id
        pInfo.mt=mt
        pInfo.m=m
        pInfo.l_ACS_HHt=l_ACS_HHt
    return(pInfo)
    }
end

mata: 
	mata matuse MataPriceData
	P=priceInfoInit(lnppst,lnewst,otherlt,nnewst,lnviewt,l_ACS_HHt,idt,mtt,mat)
end

mata:
	mata matuse gsAndus, replace
end

mata
	st_view(game=.,.,"game")
	st_view(stationid=.,.,"stationid")
	st_view(Mpop=.,.,"ACS_HH")
	st_view(si=.,.,"si")	
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

mata:
	GW=gsAndwsInit(NashW,Up,Upb,Upg,Uv,Uvg,Uvsi,Uvsg,Uvso,
		lnewsPrime,otherlPrime,nnewsPrime,lnewslnewsPrime,
		lnewsnnewsPrime,nnewslnewsPrime,nnewsnnewsPrime,lsiPrime,siXlnlnPrime,
		siXlnnnPrime,siXnnlnPrime,siXnnnnPrime,lnewstotPrime,nnewstotPrime,Uvre1,Uvre2,Upre1,Upre2,weights1,weights2,
		Uvre1g,Uvre2g,Upre1g,Upre2g,ln(Uvsi:*Mpop),l_ACS_HHPrime)	
end


mata:
void logLikelihoodPriceViewComplete(M,todo,b,lnf,g,H)
{
	real scalar valPrice,valView,valSelect,mu_l,mu_o,
			mu_n,mu_c,eta_l,eta_o,
			eta_n,sd_stav,sd_marv,sd_modv,
			alphav,nu_l,nu_o,nu_n,gamma_l,gamma_o,
			sd_stap,sd_marp,sd_modp,alphap

	struct dynoInfo Dy
	struct priceInfo scalar P
	struct gsAndws scalar GW
	
	mu_l=(moptimize_util_xb(M,b,1))^2
	mu_o=(moptimize_util_xb(M,b,2))^2
	mu_n=(moptimize_util_xb(M,b,3))^2
	mu_c=(moptimize_util_xb(M,b,4))^2
	eta_l=moptimize_util_xb(M,b,5)
	eta_o=moptimize_util_xb(M,b,6)
	eta_n=moptimize_util_xb(M,b,7)
	eta_ll=moptimize_util_xb(M,b,8)
	eta_ln=moptimize_util_xb(M,b,9)
	eta_nl=moptimize_util_xb(M,b,10)
	eta_nn=moptimize_util_xb(M,b,11)
	lam_own=moptimize_util_xb(M,b,12)
	lam_ll=moptimize_util_xb(M,b,13)
	lam_ln=moptimize_util_xb(M,b,14)
	lam_nl=moptimize_util_xb(M,b,15)
	lam_nn=moptimize_util_xb(M,b,16)
	rho_l=moptimize_util_xb(M,b,17)
	rho_n=moptimize_util_xb(M,b,18)
	omega=moptimize_util_xb(M,b,19)
	zeta_l=moptimize_util_xb(M,b,20)
	zeta_o=moptimize_util_xb(M,b,21)
	zeta_n=moptimize_util_xb(M,b,22)
	zeta_c=moptimize_util_xb(M,b,23)
	sd_stav=exp(moptimize_util_xb(M,b,24))
	sd_marv=exp(moptimize_util_xb(M,b,25))
	sd_modv=exp(moptimize_util_xb(M,b,26))	
	alphav=moptimize_util_xb(M,b,27)	
	nu_l=moptimize_util_xb(M,b,28)
	nu_o=moptimize_util_xb(M,b,29)
	nu_n=moptimize_util_xb(M,b,30)
	gamma_l=moptimize_util_xb(M,b,31)
	gamma_o=moptimize_util_xb(M,b,32)
	omegap=moptimize_util_xb(M,b,33)
	sd_stap=exp(moptimize_util_xb(M,b,34))
	sd_marp=exp(moptimize_util_xb(M,b,35))
	sd_modp=exp(moptimize_util_xb(M,b,36))
	alphap=moptimize_util_xb(M,b,37)	
	
	id=moptimize_util_userinfo(M,1)
	mt=moptimize_util_userinfo(M,2)
	m =moptimize_util_userinfo(M,3)
	Dy =moptimize_util_userinfo(M,4)
	P=moptimize_util_userinfo(M,5)
	GW=moptimize_util_userinfo(M,6)
	si=moptimize_util_userinfo(M,7)
	
	y    =moptimize_util_depvar(M,1)
	lnswg=moptimize_util_depvar(M,2)
	lnews=moptimize_util_depvar(M,3)
	otherl=moptimize_util_depvar(M,4)
	nnews=moptimize_util_depvar(M,5)
	otherc=moptimize_util_depvar(M,6)
	
	valPrice=logLikelihoodPriceVal(P.y,P.lnewst,P.otherlt,P.nnewst,P.lnviewt,P.l_ACS_HHt,P.id,P.mt,P.m,
		nu_l,nu_o,nu_n,gamma_l,gamma_o,omegap,sd_stap,sd_marp,sd_modp,alphap)
	valView =logLikelihoodViewValDyna(y,lnswg,lnews,otherl,nnews,otherc,id,mt,m,
		mu_l,mu_o,mu_n,mu_c,eta_l,eta_o,eta_n,eta_ll,eta_ln,eta_nl,eta_nn,lam_own,lam_ll,lam_ln,
		lam_nl,lam_nn,rho_l,rho_n,omega,zeta_l,zeta_o,zeta_n,zeta_c,sd_stav,sd_marv,sd_modv,alphav,Dy,si)
	
	betaV=eta_l,eta_o,eta_n,
		eta_ll,eta_ln,eta_nl,eta_nn,lam_own,lam_ll,
		lam_ln,lam_nl,lam_nn,rho_l,rho_n,omega,alphav
		
	mus=mu_l,mu_o,mu_n,mu_c

	/* Now just need to use the other stuff. */
	
	betaP=nu_l,nu_o,nu_n,gamma_l,gamma_o,omegap,alphap
	
	valSelect=selectStuff(mus,betaV,betaP,sd_stav,sd_marv,sd_modv,sd_stap,sd_marp,sd_modp,GW)
	lnf=valPrice+valView+valSelect
	if (runiform(1,1)>.95) {
		lnf,valSelect
		mus
		betaV
		betaP
		sd_stav,sd_marv,sd_modv
		sd_stap,sd_marp,sd_modp
	}
}
end

mata
real scalar logLikelihoodPriceVal(y,lnewst,otherlt,nnewst,lnviewt,l_ACS_HH,id,mt,m,
				  nu_l,nu_o,nu_n,gamma_l,gamma_o,omegap,sdstap,
				  sdmarp,sdmodp,alphap)
{
	real scalar i,T,N,lam1,lam2,lam3,lam4,phi22,phi32
	real matrix X,mtp,idp,yp,Xp,Jn,Jt,En,Et,
		Q1,Q2,Q3,Q4,OmegaInv,beta

	beta=nu_l,nu_o,nu_n,gamma_l,gamma_o,omegap,alphap

	X=lnewst:*lnviewt,otherlt:*lnviewt,nnewst:*lnviewt,
		lnewst,otherlt,l_ACS_HH,J(rows(lnewst),1,1)

	lnf=J(rows(m),1,.)
	for (i=1;i<=rows(m);i++) {
		mtp=panelsubmatrix(mt,i,m)
		idp=panelsubmatrix(id,i,m)
		T=rows(uniqrows(mtp))
		N=rows(uniqrows(idp))
		yp=panelsubmatrix(y,i,m)
		Xp=panelsubmatrix(X,i,m)
	
		lam1=sdmodp^2
		lam2=T*sdstap^2+sdmodp^2
		lam3=N*sdmarp^2+sdmodp^2
		lam4=T*sdstap^2+N*sdmarp^2+sdmodp^2
		phi22=sdmodp^2/lam2
		phi32=sdmodp^2/lam3
		phi42=sdmodp^2/lam4

		Jn=J(N,N,1/N)
		Jt=J(T,T,1/T)
		En=I(N)-Jn
		Et=I(T)-Jt

		Q1=En#Et
		Q2=En#Jt
		Q3=Jn#Et
		Q4=Jn#Jt
		OmegaInv=(Q1/lam1+Q2/lam2+Q3/lam3+Q4/lam4)
		lnDetOmega=-2*N*T*ln(sdmodp)+(N-1)*ln(phi22)+(T-1)*ln(phi32)+ln(phi42)
		lnf[i]=1/2*lnDetOmega-1/2*(yp-Xp*beta')'*OmegaInv*(yp-Xp*beta')
	}
	
	if (hasmissing(lnf)) lnf=.
	else lnf=colsum(lnf)
	return(lnf)
}
end

mata:
real scalar logLikelihoodViewValDyna(y,lnswg,lnews,otherl,nnews,otherc,id,mt,m,
				 mu_l,mu_o,mu_n,mu_c,eta_l,eta_o,eta_n,eta_ll,eta_ln,
				 eta_nl,eta_nn,lam_own,lam_ll,lam_ln,lam_nl,lam_nn,
				 rho_l,rho_n,omega,zeta_l,zeta_o,zeta_n,zeta_c,sdsta,sdmar,sdmod,alpha,struct dynoInfo Dy, 
				 si)
{
	real scalar i,T,N,lam1,lam2,lam3,lam4,phi22,phi32
	real matrix X,mtp,idp,yp,Xp,Jn,Jt,En,Et,
		Q1,Q2,Q3,Q4,OmegaInv,beta,lnDefF,muVec,sip,Bip,
		siz,Biz,TimeVars,sg,Ng
	
	beta=mu_l,mu_o,mu_n,mu_c,eta_l,eta_o,eta_n,eta_ll,eta_ln,
		eta_nl,eta_nn,lam_own,lam_ll,lam_ln,lam_nl,lam_nn,
		rho_l,rho_n,omega,zeta_l,zeta_o,zeta_n,zeta_c,alpha

	X=lnews:*lnswg,otherl:*lnswg,nnews:*lnswg,otherc:*lnswg,
		lnews,otherl,nnews,Dy.lnewslnews,Dy.lnewsnnews,Dy.nnewslnews,
		Dy.nnewsnnews,Dy.lsi,Dy.siXlnln,Dy.siXlnnn,Dy.siXnnln,Dy.siXnnnn,
		Dy.lnewstot,Dy.nnewstot,Dy.l_ACS_HH,Dy.lnewsn,Dy.otherln,Dy.nnewsn,Dy.othercn,J(rows(lnews),1,1)

	B=lnews,otherl,nnews,otherc

	lnf=J(rows(m),1,.)
	lnDetF=J(rows(m),6,.)
	muVec=mu_l,mu_o,mu_n,mu_c
	
	
	for (i=1;i<=rows(m);i++) {
		mtp=panelsubmatrix(mt,i,m)
		idp=panelsubmatrix(id,i,m)
		T=rows(uniqrows(mtp))
		N=rows(uniqrows(idp))
		yp=panelsubmatrix(y,i,m)
		Xp=panelsubmatrix(X,i,m)
		sip=panelsubmatrix(si,i,m)
		Bp=panelsubmatrix(B,i,m)
		
		lam1=sdmod^2
		lam2=T*sdsta^2+sdmod^2
		lam3=N*sdmar^2+sdmod^2
		lam4=T*sdsta^2+N*sdmar^2+sdmod^2
		phi22=sdmod^2/lam2
		phi32=sdmod^2/lam3
		phi42=sdmod^2/lam4

		Jn=J(N,N,1/N)
		Jt=J(T,T,1/T)
		En=I(N)-Jn
		Et=I(T)-Jt

		Q1=En#Et
		Q2=En#Jt
		Q3=Jn#Et
		Q4=Jn#Jt
		OmegaInv=(Q1/lam1+Q2/lam2+Q3/lam3+Q4/lam4)
		lnDetOmega=-2*N*T*ln(sdmod)+(N-1)*ln(phi22)+(T-1)*ln(phi32)+ln(phi42)
		lnf[i]=1/2*lnDetOmega-1/2*(yp-Xp*beta')'*OmegaInv*(yp-Xp*beta')
		
		TimeVars=uniqrows(mtp)	

		for (z=1;z<=rows(TimeVars);z++) {
			siz=select(sip,mtp:==TimeVars[z])
			Bz=select(Bp,mtp:==TimeVars[z])
			sizBz=siz:*Bz
			sg=colsum(sizBz)
			Ng=colsum(sizBz:!=0)
	
			lnDetF[i,z]=rowsum((Ng:>0):*(Ng:-1):*ln(1:-muVec))
		}			
		
	}
	if (hasmissing(lnf)) lnf=.
	else if (hasmissing(lnDetF)) lnf=.
	else lnf=colsum(lnf) +colsum(rowsum(lnDetF))
	return(lnf)
}
end

mata:
real scalar selectStuff(mus,betaV,betaP,sd_stav,sd_marv,sd_modv,sd_stap,sd_marp,sd_modp,struct gsAndws GW)
{

	Xv=GW.lnewsPrime,GW.otherlPrime,GW.nnewsPrime,
		GW.lnewslnewsPrime,GW.lnewsnnewsPrime,GW.nnewslnewsPrime,
		GW.nnewsnnewsPrime,GW.lsiPrime,GW.siXlnlnPrime,GW.siXlnnnPrime,
		GW.siXnnlnPrime,GW.siXnnnnPrime,GW.lnewstotPrime,GW.nnewstotPrime,GW.l_ACS_HH,
		J(rows(GW.lnewsPrime),1,1)


	mug=GW.lnewsPrime:*mus[1]:+GW.otherlPrime:*mus[2]:+GW.nnewsPrime:*mus[3]:+
		(1:-GW.lnewsPrime:-GW.otherlPrime:-GW.nnewsPrime):*mus[4]

	lnsig=ln(GW.Uvsi:/GW.Uvsg)	
	
	lnvMg=lnnd(GW.Uv,Xv*betaV':+GW.Uvre1:+GW.Uvre2,ln(sd_modv)):-GW.Uvg:-ln(1:/(1:-mug):-mug:*GW.Uvsi:/GW.Uvsg:-GW.Uvsi):-
		GW.Uvre1g:-GW.Uvre2g:+GW.weights1:*lnnd(GW.Uvre1,0,ln(sd_stav)):+GW.weights2:*lnnd(GW.Uvre2,0,ln(sd_marv))	
		     /* This might be wrong - no need for lnsig */

	betaXp=J(rows(GW.Uv),cols(GW.Uv),.)
	for (s=1;s<=cols(GW.Uv);s++) {
		Xp=GW.lnewsPrime:*GW.lnview[,s],GW.otherlPrime:*GW.lnview[,s],GW.nnewsPrime:*GW.lnview[,s],
			GW.lnewsPrime,GW.otherlPrime,GW.l_ACS_HH,J(rows(GW.lnewsPrime),1,1)
		betaXp[,s]=Xp*betaP'
	}

	lnpMg=lnnd(GW.Up,betaXp:+GW.Upre1:+GW.Upre2,ln(sd_modp)):-ln(normal((GW.Upb:-betaXp:-GW.Upre1:-GW.Upre2):/sd_modp)):-GW.Upg:-GW.NashW:-GW.Upre1g:-	/* Changed to a plus, probably wrongly */
		GW.Upre2g +GW.weights1:*lnnd(GW.Upre1,0,ln(sd_stap)):+GW.weights2:*lnnd(GW.Upre2,0,ln(sd_marp))	/* Question is whether or not GW.Upg contains truncation information or not */

	maxP=rowmax(lnpMg)
	maxV=rowmax(lnvMg)

	lnp=ln(rowsum(exp(lnpMg:-maxP))):+maxP:-ln(cols(lnpMg))		
	lnv=ln(rowsum(exp(lnvMg:-maxV))):+maxV:-ln(cols(lnvMg))

	//if (hasmissing(lnp)) colmissing(lnp)
//	if (hasmissing(lnv)) printf("Danger!\n")
	return(colsum(lnv):+colsum(lnp))
}
end

do MataFunctions\lnnd.do

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
	
	moptimize_init_depvar(Z,1,"dln")
	moptimize_init_depvar(Z,2,"ln_swg")
	moptimize_init_depvar(Z,3,"lnews")
	moptimize_init_depvar(Z,4,"otherl")
	moptimize_init_depvar(Z,5,"nnews")
	moptimize_init_depvar(Z,6,"otherc")

	st_view(ma=.,.,"market")
	st_view(id=.,.,"stationid")
	st_view(mt=.,.,"mt")
	
	m=panelsetup(ma,1)
	moptimize_init_userinfo(Z,1,id)
	moptimize_init_userinfo(Z,2,mt)
	moptimize_init_userinfo(Z,3,m)
	moptimize_init_userinfo(Z,4,Dy)
	moptimize_init_userinfo(Z,5,P)
	moptimize_init_userinfo(Z,6,GW)
	moptimize_init_userinfo(Z,7,si)
	moptimize_evaluate(Z)
	mata matuse betaPDynoStarts
	mata matuse DynoStarts23
	
	bv=bo,bpo
	Vstart=diag(sqrt(abs(bv)))
	
	alginfo="mwg","d0","moptimize"
	b_start=amcmc(alginfo,&logLikelihoodPriceViewComplete(),bv,Vstart,401,1,1,.4,arate=.,vals=.,lambda=.,.,Z,"noisy")

	mata matsave /user/mjbaker/TV/ad_sample/Results b_start, replace
end	


