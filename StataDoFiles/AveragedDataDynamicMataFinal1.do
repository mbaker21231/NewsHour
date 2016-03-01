mata:
real matrix paneldraw(real matrix id, real scalar draws)
{
	rowsToDraw=uniqrows(id)

	U=J(rows(id),draws,.)
	for (i=1;i<=rows(rowsToDraw);i++) {
		obs=mm_which(rowsToDraw[i]:==id)
		D=runiform(1,draws)
		U[obs,]=J(rows(obs),1,D)
	}
	return(U)
}
real matrix resampler(real rowvector probs, real scalar sampsize) {
	cutter=runningsum(probs)
	samplePos=J(1,sampsize,.)
	for (i=1;i<=length(samplePos);i++) {
		draw=runiform(1,1)
		samplePos[i]=min(mm_which((cutter:-draw):>0))
	}
	return(samplePos)
}
real matrix indEffect3(real matrix mar, real matrix mt,real matrix id,
			real matrix ymXBhat, real scalar sdmod,
			real scalar sdsta, real scalar sdmar)
{
	/* Data must be sorted by market-time, and then by id */

	real scalar i,lam1,lam2,lam3,lam4,N,T
	real matrix Q1,Q2,Q3,Q4,En,Et,Jn,Jt,
		mtp,idp,Zs,Zm,ymXBhatp,Vu,VuInv

	m=panelsetup(mar,1)

	alphaS=J(rows(mt),1,.)
	alphaM=J(rows(mt),1,.)

	for (i=1;i<=rows(m);i++) {
		mtp=panelsubmatrix(mt,i,m)
		idp=panelsubmatrix(id,i,m)
		T=rows(uniqrows(mtp))
		N=rows(uniqrows(idp))
		ymXBhatp=panelsubmatrix(ymXBhat,i,m)
	
		Zs=I(N)#J(T,1,1)
		Zm=J(N,1,1)#I(T)
		
		Vu=I(N*T)+sdsta^2/sdmod^2*Zs*Zs'+sdmar^2/sdmod^2*Zm*Zm'
		VuInv=invsym(Vu)
		
		alphaS[m[i,1]::m[i,2],1]=(sdsta^2/sdmod^2*Zs'VuInv*ymXBhatp)#J(T,1,1)
		alphaM[m[i,1]::m[i,2],1]=J(N,1,1)#(sdmar^2/sdmod^2*Zm'VuInv*ymXBhatp)	
	}

	return((alphaS,alphaM))
}
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
void logLikelihood2DynaA(M,todo,b,lnf,g,H)
{
	real scalar mu_l,mu_o,mu_n,mu_c,eta_l,eta_n,
		sdsta,sdmar,sdmod,i,T,N,lam1,
		lam2,lam3,lam4,phi22,phi32
	real matrix X,mt,id,mtp,idp,yp,Xp,Jn,Jt,En,Et,
		Q1,Q2,Q3,Q4,OmegaInv,beta
	struct dynoInfo scalar Dy
	 		
	mu_l =(moptimize_util_xb(M,b,1))^2
	mu_o =(moptimize_util_xb(M,b,2))^2
	mu_n =(moptimize_util_xb(M,b,3))^2
	mu_c =(moptimize_util_xb(M,b,4))^2
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
	sdsta=exp(moptimize_util_xb(M,b,24))
	sdmar=exp(moptimize_util_xb(M,b,25))
	sdmod=exp(moptimize_util_xb(M,b,26))
	
	alpha=moptimize_util_xb(M,b,27)

	y     =moptimize_util_depvar(M,1)
	lnswg =moptimize_util_depvar(M,2)
	lnews =moptimize_util_depvar(M,3)
	otherl=moptimize_util_depvar(M,4)
	nnews =moptimize_util_depvar(M,5)
	otherc=moptimize_util_depvar(M,6)
	l_ACS_HH=moptimize_util_depvar(M,7)

	id=moptimize_util_userinfo(M,1)
	mt=moptimize_util_userinfo(M,2)
	m =moptimize_util_userinfo(M,3)
	Dy =moptimize_util_userinfo(M,4)
	si =moptimize_util_userinfo(M,5)

	beta=mu_l,mu_o,mu_n,mu_c,eta_l,eta_o,eta_n,
		eta_ll,eta_ln,eta_nl,eta_nn,lam_own,lam_ll,
		lam_ln,lam_nl,lam_nn,rho_l,rho_n,omega,zeta_l,zeta_o,zeta_n,zeta_c,alpha
	X=lnews:*lnswg,otherl:*lnswg,nnews:*lnswg,otherc:*lnswg,
		lnews,otherl,nnews,
		Dy.lnewslnews,Dy.lnewsnnews,Dy.nnewslnews,Dy.nnewsnnews,
		Dy.lsi,Dy.siXlnln,Dy.siXlnnn,Dy.siXnnln,Dy.siXnnnn,
		Dy.lnewstot,Dy.nnewstot,l_ACS_HH,Dy.lnewsn,Dy.otherln,Dy.nnewsn,Dy.othercn,J(rows(lnews),1,1)

	B=lnews,otherl,nnews,otherc

	lnf=J(rows(m),1,.)
	lnDetF=J(rows(m),6,.)
	muVec=mu_l,mu_o,mu_n,mu_c
	if (runiform(1,1)>.96) muVec,eta_l,eta_n,omega,zeta_l,zeta_o,zeta_n,zeta_c
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
		
		/* New block to calculate log determinant */

		TimeVars=uniqrows(mtp)	

		for (z=1;z<=rows(TimeVars);z++) {
			siz=select(sip,mtp:==TimeVars[z])
			Bz=select(Bp,mtp:==TimeVars[z])
			sizBz=siz:*Bz
			sg=colsum(sizBz)
			Ng=colsum(sizBz:!=0)
			term=(Ng:>0):*(Ng:-1):*ln(1:-muVec)
			if (hasmissing(term)) lnDetF[i,z]=.
			else lnDetF[i,z]=rowsum(term)
		}		
	}
	lnDetF=B:*ln(1:-muVec)
	if (hasmissing(lnf)) lnf=.
	else lnf=colsum(lnf)
	if (hasmissing(lnDetF)) lnf=.
	else lnf=colsum(lnf) +colsum(rowsum(lnDetF))
	if (runiform(1,1)>.96) colsum(rowsum(lnDetF)),lnf
}
real matrix eshares_up(Uv,lnews,otherl,nnews,otherc,sigma_l,sigma_o,sigma_n,sigma_c)
{
	sigg=lnews:*sigma_l:+otherl:*sigma_o:+nnews:*sigma_n:+otherc:*sigma_c
        lu=exp(Uv:/(1:-sigg))
	lu_l=colsum(lnews:*lu)
	lu_n=colsum(nnews:*lu)
	lu_o=colsum(otherl:*lu)
	lu_c=colsum(otherc:*lu)
	swg=lu:/(lu_l:*lnews:+lu_n:*nnews:+lu_o:*otherl:+lu_c:*otherc)
	share=swg:*(lu_l:*lnews:+lu_n:*nnews:+lu_o:*otherl:+lu_c:*otherc):^(1:-sigg):/
	      (1:+lu_l:^(1:-sigma_l):+lu_n:^(1:-sigma_n):+lu_o:^(1:-sigma_o):+lu_c:^(1:-sigma_c))
	return(share)
}
real matrix esharesStable(Uv,lnews,otherl,nnews,otherc,sigma_l,sigma_o,sigma_n,sigma_c)
{
	sigg=lnews:*sigma_l:+otherl:*sigma_o:+nnews:*sigma_n:+otherc:*sigma_c
	lnLu=Uv:/(1:-sigg)
	if (colsum(lnews)>0) {
		lnU=select(lnLu,lnews)
		max=max(lnU)
		lu_l=exp(max):*colsum(exp(lnU:-max))
	}
	else lu_l=0
	if (colsum(nnews)>0) {
		lnU=select(lnLu,nnews)
		max=max(lnU)
		lu_n=exp(max):*colsum(exp(lnU:-max))
	}
	else lu_n=0
	if (colsum(otherl)>0) {
		lnU=select(lnLu,otherl)
		max=max(lnU)
		lu_o=exp(max):*colsum(exp(lnU:-max))
	}
	else lu_o=0
	if (colsum(otherc)>0) {
		lnU=select(lnLu,otherc)
		max=max(lnU)
		lu_c=exp(max):*colsum(exp(lnU:-max))
	}
	else lu_c=0
	swg=exp(lnLu):/(lu_l:*lnews:+lu_n:*nnews:+lu_o:*otherl:+lu_c:*otherc)
	share=swg:*(lu_l:*lnews:+lu_n:*nnews:+lu_o:*otherl:+lu_c:*otherc):^(1:-sigg):/
	      (1:+lu_l:^(1:-sigma_l):+lu_n:^(1:-sigma_n):+lu_o:^(1:-sigma_o):+lu_c:^(1:-sigma_c))	
	return(share)
}
real matrix esharesMoreStable(Uv,lnews,otherl,nnews,otherc,sigma_l,sigma_o,sigma_n,sigma_c)
{
	sigg=lnews:*sigma_l:+otherl:*sigma_o:+nnews:*sigma_n:+otherc:*sigma_c
	lnLu=Uv:/(1:-sigg)
	if (colsum(lnews)>0) {
		lnU=select(lnLu,lnews)
		max=max(lnU)
		lu_l=exp(max):*colsum(exp(lnU:-max))
	}
	else lu_l=0
	if (colsum(nnews)>0) {
		lnU=select(lnLu,nnews)
		max=max(lnU)
		lu_n=exp(max):*colsum(exp(lnU:-max))
	}
	else lu_n=0
	if (colsum(otherl)>0) {
		lnU=select(lnLu,otherl)
		max=max(lnU)
		lu_o=exp(max):*colsum(exp(lnU:-max))
	}
	else lu_o=0
	if (colsum(otherc)>0) {
		lnU=select(lnLu,otherc)
		max=max(lnU)
		lu_c=exp(max):*colsum(exp(lnU:-max))
	}
	else lu_c=0
	lso=ln(1:+lu_l:^(1-sigma_l):+lu_o:^(1-sigma_o):+lu_n:^(1-sigma_n):+lu_c:^(1-sigma_c))
	lnlu_l=ln(lu_l)
	lnlu_o=ln(lu_o)
	lnlu_n=ln(lu_n)
	lnlu_c=ln(lu_c)
	_editmissing(lnlu_l,0)
	_editmissing(lnlu_o,0)
	_editmissing(lnlu_c,0)
	_editmissing(lnlu_n,0)
	lsi=lnLu:-lnlu_l:*sigma_l:*lnews:-lnlu_n:*sigma_n:*nnews:-lnlu_o:*sigma_o:*otherl:-lnlu_c:*sigma_c:*otherc:-lso
	return(exp(lsi))
}
real matrix esharesQuadPres(Uv,lnews,otherl,nnews,otherc,sigma_l,sigma_o,sigma_n,sigma_c)
{
	sigg=lnews:*sigma_l:+otherl:*sigma_o:+nnews:*sigma_n:+otherc:*sigma_c
	lnLu=Uv:/(1:-sigg)
	if (colsum(lnews)>0) {
		lnU=select(lnLu,lnews)
		max=max(lnU)
		lu_l=exp(max):*quadcolsum(exp(lnU:-max))
	}
	else lu_l=0
	if (colsum(nnews)>0) {
		lnU=select(lnLu,nnews)
		max=max(lnU)
		lu_n=exp(max):*quadcolsum(exp(lnU:-max))
	}
	else lu_n=0
	if (colsum(otherl)>0) {
		lnU=select(lnLu,otherl)
		max=max(lnU)
		lu_o=exp(max):*quadcolsum(exp(lnU:-max))
	}
	else lu_o=0
	if (colsum(otherc)>0) {
		lnU=select(lnLu,otherc)
		max=max(lnU)
		lu_c=exp(max):*quadcolsum(exp(lnU:-max))
	}
	else lu_c=0
	lso=ln(1:+lu_l:^(1-sigma_l):+lu_o:^(1-sigma_o):+lu_n:^(1-sigma_n):+lu_c:^(1-sigma_c))
	lnlu_l=ln(lu_l)
	lnlu_o=ln(lu_o)
	lnlu_n=ln(lu_n)
	lnlu_c=ln(lu_c)
	_editmissing(lnlu_l,0)
	_editmissing(lnlu_o,0)
	_editmissing(lnlu_c,0)
	_editmissing(lnlu_n,0)
	lsi=lnLu:-lnlu_l:*sigma_l:*lnews:-lnlu_n:*sigma_n:*nnews:-lnlu_o:*sigma_o:*otherl:-lnlu_c:*sigma_c:*otherc:-lso
	return(exp(lsi))
}
real matrix lnnd(y,m,lnsd) return(-(y:-m):^2:/(2:*exp(lnsd):^2):-ln(2*pi())/2:-lnsd)
real matrix unilateral_devs(Hyposhares,lnews_lpk)
{
	Test=abs(lnews_lpk:-lnews_lpk[,1])
	devpos=mm_which(colsum(Test):==1)

	Devs=J(rows(Test),1,.)
	for (i=1;i<=cols(devpos);i++) {
		dev_player=mm_which(lnews_lpk[,devpos[i]]:-lnews_lpk[,1]:!=0)
		Devs[dev_player]=Hyposhares[dev_player,devpos[i]]
				       }
return(Devs)
}
real matrix stratmat(real scalar x)
{
	real matrix M,MM,V
	real colvector p
	real scalar info,j
		V=J(x,x,1)
		V=J(rows(V),1,0),lowertriangle(V)
		MM=J(rows(V),1,.)
		for (j=1;j<=cols(V);j++) {
			info=cvpermutesetup(V[.,j])
			M=J(rows(V),1,.)
				while ((p=cvpermute(info)) !=J(0,1,.)) {
					M=M,p
						}
			M=M[.,2::cols(M)]
		MM=MM,M
					}
		MM=MM[.,2::cols(MM)]
	return(MM)
}
real matrix putfirst(real colvector y,real matrix M)
{
	real matrix T,Z
	real scalar s
	T=M:-y
	s=mm_which(colsum(abs(T)):==0)
	if (s==1) Z=M
		else if (s==cols(M)) Z=M[.,s],M[.,1::s-1]
	else Z=M[.,s],M[.,1::s-1],M[.,s+1::cols(M)]
	return(Z)
}
void logLikelihoodPrice(M,todo,b,lnf,g,H)
{
	real scalar nu_l,nu_o,nu_n,gamma_l,gamma_o,
		sdsta,sdmar,sdmod,i,T,N,lam1,
		lam2,lam3,lam4,phi22,phi32
	real matrix X,mt,id,mtp,idp,yp,Xp,Jn,Jt,En,Et,
		Q1,Q2,Q3,Q4,OmegaInv,beta
	 		
	nu_l =moptimize_util_xb(M,b,1)
	nu_o =moptimize_util_xb(M,b,2)
	nu_n =moptimize_util_xb(M,b,3)
	gamma_l=moptimize_util_xb(M,b,4)
	gamma_o=moptimize_util_xb(M,b,5)
	omega=moptimize_util_xb(M,b,6)
	sdsta=exp(moptimize_util_xb(M,b,7))
	sdmar=exp(moptimize_util_xb(M,b,8))
	sdmod=exp(moptimize_util_xb(M,b,9))
	alpha=moptimize_util_xb(M,b,10)

	y      =moptimize_util_depvar(M,1)
	lnews  =moptimize_util_depvar(M,2)
	otherl =moptimize_util_depvar(M,3)
	nnews  =moptimize_util_depvar(M,4)
	lnview =moptimize_util_depvar(M,5)
	l_ACS_HH=moptimize_util_depvar(M,6)

	id=moptimize_util_userinfo(M,1)
	mt=moptimize_util_userinfo(M,2)
	m =moptimize_util_userinfo(M,3)

	beta=nu_l,nu_o,nu_n,gamma_l,gamma_o,omega,alpha
	X=lnews:*lnview,otherl:*lnview,nnews:*lnview,
		lnews,otherl,l_ACS_HH,J(rows(lnews),1,1)

	lnf=J(rows(m),1,.)
	for (i=1;i<=rows(m);i++) {
		mtp=panelsubmatrix(mt,i,m)
		idp=panelsubmatrix(id,i,m)
		T=rows(uniqrows(mtp))
		N=rows(uniqrows(idp))
		yp=panelsubmatrix(y,i,m)
		Xp=panelsubmatrix(X,i,m)
		
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
	}
	if (hasmissing(lnf)) lnf=.
	else lnf=colsum(lnf)
	//lnf,b
}
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
mata
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
struct viewInfo {
	real matrix y,lnswg,lnews,otherl,nnews,otherc,id,mt,m
}
struct viewInfo viewInfoInit(y,lnswg,lnews,otherl,nnews,otherc,id,mt,m)
{
	struct viewInfo scalar vInfo
	vInfo.y=y
	vInfo.lnews=lnews
	vInfo.otherl=otherl
	vInfo.nnews=nnews
	vInfo.otherc=otherc
	vInfo.lnswg=lnswg
	vInfo.id=id
	vInfo.mt=mt
	vInfo.m=m
	return(vInfo)	
}
struct selectEffects {
	real matrix lnewsg, otherlg, lnppsg,
		xbvgg, lnviewg, Epmtg, Uvsg, Uvmtg, Epsg,
		lngvmtg, lngvsg, betaV, betaP, lngxbvgg,
		mtg,idg,marg		
	real scalar sdmodp, sdmarp, sdstap, sdmodv, sdmarv,
		sdstav,alphap,alphav
}
struct selectEffects selectEffectsDataInit(lnewsg,otherlg,
		lnppsg,lnviewg,mtg,idg,marg)
{
	struct selectEffects scalar SE
	SE.lnewsg=lnewsg
	SE.otherlg=otherlg
	SE.lnppsg=lnppsg
	SE.lnviewg=lnviewg
	SE.mtg=mtg
	SE.idg=idg
	SE.marg=marg			/* To be clear - this is not in panelsetup form yet */
	return(SE)
}	
void selectEffectsSimInfo(struct selectEffects SI,xbvgg,Epmtg,
	Uvsg,Uvmtg,Epsg,lngxbvgg,lngvmtg,lngvsg)
{
	SI.xbvgg=xbvgg
	SI.Epmtg=Epmtg
	SI.Uvsg=Uvsg
	SI.Uvmtg=Uvmtg
	SI.Epsg=Epsg
	SI.lngvmtg=lngvmtg
	SI.lngvsg=lngvsg
	SI.lngxbvgg=lngxbvgg
}
void selectEffectsParmInfo(struct selectEffects SI, betaV, betaP,
	sdmodp,sdmarp,sdstap,sdmodv,sdmarv,sdstav,alphap,alphav)
{
	SI.betaV=betaV
	SI.betaP=betaP
	SI.sdmodp=sdmodp
	SI.sdmarp=sdmarp
	SI.sdstap=sdstap
	SI.sdmodv=sdmodv
	SI.sdmarv=sdmarv
	SI.sdstav=sdstav
	SI.alphap=alphap
	SI.alphav=alphav
}
real scalar selectEffect(struct selectEffects SI)
{
	real scalar nu_l, nu_o 
	real matrix lnewshat,otherlhat,betaP,betaV,XPview,XP,XPB,
		valPart1,valPart21,valPart22,XVB,val
	
	lnewshat=SI.otherlg
	otherlhat=SI.lnewsg
	nu_l=SI.betaP[1]
	nu_o=SI.betaP[2]
	betaP=SI.betaP[3::cols(SI.betaP)],SI.alphap

	XPview=nu_l*lnewshat:*SI.lnviewg:+nu_o*otherlhat:*SI.lnviewg
	XP=J(rows(lnewshat),1,0),lnewshat,otherlhat,J(rows(SI.lnewsg),1,1)
	XPB=rowsum(XP*betaP'):+XPview
	
	valPart1=lnnormal((SI.lnppsg:-XPB-SI.Epsg*SI.sdstap:-SI.Epmtg*SI.sdmarp):/SI.sdmodp)

	/* As these contain market-level effects, they need to be weighted correctly */
	valPart21=uniqrows(lnnormalden(SI.Uvmtg,SI.sdmarv)):-uniqrows(SI.lngvmtg)
	valPart22=uniqrows(lnnormalden(SI.Uvsg,SI.sdstav)):-uniqrows(SI.lngvsg)
		 
	betaV=SI.betaV,SI.alphav
	Foo=lnewshat,otherlhat,J(rows(lnewshat),1,0),J(rows(lnewshat),1,1)
	XVB=rowsum(Foo*betaV')

	valPart3=lnnormalden(SI.xbvgg,XVB:+SI.Uvmtg:+SI.Uvsg,SI.sdmodv):-SI.lngxbvgg

	val=colsum(ln(rowsum(exp(valPart1)))):+
	    colsum(ln(rowsum(exp(valPart21)))):+
	    colsum(ln(rowsum(exp(valPart22)))):+
	    colsum(ln(rowsum(exp(valPart3))))	
	return(val)
}
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
	if (hasmissing(lnv)) printf("Danger!\n")
	return(colsum(lnv):+colsum(lnp))
}

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
