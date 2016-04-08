// RandomEffects prediction

	clear all
	use "/user/mjbaker/TV/ad_sample/AveragedDataDyno.dta", clear
        quietly do "/user/mjbaker/TV/ad_sample/AveragedDataDynamicMata.do"  /* Load Mata programs */	

	/* Make a variable defining competitors in the game */
	gen keeper=1
	gen game=lnpps!=.&(lnews | otherl)
	gen price=lnpps!=.
	gen lnview=ln(si*ACS_HH)
	gen obsid=_n
	
	mata

	st_view(dln=.,.,"dln")
	st_view(ln_swg=.,.,"ln_swg")
	st_view(lnews=.,.,"lnews")
	st_view(otherl=.,.,"otherl")
	st_view(nnews=.,.,"nnews")
	st_view(otherc=.,.,"otherc")
	st_view(ma=.,.,"market")
	st_view(id=.,.,"stationid")
	st_view(mt=.,.,"mt")
	st_view(game=.,.,"game")

	st_view(lnewslnews=.,.,"lnewslnews")
	st_view(lnewsnnews=.,.,"nnewsnnews")
	st_view(nnewslnews=.,.,"nnewslnews")
	st_view(nnewsnnews=.,.,"nnewsnnews")
	st_view(siXlnln=.,.,"siXlnln")
	st_view(siXlnnn=.,.,"siXlnnn")
	st_view(siXnnln=.,.,"siXnnln")
	st_view(siXnnnn=.,.,"siXnnnn")
	st_view(lnewstot=.,.,"lnewstot")
	st_view(nnewstot=.,.,"nnewstot")
	st_view(lsi=.,.,"lsi")	
	st_view(l_ACS_HH=.,.,"l_ACS_HH")
	
	st_view(lnewsn=.,.,"lnewsn")
	st_view(nnewsn=.,.,"nnewsn")
	st_view(otherln=.,.,"otherln")
	st_view(othercn=.,.,"othercn")


	mata matuse /user/mjbaker/TV/ad_sample/Results /* We should be doing this */
	b_start[,1::4]=b_start[,1::4]:^2
	bo=mean(b_start)

	Xv=ln_swg:*lnews,ln_swg:*otherl,ln_swg:*nnews,ln_swg:*otherc,
		lnews,otherl,nnews,
		lnewslnews,lnewsnnews,nnewslnews,nnewsnnews,lsi,
		siXlnln,siXlnnn,siXnnln,siXnnnn,lnews:*ln(1:+lnewstot),nnews:*ln(1:+nnewstot),l_ACS_HH,
		lnewsn,otherln,nnewsn,othercn,J(rows(lnews),1,1)

	b=bo[(1..23,27)]
	sdmodDy=exp(bo[26])
	sdstaDy=exp(bo[24])
	sdmarDy=exp(bo[25])
	
	IEv=indEffect3(ma,mt,id,dln:-Xv*b', sdmodDy,sdstaDy,sdmarDy)
	
	/* That takes care of our first block of effects */
	/* Now, the next one should revolve around price */
	/* first, make an id variable */

	st_view(lnpps=.,.,"lnpps","price")
	st_view(lnewsp=.,.,"lnews","price")
	st_view(nnewsp=.,.,"nnews","price")
	st_view(otherlp=.,.,"otherl","price")
	st_view(lnview=.,.,"lnview","price")
	st_view(map=.,.,"market","price")
	st_view(mtp=.,.,"mt","price")
	st_view(idp=.,.,"stationid","price")
	st_view(obsid=.,.,"obsid","price")
	st_view(l_ACS_HHp=.,.,"l_ACS_HH","price")
	
	Xp=lnewsp:*lnview,otherlp:*lnview,nnewsp:*lnview,lnewsp,otherlp,l_ACS_HHp,J(rows(lnewsp),1,1)
	
	bp=bo[(28..33,37)]
	sdstap=exp(bo[34])
	sdmarp=exp(bo[35])	/* Once again, a stand-in bit of stuff */
	sdmodp=exp(bo[36])
	
	IEp=indEffect3(map,mtp,idp,lnpps:-Xp*bp',sdmodp,sdstap,sdmarp)
	
	end
	
	/* THe next thing to do now that we have all the random effects and all that is get utility */
	/* Let's try to rely on estimates as little as possible, but let's get them */
	/* all into the data set in the right place */
	
	getmata (ievr*)=IEv, double
	getmata (iepr*)=IEp, id(obsid) double

	save /user/mjbaker/TV/ad_sample/AveragedDataDynamicREr.dta, replace
	exit, clear
