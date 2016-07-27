mata:
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
end