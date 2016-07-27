mata:
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
end
end