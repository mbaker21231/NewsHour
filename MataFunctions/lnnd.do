mata
    real matrix lnnd(y,m,lnsd) return(-(y:-m):^2:/(2:*exp(lnsd):^2):-ln(2*pi())/2:-lnsd)   
end
end