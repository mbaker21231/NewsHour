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
end
end