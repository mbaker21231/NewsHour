/* This file describes what all the files do sequentially as we move along */
/* The following is constructed to follow the order that we would like to  */
/* run them as well. 

Step 1: Run AveragedData.do. This creates averaged data. One thing that is crucial
        for subsequent steps is the adjustment factor, which is the population *2. This is 
	needed so that we don't get viewership shares greater than one! This file calls zero special
	mata functions.
Step 2: Run AveragedDataDynamicFinal1.do This creates a dynamic data set with lags and all that
        and estimates a preliminary viewership model. The model includes two-way random effects 
	at the market-time level, and at the station level. It also saves the mata matrix DynoStarts
	and the data set. Mata functions: DynoInfoInit() and logLikelihood2Dyna(). That it. 
	
	/* Do we still need to do this? */
	
Step 3: Run AveragedDataPriceDynamicFinal1.do Which uses the data set created by step 2 but estimates a
	free-standing model of prices. It also saves the start values. One question: this saves some
	more mata matrices that we might not need. Mata functions used: logLikelihoodPrice().
	
/* We might wish to form a loop starting at this point */
	
Step 4: Run CollateAckerbergFinal1.do This function creates and sets up the Ackerberg simulations based on previous
	model estimates. 
Step 5: Run CollateAckandEstFinal1.do This program renders the results of the previous simulation exercise more
	readily useable in estimation. 
Step 7: Run EstimateFinal1.do Which estimates the model and saves the results in a mata matrix.

/* We probably need to run ResultsAnalyze.do at this point */

Step 8: Once Step 7 has been run, we can re-estimate the model fixed effects using 
	RandomEffectsPredictionDynamicPart2Final1.do as the first step.
Step 9: The next step after reestimating the model is to run "ResultsAnalyze.do," which gives us a 
        set of simulations to work with. 
Step 10:We can the do SimSetupFinal1.do, which sets up the simulations so that everything is readily available in Stata,
	and finally, we can do ResultsPresentFinal1.do and ResultsPresentEstFinal1.do to get estimation results.


Estimation results include the following set of tables: */

/* Summary statistics and market descriptors */
/* This comes from "ResultsPresentFinal1.do" */

/*	MarketChars.tex
	SharesPrices.tex
	TotalShares.tex
	BroadCastCount.tex */

/* Model estimation results */
/* This comes from "ResultsPresentEstFinal1.do" */
/*	
	EstimationResults.tex 	*/

/* Then, simulation results are analyzed in */
/* SimulationsAnalyzeFinal1.do */

/* Tables produced include: 
	
	


		
	
	
	
	
	
	
	

