############################################################################
#Example Rumen Passage and SI Digestion Model for NANP Modeling Workshop
#This model contains CP and NDF pools. 
#Written by Mark D. Hanigan, Virginia Tech
#############################################################################

#Abbreviations: F = flux, Q = pool size, dQ = differential of Q with respect to time, 
#	C = concentration, K = mass action rate constant, Rum = rumen, SI = small intestine, 
#	Fec = fecal output, RD = ruminally degraded, TD = total tract digested, Abs = absorbed, 
#	In = input, Fa,b = the flux from pool a to pool b
#
#Time is in days, mass in kg, and fluxes in kg/d

require(deSolve);			#Load the deSolve package)

#A list of times to output predictions 
t <- seq(0, 4, by = 0.2)

#Gut model - Rum plus SI
Gut <- function(t, parameters) { 

	#Derivative section
	derivs <- function(t, state, parameters) {
	   with(as.list(c(state, parameters)), {

		#Rumen Fluxes
		FCpIn <- fDMI * cCp
		FCpDeg <- QCpRum * KdCp
		FCpSi <- QCpRum * Kp

		FChoIn <- fDMI * cCho
		FChoDeg <- QChoRum * KdCho
		FChoSi <- QChoRum * Kp


		#Rumen Differentials
		dQCpRum <- FCpIn - FCpDeg - FCpSi
		dQChoRum <- FChoIn - FChoDeg - FChoSi

		#IntestInal Fluxes
		FAaAbs <- FCpSi * KCpAbs
		FCpFec <- FCpSi - FAaAbs 

		FGlcAbs <- FChoSi * KChoAbs
		FChoFec <- FChoSi - FGlcAbs 

		#Digestibility Calculations
		RDCp <- FCpDeg/FCpIn			#Ruminal
		RDCho <- FChoDeg/FChoIn

		TDCp <- (FCpIn - FCpFec) / FCpIn		#Total Tract
		TDCho <- (FChoIn - FChoFec) / FChoIn

		return(list(c(dQCpRum,dQChoRum),FCpIn=FCpIn,FCpDeg=FCpDeg,FCpSi=FCpSi,
			FAaAbs=FAaAbs,RDCp=RDCp,TDCp=TDCp,FChoIn=FChoIn,FChoDeg=FChoDeg,
			FChoSi=FChoSi,FGlcAbs=FGlcAbs,RDCho=RDCho,TDCho=TDCho))
	   })
	}
	state <- c(QCpRum=.120 * 6.25, QChoRum=4.5) #Observed rumInal particulate pool sizes 
	return(ode(y=state, times=t, func=derivs, parms=parameters, method="rk4"))
}	#End of the Gut model function

#Model Inputs
parameters <- c(fDMI = 22,		#kg DM/d
     cCp = 0.196*0.644,	#kg insoluble CP/kg DM
     cCho = 0.50,
     KdCp = 7.5/100*24,	#fractional rates of degradation per day
     KdCho = 5.5/100*24,
     Kp = 4.0/100*24,	#fractional rate of passage per day
     KCpAbs = 0.75,		#Intestinal digestibilities
     KChoAbs = 0.45)


out <- Gut(t, parameters);		#Call the model passing time and pars vectors
 					#Model output is collected in the out matrix
summary(out);			#Summarize output
plot(out)

plot(out[,1:2])


parameters['Kp'] <- 6.0/100*24	#update Kp in the pars vector
out2 <- Gut(t, parameters)		   #Call the model passing time and pars vectors

last_time <- length(out[,1])
rbind(out[last_time,], out2[last_time,])
