############################################################################
#Example Rumen Passage and SI Digestion Model for NANP Modeling Workshop
#This model contains CP, NDF, and Microbial CP pools
#Written by Mark D. Hanigan, Virginia Tech
#############################################################################

#Abbreviations: F = flux, Q = pool size, dQ = differential of Q with respect to time, 
#	C = concentration, K = mass action rate constant, Rum = rumen, SI = small intestine, 
#	Fec = fecal output, RD = ruminally degraded, TD = total tract digested, Abs = absorbed, 
#	In = input, Fa,b = the flux from pool a to pool b
#
#Time is in d, mass in kg, and fluxes in kg/d

library(deSolve);			#Load the deSolve package)

#A list of times to output predictions 
t <- seq(0, 5, by = 0.2)   #time in days.  units here define the model time units

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

          #2 alternatives for representing microbial growth
		FCpMi <- (VmMiG + fVmMiG*FChoDeg) / (1 + Kmig/FCpDeg)     #Lin Cho response, nonlin Rdp
#		FCpMi <- (VmMiG * fVmMiG*FCpDeg) / (1 + Kmig/FChoDeg)     #Lin Rdp response, nonlin Cho
		#the latter won't converge.  ends with a singularity error in estimating covariance.
		#Perhaps it can be coerced to converge by building the MiG solve 1 parm at a time.
		#using more data may also solve the problem.
		FCpMiSi <- QCpMiRum * Kp

		#Rumen Differentials
		dQCpRum <- FCpIn - FCpDeg - FCpSi
		dQChoRum <- FChoIn - FChoDeg - FChoSi
		dQCpMiRum <- FCpMi - FCpMiSi

		#IntestInal Fluxes
		FAaAbs <- (FCpSi + FCpMiSi) * KCpAbs
		FCpFec <- FCpSi + FCpMiSi - FAaAbs 

		FGlcAbs <- FChoSi * KChoAbs
		FChoFec <- FChoSi - FGlcAbs 

		#Digestibility Calculations
		RDCp <- FCpDeg/FCpIn			#Ruminal
		RDCho <- FChoDeg/FChoIn

		TDCp <- (FCpIn - FCpFec) / FCpIn		#Total Tract
		TDCho <- (FChoIn - FChoFec) / FChoIn

		return(list(c(dQCpRum,dQChoRum,dQCpMiRum),FDMI=fDMI, CCp=cCp, FCpIn=FCpIn,FCpDeg=FCpDeg,
		     FCpSi=FCpSi,FAaAbs=FAaAbs,RDCp=RDCp,TDCp=TDCp,FChoIn=FChoIn,FChoDeg=FChoDeg,
			FChoSi=FChoSi,FGlcAbs=FGlcAbs,RDCho=RDCho,TDCho=TDCho,FCpMi=FCpMi,
			FCpMiSi=FCpMiSi))
		#the list of returned differentials must be in the same order as the state variable list
	   })
	}
	state <- c(QCpRum=0.120*6.25, QChoRum=4.5, QCpMiRum=0.186*6.25) #Initial ruminal pool sizes 
	return(ode(y=state, times=t, func=derivs, parms=parameters, method="lsoda")) #solve through time using ode
}	#End of the Gut model function

#Model Inputs and Parameters
parameters <- c(fDMI = 20,		#kg DM/d
     cCp = 0.196*0.644,	               #kg/kg DM, 0.53+.114 = .644 from publication
     cCho = 0.50,
     KdCp = 0.075 * 24,		#value per hour times 24 h to convert to per day
     KdCho = 0.055 * 24,
     Kp = 0.04 * 24,			#per day
     KCpAbs = 0.75,
     KChoAbs = 0.45,
     Kmig = 0.5,
     VmMiG = 0.25,
     fVmMiG = 0)

out <- Gut(t, parameters)		#Call the model passing the time and parameter vectors
 					#Model output is collected in the out matrix
out[24,]			#Summarize output
plot(out)


plot(out[,1:2])

#Simulate a greater rate of passage
parameters['Kp'] = 0.06 * 24		#passage rate per hour scaled to a day
#update Kp in the pars vector
out2 <- Gut(t, parameters)		#Call the model passing time and pars vectors

lasttime <- length(out[,1])
rbind(out[lasttime,], out2[lasttime,])

#The ruminal DC
out[lasttime,"FCpDeg"]/out[lasttime,"FCpIn"]
out2[lasttime,"FChoDeg"]/out[lasttime,"FChoIn"]

