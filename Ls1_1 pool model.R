############################################################################
#Example Rumen Passage and SI Digestion Model for NANP Modeling Workshop
#This model contains a single ruminal nutrient pool. 
#Written by Mark D. Hanigan, Virginia Tech
#############################################################################

#Abbreviations: F = flux, Q = pool size, dQ = differential of Q with respect to time, 
#	C = concentration, K = mass action rate constant, Rum = rumen, SI = small intestine, 
#	Fec = fecal output, RD = ruminally degraded, TD = total tract digested, Abs = absorbed, 
#	In = input, Fa,b = the flux from pool a to pool b
#
#Time is in days, mass in kg, and fluxes in kg/d

require(deSolve);			#Load the deSolve package for numerical integration)

#A list of times (d) to output predictions 
t <- seq(0, 4, by = 0.2)  #predictions at each time will be returned from the function

#Digestion model function - Rum plus SI
#The model is defined as a function that can be called with input arguments of:
#	t, and parameters.  Can be called by hand as below or by an optimizer solving for
#	parameter estimates or attempting to minimize or maximize any objective function
 
Gut <- function(t, parameters) { 
	#Derivative section
	derivs <- function(t, state, parameters) {
	   with(as.list(c(state, parameters)), {

		#Rumen Fluxes
		Fin <- DMI * Cnut
		FRumDeg <- Qrum * Kdeg
		FRumSI <- Qrum * Kpas

		#Rumen Differentials
		dQrum <- Fin - FRumDeg - FRumSI

		#Intestinal Fluxes
		FSIAbs <- FRumSI * KSIAbs
		FSIFec <- FRumSI - FSIAbs 

		#Digestibility Calculations
		RDnut <- FRumDeg/Fin			#Ruminal
		TDnut <- (Fin - FSIFec) / Fin		#Total Tract

		return(list(dQrum,Fin=Fin,FRumDeg=FRumDeg,FRumSI=FRumSI,FSIAbs=FSIAbs,RDnut=RDnut,TDnut=TDnut))
	   })
	}
	state <- c(Qrum=0.086) #Initial ruminal pool size 
	return(ode(y=state, times=t, func=derivs, parms=parameters, method="rk4"))  #solve the model in time using ode
}	#End of the Gut model function

#Create a Vector of Model Inputs and parameters
parameters <- c(DMI = 22,		#kg DM/h
     Cnut = 0.196*0.16*.53,	#kg/kg DM, in this case 19.6% CP at 16% N and 53% ruminally insoluble
     Kdeg = 7.5/100*24,		#degradation rate (%/h) converted to fraction per day
     Kpas = 0.06*24,		#passage rate per day
     KSIAbs = 0.75)		#SI digestibilty of escaped nutrient

#Call the model passing time and the vector of parameters
out <- Gut(t, parameters);	 #Model output is collected in the out matrix
summary(out);			#Summarize output contained in the out matrix
out[dim(out)[1],]        #Examine the last row of the out matrix to see the steady state values
plot(out);				#Plot all of the variables in the out matrix vs time
out[,"FRumSI"] #missing leading comman in distributed scripts

plot(out[,1:2])


#Simulate a greater rate of passage
parameters <- c(DMI = 22,		#kg DM/d
                Cnut = 0.196*0.16*.53,	#kg/kg DM, in this case 19.6% CP at 16% N and 53% ruminally insoluble
                Kdeg = 7.5/100*24,		#degradation rate per day
                Kpas = 0.08*24,		#passage rate per day
                KSIAbs = 0.75)		#SI digestibilty of escaped nutrient
out2 <- Gut(t, parameters)		#Call the model passing time and pars vectors

last_time <- length(out[,1])
out[last_time,c("time","Qrum","FRumDeg","FRumSI")]  # last timepoint, i.e. at steady state from slower Kp
out2[last_time,c("time","Qrum","FRumDeg","FRumSI")]  #last timepoint from faster Kp

#The ruminal DC
out[last_time,"FRumDeg"]/out[last_time,"Fin"]
out2[last_time,"FRumDeg"]/out[last_time,"Fin"]


