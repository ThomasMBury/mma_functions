(* ::Package:: *)

(* begin package containing my user-defined functions *)
BeginPackage["myFunctions`"];


(* function names and descriptions *)
myAdd::usage="myAdd[x0,y0] computes z=x0+y0 and returns the result";

TBPowerSpec::usage="{freqVals,pSpec} = TBPowerSpec[{yVals,dt}] computes the periodogram of yVals and then outputs 
this in the form of a powerspectrum on the appropriate frequency domain taking into 
account dt - the time serparation between each data point.";

TBPowerSpecWelch::usage="{freqVals,pSpec} = TBPowerSpecWelch[{yVals,dt,hamLength,hamOffset,wProp_:1}] estimates the power spectrum usign Welch's method.
This involves computing the periodogram with overlapping Hamming windows.
hamLength: number of data points in Hamming window, or if in (0,1) taken as a proportion of total data input
hamOffset: number of data points to offset the window by on each interation, or if in (0,1) taken as a proportion of Hamming window size
wProp: optional argument of proportion of frequency values to go up to (can cutoff higher frequencies if necesssary) "


TBFitSpec::usage="TBFitSpec[pSpec] fits three power spectrum models to the input: unimodal, bimodal and flat (null).
pSpec = {freqVals,powerVals}
Outputs {foldWeight, hopfWeight, nullWeight}
AIC weights can be interpreted as the probability that the fitted model is the best model (in the AIC sense);"


TBFitSpecFixW::usage="TBFitSpec[pSpec,\[Omega]0] is the same as TBFitSpec except now we fix \[Omega]0 to a chosen value.
pSpec = {freqVals,powerVals}
Outputs {hopfAICweight,rSquaredRatio,cUni,\[Lambda]Uni,cBi,\[Lambda]Bi,\[Omega]0Bi}
AIC weights can be interpreted as the probability that the fitted model is the best model (in the AIC sense)
RSquared ratio is R^2 of Fold divided by R^2 of Hopf
c is the noise parameter
\[Lambda] is the absolute value of the dominant eigenvalue
\[Omega]0 is the intrinsic frequency
as found in each of the fitted power spectra";


TBHopfAIC::usage="TBHopfAIC[pSpec] fits the analytical forms for the Hopf and Fold bifurcation to the data in pSpec. 
It then outputs the AIC weight that corresponds to the Hopf fit. 
Note the Hopf fit is restricted such that S(\!\(\*SubscriptBox[\(\[Omega]\), \(0\)]\))> 2 S(0). This stops the model being selected for the Fold spectrum.
pSpec = {freqVals,powerVals}
Outputs = hopfAICweight
AIC weights can be interpreted as the probability that the fitted model is the best model (in the AIC sense)";


TBCoherFactor::usage="TBCoherFactor[{freqVals,powerVals}] computes the coherence factor of the given power specrum
output = coherenceFactor
Coherence factor is computed as CF=H/W where H is the height of the peak in spectrum and W is the relative width given by the width at half the height of the peak divided by the frequency that gives the peak";


TBCoherFactorMod::usage="TBCoherFactorMod[{freqVals,powerVals}] computes the coherence factor of the given power specrum
output = coherenceFactor
Coherence factor is computed as CF=H/W where H is the height of the peak in spectrum and W is the relative width given by the width at half the height of the peak divided by the frequency that gives the peak"


(* begin private context *)
Begin["`Private`"];


(* Coherence factor function *)
TBCoherFactor[{freqValsFull_,powerValsFull_}]:=Module[{freqVals,powerVals,numComps,\[Omega]Max,sMax,peakIndex,pSpecInterpol,\[Omega]Half,peakWidth,coherFactor},
(* work just with power spec for \[Omega]\[GreaterEqual]0 *)
numComps=Length[freqValsFull];
freqVals=freqValsFull[[Ceiling[numComps/2];;]];
powerVals=powerValsFull[[Ceiling[numComps/2];;]];
(* find the peak frequency and power *)
peakIndex=Position[powerVals,Max[powerVals]][[1,1]];
\[Omega]Max=Abs[freqVals[[peakIndex]]];
sMax=powerVals[[peakIndex]];
(* find the width of the peak by incrementing frequency until S\[LessEqual]Smax/2 *)
i=peakIndex;
(* increment *)
While[powerVals[[i]]>=sMax/2 && i<Length[powerVals],
i=i+1;];
(* if power fails to decrease (i.e. no peak) then  make width full spectrum *)
If[i==Length[powerVals],peakWidth=Max[freqVals],
	\[Omega]Half=freqVals[[i]];
	peakWidth=2*(\[Omega]Half-\[Omega]Max);];
(* compute and output the coherence factor - if omegaMax=0, then CF=0 by limits. *)
coherFactor=If[\[Omega]Max==0,0,
sMax*\[Omega]Max/peakWidth]
]


(* Modified Coherence factor function *)
TBCoherFactorMod[{freqValsFull_,powerValsFull_}]:=Module[{freqVals,powerVals,numComps,\[Omega]Max,sMax,peakIndex,pSpecInterpol,\[Omega]Half,peakWidth,coherFactor},
(* work just with power spec for \[Omega]\[GreaterEqual]0 *)
numComps=Length[freqValsFull];
freqVals=freqValsFull[[Ceiling[numComps/2];;]];
powerVals=powerValsFull[[Ceiling[numComps/2];;]];
(* find the peak frequency and power *)
peakIndex=Position[powerVals,Max[powerVals]][[1,1]];
\[Omega]Max=Abs[freqVals[[peakIndex]]];
sMax=powerVals[[peakIndex]];
(* find the width of the peak by incrementing frequency until S\[LessEqual]Smax/2 *)
i=peakIndex;
(* increment *)
While[powerVals[[i]]>=sMax/2 && i<Length[freqVals],
i=i+1;];
\[Omega]Half=freqVals[[i]];
peakWidth=2*(\[Omega]Half-\[Omega]Max);
(* compute and output the coherence factor - if omegaMax=0, then CF=0 by limits. Also, if half the width of peak is greater than omegaMax set coherence to zero as this includes the zero frequency component.  *)
coherFactor=If[\[Omega]Max==0||peakWidth/2>=\[Omega]Max,0,
sMax*\[Omega]Max/peakWidth]
]



(* TBPowerSpec function *)
TBPowerSpec[{yVals_,dt_}]:=Module[{ts,len,periodogram,p2,pSpec,freqVals,yValsNew},
ts=1/dt; (* sampling frequency *)
len=Length[yVals]; (* length of signal *)

periodogram=PeriodogramArray[yVals];

(* convert to form of power spectrum *)
pSpec=Join [Reverse[periodogram[[2;;Floor[len/2]+1]]],periodogram[[1;;Floor[len/2]+1]]];

(* define the frequency domain *)
freqVals=(2Pi*ts/(len))Range[-Floor[len/2],Floor[len/2]];

(* output data is {freqVals, pSpec) *)
{freqVals,pSpec}
]


(* powerSpecWelch function *)
TBPowerSpecWelch[{yVals_,dt_,hamLengthIn_,hamOffsetIn_,wprop_:1}]:=Module[{windexCutoff,ts,len,hamLength,hamOffset,periodogram,p2,pSpec,freqVals},
ts=1/dt; (* sampling frequency *)
len=Length[yVals]; (* length of signal *)
(* if HamLength and HamOffset given as proportions, turn them into number of data points *)
If[0<=hamLengthIn<=1,hamLength=Floor[hamLengthIn*len],hamLength=hamLengthIn];
If[0<=hamOffsetIn<=1,hamOffset=Floor[hamLengthIn*len*hamOffsetIn],hamOffset=hamOffsetIn];

(* compute the periodogram using Hamming window of length hamLength, and offset hamOffset *)
periodogram=PeriodogramArray[yVals,hamLength,hamOffset,HammingWindow];

(* convert to form of power spectrum *)
pSpec=Join [Reverse[periodogram[[2;;Floor[hamLength/2]+1]]],periodogram[[1;;Floor[hamLength/2]+1]]];

(* define the frequency domain *)
freqVals=(2Pi*ts/(hamLength))Range[-Floor[hamLength/2],Floor[hamLength/2]];

(* index to start from given cutoff of higher frequencies *)
windexCutoff=Floor[(1-wprop)*Length[freqVals]/2 + 1];

(* power spectrum data is of the form {freqVals, pSpec) *)
{freqVals,pSpec};

(* output values after filtering frequnecies beyond prop *)
{freqVals[[windexCutoff;;-windexCutoff]],pSpec[[windexCutoff;;-windexCutoff]]}
]


(* TBHopfAIC *)
TBHopfAIC[pSpec_]:=Module[{\[Omega]Max,\[Omega]0Thresh,\[Psi],aicUnimodal,aicBimodal,fitUnimodal,fitBimodal,aicDiff1,aicDiff2,likelihood1,likelihood2,foldAICweight,hopfAICweight},
(* set constraint value for \[Omega]0 in terms of \[Mu] *)
\[Psi]=0.2;
(* max bound for \[Omega] is max \[Omega] in power spec *)
\[Omega]Max=pSpec[[1,-1]];
Clear[\[Omega]0Thresh];
\[Omega]0Thresh[\[Mu]_,\[Psi]_]:=Sqrt[(\[Mu]^2/(4\[Psi]))(4-3\[Psi] + Sqrt[\[Psi]^2-16\[Psi]+16])];
(* fit unimdodal and bimodal forms to the power spectrum *)
fitUnimodal=NonlinearModelFit[Transpose[pSpec],c*(1/(\[Omega]^2+\[Mu]^2)),{c,\[Mu]},\[Omega]];
fitBimodal=NonlinearModelFit[Transpose[pSpec],
{c*(1/((\[Omega]-\[Omega]0)^2+\[Mu]^2)+1/((\[Omega]+\[Omega]0)^2+\[Mu]^2)),\[Mu]<0,c>0,\[Omega]Max>\[Omega]0>\[Omega]0Thresh[\[Mu],\[Psi]]},{{c,0.1},{\[Omega]0,1},{\[Mu],-1}},\[Omega]];
(* AIC scores from fitting *)
aicUnimodal=fitUnimodal["AIC"];
aicBimodal=Quiet[fitBimodal["AIC"]];
(* compute AIC weights *)
aicDiff1=aicUnimodal-Min[aicUnimodal,aicBimodal];
aicDiff2=aicBimodal-Min[aicUnimodal,aicBimodal];
likelihood1=Exp[-(1/2)*aicDiff1];
likelihood2=Exp[-(1/2)*aicDiff2];
foldAICweight=likelihood1/(likelihood1+likelihood2);
hopfAICweight=likelihood2/(likelihood1+likelihood2);
(* export the hopf weight *)
hopfAICweight
]






(* TBFitSpecFixW *)
TBFitSpecFixW[pSpec_,\[Omega]0_]:=Module[{cUni,\[Lambda]Uni,cBi,\[Lambda]Bi,aicUnimodal,aicBimodal,fitUnimodal,fitBimodal,aicDiff1,aicDiff2,likelihood1,likelihood2,foldAICweight,hopfAICweight,rSquareUnimodal,rSquareBimodal,rSquareRatio},
(* fit unimdodal and bimodal forms to the power spectrum *)
fitUnimodal=NonlinearModelFit[Transpose[pSpec],c*(1/(\[Omega]^2+\[Lambda]^2)),{c,\[Lambda]},\[Omega],MaxIterations->10000];
fitBimodal=NonlinearModelFit[Transpose[pSpec],c*(1/((\[Omega]-\[Omega]0)^2+\[Lambda]^2)+1/((\[Omega]+\[Omega]0)^2+\[Lambda]^2)),{c,\[Lambda]},\[Omega],MaxIterations->10000];
(* parameter value estimates for each fit *)
{cUni,\[Lambda]Uni}=fitUnimodal["ParameterTableEntries"][[;;,1]];
{cBi,\[Lambda]Bi}=fitBimodal["ParameterTableEntries"][[;;,1]];
(* AIC scores from fitting *)
aicUnimodal=fitUnimodal["AIC"];
aicBimodal=fitBimodal["AIC"];
(* AIC weights *)
aicDiff1=aicUnimodal-Min[aicUnimodal,aicBimodal];
aicDiff2=aicBimodal-Min[aicUnimodal,aicBimodal];
likelihood1=Exp[-(1/2)*aicDiff1];
likelihood2=Exp[-(1/2)*aicDiff2];
foldAICweight=likelihood1/(likelihood1+likelihood2);
hopfAICweight=likelihood2/(likelihood1+likelihood2);
(* R^2 error from fitting *)
rSquareUnimodal=fitUnimodal["RSquared"];
rSquareBimodal=fitBimodal["RSquared"];
rSquareRatio=rSquareUnimodal/rSquareBimodal;
(* export the hopf weight and the rSquared ratio *)
{hopfAICweight,rSquareRatio,cUni,\[Lambda]Uni,cBi,\[Lambda]Bi}
]




(* TBFitSpec *)
TBFitSpec[pSpec_,\[Psi]_:0.1]:=Module[{\[Omega]Max,\[Omega]0Thresh,fitUnimodal,fitBimodal,fitNull,aicUnimodal,aicBimodal,aicNull,delUnimodal,delBimodal,delNull,llhUnimodal,llhBimodal,llhNull,llhTotal,foldAICweight,hopfAICweight,weightUnimodal,weightBimodal,weightNull},
(* max bound for \[Omega] is max \[Omega] in power spec *)
\[Omega]Max=pSpec[[1,-1]];
Clear[\[Omega]0Thresh];
\[Omega]0Thresh[\[Mu]_,psi_]:=Sqrt[(\[Mu]^2/(4psi))(4-3psi + Sqrt[psi^2-16psi+16])];
(* fit power spectrum models to power spectrum *)
fitUnimodal=NonlinearModelFit[Transpose[pSpec],c*(1/(\[Omega]^2+\[Lambda]^2)),{c,\[Lambda]},\[Omega],
Method->"NMinimize"];
fitBimodal=NonlinearModelFit[Transpose[pSpec],
{c*(1/((\[Omega]-\[Omega]0)^2+\[Mu]^2)+1/((\[Omega]+\[Omega]0)^2+\[Mu]^2)),\[Mu]<0,c>0,\[Omega]Max>\[Omega]0>\[Omega]0Thresh[\[Mu],\[Psi]]},{{c,0.1},{\[Omega]0,1},{\[Mu],-1}},\[Omega],
Method->"NMinimize"];
fitNull=NonlinearModelFit[Transpose[pSpec],c,{c},\[Omega]];

(* AIC scores from fitting *)
aicUnimodal=fitUnimodal["AIC"];
aicBimodal=fitBimodal["AIC"]//Quiet;
aicNull=fitNull["AIC"];

(* compute AIC deviations from best model *)
delUnimodal=aicUnimodal-Min[aicUnimodal,aicBimodal,aicNull];
delBimodal=aicBimodal-Min[aicUnimodal,aicBimodal,aicNull];
delNull=aicNull-Min[aicUnimodal,aicBimodal,aicNull];

(* compute relative likelihoods of each model *)
llhUnimodal=Exp[-(1/2)*delUnimodal];
llhBimodal=Exp[-(1/2)*delBimodal];
llhNull=Exp[-(1/2)*delNull];
llhTotal=Total[{llhUnimodal,llhBimodal,llhNull}];

(* normalise to get weights (all weight sum to 1) *)
weightUnimodal=llhUnimodal/llhTotal;
weightBimodal=llhBimodal/llhTotal;
weightNull=llhNull/llhTotal;
(* export the hopf weight and the rSquared ratio *)
{weightUnimodal,weightBimodal,weightNull}
]





(* end private context *)
End[];


EndPackage[]
