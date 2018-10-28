(* ::Package:: *)

(* begin package containing my user-defined functions *)
BeginPackage["ewsFunctions`"];


(* ::Section:: *)
(*Function usage messages*)


(* TBdetrend *)
TBdetrend::usage="smooth=TBdetrend[data, bandwidth] uses a Gaussian filter to smooth data
Input vars
data: 1xn array - time-series
bandwidth: number in (0,1) - proportion of the length of data to use as a bandwidth
Output vars
smooth: 1xn array of smoothed data";


(* TBvariance *)
TBvariance::usage="varSeries=TBvariance[data, rollWindow] computes the variance of data over a rolling window
Input vars
data: 1xn array - time-series (residuals)
rollWindow: number in (0,1) - proportion of the length of data to use as a rolling window
Output vars
varSeries: variance as a time-series, 1x(1-rollWindow)n array ";


(* TBskewness *)
TBskewness::usage="skewSeries=TBskewness[data, rollWindow] computes the skewness of data over a rolling window
Input vars
data: 1xn array - time-series (residuals)
rollWindow: number in (0,1) - proportion of the length of data to use as a rolling window
Output vars
skewSeries: skewness as a time-series, 1x(1-rollWindow)n array ";


(* TBautocorrelation *)
TBautocorrelation::usage="acSeries=TBautocorrelation[data, rollWindow,lagTimes] computes the autocorrelation at lag times lagTimes of data over a rolling window
Input vars
data: 1xn array: time-series (residuals)
rollWindow: number in (0,1) - proportion of the length of data to use as a rolling window
lagTimes: list of integers - spacing between data points for computing autocorrelation
Output vars
acSeries: list of autocorrelation time-series data ";


(* TBewsCompute *)
TBewsCompute::usage="{smooth, residuals, varSeries, acSeries, skewSeries} = TBewsCompute[data] takes in non-stationary data and outputs the smoothed data, residuals, and variance and autocorrelation computed over a rolling window.
Input vars
data: 1xn array: time-series

Optional arguments (default)
RollWindow (0.25): number in (0,1) - proportion of the length of data to use as a rolling window
BandWidth (0.2): integer value - spacing between data points for computing autocorrelation
LagTimes ({1}): list of integers - spacing between data points for computing autocorrelation

Output vars
smooth: time-series of smoothed data
residuals: time-series of residuals
varSeries: time-seires of variance in residuals
acSeries: list of time-series of autocorrelation of resiudals at each lag time
skewSeries: time-series of skewness in residuals";


(* ::Section:: *)
(*Function code*)


(* begin private context *)
Begin["`Private`"];


(* TBdetrend *)
TBdetrend[data_,bandWidth_]:=GaussianFilter[data,Length[data]*bandWidth];


(* TBvariance *)
TBvariance[residuals_,rollWindow_]:=Module[{windowComps,varSeries},
(* number of components in the rolling window *)
windowComps=Floor[rollWindow*Length[residuals]];
varSeries=MovingMap[Variance,residuals,windowComps]
];


(* TBskewness *)
TBskewness[residuals_,rollWindow_]:=Module[{windowComps,skewSeries},
(* number of components in the rolling window *)
windowComps=Floor[rollWindow*Length[residuals]];
skewSeries=MovingMap[Skewness,residuals,windowComps]
];


(* TBautocorrelation *)
TBautocorrelation[residuals_,rollWindow_,lags_]:=Module[{windowComps,acSeries},
(* number of components in the rolling window *)
windowComps=Floor[rollWindow*Length[residuals]];
acSeries=Table[MovingMap[CorrelationFunction[#,lags[[i]]]&,residuals,windowComps],{i,1,Length[lags]}]
]


(* TBewsCompute *)

(* Define options for TBewsCompute *)
Options[TBewsCompute]={RollWindow->0.25,BandWidth->0.2,LagTimes->{1}}; 

TBewsCompute[data_,OptionsPattern[]]:=Module[{rw,bw,lags,smoothData,residuals,variance,skewness,autocorrelation,output},
(* assign options to variables *)
rw=OptionValue[RollWindow];
bw=OptionValue[BandWidth];
lags=OptionValue[LagTimes];
(* detrend the data *)
smoothData=TBdetrend[data,bw];
(* find residuals *)
residuals=data-smoothData;
(* find variance of residuals *)
variance=TBvariance[residuals,rw];
(* find skewness of residuals *)
skewness=TBskewness[residuals,rw];
(* find the autocorrelation of residuals *)
autocorrelation=TBautocorrelation[residuals,rw,lags];
(* output *)
output={smoothData,residuals,variance,autocorrelation,skewness}
]


(* end private context *)
End[];


EndPackage[]
