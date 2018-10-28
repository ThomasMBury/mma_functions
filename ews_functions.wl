(* ::Package:: *)

(* begin package containing my user-defined functions *)
BeginPackage["myFunctions`"];


(* ::Section:: *)
(*Function names and descriptions*)


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


(* TBautocorrelation *)
TBautocorrelation::usage="acSeries=TBautocorrelation[data, rollWindow,lagTime] computes the autocorrelation at lag lagTime of data over a rolling window
Input vars
data: 1xn array - time-series (residuals)
rollWindow: number in (0,1) - proportion of the length of data to use as a rolling window
lagTime: integer value - spacing between data points for computing autocorrelation
Output vars
acSeries: autocorrelation as a time-series, 1x(1-rollWindow)n array ";


(* TBewsCompute *)
TBewsCompute::usage="{smoothed function, residuals, variance, autocorrelation} = TBewsCompute[data] takes in non-stationary data and outputs the smoothed data, residuals, and variance and autocorrelation computed over a rolling window.
Input vars
data: 1xn array - time-series
Optional arguments
rollWindow: number in (0,1) - proportion of the length of data to use as a rolling window
lagTime: integer value - spacing between data points for computing autocorrelation
Output vars
acSeries: autocorrelation as a time-series, 1x(1-rollWindow)n array ";


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


(* TBautocorrelation *)
TBautocorrelation[residuals_,rollWindow_,lag_]:=Module[{windowComps,acSeries},
(* number of components in the rolling window *)
windowComps=Floor[rollWindow*Length[residuals]];
acSeries=MovingMap[CorrelationFunction[#,lag]&,residuals,windowComps]
]



TBewsCompute[data_,bandWidth_:0.2,rollWindow_:0.25,lag_:1]:=Module[{smoothData,residuals,variance,autocorrelation,output},
(* detrend the data *)
smoothData=TBdetrend[data,bandWidth];
(* find residuals *)
residuals=data-smoothData;
(* find variance of residuals *)
variance=TBvariance[residuals,rollWindow];
(* find the autocorrelation of residuals *)
autocorrelation=TBautocorrelation[residuals,rollWindow,lag];
(* output *)
output={smoothData,residuals,variance,autocorrelation}
]


(* end private context *)
End[];


EndPackage[]
