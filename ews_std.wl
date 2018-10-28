(* ::Package:: *)

(* begin package containing my user-defined functions *)
BeginPackage["ewsFunctions`"];


(* ::Section:: *)
(*Function usage messages*)


(* TBdetrend *)
TBdetrend::usage="smooth=TBdetrend[data, bandwidth] uses a Gaussian filter to smooth data
Input vars
data: nx2 array of time-series data - form {{t1,x1},{t2,x2},...,{tn,xn}}
bandwidth: number in (0,1) - proportion of the length of data to use as a bandwidth
Output vars
smooth: time-seires of smoothed function";


(* TBvariance *)
TBvariance::usage="varSeries=TBvariance[data, rollWindow] computes the variance of data over a rolling window
Input vars
data: nx2 array of time-series data - form {{t1,x1},{t2,x2},...,{tn,xn}}
rollWindow: number in (0,1) - proportion of the length of data to use as a rolling window
Output vars
varSeries: time-series of variance";


(* TBautocorrelation *)
TBautocorrelation::usage="acSeries=TBautocorrelation[data, rollWindow,lagTimes] computes the autocorrelation at lag times lagTimes of data over a rolling window
Input vars
data: nx2 array of time-series data - form {{t1,x1},{t2,x2},...,{tn,xn}}
rollWindow: number in (0,1) - proportion of the length of data to use as a rolling window
lagTimes: list of integers - spacing between data points for computing autocorrelation
Output vars
acSeries: list of autocorrelation time-series data ";


(* TBskewness *)
TBskewness::usage="skewSeries=TBskewness[data, rollWindow] computes the skewness of data over a rolling window
Input vars
data: nx2 array of time-series data - form {{t1,x1},{t2,x2},...,{tn,xn}}
rollWindow: number in (0,1) - proportion of the length of data to use as a rolling window
Output vars
skewSeries: time-series of skewness ";


(* TBewsCompute *)
TBewsCompute::usage="{smooth, residuals, varSeries, acSeries, skewSeries} = TBewsCompute[data] takes in non-stationary data and outputs the smoothed data, residuals, and variance and autocorrelation computed over a rolling window.
Input vars
data: nx2 array of time-series data - form {{t1,x1},{t2,x2},...,{tn,xn}}

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


(* TBindPlot *)
TBindPlot::usage="plot = TBindPlot[data] takes in times-series data and produces a plot with the chosen specifications.
Input vars
data: time-series of the form {{t1,x1},{t2,x2},...,{tn,xn}}

Optional arguments (default)
TBxAxes: (False) : choose whether to include x ticks and label
TBlabel: ("") : figure label in top left corner
TByLabel: ("") : y axes label
TBarrow: (False) : choose whether to include an arrow denoting rolling window
TByRange: (All) : specify a particular range in y values

Output vars
plot: a grid of plots";


(* TBewsPlot *)
TBewsPlot::usage="plot = TBewsPlot[data] takes in times-series data of the state variable and the EWS to produce a gridded plot.
Input vars
data: list of time-series of the form {{t1,x1},{t2,x2},...,{tn,xn}}

Optional arguments (default)
RollWindow (0.25): number in (0,1) - proportion of the length of data to use as a rolling window
BandWidth (0.2): integer value - spacing between data points for computing autocorrelation
LagTimes ({1}): list of integers - spacing between data points for computing autocorrelation

Output vars
plot: a grid of plots";


(* ::Section:: *)
(*Function code*)


(* begin private context *)
Begin["`Private`"];


(* TBdetrend *)
TBdetrend[data_,bandWidth_]:=Module[{tVals,xVals,xValsDetrend,output},
tVals=data[[;;,1]];
xVals=data[[;;,2]];
xValsDetrend=GaussianFilter[xVals,Length[data]*bandWidth];
output=Transpose[{tVals,xValsDetrend}]
]


(* TBvariance *)
TBvariance[data_,rollWindow_]:=Module[{tVals,xVals,windowComps,varSeries,output},
tVals=data[[;;,1]];
xVals=data[[;;,2]];
(* number of components in the rolling window *)
windowComps=Floor[rollWindow*Length[tVals]];
varSeries=MovingMap[Variance,xVals,windowComps];
(* output as a time-series *)
output=Transpose[{tVals[[windowComps+1;;]],varSeries}]
];


(* TBautocorrelation *)
TBautocorrelation[data_,rollWindow_,lags_]:=Module[{tVals,xVals,windowComps,acSeries,output},
tVals=data[[;;,1]];
xVals=data[[;;,2]];
(* number of components in the rolling window *)
windowComps=Floor[rollWindow*Length[tVals]];
acSeries=Table[MovingMap[CorrelationFunction[#,lags[[i]]]&,xVals,windowComps],{i,1,Length[lags]}];
(* output as list of time-series *)
output=Table[Transpose[{tVals[[windowComps+1;;]],acSeries[[i]]}],{i,1,Length[lags]}]
];


(* TBskewness *)
TBskewness[data_,rollWindow_]:=Module[{tVals,xVals,windowComps,skewSeries,output},
tVals=data[[;;,1]];
xVals=data[[;;,2]];
(* number of components in the rolling window *)
windowComps=Floor[rollWindow*Length[tVals]];
skewSeries=MovingMap[Skewness,xVals,windowComps];
(* output as a time-series in skewness *)
output=Transpose[{tVals[[windowComps+1;;]],skewSeries}]
];


(* TBewsCompute *)

(* Define options and their default settings for TBewsCompute *)
Options[TBewsCompute]={RollWindow->0.25,BandWidth->0.2,LagTimes->{1}}; 

TBewsCompute[data_,OptionsPattern[]]:=Module[{rw,bw,lags,tVals,xVals,smooth,residuals,variance,skewness,autocorrelation,output},
(* assign options to variables *)
rw=OptionValue[RollWindow];
bw=OptionValue[BandWidth];
lags=OptionValue[LagTimes];
(* extract data *)
tVals=data[[;;,1]];
xVals=data[[;;,2]];
(* detrend the data *)
smooth=TBdetrend[data,bw];
(* find residuals *)
residuals=Transpose[{tVals,xVals-smooth[[;;,2]]}];
(* find variance of residuals *)
variance=TBvariance[residuals,rw];
(* find skewness of residuals *)
skewness=TBskewness[residuals,rw];
(* find the autocorrelation of residuals *)
autocorrelation=TBautocorrelation[residuals,rw,lags];
(* output *)
output={smooth,residuals,variance,autocorrelation,skewness}
]


(* TBindPlot *)

(* define options and their defualt settings *)
Options[TBindPlot]={TBxAxes->True,TBlabel->"",TByLabel->"",TBarrow->False,TByRange->All};

TBindPlot[data_,opts:OptionsPattern[]]:=
Module[{xAxes,label,yLabel,arrow,yRange},
(* assign options to local varialbes *)
xAxes=OptionValue[TBxAxes];
label=OptionValue[TBlabel];
yLabel=OptionValue[TByLabel];
arrow=OptionValue[TBarrow];
yRange=OptionValue[TByRange];

(* make the plot *)
ListLinePlot[data,FilterRules[{opts},Options[ListLinePlot]],
Frame->True,
PlotRange->{{0,Max[data[[;;,1]]]},yRange},
LabelStyle->14,
FrameLabel->{{yLabel,""},{If[xAxes,"Time",""],""}},FrameTicksStyle->If[xAxes,{{Automatic,Automatic},{Automatic,Automatic}},
{{Automatic,Automatic},{Directive[FontOpacity->0,FontSize->0],Automatic}}],
PlotRangeClipping->False, (* allows display of text outside of frame *)
ImagePadding->{{60,50},{If[xAxes,40,20],20}},
ImageSize->400,
AspectRatio->0.3,
Epilog->{Directive[{Black}],Arrowheads[{-0.03,0.03}],
If[arrow,Arrow[{Scaled[{0,0.15},{0,0}],Scaled[{0,0.15},{Min[data[[;;,1]]],0}]}]],
Text[Style[label,14,Bold],Scaled[{0.035,0.86}]]}
]
];


(* end private context *)
End[];


EndPackage[]
