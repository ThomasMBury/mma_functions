(* ::Package:: *)

(* begin package containing my user-defined functions *)
BeginPackage["myFunctions`"];


(* ::Section:: *)
(*Function names and descriptions*)


(* TBdetrend *)
TBdetrend::usage="\!\(\*
StyleBox[\"smooth\",\nFontSlant->\"Italic\"]\)=TBdetrend[\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"bandwidth\",\nFontSlant->\"Italic\"]\)] uses a Gaussian filter to smooth data
\!\(\*
StyleBox[\"Input\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"vars\",\nFontWeight->\"Bold\"]\)
data: 1xn array - time-series
bandwidth: number in (0,1) - proportion of the length of data to use as a bandwidth
\!\(\*
StyleBox[\"Output\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"vars\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"smooth\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\":\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"xn\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"array\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"of\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"smoothed\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"data\",\nFontWeight->\"Plain\"]\)";


(* TBvariance *)
TBvariance::usage="\!\(\*
StyleBox[\"varSeries\",\nFontSlant->\"Italic\"]\)=TBvariance[\!\(\*
StyleBox[\"residuals\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"rollWindow\",\nFontSlant->\"Italic\"]\)] computes the variance of \!\(\*
StyleBox[\"residuals\",\nFontSlant->\"Italic\"]\) over a rolling window.
\!\(\*
StyleBox[\"Input\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"vars\",\nFontWeight->\"Bold\"]\)
residuals: 1xn array - residuals of time-series data - should be stationary
rollWindow: number in (0,1) - proportion of the length of data to use as a rolling window
\!\(\*
StyleBox[\"Output\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"vars\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"varSeries\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\":\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"x\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"-\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"rollWindow\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\")\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"n\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\")\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"array\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Plain\"]\)";


(* TBautocorrelation *)
TBautocorrelation::usage="\!\(\*
StyleBox[\"acSeries\",\nFontSlant->\"Italic\"]\)=TBdetrend[\!\(\*
StyleBox[\"residuals\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"rollWindow\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"lag\",\nFontSlant->\"Italic\"]\)] computes the autocorrelation of \!\(\*
StyleBox[\"residuals\",\nFontSlant->\"Italic\"]\) over a rolling window.
\!\(\*
StyleBox[\"Input\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"vars\",\nFontWeight->\"Bold\"]\)
residuals: 1xn array - residuals of time-series data - should be stationary
rollWindow: number in (0,1) - proportion of the length of data to use as a rolling window
lag: ingeger value - lag between data points where correlation evaluated.
\!\(\*
StyleBox[\"Output\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"vars\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"acSeries\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\":\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"x\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"-\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"rollWindow\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\")\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"n\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\")\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"array\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Plain\"]\)";


(* TBautocorrelation *)
TBautocorrelation::usage="\!\(\*
StyleBox[\"acSeries\",\nFontSlant->\"Italic\"]\)=TBdetrend[\!\(\*
StyleBox[\"residuals\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"rollWindow\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"lag\",\nFontSlant->\"Italic\"]\)] computes the autocorrelation of \!\(\*
StyleBox[\"residuals\",\nFontSlant->\"Italic\"]\) over a rolling window.
\!\(\*
StyleBox[\"Input\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"vars\",\nFontWeight->\"Bold\"]\)
residuals: 1xn array - residuals of time-series data - should be stationary
rollWindow: number in (0,1) - proportion of the length of data to use as a rolling window
lag: ingeger value - lag between data points where correlation evaluated.
\!\(\*
StyleBox[\"Output\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"vars\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"acSeries\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\":\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"x\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"-\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"rollWindow\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\")\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"n\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\")\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"array\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Plain\"]\)";


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


(* end private context *)
End[];


EndPackage[]
