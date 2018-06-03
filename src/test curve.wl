(* ::Package:: *)

While[True,Pause[1];data=Table[{x,.01x^3+RandomReal[.1{-1,1}]},{x,-5,5,.5}];
lm=Table[x^i,{i,1,5}];
lmfunc=Fit[data,lm,x];
Print@Show[Plot[lmfunc,{x,-5.1,5.1}],ListPlot[data,PlotStyle->Red]]]
