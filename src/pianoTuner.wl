(* ::Package:: *)

Options[pianoTuner]={noteRange->{"A0","C8"},deleteNotes->{},noteStart->"A0",tuningSplit->"C#4",
tuningMethod->{"6:3","4:1"},polynomialOrder->7,temperment->"",tempermentMajor->"C"};
pianoTuner[folder_,OptionsPattern[]]:=Module[{catchupFunction,catchupOvertone,catchupPosition,currentOvertone,currentOvertonePosition,
deleteNotesO,fitData,freqRatio2cents,freqRatio2pitch,guessNextOvertonePosition,guessOneOvertoneLengthPoints,headSampleVolume,
ihFitScaling,ihfunc,ihFunction,ihFunctionExtraction,ihPlot,ihProperty,ihProperty0,ihProperty2,ihPropertyFunction,largefunc,maxBands,
note2num,noteDict,noteNames,noteNums,noteRangeNum,noteRangeO,noteStartN,noteStartO,num2freq,num2note,num2wb,oneOvertoneLengthPoints,
overtoneAnalysis,overtoneDifferenceCents,overtoneSequence,overtoneTable,revNoteDict,smallfunc,tailSampleVolume,tem,temArray,temDecode,
temDirectory,temp,temRotate,tunCurveFunction,tunCurveObjFunction,tunCurvePlot,tunMethod,tunOptimize,tunPolyOrder,
tunTrialPloy,vars,wavAnalyzeCutFrequency,wavAnalyzeCutOvertone,wavAnalyzeCutOvertoneF,wavAnalyzeOvertoneData,wavAnalyzeOvertoneDataPool,
wavAnalyzePartitionTime,wavCatchupAnalyzeFrequencyBands,wavCatchupWeightedAverageBands,wavCutFrequency,wavData,wavDirectory,wavFourier,
wavFourierCatchupPeakStart,wavFourierCatchupPeakStartPosition,wavGuessOneOvertoneLengthPosition,wavIdealFreq,wavImport,wavNames,
wavOneOvertoneSamples,wavPartitions,wavPartitionsLength,wavPeakOvertone,wavPeakPosition,wavSampleRate,wavTrimData,
weightedAverageOvertone,whiteBlackKeyDict,tunSplitPoint,tunBassOctave,tunBassOctavePitch,tunTenorOctave,tunTenorOctavePitch,
tunCurveBassObjFunction,tunCurveTenorObjFunction,tunDeviation,tunDeviationPlot,tunLimitFreq,tunPartials,
tunTable,playFreq},
(*1. global parameters and functions*)
(*note dictionaries*)
noteDict=Association["C"->0,"C#"->1,"D"->2,"D#"->3,"E"->4,"F"->5,"F#"->6,"G"->7,"G#"->8,"A"->9,"A#"->10,"B"->11];
revNoteDict=Association[0->"C",1->"C#",2->"D",3->"D#",4->"E",5->"F",6->"F#",7->"G",8->"G#",9->"A",10->"A#",11->"B"];
(*start from A, False:white; True:black*)
whiteBlackKeyDict={False,True,False,False,True,False,True,False,False,True,False,True};
(*note conversion; A0 is 0; C8 is 87*)
note2num[x_]:=If[StringContainsQ[x,"#"],
noteDict[ToUpperCase[StringTake[x,2]]]+12*ToExpression[StringDrop[x,2]]-9,
noteDict[ToUpperCase[StringTake[x,1]]]+12*ToExpression[StringDrop[x,1]]-9];
num2note[x_]:=revNoteDict[Mod[x+9,12]]<>ToString[Floor[(x+9)/12]];
num2freq[x_]:=440.*2^((x-48)/12);
num2wb[x_]:=whiteBlackKeyDict[[Mod[x,12]+1]];
(*note range*)
noteRangeO=OptionValue[noteRange];
noteRangeNum=note2num/@noteRangeO;
(*starting customer note*)
noteStartO=OptionValue[noteStart];
noteStartN=note2num[noteStartO]+1;
(*frequency ratio utils*)
freqRatio2cents[x_]:=1200.*Log[2,x];
freqRatio2pitch[x_]:=12.*Log[2,x];

(**********************************************************************************)
(**********************************************************************************)
(*2. import samples*)
(*import samples*)
wavDirectory=folder;
wavNames=Import[wavDirectory];
noteNames=First[StringSplit[#,"."]]&/@wavNames;
noteNums=If[LetterQ[StringTake[#,1]],note2num[#],ToExpression[#]-noteStartN]&/@noteNames;

(*wave slices parameters*)
headSampleVolume=0.9;
tailSampleVolume=0.05;
wavAnalyzePartitionTime=0.08;
wavAnalyzeCutFrequency=10000;
wavAnalyzeCutOvertoneF[x_]:=Which[x<40,17,x<60,13,True,8];
wavCatchupAnalyzeFrequencyBands=0.08;
wavFourierCatchupPeakStart=0.8;
(*weighted average for the targeted catchup frequencies*)
wavCatchupWeightedAverageBands=0.01;

(**********************************************************************************)
(**********************************************************************************)
(*3. catchup frequency peaks*)
(*catchup functions:
input: next overtone position: positive is forward, negative is backward
output: the overtone around that position*)
catchupFunction[guessNextOvertonePosition_,guessOneOvertoneLengthPointsF_]:=((*find the peak position*)
wavAnalyzeOvertoneData=wavFourier[[Round[guessNextOvertonePosition-wavCatchupAnalyzeFrequencyBands*guessOneOvertoneLengthPointsF];;Round[guessNextOvertonePosition+wavCatchupAnalyzeFrequencyBands*guessOneOvertoneLengthPointsF]]];
maxBands=wavCatchupAnalyzeFrequencyBands+wavCatchupWeightedAverageBands;
wavAnalyzeOvertoneDataPool=wavFourier[[Round[guessNextOvertonePosition-maxBands*guessOneOvertoneLengthPointsF];;Round[guessNextOvertonePosition+maxBands*guessOneOvertoneLengthPointsF]]];
(*peak position*)
catchupPosition=First@First@Position[wavAnalyzeOvertoneDataPool[[;;,2]],Max[wavAnalyzeOvertoneData[[;;,2]]]];
(*get weight average overtone*)
weightedAverageOvertone=wavAnalyzeOvertoneDataPool[[Round[catchupPosition-wavCatchupWeightedAverageBands*guessOneOvertoneLengthPointsF];;Round[catchupPosition+wavCatchupWeightedAverageBands*guessOneOvertoneLengthPointsF]]];
weightedAverageOvertone=Total[Table[weightedAverageOvertone[[i,1]]*weightedAverageOvertone[[i,2]],{i,Length[weightedAverageOvertone]}]]/Total[Table[weightedAverageOvertone[[i,2]],{i,Length[weightedAverageOvertone]}]];
weightedAverageOvertone);

(*overtone analysis function:
input: the index of samples namespace;
output: the association of note number \[Rule] overtones sequences*)
overtoneAnalysis[x_]:=(
(*prepare analyze data*)
wavImport=Import[wavDirectory<>wavNames[[x]],"Sound"];
wavIdealFreq=num2freq[noteNums[[x]]];
wavSampleRate=wavImport[[1,2]];
wavData=Mean[wavImport[[1,1]]];
wavData=wavData/Max[wavData];
(*trim wave data*)
wavData=wavData[[First[First[Position[wavData,1.]]];;]];
wavPartitions=Partition[wavData,Floor[wavAnalyzePartitionTime*wavSampleRate]];
wavPartitionsLength=Length[wavPartitions];
i=1;
wavTrimData={};
While[Max[Abs[wavPartitions[[i]]]]>=headSampleVolume&&i<=wavPartitionsLength;i++];
While[Max[Abs[wavPartitions[[i]]]]>=tailSampleVolume&&i<=wavPartitionsLength,AppendTo[wavTrimData,wavPartitions[[i]]];i++];
wavTrimData=Flatten[wavTrimData];
(*fourier analysis*)
wavAnalyzeCutOvertone=wavAnalyzeCutOvertoneF[noteNums[[x]]]+1;
wavFourier=Abs[Fourier[wavTrimData]];
wavFourier=Table[{i*wavSampleRate/Length[wavTrimData],wavFourier[[i]]},{i,IntegerPart[Length[wavFourier]/2]}];
wavCutFrequency=Min[wavIdealFreq*wavAnalyzeCutOvertone,wavAnalyzeCutFrequency];
wavFourier=Select[wavFourier,First[#]<wavCutFrequency&];
wavFourier=Table[{wavFourier[[i,1]]/wavIdealFreq,wavFourier[[i,2]]},{i,Length[wavFourier]}];
(*ListLogPlot[wavFourier,PlotRange\[Rule]All,Joined\[Rule]True,Frame\[Rule]True,Axes\[Rule]False,AspectRatio\[Rule]1/10,ImageSize\[Rule]1600,FrameTicks\[Rule]{Table[i,{i,0,wavAnalyzeCutOvertone}],None},GridLines\[Rule]{Table[i,{i,0,wavAnalyzeCutOvertone}],Automatic},FrameLabel\[Rule]{"Overtone","Volume (dB)"}]*)

(*catch up the peaks of frequencies*)
wavOneOvertoneSamples=wavIdealFreq/wavSampleRate*Length[wavTrimData];
wavFourierCatchupPeakStartPosition=Round[wavFourierCatchupPeakStart*wavOneOvertoneSamples];
wavPeakPosition=First@First@Position[wavFourier[[;;,2]],Max[wavFourier[[wavFourierCatchupPeakStartPosition;;,2]]]];
wavPeakOvertone=wavFourier[[wavPeakPosition,1]];
(*build up overtone sequences*)
overtoneSequence={};
(*initialize the peak points properties*)
wavGuessOneOvertoneLengthPosition=wavPeakPosition/Round[wavPeakOvertone];
oneOvertoneLengthPoints=wavPeakPosition/wavPeakOvertone;

(*forward catchup frequencies*)
(*loop initilization parameters*)
currentOvertone=wavPeakOvertone;
currentOvertonePosition=wavPeakPosition;
guessOneOvertoneLengthPoints=wavGuessOneOvertoneLengthPosition;
overtoneSequence=Union[Append[overtoneSequence,catchupFunction[currentOvertonePosition,guessOneOvertoneLengthPoints]]];
(*loop body*)
While[currentOvertonePosition+guessOneOvertoneLengthPoints+(wavCatchupAnalyzeFrequencyBands+wavCatchupWeightedAverageBands)*guessOneOvertoneLengthPoints<Length[wavFourier],
(*guess the next overtone*)
guessNextOvertonePosition=currentOvertonePosition+guessOneOvertoneLengthPoints;
(*catchup the overtone*)
catchupOvertone=catchupFunction[guessNextOvertonePosition,guessOneOvertoneLengthPoints];
(*loop update parameters*)
guessOneOvertoneLengthPoints=Round[(catchupOvertone-currentOvertone)*oneOvertoneLengthPoints];
currentOvertone=catchupOvertone;
currentOvertonePosition=Round[catchupOvertone*oneOvertoneLengthPoints];
overtoneSequence=Union[Append[overtoneSequence,catchupOvertone]];];

(*backward catchup frequencies*)
If[Round[wavPeakOvertone]!=1,
(*loop initilization parameters*)
currentOvertone=wavPeakOvertone;
currentOvertonePosition=wavPeakPosition;
(*loop body*)
While[currentOvertonePosition-wavGuessOneOvertoneLengthPosition-(wavCatchupAnalyzeFrequencyBands+wavCatchupWeightedAverageBands)>wavFourierCatchupPeakStart*oneOvertoneLengthPoints,
(*guess the next overtone*)
guessNextOvertonePosition=currentOvertonePosition-wavGuessOneOvertoneLengthPosition;
(*catchup the overtone*)
catchupOvertone=catchupFunction[guessNextOvertonePosition,oneOvertoneLengthPoints];
(*loop update parameters*)
currentOvertone=catchupOvertone;
currentOvertonePosition=Round[catchupOvertone*oneOvertoneLengthPoints];
overtoneSequence=Union[Prepend[overtoneSequence,catchupOvertone]];
]];
noteNums[[x]]->overtoneSequence
);

(*find out all overtone properties*)
overtoneTable=Association[ParallelTable[overtoneAnalysis[i],{i,Length[noteNums]}]];

(**********************************************************************************)
(**********************************************************************************)
(*4. inharmonicity model*)
(*ih parameters*)
ihFitScaling=1000;
(*function build*)
ihProperty0=SortBy[Table[
fitData=overtoneTable[noteNums[[i]]];
fitData=fitData/fitData[[1]];
Clear[B,n];
fitData=Table[{i-1,fitData[[i]]/i},{i,Length[fitData]}];
(*A is always nearly 1, thus ignored*)
Flatten@{noteNums[[i]],ihFitScaling*B/.FindFit[fitData,A*Sqrt[1+B*n^2],{{A,1},{B,0}},n]},{i,Length[noteNums]}],First];
ihProperty=Select[ihProperty0,Last[#]>0&];
ihProperty=Table[{ihProperty[[i,1]],Log[ihProperty[[i,2]]]},{i,Length[ihProperty]}];
(*ignore the notes for outliers*)
deleteNotesO=OptionValue[deleteNotes];
deleteNotesO=If[StringQ[#],note2num[#],#-noteStartN]&/@deleteNotesO;
ihProperty=Select[ihProperty,!MemberQ[deleteNotesO,#[[1]]]&];
(*ih property is the IH parameters*)
ihFunctionExtraction[ihProperty_]:=(temp=ihProperty[[;;,1]];
smallfunc=Fit[Select[ihProperty,#[[1]]<=15&],{1,x},x];
largefunc=Fit[Select[ihProperty,#[[1]]>=40&],{1,x},x];
ihProperty2=Flatten[{Table[{x,smallfunc},{x,noteRangeNum[[1]]-48,temp[[1]]-1}],ihProperty,Table[{x,largefunc},{x,temp[[-1]]+1,noteRangeNum[[2]]+48}]},1];
ihPropertyFunction=Interpolation[ihProperty2];
ihPlot=Show[Plot[ihPropertyFunction[k],{k,noteRangeNum[[1]],noteRangeNum[[2]]},PlotRange->All,Axes->None,ImageSize->1600,Frame->True,AspectRatio->1/4,PlotStyle->Directive[Thick,Red],FrameLabel->{"Keys","Inharmonicity Value"}],Graphics[Flatten[{Black,PointSize[.007],Table[Point[ihProperty[[i]]],{i,Length[ihProperty]}],Black,Table[Text[num2note[ihProperty[[i,1]]],{ihProperty[[i,1]],ihProperty[[i,2]]+0.3If[OddQ[ihProperty[[i,1]]],1,-1]},Background->White],{i,Length[ihProperty]}]}]],FrameTicks->{None,Automatic},GridLines->{Table[i,{i,noteRangeNum[[1]],noteRangeNum[[2]]}],Table[i,{i,-100,100}]},GridLinesStyle->LightRed];
(*restore ih overtone property function*)
ihfunc[k_,n_]:=n*Sqrt[1+E^ihPropertyFunction[k]/ihFitScaling*(n-1)^2];ihfunc);
ihFunction=ihFunctionExtraction[ihProperty];

(**********************************************************************************)
(**********************************************************************************)
(*5. tuning curve optimization*)
(*tuning curve parameters*)
tunLimitFreq=16000;
tunPolyOrder=OptionValue[polynomialOrder];
vars=Table[ToExpression["X"<>ToString[i]],{i,tunPolyOrder}];
Clear/@vars;
(*unit: cents*)
tunTrialPloy[x_]=Expand[Total[Table[vars[[i]]*(x-note2num["A4"])^i,{i,tunPolyOrder}]]];
(*define tuning method*)
tunMethod=OptionValue[tuningMethod];
tunSplitPoint=OptionValue[tuningSplit];
tunMethod=Table[Sort[ToExpression[StringSplit[tunMethod[[i]],":"]]],{i,Length[tunMethod]}];
tunSplitPoint=note2num@tunSplitPoint;
(*bass optimize*)
tunBassOctave=tunMethod[[1,2]]/tunMethod[[1,1]];
tunBassOctavePitch=freqRatio2pitch[tunBassOctave];
tunCurveBassObjFunction=Expand[Total[Table[overtoneDifferenceCents=freqRatio2cents[ihFunction[j,tunMethod[[1,2]]]/ihFunction[j+tunBassOctavePitch,tunMethod[[1,1]]]/tunBassOctave];
(overtoneDifferenceCents-(tunTrialPloy[j+tunBassOctavePitch]-tunTrialPloy[j]))^2
,{j,noteRangeNum[[1]],tunSplitPoint}]]];
(*tenor optimize*)
tunTenorOctave=tunMethod[[2,2]]/tunMethod[[2,1]];
tunTenorOctavePitch=freqRatio2pitch[tunTenorOctave];
tunCurveTenorObjFunction=Expand[Total[Table[overtoneDifferenceCents=freqRatio2cents[ihFunction[j-tunTenorOctavePitch,tunMethod[[2,2]]]/ihFunction[j,tunMethod[[2,1]]]/tunTenorOctave];
(overtoneDifferenceCents-(tunTrialPloy[j]-tunTrialPloy[j-tunTenorOctavePitch]))^2
,{j,tunSplitPoint+1,noteRangeNum[[2]]}]]];
(*optimize function*)
tunCurveObjFunction=tunCurveBassObjFunction+tunCurveTenorObjFunction;
tunOptimize=NMinimize[tunCurveObjFunction,vars];
tunCurveFunction[x_]=tunTrialPloy[x]/.(tunOptimize[[2]]);
tunCurvePlot=Graphics[Flatten[{Table[{If[Mod[x-3,12]==0,{GrayLevel[.4],Disk[{x,tunCurveFunction[x]},.6]}],If[num2wb[x],Black,If[x==48,Red,LightGray]],Disk[{x,tunCurveFunction[x]},.3]},{x,noteRangeNum[[1]],noteRangeNum[[2]]}]}],ImageSize->1600,Frame->True,GridLines->{Table[i,{i,noteRangeNum[[1]],noteRangeNum[[2]]}],Table[i,{i,-100,100,2}]},GridLinesStyle->LightBlue,FrameLabel->{None,"Tuning Curve (cents)"},FrameTicks->{None,Automatic}];
(*calculate Deviation*)
tunDeviation=Flatten[{Table[overtoneDifferenceCents=freqRatio2cents[ihFunction[j,tunMethod[[1,2]]]/ihFunction[j+tunBassOctavePitch,tunMethod[[1,1]]]/tunBassOctave];
(overtoneDifferenceCents-(tunTrialPloy[j+tunBassOctavePitch]-tunTrialPloy[j]))/.tunOptimize[[2]]
,{j,noteRangeNum[[1]],tunSplitPoint}],Table[overtoneDifferenceCents=freqRatio2cents[ihFunction[j-tunTenorOctavePitch,tunMethod[[2,2]]]/ihFunction[j,tunMethod[[2,1]]]/tunTenorOctave];
(overtoneDifferenceCents-(tunTrialPloy[j]-tunTrialPloy[j-tunTenorOctavePitch]))/.tunOptimize[[2]]
,{j,tunSplitPoint+1,noteRangeNum[[2]]}]}];
tunDeviationPlot=Graphics[Flatten[{Table[{If[Mod[x-3,12]==0,{GrayLevel[.4],Disk[{x,tunDeviation[[x+1]]},.6]}],If[num2wb[x],Black,If[x==48,Red,LightGray]],Disk[{x,tunDeviation[[x+1]]},.3]},{x,noteRangeNum[[1]],noteRangeNum[[2]]}]}],ImageSize->1600,Frame->True,GridLines->{Table[i,{i,noteRangeNum[[1]],noteRangeNum[[2]]}],Table[i,{i,-100,100,2}]},GridLinesStyle->LightBlue,FrameLabel->{None,"Deviation (cents)"},FrameTicks->{None,Automatic}];


(**********************************************************************************)
(**********************************************************************************)
(*6. temperment mask model*)
temDirectory=OptionValue[temperment];
If[temDirectory!="",
If[!DirectoryQ[temDirectory],temDirectory=NotebookDirectory[]<>"../res/temperments/"<>temDirectory];
tem=Import[temDirectory];
temDecode=Map[StringSplit,StringSplit[tem,"\n"]];
temArray=SortBy[Table[{noteDict[temDecode[[i,1]]],temDecode[[i,2]]},{i,Length[temDecode]}],First][[;;,2]];
temArray=ToExpression/@temArray;
(*input: note name [such as: "G"]*)
temRotate[x_]:=(temp=RotateRight[temArray,noteDict[x]];temp-temp[[10]]);
temArray=temRotate[OptionValue[tempermentMajor]];
temp=RotateRight[temArray,3];
temDict=Association[Table[i->temp[[i+1]],{i,0,11}]];,
temDict=Association[Table[i->0,{i,0,11}]];
];

(**********************************************************************************)
(**********************************************************************************)
(*7. tuning piano data*)
tunRestoreFunction[k_,n_]:=(num2freq[k]*2^((tunCurveFunction[k]+temDict[Mod[k,12]])/1200)*ihFunction[k,n]);
playFreq[x_]:=EmitSound[Play[Sin[2*Pi*t*x],{t,0,0.2},SampleRate->44100]];
(*create tuning table*)
tunPartials=Import[NotebookDirectory[]<>"params/"<>"tuning_partials"];
tunPartials=Association[Flatten[Table[temp=StringSplit[tunPartials[[i,1]],"~"];
If[temp[[1]]=="*",temp[[1]]=noteRangeO[[1]]];
If[temp[[2]]=="*",temp[[2]]=noteRangeO[[2]]];
temp=note2num/@temp;
Table[j->tunPartials[[i,2]],{j,temp[[1]],temp[[2]]}]
,{i,Length[tunPartials]}]]];

tunTable=Framed[TableForm[Table[temp=tunRestoreFunction[k,n];
If[temp>tunLimitFreq,
"",With[{s=temp},Button[Style[temp,If[tunPartials[k]==n,Red,Black]],playFreq[s],Appearance->None]]
],{k,noteRangeNum[[1]],noteRangeNum[[2]]},{n,1,16}],
TableHeadings->{Table[num2note[k],{k,noteRangeNum[[1]],noteRangeNum[[2]]}],Table[i,{i,1,16}]}]];

(**********************************************************************************)
(**********************************************************************************)
(*8. return result*)
Column[{Deploy[Framed[Column[{tunCurvePlot,tunDeviationPlot,ihPlot}]]],tunTable}]
]
