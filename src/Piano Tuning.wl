(* ::Package:: *)

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
noteRange={"A0","C8"};
noteRangeNum=note2num/@noteRange;
(*frequency ratio utils*)
freqRatio2cents[x_]:=1200.*Log[2,x];
freqRatio2pitch[x_]:=12.*Log[2,x];


(*temperment functions*)
temDirectory=NotebookDirectory[]<>"../res/temperments/"<>"1-4 Syntonic Meantone.tem";
tem=Import[temDirectory];
temDecode=Map[StringSplit,StringSplit[tem,"\n"]];
temArray=SortBy[Table[{noteDict[temDecode[[i,1]]],temDecode[[i,2]]},{i,Length[temDecode]}],First][[;;,2]];
(*input: note name [such as: "G"]*)
temRotate[x_]:=RotateRight[temArray,noteDict[x]];


(*import samples*)
wavDirectory=NotebookDirectory[]<>"../res/samples/"<>"upright/";
wavNames=Import[wavDirectory];
noteNames=First[StringSplit[#,"."]]&/@wavNames;
noteNums=If[LetterQ[StringTake[#,1]],note2num[#],ToExpression[#]-1]&/@noteNames;

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
overtoneAnalysis[x_]:=((*test note x*)
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


(*inharmonicity model*)
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
deleteNotes={"F#7"};
deleteNotes=If[StringQ[#],note2num[#],#-1]&/@deleteNotes;
ihProperty=Select[ihProperty,!MemberQ[deleteNotes,#[[1]]]&];
(*ih property is the IH parameters*)
ihFunctionExtraction[ihProperty_]:=(ihPropertyFunction=Interpolation[ihProperty];
ihPlot=Show[Plot[ihPropertyFunction[x],{x,0,87},Axes->None,ImageSize->1600,Frame->True,AspectRatio->1/GoldenRatio,PlotStyle->LightGray,FrameLabel->{"Key Number","Inharmonicity Value"}],Graphics[Flatten[{Red,PointSize[Large],Table[Point[ihProperty[[i]]],{i,Length[ihProperty]}],Black,Table[Text[num2note[ihProperty[[i,1]]],{ihProperty[[i,1]],ihProperty[[i,2]]+0.15If[OddQ[ihProperty[[i,1]]],1,-1]},Background->White],{i,Length[ihProperty]}]}]]];
(*restore ih overtone property function*)
ihfunc[k_,n_]:=n*Sqrt[1+E^ihPropertyFunction[k]/ihFitScaling*(n-1)^2];ihfunc);
ihFunction=ihFunctionExtraction[ihProperty];


(*tuning curve model*)
(*tuning curve parameters*)
tunPolyOrder=5;
vars=Table[ToExpression["X"<>ToString[i]],{i,tunPolyOrder}];
Clear/@vars;
(*unit: cents*)
tunTrialPloy[x_]=Expand[Total[Table[vars[[i]]*(x-note2num["A4"])^i,{i,tunPolyOrder}]]];
(*define tuning method*)
tunMethod=Import[NotebookDirectory[]<>"prams/"<>"tuning_method","Text"];
tunMethod=StringSplit/@StringSplit[tunMethod,"\n"];
tunMethod=Table[temp=StringSplit[tunMethod[[i,1]],"~"];
If[temp[[1]]=="*",temp[[1]]=noteRange[[1]]];
If[temp[[2]]=="*",temp[[2]]=noteRange[[2]]];
temp=note2num/@temp;
temp1=Reverse[Sort[ToExpression/@StringSplit[tunMethod[[i,2]],"/"]]];
temp2=freqRatio2pitch@(Divide@@temp1);
{temp,temp1,temp2},{i,Length[tunMethod]}];



tunCurveObjFunction=Total@Flatten@Table[temp=tunMethod[[i,1]];
temp1=tunMethod[[i,2]];
temp2=tunMethod[[i,3]];
Table[
overtoneDifferenceCents=freqRatio2cents[(ihFunction[j,temp1[[1]]])/(ihFunction[j+temp2,temp1[[2]]]/temp1[[2]]*temp1[[1]])];
Expand[(overtoneDifferenceCents-tunTrialPloy[j+temp2]+tunTrialPloy[j])^2]
,{j,temp[[1]],Round[Min[noteRangeNum[[2]]-temp2,temp[[2]]]]}]
,{i,Length[tunMethod]}];
tunOptimize=NMinimize[tunCurveObjFunction,vars];
tunCurveFunction[x_]=tunTrialPloy[x]/.(tunOptimize[[2]]);
tunCurvePlot=Graphics[Flatten[{{Red,Thick,Circle[{48,tunCurveFunction[48]},.6]},Table[{If[Mod[x-3,12]==0,{GrayLevel[.4],Disk[{x,tunCurveFunction[x]},.6]}],If[num2wb[x],Black,LightGray],Disk[{x,tunCurveFunction[x]},.3]},{x,noteRangeNum[[1]],noteRangeNum[[2]]}]}],ImageSize->1600,Frame->True,GridLines->{Table[i,{i,noteRangeNum[[1]],noteRangeNum[[2]]}],Automatic},GridLinesStyle->LightGray,FrameLabel->{"Key Number","Pitch (cents)"}];
