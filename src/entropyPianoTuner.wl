(* ::Package:: *)

(*package directory*)
packageDirectory=NotebookDirectory[];
(*piano tuner function*)
Options[entropyPianoTuner]={noteRange->{"A0","C8"},noteStart->"A0",
A4Frequency->440,exportTunedSamples->"rbk",saveTuningFile->"",loadTuneShift->"",
reportFormat->"pdf"};
entropyPianoTuner[folder_,OptionsPattern[]]:=Module[{catchupFunction,catchupOvertone,catchupPosition,currentOvertone,
currentOvertonePosition,entropyCheck,entropyCost,entropyCostLast,entropyCurvePlot,entropyEvaluate,entropyPlotAxis,
entropyRandomDirection,entropyRandomNote,entropyResultReconstruct,entropyRoughness,entropyRoughSampleConstruct,
entropyRoughSamples,entropySampleConstruct,entropySamples,entropyShift,entropyShiftIdeal,entropyShiftTrial,
entropyStepChange,entropyTotal,freqPeakTable,freqRatio2cents,freqRatio2pitch,guessNextOvertonePosition,
guessOneOvertoneLengthSamples,headSampleVolume,initialTuning,note2num,noteChoicePool,noteChoicePoolO,noteDict,
noteNames,noteNums,noteRangeNum,noteRangeO,noteStartN,noteStartO,num2freq,num2note,num2pos,num2wb,oneOvertoneLengthPoints,
overtoneAnalysis,overtoneSequence,pitchErrorElimiate,playFreq,pos2num,result,revNoteDict,str,tableOvertoneLimit,
tailSampleVolume,temp,temp1,temp2,tunedPitchDeviationTable,tunPartials,tunPolyOrder,tunTable,tunTableString,
tunTableStringCents,tunTrialFunction,tunTrialPloy,vars,wavAnalyzeCutFrequency,wavAnalyzeCutNoteNum,wavAnalyzeOvertoneData,
wavAnalyzeOvertoneDataPool,wavAnalyzePartitionTime,wavAnalyzePitchMargin,wavAnalyzeRange,wavCatchupAnalyzeFrequencyBands,
wavCutFrequency,wavData,wavDirectory,wavFourier,wavFourierAnalyzePrecision,wavFourierCatchupPeakStart,
wavFourierCatchupPeakStartPosition,wavFourierInterpolation,wavGuessOneOvertoneLengthPosition,wavIdealFreq,wavImport,
wavInterpolation,wavInterpolationReconstruct,wavLeastAnalyzeTime,wavNames,wavNoteNum,wavOneOvertoneSamples,wavOvertone,
wavPartitions,wavPartitionsLength,wavPeakOvertone,wavPeakPosition,wavPitchShiftSample,wavSampleRate,wavStep,wavTrimData,
whiteBlackKeyDict,cent2shift,shift2cent,wavInterpolation1},
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
num2freq[x_]:=OptionValue[A4Frequency]*2^((x-48)/12);
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
wavLeastAnalyzeTime=0.5;
wavAnalyzeCutFrequency=10000;
wavAnalyzeCutNoteNum=Floor[Log[2,wavAnalyzeCutFrequency/440.]*12+48.];
(*catchup frequency peaks*)
wavCatchupAnalyzeFrequencyBands=0.08;
wavFourierCatchupPeakStart=0.8;
(*limit 12 overtones for tuning table*)
tableOvertoneLimit=12;
(*pitch analyze range*)
wavAnalyzePitchMargin=2;
wavAnalyzeRange={noteRangeNum[[1]]-wavAnalyzePitchMargin,wavAnalyzeCutNoteNum+wavAnalyzePitchMargin};
(*interpolation model functions*)
wavFourierAnalyzePrecision=0.001;
pos2num[x_]:=(x-1)*wavFourierAnalyzePrecision+wavAnalyzeRange[[1]];
num2pos[x_]:=Round[(x-wavAnalyzeRange[[1]])/wavFourierAnalyzePrecision+1];
(*entropy step & cent conversion*)
cent2shift[x_]:=Round[x/100/wavFourierAnalyzePrecision];
shift2cent[x_]:=x*100*wavFourierAnalyzePrecision;

(**********************************************************************************)
(**********************************************************************************)
(*3. construct spectrum*)
(*construct spectrum samples*)
entropySampleConstruct[x_]:=(
wavImport=Import[wavDirectory<>wavNames[[x]],"Sound"];
wavNoteNum=noteNums[[x]];
wavIdealFreq=num2freq[wavNoteNum];
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
If[i<wavLeastAnalyzeTime/wavAnalyzePartitionTime,While[i<wavLeastAnalyzeTime/wavAnalyzePartitionTime&&i<=wavPartitionsLength,AppendTo[wavTrimData,wavPartitions[[i]]];i++];];
wavTrimData=Flatten[wavTrimData];
(*fourier analysis*)
wavFourier=Abs[Fourier[wavTrimData]];
(*squared spectrum for analyze*)
wavFourier=Table[{Log[2.,((i-1)*wavSampleRate/Length[wavTrimData])/440]*12+48,wavFourier[[i]]^2},{i,IntegerPart[Length[wavFourier]/2]}];
wavFourier=Select[wavFourier,First[#]<wavAnalyzeRange[[2]]+5&&First[#]>=wavAnalyzeRange[[1]]-5&];
(*two sides are 0*)
Do[If[wavFourier[[i,1]]<wavNoteNum-5||wavFourier[[i,1]]>wavAnalyzeRange[[2]]-wavAnalyzePitchMargin,wavFourier[[i,2]]=0],{i,Length[wavFourier]}];
wavFourierInterpolation=Interpolation[wavFourier,InterpolationOrder->1];
wavInterpolationReconstruct=Table[{i,wavFourierInterpolation[i]},{i,wavAnalyzeRange[[1]],wavAnalyzeRange[[2]],wavFourierAnalyzePrecision}];
temp=1/Total[wavInterpolationReconstruct[[;;,2]]]/wavFourierAnalyzePrecision/(wavAnalyzeRange[[2]]-wavAnalyzeRange[[1]]);
wavInterpolationReconstruct[[;;,2]]=wavInterpolationReconstruct[[;;,2]]*temp;
(*calculate initial shift*)
(*calculate within 1.3 octave max frequency position*)
temp=Select[wavInterpolationReconstruct,#[[1]]<wavNoteNum+12*1.3&][[;;,2]];
temp1=Position[temp,Max[temp]][[1,1]];
wavOvertone=Round[num2freq[pos2num[temp1]]/num2freq[wavNoteNum]];
wavPitchShiftSample=(num2pos[wavNoteNum+freqRatio2pitch[wavOvertone]]-temp1);
result={wavNoteNum->wavPitchShiftSample,wavNoteNum->wavInterpolationReconstruct[[;;,2]]};
(*wavInterpolationReconstruct=Transpose[{wavInterpolationReconstruct[[;;,1]],RotateRight[wavInterpolationReconstruct[[;;,2]],wavPitchShiftSample]}];
ListPlot[wavInterpolationReconstruct,PlotRange\[Rule]All,Joined\[Rule]True,Frame\[Rule]True,Axes\[Rule]False,PlotStyle\[Rule]Pink,AspectRatio\[Rule]1/10,ImageSize\[Rule]1600,GridLines\[Rule]{Table[i,{i,wavAnalyzeRange[[1]],wavAnalyzeRange[[2]]}],Automatic},FrameLabel\[Rule]{"Key","Volume"}]*)
result);

(*build spectrum samples*)
result=ParallelTable[entropySampleConstruct[x],{x,Length[noteNums]}];
entropySamples=Association[result[[;;,2]]];
entropyShiftIdeal=Association[result[[;;,1]]];

(**********************************************************************************)
(**********************************************************************************)
(*4. capture frequency peaks*)
(*catchup functions:
input: next overtone position: positive is forward, negative is backward
output: the overtone around that position*)
catchupFunction[guessNextOvertonePosition_,guessOneOvertoneLengthSamplesF_]:=((*find the peak position*)wavAnalyzeOvertoneData=wavFourier[[Round[guessNextOvertonePosition-wavCatchupAnalyzeFrequencyBands*guessOneOvertoneLengthSamplesF];;Round[guessNextOvertonePosition+wavCatchupAnalyzeFrequencyBands*guessOneOvertoneLengthSamplesF]]];
wavAnalyzeOvertoneDataPool=wavFourier[[Floor[guessNextOvertonePosition-wavCatchupAnalyzeFrequencyBands*guessOneOvertoneLengthSamplesF];;1+Floor[guessNextOvertonePosition+wavCatchupAnalyzeFrequencyBands*guessOneOvertoneLengthSamplesF]]];
(*peak position*)catchupPosition=First@First@Position[wavAnalyzeOvertoneDataPool[[;;,2]],Max[wavAnalyzeOvertoneData[[;;,2]]]];
(*return the pitch*)
wavAnalyzeOvertoneDataPool[[catchupPosition,1]]);

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
If[i<wavLeastAnalyzeTime/wavAnalyzePartitionTime,While[i<wavLeastAnalyzeTime/wavAnalyzePartitionTime&&i<=wavPartitionsLength,AppendTo[wavTrimData,wavPartitions[[i]]];i++];];
wavTrimData=Flatten[wavTrimData];
(*fourier analysis*)
wavFourier=Abs[Fourier[wavTrimData]];
wavFourier=Table[{(i-1)*wavSampleRate/Length[wavTrimData],wavFourier[[i]]},{i,IntegerPart[Length[wavFourier]/2]}];
wavCutFrequency=Min[wavAnalyzeCutFrequency,(tableOvertoneLimit+1)*wavIdealFreq];
wavFourier=Select[wavFourier,First[#]<wavCutFrequency&];
wavFourier=Table[{wavFourier[[i,1]]/wavIdealFreq,wavFourier[[i,2]]},{i,Length[wavFourier]}];

(*catch up the peaks of frequencies*)
(*the highest peak*)
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
guessOneOvertoneLengthSamples=wavGuessOneOvertoneLengthPosition;
overtoneSequence=Union[Append[overtoneSequence,catchupFunction[currentOvertonePosition,guessOneOvertoneLengthSamples]]];
(*loop body*)
While[currentOvertonePosition+guessOneOvertoneLengthSamples+(wavCatchupAnalyzeFrequencyBands)*guessOneOvertoneLengthSamples<Length[wavFourier],
(*guess the next overtone*)
guessNextOvertonePosition=currentOvertonePosition+guessOneOvertoneLengthSamples;
(*catchup the overtone*)
catchupOvertone=catchupFunction[guessNextOvertonePosition,guessOneOvertoneLengthSamples];
(*loop update parameters*)
guessOneOvertoneLengthSamples=Round[(catchupOvertone-currentOvertone)*oneOvertoneLengthPoints];
currentOvertone=catchupOvertone;
currentOvertonePosition=Round[catchupOvertone*oneOvertoneLengthPoints];
overtoneSequence=Union[Append[overtoneSequence,catchupOvertone]];];

(*backward catchup frequencies*)
If[Round[wavPeakOvertone]!=1,
(*loop initilization parameters*)
currentOvertone=wavPeakOvertone;
currentOvertonePosition=wavPeakPosition;
(*loop body*)
While[currentOvertonePosition-wavGuessOneOvertoneLengthPosition-(wavCatchupAnalyzeFrequencyBands)>wavFourierCatchupPeakStart*oneOvertoneLengthPoints,
(*guess the next overtone*)
guessNextOvertonePosition=currentOvertonePosition-wavGuessOneOvertoneLengthPosition;
(*catchup the overtone*)
catchupOvertone=catchupFunction[guessNextOvertonePosition,oneOvertoneLengthPoints];
(*loop update parameters*)
currentOvertone=catchupOvertone;
currentOvertonePosition=Round[catchupOvertone*oneOvertoneLengthPoints];
overtoneSequence=Union[Prepend[overtoneSequence,catchupOvertone]];
]];
noteNums[[x]]->1.*overtoneSequence*wavIdealFreq
);

(*find all frequency peaks*)
freqPeakTable=Association[ParallelTable[overtoneAnalysis[i],{i,Length[noteNums]}]];
(*eliminate pitch error*)
pitchErrorElimiate=num2freq[48]/First[freqPeakTable[48]]/2^(shift2cent[entropyShift[48]]/1200);
(*initial tuning of entropy algorithm: 
Similar to traditional tuning method (simpler method): <=C#4: 6:3, > C#4: 4:1*)
tunPolyOrder=3;
vars=Table[ToExpression["X"<>ToString[i]],{i,tunPolyOrder}];
Clear/@vars;
(*unit: cents*)
tunTrialPloy[x_]=Expand[Total[Table[vars[[i]]^2*(x-note2num["A4"])^i,{i,tunPolyOrder}]]];
temp=Expand[Total[Table[(tunTrialPloy[i]-tunTrialPloy[i-24]-freqRatio2cents[Sort[freqPeakTable[i-24]][[4]]/Sort[freqPeakTable[i-24]][[1]]/4])^2, {i,41,noteRangeNum[[2]]}]]+Total[Table[(tunTrialPloy[i+12]-tunTrialPloy[i]-(freqRatio2cents[Sort[freqPeakTable[i]][[6]]/Sort[freqPeakTable[i]][[1]]/6]-freqRatio2cents[Sort[freqPeakTable[i+12]][[3]]/Sort[freqPeakTable[i+12]][[1]]/3]))^2, {i,noteRangeNum[[1]],40}]]];
tunTrialFunction=tunTrialPloy[x]/.(NMinimize[Expand[temp],vars][[2]]);
initialTuning=Association[Table[x->cent2shift[tunTrialFunction],{x,noteRangeNum[[1]],noteRangeNum[[2]]}]];

(**********************************************************************************)
(**********************************************************************************)
(*5. entropy model*)
(*run entropy model*)
(*pick rough tune samples from original samples*)
entropyRoughSampleConstruct[x_]:=(entropyRoughness=x;
entropyRoughness=cent2shift[entropyRoughness];
entropyRoughSamples=Table[i->Table[entropySamples[i][[j]],{j,1,Length[entropySamples[1]],entropyRoughness}],{i,noteRangeNum[[1]],noteRangeNum[[2]]}];
entropyRoughSamples=Association[entropyRoughSamples];);

(*tune shift file load or not*)
If[OptionValue[loadTuneShift]=="",
(*initialize tuning curve*)
entropyShift=Association[Table[i->initialTuning[i]+entropyShiftIdeal[i],{i,noteRangeNum[[1]],noteRangeNum[[2]]}]];
(*print tuning curve dynamic plot*)
Print[Dynamic[ListPlot[Table[{i,shift2cent[-entropyShiftIdeal[i]+entropyShift[i]]},{i,0,87}],PlotRange->All,Frame->True,Axes->False,PlotStyle->Pink,AspectRatio->1/10,ImageSize->1600,GridLines->{Table[i,{i,wavAnalyzeRange[[1]],wavAnalyzeRange[[2]]}],Automatic},FrameLabel->{"Key","Deviation (cent)"},GridLinesStyle->LightRed]]];
Do[(*step precision: cent*)
entropyRoughSampleConstruct[x];

entropyStepChange=1;
entropyCostLast=Infinity;
SetSharedVariable[entropyCostLast];
SetSharedVariable[entropyStepChange];
SetSharedVariable[entropyShift];
noteChoicePoolO=DeleteCases[Table[i,{i,noteRangeNum[[1]],noteRangeNum[[2]]}],48];

entropyEvaluate:=(entropyShiftTrial=entropyShift;
entropyShiftTrial[entropyRandomNote]=Round[entropyShiftTrial[entropyRandomNote]+entropyRoughness*entropyRandomDirection*If[x<1,RandomReal[],1]];
entropyTotal=Total[Table[RotateRight[entropyRoughSamples[i],Round[entropyShiftTrial[i]/entropyRoughness]],{i,noteRangeNum[[1]],noteRangeNum[[2]]}]];
entropyTotal=entropyTotal/Total[entropyTotal];
entropyCost=Total[Table[If[entropyTotal[[i]]!=0.,-entropyTotal[[i]]*Log[entropyTotal[[i]]],0],{i,Length[entropyTotal]}]];);
entropyCheck:=(If[entropyCostLast>entropyCost,entropyShift=entropyShiftTrial;
entropyCostLast=entropyCost;entropyStepChange++];);

While[entropyStepChange!=0,
entropyStepChange=0;noteChoicePool=Table[RandomSample[noteChoicePoolO,Length[noteChoicePoolO]],{$KernelCount}];
ParallelDo[entropyRandomNote=noteChoicePool[[$KernelID,j]];
entropyRandomDirection=RandomChoice[{1,-1}];
entropyEvaluate;
entropyCheck;
(*another direction*)
If[x<1||entropyCostLast<=entropyCost,entropyRandomDirection=-entropyRandomDirection;
entropyEvaluate;];
entropyCheck;
,{j,Length[noteChoicePoolO]}]];
Print[entropyTotal=Total[Table[RotateRight[entropyRoughSamples[i],Round[entropyShift[i]/entropyRoughness]],{i,noteRangeNum[[1]],noteRangeNum[[2]]}]];
entropyPlotAxis=Table[pos2num[j],{j,1,Length[entropySamples[1]],entropyRoughness}];
ListPlot[Transpose@{entropyPlotAxis,entropyTotal},Joined->True,PlotRange->All,Frame->True,Axes->False,PlotStyle->Pink,AspectRatio->1/10,ImageSize->1600,GridLines->{Table[i,{i,wavAnalyzeRange[[1]],wavAnalyzeRange[[2]]}],Automatic},FrameLabel->{"Key","Volume"},GridLinesStyle->LightBlue]],
{x,{1,0.5}}];,

(*else: read tune shift file*)
temp=ToExpression/@StringSplit[Import["/home/robert/gits/Piano_Tuning/res/samples/tune_shift","Text"]];
entropyShift=Association[Table[i->temp[[i-noteRangeNum[[1]]+1]],{i,noteRangeNum[[1]],noteRangeNum[[2]]}]];
];
(*plot the tuning curve*)
entropyRoughSampleConstruct[1];
entropyTotal=Total[Table[RotateRight[entropyRoughSamples[i],Round[entropyShift[i]/entropyRoughness]],{i,noteRangeNum[[1]],noteRangeNum[[2]]}]];
entropyPlotAxis=Table[pos2num[j],{j,1,Length[entropySamples[1]],entropyRoughness}];
entropyCurvePlot=Framed[Column[{Graphics[Flatten[{LightRed,Line[{{noteRangeNum[[1]],0},{noteRangeNum[[2]],0}}],Table[temp=shift2cent[entropyShift[x]-entropyShiftIdeal[x]];{If[Mod[x-3,12]==0,{GrayLevel[.4],Disk[{x,temp},.6]}],If[num2wb[x],Black,If[x==48,Red,LightGray]],Disk[{x,temp},If[num2wb[x],.22,.33]]},{x,noteRangeNum[[1]],noteRangeNum[[2]]}]}],ImageSize->1600,Frame->True,GridLines->{Table[i,{i,noteRangeNum[[1]],noteRangeNum[[2]]}],Table[i,{i,-100,100}]},GridLinesStyle->LightBlue,FrameLabel->{None,"Tuning Curve (cents)"},FrameTicks->{Automatic,Automatic}],
ListPlot[Transpose@{entropyPlotAxis,entropyTotal},Joined->True,PlotRange->All,Frame->True,Axes->False,PlotStyle->Pink,AspectRatio->1/10,ImageSize->1600,GridLines->{Table[i,{i,wavAnalyzeRange[[1]],wavAnalyzeRange[[2]]}],Automatic},FrameLabel->{"Key","Volume"},GridLinesStyle->LightRed]}]];

(**********************************************************************************)
(**********************************************************************************)
(*6. capture peaks and generate tuning table*)

tunedPitchDeviationTable=Association[Table[temp=Sort[freqPeakTable[i]*pitchErrorElimiate*2^(shift2cent[entropyShift[i]]/1200)];i->Table[{temp[[j]],freqRatio2cents[temp[[j]]/num2freq[i]/j]},{j,Length[temp]}],{i,Keys[freqPeakTable]}]];
(*generate tuning table*)
playFreq[x_]:=EmitSound[Play[Sin[2*Pi*t*x],{t,0,0.2},SampleRate->44100]];
(*create tuning table*)
tunPartials=Import[packageDirectory<>"params/"<>"tuning_partials"];
tunPartials=Association[Flatten[Table[temp=StringSplit[tunPartials[[i,1]],"~"];
If[temp[[1]]=="*",temp[[1]]=noteRangeO[[1]]];
If[temp[[2]]=="*",temp[[2]]=noteRangeO[[2]]];
temp=note2num/@temp;
Table[j->tunPartials[[i,2]],{j,temp[[1]],temp[[2]]}]
,{i,Length[tunPartials]}]]];
(*generate tuning table*)
tunTable=Framed[TableForm[Table[If[n<=Length[tunedPitchDeviationTable[k]],temp=tunedPitchDeviationTable[k][[n,1]];
temp2=tunedPitchDeviationTable[k][[n,2]];
tunTableString=ToString[temp];
tunTableStringCents=If[temp2>0,"+",""]<>ToString[Ceiling[temp2,0.01]]<>"\[Cent]";,tunTableString=""];
If[temp2==0.,tunTableString="["<>ToString[temp]<>"Hz]";tunTableStringCents=""];
If[tunTableString==""&&temp<wavAnalyzeCutFrequency,
"",With[{s=temp},Button[Row[{Style[tunTableString,If[temp2==0,Blue,If[tunPartials[k]==n,Red,Black]]],
Style[tunTableStringCents,If[tunPartials[k]==n,Pink,Gray],6]}],playFreq[s],Appearance->None]]
],{k,noteRangeNum[[1]],noteRangeNum[[2]]},{n,1,tableOvertoneLimit}],
TableHeadings->{Table[num2note[k],{k,noteRangeNum[[1]],noteRangeNum[[2]]}],Table[i,{i,tableOvertoneLimit}]},
TableSpacing->{.5,.3}]];

(**********************************************************************************)
(**********************************************************************************)
(*7. save tuned samples and tuned shifts*)
entropyResultReconstruct[x_]:=(
wavImport=Import[wavDirectory<>wavNames[[x]],"Sound"];
wavNoteNum=noteNums[[x]];
wavSampleRate=wavImport[[1,2]];
wavData=wavImport[[1,1,1]];
wavData=wavData/Max[wavData];
wavInterpolation=Interpolation[wavData];
wavData=wavImport[[1,1,2]];
wavData=wavData/Max[wavData];
wavInterpolation1=Interpolation[wavData];
wavStep=2^(shift2cent[entropyShift[wavNoteNum]]/1200)*pitchErrorElimiate;
temp={Table[wavInterpolation[i],{i,1,Length[wavData],wavStep}],Table[wavInterpolation1[i],{i,1,Length[wavData],wavStep}]};
ListPlay[{Table[wavInterpolation[i],{i,1,Length[wavData],wavStep}],Table[wavInterpolation1[i],{i,1,Length[wavData],wavStep}]},SampleRate->wavSampleRate,PlayRange->All]);
If[DirectoryQ[OptionValue[exportTunedSamples]],ParallelDo[temp=entropyResultReconstruct[i];str=OptionValue[exportTunedSamples]<>ToString[noteNums[[i]]]<>".wav";Export[str,temp],{i,Length[noteNums]}];];

(*save tune shift*)
If[OptionValue[saveTuningFile]!="",Export[packageDirectory<>OptionValue[saveTuningFile],StringRiffle[Table[ToString[entropyShift[i]],{i,noteRangeNum[[1]],noteRangeNum[[2]]}]," "],"Text"]];

(**********************************************************************************)
(**********************************************************************************)
(*8. return result*)
If[OptionValue[saveTuningFile]!="",
Export[packageDirectory<>OptionValue[saveTuningFile]<>"_curve."<>OptionValue[reportFormat],entropyCurvePlot];
Export[packageDirectory<>OptionValue[saveTuningFile]<>"_tuning."<>OptionValue[reportFormat],tunTable];
];
Column[{tunTable,entropyCurvePlot}];
];
