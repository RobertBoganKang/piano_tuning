(* ::Package:: *)

Clear["Global`*"];
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
num2freq[x_]:=440*2^((x-48)/12);
num2wb[x_]:=whiteBlackKeyDict[[Mod[x,12]+1]];
(*note range*)
noteRangeO={"A0","C8"};
noteRangeNum=note2num/@noteRangeO;
(*starting customer note*)
noteStartO="A0";
noteStartN=note2num[noteStartO]+1;
(*frequency ratio utils*)
freqRatio2cents[x_]:=1200.*Log[2,x];
freqRatio2pitch[x_]:=12.*Log[2,x];

(**********************************************************************************)
(**********************************************************************************)
(*2. import samples*)
(*import samples*)
wavDirectory="/home/robert/gits/Piano_Tuning/res/samples/upright/";
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
wavCatchupAnalyzeFrequencyBands=0.08;
wavFourierCatchupPeakStart=0.8;
(*weighted average for the targeted catchup frequencies*)
wavCatchupWeightedAverageBands=0.01;
(*pitch range*)
wavAnalyzePitchMargin=2;
wavAnalyzeRange={noteRangeNum[[1]]-wavAnalyzePitchMargin,wavAnalyzeCutNoteNum+wavAnalyzePitchMargin};
(*interpolation model functions*)
wavFourierAnalyzePrecision=0.001;
pos2num[x_]:=(x-1)*wavFourierAnalyzePrecision+wavAnalyzeRange[[1]];
num2pos[x_]:=Round[(x-wavAnalyzeRange[[1]])/wavFourierAnalyzePrecision+1];

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
wavFourier=Table[{Log[2.,((i-1)*wavSampleRate/Length[wavTrimData])/440]*12+48,wavFourier[[i]]},{i,IntegerPart[Length[wavFourier]/2]}];
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
ListPlot[wavInterpolationReconstruct,PlotRange\[Rule]All,Joined\[Rule]True,Frame\[Rule]True,Axes\[Rule]False,PlotStyle\[Rule]Pink,AspectRatio\[Rule]1/10,ImageSize\[Rule]1600,GridLines\[Rule]{Table[i,{i,wavAnalyzeRange[[1]],wavAnalyzeRange[[2]]}],Automatic},FrameLabel\[Rule]{"Key","Volume (dB)"}]*)
result);


result=ParallelTable[entropySampleConstruct[x],{x,Length[noteNums]}];
entropySamples=Association[result[[;;,2]]];
entropyShiftIdeal=Association[result[[;;,1]]];
entropyShift=entropyShiftIdeal;


Print[Dynamic[ListPlot[Table[{i,(-entropyShiftIdeal[i]+entropyShift[i])*100*wavFourierAnalyzePrecision},{i,0,87}],PlotRange->All,Frame->True,Axes->False,PlotStyle->Pink,AspectRatio->1/10,ImageSize->1600,GridLines->{Table[i,{i,wavAnalyzeRange[[1]],wavAnalyzeRange[[2]]}],Automatic},FrameLabel->{"Key","Volume (dB)"},GridLinesStyle->LightBlue]]];
Do[(*step precision: cent*)
entropyRoughness=x;
entropyRoughness=Round[entropyRoughness/100/wavFourierAnalyzePrecision];
entropyRoughSamples=Table[i->Table[entropySamples[i][[j]],{j,1,Length[entropySamples[1]],entropyRoughness}],{i,noteRangeNum[[1]],noteRangeNum[[2]]}];
entropyRoughSamples=Association[entropyRoughSamples];

entropyStepChange=1;
entropyCostLast=Infinity;
SetSharedVariable[entropyCostLast];
SetSharedVariable[entropyStepChange];
SetSharedVariable[entropyShift];
noteChoicePoolO=DeleteCases[Table[i,{i,noteRangeNum[[1]],noteRangeNum[[2]]}],48];
noteChoicePoolO=Flatten[Table[noteChoicePoolO[[i]],{i,Length[noteChoicePoolO]},{j,2}],1];

entropyEvaluate:=(entropyShiftTrial=entropyShift;
entropyShiftTrial[entropyRandomNote]=Round[entropyShiftTrial[entropyRandomNote]+entropyRoughness*entropyRandomDirection];
entropyTotal=Total[Table[RotateRight[entropyRoughSamples[i],Round[entropyShiftTrial[i]/entropyRoughness]],{i,noteRangeNum[[1]],noteRangeNum[[2]]}]];
entropyTotal=entropyTotal/Total[entropyTotal];
entropyCost=Total[Table[If[entropyTotal[[i]]!=0.,-entropyTotal[[i]]*Log[entropyTotal[[i]]],0],{i,Length[entropyTotal]}]];);
While[entropyStepChange!=0,
entropyStepChange=0;noteChoicePool=Table[RandomSample[noteChoicePoolO,Length[noteChoicePoolO]],{$KernelCount}];
ParallelDo[entropyRandomNote=noteChoicePool[[$KernelID,j]];
entropyRandomDirection=RandomChoice[{1,-1}];
entropyEvaluate;
entropyRandomDirection=-entropyRandomDirection;
entropyEvaluate;
If[entropyCostLast>entropyCost,entropyShift=entropyShiftTrial;
entropyCostLast=entropyCost;entropyStepChange++];
,{j,Length[noteChoicePoolO]}]];
Print[entropyTotal=Total[Table[RotateRight[entropyRoughSamples[i],Round[entropyShift[i]/entropyRoughness]],{i,noteRangeNum[[1]],noteRangeNum[[2]]}]];
entropyPlotAxis=Table[pos2num[j],{j,1,Length[entropySamples[1]],entropyRoughness}];
ListPlot[Transpose@{entropyPlotAxis,entropyTotal},Joined->True,PlotRange->All,Frame->True,Axes->False,PlotStyle->Pink,AspectRatio->1/10,ImageSize->1600,GridLines->{Table[i,{i,wavAnalyzeRange[[1]],wavAnalyzeRange[[2]]}],Automatic},FrameLabel->{"Key","Volume (dB)"},GridLinesStyle->LightBlue]],
{x,{5,2,1,0.5}}]


(*reset*)
entropyShift=entropyShiftIdeal;


ListPlot[Table[{i,(-entropyShiftIdeal[i]+entropyShift[i])*100*wavFourierAnalyzePrecision},{i,0,87}],PlotRange->All,Frame->True,Axes->False,PlotStyle->Pink,AspectRatio->1/10,ImageSize->1600,GridLines->{Table[i,{i,wavAnalyzeRange[[1]],wavAnalyzeRange[[2]]}],Automatic},FrameLabel->{"Key","Volume (dB)"},GridLinesStyle->LightBlue]


entropyShift=<|1->178,0->118,13->-35,12->45,25->-58,24->-14,37->-58,36->-26,49->-78,48->-36,61->-102,60->-53,73->-127,72->-127,85->-166,84->-158,2->135,14->86,26->34,38->32,50->24,62->-23,74->-35,86->-164,4->-63,3->42,16->-97,15->-15,28->-115,27->-73,40->-154,39->-65,52->-169,51->-107,64->-170,63->-103,76->-174,75->-120,87->-164,6->29,5->260,18->-67,17->-21,30->-80,29->-39,42->-92,41->-48,54->-88,53->-72,66->-125,65->-102,78->-142,77->-120,7->124,19->42,31->-3,43->-6,55->-40,67->-62,79->-137,9->145,8->24,21->49,20->-68,33->16,32->-57,45->21,44->-62,57->-201,56->-99,69->-171,68->-105,81->-164,80->-133,11->-55,10->5,23->-84,22->-51,35->-151,34->-55,47->-161,46->-52,59->-155,58->-102,71->-182,70->-132,83->-160,82->-155|>;


entropyResultReconstruct[x_]:=(
wavImport=Import[wavDirectory<>wavNames[[x]],"Sound"];
wavNoteNum=noteNums[[x]];
wavIdealFreq=num2freq[wavNoteNum];
wavSampleRate=wavImport[[1,2]];
wavData=Mean[wavImport[[1,1]]];
wavData=wavData/Max[wavData];
wavInterpolation=Interpolation[wavData];
wavStep=2^(entropyShift[wavNoteNum]*100*wavFourierAnalyzePrecision/1200);
Table[wavInterpolation[i],{i,1,Length[wavData],wavStep}])


ParallelDo[temp=entropyResultReconstruct[i];Export[NotebookDirectory[]<>"../res/instruments/traditional/resample/"<>ToString[noteNums[[i]]]<>".wav",temp],{i,Length[noteNums]}]
