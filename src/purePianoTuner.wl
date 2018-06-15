(* ::Package:: *)

purePianoTuner[waveFolder_,freqFile_,inharmonicityFile_,exportFolder_]:=Module[{},
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
(*frequency ratio utils*)
freqRatio2cents[x_]:=1200.*Log[2,x];
freqRatio2pitch[x_]:=12.*Log[2,x];

(**********************************************************************************)
(**********************************************************************************)
(*2. rebuild inharmonicity model & import frequency info*)
(*load frequency info*)
temp=StringSplit[Import[freqFile,"Text"],"\n"];
freqTable=Association[Table[temp1=StringSplit[temp[[i]],"  "];note2num[temp1[[1]]]->ToExpression[temp1[[2]]],{i,Length[temp]}]];
(*load inharmonicity info*)
temp=StringSplit[Import[inharmonicityFile,"Text"],"\n"];
ihProperty=Table[temp1=StringSplit[temp[[i]]];{note2num[temp1[[1]]],ToExpression[temp1[[2]]]},{i,Length[temp]}];

(*ih parameters*)
ihFitScaling=10^4;
ihFunctionExtraction[ihProperty_]:=(temp=ihProperty[[;;,1]];
smallfunc=Fit[Select[ihProperty,#[[1]]<=15&],{1,x},x];
smallfunc=(ihProperty[[1,2]]-CoefficientList[smallfunc,x][[-1]]*ihProperty[[1,1]])+CoefficientList[smallfunc,x][[-1]]*x;
largefunc=Fit[Select[ihProperty,#[[1]]>=40&],{1,x},x];
largefunc=(ihProperty[[-1,2]]-CoefficientList[largefunc,x][[-1]]*ihProperty[[-1,1]])+CoefficientList[largefunc,x][[-1]]*x;
ihProperty2=Flatten[{Table[{x,smallfunc},{x,noteRangeNum[[1]]-100,temp[[1]]-1,12}],ihProperty,Table[{x,largefunc},{x,temp[[-1]]+1,noteRangeNum[[2]]+100,12}]},1];
ihPropertyFunction=Interpolation[ihProperty2];
(*restore ih overtone property function*)
ihfunc[k_,n_]:=n*Sqrt[1+E^ihPropertyFunction[k]/ihFitScaling*(n-1)^2];ihfunc);
ihFunction=ihFunctionExtraction[ihProperty];

(**********************************************************************************)
(**********************************************************************************)
(*3. stretch the piano into no-inharmonicity sound*)



(**********************************************************************************)
(**********************************************************************************)
(*4. import samples*)
(*import samples*)
wavDirectory=waveFolder;

wavNames=Import[wavDirectory];
noteNames=First[StringSplit[#,"."]]&/@wavNames;
noteNums=note2num[#]&/@noteNames;

purify[x_]:=(
wavImport=Import[wavDirectory<>wavNames[[x]],"Sound"];
wavIdealFreq=num2freq[noteNums[[x]]];
wavNoteNum=noteNums[[x]];
wavNoteFreq=num2freq[wavNoteNum];
wavData=wavImport[[1,1]];
wavLength=Length[wavData[[1]]];
wavLength=Floor[wavLength/2]*2;
wavSampleRate=wavImport[[1,2]];

(*input: audio data; output: stretched audio data*)
stretchFourier[s_]:=(wavFourierFreqRe=Table[{(i-1)*wavSampleRate/wavLength,Re[s[[i]]]},{i,Length[s]}];
wavFourierFreqIm=Table[{(i-1)*wavSampleRate/wavLength,Im[s[[i]]]},{i,Length[s]}];
wavInterpolationRe=Interpolation[wavFourierFreqRe];
wavInterpolationIm=Interpolation[wavFourierFreqIm];

(*tune to standard frequency*)
wavNoteRealFreq=freqTable[wavNoteNum][[2]]/ihFunction[wavNoteNum,freqTable[wavNoteNum][[1]]];
wavStepFreqSize=wavSampleRate/wavLength;

(*loop*)
i=0;
j=1;
loopEnd=wavFourierFreqIm[[-1,1]];
arrRe=Table[0,{i,wavLength}];
arrIm=Table[0,{i,wavLength}];
While[True,
temp0=wavNoteFreq*ihFunction[wavNoteNum,i/wavNoteRealFreq];
If[temp0>loopEnd,Break[]];
arrRe[[j]]=wavInterpolationRe[temp0];
arrIm[[j]]=wavInterpolationIm[temp0];
i+=wavStepFreqSize;
j+=1];
arrRe[[;;Round[wavLength/2]]]+arrIm[[;;Round[wavLength/2]]]*I);

wavStretch[wavData_]:=(
If[OddQ[Length[wavData]],temp=Drop[wavData,-1]];
wavFourier=Fourier[temp];
wavFourier0=wavFourier[[;;Round[wavLength/2]]];
wavFourier1=Reverse[wavFourier][[;;Round[wavLength/2]]];
(*reconstruct stretched data*)
wavFourierReconstructed0=stretchFourier[wavFourier0];
wavFourierReconstructed1=stretchFourier[wavFourier1];
wavFourierReconstructed=Flatten[{wavFourierReconstructed0,Reverse[wavFourierReconstructed1]}];
wavReconstruct=InverseFourier[wavFourierReconstructed];
Re[wavReconstruct]);


wavStretchedData={wavStretch[wavData[[1]]],wavStretch[wavData[[2]]]};
ListPlay[wavStretchedData,SampleRate->wavSampleRate,PlayRange->All]);
Do[Speak[noteNums[[i]]];
Export[exportFolder<>ToString[noteNums[[i]]]<>".wav",purify[i]]
,{i,Length[noteNums]}];
];
