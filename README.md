# Piano Tuning
## Traditional Method
Similar to [tunelab](https://www.tunelab-world.com/).
Algorithm is complicated and is written in [document](https://github.com/RobertBoganKang/piano_tuning/blob/master/res/document/Piano%20Tuning%20Method.pdf).
This algorithm is fully construct by myself, it dose not violate the copyright. 

However, the temperment file is copied from tunelab apps, it is under the copyright of tunelab.
## Entropy Method (Experiment)
The optimization function is using entropy function, however this method cannot get good result, and calculation is extremely slow. The algorithm itself might not work.
## Supported Functions
* Set A4 frequency
* Tuning for non-88 keys piano
* Different tuning method at bass and tenor for optimization
* Save the inharmonicity parameters into file, and save report
* Ignore notes for tuning
* Tuning for different temperment

## Demo
There are 2 fully sampled pianos as demo.
### Wesleiter Upright 123
The samples: [[link](https://github.com/RobertBoganKang/WesleiterUpright123); however I used older samples sampled in 2011].
#### Audio demo
* Traditional Tuning Method [[1](https://github.com/RobertBoganKang/piano_tuning/blob/master/res/demo/audio/tuned%200.mp3), [2](https://github.com/RobertBoganKang/piano_tuning/blob/master/res/demo/audio/tuned%201.mp3)]
* Entropy Tuning Method [[1](https://github.com/RobertBoganKang/piano_tuning/blob/master/res/demo/audio/entropy%200.mp3), [2](https://github.com/RobertBoganKang/piano_tuning/blob/master/res/demo/audio/entropy%201.mp3)]
#### Tuning Data
![Upright Tuning](https://github.com/RobertBoganKang/piano_tuning/blob/master/res/demo/upright%20tuning.png)
#### Curve
![Upright Curve](https://github.com/RobertBoganKang/piano_tuning/blob/master/res/demo/upright%20curve.png)
### Xinghai Grand Concert Model D 
[[link](https://github.com/RobertBoganKang/Resonance_Grand__Model_D_Concert_Grand); The strongest strike pedalup sound]
#### Tuning Data
![Grand Tuning](https://github.com/RobertBoganKang/piano_tuning/blob/master/res/demo/grand%20tuning.png)
#### Curve
![Grand Curve](https://github.com/RobertBoganKang/piano_tuning/blob/master/res/demo/grand%20curve.png)
