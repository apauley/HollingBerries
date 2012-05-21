# HollingBerries Solution in Smalltalk

A Smalltalk solution based on [Pharo][3]. 

## Solution

### Solution Installation

After [**installing and launching Pharo**](#pharo-installation), execute the
following in a Smalltalk workspace (edit the path to point to your
clone):

```Smalltalk
(FileStream fileNamed: '/opt/git/HollingBerries/smalltalk/dalehenrich/HollingBerries-Core.st') fileIn.
```
### Solution Execution

```Smalltalk
HBPriceListGenerator 
    generatePriceList: '/opt/git/HollingBerries/smalltalk_pricefile.txt' 
    from: '/opt/git/HollingBerries/produce.csv'
```

## Pharo Installation

You'll need to install a [Pharo1.3 image][1]. The [Pharo 1.3
on-click][2] is a good choice. First 
`unzip Pharo-1.3-13328-OneClick.zip`:

### On Linux

```
cd Pharo-1.3-13328-OneClick.app
./pharo.sh 
```

### On Windows

```
cd Pharo-1.3-13328-OneClick.app
./pharo.exe
```

### On OSX

Double-click on `Pharo-1.3-13328-OneClick` in the finder.

[1]: http://www.pharo-project.org/pharo-download/release-1-3
[2]: http://gforge.inria.fr/frs/download.php/30586/Pharo-1.3-13328-OneClick.zip
[3]: http://www.pharo-project.org/
