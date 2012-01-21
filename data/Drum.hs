module Drum where

import Midi ( note, channel )


drum kind dur = note dur kind ;

drumChannel = channel 9 ;


-- general MIDI drum aliases

acousticBassDrum = 35 ;
bassDrum1        = 36 ;
sideStick        = 37 ;
acousticSnare    = 38 ;
handClap         = 39 ;
electricSnare    = 40 ;
lowFloorTom      = 41 ;
closedHiHat      = 42 ;
highFloorTom     = 43 ;
pedalHiHat       = 44 ;
lowTom           = 45 ;
openHiHat        = 46 ;
lowMidTom        = 47 ;
hiMidTom         = 48 ;
crashCymbal1     = 49 ;
highTom          = 50 ;
rideCymbal1      = 51 ;
chineseCymbal    = 52 ;
rideBell         = 53 ;
tambourine       = 54 ;
splashCymbal     = 55 ;
cowbell          = 56 ;
crashCymbal2     = 57 ;
vibraslap        = 58 ;
rideCymbal2      = 59 ;
hiBongo          = 60 ;
lowBongo         = 61 ;
muteHiConga      = 62 ;
openHiConga      = 63 ;
lowConga         = 64 ;
highTimbale      = 65 ;
lowTimbale       = 66 ;
highAgogo        = 67 ;
lowAgogo         = 68 ;
cabasa           = 69 ;
maracas          = 70 ;
shortWhistle     = 71 ;
longWhistle      = 72 ;
shortGuiro       = 73 ;
longGuiro        = 74 ;
claves           = 75 ;
hiWoodBlock      = 76 ;
lowWoodBlock     = 77 ;
muteCuica        = 78 ;
openCuica        = 79 ;
muteTriangle     = 80 ;
openTriangle     = 81 ;
