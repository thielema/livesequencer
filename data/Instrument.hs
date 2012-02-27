module Instrument where

type Instrument = Integer ;


acousticGrandPiano , brightAcousticPiano ,
 electricGrandPiano  , honkyTonk           ,
 electricPiano1      , electricPiano2      ,
 harpsichord         , clavinet            ,
 celesta             , glockenspiel        ,
 musicBox            , vibraphone          ,
 marimba             , xylophone           ,
 tubularBells        , dulcimer            ,
 drawbarOrgan        , percussiveOrgan     ,
 rockOrgan           , churchOrgan         ,
 reedOrgan           , accordion           ,
 harmonica           , tangoAccordian      ,
 acousticGuitarNylon , acousticGuitarSteel ,
 electricGuitarJazz  , electricGuitarClean ,
 electricGuitarMuted , overdrivenGuitar    ,
 distortionGuitar    , guitarHarmonics     ,
 acousticBass        , electricBassFinger  ,
 electricBassPick    , fretlessBass        ,
 slapBass1           , slapBass2           ,
 synthBass1          , synthBass2          ,
 violin              , viola               ,
 cello               , contrabass          ,
 tremoloStrings      , pizzicatoStrings    ,
 orchestralHarp      , timpani             ,
 stringEnsemble1     , stringEnsemble2     ,
 synthStrings1       , synthStrings2       ,
 choirAahs           , voiceOohs           ,
 synthVoice          , orchestraHit        ,
 trumpet             , trombone            ,
 tuba                , mutedTrumpet        ,
 frenchHorn          , brassSection        ,
 synthBrass1         , synthBrass2         ,
 sopranoSax          , altoSax             ,
 tenorSax            , baritoneSax         ,
 oboe                , englishHorn         ,
 bassoon             , clarinet            ,
 piccolo             , flute               ,
 recorder            , panFlute            ,
 blownBottle         , skakuhachi          ,
 whistle             , ocarina             ,
 lead1Square         , lead2Sawtooth       ,
 lead3Calliope       , lead4Chiff          ,
 lead5Charang        , lead6Voice          ,
 lead7Fifths         , lead8BassLead       ,
 pad1NewAge          , pad2Warm            ,
 pad3Polysynth       , pad4Choir           ,
 pad5Bowed           , pad6Metallic        ,
 pad7Halo            , pad8Sweep           ,
 fX1Rain             , fX2Soundtrack       ,
 fX3Crystal          , fX4Atmosphere       ,
 fX5Brightness       , fX6Goblins          ,
 fX7Echoes           , fX8SciFi            ,
 sitar               , banjo               ,
 shamisen            , koto                ,
 kalimba             , bagpipe             ,
 fiddle              , shanai              ,
 tinkleBell          , agogo               ,
 steelDrums          , woodblock           ,
 taikoDrum           , melodicTom          ,
 synthDrum           , reverseCymbal       ,
 guitarFretNoise     , breathNoise         ,
 seashore            , birdTweet           ,
 telephoneRing       , helicopter          ,
 applause            , gunshot :: Instrument ;


acousticGrandPiano  =   0 ; brightAcousticPiano =   1 ;
electricGrandPiano  =   2 ; honkyTonk           =   3 ;
electricPiano1      =   4 ; electricPiano2      =   5 ;
harpsichord         =   6 ; clavinet            =   7 ;
celesta             =   8 ; glockenspiel        =   9 ;
musicBox            =  10 ; vibraphone          =  11 ;
marimba             =  12 ; xylophone           =  13 ;
tubularBells        =  14 ; dulcimer            =  15 ;
drawbarOrgan        =  16 ; percussiveOrgan     =  17 ;
rockOrgan           =  18 ; churchOrgan         =  19 ;
reedOrgan           =  20 ; accordion           =  21 ;
harmonica           =  22 ; tangoAccordian      =  23 ;
acousticGuitarNylon =  24 ; acousticGuitarSteel =  25 ;
electricGuitarJazz  =  26 ; electricGuitarClean =  27 ;
electricGuitarMuted =  28 ; overdrivenGuitar    =  29 ;
distortionGuitar    =  30 ; guitarHarmonics     =  31 ;
acousticBass        =  32 ; electricBassFinger  =  33 ;
electricBassPick    =  34 ; fretlessBass        =  35 ;
slapBass1           =  36 ; slapBass2           =  37 ;
synthBass1          =  38 ; synthBass2          =  39 ;
violin              =  40 ; viola               =  41 ;
cello               =  42 ; contrabass          =  43 ;
tremoloStrings      =  44 ; pizzicatoStrings    =  45 ;
orchestralHarp      =  46 ; timpani             =  47 ;
stringEnsemble1     =  48 ; stringEnsemble2     =  49 ;
synthStrings1       =  50 ; synthStrings2       =  51 ;
choirAahs           =  52 ; voiceOohs           =  53 ;
synthVoice          =  54 ; orchestraHit        =  55 ;
trumpet             =  56 ; trombone            =  57 ;
tuba                =  58 ; mutedTrumpet        =  59 ;
frenchHorn          =  60 ; brassSection        =  61 ;
synthBrass1         =  62 ; synthBrass2         =  63 ;
sopranoSax          =  64 ; altoSax             =  65 ;
tenorSax            =  66 ; baritoneSax         =  67 ;
oboe                =  68 ; englishHorn         =  69 ;
bassoon             =  70 ; clarinet            =  71 ;
piccolo             =  72 ; flute               =  73 ;
recorder            =  74 ; panFlute            =  75 ;
blownBottle         =  76 ; skakuhachi          =  77 ;
whistle             =  78 ; ocarina             =  79 ;
lead1Square         =  80 ; lead2Sawtooth       =  81 ;
lead3Calliope       =  82 ; lead4Chiff          =  83 ;
lead5Charang        =  84 ; lead6Voice          =  85 ;
lead7Fifths         =  86 ; lead8BassLead       =  87 ;
pad1NewAge          =  88 ; pad2Warm            =  89 ;
pad3Polysynth       =  90 ; pad4Choir           =  91 ;
pad5Bowed           =  92 ; pad6Metallic        =  93 ;
pad7Halo            =  94 ; pad8Sweep           =  95 ;
fX1Rain             =  96 ; fX2Soundtrack       =  97 ;
fX3Crystal          =  98 ; fX4Atmosphere       =  99 ;
fX5Brightness       = 100 ; fX6Goblins          = 101 ;
fX7Echoes           = 102 ; fX8SciFi            = 103 ;
sitar               = 104 ; banjo               = 105 ;
shamisen            = 106 ; koto                = 107 ;
kalimba             = 108 ; bagpipe             = 109 ;
fiddle              = 110 ; shanai              = 111 ;
tinkleBell          = 112 ; agogo               = 113 ;
steelDrums          = 114 ; woodblock           = 115 ;
taikoDrum           = 116 ; melodicTom          = 117 ;
synthDrum           = 118 ; reverseCymbal       = 119 ;
guitarFretNoise     = 120 ; breathNoise         = 121 ;
seashore            = 122 ; birdTweet           = 123 ;
telephoneRing       = 124 ; helicopter          = 125 ;
applause            = 126 ; gunshot             = 127 ;
