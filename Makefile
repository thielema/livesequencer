ghci:
	ghci -Wall -i:data/base:data/example data/base/*.hs data/example/*.hs

ghci-prg:
	ghci -Wall -i:src:dist/build/autogen:http/enable \
	   -package=alsa-seq-0.5.1 -package=midi-alsa-0.1.3 -package=midi-0.1.7 \
	   src/Module.hs


testbuild:
	runhaskell Setup configure --user -f-httpServer --ghc-option=-dynamic --disable-library-profiling
	runhaskell Setup build

	runhaskell Setup configure --user -fhttpServer --ghc-option=-dynamic --disable-library-profiling
	runhaskell Setup build
	runhaskell Setup haddock


# targets for screencasts

SIZE = 1280x720
PATTERN = /tmp/klingklong/%04d.png

# xvidcap crashes from time to time for unknown reasons
# even if it keeps recording, it drop frames
# independent from the frame rate.
xvidcap:
# --window
# --gui no
	$@ --file $(PATTERN) --fps 10 --cap_geometry $(SIZE)+14+67

# klingklong.flv:	/tmp/klingklong/0000.png
#	ffmpeg -r 12 -f image2 -i $(PATTERN) -s $(SIZE) -t 40 -vcodec flashsv $@

# I got the offset from xvidcap
# under Suse the video recording is notably faster than the audio recording
ffmpegcast:
	ffmpeg -f x11grab -r 10 -s 1280x720 -i :0.0+14,67 -vcodec flashsv -sameq /data2/video/klingklong.flv
#	ffmpeg -f x11grab -r 10 -s 1280x720 -i :0.0+14,67 -vcodec huffyuv -sameq /data2/video/klingklong.avi

# this places the GUI window according to the box that ffmpeg grabs
place:
#	xwininfo -name live-sequencer | fgrep -i 'window id' | cut -d' ' -f4
	wmctrl -r live-sequencer -e 0,11,43,1280,720

place-suse:
	wmctrl -r live-sequencer -e 0,11,34,1280,720

%.wav:	%.f32
# sox clips the floating point samples before amplification,
# thus I use this simple call to GHC.
	ghc -e 'Data.StorableVector.Lazy.hPut IO.stdout . Data.StorableVector.Lazy.map (\x -> x*0.5::Float) . snd =<< Data.StorableVector.Lazy.readFileAsync Data.StorableVector.Lazy.defaultChunkSize "'$<'"' \
            | sox -t f32 -r 44100 -c 2 - -b 16 $@
#	sox -r 44100 -c 2 -v 0.7 $< -b 16 $@

# at 2:10 there should be the first tone to be heard
klingklong-complete.flv:	klingklong.flv klingklong.wav
	ffmpeg -ss 00:01:40.0 -i klingklong.flv -ss 00:02:54.0 -i klingklong.wav -t 00:17:25 -vcodec flashsv $@


timidity-ubuntu:
	timidity -A300 -s44100 -iA -Os -B4,4 -o tee:default,speisekarte.s16,raw


speisekarte.wav: speisekarte.s16
	sox -r 44100 -c2 $< $@

speisekarte-complete.flv:	speisekarte.flv speisekarte.wav
# vcodec copy is very fast and lossless, however '-t' option seems to be ignored in this case
#	ffmpeg -ss 00:00:06.0 -i speisekarte.flv -ss 00:00:15.0 -i speisekarte.wav -vcodec copy -acodec adpcm_swf -t 00:04:05 $@
	ffmpeg -ss 00:00:06.0 -i speisekarte.flv -ss 00:00:15.0 -i speisekarte.wav -vcodec flashsv -acodec adpcm_swf -sameq -t 00:04:05 $@
#	ffmpeg -ss 00:00:06.0 -i speisekarte.flv -ss 00:00:15.0 -i speisekarte.wav -t 00:04:05 -vcodec flashsv $@
