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
ffmpegcast:
	ffmpeg -f x11grab -r 10 -s 1280x720 -i :0.0+14,67 -vcodec flashsv -sameq /data2/video/klingklong.flv
#	ffmpeg -f x11grab -r 10 -s 1280x720 -i :0.0+14,67 -vcodec huffyuv -sameq /data2/video/klingklong.avi

# this places the GUI window according to the box that ffmpeg grabs
place:
#	xwininfo -name live-sequencer | fgrep -i 'window id' | cut -d' ' -f4
	wmctrl -r live-sequencer -e 0,11,43,1280,720

%.wav:	%.f32
# sox clips the floating point samples before amplification,
# thus I use this simple call to GHC.
	ghc -e 'Data.StorableVector.Lazy.hPut IO.stdout . Data.StorableVector.Lazy.map (\x -> x*0.5::Float) . snd =<< Data.StorableVector.Lazy.readFileAsync Data.StorableVector.Lazy.defaultChunkSize "'$<'"' \
            | sox -t f32 -r 44100 -c 2 - -b 16 $@
#	sox -r 44100 -c 2 -v 0.7 $< -b 16 $@

# at 2:10 there should be the first tone to be heard
%-complete.flv:	%.flv %.wav
	ffmpeg -ss 00:01:40.0 -i $< -ss 00:02:54.0 -i $*.wav -t 00:17:25 -vcodec flashsv $@
