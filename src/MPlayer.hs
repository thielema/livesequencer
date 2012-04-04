{- |
This program receives MIDI commands
and controls MPlayer accordingly.

The program listens to MIDI channel 0.

When the program receives AllNotesOff or AllNotesOn it pauses MPlayer.

If the program receives a MIDI-CC 0 with value 0,
then it seeks a certain position in the movie and triggers playback.

You can set the seek time by sending the following controller changes:

* CC 1: 100th seconds

* CC 2: seconds

* CC 3: minutes

* CC 4: hours
-}
module Main where

import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel.Mode as ModeMsg
import qualified Sound.MIDI.ALSA.Check as Check

import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc

import qualified Control.Monad.Trans.State as MS
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when, forever, )
import Data.Foldable (forM_, )

import qualified System.Directory as Dir
import qualified System.Posix.Files as File
import qualified System.IO as IO
import qualified System.Environment as Env
import qualified Control.Exception as Exc

import qualified Text.Printf as Printf

import Option.Utility (exitFailureMsg)


defltPipeName :: FilePath
defltPipeName = "/tmp/mppipe"

seqName :: String
seqName = "MPlayer control"

channel :: ChannelMsg.Channel
channel = ChannelMsg.toChannel 0

commands :: MonadIO io => IO.Handle -> [String] -> io ()
commands pipe =
  liftIO . mapM_ (\cmd -> putStrLn cmd >> IO.hPutStrLn pipe cmd)


data Time = Time {hours, minutes, seconds, fracSecs :: Int}
  deriving (Show)


main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    _:_:_ -> exitFailureMsg "too many arguments"
    [pipeName] -> process pipeName
    [] ->
      Exc.bracket_
        (File.createNamedPipe defltPipeName 0o644 >>
         putStrLn ("Created pipe: " ++ defltPipeName))
        (Dir.removeFile defltPipeName)
        (process defltPipeName)

process :: FilePath -> IO ()
process pipeName = (do
  pipe <- IO.openFile pipeName IO.WriteMode
  IO.hSetBuffering pipe IO.LineBuffering
  putStrLn $ "Start MPlayer like this:"
  putStrLn $ "mplayer -input file=" ++ pipeName
  putStrLn ""

  SndSeq.with SndSeq.defaultName SndSeq.Block $ \h -> do
  Client.setName (h :: SndSeq.T SndSeq.InputMode) seqName
  putStrLn $ "Created sequencer: " ++ seqName
  putStrLn $ "Start the live sequencer like this:"
  putStrLn $ "live-sequencer --new-out-port control --connect-to " ++ show seqName
  putStrLn ""
  putStrLn $ "or connect with the live sequencer like this:"
  putStrLn $ "aconnect Rewrite:1 " ++ show seqName
  putStrLn ""

  Port.withSimple h "control"
     (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric $ \ _p1 -> do

  flip MS.evalStateT (Time 0 0 0 0) $ forever $ do
     ev <- liftIO $ Event.input h
     -- print ev
     forM_ (Check.mode channel ev) $ \mode ->
       case mode of
         {-
         'pause' toggles the playing mode
         To make sure, that 'pause' stops,
         we have to run the movie with 'seek' first.
         -}
         ModeMsg.AllNotesOff -> commands pipe ["seek 0", "pause"]
         ModeMsg.AllSoundOff -> commands pipe ["seek 0", "pause"]
         _ -> return ()

     forM_ (Check.anyController channel ev) $ \(ctrl, val) -> do
       let setTime update cc =
             when (ctrl == VoiceMsg.toController cc) $
               MS.modify update

       setTime (\t -> t {fracSecs = val}) 1
       setTime (\t -> t {seconds  = val}) 2
       setTime (\t -> t {minutes  = val}) 3
       setTime (\t -> t {hours    = val}) 4

       when (ctrl == VoiceMsg.toController 0 && val == 0) $ do
         (Time hr m s f) <- MS.get
         commands pipe [Printf.printf "seek %d.%02d 2" (hr*3600 + m*60 + s) f]

  )
  `AlsaExc.catch` \e ->
     putStrLn $ "alsa_exception: " ++ AlsaExc.show e
