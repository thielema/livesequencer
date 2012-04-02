import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
import Control.Monad (when, forever, )

import qualified System.Directory as Dir
import qualified System.Posix.Files as File
import qualified System.IO as IO
import qualified Control.Exception as Exc

import qualified Text.Printf as Printf


pipeName :: FilePath
pipeName = "/tmp/mppipe"

seqName :: String
seqName = "MPlayer control"

main :: IO ()
main = (do
  Exc.bracket_
    (File.createNamedPipe pipeName 0o644)
    (Dir.removeFile pipeName) $ do

  pipe <- IO.openFile pipeName IO.WriteMode
  IO.hSetBuffering pipe IO.LineBuffering
  putStrLn $ "Created pipe: " ++ pipeName
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

  putStrLn $ "You can seek to second x by sending MIDI-CC 0 x on MIDI channel 0."
  putStrLn ""

  forever $ do
     ev <- Event.input h
     -- print ev
     case Event.body ev of
       Event.CtrlEv Event.Controller param ->
         when (Event.ctrlChannel param == 0 &&
               Event.ctrlParam param == 0) $
           case Event.ctrlValue param of
             x ->
               let cmd = Printf.printf "seek %d 2\n" x
               in  putStrLn cmd >> IO.hPutStrLn pipe cmd
       _ -> return ())

  `AlsaExc.catch` \e ->
     putStrLn $ "alsa_exception: " ++ AlsaExc.show e
