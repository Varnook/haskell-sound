import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process

type Beat = Float
type Sec  = Float
type Hz   = Float
type Wave = [Float]
type Note = [Float]

data WaveShape = Sin | Sqr | Saw | Tri               deriving (Enum, Bounded, Show)
data Chord = Major | Minor | Dim | VII | Maj7 | Min7 deriving (Enum, Bounded, Show)

rate :: Hz
rate = 44100

bpm :: Beat
bpm = 120

bpmHelper :: Sec
bpmHelper = 60 / bpm

inf :: Float
inf = 1/0

getWave :: WaveShape -> (Float -> Float -> Float -> Float)
getWave Sin = (\ amp freq t -> amp * sin(freq * t))
getWave Tri = (\ amp freq t -> (2*amp / pi) * asin(sin(freq * t)))
getWave Sqr = (\ amp freq t -> amp * signum(sin(freq * t)))
getWave Saw = (\ amp freq t -> (2*amp / pi) * atan(cot(freq * t)))
     where cot x = 1 / (tan x)

audioFile :: String -> Wave -> IO()
audioFile file sound = B.writeFile file $ B.toLazyByteString $ fold $ map B.floatLE sound

playAudio :: String -> IO()
playAudio file = callCommand $ "aplay -t RAW -f FLOAT_LE -c2 -r " ++ show (truncate rate) ++ " " ++ file

saveAndPlay :: String -> Wave -> IO()
saveAndPlay file sound = do
                         audioFile file sound
                         playAudio file

wave :: Sec -> WaveShape -> Float -> Hz -> Wave
wave duration shape amp freq = [ getWave shape amp freq' t | t <- [0.0 .. rate * duration]]
    where freq' = 2*pi*freq / rate

amplify :: Float -> Wave -> Wave
amplify vol = map (vol*)

linearAmp :: Float -> Float -> Wave -> Wave
linearAmp start step = zipWith (*) [start, (start + step) .. ]

lfoAmp :: Hz -> WaveShape -> Wave -> Wave
lfoAmp lowFreq shape = zipWith (*) volWave 
    where 
    volWave = map abs $ wave inf shape 1 lowFreq

fadeInOut :: Sec -> Sec -> Float -> Float -> Wave -> Wave
fadeInOut dur at start stop audio = take at' audio ++ linearAmp start step (drop at' audio) ++ drop end audio
    where
    at'  = truncate (rate * at)
    end  = truncate (rate * (at + dur))
    step = (stop - start) / (rate * dur)

-- No esta andando
envelope :: Float -> Float -> Float -> Float -> Wave -> Wave
envelope atk dec rel peak audio = 
    let audioLen = fromIntegral $ length audio
        atkDur   = atk * audioLen
        decDur   = dec * audioLen
        relDur   = rel * audioLen
        relAt    = audioLen - relDur
    in  fadeInOut atkDur 0 0 peak . fadeInOut decDur atkDur peak 1 . fadeInOut relDur relAt 1 0 $ audio

chordRatio :: Chord -> [Float]
chordRatio Major = [4, 3]
chordRatio Minor = [3, 4]
chordRatio Dim   = [3, 3]
chordRatio VII   = [4, 3, 3]
chordRatio Maj7  = [4, 3, 4]
chordRatio Min7  = [3, 4, 3]

chord :: Float -> WaveShape -> Float -> Float -> Chord -> [Float]
chord duration shape amp freq tipo = foldr1 (zipWith (+)) $ map (wave duration shape $ (/) amp $ ratLen) freqList
    where 
    freqList = freq : [freq * (2**(n/12)) | n <- scanl1 (+) ratios] 
    ratLen   = 1 + (fromIntegral $ length ratios)
    ratios   = chordRatio tipo
