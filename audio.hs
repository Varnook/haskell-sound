import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable

audioFile :: String -> [Float] -> IO()
audioFile file sound = B.writeFile file $ B.toLazyByteString $ fold $ map B.floatLE sound

rate :: Float
rate = 44100

wave :: Float -> Float -> Float -> [Float]
wave duration amp freq = [amp * sin(freq' * t) | t <- [0.0 .. rate * duration]]
    where freq' = 2*pi*freq / rate

amplify :: Float -> [Float] -> [Float]
amplify vol = map (vol*)

lfoAmp :: Float -> [Float] -> [Float]
lfoAmp beats sound = zipWith (*) volWave sound
    where 
    volWave = map abs $ amplify 1 (wave dur 1 (beats / 2))
    dur     = (fromIntegral $ length sound) / rate

fadeInOut :: Float -> Float -> Float -> Float -> [Float] -> [Float]
fadeInOut dur start stop at audio = (take begin audio) ++ (zipWith (*) (drop begin audio) fadeWave) ++ (drop end audio)
    where
    begin = truncate (rate * at)
    end   = truncate (rate * (at + dur))
    fadeWave = if start > 1 then 
                  map (start+) fadeWave'
               else 
                  if m < 0 then
                     map ((-1)*) $ reverse fadeWave'
                  else
                     fadeWave'
    fadeWave' = map (m*) [0.0 .. rate * dur]
    m = (stop - start) / (rate * dur)

envelope :: Float -> Float -> Float -> Float -> Float -> [Float] -> [Float]
envelope atk dec sus rel peak audio = fadeInOut atk 0 peak 0 (fadeInOut dec peak 1 atk $ fadeInOut rel 1 0 (atk + dec + sus) audio)

data Chord = Major | Minor | Dim | VII | Maj7 | Min7 deriving (Enum, Bounded, Show)

chordRatio :: Chord -> [Float]
chordRatio Major = [4, 3]
chordRatio Minor = [3, 4]
chordRatio Dim   = [3, 3]
chordRatio VII   = [4, 3, 3]
chordRatio Maj7  = [4, 3, 4]
chordRatio Min7  = [3, 4, 3]

chord :: Float -> Float -> Float -> Chord -> [Float]
chord duration amp freq tipo = foldr1 (zipWith (+)) $ map (wave duration $ (/) amp $ ratLen) freqList
    where 
    freqList = freq : [freq * (2**(n/12)) | n <- scanl1 (+) ratios] 
    ratLen   = 1 + (fromIntegral $ length ratios)
    ratios   = chordRatio tipo
