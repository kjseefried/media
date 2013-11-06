module PNG(PNG, PNGError(..), computeCRC, makePNG) where

import Data.Bits
import Data.Word

data Chunk    = Chunk { chunkType :: [Word8], chunkData :: [Word8] }
data PNG      = PNG [Chunk]
data PNGError = BadSignature | BadChecksum [Int]

makePNG :: [Word8] -> Either PNGError PNG
makePNG pngData
    | not (hasPNGHeader pngData) = Left BadSignature
    | otherwise = let chunks  = splitPNG (drop 8 pngData)
                      badList = filter (\(_, valid) -> not valid) $ zip [1..] (verifyCRC chunks)
                  in if length badList /= 0
                     then Left  $ BadChecksum (map (\item -> fst item) badList)
                     else Right $ PNG (map unpackChunk chunks)
                  where unpackChunk someChunk = 
                                    Chunk { chunkType = drop 4 $ take 8 someChunk,
                                            chunkData = drop 8 $ take ((length someChunk) - 4) someChunk }

-- Returns True if the given list starts with a valid PNG signature.
hasPNGHeader :: [Word8] -> Bool
hasPNGHeader pngData = take 8 pngData == [137, 80, 78, 71, 13, 10, 26, 10]

-- Converts a list of exactly four big endian bytes into a Word32.
extractWord32 :: [Word8] -> Word32
extractWord32 byteSequence = foldl combineByte 0 byteSequence
    where combineByte accumulator byte = shift accumulator 8 + fromIntegral byte

-- Returns the number of PNG chunks in the list. Here I assume the signature has been removed.
chunkCount :: [Word8] -> Int
chunkCount [] = 0
chunkCount pngData =
    let chunkLength    = extractWord32 $ take 4 pngData
        intChunkLength = fromIntegral chunkLength
    in 1 + chunkCount (drop (intChunkLength + 12) pngData)


-- Breaks the PNG file into a list of its chunks. Here I assume the signature has been removed.
splitPNG :: [Word8] -> [[Word8]]
splitPNG [] = []
splitPNG pngData =
    let chunkLength    = extractWord32 $ take 4 pngData
        intChunkLength = fromIntegral chunkLength
        (headChunk, remainingChunks) = splitAt (intChunkLength + 12) pngData
    in headChunk : splitPNG remainingChunks


-- Computes the CRC checksum over the given list using the algorithm in the PNG spec.
computeCRC :: [Word8] -> Word32
computeCRC rawData = complement $ foldl update 0xFFFFFFFF rawData
    where update :: Word32 -> Word8 -> Word32
          update c dataItem =
              let cLowByte = fromIntegral (c .&. 0x000000FF)
                  index    = cLowByte `xor` dataItem
                  intIndex = fromIntegral index
              in (crcTable !! intIndex) `xor` shift c (-8)

          crcTable :: [Word32]
          crcTable = [ processByte n | n <- [0 .. 255]]
              where processByte :: Word32 -> Word32
                    processByte = bitLoop 7

                    bitLoop :: Int -> Word32 -> Word32
                    bitLoop 0       c = processBit c
                    bitLoop counter c = processBit $ bitLoop (counter - 1) c

                    processBit :: Word32 -> Word32
                    processBit c
                        | testBit c 0 = 0xEDB88320 `xor` shift c (-1)
                        | otherwise   = shift c (-1)


-- Verifies the CRC checksum on each chunk in a list of chunks.
verifyCRC :: [[Word8]] -> [Bool]
verifyCRC chunkList = map crcOkay chunkList
    where crcOkay chunk =
              let lengthlessChunk = drop 4 chunk
                  (chunkData, crc) = splitAt (length lengthlessChunk - 4) lengthlessChunk
              in computeCRC chunkData == extractWord32 crc
              
              
-- Verifies that the given list is a valid PNG file.
verifyPNG :: [Word8] -> Bool
verifyPNG pngData = hasPNGHeader pngData && all (==True) (verifyCRC $ splitPNG (drop 8 pngData))
