module PNG(PNG, PNGError(..), computeCRC, makePNG, verifyPNG) where

import qualified Data.ByteString as B
import Data.Bits
import Data.Word

data Chunk    = Chunk { chunkType :: B.ByteString, chunkData :: B.ByteString }
data PNG      = PNG [Chunk]
data PNGError = BadSignature | BadChecksum [Int]

makePNG :: B.ByteString -> Either PNGError PNG
makePNG pngData
    | not (hasPNGHeader pngData) = Left BadSignature
    | otherwise = let chunks  = splitPNG (B.drop 8 pngData)
                      badList = filter (\(_, valid) -> not valid) $ zip [1..] (verifyCRC chunks)
                  in if length badList /= 0
                     then Left  $ BadChecksum (map (\item -> fst item) badList)
                     else Right $ PNG (map unpackChunk chunks)
                  where unpackChunk someChunk = 
                                    Chunk { chunkType = B.drop 4 $ B.take 8 someChunk,
                                            chunkData = B.drop 8 $ B.take (B.length someChunk - 4) someChunk }

-- Returns True if the given list starts with a valid PNG signature.
hasPNGHeader :: B.ByteString -> Bool
hasPNGHeader pngData = B.take 8 pngData == B.pack [137, 80, 78, 71, 13, 10, 26, 10]

-- Converts a list of exactly four big endian bytes into a Word32.
extractWord32 :: B.ByteString -> Word32
extractWord32 byteSequence = B.foldl combineByte 0 byteSequence
    where combineByte accumulator byte = shift accumulator 8 + fromIntegral byte

-- Returns the number of PNG chunks in the list. Here I assume the signature has been removed.
chunkCount :: B.ByteString -> Int
chunkCount pngData
    | B.length pngData == 0 = 0
    | otherwise =
          let chunkLength    = extractWord32 $ B.take 4 pngData
              intChunkLength = fromIntegral chunkLength
          in 1 + chunkCount (B.drop (intChunkLength + 12) pngData)


-- Breaks the PNG file into a list of its chunks. Here I assume the signature has been removed.
splitPNG :: B.ByteString -> [B.ByteString]
splitPNG pngData
    | B.length pngData == 0 = []
    | otherwise =
          let chunkLength    = extractWord32 $ B.take 4 pngData
              intChunkLength = fromIntegral chunkLength
              (headChunk, remainingChunks) = B.splitAt (intChunkLength + 12) pngData
          in headChunk : splitPNG remainingChunks


-- Computes the CRC checksum over the given list using the algorithm in the PNG spec.
computeCRC :: B.ByteString -> Word32
computeCRC rawData = complement $ B.foldl update 0xFFFFFFFF rawData
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
verifyCRC :: [B.ByteString] -> [Bool]
verifyCRC chunkList = map crcOkay chunkList
    where crcOkay chunk =
              let lengthlessChunk = B.drop 4 chunk
                  (chunkData, crc) = B.splitAt (B.length lengthlessChunk - 4) lengthlessChunk
              in computeCRC chunkData == extractWord32 crc
              
              
-- Verifies that the given list is a valid PNG file.
verifyPNG :: B.ByteString -> Bool
verifyPNG pngData = hasPNGHeader pngData && all (==True) (verifyCRC $ splitPNG (B.drop 8 pngData))
