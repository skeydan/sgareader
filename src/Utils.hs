{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module Utils

where

import Foreign
import Foreign.Ptr
import Foreign.C
import Control.Exception
import Text.ParserCombinators.Parsec
import Numeric
import Debug.Trace

{-
 - void *shmat(int shmid, const void *shmaddr, int shmflg);
 - shmid:    shared memory segment to attach to
 - shmaddr:  address to attach to
 - shmflg:   use SHM_RDONLY = 16
 - returns:  address of attached shared memory segment, or (void *) -1 is returned and errno is set
 -
-}
foreign import ccall unsafe "sys/types.h shmat"
  c_shmat :: CInt -> CInt -> CInt -> IO CInt

shmat :: Integer -> Integer -> IO (Maybe Integer)
shmat shmid shmaddr = do
  ret <-  c_shmat (fromIntegral shmid) (fromIntegral shmaddr) (fromIntegral 16)
  let  attached = fromIntegral ret
  case  attached of
      -1 -> return Nothing
      otherwise -> return (Just attached)

{-
 int shmdt(const void *shmaddr);
-}
foreign import ccall unsafe "sys/types.h shmdt"
  c_shmdt :: CInt -> IO CInt

shmdt :: Integer -> IO (Maybe ())
shmdt shmaddr = do
  ret <- c_shmdt (fromIntegral shmaddr)
  case (fromIntegral ret) of
    -1 -> return Nothing
    0  -> return (Just ())


data SessInfo = SessInfo {
  sgaBase      :: Integer,
  sessAddrs    :: [Integer],
  fields       :: [(String, Integer, Integer)]
} deriving (Eq, Show)

parseSgaBase :: CharParser () Integer
parseSgaBase = do
  hexChars <- between (string "SGA_BASE=") (string "\n\n") (many1 hexDigit)
  return $ read $ ("0x" ++) hexChars
  --trace ("SGA_BASE=" ++ hexChars) return $ read $ ("0x" ++) hexChars

parseSessAddrs :: CharParser () [Integer]
parseSessAddrs = do
  sessAddrs <- manyTill parseSessAddr (string "\n")
  return sessAddrs
  --trace (show $ map (\a -> showHex a "") sessAddrs) return sessAddrs

parseSessAddr :: CharParser () Integer
parseSessAddr = do
  manyTill anyChar (string "ADDR=")
  addr <- manyTill hexDigit (char ',')
  manyTill anyChar (char '\n')
  return $ read $ ("0x" ++) addr
  --trace ("session address: " ++ addr) return $ read $ ("0x" ++) addr

parseField :: CharParser () (String, Integer, Integer)
parseField = do
  fieldName <- many1 (noneOf ['='])
  char '='
  fieldStart <- many1 digit
  char ','
  fieldSize <- many1 digit
  char '\n'
  return (fieldName, read fieldStart, read fieldSize)
  --trace ("field info: " ++ fieldName ++ ", " ++ fieldStart ++ ", " ++ fieldSize) return (fieldName, read fieldStart, read fieldSize)


parseFields :: CharParser () [(String, Integer, Integer)]
parseFields = many1 parseField

parseSessInfo :: CharParser () SessInfo
parseSessInfo = do
  sgaBase <- parseSgaBase
  sessAddrs <- parseSessAddrs
  fields <- parseFields
  return (SessInfo sgaBase sessAddrs fields)





