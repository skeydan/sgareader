{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Main (
    main
)

where

import Text.ParserCombinators.Parsec
import Control.Monad
import System.Environment
import Utils
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Data.Word
import Numeric
import System.Directory
import GHC.Base

main = do
--  shmid <- liftM (read . head) getArgs
  let shmid = 265945098
  sessions <- readFile =<< liftM (++ "/ksuse.txt") getCurrentDirectory
  let maybeSessInfo = parse parseSessInfo "" sessions
  case maybeSessInfo of
    Left _  -> putStrLn $ "Could not parse session info"
    Right sessInfo -> do
      --putStrLn $ "Session addresses are " ++ concat (map (\s -> showHex s "" ++ " ") (sessAddrs sessInfo))
      attachedAddr <- shmat shmid (sgaBase sessInfo)
      case attachedAddr of
        Nothing -> putStrLn $ "Could not attach to requested " ++ "memory address at shmid "
          ++ (show shmid) ++ ", addr " ++ (show $ sgaBase sessInfo)
        Just sgaAddr -> do
          putStrLn $ "Attached to SGA base address at " ++ showHex sgaAddr ""
          mapM_ printSession (sessAddrs sessInfo)


printSession :: Integer -> IO ()
printSession addr = do
  startFPtr <- newForeignPtr finalizerFree $ plusPtr nullPtr (fromIntegral addr)
  withForeignPtr startFPtr $ \p -> do
    let ksuudsesAddr = p `plusPtr` 128
    --ksuudsesVal <- peek ksuudsesAddr :: IO Word8
    --print (p, ksuudsesAddr, ksuudsesVal, chr $ fromIntegral ksuudsesVal)
    print (ksuudsesAddr)

