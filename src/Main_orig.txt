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

main = do
    shmid <- liftM (read . Prelude.head ) getArgs
    defs <- Prelude.readFile "/home/unic/haskell/defs.txt"
    let si = parse parseStructInfo "" defs
    case si of
        Left _  -> Prelude.putStrLn $ "Could not parse struct info"
        Right structInfo -> do
        attachedAddr <- shmat shmid (sgaBase structInfo)
        case attachedAddr of
            Nothing -> Prelude.putStrLn $ "Could not attach to requested " ++
                       "memory address at shmid " ++ (show shmid) ++ ", addr "
                       ++ (show $ sgaBase structInfo)
            Just sgaAddr -> do
                putStrLn $ "Attached to SGA base address at " ++ showHex sgaAddr ""
                putStrLn $ "Now seeking struct's start address at " ++ showHex (structStart structInfo) ""
                structBaseFPtr <- newForeignPtr finalizerFree $ plusPtr nullPtr (fromIntegral . structStart $ structInfo)
                withForeignPtr structBaseFPtr $ \p -> do
                    ksmlrdur <- peek (plusPtr p 0) :: IO Word8
                    --ksmchidx <- peek (alignPtr (plusPtr p 16) 2) :: IO Word8
               -- ksmchdur <- peekByteOff ksmspBasePtr 17 :: IO Word8
               -- ksmchcom1 <- peekByteOff ksmspBasePtr 18 :: IO Word64 --16
               -- ksmchcom2 <- peekByteOff ksmspBasePtr 18 :: IO Word64 --16
               -- ksmchptr <- peekByteOff ksmspBasePtr 40 :: IO Word64 -- 8
               -- ksmchsiz <- peekByteOff ksmspBasePtr 48 :: IO Word64 -- 8
               -- ksmchcls <- peekByteOff ksmspBasePtr 56 :: IO Word64 -- 8
               -- ksmchtyp <- peekByteOff ksmspBasePtr 64 :: IO Word64 -- 8
               -- ksmchpar <- peekByteOff ksmspBasePtr 72 :: IO Word64 -- 8
                --print (addr, ksmchidx)
                    print (ksmlrdur)

           -- withForeignPtr fp $ \p -> do
           -- let addr = p `plusPtr` i
           -- val <- peek addr :: IO Word8
           -- print (addr, val, chr $ fromIntegral val)


