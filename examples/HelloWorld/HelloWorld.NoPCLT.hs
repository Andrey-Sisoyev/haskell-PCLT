{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- This code is WITHOUT extending application with PCLT. A code that uses subject package is listed in "HelloWorld.hs" file

module HelloWorld where

import Control.Exception

-----------------------------------------------------
-----------------------------------------------------
-- Application specific data structures

-- \/ data
data HelloWorld = HelloWorld

-- \/ errors
type WorldName = String
type WorldIndex = Int
data HelloWorldError =
          NoWorld_HWE
        | AmbiguousChoiceOfWorlds_HWE (WorldName, WorldIndex) (WorldName, WorldIndex) [(WorldName, WorldIndex)]
        | SomeVeryStrangeError_HWE Int String Bool (Maybe Bool) (Maybe Bool) SomeException
        | FailedDueToErrorInSubsystem_HWE ErrorInSubsystem

data ErrorInSubsystem =
          ErrorType1_EIS
        | ErrorType2_EIS
        | FailedDueToErrorInSubsubsystem_EIS ErrorInSub_sub_system

data ErrorInSub_sub_system =
          ErrorType1_EISS
        | ErrorType2_EISS

-----------------------------------------------------
-----------------------------------------------------
-- Functional part of app

type SayHelloWorld_Mode = Int
sayHelloWorld :: SayHelloWorld_Mode -> Either HelloWorldError HelloWorld
sayHelloWorld mode =
        case mode of
            0 -> Right HelloWorld
            1 -> Left NoWorld_HWE
            2 -> Left $ AmbiguousChoiceOfWorlds_HWE ("RealWorld", 1) ("VirtualWorld", 2) [("OtherWorld1", 3), ("OtherWorld2", 4), ("OtherWorld3", 5)]
            3 -> Left $ SomeVeryStrangeError_HWE 5789 "Noise..." True (Just True) Nothing (SomeException DivideByZero)
            4 -> Left $ FailedDueToErrorInSubsystem_HWE ErrorType1_EIS
            5 -> Left $ FailedDueToErrorInSubsystem_HWE ErrorType2_EIS
            6 -> Left $ FailedDueToErrorInSubsystem_HWE $ FailedDueToErrorInSubsubsystem_EIS ErrorType1_EISS
            7 -> Left $ FailedDueToErrorInSubsystem_HWE $ FailedDueToErrorInSubsubsystem_EIS ErrorType2_EISS

showHelloWorld :: SayHelloWorld_Mode -> String
showHelloWorld mode = show $ sayHelloWorld mode

main = let iterate_ = do
                  putStrLn "\n---------------------------------\n"
                  putStrLn "Input sayHelloWorld mode (0-7; '-1' to exit): "
                  mode <- readLn
                  case mode >= 0 && mode <= 7 of
                      True -> do putStrLn $ showHelloWorld mode
                                 putStrLn "\n---------------------------------\n"
                                 iterate_
                      False -> case mode == (-1) of
                                   True  -> return ()
                                   False -> iterate_
        in iterate_

-----------------------------------------------------
-----------------------------------------------------
-- Representations

instance Show HelloWorld where
     show hw = "Hello world!"

instance Show HelloWorldError where
     show hwe = "Hello world failure!\nReason:\n" ++
        case hwe of
            NoWorld_HWE -> "no world!"
            AmbiguousChoiceOfWorlds_HWE (wn1, wi1) (wn2, wi2) other_worlds -> "ambiguous choice of worlds!\nWorld 1: " ++ show (wn1, wi1) ++ ".\nWorld 2: " ++ show (wn2, wi2) ++ ".\nOther worlds: " ++ show other_worlds
            SomeVeryStrangeError_HWE i s b mb_b1 mb_b2 sm_excpt -> "some very strange error! Data: " ++ show (i, s, b, mb_b1, mb_b2, sm_excpt)
            FailedDueToErrorInSubsystem_HWE eis -> " failed due to error(s) in subsystem!\nLower level exception message:\n" ++ show eis

instance Show ErrorInSubsystem where
     show eis = "Subsystem failure!\nReason:\n" ++
        case eis of
            ErrorType1_EIS -> "error of type 1."
            ErrorType2_EIS -> "error of type 2."
            FailedDueToErrorInSubsubsystem_EIS eiss -> "failed due to error(s) in subSUBsystem!\nLower level exception message:\n" ++ show eiss

instance Show ErrorInSub_sub_system where
     show eis = "SubSUBsystem failure!\nReason:\n " ++
        case eis of
            ErrorType1_EISS -> "error of type 1."
            ErrorType2_EISS -> "error of type 2."
