{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Shelley.Run
  ( runShelleyCommand
  ) where

import           Cardano.Prelude hiding (option, trace)

import           Control.Monad.Trans.Except (ExceptT)

import           Cardano.CLI.Ops (CliError)
import           Cardano.CLI.Shelley.Parsers (ShelleyCommand(..))


runShelleyCommand :: ShelleyCommand -> ExceptT CliError IO ()
runShelleyCommand cc =
  liftIO . putStrLn $ "runShelleyCommand: " ++ show cc
