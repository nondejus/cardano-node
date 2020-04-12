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
import           Cardano.CLI.Shelley.Run.Genesis (runGenesisCreate)

runShelleyCommand :: ShelleyCommand -> ExceptT CliError IO ()
runShelleyCommand cc =
  case cc of
    ShelleyCreateGenesis genSyr mstart amount -> runGenesisCreate genSyr mstart amount
    _ -> liftIO . putStrLn $ "runShelleyCommand: " ++ show cc
