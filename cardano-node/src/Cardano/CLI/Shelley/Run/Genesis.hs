{-# LANGUAGE StrictData #-}
module Cardano.CLI.Shelley.Run.Genesis
  ( runGenesisCreate
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (handleIOExceptT)

import           Cardano.Chain.Common (Lovelace, unsafeGetLovelace)
import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers (GenesisDir (..))
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import           Cardano.Slotting.Slot (EpochSize (..))

import           Data.Aeson (Value, toJSON)
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..), slotLengthFromSec)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Ouroboros.Network.Magic (NetworkMagic (..))

import qualified Shelley.Spec.Ledger.Crypto as Shelley
import           Shelley.Spec.Ledger.Keys (GenKeyHash, KeyHash)
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.OCert as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley

import           System.FilePath ((</>))

runGenesisCreate :: GenesisDir -> Maybe SystemStart -> Lovelace -> ExceptT CliError IO ()
runGenesisCreate (GenesisDir gendir) mStart amount = do
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart
  _template <- readFileExceptT (gendir </> "genesis.spec.json")
  liftIO . putStrLn $ "runGenesisCreate " ++ show (gendir, start, amount)


-- | Current UTCTime plus 30 seconds
getCurrentTimePlus30 :: ExceptT CliError IO UTCTime
getCurrentTimePlus30 =
    plus30sec <$> liftIO getCurrentTime
  where
    plus30sec :: UTCTime -> UTCTime
    plus30sec = addUTCTime (30 :: NominalDiffTime)


readFileExceptT :: FilePath -> ExceptT CliError IO ByteString
readFileExceptT fpath =
  handleIOExceptT (IOError fpath) $ BS.readFile fpath


shelleyGenesisToJSON :: ShelleyGenesis TPraosStandardCrypto -> Value
shelleyGenesisToJSON sg =
  Aeson.object
    [ "sgStartTime" .= toJSON (getSystemStart $ sgStartTime sg)
    , "sgNetworkMagic" .= toJSON (unNetworkMagic $ sgNetworkMagic sg)
    , "sgProtocolMagicId" .= toJSON (unProtocolMagicId $ sgProtocolMagicId sg)
    , "sgActiveSlotsCoeff" .= sgActiveSlotsCoeff sg
    , "sgDecentralisationParam" .= sgDecentralisationParam sg
    , "sgSecurityParam" .= sgSecurityParam sg
    , "sgEpochLength" .= sgEpochLength sg
    , "sgSlotsPerKESPeriod" .= sgSlotsPerKESPeriod sg
    , "sgMaxKESEvolutions" .= sgMaxKESEvolutions sg
    , "sgSlotLength" .= sgSlotLength sg
    , "sgUpdateQuorum" .= sgUpdateQuorum sg
    , "sgMaxMajorPV" .= sgMaxMajorPV sg
    , "sgMaxLovelaceSupply" .= sgMaxLovelaceSupply sg
    , "sgMaxBodySize" .= sgMaxBodySize sg
    , "sgMaxHeaderSize" .= sgMaxHeaderSize sg
    , "sgGenDelegs" .= sgGenDelegs sg
    ]

shelleyGenesisFromJSON :: Value -> ShelleyGenesis TPraosStandardCrypto
shelleyGenesisFromJSON =
    Aeson.withObject "ShelleyGenesis" $ \ obj ->
      ShelleyGenesis
        <$> fmap (SystemStart . posixSecondsToUTCTime) (obj .: "sgStartTime")
        <*> fmap NetworkMagic (obj .: "sgNetworkMagic")
        <*> fmap ProtocolMagicId (obj .: "sgProtocolMagicId")
        <*> obj .: "sgActiveSlotsCoeff"
        <*> obj .: "sgDecentralisationParam"
        <*> obj .: "sgSecurityParam"
        <*> obj .: "sgEpochLength"
        <*> obj .: "sgSlotsPerKESPeriod"
        <*> obj .: "sgMaxKESEvolutions"
        <*> obj .: "sgSlotLength"
        <*> obj .: "sgUpdateQuorum"
        <*> obj .: "sgMaxMajorPV"
        <*> obj .: "sgMaxLovelaceSupply"
        <*> obj .: "sgMaxBodySize"
        <*> obj .: "sgMaxHeaderSize"
        <*> obj .: "sgGenDelegs"
        <*> obj .: "sgInitialFunds"


defShelleyGenesis :: UTCTime -> Lovelace -> [CoreNode TPraosStandardCrypto] -> ShelleyGenesis TPraosStandardCrypto
defShelleyGenesis startTime supply coreNodes =
  ShelleyGenesis
    { sgStartTime = SystemStart startTime
    , sgNetworkMagic = NetworkMagic 0
    , sgProtocolMagicId = ProtocolMagicId 0
    , sgActiveSlotsCoeff = 0.5 -- TODO 1 is not accepted by 'mkActiveSlotCoeff'
    , sgDecentralisationParam = 1
    , sgSecurityParam = k
    , sgEpochLength = EpochSize (10 * maxRollbacks k)
    , sgSlotsPerKESPeriod = 10 -- TODO
    , sgMaxKESEvolutions = 100
    , sgSlotLength = slotLengthFromSec 2
    , sgUpdateQuorum = 1  -- TODO
    , sgMaxMajorPV = 1000 -- TODO
    , sgMaxLovelaceSupply = unsafeGetLovelace supply
    , sgMaxBodySize = 1000 -- TODO
    , sgMaxHeaderSize = 1000 -- TODO
    , sgGenDelegs = Map.fromList $ map toGenesis coreNodes
    , sgInitialFunds = Map.empty -- TODO
    }
  where
    k :: SecurityParam
    k = SecurityParam 2160 -- Same as Byron????

    toGenesis :: _ -> (GenKeyHash TPraosStandardCrypto, KeyHash TPraosStandardCrypto)
    toGenesis = undefined
