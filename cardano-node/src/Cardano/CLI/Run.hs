{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Run (
    CliError (..)
  , ClientCommand(..)
  , runClientCommand
  --
  , NewDirectory(..)
  , SigningKeyFile(..)
  , VerificationKeyFile(..)
  , NewVerificationKeyFile(..)
  , CertificateFile(..)
  , NewCertificateFile(..)
  , TxFile(..)
  , NewTxFile(..)

  -- * re-exports from Ouroboros-Network
  , IOManager
  , withIOManager
  ) where

import           Cardano.Prelude hiding (option, trace)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (hoistEither, firstExceptT, left)
import qualified Data.ByteString.Lazy as LB
import           Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Version (showVersion)
import qualified Formatting as F
import           Paths_cardano_node (version)
import           System.Directory (doesPathExist)
import           System.Info (arch, compilerName, compilerVersion, os)

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochNumber)
import           Cardano.Chain.Update (ApplicationName(..))
import           Cardano.Chain.UTxO (TxIn, TxOut)

import           Cardano.Crypto (RequiresNetworkMagic(..))
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import           Ouroboros.Network.NodeToClient ( IOManager
                                                , withIOManager
                                                )

import           Cardano.CLI.Byron.Parsers (ByronCommand(..))
import           Cardano.CLI.Byron.UpdateProposal
                   (createUpdateProposal, serialiseByronUpdateProposal, submitByronUpdateProposal)
import           Cardano.CLI.Delegation
import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.CLI.Ops
import           Cardano.CLI.Parsers
import           Cardano.CLI.Shelley.Run (runShelleyCommand)
import           Cardano.CLI.Tx
import           Cardano.Common.LocalSocket
import           Cardano.Config.Types


runClientCommand :: ClientCommand -> ExceptT CliError IO ()
runClientCommand cc =
  case cc of
    ByronClientCommand bc -> runByronClientCommand bc
    ShelleyClientCommand bc -> runShelleyCommand bc
    DisplayVersion -> runDisplayVersion
    Genesis outDir params ptcl -> runGenesisCommand outDir params ptcl
    GetLocalNodeTip configFp mSockPath -> runGetLocalNodeTip configFp mSockPath
    ValidateCBOR cborObject fp -> runValidateCBOR cborObject fp
    PrettyPrintCBOR fp -> runPrettyPrintCBOR fp
    PrettySigningKeyPublic ptcl skF -> runPrettySigningKeyPublic ptcl skF
    MigrateDelegateKeyFrom oldPtcl oldKey newPtcl nskf -> runMigrateDelegateKeyFrom oldPtcl oldKey newPtcl nskf
    PrintGenesisHash genFp -> runPrintGenesisHash genFp
    PrintSigningKeyAddress ptcl netMagic skF -> runPrintSigningKeyAddress ptcl netMagic skF
    Keygen ptcl nskf passReq -> runKeygen ptcl nskf passReq
    ToVerification ptcl skFp nvkFp -> runToVerification ptcl skFp nvkFp
    IssueDelegationCertificate configFp epoch issuerSK delVK cert -> runIssueDelegationCertificate configFp epoch issuerSK delVK cert
    CheckDelegation configFp cert issuerVF delegateVF -> runCheckDelegation configFp cert issuerVF delegateVF
    SubmitTx fp configFp mCliSockPath -> runSubmitTx fp configFp mCliSockPath
    SpendGenesisUTxO configFp nftx ctKey genRichAddr outs -> runSpendGenesisUTxO configFp nftx ctKey genRichAddr outs
    SpendUTxO configFp nftx ctKey ins outs -> runSpendUTxO configFp nftx ctKey ins outs

-- -----------------------------------------------------------------------------

runByronClientCommand :: ByronCommand -> ExceptT CliError IO ()
runByronClientCommand bcc =
  case bcc of
    UpdateProposal configFp sKey pVer sVer sysTag insHash outputFp params -> do
        sK <- readSigningKey RealPBFT sKey
        proposal <- createUpdateProposal configFp sK pVer sVer sysTag insHash params
        ensureNewFileLBS outputFp (serialiseByronUpdateProposal proposal)

    SubmitUpdateProposal configFp proposalFp mSocket ->
        withIOManagerE $ \iocp -> submitByronUpdateProposal iocp configFp proposalFp mSocket

runDisplayVersion :: ExceptT CliError IO ()
runDisplayVersion = do
    liftIO . putTextLn $ mconcat
                [ "cardano-cli " <> renderVersion version
                , " - " <> Text.pack os <> "-" <> Text.pack arch
                , " - " <> Text.pack compilerName <> "-" <> renderVersion compilerVersion
                ]
  where
    renderVersion = Text.pack . showVersion

runGenesisCommand :: NewDirectory -> GenesisParameters -> Protocol -> ExceptT CliError IO ()
runGenesisCommand outDir params ptcl = do
  (genData, genSecrets) <- mkGenesis params
  dumpGenesis ptcl outDir genData genSecrets

runGetLocalNodeTip :: ConfigYamlFilePath -> Maybe CLISocketPath -> ExceptT e IO ()
runGetLocalNodeTip configFp mSockPath =
  withIOManagerE $ \iocp -> liftIO $ getLocalTip configFp mSockPath iocp

runValidateCBOR :: CBORObject -> FilePath -> ExceptT CliError IO ()
runValidateCBOR cborObject fp = do
  bs <- readCBOR fp
  res <- hoistEither $ validateCBOR cborObject bs
  liftIO $ putTextLn res

runPrettyPrintCBOR :: FilePath -> ExceptT CliError IO ()
runPrettyPrintCBOR fp = do
  bs <- readCBOR fp
  pPrintCBOR bs

runPrettySigningKeyPublic :: Protocol -> SigningKeyFile -> ExceptT CliError IO ()
runPrettySigningKeyPublic ptcl skF = do
  sK <- readSigningKey ptcl skF
  liftIO . putTextLn . prettyPublicKey $ Crypto.toVerification sK

runMigrateDelegateKeyFrom
        :: Protocol -> SigningKeyFile -> Protocol -> NewSigningKeyFile
        -> ExceptT CliError IO ()
runMigrateDelegateKeyFrom oldPtcl oldKey newPtcl (NewSigningKeyFile newKey) = do
  sk <- readSigningKey oldPtcl oldKey
  sDk <- hoistEither $ serialiseDelegateKey newPtcl sk
  ensureNewFileLBS newKey sDk

runPrintGenesisHash :: GenesisFile -> ExceptT CliError IO ()
runPrintGenesisHash genFp = do
    gen <- readGenesis genFp
    liftIO . putTextLn $ formatter gen
  where
    formatter :: (a, Genesis.GenesisHash)-> Text
    formatter = F.sformat Crypto.hashHexF . Genesis.unGenesisHash . snd

runPrintSigningKeyAddress :: Protocol -> Common.NetworkMagic -> SigningKeyFile -> ExceptT CliError IO ()
runPrintSigningKeyAddress ptcl netMagic skF = do
  sK <- readSigningKey ptcl skF
  let sKeyAddress = prettyAddress . Common.makeVerKeyAddress netMagic $ Crypto.toVerification sK
  liftIO $ putTextLn sKeyAddress

runKeygen :: Protocol -> NewSigningKeyFile -> PasswordRequirement -> ExceptT CliError IO ()
runKeygen ptcl (NewSigningKeyFile skF) passReq = do
  pPhrase <- liftIO $ getPassphrase ("Enter password to encrypt '" <> skF <> "': ") passReq
  sK <- liftIO $ keygen pPhrase
  serDk <- hoistEither $ serialiseDelegateKey ptcl sK
  ensureNewFileLBS skF serDk

runToVerification :: Protocol -> SigningKeyFile -> NewVerificationKeyFile -> ExceptT CliError IO ()
runToVerification ptcl skFp (NewVerificationKeyFile vkFp) = do
  sk <- readSigningKey ptcl skFp
  let vKey = Builder.toLazyText . Crypto.formatFullVerificationKey $ Crypto.toVerification sk
  ensureNewFile TL.writeFile vkFp vKey

runIssueDelegationCertificate
        :: ConfigYamlFilePath -> EpochNumber -> SigningKeyFile -> VerificationKeyFile -> NewCertificateFile
        -> ExceptT CliError IO ()
runIssueDelegationCertificate configFp epoch issuerSK delegateVK cert = do
  nc <- liftIO $ parseNodeConfigurationFP configFp
  vk <- readVerificationKey delegateVK
  sk <- readSigningKey (ncProtocol nc) issuerSK
  pmId <- readProtocolMagicId $ ncGenesisFile nc
  let byGenDelCert :: Delegation.Certificate
      byGenDelCert = issueByronGenesisDelegation pmId epoch sk vk
  sCert <- hoistEither $ serialiseDelegationCert (ncProtocol nc) byGenDelCert
  ensureNewFileLBS (nFp cert) sCert

runCheckDelegation
        :: ConfigYamlFilePath -> CertificateFile -> VerificationKeyFile -> VerificationKeyFile
        -> ExceptT CliError IO ()
runCheckDelegation configFp cert issuerVF delegateVF = do
  nc <- liftIO $ parseNodeConfigurationFP configFp
  issuerVK <- readVerificationKey issuerVF
  delegateVK <- readVerificationKey delegateVF
  pmId <- readProtocolMagicId $ ncGenesisFile nc
  checkByronGenesisDelegation cert pmId issuerVK delegateVK

runSubmitTx :: TxFile -> ConfigYamlFilePath -> Maybe CLISocketPath -> ExceptT CliError IO ()
runSubmitTx fp configFp mCliSockPath =
  withIOManagerE $ \iocp -> do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0
    tx <- readByronTx fp

    firstExceptT
      NodeSubmitTxError
      $ nodeSubmitTx
          iocp
          Nothing
          (ncGenesisFile nc)
          RequiresNoMagic
          Nothing
          Nothing
          Nothing
          (chooseSocketPath (ncSocketPath nc) mCliSockPath)
          update
          (ncProtocol nc)
          tx

runSpendGenesisUTxO
        :: ConfigYamlFilePath -> NewTxFile -> SigningKeyFile -> Common.Address -> NonEmpty TxOut
        -> ExceptT CliError IO ()
runSpendGenesisUTxO configFp (NewTxFile ctTx) ctKey genRichAddr outs = do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    sk <- readSigningKey (ncProtocol nc) ctKey
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0

    tx <- firstExceptT SpendGenesisUTxOError
            $ issueGenesisUTxOExpenditure
                genRichAddr
                outs
                (ncGenesisFile nc)
                RequiresNoMagic
                Nothing
                Nothing
                Nothing
                update
                (ncProtocol nc)
                sk
    ensureNewFileLBS ctTx $ toCborTxAux tx

runSpendUTxO
        :: ConfigYamlFilePath -> NewTxFile -> SigningKeyFile -> NonEmpty TxIn -> NonEmpty TxOut
        -> ExceptT CliError IO ()
runSpendUTxO configFp (NewTxFile ctTx) ctKey ins outs = do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    sk <- readSigningKey (ncProtocol nc) ctKey
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0

    gTx <- firstExceptT
             IssueUtxoError
             $ issueUTxOExpenditure
                 ins
                 outs
                 (ncGenesisFile nc)
                 RequiresNoMagic
                 Nothing
                 Nothing
                 Nothing
                 update
                 (ncProtocol nc)
                 sk
    ensureNewFileLBS ctTx $ toCborTxAux gTx

{-------------------------------------------------------------------------------
  Supporting functions
-------------------------------------------------------------------------------}

-- TODO:  we'd be better served by a combination of a temporary file
--        with an atomic rename.
-- | Checks if a path exists and throws and error if it does.
ensureNewFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> ExceptT CliError IO ()
ensureNewFile writer outFile blob = do
  exists <- liftIO $ doesPathExist outFile
  when exists $
    left $ OutputMustNotAlreadyExist outFile
  liftIO $ writer outFile blob

ensureNewFileLBS :: FilePath -> LB.ByteString -> ExceptT CliError IO ()
ensureNewFileLBS = ensureNewFile LB.writeFile

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
