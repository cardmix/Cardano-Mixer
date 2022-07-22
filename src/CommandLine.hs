{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module CommandLine where

import           Options.Applicative                      hiding (Applicative (..))
import           PlutusTx.Prelude                         hiding (Semigroup(..), Applicative(..), mconcat, (<$>))
import           Prelude                                  (Monoid(..), Semigroup (..), Applicative (..), FilePath)


data Command = Serve | MakeMixer FilePath Integer

commandParserInfo :: ParserInfo Command
commandParserInfo = info (commandParser <**> helper)
       (fullDesc <> progDesc "Relay server app")

commandParser :: Parser Command
commandParser = subparser $ mconcat [ serveParser, makeMixerParser ]

serveParser :: Mod CommandFields Command
serveParser = command "serve" $ flip info (fullDesc <> progDesc "Start the relay server") $ do
    pure Serve

makeMixerParser :: Mod CommandFields Command
makeMixerParser = command "make-mixer" $ flip info 
    (fullDesc <> progDesc "Create a new mixing pool. \
    \ Expects JSON file containing the fee Value for the mixing pool and the maximal number of mixing rounds.") $ do
    MakeMixer <$> argument str (metavar "VALUE_FILE" <> help "JSON file containing the fee Value for the mixing pool.")
              <*> argument auto (metavar "ROUNDS" <> help "Maximal number of mixing rounds in the mixer.")


