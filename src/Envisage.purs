module Envisage
(
  module Envisage.Internal
, module Envisage.Var
)
where

import Envisage.Internal (Var(..), readEnv, defaultTo, withParser, withShow, describe, showParsed, mkComponent, Component)
import Envisage.Var (var, var')

{-
  New thoughts, ideally a component should describe what env vars it needs
  so maybe you start with (Tuple envVars (config -> component)) which returns (Tuple (Array ReadResult) (Maybe component))
  question then is can you do that for multiple components, or continue using the Writer monad to compose the output?

  data Component = Component env (config -> component)

  ^ implement record recursive shenanigans for this type

  so it would like e.g.

  initComponents env {
    oauth: oauthComponent (where oauthComponent = Component oauthVars oauthConstructor)
  }

  or just use a do monad, to combine loading component results - combination of reader and writer?s ReaderT

  runInitComponents :: ReaderT (Object String) (WriterT (Array ReadResult) (Maybe ))

  e.g. toError $ runWriter $ runReader $ do
    oauth <- initComponent oauthComponent
    db <- initComponent dbComponent
    pure {oauth, db}
-}
