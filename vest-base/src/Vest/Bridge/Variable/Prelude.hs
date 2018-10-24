module Vest.Bridge.Variable.Prelude
  ( module Vest.Bridge.Variable.Prelude
  ) where

import Vest.Prelude

type VariableName = Text' "VariableName"

class VariableTransport t where
  publishRaw ::
       t -- ^ Should be mutated to store cleanup details.
    -> VariableName
    -> IO (Text' "a" -> IO ())
    -- ^ Returns publish fn for this topic.
  subscribeRaw ::
       t -- ^ Should be mutated to store cleanup details.
    -> VariableName
    -> (Text' "a" -> IO ()) -- ^ Called per item received from the transport.
    -> IO () -- ^ Expects the most recent value to be delivered on subscription.

class (VariableTransport transport) =>
      HasVariableTransport transport t
  where
  variableTransport :: t -> transport

data Variable_ (fmt :: SerializationFormat) service transport (name :: k) a

type Variable = Variable_ 'Haskell
