module Butler.Dsl
  ( module Butler.Dsl
  ) where

import VestPrelude

data TopicAs (f :: Format) (s :: k) (a :: *)

type Topic = TopicAs 'Haskell

type TopicJSON = TopicAs 'JSON

data DirectEndpointAs (f :: Format) (s :: k) (a :: *) (b :: *)

type DirectEndpoint = DirectEndpointAs 'Haskell

type DirectEndpointJSON = DirectEndpointAs 'JSON

data StreamingEndpointAs (f :: Format) (s :: k) (a :: *) (b :: *)

type StreamingEndpoint = StreamingEndpointAs 'Haskell

type StreamingEndpointJSON = StreamingEndpointAs 'JSON
