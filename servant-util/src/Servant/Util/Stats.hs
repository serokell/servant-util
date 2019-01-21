-- | Verious information about your API.
module Servant.Util.Stats
    ( methodsCoveringAPI
    ) where

import Universum

import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
import Network.HTTP.Types.Method (Method, StdMethod)
import Servant ((:<|>), (:>), ReflectMethod (..), Verb)

-------------------------------------------------------------------------
-- CORS methods coherence
-------------------------------------------------------------------------

-- | Whether a given type item is element of a given list.
type family IsElemBool (x :: StdMethod) (l :: [StdMethod]) :: Bool where
    IsElemBool x (x : _) = 'True
    IsElemBool x (y : xs) = IsElemBool x xs
    IsElemBool x '[] = 'False

type family FailOnDissallowedMethod (method :: StdMethod) (allowed :: Bool) :: Constraint where
    FailOnDissallowedMethod _ 'True = ()
    FailOnDissallowedMethod m 'False = TypeError
        ( 'Text "Method " ':$$: 'ShowType m ':$$: 'Text " is not allowed, but appears in API"
        )

-- | Ensure that the given api uses only methods from the list provided.
type family ContainsOnlyMethods (methods :: [StdMethod]) api :: Constraint where
    ContainsOnlyMethods ms ((path :: Symbol) :> sub) = ContainsOnlyMethods ms sub
    ContainsOnlyMethods ms (part :> sub) = ContainsOnlyMethods ms sub
    ContainsOnlyMethods ms (api1 :<|> api2) = (ContainsOnlyMethods ms api1,
                                               ContainsOnlyMethods ms api2)
    ContainsOnlyMethods ms (Verb m _ _ _) = FailOnDissallowedMethod m (IsElemBool m ms)

-- | 'ReflectMethod' lifted to method lists.
class ReflectMethods (methods :: [StdMethod]) where
    reflectMethods :: Proxy methods -> [Method]
instance ReflectMethods '[] where
    reflectMethods _ = []
instance (ReflectMethod m, ReflectMethods ms) => ReflectMethods (m ': ms) where
    reflectMethods _ = reflectMethod @m Proxy : reflectMethods @ms Proxy

-- | For the given list of methods, ensure only they are used in API, and get corresponding
-- 'Method' terms.
--
-- A primary use case for this function is specifying CORS methods where we need to think
-- about each single method we allow, thus expecting methods list to be specified manually.
methodsCoveringAPI
    :: forall methods api.
       (ContainsOnlyMethods methods api, ReflectMethods methods)
    => [Method]
methodsCoveringAPI = reflectMethods @methods Proxy
