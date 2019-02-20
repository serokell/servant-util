module Servant.Util.Combinators.Tag
    ( Tag
    , TagDescriptions
    , TagsVerification (..)
    ) where

import Universum

import Control.Lens (at, (?~))
import qualified Data.Set as S
import qualified Data.Swagger as S
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError)
import Servant ((:<|>), (:>), HasServer (..), StdMethod, Verb)
import Servant.Client (HasClient (..))
import Servant.Swagger (HasSwagger (..))

import Servant.Util.Combinators.Logging
import Servant.Util.Common

-- | Attaches a tag to swagger documentation.
-- Server implementation remains intact.
data Tag (name :: Symbol)

instance HasServer subApi ctx => HasServer (Tag name :> subApi) ctx where
    type ServerT (Tag name :> subApi) m = ServerT subApi m
    route _ = route (Proxy @subApi)
    hoistServerWithContext _ = hoistServerWithContext (Proxy @subApi)

instance HasClient m subApi => HasClient m (Tag name :> subApi) where
    type Client m (Tag name :> subApi) = Client m subApi
    clientWithRoute pm _ = clientWithRoute pm (Proxy @subApi)

instance HasLoggingServer config subApi ctx =>
         HasLoggingServer config (Tag name :> subApi) ctx where
    routeWithLog = inRouteServer @(Tag name :> LoggingApiRec config subApi) route identity

instance (HasSwagger subApi, KnownSymbol name) =>
         HasSwagger (Tag name :> subApi) where
    toSwagger _ = toSwagger (Proxy @subApi)
        & S.allOperations . S.tags . at name ?~ ()
      where
        name = symbolValT @name

-- | Whether to enable some type-level checks for 'Tag's and 'TagsVerification'
-- correspondence.
--
-- Note that with the current implementation of 'S.Swagger' datatype your tags
-- (and thus endpoints) will be sorted alphabetically in UI, relevant ticket:
-- https://github.com/GetShopTV/swagger2/issues/165.
data TagsVerification
    = -- | Ensure that mappings are specified exactly for those tags which
      -- appear in API.
      -- This may slow down compilation dramatically starting from ~15 tags.
      VerifyTags
      -- | Do not check anything.
    | NoVerifyTags

-- | Attaches descriptions to tags according to the given
-- @name -> description@ mapping.
-- Unused elements of mapping will cause a compile error; tags which have
-- no mapping declared are not allowed as well.
data TagDescriptions (verify :: TagsVerification) (mapping :: [TyNamedParam Symbol])

instance HasServer subApi ctx => HasServer (TagDescriptions ver mapping :> subApi) ctx where
    type ServerT (TagDescriptions ver mapping :> subApi) m = ServerT subApi m
    route _ = route (Proxy @subApi)
    hoistServerWithContext _ = hoistServerWithContext (Proxy @subApi)

instance HasClient m subApi => HasClient m (TagDescriptions ver mapping :> subApi) where
    type Client m (TagDescriptions ver mapping :> subApi) = Client m subApi
    clientWithRoute pm _ = clientWithRoute pm (Proxy @subApi)

instance HasLoggingServer config subApi ctx =>
         HasLoggingServer config (TagDescriptions ver mapping :> subApi) ctx where
    routeWithLog =
        inRouteServer @(TagDescriptions ver mapping :> LoggingApiRec config subApi)
        route identity

-- | Gather all tag names used in API. Result may contain duplicates.
type family AllApiTags api :: [Symbol] where
    AllApiTags (Tag name :> api) = name `InsSorted` AllApiTags api
    AllApiTags (arg :> api) = AllApiTags api
    AllApiTags ((path :: Symbol) :> api) = AllApiTags api
    AllApiTags (api1 :<|> api2) = AllApiTags api1 `UnionSorted` AllApiTags api2
    AllApiTags (Verb (method :: StdMethod) (code :: Nat) ctx a) = '[]

-- | Extract tags defined by this mapping.
class ReifyTagsFromMapping (mapping :: [TyNamedParam Symbol]) where
    reifyTagsFromMapping :: Set S.Tag

instance ReifyTagsFromMapping '[] where
    reifyTagsFromMapping = mempty

instance ( KnownSymbol name, KnownSymbol desc
         , ReifyTagsFromMapping mapping
         , ParamsContainNoName mapping name
         ) =>
         ReifyTagsFromMapping ('TyNamedParam name desc ': mapping) where
    reifyTagsFromMapping =
        S.Tag
        { S._tagName = symbolValT @name
        , S._tagDescription = Just $ symbolValT @desc
        , S._tagExternalDocs = Nothing
        } `S.insert` reifyTagsFromMapping @mapping

instance ( HasSwagger api
         , ReifyTagsFromMapping mapping
         ) =>
         HasSwagger (TagDescriptions 'NoVerifyTags mapping :> api) where
    toSwagger _ = toSwagger (Proxy @api)
        & S.tags .~ reifyTagsFromMapping @mapping

instance ( HasSwagger api
         , ReifyTagsFromMapping mapping
         , missingMapping ~ (AllApiTags api // TyNamedParamsNames mapping)
         , If (missingMapping == '[])
            (() :: Constraint)
            (TypeError ('Text "Following tags have no mapping specified in \
                        \TagDescriptions: " ':<>: 'ShowType missingMapping))
         , extraMapping ~ (TyNamedParamsNames mapping // AllApiTags api)
         , If (extraMapping == '[])
            (() :: Constraint)
            (TypeError ('Text "Mappings for the following names specified in \
                              \TagDescriptions are unused: "
                        ':<>: 'ShowType extraMapping))
         ) =>
         HasSwagger (TagDescriptions 'VerifyTags mapping :> api) where
    toSwagger _ = toSwagger (Proxy @api)
        & S.tags .~ reifyTagsFromMapping @mapping
