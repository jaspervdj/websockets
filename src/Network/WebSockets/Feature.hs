
module Network.WebSockets.Feature 
    ( Feature(..)
    , Features
    , binary, ping, fragmentation, close

    -- for convenience
    , S.fromList
    , S.toList
    , S.isSubsetOf
    , S.difference
    , S.empty
    , S.null
    , S.unions
    ) where

import qualified Data.Set as S

-- | Available features. Subject to future change. Use the bindings below
-- instead.
data Feature = Binary | Ping | Fragmentation | Close
    deriving (Eq, Ord, Show, Read)

type Features = S.Set Feature

binary, ping, fragmentation, close :: Features
binary = S.fromList [Binary]
ping = S.fromList [Ping]
fragmentation = S.fromList [Fragmentation]
close = S.fromList [Close]

