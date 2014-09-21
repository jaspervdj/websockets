--------------------------------------------------------------------------------
module Network.WebSockets.Tests.Util
    ( ArbitraryUtf8 (..)
    , arbitraryUtf8
    , arbitraryByteString
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative      ((<$>))
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           Test.QuickCheck          (Arbitrary (..), Gen)


--------------------------------------------------------------------------------
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
newtype ArbitraryUtf8 = ArbitraryUtf8 {unArbitraryUtf8 :: BL.ByteString}
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
instance Arbitrary ArbitraryUtf8 where
    arbitrary = ArbitraryUtf8 <$> arbitraryUtf8


--------------------------------------------------------------------------------
arbitraryUtf8 :: Gen BL.ByteString
arbitraryUtf8 = toLazyByteString . TL.encodeUtf8 . TL.pack <$> arbitrary


--------------------------------------------------------------------------------
arbitraryByteString :: Gen BL.ByteString
arbitraryByteString = BL.pack <$> arbitrary
