--------------------------------------------------------------------------------
module Network.WebSockets.Tests.Util
    ( ArbitraryUtf8 (..)
    , arbitraryUtf8
    , makeChanPipe
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative          ((<$>), (<*>))
import           Control.Concurrent.Chan      (newChan)
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TL
import           System.IO.Streams            (InputStream, OutputStream)
import qualified System.IO.Streams.Concurrent as Streams
import           Test.QuickCheck              (Arbitrary (..), Gen)


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
-- | TODO: I added this function to the io-streams library but it isn't released
-- yet, at some point we should be able to remove it here.
makeChanPipe :: IO (InputStream a, OutputStream a)
makeChanPipe = do
    chan <- newChan
    (,) <$> Streams.chanToInput chan <*> Streams.chanToOutput chan
