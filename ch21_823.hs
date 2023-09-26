module HttpStuff where
import           Data.ByteString.Lazy    hiding ( map )
import           Network.Wreq

urls :: [String]
urls =
  [ "https://dog.ceo/api/breeds/list/all"
  , "https://dog.ceo/api/breed/hound/list"
  ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
