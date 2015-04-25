import Control.Monad

import Network.TLS (Context)

import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Set as Set

-- import Data.Time

-- import GHC.Exts

import System.Directory

import Network.POP3.SSLClient

main :: IO ()
main = withPOP3Connection retrieveAllUsingUIDWithoutDeletion

withPOP3Connection :: (Context -> IO ()) -> IO ()
withPOP3Connection action = do
  [ host, user, password ] <- lines <$> readFile "config"
  ctx <- connectSSL host user password
  action ctx
  closeConnection ctx

retrieveAllWithoutDeletion :: Context -> IO ()
retrieveAllWithoutDeletion ctx = do
  messageList <- parseMessageList <$> rpcT ctx LALL
  when debug $ mapM_ print messageList
  mapM_ (uncurry $ retrieveMessage ctx) messageList
  
retrieveAllUsingUIDWithoutDeletion :: Context -> IO ()
retrieveAllUsingUIDWithoutDeletion ctx = do
  haves <- getAlreadyDownloadedMessages
  messageList <- parseUIDList <$> rpcT ctx UALL
  let messageListNew = filter ((`Set.notMember` haves) . snd) messageList
  when debug $ mapM_ print messageList
  mapM_ (uncurry $ retrieveMessageUID ctx) messageListNew

-- assume a simple data base of already downloaded messages.  it is
-- implemented by a directory, with each message in a separate file.
-- we assume here that the directory is 'msg'.

getAlreadyDownloadedMessages :: IO (Set.Set ByteString)
getAlreadyDownloadedMessages = do
  haves <- Set.fromList . map BC.pack . filter (`notElem` [".", ".."]) <$> getDirectoryContents "msg"
  return haves
