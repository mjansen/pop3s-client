{-# LANGUAGE OverloadedStrings #-}

module Network.POP3.SSLClient where

import Control.Monad

import Crypto.Random -- (SystemRNG(..), createEntropyPool)
import Network.Socket hiding (HostName)
import Network.TLS
import Network.TLS.Extra.Cipher

import Data.Default
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as L

import Data.X509
import Data.X509.File
import Data.X509.CertificateStore
import Data.Time

import GHC.Exts

import System.FilePath

type UserName  = String
type Password  = String
type HostName  = String
type MessageID = Int
type Port      = Int
type ByteString = BC.ByteString

debug :: Bool
debug = True

connectSSL :: HostName -> UserName -> Password -> IO Context
connectSSL mailHost user password = do
  -- s <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME] }
  addrs <- getAddrInfo (Just hints) (Just mailHost) (Just "pop3s")
  when debug $ mapM_ print addrs
  let addr = head addrs
  s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect s (addrAddress addr)
  epool <- createEntropyPool
  bundle <- readSignedObject "bundle" :: IO [SignedCertificate]
  let caStore = makeCertificateStore bundle
  let params = ClientParams Nothing (mailHost, BC.empty) True Nothing
               (def { sharedCAStore = caStore }) def (def { supportedCiphers = ciphersuite_all })
      rng = cprgCreate epool :: SystemRNG
  ctx <- contextNew s params rng
  handshake ctx
  _ <- recvDataD ctx  -- greeting
  _ <- rpcT ctx (USER user)
  _ <- rpcT ctx (PASS password)
  return ctx

closeConnection :: Context -> IO ()
closeConnection ctx = do
  rpcT ctx QUIT
  contextClose ctx
  
data POP3Command = USER UserName
                 | PASS Password
                 | QUIT
                 | STAT
                 | LALL
                 | LIST MessageID
                 | RETR MessageID
                 | DELE MessageID
                 | NOOP
                 | LAST
                 | RSET
                 | RPOP UserName
                 | TOP MessageID Int
                 | UIDL MessageID
                 | UALL

instance Show POP3Command where
  show (USER name)       = "USER " ++ name
  show (PASS pwd)        = "PASS " ++ pwd
  show  QUIT             = "QUIT"
  show  STAT             = "STAT"
  show  LALL             = "LIST"
  show (LIST msg)        = "LIST " ++ show msg
  show (RETR msg)        = "RETR " ++ show msg
  show (DELE msg)        = "DELE " ++ show msg
  show  NOOP             = "NOOP"
  show  LAST             = "LAST"
  show  RSET             = "RSET"
  show (RPOP name)       = "RPOP " ++ name
  show (TOP msg n)       = "TOP "  ++ show msg ++ " " ++ show n
  show (UIDL msg)        = "UIDL " ++ show msg
  show  UALL             = "UIDL"

data POP3Response = Message ByteString
                  | Size Int
                  | Status Int Int
                  | Error ByteString
                  | OK ByteString
                  deriving Show

parseReply :: ByteString -> POP3Response
parseReply str =
  if BC.null str
  then error "unexpected end of session"
  else case BC.head str of
        '-' -> Error . BC.tail $ str
        '+' -> OK    . BC.tail $ str

recvDataD ctx = do
  r <- recvData ctx
  when debug $ print ("received", BC.take 64 r)
  return r

sendDataD ctx str = do
  when debug $ print ("sending", str)
  sendData ctx str

recvDataUntil :: BC.ByteString -> BC.ByteString -> Context -> IO ByteString
recvDataUntil sentinel sofar ctx = do
  str <- BC.append sofar <$> recvDataD ctx
  if sentinel `BC.isSuffixOf` str
    then return str
    else recvDataUntil sentinel str ctx

recvDataSingleLine :: Context -> IO ByteString
recvDataSingleLine = recvDataUntil (BC.pack "\r\n") BC.empty

recvDataMultiLine :: Context -> IO ByteString
recvDataMultiLine = recvDataUntil (BC.pack "\r\n.\r\n") BC.empty

data SessionState = Authentication | Transaction

-- transaction state rpc:

rpcT :: Context -> POP3Command -> IO POP3Response
rpcT ctx cmd = do
  sendDataD ctx (L.fromStrict . (`BC.append` BC.pack "\r\n") . BC.pack . show $ cmd)
  case cmd of
   LALL        -> parseReply <$> recvDataMultiLine  ctx
   (RETR _)    -> parseReply <$> recvDataMultiLine  ctx
   (TOP msg n) -> parseReply <$> recvDataMultiLine  ctx
   _           -> parseReply <$> recvDataSingleLine ctx
  
parseMessageList :: POP3Response -> [(Int, Int)]
parseMessageList (OK str) = map (\ [msgIdStr, msgSzStr] -> (read' msgIdStr, read' msgSzStr))
                          . map BC.words . init . tail . BC.lines $ str
  where read' = read . BC.unpack
parseMessageList _ = error "expected a simple string"

parseUIDList :: POP3Response -> [(Int, ByteString)]
parseUIDList (OK str) = map (\ [msgIdStr, msgUID] -> (read' msgIdStr, msgUID))
                          . map BC.words . init . tail . BC.lines $ str
  where read' = read . BC.unpack
parseUIDList _ = error "expected a simple string"

retrieveMessage :: Context -> Int -> Int -> IO ()
retrieveMessage ctx msgId sz = do
  OK msg' <- rpcT ctx (RETR msgId)
  let (header, msg) = decodeMultiLine msg'
  let fName = "msg." ++ show msgId
  BC.writeFile fName msg
  putStrLn $ "wrote " ++ show sz ++ " bytes to " ++ fName ++ " but was "
    ++ (show $ BC.length msg + length (BC.lines msg))
    ++ " (lines: " ++ (show . length . BC.lines $ msg)
    ++ ")"
  writeToMBOX "new" msg

retrieveMessageUID :: Context -> Int -> ByteString -> IO ()
retrieveMessageUID ctx msgId muid = do
  OK msg' <- rpcT ctx (RETR msgId)
  let (header, msg) = decodeMultiLine msg'
  let fName = "msg" </> BC.unpack muid
  BC.writeFile fName msg
  putStrLn $ "wrote " ++ show (BC.length msg) ++ " bytes to " ++ fName
    ++ " (" ++ (show . length . BC.lines $ msg) ++ " lines)"
  writeToMBOX "new" msg

encodeSingleLine :: ByteString -> ByteString
encodeSingleLine = (`BC.append` "\r\n")

decodeSingleLine :: ByteString -> ByteString
decodeSingleLine str =
  if "\r\n" `BC.isSuffixOf` str
  then BC.take (BC.length str - 2) str
  else error "string not terminated with crlf"

encodeMultiLine :: ByteString -> ByteString
encodeMultiLine = BC.concat
                . (++ [".\r\n"])
                . map (`BC.append` "\r\n")
                . map (\ str -> if not (BC.null str) && BC.head str == '.'
                                then "." `BC.append` str
                                else str)
                . BC.lines

decodeMultiLine :: ByteString -> (ByteString, ByteString)
decodeMultiLine = (\(l:ls) -> (l, BC.unlines ls))
                . map trimDot  
                . map trimCR
                . init
                . BC.lines
  where trimCR :: ByteString -> ByteString
        trimCR str = if not (BC.null str) && BC.last str == '\r'
                     then BC.init str
                     else error "did not find a cr at end of line"
        trimDot :: ByteString -> ByteString
        trimDot str = if not (BC.null str) && BC.head str == '.'
                      then BC.tail str
                      else str

writeToMBOX :: FilePath -> ByteString -> IO ()
writeToMBOX fName msg = do
  now <- getCurrentTime
  let envelopeSender = "a@b.c"
  BC.appendFile fName (encodeMessageForMBOX now envelopeSender msg)

encodeMessageForMBOX :: UTCTime -> String -> ByteString -> ByteString
encodeMessageForMBOX ts sender msg =
  BC.unlines [ BC.pack . unwords $ ["From", sender
                                   , formatTime defaultTimeLocale asctimeFormat ts]
             , escapeFrom msg
             , ""
             ]
  where
    escapeFrom :: ByteString -> ByteString
    escapeFrom = BC.unlines . map increaseEscape . BC.lines
    hasLeadingFrom str = if BC.null str
                         then False
                         else case BC.head str of
                           '>' -> hasLeadingFrom (BC.tail str)
                           _   -> BC.take 5 str == "From "
    increaseEscape :: ByteString -> ByteString
    increaseEscape str | hasLeadingFrom str = BC.cons '>' str
                       | otherwise = str

asctimeFormat :: String
asctimeFormat = "%a %b %e %H:%M:%S %Y" -- "Wed Jun 30 21:49:08 1993"
