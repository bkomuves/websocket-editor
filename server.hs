{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------

import Data.Char (isPunctuation, isSpace)
import Data.List
import Data.Monoid (mappend)
import Data.Text (Text)
import Text.Read (readMaybe)

import Control.Exception (finally,try,SomeException,ErrorCall,PatternMatchFail)
import Control.Monad (forM_, forever)
import Control.Concurrent 

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LT

import qualified Data.Sequence as Seq ; import Data.Sequence ( Seq , (<|) , (|>) , (><) , ViewL(..) , ViewR(..) )
import qualified Data.Foldable as F

import qualified Network.WebSockets as WS

import LatexInput

--------------------------------------------------------------------------------

-- import Debug.Trace
-- debug_ t y   = debug t y y
-- debug  t x y = trace (">>> " ++ t ++ " = " ++ show x) y

--------------------------------------------------------------------------------

serverPort = 11667

--------------------------------------------------------------------------------

type Client = (WS.Connection)

-- -- Send a message to all clients, and log it on stdout:
-- broadcast :: Text -> ServerState -> IO ()
-- broadcast message clients = do
--   T.putStrLn message
--   forM_ clients $ \(conn) -> WS.sendTextData conn message

-- The main function first creates a new state for the server, then spawns the
-- actual server. For this purpose, we use the simple server provided by
-- `WS.runServer`.
main :: IO ()
main = do
  let url = "127.0.0.1"
  putStrLn $ "listening on ws://" ++ url ++ ":" ++ show serverPort
  WS.runServer url serverPort $ application

-- Our main application has the type:
application :: WS.PendingConnection -> IO ()
application pending = do
  conn <- WS.acceptRequest pending
  putStrLn ("connection accepted")
  state <- newMVar initialServerState 
  let disconnect = putStrLn "disconnected."
  WS.withPingThread conn 30 (return ()) $ flip finally disconnect $ handlerLoop state conn

-- trySome :: IO a -> IO (Either SomeException a)
-- trySome = try

tryError :: IO a -> IO (Either ErrorCall a)
tryError = try

tryPatternMatch :: IO a -> IO (Either PatternMatchFail a)
tryPatternMatch = try

tryPure :: IO a -> IO (Either (Either ErrorCall PatternMatchFail) a)
tryPure action = tryPatternMatch (tryError action) >>= \ei -> case ei of
  Left  err        -> return (Left (Right err))
  Right (Left err) -> return (Left (Left err)) 
  Right (Right y)  -> return (Right y)

handlerLoop :: MVar ServerState -> WS.Connection -> IO () 
handlerLoop state conn = forever $ do
  ei <- tryPure (handler state conn)
  case ei of 
    Right _  -> return ()
    Left exc -> do 
      putStrLn $ "exception happened: " ++ show exc
      threadDelay 1000

--------------------------------------------------------------------------------

data Pos = Pos 
  { _line   :: !Int 
  , _column :: !Int 
  } 
  deriving (Eq,Ord,Show)

nextCol :: Pos -> Pos
nextCol (Pos ln col) = Pos ln (col+1)

nextLine :: Pos -> Pos
nextLine (Pos ln col) = Pos (ln+1) col

moveLeft :: Int -> Pos -> Pos
moveLeft k (Pos ln col) = Pos ln (max 0 (col-k))

moveRight :: Int -> Pos -> Pos
moveRight k (Pos ln col) = Pos ln (col+k)

data Range  
  = Range !Pos !Pos 
  deriving (Eq,Show)

data Change 
  = Change !Range !LT.Text
  deriving (Eq,Show)

data Buffer 
  = Buffer { fromBuffer :: Seq T.Text }
  deriving (Eq,Show)

emptyBuffer :: Buffer
emptyBuffer = Buffer (Seq.singleton T.empty)

bufferFromText :: LT.Text -> Buffer
bufferFromText text = Buffer (Seq.fromList $ map LT.toStrict $ LT.splitOn "\n" text)

bufferToText :: Buffer -> LT.Text 
bufferToText (Buffer seq) = LT.unlines (map LT.fromStrict $ F.toList seq)

takeBuffer :: Pos -> Buffer -> Buffer
takeBuffer (Pos ln col) (Buffer seq) 
  | ln < n     = Buffer (Seq.take ln seq |> T.take col (Seq.index seq ln))
  | otherwise  = Buffer (Seq.take ln seq >< Seq.replicate (ln-n+1) T.empty)
  where
    n = Seq.length seq

dropBuffer :: Pos -> Buffer -> Buffer
dropBuffer (Pos ln col) (Buffer seq) 
  | ln < n     = Buffer (T.drop col (Seq.index seq ln) <| Seq.drop (ln+1) seq)
  | otherwise  = emptyBuffer
  where
    n = Seq.length seq

applyChange :: Change -> Buffer -> Buffer
applyChange (Change (Range pos1 pos2) newtext) buf = takeBuffer pos1 buf <+> bufferFromText newtext <+> dropBuffer pos2 buf

infixl 5 <+>
(<+>) = glueBuffer

glueBuffer :: Buffer -> Buffer -> Buffer
glueBuffer (Buffer seq1) (Buffer seq2) = case Seq.viewr seq1 of
  top  :> mid1 -> case Seq.viewl seq2 of 
    mid2 :< bot  -> Buffer ((top |> (T.append mid1 mid2)) >< bot)

--------------------------------------------------------------------------------

encodePos :: Pos -> String
encodePos (Pos y x) = show y ++ ":" ++ show x

encodeRange :: Range -> String
encodeRange (Range pos1 pos2) = encodePos pos1 ++ "-" ++ encodePos pos2

encodeChange :: Change -> String
encodeChange (Change range replacement) = encodeRange range ++ "," ++ LT.unpack (replacement) 

encodeChangeLT :: Change -> LT.Text
encodeChangeLT = LT.pack . encodeChange

--------------------------------------------------------------------------------
-- LaTeX style input

-- | Last few characters (within the line)
lookback :: Int -> Buffer -> String
lookback k (Buffer seq) = case Seq.viewr seq of
  _ :> line -> T.unpack (T.takeEnd k line)

-- > usage: "\alpha<tab>"
handleLaTeX :: Buffer -> Change -> Maybe Change
handleLaTeX buffer change = case change of
  Change range@(Range pos1 pos2) repl  
    | repl /= "\t" -> Nothing
    | otherwise    -> 
        let revchars = reverse (lookback 16 (takeBuffer pos1 buffer)) in case findIndex (=='\\') revchars of
          Nothing      -> Just (Change (Range pos1 (nextCol pos1)) "  ")   -- no latex -> tab = 2 spaces
          Just j       -> let prefix = reverse $ take j revchars in case latexCompletion prefix of
            Nothing      -> Just (Change (Range pos1 (nextCol pos1)) "")   -- not unique -> delete the tab character
            Just ch      -> Just (Change (Range (moveLeft (j+1) pos1) (nextCol pos1)) (LT.singleton ch))

--------------------------------------------------------------------------------

htmlUnlines :: [String] -> String
htmlUnlines = intercalate "<br/>"

escapeHTML :: String -> String
escapeHTML = concatMap f where
  f '&'  = "&amp;"
  f '<'  = "&lt;"
  f '>'  = "&gt;"
  f '\n' = "<br/>"
  f ' '  = "&nbsp;"
  f c    = [c]

--------------------------------------------------------------------------------

type ServerState = Buffer

initialServerState :: ServerState
initialServerState = emptyBuffer

parsePos :: LT.Text -> Maybe Pos
parsePos what = case LT.splitOn ":" what of
  [ln,col] -> Pos <$> readMaybe (LT.unpack ln) <*> readMaybe (LT.unpack col)
  _        -> Nothing

parseRange :: LT.Text -> Maybe Range
parseRange what = case LT.splitOn "-" what of
  [pos1,pos2] -> Range <$> parsePos pos1 <*> parsePos pos2 
  _           -> Nothing

parseChange :: LT.Text -> Maybe Change
parseChange what = Change <$> parseRange header <*> pure (LT.tail rest) where 
  (header,rest) = LT.span (/= ',') what

handler :: MVar ServerState -> WS.Connection -> IO () 
handler state conn = forever $ do
  msg <- WS.receiveData conn
  -- LT.putStrLn $ LT.append "message received: " msg

  case parseChange msg of

    Nothing -> do 
      putStrLn "error: cannot parse change message!!!"
      LT.putStrLn msg

    Just change -> do
      -- print change

      buf <- readMVar state
      let buf' = applyChange change buf

      case handleLaTeX buf' change of 
        Nothing    -> return ()
        Just extra -> WS.sendTextData conn (LT.append "d," (encodeChangeLT extra))

      takeMVar state >> (putMVar state $! buf')
    
      let text = bufferToText buf'

      -- putStrLn "================="
      -- LT.putStrLn text
      -- putStrLn "^^^^^^^^^^^^^^^^^"

      let feedback = LT.pack $ escapeHTML $ unlines
            [ "chars = " ++ show (LT.length text)
            , "words = " ++ show (length $ LT.words text)
            , "lines = " ++ show (length $ LT.lines text)
            ]
    
      let htmltext = LT.pack $ escapeHTML $ LT.unpack text
      WS.sendTextData conn (LT.append "f," feedback)
      WS.sendTextData conn (LT.append "e," htmltext)
    
