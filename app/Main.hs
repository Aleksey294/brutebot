{-# LANGUAGE OverloadedStrings #-}

module Main where

import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
import Telegram.Bot.API (Update, Token(..), defaultTelegramClientEnv)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Digest.Pure.MD5 (md5)
import Control.Applicative ((<|>), empty)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Concurrent.Async (async, waitAny, cancel)
import GHC.Conc (getNumCapabilities)
import Data.List (find)

allowedChars :: [Char]
allowedChars = "0123456789"

maxLength :: Int
maxLength = 5

-- Генерация всех вариантов от длины 1 до maxLength.
candidates :: [String]
candidates = concat [ sequence (replicate len allowedChars) | len <- [1..maxLength] ]

-- Функция вычисления MD5-хэша строки в виде шестнадцатеричной строки.
computeMD5 :: String -> String
computeMD5 s = show (md5 (C8.pack s))

-- Последовательный перебор.
bruteForceSeq :: String -> Maybe String
bruteForceSeq targetHash =
    find (\candidate -> computeMD5 candidate == targetHash) candidates

-- Разбиение списка на n примерно равных частей.
splitChunks :: Int -> [a] -> [[a]]
splitChunks n xs =
  let len = length xs
      chunkSize = (len + n - 1) `div` n
  in chunkList chunkSize xs
  where
    chunkList _ [] = []
    chunkList size ys =
      let (chunk, rest) = splitAt size ys
      in chunk : chunkList size rest

-- Параллельный перебор.
bruteForcePar :: String -> IO (Maybe String)
bruteForcePar targetHash = do
  numCores <- getNumCapabilities
  let chunks = splitChunks numCores candidates
      predicate candidate = computeMD5 candidate == targetHash
  tasks <- mapM (async . return . find predicate) chunks
  (winner, result) <- waitAny tasks
  mapM_ (\t -> if t /= winner then cancel t else return ()) tasks
  return result

-- Модель данных бота
type Model = ()

-- Режим перебора
data Mode = Seq | Par deriving (Show, Eq)

-- Действия бота
data Action
  = Start
  | Bruteforce Mode T.Text
  deriving (Show)

--------------------------------------------------------------------------------
-- Команды /bruteforce
parseBruteforce :: UpdateParser Action
parseBruteforce = do
  _ <- command "bruteforce"
  txt <- text
  let cleaned = if "/bruteforce" `T.isPrefixOf` txt
                  then T.strip (T.drop (T.length "/bruteforce") txt)
                  else txt
      tokens = T.words cleaned
  case tokens of
    (modeTxt:hashTxt:_) 
      | T.toLower modeTxt == "seq" -> pure (Bruteforce Seq hashTxt)
      | T.toLower modeTxt == "par" -> pure (Bruteforce Par hashTxt)
    (hashTxt:_) -> pure (Bruteforce Seq hashTxt)
    _ -> empty

--------------------------------------------------------------------------------
-- Входящие обновления
handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _ update = parseUpdate parser update
  where
    parser = Start <$ command "start" <|> parseBruteforce



-- Действия бота
handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
  case action of
    Start ->
      model <# replyText
        "Привет! Я бот для подбора пароля брут-форсом.\n\
        \Используйте команду /bruteforce [seq|par] <хэш>.\n\
        \Примеры:\n\
        \ /bruteforce seq 827ccb0eea8a706c4c34a16891f84e7b\n\
        \ /bruteforce par 827ccb0eea8a706c4c34a16891f84e7b"
    Bruteforce mode hashText ->
      model <# do
        let targetHash = T.unpack hashText
        case mode of
          Seq -> do
            startSeq <- liftIO getCurrentTime
            let seqResult = bruteForceSeq targetHash
            endSeq <- liftIO getCurrentTime
            let timeSeq = diffUTCTime endSeq startSeq
            let reply = case seqResult of
                          Just password -> "Последовательный перебор: найден пароль - " <> T.pack password <> " (время: " <> T.pack (show timeSeq) <> ")"
                          Nothing -> "Последовательный перебор: пароль не найден."
            replyText reply
          Par -> do
            startPar <- liftIO getCurrentTime
            parResult <- liftIO $ bruteForcePar targetHash
            endPar <- liftIO getCurrentTime
            let timePar = diffUTCTime endPar startPar
            let reply = case parResult of
                          Just password -> "Параллельный перебор: найден пароль - " <> T.pack password <> " (время: " <> T.pack (show timePar) <> ")"
                          Nothing -> "Параллельный перебор: пароль не найден."
            replyText reply


main :: IO ()
main = do
  let token = "8012544169:AAH2bGamH4cZiIW-DEWE22M-d9y5uZko72o" 
  env <- defaultTelegramClientEnv (Token (T.pack token))
  startBot_ bot env
  where
    bot :: BotApp Model Action
    bot = BotApp
      { botInitialModel = ()
      , botAction       = flip handleUpdate
      , botHandler      = handleAction
      , botJobs         = []
      }
